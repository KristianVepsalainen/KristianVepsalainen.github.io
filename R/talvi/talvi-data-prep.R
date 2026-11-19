# R/talvi/talvi-data-prep.R
# Talvi-sarjan yhteinen säädata: asemat, päivähavainnot ja talvikausien indikaattorit.
#
# Lähteet
#   Suomi: Ilmatieteen laitoksen avoin WFS (fmi::observations::weather::daily::simple)
#   Kanadan territoriot, Alaska ja USA:n talviosavaltiot, Ruotsin ja Norjan pohjoiset
#   alueet: NOAA GHCN-Daily (asemakohtaiset CSV-tiedostot)
#
# Käyttö
#   TALVI_TEST=1 Rscript R/talvi/talvi-data-prep.R   # pieni koeajo, ei tallennuksia
#   Rscript R/talvi/talvi-data-prep.R                # kaikki vaiheet
#   TALVI_RUN=ghcn,kausi Rscript R/talvi/talvi-data-prep.R
#
# Vaiheet: asemat -> fmi -> ghcn -> kausi
# Raakavastaukset välimuistiin data/talvi/raw/ (gitignore), joten keskeytynyt ajo
# jatkuu siitä mihin jäi.
#
# Tuotokset
#   data/talvi/talvi_asemat.qs2         asemat, lähde, alue, koordinaatit
#   data/talvi/paiva/<lahde>_<alue>.qs2 päivähavainnot alueittain
#   data/talvi/talvi_kausi.qs2          asema x talvikausi -indikaattorit

source(here::here("R", "talvi", "talvi-helpers.R"))
suppressPackageStartupMessages({
  library(httr2)
  library(xml2)
  library(sf)
  library(giscoR)
})

vaiheet <- trimws(strsplit(Sys.getenv("TALVI_RUN", "asemat,fmi,ghcn,kausi"), ",")[[1]])
stopifnot(length(vaiheet) > 0, all(vaiheet %in% c("asemat", "fmi", "ghcn", "kausi")))
TESTI <- Sys.getenv("TALVI_TEST", "0") == "1"

ALKU_VUOSI <- 1961L
LOPPU_PVM  <- Sys.Date() - 1L
for (k in c(TALVI_DIR, TALVI_PAIVA, file.path(TALVI_RAW, "fmi"), file.path(TALVI_RAW, "ghcn"))) {
  dir.create(k, recursive = TRUE, showWarnings = FALSE)
}

# Asemien enimmäismäärät alueittain (latausajan rajaamiseksi)
FMI_MAX_PER_MAAKUNTA <- 5L
GHCN_MAX_PER_ALUE    <- c(US = 25L, AK = 40L, CA = 30L, EU = 30L)

# GHCN-alueet. Yhdysvalloille ja Kanadalle alue tulee asemaluettelon
# osavaltio/provinssi-sarakkeesta, Ruotsille ja Norjalle NUTS2-alueesta.
GHCN_ALUEET <- tribble(
  ~maa, ~alue,   ~alue_nimi,                ~ryhma,
  "US", "US-AK", "Alaska",                  "AK",
  "US", "US-MN", "Minnesota",               "US",
  "US", "US-ND", "Pohjois-Dakota",          "US",
  "US", "US-MT", "Montana",                 "US",
  "US", "US-WI", "Wisconsin",               "US",
  "US", "US-MI", "Michigan",                "US",
  "US", "US-ME", "Maine",                   "US",
  "US", "US-VT", "Vermont",                 "US",
  "US", "US-NH", "New Hampshire",           "US",
  "CA", "CA-YT", "Jukon",                   "CA",
  "CA", "CA-NT", "Luoteisterritoriot",      "CA",
  "CA", "CA-NU", "Nunavut",                 "CA",
  "SE", "SE33",  "Övre Norrland (Ruotsin Lappi ja Västerbotten)", "EU",
  "NO", "NO07",  "Pohjois-Norja",           "EU"
)
GHCN_MAAKOODI <- c(US = "US", CA = "CA", SE = "SW", NO = "NO")   # GHCN-tunnuksen etuliite

# ===========================================================================
# Yleiset HTTP-apurit
# ===========================================================================
perus_req <- function(url) {
  request(url) |>
    req_user_agent("talvi-sarja (kristianvepsalainen.com)") |>
    req_retry(max_tries = 5,
              is_transient = \(r) resp_status(r) %in% c(429L, 500L, 502L, 503L, 504L),
              retry_on_failure = TRUE,          # myös katkenneet yhteydet
              backoff = \(i) 5 * i) |>
    req_options(http_version = 2L) |>       # HTTP/1.1: NCEI katkaisee HTTP/2-siirtoja
    req_error(is_error = \(r) FALSE)
}

# FMI sallii avoimessa rajapinnassa noin 600 pyyntöä / 5 min; pysytään alle.
fmi_req <- function(...) {
  perus_req("https://opendata.fmi.fi/wfs") |>
    req_url_query(service = "WFS", version = "2.0.0", request = "getFeature", ...) |>
    req_throttle(rate = 1, realm = "fmi")
}

lataa_tiedosto <- function(url, polku, yritykset = 5L) {
  if (file.exists(polku) && file.size(polku) > 0) return(invisible(polku))
  tmp <- paste0(polku, ".tmp")
  for (i in seq_len(yritykset)) {
    unlink(tmp)
    resp <- tryCatch(
      perus_req(url) |> req_perform(path = tmp),
      error = \(e) {
        message(sprintf("Lataus %d/%d keskeytyi (%s): %s", i, yritykset, basename(polku),
                        conditionMessage(e)))
        NULL
      })
    if (!is.null(resp) && resp_status(resp) == 200L && file.size(tmp) > 0) {
      stopifnot(file.rename(tmp, polku))
      return(invisible(polku))
    }
    if (!is.null(resp) && resp_status(resp) %in% c(403L, 404L)) {   # ei kannata yrittää uudelleen
      message(sprintf("HTTP %d: %s", resp_status(resp), url))
      break
    }
    Sys.sleep(10 * i)
  }
  unlink(tmp)
  warning(sprintf("Lataus epäonnistui: %s", url))
  invisible(NULL)
}

# Liittää pisteet alueisiin. Rannikon ja saarten asemat voivat jäädä 1:1 milj.
# mittakaavan polygonin ulkopuolelle, joten ne liitetään lähimpään alueeseen,
# jos etäisyys on alle 25 km.
liita_alueeseen <- function(pisteet, alueet, koodisarake, nimisarake) {
  stopifnot(inherits(pisteet, "sf"), inherits(alueet, "sf"))
  sisalla <- st_within(pisteet, alueet)
  idx <- vapply(sisalla, \(v) if (length(v)) v[1] else NA_integer_, integer(1))
  puuttuu <- which(is.na(idx))
  if (length(puuttuu)) {
    lahin <- st_nearest_feature(pisteet[puuttuu, ], alueet)
    etaisyys <- as.numeric(st_distance(pisteet[puuttuu, ], alueet[lahin, ], by_element = TRUE))
    idx[puuttuu] <- ifelse(etaisyys < 25000, lahin, NA_integer_)
  }
  pisteet[[koodisarake]] <- alueet[[koodisarake]][idx]
  pisteet[[nimisarake]]  <- alueet[[nimisarake]][idx]
  pisteet
}

# ===========================================================================
# FMI
# ===========================================================================

# Asemaluettelo fmi::ef::stations. Palauttaa kaikki asemat verkostotietoineen.
fmi_asemaluettelo <- function() {
  resp <- fmi_req(storedquery_id = "fmi::ef::stations") |> req_perform()
  stopifnot(resp_status(resp) == 200L)
  doc <- read_xml(resp_body_string(resp))
  fac <- xml_find_all(doc, "//*[local-name()='EnvironmentalMonitoringFacility']")
  stopifnot(length(fac) > 100)

  teksti <- function(f, xp) {
    n <- xml_find_first(f, xp)
    if (inherits(n, "xml_missing")) NA_character_ else xml_text(n)
  }
  map(fac, function(f) {
    nimet <- xml_find_all(f, ".//*[local-name()='name']")
    cs <- xml_attr(nimet, "codeSpace")
    verkot <- xml_find_all(f, ".//*[local-name()='belongsTo']") |>
      map_chr(\(n) { a <- xml_attrs(n); v <- a[grepl("title$", names(a))]; if (length(v)) v[[1]] else NA_character_ })
    tibble(
      fmisid  = teksti(f, ".//*[local-name()='identifier']"),
      nimi    = if (any(grepl("/name$", cs))) xml_text(nimet[grepl("/name$", cs)][1]) else xml_text(nimet[1]),
      pos     = teksti(f, ".//*[local-name()='representativePoint']//*[local-name()='pos']"),
      alku    = teksti(f, ".//*[local-name()='beginPosition']"),
      loppu   = teksti(f, ".//*[local-name()='endPosition']"),
      verkot  = paste(unique(na.omit(verkot)), collapse = "; ")
    )
  }) |>
    list_rbind() |>
    mutate(
      lat  = as.numeric(sub("^\\s*(\\S+)\\s+\\S+.*$", "\\1", pos)),
      lon  = as.numeric(sub("^\\s*\\S+\\s+(\\S+).*$", "\\1", pos)),
      alku = as.Date(substr(alku, 1, 10)),
      loppu = suppressWarnings(as.Date(substr(loppu, 1, 10)))
    ) |>
    select(-pos)
}

# Yhden aseman yhden kalenterivuoden päivähavainnot (FMI rajoittaa
# päiväkyselyn noin vuoteen). Välimuisti raw/fmi/<fmisid>_<vuosi>.qs2.
# Kuluva ja edellinen vuosi haetaan aina uudelleen, koska data täydentyy.
FMI_SARAKKEET <- c("tday", "tmin", "tmax", "rrday", "snow")

fmi_paiva_vuosi <- function(fmisid, vuosi, valimuisti = TRUE) {
  polku <- file.path(TALVI_RAW, "fmi", sprintf("%s_%d.qs2", fmisid, vuosi))
  tuore <- vuosi >= year(Sys.Date()) - 1L
  if (valimuisti && file.exists(polku) && !tuore) return(qd_read(polku))

  alku  <- as.Date(sprintf("%d-01-01", vuosi))
  loppu <- min(as.Date(sprintf("%d-12-31", vuosi)), LOPPU_PVM)
  if (alku > loppu) return(tibble())
  resp <- fmi_req(storedquery_id = "fmi::observations::weather::daily::simple",
                  fmisid = fmisid,
                  starttime = paste0(alku, "T00:00:00Z"),
                  endtime   = paste0(loppu, "T00:00:00Z"),
                  parameters = paste(FMI_SARAKKEET, collapse = ",")) |>
    req_perform()
  if (resp_status(resp) != 200L) {
    warning(sprintf("FMI %s %d: HTTP %d", fmisid, vuosi, resp_status(resp)))
    return(NULL)   # NULL = virhe, ei tallenneta välimuistiin
  }
  doc <- read_xml(resp_body_string(resp))
  aika  <- xml_text(xml_find_all(doc, "//*[local-name()='BsWfsElement']/*[local-name()='Time']"))
  param <- xml_text(xml_find_all(doc, "//*[local-name()='BsWfsElement']/*[local-name()='ParameterName']"))
  arvo  <- xml_text(xml_find_all(doc, "//*[local-name()='BsWfsElement']/*[local-name()='ParameterValue']"))
  stopifnot(length(aika) == length(param), length(param) == length(arvo))

  out <- if (!length(aika)) {
    tibble(pvm = as.Date(character()))
  } else {
    a <- suppressWarnings(as.numeric(arvo))
    a[is.nan(a)] <- NA_real_
    tibble(pvm = as.Date(substr(aika, 1, 10)), param = param, arvo = a) |>
      pivot_wider(names_from = param, values_from = arvo, values_fn = \(x) x[1])
  }
  for (k in FMI_SARAKKEET) if (!k %in% names(out)) out[[k]] <- NA_real_
  out <- out |> select(pvm, all_of(FMI_SARAKKEET)) |> filter(!if_all(all_of(FMI_SARAKKEET), is.na))
  stopifnot(!anyDuplicated(out$pvm))
  if (valimuisti) qd_save(out, polku)
  out
}

# FMI:n koodaus: rrday = -1 tarkoittaa "ei sadetta" (0,0 = alle 0,1 mm) ja
# snow = -1 lumetonta maata. Muunnetaan nolliksi, EI puuttuviksi: puuttuva
# havainto on FMI:llä NaN. (Tarkistettava testiajon taulukosta.)
fmi_siivoa <- function(d) {
  d |>
    mutate(
      rr      = if_else(rrday == -1, 0, rrday),
      lumi_cm = if_else(snow == -1, 0, snow)
    ) |>
    select(any_of("asema_id"), pvm, tmin, tmax, tday, rr, lumi_cm)
}

fmi_testi <- function() {
  message("FMI-testi: Helsinki Kaisaniemi (100971), tammikuu 2024 ja heinäkuu 2024")
  d <- fmi_paiva_vuosi("100971", 2024L, valimuisti = FALSE)
  stopifnot(nrow(d) > 300)
  print(d |> filter(month(pvm) %in% c(1, 7)) |> slice(c(1:5, 32:36)))
  message("Arvot -1 sarakkeittain (odotus: rrday ja snow sisältävät -1:iä, lämpötilat eivät):")
  print(sapply(d[FMI_SARAKKEET], \(x) sum(x == -1, na.rm = TRUE)))
  message("Puuttuvat sarakkeittain:")
  print(sapply(d[FMI_SARAKKEET], \(x) sum(is.na(x))))
  rng <- sapply(d[c("tday", "tmin", "tmax")], range, na.rm = TRUE)
  print(rng)
  stopifnot(rng[1, "tmin"] > -45, rng[2, "tmax"] < 40)

  message("FMI-testi: asemaluettelo")
  as <- fmi_asemaluettelo()
  message(sprintf("%d asemaa. Verkostot (yleisimmät):", nrow(as)))
  print(head(sort(table(unlist(strsplit(as$verkot, "; "))), decreasing = TRUE), 15))
  print(as |> filter(fmisid %in% c("100971", "101932", "101786")))   # Kaisaniemi, Sodankylä, Oulu lentoasema
  invisible(as)
}

# ===========================================================================
# GHCN-Daily
# ===========================================================================
# NOAA:n oma palvelin (www.ncei.noaa.gov) katkaisee TLS-yhteydet tavalla, jota
# OpenSSL 3+/4 ei hyväksy ("unexpected eof"). Käytetään NOAA:n virallista
# peiliä AWS:n avoimen datan rekisterissä (päivittyy päivittäin).
GHCN_URL <- "https://noaa-ghcn-pds.s3.amazonaws.com"

ghcn_meta <- function() {
  ps <- file.path(TALVI_RAW, "ghcn", "ghcnd-stations.txt")
  pi <- file.path(TALVI_RAW, "ghcn", "ghcnd-inventory.txt")
  lataa_tiedosto(paste0(GHCN_URL, "/ghcnd-stations.txt"), ps)
  lataa_tiedosto(paste0(GHCN_URL, "/ghcnd-inventory.txt"), pi)
  stopifnot(file.exists(ps), file.exists(pi))
  asemat <- read_fwf(ps, fwf_positions(c(1, 13, 22, 32, 39, 42), c(11, 20, 30, 37, 40, 71),
                                       c("id", "lat", "lon", "korkeus", "osavaltio", "nimi")),
                     col_types = "cdddcc", progress = FALSE) |>
    mutate(nimi = trimws(nimi), osavaltio = trimws(osavaltio))
  inv <- read_fwf(pi, fwf_positions(c(1, 32, 37, 42), c(11, 35, 40, 45),
                                    c("id", "elementti", "eka", "vika")),
                  col_types = "ccii", progress = FALSE)
  stopifnot(nrow(asemat) > 100000, all(c("TMAX", "TMIN", "SNWD") %in% inv$elementti))
  list(asemat = asemat, inv = inv)
}

# Asemakohtainen CSV (pitkä muoto): ID, DATE (YYYYMMDD), ELEMENT, DATA_VALUE,
# M_FLAG, Q_FLAG, S_FLAG, OBS_TIME. Raakayksiköt: lämpötila 0,1 °C, sade 0,1 mm,
# lumensyvyys mm (tarkistus ghcn_yksikkotarkistus()-funktiolla).
# Havainnot, joilla on laatulippu (Q_FLAG ei tyhjä), eli jotka ovat hylänneet
# NOAA:n laatutarkistuksen, poistetaan.
GHCN_ELEMENTIT <- c("TMAX", "TMIN", "PRCP", "SNWD")

GHCN_SARAKKEET <- c("id", "pvm", "elementti", "arvo", "m_lippu", "q_lippu", "s_lippu", "aika")

# Pitkä muoto -> leveä (id, pvm, TMAX, TMIN, PRCP, SNWD), laatuliputetut pois.
ghcn_leveaksi <- function(d) {
  d <- d |> filter(elementti %in% GHCN_ELEMENTIT)
  hylatyt <- sum(!is.na(d$q_lippu) & trimws(d$q_lippu) != "")
  d <- d |>
    filter(is.na(q_lippu) | trimws(q_lippu) == "", !is.na(arvo), arvo != -9999) |>
    transmute(id, pvm = as.Date(pvm, format = "%Y%m%d"), elementti, arvo) |>
    filter(!is.na(pvm), pvm >= as.Date(sprintf("%d-01-01", ALKU_VUOSI))) |>
    pivot_wider(names_from = elementti, values_from = arvo, values_fn = \(x) x[1])
  for (k in GHCN_ELEMENTIT) if (!k %in% names(d)) d[[k]] <- NA_real_
  d <- d |> select(id, pvm, all_of(GHCN_ELEMENTIT)) |> arrange(id, pvm)
  attr(d, "hylatyt_laatulippu") <- hylatyt
  d
}

ghcn_asema <- function(id) {
  polku <- file.path(TALVI_RAW, "ghcn", paste0(id, ".csv"))
  lataa_tiedosto(sprintf("%s/csv/by_station/%s.csv", GHCN_URL, id), polku)
  if (!file.exists(polku)) return(NULL)
  otsikko <- grepl('^"?ID"?,', readLines(polku, n = 1L))   # otsikkorivi tai ei
  d <- read_csv(polku, col_names = GHCN_SARAKKEET, col_types = "cccdcccc",
                skip = as.integer(otsikko), na = "", progress = FALSE, show_col_types = FALSE)
  stopifnot(all(d$id == id, na.rm = TRUE))
  out <- ghcn_leveaksi(d)
  h <- attr(out, "hylatyt_laatulippu")
  out <- out |> select(-id)
  attr(out, "hylatyt_laatulippu") <- h
  out
}

# AWS-peilin asemakohtaiset tiedostot voivat jäädä jälkeen vuositiedostoista
# (testissä Minneapolis päättyi 6.2.2025). Puuttuva häntä täydennetään
# vuositiedostoista csv.gz/<vuosi>.csv.gz, joista poimitaan vain valitut asemat.
# Kuluvan vuoden tiedosto haetaan uudelleen, jos se on vanhempi kuin tämä päivä.
# AWS-ämpärin rakenne on muuttunut ajan myötä, joten kokeillaan vuositiedostolle
# useita polkuja. Jos mikään ei toimi, palautetaan NULL ja varoitetaan: häntä jää
# täydentämättä, mutta ajo jatkuu.
ghcn_vuosi <- function(vuosi, idt) {
  vm <- file.path(TALVI_RAW, "ghcn", sprintf("vuosi_%d_valitut.qs2", vuosi))
  ehdokkaat <- c(sprintf("%s/csv.gz/by_year/%d.csv.gz", GHCN_URL, vuosi),
                 sprintf("%s/csv.gz/%d.csv.gz", GHCN_URL, vuosi),
                 sprintf("%s/csv/by_year/%d.csv", GHCN_URL, vuosi))
  paikalliset <- file.path(TALVI_RAW, "ghcn", sprintf("vuosi_%d%s", vuosi,
                                                      ifelse(grepl("gz$", ehdokkaat), ".csv.gz", ".csv")))
  if (vuosi >= year(Sys.Date())) {
    vanhat <- paikalliset[file.exists(paikalliset) & as.Date(file.mtime(paikalliset)) < Sys.Date()]
    if (length(vanhat)) unlink(c(vanhat, vm))
  }
  if (file.exists(vm)) {
    v <- qs_read(vm)
    if (all(idt %in% v$idt)) return(v$data |> filter(id %in% idt))
  }
  tiedosto <- NULL
  for (k in seq_along(ehdokkaat)) {
    if (!is.null(lataa_tiedosto(ehdokkaat[k], paikalliset[k]))) { tiedosto <- paikalliset[k]; break }
  }
  if (is.null(tiedosto)) {
    warning(sprintf("GHCN-vuositiedostoa %d ei saatu mistään polusta; häntä jää täydentämättä.", vuosi))
    return(NULL)
  }
  message(sprintf("Luetaan %s (iso, kestää muutaman minuutin)...", basename(tiedosto)))
  d <- read_csv_chunked(
    tiedosto,
    DataFrameCallback$new(\(x, pos) x[x$id %in% idt & x$elementti %in% GHCN_ELEMENTIT, ]),
    chunk_size = 2e6, col_names = GHCN_SARAKKEET, col_types = "cccdcccc", na = "",
    progress = FALSE
  ) |>
    ghcn_leveaksi()
  qs_save(list(idt = idt, data = d), vm)
  d
}

# Tarkistaa, että lämpötilat ovat kymmenyksinä (°C*10). Heinäkuun TMAX-mediaani
# ei kelpaa mittariksi, koska korkean arktisen alueen asemilla (esim. Nunavut)
# heinäkuun ylin lämpö on vain noin 5–7 °C. Käytetään sen sijaan itseisarvojen
# 99. persentiiliä koko sarjasta: kymmenyksinä se on pohjoisilla asemilla
# 100–700 (10–70 °C), kokonaisina asteina alle 70.
# Palauttaa FALSE ja varoittaa, jos yksiköt eivät täsmää; asema jätetään pois.
ghcn_yksikkotarkistus <- function(d, id) {
  q <- quantile(abs(c(d$TMAX, d$TMIN)), 0.99, na.rm = TRUE, names = FALSE)
  ok <- is.finite(q) && q >= 100 && q <= 700
  if (!ok) warning(sprintf("GHCN %s: |T| 99. persentiili %.1f ei näytä kymmenyksiltä, asema ohitetaan.", id, q))
  ok
}

ghcn_siivoa <- function(d) {
  d |>
    transmute(pvm,
              tmin = TMIN / 10, tmax = TMAX / 10, tday = NA_real_,
              rr = PRCP / 10, lumi_cm = SNWD / 10) |>
    # GHCN:n laatuliput eivät ole mukana tässä, joten karsitaan fyysisesti mahdottomat
    mutate(tmin = if_else(tmin < -70 | tmin > 45, NA_real_, tmin),
           tmax = if_else(tmax < -65 | tmax > 50, NA_real_, tmax),
           across(c(tmin, tmax), \(x) if_else(!is.na(tmin) & !is.na(tmax) & tmin > tmax + 0.5, NA_real_, x)),
           lumi_cm = if_else(lumi_cm < 0 | lumi_cm > 1000, NA_real_, lumi_cm),
           rr = if_else(rr < 0 | rr > 500, NA_real_, rr))
}

ghcn_testi <- function() {
  message("GHCN-testi: Minneapolis–St Paul (USW00014922)")
  d <- ghcn_asema("USW00014922")
  stopifnot(!is.null(d), nrow(d) > 10000)
  print(head(d)); print(tail(d))
  message(sprintf("Laatulipun vuoksi hylättyjä havaintoja: %s", attr(d, "hylatyt_laatulippu")))
  stopifnot(ghcn_yksikkotarkistus(d, "USW00014922"))
  s <- ghcn_siivoa(d)
  print(summary(s))
  message("Tammikuun lumensyvyyden mediaani (cm), pitäisi olla luokkaa 5–30:")
  print(median(s$lumi_cm[month(s$pvm) == 1], na.rm = TRUE))
  m <- ghcn_meta()
  message("Asemia alueittain (TMAX, TMIN ja SNWD vähintään 1995–2024):")
  print(ghcn_valitse_asemat(m) |> count(alue))
}

ghcn_valitse_asemat <- function(m) {
  pitkat <- m$inv |>
    filter(elementti %in% c("TMAX", "TMIN", "SNWD")) |>
    group_by(id) |>
    filter(n_distinct(elementti) == 3L) |>
    summarise(eka = max(eka), vika = min(vika), .groups = "drop") |>
    filter(eka <= 1995L, vika >= 2024L)

  ehdokkaat <- m$asemat |>
    inner_join(pitkat, by = "id") |>
    mutate(etuliite = substr(id, 1, 2))

  # USA ja Kanada osavaltio-/provinssikoodin perusteella
  pohjois_amerikka <- ehdokkaat |>
    filter(etuliite %in% c("US", "CA")) |>
    mutate(alue = paste0(etuliite, "-", osavaltio)) |>
    inner_join(GHCN_ALUEET |> select(alue, alue_nimi, ryhma, maa), by = "alue")

  # Ruotsi ja Norja NUTS2-alueen perusteella
  eu_ehd <- ehdokkaat |> filter(etuliite %in% GHCN_MAAKOODI[c("SE", "NO")])
  nuts2 <- gisco_get_nuts(year = "2021", nuts_level = 2, country = c("SE", "NO"),
                          resolution = "01", epsg = "4326") |>
    select(alue = NUTS_ID, nuts_nimi = NAME_LATN)
  eu <- st_as_sf(eu_ehd, coords = c("lon", "lat"), crs = 4326, remove = FALSE) |>
    liita_alueeseen(nuts2, "alue", "nuts_nimi") |>
    st_drop_geometry() |>
    inner_join(GHCN_ALUEET |> select(alue, alue_nimi, ryhma, maa), by = "alue") |>
    select(-nuts_nimi)

  bind_rows(pohjois_amerikka, eu) |>
    group_by(alue) |>
    arrange(eka, desc(vika), .by_group = TRUE) |>
    mutate(rivi = row_number()) |>
    filter(rivi <= GHCN_MAX_PER_ALUE[ryhma]) |>
    ungroup() |>
    transmute(lahde = "ghcn", asema_id = id, nimi, lat, lon, korkeus, maa,
              alue, alue_nimi, eka_vuosi = eka)
}

# ===========================================================================
# TESTIAJO
# ===========================================================================
if (TESTI) {
  fmi_testi()
  ghcn_testi()
  message("Testiajo valmis. Mitään ei tallennettu data/talvi-kansioon.")
  quit(save = "no", status = 0)
}

# ===========================================================================
# VAIHE: asemat
# ===========================================================================
asemat_polku <- file.path(TALVI_DIR, "talvi_asemat.qs2")

if ("asemat" %in% vaiheet) {
  message("== Vaihe asemat ==")
  nuts3_fi <- gisco_get_nuts(year = "2021", nuts_level = 3, country = "FI",
                             resolution = "01", epsg = "4326") |>
    select(alue = NUTS_ID, alue_nimi = NAME_LATN)
  stopifnot(nrow(nuts3_fi) == 19L)

  fmi_kaikki <- fmi_asemaluettelo()
  # Säähavaintoasemat, jotka ovat yhä toiminnassa. Lopullinen karsinta tehdään
  # empiirisesti koehauilla, koska verkostonimi ei kerro sarjan pituutta.
  fmi_ehd <- fmi_kaikki |>
    filter(grepl("sää|weather", verkot, ignore.case = TRUE),
           is.na(loppu) | loppu >= LOPPU_PVM - 30,
           !is.na(lat), !is.na(lon)) |>
    st_as_sf(coords = c("lon", "lat"), crs = 4326, remove = FALSE) |>
    liita_alueeseen(nuts3_fi, "alue", "alue_nimi") |>
    st_drop_geometry() |>
    filter(!is.na(alue))
  message(sprintf("FMI-ehdokkaita %d.", nrow(fmi_ehd)))
  stopifnot(nrow(fmi_ehd) > 50)

  # Koehaut: vuodet 1985 ja 2025. Hyväksytään asema, jolla molempina vuosina
  # vähintään 300 päivää lämpötilahavaintoja. Haut menevät välimuistiin ja
  # käytetään uudelleen varsinaisessa latauksessa.
  koe <- function(id, v) {
    d <- fmi_paiva_vuosi(id, v)
    if (is.null(d)) NA_integer_ else sum(!is.na(d$tmin) & !is.na(d$tmax))
  }
  fmi_ehd <- fmi_ehd |>
    mutate(n1985 = map_int(fmisid, \(i) koe(i, 1985L), .progress = "Koehaku 1985"),
           n2025 = map_int(fmisid, \(i) koe(i, 2025L), .progress = "Koehaku 2025"))

  fmi_valitut <- fmi_ehd |>
    mutate(pitka = n1985 >= 300 & n2025 >= 300,
           nykyinen = n2025 >= 300) |>
    filter(nykyinen) |>
    group_by(alue) |>
    # Ensisijaisesti pitkät sarjat; jos maakunnassa alle 2 pitkää, täydennetään
    # aikaisimmin aloittaneilla nykyasemilla.
    arrange(desc(pitka), alku, .by_group = TRUE) |>
    slice_head(n = FMI_MAX_PER_MAAKUNTA) |>
    ungroup() |>
    transmute(lahde = "fmi", asema_id = fmisid, nimi, lat, lon, korkeus = NA_real_,
              maa = "FI", alue, alue_nimi, eka_vuosi = as.integer(year(alku)),
              pitka_sarja = pitka)
  print(count(fmi_valitut, alue, alue_nimi, pitka_sarja), n = Inf)
  stopifnot(n_distinct(fmi_valitut$alue) == 19L)

  ghcn_valitut <- ghcn_valitse_asemat(ghcn_meta()) |> mutate(pitka_sarja = eka_vuosi <= 1961L)
  print(count(ghcn_valitut, alue, alue_nimi), n = Inf)
  puuttuvat <- setdiff(GHCN_ALUEET$alue, ghcn_valitut$alue)
  if (length(puuttuvat)) stop("Ei GHCN-asemia alueille: ", paste(puuttuvat, collapse = ", "))

  asemat <- bind_rows(fmi_valitut, ghcn_valitut)
  stopifnot(!anyDuplicated(paste(asemat$lahde, asemat$asema_id)))
  tallenna_atom(asemat, asemat_polku)
  message(sprintf("Tallennettu %d asemaa.", nrow(asemat)))
}

# ===========================================================================
# VAIHE: fmi (päivähavainnot 1961–)
# ===========================================================================
if ("fmi" %in% vaiheet) {
  message("== Vaihe fmi ==")
  asemat <- qd_read(asemat_polku) |> filter(lahde == "fmi")
  vuodet <- ALKU_VUOSI:year(LOPPU_PVM)
  message(sprintf("%d asemaa x %d vuotta = enintään %d pyyntöä (välimuistissa olevat ohitetaan).",
                  nrow(asemat), length(vuodet), nrow(asemat) * length(vuodet)))

  for (a in split(asemat, asemat$alue)) {
    d <- map(seq_len(nrow(a)), function(i) {
      id <- a$asema_id[i]
      # Pitkät sarjat (koehaku 1985 onnistui) haetaan alusta. Muilta vain
      # aloitusvuodesta alkaen; metatieto voi olla epätarkka, joten 5 v varaa.
      vv <- if (isTRUE(a$pitka_sarja[i])) vuodet else
        vuodet[vuodet >= max(ALKU_VUOSI, coalesce(a$eka_vuosi[i], ALKU_VUOSI) - 5L)]
      map(vv, \(v) fmi_paiva_vuosi(id, v)) |>
        list_rbind() |>
        mutate(asema_id = id)
    }, .progress = a$alue_nimi[1]) |>
      list_rbind()
    if (!nrow(d)) { warning("Ei dataa: ", a$alue[1]); next }
    d <- d |>
      fmi_siivoa() |>
      mutate(lahde = "fmi", alue = a$alue[1]) |>
      select(lahde, alue, asema_id, pvm, tmin, tmax, tday, rr, lumi_cm)
    stopifnot(!anyDuplicated(d[c("asema_id", "pvm")]),
              all(d$tmin <= d$tmax + 0.5, na.rm = TRUE))
    tallenna_atom(d, file.path(TALVI_PAIVA, sprintf("fmi_%s.qs2", a$alue[1])))
  }
}

# ===========================================================================
# VAIHE: ghcn
# ===========================================================================
if ("ghcn" %in% vaiheet) {
  message("== Vaihe ghcn ==")
  asemat <- qd_read(asemat_polku) |> filter(lahde == "ghcn")

  # 1) Asemakohtaiset tiedostot
  asemadata <- map(asemat$asema_id, ghcn_asema, .progress = "GHCN-asemat") |>
    set_names(asemat$asema_id)
  viimeiset <- map(asemadata, \(x) if (is.null(x) || !nrow(x)) as.Date(NA) else max(x$pvm)) |>
    list_c()
  # Asemat, joiden asematiedosto loppuu ennen 2024, ovat oikeasti lopettaneet;
  # muiden häntä täydennetään vuositiedostoista.
  jatkuvat <- !is.na(viimeiset) & viimeiset >= as.Date("2024-01-01")
  message("Asematiedostojen viimeiset päivät (jatkuvat asemat):")
  print(summary(viimeiset[jatkuvat]))

  # 2) Häntä vuositiedostoista
  hanta <- NULL
  jaljessa <- jatkuvat & viimeiset < LOPPU_PVM - 30
  if (any(jaljessa)) {
    message(sprintf("%d asemaa yli 30 pv jäljessä; täydennetään vuositiedostoista:", sum(jaljessa)))
    print(tibble(asema_id = names(asemadata)[jaljessa], viimeinen = viimeiset[jaljessa]) |>
            left_join(asemat |> select(asema_id, nimi, alue), by = "asema_id") |>
            arrange(viimeinen), n = Inf)
    vuodet <- year(min(viimeiset[jaljessa])):year(LOPPU_PVM)
    hanta <- map(vuodet, \(v) ghcn_vuosi(v, names(asemadata)[jaljessa])) |> list_rbind()
    if (is.null(hanta) || !nrow(hanta)) hanta <- NULL
  }

  # 3) Yhdistys ja tallennus alueittain
  for (a in split(asemat, asemat$alue)) {
    d <- map(a$asema_id, function(id) {
      x <- asemadata[[id]]
      if (is.null(x) || !nrow(x)) return(NULL)
      if (!is.null(hanta)) {
        h <- hanta |> filter(id == !!id, pvm > max(x$pvm)) |> select(-id)
        x <- bind_rows(x, h)
      }
      if (!ghcn_yksikkotarkistus(x, id)) return(NULL)
      ghcn_siivoa(x) |> mutate(asema_id = id)
    }) |>
      list_rbind()
    if (is.null(d) || !nrow(d)) { warning("Ei dataa: ", a$alue[1]); next }
    d <- d |>
      filter(!if_all(c(tmin, tmax, rr, lumi_cm), is.na)) |>
      mutate(lahde = "ghcn", alue = a$alue[1]) |>
      select(lahde, alue, asema_id, pvm, tmin, tmax, tday, rr, lumi_cm)
    stopifnot(!anyDuplicated(d[c("asema_id", "pvm")]))
    message(sprintf("%s: viimeinen päivä %s", a$alue[1], max(d$pvm)))
    tallenna_atom(d, file.path(TALVI_PAIVA, sprintf("ghcn_%s.qs2", a$alue[1])))
  }
}

# ===========================================================================
# VAIHE: kausi (asema x talvikausi -indikaattorit)
# ===========================================================================
if ("kausi" %in% vaiheet) {
  message("== Vaihe kausi ==")
  asemat <- qd_read(asemat_polku)
  paiva <- lue_paiva()
  stopifnot(all(c("lahde", "alue", "asema_id", "pvm", "tmin", "tmax", "tday", "rr", "lumi_cm") %in% names(paiva)))

  kausi <- paiva |>
    mutate(kausi_alku = talvikausi_alku(pvm)) |>
    # Vain kokonaiset kaudet: ensimmäinen 1961-62, viimeinen päättynyt kausi
    filter(kausi_alku >= ALKU_VUOSI,
           as.Date(sprintf("%d-06-30", kausi_alku + 1L)) <= LOPPU_PVM) |>
    group_by(lahde, alue, asema_id, kausi_alku) |>
    group_modify(\(d, k) {
      # Täydennetään puuttuvat kalenteripäivät, jotta jaksot ja kattavuus ovat oikein
      kaikki <- seq(as.Date(sprintf("%d-07-01", k$kausi_alku)),
                    as.Date(sprintf("%d-06-30", k$kausi_alku + 1L)), by = "day")
      tibble(pvm = kaikki) |>
        left_join(d, by = "pvm") |>
        kauden_indikaattorit()
    }) |>
    ungroup() |>
    mutate(kausi = talvikausi_nimi(kausi_alku)) |>
    left_join(asemat |> select(lahde, asema_id, nimi, lat, lon, maa, alue_nimi),
              by = c("lahde", "asema_id"))

  stopifnot(!anyDuplicated(kausi[c("lahde", "asema_id", "kausi_alku")]),
            all(kausi$n_paivia %in% c(365L, 366L)),
            all(kausi$lumipaivat <= 213, na.rm = TRUE),
            all(kausi$fdd >= 0, na.rm = TRUE))
  message("Käyttökelpoisia asema-kausia (lämpötila ja lumi kattavia) alueittain:")
  print(kausi |>
          filter(kattavuus_t >= KATTAVUUS_RAJA, kattavuus_lumi >= KATTAVUUS_RAJA) |>
          count(maa, alue_nimi) |> arrange(maa, alue_nimi), n = 50)
  tallenna_atom(kausi, file.path(TALVI_DIR, "talvi_kausi.qs2"))
}

message("Valmis.")
