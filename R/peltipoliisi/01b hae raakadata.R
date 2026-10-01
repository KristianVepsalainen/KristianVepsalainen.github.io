# =============================================================================
# 01b-hae-raakadata.R
#
# Peltipoliisi-sarja, osa 1 jatkuu. Aja 01a-asemat-korjaus.R ensin.
#
# Tama korvaa alkuperaisen 01-hae-lam-data.R:n jaksot 2-5 kokonaan.
# Alkuperaista tiedostoa ei tarvitse editoida - taman voi ajaa sellaisenaan.
#
# Hakee:
#   2. LAM-raakadatan valituille pisteille ja paiville
#      -> tiivistetaan nopeushistogrammeiksi ja tuntitunnusluvuiksi
#   3. OSM: nopeusvalvontakameroiden sijainnit + tieosuuksien nopeusrajoitukset
#   4. Ilmatieteen laitos: sade ja lampotila (sekoittavat tekijat)
#   5. Kalenterimuuttujat
#
# Kesto: muutama tunti. Keskeytettavissa - uudelleenajo jatkaa samasta
# kohdasta, koska jokainen (piste, paiva) valimuistitetaan erikseen.
# =============================================================================

library(here)
library(tidyverse)
library(httr2)
library(qs2)
library(lubridate)
library(sf)

select <- dplyr::select
filter <- dplyr::filter

PROJEKTI  <- "peltipoliisi"
DATA_DIR  <- here("data", PROJEKTI)
CACHE_DIR <- file.path(DATA_DIR, "cache")
dir.create(CACHE_DIR, recursive = TRUE, showWarnings = FALSE)

DIGITRAFFIC_USER <- "kristianvepsalainen.com / peltipoliisi-analyysi"

# Havaintojakso. 2026 sisaltaa intervention (kameroiden paluu),
# 2025 on sama kalenterijakso verrokkivuotena.
JAKSO_2026 <- seq(as.Date("2026-04-15"), as.Date("2026-09-25"), by = "day")
JAKSO_2025 <- seq(as.Date("2025-04-15"), as.Date("2025-09-25"), by = "day")
PAIVAT <- c(JAKSO_2025, JAKSO_2026)

# Vapaan liikennevirran raja sekunteina: alle taman ajoneuvo on jonossa,
# jolloin nopeus kertoo liikennemaarasta eika kuljettajan valinnasta.
VAPAA_VIRTA_S <- 5

# Aikavali-kentan yksikko. Paatellaan empiirisesti alla - ala muuta kasin.
AIKAVALI_SKAALA <- 1000

# ----------------------------------------------------------------------------
# Pisteet ja asemat edellisesta vaiheesta
# ----------------------------------------------------------------------------

polku_pisteet <- file.path(DATA_DIR, "valitut_pisteet.qs2")
polku_asemat  <- file.path(DATA_DIR, "lam_asemat.qs2")

if (!file.exists(polku_pisteet) || !file.exists(polku_asemat)) {
  stop("Aja 01a-asemat-korjaus.R ensin - ", polku_pisteet, " puuttuu.")
}

PISTEET <- qd_read(polku_pisteet)$lam_id
asemat  <- qs_read(polku_asemat)

stopifnot(length(PISTEET) > 0, 116L %in% PISTEET)

message("Pisteita: ", length(PISTEET),
        " | paivia: ", length(PAIVAT),
        " | tiedostoja: ", length(PISTEET) * length(PAIVAT))

message("\n=== Haettavat pisteet ===")
print(asemat |>
        st_drop_geometry() |>
        filter(lam_id %in% PISTEET) |>
        select(lam_id, nimi, kunta, tienumero_lopullinen) |>
        arrange(tienumero_lopullinen, lam_id), n = 100)

# ----------------------------------------------------------------------------
# 2. LAM-RAAKADATAN HAKU JA TIIVISTYS
# ----------------------------------------------------------------------------

LAM_SARAKKEET <- c(
  "pistetunnus", "vuosi", "paiva", "tunti", "minuutti", "sekunti", "sadasosa",
  "pituus", "kaista", "suunta", "luokka", "nopeus", "faulty",
  "kokonaisaika", "aikavali", "jonoalku"
)

lamraw_url <- function(lam_id, pvm) {
  sprintf("https://tie.digitraffic.fi/api/tms/v1/history/raw/lamraw_%d_%d_%d.csv",
          lam_id, as.integer(format(pvm, "%y")), yday(pvm))
}

lataa_lamraw <- function(lam_id, pvm) {
  resp <- request(lamraw_url(lam_id, pvm)) |>
    req_headers("Digitraffic-User" = DIGITRAFFIC_USER) |>
    req_throttle(rate = 50 / 60) |>
    req_retry(max_tries = 3, backoff = \(i) 2^i) |>
    req_error(is_error = \(r) FALSE) |>
    req_perform()
  
  if (resp_status(resp) != 200) return(NULL)
  
  txt <- resp_body_string(resp)
  if (nchar(txt) < 200) return(NULL)
  
  # Dokumentaation mukaan otsikkorivia ei ole, mutta tarkistetaan.
  eka_rivi <- sub("\n.*$", "", txt)
  on_otsikko <- grepl("[A-Za-z]", eka_rivi)
  sarakkeita <- lengths(regmatches(eka_rivi, gregexpr(";", eka_rivi))) + 1L
  
  if (sarakkeita != length(LAM_SARAKKEET)) {
    warning(sprintf("Piste %d %s: %d saraketta (odotettu %d) - ohitetaan",
                    lam_id, pvm, sarakkeita, length(LAM_SARAKKEET)))
    return(NULL)
  }
  
  suppressWarnings(
    readr::read_delim(
      I(txt), delim = ";",
      col_names = LAM_SARAKKEET,
      col_types = readr::cols(.default = readr::col_double()),
      skip = if (on_otsikko) 1L else 0L,
      locale = readr::locale(decimal_mark = ".")
    )
  )
}

# Sisainen johdonmukaisuus + yksikkodiagnostiikka.
tarkista_lamraw <- function(d, lam_id, pvm) {
  stopifnot(is.data.frame(d), nrow(d) > 0)
  
  virheet <- character(0)
  if (!all(d$pistetunnus == lam_id))
    virheet <- c(virheet, "pistetunnus ei vastaa pyydettya pistetta")
  if (!all(d$vuosi == as.integer(format(pvm, "%y"))))
    virheet <- c(virheet, "vuosi ei vastaa pyydettya paivaa")
  if (!all(d$paiva == yday(pvm)))
    virheet <- c(virheet, "paivan jarjestysnumero ei vastaa pyydettya paivaa")
  if (length(virheet) > 0)
    stop(sprintf("Piste %d %s: %s", lam_id, pvm, paste(virheet, collapse = "; ")))
  
  sek_kellosta <- d$tunti * 3600 + d$minuutti * 60 + d$sekunti
  suhde_kokonaisaika <- median(d$kokonaisaika / pmax(sek_kellosta, 1), na.rm = TRUE)
  
  # Aikavalin yksikko: keskimaarainen aikavali kaistalla tunnissa pitaisi olla
  # noin 3600 / (ajoneuvoa tunnissa) sekuntia.
  ai <- d |>
    filter(faulty == 0, aikavali > 0) |>
    group_by(tunti, suunta, kaista) |>
    summarise(n = n(), ka_aikavali = mean(aikavali), .groups = "drop") |>
    filter(n >= 200) |>
    mutate(odotettu_s = 3600 / n, suhde = ka_aikavali / odotettu_s)
  
  tibble(
    lam_id = lam_id, pvm = pvm,
    n_rivia = nrow(d),
    osuus_faulty = mean(d$faulty != 0),
    tunteja_kattavuus = n_distinct(d$tunti[d$faulty == 0]),
    suhde_kokonaisaika = suhde_kokonaisaika,
    suhde_aikavali = if (nrow(ai) > 0) median(ai$suhde) else NA_real_
  )
}

tiivista_lamraw <- function(d, lam_id, pvm) {
  dd <- d |>
    filter(faulty == 0, nopeus >= 2, nopeus <= 198,
           suunta %in% c(1, 2), kaista >= 1, kaista <= 8,
           luokka >= 1, luokka <= 9) |>
    mutate(
      nopeus = as.integer(round(nopeus)),
      tunti  = as.integer(tunti),
      suunta = as.integer(suunta),
      kaista = as.integer(kaista),
      lk_ryhma = case_when(
        luokka == 1 ~ 1L,   # henkilo- ja pakettiautot
        luokka == 8 ~ 3L,   # moottoripyorat ja mopot
        TRUE        ~ 2L    # raskas kalusto ja peravaunulliset
      ),
      aikavali_s = aikavali / AIKAVALI_SKAALA,
      vapaa = as.integer(aikavali_s >= VAPAA_VIRTA_S)
    )
  
  if (nrow(dd) == 0) return(NULL)
  
  hist <- dd |>
    count(tunti, suunta, kaista, lk_ryhma, vapaa, nopeus, name = "n") |>
    mutate(lam_id = as.integer(lam_id), pvm = pvm) |>
    select(lam_id, pvm, tunti, suunta, kaista, lk_ryhma, vapaa, nopeus, n)
  
  tunti_yht <- dd |>
    group_by(tunti, suunta, kaista, lk_ryhma) |>
    summarise(
      n = n(),
      n_vapaa = sum(vapaa),
      ka_nopeus = mean(nopeus),
      kh_nopeus = sd(nopeus),
      p50 = quantile(nopeus, 0.50, names = FALSE),
      p85 = quantile(nopeus, 0.85, names = FALSE),
      p95 = quantile(nopeus, 0.95, names = FALSE),
      ka_nopeus_vapaa = if (sum(vapaa) >= 20) mean(nopeus[vapaa == 1]) else NA_real_,
      p85_vapaa = if (sum(vapaa) >= 20) quantile(nopeus[vapaa == 1], 0.85, names = FALSE) else NA_real_,
      med_aikavali_s = median(aikavali_s),
      ka_pituus = mean(pituus),
      .groups = "drop"
    ) |>
    mutate(lam_id = as.integer(lam_id), pvm = pvm) |>
    select(lam_id, pvm, everything())
  
  list(hist = hist, tunti = tunti_yht)
}

cache_polku <- function(lam_id, pvm) {
  file.path(CACHE_DIR, sprintf("agg_%d_%d_%03d.qs2",
                               lam_id, as.integer(format(pvm, "%y")), yday(pvm)))
}

kasittele_paiva <- function(lam_id, pvm) {
  polku <- cache_polku(lam_id, pvm)
  if (file.exists(polku)) return(invisible(NULL))
  
  raaka <- tryCatch(lataa_lamraw(lam_id, pvm), error = function(e) NULL)
  
  if (is.null(raaka) || nrow(raaka) == 0) {
    qs_save(list(status = "puuttuu", lam_id = lam_id, pvm = pvm), polku)
    return(invisible(NULL))
  }
  
  diag <- tarkista_lamraw(raaka, lam_id, pvm)
  tiiv <- tiivista_lamraw(raaka, lam_id, pvm)
  
  qs_save(list(status = "ok", lam_id = lam_id, pvm = pvm,
               diagnostiikka = diag, hist = tiiv$hist, tunti = tiiv$tunti),
          polku)
  invisible(NULL)
}

# --- Koeajo: yksikot varmistetaan ennen isoa latausta ------------------------

koe_pvm <- max(JAKSO_2026)
koe <- lataa_lamraw(116L, koe_pvm)

if (is.null(koe)) {
  # Kokeillaan muutamaa aiempaa paivaa - viimeisin ei aina ole viela julkaistu.
  for (d in as.list(rev(tail(JAKSO_2026, 10)))) {
    koe <- lataa_lamraw(116L, d)
    if (!is.null(koe)) { koe_pvm <- d; break }
  }
}
stopifnot(!is.null(koe))

message("\n=== Koeajo: piste 116, ", koe_pvm, " ===")
koe_diag <- tarkista_lamraw(koe, 116L, koe_pvm)
print(koe_diag)
print(head(koe, 3))

if (!is.na(koe_diag$suhde_aikavali)) {
  suhde <- koe_diag$suhde_aikavali / AIKAVALI_SKAALA
  message(sprintf("Aikavalin yksikkotarkistus: havaittu/odotettu = %.2f", suhde))
  if (suhde < 0.33 || suhde > 3) {
    stop(sprintf(paste0(
      "Aikavali-kentan yksikko ei vastaa oletusta (AIKAVALI_SKAALA = %d).\n",
      "Havaittu keskimaarainen aikavali / odotettu = %.1f.\n",
      "Korjaa AIKAVALI_SKAALA arvoon noin %.0f ja aja uudelleen."),
      AIKAVALI_SKAALA, koe_diag$suhde_aikavali, koe_diag$suhde_aikavali))
  }
}

# --- Varsinainen lataus ------------------------------------------------------

grid <- expand_grid(lam_id = PISTEET, pvm = PAIVAT)

valmiina <- sum(file.exists(map2_chr(grid$lam_id, grid$pvm, cache_polku)))
message("\nLataus alkaa. Valmiina jo: ", valmiina, " / ", nrow(grid))

pwalk(grid, kasittele_paiva, .progress = TRUE)

# --- Valimuistin kokoaminen --------------------------------------------------

odotetut <- map2_chr(grid$lam_id, grid$pvm, cache_polku)
puuttuu_levylta <- odotetut[!file.exists(odotetut)]
if (length(puuttuu_levylta) > 0) {
  stop(length(puuttuu_levylta), " valimuistitiedostoa puuttuu - ",
       "lataus keskeytyi. Aja skripti uudelleen, se jatkaa samasta kohdasta.")
}

palat <- map(odotetut, qs_read, .progress = TRUE)

puuttuvat <- palat |> keep(\(x) x$status == "puuttuu") |>
  map_dfr(\(x) tibble(lam_id = x$lam_id, pvm = x$pvm))
ok <- palat |> keep(\(x) x$status == "ok")

stopifnot(length(ok) > 0)

lam_hist  <- map_dfr(ok, "hist")
lam_tunti <- map_dfr(ok, "tunti")
lam_diag  <- map_dfr(ok, "diagnostiikka")

# TARKISTUS: avaimet ovat yksikasitteisia
stopifnot(
  !any(duplicated(lam_hist[c("lam_id","pvm","tunti","suunta","kaista","lk_ryhma","vapaa","nopeus")])),
  !any(duplicated(lam_tunti[c("lam_id","pvm","tunti","suunta","kaista","lk_ryhma")]))
)

# TARKISTUS: histogrammin ja tuntitaulun havaintomaarat tasmaavat
tark <- full_join(
  lam_hist |> group_by(lam_id, pvm, tunti, suunta, kaista, lk_ryhma) |>
    summarise(n_hist = sum(n), .groups = "drop"),
  lam_tunti |> select(lam_id, pvm, tunti, suunta, kaista, lk_ryhma, n_tunti = n),
  by = c("lam_id","pvm","tunti","suunta","kaista","lk_ryhma")
)
if (!all(tark$n_hist == tark$n_tunti) || any(is.na(tark$n_hist)) || any(is.na(tark$n_tunti))) {
  print(tark |> filter(is.na(n_hist) | is.na(n_tunti) | n_hist != n_tunti) |> head(20))
  stop("Histogrammin ja tuntitaulun havaintomaarat eivat tasmaa.")
}

message("\nHistogrammirivaja: ", nrow(lam_hist),
        " | tuntirivaja: ", nrow(lam_tunti),
        " | puuttuvia paivia: ", nrow(puuttuvat))

qd_save(lam_hist,  file.path(DATA_DIR, "lam_hist.qs2"))
qd_save(lam_tunti, file.path(DATA_DIR, "lam_tunti.qs2"))
qd_save(lam_diag,  file.path(DATA_DIR, "lam_diagnostiikka.qs2"))
qd_save(puuttuvat, file.path(DATA_DIR, "lam_puuttuvat.qs2"))

# --- Kattavuusraportti: katso tama ennen analyysia ---------------------------

kattavuus <- lam_tunti |>
  mutate(vuosi = year(pvm)) |>
  group_by(lam_id, vuosi) |>
  summarise(paivia = n_distinct(pvm),
            ajoneuvoa_pv = round(sum(n) / n_distinct(pvm)),
            .groups = "drop") |>
  pivot_wider(names_from = vuosi, values_from = c(paivia, ajoneuvoa_pv)) |>
  left_join(asemat |> st_drop_geometry() |>
              select(lam_id, nimi, tienumero_lopullinen), by = "lam_id")

message("\n=== Kattavuus pisteittain ===")
print(kattavuus, n = 100)

vajaat_paivat <- lam_tunti |>
  group_by(lam_id, pvm) |>
  summarise(tunteja = n_distinct(tunti), .groups = "drop") |>
  filter(tunteja < 24)

message("Vajaita paivia (alle 24 h dataa): ", nrow(vajaat_paivat))
qd_save(vajaat_paivat, file.path(DATA_DIR, "lam_vajaat_paivat.qs2"))

# ----------------------------------------------------------------------------
# 3. NOPEUSVALVONTAKAMERAT JA NOPEUSRAJOITUKSET (OpenStreetMap)
# ----------------------------------------------------------------------------
# Vain sf-pohjaista vektoridataa. Ei terra-pakettia.

hae_osm <- function() {
  polku <- file.path(DATA_DIR, "osm_kamerat_tiet.qs2")
  if (file.exists(polku)) return(qs_read(polku))
  
  if (!requireNamespace("osmextract", quietly = TRUE))
    stop("Asenna osmextract: install.packages('osmextract')")
  
  alue <- "Uusimaa"
  
  kamerat <- osmextract::oe_get(
    alue, layer = "points",
    extra_tags = c("highway", "maxspeed", "direction"),
    quiet = FALSE) |>
    filter(highway == "speed_camera") |>
    st_transform(3067)
  
  tiet <- osmextract::oe_get(
    alue, layer = "lines",
    extra_tags = c("highway", "maxspeed", "ref", "lanes"),
    quiet = FALSE) |>
    filter(highway %in% c("motorway", "trunk", "primary", "motorway_link")) |>
    st_transform(3067)
  
  if (nrow(kamerat) < 5)
    stop("OSM:sta loytyi vain ", nrow(kamerat), " nopeusvalvontakameraa. ",
         "Tarkista alueen nimi ('", alue, "') ennen jatkoa.")
  
  message("OSM-kameroita: ", nrow(kamerat), " | tielinjoja: ", nrow(tiet))
  qs_save(list(kamerat = kamerat, tiet = tiet), polku)
  list(kamerat = kamerat, tiet = tiet)
}

osm <- hae_osm()

asemat_analyysi <- asemat |> filter(lam_id %in% PISTEET)
idx_kamera <- st_nearest_feature(asemat_analyysi, osm$kamerat)
idx_tie    <- st_nearest_feature(asemat_analyysi, osm$tiet)

piste_konteksti <- asemat_analyysi |>
  st_drop_geometry() |>
  mutate(
    etaisyys_kameraan_m = as.numeric(st_distance(
      asemat_analyysi, osm$kamerat[idx_kamera, ], by_element = TRUE)),
    etaisyys_tiehen_m = as.numeric(st_distance(
      asemat_analyysi, osm$tiet[idx_tie, ], by_element = TRUE)),
    osm_maxspeed = osm$tiet$maxspeed[idx_tie],
    osm_ref      = osm$tiet$ref[idx_tie]
  )

epavarmat <- piste_konteksti |> filter(etaisyys_tiehen_m > 50)
if (nrow(epavarmat) > 0)
  warning("Nailla pisteilla nopeusrajoituksen liitos on epavarma: ",
          paste(epavarmat$lam_id, collapse = ", "))

message("\n=== Pisteiden etaisyys lahimpaan kameraan ===")
print(piste_konteksti |>
        select(lam_id, nimi, osm_ref, osm_maxspeed,
               etaisyys_kameraan_m, etaisyys_tiehen_m) |>
        arrange(etaisyys_kameraan_m), n = 100)

qd_save(piste_konteksti, file.path(DATA_DIR, "piste_konteksti.qs2"))

# ----------------------------------------------------------------------------
# 4. SAA
# ----------------------------------------------------------------------------

hae_saa <- function() {
  polku <- file.path(DATA_DIR, "saa.qs2")
  if (file.exists(polku)) return(qd_read(polku))
  
  if (!requireNamespace("xml2", quietly = TRUE))
    stop("Asenna xml2: install.packages('xml2')")
  
  kuukaudet <- unique(floor_date(PAIVAT, "month"))
  
  hae_kk <- function(alku) {
    loppu <- min(ceiling_date(alku, "month") - days(1), max(PAIVAT))
    url <- paste0(
      "https://opendata.fmi.fi/wfs?service=WFS&version=2.0.0",
      "&request=getFeature",
      "&storedquery_id=fmi::observations::weather::daily::simple",
      "&place=Helsinki",
      "&starttime=", format(alku, "%Y-%m-%d"), "T00:00:00Z",
      "&endtime=", format(loppu, "%Y-%m-%d"), "T23:59:59Z",
      "&parameters=rrday,tday")
    
    resp <- request(url) |> req_retry(max_tries = 3) |> req_perform()
    xml <- xml2::read_xml(resp_body_string(resp))
    
    # Nimiavaruudesta riippumaton haku: elementin paikallinen nimi.
    elems <- xml2::xml_find_all(xml, "//*[local-name()='BsWfsElement']")
    if (length(elems) == 0) return(tibble())
    
    kentta <- function(nimi)
      xml2::xml_text(xml2::xml_find_first(
        elems, paste0(".//*[local-name()='", nimi, "']")))
    
    tibble(aika = kentta("Time"),
           muuttuja = kentta("ParameterName"),
           arvo = suppressWarnings(as.numeric(kentta("ParameterValue"))))
  }
  
  raaka <- map_dfr(kuukaudet, hae_kk, .progress = TRUE)
  stopifnot(nrow(raaka) > 0)
  
  saa <- raaka |>
    mutate(pvm = as.Date(aika)) |>
    select(pvm, muuttuja, arvo) |>
    pivot_wider(names_from = muuttuja, values_from = arvo, values_fn = mean)
  
  # FMI koodaa puuttuvan arvon -1:lla.
  if ("rrday" %in% names(saa))
    saa <- saa |> rename(sade_mm = rrday) |>
    mutate(sade_mm = if_else(sade_mm < 0, NA_real_, sade_mm))
  if ("tday" %in% names(saa))
    saa <- saa |> rename(lampotila_c = tday)
  
  if (!all(c("sade_mm", "lampotila_c") %in% names(saa))) {
    print(names(saa))
    stop("FMI palautti odottamattomat muuttujat - katso yllä oleva lista.")
  }
  
  qd_save(saa, polku)
  saa
}

saa <- hae_saa()
message("\nSaahavaintopaivia: ", nrow(saa),
        " | puuttuvia sadearvoja: ", sum(is.na(saa$sade_mm)))

# ----------------------------------------------------------------------------
# 5. KALENTERI
# ----------------------------------------------------------------------------

arkipyhat <- as.Date(c(
  "2025-05-01", "2025-05-29", "2025-06-20", "2025-06-21",
  "2026-05-01", "2026-05-14", "2026-06-19", "2026-06-20"
))

kalenteri <- tibble(pvm = PAIVAT) |>
  mutate(
    vuosi = year(pvm),
    viikonpaiva = wday(pvm, week_start = 1),
    arkipaiva = viikonpaiva <= 5,
    arkipyha = pvm %in% arkipyhat,
    kesaloma = pvm >= as.Date(paste0(vuosi, "-06-01")) &
      pvm <= as.Date(paste0(vuosi, "-08-09")),
    viikko = isoweek(pvm)
  ) |>
  left_join(saa, by = "pvm")

stopifnot(nrow(kalenteri) == length(PAIVAT), !any(duplicated(kalenteri$pvm)))
qd_save(kalenteri, file.path(DATA_DIR, "kalenteri.qs2"))

# ----------------------------------------------------------------------------
# YHTEENVETO
# ----------------------------------------------------------------------------

message("\n=== Valmiit aineistot: ", DATA_DIR, " ===")
for (f in c("lam_hist.qs2", "lam_tunti.qs2", "lam_diagnostiikka.qs2",
            "lam_puuttuvat.qs2", "lam_vajaat_paivat.qs2",
            "piste_konteksti.qs2", "kalenteri.qs2", "lam_asemat.qs2",
            "osm_kamerat_tiet.qs2")) {
  p <- file.path(DATA_DIR, f)
  if (file.exists(p))
    message(sprintf("  %-24s %6.1f MB", f, file.size(p) / 1024^2))
}