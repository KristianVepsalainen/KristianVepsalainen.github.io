# =============================================================================
# 01-hae-lam-data.R
#
# Peltipoliisi-sarja: kaiken sarjan tarvitseman datan haku ja tiivistys.
#
# Hakee:
#   1. LAM-asemien metatiedot (Digitraffic)
#   2. LAM-raakadatan (ajoneuvokohtaiset ohitukset) valituille pisteille
#      ja päiville -> tiivistetään nopeushistogrammeiksi ja tuntisummiksi
#   3. OSM:sta nopeusvalvontakameroiden sijainnit + tieosuuksien maxspeed
#   4. Ilmatieteen laitokselta sää (sekoittava tekijä)
#   5. Kalenterimuuttujat (arkipyhät, koulujen loma-ajat)
#
# Tulokset: here("data", "peltipoliisi", ...)
#
# HUOM: tämä skripti on tarkoitettu ajettavaksi kerran (yön yli). Se on
# keskeytettävissä ja jatkettavissa: jokainen (piste, päivä) -pari
# välimuistitetaan erikseen, ja uudelleenajo hyppää valmiiden yli.
# =============================================================================

library(here)
library(tidyverse)
library(httr2)
library(qs2)
library(lubridate)
library(sf)

select <- dplyr::select
filter <- dplyr::filter

# ----------------------------------------------------------------------------
# KONFIGURAATIO
# ----------------------------------------------------------------------------

PROJEKTI <- "peltipoliisi"
DATA_DIR <- here("data", PROJEKTI)
CACHE_DIR <- file.path(DATA_DIR, "cache")
dir.create(CACHE_DIR, recursive = TRUE, showWarnings = FALSE)

# Havaintojakso. 2026: interventio (kameroiden paluu) sijoittuu tähän väliin.
# 2025: sama kalenterijakso verrokkivuotena kausivaihtelun erottamiseksi.
JAKSO_2026 <- seq(as.Date("2026-04-15"), as.Date("2026-09-10"), by = "day")
JAKSO_2025 <- seq(as.Date("2025-04-15"), as.Date("2025-09-10"), by = "day")
PAIVAT <- c(JAKSO_2025, JAKSO_2026)

# Käsiteltävät LAM-pisteet täydennetään asemametatietojen perusteella alla.
# Tähän listaan tulevat pisteet, jotka halutaan mukaan joka tapauksessa.
PAKOLLISET_PISTEET <- c(116L)   # Leppäsolmu, Kehä I (haasteen kohde)

# Tiet, joilta pisteet poimitaan.
#   101 = Kehä I (käsiteltävä tie)
#    50 = Kehä III, 51 = Länsiväylä, 45 = Tuusulanväylä, 4 = Lahdenväylä,
#     7 = Porvoonväylä, 3 = Hämeenlinnanväylä  -> verrokit
TIET_KASITELTAVA <- c(101L)
TIET_VERROKKI <- c(50L, 51L, 45L, 4L, 7L, 3L)

# Kuinka monta verrokkipistettä per tie enintään (rajaa latausmäärää).
VERROKKEJA_PER_TIE <- 2L

# Digitraffic pyytää tunnistautumaan otsakkeella. Muuta omaksesi.
DIGITRAFFIC_USER <- "kristianvepsalainen.com / peltipoliisi-analyysi"

# Aikaväli-kentän oletettu yksikkö (millisekunti). Tarkistetaan empiirisesti.
AIKAVALI_SKAALA <- 1000   # jakajana: aikavali / skaala = sekuntia

# Vapaan liikennevirran raja sekunteina (aikaväli edelliseen ajoneuvoon).
# Alle tämän ajoneuvo on jonossa, jolloin nopeus kertoo liikennemäärästä,
# ei kuljettajan valinnasta.
VAPAA_VIRTA_S <- 5

# ----------------------------------------------------------------------------
# 1. LAM-ASEMIEN METATIEDOT
# ----------------------------------------------------------------------------

hae_asemat <- function() {
  polku <- file.path(DATA_DIR, "lam_asemat.qs2")
  if (file.exists(polku)) return(qs_read(polku))

  resp <- request("https://tie.digitraffic.fi/api/tms/v1/stations") |>
    req_headers("Digitraffic-User" = DIGITRAFFIC_USER) |>
    req_retry(max_tries = 4) |>
    req_perform()

  stopifnot(resp_status(resp) == 200)
  js <- resp_body_json(resp, check_type = FALSE)

  stopifnot("features" %in% names(js), length(js$features) > 100)

  asemat <- map_dfr(js$features, function(f) {
    p <- f$properties
    koord <- f$geometry$coordinates
    tibble(
      lam_id     = as.integer(p$tmsNumber %||% NA),
      asema_id   = as.integer(p$id %||% NA),
      nimi       = as.character(p$name %||% NA),
      kunta      = as.character(p$municipality %||% NA),
      tienumero  = as.integer(p$roadAddress$roadNumber %||% NA),
      tieosa     = as.integer(p$roadAddress$roadSection %||% NA),
      etaisyys   = as.integer(p$roadAddress$distance %||% NA),
      suunta1    = as.character(p$direction1Municipality %||% NA),
      suunta2    = as.character(p$direction2Municipality %||% NA),
      kerayksessa = as.character(p$collectionStatus %||% NA),
      lon        = as.numeric(koord[[1]]),
      lat        = as.numeric(koord[[2]])
    )
  })

  # TARKISTUS: pakolliset kentät eivät saa olla tyhjiä
  stopifnot(
    nrow(asemat) > 100,
    !any(is.na(asemat$lam_id)),
    !any(is.na(asemat$lon)), !any(is.na(asemat$lat))
  )
  if (all(is.na(asemat$tienumero))) {
    stop("Tienumero puuttuu kaikilta asemilta - rajapinnan rakenne on muuttunut. ",
         "Tarkista js$features[[1]]$properties käsin ennen jatkamista.")
  }

  asemat_sf <- st_as_sf(asemat, coords = c("lon", "lat"), crs = 4326, remove = FALSE) |>
    st_transform(3067)

  qs_save(asemat_sf, polku)
  asemat_sf
}

asemat <- hae_asemat()

message("Asemia yhteensä: ", nrow(asemat))
message("Kehä I:n (tie 101) pisteet:")
asemat |>
  st_drop_geometry() |>
  filter(tienumero %in% TIET_KASITELTAVA) |>
  arrange(tieosa, etaisyys) |>
  select(lam_id, nimi, kunta, tieosa, etaisyys, kerayksessa) |>
  print(n = 100)

# Pistevalinta: kaikki Kehä I:n pisteet + rajattu joukko verrokkeja.
pisteet_kasiteltava <- asemat |>
  st_drop_geometry() |>
  filter(tienumero %in% TIET_KASITELTAVA) |>
  pull(lam_id)

pisteet_verrokki <- asemat |>
  st_drop_geometry() |>
  filter(tienumero %in% TIET_VERROKKI) |>
  group_by(tienumero) |>
  arrange(tieosa, etaisyys, .by_group = TRUE) |>
  slice_head(n = VERROKKEJA_PER_TIE) |>
  ungroup() |>
  pull(lam_id)

PISTEET <- sort(unique(c(PAKOLLISET_PISTEET, pisteet_kasiteltava, pisteet_verrokki)))

# TARKISTUS: haasteen kohdepiste on mukana ja olemassa
stopifnot(116L %in% PISTEET, 116L %in% asemat$lam_id)

message("Haettavia pisteitä: ", length(PISTEET),
        " | päiviä: ", length(PAIVAT),
        " | tiedostoja: ", length(PISTEET) * length(PAIVAT))

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

# Lataa yksi (piste, päivä) -tiedosto. Palauttaa NULL jos ei saatavilla.
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

  # TARKISTUS: onko tiedostossa otsikkorivi (dokumentaation mukaan ei ole)
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
      I(txt),
      delim = ";",
      col_names = LAM_SARAKKEET,
      col_types = readr::cols(.default = readr::col_double()),
      skip = if (on_otsikko) 1L else 0L,
      locale = readr::locale(decimal_mark = ".")
    )
  )
}

# Tarkistaa raakadatan sisäisen johdonmukaisuuden ja palauttaa diagnostiikan.
tarkista_lamraw <- function(d, lam_id, pvm) {
  stopifnot(is.data.frame(d), nrow(d) > 0)

  virheet <- character(0)

  if (!all(d$pistetunnus == lam_id)) {
    virheet <- c(virheet, "pistetunnus ei vastaa pyydettyä pistettä")
  }
  if (!all(d$vuosi == as.integer(format(pvm, "%y")))) {
    virheet <- c(virheet, "vuosi ei vastaa pyydettyä päivää")
  }
  if (!all(d$paiva == yday(pvm))) {
    virheet <- c(virheet, "päivän järjestysnumero ei vastaa pyydettyä päivää")
  }
  if (length(virheet) > 0) {
    stop(sprintf("Piste %d %s: %s", lam_id, pvm, paste(virheet, collapse = "; ")))
  }

  # Kellonajan ja kokonaisaika-kentän yhteensopivuus -> paljastaa yksikön
  sek_kellosta <- d$tunti * 3600 + d$minuutti * 60 + d$sekunti
  suhde_kokonaisaika <- median(d$kokonaisaika / pmax(sek_kellosta, 1), na.rm = TRUE)

  # Aikavälin yksikön johtaminen: keskimääräinen aikaväli kaistalla tunnissa
  # pitäisi olla noin 3600 / (ajoneuvoa tunnissa kaistalla) sekuntia.
  ai <- d |>
    filter(faulty == 0, aikavali > 0) |>
    group_by(tunti, suunta, kaista) |>
    summarise(n = n(), ka_aikavali = mean(aikavali), .groups = "drop") |>
    filter(n >= 200) |>
    mutate(odotettu_s = 3600 / n, suhde = ka_aikavali / odotettu_s)

  suhde_aikavali <- if (nrow(ai) > 0) median(ai$suhde) else NA_real_

  tibble(
    lam_id = lam_id, pvm = pvm,
    n_rivia = nrow(d),
    osuus_faulty = mean(d$faulty != 0),
    tunteja_kattavuus = n_distinct(d$tunti[d$faulty == 0]),
    suhde_kokonaisaika = suhde_kokonaisaika,
    suhde_aikavali = suhde_aikavali
  )
}

# Tiivistää yhden päivän raakadatan: histogrammi + tuntisummat.
tiivista_lamraw <- function(d, lam_id, pvm) {
  dd <- d |>
    filter(
      faulty == 0,
      nopeus >= 2, nopeus <= 198,
      suunta %in% c(1, 2),
      kaista >= 1, kaista <= 8,
      luokka >= 1, luokka <= 9
    ) |>
    mutate(
      nopeus = as.integer(round(nopeus)),
      tunti = as.integer(tunti),
      suunta = as.integer(suunta),
      kaista = as.integer(kaista),
      lk_ryhma = case_when(
        luokka == 1 ~ 1L,           # henkilö- ja pakettiautot
        luokka == 8 ~ 3L,           # moottoripyörät ja mopot
        TRUE ~ 2L                   # raskas kalusto ja perävaunulliset
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
      ka_nopeus_vapaa = mean(nopeus[vapaa == 1]),
      p85_vapaa = if (sum(vapaa) >= 20) quantile(nopeus[vapaa == 1], 0.85, names = FALSE) else NA_real_,
      med_aikavali_s = median(aikavali_s),
      ka_pituus = mean(pituus),
      .groups = "drop"
    ) |>
    mutate(lam_id = as.integer(lam_id), pvm = pvm) |>
    select(lam_id, pvm, everything())

  list(hist = hist, tunti = tunti_yht)
}

# Yhden (piste, päivä) -parin käsittely välimuistilla.
kasittele_paiva <- function(lam_id, pvm) {
  avain <- sprintf("agg_%d_%d_%03d.qs2", lam_id, as.integer(format(pvm, "%y")), yday(pvm))
  polku <- file.path(CACHE_DIR, avain)
  if (file.exists(polku)) return(invisible(NULL))

  raaka <- tryCatch(lataa_lamraw(lam_id, pvm), error = function(e) NULL)

  if (is.null(raaka) || nrow(raaka) == 0) {
    qs_save(list(status = "puuttuu", lam_id = lam_id, pvm = pvm), polku)
    return(invisible(NULL))
  }

  diag <- tarkista_lamraw(raaka, lam_id, pvm)
  tiiv <- tiivista_lamraw(raaka, lam_id, pvm)

  qs_save(
    list(status = "ok", lam_id = lam_id, pvm = pvm,
         diagnostiikka = diag, hist = tiiv$hist, tunti = tiiv$tunti),
    polku
  )
  invisible(NULL)
}

# --- Koeajo yhdellä tiedostolla: yksiköiden varmistus ennen isoa latausta ----

koe <- lataa_lamraw(116L, as.Date("2026-09-04"))
stopifnot(!is.null(koe))
koe_diag <- tarkista_lamraw(koe, 116L, as.Date("2026-09-04"))
print(koe_diag)

# TARKISTUS: aikaväli-kentän yksikkö. Jos oletettu skaala on oikea, suhteen
# pitää olla suuruusluokkaa AIKAVALI_SKAALA.
if (!is.na(koe_diag$suhde_aikavali)) {
  suhde <- koe_diag$suhde_aikavali / AIKAVALI_SKAALA
  message(sprintf("Aikaväli-kentän yksikkötarkistus: suhde odotettuun = %.2f", suhde))
  if (suhde < 0.33 || suhde > 3) {
    stop(sprintf(
      paste0("Aikaväli-kentän yksikkö ei vastaa oletusta (AIKAVALI_SKAALA = %d).\n",
             "Havaittu keskimääräinen aikaväli / odotettu aikaväli = %.1f.\n",
             "Korjaa AIKAVALI_SKAALA arvoon ~%.0f ja aja uudelleen."),
      AIKAVALI_SKAALA, koe_diag$suhde_aikavali, koe_diag$suhde_aikavali))
  }
}

# --- Varsinainen latauskierros ----------------------------------------------

grid <- expand_grid(lam_id = PISTEET, pvm = PAIVAT)

message("Aloitetaan lataus: ", nrow(grid), " tiedostoa. ",
        "Keskeytettävissä, jatkuu samasta kohdasta.")

pwalk(grid, function(lam_id, pvm) {
  kasittele_paiva(lam_id, pvm)
}, .progress = TRUE)

# --- Välimuistin kokoaminen --------------------------------------------------

tiedostot <- list.files(CACHE_DIR, pattern = "^agg_.*\\.qs2$", full.names = TRUE)
stopifnot(length(tiedostot) == nrow(grid))

palat <- map(tiedostot, qs_read, .progress = TRUE)

puuttuvat <- palat |>
  keep(\(x) x$status == "puuttuu") |>
  map_dfr(\(x) tibble(lam_id = x$lam_id, pvm = x$pvm))

ok <- palat |> keep(\(x) x$status == "ok")

lam_hist <- map_dfr(ok, "hist")
lam_tunti <- map_dfr(ok, "tunti")
lam_diag <- map_dfr(ok, "diagnostiikka")

# TARKISTUS: ei duplikaatteja avainten tasolla
stopifnot(
  !any(duplicated(lam_hist[c("lam_id", "pvm", "tunti", "suunta", "kaista",
                             "lk_ryhma", "vapaa", "nopeus")])),
  !any(duplicated(lam_tunti[c("lam_id", "pvm", "tunti", "suunta", "kaista", "lk_ryhma")]))
)

# TARKISTUS: histogrammin ja tuntisumman havaintomäärät täsmäävät
tark <- full_join(
  lam_hist |> group_by(lam_id, pvm, tunti, suunta, kaista, lk_ryhma) |>
    summarise(n_hist = sum(n), .groups = "drop"),
  lam_tunti |> select(lam_id, pvm, tunti, suunta, kaista, lk_ryhma, n_tunti = n),
  by = c("lam_id", "pvm", "tunti", "suunta", "kaista", "lk_ryhma")
)
stopifnot(all(tark$n_hist == tark$n_tunti, na.rm = FALSE))

message("Histogrammirivejä: ", nrow(lam_hist),
        " | tuntirivejä: ", nrow(lam_tunti),
        " | puuttuvia päiviä: ", nrow(puuttuvat))

qd_save(lam_hist, file.path(DATA_DIR, "lam_hist.qs2"))
qd_save(lam_tunti, file.path(DATA_DIR, "lam_tunti.qs2"))
qd_save(lam_diag, file.path(DATA_DIR, "lam_diagnostiikka.qs2"))
qd_save(puuttuvat, file.path(DATA_DIR, "lam_puuttuvat.qs2"))

# --- Kattavuusraportti: katso tämä ennen analyysiä ---------------------------

kattavuus <- lam_tunti |>
  group_by(lam_id, vuosi = year(pvm)) |>
  summarise(
    paivia = n_distinct(pvm),
    ka_ajoneuvoa_pv = sum(n) / n_distinct(pvm),
    .groups = "drop"
  ) |>
  pivot_wider(names_from = vuosi, values_from = c(paivia, ka_ajoneuvoa_pv)) |>
  left_join(st_drop_geometry(asemat) |> select(lam_id, nimi, tienumero), by = "lam_id")

print(kattavuus, n = 100)

# Osittaiset päivät (anturikatko) vääristävät päivätason tunnuslukuja.
vajaat_paivat <- lam_tunti |>
  group_by(lam_id, pvm) |>
  summarise(tunteja = n_distinct(tunti), .groups = "drop") |>
  filter(tunteja < 24)

message("Vajaita päiviä (alle 24 tuntia dataa): ", nrow(vajaat_paivat))
qd_save(vajaat_paivat, file.path(DATA_DIR, "lam_vajaat_paivat.qs2"))

# ----------------------------------------------------------------------------
# 3. NOPEUSVALVONTAKAMERAT JA NOPEUSRAJOITUKSET (OpenStreetMap)
# ----------------------------------------------------------------------------
# Huom: ei terra-pakettia. Kaikki vektorimuotoista sf:ää.

hae_osm <- function() {
  polku <- file.path(DATA_DIR, "osm_kamerat_tiet.qs2")
  if (file.exists(polku)) return(qs_read(polku))

  if (!requireNamespace("osmextract", quietly = TRUE)) {
    stop("Asenna osmextract.")
  }

  # Uudenmaan alue riittää kattamaan Kehä I:n ja verrokkitiet.
  kamerat <- osmextract::oe_get(
    "Helsinki", layer = "points",
    extra_tags = c("highway", "maxspeed", "direction"),
    quiet = FALSE
  ) |>
    filter(highway == "speed_camera") |>
    st_transform(3067)

  tiet <- osmextract::oe_get(
    "Helsinki", layer = "lines",
    extra_tags = c("highway", "maxspeed", "ref", "lanes"),
    quiet = FALSE
  ) |>
    filter(highway %in% c("motorway", "trunk", "primary", "motorway_link")) |>
    st_transform(3067)

  # TARKISTUS: kameroita pitää löytyä. Jos ei löydy, tagi tai alue on väärä.
  if (nrow(kamerat) < 5) {
    stop("OSM:sta löytyi alle 5 nopeusvalvontakameraa. ",
         "Tarkista alueen nimi ja highway=speed_camera -tagitus ennen jatkoa.")
  }
  message("OSM-kameroita: ", nrow(kamerat), " | tielinjoja: ", nrow(tiet))

  qs_save(list(kamerat = kamerat, tiet = tiet), polku)
  list(kamerat = kamerat, tiet = tiet)
}

osm <- hae_osm()

asemat_analyysi <- asemat |> filter(lam_id %in% PISTEET)

# Etäisyys lähimpään kameraan (metreinä) ja lähimmän tielinjan nopeusrajoitus.
idx_kamera <- st_nearest_feature(asemat_analyysi, osm$kamerat)
idx_tie <- st_nearest_feature(asemat_analyysi, osm$tiet)

piste_konteksti <- asemat_analyysi |>
  st_drop_geometry() |>
  mutate(
    etaisyys_kameraan_m = as.numeric(st_distance(
      asemat_analyysi, osm$kamerat[idx_kamera, ], by_element = TRUE)),
    etaisyys_tiehen_m = as.numeric(st_distance(
      asemat_analyysi, osm$tiet[idx_tie, ], by_element = TRUE)),
    osm_maxspeed = osm$tiet$maxspeed[idx_tie],
    osm_ref = osm$tiet$ref[idx_tie]
  )

# TARKISTUS: jos LAM-piste on yli 50 m päässä lähimmästä tielinjasta,
# rajoituksen liittäminen on epäluotettavaa.
epavarmat <- piste_konteksti |> filter(etaisyys_tiehen_m > 50)
if (nrow(epavarmat) > 0) {
  warning("Näillä pisteillä nopeusrajoituksen liitos on epävarma: ",
          paste(epavarmat$lam_id, collapse = ", "))
}

print(piste_konteksti |>
        select(lam_id, nimi, tienumero, osm_ref, osm_maxspeed,
               etaisyys_kameraan_m, etaisyys_tiehen_m) |>
        arrange(etaisyys_kameraan_m), n = 100)

qd_save(piste_konteksti, file.path(DATA_DIR, "piste_konteksti.qs2"))

# ----------------------------------------------------------------------------
# 4. SÄÄ (sekoittava tekijä: sade ja lämpötila laskevat nopeuksia)
# ----------------------------------------------------------------------------

hae_saa <- function() {
  polku <- file.path(DATA_DIR, "saa.qs2")
  if (file.exists(polku)) return(qd_read(polku))

  # FMI WFS, päivittäiset arvot. Haetaan kuukausi kerrallaan (rajapinnan raja).
  kuukaudet <- unique(floor_date(PAIVAT, "month"))

  hae_kk <- function(alku) {
    loppu <- min(ceiling_date(alku, "month") - days(1), max(PAIVAT))
    url <- paste0(
      "https://opendata.fmi.fi/wfs?service=WFS&version=2.0.0",
      "&request=getFeature&storedquery_id=fmi::observations::weather::daily::simple",
      "&place=Helsinki",
      "&starttime=", format(alku, "%Y-%m-%d"), "T00:00:00Z",
      "&endtime=", format(loppu, "%Y-%m-%d"), "T23:59:59Z",
      "&parameters=rrday,tday"
    )
    resp <- request(url) |> req_retry(max_tries = 3) |> req_perform()
    xml <- xml2::read_xml(resp_body_string(resp))
    ns <- xml2::xml_ns(xml)
    elems <- xml2::xml_find_all(xml, "//BsWfs:BsWfsElement", ns)
    if (length(elems) == 0) return(tibble())
    tibble(
      aika = xml2::xml_text(xml2::xml_find_first(elems, ".//BsWfs:Time", ns)),
      muuttuja = xml2::xml_text(xml2::xml_find_first(elems, ".//BsWfs:ParameterName", ns)),
      arvo = as.numeric(xml2::xml_text(xml2::xml_find_first(elems, ".//BsWfs:ParameterValue", ns)))
    )
  }

  raaka <- map_dfr(kuukaudet, hae_kk, .progress = TRUE)
  stopifnot(nrow(raaka) > 0)

  saa <- raaka |>
    mutate(pvm = as.Date(aika)) |>
    select(pvm, muuttuja, arvo) |>
    pivot_wider(names_from = muuttuja, values_from = arvo, values_fn = mean) |>
    rename(sade_mm = rrday, lampotila_c = tday) |>
    # FMI koodaa puuttuvan sateen arvolla -1
    mutate(sade_mm = if_else(sade_mm < 0, NA_real_, sade_mm))

  stopifnot(all(c("sade_mm", "lampotila_c") %in% names(saa)))
  qd_save(saa, polku)
  saa
}

saa <- hae_saa()
message("Säähavaintopäiviä: ", nrow(saa),
        " | puuttuvia sadearvoja: ", sum(is.na(saa$sade_mm)))

# ----------------------------------------------------------------------------
# 5. KALENTERI
# ----------------------------------------------------------------------------

arkipyhat <- as.Date(c(
  # 2025
  "2025-05-01", "2025-05-29", "2025-06-20", "2025-06-21",
  # 2026
  "2026-05-01", "2026-05-14", "2026-06-19", "2026-06-20"
))

kalenteri <- tibble(pvm = PAIVAT) |>
  mutate(
    vuosi = year(pvm),
    viikonpaiva = wday(pvm, week_start = 1),
    arkipaiva = viikonpaiva <= 5,
    arkipyha = pvm %in% arkipyhat,
    # Pääkaupunkiseudun koulujen kesäloma (tarkista vuosittain)
    kesaloma = (pvm >= as.Date(paste0(vuosi, "-06-01")) &
                  pvm <= as.Date(paste0(vuosi, "-08-09"))),
    viikko = isoweek(pvm)
  ) |>
  left_join(saa, by = "pvm")

stopifnot(nrow(kalenteri) == length(PAIVAT), !any(duplicated(kalenteri$pvm)))
qd_save(kalenteri, file.path(DATA_DIR, "kalenteri.qs2"))

# ----------------------------------------------------------------------------
# YHTEENVETO
# ----------------------------------------------------------------------------

message("\nValmiit aineistot kansiossa ", DATA_DIR, ":")
message("  lam_hist.qs2           - nopeushistogrammit (piste, pvm, tunti, suunta, kaista, luokkaryhmä, vapaa virta, nopeus)")
message("  lam_tunti.qs2          - tuntitason tunnusluvut")
message("  lam_diagnostiikka.qs2  - tiedostokohtaiset laatutarkistukset")
message("  lam_puuttuvat.qs2      - päivät joilta dataa ei saatu")
message("  lam_vajaat_paivat.qs2  - päivät joilta puuttuu tunteja")
message("  piste_konteksti.qs2    - etäisyys kameraan, nopeusrajoitus")
message("  kalenteri.qs2          - kalenteri- ja säämuuttujat")
message("  lam_asemat.qs2         - asemien metatiedot (sf)")
message("  osm_kamerat_tiet.qs2   - OSM-kamerat ja tielinjat (sf)")

for (f in c("lam_hist.qs2", "lam_tunti.qs2")) {
  koko <- file.size(file.path(DATA_DIR, f)) / 1024^2
  message(sprintf("  %s: %.1f MB", f, koko))
}
