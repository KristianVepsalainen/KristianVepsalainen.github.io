# =============================================================================
# 06-poikkeustilanne-haku.R
#
# Peltipoliisi-sarjan bonusosa: nakyyko liikenteen poikkeustilanne datassa?
#
# TUTKIMUSASETELMA
#
# Havaintojaksolle osuu kaksi valtiovierailua, joiden paivamaarat ovat
# julkisia:
#   28.-29.4.2026  Viron presidentti Alar Karis
#   10.-11.6.2026  Kenian presidentti William Ruto
#
# Poliisi tiedottaa vierailuista noin vuorokautta etukateen ja kertoo,
# etta saattueet aiheuttavat "lyhyita liikennekatkoksia". Katkojen
# kestoa, kellonaikaa tai reittia ei julkaista.
#
# Kysymys: nakyvatko katkot LAM-datassa, ja kuinka tarkasti?
#
# MENETELMA
#
# Saattuesulku tuottaa kaksoissignaalin: ensin minuutteja lahes ilman
# ajoneuvoja, sitten purkautuva jono. Molemmat ovat mitattavissa
# minuuttitasolla. Vertailukohtana ovat samat viikonpaivat edeltavilta
# ja seuraavilta viikoilta.
#
# Tama skripti EI tee analyysia eika piirra mitaan. Se hakee datan,
# tiivistaa sen minuuttitasolle ja tulostaa yhteenvedon.
#
# Ajoaika noin 15-30 min. Keskeytettavissa: valimuisti per (piste, paiva).
# =============================================================================

library(here)
library(tidyverse)
library(httr2)
library(qs2)
library(lubridate)
library(sf)

select <- dplyr::select
filter <- dplyr::filter

DATA_DIR  <- here("data", "peltipoliisi")
CACHE_DIR <- file.path(DATA_DIR, "cache_minuutti")
dir.create(CACHE_DIR, recursive = TRUE, showWarnings = FALSE)

DIGITRAFFIC_USER <- "kristianvepsalainen.com / poikkeustilanne-analyysi"

# Aikavalin yksikko varmistettiin aiemmin: millisekunti.
AIKAVALI_SKAALA <- 1000

# --- Tapahtumat ja verrokkipaivat -------------------------------------------

TAPAHTUMAT <- tribble(
  ~pvm,                      ~tapahtuma,
  as.Date("2026-04-28"),     "Viron presidentti, 1. paiva",
  as.Date("2026-04-29"),     "Viron presidentti, 2. paiva",
  as.Date("2026-06-10"),     "Kenian presidentti, 1. paiva",
  as.Date("2026-06-11"),     "Kenian presidentti, 2. paiva"
)

# Verrokit: sama viikonpaiva +-1 ja +-2 viikkoa. Nain viikonpaivarytmi
# ja vuodenaika pysyvat samoina.
VERROKKI_SIIRROT <- c(-14, -7, 7, 14)

verrokit <- TAPAHTUMAT |>
  tidyr::expand_grid(siirto = VERROKKI_SIIRROT) |>
  mutate(pvm = pvm + siirto, tapahtuma = "verrokki") |>
  select(pvm, tapahtuma) |>
  filter(!pvm %in% TAPAHTUMAT$pvm)

PAIVAT <- bind_rows(TAPAHTUMAT, verrokit) |>
  distinct(pvm, .keep_all = TRUE) |>
  arrange(pvm)

message("Paivia yhteensa: ", nrow(PAIVAT),
        " (tapahtumia ", nrow(TAPAHTUMAT),
        ", verrokkeja ", nrow(PAIVAT) - nrow(TAPAHTUMAT), ")")

# --- Pisteiden valinta ------------------------------------------------------
# Kaikki LAM-pisteet, jotka ovat alle 30 km Helsingin keskustasta.
# Saattuereitit lentoasemalta keskustaan kulkevat tassa joukossa.

polku_asemat <- file.path(DATA_DIR, "lam_asemat.qs2")
if (!file.exists(polku_asemat))
  stop("lam_asemat.qs2 puuttuu. Aja 01a-asemat-korjaus.R ensin.")

asemat <- qs_read(polku_asemat)

keskusta <- st_sfc(st_point(c(24.9384, 60.1699)), crs = 4326) |>
  st_transform(3067)

lentoasema <- st_sfc(st_point(c(24.9633, 60.3172)), crs = 4326) |>
  st_transform(3067)

asemat_hki <- asemat |>
  mutate(
    etaisyys_keskustaan_km = as.numeric(st_distance(geometry, keskusta)) / 1000,
    etaisyys_lentoasemalle_km = as.numeric(st_distance(geometry, lentoasema)) / 1000
  ) |>
  filter(etaisyys_keskustaan_km < 30)

PISTEET <- sort(unique(asemat_hki$lam_id))

stopifnot(length(PISTEET) > 10)

message("Pisteita: ", length(PISTEET),
        " | tiedostoja haettavana: ", length(PISTEET) * nrow(PAIVAT))

message("\n--- Pisteet lahimpana lentoasema-keskusta-akselia ---")
print(asemat_hki |>
        st_drop_geometry() |>
        arrange(etaisyys_lentoasemalle_km) |>
        select(lam_id, nimi, tienumero_lopullinen,
               km_lentoasemalta = etaisyys_lentoasemalle_km,
               km_keskustaan = etaisyys_keskustaan_km) |>
        mutate(across(starts_with("km_"), \(x) round(x, 1))) |>
        head(20), n = 20)

# --- Raakadatan haku --------------------------------------------------------

LAM_SARAKKEET <- c(
  "pistetunnus", "vuosi", "paiva", "tunti", "minuutti", "sekunti", "sadasosa",
  "pituus", "kaista", "suunta", "luokka", "nopeus", "faulty",
  "kokonaisaika", "aikavali", "jonoalku"
)

lataa_lamraw <- function(lam_id, pvm) {
  url <- sprintf(
    "https://tie.digitraffic.fi/api/tms/v1/history/raw/lamraw_%d_%d_%d.csv",
    lam_id, as.integer(format(pvm, "%y")), yday(pvm))

  resp <- request(url) |>
    req_headers("Digitraffic-User" = DIGITRAFFIC_USER) |>
    req_throttle(rate = 50 / 60) |>
    req_retry(max_tries = 3, backoff = \(i) 2^i) |>
    req_error(is_error = \(r) FALSE) |>
    req_perform()

  if (resp_status(resp) != 200) return(NULL)
  txt <- resp_body_string(resp)
  if (nchar(txt) < 200) return(NULL)

  eka <- sub("\n.*$", "", txt)
  if (lengths(regmatches(eka, gregexpr(";", eka))) + 1L != length(LAM_SARAKKEET))
    return(NULL)

  suppressWarnings(readr::read_delim(
    I(txt), delim = ";", col_names = LAM_SARAKKEET,
    col_types = readr::cols(.default = readr::col_double()),
    skip = if (grepl("[A-Za-z]", eka)) 1L else 0L,
    locale = readr::locale(decimal_mark = ".")))
}

# Tiivistys minuuttitasolle. Kaksi mittaria, jotka molemmat reagoivat
# sulkuun mutta eri tavalla:
#   ajoneuvoja   - montako ajoneuvoa minuutin aikana (romahtaa sulussa)
#   max_vali_s   - pisin aikavali perakkaisten ajoneuvojen valilla
#                  (rajahtaa sulussa)
tiivista_minuutit <- function(d, lam_id, pvm) {
  dd <- d |>
    filter(faulty == 0, suunta %in% c(1, 2)) |>
    mutate(
      minuutti_vrk = as.integer(tunti) * 60L + as.integer(minuutti),
      aikavali_s = aikavali / AIKAVALI_SKAALA,
      suunta = as.integer(suunta)
    ) |>
    filter(minuutti_vrk >= 0, minuutti_vrk < 1440)

  if (nrow(dd) == 0) return(NULL)

  dd |>
    group_by(suunta, minuutti_vrk) |>
    summarise(
      ajoneuvoja  = n(),
      max_vali_s  = max(aikavali_s, na.rm = TRUE),
      ka_nopeus   = mean(nopeus),
      .groups = "drop") |>
    mutate(lam_id = as.integer(lam_id), pvm = pvm) |>
    select(lam_id, pvm, suunta, minuutti_vrk, ajoneuvoja, max_vali_s, ka_nopeus)
}

cache_polku <- function(lam_id, pvm) {
  file.path(CACHE_DIR, sprintf("min_%d_%s.qs2", lam_id, format(pvm, "%Y%m%d")))
}

kasittele <- function(lam_id, pvm) {
  polku <- cache_polku(lam_id, pvm)
  if (file.exists(polku)) return(invisible(NULL))

  raaka <- tryCatch(lataa_lamraw(lam_id, pvm), error = function(e) NULL)

  if (is.null(raaka) || nrow(raaka) == 0) {
    qs_save(list(status = "puuttuu", lam_id = lam_id, pvm = pvm), polku)
    return(invisible(NULL))
  }

  # TARKISTUS: tiedoston pitaa vastata pyydettya pistetta ja paivaa.
  if (!all(raaka$pistetunnus == lam_id) || !all(raaka$paiva == yday(pvm)))
    stop(sprintf("Piste %d %s: tiedoston sisalto ei vastaa pyyntoa.",
                 lam_id, pvm))

  qs_save(list(status = "ok", lam_id = lam_id, pvm = pvm,
               minuutit = tiivista_minuutit(raaka, lam_id, pvm)), polku)
  invisible(NULL)
}

# --- Koeajo ennen isoa latausta ---------------------------------------------

koe <- lataa_lamraw(PISTEET[1], TAPAHTUMAT$pvm[1])
if (is.null(koe))
  stop("Koeajo epaonnistui pisteella ", PISTEET[1], " paivalle ",
       TAPAHTUMAT$pvm[1], ". Tarkista verkkoyhteys ja pisteen tunnus.")

message("\nKoeajo ok: ", nrow(koe), " riviä pisteeltä ", PISTEET[1])

# --- Lataus -----------------------------------------------------------------

grid <- tidyr::expand_grid(lam_id = PISTEET, pvm = PAIVAT$pvm)

valmiina <- sum(file.exists(map2_chr(grid$lam_id, grid$pvm, cache_polku)))
message("Valmiina jo: ", valmiina, " / ", nrow(grid))

pwalk(grid, kasittele, .progress = TRUE)

# --- Kokoaminen erissa ------------------------------------------------------

odotetut <- map2_chr(grid$lam_id, grid$pvm, cache_polku)
if (any(!file.exists(odotetut)))
  stop(sum(!file.exists(odotetut)), " tiedostoa puuttuu - aja uudelleen.")

erat <- split(odotetut, ceiling(seq_along(odotetut) / 200))
palat <- vector("list", length(erat))

for (i in seq_along(erat)) {
  x <- map(erat[[i]], qs_read)
  palat[[i]] <- x |> keep(\(e) e$status == "ok") |> map_dfr("minuutit")
  rm(x); gc(verbose = FALSE)
}

minuutit <- bind_rows(palat)
rm(palat); gc(verbose = FALSE)

stopifnot(nrow(minuutit) > 0,
          !any(duplicated(minuutit[c("lam_id", "pvm", "suunta",
                                     "minuutti_vrk")])))

minuutit <- minuutit |>
  left_join(PAIVAT, by = "pvm") |>
  mutate(on_tapahtuma = tapahtuma != "verrokki")

qd_save(minuutit, file.path(DATA_DIR, "minuuttidata.qs2"))

message(sprintf("\nminuuttidata.qs2: %.1f M riviä, %.0f MB",
                nrow(minuutit) / 1e6,
                file.size(file.path(DATA_DIR, "minuuttidata.qs2")) / 1024^2))

# --- Yhteenveto: nakyyko mitaan ---------------------------------------------

viiva <- function(x) cat("\n", strrep("=", 70), "\n", x, "\n",
                         strrep("=", 70), "\n", sep = "")

viiva("1. KATTAVUUS")

print(as.data.frame(
  minuutit |>
    group_by(pvm, tapahtuma) |>
    summarise(pisteita = n_distinct(lam_id),
              minuutteja = n(),
              ajoneuvoja = sum(ajoneuvoja), .groups = "drop")))

viiva("2. PISIMMAT AUKOT TAPAHTUMAPAIVINA (klo 6-22)")

# Odotusarvo kullekin (piste, suunta, minuutti) verrokkipaivista.
odotus <- minuutit |>
  filter(!on_tapahtuma) |>
  group_by(lam_id, suunta, minuutti_vrk) |>
  summarise(odotus_ajoneuvoja = median(ajoneuvoja),
            odotus_max_vali = median(max_vali_s),
            verrokkeja = n(), .groups = "drop")

poikkeamat <- minuutit |>
  filter(on_tapahtuma, minuutti_vrk >= 360, minuutti_vrk < 1320) |>
  left_join(odotus, by = c("lam_id", "suunta", "minuutti_vrk")) |>
  filter(!is.na(odotus_ajoneuvoja), verrokkeja >= 3, odotus_ajoneuvoja >= 10) |>
  mutate(
    suhde = ajoneuvoja / odotus_ajoneuvoja,
    vali_kerroin = max_vali_s / pmax(odotus_max_vali, 1),
    kello = sprintf("%02d:%02d", minuutti_vrk %/% 60, minuutti_vrk %% 60)
  )

cat("Minuutteja tarkastelussa:", nrow(poikkeamat), "\n")
cat("Minuutteja joissa liikenne alle 25 % odotetusta:",
    sum(poikkeamat$suhde < 0.25), "\n\n")

kiinnostavat <- poikkeamat |>
  filter(suhde < 0.25) |>
  left_join(asemat_hki |> st_drop_geometry() |>
              select(lam_id, nimi, tienumero_lopullinen), by = "lam_id") |>
  arrange(pvm, minuutti_vrk) |>
  select(pvm, tapahtuma, kello, lam_id, nimi, tie = tienumero_lopullinen,
         suunta, ajoneuvoja, odotus_ajoneuvoja, max_vali_s, vali_kerroin)

print(as.data.frame(head(kiinnostavat, 60)))

write_csv(kiinnostavat, file.path(DATA_DIR, "poikkeamat_tapahtumapaivat.csv"))

viiva("3. SAMA MITTA VERROKKIPAIVILLE (vaarat havainnot)")

# Sama kynnys verrokkipaiviin: kuinka usein tama osuu vaaraan.
odotus_v <- minuutit |>
  filter(!on_tapahtuma) |>
  group_by(lam_id, suunta, minuutti_vrk) |>
  summarise(odotus_ajoneuvoja = median(ajoneuvoja),
            verrokkeja = n(), .groups = "drop")

vaarat <- minuutit |>
  filter(!on_tapahtuma, minuutti_vrk >= 360, minuutti_vrk < 1320) |>
  left_join(odotus_v, by = c("lam_id", "suunta", "minuutti_vrk")) |>
  filter(!is.na(odotus_ajoneuvoja), verrokkeja >= 3, odotus_ajoneuvoja >= 10) |>
  mutate(suhde = ajoneuvoja / odotus_ajoneuvoja)

cat("Verrokkiminuutteja:", nrow(vaarat), "\n")
cat("Niista alle 25 % odotetusta:", sum(vaarat$suhde < 0.25),
    sprintf("(%.3f %%)\n", 100 * mean(vaarat$suhde < 0.25)))
cat("\nTapahtumapaivina vastaava osuus:",
    sprintf("%.3f %%\n", 100 * mean(poikkeamat$suhde < 0.25)))

cat("\nJos naiden osuuksien valilla ei ole eroa, saattuesulut eivat\n")
cat("erotu tavallisesta hairiosta tallä mittarilla.\n")

viiva("VALMIS")
message("Liitä tuloste keskusteluun. Olennaisia ovat jakso 2 ja 3.")
