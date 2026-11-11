# =============================================================================
# 08-hairion-sade-haku.R
#
# Kysymys: kuinka kauas suurtapahtuman liikennehairio ulottuu?
#
# ASETELMA
#
# Edellinen koe etsi saattuesulkua: muutaman minuutin tapahtumaa muutamassa
# pisteessa. Se ei loytynyt, ja voima-analyysi kertoi miksi: ilmio oli
# pienempi kuin paivien valinen kohina.
#
# Tama koe on painvastainen. Helsinki City Running Day sulkee kaupungin
# ytimen tunneiksi, ja osallistujia on kymmenia tuhansia. Jos tallainen
# tapahtuma ei nay LAM-datassa, mikaan ei nay.
#
# Tapahtumapaivaa EI kiinniteta etukateen. Haetaan kaikki toukokuun
# lauantait kahdelta vuodelta ja annetaan datan kertoa, mika niista
# poikkeaa. Julkaistu paivamaara (16.5.2026) on vasta tarkistus.
#
# MITTARI
#
# Tuntikohtainen liikennemaara ja vapaan virran keskinopeus. Poikkeama
# lasketaan saman pisteen muihin lauantaihin nahden, ja se suhteutetaan
# saman pisteen lauantaiden valiseen normaaliin vaihteluun. Nain
# "hairio" tarkoittaa samaa asiaa vilkkaalla ja hiljaisella tiella.
#
# Etaisyys mitataan linnuntieta tapahtuman keskipisteesta, koska
# kysymys on muodossa "kuinka kauas pitaa lahtea".
#
# Ajoaika noin 20-30 min. Keskeytettavissa.
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
CACHE_DIR <- file.path(DATA_DIR, "cache_tunti")
dir.create(CACHE_DIR, recursive = TRUE, showWarnings = FALSE)

DIGITRAFFIC_USER <- "kristianvepsalainen.com / hairiosade-analyysi"
AIKAVALI_SKAALA <- 1000
VAPAA_VIRTA_S <- 5

# --- Tapahtuman keskipiste ---------------------------------------------------
# Helsinki City Running Day: lahto ja maali Olympiastadionin kupeessa.

KESKIPISTE_LON <- 24.9270
KESKIPISTE_LAT <- 60.1866

SADE_KM <- 45

# --- Paivat ------------------------------------------------------------------
# Kaikki lauantait toukokuussa 2025 ja 2026, seka huhti- ja kesakuun
# reunalauantait verrokeiksi. Tapahtumapaivaa ei kiinniteta etukateen.

lauantait <- function(alku, loppu) {
  d <- seq(as.Date(alku), as.Date(loppu), by = "day")
  d[wday(d, week_start = 1) == 6]
}

PAIVAT <- c(lauantait("2025-04-19", "2025-06-14"),
            lauantait("2026-04-18", "2026-06-13"))

# TARKISTUS: julkaistu tapahtumapaiva on mukana.
JULKAISTU_2026 <- as.Date("2026-05-16")
stopifnot(JULKAISTU_2026 %in% PAIVAT, all(wday(PAIVAT, week_start = 1) == 6))

message("Lauantaita yhteensa: ", length(PAIVAT))
print(PAIVAT)

# --- Pisteet -----------------------------------------------------------------

polku_asemat <- file.path(DATA_DIR, "lam_asemat.qs2")
if (!file.exists(polku_asemat))
  stop("lam_asemat.qs2 puuttuu. Aja 01a-asemat-korjaus.R ensin.")

asemat <- qs_read(polku_asemat)

keskipiste <- st_sfc(st_point(c(KESKIPISTE_LON, KESKIPISTE_LAT)),
                     crs = 4326) |> st_transform(3067)

asemat_mukana <- asemat |>
  mutate(etaisyys_km = as.numeric(st_distance(geometry, keskipiste)) / 1000) |>
  filter(etaisyys_km <= SADE_KM)

PISTEET <- sort(unique(asemat_mukana$lam_id))
stopifnot(length(PISTEET) > 20)

etaisyydet <- asemat_mukana |>
  st_drop_geometry() |>
  select(lam_id, nimi, tie = tienumero_lopullinen, etaisyys_km)

message("Pisteita ", SADE_KM, " km sateella: ", length(PISTEET))
message("Tiedostoja haettavana: ", length(PISTEET) * length(PAIVAT))

message("\n--- Etaisyysjakauma ---")
print(etaisyydet |>
        mutate(vyohyke = cut(etaisyys_km, c(0, 5, 10, 15, 25, 45),
                             labels = c("0-5", "5-10", "10-15",
                                        "15-25", "25-45"))) |>
        count(vyohyke, name = "pisteita"))

# --- Haku --------------------------------------------------------------------

LAM_SARAKKEET <- c(
  "pistetunnus", "vuosi", "paiva", "tunti", "minuutti", "sekunti", "sadasosa",
  "pituus", "kaista", "suunta", "luokka", "nopeus", "faulty",
  "kokonaisaika", "aikavali", "jonoalku")

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

# Tiivistys tuntitasolle. Liikennemaara kaikista ajoneuvoista, nopeus
# vain vapaasta virrasta, jotta ruuhka ei sekoita nopeuslukua.
tiivista_tunnit <- function(d, lam_id, pvm) {
  dd <- d |>
    filter(faulty == 0, suunta %in% c(1, 2), nopeus >= 2, nopeus <= 198) |>
    mutate(tunti = as.integer(tunti),
           suunta = as.integer(suunta),
           vapaa = (aikavali / AIKAVALI_SKAALA) >= VAPAA_VIRTA_S)
  
  if (nrow(dd) == 0) return(NULL)
  
  dd |>
    group_by(suunta, tunti) |>
    summarise(
      maara      = n(),
      maara_hapa = sum(luokka == 1),
      ka_nopeus  = mean(nopeus),
      ka_nopeus_vapaa = if (sum(vapaa) >= 20) mean(nopeus[vapaa]) else NA_real_,
      .groups = "drop") |>
    mutate(lam_id = as.integer(lam_id), pvm = pvm) |>
    select(lam_id, pvm, suunta, tunti, maara, maara_hapa,
           ka_nopeus, ka_nopeus_vapaa)
}

cache_polku <- function(lam_id, pvm) {
  file.path(CACHE_DIR, sprintf("t_%d_%s.qs2", lam_id, format(pvm, "%Y%m%d")))
}

kasittele <- function(lam_id, pvm) {
  polku <- cache_polku(lam_id, pvm)
  if (file.exists(polku)) return(invisible(NULL))
  
  raaka <- tryCatch(lataa_lamraw(lam_id, pvm), error = function(e) NULL)
  
  if (is.null(raaka) || nrow(raaka) == 0) {
    qs_save(list(status = "puuttuu", lam_id = lam_id, pvm = pvm), polku)
    return(invisible(NULL))
  }
  
  if (!all(raaka$pistetunnus == lam_id) || !all(raaka$paiva == yday(pvm)))
    stop(sprintf("Piste %d %s: sisalto ei vastaa pyyntoa.", lam_id, pvm))
  
  qs_save(list(status = "ok", lam_id = lam_id, pvm = pvm,
               tunnit = tiivista_tunnit(raaka, lam_id, pvm)), polku)
  invisible(NULL)
}

koe <- lataa_lamraw(PISTEET[1], PAIVAT[1])
if (is.null(koe))
  stop("Koeajo epaonnistui. Tarkista verkkoyhteys.")
message("\nKoeajo ok: ", nrow(koe), " rivia")

grid <- tidyr::expand_grid(lam_id = PISTEET, pvm = PAIVAT)
message("Valmiina jo: ",
        sum(file.exists(map2_chr(grid$lam_id, grid$pvm, cache_polku))),
        " / ", nrow(grid))

pwalk(grid, kasittele, .progress = TRUE)

# --- Kokoaminen --------------------------------------------------------------

odotetut <- map2_chr(grid$lam_id, grid$pvm, cache_polku)
if (any(!file.exists(odotetut)))
  stop(sum(!file.exists(odotetut)), " tiedostoa puuttuu. Aja uudelleen.")

erat <- split(odotetut, ceiling(seq_along(odotetut) / 300))
palat <- vector("list", length(erat))
for (i in seq_along(erat)) {
  x <- map(erat[[i]], qs_read)
  palat[[i]] <- x |> keep(\(e) e$status == "ok") |> map_dfr("tunnit")
  rm(x); gc(verbose = FALSE)
}
tunnit <- bind_rows(palat)
rm(palat); gc(verbose = FALSE)

stopifnot(nrow(tunnit) > 0,
          !any(duplicated(tunnit[c("lam_id", "pvm", "suunta", "tunti")])))

tunnit <- tunnit |> left_join(etaisyydet, by = "lam_id")
qd_save(tunnit, file.path(DATA_DIR, "tuntidata_tapahtuma.qs2"))

message(sprintf("\ntuntidata_tapahtuma.qs2: %s rivia, %.1f MB",
                format(nrow(tunnit), big.mark = " "),
                file.size(file.path(DATA_DIR,
                                    "tuntidata_tapahtuma.qs2")) / 1024^2))

# --- Yhteenveto --------------------------------------------------------------

viiva <- function(x) cat("\n", strrep("=", 70), "\n", x, "\n",
                         strrep("=", 70), "\n", sep = "")

viiva("1. KATTAVUUS")

print(as.data.frame(
  tunnit |>
    group_by(pvm) |>
    summarise(pisteita = n_distinct(lam_id),
              ajoneuvoja = sum(maara), .groups = "drop")))

# Vain pisteet joilta on riittavasti lauantaita kumpanakin vuonna, jotta
# vertailu on mahdollinen.
kelpaa <- tunnit |>
  group_by(lam_id, pvm) |>
  summarise(pv_maara = sum(maara), .groups = "drop") |>
  filter(pv_maara >= 2000) |>
  count(lam_id, name = "lauantaita") |>
  filter(lauantaita >= 8)

message("\nPisteita joilla vahintaan 8 kelvollista lauantaita: ",
        nrow(kelpaa))

viiva("2. MIKA LAUANTAI POIKKEAA")

# Paivataso pisteittain. Poikkeama lasketaan saman pisteen ja saman
# vuoden muiden lauantaiden mediaaniin nahden.
# HUOM: koostetta ei nimeta samaksi kuin lahdesaraketta. summarise()
# evaluoi lausekkeet jarjestyksessa, joten maara = sum(maara) korvaisi
# painomuuttujan skalaarilla jo seuraavalla rivilla.
paiva_piste <- tunnit |>
  filter(lam_id %in% kelpaa$lam_id) |>
  group_by(lam_id, pvm, etaisyys_km) |>
  summarise(ka_nopeus_vapaa = weighted.mean(ka_nopeus_vapaa, maara,
                                            na.rm = TRUE),
            maara_pv = sum(maara),
            .groups = "drop") |>
  mutate(vuosi = year(pvm))

stopifnot(all(paiva_piste$maara_pv > 0),
          all(is.na(paiva_piste$ka_nopeus_vapaa) |
                (paiva_piste$ka_nopeus_vapaa > 20 &
                   paiva_piste$ka_nopeus_vapaa < 140)))

# Jatetaan kukin paiva vuorollaan pois omasta odotusarvostaan.
poikkeamat <- paiva_piste |>
  group_by(lam_id, vuosi) |>
  mutate(
    muiden_mediaani = map_dbl(row_number(),
                              \(i) median(maara_pv[-i], na.rm = TRUE)),
    muiden_kh = map_dbl(row_number(), \(i) sd(maara_pv[-i], na.rm = TRUE)),
    poikkeama_pros = 100 * (maara_pv / muiden_mediaani - 1),
    z = (maara_pv - muiden_mediaani) / pmax(muiden_kh, 1)
  ) |>
  ungroup()

paivittain <- poikkeamat |>
  group_by(pvm) |>
  summarise(pisteita = n(),
            med_poikkeama = median(poikkeama_pros),
            med_z = median(z),
            osuus_poikkeavia = 100 * mean(abs(z) > 2),
            .groups = "drop") |>
  arrange(desc(abs(med_z)))

print(as.data.frame(paivittain))

cat("\nSuurimman poikkeaman paiva:", format(paivittain$pvm[1]), "\n")
cat("Julkaistu tapahtumapaiva 2026:", format(JULKAISTU_2026), "\n")
cat("Osuvatko yksiin:",
    if (paivittain$pvm[1] == JULKAISTU_2026) "KYLLA" else "EI", "\n")

viiva("3. POIKKEAMA ETAISYYDEN MUKAAN, TAPAHTUMAPAIVA")

tapahtuma_pvm <- JULKAISTU_2026

vyohykkeet <- poikkeamat |>
  filter(pvm == tapahtuma_pvm) |>
  mutate(vyohyke = cut(etaisyys_km, c(0, 3, 6, 10, 15, 25, 45),
                       labels = c("0-3 km", "3-6 km", "6-10 km",
                                  "10-15 km", "15-25 km", "25-45 km"))) |>
  group_by(vyohyke) |>
  summarise(pisteita = n(),
            med_poikkeama = round(median(poikkeama_pros), 1),
            med_z = round(median(z), 2),
            .groups = "drop")

print(as.data.frame(vyohykkeet))

cat("\nz-arvo kertoo poikkeaman suhteessa saman pisteen lauantaiden\n")
cat("normaaliin vaihteluun. |z| alle 2 = tavanomaisen vaihtelun sisalla.\n")

viiva("4. LAHIMMAT JA KAUIMMAT PISTEET TAPAHTUMAPAIVANA")

print(as.data.frame(
  poikkeamat |>
    filter(pvm == tapahtuma_pvm) |>
    left_join(etaisyydet |> select(lam_id, nimi, tie), by = "lam_id") |>
    arrange(etaisyys_km) |>
    mutate(etaisyys_km = round(etaisyys_km, 1),
           poikkeama_pros = round(poikkeama_pros, 1),
           z = round(z, 2)) |>
    select(lam_id, nimi, tie, etaisyys_km, maara_pv, muiden_mediaani,
           poikkeama_pros, z) |>
    head(25)))

viiva("5. TUNTIPROFIILI LAHIVYOHYKKEELLA")

print(as.data.frame(
  tunnit |>
    filter(lam_id %in% kelpaa$lam_id, etaisyys_km <= 10) |>
    mutate(tapahtuma = pvm == tapahtuma_pvm) |>
    group_by(tapahtuma, tunti) |>
    summarise(ka_maara = round(mean(maara)), .groups = "drop") |>
    pivot_wider(names_from = tapahtuma, values_from = ka_maara,
                names_prefix = "tapahtuma_") |>
    mutate(muutos_pros = round(100 * (tapahtuma_TRUE / tapahtuma_FALSE - 1), 1))))

qd_save(poikkeamat, file.path(DATA_DIR, "tapahtuma_poikkeamat.qs2"))

viiva("VALMIS")
cat("Olennaisia: jakso 2 (loytyyko tapahtuma), jakso 3 (sade), jakso 5 (ajoitus).\n")