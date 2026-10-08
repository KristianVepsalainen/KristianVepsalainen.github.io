# =============================================================================
# 02b-diagnostiikka-korjaus.R
#
# Korvaa 02-diagnostiikka.R:n jaksot 5 ja 6, jotka olivat rikki.
#
# VIRHE: summarise() evaluoi lausekkeet jarjestyksessa, ja aiemman
# lausekkeen tulos korvaa saman nimisen sarakkeen seuraavissa. Koodissa oli
#
#     summarise(n  = sum(as.numeric(n)),              # n -> skalaari
#               ka = sum(as.numeric(n) * nopeus) / sum(as.numeric(n)))
#
# jolloin toisessa rivissa n ei enaa ollut havaintomaarien vektori vaan
# edellisen rivin skalaari. Siksi "keskinopeudeksi" tuli nopeusarvojen
# summa (2 850 955) ja ylitysosuuksiksi NA - skalaarin indeksointi
# vektorilla antaa NA:ta.
#
# KORJAUS: koostetta ei nimeta koskaan samaksi kuin lahdesaraketta.
# Havaintomaara on nyt havainnot, ei n.
#
# Jaksot 1-4 ja 7 olivat kunnossa, eika niita ajeta uudelleen.
# =============================================================================

library(here)
library(tidyverse)
library(qs2)
library(lubridate)
library(sf)

select <- dplyr::select
filter <- dplyr::filter

DATA_DIR <- here("data", "peltipoliisi")

viiva <- function(otsikko) {
  cat("\n", strrep("=", 70), "\n", otsikko, "\n", strrep("=", 70), "\n", sep = "")
}

hist_pv   <- qd_read(file.path(DATA_DIR, "lam_hist_pv.qs2"))
kalenteri <- qd_read(file.path(DATA_DIR, "kalenteri.qs2"))
kontek    <- qd_read(file.path(DATA_DIR, "piste_konteksti.qs2"))
asemat    <- qs_read(file.path(DATA_DIR, "lam_asemat.qs2"))

# ----------------------------------------------------------------------------
# Apufunktiot
# ----------------------------------------------------------------------------

# Painotettu kvantiili histogrammista. nopeus ja paino ovat samanmittaiset;
# nopeus saa toistua (eri kaistat), mika ei haittaa kun jarjestetaan ensin.
wq <- function(nopeus, paino, q) {
  o <- order(nopeus)
  v <- nopeus[o]
  w <- as.numeric(paino)[o]
  kum <- cumsum(w) / sum(w)
  v[which(kum >= q)[1]]
}

stopifnot(
  wq(c(1, 2, 3), c(1, 1, 1), 0.5) == 2,
  wq(c(1, 2, 3), c(98, 1, 1), 0.5) == 1,
  wq(c(3, 1, 2), c(1, 1, 98), 0.85) == 2
)

# Pistekohtainen nopeusrajoitus. OSM antoi kelvollisen luvun kaikille,
# mutta varmistus jatetaan paikalleen.
rajoitus <- kontek |>
  mutate(raja = suppressWarnings(as.integer(osm_maxspeed))) |>
  select(lam_id, raja)

if (any(is.na(rajoitus$raja))) {
  print(rajoitus |> filter(is.na(raja)))
  stop("Nopeusrajoitus puuttuu joiltakin pisteilta - aseta kasin.")
}

arkipv <- kalenteri |> filter(arkipaiva, !arkipyha) |> pull(pvm)

# Analyysiaineisto: henkiloautot, vapaa virta, arkipaivat ilman arkipyhia.
pohja <- hist_pv |>
  filter(lk_ryhma == 1L, vapaa == 1L, pvm %in% arkipv) |>
  left_join(rajoitus, by = "lam_id")

stopifnot(nrow(pohja) > 0, !any(is.na(pohja$raja)))

# Tunnusluvut annetuilla ryhmilla. havainnot != n, jotta sama virhe ei toistu.
tunnusluvut <- function(d, ...) {
  d |>
    group_by(...) |>
    summarise(
      havainnot = sum(as.numeric(n)),
      ka   = sum(as.numeric(n) * nopeus) / sum(as.numeric(n)),
      p50  = wq(nopeus, n, 0.50),
      p85  = wq(nopeus, n, 0.85),
      p95  = wq(nopeus, n, 0.95),
      yli0 = 100 * sum(as.numeric(n)[nopeus >  raja])      / sum(as.numeric(n)),
      yli6 = 100 * sum(as.numeric(n)[nopeus >= raja + 6])  / sum(as.numeric(n)),
      yli11= 100 * sum(as.numeric(n)[nopeus >= raja + 11]) / sum(as.numeric(n)),
      .groups = "drop")
}

# ----------------------------------------------------------------------------
# 5. Tunnusluvut pisteittain, 2025 vs 2026
# ----------------------------------------------------------------------------

viiva("5. TUNNUSLUVUT PISTEITTAIN, 2025 vs 2026 (korjattu)")

tl <- pohja |>
  mutate(vuosi = year(pvm)) |>
  tunnusluvut(lam_id, vuosi)

# TARKISTUS: keskinopeuden pitaa olla uskottava.
if (any(tl$ka < 20 | tl$ka > 140)) {
  print(tl |> filter(ka < 20 | ka > 140))
  stop("Keskinopeus epauskottava - laskenta on yha rikki.")
}

tl_leveä <- tl |>
  select(lam_id, vuosi, ka, p85, yli6, yli11) |>
  pivot_wider(names_from = vuosi, values_from = c(ka, p85, yli6, yli11)) |>
  left_join(rajoitus, by = "lam_id") |>
  left_join(kontek |> select(lam_id, kam_m = etaisyys_kameraan_m), by = "lam_id") |>
  left_join(st_drop_geometry(asemat) |>
              select(lam_id, tie = tienumero_lopullinen), by = "lam_id") |>
  mutate(kam_m = round(kam_m),
         d_ka   = round(ka_2026 - ka_2025, 2),
         d_yli6 = round(yli6_2026 - yli6_2025, 2),
         across(where(is.numeric), \(x) round(x, 2))) |>
  select(lam_id, tie, raja, kam_m,
         ka_2025, ka_2026, d_ka,
         p85_2025, p85_2026,
         yli6_2025, yli6_2026, d_yli6,
         yli11_2025, yli11_2026) |>
  arrange(tie, kam_m)

cat("raja = nopeusrajoitus, kam_m = etaisyys lahimpaan OSM-kameraan\n")
cat("yli6 = osuus (%) joka ylittaa rajan vahintaan 6 km/h\n\n")
print(tl_leveä, n = 50, width = 220)

write_csv(tl_leveä, file.path(DATA_DIR, "tunnusluvut_pisteittain.csv"))

# ----------------------------------------------------------------------------
# 5b. Kaistarakenne: kumpi kaista on nopea kaista
# ----------------------------------------------------------------------------
# Kaistanumerointi ei ole itsestaan selva. Katsotaan empiirisesti.

viiva("5b. KAISTAT KEHA I:N PISTEILLA (2026)")

kehaI <- asemat |> st_drop_geometry() |>
  filter(tienumero_lopullinen == 101) |> pull(lam_id)

kaistat <- pohja |>
  filter(lam_id %in% kehaI, year(pvm) == 2026) |>
  tunnusluvut(lam_id, suunta, kaista) |>
  select(lam_id, suunta, kaista, havainnot, ka, p85) |>
  mutate(ka = round(ka, 1)) |>
  arrange(lam_id, suunta, kaista)

print(kaistat |> filter(lam_id %in% c(116L, 145L, 146L)), n = 60)
cat("Korkein ka kussakin suunnassa = nopea kaista.\n")

write_csv(kaistat, file.path(DATA_DIR, "kaistat_kehaI_2026.csv"))

# ----------------------------------------------------------------------------
# 6. Piste 116 viikoittain 2026
# ----------------------------------------------------------------------------

viiva("6. PISTE 116 VIIKOITTAIN 2026 (korjattu)")

vk116 <- pohja |>
  filter(lam_id == 116L, year(pvm) == 2026) |>
  mutate(vk = isoweek(pvm)) |>
  tunnusluvut(vk, suunta) |>
  mutate(across(c(ka, yli0, yli6, yli11), \(x) round(x, 2)))

print(vk116 |> select(vk, suunta, havainnot, ka, p85, p95, yli6, yli11),
      n = 60, width = 200)

write_csv(vk116, file.path(DATA_DIR, "piste116_viikko_2026.csv"))

# ----------------------------------------------------------------------------
# 6b. Kehä I vs verrokkitiet viikoittain: erottuuko Kehä I
# ----------------------------------------------------------------------------
# Jos valvonta vaikuttaa, Kehä I:n ylitysosuuden pitaisi laskea
# samalla kun verrokkiteilla ei tapahdu mitaan.

viiva("6b. KEHA I vs VERROKIT VIIKOITTAIN 2026")

# Vain pisteet joilta on dataa koko jaksolta kumpanakin vuonna.
taydet <- hist_pv |>
  mutate(vuosi = year(pvm)) |>
  group_by(lam_id, vuosi) |>
  summarise(pv = n_distinct(pvm), .groups = "drop") |>
  pivot_wider(names_from = vuosi, values_from = pv) |>
  filter(!is.na(`2025`), !is.na(`2026`), `2025` > 140, `2026` > 140) |>
  pull(lam_id)

cat("Pisteita joilta kumpikin vuosi riittavan kattava: ", length(taydet), "\n")
cat("Niista Keha I:lla: ", sum(taydet %in% kehaI), "\n\n")

ryhma_vk <- pohja |>
  filter(lam_id %in% taydet, year(pvm) == 2026) |>
  mutate(vk = isoweek(pvm),
         ryhma = if_else(lam_id %in% kehaI, "KehaI", "verrokki")) |>
  tunnusluvut(ryhma, vk) |>
  select(ryhma, vk, ka, p85, yli6) |>
  mutate(ka = round(ka, 2), yli6 = round(yli6, 2)) |>
  pivot_wider(names_from = ryhma, values_from = c(ka, p85, yli6))

print(ryhma_vk, n = 40, width = 200)

# ----------------------------------------------------------------------------
# 6c. Paivittain elo-syyskuu 2026: tarkka muutoskohdan haku
# ----------------------------------------------------------------------------
# Vesan virhemaksu on paivalta 2026-09-04. Katsotaan sita ympäroiva jakso
# paivatasolla, jotta nahdaan muuttuuko mikaan ja milloin.

viiva("6c. PAIVITTAIN 2026-08-10 ... 2026-09-25")

jakso <- seq(as.Date("2026-08-10"), as.Date("2026-09-25"), by = "day")

pv_sarja <- pohja |>
  filter(lam_id %in% taydet, pvm %in% jakso) |>
  mutate(ryhma = if_else(lam_id %in% kehaI, "KehaI", "verrokki")) |>
  tunnusluvut(ryhma, pvm) |>
  select(ryhma, pvm, ka, p85, yli6) |>
  mutate(ka = round(ka, 2), yli6 = round(yli6, 2)) |>
  pivot_wider(names_from = ryhma, values_from = c(ka, p85, yli6)) |>
  mutate(vpv = wday(pvm, label = TRUE, week_start = 1))

pv116 <- pohja |>
  filter(lam_id == 116L, pvm %in% jakso) |>
  tunnusluvut(pvm) |>
  select(pvm, ka116 = ka, p85_116 = p85, yli6_116 = yli6) |>
  mutate(ka116 = round(ka116, 2), yli6_116 = round(yli6_116, 2))

pv_sarja <- pv_sarja |> left_join(pv116, by = "pvm")

print(pv_sarja, n = 60, width = 220)

write_csv(pv_sarja, file.path(DATA_DIR, "paivasarja_elo_syys_2026.csv"))

# ----------------------------------------------------------------------------
# 6d. Kamerat pisteen 116 lahella
# ----------------------------------------------------------------------------

viiva("6d. OSM-KAMERAT LAHELLA PISTETTA 116")

osm <- qs_read(file.path(DATA_DIR, "osm_kamerat_tiet.qs2"))
p116 <- asemat |> filter(lam_id == 116L)

lahella <- osm$kamerat |>
  mutate(etaisyys_m = as.numeric(st_distance(geometry, p116))) |>
  st_drop_geometry() |>
  filter(etaisyys_m < 3000) |>
  arrange(etaisyys_m) |>
  mutate(etaisyys_m = round(etaisyys_m))

print(lahella |> select(any_of(c("osm_id", "direction", "maxspeed", "etaisyys_m"))),
      n = 20)

cat("\nKeha I:n kamerat yhteensa 2 km sateella kustakin LAM-pisteesta:\n")
kam_lkm <- asemat |>
  filter(lam_id %in% kehaI) |>
  mutate(kameroita_2km = lengths(st_is_within_distance(geometry, osm$kamerat, 2000))) |>
  st_drop_geometry() |>
  select(lam_id, nimi, kameroita_2km) |>
  arrange(desc(kameroita_2km))
print(kam_lkm, n = 20)

viiva("VALMIS - kopioi koko tuloste")
