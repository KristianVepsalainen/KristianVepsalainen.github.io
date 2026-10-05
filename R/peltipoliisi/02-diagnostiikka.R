# =============================================================================
# 02-diagnostiikka.R
#
# Peltipoliisi-sarja: mita aineisto sisaltaa ja mita siita voi vaittaa.
#
# Tama EI ole analyysi vaan tarkistus. Se tulostaa tiiviisti ne luvut,
# joiden perusteella paatetaan, mika osan 1 vaite on perusteltavissa.
# Tuloste on tarkoitettu kopioitavaksi sellaisenaan.
#
# Ei muuta mitaan eika kirjoita muuta kuin yhden CSV-tiedoston.
# Ajoaika alle minuutti.
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

# ----------------------------------------------------------------------------
# 1. Mita tiedostoja on olemassa
# ----------------------------------------------------------------------------

viiva("1. TIEDOSTOT")

odotetut <- c("lam_hist_pv.qs2", "lam_hist_h.qs2", "lam_tunti.qs2",
              "lam_diagnostiikka.qs2", "lam_puuttuvat.qs2",
              "lam_vajaat_paivat.qs2", "piste_konteksti.qs2",
              "kalenteri.qs2", "lam_asemat.qs2", "osm_kamerat_tiet.qs2")

for (f in odotetut) {
  p <- file.path(DATA_DIR, f)
  cat(sprintf("%-26s %s\n", f,
              if (file.exists(p)) sprintf("%8.1f MB", file.size(p)/1024^2)
              else "PUUTTUU"))
}

if (!file.exists(file.path(DATA_DIR, "lam_hist_pv.qs2")))
  stop("lam_hist_pv.qs2 puuttuu - aja 01c-kokoa-valimuisti.R uudelleen.")

hist_pv   <- qd_read(file.path(DATA_DIR, "lam_hist_pv.qs2"))
tunti     <- qd_read(file.path(DATA_DIR, "lam_tunti.qs2"))
diag      <- qd_read(file.path(DATA_DIR, "lam_diagnostiikka.qs2"))
kalenteri <- qd_read(file.path(DATA_DIR, "kalenteri.qs2"))
kontek    <- qd_read(file.path(DATA_DIR, "piste_konteksti.qs2"))
asemat    <- qs_read(file.path(DATA_DIR, "lam_asemat.qs2"))

cat(sprintf("\nhist_pv: %s rivia | tunti: %s rivia\n",
            format(nrow(hist_pv), big.mark = " "),
            format(nrow(tunti), big.mark = " ")))

# ----------------------------------------------------------------------------
# 2. KRIITTINEN: aikavalin yksikko
# ----------------------------------------------------------------------------
# Jos tama ei ole lahella 1000, vapaan virran suodatus on vaarin
# kalibroitu ja kaikki "vapaa virta" -luvut ovat roskaa.

viiva("2. AIKAVALIN YKSIKKO (kriittinen)")

cat(sprintf("suhde_aikavali: mediaani %.0f | ala-dec %.0f | yla-dec %.0f\n",
            median(diag$suhde_aikavali, na.rm = TRUE),
            quantile(diag$suhde_aikavali, 0.1, na.rm = TRUE, names = FALSE),
            quantile(diag$suhde_aikavali, 0.9, na.rm = TRUE, names = FALSE)))
cat("Odotettu noin 1000 (millisekunti). Jos ei ole, kerro ennen jatkoa.\n")

cat(sprintf("\nsuhde_kokonaisaika: mediaani %.0f\n",
            median(diag$suhde_kokonaisaika, na.rm = TRUE)))

cat(sprintf("faulty-osuus: mediaani %.3f %% | max %.2f %%\n",
            100 * median(diag$osuus_faulty),
            100 * max(diag$osuus_faulty)))

# Vapaan virran osuus: jos tama on ~0 tai ~1, raja on pielessa.
vapaa_osuus <- hist_pv |>
  filter(lk_ryhma == 1L) |>
  group_by(vapaa) |>
  summarise(n = sum(as.numeric(n)), .groups = "drop") |>
  mutate(osuus = n / sum(n))

cat("\nVapaan virran osuus henkiloautoista:\n")
print(vapaa_osuus)
cat("Jarkeva arvo on noin 0.3-0.7. Aariarvo kertoo vaarasta rajasta.\n")

# ----------------------------------------------------------------------------
# 3. Kattavuus ja pistekonteksti
# ----------------------------------------------------------------------------

viiva("3. KATTAVUUS PISTEITTAIN")

kattavuus <- hist_pv |>
  mutate(vuosi = year(pvm)) |>
  group_by(lam_id, vuosi) |>
  summarise(pv = n_distinct(pvm),
            ajon_pv = round(sum(as.numeric(n)) / n_distinct(pvm)),
            .groups = "drop") |>
  pivot_wider(names_from = vuosi, values_from = c(pv, ajon_pv)) |>
  left_join(kontek |> select(lam_id, nimi, osm_maxspeed,
                             etaisyys_kameraan_m), by = "lam_id") |>
  left_join(st_drop_geometry(asemat) |> select(lam_id, tie = tienumero_lopullinen),
            by = "lam_id") |>
  mutate(kam_m = round(etaisyys_kameraan_m)) |>
  select(lam_id, tie, nimi, raj = osm_maxspeed, kam_m, everything(),
         -etaisyys_kameraan_m) |>
  arrange(tie, kam_m)

print(kattavuus, n = 50, width = 200)

# ----------------------------------------------------------------------------
# 4. Nopeusrajoitus pisteittain: mista ylitys lasketaan
# ----------------------------------------------------------------------------

viiva("4. OSM-NOPEUSRAJOITUSTEN ARVOT")
print(table(kontek$osm_maxspeed, useNA = "ifany"))
cat("Jos arvoissa on tekstia tai NA:ta, rajoitus pitaa asettaa kasin.\n")

# ----------------------------------------------------------------------------
# 5. Tunnusluvut: muuttuuko mikaan 2025 -> 2026
# ----------------------------------------------------------------------------
# Rajataan: henkiloautot, vapaa virta, paivaliikenne (klo 6-21),
# arkipaivat ilman arkipyhia. Nain ruuhka ja viikonloppu eivat sekoita.

viiva("5. TUNNUSLUVUT PISTEITTAIN, 2025 vs 2026")

arkipv <- kalenteri |> filter(arkipaiva, !arkipyha) |> pull(pvm)

# Rajoitus pisteittain. Oletus 80 jos OSM ei anna kelvollista lukua.
rajoitus <- kontek |>
  mutate(raja = suppressWarnings(as.integer(osm_maxspeed)),
         raja = if_else(is.na(raja) | raja < 30 | raja > 120, 80L, raja)) |>
  select(lam_id, raja)

tunnusluvut <- hist_pv |>
  filter(lk_ryhma == 1L, vapaa == 1L, pvm %in% arkipv) |>
  left_join(rajoitus, by = "lam_id") |>
  mutate(vuosi = year(pvm)) |>
  group_by(lam_id, vuosi) |>
  summarise(
    n          = sum(as.numeric(n)),
    ka         = sum(as.numeric(n) * nopeus) / sum(as.numeric(n)),
    yli_raja   = sum(as.numeric(n[nopeus >  raja])) / sum(as.numeric(n)),
    yli_raja6  = sum(as.numeric(n[nopeus >= raja + 6])) / sum(as.numeric(n)),
    yli_raja11 = sum(as.numeric(n[nopeus >= raja + 11])) / sum(as.numeric(n)),
    .groups = "drop") |>
  pivot_wider(names_from = vuosi,
              values_from = c(n, ka, yli_raja, yli_raja6, yli_raja11)) |>
  left_join(kontek |> select(lam_id, kam_m = etaisyys_kameraan_m), by = "lam_id") |>
  left_join(st_drop_geometry(asemat) |> select(lam_id, tie = tienumero_lopullinen),
            by = "lam_id") |>
  mutate(across(starts_with("ka_"), \(x) round(x, 2)),
         across(starts_with("yli_"), \(x) round(100 * x, 2)),
         kam_m = round(kam_m),
         d_ka = round(ka_2026 - ka_2025, 2),
         d_yli6 = round(yli_raja6_2026 - yli_raja6_2025, 2)) |>
  select(lam_id, tie, kam_m, ka_2025, ka_2026, d_ka,
         yli_raja6_2025, yli_raja6_2026, d_yli6,
         yli_raja11_2025, yli_raja11_2026) |>
  arrange(tie, kam_m)

cat("ka = keskinopeus, yli_raja6 = osuus (%) joka ylittaa rajan >= 6 km/h\n")
cat("(6 km/h = pienin ylitys josta Vesa sai maksun teknisen vahennyksen jalkeen)\n\n")
print(tunnusluvut, n = 50, width = 200)

# ----------------------------------------------------------------------------
# 6. Piste 116: viikkosarja 2026 - nakyyko muutoskohta silmalla
# ----------------------------------------------------------------------------

viiva("6. PISTE 116 VIIKOITTAIN 2026")

viikkosarja <- hist_pv |>
  filter(lam_id == 116L, lk_ryhma == 1L, vapaa == 1L,
         pvm %in% arkipv, year(pvm) == 2026) |>
  left_join(rajoitus, by = "lam_id") |>
  mutate(vk = isoweek(pvm)) |>
  group_by(vk, suunta) |>
  summarise(
    pv = n_distinct(pvm),
    n = sum(as.numeric(n)),
    ka = sum(as.numeric(n) * nopeus) / sum(as.numeric(n)),
    p85 = {
      kum <- cumsum(as.numeric(n[order(nopeus)])) / sum(as.numeric(n))
      sort(nopeus)[which(kum >= 0.85)[1]]
    },
    yli6 = 100 * sum(as.numeric(n[nopeus >= raja + 6])) / sum(as.numeric(n)),
    .groups = "drop") |>
  mutate(across(c(ka, yli6), \(x) round(x, 2))) |>
  pivot_wider(names_from = suunta, values_from = c(pv, n, ka, p85, yli6))

print(viikkosarja, n = 40, width = 200)

# Sama taulukko levylle, jotta sen voi liittaa kokonaisena.
write_csv(viikkosarja, file.path(DATA_DIR, "piste116_viikkosarja_2026.csv"))
cat("\nTallennettu: ", file.path(DATA_DIR, "piste116_viikkosarja_2026.csv"), "\n")

# ----------------------------------------------------------------------------
# 7. Aineiston reunat
# ----------------------------------------------------------------------------

viiva("7. REUNAEHDOT")

cat("Paivavali: ", format(min(hist_pv$pvm)), " - ", format(max(hist_pv$pvm)), "\n")
cat("Pisteita: ", n_distinct(hist_pv$lam_id), "\n")

puuttuvat <- qd_read(file.path(DATA_DIR, "lam_puuttuvat.qs2"))
cat("Puuttuvia pistepaivia: ", nrow(puuttuvat), "\n")
if (nrow(puuttuvat) > 0) {
  print(puuttuvat |> count(lam_id, vuosi = year(pvm)) |>
          pivot_wider(names_from = vuosi, values_from = n, values_fill = 0),
        n = 30)
}

vajaat <- qd_read(file.path(DATA_DIR, "lam_vajaat_paivat.qs2"))
cat("\nVajaita paivia (alle 24 h): ", nrow(vajaat), "\n")
if (nrow(vajaat) > 0) print(vajaat |> count(lam_id, sort = TRUE), n = 30)

cat("\nKaistojen arvot: "); print(sort(unique(hist_pv$kaista)))
cat("Suuntien arvot: ");   print(sort(unique(hist_pv$suunta)))
cat("Nopeusvali: ", min(hist_pv$nopeus), " - ", max(hist_pv$nopeus), "\n")

viiva("VALMIS - kopioi koko tuloste")
