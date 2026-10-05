# =============================================================================
# 01c-kokoa-valimuisti.R
#
# Peltipoliisi-sarja. Korvaa 01b-hae-raakadata.R:n kokoamisvaiheen.
#
# MITA TAPAHTUI: lataus onnistui kokonaan (7216 valimuistitiedostoa =
# 22 pistetta x 328 paivaa). Kaatuminen tuli kokoamisessa, jossa
#     palat <- map(odotetut, qs_read)
# luki KAIKKI tiedostot muistiin yhta aikaa, ja vasta sen jalkeen
# map_dfr() teki niista toisen kopion. Tunnin tarkkuudella pidetty
# nopeushistogrammi on noin 6 000 rivia per pistepaiva, eli yhteensa
# luokkaa 40 miljoonaa rivia - useita gigatavuja.
#
# RATKAISU: valimuistia ei tarvitse ladata uudelleen. Tiedostot luetaan
# erissa ja tiivistetaan heti, eika koko aineistoa pideta kerralla
# muistissa. Lopputuloksena kolme aineistoa, joista jokaisella on oma
# tehtavansa:
#
#   lam_hist_pv   paivatason nopeushistogrammi, kaikki ajoneuvoluokat
#                 -> jakauman muoto, ylapaan osuudet, muutoskohta
#   lam_tunti     tuntitason tunnusluvut, vain henkiloautot
#                 -> vuorokaudenaika, ruuhkan vaikutus
#   lam_hist_h    tuntitason TAYSI histogrammi vain fokuspisteille
#                 -> kenguru-ilmio ja muut tarkat tarkastelut
#
# Aja tama 01b:n tilalle. Lataus ohitetaan, koska valimuisti on valmis.
# =============================================================================

library(here)
library(tidyverse)
library(qs2)
library(lubridate)

select <- dplyr::select
filter <- dplyr::filter

PROJEKTI  <- "peltipoliisi"
DATA_DIR  <- here("data", PROJEKTI)
CACHE_DIR <- file.path(DATA_DIR, "cache")

# Pisteet, joilta sailytetaan tuntitason taysi histogrammi.
# Leppasolmu (haasteen kohde) + sen naapurit lisataan alla etaisyyden mukaan.
FOKUSPISTEET <- c(116L)

# Montako valimuistitiedostoa luetaan kerralla. Pienenna jos muisti loppuu.
ERA_KOKO <- 100L

# ----------------------------------------------------------------------------
# 0. Paljonko dataa on tulossa? Mitataan, ei arvata.
# ----------------------------------------------------------------------------

tiedostot <- list.files(CACHE_DIR, pattern = "^agg_.*\\.qs2$", full.names = TRUE)
stopifnot(length(tiedostot) > 0)

message("Valimuistitiedostoja: ", length(tiedostot))
message(sprintf("Valimuistin koko levylla: %.1f GB",
                sum(file.size(tiedostot)) / 1024^3))

# Otos: luetaan muutama tiedosto ja katsotaan paljonko rivaja niissa on.
set.seed(1)
otos <- sample(tiedostot, min(10, length(tiedostot)))
otos_tiedot <- map_dfr(otos, function(p) {
  x <- qs_read(p)
  if (x$status != "ok") return(tibble(hist_rivit = 0L, tunti_rivit = 0L))
  tibble(hist_rivit = nrow(x$hist), tunti_rivit = nrow(x$tunti))
})

ka_hist <- mean(otos_tiedot$hist_rivit)
message(sprintf(
  "\nOtos (%d tiedostoa): keskimaarin %.0f histogrammirivia / pistepaiva.",
  nrow(otos_tiedot), ka_hist))
message(sprintf(
  "Koko aineisto tunnin tarkkuudella olisi noin %.0f miljoonaa rivia.",
  ka_hist * length(tiedostot) / 1e6))
message("Siksi tunnin tarkkuus sailytetaan vain fokuspisteille.\n")

rm(otos_tiedot); gc(verbose = FALSE)

# ----------------------------------------------------------------------------
# 1. Luku erissa ja tiivistys lennossa
# ----------------------------------------------------------------------------

# Vuorokaudenaikaluokka. Ruuhkatunnit erikseen, koska ruuhkassa nopeutta
# rajoittaa liikenne eika kuljettaja.
aikaluokka <- function(tunti) {
  case_when(
    tunti >= 6  & tunti <  9 ~ 1L,   # aamuruuhka
    tunti >= 9  & tunti < 15 ~ 2L,   # paiva
    tunti >= 15 & tunti < 18 ~ 3L,   # iltaruuhka
    TRUE                     ~ 4L    # ilta ja yo
  )
}

erat <- split(tiedostot, ceiling(seq_along(tiedostot) / ERA_KOKO))

kerays_hist_pv <- vector("list", length(erat))
kerays_tunti   <- vector("list", length(erat))
kerays_hist_h  <- vector("list", length(erat))
kerays_diag    <- vector("list", length(erat))
kerays_puuttuu <- vector("list", length(erat))

message("Kasitellaan ", length(erat), " eraa (", ERA_KOKO, " tiedostoa / era)")

for (i in seq_along(erat)) {

  palat <- map(erat[[i]], qs_read)

  ok <- keep(palat, \(x) x$status == "ok")

  kerays_puuttuu[[i]] <- palat |>
    keep(\(x) x$status == "puuttuu") |>
    map_dfr(\(x) tibble(lam_id = x$lam_id, pvm = x$pvm))

  if (length(ok) > 0) {

    kerays_diag[[i]] <- map_dfr(ok, "diagnostiikka")

    h <- map_dfr(ok, "hist")

    # (a) Paivataso, kaikki luokat. Tunti summataan pois.
    kerays_hist_pv[[i]] <- h |>
      group_by(lam_id, pvm, suunta, kaista, lk_ryhma, vapaa, nopeus) |>
      summarise(n = sum(n), .groups = "drop") |>
      mutate(across(c(lam_id, suunta, kaista, lk_ryhma, vapaa, nopeus, n),
                    as.integer))

    # (b) Tuntitaso, taysi histogrammi, vain fokuspisteet.
    kerays_hist_h[[i]] <- h |>
      filter(lam_id %in% FOKUSPISTEET) |>
      mutate(across(c(lam_id, tunti, suunta, kaista, lk_ryhma, vapaa,
                      nopeus, n), as.integer))

    rm(h)

    # (c) Tuntitunnusluvut, vain henkiloautot (lk_ryhma 1).
    kerays_tunti[[i]] <- map_dfr(ok, "tunti") |>
      filter(lk_ryhma == 1L) |>
      mutate(aikaluokka = aikaluokka(tunti))
  }

  rm(palat, ok); gc(verbose = FALSE)

  if (i %% 10 == 0 || i == length(erat)) {
    message(sprintf("  era %d / %d  (muistissa %.0f MB)",
                    i, length(erat), sum(gc()[, 2])))
  }
}

# ----------------------------------------------------------------------------
# 2. Yhdistys ja tallennus
# ----------------------------------------------------------------------------

lam_hist_pv <- bind_rows(kerays_hist_pv)
rm(kerays_hist_pv); gc(verbose = FALSE)

# Erien rajalla sama pistepaiva ei voi jakautua kahteen eraan, koska yksi
# tiedosto = yksi pistepaiva. Varmistetaan silti.
stopifnot(!any(duplicated(
  lam_hist_pv[c("lam_id","pvm","suunta","kaista","lk_ryhma","vapaa","nopeus")])))

qd_save(lam_hist_pv, file.path(DATA_DIR, "lam_hist_pv.qs2"))
message(sprintf("\nlam_hist_pv: %.1f M rivia, %.0f MB levylla",
                nrow(lam_hist_pv) / 1e6,
                file.size(file.path(DATA_DIR, "lam_hist_pv.qs2")) / 1024^2))

lam_tunti <- bind_rows(kerays_tunti)
rm(kerays_tunti); gc(verbose = FALSE)
stopifnot(!any(duplicated(
  lam_tunti[c("lam_id","pvm","tunti","suunta","kaista","lk_ryhma")])))
qd_save(lam_tunti, file.path(DATA_DIR, "lam_tunti.qs2"))
message(sprintf("lam_tunti:   %.1f M rivia, %.0f MB levylla",
                nrow(lam_tunti) / 1e6,
                file.size(file.path(DATA_DIR, "lam_tunti.qs2")) / 1024^2))

lam_hist_h <- bind_rows(kerays_hist_h)
rm(kerays_hist_h); gc(verbose = FALSE)
qd_save(lam_hist_h, file.path(DATA_DIR, "lam_hist_h.qs2"))
message(sprintf("lam_hist_h:  %.1f M rivia, %.0f MB levylla",
                nrow(lam_hist_h) / 1e6,
                file.size(file.path(DATA_DIR, "lam_hist_h.qs2")) / 1024^2))

lam_diag <- bind_rows(kerays_diag)
puuttuvat <- bind_rows(kerays_puuttuu)
rm(kerays_diag, kerays_puuttuu); gc(verbose = FALSE)

qd_save(lam_diag, file.path(DATA_DIR, "lam_diagnostiikka.qs2"))
qd_save(puuttuvat, file.path(DATA_DIR, "lam_puuttuvat.qs2"))

# ----------------------------------------------------------------------------
# 3. Tarkistukset
# ----------------------------------------------------------------------------

# Havaintomaarat tasmaavat paivahistogrammin ja tuntitaulun valilla
# (henkiloautot, jotka ovat molemmissa).
tark <- full_join(
  lam_hist_pv |> filter(lk_ryhma == 1L) |>
    group_by(lam_id, pvm, suunta, kaista) |>
    summarise(n_hist = sum(n), .groups = "drop"),
  lam_tunti |>
    group_by(lam_id, pvm, suunta, kaista) |>
    summarise(n_tunti = sum(n), .groups = "drop"),
  by = c("lam_id","pvm","suunta","kaista"))

eroavat <- tark |> filter(is.na(n_hist) | is.na(n_tunti) | n_hist != n_tunti)
if (nrow(eroavat) > 0) {
  print(head(eroavat, 20))
  stop("Paivahistogrammin ja tuntitaulun havaintomaarat eivat tasmaa.")
}
message("\nTarkistus: havaintomaarat tasmaavat aineistojen valilla.")
rm(tark, eroavat); gc(verbose = FALSE)

# Aikavalin yksikko: suhteen pitaisi olla noin 1000 (millisekunti).
message(sprintf("Aikavalin yksikkosuhde: mediaani %.0f (odotettu ~1000)",
                median(lam_diag$suhde_aikavali, na.rm = TRUE)))

message(sprintf("Faulty-havaintojen osuus: mediaani %.2f %%, max %.1f %%",
                100 * median(lam_diag$osuus_faulty),
                100 * max(lam_diag$osuus_faulty)))

message("Puuttuvia pistepaivia: ", nrow(puuttuvat))
if (nrow(puuttuvat) > 0) {
  print(puuttuvat |> count(lam_id, vuosi = year(pvm)) |>
          pivot_wider(names_from = vuosi, values_from = n, values_fill = 0))
}

# ----------------------------------------------------------------------------
# 4. Kattavuusraportti
# ----------------------------------------------------------------------------

asemat <- qs_read(file.path(DATA_DIR, "lam_asemat.qs2"))

kattavuus <- lam_hist_pv |>
  mutate(vuosi = year(pvm)) |>
  group_by(lam_id, vuosi) |>
  summarise(paivia = n_distinct(pvm),
            ajoneuvoa_pv = round(sum(n) / n_distinct(pvm)),
            .groups = "drop") |>
  pivot_wider(names_from = vuosi,
              values_from = c(paivia, ajoneuvoa_pv)) |>
  left_join(sf::st_drop_geometry(asemat) |>
              select(lam_id, nimi, tienumero_lopullinen), by = "lam_id") |>
  arrange(tienumero_lopullinen, lam_id)

message("\n=== Kattavuus pisteittain ===")
print(kattavuus, n = 100)

vajaat_paivat <- lam_tunti |>
  group_by(lam_id, pvm) |>
  summarise(tunteja = n_distinct(tunti), .groups = "drop") |>
  filter(tunteja < 24)

message("Vajaita paivia (alle 24 h dataa): ", nrow(vajaat_paivat))
qd_save(vajaat_paivat, file.path(DATA_DIR, "lam_vajaat_paivat.qs2"))

message("\nValmis. Jatka 01b:n jaksoista 3-5 (OSM, saa, kalenteri) -")
message("ne eivat riipu tasta vaiheesta eivatka kuormita muistia.")
