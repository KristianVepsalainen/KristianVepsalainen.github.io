# =============================================================================
# R/peltipoliisi-yhteiset.R
#
# Peltipoliisi-sarjan yhteiset apufunktiot ja johdetut aineistot.
# Sourcetaan jokaisen osan alussa: source(here("R", "peltipoliisi-yhteiset.R"))
#
# Ei tulosta mitaan, ei aja malleja. Maarittelee vain.
# =============================================================================

library(here)
library(tidyverse)
library(qs2)
library(lubridate)
library(sf)

select <- dplyr::select
filter <- dplyr::filter

PP_DATA <- here("data", "peltipoliisi")

# Muutoskohta, joka paateltiin bayesilaisella mallilla sarjan ensimmaisessa
# osassa. Kaytetaan kaikissa osissa samaa, jotta tulokset ovat vertailukelpoisia.
PP_MUUTOS <- as.Date("2026-08-25")
PP_VK_MUUTOS <- isoweek(PP_MUUTOS)

# Ennen- ja jalkeen-jaksot viikkoina. Viikot 34-35 jatetaan pois, koska
# muutoskohta osuu niihin eika niita voi lukea kummaksikaan.
PP_VK_ENNEN   <- 28:33
PP_VK_JALKEEN <- 36:39

# --- Varit -------------------------------------------------------------------
# Validoitu varinaon poikkeamien varalta: pienin erottuvuus vierekkaisten
# varien valilla protanopiassa dE 22.8, normaalinaossa dE 29.2.
PP_VARI_KEHA     <- "#2166ac"
PP_VARI_VERROKKI <- "#d95f02"
PP_VARI_KOLMAS   <- "#7b3294"
PP_VARI_MUSTE    <- "#2b2b2b"
PP_VARI_HIMMEA   <- "#8a8a8a"

pp_teema <- function() {
  theme_minimal(base_size = 12) +
    theme(
      plot.title    = element_text(face = "bold", size = 13,
                                   colour = PP_VARI_MUSTE),
      plot.subtitle = element_text(size = 11, colour = PP_VARI_HIMMEA,
                                   margin = margin(b = 10)),
      plot.caption  = element_text(size = 9, colour = PP_VARI_HIMMEA,
                                   hjust = 0),
      axis.title    = element_text(size = 10, colour = PP_VARI_HIMMEA),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(linewidth = 0.3, colour = "grey88"),
      legend.position = "top",
      legend.title = element_blank()
    )
}

# --- Apufunktiot -------------------------------------------------------------

# Painotettu kvantiili histogrammista. nopeus saa toistua (eri kaistat).
pp_wq <- function(nopeus, paino, q) {
  o <- order(nopeus); v <- nopeus[o]; w <- as.numeric(paino)[o]
  v[which(cumsum(w) / sum(w) >= q)[1]]
}

# Tunnusluvut histogrammista. HUOM: koostetta ei nimeta samaksi kuin
# lahdesaraketta, koska summarise() evaluoi lausekkeet jarjestyksessa.
pp_tunnusluvut <- function(d, ...) {
  d |>
    group_by(...) |>
    summarise(
      havainnot = sum(as.numeric(n)),
      ka   = sum(as.numeric(n) * nopeus) / sum(as.numeric(n)),
      p50  = pp_wq(nopeus, n, 0.50),
      p85  = pp_wq(nopeus, n, 0.85),
      p95  = pp_wq(nopeus, n, 0.95),
      yli6 = 100 * sum(as.numeric(n)[nopeus >= raja + 6]) / sum(as.numeric(n)),
      .groups = "drop")
}

# --- Aineistojen lataus ------------------------------------------------------

pp_lataa <- function() {
  tiedostot <- c(hist_pv = "lam_hist_pv.qs2", hist_h = "lam_hist_h.qs2",
                 tunti = "lam_tunti.qs2", kalenteri = "kalenteri.qs2",
                 kontek = "piste_konteksti.qs2", asemat = "lam_asemat.qs2",
                 osm = "osm_kamerat_tiet.qs2")
  puuttuu <- tiedostot[!file.exists(file.path(PP_DATA, tiedostot))]
  if (length(puuttuu) > 0)
    stop("Aineistoja puuttuu kansiosta ", PP_DATA, ": ",
         paste(puuttuu, collapse = ", "))

  list(
    hist_pv   = qd_read(file.path(PP_DATA, "lam_hist_pv.qs2")),
    hist_h    = qd_read(file.path(PP_DATA, "lam_hist_h.qs2")),
    tunti     = qd_read(file.path(PP_DATA, "lam_tunti.qs2")),
    kalenteri = qd_read(file.path(PP_DATA, "kalenteri.qs2")),
    kontek    = qd_read(file.path(PP_DATA, "piste_konteksti.qs2")),
    asemat    = qs_read(file.path(PP_DATA, "lam_asemat.qs2")),
    osm       = qs_read(file.path(PP_DATA, "osm_kamerat_tiet.qs2"))
  )
}

pp <- pp_lataa()

# --- Johdetut perusjoukot ----------------------------------------------------

pp_rajoitus <- pp$kontek |>
  mutate(raja = as.integer(osm_maxspeed)) |>
  select(lam_id, raja)

stopifnot(!any(is.na(pp_rajoitus$raja)))

pp_kehaI <- pp$asemat |> st_drop_geometry() |>
  filter(tienumero_lopullinen == 101) |> pull(lam_id)

pp_arkipv <- pp$kalenteri |> filter(arkipaiva, !arkipyha) |> pull(pvm)

# Analyysijoukko: henkiloautot, vapaa liikennevirta, arkipaivat.
pp_pohja <- pp$hist_pv |>
  filter(lk_ryhma == 1L, vapaa == 1L, pvm %in% pp_arkipv) |>
  left_join(pp_rajoitus, by = "lam_id")

# Pisteet joilta on riittava kattavuus molempina vuosina.
pp_taydet <- pp$hist_pv |>
  mutate(vuosi = year(pvm)) |>
  group_by(lam_id, vuosi) |>
  summarise(pv = n_distinct(pvm), .groups = "drop") |>
  pivot_wider(names_from = vuosi, values_from = pv) |>
  filter(!is.na(`2025`), !is.na(`2026`), `2025` > 140, `2026` > 140) |>
  pull(lam_id)

# Liikennemaaran muutos. Yli 8 % muutos = tien fyysinen tilanne on muuttunut,
# jolloin nopeuksia ei voi verrata viime vuoteen.
pp_volyymi <- pp$hist_pv |>
  filter(pvm %in% pp_arkipv, lam_id %in% pp_taydet) |>
  mutate(vuosi = year(pvm)) |>
  group_by(lam_id, vuosi) |>
  summarise(ajon_pv = sum(as.numeric(n)) / n_distinct(pvm), .groups = "drop") |>
  pivot_wider(names_from = vuosi, values_from = ajon_pv, names_prefix = "v") |>
  mutate(muutos_pros = 100 * (v2026 / v2025 - 1))

pp_epavakaat <- pp_volyymi |> filter(abs(muutos_pros) > 8) |> pull(lam_id)
pp_vakaat    <- setdiff(pp_taydet, pp_epavakaat)

# --- Vuosierotus viikoittain, vapaasti ryhmiteltavissa -----------------------
# Jokaiselle ryhmalle lasketaan sama viikko 2026 miinus 2025. Tama poistaa
# kausivaihtelun. Ryhmittelymuuttujat annetaan ...-argumenttina.

pp_yoy <- function(d = pp_pohja, ..., min_havainnot = 2000) {
  d |>
    mutate(vuosi = year(pvm), vk = isoweek(pvm)) |>
    pp_tunnusluvut(..., vuosi, vk) |>
    filter(havainnot > min_havainnot) |>
    select(..., vuosi, vk, ka, p85, p95, yli6) |>
    pivot_wider(names_from = vuosi, values_from = c(ka, p85, p95, yli6)) |>
    filter(!is.na(ka_2025), !is.na(ka_2026)) |>
    mutate(d_ka   = ka_2026 - ka_2025,
           d_p85  = p85_2026 - p85_2025,
           d_yli6 = yli6_2026 - yli6_2025,
           jakso  = case_when(vk %in% PP_VK_ENNEN   ~ "ennen",
                              vk %in% PP_VK_JALKEEN ~ "jalkeen",
                              TRUE ~ NA_character_),
           jalkeen = as.integer(vk >= PP_VK_MUUTOS))
}

# Ennen/jalkeen-muutos annetuilla ryhmilla: erotusten erotus kausikorjattuna.
pp_ennen_jalkeen <- function(yoy, ...) {
  yoy |>
    filter(!is.na(jakso)) |>
    group_by(..., jakso) |>
    summarise(d_yli6 = mean(d_yli6), d_ka = mean(d_ka),
              d_p85 = mean(d_p85), .groups = "drop") |>
    pivot_wider(names_from = jakso,
                values_from = c(d_yli6, d_ka, d_p85)) |>
    mutate(muutos_yli6 = d_yli6_jalkeen - d_yli6_ennen,
           muutos_ka   = d_ka_jalkeen - d_ka_ennen,
           muutos_p85  = d_p85_jalkeen - d_p85_ennen)
}
