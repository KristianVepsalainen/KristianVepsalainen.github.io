# R/talvi/talvi-helpers.R
# Talvi-sarjan yhteiset apurit: kausimääritys, talvi-indikaattorit, tallennus.
# Ladataan sekä data-prep-skriptistä että postauksista.

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(readr)
  library(lubridate)
  library(here)
  library(qs2)
})

select <- dplyr::select
filter <- dplyr::filter

TALVI_DIR   <- here("data", "talvi")
TALVI_RAW   <- here("data", "talvi", "raw")     # raw/ on .gitignoressa
TALVI_PAIVA <- here("data", "talvi", "paiva")   # päivädata alueittain, gitissä

# ---------------------------------------------------------------------------
# Talvikausi: 1.7.–30.6. Kausi nimetään alkuvuoden mukaan, esim. 1990 = "1990-91".
# Heinäkuun alku valitaan, jotta syksyn ensilumi ja kevään sulaminen ovat
# samassa kaudessa.
# ---------------------------------------------------------------------------
talvikausi_alku <- function(pvm) {
  stopifnot(inherits(pvm, "Date"))
  y <- as.integer(year(pvm))
  as.integer(ifelse(month(pvm) >= 7L, y, y - 1L))
}

talvikausi_nimi <- function(alku) {
  sprintf("%d-%02d", alku, (alku + 1L) %% 100L)
}

kauden_paiva <- function(pvm) {
  # 1 = 1. heinäkuuta
  as.integer(pvm - as.Date(sprintf("%d-07-01", talvikausi_alku(pvm)))) + 1L
}

# Täyttää lyhyet (<= max_aukko pv) puuttuvien arvojen aukot edellisellä arvolla.
# Käytetään vain lumensyvyyteen, jotta yksittäinen puuttuva havainto ei katkaise
# pysyvää lumipeitettä kahtia.
tayta_aukot <- function(x, max_aukko = 3L) {
  if (!anyNA(x)) return(x)
  r <- rle(is.na(x))
  loput <- cumsum(r$lengths)
  alut  <- loput - r$lengths + 1L
  for (k in which(r$values & r$lengths <= max_aukko)) {
    if (alut[k] > 1L) x[alut[k]:loput[k]] <- x[alut[k] - 1L]
  }
  x
}

# Pisin yhtäjaksoinen TRUE-jakso. Palauttaa alku- ja loppuindeksin sekä pituuden.
pisin_jakso <- function(x) {
  x[is.na(x)] <- FALSE
  if (!any(x)) return(list(alku = NA_integer_, loppu = NA_integer_, pituus = 0L))
  r <- rle(x)
  loput <- cumsum(r$lengths)
  alut  <- loput - r$lengths + 1L
  i <- which(r$values)[which.max(r$lengths[r$values])]
  list(alku = alut[i], loppu = loput[i], pituus = r$lengths[i])
}

# ---------------------------------------------------------------------------
# Talvi-indikaattorit yhdelle asemalle ja kaudelle.
# Syöte: päivät (pvm, tmin, tmax, tday, rr, lumi_cm), kausi täydennettynä
# kaikilla kalenteripäivillä (puuttuvat NA).
#
# Lämpötilana käytetään kaikille lähteille samaa määritelmää
# tmean = (tmin + tmax) / 2, jotta Suomi, Kanada, USA ja Pohjoismaat ovat
# vertailukelpoisia (USA:n asemilla ei useimmiten ole todellista vuorokauden
# keskilämpöä). FMI:n oikea vuorokausikeskiarvo tday säilytetään erikseen.
#
# Lukumäärät ja summat skaalataan havaintokattavuudella, ja ne asetetaan NA:ksi,
# jos talvijakson (loka–huhtikuu) kattavuus jää alle 90 %.
# ---------------------------------------------------------------------------
KATTAVUUS_RAJA <- 0.90
TALVIKUUT <- c(10L, 11L, 12L, 1L, 2L, 3L, 4L)

kauden_indikaattorit <- function(d) {
  stopifnot(all(c("pvm", "tmin", "tmax", "rr", "lumi_cm") %in% names(d)),
            !anyDuplicated(d$pvm))
  d <- arrange(d, pvm)
  tmean <- (d$tmin + d$tmax) / 2
  talvi <- month(d$pvm) %in% TALVIKUUT
  djf   <- month(d$pvm) %in% c(12L, 1L, 2L)

  n_talvi <- sum(talvi)
  kat_t    <- sum(!is.na(tmean[talvi])) / n_talvi
  kat_lumi <- sum(!is.na(d$lumi_cm[talvi])) / n_talvi
  kat_rr   <- sum(!is.na(d$rr[talvi])) / n_talvi
  kat_koko <- sum(!is.na(tmean)) / nrow(d)

  # Skaalattu lukumäärä: havaittu määrä / kattavuus, NA jos kattavuus liian pieni
  skaalaa <- function(n, kat) if (is.na(kat) || kat < KATTAVUUS_RAJA) NA_real_ else n / kat

  lumi_t <- tayta_aukot(d$lumi_cm)
  pj <- pisin_jakso(lumi_t >= 1)

  hdd_paiva <- ifelse(tmean <= 15, 18 - tmean, 0)   # Eurostatin määritelmä

  tibble(
    n_paivia          = nrow(d),
    kattavuus_t       = kat_t,
    kattavuus_lumi    = kat_lumi,
    kattavuus_rr      = kat_rr,
    tmean_djf         = if (sum(!is.na(tmean[djf])) / sum(djf) >= KATTAVUUS_RAJA)
                          mean(tmean[djf], na.rm = TRUE) else NA_real_,
    fdd               = skaalaa(sum(pmax(0, -tmean[talvi]), na.rm = TRUE), kat_t),
    hdd               = skaalaa(sum(hdd_paiva, na.rm = TRUE), kat_koko),
    pakkaspaivat      = skaalaa(sum(d$tmin[talvi] < 0, na.rm = TRUE), kat_t),
    kovat_pakkaset    = skaalaa(sum(d$tmin[talvi] <= -20, na.rm = TRUE), kat_t),
    nollanylitykset   = skaalaa(sum(d$tmin[talvi] < 0 & d$tmax[talvi] > 0, na.rm = TRUE), kat_t),
    lumipaivat        = skaalaa(sum(d$lumi_cm[talvi] >= 1, na.rm = TRUE), kat_lumi),
    max_lumi_cm       = if (kat_lumi >= KATTAVUUS_RAJA) max(d$lumi_cm, na.rm = TRUE) else NA_real_,
    pysyva_lumi_alku  = if (kat_lumi >= KATTAVUUS_RAJA && pj$pituus > 0) d$pvm[pj$alku] else as.Date(NA),
    pysyva_lumi_loppu = if (kat_lumi >= KATTAVUUS_RAJA && pj$pituus > 0) d$pvm[pj$loppu] else as.Date(NA),
    pysyva_lumi_pv    = if (kat_lumi >= KATTAVUUS_RAJA) as.numeric(pj$pituus) else NA_real_,
    # Vesisade lumipeitteen päälle: karkea mittari kuoren ja jääkerroksen riskille
    # (porolaitumet, liukkaus). Ehto: sadetta >= 1 mm, tmax > 0 ja lunta maassa.
    vesisade_lumelle  = skaalaa(sum(talvi & d$rr >= 1 & d$tmax > 0 & d$lumi_cm >= 1,
                                    na.rm = TRUE),
                                min(kat_t, kat_lumi, kat_rr))
  )
}

# ---------------------------------------------------------------------------
# Tallennus: atominen kirjoitus (väliaikaistiedosto -> rename), varoitus jos
# tiedosto lähestyy GitHubin 100 Mt rajaa.
# ---------------------------------------------------------------------------
tallenna_atom <- function(x, polku) {
  stopifnot(is.data.frame(x))
  dir.create(dirname(polku), recursive = TRUE, showWarnings = FALSE)
  tmp <- paste0(polku, ".tmp")
  qd_save(x, tmp, compress_level = 9L)
  if (file.exists(polku)) unlink(polku)
  ok <- file.rename(tmp, polku)
  stopifnot(ok)
  mt <- file.size(polku) / 1024^2
  if (mt > 45) warning(sprintf("%s on %.1f Mt (yli 45 Mt), pilko pienemmäksi.", basename(polku), mt))
  invisible(polku)
}

# Lukee kaikki päivädatatiedostot (tai valitut lähteet) yhdeksi tauluksi.
lue_paiva <- function(lahteet = NULL) {
  tiedostot <- list.files(TALVI_PAIVA, pattern = "\\.qs2$", full.names = TRUE)
  if (!is.null(lahteet)) {
    tiedostot <- tiedostot[sub("_.*$", "", basename(tiedostot)) %in% lahteet]
  }
  stopifnot(length(tiedostot) > 0)
  map(tiedostot, qd_read) |> list_rbind()
}
