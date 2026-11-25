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
  attr(x, "problems") <- NULL   # readr:n ulkoiset osoittimet eivät tallennu qs2:een
  attr(x, "spec") <- NULL
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

# ---------------------------------------------------------------------------
# Alueen "tyypillisen sääaseman" talvi: y ~ asema + talvi alueittain, ennuste
# kaikille alueen asemille kunakin talvena, keskiarvona. Asemalla vähintään
# 10 kelvollista talvea. Syöte: alue_id, asema_id, kausi_alku ja mittari.
# ---------------------------------------------------------------------------
alue_sarja <- function(data, mittari) {
  d0 <- data |>
    filter(!is.na(.data[[mittari]])) |>
    group_by(asema_id) |> filter(n() >= 10) |> ungroup() |>
    transmute(alue_id, asema_id, kausi_alku, y = .data[[mittari]])
  d0 |>
    group_by(alue_id) |>
    group_modify(\(d, k) {
      n_as <- d |> count(kausi_alku, name = "n_asemat")
      if (n_distinct(d$asema_id) == 1L) {
        return(d |> transmute(kausi_alku, y) |> left_join(n_as, by = "kausi_alku"))
      }
      fit <- lm(y ~ factor(asema_id) + factor(kausi_alku), data = d)
      if (fit$rank < length(coef(fit))) {
        warning(sprintf("%s: asemien mittausjaksot eivät limity, käytetään keskiarvoa.", k$alue_id))
        return(d |> group_by(kausi_alku) |> summarise(y = mean(y), .groups = "drop") |>
                 left_join(n_as, by = "kausi_alku"))
      }
      grid <- expand_grid(asema_id = unique(d$asema_id), kausi_alku = unique(d$kausi_alku))
      grid$p <- predict(fit, newdata = grid)
      grid |> group_by(kausi_alku) |> summarise(y = mean(p), .groups = "drop") |>
        left_join(n_as, by = "kausi_alku")
    }) |>
    ungroup()
}

# ---------------------------------------------------------------------------
# Bayesilainen lineaarinen regressio tietämättömällä priorilla p(b, sigma) ∝ 1/sigma.
# Posteriori suljetussa muodossa: sigma^2 ~ skaalattu käänteinen khiin neliö,
# b | sigma ~ N(b_hat, sigma^2 (X'X)^-1). Ei MCMC:tä, laskenta sekunneissa.
# Palauttaa arvonnat (b: n_draws x p, sigma) sekä OLS-tunnusluvut, jotka
# vastaavat klassista t-testiä (sama jakauma tällä priorilla).
# ---------------------------------------------------------------------------
bayes_lm <- function(X, y, n_draws = 4000L) {
  stopifnot(is.matrix(X), length(y) == nrow(X), !anyNA(X), !anyNA(y))
  n <- nrow(X); p <- ncol(X)
  qx <- qr(X)
  if (qx$rank < p) stop("bayes_lm: suunnittelumatriisi ei ole täysiasteinen (", qx$rank, " < ", p, ").")
  b_hat <- qr.coef(qx, y)
  XtX_inv <- chol2inv(qr.R(qx))[order(qx$pivot), order(qx$pivot)]
  df <- n - p
  s2 <- sum((y - drop(X %*% b_hat))^2) / df
  sigma2 <- df * s2 / rchisq(n_draws, df = df)
  z <- matrix(rnorm(n_draws * p), n_draws) %*% chol(XtX_inv)
  b <- sweep(z * sqrt(sigma2), 2, b_hat, "+")
  colnames(b) <- colnames(X)
  se <- sqrt(diag(XtX_inv) * s2)
  list(b = b, sigma = sqrt(sigma2), b_hat = setNames(b_hat, colnames(X)), se = setNames(se, colnames(X)),
       df = df, p_arvo = setNames(2 * pt(-abs(b_hat / se), df), colnames(X)))
}

# ---------------------------------------------------------------------------
# Tilastokeskuksen PxWebistä selaimella ladattu CSV (taulukkomuoto: aika
# sarakkeissa, esim. "Liikevaihto, alkuperäinen indeksi 2024M01" tai
# "Liikevaihto (1000 euroa) 2023"). Erotin tunnistetaan (sarkain, puolipiste,
# pilkku), puuttuva = "." ym., ennakkotieto = "*". Palauttaa pitkän taulun:
# id-sarakkeet sellaisinaan + tieto, aika, pvm, ennakko, arvo.
# ---------------------------------------------------------------------------
PX_PUUTTUVA <- c("", ".", "..", "...", "-", "x")

px_lue <- function(polku) {
  stopifnot(file.exists(polku))
  # Merkistö: UTF-8 tai (vanhat passiivitaulut) ISO-8859-1
  tavut <- readBin(polku, "raw", n = min(file.size(polku), 200000L))
  koodaus <- if (validUTF8(rawToChar(tavut[tavut != as.raw(0)]))) "UTF-8" else "ISO-8859-1"
  eka <- readLines(polku, n = 1L, encoding = koodaus, warn = FALSE)
  erotin <- if (grepl("\t", eka)) "\t" else if (grepl(";", eka)) ";" else ","
  raaka <- read_delim(polku, delim = erotin, col_types = cols(.default = col_character()),
                      na = PX_PUUTTUVA, locale = locale(encoding = koodaus),
                      progress = FALSE, show_col_types = FALSE, trim_ws = TRUE,
                      name_repair = "unique_quiet")
  aika_re <- "(^|\\s)(\\d{4}(M\\d{2}|Q\\d|/\\d{2})?)\\*?$"
  aika_rivi_re <- "^\\d{4}(M\\d{2}|Q\\d|/\\d{2})?\\*?$"
  aika_esi_re <- "^(\\d{4}(M\\d{2}|Q\\d|/\\d{2})?)(\\*?)\\s+(.+)$"
  aika_sar <- names(raaka)[grepl(aika_re, names(raaka))]
  esi_sar <- names(raaka)[grepl(aika_esi_re, names(raaka))]
  if (!length(aika_sar) && length(esi_sar)) {
    # Aika sarakeotsikon alussa: "2003 Kuolleet", "2019* Loukkaantuneet"
    id_sar <- setdiff(names(raaka), esi_sar)
    pitka <- raaka |>
      pivot_longer(all_of(esi_sar), names_to = "sarake", values_to = "arvo_txt") |>
      mutate(tieto = sub(aika_esi_re, "\\4", sarake), aika_txt = sub(aika_esi_re, "\\1", sarake),
             ennakko = sub(aika_esi_re, "\\3", sarake) == "*")
  } else if (length(aika_sar)) {
    # Aika sarakkeissa: "Tieto 2024M01"
    id_sar <- setdiff(names(raaka), aika_sar)
    pitka <- raaka |>
      pivot_longer(all_of(aika_sar), names_to = "sarake", values_to = "arvo_txt") |>
      mutate(tieto = sub(aika_re, "", sarake), aika_txt = sub(paste0("^.*?", aika_re), "\\2", sarake, perl = TRUE),
             ennakko = grepl("\\*$", sarake))
  } else {
    # Aika riveillä jossakin sarakkeessa (esim. "Kuukausi", "Vuosi").
    # Muut tekstisarakkeet ovat ulottuvuuksia (esim. "Tieto", "Omistajaryhmä",
    # "Maakunta") ja numeeriset sarakkeet mittareita; mittarin nimi -> tieto.
    # Kattaa myös muodon, jossa aika on ensimmäinen sarake ja sarakenimessä on
    # kaikki muut ulottuvuudet.
    on_aika <- function(x) { x <- x[!is.na(x)]; length(x) > 0 && all(grepl(aika_rivi_re, x)) }
    on_luku <- function(x) {
      x <- x[!is.na(x)]
      all(!is.na(suppressWarnings(as.numeric(gsub(",", ".", gsub("[[:space:] ]", "", x))))))
    }
    aika_ehd <- names(raaka)[vapply(raaka, on_aika, logical(1))]
    if (!length(aika_ehd))
      stop(basename(polku), ": aikaa ei löytynyt sarakkeista ('Tieto 2024M01') eikä riveiltä ('2024M01').")
    aika1 <- aika_ehd[1]
    muut <- setdiff(names(raaka), aika1)
    mittarit <- muut[vapply(raaka[muut], on_luku, logical(1))]
    id_sar <- setdiff(muut, mittarit)
    if (!length(mittarit)) stop(basename(polku), ": numeerisia sarakkeita ei löytynyt.")
    pitka <- raaka |>
      rename(aika_raaka = all_of(aika1)) |>
      pivot_longer(all_of(mittarit), names_to = "tieto", values_to = "arvo_txt") |>
      mutate(ennakko = grepl("\\*$", aika_raaka), aika_txt = sub("\\*$", "", aika_raaka)) |>
      select(-aika_raaka)
  }
  out <- pitka |>
    mutate(aika = aika_txt,
           kk = case_when(grepl("M|/", aika) ~ as.integer(substr(aika, 6, 7)),
                          grepl("Q", aika) ~ (as.integer(substr(aika, 6, 6)) - 1L) * 3L + 1L,
                          TRUE ~ 1L),
           pvm = as.Date(sprintf("%s-%02d-01", substr(aika, 1, 4), kk), format = "%Y-%m-%d"),
           arvo = suppressWarnings(as.numeric(gsub(",", ".", gsub("[[:space:]\u00a0]", "", arvo_txt)))))
  huonot <- out |> filter(!is.na(arvo_txt), is.na(arvo)) |> distinct(arvo_txt)
  if (nrow(huonot)) stop(basename(polku), ": numeroksi muuttumattomia arvoja: ",
                         paste(utils::head(huonot$arvo_txt), collapse = ", "))
  stopifnot(!anyNA(out$pvm))
  out |> select(all_of(id_sar), tieto, aika, pvm, ennakko, arvo)
}
