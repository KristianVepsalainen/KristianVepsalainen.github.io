# R/talvi/talvi-elinkeino-data.R
# Talvi-sarjan osa 4 (talven elinkeinot: metsä, poro, jäätiet): datan keruu.
#
# Vaiheet (TALVI_RUN, oletus kaikki):
#   hakkuut     Luken teollisuuspuun hakkuut kuukausittain (selaimella ladattu CSV)
#               -> data/talvi/luke_hakkuut_kk.qs2
#   hakkuut_mk  Luken teollisuuspuun hakkuut maakunnittain vuosittain (painot säälle)
#               -> data/talvi/luke_hakkuut_maakunta.qs2
#   jaatiet     Luoteisterritorioiden talviteiden ja jääsiltojen avaus- ja sulkemispäivät
#               2000/01–2024/25 (GNWT:n liikenneraportti 2024, purettu CSV:ksi)
#               -> data/talvi/nwt_talvitiet.qs2
#   poro        Paliskuntain yhdistyksen tilastot (selaimella ladattu, ks. alla)
#               -> data/talvi/poro_raaka.qs2
#
# Käyttö:  Rscript R/talvi/talvi-elinkeino-data.R
#          TALVI_RUN=hakkuut,hakkuut_mk Rscript R/talvi/talvi-elinkeino-data.R
#
# Jäätie- ja porovaiheet tulostavat aineiston rakenteen; postauksen jäsennys
# tehdään vasta, kun rakenne on nähty.
#
# --- Selaimella ladattavat taulut (statdb.luke.fi) -----------------------------
# Lataa CSV:nä (puolipiste- tai sarkaineroteltu, otsikollinen) kansioon
# data/talvi/raw/. Aika saa olla riveillä tai sarakkeissa.
#
# 1) luke_hakkuut_kk.csv
#    Metsätilastot > Puun markkinahakkuut > Kuukausitilastot >
#    "Teollisuuspuun hakkuut kuukausittain (koko maa, ennakko)" (2008M01 alkaen)
#    Kuukausi: kaikki. Omistajaryhmä: kaikki. Puutavaralaji: kaikki.
# 1b) luke_hakkuut_kk_arkisto.csv (valinnainen, pidentää sarjan 1981/07 alkaen)
#    Puun markkinahakkuut > Arkisto > "Teollisuuspuun hakkuut kuukausittain
#    1981-2007". Kaikki kuukaudet, omistajaryhmät ja puutavaralajit.
#
# 2) luke_hakkuut_maakunta.csv
#    Metsätilastot > Puun markkinahakkuut > Vuositilastot >
#    "Teollisuuspuun hakkuut omistajaryhmittäin ja maakunnittain" (taulu 0300_marhak).
#    HUOM: ei metsäteollisuuden puunkäyttöä maakunnittain (siinä on sarakkeet
#    "Tuontihake ja -puru" ja "Metsäteollisuuden sivutuotteet").
#    Vuosi: kaikki. Maakunta: kaikki. Omistajaryhmä: YHTEENSÄ.
#    Puutavaralaji: YHTEENSÄ.
#
# 3) poro_*.csv / poro_*.xlsx (Paliskuntain yhdistys, paliskunnat.fi > Materiaalit
#    > Tilastot): paliskunnittaiset vuositilastot (eloporot, teurastetut,
#    vasat, vahingot) niin pitkältä ajalta kuin saatavilla. Tallenna nimellä,
#    joka alkaa "poro_".
# ------------------------------------------------------------------------------

source(here::here("R", "talvi", "talvi-helpers.R"))

vaiheet <- trimws(strsplit(Sys.getenv("TALVI_RUN", "hakkuut,hakkuut_mk,jaatiet,poro"), ",")[[1]])
stopifnot(length(vaiheet) > 0, all(vaiheet %in% c("hakkuut", "hakkuut_mk", "jaatiet", "poro")))
options(width = 200)

# Yhteenveto px_lue-tuloksesta: ulottuvuudet ja aikaväli
kuvaa <- function(d) {
  ulottuvuudet <- setdiff(names(d), c("aika", "pvm", "ennakko", "arvo"))
  print(suppressWarnings(d |> group_by(across(all_of(ulottuvuudet))) |>
          summarise(eka = min(pvm[!is.na(arvo)]), vika = max(pvm[!is.na(arvo)]),
                    puuttuu = sum(is.na(arvo)), .groups = "drop")), n = 60, width = 200)
}

lue_raaka <- function(nimi) {
  p <- file.path(TALVI_RAW, nimi)
  if (!file.exists(p)) stop("Lataa ensin taulu: ", p, " (ohjeet skriptin alussa).")
  d <- px_lue(p)
  stopifnot(nrow(d) > 0, !anyDuplicated(d[setdiff(names(d), c("aika", "ennakko", "arvo"))]))
  d
}

# ===========================================================================
if ("hakkuut" %in% vaiheet) {
  message("== Vaihe hakkuut ==")
  hk <- lue_raaka("luke_hakkuut_kk.csv") |> mutate(lahde = "nykyinen")
  # Valinnainen arkistotaulu 1981–2007; liitetään vain nykyistä taulua edeltäviltä kuukausilta
  if (file.exists(file.path(TALVI_RAW, "luke_hakkuut_kk_arkisto.csv"))) {
    ark <- lue_raaka("luke_hakkuut_kk_arkisto.csv") |> mutate(lahde = "arkisto") |>
      filter(pvm < min(hk$pvm))
    # Arkistotaulun nimet nykyisen taulun nimiksi. Metsäteollisuus ja valtio
    # yhdistettiin tilastossa vuoden 2008 alusta, joten arkistossa ne summataan.
    omist <- c("Kaikki yhteensä" = "YHTEENSÄ (omistajaryhmät)", "Yksityismetsät, yhteensä" = "Yksityismetsät",
               "Yksityismetsät, pystykaupat" = "Pystykaupat", "Yksityismetsät, hankintakaupat" = "Hankintakaupat",
               "Metsäteollisuus" = "Metsäteollisuus ja valtio", "Valtio" = "Metsäteollisuus ja valtio")
    lajit <- c("Kaikki puutavaralajit yhteensä" = "YHTEENSÄ (puutavaralajit)", "Ainespuu yhteensä" = "Teollisuuspuu",
               "Tukkipuu yhteensä" = "Tukkipuu", "Kuitupuu yhteensä" = "Kuitupuu", "Mäntykuitu" = "Mäntykuitupuu",
               "Kuusikuitu" = "Kuusikuitupuu", "Lehtikuitu" = "Lehtikuitupuu")
    ark <- ark |>
      mutate(Omistajaryhmä = sub("^\\.+", "", Omistajaryhmä), tieto = sub("^\\.+", "", tieto))
    tuntemattomat <- setdiff(unique(ark$Omistajaryhmä), names(omist))
    if (length(tuntemattomat)) stop("Arkiston tuntemattomat omistajaryhmät: ", paste(tuntemattomat, collapse = ", "))
    ark <- ark |>
      mutate(Omistajaryhmä = unname(omist[Omistajaryhmä]),
             tieto = if_else(tieto %in% names(lajit), unname(lajit[tieto]), tieto)) |>
      group_by(Omistajaryhmä, tieto, aika, pvm, lahde) |>
      summarise(ennakko = any(ennakko), arvo = if (all(is.na(arvo))) NA_real_ else sum(arvo, na.rm = TRUE),
                .groups = "drop") |>
      mutate(Tieto = unique(hk$Tieto)[1])
    puuttuvat <- setdiff(unique(ark$tieto), unique(sub("^\\.+", "", hk$tieto)))
    if (length(puuttuvat)) stop("Arkiston puutavaralajit, joita nykyisessä taulussa ei ole: ", paste(puuttuvat, collapse = ", "))
    hk <- bind_rows(ark, hk)
  } else {
    message("Arkistotaulua (1981–2007) ei ladattu: sarja alkaa ", format(min(hk$pvm), "%m/%Y"), ".")
  }
  # Ulottuvuuksien nimet yhtenäisiksi: poistetaan hierarkiapisteet ("..Mäntytukki" -> "Mäntytukki")
  hk <- hk |> mutate(tieto = sub("^\\.+", "", tieto),
                     across(any_of("Omistajaryhmä"), \(x) sub("^\\.+", "", x)))
  kuvaa(hk)
  if (any(hk$lahde == "arkisto")) {
    sauma <- hk |> filter(Omistajaryhmä == "YHTEENSÄ (omistajaryhmät)", tieto == "YHTEENSÄ (puutavaralajit)",
                          year(pvm) %in% 2006:2009) |>
      group_by(vuosi = year(pvm), lahde) |> summarise(milj_m3 = sum(arvo) / 1000, .groups = "drop")
    message("Sauma arkiston ja nykyisen taulun välillä (vuosihakkuut, milj. m³):")
    print(sauma)
  }
  stopifnot(all(hk$arvo >= 0, na.rm = TRUE), !anyDuplicated(hk[c(intersect("Omistajaryhmä", names(hk)), "tieto", "pvm")]))
  tallenna_atom(hk, file.path(TALVI_DIR, "luke_hakkuut_kk.qs2"))
}

# ===========================================================================
if ("hakkuut_mk" %in% vaiheet) {
  message("== Vaihe hakkuut_mk ==")
  hm <- lue_raaka("luke_hakkuut_maakunta.csv")
  if (any(grepl("Tuontihake|sivutuotteet", hm$tieto)))
    stop("luke_hakkuut_maakunta.csv on metsäteollisuuden puunkäyttötaulu, ei hakkuutaulu. ",
         "Lataa 'Teollisuuspuun hakkuut omistajaryhmittäin ja maakunnittain' (0300_marhak).")
  kuvaa(hm)
  tallenna_atom(hm, file.path(TALVI_DIR, "luke_hakkuut_maakunta.qs2"))
}

# ===========================================================================
# GNWT:n liikenneraportti 2024 (Highway Traffic Report), kuva 2.6 "Opening and
# Close Dates for Winter Roads & Ice Bridges", kaudet 2000/01–2024/25. Taulukko
# on purettu PDF:stä valmiiksi tiedostoon data/talvi/raw/nwt_talvitiet.csv
# (kausi, tie, tyyppi, tapahtuma = avaus/sulkeminen, pvm). Tyhjä solu = tie ei
# ollut auki tai tieto puuttuu (esim. Fort Providencen jääsilta korvattiin
# sillalla 2012).
if ("jaatiet" %in% vaiheet) {
  message("== Vaihe jaatiet ==")
  p <- file.path(TALVI_RAW, "nwt_talvitiet.csv")
  if (!file.exists(p)) stop("Puuttuu ", p)
  jt <- readr::read_csv(p, col_types = "ccccD", locale = readr::locale(encoding = "UTF-8"))
  jt <- jt |>
    pivot_wider(names_from = tapahtuma, values_from = pvm) |>
    mutate(kausi_alku = as.integer(substr(kausi, 1, 4)),
           auki_pv = as.integer(sulkeminen - avaus),
           avaus_pv = as.integer(avaus - as.Date(sprintf("%d-10-01", kausi_alku))),
           sulku_pv = as.integer(sulkeminen - as.Date(sprintf("%d-10-01", kausi_alku))))
  stopifnot(nrow(jt) > 400, !anyDuplicated(jt[c("kausi", "tie")]),
            all(jt$auki_pv > 0, na.rm = TRUE), all(jt$auki_pv < 220, na.rm = TRUE))
  print(jt |> group_by(tie, tyyppi) |>
          summarise(kausia = sum(!is.na(auki_pv)), auki_ka = round(mean(auki_pv, na.rm = TRUE)),
                    .groups = "drop") |> arrange(tyyppi, tie), n = 30)
  tallenna_atom(jt, file.path(TALVI_DIR, "nwt_talvitiet.qs2"))
}

# ===========================================================================
if ("poro" %in% vaiheet) {
  message("== Vaihe poro ==")
  suppressPackageStartupMessages(library(readxl))
  tiedostot <- list.files(TALVI_RAW, pattern = "^poro_.*\\.(csv|xlsx?)$", full.names = TRUE, ignore.case = TRUE)
  if (!length(tiedostot)) {
    warning("Ei poro_-alkuisia tiedostoja kansiossa ", TALVI_RAW, ". Ohita tämä vaihe tai lataa tiedostot.")
  } else {
    raaka <- list()
    for (f in tiedostot) {
      if (grepl("\\.xlsx?$", f, ignore.case = TRUE)) {
        for (lehti in excel_sheets(f)) {
          raaka[[paste(basename(f), lehti, sep = " :: ")]] <-
            read_excel(f, sheet = lehti, col_names = FALSE, col_types = "text", .name_repair = "unique_quiet")
        }
      } else {
        eka <- readLines(f, n = 1L, encoding = "UTF-8", warn = FALSE)
        erotin <- if (grepl("\t", eka)) "\t" else if (grepl(";", eka)) ";" else ","
        raaka[[basename(f)]] <- readr::read_delim(f, delim = erotin, col_names = FALSE,
                                                  col_types = readr::cols(.default = "c"),
                                                  name_repair = "unique_quiet", show_col_types = FALSE)
      }
    }
    for (nm in names(raaka)) {
      message("--- ", nm, ": ", nrow(raaka[[nm]]), " riviä x ", ncol(raaka[[nm]]), " saraketta")
      print(utils::head(as.data.frame(raaka[[nm]]), 12))
    }
    qs_save(raaka, file.path(TALVI_DIR, "poro_raaka.qs2"))
  }
}

message("Valmis.")
