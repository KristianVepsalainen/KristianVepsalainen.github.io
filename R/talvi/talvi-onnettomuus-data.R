# R/talvi/talvi-onnettomuus-data.R
# Talvi-sarjan osa 5 (liukas tie: tieliikenneonnettomuudet): datan keruu.
#
# Vaiheet (TALVI_RUN, oletus kaikki):
#   ton     Tilastokeskuksen tieliikenneonnettomuustilasto (selaimella ladatut
#           CSV:t) maakunnittain ja kuukausittain -> data/talvi/ton_kk.qs2
#           sekä keliluokittain vuosittain -> data/talvi/ton_keli.qs2
#   saa_kk  maakuntien tyypillisen sääaseman kuukausisää loka–huhtikuulta
#           1991 alkaen (lumipäivät, nollanylitykset, uuden lumen päivät,
#           vesisade lumelle, keskilämpötila, ensilumen kuukausi)
#           -> data/talvi/saa_kk.qs2
#
# Käyttö:  Rscript R/talvi/talvi-onnettomuus-data.R
#          TALVI_RUN=ton Rscript R/talvi/talvi-onnettomuus-data.R
#
# --- Selaimella ladattavat taulut (pxdata.stat.fi > StatFin > Tieliikenneonnettomuustilasto)
# Lataa CSV:nä kansioon data/talvi/raw/. Asettelulla ei ole väliä.
#
# 1) ton_111g.csv  "111g -- Tieliikenteen henkilövahinko-onnettomuudet alueen,
#    tielajin ja osallisten mukaan kuukausittain, 2015M01-..."
#    Tiedot: kaikki. Alue: KOKO MAA ja kaikki 19 maakuntaa (ei kuntia).
#    Tielaji: yhteensä. Osalliset: kaikki. Kuukausi: kaikki.
#
# 2) ton_001.csv  Passiivitaulu (StatFin_Passiivi > Tieliikenneonnettomuustilasto)
#    "001 -- Henkilövahinko-onnettomuudet, kuolleet ja loukkaantuneet
#    kuukausittain maakunnissa 2003-2020". Kaikki valinnat.
#
# 3) ton_111f.csv  "111f -- Tieliikenneonnettomuudet ja niiden henkilövahingot
#    tielajin, nopeusrajoituksen, valoisuuden, kelin ja viikonpäivän mukaan"
#    Tiedot: henkilövahinkoon johtaneita ja kuolemaan johtaneita onnettomuuksia.
#    Keli: kaikki. Muut muuttujat: yhteensä. Vuosi: kaikki.
# ------------------------------------------------------------------------------

source(here::here("R", "talvi", "talvi-helpers.R"))

vaiheet <- trimws(strsplit(Sys.getenv("TALVI_RUN", "ton,saa_kk"), ",")[[1]])
stopifnot(length(vaiheet) > 0, all(vaiheet %in% c("ton", "saa_kk")))
options(width = 200)

# Maakuntien nimet NUTS3-tunnuksiksi (Tilastokeskus: "Uusimaa", "Ahvenanmaa")
vaesto <- qd_read(file.path(TALVI_DIR, "maakunta_vaesto.qs2"))
MAAKUNNAT <- vaesto |> transmute(alue = alue,
  nimi = case_when(alue == "FI1B1" ~ "Uusimaa", alue == "FI200" ~ "Ahvenanmaa", TRUE ~ alue_nimi))
stopifnot(nrow(MAAKUNNAT) == 19L)

alue_tunnus <- function(x) {
  x <- trimws(sub("^(MK\\d+|MA\\d+|SSS|KU\\d+|\\d+)\\s+", "", trimws(x)))
  x <- sub("\\s*-\\s*Landskapet Åland$|\\s*\\(.*\\)$", "", x)
  case_when(grepl("^(koko maa|koko maa yhteensä|koko suomi|kaikki alueet|yhteensä)$", x, ignore.case = TRUE) ~ "FI",
            x %in% MAAKUNNAT$nimi ~ MAAKUNNAT$alue[match(x, MAAKUNNAT$nimi)],
            TRUE ~ NA_character_)
}
mittari_koodi <- function(x) {
  case_when(
    grepl("henkilövahinko", x, ignore.case = TRUE) & !grepl("kuole|loukkaan", x, ignore.case = TRUE) ~ "hvo",
    grepl("^kuolemaan johtan", x, ignore.case = TRUE) ~ "kuolema_onn",
    grepl("^loukkaantumiseen johtan", x, ignore.case = TRUE) ~ "loukk_onn",
    grepl("^vakavaan loukkaantumiseen johtan", x, ignore.case = TRUE) ~ "vakava_onn",
    grepl("^(kuolleit|kuolleet)", x, ignore.case = TRUE) ~ "kuolleet",
    grepl("^(loukkaantunei|loukkaantuneet)", x, ignore.case = TRUE) ~ "loukkaantuneet",
    grepl("^vakavasti loukk", x, ignore.case = TRUE) ~ "vakavasti_loukk",
    TRUE ~ NA_character_)
}
on_yhteensa <- function(x) grepl("yhteensä|kaikki|^total", x, ignore.case = TRUE)

# Yhtenäistää minkä tahansa onnettomuustaulun muotoon
# alue, pvm, mittari, osallinen, arvo, ennakko
yhtenaista <- function(nimi) {
  p <- file.path(TALVI_RAW, nimi)
  d <- px_lue(p)
  sar <- names(d)
  alue_s <- sar[grepl("^(alue|maakunta)", sar, ignore.case = TRUE)][1]
  osal_s <- sar[grepl("osalli|tienkäyttäj", sar, ignore.case = TRUE)][1]
  tiel_s <- sar[grepl("^tielaji", sar, ignore.case = TRUE)][1]
  tieto_s <- sar[grepl("^tiedot$", sar, ignore.case = TRUE)][1]
  # Mittarin nimi on joko Tiedot-ulottuvuudessa tai px_lue:n tieto-sarakkeessa
  d$mittari_txt <- if (!is.na(tieto_s)) d[[tieto_s]] else d$tieto
  # Kuukausi riveillä nimenä ("Tammikuu") ja vuosi sarakkeessa: yhdistetään
  kk_s <- sar[grepl("^kuukausi$", sar, ignore.case = TRUE)][1]
  if (!is.na(kk_s)) {
    KK_NIMET <- c("tammikuu", "helmikuu", "maaliskuu", "huhtikuu", "toukokuu", "kesäkuu", "heinäkuu",
                  "elokuu", "syyskuu", "lokakuu", "marraskuu", "joulukuu")
    d <- d[!grepl("yhteensä", d[[kk_s]], ignore.case = TRUE), ]
    nro <- match(tolower(trimws(d[[kk_s]])), KK_NIMET)
    if (anyNA(nro)) stop(nimi, ": tunnistamattomat kuukaudet: ", paste(unique(d[[kk_s]][is.na(nro)]), collapse = ", "))
    d$pvm <- as.Date(sprintf("%s-%02d-01", substr(d$aika, 1, 4), nro))
  }
  if (is.na(alue_s)) stop(nimi, ": alue- tai maakuntasaraketta ei löytynyt: ", paste(sar, collapse = ", "))
  if (!is.na(tiel_s)) d <- d[on_yhteensa(d[[tiel_s]]), ]
  d <- d |>
    mutate(alue = alue_tunnus(.data[[alue_s]]),
           osallinen = if (!is.na(osal_s)) .data[[osal_s]] else "Yhteensä",
           mittari = mittari_koodi(mittari_txt))
  tuntemattomat_alueet <- unique(d[[alue_s]][is.na(d$alue)])
  if (length(tuntemattomat_alueet))
    message(nimi, ": ohitetaan ", length(tuntemattomat_alueet), " aluetta, jotka eivät ole maakuntia: ",
            paste(head(tuntemattomat_alueet, 8), collapse = ", "), if (length(tuntemattomat_alueet) > 8) " ..." else "")
  tuntemattomat_mittarit <- unique(d$mittari_txt[is.na(d$mittari)])
  if (length(tuntemattomat_mittarit))
    stop(nimi, ": tunnistamattomat mittarit: ", paste(tuntemattomat_mittarit, collapse = " | "))
  d <- d |> filter(!is.na(alue)) |>
    mutate(osallinen = if_else(on_yhteensa(osallinen), "Yhteensä", sub("^\\.+", "", osallinen))) |>
    transmute(alue, pvm, mittari, osallinen, arvo, ennakko, lahde = sub("\\.csv$", "", nimi))
  stopifnot(n_distinct(d$alue) == 20L, !anyDuplicated(d[c("alue", "pvm", "mittari", "osallinen")]))
  d
}

# ===========================================================================
if ("ton" %in% vaiheet) {
  message("== Vaihe ton ==")
  osat <- list()
  for (f in c("ton_111g.csv", "ton_001.csv")) {
    if (file.exists(file.path(TALVI_RAW, f))) osat[[f]] <- yhtenaista(f)
    else message("Ei tiedostoa ", f)
  }
  if (!length(osat)) stop("Lataa vähintään ton_111g.csv (ohjeet skriptin alussa).")
  # Passiivitaulun viimeiset kuukaudet voivat olla nollia ilman tietoa:
  # kuukausi, jona koko maan kaikki mittarit ovat nollia, merkitään puuttuvaksi.
  if (!is.null(osat[["ton_001.csv"]])) {
    tyhjat <- osat[["ton_001.csv"]] |> filter(alue == "FI") |> group_by(pvm) |>
      summarise(tyhja = all(arvo == 0, na.rm = TRUE), .groups = "drop") |> filter(tyhja) |> pull(pvm)
    if (length(tyhjat)) {
      message("ton_001: ", length(tyhjat), " kuukautta ilman tietoja (", paste(format(tyhjat, "%m/%Y"), collapse = ", "), ") -> NA")
      osat[["ton_001.csv"]] <- osat[["ton_001.csv"]] |> mutate(arvo = if_else(pvm %in% tyhjat, NA_real_, arvo))
    }
  }
  kk <- bind_rows(osat)
  # Päällekkäiset kuukaudet: uudempi taulu (111g) voittaa. Tarkistetaan saumasta
  # vertailukelpoisuus kokonaismäärissä.
  if (length(osat) == 2L) {
    yhteiset <- unique(osat[[1]]$pvm[osat[[1]]$pvm %in% osat[[2]]$pvm])
    if (length(yhteiset)) {
      message("Päällekkäisiä kuukausia ", length(yhteiset), ". Taulut sisältävät mittarit: ",
              paste(names(osat), map_chr(osat, \(o) paste(sort(unique(o$mittari)), collapse = ", ")), sep = ": ", collapse = " | "))
    }
    # Poistetaan vanhemman taulun rivit vain, jos sama mittari on uudemmassa taulussa
    uudet <- osat[["ton_111g.csv"]] |> distinct(pvm, mittari) |> mutate(uusi = TRUE)
    kk <- kk |> left_join(uudet, by = c("pvm", "mittari")) |>
      filter(lahde == "ton_111g" | is.na(uusi)) |> select(-uusi)
  }
  stopifnot(!anyDuplicated(kk[c("alue", "pvm", "mittari", "osallinen")]), all(kk$arvo >= 0, na.rm = TRUE))
  # Maakuntien summa vs. koko maa
  tark <- kk |> filter(mittari == "hvo", osallinen == "Yhteensä", lahde == "ton_111g") |>
    group_by(pvm) |> summarise(fi = sum(arvo[alue == "FI"]), mk = sum(arvo[alue != "FI"]), .groups = "drop")
  message(sprintf("HVO: maakuntien summa / koko maa = %.3f (kuukausien vaihteluväli %.2f–%.2f)",
                  sum(tark$mk) / sum(tark$fi), min(tark$mk / tark$fi), max(tark$mk / tark$fi)))
  print(kk |> group_by(lahde, mittari, osallinen) |>
          summarise(alku = min(pvm), loppu = max(pvm), summa = sum(arvo[alue == "FI"], na.rm = TRUE), .groups = "drop"),
        n = 60, width = 200)
  tallenna_atom(kk, file.path(TALVI_DIR, "ton_kk.qs2"))

  if (file.exists(file.path(TALVI_RAW, "ton_111f.csv"))) {
    kf <- px_lue(file.path(TALVI_RAW, "ton_111f.csv"))
    keli_s <- names(kf)[grepl("^keli", names(kf), ignore.case = TRUE)][1]
    tieto_s <- names(kf)[grepl("^tiedot$", names(kf), ignore.case = TRUE)][1]
    if (is.na(keli_s)) stop("ton_111f.csv: keli-saraketta ei löytynyt: ", paste(names(kf), collapse = ", "))
    muut <- setdiff(names(kf), c(keli_s, tieto_s, "tieto", "aika", "pvm", "ennakko", "arvo"))
    for (m in muut) kf <- kf[on_yhteensa(kf[[m]]), ]
    kf <- kf |> transmute(keli = .data[[keli_s]],
                          mittari = mittari_koodi(if (!is.na(tieto_s)) .data[[tieto_s]] else tieto),
                          vuosi = as.integer(substr(aika, 1, 4)), arvo, ennakko)
    stopifnot(!anyNA(kf$mittari), !anyDuplicated(kf[c("keli", "mittari", "vuosi")]))
    print(kf |> filter(mittari == "hvo") |> group_by(keli) |> summarise(yht = sum(arvo, na.rm = TRUE)), n = 30)
    tallenna_atom(kf, file.path(TALVI_DIR, "ton_keli.qs2"))
  } else message("Ei tiedostoa ton_111f.csv: keliosio jää postauksesta pois.")
}

# ===========================================================================
if ("saa_kk" %in% vaiheet) {
  message("== Vaihe saa_kk ==")
  KK <- c(10L, 11L, 12L, 1L, 2L, 3L, 4L)
  pv <- lue_paiva("fmi") |>
    filter(pvm >= as.Date("1990-09-01")) |>
    arrange(asema_id, pvm)
  stopifnot(all(c("alue", "asema_id", "pvm", "tmin", "tmax", "lumi_cm", "rr") %in% names(pv)))
  # Ensilumi: kauden ensimmäinen päivä 1.9. alkaen, jona lunta >= 1 cm
  ensi <- pv |>
    filter(month(pvm) %in% c(9:12, 1L)) |>
    mutate(kausi_alku = talvikausi_alku(pvm)) |>
    group_by(alue, asema_id, kausi_alku) |>
    summarise(kattavuus = mean(!is.na(lumi_cm)),
              eka = suppressWarnings(min(pvm[!is.na(lumi_cm) & lumi_cm >= 1])), .groups = "drop") |>
    filter(kattavuus >= KATTAVUUS_RAJA, is.finite(eka)) |>
    mutate(alue_id = alue, ensilumi_pv = as.numeric(eka - as.Date(sprintf("%d-09-01", kausi_alku))))
  ensi_alue <- alue_sarja(ensi, "ensilumi_pv") |>
    transmute(alue = alue_id, kausi_alku, ensilumi = as.Date(sprintf("%d-09-01", kausi_alku)) + round(y))
  # Kuukausimittarit asemittain
  asema_kk <- pv |>
    filter(month(pvm) %in% KK) |>
    group_by(asema_id) |>
    mutate(lumi_lisays = lumi_cm - lag(lumi_cm), edell = pvm - lag(pvm)) |>
    ungroup() |>
    mutate(kk = floor_date(pvm, "month")) |>
    group_by(alue, asema_id, kk) |>
    summarise(paivia = as.integer(days_in_month(kk[1])),
              kat_t = sum(!is.na(tmin) & !is.na(tmax)) / paivia,
              kat_lumi = sum(!is.na(lumi_cm)) / paivia,
              kat_rr = sum(!is.na(rr)) / paivia,
              lumipv = sum(lumi_cm >= 1, na.rm = TRUE) / max(kat_lumi, 1e-9),
              uusilumi_pv = sum(lumi_lisays >= 3 & edell == 1, na.rm = TRUE) / max(kat_lumi, 1e-9),
              nollapv = sum(tmin < 0 & tmax > 0, na.rm = TRUE) / max(kat_t, 1e-9),
              vesisade_lumelle = sum(rr >= 1 & tmax > 0 & lumi_cm >= 1, na.rm = TRUE) / max(min(kat_t, kat_lumi, kat_rr), 1e-9),
              tmean = mean((tmin + tmax) / 2, na.rm = TRUE),
              .groups = "drop") |>
    mutate(across(c(lumipv, uusilumi_pv), \(x) if_else(kat_lumi >= KATTAVUUS_RAJA, x, NA_real_)),
           across(c(nollapv, tmean), \(x) if_else(kat_t >= KATTAVUUS_RAJA, x, NA_real_)),
           vesisade_lumelle = if_else(pmin(kat_t, kat_lumi, kat_rr) >= KATTAVUUS_RAJA, vesisade_lumelle, NA_real_),
           alue_id = alue, kausi_alku = as.integer(format(kk, "%Y%m")))
  mittarit <- c("lumipv", "uusilumi_pv", "nollapv", "vesisade_lumelle", "tmean")
  saa <- map(mittarit, \(m) alue_sarja(asema_kk, m) |> rename(!!m := y) |> select(-n_asemat)) |>
    reduce(full_join, by = c("alue_id", "kausi_alku")) |>
    transmute(alue = alue_id, kk = as.Date(sprintf("%d-%s-01", kausi_alku %/% 100, sprintf("%02d", kausi_alku %% 100))),
              across(all_of(mittarit))) |>
    mutate(paivia = days_in_month(kk),
           across(c(lumipv, uusilumi_pv, nollapv, vesisade_lumelle), \(x) pmin(pmax(x, 0), paivia)),
           kausi_alku = talvikausi_alku(kk)) |>
    left_join(ensi_alue, by = c("alue", "kausi_alku")) |>
    mutate(ensilumi_kk = !is.na(ensilumi) & floor_date(ensilumi, "month") == kk) |>
    select(-paivia)
  stopifnot(n_distinct(saa$alue) >= 18, !anyDuplicated(saa[c("alue", "kk")]))
  print(saa |> group_by(kuukausi = month(kk)) |>
          summarise(across(all_of(mittarit), \(x) round(mean(x, na.rm = TRUE), 1)), .groups = "drop"))
  message("Alueita ", n_distinct(saa$alue), ", kuukausia ", n_distinct(saa$kk),
          " (", format(min(saa$kk), "%m/%Y"), "–", format(max(saa$kk), "%m/%Y"), ")")
  tallenna_atom(saa, file.path(TALVI_DIR, "saa_kk.qs2"))
}

message("Valmis.")
