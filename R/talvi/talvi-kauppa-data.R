# R/talvi/talvi-kauppa-data.R
# Talvi-sarjan osa 3 (talvi kauppana): datan keruu ja siivous.
#
# Vaiheet (TALVI_RUN, oletus kaikki):
#   klv     Tilastokeskuksen kaupan liikevaihtokuvaaja kuukausittain (selaimella
#           ladattu CSV) -> data/talvi/klv_kuukausi.qs2
#   yrtt    (valinnainen) toimialojen liikevaihto euroina vuosittain, jotta
#           indeksit voi muuttaa euroiksi -> data/talvi/yrtt_liikevaihto.qs2
#   vaesto  maakuntien asukasluvut Paavo-aineistosta (väestöpainot säälle)
#           -> data/talvi/maakunta_vaesto.qs2
#   tutki   valtion ja kaupunkien talvikunnossapidon ostolaskut
#           Tutkihankintoja-aineistosta (Hilma-sarjan data) -> data/talvi/tutki_talvi.qs2
#
# Käyttö:  Rscript R/talvi/talvi-kauppa-data.R
#          TALVI_RUN=klv,yrtt Rscript R/talvi/talvi-kauppa-data.R
#
# --- Selaimella ladattavat taulut (pxdata.stat.fi -> StatFin) ------------------
# Lataa muodossa "Sarkaineroteltu ... (otsikollinen)" tai puolipisteeroteltu,
# aika SARAKKEISSA (oletusasettelu). Tallenna kansioon data/talvi/raw/.
#
# 1) klv_kuukausi.csv  Kaupan liikevaihtokuvaaja -> kuukausittainen taulu, jossa
#    on Toimiala (TOL 2008), Tiedot ja Kuukausi.
#    Toimiala: valitse ne näistä, jotka taulussa ovat (koodit nimen alussa):
#      G kauppa yhteensä, 45 moottoriajoneuvojen kauppa ja korjaus,
#      452 / 4520 moottoriajoneuvojen huolto ja korjaus (rengasvaihdot),
#      453 / 4532 moottoriajoneuvojen osien ja varusteiden vähittäiskauppa,
#      47 vähittäiskauppa, 4711 päivittäistavarakauppa (vertailuryhmä),
#      4752 rauta-, maali- ja lasikauppa, 4764 urheiluvälinekauppa,
#      4771 vaatteiden vähittäiskauppa, 4772 jalkineiden vähittäiskauppa
#    Tiedot: liikevaihdon ALKUPERÄINEN indeksi (ei kausitasoitettu eikä
#      työpäiväkorjattu) ja jos saatavilla myynnin määrän alkuperäinen indeksi.
#    Kuukausi: kaikki.
#
# 2) yrtt_liikevaihto.csv (valinnainen) Yritysten rakenne- ja tilinpäätöstilasto
#    -> toimialoittainen taulu (TOL 2008), Tiedot: Liikevaihto, Vuosi: kaikki
#    saatavilla olevat, samat toimialat kuin yllä.
# ------------------------------------------------------------------------------

source(here::here("R", "talvi", "talvi-helpers.R"))

vaiheet <- trimws(strsplit(Sys.getenv("TALVI_RUN", "klv,yrtt,vaesto,tutki"), ",")[[1]])
stopifnot(length(vaiheet) > 0, all(vaiheet %in% c("klv", "yrtt", "vaesto", "tutki")))

# Toimialakoodi tekstin alusta: "45320 Moottori..." -> "45320", "G Kauppa" -> "G"
toimialakoodi <- function(x) sub("^([A-Z0-9.]+)\\s.*$", "\\1", trimws(x))

# ===========================================================================
if ("klv" %in% vaiheet) {
  message("== Vaihe klv ==")
  p <- file.path(TALVI_RAW, "klv_kuukausi.csv")
  if (!file.exists(p)) stop("Lataa ensin taulu: ", p, " (ohjeet skriptin alussa).")
  klv <- px_lue(p)
  tol_sar <- names(klv)[grepl("^Toimiala", names(klv))][1]
  if (!is.na(tol_sar)) {
    klv <- klv |> rename(toimiala = all_of(tol_sar))
  } else {
    # Aika riveillä: sarakkeen nimi = "<toimiala> <tieto>", esim.
    # "45 Moottoriajoneuvojen kauppa ja korjaus Liikevaihto Alkuperäinen indeksisarja"
    tieto_re <- "\\s((Liikevaihto|Myynnin määrä)\\b.*)$"
    if (!all(grepl(tieto_re, klv$tieto, ignore.case = TRUE)))
      stop("Sarakenimistä ei erottunut toimiala ja tieto: ",
           paste(unique(klv$tieto[!grepl(tieto_re, klv$tieto, ignore.case = TRUE)]), collapse = " | "))
    klv <- klv |>
      mutate(toimiala = trimws(sub(tieto_re, "", tieto, ignore.case = TRUE)),
             tieto = sub(paste0("^.*", tieto_re), "\\1", tieto, ignore.case = TRUE))
  }
  klv <- klv |>
    mutate(koodi = gsub("\\.", "", toimialakoodi(toimiala)),
           kausitasoitettu = grepl("kausitasoit|trendi|työpäiväkorj", tieto, ignore.case = TRUE)) |>
    filter(!kausitasoitettu, !grepl("muutos", tieto, ignore.case = TRUE)) |>
    select(-kausitasoitettu)
  stopifnot(nrow(klv) > 0, !anyDuplicated(klv[c("toimiala", "tieto", "pvm")]),
            all(klv$arvo > 0, na.rm = TRUE))
  message("Toimialat ja tiedot:")
  print(klv |> group_by(koodi, toimiala, tieto) |>
          summarise(eka = min(pvm[!is.na(arvo)]), vika = max(pvm[!is.na(arvo)]),
                    puuttuu = sum(is.na(arvo)), .groups = "drop"), n = 50, width = 200)
  if (!any(klv$koodi %in% c("4532", "45320", "453")))
    warning("Renkaiden ja autotarvikkeiden toimiala (45.32) puuttuu valinnoista.")
  if (!any(klv$koodi %in% c("4711", "47110", "471")))
    warning("Päivittäistavarakauppa (vertailuryhmä) puuttuu valinnoista.")
  tallenna_atom(klv, file.path(TALVI_DIR, "klv_kuukausi.qs2"))
}

# ===========================================================================
if ("yrtt" %in% vaiheet) {
  message("== Vaihe yrtt (valinnainen) ==")
  p <- file.path(TALVI_RAW, "yrtt_liikevaihto.csv")
  if (!file.exists(p)) {
    message("Ei tiedostoa ", p, ": euromuunnos tehdään ilman sitä (osa 3 raportoi prosentteina).")
  } else {
    yr <- px_lue(p)
    tol_sar <- names(yr)[grepl("^Toimiala", names(yr))][1]
    stopifnot(!is.na(tol_sar))
    yr <- yr |>
      rename(toimiala = all_of(tol_sar)) |>
      mutate(koodi = gsub("\\.", "", toimialakoodi(toimiala)), vuosi = as.integer(substr(aika, 1, 4))) |>
      filter(grepl("^Liikevaihto", tieto))
    # Yksikkö tiedon nimestä: "1000 euroa" -> kerroin 1000
    yr <- yr |> mutate(kerroin = if_else(grepl("1 ?000", tieto), 1000, 1), euroa = arvo * kerroin)
    print(yr |> filter(!is.na(euroa)) |> group_by(koodi, toimiala) |> summarise(vuodet = paste(range(vuosi), collapse = "–"),
                                                       viimeisin_meur = round(euroa[which.max(vuosi)] / 1e6),
                                                       .groups = "drop"), n = 50)
    tallenna_atom(yr, file.path(TALVI_DIR, "yrtt_liikevaihto.qs2"))
  }
}

# ===========================================================================
if ("vaesto" %in% vaiheet) {
  message("== Vaihe vaesto ==")
  suppressPackageStartupMessages({ library(sf); library(giscoR); library(fst) })
  pv <- read_fst(here("data", "paavo", "paavo.fst"))
  sar <- c("Postinumeroalue", "Vuosi", "X-koordinaatti", "Y-koordinaatti", "Asukkaat yhteensä (HE)")
  stopifnot(all(sar %in% names(pv)))
  pv <- pv[pv$Postinumeroalue != "KOKO MAA", sar]
  pv$Vuosi <- as.integer(pv$Vuosi)
  uusin <- max(pv$Vuosi)
  pv <- pv[pv$Vuosi == uusin, ]
  pv$x <- as.numeric(pv$`X-koordinaatti`); pv$y <- as.numeric(pv$`Y-koordinaatti`)
  pv$asukkaat <- as.numeric(pv$`Asukkaat yhteensä (HE)`)
  pv <- pv[!is.na(pv$x) & !is.na(pv$y) & !is.na(pv$asukkaat), ]
  pist <- st_as_sf(pv, coords = c("x", "y"), crs = 3067)
  mk <- gisco_get_nuts(year = "2021", nuts_level = 3, country = "FI", resolution = "01", epsg = "4326") |>
    st_transform(3067) |> select(alue = NUTS_ID, alue_nimi = NAME_LATN)
  idx <- st_nearest_feature(pist, mk)      # rannikon postinumerot polygonin ulkopuolella -> lähin
  vaesto <- tibble(alue = mk$alue[idx], alue_nimi = mk$alue_nimi[idx], asukkaat = pist$asukkaat) |>
    group_by(alue, alue_nimi) |> summarise(asukkaat = sum(asukkaat), .groups = "drop") |>
    mutate(vuosi = uusin, osuus = asukkaat / sum(asukkaat))
  print(vaesto |> arrange(desc(asukkaat)))
  message(sprintf("Asukkaita yhteensä %s (Paavo %s).", format(sum(vaesto$asukkaat), big.mark = " "), uusin))
  stopifnot(nrow(vaesto) == 19L, abs(sum(vaesto$osuus) - 1) < 1e-9, sum(vaesto$asukkaat) > 5e6)
  tallenna_atom(vaesto, file.path(TALVI_DIR, "maakunta_vaesto.qs2"))
}

# ===========================================================================
# Tutkihankintoja: Hilma-sarjan tiedostot th_data_YYYY(.qs2 | __partNN.qs2).
# Talvikunnossapito tunnistetaan tekstistä (hankintakategoria, tuote- ja
# palveluryhmä, tili). Tulostetaan osumat tarkistettaviksi.
TALVI_REGEX <- paste0("talvikunnossa|talvihoito|talvityö|(^|[^a-zåäö])lume|lumity|kattolum|auraus|",
                      "liukkau|hiekoi|hiekot|(^|[^a-zåäö])suola|jäänpoist|jäänesto")
# Laajempi ehdokasjoukko diagnostiikkaan: ulkoalueiden ja väylien hoito.
EHDOKAS_REGEX <- "kiinteist|ylläpi|kunnossapi|hoitourak|väylän|teiden|tienpi|katujen|kadun|ulkoalue|piha|viheralue|alueurak"

if ("tutki" %in% vaiheet) {
  message("== Vaihe tutki ==")
  tiedostot <- list.files(here("data", "hilma"), pattern = "^th_data_\\d{4}(__part\\d+)?\\.qs2$",
                          recursive = TRUE, full.names = TRUE)
  if (!length(tiedostot)) stop("Tutkihankintoja-tiedostoja (th_data_YYYY*.qs2) ei löytynyt data/hilma-kansiosta.")
  message(length(tiedostot), " tiedostoa.")
  tekstisar <- c("hankintakategoria", "tuote_palveluryhma", "tili")
  osat <- map(tiedostot, function(f) {
    d <- tryCatch(qd_read(f), error = \(e) qs_read(f))
    d <- as_tibble(d)
    puuttuu <- setdiff(c(tekstisar, "tositepvm", "tiliointisumma", "ylaorganisaatio", "hankintayksikko"), names(d))
    if (length(puuttuu)) stop(basename(f), ": puuttuvat sarakkeet ", paste(puuttuu, collapse = ", "))
    d <- d |> mutate(pvm = as.Date(tositepvm), euroa = as.numeric(tiliointisumma))
    teksti <- tolower(paste(d$hankintakategoria, d$tuote_palveluryhma, d$tili))
    # Luokkien kuukausisummat diagnostiikkaa varten (pieni taulu)
    kat <- d |> mutate(kk = floor_date(pvm, "month")) |>
      count(hankintakategoria, tuote_palveluryhma, tili, kk, wt = euroa, name = "euroa")
    osumat <- d[grepl(TALVI_REGEX, teksti), ] |>
      transmute(ylaorganisaatio, hankintayksikko,
                toimittaja_nimi = if ("toimittaja_nimi" %in% names(d)) toimittaja_nimi else NA_character_,
                hankintakategoria, tuote_palveluryhma, tili, pvm, euroa,
                sektori = if ("sektori" %in% names(d)) sektori else NA_character_)
    rm(d); invisible(gc())
    list(kat = kat, osumat = osumat)
  }, .progress = "Tutkihankintoja")
  kat <- map(osat, "kat") |> list_rbind() |>
    group_by(hankintakategoria, tuote_palveluryhma, tili, kk) |> summarise(euroa = sum(euroa), .groups = "drop")
  osumat <- map(osat, "osumat") |> list_rbind()
  rm(osat); invisible(gc())
  tallenna_atom(kat, file.path(TALVI_DIR, "tutki_kategoriat_kk.qs2"))

  # Diagnostiikka: luokat, joiden nimi viittaa ulkoalueiden tai väylien hoitoon,
  # ja niiden talvikuukausien (marras–maalis) osuus. Tasaisesti laskutettu luokka ~ 42 %.
  vuosia <- n_distinct(year(kat$kk))
  diag <- kat |>
    group_by(hankintakategoria, tuote_palveluryhma, tili) |>
    summarise(meur_v = sum(euroa) / 1e6 / vuosia,
              talviosuus = sum(euroa[month(kk) %in% c(11:12, 1:3)]) / sum(euroa), .groups = "drop") |>
    filter(meur_v > 0.2)
  options(width = 250)
  message("Ehdokasluokat (ulkoalueet, väylät, kiinteistöt), suurimmat:")
  print(diag |> filter(grepl(EHDOKAS_REGEX, tolower(paste(hankintakategoria, tuote_palveluryhma, tili)))) |>
          arrange(desc(meur_v)) |> mutate(across(c(meur_v, talviosuus), \(x) round(x, 2))), n = 40, width = 250)
  message("Luokat, joissa talvikuukausien osuus on suurin (vähintään 0,2 M€/v):")
  print(diag |> arrange(desc(talviosuus)) |> mutate(across(c(meur_v, talviosuus), \(x) round(x, 2))), n = 30, width = 250)

  ulos <- file.path(TALVI_DIR, "tutki_talvi.qs2")
  if (nrow(osumat) == 0) {
    if (file.exists(ulos)) file.remove(ulos)
    warning("Talvikunnossapidon hakulauseke ei osunut yhteenkään laskuun. Postauksen lumityöosio jää pois. ",
            "Katso yllä olevat ehdokasluokat ja lähetä ne, niin tarkennetaan luokitus.")
  } else {
    stopifnot(!anyNA(osumat$pvm))
    message("Osumat luokittain (tarkista, ettei mukana ole vääriä):")
    print(osumat |> count(hankintakategoria, tuote_palveluryhma, tili, wt = euroa, name = "euroa", sort = TRUE) |>
            mutate(meur = round(euroa / 1e6, 2)), n = 40, width = 250)
    message("Osumat ylätason organisaatioittain:")
    print(osumat |> count(ylaorganisaatio, wt = euroa, name = "euroa", sort = TRUE) |>
            mutate(meur = round(euroa / 1e6, 2)), n = 30)
    tallenna_atom(osumat, ulos)
  }
}

message("Valmis.")
