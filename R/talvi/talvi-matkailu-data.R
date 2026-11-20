# R/talvi/talvi-matkailu-data.R
# Talvi-sarjan osa 2 (matkailu): datan keruu.
#
# Lähteet
#   USA:      BLS QCEW, kuukausittaiset työlliset (yksityinen sektori) toimialoilla
#             721 majoitus, 713920 laskettelukeskukset ja 10 kaikki toimialat,
#             osavaltio- ja piirikuntatasolla yhdeksässä talviosavaltiossa.
#   Kanada:   Statistics Canada, SEPH-taulu 14-10-0201-01 (työlliset toimialoittain,
#             kuukausittain, kausitasoittamaton), territoriot ja koko Kanada.
#   Pohjoismaat: Eurostat tour_occ_nin2m (yöpymiset kuukausittain NUTS2-alueittain,
#             vuodesta 2020): Ruotsin Lappi ja Västerbotten (SE33), Pohjois-Norja
#             (NO07) sekä Suomen NUTS2-alueet vertailuksi.
#   Suomi:    Tilastokeskuksen majoitustilasto (taulut 11iz, 11ix, 11lm) selaimella
#             ladattuina CSV-tiedostoina: ohjeet tiedoston lopussa (vaihe fi).
#
# Käyttö
#   TALVI_TEST=1 Rscript R/talvi/talvi-matkailu-data.R       # koeajo, ei tallennuksia
#   Rscript R/talvi/talvi-matkailu-data.R                    # vaiheet qcew, statcan, eurostat
#   TALVI_RUN=fi Rscript R/talvi/talvi-matkailu-data.R       # siivoaa itse ladatun Tilastokeskus-datan
#
# Ympäristömuuttujat
#   TALVI_QCEW_ALKU   ensimmäinen vuosi (oletus 2001). Vanhemmat vuodet haetaan
#                     vuosittaisista zip-tiedostoista (iso lataus per vuosi).
#   BLS_UA            User-Agent BLS:lle. BLS voi estää pyynnöt (HTTP 403) ilman
#                     yhteystietoja; aseta tarvittaessa esim.
#                     "talvi-sarja (etunimi.sukunimi@esimerkki.fi)".
#
# Tuotokset (data/talvi/)
#   qcew_talvi.qs2        osavaltiot ja piirikunnat x toimiala x kuukausi
#   qcew_alueet.qs2       QCEW-aluekoodien nimet
#   statcan_seph.qs2      territoriot ja Kanada x toimiala x kuukausi
#   eurostat_yopymiset.qs2  NUTS2 x asuinpaikka x kuukausi
#   fi_majoitus.qs2       Suomen maakunnat/kunnat x kuukausi, taulut 11iz, 11ix, 11lm (vaihe fi)

source(here::here("R", "talvi", "talvi-helpers.R"))
suppressPackageStartupMessages({
  library(httr2)
})

vaiheet <- trimws(strsplit(Sys.getenv("TALVI_RUN", "qcew,statcan,eurostat"), ",")[[1]])
stopifnot(length(vaiheet) > 0, all(vaiheet %in% c("qcew", "statcan", "eurostat", "fi")))
TESTI <- Sys.getenv("TALVI_TEST", "0") == "1"

for (k in file.path(TALVI_RAW, c("qcew", "cansim", "eurostat"))) {
  dir.create(k, recursive = TRUE, showWarnings = FALSE)
}

# ===========================================================================
# QCEW
# ===========================================================================
QCEW_OSAVALTIOT <- tribble(
  ~fips, ~alue,   ~alue_nimi,
  "02",  "US-AK", "Alaska",
  "27",  "US-MN", "Minnesota",
  "38",  "US-ND", "Pohjois-Dakota",
  "30",  "US-MT", "Montana",
  "55",  "US-WI", "Wisconsin",
  "26",  "US-MI", "Michigan",
  "23",  "US-ME", "Maine",
  "50",  "US-VT", "Vermont",
  "33",  "US-NH", "New Hampshire"
)
QCEW_TOIMIALAT <- c("10", "721", "713920")   # kaikki, majoitus, laskettelukeskukset
QCEW_ALKU <- as.integer(Sys.getenv("TALVI_QCEW_ALKU", "2001"))
stopifnot(QCEW_ALKU >= 2001L)   # NAICS-pohjainen sarja on yhtenäinen 2001 alkaen

bls_req <- function(url) {
  request(url) |>
    req_user_agent(Sys.getenv("BLS_UA", "talvi-sarja (kristianvepsalainen.com)")) |>
    req_retry(max_tries = 5, retry_on_failure = TRUE,
              is_transient = \(r) resp_status(r) %in% c(429L, 500L, 502L, 503L, 504L),
              backoff = \(i) 5 * i) |>
    req_throttle(rate = 1, realm = "bls") |>
    req_error(is_error = \(r) FALSE)
}

# Lataa URL:n tiedostoon. Palauttaa HTTP-tilan (NA = verkkovirhe).
bls_lataa <- function(url, polku) {
  if (file.exists(polku) && file.size(polku) > 0) return(200L)
  tmp <- paste0(polku, ".tmp")
  resp <- tryCatch(bls_req(url) |> req_perform(path = tmp), error = \(e) {
    message("Verkkovirhe: ", conditionMessage(e)); NULL })
  tila <- if (is.null(resp)) NA_integer_ else resp_status(resp)
  if (isTRUE(tila == 200L) && file.size(tmp) > 0) {
    stopifnot(file.rename(tmp, polku))
  } else {
    unlink(tmp)
    if (isTRUE(tila == 403L)) {
      stop("BLS palautti HTTP 403. Aseta ympäristömuuttuja BLS_UA, jossa on sähköpostiosoite, ",
           "esim. BLS_UA='talvi-sarja (nimi@esimerkki.fi)'.")
    }
  }
  tila
}

QCEW_SARAKKEET <- c("area_fips", "own_code", "industry_code", "agglvl_code", "year", "qtr",
                    "disclosure_code", "qtrly_estabs", "month1_emplvl", "month2_emplvl",
                    "month3_emplvl", "total_qtrly_wages")

qcew_lue <- function(polku) {
  # Vuosittaisissa zip-tiedostoissa toimipaikkasarake on nimeltään
  # qtrly_estabs_count ja yksi sarake on kahdesti; rajapinnan CSV:ssä qtrly_estabs.
  d <- read_csv(polku, col_types = cols(.default = col_character()), progress = FALSE,
                show_col_types = FALSE, name_repair = "unique_quiet")
  if (!"qtrly_estabs" %in% names(d) && "qtrly_estabs_count" %in% names(d)) {
    d <- rename(d, qtrly_estabs = qtrly_estabs_count)
  }
  puuttuu <- setdiff(QCEW_SARAKKEET, names(d))
  if (length(puuttuu)) stop(basename(polku), ": puuttuvat sarakkeet ", paste(puuttuu, collapse = ", "))
  d |> select(all_of(QCEW_SARAKKEET))
}

# Suodatus: valitut osavaltiot (osavaltio + piirikunnat), yksityinen sektori (own_code 5)
qcew_suodata <- function(d) {
  d |>
    filter(substr(area_fips, 1, 2) %in% QCEW_OSAVALTIOT$fips,
           grepl("^[0-9]{5}$", area_fips),
           own_code == "5")
}

# Yhden vuoden ja toimialan data: ensin avoimen datan rajapinta (viimeiset
# vuodet), sitten vuosittainen zip, josta puretaan vain tarvittava toimiala.
qcew_vuosi_toimiala <- function(vuosi, toimiala) {
  valmis <- file.path(TALVI_RAW, "qcew", sprintf("qcew_%d_%s.qs2", vuosi, toimiala))
  kuluva <- vuosi >= year(Sys.Date()) - 1L
  if (file.exists(valmis) && !kuluva) return(qd_read(valmis))

  # 1) Rajapinta: neljä neljännesvuotta
  neljannekset <- map(1:4, function(q) {
    p <- file.path(TALVI_RAW, "qcew", sprintf("api_%d_q%d_%s.csv", vuosi, q, toimiala))
    if (kuluva) unlink(p)    # tuoreet vuodet päivittyvät, haetaan aina uudelleen
    tila <- bls_lataa(sprintf("https://data.bls.gov/cew/data/api/%d/%d/industry/%s.csv",
                              vuosi, q, toimiala), p)
    if (isTRUE(tila == 200L)) qcew_lue(p) |> qcew_suodata() else NULL
  })
  saatiin <- !map_lgl(neljannekset, is.null)

  if (any(saatiin)) {
    d <- list_rbind(neljannekset)
  } else {
    # 2) Vuosittainen zip
    zip <- file.path(TALVI_RAW, "qcew", sprintf("%d_qtrly_by_industry.zip", vuosi))
    tila <- bls_lataa(sprintf("https://data.bls.gov/cew/data/files/%d/csv/%d_qtrly_by_industry.zip",
                              vuosi, vuosi), zip)
    if (!isTRUE(tila == 200L)) {
      warning(sprintf("QCEW %d %s: ei rajapinnasta eikä zipistä (HTTP %s).", vuosi, toimiala, tila))
      return(NULL)
    }
    sis <- unzip(zip, list = TRUE)$Name
    osuma <- sis[grepl(sprintf("\\s%s\\s", toimiala), sis) & grepl("\\.csv$", sis)]
    if (length(osuma) != 1L) {
      stop(sprintf("QCEW %d: toimialan %s tiedostoa ei löytynyt yksiselitteisesti zipistä (%d osumaa).",
                   vuosi, toimiala, length(osuma)))
    }
    ulos <- file.path(TALVI_RAW, "qcew", "pura")
    unzip(zip, files = osuma, exdir = ulos, junkpaths = TRUE)
    d <- qcew_lue(file.path(ulos, basename(osuma))) |> qcew_suodata()
    unlink(file.path(ulos, basename(osuma)))
  }

  stopifnot(all(d$year == as.character(vuosi)), all(d$industry_code == toimiala))
  if (!kuluva) qd_save(d, valmis)
  d
}

# Neljännesvuosirivit kuukausiriveiksi. Salatut solut (disclosure_code N)
# ovat BLS:llä nollia; ne muutetaan puuttuviksi.
qcew_kuukausittain <- function(d) {
  d |>
    mutate(across(c(year, qtr), as.integer),
           across(c(qtrly_estabs, month1_emplvl, month2_emplvl, month3_emplvl, total_qtrly_wages),
                  \(x) suppressWarnings(as.numeric(x))),
           salattu = !is.na(disclosure_code) & disclosure_code == "N") |>
    pivot_longer(c(month1_emplvl, month2_emplvl, month3_emplvl),
                 names_to = "kk_nro", values_to = "tyolliset") |>
    mutate(kk = (qtr - 1L) * 3L + as.integer(substr(kk_nro, 6, 6)),
           pvm = as.Date(sprintf("%d-%02d-01", year, kk)),
           tyolliset = if_else(salattu, NA_real_, tyolliset),
           fips = substr(area_fips, 1, 2),
           taso = if_else(substr(area_fips, 3, 5) == "000", "osavaltio", "piirikunta")) |>
    left_join(QCEW_OSAVALTIOT, by = "fips") |>
    select(alue, alue_nimi, area_fips, taso, toimiala = industry_code, pvm, vuosi = year,
           tyolliset, toimipaikat = qtrly_estabs, salattu)
}

qcew_aluenimet <- function() {
  p <- file.path(TALVI_RAW, "qcew", "area_titles.csv")
  tila <- bls_lataa("https://data.bls.gov/cew/doc/titles/area/area_titles.csv", p)
  if (!isTRUE(tila == 200L)) { warning("QCEW-aluenimiä ei saatu."); return(NULL) }
  read_csv(p, col_types = "cc", show_col_types = FALSE) |>
    rename_with(tolower) |>
    filter(substr(area_fips, 1, 2) %in% QCEW_OSAVALTIOT$fips)
}

qcew_testi <- function() {
  message("QCEW-testi: rajapinta 2024 Q1, toimiala 721")
  p <- tempfile(fileext = ".csv")
  tila <- bls_lataa("https://data.bls.gov/cew/data/api/2024/1/industry/721.csv", p)
  message("HTTP-tila: ", tila)
  stopifnot(isTRUE(tila == 200L))
  d <- qcew_lue(p) |> qcew_suodata()
  print(d |> filter(substr(area_fips, 3, 5) == "000"))
  message("Kuukausimuodossa (osavaltiot):")
  print(qcew_kuukausittain(d) |> filter(taso == "osavaltio") |> arrange(alue, pvm), n = 30)
  message("Rajapinnan vanhin vuosi: kokeillaan 2015 Q1")
  p2 <- tempfile(fileext = ".csv")
  message("HTTP-tila 2015: ", bls_lataa("https://data.bls.gov/cew/data/api/2015/1/industry/721.csv", p2),
          " (404 = vanhat vuodet haetaan zipeistä)")
}

# ===========================================================================
# Statistics Canada (cansim-paketti)
# ===========================================================================
STATCAN_ALUEET <- c("Yukon", "Northwest Territories", "Nunavut", "Canada")

statcan_hae <- function() {
  if (!requireNamespace("cansim", quietly = TRUE)) stop("Asenna cansim-paketti: install.packages('cansim')")
  options(cansim.cache_path = file.path(TALVI_RAW, "cansim"))
  d <- cansim::get_cansim("14-10-0201-01")
  stopifnot(nrow(d) > 1000)
  nimet <- names(d)
  sar_naics <- nimet[grepl("^North American Industry Classification", nimet)][1]
  sar_tyyppi <- nimet[grepl("^Type of employee", nimet)][1]
  stopifnot(!is.na(sar_naics), !is.na(sar_tyyppi), all(c("REF_DATE", "GEO", "VALUE") %in% nimet))

  # Toimialan nimessä cansim liittää mukaan hierarkian ("X ==> Y ==> ...") ja
  # hakasulkeissa on jäsentunnus, EI NAICS-koodia. Varsinainen koodi on omassa
  # sarakkeessaan "Classification Code for ...".
  sar_koodi <- nimet[grepl("^Classification Code for North American", nimet)][1]
  nimi_lyhyt <- sub("\\s*==>.*$", "", as.character(d[[sar_naics]]))
  nimi_lyhyt <- sub("\\s*\\[\\d+\\]$", "", nimi_lyhyt)
  koodi <- if (!is.na(sar_koodi)) as.character(d[[sar_koodi]]) else NA_character_
  valinta <- koodi %in% c("[72]", "[721]", "[7211]", "[71]", "[713]") |
    grepl("^(Industrial aggregate excluding unclassified|Accommodation and food services|Accommodation services|Traveller accommodation|Amusement, gambling and recreation)",
          nimi_lyhyt)
  message("Valitut toimialat:")
  print(unique(tibble(koodi = koodi[valinta], toimiala = nimi_lyhyt[valinta])))

  d[valinta, ] |>
    mutate(toimiala = nimi_lyhyt[valinta], koodi = koodi[valinta]) |>
    filter(GEO %in% STATCAN_ALUEET,
           grepl("^All employees", .data[[sar_tyyppi]])) |>
    transmute(alue = recode(as.character(GEO), "Yukon" = "CA-YT", "Northwest Territories" = "CA-NT",
                            "Nunavut" = "CA-NU", "Canada" = "CA"),
              alue_nimi = recode(as.character(GEO), "Yukon" = "Jukon", "Northwest Territories" = "Luoteisterritoriot",
                                 "Nunavut" = "Nunavut", "Canada" = "Kanada"),
              toimiala, koodi,
              pvm = as.Date(paste0(REF_DATE, "-01")),
              tyolliset = as.numeric(VALUE),
              tila = if ("STATUS" %in% nimet) as.character(STATUS) else NA_character_) |>
    # Sama toimiala voi esiintyä kahdesti eri hierarkiapolulla: pidetään ensimmäinen
    arrange(alue, toimiala, pvm, is.na(tyolliset)) |>
    distinct(alue, toimiala, pvm, .keep_all = TRUE)
}

# ===========================================================================
# Eurostat
# ===========================================================================
EUROSTAT_GEO <- c("SE33", "NO07", "FI1D", "FI1B", "FI1C", "FI19", "FI20")

eurostat_hae <- function() {
  if (!requireNamespace("eurostat", quietly = TRUE)) stop("Asenna eurostat-paketti.")
  d <- eurostat::get_eurostat("tour_occ_nin2m", time_format = "date",
                              filters = list(geo = EUROSTAT_GEO),
                              cache_dir = file.path(TALVI_RAW, "eurostat"))
  # Taulun rakenne: time on vuosi (freq = A) ja kuukausi on oma
  # dimensionsa month (M01 ... M12 sekä TOTAL = koko vuosi).
  aikasar <- intersect(c("time", "TIME_PERIOD"), names(d))[1]
  stopifnot(nrow(d) > 0, !is.na(aikasar), all(c("geo", "month", "values") %in% names(d)))
  out <- d |>
    filter(month != "TOTAL", unit == "NR") |>    # vain lukumäärät; muut yksiköt ovat suhdelukuja
    mutate(kk = as.integer(sub("^M", "", month)),
           pvm = make_date(year(as.Date(.data[[aikasar]])), kk, 1L)) |>
    select(geo, c_resid, nace_r2, unit, pvm, yopymiset = values)
  stopifnot(!anyNA(out$pvm), all(out$pvm >= as.Date("2020-01-01")),
            !anyDuplicated(out[c("geo", "c_resid", "nace_r2", "unit", "pvm")]))
  # Tarkistus: kuukausien summa vastaa taulun omaa vuosisummaa
  vuosi <- d |> filter(month == "TOTAL", unit == "NR") |>
    transmute(geo, c_resid, nace_r2, unit, vuosi = year(as.Date(.data[[aikasar]])), total = values)
  ero <- out |> group_by(geo, c_resid, nace_r2, unit, vuosi = year(pvm)) |>
    summarise(summa = sum(yopymiset), n_kk = sum(!is.na(yopymiset)), .groups = "drop") |>
    inner_join(vuosi, by = c("geo", "c_resid", "nace_r2", "unit", "vuosi")) |>
    filter(n_kk == 12, !is.na(total), abs(summa - total) > 0.01 * total)
  if (nrow(ero)) {
    warning(sprintf("%d alue-vuodella kuukausien summa poikkeaa vuosisummasta yli 1 %%.", nrow(ero)))
    print(ero)
  }
  out
}

# ===========================================================================
# TESTIAJO
# ===========================================================================
if (TESTI) {
  qcew_testi()
  message("Statistics Canada -testi: metatiedot")
  if (requireNamespace("cansim", quietly = TRUE)) {
    print(cansim::get_cansim_table_overview("14-10-0201-01"))
  } else message("cansim-pakettia ei ole asennettu.")
  message("Eurostat-testi: tour_occ_nin2m, SE33 ja NO07")
  e <- eurostat::get_eurostat("tour_occ_nin2m", time_format = "date",
                              filters = list(geo = c("SE33", "NO07")))
  print(dplyr::count(e, geo, sort = TRUE))
  print(utils::head(e))
  message("Koeajo valmis. Mitään ei tallennettu data/talvi-kansioon.")
  quit(save = "no", status = 0)
}

# ===========================================================================
# VAIHEET
# ===========================================================================
if ("qcew" %in% vaiheet) {
  message("== Vaihe qcew ==")
  vuodet <- QCEW_ALKU:year(Sys.Date())
  # Vuosi kerrallaan: kaikki toimialat samasta zipistä, minkä jälkeen zip
  # poistetaan heti, jottei levylle kerry useita gigatavuja.
  raaka <- map(vuodet, function(v) {
    d <- map(QCEW_TOIMIALAT, \(t) {
      message(sprintf("QCEW %d %s", v, t))
      qcew_vuosi_toimiala(v, t)
    }) |> list_rbind()
    unlink(file.path(TALVI_RAW, "qcew", sprintf("%d_qtrly_by_industry.zip", v)))
    d
  }) |>
    list_rbind()
  qcew <- qcew_kuukausittain(raaka) |>
    filter(pvm <= Sys.Date())
  stopifnot(nrow(qcew) > 0,
            !anyDuplicated(qcew[c("area_fips", "toimiala", "pvm")]),
            all(qcew$tyolliset >= 0, na.rm = TRUE),
            setequal(unique(qcew$alue[qcew$taso == "osavaltio"]), QCEW_OSAVALTIOT$alue))
  message("Osavaltiotason rivit toimialoittain, ensimmäinen ja viimeinen kuukausi, salattujen osuus:")
  print(qcew |> filter(taso == "osavaltio") |>
          group_by(toimiala) |>
          summarise(rivit = n(), eka = min(pvm), vika = max(pvm),
                    salattu_pros = round(100 * mean(salattu)), .groups = "drop"))
  tallenna_atom(qcew, file.path(TALVI_DIR, "qcew_talvi.qs2"))
  an <- qcew_aluenimet()
  if (!is.null(an)) tallenna_atom(an, file.path(TALVI_DIR, "qcew_alueet.qs2"))
}

if ("statcan" %in% vaiheet) {
  message("== Vaihe statcan ==")
  sc <- statcan_hae()
  stopifnot(nrow(sc) > 0, setequal(unique(sc$alue), c("CA-YT", "CA-NT", "CA-NU", "CA")),
            !anyDuplicated(sc[c("alue", "toimiala", "pvm")]),
            any(grepl("^Accommodation", sc$toimiala)))
  message("Rivit ja puuttuvien (salattujen) osuus alueittain ja toimialoittain:")
  print(sc |> group_by(alue, toimiala) |>
          summarise(eka = min(pvm), vika = max(pvm),
                    puuttuu_pros = round(100 * mean(is.na(tyolliset))), .groups = "drop"), n = 50)
  tallenna_atom(sc, file.path(TALVI_DIR, "statcan_seph.qs2"))
}

if ("eurostat" %in% vaiheet) {
  message("== Vaihe eurostat ==")
  es <- eurostat_hae()
  message("Rivit alueittain ja ensimmäinen/viimeinen kuukausi:")
  print(es |> group_by(geo) |> summarise(rivit = n(), eka = min(pvm), vika = max(pvm), .groups = "drop"))
  print(dplyr::count(es, c_resid, nace_r2, unit))
  puuttuvat <- setdiff(c("SE33", "NO07"), unique(es$geo))
  if (length(puuttuvat)) warning("Eurostatista puuttuvat alueet: ", paste(puuttuvat, collapse = ", "))
  tallenna_atom(es, file.path(TALVI_DIR, "eurostat_yopymiset.qs2"))
}

# ===========================================================================
# VAIHE fi: Tilastokeskuksen majoitustilasto
# ===========================================================================
# Lataa taulut selaimella PxWebistä (pxdata.stat.fi -> StatFin -> Majoitustilasto)
# muodossa "Puolipiste- tai sarkainerotettu, otsikollinen" (UTF-8). Tallenna:
#
#   data/talvi/raw/fi_11iz.csv  11iz Kuukausittaiset yöpymiset ja saapuneet vieraat asuinmaittain
#                               (maakunnat, maat, kaikki majoitusliikkeet, saapuneet + yöpymiset)
#   data/talvi/raw/fi_11ix.csv  11ix Majoitusliikkeiden kuukausittainen kapasiteetti ja sen käyttö
#   data/talvi/raw/fi_11lm.csv  11lm Hotellien kuukausittainen kapasiteetti ja yöpymiset kunnittain
#
# PxWebin "taulukko"-muodossa jokainen tieto x kuukausi on oma sarakkeensa
# ("Yöpymiset, lkm 2024M01"), puuttuva arvo on "." ja ennakkotiedot on merkitty
# tähdellä ("2026M09*"). Alla oleva lukija muuttaa tämän pitkään muotoon.
#
# Sitten: TALVI_RUN=fi Rscript R/talvi/talvi-matkailu-data.R

FI_PUUTTUVA <- c("", ".", "..", "...", "-", "x")

fi_lue <- function(polku, taulu) {
  eka <- readLines(polku, n = 1L, encoding = "UTF-8", warn = FALSE)
  erotin <- if (grepl("\t", eka)) "\t" else ";"
  raaka <- read_delim(polku, delim = erotin, col_types = cols(.default = col_character()),
                      na = FI_PUUTTUVA, locale = locale(encoding = "UTF-8"),
                      progress = FALSE, show_col_types = FALSE, trim_ws = TRUE)
  kk_sar <- names(raaka)[grepl("\\s\\d{4}M\\d{2}\\*?$", names(raaka))]
  id_sar <- setdiff(names(raaka), kk_sar)
  if (!length(kk_sar)) stop(taulu, ": kuukausisarakkeita (muotoa 'Tieto 2024M01') ei löytynyt.")
  if (!any(grepl("^Maakunta|^Kunta", id_sar))) stop(taulu, ": alue-sarake puuttuu: ", paste(id_sar, collapse = ", "))

  alue_sar   <- id_sar[grepl("^Maakunta|^Kunta", id_sar)][1]
  maa_sar    <- id_sar[grepl("^Maa$", id_sar)][1]
  tyyppi_sar <- id_sar[grepl("^Majoitusliikkeen tyyppi", id_sar)][1]

  out <- raaka |>
    pivot_longer(all_of(kk_sar), names_to = "sarake", values_to = "arvo_txt") |>
    mutate(
      taulu     = taulu,
      alue      = .data[[alue_sar]],
      alue_taso = if (grepl("^Kunta", alue_sar)) "kunta" else "maakunta",
      maa       = if (!is.na(maa_sar)) .data[[maa_sar]] else "Kaikki yhteensä",
      tyyppi    = if (!is.na(tyyppi_sar)) .data[[tyyppi_sar]] else "Hotellit",
      tieto     = sub("\\s\\d{4}M\\d{2}\\*?$", "", sarake),
      kuukausi  = sub("^.*\\s(\\d{4}M\\d{2})\\*?$", "\\1", sarake),
      ennakko   = grepl("\\*$", sarake),
      pvm       = as.Date(paste0(sub("M", "-", kuukausi), "-01")),
      arvo      = suppressWarnings(as.numeric(gsub(",", ".", gsub("\\s", "", arvo_txt))))
    )
  # Tarkistus: kaikki ei-puuttuvat tekstiarvot muuttuivat luvuiksi
  huonot <- out |> filter(!is.na(arvo_txt), is.na(arvo)) |> distinct(arvo_txt)
  if (nrow(huonot)) stop(taulu, ": numeroksi muuttumattomia arvoja: ", paste(utils::head(huonot$arvo_txt), collapse = ", "))
  out <- out |>
    filter(!grepl("muutos", tieto, ignore.case = TRUE)) |>
    select(taulu, alue, alue_taso, maa, tyyppi, tieto, pvm, ennakko, arvo)
  stopifnot(!anyNA(out$pvm), !anyNA(out$alue),
            !anyDuplicated(out[c("alue", "maa", "tyyppi", "tieto", "pvm")]))
  out
}

if ("fi" %in% vaiheet) {
  message("== Vaihe fi ==")
  taulut <- c("11iz", "11ix", "11lm")
  polut <- file.path(TALVI_RAW, sprintf("fi_%s.csv", taulut))
  puuttuu <- taulut[!file.exists(polut)]
  if (length(puuttuu)) stop("Lataa ensin taulut: ", paste(puuttuu, collapse = ", "),
                            " (ohjeet skriptin lopussa).")
  fi <- map2(polut, taulut, fi_lue) |> list_rbind()

  # Tarkistukset: ei-negatiiviset luvut; maakuntien summa vastaa koko maata
  stopifnot(all(fi$arvo >= 0, na.rm = TRUE))
  yop <- fi |> filter(taulu == "11iz", grepl("^Yöpymiset", tieto), maa == "Kaikki yhteensä")
  stopifnot(nrow(yop) > 0)
  tark <- yop |>
    filter(!alue %in% c("KOKO MAA", "MANNER-SUOMI")) |>
    group_by(pvm) |> summarise(summa = sum(arvo, na.rm = TRUE), n_na = sum(is.na(arvo)), .groups = "drop") |>
    inner_join(yop |> filter(alue == "KOKO MAA") |> select(pvm, koko = arvo), by = "pvm") |>
    filter(n_na == 0) |>
    mutate(ero_pros = 100 * (summa - koko) / koko)
  message(sprintf("Maakuntien yöpymisten summa vs. koko maa (kuukaudet ilman puuttuvia): suurin ero %.2f %%",
                  max(abs(tark$ero_pros), na.rm = TRUE)))
  if (max(abs(tark$ero_pros), na.rm = TRUE) > 1) warning("Maakuntien summa poikkeaa koko maasta yli 1 %.")

  print(fi |> group_by(taulu, alue_taso) |>
          summarise(alueita = n_distinct(alue), maita = n_distinct(maa), tietoja = n_distinct(tieto),
                    eka = min(pvm), vika = max(pvm), ennakko_kk = n_distinct(pvm[ennakko]),
                    puuttuu_pros = round(100 * mean(is.na(arvo)), 1), .groups = "drop"))
  tallenna_atom(fi, file.path(TALVI_DIR, "fi_majoitus.qs2"))
}

message("Valmis.")
