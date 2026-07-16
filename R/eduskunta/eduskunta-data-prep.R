# =====================================================================
# Eduskunta jakaumana — YHTEINEN DATA-PREP (osat 3–7)
# Hakee kerran kaiken, mitä sarjan osat tarvitsevat. Aja tämä ENSIN.
#   source(here::here("R", "eduskunta-data-prep.R"))
# Tulokset: here("data","eduskunta","*.qs")
# =====================================================================
library(tidyverse); library(here); library(httr2); library(xml2); library(qs2)
select <- dplyr::select; filter <- dplyr::filter   # MASS-maskaus pois

DATA <- here("data", "eduskunta")
dir.create(DATA, recursive = TRUE, showWarnings = FALSE)
REF_VUOSI <- 2025L

# ---------- Geneerinen sivutettu haku ----------
hae_taulu <- function(taulu, per_page = 100, max_sivut = 20000) {
  base <- "https://avoindata.eduskunta.fi/api/v1/tables"
  rivit <- list(); sar <- NULL; sivu <- 0
  repeat {
    js <- request(base) |> req_url_path_append(taulu, "rows") |>
      req_url_query(page = sivu, perPage = per_page) |>
      req_user_agent("kristianvepsalainen.com") |>
      req_retry(max_tries = 5) |> req_perform() |>
      resp_body_json(simplifyVector = FALSE)
    if (is.null(sar)) sar <- unlist(js$columnNames)
    if (length(js$rowData) == 0) break
    rivit <- c(rivit, js$rowData)
    if (isFALSE(js$hasMore)) break
    sivu <- sivu + 1; if (sivu >= max_sivut) break
    if (sivu %% 10 == 0) message("  sivu ", sivu, " (", length(rivit), " riviä)")
  }
  m <- do.call(rbind, lapply(rivit, \(r) unlist(lapply(r, \(x) x %||% NA))))
  colnames(m) <- sar
  as_tibble(m)
}

cache <- function(nimi, expr) {
  p <- file.path(DATA, paste0(nimi, ".qs"))
  if (!file.exists(p)) { message("Haetaan: ", nimi); qs2::qd_save(force(expr), p) }
  tryCatch(qs2::qd_read(p), error = function(e) qs2::qs_read(p))
}

# ---------- 1. Kansanedustajat (henkilötaso) ----------
mp_raw <- cache("mp_raw", hae_taulu("MemberOfParliament"))
stopifnot("MP-haku epäonnistui" = nrow(mp_raw) >= 2000,
          "XML-sarake puuttuu"  = "XmlDataFi" %in% names(mp_raw))

vuosi <- function(x) suppressWarnings(as.integer(str_extract(x, "\\d{4}")))

jasenna <- function(xml_str) {
  if (is.na(xml_str) || !nzchar(xml_str)) return(NULL)
  doc <- read_xml(xml_str)
  g <- function(xp) { v <- xml_text(xml_find_first(doc, xp))
                      if (length(v) == 0) NA_character_ else v }
  toimet <- xml_find_all(doc, ".//Edustajatoimet/Edustajatoimi")
  kaudet <- if (length(toimet) == 0) tibble(alku = NA_integer_, loppu = NA_integer_)
    else map_dfr(toimet, \(t) tibble(
      alku  = vuosi(xml_text(xml_find_first(t, "./AlkuPvm"))),
      loppu = vuosi(xml_text(xml_find_first(t, "./LoppuPvm")))))
  # Eduskuntaryhmä (suomeksi, päivätty) — nykyinen + edelliset
  ryhma_nyt <- g(".//Eduskuntaryhmat/NykyinenEduskuntaryhma/Nimi")
  ryhmat_ent <- xml_find_all(doc, ".//EdellisetEduskuntaryhmat/Eduskuntaryhma")
  ryhmat <- if (length(ryhmat_ent) == 0) tibble(ryhma = character(), alku = integer(), loppu = integer())
    else map_dfr(ryhmat_ent, \(r) {
      nimi <- xml_text(xml_find_first(r, "./Nimi"))
      js <- xml_find_all(r, "./Jasenyys")
      if (length(js) == 0) return(tibble(ryhma = nimi, alku = NA_integer_, loppu = NA_integer_))
      map_dfr(js, \(j) tibble(ryhma = nimi,
        alku  = vuosi(xml_text(xml_find_first(j, "./AlkuPvm"))),
        loppu = vuosi(xml_text(xml_find_first(j, "./LoppuPvm")))))
    })
  tibble(
    henkilo_nro   = g(".//HenkiloNro"),
    etunimi       = g(".//EtunimetNimi"),
    sukunimi      = g(".//SukuNimi"),
    sp_koodi      = g(".//SukuPuoliKoodi"),
    synt_pvm      = g(".//SyntymaPvm"),
    ammatti       = g(".//Ammatti"),
    ryhma_nyt     = ryhma_nyt,
    ministeri     = as.integer(length(xml_find_all(doc, ".//ValtioneuvostonJasenyydet/Jasenyys")) > 0),
    toimielin_lkm = length(xml_find_all(doc, ".//AiemmatToimielinjasenyydet/Toimielin")) +
                    length(xml_find_all(doc, ".//NykyisetToimielinjasenyydet/Toimielin")),
    kaudet        = list(kaudet),
    ryhmat        = list(ryhmat))
}

mp_long <- cache("mp_parsed", map_dfr(mp_raw$XmlDataFi, jasenna))
stopifnot("Parsittu MP-data vajaa" = nrow(mp_long) >= 2000,
          "toimielin_lkm puuttuu — poista mp_parsed.qs ja aja uudelleen" =
            "toimielin_lkm" %in% names(mp_long))

mp <- mp_long |> mutate(
  nimi = str_squish(paste(etunimi, sukunimi)),
  synt_vuosi = vuosi(synt_pvm),
  sp_l = str_to_lower(replace_na(sp_koodi, "")),
  sukupuoli = case_when(str_starts(sp_l, "mie") | sp_l == "1" ~ "Mies",
                        str_starts(sp_l, "nai") | sp_l == "2" ~ "Nainen",
                        TRUE ~ NA_character_))
tark <- mp |> filter(nimi %in% c("Esko Aho", "Raila Aho")) |> select(nimi, sukupuoli) |> deframe()
stopifnot("Sukupuolikoodin suunta väärin" =
  (is.na(tark["Esko Aho"]) || tark["Esko Aho"] == "Mies") &&
  (is.na(tark["Raila Aho"]) || tark["Raila Aho"] == "Nainen"))

# Edustajavuodet + urat
mp_years <- mp |> select(henkilo_nro, synt_vuosi, sukupuoli, kaudet) |> unnest(kaudet) |>
  filter(!is.na(alku)) |> mutate(loppu = coalesce(loppu, REF_VUOSI)) |>
  filter(loppu >= alku, between(alku, 1907, REF_VUOSI)) |>
  mutate(v = map2(alku, pmin(loppu, REF_VUOSI), seq)) |> unnest(v) |>
  rename(vuosi = v) |> distinct(henkilo_nro, vuosi, .keep_all = TRUE) |>
  mutate(ika = vuosi - synt_vuosi)
qs2::qs_save(mp_years, file.path(DATA, "mp_years.qs"))

# Ryhmäjäsenyys vuositasolla (puolue ajassa) — osaan 4 (hallitus/oppositio)
ryhma_years <- mp |> select(henkilo_nro, ryhmat) |> unnest(ryhmat) |>
  filter(!is.na(alku)) |> mutate(loppu = coalesce(loppu, REF_VUOSI)) |>
  filter(loppu >= alku) |>
  mutate(v = map2(alku, pmin(loppu, REF_VUOSI), seq)) |> unnest(v) |>
  rename(vuosi = v) |> distinct(henkilo_nro, vuosi, ryhma)
qs2::qs_save(ryhma_years, file.path(DATA, "ryhma_years.qs"))
qs2::qs_save(mp, file.path(DATA, "mp.qs"))

# ---------- 2. Täysistuntopuheenvuorot ----------
# !! VARMISTA kenttänimet: aja `glimpse(pv_raw)` ja tarkista alla oleva mäppäys.
pv_raw <- cache("puheenvuorot_raw", hae_taulu("SaliDBPuheenvuoro"))
message("SaliDBPuheenvuoro sarakkeet: ", paste(names(pv_raw), collapse = ", "))
stopifnot("Puhedata tyhjä" = nrow(pv_raw) > 0)

# Autodetect: etsitään kenttiä nimen perusteella, ettei koodi hajoa arvauksiin.
nm <- names(pv_raw)
pick <- function(...) { pats <- c(...); hit <- nm[str_detect(str_to_lower(nm), paste(pats, collapse = "|"))]
                        if (length(hit) == 0) NA_character_ else hit[1] }
c_hlo   <- pick("henkilonro", "henkilo_nro", "henkilonumero")
c_pvm   <- pick("^istuntopvm", "pvm", "aika", "date")
c_kesto <- pick("kesto", "duration")
c_tyyppi<- pick("tyyppi", "laji", "type")
c_teksti<- pick("teksti", "sisalto", "text")
message("Tunnistetut kentät -> henkilö: ", c_hlo, " | pvm: ", c_pvm,
        " | kesto: ", c_kesto, " | tyyppi: ", c_tyyppi)
stopifnot("Henkilö- tai pvm-kenttää ei tunnistettu — tarkista sarakelista yllä" =
            !is.na(c_hlo) && !is.na(c_pvm))

puheet <- pv_raw |>
  transmute(
    henkilo_nro = as.character(.data[[c_hlo]]),
    pvm   = as.Date(str_sub(.data[[c_pvm]], 1, 10)),
    vuosi = as.integer(format(pvm, "%Y")),
    kesto_s = if (!is.na(c_kesto)) suppressWarnings(as.numeric(.data[[c_kesto]])) else NA_real_,
    tyyppi  = if (!is.na(c_tyyppi)) as.character(.data[[c_tyyppi]]) else NA_character_,
    pituus_merkit = if (!is.na(c_teksti)) nchar(as.character(.data[[c_teksti]])) else NA_integer_) |>
  filter(!is.na(henkilo_nro), !is.na(vuosi))
stopifnot("Puheet tyhjä jäsennyksen jälkeen" = nrow(puheet) > 1000)
qs2::qs_save(puheet, file.path(DATA, "puheet.qs"))

# ---------- 3. Puhujakohtaiset vuosisummat (osien 3–7 työtaulu) ----------
puhe_vuosi <- puheet |> group_by(henkilo_nro, vuosi) |>
  summarise(n_puheita = n(),
            kesto_s_yht = sum(kesto_s, na.rm = TRUE),
            merkit_yht  = sum(pituus_merkit, na.rm = TRUE), .groups = "drop")

# Yhdistetään edustajavuosiin: myös NOLLAT mukaan (hiljaiset edustajat!)
paneeli <- mp_years |> filter(vuosi >= min(puheet$vuosi)) |>
  select(henkilo_nro, vuosi, ika, sukupuoli) |>
  left_join(puhe_vuosi, by = c("henkilo_nro", "vuosi")) |>
  left_join(ryhma_years, by = c("henkilo_nro", "vuosi")) |>
  mutate(across(c(n_puheita, kesto_s_yht, merkit_yht), \(x) replace_na(x, 0)))
stopifnot("Paneeli tyhjä" = nrow(paneeli) > 0,
          "Nollia ei löydy — hiljaiset puuttuvat" = any(paneeli$n_puheita == 0))
qs2::qs_save(paneeli, file.path(DATA, "paneeli.qs"))

message("VALMIS. Tiedostot: ", paste(list.files(DATA), collapse = ", "))
