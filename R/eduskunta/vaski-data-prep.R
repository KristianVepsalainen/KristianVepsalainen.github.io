# =====================================================================
# Eduskunta jakaumana — VASKI-DATA-PREP (osat 8–13)
# Hakee ja jäsentää valtiopäiväasiakirjat (VaskiData) kerran.
#   source(here::here("R", "vaski-data-prep.R"))
# Edellyttää: R/eduskunta-data-prep.R ajettu (mp.qs olemassa).
# =====================================================================
library(tidyverse); library(here); library(httr2); library(xml2); library(qs2)
select <- dplyr::select; filter <- dplyr::filter

DATA <- here("data", "eduskunta")
stopifnot("Aja ensin R/eduskunta-data-prep.R" = file.exists(file.path(DATA, "mp.qs")))
mp <- qs2::qd_read(file.path(DATA, "mp.qs"))

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
    if (sivu %% 50 == 0) message("  sivu ", sivu, " (", length(rivit), " riviä)")
  }
  m <- do.call(rbind, lapply(rivit, \(r) unlist(lapply(r, \(x) x %||% NA))))
  colnames(m) <- sar; as_tibble(m)
}
cache <- function(nimi, expr) {
  p <- file.path(DATA, paste0(nimi, ".qs"))
  if (!file.exists(p)) { message("Haetaan: ", nimi); qs2::qd_save(force(expr), p) }
  tryCatch(qs2::qd_read(p), error = function(e) qs2::qs_read(p))
}

# ---------- 1. VaskiData raakana ----------
vaski_raw <- cache("vaski_raw", hae_taulu("VaskiData"))
stopifnot("VaskiData tyhjä" = nrow(vaski_raw) > 100000)

# ---------- 2. Kaikkien asiakirjojen perustiedot (EDA, osa 8) ----------
# Ei XML-purkua — tyyppi ja vuosi tulevat Eduskuntatunnuksesta.
vaski_docs <- vaski_raw |>
  transmute(
    id = Id,
    tyyppi  = str_extract(Eduskuntatunnus, "^[A-Za-zÄÖ]+"),
    vpvuosi = as.integer(str_extract(Eduskuntatunnus, "\\d{4}")),
    created = suppressWarnings(as.Date(str_sub(Created, 1, 10))),
    status  = Status,
    tunnus  = Eduskuntatunnus) |>
  filter(!is.na(tyyppi))
stopifnot("vaski_docs tyhjä" = nrow(vaski_docs) > 100000)
qs2::qd_save(vaski_docs, file.path(DATA, "vaski_docs.qs"))

# ---------- 3. Aloitteet: tekijä, aiheet, allekirjoittajat (osat 9–11) ----------
# Nimiavaruudet -> local-name(). Tekijäpolku validoitu (osuvuus > 0.8 aiemmin).
ln  <- function(x) paste0("*[local-name()='", x, "']")
xp  <- function(...) paste0(".//", paste(sapply(c(...), ln), collapse = "//"))

poimi_init <- function(xml_str, tyyppi_koodi) {
  doc <- tryCatch(read_xml(xml_str), error = function(e) NULL)
  if (is.null(doc)) return(tibble(tekija=NA_character_, vpvuosi=NA_integer_,
                                  tyyppi=tyyppi_koodi, aiheet=list(character()),
                                  allekirjoittajat=list(character())))
  tekija <- xml_attr(xml_find_first(doc, xp("RakenneAsiakirja","IdentifiointiOsa","Henkilo")),
                     "muuTunnus")
  vpv <- suppressWarnings(as.integer(xml_text(xml_find_first(doc, xp("ValtiopaivavuosiTeksti")))))
  aiheet <- xml_text(xml_find_all(doc, xp("AiheTeksti")))
  allek  <- xml_attr(xml_find_all(doc, xp("AllekirjoitusOsa","Henkilo")), "muuTunnus")
  tibble(tekija = tekija, vpvuosi = vpv, tyyppi = tyyppi_koodi,
         aiheet = list(aiheet[!is.na(aiheet) & aiheet != ""]),
         allekirjoittajat = list(unique(allek[!is.na(allek)])))
}

init_raw <- vaski_docs |> filter(tyyppi %in% c("KK","LA","TPA","TAA")) |>
  left_join(vaski_raw |> select(Id, XmlData), by = c("id" = "Id"))
stopifnot("Aloitteita ei löytynyt" = nrow(init_raw) > 1000)

vaski_init <- cache("vaski_init",
  init_raw |> mutate(p = map2(XmlData, tyyppi, poimi_init)) |>
    select(id, tunnus, created, p) |> unnest(p))

osuvuus <- mean(vaski_init$tekija %in% mp$henkilo_nro, na.rm = TRUE)
stopifnot("Tekijätunnus ei linkity MP-dataan" = osuvuus > 0.7)
message("Aloitteiden tekijälinkitys: ", scales::percent(osuvuus))

# ---------- 4. VERIFIOINTIA VAATIVAT (osat 9, 12, 13) ----------
# Aja nämä diagnostiikat kerran ja liitä tulokset, niin viimeistelen parserit.
if (FALSE) {
  # (A) OSA 9 — Aloitteen lopputulos: mistä "hyväksytty/hylätty" löytyy?
  #     Lopputulos EI ole aloitteessa itsessään vaan valiokuntamietinnössä (VM)
  #     tai täysistunnon päätöksessä (EK/EV). Katso mietinnön rakenne:
  vm <- vaski_raw |> filter(str_detect(Eduskuntatunnus, "VM ")) |> slice(1) |> pull(XmlData)
  xml2::xml_structure(xml2::read_xml(vm))

  # (B) OSA 12 — Valiokuntien käsittelyajat: mietinnön pvm vs. asian vireilletulo.
  #     Etsi laadintaPvm ja vireilletulopvm mietinnöstä:
  xml2::xml_find_all(xml2::read_xml(vm), ".//*[local-name()='LaadintaPvmTeksti']") |> xml2::xml_text()

  # (C) OSA 13 — KK:n vastaanottava ministeri: mistä roolista se löytyy?
  kk <- vaski_raw |> filter(str_starts(Eduskuntatunnus, "KK ")) |> slice(1) |> pull(XmlData)
  doc <- xml2::read_xml(kk)
  xml2::xml_find_all(doc, ".//*[local-name()='Toimija']") |>
    purrr::map(\(t) c(rooli = xml2::xml_attr(t, "rooliKoodi"),
                      teksti = xml2::xml_text(xml2::xml_find_first(t, ".//*[local-name()='YhteisoTeksti']"))))
}

message("VALMIS. Tiedostot: ", paste(list.files(DATA, pattern = "^vaski"), collapse = ", "))
