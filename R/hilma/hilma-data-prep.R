# =============================================================================
# R/hilma/hilma-data-prep.R
# Yhteinen datahaku Hilma-sarjalle "Julkinen raha ei valehtele".
#
# Hakee KERRAN kaikki sarjan osien tarvitsemat aineistot ja tallentaa ne
# kansioon data/hilma/. Postaukset lukevat vain näitä tiedostoja, eivätkä
# koskaan kutsu rajapintoja itse (freeze + CI eivät aja R:ää).
#
# LÄHTEET
#   1. Hilma AVP -hakuindeksi (kaikki ilmoitukset, myös ennen eFormsia)
#        POST https://api.hankintailmoitukset.fi/avp/eformnotices/docs/search
#   2. Hilma eForms -lukurajapinta (koko ilmoitus Base64-XML:nä)
#        GET  https://api.hankintailmoitukset.fi/avp-eforms/external-read/v1/notice/{id}
#   3. PRH/YTJ v3 (voittajien ja hankintayksiköiden perustiedot)
#   4. Paavo-postinumeroaluedata (paikallinen data/paavo/paavo.fst)
#   5. Eurostat: HICP-hintaindeksi ja julkisyhteisöjen menot
#   6. Tutkihankintoja.fi-ostolaskut (Avoindata.fi, CC BY 4.0)
#
# ENNEN AJOA
#   * Luo maksuton avain: https://hns-hilma-prod-apim.developer.azure-api.net
#     -> Products -> avp-read -> Subscribe. Lisää .Renviron-tiedostoon:
#         HILMA_API_KEY=xxxxxxxx
#   * Aja vaiheittain (RUN-lippu alla). Jokainen vaihe välimuistittaa itsensä,
#     joten keskeytys ei pakota aloittamaan alusta. Poista välimuistitiedosto,
#     jos haluat hakea vaiheen uudelleen.
#   * eForms-haku on hidas (kymmeniä tuhansia ilmoituksia, rajapinnalla on
#     avainkohtainen nopeusrajoitus). Aja se yön yli.
#
# TALLENNUSMUOTO (qs2)
#   qd_save()/qd_read()  -> data.framet (ei list-sarakkeita!)
#   qs_save()/qs_read()  -> listat ja muut R-oliot
# =============================================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(httr2)
  library(here)
  library(qs2)
  library(fst)
  library(jsonlite)
})

select <- dplyr::select
filter <- dplyr::filter

source(here("R", "hilma", "hilma-eforms-parse.R"))

RUN <- list(
  index     = TRUE,   # 1. hakuindeksi
  eforms    = TRUE,   # 2. eForms-XML:t (hidas)
  ytj       = TRUE,   # 3. YTJ-rikastus
  paavo     = TRUE,   # 4. postinumero -> kunta + koordinaatit
  eurostat  = TRUE,   # 5. hintaindeksi + makrotason hankinnat
  tutki     = TRUE    # 6. ostolaskut (suuri lataus)
)

out_dir <- here("data", "hilma")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
stopifnot(dir.exists(out_dir))

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

hilma_key <- Sys.getenv("HILMA_API_KEY")
if (RUN$index || RUN$eforms) {
  if (!nzchar(hilma_key)) stop("HILMA_API_KEY puuttuu .Renviron-tiedostosta.")
}

HILMA_SEARCH <- "https://api.hankintailmoitukset.fi/avp/eformnotices/docs/search"
HILMA_DEF    <- "https://api.hankintailmoitukset.fi/avp/eformnotices"
HILMA_READ   <- "https://api.hankintailmoitukset.fi/avp-eforms/external-read/v1/notice"

# Yhteinen pyyntöpohja: avain, nopeusrajoitus, uudelleenyritys 429/5xx-virheissä
hilma_req <- function(url) {
  request(url) |>
    req_headers(`Ocp-Apim-Subscription-Key` = hilma_key,
                `Content-Type` = "application/json") |>
    req_throttle(rate = 2) |>                       # max 2 pyyntöä/s
    req_retry(max_tries = 6,
              is_transient = \(resp) resp_status(resp) %in% c(429, 500, 502, 503, 504),
              backoff = \(i) min(60, 2^i)) |>
    req_timeout(120)
}

# Taulukoi Azure Search -vastauksen rivit. qd_save ei tue list-sarakkeita, joten
#  - yksinkertaiset taulukot (esim. nutsCodes, cpvCodes) -> "|"-merkkijono
#  - sisäkkäiset oliot (esim. lots, winnerOrganisations) -> JSON-merkkijono,
#    jotta rakenne säilyy ja sen voi purkaa myöhemmin jsonlite::fromJSON():lla
flatten_hits <- function(hits) {
  if (length(hits) == 0) return(tibble())
  map_dfr(hits, function(h) {
    h[["@search.score"]] <- NULL
    h <- map(h, function(v) {
      if (is.null(v) || length(v) == 0) return(NA_character_)
      if (is.list(v) && any(map_lgl(v, \(e) is.list(e) || length(e) > 1)))
        return(as.character(jsonlite::toJSON(v, auto_unbox = TRUE, null = "null")))
      if (is.list(v) || length(v) > 1) return(paste(unlist(v), collapse = "|"))
      as.character(v)
    })
    as_tibble(h)
  })
}

# =============================================================================
# 1. HAKUINDEKSI
# =============================================================================
index_file <- file.path(out_dir, "hilma_index.qs2")
fields_file <- file.path(out_dir, "hilma_index_fields.qs2")

hilma_search <- function(body) {
  hilma_req(HILMA_SEARCH) |>
    req_body_json(body, auto_unbox = TRUE) |>
    req_perform() |>
    resp_body_json(check_type = FALSE, simplifyVector = FALSE)
}

if (RUN$index && !file.exists(index_file)) {

  # 1a. Indeksin kenttämäärittely talteen (dokumentoi mitä kenttiä on)
  defs <- tryCatch(
    hilma_req(HILMA_DEF) |> req_perform() |>
      resp_body_json(check_type = FALSE, simplifyVector = TRUE),
    error = function(e) { message("Kenttämäärittelyä ei saatu: ", conditionMessage(e)); NULL })
  if (!is.null(defs)) qs_save(defs, fields_file)

  # 1b. Testihaku: rakenne kunnossa?
  # Huom: nouseva järjestys laittaa tyhjät (null) päivämäärät ensimmäisiksi,
  # joten vanhin päivämäärä haetaan vain riveistä, joilla päivämäärä on.
  probe <- hilma_search(list(search = "*", top = 1L, count = TRUE,
                             filter = "datePublished ne null",
                             orderby = "datePublished asc"))
  n_total_all <- hilma_search(list(search = "*", top = 0L, count = TRUE))[["@odata.count"]]
  n_total <- probe[["@odata.count"]]
  stopifnot(!is.null(n_total), n_total > 1000, length(probe$value) == 1)
  first_date <- as.Date(str_sub(probe$value[[1]]$datePublished %||% NA_character_, 1, 10))
  stopifnot(length(first_date) == 1, !is.na(first_date),
            first_date > as.Date("2000-01-01"))
  n_nodate <- n_total_all - n_total
  message("Indeksissä ", n_total_all, " ilmoitusta, joista ", n_nodate,
          " ilman julkaisupäivää. Vanhin julkaisupäivä ", first_date)

  # Päivämäärättömät talteen erikseen, jotta niiden luonne voidaan tarkistaa
  nodate_file <- file.path(out_dir, "hilma_index_nodate.qs2")
  if (n_nodate > 0 && !file.exists(nodate_file)) {
    stopifnot(n_nodate < 100000)
    nodate <- map(seq(0, n_nodate - 1, by = 1000), function(sk) {
      res <- hilma_search(list(search = "*", filter = "datePublished eq null",
                               top = 1000L, skip = as.integer(sk),
                               orderby = "noticeId asc"))
      flatten_hits(res$value)
    }) |> bind_rows()
    qd_save(nodate, nodate_file)
    message("Päivämäärättömät: ", nrow(nodate), " riviä; mainType-jakauma:")
    if ("mainType" %in% names(nodate)) print(count(nodate, mainType, sort = TRUE))
  }

  required <- c("noticeId", "datePublished", "mainType", "estimatedValue",
                "cpvCodes", "organisationNationalRegistrationNumber",
                "procedureType", "procurementTypeCode", "eFormsId")
  missing_fields <- setdiff(required, names(probe$value[[1]]))
  if (length(missing_fields) > 0) {
    # Kenttä voi puuttua ensimmäisestä rivistä, jos se on tyhjä -> varoitus
    warning("Ensimmäiseltä riviltä puuttuu kenttiä: ",
            paste(missing_fields, collapse = ", "),
            ". Tarkista kenttämäärittely (", fields_file, ").")
  }

  # 1c. Haku kuukausi kerrallaan (Azure Search: top <= 1000, skip <= 100 000)
  month_seq <- seq(floor_date(first_date, "month"), floor_date(Sys.Date(), "month"),
                   by = "month")
  month_dir <- file.path(out_dir, "index_months")
  dir.create(month_dir, showWarnings = FALSE)

  for (i in seq_along(month_seq)) {
    m0 <- month_seq[i]
    m1 <- m0 %m+% months(1)
    f  <- file.path(month_dir, sprintf("idx_%s.qs2", format(m0, "%Y_%m")))
    is_current <- m0 == floor_date(Sys.Date(), "month")
    if (file.exists(f) && !is_current) next

    flt <- sprintf("datePublished ge %sT00:00:00Z and datePublished lt %sT00:00:00Z",
                   m0, m1)
    first <- hilma_search(list(search = "*", filter = flt, top = 0L, count = TRUE))
    n_m <- first[["@odata.count"]] %||% 0L
    stopifnot(n_m < 100000)
    if (n_m == 0) { qd_save(tibble(), f); next }

    pages <- map(seq(0, n_m - 1, by = 1000), function(sk) {
      res <- hilma_search(list(search = "*", filter = flt, top = 1000L,
                               skip = as.integer(sk), count = FALSE,
                               orderby = "noticeId asc"))
      flatten_hits(res$value)
    })
    df <- bind_rows(pages)
    # Tarkistus: haettiin yhtä monta kuin rajapinta ilmoitti
    if (nrow(df) != n_m) warning(format(m0), ": haettu ", nrow(df), " / ", n_m)
    qd_save(df, f)
    message(format(m0, "%Y-%m"), ": ", nrow(df), " ilmoitusta")
  }

  idx_raw <- list.files(month_dir, full.names = TRUE) |>
    map(qd_read) |>
    bind_rows()

  # Yksilöivä avain on indeksin oma `id`, jonka etuliite kertoo lähteen:
  #   OLD-  ennen eFormsia (2018–2024) julkaistut ilmoitukset
  #   EF-   eForms-ilmoitukset (2023/2024 alkaen)
  #   PLAN- hankintasuunnitelmat (noticeId = 0 -> NA)
  # OLD- ja EF- ovat ERI numeroavaruuksia: sama noticeId voi esiintyä
  # kummassakin täysin eri ilmoituksena. noticeId on siis yksilöivä vain
  # lähteen sisällä, ja eForms-lukurajapinta käyttää EF-rivien noticeId:tä.
  stopifnot("id" %in% names(idx_raw), !any(is.na(idx_raw$id)))
  idx <- idx_raw |>
    distinct(id, .keep_all = TRUE) |>
    mutate(
      source         = str_extract(id, "^[A-Z]+"),
      noticeId       = na_if(as.integer(noticeId), 0L),
      datePublished  = ymd_hms(datePublished, tz = "UTC", quiet = TRUE),
      estimatedValue = suppressWarnings(as.numeric(estimatedValue)),
      across(any_of(c("expirationDate", "deadline", "dateModified")),
             \(x) ymd_hms(x, tz = "UTC", quiet = TRUE)),
      across(any_of(c("noticeResultTotalAmount", "overallMaximumFrameworkContractsAmount",
                      "overallApproximateFrameworkContractsAmount")),
             \(x) suppressWarnings(as.numeric(x))),
      across(any_of(c("isPlan", "isNationalProcurement", "isEuProcurement",
                      "isNationalSmallValueProcurement", "isPrivateSmallValueProcurement",
                      "isEForms", "isCorrigendum", "isCancelled",
                      "includesFrameworkAgreement", "includesDynamicPurcharingSystem")),
             \(x) x %in% c("TRUE", "true"))
    )

  # Tarkistukset
  n_dup_id <- nrow(idx_raw) - nrow(idx)
  if (n_dup_id > 0) message("Poistettu ", n_dup_id, " sivutuksen kaksoisriviä (sama id)")
  print(count(idx, source, isPlan, isEForms))
  non_plan <- idx |> filter(!isPlan)
  stopifnot(
    nrow(idx) > 0.99 * n_total,                       # lähes kaikki saatiin
    !anyDuplicated(idx$id),
    all(idx$source %in% c("OLD", "EF", "PLAN")),      # ei tuntemattomia etuliitteitä
    all(idx$isPlan == (idx$source == "PLAN")),
    all(idx$isEForms[idx$source == "EF"]),            # EF-rivit ovat eFormseja
    !anyDuplicated(non_plan[c("source", "noticeId")]),# noticeId yksilöivä lähteen sisällä
    all(!is.na(non_plan$noticeId)),
    all(is.na(idx$noticeId[idx$isPlan])),             # suunnitelmilla ei noticeId:tä
    mean(!is.na(idx$datePublished)) > 0.99,
    all(idx$estimatedValue >= 0, na.rm = TRUE)
  )

  # Varoitus: kasautuuko sama julkaisuaikaleima epäilyttävän monelle riville?
  # (esim. EF-10054…10058 = 2024-05-23 07:05:07 -> mahdollinen migraatioajo)
  stamp <- idx |> filter(!isPlan) |> count(datePublished, sort = TRUE)
  if (stamp$n[1] > 20) {
    message("HUOM: ", sum(stamp$n[stamp$n > 20]), " riviä jakaa saman julkaisu-",
            "aikaleiman jonkin toisen kanssa (>20 samalla sekunnilla). Yleisimmät:")
    print(head(stamp, 5))
  }

  message("Indeksi: ", nrow(idx), " riviä (", sum(idx$isPlan), " hankintasuunnitelmaa), ",
          ncol(idx), " saraketta")
  print(count(idx, mainType, sort = TRUE))
  qd_save(idx, index_file)
}

# =============================================================================
# 2. eFORMS-ILMOITUKSET (koko XML) -> tidy-taulut
# =============================================================================
eforms_dir   <- file.path(out_dir, "eforms_raw")      # raaka-XML paloina
parsed_dir   <- file.path(out_dir, "eforms_parsed")   # jäsennetyt palat
manifest_f   <- file.path(out_dir, "eforms_manifest.qs2")
eforms_file  <- file.path(out_dir, "hilma_eforms.qs2")
awards_file  <- file.path(out_dir, "hilma_awards.qs2")
CHUNK <- 500L

read_eform_one <- function(id) {
  resp <- hilma_req(paste0(HILMA_READ, "/", id)) |>
    req_error(is_error = \(r) FALSE) |>
    req_perform()
  st <- resp_status(resp)
  if (st != 200) return(list(status = st, xml = NA_character_))
  js <- resp_body_json(resp, check_type = FALSE, simplifyVector = FALSE)
  b64 <- js$eForm %||% NA_character_
  if (is.na(b64)) return(list(status = -1L, xml = NA_character_))
  list(status = 200L, xml = rawToChar(base64_dec(b64)))
}

if (RUN$eforms) {
  stopifnot(file.exists(index_file))
  dir.create(eforms_dir, showWarnings = FALSE)
  dir.create(parsed_dir, showWarnings = FALSE)
  idx <- qd_read(index_file)

  # Kohdejoukko: eForms-muotoiset hankinta- ja jälki-ilmoitukset
  target <- idx |>
    filter(source == "EF", !is.na(eFormsId), eFormsId != "",
           mainType %in% c("ContractNotices", "ContractAwardNotices")) |>
    pull(noticeId) |>
    sort()
  message("eForms-kohdejoukko: ", length(target), " ilmoitusta")

  # Ensimmäinen ilmoitus testataan erikseen ennen pitkää ajoa
  t1 <- read_eform_one(target[1])
  stopifnot(t1$status == 200,
            str_detect(t1$xml, "^\\s*<\\?xml|<(ContractNotice|ContractAwardNotice|PriorInformationNotice)"))
  stopifnot(!is.null(parse_eform(t1$xml, target[1])))

  manifest <- if (file.exists(manifest_f)) qd_read(manifest_f) else
    tibble(noticeId = integer(), status = integer(), chunk = integer())
  todo <- setdiff(target, manifest$noticeId[manifest$status %in% c(200L, 404L)])
  next_chunk <- if (nrow(manifest) == 0) 1L else max(manifest$chunk) + 1L
  message("Haettavana ", length(todo), " ilmoitusta")

  for (ids in split(todo, ceiling(seq_along(todo) / CHUNK))) {
    res <- map(ids, read_eform_one, .progress = TRUE)
    xmls <- set_names(map_chr(res, "xml"), ids)
    stat <- map_int(res, "status")

    ok <- !is.na(xmls)
    qs_save(as.list(xmls[ok]), file.path(eforms_dir, sprintf("chunk_%05d.qs2", next_chunk)))
    parsed <- map2(xmls[ok], as.integer(names(xmls)[ok]), parse_eform_safe) |> bind_eforms()
    qs_save(parsed, file.path(parsed_dir, sprintf("chunk_%05d.qs2", next_chunk)))

    manifest <- bind_rows(manifest,
                          tibble(noticeId = as.integer(ids), status = stat,
                                 chunk = next_chunk))
    qd_save(manifest, manifest_f)
    message(sprintf("Pala %d: %d ok, %d virhettä", next_chunk, sum(ok), sum(!ok)))
    next_chunk <- next_chunk + 1L
  }

  # Kaikki palat yhteen
  chunks <- list.files(parsed_dir, full.names = TRUE) |> map(qs_read)
  ef <- set_names(
    map(c("notice", "lots", "results", "tenders", "parties", "contracts", "orgs"),
        \(n) bind_rows(map(chunks, n)) |> distinct()),
    c("notice", "lots", "results", "tenders", "parties", "contracts", "orgs"))

  # eFormsin juurielementtejä on kolme. Hilman "ContractNotices" sisältää myös
  # ennakkoilmoituksia, joilla avataan kilpailu (PriorInformationNotice,
  # alalajit 7–9 ja E2). Ne ovat oikeita ilmoituksia, eivät virheitä.
  message("Juurielementit ja alalajit:")
  print(count(ef$notice, root_element, notice_subtype, sort = TRUE), n = 50)
  known_roots <- c("ContractNotice", "ContractAwardNotice", "PriorInformationNotice")
  unknown <- ef$notice |> filter(!root_element %in% known_roots)
  if (nrow(unknown) > 0) {
    warning(nrow(unknown), " ilmoitusta tuntemattomalla juurielementillä: ",
            paste(unique(unknown$root_element), collapse = ", "))
  }
  # Negatiiviset euro- ja lukumääräarvot ovat syöttövirheitä (ilmoittaja on
  # kirjoittanut esim. -1 tai miinusmerkin). Ne raportoidaan, tallennetaan
  # erikseen tarkastelua varten ja muutetaan NA:ksi. Jos niitä on paljon,
  # kyse ei ole yksittäisistä virheistä vaan jäsennysongelmasta -> pysäytys.
  neg_tend <- ef$tenders |> filter(tender_value < 0)
  neg_res  <- ef$results |> filter(n_tenders < 0)
  message("Negatiivisia tarjousarvoja: ", nrow(neg_tend), " / ",
          sum(!is.na(ef$tenders$tender_value)),
          "; negatiivisia tarjousmääriä: ", nrow(neg_res))
  if (nrow(neg_tend) > 0) print(head(arrange(neg_tend, tender_value), 10))
  qd_save(bind_rows(neg_tend |> mutate(issue = "tender_value < 0"),
                    neg_res  |> mutate(issue = "n_tenders < 0")),
          file.path(out_dir, "eforms_negative_values.qs2"))
  stopifnot(nrow(neg_tend) < 0.01 * sum(!is.na(ef$tenders$tender_value)),
            nrow(neg_res)  < 0.01 * sum(!is.na(ef$results$n_tenders)))
  ef$tenders <- ef$tenders |> mutate(tender_value = if_else(tender_value < 0, NA_real_, tender_value))
  ef$results <- ef$results |> mutate(n_tenders = if_else(n_tenders < 0, NA_real_, n_tenders))

  stopifnot(
    !anyDuplicated(ef$notice$notice_id),
    mean(ef$notice$root_element %in% known_roots) > 0.99,
    all(ef$results$n_tenders >= 0, na.rm = TRUE),
    all(ef$tenders$tender_value >= 0, na.rm = TRUE)
  )
  fail_rate <- mean(!manifest$status %in% c(200L, 404L))
  message("eForms: ", nrow(ef$notice), " jäsennettyä ilmoitusta, virheosuus ",
          round(100 * fail_rate, 2), " %")
  if (fail_rate > 0.02) warning("Yli 2 % hauista epäonnistui - aja vaihe uudelleen.")

  qs_save(ef, eforms_file)

  awards <- build_awards(ef)
  stopifnot(nrow(awards) > 0,
            all(awards$result_code == "selec-w"))
  qd_save(awards, awards_file)
  message("Voittajataulu: ", nrow(awards), " riviä")
}

# =============================================================================
# 3. YTJ-RIKASTUS (voittajat + hankintayksiköt)
# =============================================================================
ytj_file <- file.path(out_dir, "ytj_hilma.qs2")

# Normalisoi Y-tunnus muotoon 1234567-8. Muut maat -> NA.
norm_ytunnus <- function(x) {
  x <- str_to_upper(str_remove_all(x %||% NA_character_, "\\s"))
  x <- str_remove(x, "^FI")
  x <- if_else(str_detect(x, "^\\d{8}$"),
               paste0(str_sub(x, 1, 7), "-", str_sub(x, 8, 8)), x)
  if_else(str_detect(x, "^\\d{7}-\\d$"), x, NA_character_)
}

# Y-tunnuksen tarkistusmerkki (PRH:n algoritmi) -> karsii kirjoitusvirheet
valid_ytunnus <- function(x) {
  w <- c(7, 9, 10, 5, 8, 4, 2)
  map_lgl(x, function(y) {
    if (is.na(y)) return(FALSE)
    d <- as.integer(strsplit(str_sub(y, 1, 7), "")[[1]])
    r <- sum(d * w) %% 11
    chk <- if (r == 0) 0L else 11L - r
    r != 1 && chk == as.integer(str_sub(y, 9, 9))
  })
}

parse_ytj_company <- function(x) {
  addrs <- x$addresses %||% list()
  pc <- NA_character_
  if (length(addrs) > 0) {
    types <- map_chr(addrs, \(a) as.character(a$type %||% NA))
    for (i in order(types != "1")) {          # käyntiosoite (type 1) ensin
      p <- addrs[[i]]$postCode %||% NA_character_
      if (!is.na(p) && nzchar(p)) { pc <- p; break }
    }
  }
  nm <- x$names %||% list()
  tibble(
    business_id  = x$businessId$value %||% NA_character_,
    ytj_name     = if (length(nm) > 0) nm[[1]]$name %||% NA_character_ else NA_character_,
    tol          = x$mainBusinessLine$type %||% NA_character_,
    company_form = pluck(x, "companyForms", 1, "type", .default = NA_character_),
    reg_date     = x$registrationDate %||% NA_character_,
    end_date     = x$endDate %||% NA_character_,
    ytj_postcode = pc
  )
}

# 3a. ENSISIJAINEN: YTJ-sarjan valmis koko rekisterin aineisto
#     data/ytj/ytj_perustiedot.qs ("Suomalaisen yritystiedon syväanalyysi").
#     Se kattaa kaikki suomalaiset yritykset, joten voittajat löytyvät sieltä
#     ilman yhtään API-kutsua. API-haku (3b) ajetaan vain, jos tiedostoa ei ole.
ytj_local <- here("data", "ytj", "ytj_perustiedot.qs")

read_qs_any <- function(f) tryCatch(qd_read(f), error = function(e) qs_read(f))

if (RUN$ytj && !file.exists(ytj_file) && file.exists(ytj_local)) {
  pt <- read_qs_any(ytj_local) |> as_tibble()
  message("YTJ-sarjan perustiedot: ", nrow(pt), " riviä; sarakkeet: ",
          paste(names(pt), collapse = ", "))
  stopifnot(all(c("business_id", "company_form_code", "registration_date",
                  "main_line_code") %in% names(pt)))

  # Valinnaiset sarakkeet otetaan, jos ne ovat olemassa
  pick <- function(cands) {
    hit <- intersect(cands, names(pt))
    if (length(hit) == 0) NA_character_ else as.character(pt[[hit[1]]])
  }
  ytj <- tibble(
    business_id  = pt$business_id,
    ytj_name     = pick(c("name", "company_name", "nimi")),
    tol          = as.character(pt$main_line_code),
    company_form = as.character(pt$company_form_code),
    reg_date     = as.Date(pt$registration_date),
    end_date     = as.Date(pick(c("end_date", "endDate", "lakkautus_pvm"))),
    ytj_postcode = str_pad(pick(c("post_code", "postcode", "postinumero", "postCode")),
                           5, pad = "0"),
    location     = pick(c("location", "kotipaikka")),
    trade_register_status = pick(c("trade_register_status")),
    fetch_ok     = TRUE
  ) |>
    distinct(business_id, .keep_all = TRUE)

  # Kattavuus: kuinka moni Hilman voittajista ja ostajista löytyy?
  idx_tmp <- qd_read(index_file)
  hilma_ids <- norm_ytunnus(idx_tmp$organisationNationalRegistrationNumber)
  if (file.exists(awards_file)) {
    win_ids <- norm_ytunnus(qd_read(awards_file)$winner_company_id)
    win_ids <- unique(win_ids[!is.na(win_ids) & valid_ytunnus(win_ids)])
    missing_win <- setdiff(win_ids, ytj$business_id)
    message("Voittajista löytyy YTJ-aineistosta ",
            round(100 * mean(win_ids %in% ytj$business_id), 1), " % (",
            length(missing_win), " puuttuu)")

    # Puuttuvat ovat todennäköisesti muita kuin YTJ-sarjan hakemia
    # yhtiömuotoja: toiminimiä, yhdistyksiä, säätiöitä, kuntayhtymiä,
    # hyvinvointialueita (in-house-hankinnat). Haetaan VAIN ne API:sta
    # nopeasti epäonnistuvalla haulla: jos PRH ei vastaa, luovutetaan heti.
    if (length(missing_win) > 0) {
      fetch_fast <- function(bid) {
        tryCatch({
          resp <- request("https://avoindata.prh.fi/opendata-ytj-api/v3/companies") |>
            req_url_query(businessId = bid) |>
            req_throttle(rate = 2) |>
            req_retry(max_tries = 2, retry_on_failure = TRUE, backoff = \(i) 3) |>
            req_timeout(20) |>
            req_error(is_error = \(r) FALSE) |>
            req_perform()
          if (resp_status(resp) != 200) return(tibble(business_id = bid, fetch_ok = resp_status(resp) == 404))
          comp <- resp_body_json(resp, check_type = FALSE, simplifyVector = FALSE)$companies %||% list()
          if (length(comp) == 0) tibble(business_id = bid, fetch_ok = TRUE)
          else parse_ytj_company(comp[[1]]) |> mutate(business_id = bid, fetch_ok = TRUE)
        }, error = function(e) tibble(business_id = bid, fetch_ok = FALSE))
      }

      miss_file <- file.path(out_dir, "ytj_missing_winners.qs2")
      got <- if (file.exists(miss_file)) qd_read(miss_file) else tibble(business_id = character(), fetch_ok = logical())
      todo_m <- setdiff(missing_win, got$business_id[got$fetch_ok])
      message("Haetaan API:sta ", length(todo_m), " puuttuvaa voittajaa (n. ",
              ceiling(length(todo_m) / 2 / 60), " min)")
      fails_in_row <- 0L
      for (b in todo_m) {
        r <- fetch_fast(b)
        got <- bind_rows(got |> filter(business_id != b), r)
        fails_in_row <- if (r$fetch_ok) 0L else fails_in_row + 1L
        if (fails_in_row >= 20) {
          warning("PRH ei vastaa (20 peräkkäistä epäonnistumista). Jatketaan ilman ",
                  "loppuja; aja vaihe myöhemmin uudelleen täydentääksesi.")
          break
        }
      }
      # Jos kaikki haut epäonnistuivat, tietosarakkeita ei synny lainkaan ->
      # lisätään puuttuvat sarakkeet tyhjinä, ettei suodatus kaadu.
      for (col in c("ytj_name", "tol", "company_form", "reg_date", "end_date", "ytj_postcode")) {
        if (!col %in% names(got)) got[[col]] <- NA_character_
      }
      qd_save(got, miss_file)
      add <- got |> filter(fetch_ok, !is.na(ytj_name)) |>
        mutate(reg_date = as.Date(reg_date), end_date = as.Date(end_date),
               ytj_postcode = str_pad(ytj_postcode, 5, pad = "0"))
      message("API-täydennys: ", nrow(add), " / ", length(todo_m), " löytyi")
      ytj <- bind_rows(ytj, add) |> distinct(business_id, .keep_all = TRUE)
      cov <- mean(win_ids %in% ytj$business_id)
      message("Kattavuus täydennyksen jälkeen: ", round(100 * cov, 1), " %")
      print(count(add, company_form, sort = TRUE))
      if (cov < 0.95) warning("Voittajista puuttuu yhä ", round(100 * (1 - cov), 1),
                              " % - yrityskokoon perustuvat tulokset kattavat vain löydetyt.")
    }
  }
  buy_ids <- unique(hilma_ids[!is.na(hilma_ids)])
  message("Hankintayksiköistä löytyy ",
          round(100 * mean(buy_ids %in% ytj$business_id), 1),
          " % (kunnat, valtion virastot ym. eivät ole kaupparekisterissä)")

  # Pidetään vain Hilmassa esiintyvät, jotta tiedosto pysyy pienenä
  ytj <- ytj |> filter(business_id %in% c(buy_ids, if (exists("win_ids")) win_ids))
  qd_save(ytj, ytj_file)
  message("YTJ-rikastus valmis paikallisesta aineistosta: ", nrow(ytj), " yritystä")
}

# 3b. VARA: API-haku (hidas; vain jos paikallista aineistoa ei ole)
if (RUN$ytj && !file.exists(ytj_file)) {
  stopifnot(file.exists(index_file))
  idx <- qd_read(index_file)
  ids <- norm_ytunnus(idx$organisationNationalRegistrationNumber)
  if (file.exists(awards_file)) ids <- c(ids, norm_ytunnus(qd_read(awards_file)$winner_company_id))
  ids <- unique(ids[!is.na(ids)])
  ids <- ids[valid_ytunnus(ids)]
  message("YTJ: haetaan ", length(ids), " Y-tunnusta")

  ytj_dir <- file.path(out_dir, "ytj_chunks")
  dir.create(ytj_dir, showWarnings = FALSE)

  # PRH:n palvelin katkaisee välillä yhteyksiä kokonaan (ei HTTP-virhettä vaan
  # "Could not connect"). Siksi:
  #  - retry_on_failure = TRUE: yritetään uudelleen myös yhteysvirheissä
  #  - tryCatch: yksittäinen epäonnistuminen ei kaada ajoa, vaan rivi
  #    merkitään fetch_ok = FALSE ja haetaan uudelleen lopuksi
  #  - maltillisempi tahti (2 pyyntöä/s)
  fetch_ytj_one <- function(bid) {
    res <- tryCatch({
      resp <- request("https://avoindata.prh.fi/opendata-ytj-api/v3/companies") |>
        req_url_query(businessId = bid) |>
        req_throttle(rate = 2) |>
        req_retry(max_tries = 6, retry_on_failure = TRUE,
                  is_transient = \(r) resp_status(r) %in% c(429, 500, 502, 503, 504),
                  backoff = \(i) min(120, 5 * 2^i)) |>
        req_timeout(60) |>
        req_error(is_error = \(r) FALSE) |>
        req_perform()
      st <- resp_status(resp)
      if (st != 200) {
        tibble(business_id = bid, fetch_ok = st == 404)   # 404 = ei löydy, ei uudelleenhakua
      } else {
        js <- resp_body_json(resp, check_type = FALSE, simplifyVector = FALSE)
        comp <- js$companies %||% list()
        if (length(comp) == 0) tibble(business_id = bid, fetch_ok = TRUE)
        else parse_ytj_company(comp[[1]]) |> mutate(business_id = bid, fetch_ok = TRUE)
      }
    }, error = function(e) tibble(business_id = bid, fetch_ok = FALSE))
    res
  }

  for (k in seq(1, length(ids), by = 500)) {
    f <- file.path(ytj_dir, sprintf("ytj_%06d.qs2", k))
    if (file.exists(f)) next
    part <- map_dfr(ids[k:min(k + 499, length(ids))], fetch_ytj_one, .progress = TRUE)
    message(sprintf("YTJ-pala %d: %d ok, %d epäonnistui", k, sum(part$fetch_ok), sum(!part$fetch_ok)))
    if (mean(!part$fetch_ok) > 0.5) {
      stop("Yli puolet palan hauista epäonnistui - PRH ei vastaa. ",
           "Pala jätettiin tallentamatta; aja myöhemmin uudelleen, valmiit palat säilyvät.")
    }
    qd_save(part, f)
  }

  ytj_all <- list.files(ytj_dir, pattern = "^ytj_\\d+\\.qs2$", full.names = TRUE) |>
    map_dfr(qd_read)

  # Toinen kierros epäonnistuneille
  retry_ids <- ytj_all$business_id[!ytj_all$fetch_ok]
  if (length(retry_ids) > 0) {
    message("Haetaan uudelleen ", length(retry_ids), " epäonnistunutta Y-tunnusta")
    Sys.sleep(30)
    retried <- map_dfr(retry_ids, fetch_ytj_one, .progress = TRUE)
    ytj_all <- bind_rows(ytj_all |> filter(fetch_ok), retried)
  }

  ytj <- ytj_all |>
    arrange(desc(fetch_ok)) |>
    distinct(business_id, .keep_all = TRUE) |>
    mutate(reg_date = as.Date(reg_date), end_date = as.Date(end_date),
           ytj_postcode = str_pad(ytj_postcode, 5, pad = "0"))

  n_fail <- sum(!ytj$fetch_ok)
  message("YTJ: ", nrow(ytj), " Y-tunnusta, nimi löytyi ", sum(!is.na(ytj$ytj_name)),
          ", haku epäonnistui yhä ", n_fail)
  stopifnot(!anyDuplicated(ytj$business_id),
            n_fail < 0.02 * nrow(ytj),                  # alle 2 % puuttuu
            mean(!is.na(ytj$ytj_name)) > 0.8)          # valtaosa löytyi
  qd_save(ytj, ytj_file)
}

# =============================================================================
# 4. PAAVO: postinumero -> kunta + koordinaatit
# =============================================================================
paavo_file <- file.path(out_dir, "paavo_postinumerot.qs2")

if (RUN$paavo && !file.exists(paavo_file)) {
  pv <- read_fst(here("data", "paavo", "paavo.fst"))
  need <- c("Postinumeroalue", "Vuosi", "X-koordinaatti", "Y-koordinaatti",
            "Asukkaat yhteensä (HE)")
  stopifnot(all(need %in% names(pv)))

  paavo <- pv |>
    as_tibble() |>
    filter(Postinumeroalue != "KOKO MAA") |>
    transmute(
      postinumero = str_sub(Postinumeroalue, 1, 5),
      kunta       = str_match(Postinumeroalue, "\\(([^()]*)\\)\\s*$")[, 2],
      vuosi       = as.integer(Vuosi),
      x_tm35      = as.numeric(`X-koordinaatti`),
      y_tm35      = as.numeric(`Y-koordinaatti`),
      asukkaat    = as.numeric(`Asukkaat yhteensä (HE)`)
    ) |>
    group_by(postinumero) |>
    slice_max(vuosi, n = 1, with_ties = FALSE) |>
    ungroup()

  stopifnot(!anyDuplicated(paavo$postinumero),
            all(str_detect(paavo$postinumero, "^\\d{5}$")),
            mean(!is.na(paavo$kunta)) > 0.99,
            n_distinct(paavo$kunta) > 250)
  qd_save(paavo, paavo_file)
}

# =============================================================================
# 5. EUROSTAT: hintaindeksi (reaaliset eurot) + julkisyhteisöjen menot
# =============================================================================
hicp_file <- file.path(out_dir, "eurostat_hicp_fi.qs2")
gov_file  <- file.path(out_dir, "eurostat_gov_fi.qs2")

eurostat_time <- function(df) {
  if ("TIME_PERIOD" %in% names(df)) df <- rename(df, time = TIME_PERIOD)
  mutate(df, year = as.integer(format(as.Date(time), "%Y")))
}

if (RUN$eurostat && !file.exists(hicp_file)) {
  hicp <- eurostat::get_eurostat("prc_hicp_aind",
                                 filters = list(geo = "FI", coicop = "CP00",
                                                unit = "INX_A_AVG"),
                                 time_format = "date") |>
    eurostat_time() |>
    select(year, hicp = values)
  stopifnot(nrow(hicp) > 15, !anyDuplicated(hicp$year), all(hicp$hicp > 0))
  qd_save(hicp, hicp_file)
}

if (RUN$eurostat && !file.exists(gov_file)) {
  # OECD:n määritelmä: hankinnat = välituotekäyttö (P2) + kiinteän pääoman
  # bruttomuodostus (P51G) + markkinatuottajilta ostetut luontoissosiaalietuudet
  # (D632). Haetaan kaikki erät ja valitaan koodilla.
  gov <- eurostat::get_eurostat("gov_10a_main",
                                filters = list(geo = "FI", sector = "S13",
                                               unit = "MIO_EUR"),
                                time_format = "date") |>
    eurostat_time()
  items <- unique(gov$na_item)
  message("gov_10a_main na_item-koodit: ", paste(sort(items), collapse = ", "))
  keep <- c("P2", "P51G", grep("^D632", items, value = TRUE), "B1GQ")
  if (!any(grepl("^D632", items))) warning("D632 puuttuu - hankintojen summa on alaraja.")
  gov <- gov |> filter(na_item %in% keep) |> select(year, na_item, values)
  stopifnot(all(c("P2", "P51G") %in% gov$na_item))
  qd_save(gov, gov_file)
}

# =============================================================================
# 6. TUTKIHANKINTOJA.FI -OSTOLASKUT (valtio + osa kunnista ja hyvinvointialueista)
# =============================================================================
# Aineisto on suuri, joten se tallennetaan VUOSITTAIN omiin tiedostoihinsa.
tutki_dir <- file.path(out_dir, "tutkihankintoja")

if (RUN$tutki) {
  dir.create(file.path(tutki_dir, "raw"), recursive = TRUE, showWarnings = FALSE)

  pkg <- request("https://www.avoindata.fi/data/api/3/action/package_show") |>
    req_url_query(id = "tutkihankintoja-data") |>
    req_retry(max_tries = 4) |>
    req_perform() |>
    resp_body_json(simplifyVector = FALSE)
  stopifnot(isTRUE(pkg$success))

  res <- map_dfr(pkg$result$resources, \(r) tibble(
    name = r$name %||% NA_character_, url = r$url %||% NA_character_,
    format = str_to_lower(r$format %||% "")))
  print(res)

  # Vuosiaineistot (muoto vaihtelee: csv tai tsv, nimi "th_data_2016.csv" tai
  # "Tutkihankintoja data 2021") + sarakkeiden käännöstaulukko.
  res <- res |>
    mutate(year = as.integer(str_extract(name, "20\\d{2}")),
           kind = case_when(format %in% c("csv", "tsv") & !is.na(year) ~ "data",
                            str_detect(name, "kaannok") ~ "dict",
                            TRUE ~ NA_character_),
           file = if_else(kind == "data", sprintf("th_data_%d.txt", year),
                          "th_kaannokset.xlsx")) |>
    filter(!is.na(kind))
  stopifnot(sum(res$kind == "data") >= 10, !anyDuplicated(res$file))

  # LATAUS: tiedostot ovat ~270 Mt ja S3-palvelin on hidas. curl::multi_download
  #  - jatkaa keskeytynyttä latausta siitä, mihin se jäi (resume = TRUE)
  #  - ei aikakatkaise koko latausta, vain jumittuneen yhteyden
  # Valmis tiedosto tunnistetaan vertaamalla kokoa palvelimen ilmoittamaan
  # kokoon, joten katkennut (vajaa) tiedosto ei koskaan päädy jäsennykseen.
  raw_dir <- file.path(tutki_dir, "raw")
  dests <- file.path(raw_dir, res$file)
  outs  <- file.path(tutki_dir, str_replace(res$file, "\\.txt$", ".qs2"))
  need  <- res$kind == "data" & !file.exists(outs) | res$kind == "dict" & !file.exists(dests)

  if (any(need)) {
    message("Ladataan ", sum(need), " tiedostoa (voi kestää tunteja; ",
            "keskeytys ei haittaa, seuraava ajo jatkaa)")
    # Ylimääräiset argumentit menevät suoraan curl-asetuksiksi. Kokonaisaika-
    # rajaa ei aseteta; yhteys katkaistaan vain, jos se jumittaa 10 min.
    dl <- curl::multi_download(res$url[need], dests[need], resume = TRUE,
                               progress = TRUE, multiplex = FALSE,
                               low_speed_limit = 1, low_speed_time = 600)
    print(dl[, c("destfile", "success", "status_code")])
    # Koko tarkistetaan: Content-Length vs levyllä oleva tiedosto
    bad <- dl |> filter(!success | !status_code %in% c(200, 206, 416))
    if (nrow(bad) > 0) {
      stop(nrow(bad), " latausta keskeytyi. Aja vaihe uudelleen - lataus jatkuu ",
           "siitä, mihin jäi.")
    }
  }

  # Varmistus ennen jäsennystä: levyllä olevan tiedoston koko = palvelimen koko
  remote_size <- function(u) {
    h <- request(u) |> req_method("HEAD") |> req_timeout(60) |>
      req_retry(max_tries = 3) |> req_perform()
    as.numeric(resp_header(h, "content-length") %||% NA)
  }

  for (i in which(res$kind == "data")) {
    if (file.exists(outs[i])) next
    rs <- remote_size(res$url[i])
    ls <- file.size(dests[i])
    if (!is.na(rs) && ls != rs) {
      stop(res$file[i], " on vajaa (", ls, " / ", rs, " tavua). Aja vaihe uudelleen.")
    }
    df <- data.table::fread(dests[i], encoding = "UTF-8", showProgress = FALSE) |> as_tibble()
    message(res$file[i], ": ", nrow(df), " riviä; sarakkeet: ",
            paste(names(df), collapse = " | "))
    stopifnot(nrow(df) > 1000, ncol(df) > 3)            # sarake-erotin tunnistui
    qd_save(df, outs[i])
    file.remove(dests[i])                                # raakatiedosto pois (tilaa)
  }
}

message("Valmis. Tiedostot kansiossa: ", out_dir)
