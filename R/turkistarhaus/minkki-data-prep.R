# =============================================================================
# minkki-data-prep.R
# Hakee kaiken minkkipostauksen tarvitseman avoimen datan kerralla ja tallentaa
# sen qs2-muodossa kansioon data/minkki/.
#
# Lähteet:
#   1. PRH/YTJ avoin data (v3): turkistarhausyritykset (TOL 2008 01491 ja
#      TOL 2025 01481), mukana myös lopettaneet yritykset (endDate).
#   2. UN Comtrade public preview API: minkinnahkojen (HS 430110) vienti ja
#      tuonti maittain 2000–2025. Ei vaadi avainta, 1 vuosi / pyyntö.
#   3. Danmarks Statistik (api.statbank.dk): kokoaikaiseksi muunnetut
#      työttömät (fuldtidsledige) kunnittain, taulu AUL01.
#   4. Paavo-postinumeroaluedata luetaan paikallisesta tiedostosta
#      data/paavo/paavo.fst (ei haeta verkosta).
#
# Aja kerran. Jokainen haku tallennetaan omaan välimuistiinsa, joten keskeytys
# ei pakota aloittamaan alusta. Poista välimuistitiedosto, jos haluat hakea
# kyseisen lähteen uudelleen.
# =============================================================================

library(tidyverse)
library(httr2)
library(here)
library(qs2)
library(fst)

select <- dplyr::select
filter <- dplyr::filter

out_dir <- here("data", "minkki")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
stopifnot(dir.exists(out_dir))

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

# -----------------------------------------------------------------------------
# 1. YTJ: turkistarhausyritykset
# -----------------------------------------------------------------------------
# Huom: PRH siirtyi TOL 2025 -luokitukseen. Vanha koodi 01491 (Turkistarhaus)
# osuu lähinnä jäädytettyihin, usein lopettaneisiin yrityksiin; uusi koodi on
# 01481 (Turkiseläinten kasvatus). Haetaan molemmat ja yhdistetään.

ytj_cache <- file.path(out_dir, "ytj_raw.qs2")

parse_ytj_company <- function(x) {
  addrs <- x$addresses %||% list()
  pc <- NA_character_
  street <- NA_character_
  if (length(addrs) > 0) {
    # Suositaan käyntiosoitetta (type 1), muuten ensimmäinen jolla postinumero
    types <- map_chr(addrs, \(a) as.character(a$type %||% NA))
    ord <- order(types != "1")
    for (i in ord) {
      p <- addrs[[i]]$postCode %||% NA_character_
      if (!is.na(p) && nzchar(p)) {
        pc <- p
        street <- addrs[[i]]$street %||% NA_character_
        break
      }
    }
  }
  nm <- x$names %||% list()
  tibble(
    business_id  = x$businessId$value %||% NA_character_,
    name         = if (length(nm) > 0) nm[[1]]$name %||% NA_character_ else NA_character_,
    tol          = x$mainBusinessLine$type %||% NA_character_,
    company_form = pluck(x, "companyForms", 1, "type", .default = NA_character_),
    reg_date     = x$registrationDate %||% NA_character_,
    end_date     = x$endDate %||% NA_character_,
    status       = as.character(x$status %||% NA_character_),
    post_code    = pc,
    street       = street
  )
}

fetch_ytj_tol <- function(code, max_pages = 60) {
  base <- "https://avoindata.prh.fi/opendata-ytj-api/v3/companies"
  out <- list()
  page <- 1
  total <- NA_integer_
  repeat {
    resp <- request(base) |>
      req_url_query(mainBusinessLine = code, page = page) |>
      req_retry(max_tries = 4, backoff = \(i) 2^i) |>
      req_perform()
    js <- resp_body_json(resp, simplifyVector = FALSE)
    if (is.na(total)) total <- js$totalResults %||% NA_integer_
    comps <- js$companies %||% list()
    if (length(comps) == 0) break
    out[[page]] <- map_dfr(comps, parse_ytj_company)
    message(sprintf("YTJ %s: sivu %d, rivejä yhteensä %d / %s",
                    code, page, sum(map_int(out, nrow)), total))
    if (!is.na(total) && sum(map_int(out, nrow)) >= total) break
    page <- page + 1
    if (page > max_pages) {
      warning("max_pages saavutettu koodille ", code)
      break
    }
    Sys.sleep(0.5)
  }
  bind_rows(out) |> mutate(query_code = code)
}

if (!file.exists(ytj_cache)) {
  ytj_raw <- bind_rows(
    fetch_ytj_tol("01491"),
    fetch_ytj_tol("01481")
  )
  qs_save(ytj_raw, ytj_cache)
} else {
  ytj_raw <- qs_read(ytj_cache)
}

# Tarkistukset
stopifnot(
  nrow(ytj_raw) > 0,
  all(c("business_id", "reg_date", "end_date", "post_code") %in% names(ytj_raw)),
  mean(!is.na(ytj_raw$business_id)) == 1
)

ytj <- ytj_raw |>
  distinct(business_id, .keep_all = TRUE) |>
  mutate(
    reg_date  = as.Date(reg_date),
    end_date  = as.Date(end_date),
    active    = is.na(end_date),
    post_code = str_pad(post_code, 5, pad = "0")
  )

stopifnot(!anyDuplicated(ytj$business_id))
n_bad_dates <- sum(!is.na(ytj$end_date) & !is.na(ytj$reg_date) &
                     ytj$end_date < ytj$reg_date)
if (n_bad_dates > 0) {
  warning(n_bad_dates, " yrityksellä lopettamispäivä on ennen rekisteröitymistä; ",
          "nämä jätetään pois kanta-analyysistä.")
  ytj <- ytj |> filter(is.na(end_date) | is.na(reg_date) | end_date >= reg_date)
}
message("YTJ: ", nrow(ytj), " uniikkia yritystä, joista toiminnassa ",
        sum(ytj$active))

# -----------------------------------------------------------------------------
# 2. UN Comtrade: HS 430110 (minkin raakaturkisnahat, kokonaiset)
# -----------------------------------------------------------------------------
# Raportoijat: Suomi 246, Tanska 208, Puola 616, Alankomaat 528, Kreikka 300,
# Ruotsi 752, Kiina 156, Hongkong 344.
# Preview-rajapinta hyväksyy vain yhden vuoden per pyyntö.

ct_cache <- file.path(out_dir, "comtrade_raw.qs2")

reporters <- c(FIN = 246, DNK = 208, POL = 616, NLD = 528,
               GRC = 300, SWE = 752, CHN = 156, HKG = 344)
years <- 2000:2025

fetch_ct <- function(reporter, year) {
  url <- "https://comtradeapi.un.org/public/v1/preview/C/A/HS"
  resp <- tryCatch(
    request(url) |>
      req_url_query(reporterCode = reporter, period = year,
                    cmdCode = "430110", flowCode = "M,X",
                    partnerCode = 0) |>
      req_retry(max_tries = 5, backoff = \(i) 3 * i,
                is_transient = \(r) resp_status(r) %in% c(429, 500, 502, 503)) |>
      req_error(is_error = \(r) FALSE) |>
      req_perform(),
    error = function(e) NULL
  )
  if (is.null(resp) || resp_status(resp) != 200) {
    return(tibble(reporterCode = reporter, period = year, fetch_ok = FALSE))
  }
  js <- resp_body_json(resp, simplifyVector = TRUE)
  el <- js$data %||% js$elements
  if (is.null(el) || length(el) == 0 || NROW(el) == 0) {
    return(tibble(reporterCode = reporter, period = year, fetch_ok = TRUE))
  }
  as_tibble(el) |>
    select(any_of(c("reporterCode", "period", "flowCode", "partnerCode",
                    "cmdCode", "qtyUnitCode", "qty", "netWgt",
                    "primaryValue", "isQtyEstimated"))) |>
    mutate(fetch_ok = TRUE)
}

ct_list <- if (file.exists(ct_cache)) qs_read(ct_cache) else list()
grid <- expand_grid(rep = unname(reporters), yr = years) |>
  mutate(key = paste(rep, yr, sep = "_"))

for (k in setdiff(grid$key, names(ct_list))) {
  g <- grid[grid$key == k, ]
  ct_list[[k]] <- fetch_ct(g$rep, g$yr)
  qs_save(ct_list, ct_cache)  # tallennetaan joka kierroksella
  Sys.sleep(1.2)
}

# Yhtenäistetään tyypit: paikkamerkkirivit (integer) vs. rajapinnan rivit (character)
ct_all <- ct_list |>
  map(\(d) mutate(d, across(any_of(c("period", "reporterCode", "partnerCode",
                                     "qtyUnitCode", "flowCode", "cmdCode")),
                            as.character))) |>
  bind_rows()

comtrade <- ct_all |>
  filter(!is.na(flowCode)) |>
  mutate(period       = as.integer(period),
         reporterCode = as.integer(reporterCode),
         partnerCode  = as.integer(partnerCode),
         qtyUnitCode  = as.integer(qtyUnitCode),
         iso3 = names(reporters)[match(reporterCode, reporters)])

stopifnot(
  nrow(comtrade) > 0,
  all(comtrade$cmdCode == "430110"),
  all(comtrade$flowCode %in% c("M", "X")),
  all(comtrade$primaryValue >= 0, na.rm = TRUE),
  !anyNA(comtrade$iso3)
)
# Yksi rivi per maa-vuosi-suunta
stopifnot(!anyDuplicated(comtrade[, c("reporterCode", "period", "flowCode")]))

n_failed <- ct_all |> filter(!fetch_ok) |> nrow()
if (n_failed > 0) warning(n_failed, " Comtrade-hakua epäonnistui; aja silmukka uudelleen ",
                          "poistamalla epäonnistuneet avaimet ct_listista.")

# -----------------------------------------------------------------------------
# 3. Danmarks Statistik: fuldtidsledige kunnittain (AUL01)
# -----------------------------------------------------------------------------

dst_cache <- file.path(out_dir, "dst_aul01_raw.qs2")

if (!file.exists(dst_cache)) {
  info <- request("https://api.statbank.dk/v1/tableinfo/AUL01") |>
    req_url_query(lang = "da", format = "JSON") |>
    req_perform() |>
    resp_body_json(simplifyVector = FALSE, check_type = FALSE)

  vars <- info$variables
  var_ids <- map_chr(vars, "id")
  message("AUL01 muuttujat: ", paste(var_ids, collapse = ", "))
  stopifnot(any(str_detect(var_ids, "OMR")), "Tid" %in% var_ids)

  pick_total <- function(v) {
    vals <- v$values
    txt <- map_chr(vals, "text")
    idx <- which(str_detect(txt, regex("^(i alt|alle|total|bruttoledige)",
                                       ignore_case = TRUE)))
    if (length(idx) == 0) idx <- 1
    message(sprintf("  %s -> '%s' (%s)", v$id, txt[idx[1]], vals[[idx[1]]]$id))
    vals[[idx[1]]]$id
  }

  body_vars <- map(vars, function(v) {
    if (str_detect(v$id, "OMR") || v$id == "Tid") {
      list(code = v$id, values = list("*"))
    } else {
      list(code = v$id, values = list(pick_total(v)))
    }
  })

  omr_var <- vars[[which(str_detect(var_ids, "OMR"))]]
  omr_lookup <- tibble(
    omr_id   = map_chr(omr_var$values, "id"),
    omr_text = map_chr(omr_var$values, "text")
  )

  resp <- request("https://api.statbank.dk/v1/data") |>
    req_body_json(list(table = "AUL01", format = "CSV", lang = "da",
                       valuePresentation = "Code",
                       variables = body_vars)) |>
    req_perform()

  dst_raw <- read_delim(I(resp_body_string(resp)), delim = ";",
                        locale = locale(decimal_mark = ","),
                        na = c("..", ""), show_col_types = FALSE,
                        col_types = cols(.default = col_character()))
  qs_save(list(data = dst_raw, omr_lookup = omr_lookup), dst_cache)
}

dst <- qs_read(dst_cache)
omr_col <- names(dst$data)[str_detect(names(dst$data), "OMR")]
stopifnot(length(omr_col) == 1, "TID" %in% toupper(names(dst$data)))

dk_unemp <- dst$data |>
  rename(omr_id = all_of(omr_col)) |>
  rename_with(toupper, .cols = -omr_id) |>
  mutate(year = as.integer(str_sub(TID, 1, 4)),
         unemployed = parse_number(INDHOLD,
                                   locale = locale(decimal_mark = ","))) |>
  left_join(dst$omr_lookup, by = "omr_id") |>
  mutate(omr_num = suppressWarnings(as.integer(omr_id))) |>
  # Kunnat: koodi >= 101 (000 = koko maa, 081–085 = alueet)
  filter(!is.na(omr_num), omr_num >= 101) |>
  group_by(omr_id, omr_text, year) |>
  summarise(unemployed = sum(unemployed, na.rm = TRUE), .groups = "drop")

stopifnot(
  n_distinct(dk_unemp$omr_id) >= 95,          # Tanskassa 98 kuntaa
  !anyDuplicated(dk_unemp[, c("omr_id", "year")]),
  all(dk_unemp$unemployed >= 0)
)

# -----------------------------------------------------------------------------
# 4. Paavo (paikallinen)
# -----------------------------------------------------------------------------
paavo_path <- here("data", "paavo", "paavo.fst")
stopifnot(file.exists(paavo_path))
paavo <- read_fst(paavo_path) |> as_tibble()
message("Paavo: ", nrow(paavo), " riviä, sarakkeita ", ncol(paavo))

# -----------------------------------------------------------------------------
# Tallennus: yksi tiivis paketti koko postaukselle
# -----------------------------------------------------------------------------
minkki_data <- list(
  ytj      = ytj,
  comtrade = comtrade,
  dk_unemp = dk_unemp,
  meta = list(
    fetched   = Sys.time(),
    sources   = c(ytj = "avoindata.prh.fi/opendata-ytj-api/v3 (CC BY 4.0)",
                  comtrade = "comtradeapi.un.org public preview, HS 430110",
                  dst = "api.statbank.dk AUL01"),
    reporters = reporters
  )
)
qs_save(minkki_data, file.path(out_dir, "minkki_data.qs2"))
message("Valmis: ", file.path(out_dir, "minkki_data.qs2"))
