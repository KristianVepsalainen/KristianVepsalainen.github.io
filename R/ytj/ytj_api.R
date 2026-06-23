# R/ytj_api.R
# Jaetut hakufunktiot PRH:n kolmeen avoimen datan rajapintaan.
# Lähde: https://avoindata.prh.fi  (CC BY 4.0 - mainitse lähde)
#
# PERIAATE (freeze-yhteensopiva):
#   - Näitä funktioita kutsutaan VAIN paikallisesti, ei koskaan CI:ssä.
#   - Jokainen postaus tallentaa tuloksen .rds-tiedostoksi (data/),
#     joka committoidaan repoon. GitHub Actions renderöi cachesta.
#   - Hakuchunkit: cache: false, ja kääritään if (!file.exists(path)) -ehtoon.
#
# Rajapinnan rajoitukset:
#   - 300 kyselyä / minuutti (kaikki käyttäjät yhteensä) -> throttlataan.
#   - 429 Too many requests -> uudelleenyritys backoffilla.

suppressPackageStartupMessages({
  library(httr2)
  library(jsonlite)
  library(purrr)
  library(dplyr)
  library(tibble)
})

# Yhteinen request-runko: throttle + retry --------------------------------------
.kvar_req <- function(url) {
  request(url) |>
    req_user_agent("kristianvepsalainen.com YTJ-sarja (R httr2)") |>
    req_throttle(rate = 250 / 60) |>          # turvamarginaali 300/min rajaan
    req_retry(
      max_tries = 5,
      is_transient = \(resp) resp_status(resp) %in% c(429, 500, 503),
      backoff = \(n) min(2^n, 30)
    ) |>
    req_timeout(120)   # 60 s oli liian tiukka hitaille sivuille
}

# Suorittaa pyynnön ja uudelleenyrittää MYÖS aikakatkaisut ja verkkovirheet.
# req_retry tutkii vain HTTP-statuksen; transport-virheessä vastausta ei tule,
# joten ne on napattava tällä erikseen (tämä oli OYJ-haun kaatumisen syy).
.perform <- function(req, max_tries = 6) {
  for (attempt in seq_len(max_tries)) {
    resp <- tryCatch(req_perform(req), error = function(e) e)
    if (!inherits(resp, "error")) return(resp)
    if (attempt == max_tries) stop(resp)
    wait <- min(2^attempt, 60)
    message(sprintf("    pyyntö epäonnistui (yritys %d/%d): %s — odotetaan %d s",
                    attempt, max_tries, conditionMessage(resp), wait))
    Sys.sleep(wait)
  }
}

# --- 1) YTJ-perustiedot: /companies -------------------------------------------
# Hakee yritykset annetuista sijainneista (location = postitoimipaikka),
# sivuttaen kunnes kaikki sivut on käyty läpi.
fetch_ytj_companies <- function(locations,
                                base = "https://avoindata.prh.fi/opendata-ytj-api/v3/companies") {
  stopifnot(is.character(locations), length(locations) >= 1)
  
  one_location <- function(loc) {
    page <- 1L
    acc  <- list()
    repeat {
      resp <- .kvar_req(base) |>
        req_url_query(location = loc, page = page) |>
        .perform()
      
      parsed <- resp_body_json(resp, simplifyVector = FALSE)
      comp   <- pluck(parsed, "companies", .default = list())
      if (length(comp) == 0) break
      
      acc[[length(acc) + 1]] <- comp
      total <- pluck(parsed, "totalResults", .default = length(comp))
      # 100 osumaa per sivu on rajapinnan oletus
      if (page * 100 >= total) break
      page <- page + 1L
    }
    unlist(acc, recursive = FALSE)
  }
  
  raw <- unlist(map(locations, one_location), recursive = FALSE)
  message(sprintf("Haettu %d yritystä %d sijainnista.", length(raw), length(locations)))
  raw
}

# Hakee KOKO kaupparekisterin iteroimalla yhtiömuotojen (companyForm) yli.
# Jokaisella yrityksellä on yksi voimassa oleva yhtiömuoto -> kattaa kaikki
# ilman riippuvuutta location-kentästä. ~600 000 yritystä, ~puoli tuntia
# throttlattuna. company_forms on Swaggerista vahvistettu enum.
fetch_ytj_all <- function(base = "https://avoindata.prh.fi/opendata-ytj-api/v3/companies",
                          checkpoint_dir = NULL) {
  company_forms <- c("AOY","ASH","ASY","AY","AYH","ETS","ETY","SCE","SCP","HY",
                     "KOY","KVJ","KVY","KY","OK","OP","OY","OYJ","SE","SL","SP",
                     "S\u00C4\u00C4","TYH","VOJ","VOY","VY","VALTLL")
  
  one_form <- function(cf) {
    cp      <- if (!is.null(checkpoint_dir)) file.path(checkpoint_dir, paste0("ytj_form_", cf, ".rds")) else NULL
    partial <- if (!is.null(checkpoint_dir)) file.path(checkpoint_dir, paste0("ytj_form_", cf, "_partial.rds")) else NULL
    
    # 1) Valmis muoto: ladataan suoraan levyltä.
    if (!is.null(cp) && file.exists(cp)) return(readRDS(cp))
    
    # 2) Keskeneräinen muoto: jatketaan siitä, mihin sivutus jäi.
    if (!is.null(partial) && file.exists(partial)) {
      state <- readRDS(partial); acc <- state$acc; page <- state$page
      message(sprintf("  %s: jatketaan sivulta %d", cf, page))
    } else {
      acc <- list(); page <- 1L
    }
    
    repeat {
      resp <- .kvar_req(base) |>
        req_url_query(companyForm = cf, page = page) |>
        .perform()
      parsed <- resp_body_json(resp, simplifyVector = FALSE)
      comp   <- pluck(parsed, "companies", .default = list())
      if (length(comp) == 0) break
      acc[[length(acc) + 1]] <- comp
      total <- pluck(parsed, "totalResults", .default = length(comp))
      message(sprintf("  %s: sivu %d / ~%d", cf, page, ceiling(total / 100)))
      # Sivutason osittaischeckpoint jättiformeille (esim. OY ~5800 sivua)
      if (!is.null(partial) && page %% 250 == 0) {
        saveRDS(list(acc = acc, page = page + 1L), partial)
      }
      if (page * 100 >= total) break
      page <- page + 1L
    }
    out <- unlist(acc, recursive = FALSE)
    if (!is.null(cp)) saveRDS(out, cp)                       # muoto valmis
    if (!is.null(partial) && file.exists(partial)) file.remove(partial)  # siivoa osittainen
    out
  }
  
  raw <- unlist(map(company_forms, one_form), recursive = FALSE)
  message(sprintf("Haettu yhteensä %d yritystä koko rekisteristä.", length(raw)))
  raw
}

# --- Skalaariturvalliset poimijat ---------------------------------------------
# Palauttaa AINA yhden atomisen merkkijonon (tai NA) -> ei koskaan list-saraketta,
# jolloin bind_rows/map_dfr ei voi kaatua tyyppiristiriitaan.
.as_chr <- function(x) {
  if (is.null(x) || length(x) == 0) return(NA_character_)
  x <- x[[1]]
  if (is.null(x) || length(x) == 0) return(NA_character_)
  as.character(x)[1]
}

# Voimassa oleva (endDate == NULL) alkio listamuotoisesta kentästä -> kentän arvo
.current <- function(items, field = "name") {
  if (is.null(items) || length(items) == 0) return(NA_character_)
  cur <- keep(items, \(x) is.null(x[["endDate"]]))
  use <- if (length(cur) > 0) cur[[1]] else items[[1]]
  .as_chr(pluck(use, field, .default = NA_character_))
}

# Kuvaus suomeksi descriptions-listasta. PRH koodaa kielen joko tekstillä
# ("fi"/"sv"/"en") TAI numerolla ("1"=suomi, "2"=ruotsi, "3"=englanti) — siksi
# tunnistetaan suomi molemmilla konventioilla. Ilman tätä poiminta putosi
# ensimmäiseen kuvaukseen, joka oli osalle yhtiömuodoista ruotsi/englanti.
.FI_LANG <- c("fi", "fin", "finnish", "suomi", "1")
.descr <- function(node) {
  d <- pluck(node, "descriptions", .default = list())
  if (length(d) == 0) return(NA_character_)
  langval <- function(x) tolower(as.character(
    pluck(x, "languageCode",
          .default = pluck(x, "language", .default = pluck(x, "lang", .default = NA)))))
  hit <- keep(d, \(x) langval(x) %in% .FI_LANG)
  if (length(hit) > 0) .as_chr(pluck(hit[[1]], "description"))
  else .as_chr(pluck(d, 1, "description"))   # viimekätinen vara (yksikielinen kenttä)
}

# Y-tunnus: toimii sekä merkkijonona että objektina {value, registrationDate}
.bid <- function(co) {
  b <- pluck(co, "businessId")
  if (is.character(b)) return(b[1])
  .as_chr(pluck(b, "value"))
}
.bid_date <- function(co) {
  b <- pluck(co, "businessId")
  if (is.list(b)) .as_chr(pluck(b, "registrationDate")) else NA_character_
}

# Kotipaikka/postitoimipaikka: postOffices on TAULUKKO [{languageCode, city}, ...]
.city <- function(addresses, lang = "fi") {
  if (is.null(addresses) || length(addresses) == 0) return(NA_character_)
  cur  <- keep(addresses, \(a) is.null(a[["endDate"]]))
  pool <- if (length(cur) > 0) cur else addresses
  for (a in pool) {
    pos <- pluck(a, "postOffices", .default = list())
    if (length(pos) == 0) next
    hit <- keep(pos, \(p) identical(pluck(p, "languageCode"), lang))
    v <- if (length(hit) > 0) .as_chr(pluck(hit[[1]], "city")) else .as_chr(pluck(pos[[1]], "city"))
    if (!is.na(v)) return(v)
  }
  NA_character_
}

tidy_ytj_companies <- function(raw) {
  # Rakennetaan sarakkeittain: map_chr takaa merkkijonovektorin (ei list-saraketta)
  # ja välttää 817 000 yhden rivin tibblen bind_rows-yhdistämisen (hidas + muistisyöppö).
  tibble(
    business_id           = map_chr(raw, .bid),
    registration_date     = map_chr(raw, .bid_date),
    name                  = map_chr(raw, \(co) .current(pluck(co, "names"), "name")),
    company_form          = map_chr(raw, \(co) .descr(pluck(co, "companyForms", 1))),
    company_form_code     = map_chr(raw, \(co) .as_chr(pluck(co, "companyForms", 1, "type"))),
    main_line_code        = map_chr(raw, \(co) .as_chr(pluck(co, "mainBusinessLine", "type"))),
    main_line             = map_chr(raw, \(co) .descr(pluck(co, "mainBusinessLine"))),
    location              = map_chr(raw, \(co) .city(pluck(co, "addresses"))),
    registered_office     = map_chr(raw, \(co) .descr(pluck(co, "registeredOffice", 1))),
    status                = map_chr(raw, \(co) .as_chr(pluck(co, "status"))),
    trade_register_status = map_chr(raw, \(co) .as_chr(pluck(co, "tradeRegisterStatus"))),
    last_modified         = map_chr(raw, \(co) .as_chr(pluck(co, "lastModified"))),
    # registeredEntries -> merkintälajit listasarakkeena osaa 2/4 varten
    entry_codes           = map(raw, \(co) map_chr(pluck(co, "registeredEntries", .default = list()),
                                                   \(e) .as_chr(pluck(e, "type"))))
  )
}

# --- Muistitehokas käsittely ---------------------------------------------------
# Kevyt välimuistikääre: qs2 jos saatavilla (nopea + pieni), muuten saveRDS.
.qsave <- function(x, path) {
  if (requireNamespace("qs2", quietly = TRUE)) qs2::qs_save(x, path)
  else saveRDS(x, sub("\\.qs$", ".rds", path))
}
.qread <- function(path) {
  if (requireNamespace("qs2", quietly = TRUE) && file.exists(path)) qs2::qs_read(path)
  else readRDS(sub("\\.qs$", ".rds", path))
}

# PELASTUS ilman uudelleenhakua: litistää olemassa olevat yhtiömuoto-checkpointit
# YKSI KERRALLAAN pienimmästä suurimpaan ja vapauttaa raskaan listan heti.
# Näin koko ~24 GB raakalistaa ei pidetä muistissa yhtä aikaa; vain yksi
# yhtiömuoto kerrallaan (OY viimeisenä, jolloin muu muisti on jo vapaa).
# AJA ENSIN konsolissa:  rm(raw); gc()   -- vapauttaa fetch_ytj_all():n listan.
tidy_ytj_from_checkpoints <- function(checkpoint_dir) {
  files <- list.files(checkpoint_dir, pattern = "^ytj_form_.*\\.rds$", full.names = TRUE)
  files <- files[!grepl("_partial\\.rds$", files)]
  stopifnot(length(files) > 0)
  files <- files[order(file.size(files))]          # pienin ensin, OY viimeisenä
  
  palat <- vector("list", length(files))
  for (i in seq_along(files)) {
    raw_form    <- readRDS(files[i])
    palat[[i]]  <- tidy_ytj_companies(raw_form)
    rm(raw_form); gc(verbose = FALSE)              # vapauta heti ennen seuraavaa
    message(sprintf("[%2d/%2d] %-26s -> %7d riviä",
                    i, length(files), basename(files[i]), nrow(palat[[i]])))
  }
  dplyr::bind_rows(palat)
}

# SUOSITELTU ARKKITEHTUURI: hakee JA litistää sivu kerrallaan. Sisäkkäistä
# raakalistaa ei koskaan kerätä muistiin -> huippumuisti vain yksi sivu (100
# yritystä) + kasvava kompakti tibble. Checkpoint per yhtiömuoto kompaktina
# .qs-tiedostona (nopea ladata). Aja tämä fetch_ytj_all():n sijaan.
fetch_ytj_all_tidy <- function(base = "https://avoindata.prh.fi/opendata-ytj-api/v3/companies",
                               checkpoint_dir = NULL) {
  company_forms <- c("AOY","ASH","ASY","AY","AYH","ETS","ETY","SCE","SCP","HY",
                     "KOY","KVJ","KVY","KY","OK","OP","OY","OYJ","SE","SL","SP",
                     "S\u00C4\u00C4","TYH","VOJ","VOY","VY","VALTLL")
  
  one_form <- function(cf) {
    cp <- if (!is.null(checkpoint_dir)) file.path(checkpoint_dir, paste0("tidy_form_", cf, ".qs")) else NULL
    if (!is.null(cp) && file.exists(cp)) return(.qread(cp))
    
    page <- 1L; acc <- list()
    repeat {
      resp <- .kvar_req(base) |>
        req_url_query(companyForm = cf, page = page) |>
        .perform()
      parsed <- resp_body_json(resp, simplifyVector = FALSE)
      comp   <- pluck(parsed, "companies", .default = list())
      if (length(comp) == 0) break
      acc[[length(acc) + 1]] <- tidy_ytj_companies(comp)   # LITISTÄ HETI -> kompakti
      total <- pluck(parsed, "totalResults", .default = length(comp))
      message(sprintf("  %s: sivu %d / ~%d", cf, page, ceiling(total / 100)))
      if (page * 100 >= total) break
      page <- page + 1L
    }
    out <- dplyr::bind_rows(acc)
    if (!is.null(cp)) .qsave(out, cp)
    out
  }
  
  dplyr::bind_rows(map(company_forms, one_form))
}

# --- 2) Rekisteröidyt ilmoitukset: krek ----------------------------------------
# Rekisterimerkinnät 7.11.2014 alkaen. Haku aikavälillä.
# HUOM: tarkista parametrinimet elävästä Swaggerista:
#   https://avoindata.prh.fi/en/krek/swagger-ui
fetch_krek_notifications <- function(start_date, end_date,
                                     base = "https://avoindata.prh.fi/opendata-krek-api/v3/registered-notifications") {
  stopifnot(!is.na(as.Date(start_date)), !is.na(as.Date(end_date)))
  page <- 1L
  acc  <- list()
  repeat {
    resp <- .kvar_req(base) |>
      req_url_query(
        registrationDateStart = as.character(start_date),
        registrationDateEnd   = as.character(end_date),
        page = page
      ) |>
      .perform()
    
    parsed <- resp_body_json(resp, simplifyVector = FALSE)
    items  <- pluck(parsed, "notifications", .default = list())
    if (length(items) == 0) break
    acc[[length(acc) + 1]] <- items
    total <- pluck(parsed, "totalResults", .default = length(items))
    if (page * 100 >= total) break
    page <- page + 1L
  }
  unlist(acc, recursive = FALSE)
}

tidy_krek <- function(raw) {
  map_dfr(raw, function(n) {
    codes <- map_chr(pluck(n, "registerEntries", .default = list()),
                     \(e) pluck(e, "type", .default = NA_character_))
    tibble(
      business_id       = pluck(n, "businessId", .default = NA_character_),
      registration_date = pluck(n, "registrationDate", .default = NA_character_),
      registered_office = pluck(n, "registeredOffice", .default = NA_character_),
      record_number     = pluck(n, "recordNumber", .default = NA_character_),
      type_of_registration = pluck(n, "typeOfRegistration", .default = NA_character_),
      n_entries         = length(codes),
      entry_codes       = list(codes)
    )
  })
}

# --- 3) Digitaaliset tilinpäätökset --------------------------------------------
# /financials ottaa pelkän Y-tunnuksen (vrt. /financial, joka vaatii myös
# tilikauden päättymispäivän). VARMISTA paikallisesti, palauttaako /financials
# itse tunnusluvut vai vain saatavilla olevat tilikaudet — jälkimmäisessä
# tapauksessa tarvitaan kaksivaihehaku /financial-päätepisteellä per tilikausi.
fetch_xbrl_financial <- function(business_id,
                                 base = "https://avoindata.prh.fi/opendata-xbrl-api/v3/financials") {
  req <- .kvar_req(base) |>
    req_url_query(businessId = business_id) |>
    req_error(is_error = \(resp) FALSE)    # ei heittoa HTTP-statuksesta; käsitellään itse
  resp <- .perform(req)                    # uudelleenyrittää vain timeout/verkkovirheet
  if (resp_status(resp) >= 400) return(NULL)  # 404 = ei iXBRL-aineistoa kyseiselle yritykselle
  resp_body_json(resp, simplifyVector = FALSE)
}

# Poimii iXBRL-konseptin arvon raakavastauksesta annetuilla nimivaihtoehdoilla.
# Taksonomia vaihtelee tilikausittain -> annetaan useita synonyymejä.
.xbrl_value <- function(facts, concepts) {
  hit <- keep(facts, \(f) pluck(f, "name", .default = "") %in% concepts)
  if (length(hit) == 0) return(NA_real_)
  suppressWarnings(as.numeric(pluck(hit[[1]], "value", .default = NA)))
}

# Litistää yhden yrityksen tilinpäätökset (yksi rivi per tilikausi).
tidy_xbrl <- function(raw, business_id) {
  if (is.null(raw)) return(tibble())
  statements <- pluck(raw, "financialStatements", .default = list())
  map_dfr(statements, function(s) {
    facts <- pluck(s, "facts", .default = list())
    tibble(
      business_id   = business_id,
      period_end    = pluck(s, "financialPeriod", "endDate", .default = NA_character_),
      period_start  = pluck(s, "financialPeriod", "startDate", .default = NA_character_),
      revenue       = .xbrl_value(facts, c("LiikevaihtoTuotot", "Liikevaihto", "Revenue")),
      profit        = .xbrl_value(facts, c("TilikaudenVoittoTappio", "ProfitLoss")),
      equity        = .xbrl_value(facts, c("OmaPaaomaYhteensa", "Equity")),
      balance_total = .xbrl_value(facts, c("VastaavaaYhteensa", "Assets", "BalanceSheetTotal"))
    )
  })
}

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || is.na(a[1])) b else a