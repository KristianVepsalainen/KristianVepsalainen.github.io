# =============================================================================
# R/hilma/hilma-repair.R
# Korjaa keskeytyneen pilkkomisen jäljet ILMAN uusia rajapintahakuja.
#
# Kaikki Hilma-taulut syntyvät pienistä välimuistitiedostoista, jotka ovat
# yhä tallessa:
#   hilma_index        <- data/hilma/index_months/   (kuukausipalat)
#   hilma_eforms_*     <- data/hilma/eforms_parsed/  (500 ilmoituksen palat)
#   hilma_awards       <- hilma_eforms_*-tauluista
# Skripti testaa jokaisen taulun, rakentaa rikkinäiset uudelleen välimuisteista
# ja tarkistaa lopuksi, että kaikki luetaan ja ovat alle 95 Mt.
# =============================================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(here)
  library(qs2)
})
select <- dplyr::select
filter <- dplyr::filter

source(here("R", "hilma", "hilma-eforms-parse.R"))
source(here("R", "hilma", "hilma-io.R"))
d <- here("data", "hilma")

readable <- function(name) {
  if (!exists_split(d, name)) return(FALSE)
  tryCatch({ read_split(d, name); TRUE }, error = function(e) FALSE)
}

# Keskeytyneiden ajojen väliaikaiskansiot pois
unlink(list.files(d, pattern = "^\\.tmp_", full.names = TRUE, all.files = TRUE),
       recursive = TRUE)

# --- 1. Hakuindeksi kuukausipaloista ----------------------------------------
if (!readable("hilma_index")) {
  message("hilma_index rikki -> rakennetaan kuukausipaloista")
  months <- list.files(file.path(d, "index_months"), full.names = TRUE)
  stopifnot(length(months) > 100)
  idx_raw <- map(months, qd_read) |> bind_rows()

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
  non_plan <- idx |> filter(!isPlan)
  stopifnot(nrow(idx) > 150000, !anyDuplicated(idx$id),
            !anyDuplicated(non_plan[c("source", "noticeId")]))
  save_split(idx, d, "hilma_index")
}

# --- 2. eForms-taulut jäsennetyistä paloista --------------------------------
ef_names <- c("notice", "lots", "results", "tenders", "parties", "contracts", "orgs")
if (!all(map_lgl(paste0("hilma_eforms_", ef_names), readable))) {
  message("eForms-taulut rikki -> rakennetaan jäsennetyistä paloista")
  chunks <- list.files(file.path(d, "eforms_parsed"), full.names = TRUE) |> map(qs_read)
  stopifnot(length(chunks) > 50)
  ef <- set_names(map(ef_names, \(n) bind_rows(map(chunks, n)) |> distinct()), ef_names)
  ef$tenders <- ef$tenders |> mutate(tender_value = if_else(tender_value < 0, NA_real_, tender_value))
  ef$results <- ef$results |> mutate(n_tenders = if_else(n_tenders < 0, NA_real_, n_tenders))
  stopifnot(nrow(ef$notice) > 40000, !anyDuplicated(ef$notice$notice_id))
  for (nm in ef_names) save_split(ef[[nm]], d, paste0("hilma_eforms_", nm))
}

# --- 3. Voittajataulu --------------------------------------------------------
if (!readable("hilma_awards")) {
  message("hilma_awards rikki -> rakennetaan eForms-tauluista")
  ef <- set_names(map(ef_names, \(n) read_split(d, paste0("hilma_eforms_", n))), ef_names)
  awards <- build_awards(ef)
  stopifnot(nrow(awards) > 50000, all(awards$result_code == "selec-w"))
  save_split(awards, d, "hilma_awards")
}

# --- 3b. Ostolaskut: rikkinäiset vuodet pois (ladataan uudelleen myöhemmin) ---
td <- file.path(d, "tutkihankintoja")
if (dir.exists(td)) {
  unlink(list.files(td, pattern = "^\\.tmp_", full.names = TRUE, all.files = TRUE), recursive = TRUE)
  for (nm in unique(sub("(__part\\d{2})?\\.qs2$", "", list.files(td, pattern = "^th_data_\\d{4}.*\\.qs2$")))) {
    ok <- tryCatch({ read_split(td, nm); TRUE }, error = function(e) FALSE)
    if (!ok) {
      file.remove(.split_files(td, nm))
      message("Ostolaskut ", nm, " rikki -> poistettu. Lataa uudelleen ajamalla ",
              "hilma-data-prep.R:n vaihe 6 (ei tarvita osaan 1).")
    } else if (exists_split(td, nm) && any(file.size(.split_files(td, nm)) / 1e6 > SPLIT_LIMIT_MB)) {
      save_split(read_split(td, nm), td, nm)       # ehjä mutta liian iso -> pilkotaan
    }
  }
}

# --- 4. Vanhat tarpeettomat tiedostot pois -----------------------------------
file.remove(file.path(d, "hilma_eforms.qs2")[file.exists(file.path(d, "hilma_eforms.qs2"))])
unlink(file.path(d, "git"), recursive = TRUE)

# --- 5. Lopputarkistus -------------------------------------------------------
tables <- c("hilma_index", "hilma_awards", paste0("hilma_eforms_", ef_names),
            "eurostat_hicp_fi", "eurostat_gov_fi")
status <- tibble(table = tables, ok = map_lgl(tables, readable))
print(status)
if (!all(status$ok)) stop("Lukukelvottomia tauluja: ",
                          paste(status$table[!status$ok], collapse = ", "))

# Tiedostokoot tarkistaa push-safe.sh gitin näkökulmasta (.gitignore huomioiden)
message("KAIKKI KUNNOSSA: Hilma-taulut luettavissa.")
