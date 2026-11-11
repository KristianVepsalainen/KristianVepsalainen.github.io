# =============================================================================
# R/hilma/hilma-tutki-aggregate.R
# Tiivistää Tutkihankintoja.fi-ostolaskut (miljoonia tiliöintirivejä vuodessa)
# pieniksi analyysitauluiksi, joita osat 5 ja 6 käyttävät:
#
#   tutki_agg_supplier : vuosi x sektori x yläorganisaatio x hankintayksikkö x
#                        hankintakategoria x toimittaja -> euromäärä, rivit, laskut
#   tutki_agg_daily    : päivä x sektori x yläorganisaatio -> euromäärä
#
# Ajetaan automaattisesti postauksesta, jos taulut puuttuvat. Lukee yhden
# vuoden kerrallaan, jotta muisti riittää.
#
# Sarakkeet (Tutkihankintoja.fi:n datakuvaus): lasku_id, hankintayksikko,
# hankintayksikko_tunnus, ylaorganisaatio, ylaorganisaatio_tunnus,
# toimittaja_y_tunnus, toimittaja_nimi, tili, hankintakategoria,
# tuote_palveluryhma, tositepvm, tiliointisumma, sektori.
# Osa vuosista voi olla englanninkielisin sarakenimin -> nimetään suomeksi.
# =============================================================================

tutki_dir <- here::here("data", "hilma", "tutkihankintoja")

.th_rename <- c(
  invoice_id = "lasku_id", procurement_unit = "hankintayksikko",
  procurement_unit_id = "hankintayksikko_tunnus", parent_organization = "ylaorganisaatio",
  parent_organization_id = "ylaorganisaatio_tunnus", supplier_business_id = "toimittaja_y_tunnus",
  supplier_name = "toimittaja_nimi", account = "tili", procurement_category = "hankintakategoria",
  product_service_group = "tuote_palveluryhma", invoice_entry_date = "tositepvm",
  posting_sum = "tiliointisumma", sector = "sektori"
)
.th_need <- c("lasku_id", "hankintayksikko", "ylaorganisaatio", "toimittaja_y_tunnus",
              "toimittaja_nimi", "hankintakategoria", "tositepvm", "tiliointisumma", "sektori")

th_standardize <- function(df) {
  nm <- tolower(trimws(names(df)))
  nm <- ifelse(nm %in% names(.th_rename), .th_rename[nm], nm)
  names(df) <- nm
  miss <- setdiff(.th_need, names(df))
  if (length(miss) > 0) {
    stop("Ostolaskutiedostosta puuttuvat sarakkeet: ", paste(miss, collapse = ", "),
         "\nTiedoston sarakkeet: ", paste(names(df), collapse = ", "),
         "\nLisää vastaavuus .th_rename-listaan tiedostossa hilma-tutki-aggregate.R.")
  }
  df |>
    dplyr::transmute(
      lasku_id, sektori = as.character(sektori),
      ylaorganisaatio = as.character(ylaorganisaatio),
      hankintayksikko = as.character(hankintayksikko),
      hankintakategoria = as.character(hankintakategoria),
      supplier = norm_ytunnus(as.character(toimittaja_y_tunnus)),
      supplier_name = as.character(toimittaja_nimi),
      date = as.Date(lubridate::parse_date_time(as.character(tositepvm),
                                                orders = c("Ymd", "dmY", "Ymd HMS"), quiet = TRUE)),
      eur = suppressWarnings(as.numeric(gsub(",", ".", as.character(tiliointisumma))))
    )
}

tutki_aggregate <- function(force = FALSE) {
  if (!force && exists_split(hilma_dir, "tutki_agg_supplier") &&
      exists_split(hilma_dir, "tutki_agg_daily")) return(invisible(TRUE))

  years <- list.files(tutki_dir, pattern = "^th_data_\\d{4}(__part\\d{2})?\\.qs2$") |>
    sub(pattern = "(__part\\d{2})?\\.qs2$", replacement = "") |> unique() |> sort()
  if (length(years) == 0) {
    stop("Kansiossa ", tutki_dir, " ei ole jäsennettyjä ostolaskutiedostoja (th_data_VVVV.qs2).\n",
         "Aja ensin: HILMA_RUN=tutki Rscript R/hilma/hilma-data-prep.R", call. = FALSE)
  }

  sup <- list(); day <- list()
  for (y in years) {
    message("Tiivistetään ", y)
    df <- th_standardize(read_split(tutki_dir, y))
    n0 <- nrow(df)
    bad_date <- mean(is.na(df$date)); bad_eur <- mean(is.na(df$eur))
    if (bad_date > 0.01 || bad_eur > 0.01) {
      warning(y, ": päivämäärä puuttuu ", round(100 * bad_date, 1), " %, summa ",
              round(100 * bad_eur, 1), " % riveistä")
    }
    df <- dplyr::filter(df, !is.na(date), !is.na(eur))
    df$year <- as.integer(format(df$date, "%Y"))

    sup[[y]] <- df |>
      dplyr::group_by(year, sektori, ylaorganisaatio, hankintayksikko, hankintakategoria,
                      supplier, supplier_name) |>
      dplyr::summarise(eur = sum(eur), rows = dplyr::n(),
                       invoices = dplyr::n_distinct(lasku_id), .groups = "drop")
    day[[y]] <- df |>
      dplyr::group_by(date, sektori, ylaorganisaatio) |>
      dplyr::summarise(eur = sum(eur), invoices = dplyr::n_distinct(lasku_id), .groups = "drop")
    stopifnot(abs(sum(sup[[y]]$eur) - sum(df$eur)) < 1)     # summat täsmäävät
    message("  ", n0, " riviä -> ", nrow(sup[[y]]), " toimittajariviä")
    rm(df); gc()
  }
  # Sama vuosi voi esiintyä kahdessa tiedostossa (tositepäivä vuodenvaihteessa)
  sup_all <- dplyr::bind_rows(sup) |>
    dplyr::group_by(year, sektori, ylaorganisaatio, hankintayksikko, hankintakategoria,
                    supplier, supplier_name) |>
    dplyr::summarise(eur = sum(eur), rows = sum(rows), invoices = sum(invoices), .groups = "drop")
  day_all <- dplyr::bind_rows(day) |>
    dplyr::group_by(date, sektori, ylaorganisaatio) |>
    dplyr::summarise(eur = sum(eur), invoices = sum(invoices), .groups = "drop")
  save_split(sup_all, hilma_dir, "tutki_agg_supplier")
  save_split(day_all, hilma_dir, "tutki_agg_daily")
  invisible(TRUE)
}
