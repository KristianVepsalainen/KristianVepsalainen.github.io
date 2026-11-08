# =============================================================================
# R/hilma/hilma-helpers.R
# Sarjan "Julkinen raha ei valehtele" yhteiset apufunktiot osille 2–10:
# muotoilu, teema, luokittelut ja analyysitaulujen rakentajat.
# Sourcataan postauksen setup-lohkossa:
#   source(here("R", "hilma", "hilma-io.R"))
#   source(here("R", "hilma", "hilma-helpers.R"))
# =============================================================================

hilma_dir <- here::here("data", "hilma")
hd <- function(name, dir = hilma_dir) read_split(dir, name)

# --- Muotoilu (suomalainen desimaalipilkku, välilyönti tuhaterottimena) -------
.f <- function(x, d) formatC(x, format = "f", digits = d, big.mark = " ", decimal.mark = ",")
eur <- function(x, digits = 1) {
  dplyr::case_when(
    is.na(x)        ~ "–",
    abs(x) >= 1e9   ~ paste0(.f(x / 1e9, digits), " mrd. €"),
    abs(x) >= 1e6   ~ paste0(.f(x / 1e6, digits), " milj. €"),
    abs(x) >= 1e4   ~ paste0(.f(x / 1e3, 0), " 000 €"),
    TRUE            ~ paste0(.f(x, 0), " €")
  )
}
pct  <- function(x, digits = 1) paste0(.f(100 * x, digits), " %")
num  <- function(x, digits = 0) .f(x, digits)
dec  <- function(x, digits = 2) .f(x, digits)
fi_p <- function(p) if (is.na(p)) "–" else if (p < 0.001) "< 0,001" else format(signif(p, 2), decimal.mark = ",")
ci   <- function(x, lo = 0.05, hi = 0.95, f = pct) paste0(f(stats::quantile(x, lo)), "–", f(stats::quantile(x, hi)))
lab_eur <- function(x) eur(x, 0)
lab_pct <- function(x) pct(x, 0)

stopifnot(eur(1234567) == "1,2 milj. €", eur(150000) == "150 000 €",
          eur(5400) == "5 400 €", pct(0.123) == "12,3 %")

# --- Teema ja värit -----------------------------------------------------------
ggplot2::theme_set(
  ggplot2::theme_minimal(base_size = 13) +
    ggplot2::theme(plot.title = ggplot2::element_text(face = "bold"),
                   plot.title.position = "plot",
                   panel.grid.minor = ggplot2::element_blank(),
                   legend.position = "bottom")
)
pal <- c(navy = "#1d3557", blue = "#457b9d", green = "#2a9d8f",
         orange = "#f4a261", red = "#e63946", grey = "grey60")
col_nat <- c("Tavarat" = "#457b9d", "Palvelut" = "#2a9d8f",
             "Rakennusurakat" = "#f4a261", "Muu / tuntematon" = "grey60")

# --- Luokittelut --------------------------------------------------------------
nature_fi <- function(x) {
  x <- stringr::str_to_lower(dplyr::coalesce(x, ""))
  dplyr::case_when(
    stringr::str_detect(x, "suppl|tavara")      ~ "Tavarat",
    stringr::str_detect(x, "serv|palvel")       ~ "Palvelut",
    stringr::str_detect(x, "work|urak|rakenn")  ~ "Rakennusurakat",
    TRUE                                        ~ "Muu / tuntematon"
  )
}

# eFormsin menettelykoodit (BT-105) suomeksi
proc_group <- function(code) {
  dplyr::case_when(
    code == "open"                      ~ "Avoin",
    code == "restricted"                ~ "Rajoitettu",
    code %in% c("neg-w-call", "comp-dial", "innovation") ~ "Neuvottelu tms.",
    code == "neg-wo-call"               ~ "Suorahankinta",
    code %in% c("oth-single", "oth-mult", "comp-tend") ~ "Muu menettely",
    TRUE                                ~ "Tuntematon"
  )
}

# eForms-alalaji: E-alkuiset ovat kansallisia, numerot EU-ilmoituksia
regime_fi <- function(subtype) dplyr::if_else(stringr::str_detect(dplyr::coalesce(subtype, ""), "^E"),
                                              "Kansallinen", "EU")

# Hankintayksikön oikeudellinen muoto (BT-11)
buyer_type_fi <- function(x) {
  dplyr::case_when(
    x %in% c("cga", "pub-undert-cga")             ~ "Valtio",
    x %in% c("la", "pub-undert-la", "org-sub-la") ~ "Kunnat ja kuntayhtymät",
    x %in% c("ra", "pub-undert-ra")               ~ "Hyvinvointialueet ja alueet",
    x %in% c("body-pl", "body-pl-cga", "body-pl-la", "body-pl-ra") ~ "Julkisoikeudelliset laitokset",
    x %in% c("pub-undert")                        ~ "Julkiset yritykset",
    is.na(x) | x == ""                            ~ "Tuntematon",
    TRUE                                          ~ "Muu"
  )
}

cpv_names <- c(
  "03" = "Maatalous- ja metsätuotteet", "09" = "Polttoaineet ja energia",
  "14" = "Kaivostuotteet", "15" = "Elintarvikkeet", "16" = "Maatalouskoneet",
  "18" = "Vaatteet ja jalkineet", "19" = "Nahka- ja tekstiilit", "22" = "Painotuotteet",
  "24" = "Kemikaalit", "30" = "Toimistokoneet ja tietokoneet", "31" = "Sähkölaitteet",
  "32" = "Tietoliikennelaitteet", "33" = "Lääkinnälliset laitteet ja lääkkeet",
  "34" = "Kulkuneuvot", "35" = "Turvallisuus- ja puolustusvarusteet",
  "37" = "Urheilu- ja vapaa-ajan välineet", "38" = "Laboratoriolaitteet",
  "39" = "Kalusteet ja kodintarvikkeet", "41" = "Vesi", "42" = "Teollisuuskoneet",
  "43" = "Kaivos- ja rakennuskoneet", "44" = "Rakennustarvikkeet",
  "45" = "Rakennustyöt", "48" = "Ohjelmistot", "50" = "Korjaus ja huolto",
  "51" = "Asennuspalvelut", "55" = "Majoitus- ja ravintolapalvelut",
  "60" = "Kuljetuspalvelut", "63" = "Liikenteen tukipalvelut",
  "64" = "Posti- ja televiestintä", "65" = "Energia- ja vesihuolto",
  "66" = "Rahoitus ja vakuutus", "70" = "Kiinteistöpalvelut",
  "71" = "Arkkitehti- ja insinööripalvelut", "72" = "IT-palvelut",
  "73" = "Tutkimus ja kehitys", "75" = "Julkishallinnon palvelut",
  "76" = "Öljy- ja kaasualan palvelut", "77" = "Maa-, metsä- ja puutarhapalvelut",
  "79" = "Liike-elämän palvelut", "80" = "Koulutuspalvelut",
  "85" = "Terveys- ja sosiaalipalvelut", "90" = "Jäte-, puhdistus- ja ympäristöpalvelut",
  "92" = "Virkistys-, kulttuuri- ja urheilupalvelut", "98" = "Muut palvelut"
)
cpv_name <- function(cpv2) dplyr::coalesce(unname(cpv_names[cpv2]), paste("CPV", cpv2))

# --- Y-tunnus -----------------------------------------------------------------
norm_ytunnus <- function(x) {
  x <- stringr::str_to_upper(stringr::str_remove_all(x, "\\s"))
  x <- stringr::str_remove(x, "^FI")
  x <- dplyr::if_else(stringr::str_detect(x, "^\\d{8}$"),
                      paste0(stringr::str_sub(x, 1, 7), "-", stringr::str_sub(x, 8, 8)), x)
  dplyr::if_else(stringr::str_detect(dplyr::coalesce(x, ""), "^\\d{7}-\\d$"), x, NA_character_)
}

# --- Rahan arvo ---------------------------------------------------------------
# Muuntaa eurot viimeisimmän täyden vuoden hintoihin Eurostatin HICP:llä
make_deflator <- function(hicp) {
  this_year <- as.integer(format(Sys.Date(), "%Y"))
  full <- tibble::tibble(year = min(hicp$year):this_year) |>
    dplyr::left_join(hicp, by = "year") |>
    tidyr::fill(hicp, .direction = "down")
  base <- full$hicp[full$year == max(hicp$year)]
  stopifnot(length(base) == 1, !any(is.na(full$hicp)))
  function(x, year) x * base / full$hicp[match(year, full$year)]
}

# --- Sopimuksen kesto kuukausina ---------------------------------------------
duration_months <- function(value, unit) {
  dplyr::case_when(
    unit == "DAY"   ~ value / 30.44,
    unit == "WEEK"  ~ value * 7 / 30.44,
    unit == "MONTH" ~ value,
    unit == "YEAR"  ~ value * 12,
    TRUE            ~ NA_real_
  )
}

# --- eForms-taulut ------------------------------------------------------------
ef_names <- c("notice", "lots", "results", "tenders", "parties", "contracts", "orgs")
ef_load <- function() purrr::set_names(purrr::map(ef_names, \(n) hd(paste0("hilma_eforms_", n))), ef_names)

# Hankintayksikön tiedot ilmoituksittain (ensimmäinen ilmoitettu ostaja)
buyer_info <- function(ef) {
  ef$notice |>
    dplyr::transmute(notice_id,
                     buyer_org_id = stringr::str_extract(buyer_org_id, "^[^|]+"),
                     buyer_legal_type) |>
    dplyr::left_join(ef$orgs |>
                       dplyr::distinct(notice_id, org_id, .keep_all = TRUE) |>
                       dplyr::select(notice_id, buyer_org_id = org_id,
                                     buyer_name = org_name, buyer_company_id = company_id,
                                     buyer_postal = postal_zone, buyer_city = city,
                                     buyer_nuts = nuts),
                     by = c("notice_id", "buyer_org_id")) |>
    dplyr::mutate(buyer_company_id = norm_ytunnus(buyer_company_id),
                  buyer_type = buyer_type_fi(buyer_legal_type))
}

#' Jälki-ilmoitusten osat: yksi rivi per kilpailutettu osa (lot result).
#' Sisältää tarjousmäärät, voittajan (jos yksi), arvot, menettelyn ja ostajan.
lot_panel <- function(ef, deflate = NULL) {
  stopifnot(all(ef_names %in% names(ef)))
  n_lots <- ef$lots |> dplyr::count(notice_id, name = "n_lots_notice")

  winners <- build_awards(ef) |>
    dplyr::add_count(notice_id, lot_id, name = "n_winners") |>
    dplyr::group_by(notice_id, lot_id) |>
    dplyr::slice(1) |>
    dplyr::ungroup() |>
    dplyr::select(notice_id, lot_id, n_winners, tender_value, winner_org_id,
                  winner_name, winner_company_id, winner_size, winner_postal,
                  winner_city, winner_country, consortium_size)

  out <- ef$results |>
    dplyr::filter(!is.na(lot_id)) |>
    dplyr::distinct(notice_id, lot_id, .keep_all = TRUE) |>
    dplyr::left_join(ef$lots, by = c("notice_id", "lot_id")) |>
    dplyr::left_join(ef$notice |>
                       dplyr::select(notice_id, root_element, notice_subtype, issue_date,
                                     folder_id, procedure_code, contract_nature, cpv_main),
                     by = "notice_id") |>
    dplyr::filter(root_element == "ContractAwardNotice") |>
    dplyr::left_join(winners, by = c("notice_id", "lot_id")) |>
    dplyr::left_join(n_lots, by = "notice_id") |>
    dplyr::left_join(buyer_info(ef), by = "notice_id") |>
    dplyr::mutate(
      year      = as.integer(format(issue_date, "%Y")),
      nature    = nature_fi(dplyr::coalesce(lot_nature, contract_nature)),
      cpv       = dplyr::coalesce(lot_cpv, cpv_main),
      cpv2      = stringr::str_sub(cpv, 1, 2),
      procedure = proc_group(procedure_code),
      regime    = regime_fi(notice_subtype),
      awarded   = result_code == "selec-w",
      n_winners = dplyr::coalesce(n_winners, 0L),
      single_value = dplyr::if_else(awarded & n_winners == 1 & tender_value >= 100 &
                                      tender_value <= 5e9, tender_value, NA_real_),
      est_value = dplyr::if_else(lot_est_value >= 100 & lot_est_value <= 5e9, lot_est_value, NA_real_),
      dur_months = duration_months(duration_value, duration_unit),
      winner_company_id = norm_ytunnus(winner_company_id),
      sme_winner = dplyr::case_when(
        winner_size %in% c("micro", "small", "medium", "sme") ~ TRUE,
        winner_size == "large" ~ FALSE,
        TRUE ~ NA)
    )
  if (!is.null(deflate)) {
    out <- out |> dplyr::mutate(single_value_real = deflate(single_value, year),
                                est_value_real    = deflate(est_value, year))
  }
  stopifnot(!anyDuplicated(out[c("notice_id", "lot_id")]))
  out
}

#' Hankintailmoitusten osat: tarjousaika (julkaisu -> määräaika) ja menettelyn
#' tunniste, jolla ne voi yhdistää myöhempään jälki-ilmoitukseen.
cn_lot_panel <- function(ef) {
  ef$lots |>
    dplyr::left_join(ef$notice |>
                       dplyr::select(notice_id, root_element, notice_subtype, issue_date,
                                     folder_id, procedure_code, contract_nature, cpv_main),
                     by = "notice_id") |>
    dplyr::filter(root_element %in% c("ContractNotice", "PriorInformationNotice")) |>
    dplyr::mutate(days = as.numeric(deadline_date - issue_date),
                  nature = nature_fi(dplyr::coalesce(lot_nature, contract_nature)),
                  cpv2 = stringr::str_sub(dplyr::coalesce(lot_cpv, cpv_main), 1, 2),
                  procedure = proc_group(procedure_code),
                  regime = regime_fi(notice_subtype))
}

# --- Tilastoapurit -----------------------------------------------------------
# Cliffin delta Mannin–Whitneyn U:sta (as.numeric estää kokonaislukuylivuodon)
cliff_delta <- function(a, b) {
  u <- suppressWarnings(stats::wilcox.test(a, b, exact = FALSE)$statistic)
  2 * as.numeric(u) / (as.numeric(length(a)) * as.numeric(length(b))) - 1
}
# Cramérin V khiin neliö -testistä
cramer_v <- function(tab) {
  ch <- suppressWarnings(stats::chisq.test(tab))
  sqrt(as.numeric(ch$statistic) / (sum(tab) * (min(dim(tab)) - 1)))
}
# Bayesilainen bootstrap: painotetun tunnusluvun posteriori
bb <- function(x, fun = stats::weighted.mean, n = 4000, ...) {
  x <- x[!is.na(x)]; stopifnot(length(x) >= 10)
  vapply(seq_len(n), function(i) { w <- stats::rexp(length(x)); fun(x, w / sum(w), ...) }, numeric(1))
}
# Painotettu mediaani bayesilaista bootstrapia varten
wmedian <- function(x, w) { o <- order(x); x[o][which(cumsum(w[o]) >= 0.5)[1]] }

# Poistaa mallikaavasta kiinteät selittäjät, joilla on aineistossa vain yksi
# arvo (esim. takautuvan testin lyhyemmässä koulutusjaksossa). Muuten R ei voi
# muodostaa kontrasteja: "contrasts can be applied only to factors with 2 or
# more levels". Satunnaisvaikutukset ( | ) ja offset säilyvät ennallaan.
drop_constant_terms <- function(formula, data) {
  labs <- attr(stats::terms(formula), "term.labels")
  fixed <- labs[!grepl("\\|", labs) & labs %in% names(data)]
  const <- fixed[vapply(fixed, \(v) dplyr::n_distinct(data[[v]], na.rm = TRUE) < 2, logical(1))]
  if (length(const) == 0) return(formula)
  message("Poistetaan vakioselittäjät: ", paste(const, collapse = ", "))
  stats::update(formula, stats::as.formula(paste(". ~ . -", paste(const, collapse = " - "))))
}

# Pitää testiaineistosta vain rivit, joiden kiinteiden luokkamuuttujien arvot
# esiintyivät koulutusaineistossa (malli ei osaa ennustaa tuntemattomalle tasolle).
keep_seen_levels <- function(test, train, formula) {
  labs <- attr(stats::terms(formula), "term.labels")
  fixed <- labs[!grepl("\\|", labs) & labs %in% names(train)]
  for (v in fixed) {
    if (is.character(train[[v]]) || is.factor(train[[v]]) || is.logical(train[[v]])) {
      test <- test[as.character(test[[v]]) %in% unique(as.character(train[[v]])), , drop = FALSE]
    }
  }
  test
}
