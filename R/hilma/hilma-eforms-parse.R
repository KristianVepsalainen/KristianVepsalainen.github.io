# =============================================================================
# R/hilma-eforms-parse.R
# eForms-XML:n (UBL) jäsennys tidy-tauluiksi.
#
# Sarja: "Julkinen raha ei valehtele" (Hilma-hankintailmoitukset)
# Sourcataan tiedostosta R/hilma-data-prep.R. Testattu EU:n virallisilla
# eForms SDK:n esimerkki-ilmoituksilla (OP-TED/eForms-SDK, examples/notices).
#
# Kenttäviitteet (eForms Business Terms, BT):
#   BT-161  NoticeResult/TotalAmount                 ilmoituksen kokonaisarvo
#   BT-27   ProcurementProject/RequestedTenderTotal/  arvioitu arvo
#           EstimatedOverallContractAmount
#   BT-105  TenderingProcess/ProcedureCode           menettely
#   BT-23   ProcurementProject/ProcurementTypeCode   hankintalaji (contract-nature)
#   BT-262  MainCommodityClassification/ItemClassificationCode (CPV)
#   BT-142  LotResult/TenderResultCode               valittiinko voittaja
#   BT-759  ReceivedSubmissionsStatistics (tenders)  saatujen tarjousten määrä
#   BT-720  LotTender/LegalMonetaryTotal/PayableAmount  tarjouksen arvo
#   BT-1451 SettledContract/AwardDate                hankintapäätöksen pvm
#   BT-145  SettledContract/IssueDate                sopimuksen pvm
#   BT-165  Company/CompanySizeCode                  yrityksen koko
#   BT-501  PartyLegalEntity/CompanyID               Y-tunnus
#   BT-131  TenderSubmissionDeadlinePeriod/EndDate   tarjousten määräaika
#   BT-36   PlannedPeriod/DurationMeasure            sopimuksen kesto
# =============================================================================

suppressPackageStartupMessages({
  library(xml2)
  library(dplyr)
  library(purrr)
  library(tibble)
  library(stringr)
})

# --- Apurit ------------------------------------------------------------------

# Muuntaa "efac:NoticeResult/cbc:TotalAmount" -> local-name()-muotoon, jotta
# nimiavaruuksien prefiksit (jotka voivat vaihdella) eivät riko hakua.
.lx <- function(path) {
  lead <- str_extract(path, "^(\\.)?/{0,2}")
  body <- str_remove(path, "^(\\.)?/{0,2}")
  steps <- str_split(body, "/")[[1]]
  steps <- str_remove(steps, "^[A-Za-z]+:")
  steps <- ifelse(steps == "*" | steps == "..", steps,
                  sprintf("*[local-name()='%s']", steps))
  paste0(lead, paste(steps, collapse = "/"))
}

# Ensimmäisen osuman teksti tai NA
.t1 <- function(node, path) {
  if (length(node) == 0 || inherits(node, "xml_missing")) return(NA_character_)
  hit <- xml_find_first(node, .lx(path))
  if (inherits(hit, "xml_missing")) NA_character_ else str_trim(xml_text(hit))
}

# Kaikki osumat tekstinä
.tall <- function(node, path) {
  hits <- xml_find_all(node, .lx(path))
  if (length(hits) == 0) character(0) else str_trim(xml_text(hits))
}

# Ensimmäinen osuma, jonka listName-attribuutti täsmää
.t_list <- function(node, path, list_name) {
  hits <- xml_find_all(node, .lx(path))
  if (length(hits) == 0) return(NA_character_)
  ln <- xml_attr(hits, "listName")
  keep <- which(ln == list_name)
  if (length(keep) == 0) NA_character_ else str_trim(xml_text(hits[[keep[1]]]))
}

.num  <- function(x) suppressWarnings(as.numeric(x))
# eForms-päivämäärät ovat muotoa "2023-03-23+02:00" -> otetaan 10 merkkiä
.date <- function(x) suppressWarnings(as.Date(str_sub(x, 1, 10)))

# --- Pääfunktio --------------------------------------------------------------

#' Jäsennä yksi eForms-ilmoitus
#' @param xml_txt eForms XML merkkijonona
#' @param notice_id Hilman noticeId (avain kaikkiin tauluihin)
#' @return nimetty lista tibblejä: notice, lots, results, tenders,
#'         parties, orgs, contracts
parse_eform <- function(xml_txt, notice_id) {
  # Base64-dekoodattu teksti on UTF-8:aa, mutta R ei tiedä sitä ilman
  # merkintää. Ilman tätä jäsennys kaatuu ei-UTF-8-lokaalissa (esim. CI).
  if (is.raw(xml_txt)) xml_txt <- rawToChar(xml_txt)
  Encoding(xml_txt) <- "UTF-8"
  doc  <- read_xml(xml_txt, encoding = "UTF-8")
  root <- xml_root(doc)

  # ---------- Ilmoitustaso ----------
  proc_project <- xml_find_first(root, .lx("./cac:ProcurementProject"))
  proc_tp      <- xml_find_first(root, .lx("./cac:TenderingProcess"))

  notice <- tibble(
    notice_id         = notice_id,
    root_element      = xml_name(root),
    notice_subtype    = .t1(root, ".//efac:NoticeSubType/cbc:SubTypeCode"),
    notice_type_code  = .t1(root, "./cbc:NoticeTypeCode"),
    issue_date        = .date(.t1(root, "./cbc:IssueDate")),
    regulatory_domain = .t1(root, "./cbc:RegulatoryDomain"),
    folder_id         = .t1(root, "./cbc:ContractFolderID"),
    procedure_code    = .t1(proc_tp, "./cbc:ProcedureCode"),
    contract_nature   = .t_list(proc_project, "./cbc:ProcurementTypeCode",
                                "contract-nature"),
    cpv_main          = .t1(proc_project,
                            "./cac:MainCommodityClassification/cbc:ItemClassificationCode"),
    est_value_total   = .num(.t1(proc_project,
                                 "./cac:RequestedTenderTotal/cbc:EstimatedOverallContractAmount")),
    notice_value      = .num(.t1(root, ".//efac:NoticeResult/cbc:TotalAmount")),
    fa_value_max      = .num(.t1(root,
                                 ".//efac:NoticeResult/efbc:OverallMaximumFrameworkContractsAmount")),
    buyer_org_id      = paste(.tall(root,
                                    "./cac:ContractingParty/cac:Party/cac:PartyIdentification/cbc:ID"),
                              collapse = "|"),
    buyer_legal_type  = .t1(root,
                            "./cac:ContractingParty/cac:ContractingPartyType/cbc:PartyTypeCode")
  )

  # ---------- Osat (lots) ----------
  lot_nodes <- xml_find_all(root, .lx("./cac:ProcurementProjectLot"))
  lots <- map_dfr(lot_nodes, function(ln) {
    pp <- xml_find_first(ln, .lx("./cac:ProcurementProject"))
    tp <- xml_find_first(ln, .lx("./cac:TenderingProcess"))
    tt <- xml_find_first(ln, .lx("./cac:TenderingTerms"))
    dur <- xml_find_first(pp, .lx("./cac:PlannedPeriod/cbc:DurationMeasure"))
    tibble(
      notice_id      = notice_id,
      lot_id         = .t1(ln, "./cbc:ID"),
      lot_nature     = .t_list(pp, "./cbc:ProcurementTypeCode", "contract-nature"),
      lot_cpv        = .t1(pp, "./cac:MainCommodityClassification/cbc:ItemClassificationCode"),
      lot_nuts       = .t1(pp, "./cac:RealizedLocation/cac:Address/cbc:CountrySubentityCode"),
      lot_est_value  = .num(.t1(pp, "./cac:RequestedTenderTotal/cbc:EstimatedOverallContractAmount")),
      duration_value = if (inherits(dur, "xml_missing")) NA_real_ else .num(xml_text(dur)),
      duration_unit  = if (inherits(dur, "xml_missing")) NA_character_ else xml_attr(dur, "unitCode"),
      start_date     = .date(.t1(pp, "./cac:PlannedPeriod/cbc:StartDate")),
      end_date       = .date(.t1(pp, "./cac:PlannedPeriod/cbc:EndDate")),
      deadline_date  = .date(.t1(tp, "./cac:TenderSubmissionDeadlinePeriod/cbc:EndDate")),
      fa_type        = .t_list(tp, "./cac:ContractingSystem/cbc:ContractingSystemTypeCode",
                               "framework-agreement"),
      sme_suitable   = .t1(pp, "./cbc:SMESuitableIndicator"),
      award_criteria_n = length(xml_find_all(tt, .lx(".//cac:SubordinateAwardingCriterion")))
    )
  })

  nr <- xml_find_first(root, .lx(".//efac:NoticeResult"))
  has_result <- !inherits(nr, "xml_missing")

  # ---------- Tulokset osittain ----------
  results <- if (!has_result) tibble() else {
    map_dfr(xml_find_all(nr, .lx("./efac:LotResult")), function(lr) {
      stats <- xml_find_all(lr, .lx("./efac:ReceivedSubmissionsStatistics"))
      st_code <- map_chr(stats, \(s) .t1(s, "./efbc:StatisticsCode"))
      st_num  <- map_dbl(stats, \(s) .num(.t1(s, "./efbc:StatisticsNumeric")))
      g <- function(code) if (code %in% st_code) st_num[match(code, st_code)] else NA_real_
      tibble(
        notice_id    = notice_id,
        result_id    = .t1(lr, "./cbc:ID"),
        lot_id       = .t1(lr, "./efac:TenderLot/cbc:ID"),
        result_code  = .t1(lr, "./cbc:TenderResultCode"),
        n_tenders    = g("tenders"),
        n_t_sme      = g("t-sme"),
        n_t_micro    = g("t-micro"),
        n_t_small    = g("t-small"),
        n_t_medium   = g("t-med"),
        n_t_large    = g("t-large"),
        n_t_oth_eea  = g("t-oth-eea"),
        n_t_no_eea   = g("t-no-eea"),
        n_t_esubm    = g("t-esubm"),
        n_requests   = g("part-req"),
        winner_tender_ids = paste(.tall(lr, "./efac:LotTender/cbc:ID"), collapse = "|")
      )
    })
  }

  # ---------- Tarjoukset ----------
  tenders <- if (!has_result) tibble() else {
    map_dfr(xml_find_all(nr, .lx("./efac:LotTender")), function(lt) {
      tibble(
        notice_id    = notice_id,
        tender_id    = .t1(lt, "./cbc:ID"),
        lot_id       = .t1(lt, "./efac:TenderLot/cbc:ID"),
        tender_value = .num(.t1(lt, "./cac:LegalMonetaryTotal/cbc:PayableAmount")),
        tender_rank  = .num(.t1(lt, "./cbc:RankCode")),
        tpa_id       = .t1(lt, "./efac:TenderingParty/cbc:ID"),
        subcontracting = .t1(lt, "./efac:SubcontractingTerm/efbc:TermCode")
      )
    })
  }

  # ---------- Tarjoajaryhmät -> organisaatiot ----------
  parties <- if (!has_result) tibble() else {
    map_dfr(xml_find_all(nr, .lx("./efac:TenderingParty")), function(tpa) {
      tnodes <- xml_find_all(tpa, .lx("./efac:Tenderer"))
      if (length(tnodes) == 0) return(tibble())
      tibble(
        notice_id = notice_id,
        tpa_id    = .t1(tpa, "./cbc:ID"),
        org_id    = map_chr(tnodes, \(t) .t1(t, "./cbc:ID")),
        lead      = map_chr(tnodes, \(t) .t1(t, "./efbc:GroupLeadIndicator")),
        n_members = length(tnodes)
      )
    })
  }

  # ---------- Sopimukset ----------
  contracts <- if (!has_result) tibble() else {
    map_dfr(xml_find_all(nr, .lx("./efac:SettledContract")), function(sc) {
      tibble(
        notice_id     = notice_id,
        contract_id   = .t1(sc, "./cbc:ID"),
        tender_ids    = paste(.tall(sc, "./efac:LotTender/cbc:ID"), collapse = "|"),
        award_date    = .date(.t1(sc, "./cbc:AwardDate")),
        conclusion_date = .date(.t1(sc, "./cbc:IssueDate")),
        framework     = .t1(sc, "./efbc:ContractFrameworkIndicator")
      )
    })
  }

  # ---------- Organisaatiot ----------
  orgs <- map_dfr(xml_find_all(root, .lx(".//efac:Organizations/efac:Organization")),
                  function(o) {
    co <- xml_find_first(o, .lx("./efac:Company"))
    tibble(
      notice_id    = notice_id,
      org_id       = .t1(co, "./cac:PartyIdentification/cbc:ID"),
      org_name     = .t1(co, "./cac:PartyName/cbc:Name"),
      company_id   = .t1(co, "./cac:PartyLegalEntity/cbc:CompanyID"),
      company_size = .t1(co, "./efbc:CompanySizeCode"),
      postal_zone  = .t1(co, "./cac:PostalAddress/cbc:PostalZone"),
      city         = .t1(co, "./cac:PostalAddress/cbc:CityName"),
      nuts         = .t1(co, "./cac:PostalAddress/cbc:CountrySubentityCode"),
      country      = .t1(co, "./cac:PostalAddress/cac:Country/cbc:IdentificationCode"),
      natural_person = .t1(o, "./efbc:NaturalPersonIndicator")
    )
  })

  list(notice = notice, lots = lots, results = results, tenders = tenders,
       parties = parties, contracts = contracts, orgs = orgs)
}

#' Turvallinen kääre: virhe ei kaada koko ajoa, vaan kirjautuu
parse_eform_safe <- function(xml_txt, notice_id) {
  tryCatch(parse_eform(xml_txt, notice_id),
           error = function(e) {
             message("Jäsennysvirhe noticeId ", notice_id, ": ", conditionMessage(e))
             NULL
           })
}

#' Yhdistä jäsennetyt listat yhdeksi tauluiksi-listaksi
bind_eforms <- function(parsed_list) {
  parsed_list <- compact(parsed_list)
  nms <- c("notice", "lots", "results", "tenders", "parties", "contracts", "orgs")
  set_names(map(nms, \(n) bind_rows(map(parsed_list, n))), nms)
}

#' Rakenna analyysivalmis voittajataulu: yksi rivi per (ilmoitus, osa, voittaja)
#' Voittaja = LotResultin viittaama tarjous, jonka tarjoajaryhmän johtaja
#' (tai ainoa jäsen) on voittajaorganisaatio.
build_awards <- function(ef) {
  stopifnot(all(c("notice", "lots", "results", "tenders", "parties", "orgs") %in% names(ef)))
  if (nrow(ef$results) == 0) return(tibble())

  # Vain osat, joissa voittaja valittiin (BT-142 = selec-w). Muissa
  # (clos-nw, open-nw) LotResultin tarjousviitteet eivät ole voittajia.
  win <- ef$results |>
    filter(result_code == "selec-w",
           !is.na(winner_tender_ids), winner_tender_ids != "") |>
    mutate(tender_id = str_split(winner_tender_ids, "\\|")) |>
    tidyr::unnest(tender_id)

  lead_party <- ef$parties |>
    group_by(notice_id, tpa_id) |>
    arrange(desc(lead == "true"), .by_group = TRUE) |>
    slice(1) |>
    ungroup() |>
    select(notice_id, tpa_id, winner_org_id = org_id, consortium_size = n_members)

  win |>
    left_join(ef$tenders |> select(notice_id, tender_id, tender_value, tpa_id),
              by = c("notice_id", "tender_id")) |>
    left_join(lead_party, by = c("notice_id", "tpa_id")) |>
    left_join(ef$orgs |>
                distinct(notice_id, org_id, .keep_all = TRUE) |>
                select(notice_id, winner_org_id = org_id,
                       winner_name = org_name, winner_company_id = company_id,
                       winner_size = company_size, winner_postal = postal_zone,
                       winner_city = city, winner_country = country),
              by = c("notice_id", "winner_org_id")) |>
    left_join(ef$lots |> select(notice_id, lot_id, lot_nature, lot_cpv,
                                lot_est_value, duration_value, duration_unit,
                                start_date, end_date, fa_type),
              by = c("notice_id", "lot_id")) |>
    left_join(ef$notice |> select(notice_id, issue_date, notice_subtype,
                                  procedure_code, contract_nature, cpv_main,
                                  est_value_total, buyer_org_id, buyer_legal_type),
              by = "notice_id")
}
