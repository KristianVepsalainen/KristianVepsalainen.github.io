library(tidyverse); library(here); library(xml2); library(qs2)
select <- dplyr::select; filter <- dplyr::filter

DATA <- here("data", "eduskunta")
lue  <- function(p) tryCatch(qs2::qd_read(p), error = function(e) qs2::qs_read(p))
vaski_raw  <- lue(file.path(DATA, "vaski_raw.qs"))
vaski_docs <- lue(file.path(DATA, "vaski_docs.qs"))

norm_tunnus <- function(x) str_remove_all(str_to_upper(x), "\\s") |> str_remove("VP$")
ln <- function(x) paste0(".//*[local-name()='", x, "']")

kkv_raw <- vaski_docs |> filter(tyyppi == "KKV") |>
  left_join(vaski_raw |> select(Id, XmlData), by = c("id" = "Id"))
stopifnot("KKV-vastauksia ei löytynyt" = nrow(kkv_raw) > 500)

poimi_vastaaja <- function(xml_str) {
  doc <- tryCatch(read_xml(xml_str), error = function(e) NULL)
  if (is.null(doc)) return(tibble(kk_norm=NA, hallinnonala=NA, ministeri=NA))
  
  # Vastannut ministeri = Toimija, jolla ei-tyhjä AsemaTeksti (tehtävänimike)
  toimijat <- xml_find_all(doc, ln("Toimija"))
  asemat  <- map_chr(toimijat, \(t) xml_text(xml_find_first(t, ln("AsemaTeksti"))) %||% "")
  henkilot<- map_chr(toimijat, \(t) xml_text(xml_find_first(t, ln("Henkilo")))     %||% "")
  i <- which(str_squish(asemat) != "" & str_detect(str_to_lower(asemat), "ministeri"))[1]
  
  hallinnonala <- if (is.na(i)) NA_character_ else str_squish(asemat[i])
  # Nimi: poista tehtävänimike henkilo-tekstin alusta, ja lisää välit CamelCaseen
  nimi <- if (is.na(i)) NA_character_ else {
    n <- str_remove(henkilot[i], fixed(hallinnonala))
    str_squish(str_replace_all(n, "(?<=[a-zäö])(?=[A-ZÄÖ])", " "))
  }
  
  # Mihin KK:hon tämä vastaa
  kk <- xml_text(xml_find_first(doc, paste0(
    ".//*[local-name()='EduskuntaTunnus' or local-name()='ViiteTunnus' or ",
    "local-name()='AsiakirjaViiteTunnus']")))
  
  tibble(kk_norm = norm_tunnus(kk), hallinnonala = hallinnonala, ministeri = nimi)
}

vastaukset <- kkv_raw |>
  mutate(v = map(XmlData, poimi_vastaaja)) |>
  select(kkv_tunnus = tunnus, v) |> unnest(v) |>
  filter(!is.na(kk_norm))

# Linkitä alkuperäisiin kysymyksiin (KK), jotta saadaan vpvuosi ja tekijä mukaan
kk <- vaski_docs |> filter(tyyppi == "KK") |>
  transmute(kk_norm = norm_tunnus(tunnus), kk_tunnus = tunnus, vpvuosi)

kk_ministerit <- kk |> left_join(vastaukset, by = "kk_norm") |>
  transmute(tunnus = kk_tunnus, vpvuosi, hallinnonala, ministeri,
            vastattu = !is.na(hallinnonala))

# Tarkistukset
osuvuus <- mean(kk_ministerit$vastattu)
stopifnot("Alle puoleen KK:ista löytyi vastaaja — tarkista linkitys" = osuvuus > 0.5)
message("KK:ista vastattuja (ministeri tunnistettu): ", scales::percent(osuvuus),
        " | hallinnonaloja ", n_distinct(kk_ministerit$hallinnonala, na.rm = TRUE))
print(count(kk_ministerit, hallinnonala, sort = TRUE), n = 25)

qs2::qd_save(kk_ministerit, file.path(DATA, "kk_ministerit.qs"))