library(tidyverse); library(here); library(xml2); library(qs2)
select <- dplyr::select; filter <- dplyr::filter

DATA <- here("data", "eduskunta")
lue  <- function(p) tryCatch(qs2::qd_read(p), error = function(e) qs2::qs_read(p))

vaski_raw <- lue(file.path(DATA, "vaski_raw.qs"))
mp        <- lue(file.path(DATA, "mp.qs"))

# local-name()-apurit, koska VaskiData käyttää nimiavaruuksia
ln <- function(x) paste0("*[local-name()='", x, "']")
xp <- function(...) paste0(".//", paste(sapply(c(...), ln), collapse = "//"))

poimi_init <- function(xml_str, tyyppi_koodi) {
  doc <- tryCatch(read_xml(xml_str), error = function(e) NULL)
  if (is.null(doc)) return(tibble(tekija = NA_character_, vpvuosi = NA_integer_,
                                  tyyppi = tyyppi_koodi, aiheet = list(character()),
                                  allekirjoittajat = list(character())))
  tekija <- xml_attr(xml_find_first(doc, xp("RakenneAsiakirja","IdentifiointiOsa","Henkilo")),
                     "muuTunnus")
  vpv    <- suppressWarnings(as.integer(xml_text(xml_find_first(doc, xp("ValtiopaivavuosiTeksti")))))
  aiheet <- xml_text(xml_find_all(doc, xp("AiheTeksti")))
  allek  <- xml_attr(xml_find_all(doc, xp("AllekirjoitusOsa","Henkilo")), "muuTunnus")
  tibble(tekija = tekija, vpvuosi = vpv, tyyppi = tyyppi_koodi,
         aiheet = list(aiheet[!is.na(aiheet) & aiheet != ""]),
         allekirjoittajat = list(unique(allek[!is.na(allek)])))
}

init_raw <- vaski_raw |>
  mutate(tyyppi  = str_extract(Eduskuntatunnus, "^[A-Za-zÄÖ]+"),
         created = suppressWarnings(as.Date(str_sub(Created, 1, 10)))) |>
  filter(tyyppi %in% c("KK", "LA", "TPA", "TAA"))
stopifnot("Aloitteita ei löytynyt — tarkista Eduskuntatunnus-kenttä" = nrow(init_raw) > 1000)

vaski_init <- init_raw |>
  mutate(p = map2(XmlData, tyyppi, poimi_init)) |>
  transmute(id = Id, tunnus = Eduskuntatunnus, created, p) |>
  unnest(p)

# Linkitystarkistus: osuuko tekijätunnus MP-dataan?
osuvuus <- mean(vaski_init$tekija %in% mp$henkilo_nro, na.rm = TRUE)
stopifnot("Tekijätunnus ei linkity MP-dataan (muuTunnus ei olekaan henkilo_nro)" = osuvuus > 0.7)
message("Tekijälinkitys: ", scales::percent(osuvuus))

qs2::qd_save(vaski_init, file.path(DATA, "vaski_init.qs"))