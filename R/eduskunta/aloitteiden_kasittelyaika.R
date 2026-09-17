library(tidyverse); library(here); library(xml2); library(qs2)
select <- dplyr::select; filter <- dplyr::filter

DATA <- here("data", "eduskunta")
lue  <- function(p) tryCatch(qs2::qd_read(p), error = function(e) qs2::qs_read(p))
vaski_raw  <- lue(file.path(DATA, "vaski_raw.qs"))
vaski_docs <- lue(file.path(DATA, "vaski_docs.qs"))

norm_tunnus <- function(x) str_remove_all(str_to_upper(x), "\\s") |> str_remove("VP$")

# 1) Jokaisen asiakirjan laadintapäivä suoraan XML-attribuutista (nopea, ei full parse)
doc_pvm <- vaski_raw |>
  mutate(pvm = suppressWarnings(as.Date(
    str_match(XmlData, 'laadintaPvm="(\\d{4}-\\d{2}-\\d{2})"')[, 2])),
    tunnus_norm = norm_tunnus(Eduskuntatunnus)) |>
  filter(!is.na(pvm)) |>
  distinct(tunnus_norm, .keep_all = TRUE) |>
  select(tunnus_norm, pvm)
stopifnot("laadintaPvm-attribuuttia ei löytynyt — tarkista XML" = nrow(doc_pvm) > 1000)

# 2) Mietinnöt: mikä asia (HE/LA/…) niissä käsitellään?  Vireilletulo -> EduskuntaTunnus
poimi_viite <- function(xml_str) {
  doc <- tryCatch(read_xml(xml_str), error = function(e) NULL)
  if (is.null(doc)) return(NA_character_)
  v <- xml_text(xml_find_first(doc,
                               ".//*[local-name()='Vireilletulo']//*[local-name()='EduskuntaTunnus']"))
  if (is.na(v) || v == "")   # varafallback: ensimmäinen asiatunnus tekstistä
    v <- str_extract(xml_text(doc), "(?i)(HE|LA|TPA|TAA|VNS|K)\\s?\\d+/\\d{4}")
  norm_tunnus(v)
}

mietinnot <- vaski_docs |> filter(str_detect(tyyppi, "VM$")) |>
  left_join(vaski_raw |> select(Id, XmlData), by = c("id" = "Id")) |>
  mutate(valiokunta_koodi = str_remove(tyyppi, "M$"),      # "HaVM" -> "HaV"
         mietinto_norm    = norm_tunnus(tunnus),
         viite_norm       = map_chr(XmlData, poimi_viite))

# 3) Käsittelyaika = mietinnön pvm  −  käsitellyn esityksen pvm
vk_nimet <- c(HaV="Hallintovaliokunta", StV="Sosiaali- ja terveysvaliokunta",
              SiV="Sivistysvaliokunta", TaV="Talousvaliokunta", LaV="Lakivaliokunta",
              PeV="Perustuslakivaliokunta", MmV="Maa- ja metsätalousvaliokunta",
              UaV="Ulkoasiainvaliokunta", PuV="Puolustusvaliokunta", LiV="Liikenne- ja viestintävaliokunta",
              TyV="Työelämä- ja tasa-arvovaliokunta", YmV="Ympäristövaliokunta",
              VaV="Valtiovarainvaliokunta", TrV="Tarkastusvaliokunta", SuV="Suuri valiokunta",
              TuV="Tulevaisuusvaliokunta", TiV="Tiedusteluvalvontavaliokunta")

kasittelyajat <- mietinnot |>
  left_join(doc_pvm |> rename(mietinto_pvm = pvm), by = c("mietinto_norm" = "tunnus_norm")) |>
  left_join(doc_pvm |> rename(vireille_pvm = pvm), by = c("viite_norm"    = "tunnus_norm")) |>
  transmute(
    mietinto   = tunnus,
    valiokunta = coalesce(vk_nimet[valiokunta_koodi], valiokunta_koodi),
    viite      = viite_norm,
    vireille_pvm, mietinto_pvm,
    paivia     = as.integer(mietinto_pvm - vireille_pvm)) |>
  filter(!is.na(paivia), paivia >= 0, paivia < 1000)

# Järkevyystarkistukset — kaatuu heti jos linkitys tai päivät menivät pieleen
stopifnot(
  "Käsittelyaikoja ei syntynyt" = nrow(kasittelyajat) > 200,
  "Epäuskottava mediaani"       = between(median(kasittelyajat$paivia), 5, 400))
message("Käsittelyaikoja: ", nrow(kasittelyajat),
        " | mediaani ", median(kasittelyajat$paivia), " pv | valiokuntia ",
        n_distinct(kasittelyajat$valiokunta))

qs2::qd_save(kasittelyajat, file.path(DATA, "kasittelyajat.qs"))