# Reitti B2: saako lakialoite mietinnön (etenee) vai ei (raukeaa)?

#Luetaan lähtödata

DATA <- here("data", "eduskunta")
vaski_docs <- qs2::qd_read(file.path(DATA, "vaski_docs.qs"))  # id, lapimeni (0/1)

# 1) VM-mietinnöt (kaikki valiokunnat, myös ruotsi: ...VM ja ...UB)
mietinnot <- vaski_docs |>
  filter(str_detect(tyyppi, "VM$") | str_detect(tyyppi, "U[B]$")) |>
  left_join(vaski_raw |> select(Id, XmlData), by = c("id" = "Id"))

# 2) Poimi kustakin mietinnöstä käsiteltyjen asioiden tunnukset
ln <- function(x) paste0(".//*[local-name()='", x, "']")
kasitellyt <- mietinnot |>
  mutate(viitteet = map(XmlData, \(x) {
    doc <- tryCatch(xml2::read_xml(x), error = function(e) NULL)
    if (is.null(doc)) return(character())
    # Vireilletulo/käsitellyt asiat -> eduskuntatunnukset (esim. "LA 12/2023 vp")
    xml2::xml_text(xml2::xml_find_all(doc, ln("EduskuntaTunnus")))
  })) |>
  select(mietinto = tunnus, viitteet) |> tidyr::unnest(viitteet) |>
  mutate(viitteet = str_squish(viitteet))

# 3) Merkitse lakialoitteet: edennyt (sai mietinnön) vai ei
aloite_tulokset <- vaski_docs |> filter(tyyppi == "LA") |>
  mutate(eteni = tunnus %in% kasitellyt$viitteet,
         lopputulos = if_else(eteni, "Eteni käsittelyyn", "Ei edennyt")) |>
  select(id, tunnus, vpvuosi, lopputulos, eteni)

# Tarkistukset
stopifnot("Yksikään aloite ei edennyt — viitepoiminta pieleen" = any(aloite_tulokset$eteni),
          "Kaikki etenivät — epäuskottavaa" = mean(aloite_tulokset$eteni) < 0.8)
message("Edenneiden lakialoitteiden osuus: ",
        scales::percent(mean(aloite_tulokset$eteni), 0.1))

qs2::qd_save(aloite_tulokset, here::here("data", "eduskunta", "aloite_tulokset.qs"))