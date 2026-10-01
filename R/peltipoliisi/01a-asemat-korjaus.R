# =============================================================================
# 01a-asemat-korjaus.R
#
# Peltipoliisi-sarja, osa 1 korjattuna.
#
# Korvaa 01-hae-lam-data.R:n jakson "1. LAM-ASEMIEN METATIEDOT" kokonaan.
# Aja tama ensin; sen jalkeen 01-hae-lam-data.R jaksosta 2 eteenpain.
#
# Miksi korjaus: /api/tms/v1/stations -listaus EI palauta roadAddress-kenttaa,
# joten tienumero jai tyhjaksi ja skripti pysahtyi omaan tarkistukseensa.
# Tarkistus toimi oikein - oletus kentan olemassaolosta oli vaara.
#
# Tienumero paatellaan nyt kahdella riippumattomalla tavalla:
#   A) aseman nimesta (muotoa "vt1_Espoo_Hirvisuo", "kt101_...")
#   B) asemakohtaisesta /stations/{id} -vastauksesta
# ja tulokset ristiintarkistetaan. Kumpaakaan kenttanimea ei oleteta
# olemassa olevaksi ennen kuin rakenne on tulostettu nakyviin.
# =============================================================================

library(here)
library(tidyverse)
library(httr2)
library(qs2)
library(sf)
library(jsonlite)

select <- dplyr::select
filter <- dplyr::filter

PROJEKTI <- "peltipoliisi"
DATA_DIR <- here("data", PROJEKTI)
dir.create(DATA_DIR, recursive = TRUE, showWarnings = FALSE)

DIGITRAFFIC_USER <- "kristianvepsalainen.com / peltipoliisi-analyysi"

# Kehä I = kantatie 101. Verrokit: Kehä III (50), Länsiväylä (51),
# Tuusulanväylä (45), Lahdenväylä (4), Porvoonväylä (7), Hämeenlinnanväylä (3).
TIET_KASITELTAVA <- c(101L)
TIET_VERROKKI    <- c(50L, 51L, 45L, 4L, 7L, 3L)
VERROKKEJA_PER_TIE <- 2L
PAKOLLISET_PISTEET <- c(116L)   # Leppäsolmu, haasteen kohde

# ----------------------------------------------------------------------------
# Apufunktiot
# ----------------------------------------------------------------------------

# Kulkee sisakkaisen listan lapi ja palauttaa NULL heti kun jokin taso puuttuu.
# Nain yksikaan puuttuva valitaso ei kaada skriptia.
poimi <- function(x, ...) {
  for (nimi in c(...)) {
    if (is.null(x) || !is.list(x) || !nimi %in% names(x)) return(NULL)
    x <- x[[nimi]]
  }
  x
}

# Skalaariksi pakottaminen. Lista, tyhja tai monialkioinen -> NA.
skalaari <- function(v) {
  if (is.null(v) || length(v) != 1) return(NULL)
  v <- v[[1]]
  if (is.null(v) || is.list(v) || length(v) != 1) return(NULL)
  v
}
yksi_int <- function(v) { v <- skalaari(v); if (is.null(v)) NA_integer_   else suppressWarnings(as.integer(v)) }
yksi_chr <- function(v) { v <- skalaari(v); if (is.null(v)) NA_character_ else as.character(v) }
yksi_num <- function(v) { v <- skalaari(v); if (is.null(v)) NA_real_      else suppressWarnings(as.numeric(v)) }

digitraffic_json <- function(url) {
  request(url) |>
    req_headers("Digitraffic-User" = DIGITRAFFIC_USER) |>
    req_throttle(rate = 50 / 60) |>
    req_retry(max_tries = 4, backoff = \(i) 2^i) |>
    req_perform() |>
    resp_body_json(check_type = FALSE)
}

# ----------------------------------------------------------------------------
# 1A. Asemalistaus ja sen todellinen rakenne
# ----------------------------------------------------------------------------

hae_asemalistaus <- function() {
  polku <- file.path(DATA_DIR, "lam_asemat_raaka.qs2")
  if (file.exists(polku)) return(qs_read(polku))
  js <- digitraffic_json("https://tie.digitraffic.fi/api/tms/v1/stations")
  stopifnot("features" %in% names(js), length(js$features) > 100)
  qs_save(js, polku)
  js
}

js <- hae_asemalistaus()

# Tulostetaan mita rajapinta oikeasti antaa. Talla kertaa ei arvata.
message("\n=== Listauksen ensimmaisen aseman properties-kentat ===")
print(names(js$features[[1]]$properties))

message("\n=== Ensimmaisen aseman koko rakenne ===")
str(js$features[[1]], max.level = 3)

# Koko ensimmainen feature myos levylle, jotta sita voi lukea rauhassa.
write_json(js$features[[1]], file.path(DATA_DIR, "asema_esimerkki.json"),
           auto_unbox = TRUE, pretty = TRUE)
message("Esimerkkiasema tallennettu: ",
        file.path(DATA_DIR, "asema_esimerkki.json"))

# Kaikkien asemien properties-kentat: onko rakenne yhtenainen?
kentat <- js$features |> map(\(f) names(f$properties)) |> unlist() |> table()
message("\n=== Kentat ja niiden esiintymismaarat (n = ", length(js$features), ") ===")
print(sort(kentat, decreasing = TRUE))

# Koordinaatit: geometry$coordinates = [lon, lat] (mahdollisesti myos korkeus).
koordinaatti <- function(f, i) {
  k <- poimi(f, "geometry", "coordinates")
  if (is.null(k) || length(k) < 2) return(NA_real_)
  as.numeric(k[[i]])
}

asemat_perus <- map_dfr(js$features, function(f) {
  p <- f$properties
  tibble(
    asema_id    = yksi_int(poimi(p, "id")),
    lam_id      = yksi_int(poimi(p, "tmsNumber")),
    nimi        = yksi_chr(poimi(p, "name")),
    kerayksessa = yksi_chr(poimi(p, "collectionStatus")),
    tila        = yksi_chr(poimi(p, "state")),
    lon         = koordinaatti(f, 1),
    lat         = koordinaatti(f, 2)
  )
})

stopifnot(nrow(asemat_perus) > 100)

# Asemat ilman tmsNumberia tai koordinaattia eivat kelpaa. Kerrotaan montako.
vajaat <- asemat_perus |> filter(is.na(lam_id) | is.na(lon) | is.na(lat))
if (nrow(vajaat) > 0) {
  message("Pudotetaan ", nrow(vajaat),
          " asemaa joilta puuttuu tmsNumber tai koordinaatti.")
  print(vajaat |> select(asema_id, lam_id, nimi, lon, lat), n = 20)
  asemat_perus <- asemat_perus |> filter(!is.na(lam_id), !is.na(lon), !is.na(lat))
}

# TARKISTUS: lam_id on avain - ei duplikaatteja.
if (any(duplicated(asemat_perus$lam_id))) {
  print(asemat_perus |> filter(lam_id %in% lam_id[duplicated(lam_id)]) |>
          arrange(lam_id) |> select(asema_id, lam_id, nimi))
  stop("Sama tmsNumber usealla asemalla - liitokset menisivat rikki.")
}

message("\nAsemia listauksessa: ", nrow(asemat_perus))

# TARKISTUS: ilman asema_id:ta asemakohtaista hakua ei voi tehda.
if (all(is.na(asemat_perus$asema_id))) {
  warning("Kentta 'id' puuttuu listauksesta. Asemakohtainen haku ei onnistu ",
          "id:lla - tienumero jaa nimen ja sijainnin varaan.")
}

# ----------------------------------------------------------------------------
# 1B. Tienumero aseman nimesta
# ----------------------------------------------------------------------------
# vt = valtatie, kt = kantatie, mt = maantie, st = seututie,
# pt = paikallistie, yt = yleinen tie, ra = ramppi.

TIE_REGEX <- "^\\s*(vt|kt|mt|st|pt|yt|ra)\\s*_?\\s*([0-9]+)"

asemat_perus <- asemat_perus |>
  mutate(
    tie_luokka        = str_match(nimi, regex(TIE_REGEX, ignore_case = TRUE))[, 2],
    tienumero_nimesta = as.integer(str_match(nimi, regex(TIE_REGEX, ignore_case = TRUE))[, 3])
  )

osuus_nimesta <- mean(!is.na(asemat_perus$tienumero_nimesta))
message(sprintf("Tienumero tunnistettu nimesta: %.1f %% asemista",
                100 * osuus_nimesta))

if (osuus_nimesta < 0.8) {
  message("\n=== Esimerkkeja nimista joita regex ei tunnistanut ===")
  print(asemat_perus |> filter(is.na(tienumero_nimesta)) |>
          slice_head(n = 25) |> pull(nimi))
}

# ----------------------------------------------------------------------------
# 1C. Tienumero asemakohtaisesta vastauksesta
# ----------------------------------------------------------------------------
# Haetaan ensin YKSI asema ja tulostetaan sen rakenne. Vasta sitten
# paatetaan mista kentasta tienumero luetaan.

kohde <- asemat_perus |> filter(lam_id %in% PAKOLLISET_PISTEET)
stopifnot(nrow(kohde) >= 1)
message("\nKohdepiste listauksessa: ", kohde$nimi[1],
        " (asema_id ", kohde$asema_id[1], ")")

if (!is.na(kohde$asema_id[1])) {
  js1 <- digitraffic_json(sprintf(
    "https://tie.digitraffic.fi/api/tms/v1/stations/%d", kohde$asema_id[1]))

  message("\n=== Asemakohtaisen vastauksen properties-kentat ===")
  print(names(js1$properties))

  write_json(js1, file.path(DATA_DIR, "asema_116_tiedot.json"),
             auto_unbox = TRUE, pretty = TRUE)

  if ("roadAddress" %in% names(js1$properties)) {
    message("\n=== roadAddress ===")
    print(js1$properties$roadAddress)
  } else {
    message("\nHUOM: roadAddress puuttuu MYOS asemakohtaisesta vastauksesta. ",
            "Tienumero otetaan nimesta.")
  }
}

# Haetaan asemakohtaiset tiedot vain ehdokkaille, ei kaikille 490:lle.
# Ehdokkaat = nimen perusteella kiinnostavat tiet + pakolliset pisteet.
# Jos nimesta ei saatu mitaan, haetaan kaikki (hitaampi mutta varma).
ehdokkaat <- if (osuus_nimesta >= 0.5) {
  asemat_perus |>
    filter(tienumero_nimesta %in% c(TIET_KASITELTAVA, TIET_VERROKKI) |
             lam_id %in% PAKOLLISET_PISTEET) |>
    pull(asema_id)
} else {
  asemat_perus$asema_id
}
ehdokkaat <- ehdokkaat[!is.na(ehdokkaat)]

message("\nAsemakohtaisia hakuja: ", length(ehdokkaat),
        " (n. ", round(length(ehdokkaat) * 1.2 / 60, 1), " min)")

hae_asematiedot <- function(asema_idt) {
  polku <- file.path(DATA_DIR, "lam_asematiedot.qs2")
  if (file.exists(polku)) {
    vanha <- qd_read(polku)
    puuttuvat <- setdiff(asema_idt, vanha$asema_id)
    if (length(puuttuvat) == 0) return(vanha)
    asema_idt <- puuttuvat
  } else {
    vanha <- NULL
  }

  hae_yksi <- function(asema_id) {
    js1 <- tryCatch(
      digitraffic_json(sprintf(
        "https://tie.digitraffic.fi/api/tms/v1/stations/%d", asema_id)),
      error = function(e) NULL)
    if (is.null(js1)) return(tibble(asema_id = asema_id))
    p <- js1$properties
    tibble(
      asema_id   = asema_id,
      kunta      = yksi_chr(poimi(p, "municipality")),
      maakunta   = yksi_chr(poimi(p, "province")),
      tienumero  = yksi_int(poimi(p, "roadAddress", "roadNumber")),
      tieosa     = yksi_int(poimi(p, "roadAddress", "roadSection")),
      etaisyys   = yksi_int(poimi(p, "roadAddress", "distance")),
      suunta1    = yksi_chr(poimi(p, "direction1Municipality")),
      suunta2    = yksi_chr(poimi(p, "direction2Municipality")),
      vapaa_nop1 = yksi_num(poimi(p, "freeFlowSpeed1")),
      vapaa_nop2 = yksi_num(poimi(p, "freeFlowSpeed2"))
    )
  }

  uusi <- map_dfr(asema_idt, hae_yksi, .progress = TRUE)
  kaikki <- bind_rows(vanha, uusi)
  qd_save(kaikki, polku)
  kaikki
}

asematiedot <- hae_asematiedot(ehdokkaat)

if ("tienumero" %in% names(asematiedot)) {
  message(sprintf("Tienumero saatu asemakohtaisesta haussa: %.1f %% ehdokkaista",
                  100 * mean(!is.na(asematiedot$tienumero))))
}

# ----------------------------------------------------------------------------
# 1D. Yhdistys ja ristiintarkistus
# ----------------------------------------------------------------------------

asemat <- asemat_perus |>
  left_join(asematiedot, by = "asema_id")

if (!"tienumero" %in% names(asemat)) asemat$tienumero <- NA_integer_
if (!"tieosa"    %in% names(asemat)) asemat$tieosa    <- NA_integer_
if (!"etaisyys"  %in% names(asemat)) asemat$etaisyys  <- NA_integer_

asemat <- asemat |>
  mutate(tienumero_lopullinen = coalesce(tienumero, tienumero_nimesta))

# TARKISTUS: kaksi riippumatonta lahdetta eivat saa olla eri mielta.
ristiriita <- asemat |>
  filter(!is.na(tienumero), !is.na(tienumero_nimesta),
         tienumero != tienumero_nimesta)

if (nrow(ristiriita) > 0) {
  message("\n=== Ristiriitaiset tienumerot (rajapinta vs. nimi) ===")
  print(ristiriita |> select(lam_id, nimi, tienumero, tienumero_nimesta), n = 50)
  warning(nrow(ristiriita), " asemalla lahteet eroavat. Rajapinnan arvo voittaa.")
}

if (all(is.na(asemat$tienumero_lopullinen))) {
  stop("Tienumeroa ei saatu kummallakaan tavalla. Lue ", DATA_DIR,
       "/asema_esimerkki.json ja korjaa poiminta ennen jatkoa.")
}

asemat_sf <- st_as_sf(asemat, coords = c("lon", "lat"), crs = 4326,
                      remove = FALSE) |>
  st_transform(3067)

qs_save(asemat_sf, file.path(DATA_DIR, "lam_asemat.qs2"))

# ----------------------------------------------------------------------------
# 1E. Pistevalinta
# ----------------------------------------------------------------------------

message("\n=== Kehä I:n (kt 101) LAM-pisteet ===")
kehaI <- asemat_sf |>
  st_drop_geometry() |>
  filter(tienumero_lopullinen %in% TIET_KASITELTAVA) |>
  arrange(tieosa, etaisyys)

print(kehaI |> select(lam_id, nimi, kunta, tieosa, etaisyys,
                      suunta1, suunta2, kerayksessa), n = 100)

# TARKISTUS: Kehä I:lla on kymmenia pisteita. Kourallinen = paattely pieleen.
if (nrow(kehaI) < 5) {
  message("\nKaikki nimet joissa esiintyy '101':")
  print(asemat_sf |> st_drop_geometry() |> filter(str_detect(nimi, "101")) |>
          select(lam_id, nimi, tienumero, tienumero_nimesta))
  stop("Kehä I:lta loytyi vain ", nrow(kehaI), " pistetta. Tarkista ",
       "tienumeron paattely yllä olevasta tulosteesta ennen jatkoa.")
}

# TARKISTUS: kohdepisteen on oltava mukana.
if (!116L %in% kehaI$lam_id) {
  message("\nPisteen 116 tiedot:")
  print(asemat_sf |> st_drop_geometry() |> filter(lam_id == 116L) |>
          select(lam_id, nimi, kunta, tienumero, tienumero_nimesta,
                 tienumero_lopullinen))
  stop("Piste 116 ei paatynyt Kehä I:n listalle. Selvita syy ennen jatkoa.")
}

pisteet_verrokki <- asemat_sf |>
  st_drop_geometry() |>
  filter(tienumero_lopullinen %in% TIET_VERROKKI) |>
  group_by(tienumero_lopullinen) |>
  arrange(tieosa, etaisyys, .by_group = TRUE) |>
  slice_head(n = VERROKKEJA_PER_TIE) |>
  ungroup() |>
  pull(lam_id)

PISTEET <- sort(unique(c(PAKOLLISET_PISTEET, kehaI$lam_id, pisteet_verrokki)))

# TARKISTUS: keraystila. Arvot tulostetaan, ei oleteta.
message("\n=== collectionStatus-kentan arvot ===")
print(table(asemat_sf$kerayksessa, useNA = "ifany"))

aktiiviset <- asemat_sf |>
  st_drop_geometry() |>
  filter(lam_id %in% PISTEET,
         is.na(kerayksessa) | str_detect(kerayksessa, "GATHERING")) |>
  pull(lam_id)

message("\nValittuja pisteita: ", length(PISTEET),
        " | niista keraystilassa: ", length(aktiiviset))

if (!116L %in% aktiiviset) {
  warning("Piste 116 ei ole keraystilassa. Pidetaan mukana silti - ",
          "historiadata voi olla saatavilla vaikka tila olisi muu.")
  aktiiviset <- union(aktiiviset, 116L)
}

qd_save(tibble(lam_id = sort(aktiiviset)),
        file.path(DATA_DIR, "valitut_pisteet.qs2"))

message("\nValmis. Pisteet tallennettu: ",
        file.path(DATA_DIR, "valitut_pisteet.qs2"))
message("Jatka 01-hae-lam-data.R:n jaksosta 2, ja korvaa sen rivi")
message("  PISTEET <- sort(unique(c(...)))")
message("rivilla")
message("  PISTEET <- qd_read(here('data','peltipoliisi','valitut_pisteet.qs2'))$lam_id")
