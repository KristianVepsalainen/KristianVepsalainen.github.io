paavo_path <- here("data", "latausverkko", "paavo.qs")

pno <- paavo |>
  as_tibble() |>
  filter(Postinumeroalue != "KOKO MAA") |>
  transmute(
    # Kenttä on muotoa "00100  Helsinki keskusta - Etu-Töölö (Helsinki)"
    pno = str_extract(Postinumeroalue, "^\\d{5}"),
    alue_nimi = str_trim(str_remove(str_remove(Postinumeroalue, "^\\d{5}\\s+"),
                                    "\\s*\\([^)]+\\)$")),
    kunta = str_extract(Postinumeroalue, "(?<=\\()[^)]+(?=\\)$)"),
    x = `X-koordinaatti`,
    y = `Y-koordinaatti`,
    pinta_ala_m2 = `Postinumeroalueen pinta-ala`,
    asukkaat = `Asukkaat yhteensä (HE)`,
    mediaanitulot = `Asukkaiden mediaanitulot (HR)`,
    taloudet = `Taloudet yhteensä (TE)`,
    omistusasunnot = `Omistusasunnoissa asuvat taloudet (TE)`,
    vuokra_asunnot = `Vuokra-asunnoissa asuvat taloudet (TE)`,
    pientaloasunnot = `Pientaloasunnot (RA)`,
    kerrostaloasunnot = `Kerrostaloasunnot (RA)`,
    kesamokit = `Kesämökit yhteensä (RA)`,
    tyopaikat = `Työpaikat yhteensä (TP)`
    
  )

stopifnot(
  "Postinumero puuttuu joltain riviltä" = all(!is.na(pno$pno)),
  "Postinumeroita ei ole uniikkeja"     = !any(duplicated(pno$pno)),
  "Kuntatieto puuttuu laajasti"         = mean(is.na(pno$kunta)) < 0.02,
  "Koordinaatit eivät ole TM35FIN-alueella" =
    all(between(pno$x, 40000, 800000) & between(pno$y, 6.6e6, 7.8e6)),
  "Asukasluvut negatiivisia" = all(pno$asukkaat >= 0, na.rm = TRUE)
)

# Väestösumman järkevyys: Manner-Suomi + Ahvenanmaa ≈ 5,6 milj.
sum(pno$asukkaat, na.rm = TRUE)

pno_sf <- pno |> st_as_sf(coords = c("x", "y"), crs = 3067, remove = FALSE)
qs2::qs_save(pno_sf, paavo_path)

names(paavo)[60:109]