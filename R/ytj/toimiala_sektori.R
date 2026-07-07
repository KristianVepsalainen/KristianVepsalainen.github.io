# Mäppää TOL 2008 -toimialakoodin (5-numeroinen, esim. "62010") karkeaan
# pääsektoriin. Käytetään valikoitumismallin selittäjänä: yksittäisiä
# 5-numeroisia koodeja on satoja, joten ne on tiivistettävä tulkittaviksi ryhmiksi.
# Lähde: Tilastokeskuksen TOL 2008 -pääluokat (A–U).
toimiala_sektori <- function(main_line_code) {
  d2 <- suppressWarnings(as.integer(substr(main_line_code, 1, 2)))
  dplyr::case_when(
    is.na(d2)            ~ NA_character_,
    d2 <= 3              ~ "Alkutuotanto",              # A
    d2 <= 9              ~ "Kaivostoiminta",            # B
    d2 <= 33             ~ "Teollisuus",                # C
    d2 <= 35             ~ "Energia",                   # D
    d2 <= 39             ~ "Vesi ja jäte",              # E
    d2 <= 43             ~ "Rakentaminen",              # F
    d2 <= 47             ~ "Kauppa",                    # G
    d2 <= 53             ~ "Kuljetus ja varastointi",   # H
    d2 <= 56             ~ "Majoitus ja ravitsemus",    # I
    d2 <= 63             ~ "Informaatio ja viestintä",  # J
    d2 <= 66             ~ "Rahoitus ja vakuutus",      # K
    d2 == 68             ~ "Kiinteistöala",             # L
    d2 <= 75             ~ "Ammatillinen ja tieteellinen toiminta", # M
    d2 <= 82             ~ "Hallinto- ja tukipalvelut", # N
    d2 == 84             ~ "Julkinen hallinto",         # O
    d2 == 85             ~ "Koulutus",                  # P
    d2 <= 88             ~ "Terveys- ja sosiaalipalvelut", # Q
    d2 <= 93             ~ "Taide ja viihde",           # R
    d2 <= 96             ~ "Muut palvelut",             # S
    TRUE                 ~ "Muu"
  )
}
