# =============================================================================
# 03-muutoskohta.R
#
# Peltipoliisi-sarja: ratkaiseva diagnostiikka ennen osan 1 kirjoittamista.
#
# MIKSI TAMA TARVITAAN
#
# Edellinen ajo nayttaa, etta Keha I:n ylitysosuus putoaa 29 prosentista
# (vk 29) 17 prosenttiin (vk 39). Se nayttaa valvonnan vaikutukselta.
# Se ei kuitenkaan ole sita, ennen kuin kaksi vaihtoehtoista selitysta
# on suljettu pois:
#
# 1. KAUSIVAIHTELU. Keha I:n ja verrokkien erotus oli vk 16 noin -15.7,
#    kaventui kesalla arvoon -8.0 ja palasi syksylla arvoon -16.6.
#    Syksyn "pudotus" on paluu kevaan tasolle. Vuosi 2025 on ainoa
#    tapa erottaa kausivaihtelu interventiosta.
#
# 2. KOOSTUMUSMUUTOS. Ryhmatason luku lasketaan poolaamalla kaikki
#    ajoneuvot. Jos yhden pisteen liikennemaara muuttuu, poolattu luku
#    liikkuu vaikka yksikaan kuljettaja ei muuttaisi kayttaytymistaan.
#    Piste 10 menetti 29 % liikenteestaan. Siksi tassa lasketaan
#    PISTEKOHTAISET erotukset ja vasta ne keskiarvoistetaan.
#
# Lisaksi: piste 116 - ainoa piste josta tiedamme varmasti, etta kamera
# kirjoitti sakon - ei nayta juuri mitaan muutosta. Sekin on selvitettava.
#
# Ei muuta mitaan. Kirjoittaa CSV-tiedostoja. Ajoaika pari minuuttia.
# =============================================================================

library(here)
library(tidyverse)
library(qs2)
library(lubridate)
library(sf)

select <- dplyr::select
filter <- dplyr::filter

DATA_DIR <- here("data", "peltipoliisi")

viiva <- function(otsikko) {
  cat("\n", strrep("=", 70), "\n", otsikko, "\n", strrep("=", 70), "\n", sep = "")
}

hist_pv   <- qd_read(file.path(DATA_DIR, "lam_hist_pv.qs2"))
kalenteri <- qd_read(file.path(DATA_DIR, "kalenteri.qs2"))
kontek    <- qd_read(file.path(DATA_DIR, "piste_konteksti.qs2"))
asemat    <- qs_read(file.path(DATA_DIR, "lam_asemat.qs2"))

wq <- function(nopeus, paino, q) {
  o <- order(nopeus); v <- nopeus[o]; w <- as.numeric(paino)[o]
  v[which(cumsum(w) / sum(w) >= q)[1]]
}

tunnusluvut <- function(d, ...) {
  d |>
    group_by(...) |>
    summarise(
      havainnot = sum(as.numeric(n)),
      ka   = sum(as.numeric(n) * nopeus) / sum(as.numeric(n)),
      p85  = wq(nopeus, n, 0.85),
      yli6 = 100 * sum(as.numeric(n)[nopeus >= raja + 6]) / sum(as.numeric(n)),
      .groups = "drop")
}

rajoitus <- kontek |>
  mutate(raja = as.integer(osm_maxspeed)) |> select(lam_id, raja)

kehaI <- asemat |> st_drop_geometry() |>
  filter(tienumero_lopullinen == 101) |> pull(lam_id)

arkipv <- kalenteri |> filter(arkipaiva, !arkipyha) |> pull(pvm)

pohja <- hist_pv |>
  filter(lk_ryhma == 1L, vapaa == 1L, pvm %in% arkipv) |>
  left_join(rajoitus, by = "lam_id")

taydet <- hist_pv |>
  mutate(vuosi = year(pvm)) |>
  group_by(lam_id, vuosi) |>
  summarise(pv = n_distinct(pvm), .groups = "drop") |>
  pivot_wider(names_from = vuosi, values_from = pv) |>
  filter(!is.na(`2025`), !is.na(`2026`), `2025` > 140, `2026` > 140) |>
  pull(lam_id)

# ----------------------------------------------------------------------------
# A. Puuttuva paiva: onko sama paiva poissa kaikilta pisteilta
# ----------------------------------------------------------------------------

viiva("A. PUUTTUVAT PAIVAT")

puuttuvat <- qd_read(file.path(DATA_DIR, "lam_puuttuvat.qs2"))
yleiset <- puuttuvat |> count(pvm, sort = TRUE) |> filter(n >= 10)
cat("Paivat jotka puuttuvat vahintaan 10 pisteelta:\n")
print(as.data.frame(yleiset))
cat("\nJos yksi paiva puuttuu lahes kaikilta, kyse on Digitrafficin\n",
    "katkoksesta eika pisteen viasta. Se jatetaan pois sarjoista.\n", sep = "")

# ----------------------------------------------------------------------------
# B. Koostumus: muuttuuko liikennemaara pisteittain kesken kauden
# ----------------------------------------------------------------------------

viiva("B. LIIKENNEMAARAN MUUTOS PISTEITTAIN")

volyymi <- hist_pv |>
  filter(pvm %in% arkipv) |>
  mutate(vuosi = year(pvm), vk = isoweek(pvm)) |>
  group_by(lam_id, vuosi) |>
  summarise(ajon_pv = sum(as.numeric(n)) / n_distinct(pvm), .groups = "drop") |>
  pivot_wider(names_from = vuosi, values_from = ajon_pv, names_prefix = "v") |>
  mutate(muutos_pros = round(100 * (v2026 / v2025 - 1), 1)) |>
  filter(lam_id %in% taydet) |>
  mutate(ryhma = if_else(lam_id %in% kehaI, "KehaI", "verrokki")) |>
  arrange(muutos_pros)

print(as.data.frame(volyymi |> mutate(across(c(v2025, v2026), round))))

# Pisteet joilla liikennemaara muuttui yli 8 % -> koostumus epavakaa.
epavakaat <- volyymi |> filter(abs(muutos_pros) > 8) |> pull(lam_id)
cat("\nEpavakaat pisteet (liikennemaara muuttui yli 8 %): ",
    paste(epavakaat, collapse = ", "), "\n")

# ----------------------------------------------------------------------------
# C. Vapaan virran osuus: muuttuuko suodattimen lapaisevä joukko
# ----------------------------------------------------------------------------
# Jos vapaan virran osuus romahtaa syksylla, jaljelle jaava joukko on
# valikoitunut eri tavalla ja vertailu ontuu.

viiva("C. VAPAAN VIRRAN OSUUS VIIKOITTAIN")

vapaa_vk <- hist_pv |>
  filter(lk_ryhma == 1L, pvm %in% arkipv, lam_id %in% taydet) |>
  mutate(vuosi = year(pvm), vk = isoweek(pvm),
         ryhma = if_else(lam_id %in% kehaI, "KehaI", "verrokki")) |>
  group_by(ryhma, vuosi, vk) |>
  summarise(vapaa_os = round(100 * sum(as.numeric(n)[vapaa == 1]) /
                               sum(as.numeric(n)), 1), .groups = "drop") |>
  pivot_wider(names_from = c(ryhma, vuosi), values_from = vapaa_os)

print(as.data.frame(vapaa_vk))

# ----------------------------------------------------------------------------
# D. RATKAISEVA: pistekohtainen vuosierotus viikoittain
# ----------------------------------------------------------------------------
# Jokaiselle pisteelle lasketaan sama viikko 2026 miinus 2025. Nain
# kausivaihtelu katoaa. Vasta sitten pisteiden erotukset keskiarvoistetaan,
# jolloin koostumusmuutos ei paase vaikuttamaan.

viiva("D. VUOSIEROTUS VIIKOITTAIN (kausivaihtelu poistettu)")

vk_piste <- pohja |>
  filter(lam_id %in% taydet) |>
  mutate(vuosi = year(pvm), vk = isoweek(pvm)) |>
  tunnusluvut(lam_id, vuosi, vk)

# Vain viikot joilta on molemmat vuodet ja riittavasti havaintoja.
yoy <- vk_piste |>
  filter(havainnot > 5000) |>
  select(lam_id, vuosi, vk, ka, yli6) |>
  pivot_wider(names_from = vuosi, values_from = c(ka, yli6)) |>
  filter(!is.na(ka_2025), !is.na(ka_2026)) |>
  mutate(d_ka = ka_2026 - ka_2025,
         d_yli6 = yli6_2026 - yli6_2025,
         ryhma = if_else(lam_id %in% kehaI, "KehaI", "verrokki"))

stopifnot(nrow(yoy) > 0)

# Keskiarvo pisteiden yli, ei poolattu. Molemmat versiot: kaikki pisteet
# ja ilman epavakaita.
koosta <- function(d, nimi) {
  d |>
    group_by(ryhma, vk) |>
    summarise(pisteita = n_distinct(lam_id),
              m_d_ka = round(mean(d_ka), 2),
              m_d_yli6 = round(mean(d_yli6), 2),
              kh_d_yli6 = round(sd(d_yli6), 2),
              .groups = "drop") |>
    mutate(aineisto = nimi)
}

yoy_kaikki <- koosta(yoy, "kaikki")
yoy_vakaat <- koosta(yoy |> filter(!lam_id %in% epavakaat), "vakaat")

taulu <- function(d) {
  d |>
    select(vk, ryhma, m_d_yli6) |>
    pivot_wider(names_from = ryhma, values_from = m_d_yli6) |>
    mutate(DiD = round(KehaI - verrokki, 2))
}

cat("\n--- Kaikki pisteet ---\n")
print(as.data.frame(taulu(yoy_kaikki)))

cat("\n--- Ilman epavakaita pisteita ---\n")
print(as.data.frame(taulu(yoy_vakaat)))

cat("\nm_d_yli6 = keskimaarainen muutos (pp) ylitysosuudessa vs. sama viikko 2025\n")
cat("DiD = Keha I:n muutos miinus verrokkien muutos\n")
cat("Jos valvonta vaikuttaa, DiD on noin 0 ennen ja selvasti\n")
cat("negatiivinen jalkeen.\n")

write_csv(bind_rows(yoy_kaikki, yoy_vakaat),
          file.path(DATA_DIR, "yoy_viikoittain.csv"))
write_csv(yoy, file.path(DATA_DIR, "yoy_pisteittain_viikoittain.csv"))

# ----------------------------------------------------------------------------
# E. Pistekohtainen vuosierotus ennen ja jalkeen
# ----------------------------------------------------------------------------
# Ennen: vk 28-33. Jalkeen: vk 36-39. Valissa vk 34-35 jatetaan pois,
# koska muutoskohta nayttaisi osuvan siihen.

viiva("E. PISTEKOHTAINEN ENNEN/JALKEEN")

jaksot <- yoy |>
  mutate(jakso = case_when(vk >= 28 & vk <= 33 ~ "ennen",
                           vk >= 36 & vk <= 39 ~ "jalkeen",
                           TRUE ~ NA_character_)) |>
  filter(!is.na(jakso)) |>
  group_by(lam_id, ryhma, jakso) |>
  summarise(d_yli6 = mean(d_yli6), d_ka = mean(d_ka), .groups = "drop") |>
  pivot_wider(names_from = jakso, values_from = c(d_yli6, d_ka)) |>
  mutate(muutos_yli6 = round(d_yli6_jalkeen - d_yli6_ennen, 2),
         muutos_ka   = round(d_ka_jalkeen - d_ka_ennen, 2)) |>
  left_join(kontek |> select(lam_id, kam_m = etaisyys_kameraan_m), by = "lam_id") |>
  left_join(rajoitus, by = "lam_id") |>
  mutate(kam_m = round(kam_m),
         epavakaa = lam_id %in% epavakaat,
         across(starts_with("d_"), \(x) round(x, 2))) |>
  arrange(ryhma, kam_m)

print(as.data.frame(jaksot |>
  select(lam_id, ryhma, raja, kam_m, epavakaa,
         d_yli6_ennen, d_yli6_jalkeen, muutos_yli6, muutos_ka)))

cat("\nRyhmakeskiarvot (ilman epavakaita):\n")
print(as.data.frame(
  jaksot |> filter(!epavakaa) |>
    group_by(ryhma) |>
    summarise(pisteita = n(),
              ka_muutos_yli6 = round(mean(muutos_yli6), 2),
              kh = round(sd(muutos_yli6), 2),
              ka_muutos_ka = round(mean(muutos_ka), 2), .groups = "drop")))

write_csv(jaksot, file.path(DATA_DIR, "ennen_jalkeen_pisteittain.csv"))

# Etaisyysgradientti: korreloiko muutos etaisyyteen kamerasta
cat("\nKeha I: muutos vs. etaisyys lahimpaan kameraan\n")
kg <- jaksot |> filter(ryhma == "KehaI", !epavakaa)
if (nrow(kg) >= 4) {
  print(as.data.frame(kg |> select(lam_id, kam_m, muutos_yli6, muutos_ka)))
  ct <- cor.test(kg$kam_m, kg$muutos_yli6, method = "spearman", exact = FALSE)
  cat(sprintf("Spearman rho = %.3f, p = %.3f, n = %d\n",
              ct$estimate, ct$p.value, nrow(kg)))
  cat("Positiivinen rho = vaikutus heikkenee etaisyyden kasvaessa.\n")
}

# ----------------------------------------------------------------------------
# F. Paivatason kausikorjattu sarja: milloin muutos tapahtui
# ----------------------------------------------------------------------------
# Jokaisesta paivasta vahennetaan saman pisteen saman ISO-viikon
# keskiarvo vuodelta 2025. Jaljelle jaa kausikorjattu poikkeama.

viiva("F. KAUSIKORJATTU PAIVASARJA 2026-07-27 ... 2026-09-25")

perus2025 <- vk_piste |>
  filter(vuosi == 2025, havainnot > 5000) |>
  select(lam_id, vk, ka_2025 = ka, yli6_2025 = yli6)

pv2026 <- pohja |>
  filter(lam_id %in% taydet, year(pvm) == 2026,
         pvm >= as.Date("2026-07-27"), pvm <= as.Date("2026-09-25")) |>
  tunnusluvut(lam_id, pvm) |>
  mutate(vk = isoweek(pvm)) |>
  left_join(perus2025, by = c("lam_id", "vk")) |>
  filter(!is.na(yli6_2025)) |>
  mutate(poikkeama_yli6 = yli6 - yli6_2025,
         poikkeama_ka = ka - ka_2025,
         ryhma = if_else(lam_id %in% kehaI, "KehaI", "verrokki"))

pv_ryhma <- pv2026 |>
  filter(!lam_id %in% epavakaat) |>
  group_by(pvm, ryhma) |>
  summarise(pisteita = n_distinct(lam_id),
            p_yli6 = round(mean(poikkeama_yli6), 2),
            p_ka = round(mean(poikkeama_ka), 2), .groups = "drop") |>
  pivot_wider(names_from = ryhma, values_from = c(pisteita, p_yli6, p_ka)) |>
  mutate(DiD_yli6 = round(p_yli6_KehaI - p_yli6_verrokki, 2),
         vpv = as.character(wday(pvm, label = TRUE, week_start = 1)))

p116 <- pv2026 |> filter(lam_id == 116L) |>
  select(pvm, p116_yli6 = poikkeama_yli6, p116_ka = poikkeama_ka) |>
  mutate(across(where(is.numeric), \(x) round(x, 2)))

pv_ryhma <- pv_ryhma |> left_join(p116, by = "pvm")

print(as.data.frame(pv_ryhma |>
  select(pvm, vpv, p_yli6_KehaI, p_yli6_verrokki, DiD_yli6,
         p116_yli6, p116_ka)))

cat("\np_yli6 = poikkeama saman viikon 2025 tasosta (pp)\n")
cat("DiD_yli6 = Keha I:n poikkeama miinus verrokkien poikkeama\n")

write_csv(pv_ryhma, file.path(DATA_DIR, "kausikorjattu_paivasarja.csv"))
write_csv(pv2026, file.path(DATA_DIR, "kausikorjattu_pisteittain.csv"))

# ----------------------------------------------------------------------------
# G. Piste 116 kaistoittain: muuttuuko nopea kaista
# ----------------------------------------------------------------------------
# Kaistat 3 (suunta 1) ja 4 (suunta 2) ovat nopeat kaistat.
# Jos valvonta puree, sen pitaisi nakya niissa ensin.

viiva("G. PISTE 116 KAISTOITTAIN, ENNEN JA JALKEEN")

k116 <- pohja |>
  filter(lam_id == 116L) |>
  mutate(vuosi = year(pvm), vk = isoweek(pvm)) |>
  filter(vk >= 28, vk <= 39) |>
  mutate(jakso = if_else(vk <= 33, "ennen", "jalkeen")) |>
  filter(vk != 34, vk != 35) |>
  tunnusluvut(kaista, suunta, vuosi, jakso) |>
  select(suunta, kaista, vuosi, jakso, ka, p85, yli6) |>
  mutate(ka = round(ka, 2), yli6 = round(yli6, 2)) |>
  pivot_wider(names_from = c(vuosi, jakso), values_from = c(ka, p85, yli6))

print(as.data.frame(k116))

cat("\nVertaa: yli6_2026_jalkeen - yli6_2026_ennen suhteessa samaan\n")
cat("erotukseen vuonna 2025. Nopea kaista on se jolla ka on korkein.\n")

write_csv(k116, file.path(DATA_DIR, "piste116_kaistat_ennen_jalkeen.csv"))

# ----------------------------------------------------------------------------
# H. Kamerat pisteen 116 lahella (korjattu tulostus)
# ----------------------------------------------------------------------------

viiva("H. OSM-KAMERAT")

osm <- qs_read(file.path(DATA_DIR, "osm_kamerat_tiet.qs2"))
p116geom <- asemat |> filter(lam_id == 116L)

lahella <- osm$kamerat
lahella$etaisyys_m <- as.numeric(st_distance(lahella, p116geom))
lahella <- lahella |> st_drop_geometry() |> as.data.frame()

sarakkeet <- intersect(c("osm_id", "direction", "maxspeed", "etaisyys_m"),
                       names(lahella))
lahella <- lahella[lahella$etaisyys_m < 3000, sarakkeet, drop = FALSE]
lahella <- lahella[order(lahella$etaisyys_m), ]
lahella$etaisyys_m <- round(lahella$etaisyys_m)
rownames(lahella) <- NULL
print(head(lahella, 20))

kam2km <- asemat |>
  filter(lam_id %in% kehaI) |>
  mutate(kameroita_2km = lengths(st_is_within_distance(geometry, osm$kamerat, 2000))) |>
  st_drop_geometry() |>
  select(lam_id, nimi, kameroita_2km) |>
  left_join(kontek |> select(lam_id, kam_m = etaisyys_kameraan_m), by = "lam_id") |>
  mutate(kam_m = round(kam_m)) |>
  arrange(kam_m)

cat("\nKameroita 2 km sateella kustakin Keha I:n pisteesta:\n")
print(as.data.frame(kam2km))

viiva("VALMIS")
