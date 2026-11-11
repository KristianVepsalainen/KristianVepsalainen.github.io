# =============================================================================
# 07-poikkeustilanne-analyysi.R
#
# Korjattu ilmaisin. Ei lataa mitaan uudelleen: lukee minuuttidata.qs2:n,
# jonka 06-poikkeustilanne-haku.R tuotti.
#
# MITA EDELLISESSA MENI PIELEEN
#
# 1. NOLLAMINUUTIT PUUTTUIVAT. tiivista_minuutit() ryhmitteli havaintoja,
#    joten minuutti jonka aikana ei kulkenut yhtaan autoa ei tuottanut
#    rivia lainkaan. Se ei ollut nolla vaan poissa. Taydellisin sulku
#    oli siis aineistossa nakymaton.
#
# 2. VAARA MITTARI. Suodatin etsi vahaliikenteisia minuutteja. Sulku ei
#    ole vahaliikenteinen minuutti vaan pitka hiljaisuus. Liputetuista
#    minuuteista valtaosalla aikavali oli PIENEMPI kuin tavallisesti.
#
# 3. VAARA ANOMALIA. 45 % liputetuista riveista tuli pisteesta 10
#    (Espoo Laajalahti), joka on tietyomaa ja merkittiin epavakaaksi jo
#    sarjan ensimmaisessa osassa.
#
# 4. HARHAINEN VERROKKI. Odotusarvo laskettiin verrokkipaivista, joten
#    verrokkipaiva vertautui mediaaniin johon se itse vaikutti. Tama
#    painaa verrokkien vaarien havaintojen osuutta alas.
#
# MITA TASSA TEHDAAN TOISIN
#
# - Ruudukko taydennetaan nolliksi.
# - Paamittari on hiljaisuuden pituus, ei ajoneuvojen maara.
# - Verrokkien odotusarvo lasketaan jattamalla kukin paiva vuorollaan pois.
# - Epavakaat pisteet suljetaan pois.
# - Lisataan etenemistesti: saattue liikkuu, tyomaa ei.
#
# Ajoaika muutama minuutti. Ei verkkoyhteytta.
# =============================================================================

library(here)
library(tidyverse)
library(qs2)
library(lubridate)
library(sf)

select <- dplyr::select
filter <- dplyr::filter

DATA_DIR <- here("data", "peltipoliisi")

viiva <- function(x) cat("\n", strrep("=", 70), "\n", x, "\n",
                         strrep("=", 70), "\n", sep = "")

minuutit_raaka <- qd_read(file.path(DATA_DIR, "minuuttidata.qs2"))
asemat <- qs_read(file.path(DATA_DIR, "lam_asemat.qs2"))

# --- Asetukset ---------------------------------------------------------------

# Tarkastelu paivaliikenteen aikaan. Yolla hiljaisuus on normaalia.
MIN_ALKU <- 6 * 60
MIN_LOPPU <- 22 * 60

# Hiljaisuuden ehdot. Molempien taytyttava:
#   absoluuttinen - aikavali vahintaan tama, sekunteina
#   suhteellinen  - aikavali vahintaan tama kerroin pisteen omaan tasoon
HILJAISUUS_S <- 60
HILJAISUUS_KERROIN <- 8

# Pisteet joilla liikennemaara muuttui paljon kesken jakson: naita ei
# voi verrata, koska tien fyysinen tilanne on eri.
EPAVAKAAT <- c(10L, 147L)

# --- Ruudukon taydennys ------------------------------------------------------

viiva("1. NOLLAMINUUTTIEN TAYDENNYS")

cat("Rivejä ennen täydennystä:", nrow(minuutit_raaka), "\n")

paiva_tiedot <- minuutit_raaka |>
  distinct(pvm, tapahtuma, on_tapahtuma)

# Vain ne (piste, suunta) -parit, joilta on dataa, ja vain niiltä päiviltä
# joilta piste ylipäätään tuotti dataa. Muuten täydennettäisiin nolliksi
# myös pisteitä jotka olivat kokonaan pois käytöstä.
toimivat <- minuutit_raaka |>
  group_by(lam_id, pvm, suunta) |>
  summarise(havaintoja = sum(ajoneuvoja), .groups = "drop") |>
  filter(havaintoja >= 500)

minuutit <- toimivat |>
  select(lam_id, pvm, suunta) |>
  tidyr::expand_grid(minuutti_vrk = 0:1439) |>
  left_join(minuutit_raaka |>
              select(lam_id, pvm, suunta, minuutti_vrk,
                     ajoneuvoja, max_vali_s, ka_nopeus),
            by = c("lam_id", "pvm", "suunta", "minuutti_vrk")) |>
  mutate(ajoneuvoja = coalesce(ajoneuvoja, 0L)) |>
  left_join(paiva_tiedot, by = "pvm")

cat("Rivejä täydennyksen jälkeen:", nrow(minuutit), "\n")
cat("Niistä minuutteja joissa 0 ajoneuvoa:",
    sum(minuutit$ajoneuvoja == 0), "\n")

paivalla <- minuutit |>
  filter(minuutti_vrk >= MIN_ALKU, minuutti_vrk < MIN_LOPPU,
         !lam_id %in% EPAVAKAAT)

cat("Päiväaikaisia minuutteja (klo 6-22, epävakaat poistettu):",
    nrow(paivalla), "\n")
cat("Niistä täysin tyhjiä:", sum(paivalla$ajoneuvoja == 0),
    sprintf("(%.3f %%)\n", 100 * mean(paivalla$ajoneuvoja == 0)))

# --- Hiljaisuustapahtumat ----------------------------------------------------

viiva("2. HILJAISUUSTAPAHTUMAT")

# Pisteen oma normaalitaso: aikavälin mediaani samaan tuntiin kaikkina
# päivinä. Tämä on vertailukohta, johon poikkeama suhteutetaan.
taso <- paivalla |>
  filter(!is.na(max_vali_s)) |>
  mutate(tunti = minuutti_vrk %/% 60) |>
  group_by(lam_id, suunta, tunti) |>
  summarise(taso_vali = median(max_vali_s), .groups = "drop")

hiljaisuudet <- paivalla |>
  mutate(tunti = minuutti_vrk %/% 60) |>
  left_join(taso, by = c("lam_id", "suunta", "tunti")) |>
  filter(!is.na(max_vali_s), !is.na(taso_vali), taso_vali > 0) |>
  mutate(kerroin = max_vali_s / taso_vali) |>
  filter(max_vali_s >= HILJAISUUS_S, kerroin >= HILJAISUUS_KERROIN)

cat(sprintf("Ehto: aikaväli >= %d s JA >= %d x pisteen oma taso\n",
            HILJAISUUS_S, HILJAISUUS_KERROIN))
cat("Hiljaisuustapahtumia yhteensä:", nrow(hiljaisuudet), "\n\n")

osuudet <- hiljaisuudet |>
  count(pvm, tapahtuma, on_tapahtuma, name = "tapahtumia") |>
  left_join(paivalla |> count(pvm, name = "minuutteja"), by = "pvm") |>
  mutate(per_1000_min = 1000 * tapahtumia / minuutteja) |>
  arrange(desc(on_tapahtuma), pvm)

print(as.data.frame(osuudet))

ryhmat <- osuudet |>
  group_by(on_tapahtuma) |>
  summarise(paivia = n(),
            tapahtumia = sum(tapahtumia),
            minuutteja = sum(minuutteja),
            per_1000 = 1000 * sum(tapahtumia) / sum(minuutteja),
            .groups = "drop")

cat("\n")
print(as.data.frame(ryhmat))

# Tilastollinen testi. Vertailtavana on kahdeksan verrokkipäivää ja neljä
# tapahtumapäivää, ja havaintoyksikkö on PÄIVÄ eikä minuutti, koska saman
# päivän minuutit eivät ole toisistaan riippumattomia. Wilcoxonin
# järjestyssummatesti, koska päiviä on vähän eikä normaalisuutta voi
# tarkistaa.
testi <- wilcox.test(per_1000_min ~ on_tapahtuma, data = osuudet,
                     exact = FALSE)

z_arvo <- qnorm(testi$p.value / 2, lower.tail = FALSE)
r_efekti <- z_arvo / sqrt(nrow(osuudet))

cat(sprintf("\nWilcoxon: W = %.0f, p = %.3f, vaikutuskoko r = %.2f\n",
            testi$statistic, testi$p.value, r_efekti))
cat("Havaintoyksikkö on päivä, ei minuutti.\n")

# --- Etenemistesti -----------------------------------------------------------

viiva("3. ETENEMISTESTI: LIIKKUUKO HILJAISUUS")

# Saattue liikkuu. Työmaa ja onnettomuus eivät. Jos hiljaisuus on
# saattueen aiheuttama, sen pitää ilmestyä peräkkäisiin pisteisiin
# ajallisessa järjestyksessä ja nopeudella, joka on autolle uskottava.

sijainnit <- asemat |>
  st_drop_geometry() |>
  select(lam_id, nimi, tie = tienumero_lopullinen) |>
  bind_cols(st_coordinates(st_transform(asemat, 3067)) |>
              as_tibble() |> rename(x = X, y = Y))

etaisyys_m <- function(a, b) {
  pa <- sijainnit[match(a, sijainnit$lam_id), c("x", "y")]
  pb <- sijainnit[match(b, sijainnit$lam_id), c("x", "y")]
  sqrt((pa$x - pb$x)^2 + (pa$y - pb$y)^2)
}

# Kaikki hiljaisuusparit saman päivän ja saman suunnan sisällä,
# enintään 20 minuutin välein.
parit <- hiljaisuudet |>
  select(pvm, on_tapahtuma, suunta, lam_id, minuutti_vrk) |>
  inner_join(hiljaisuudet |>
               select(pvm, suunta, lam_id2 = lam_id,
                      minuutti2 = minuutti_vrk),
             by = c("pvm", "suunta"),
             relationship = "many-to-many") |>
  filter(lam_id < lam_id2) |>
  mutate(dt_min = abs(minuutti2 - minuutti_vrk)) |>
  filter(dt_min >= 1, dt_min <= 20)

if (nrow(parit) > 0) {
  parit <- parit |>
    mutate(
      dist_km = etaisyys_m(lam_id, lam_id2) / 1000,
      nopeus_kmh = dist_km / (dt_min / 60)
    ) |>
    filter(dist_km >= 1, dist_km <= 25)
  
  # Saattueelle uskottava nopeus: 40-120 km/h.
  parit <- parit |> mutate(uskottava = nopeus_kmh >= 40 & nopeus_kmh <= 120)
  
  prop_yht <- parit |>
    group_by(on_tapahtuma) |>
    summarise(pareja = n(),
              uskottavia = sum(uskottava),
              osuus = 100 * mean(uskottava),
              .groups = "drop")
  
  cat("Hiljaisuuspareja saman päivän ja suunnan sisällä:\n")
  print(as.data.frame(prop_yht))
  
  cat("\nUskottavalla saattuenopeudella etenevät parit",
      "tapahtumapäivinä:\n")
  print(as.data.frame(
    parit |>
      filter(on_tapahtuma, uskottava) |>
      left_join(sijainnit |> select(lam_id, nimi, tie), by = "lam_id") |>
      left_join(sijainnit |> select(lam_id2 = lam_id, nimi2 = nimi,
                                    tie2 = tie), by = "lam_id2") |>
      mutate(kello = sprintf("%02d:%02d", minuutti_vrk %/% 60,
                             minuutti_vrk %% 60),
             across(c(dist_km, nopeus_kmh), \(x) round(x, 1))) |>
      arrange(pvm, minuutti_vrk) |>
      select(pvm, kello, suunta, nimi, nimi2, tie, tie2,
             dt_min, dist_km, nopeus_kmh) |>
      head(40)))
  
  # Kolmen pisteen ketjut ovat ratkaiseva näyttö: kaksi pistettä voi
  # osua sattumalta, kolme peräkkäin samalla tiellä ei yhtä helposti.
  ketjut <- hiljaisuudet |>
    left_join(sijainnit |> select(lam_id, tie), by = "lam_id") |>
    filter(!is.na(tie)) |>
    group_by(pvm, on_tapahtuma, tie, suunta) |>
    summarise(pisteita = n_distinct(lam_id),
              minuutteja = n_distinct(minuutti_vrk),
              aikahaarukka = max(minuutti_vrk) - min(minuutti_vrk),
              .groups = "drop") |>
    filter(pisteita >= 3, aikahaarukka <= 20)
  
  cat("\nKolmen tai useamman pisteen ketjut samalla tiellä",
      "ja suunnassa 20 min sisällä:\n")
  if (nrow(ketjut) > 0) print(as.data.frame(ketjut)) else
    cat("  ei yhtään, ei tapahtuma- eikä verrokkipäivinä\n")
  
} else {
  cat("Hiljaisuuspareja ei löytynyt lainkaan.\n")
}

# --- Mita ilmaisin loytaa --------------------------------------------------

viiva("4. MITKA PISTEET DOMINOIVAT")

print(as.data.frame(
  hiljaisuudet |>
    count(lam_id, on_tapahtuma, name = "tapahtumia") |>
    pivot_wider(names_from = on_tapahtuma, values_from = tapahtumia,
                values_fill = 0,
                names_prefix = "tapahtumapaiva_") |>
    left_join(sijainnit |> select(lam_id, nimi, tie), by = "lam_id") |>
    arrange(desc(tapahtumapaiva_TRUE + tapahtumapaiva_FALSE)) |>
    head(20)))

viiva("5. HILJAISUUKSIEN KELLONAIKAJAKAUMA")

print(as.data.frame(
  hiljaisuudet |>
    mutate(tunti = minuutti_vrk %/% 60) |>
    count(on_tapahtuma, tunti) |>
    pivot_wider(names_from = on_tapahtuma, values_from = n,
                values_fill = 0, names_prefix = "tapahtuma_") |>
    arrange(tunti)))

qd_save(hiljaisuudet, file.path(DATA_DIR, "hiljaisuudet.qs2"))

viiva("VALMIS")
cat("Olennaisia: jakso 2 (erottuuko), jakso 3 (liikkuuko), jakso 4 (mika dominoi).\n")