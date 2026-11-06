# =============================================================================
# R/hilma/hilma-io.R
# Tallennus ja luku niin, ettei yksikään tiedosto ylitä GitHubin rajaa.
#
# GitHub hylkää yli 100 Mt tiedostot (ja varoittaa yli 50 Mt:sta).
# save_split() tallentaa taulun qs2:n pakkaustasolla 9 (taso 22 on moninkertaisesti hitaampi ja vain vähän pienempi). Jos tulos on
# yli 45 Mt, taulu pilkotaan riveittäin tasakokoisiin osiin:
#   <nimi>__part01.qs2, <nimi>__part02.qs2, ...
# read_split() lukee joko yhden tiedoston tai kaikki osat ja yhdistää ne.
# exists_split() kertoo, onko taulu jo tallennettu jommassakummassa muodossa.
# =============================================================================

SPLIT_LIMIT_MB <- 45     # alle GitHubin 50 Mt:n varoitusrajan

.split_files <- function(dir, name) {
  list.files(dir, pattern = paste0("^", name, "(__part\\d{2})?\\.qs2$"), full.names = TRUE)
}

exists_split <- function(dir, name) length(.split_files(dir, name)) > 0

# Tallennus tehdään VÄLIAIKAISEEN kansioon ja siirretään paikalleen vasta,
# kun kaikki osat on kirjoitettu ja luettu onnistuneesti takaisin. Näin
# keskeytys (Ctrl+C, kaatuminen) ei voi koskaan jättää rikkinäistä tai
# vajaata tiedostoa, eikä vanha versio katoa ennen kuin uusi on valmis.
save_split <- function(df, dir, name) {
  stopifnot(is.data.frame(df), dir.exists(dir))
  tmp <- file.path(dir, paste0(".tmp_", name))
  unlink(tmp, recursive = TRUE); dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  f <- file.path(tmp, paste0(name, ".qs2"))
  qs2::qd_save(df, f, compress_level = 9L)
  mb <- file.size(f) / 1e6
  files <- f
  if (mb > SPLIT_LIMIT_MB && nrow(df) >= 2) {
    file.remove(f)
    k <- ceiling(mb / (0.8 * SPLIT_LIMIT_MB))          # varmuusvara osakokoon
    stopifnot(k <= 99)
    grp <- cut(seq_len(nrow(df)), breaks = k, labels = FALSE)
    files <- character()
    for (j in seq_len(k)) {
      fj <- file.path(tmp, sprintf("%s__part%02d.qs2", name, j))
      qs2::qd_save(df[grp == j, , drop = FALSE], fj, compress_level = 9L)
      if (file.size(fj) / 1e6 > 95) stop(basename(fj), " on yli 95 Mt.")
      files <- c(files, fj)
    }
  }

  # Varmistus: luetaan takaisin ja verrataan rivimäärää
  n_back <- sum(vapply(files, \(x) nrow(qs2::qd_read(x)), integer(1)))
  stopifnot(n_back == nrow(df))

  # Vasta nyt vanhat pois ja uudet paikalleen
  file.remove(.split_files(dir, name))
  ok <- file.rename(files, file.path(dir, basename(files)))
  stopifnot(all(ok))
  message(sprintf("%s: %.1f Mt, %d tiedosto(a)", name, mb, length(files)))
  invisible(file.path(dir, basename(files)))
}

read_split <- function(dir, name) {
  files <- sort(.split_files(dir, name))
  if (length(files) == 0) stop("Taulua ", name, " ei löydy kansiosta ", dir)
  dplyr::bind_rows(lapply(files, qs2::qd_read))
}

# Etsii kansiosta kaikki yli rajan olevat tiedostot (tarkistus ennen pushia)
find_big_files <- function(dir, limit_mb = 95) {
  f <- list.files(dir, recursive = TRUE, full.names = TRUE, all.files = TRUE)
  sz <- file.size(f) / 1e6
  out <- data.frame(file = f[sz > limit_mb], mb = round(sz[sz > limit_mb], 1))
  out[order(-out$mb), , drop = FALSE]
}
