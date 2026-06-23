# R/theme_kristian.R
# Jaettu visuaalinen tyyli kaikille blogipostauksille.
# HUOM: Tämän tiedoston muuttaminen EI mitätöi Quarton freeze-välimuistia
# (toisin kuin _quarto.yml / _metadata.yml). Pidä silti vakaana.

suppressPackageStartupMessages({
  library(ggplot2)
})

# Vakioväripaletti -------------------------------------------------------------
kvar_palette <- c(
  red    = "#e63946",
  teal   = "#2a9d8f",
  orange = "#f4a261",
  navy   = "#1d3557",
  blue   = "#457b9d",
  dark   = "#0d1117"
)

# Diskreetti skaala kategorisille muuttujille
scale_color_kristian <- function(...) {
  ggplot2::scale_color_manual(
    values = unname(kvar_palette[c("red", "teal", "orange", "blue", "navy")]),
    ...
  )
}
scale_fill_kristian <- function(...) {
  ggplot2::scale_fill_manual(
    values = unname(kvar_palette[c("red", "teal", "orange", "blue", "navy")]),
    ...
  )
}

# Tumma teema -----------------------------------------------------------------
theme_kristian <- function(base_size = 14, base_family = "") {
  ggplot2::theme_minimal(base_size = base_size, base_family = base_family) +
    ggplot2::theme(
      plot.background    = ggplot2::element_rect(fill = kvar_palette[["dark"]], color = NA),
      panel.background   = ggplot2::element_rect(fill = kvar_palette[["dark"]], color = NA),
      panel.grid.major   = ggplot2::element_line(color = "#21262d", linewidth = 0.3),
      panel.grid.minor   = ggplot2::element_blank(),
      text               = ggplot2::element_text(color = "#c9d1d9"),
      axis.text          = ggplot2::element_text(color = "#8b949e"),
      plot.title         = ggplot2::element_text(face = "bold", size = base_size * 1.25,
                                                 color = "#f0f6fc"),
      plot.subtitle      = ggplot2::element_text(color = "#8b949e", margin = ggplot2::margin(b = 10)),
      plot.caption       = ggplot2::element_text(color = "#6e7681", size = base_size * 0.7),
      legend.position    = "top",
      legend.key         = ggplot2::element_blank(),
      plot.margin        = ggplot2::margin(15, 15, 15, 15)
    )
}

# Asetetaan oletukseksi koko istunnolle
ggplot2::theme_set(theme_kristian())

# Pieni apufunktio: euromääräinen akselimerkintä
fmt_euro <- function(x) {
  paste0(format(round(x), big.mark = " ", scientific = FALSE), " \u20AC")
}
