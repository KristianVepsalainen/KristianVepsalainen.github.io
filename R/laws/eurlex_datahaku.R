tyypit <- c("regulation", "directive", "decision", "recommendation")

hae_tyyppi <- function(tyyppi) {
  message("Haetaan: ", tyyppi)
  tulos <- elx_make_query(
    resource_type = tyyppi,
    include_date  = TRUE,
    include_force = TRUE,
    include_celex = TRUE
  ) |> elx_run_query()
  tulos |> mutate(resource_type = tyyppi)
}

raw <- map(tyypit, hae_tyyppi) |> bind_rows()
dir.create(dirname(data_path), showWarnings = FALSE, recursive = TRUE)
saveRDS(raw, data_path)