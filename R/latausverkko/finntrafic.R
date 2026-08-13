nap_path <- file.path(data_dir, "nap_raw.qs")

afir <- function(polku, cursor = NULL) {
  req <- request("https://afir.digitraffic.fi") |>
    req_url_path_append(polku) |>
    req_headers(`Digitraffic-User` = "kristianvepsalainen.com/latausverkko") |>
    req_retry(max_tries = 3)
  if (!is.null(cursor)) req <- req_url_query(req, cursor = cursor)
  resp <- req_perform(req)
  stopifnot("AFIR palautti muun kuin 200" = resp_status(resp) == 200)
  resp_body_json(resp)
}

hae_kaikki <- function(polku, max_sivuja = 200) {
  sivut <- list(); cursor <- NULL; i <- 0
  repeat {
    i <- i + 1
    stopifnot("Sivutus ei pääty – tarkista nextCursor" = i <= max_sivuja)
    v <- afir(polku, cursor)
    sivut[[i]] <- v
    cursor <- purrr::pluck(v, "pagination", "nextCursor", .default = NULL)
    if (is.null(cursor)) break
  }
  message("Sivuja haettu: ", i)
  sivut
}

if (!file.exists(nap_path)) {
  operaattorit_raw <- afir("/api/charging-network/v1/operators")
  sijainnit_sivut  <- hae_kaikki("/api/charging-network/v1/locations")
  qs2::qs_save(list(operaattorit = operaattorit_raw, sijainnit = sijainnit_sivut),
               nap_path)
} else {
  nap_raw <- qs2::qs_read(nap_path)
}