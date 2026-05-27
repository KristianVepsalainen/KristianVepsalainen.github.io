# Säädösten haku Finlexin REST-rajapinnasta
# Endpoint: https://opendata.finlex.fi/finlex/avoindata/v1/
# Strategia: hae vuosi kerrallaan per kategoria, ilman typeStatute-rajausta

library(httr)
library(jsonlite)
library(dplyr)
library(fst)

BASE_URL  <- "https://opendata.finlex.fi/finlex/avoindata/v1/akn/fi/act/statute/list"
UA        <- "FinlexAnalyysi/1.0 (kristian.vepsalainen@proton.me)"
LIMIT     <- 10
VUOSI_ALO <- 1987
VUOSI_LOP <- as.integer(format(Sys.Date(), "%Y"))
KATEGORIAT <- c("new-statute", "amending-statute", "repealing-statute")

# ── Apufunktio: hae yksi sivu ─────────────────────────────────────────────────
hae_sivu <- function(vuosi, kategoria, page, max_yritykset = 5) {
  tauot <- c(5, 15, 30, 60, 120)
  
  for (yritys in seq_len(max_yritykset)) {
    resp <- tryCatch(
      GET(
        BASE_URL,
        query = list(
          format          = "json",
          page            = page,
          limit           = LIMIT,
          sortBy          = "dateIssued",
          startYear       = vuosi,
          endYear         = vuosi,
          categoryStatute = kategoria
        ),
        add_headers(`User-Agent` = UA, `Accept` = "application/json"),
        timeout(30)
      ),
      error = function(e) {
        cat(sprintf("    Verkkovirhe (yritys %d): %s\n", yritys, conditionMessage(e)))
        NULL
      }
    )
    
    if (is.null(resp)) {
      Sys.sleep(tauot[min(yritys, length(tauot))]); next
    }
    if (status_code(resp) == 200) {
      parsed <- fromJSON(rawToChar(resp$content))
      # Palauta vain fin@-versiot
      if (is.data.frame(parsed) && nrow(parsed) > 0) {
        parsed <- parsed[grepl("/fin@$", parsed$akn_uri), ]
      }
      return(parsed)
    } else if (status_code(resp) == 429) {
      cat(sprintf("    429 — odotetaan %ds\n", tauot[min(yritys, length(tauot))]))
      Sys.sleep(tauot[min(yritys, length(tauot))])
    } else {
      cat(sprintf("    HTTP %d — odotetaan %ds\n",
                  status_code(resp), tauot[min(yritys, length(tauot))]))
      Sys.sleep(tauot[min(yritys, length(tauot))])
    }
  }
  NULL
}

# ── Pääsilmukka: vuosi x kategoria ───────────────────────────────────────────
kaikki <- list()
idx    <- 1

for (vuosi in VUOSI_ALO:VUOSI_LOP) {
  for (kategoria in KATEGORIAT) {
    page <- 1
    repeat {
      tulos <- hae_sivu(vuosi, kategoria, page)
      
      # Lopeta jos tyhjä tai virhe
      if (is.null(tulos) || !is.data.frame(tulos) || nrow(tulos) == 0) break
      
      # Poimi vuosi ja numero URI:sta (.../statute/VUOSI/NUMERO/fin@)
      urit   <- tulos$akn_uri
      osat   <- regmatches(urit, regexpr("[0-9]{4}/[0-9]+/fin@$", urit))
      v      <- as.integer(sub("/.*", "", osat))
      n      <- as.integer(sub("[0-9]+/([0-9]+)/fin@", "\\1", osat))
      
      kaikki[[idx]] <- data.frame(
        uri          = urit,
        vuosi        = v,
        numero       = paste0(n, "/", v),
        statute_type = kategoria,
        stringsAsFactors = FALSE
      )
      idx <- idx + 1
      
      # Jatka vain jos täysi sivu (10 fin+swe = 5 fin, mutta voi olla epätasainen)
      # Turvallisempaa: jatka aina kunnes palautetaan tyhjä
      page <- page + 1
      Sys.sleep(0.3)
    }
  }
  if (vuosi %% 5 == 0) cat(sprintf("Vuosi %d valmis, rivejä yhteensä: %d\n",
                                   vuosi, sum(sapply(kaikki, nrow))))
}

# ── Yhdistä ja siisti ─────────────────────────────────────────────────────────
df <- bind_rows(kaikki) |>
  mutate(
    statute_type = recode(statute_type,
                          "new-statute"       = "NewStatute",
                          "amending-statute"  = "Amendment",
                          "repealing-statute" = "Repeal"
    )
  )

cat(sprintf("\nHaettu yhteensä %d säädöstä\n", nrow(df)))
cat("\nstatute_type:\n"); print(table(df$statute_type))
cat("\nvuosijakauma (viimeiset 10):\n")
print(tail(sort(table(df$vuosi)), 10))

dir.create("data", showWarnings = FALSE)
saveRDS(df,   "data/finlex_saadokset.rds")
write_fst(df, "data/finlex_saadokset.fst")
cat("\nTallennettu data/finlex_saadokset.rds ja .fst\n")