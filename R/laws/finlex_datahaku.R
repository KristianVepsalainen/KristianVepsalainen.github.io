# Kaikkien säädösten haku Semanttisesta Finlexistä
# Sivutus LIMIT/OFFSET:lla, vuosi poimitaan numero-kentästä

library(httr)
library(jsonlite)
library(dplyr)

ENDPOINT <- "http://ldf.fi/finlex/sparql"

kysy_sivu <- function(limit, offset) {
  query <- sprintf("
PREFIX eli: <http://data.europa.eu/eli/ontology#>
PREFIX sfl: <http://data.finlex.fi/schema/sfl/>

SELECT ?laki ?numero ?doc_type ?statute_type
WHERE {
  ?laki a sfl:Statute ;
        eli:id_local ?numero .
  OPTIONAL { ?laki eli:type_document  ?doc_type . }
  OPTIONAL { ?laki sfl:statuteType    ?statute_type . }
}
LIMIT %d OFFSET %d
", limit, offset)
  
  resp <- GET(
    ENDPOINT,
    query  = list(query = query),
    add_headers(Accept = "application/sparql-results+json"),
    timeout(30)
  )
  stop_for_status(resp)
  fromJSON(rawToChar(resp$content), simplifyDataFrame = TRUE)$results$bindings
}

# Sivutus: 500 riviä kerrallaan
LIMIT   <- 500
sivut   <- list()
offset  <- 0
i       <- 1

repeat {
  cat(sprintf("Haetaan offset %d ...\n", offset))
  sivu <- tryCatch(
    kysy_sivu(LIMIT, offset),
    error = function(e) { cat("Virhe:", conditionMessage(e), "\n"); NULL }
  )
  
  # Lopeta jos tyhjä tai virhe
  if (is.null(sivu) || nrow(sivu) == 0) break
  
  sivut[[i]] <- sivu
  i      <- i + 1
  offset <- offset + LIMIT
  
  # Lopeta jos viimeinen sivu oli vajaa
  if (nrow(sivu) < LIMIT) break
  
  Sys.sleep(0.5)
}

# Yhdistä sivut
raw <- bind_rows(sivut)
cat(sprintf("\nHaettu yhteensä %d säädöstä\n", nrow(raw)))

# Siisti: poimi vuosi numero-kentästä (muoto "63/1996")
df <- data.frame(
  uri          = raw$laki$value,
  numero       = raw$numero$value,
  doc_type     = sub(".*/", "", raw$doc_type$value),
  statute_type = sub(".*/", "", raw$statute_type$value),
  stringsAsFactors = FALSE
)

# Poimi vuosi numero-kentästä ("63/1996" -> 1996)
df$vuosi <- as.integer(sub(".*/", "", df$numero))

cat("Rivejä:", nrow(df), "\n")
glimpse(df)

cat("\ndoc_type:\n")
print(sort(table(df$doc_type), decreasing = TRUE))

cat("\nstatute_type:\n")
print(sort(table(df$statute_type), decreasing = TRUE))

cat("\nVuosijakauma (eniten):\n")
print(sort(table(df$vuosi), decreasing = TRUE)[1:10])

print(count(df, vuosi, sort = TRUE) |> head(10))

# Tallenna
saveRDS(df, paste0(getwd(),"/data/finlex/finlex_saadokset.rds"))
write.csv(df, paste0(getwd(),"/data/finlex/finlex_saadokset.csv", row.names = FALSE))
cat("\nTallennettu: finlex_saadokset.rds ja finlex_saadokset.csv\n")