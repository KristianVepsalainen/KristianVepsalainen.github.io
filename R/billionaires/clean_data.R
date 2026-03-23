library(tidyverse)
library(janitor)
library(countrycode)

clean_billionaires <- function(df) {
  
  df |>
    clean_names() |>
    
    mutate(
      year = as.integer(year),
      
      net_worth_usd = parse_number(net_worth) * 1e9,
      
      country_iso3 = countrycode(
        country,
        origin = "country.name",
        destination = "iso3c"
      ),
      
      self_made = case_when(
        self_made %in% c("True", "TRUE", 1) ~ TRUE,
        TRUE ~ FALSE
      )
    ) |>
    
    select(
      year,
      name,
      country,
      country_iso3,
      industry,
      company,
      net_worth_usd,
      self_made,
      rank
    )
}