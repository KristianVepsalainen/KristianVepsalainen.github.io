library(tidyverse)
library(rvest)
library(janitor)
library(countrycode)

dir.create(
  "data/processed",
  recursive = TRUE,
  showWarnings = FALSE
)

########################
# billionaire data
########################

url <-
  "https://en.wikipedia.org/wiki/The_World%27s_Billionaires"

tables <-
  read_html(url) |>
  html_table()

# Forbes julkaisee uusimman listan sivulla ensimmäisenä isona taulukkona
billionaires_raw <-
  tables[[2]]

billionaires <-
  
  billionaires_raw |>
  
  clean_names() |>
  
  rename(
    
    name = name,
    net_worth = net_worth_usd,
    country = citizenship
    
  ) |>
  
  mutate(
    
    year = 2024,
    
    net_worth_usd =
      readr::parse_number(net_worth) * 1e9,
    
    country_iso3 =
      countrycode(
        country,
        "country.name",
        "iso3c"
      )
    
  ) |>
  
  select(
    
    year,
    name,
    country,
    country_iso3,
    net_worth_usd
    
  )

write_csv(
  
  billionaires,
  
  "data/processed/billionaires.csv"
  
)

########################
# GDP
########################

gdp <-
  
  read_csv(
    
    "https://raw.githubusercontent.com/datasets/gdp/master/data/gdp.csv",
    
    show_col_types = FALSE
    
  ) |>
  
  rename(
    
    country = Country_Name,
    year = Year,
    gdp = Value
    
  ) |>
  
  mutate(
    
    country_iso3 =
      countrycode(
        country,
        "country.name",
        "iso3c"
      )
    
  ) |>
  
  select(
    
    country_iso3,
    year,
    gdp
    
  )

write_csv(
  
  gdp,
  
  "data/processed/gdp.csv"
  
)

print("data downloaded")