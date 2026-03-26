library(tidyverse)
library(janitor)
library(countrycode)

dir.create(
  "data/processed",
  recursive = TRUE,
  showWarnings = FALSE
)

raw <-
  
  read_csv(
    "data/raw/all_billionaires_1997_2024.csv",
    show_col_types = FALSE
  )

billionaires <-
  
  raw |>
  
  clean_names() |>
  
  mutate(
    
    year =
      as.integer(year),
    
    age =
      as.numeric(age),
    
    net_worth_usd =
      readr::parse_number(net_worth) * 1e9,
    
    country_citizenship =
      country_of_citizenship,
    
    country_residence =
      country_of_residence,
    
    country_citizenship_clean =
      
      str_split(
        country_of_citizenship,
        ",| and |/"
      ) |>
      
      map_chr(1) |>
      
      str_trim(),
    
    country_residence_clean =
      
      str_split(
        country_of_residence,
        ",| and |/"
      ) |>
      
      map_chr(1) |>
      
      str_trim(),
    
    country_citizenship_iso3 =
      
      countrycode(
        country_citizenship_clean,
        "country.name",
        "iso3c"
      ),
    
    country_residence_iso3 =
      
      countrycode(
        country_residence_clean,
        "country.name",
        "iso3c"
      ),
    
    self_made =
      case_when(
        
        self_made %in%
          c("True", "TRUE", TRUE, 1) ~ TRUE,
        
        self_made %in%
          c("False", "FALSE", FALSE, 0) ~ FALSE,
        
        TRUE ~ NA
        
      ),
    
    gender =
      str_to_lower(gender),
    
    birth_date =
      as.Date(birth_date)
    
  ) |>
  
  select(
    
    # time
    year,
    month,
    
    # identity
    full_name,
    first_name,
    last_name,
    gender,
    birth_date,
    age,
    
    # geography
    country_citizenship,
    country_citizenship_iso3,
    
    country_residence,
    country_residence_iso3,
    
    city_of_residence,
    
    # wealth
    net_worth_usd,
    wealth_status,
    self_made,
    
    # business
    business_category,
    business_industries,
    
    organization_name,
    position_in_organization,
    
    # ranking
    rank
    
  )

write_csv(
  
  billionaires,
  
  "data/processed/billionaires_clean.csv"
  
)

print("clean data saved")