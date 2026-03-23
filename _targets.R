library(targets)
library(tarchetypes)
library(arrow)

source("R/billionaires/download_data.R")
source("R/billionaires/clean_data.R")

tar_option_set(
  packages = c(
    "tidyverse",
    "arrow",
    "janitor",
    "countrycode"
  )
)

list(
  
  tar_target(
    
    raw_billionaires,
    
    download_billionaires()
    
  ),
  
  tar_target(
    
    clean_billionaires,
    
    clean_billionaires(raw_billionaires)
    
  ),
  
  tar_target(
    
    parquet_file,
    
    {
      dir.create("data/processed",
                 recursive = TRUE,
                 showWarnings = FALSE)
      
      arrow::write_parquet(
        clean_billionaires,
        "data/processed/billionaires.parquet"
      )
      
      "data/processed/billionaires.parquet"
    },
    
    format = "file"
    
  )
  
)