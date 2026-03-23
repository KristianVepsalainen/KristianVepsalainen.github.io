library(tidyverse)

download_billionaires <- function() {
  
  url <- "https://raw.githubusercontent.com/guillemservera/forbes-billionaires/main/data/billionaires.csv"
  
  read_csv(url, show_col_types = FALSE)
}