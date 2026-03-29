library(data.table)
library(tidyverse)
library(furrr)
library(future)
library(lubridate)
library(fst)
library(here)
gdelt_names <- c(
  "GlobalEventID","SQLDATE","MonthYear","Year","FractionDate",
  "Actor1Code","Actor1Name","Actor1CountryCode","Actor1KnownGroupCode",
  "Actor1EthnicCode","Actor1Religion1Code","Actor1Religion2Code",
  "Actor1Type1Code","Actor1Type2Code","Actor1Type3Code",
  "Actor2Code","Actor2Name","Actor2CountryCode","Actor2KnownGroupCode",
  "Actor2EthnicCode","Actor2Religion1Code","Actor2Religion2Code",
  "Actor2Type1Code","Actor2Type2Code","Actor2Type3Code",
  "IsRootEvent","EventCode","EventBaseCode","EventRootCode",
  "QuadClass","GoldsteinScale","NumMentions","NumSources",
  "NumArticles","AvgTone","Actor1Geo_Type","Actor1Geo_FullName",
  "Actor1Geo_CountryCode","Actor1Geo_ADM1Code","Actor1Geo_Lat",
  "Actor1Geo_Long","Actor1Geo_FeatureID","Actor2Geo_Type",
  "Actor2Geo_FullName","Actor2Geo_CountryCode","Actor2Geo_ADM1Code",
  "Actor2Geo_Lat","Actor2Geo_Long","Actor2Geo_FeatureID",
  "ActionGeo_Type","ActionGeo_FullName","ActionGeo_CountryCode",
  "ActionGeo_ADM1Code","ActionGeo_Lat","ActionGeo_Long",
  "ActionGeo_FeatureID","DATEADDED","SOURCEURL"
)

read_gdelt_day <- function(date){
  
  ymd <- format(date,"%Y%m%d")
  
  url <- paste0(
    "http://data.gdeltproject.org/events/",
    ymd,
    ".export.CSV.zip"
  )
  
  tryCatch({
    
    dt <- fread(
      url,
      header = FALSE,
      showProgress = FALSE
    )
    
    setnames(dt, gdelt_names)
    
    dt[,
       
       .(
         SQLDATE,
         EventCode,
         
         ActionGeo_CountryCode,
         ActionGeo_Lat,
         ActionGeo_Long,
         
         NumMentions,
         NumSources,
         NumArticles,
         
         AvgTone
       )
       
    ]
    
  },
  error = function(e) NULL)
  
}


plan(multisession, workers = 6)

dates <-
  
  seq.Date(
    
    as.Date("2023-01-01"),
    as.Date("2026-03-28"),
    
    by = "day"
    
  )

gdelt_raw <-
  
  future_map_dfr(
    
    dates,
    
    read_gdelt_day,
    
    .progress = TRUE
    
  )

europe_iso <-
  
  c(
    "FI","SE","NO","DK","EE","LV","LT",
    "DE","FR","ES","PT","IT","NL","BE",
    "AT","CH","PL","CZ","SK","HU",
    "SI","HR","RO","BG","GR",
    "IE","GB","UA"
  )

protests_europe <-
  
  gdelt_raw |>
  
  filter(
    
    substr(EventCode,1,2) == "14",
    
    ActionGeo_CountryCode %in%
      
      europe_iso
    
  ) |>
  
  transmute(
    
    event_date = lubridate::ymd(SQLDATE),
    
    country = ActionGeo_CountryCode,
    
    latitude = ActionGeo_Lat,
    
    longitude = ActionGeo_Long,
    
    mentions = NumMentions,
    
    sources = NumSources,
    
    articles = NumArticles,
    
    tone = AvgTone
    
  ) |>
  
  filter(
    
    !is.na(latitude)
    
  )

fst::write_fst(protests_europe, here("data","processed","protest.fst"),100) #maksimipakkaus


protests_europe |>
  
  count(year(event_date))

protests_europe |>
  
  summarise(
    
    n = n(),
    
    countries = n_distinct(country)
    
  )
