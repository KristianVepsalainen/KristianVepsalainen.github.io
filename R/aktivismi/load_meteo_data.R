library(tidyverse)
library(lubridate)
library(ecmwfr)
library(data.table)
library(furrr)
library(sf)
library(terra)
library(fst)
library(here)


wf_set_key(
  user = "cds",
  key = keyring::key_get("ecmwfr")
)

#Vaihe 1 – bounding box protestidatasta: ei haeta turhaan koko Eurooppaa

protests <-
  read_fst(protests_europe, here("data","processed","protest.fst"))
bbox <-
  
  protests |>
  
  summarise(
    
    north = max(latitude) + 1,
    south = min(latitude) - 1,
    
    east = max(longitude) + 1,
    west = min(longitude) - 1
    
  )

#Vaihe 2 – ERA5 request koko periodille: testaus 2022-2024

request <- list(
  
  product_type = "reanalysis",
  
  variable = c(
    
    "2m_temperature",
    "total_precipitation",
    
    "10m_u_component_of_wind",
    "10m_v_component_of_wind"
    
  ),
  
  year = 2022:2024,
  
  month = sprintf("%02d",1:12),
  
  day = sprintf("%02d",1:31),
  
  time = "12:00",
  
  format = "netcdf",
  
  area = c(
    
    bbox$north,
    bbox$west,
    bbox$south,
    bbox$east
    
  )
  
)

wf_request(
  
  user = "cds",
  
  dataset =
    "reanalysis-era5-single-levels",
  
  request = request,
  
  path =
    "data/era5_europe.nc"
  
)

#vaihe 3 - raster -> tidy dataframe

library(terra)
library(tidyverse)

weather <-
  
  rast(
    "data/era5_europe.nc"
  )

weather_df <-
  
  as.data.frame(
    
    weather,
    
    xy = TRUE,
    
    time = TRUE
    
  ) |>
  
  rename(
    
    longitude = x,
    latitude = y
    
  )
#Vaihe 4 - tuulen nopeus

weather_df <-
  
  weather_df |>
  
  mutate(
    
    wind = sqrt(
      
      u10^2 +
        v10^2
      
    ),
    
    temp =
      
      t2m - 273.15,
    
    rain =
      
      tp * 1000
    
  )

#Huom! Yksiköt - lämpötila: Kelvin, tp: metriä vettä, wind: m/s

#Vaihe 5 - liitetään protesteihin: koordinaatit eivät identtiset -> käytetään lähintä

library(sf)

weather_sf <-
  
  st_as_sf(
    
    weather_df,
    
    coords =
      c(
        "longitude",
        "latitude"
      ),
    
    crs = 4326
    
  )

protests_sf <-
  
  st_as_sf(
    
    protests,
    
    coords =
      c(
        "longitude",
        "latitude"
      ),
    
    crs = 4326
    
  )

joined <-
  
  st_join(
    
    protests_sf,
    
    weather_sf,
    
    join =
      st_nearest_feature
    
  )

#Sanity check - tästä pitäisi tulla lähelle nolla

joined |>
  
  summarise(
    
    missing_weather =
      
      mean(is.na(temp))
    
  )