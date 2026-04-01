library(tidyverse)
library(lubridate)
library(ecmwfr)
library(data.table)
library(furrr)
library(sf)
library(terra)
library(fst)
library(here)
library(stringr)



#Vaihe 1 - bounding box protestidatasta: ei haeta turhaan koko Eurooppaa

protests <-
  read_fst(here("data","processed","protest.fst")) |>
  
  mutate(
    
    date =
      as.Date(event_date)
    
  )


#Vaihe 2 – lue ERA5 rasterit (ei dataframeksi!)

instant <-
  
  rast(
    here(
      "data/raw/era5/data_stream-oper_stepType-instant.nc"
    )
  )

accum <-
  
  rast(
    here(
      "data/raw/era5/data_stream-oper_stepType-accum.nc"
    )
  )

weather <-
  
  c(
    instant,
    accum
  )

#Vaihe 3 – parsitaan layer metadata ilman pivotointia

layer_info <-
  
  data.table(
    layer =
      names(weather)
  ) |>
  
  mutate(
    
    variable =
      sub(
        "_valid_time=.*",
        "",
        layer
      ),
    
    time =
      as.POSIXct(
        as.numeric(
          sub(
            ".*=",
            "",
            layer
          )
        ),
        origin = "1970-01-01",
        tz = "UTC"
      ),
    
    date =
      as.Date(time)
    
  )

#Vaihe 4 – pidetään vain tarvittavat päivät

needed_dates <-
  unique(
    as.Date(
      protests$event_date
    )
  )

keep_layers <-
  
  layer_info |>
  
  filter(
    date %in%
      needed_dates
  )

# Vaihe 5 – valitaan rasterista vain tarvittavat layerit
weather_small <-
  
  weather[[
    keep_layers$layer
  ]]

# Vaihe 6 – protestipisteet SpatVectoriksi
points <-
  
  terra::vect(
    
    protests |>
      
      select(
        longitude,
        latitude
      ),
    
    geom =
      c(
        "longitude",
        "latitude"
      ),
    
    crs = "EPSG:4326"
    
  )

values <-
  
  terra::extract(
    weather_small,
    points,
    ID = FALSE
  )

#Vaihe 7 – nimetään srakkeet oikein

colnames(values) <-
  
  keep_layers$layer

# Vaihe 8 - yhdistetään protesteihin

weather_wide <-
  
  cbind(
    protests,
    values
  )

#Vaihe 9 - Valitaan oikea päivä per prostesti

get_weather_for_row <- function(date, row_values, info){
  
  idx <-
    info$date == date
  
  vars <-
    info$variable[idx]
  
  vals <-
    as.numeric(
      row_values[idx]
    )
  
  out <-
    setNames(
      vals,
      vars
    )
  
  return(out)
  
}

#Vaihe 9 - rakennetaan lopullinen dataset

weather_final <-
  
  map_dfr(
    
    1:nrow(weather_wide),
    
    function(i){
      
      w <-
        
        get_weather_for_row(
          
          as.Date(
            weather_wide$event_date[i]
          ),
          
          weather_wide[i, keep_layers$layer],
          
          keep_layers
          
        )
      
      tibble(
        
        event_id =
          weather_wide$event_id[i],
        
        temp =
          w["t2m"] - 273.15,
        
        rain =
          w["tp"] * 1000,
        
        wind =
          sqrt(
            w["u10"]^2 +
              w["v10"]^2
          )
        
      )
      
    }
    
  )

#Vaihe 10 - yhdistetään takaisin protestidataan

protests_weather <-
  
  bind_cols(
    
    protests,
    
    weather_final |>
      
      select(
        temp,
        rain,
        wind
      )
    
  )

#Vaihe 11 - määritellään hyvä sää

protests_weather <-
  
  protests_weather |>
  
  mutate(
    
    good_weather =
      
      temp > 0 &
      rain < 1 &
      wind < 8
    
  )

#vaih 12 - kirjoitetaan

write_fst(
  
  protests_weather,
  
  here(
    "data/derived/protests_weather.fst"
  )
  
)

#Sanity chek

summary(
    protests_weather$temp)

protests_weather |>
  
  summarise(
    
    share_missing =
      mean(is.na(temp)),
    
    n_missing =
      sum(is.na(temp))
    
  )

protests_weather |>
  
  filter(
    is.na(temp)
  ) |>
  
  count(country)
#ongelmia erityisesti svetisissä
protests_weather |>
  
  filter(
    country == "CH"
  ) |>
  
  summarise(
    
    share_missing =
      mean(is.na(temp))
    
  )

range(needed_dates)
range(layer_info$date)