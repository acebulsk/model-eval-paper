# Script to make HOURLY CRHM met obs for Wolf Creek
# DATA INPUT: Rasouli, K., Pomeroy, J., Janowicz, J., Williams, T., Carey, S. (2018). Hydrometeorological data collected at Wolf Creek Research Basin, Yukon Territory, Canada over 1993-2014. Federated Research Data Repository. https://doi.org/10.20383/101.0113
# Above has hourly temp, rh, wind gap filled, precip gap fill is at daily resolution

library(tidyverse)
library(CRHMr)
library(weathercan)

# SETUP ----

tz <- 'Etc/GMT+7' # tz not listed on metadata so assuming LST

dt_test <- 737852 # march 2 2020
matlab_origin <- 719529 # 0000-01-00 not compatible with R's origin so we force it here
posix_date <- as.POSIXct((dt_test - matlab_origin)*86400, origin = "1970-01-01", tz = "UTC")

# LOAD DATA ----

## Newer data from Gord ----

# 
datetimes <- read_table('data/wolf-creek/met/Wolf_Creek_Forest_2021_Clean/Clean/TV.dat',
                               col_names = 'matlabtime') |>
  mutate(datetime = as.POSIXct((matlabtime - matlab_origin) * 86400,
                               origin = "1970-01-01",
                               tz = "UTC"
  ))

at <- read_table('data/wolf-creek/met/Wolf_Creek_Forest_2021_Clean/Clean/Air_Temp_2m_Avg.dat',
                 col_names = 't', na = 'NaN')

## Rasouli2019


met <- read_csv('data/wolf-creek/met/kabir2019/Meteorology/Forest.csv') |> 
  mutate(datetime = as.POSIXct(Date, format = '%m/%d/%Y %H:%M', tz = tz)) |> 
  select(datetime,
         t = AirTempLow_C_Filled,
         rh = RelHumidityLow_percent_Filled,
         u = WSpdAvgAbvCnpy_ms_1_Filled,
         swi = `IncomingShortwave_Wm-2`
         )

# WARNING: AlpineForestShrubTundra_precipitation_mm_Gaps_Filled.csv is DAILY
precip <- read_csv('data/wolf-creek/met/kabir2019/Precipitation/Forest_Precip.Rain_mm.csv') |> 
  mutate(datetime = as.POSIXct(Date, format = '%d/%m/%Y %H:%M', tz = tz)) |> 
  select(datetime, ppt = Value)

obs <- left_join(met, precip)

# ECCC Stations

stns <- weathercan::stations() |> filter(station_name %in% c('WHITEHORSE A',
                                                            'WHITEHORSE AUTO'),
                                        interval == 'hour')

# WHITEHORSE A, 
wha <- weathercan::weather_dl(stns$station_id, start = '1990-01-01', interval = 'hour')
saveRDS(wha, 'data/wolf-creek/met/eccc/weathercan_whitehorse_airport.rds')
