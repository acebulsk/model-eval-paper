# Script to make HOURLY CRHM met obs for Wolf Creek
# Options are: 
# A: wat yrs 2015-present using hourly weighing gauge + met data from Rosy
# B: wat yrs 1993-2014 from Rasouli 2018 (but precip is daily)
# DATA INPUT: Rasouli, K., Pomeroy, J., Janowicz, J., Williams, T., Carey, S. (2018). Hydrometeorological data collected at Wolf Creek Research Basin, Yukon Territory, Canada over 1993-2014. Federated Research Data Repository. https://doi.org/10.20383/101.0113
# Above has hourly temp, rh, wind gap filled, precip gap fill is at daily resolution
# NOTE: data from Rosy is in UTC

library(tidyverse)
library(CRHMr)
library(weathercan)

# SETUP ----

dt_test <- 737852 # march 2 2020
matlab_origin <- 719529 # 0000-01-00 not compatible with R's origin so we force it here
posix_date <- as.POSIXct((dt_test - matlab_origin)*86400, origin = "1970-01-01", tz = "UTC")

# LOAD DATA ----

## Newer data from Gord ----
# 
# datetimes <- read_table('data/wolf-creek/met/gord/Wolf_Creek_Forest_2021_Clean/Clean/TV.dat',
#                                col_names = 'matlabtime') |>
#   mutate(datetime = as.POSIXct((matlabtime - matlab_origin) * 86400,
#                                origin = "1970-01-01",
#                                tz = "UTC"
#   ))
# 
# at <- read_table('data/wolf-creek/met/Wolf_Creek_Forest_2021_Clean/Clean/Air_Temp_2m_Avg.dat',
#                  col_names = 't', na = 'NaN')

## Rasouli2019
tz <- 'Etc/GMT+7' # tz not listed on metadata so assuming LST
met_rasouli <- read_csv('data/wolf-creek/met/kabir2019/Meteorology/Forest.csv') |>
  mutate(datetime = as.POSIXct(Date, format = '%m/%d/%Y %H:%M', tz = tz)) |>
  select(datetime,
         t = AirTempLow_C_Filled,
         rh = RelHumidityLow_percent_Filled,
         u = WSpdAvgAbvCnpy_ms_1_Filled,
         # swi = `IncomingShortwave_Wm-2`
         ) |> mutate(group = 'rasouli')

met_rasouli |> pivot_longer(!datetime) |> 
  ggplot(aes(datetime, value, colour = name)) + 
  geom_line() +
  facet_grid(rows = vars(name), scales = 'free')

plotly::ggplotly()

# WARNING: AlpineForestShrubTundra_precipitation_mm_Gaps_Filled.csv is DAILY
# Forest_Precip.Rain_mm.csv is tipping bucket hourly and goes away in winter 
# precip <- read_csv('data/wolf-creek/met/kabir2019/Precipitation/Forest_Precip.Rain_mm.csv') |> 
#   mutate(datetime = as.POSIXct(Date, format = '%d/%m/%Y %H:%M', tz = tz)) |> 
#   select(datetime, ppt = Value)

obs <- left_join(met, precip)

## From Rosy Tutton ----

# PRECIP

tz_in <- 'UTC' # tz from header meta is UTC
tz_out <- 'Etc/GMT+7' # whitehorse LST

precip <- read_csv('data/wolf-creek/met/rosy/WCFprecip_20250622.csv') |> 
  mutate(datetime = as.POSIXct(date_time, tz = tz_in),
         datetime = with_tz(datetime, tz_out)) |> 
  select(datetime, ppt = WGg)

ggplot(precip, aes(datetime, ppt)) +
  geom_line()

# MET 

met_rosy <- read_csv('data/wolf-creek/met/rosy/WCFmet_20250101.csv') |> 
    mutate(date_time = as.POSIXct(date_time, tz = tz_in),
           date_time = with_tz(date_time, tz_out)) |> 
    rename(datetime = date_time) |> 
    # filter(datetime >= '2015-10-01') |> 
  select(datetime, t = TA_low, rh = RH_low, u = US_high) |> 
  left_join(precip) |> 
  mutate(group = 'rosy')

# compare rasouli met with rosy
rbind(met_rasouli |> 
        pivot_longer(!c(datetime, group)),
      met_rosy |>
        pivot_longer(!c(datetime, group))) |> 
  ggplot(aes(datetime, value, colour = group)) + 
  geom_line() +
  facet_grid(rows = vars(name), scales = 'free')

plotly::ggplotly()

# plot solar

solar_rosy1 <- read_csv('data/wolf-creek/met/rosy/WCFtwr1_netrad_20132018.csv') |> 
  pivot_longer(!date_time_UTC)
solar_rosy2 <- read_csv('data/wolf-creek/met/rosy/WCFmet_netrad_20172022.csv') |> 
  pivot_longer(!date_time_UTC)
solar_rosy <- rbind(solar_rosy1, solar_rosy2) |> 
  mutate(date_time_UTC = as.POSIXct(date_time_UTC, tz = tz_in),
         datetime = with_tz(date_time_UTC, tz_out)) |> 
  filter(name != 'Net_Radiation_2m_Avg') # this sensor is within canopy and shaded.

ggplot(solar_rosy, aes(datetime, value, colour = name)) + geom_line()

plotly::ggplotly()

# ECCC Stations

stns <- weathercan::stations() |> filter(station_name %in% c('WHITEHORSE A',
                                                            'WHITEHORSE AUTO'),
                                        interval == 'hour')

# WHITEHORSE A, 
# # WHITEHORSE A, 
# wha <- weathercan::weather_dl(
#   stns$station_id,
#   start = '2015-01-01',
#   interval = 'hour',
#   time_disp = "UTC" # returned times are actually UTC, otherwise are returned as UTC even though in local time
# )
# saveRDS(wha, 'data/wolf-creek/met/eccc/weathercan_whitehorse_airport.rds')
