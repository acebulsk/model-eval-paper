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

tz <- 'Etc/GMT+7' # whitehorse LST

read_crhm_obs <- function(path, prj, runtag, tz) {
  
  fullpath <- list.files(
    paste0(
      path,
      prj
    ),
    pattern = runtag,
    full.names = T
  )
  
  stopifnot(length(path) == 1)
  
  crhm_output_new <- CRHMr::readOutputFile(
    fullpath,
    timezone = tz) 
  
}

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
# tz <- 'Etc/GMT+7' # tz not listed on metadata so assuming LST
# met_rasouli <- read_csv('data/wolf-creek/met/kabir2019/Meteorology/Forest.csv') |>
#   mutate(datetime = as.POSIXct(Date, format = '%m/%d/%Y %H:%M', tz = tz)) |>
#   select(datetime,
#          t = AirTempLow_C_Filled,
#          rh = RelHumidityLow_percent_Filled,
#          u = WSpdAvgAbvCnpy_ms_1_Filled,
#          # swi = `IncomingShortwave_Wm-2`
#          ) |> mutate(group = 'rasouli')
# 
# met_rasouli |> pivot_longer(!datetime) |> 
#   ggplot(aes(datetime, value, colour = name)) + 
#   geom_line() +
#   facet_grid(rows = vars(name), scales = 'free')
# 
# plotly::ggplotly()

# WARNING: AlpineForestShrubTundra_precipitation_mm_Gaps_Filled.csv is DAILY
# Forest_Precip.Rain_mm.csv is tipping bucket hourly and goes away in winter 
# precip <- read_csv('data/wolf-creek/met/kabir2019/Precipitation/Forest_Precip.Rain_mm.csv') |> 
#   mutate(datetime = as.POSIXct(Date, format = '%d/%m/%Y %H:%M', tz = tz)) |> 
#   select(datetime, ppt = Value)

## From Whitehorse Crew (Rosy Tutton) ----

### PRECIP ----

precip <- read_csv('data/wolf-creek/met/rosy/WCFprecip_20250622.csv') |> 
  mutate(datetime = as.POSIXct(date_time, tz = tz)) |> # handles conversion from UTC to LST
  select(datetime, ppt = WGg)

# ggplot(precip, aes(datetime, ppt)) +
#   geom_line()

### MET ----

met_main <- read_csv('data/wolf-creek/met/rosy/WCFmet_20250101.csv') |> 
  mutate(date_time = as.POSIXct(date_time, tz = tz)) |> 
  rename(datetime = date_time)

# met_rosy <- met_main |> 
#   select(datetime, t = TA_low, rh = RH_low, u = US_high) |> 
#   left_join(precip) |> 
#   mutate(group = 'rosy')
# 
# # compare rasouli met with rosy
# rbind(met_rasouli |> 
#         pivot_longer(!c(datetime, group)),
#       met_rosy |>
#         pivot_longer(!c(datetime, group))) |> 
#   ggplot(aes(datetime, value, colour = group)) + 
#   geom_line() +
#   facet_grid(rows = vars(name), scales = 'free')
# 
# plotly::ggplotly()

### Solar ----

# Modelled

path <- "crhm/output/"
prj <- "wolf_creek_forest_snowsurveytransect_cansnobal"
run_tag_updt <- "v_4_0_fix_sensor_hts_airport_fltr_2015_2022_no_tz_offset_2_output"

solar_mod <- read_crhm_obs(path, prj, run_tag_updt, tz) |> 
  select(
    datetime,
    QsiS_Var.1
  ) 

# Observed 

solar1 <- read_csv('data/wolf-creek/met/rosy/WCFtwr1_SWrad_20132018.csv') |> 
  mutate(datetime = as.POSIXct(date_time_UTC, tz = tz),
         albedo_Avg_14m = Solar_Wm2_DOWN_Avg/Solar_Wm2_UP_Avg,
         albedo_Avg_14m = ifelse(albedo_Avg_14m > 20, NA, albedo_Avg_14m),
         albedo_Avg_14m = ifelse(albedo_Avg_14m < 0, NA, albedo_Avg_14m)) |> 
  select(datetime,
         sw_in_14m = Solar_Wm2_UP_Avg,
         sw_out_14m = Solar_Wm2_DOWN_Avg,
         albedo_Avg_14m) # UP vs Down are mislabelled in the header

solar2 <- read_csv('data/wolf-creek/met/rosy/WCFmet_SWrad_20172022.csv') |> 
  mutate(datetime = as.POSIXct(date_time_UTC, tz = tz),
         albedo_Avg_18m = SW_UP_18m_Avg/SW_DN_18m_Avg,
         albedo_Avg_18m = ifelse(albedo_Avg_18m > 20, NA, albedo_Avg_18m),
         albedo_Avg_18m = ifelse(albedo_Avg_18m < 0, NA, albedo_Avg_18m)) |> 
  select(datetime, sw_in_18m = SW_DN_18m_Avg, sw_out_18m = SW_UP_18m_Avg, albedo_Avg_18m)

solar <- rbind(solar1 |> pivot_longer(!datetime),
               solar2 |> pivot_longer(!datetime)) |> rbind(solar_mod |>
                                                             pivot_longer(!datetime))
# estimated solar offset to be 2.0 based on below graph comparing crhm mod solar vs obs
# need this to be close to right for gap filling for now
# albedo doesnt seem to be helpful for qc so not implementing this check
# ggplot(solar, aes(datetime, value, colour = name)) + geom_line()
# 
# plotly::ggplotly()

# ECCC Stations

# stns <- weathercan::stations() |> filter(station_name %in% c('WHITEHORSE A',
#                                                             'WHITEHORSE AUTO'),
#                                         interval == 'hour')

# WHITEHORSE A, 
# # WHITEHORSE A, 
# wha <- weathercan::weather_dl(
#   stns$station_id,
#   start = '2015-01-01',
#   interval = 'hour',
#   time_disp = "UTC" # returned times are actually UTC, otherwise are returned as UTC even though in local time
# )
# saveRDS(wha, 'data/wolf-creek/met/eccc/weathercan_whitehorse_airport.rds')

wha_in <- readRDS('data/wolf-creek/met/eccc/weathercan_whitehorse_airport.rds') |> 
  filter(station_name == 'WHITEHORSE AUTO')
