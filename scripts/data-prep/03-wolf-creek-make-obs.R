# Script to make CRHM met .obs for wolf creek 
library(CRHMr)
library(tidyverse)

tz <- 'Etc/GMT+7' # tz not listed on metadata so assuming LST
start_date <- as.POSIXct('2015-10-01 00:00', tz = tz)
end_date <- as.POSIXct('2022-10-01 00:00', tz = tz)

wcf_met <- readRDS('data/wolf-creek/met/alex/wcf_gap_fill_ac.rds') |> 
  select(datetime, 
         t = t.low,
         rh = rh.1,
         u = u.high,
         Qsi = sw_in_18m
         )

airport_precip <- readRDS('data/wolf-creek/met/alex/eccc_airport_qaqc_undercatch_corr_ac.rds') |> 
  select(-pc)

obs <- left_join(wcf_met, airport_precip) |> 
  select(datetime:rh, p = ppt, u) |> 
  filter(datetime > start_date,
         datetime <= end_date) # midnight is obs from previous day so filter to get the next one

# output to CRHM obs

CRHMr::writeObsFile(obs, 'crhm/obs/wolf_creek_forest_hourly_2015_2024.obs')

obs |> 
  pivot_longer(!datetime) |> 
  ggplot(aes(datetime, value, colour = name)) +
  geom_line() + facet_grid(rows = vars(name), scales = 'free')
plotly::ggplotly()

hist(obs$p[obs$p>0])
