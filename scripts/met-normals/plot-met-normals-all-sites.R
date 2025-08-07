# Script to plot monthly met normals for each of the four sites

library(tidyverse)

options(ggplot2.discrete.colour= palette.colors(palette = "R4"))

pretty_names <- tibble::tibble(
  name = c("p.1", "rh.1", "t.1", "u.1"),
  name_pretty = c(
    "Total Precipitation (mm)",
    "Relative Humidity (%)",
    "Air Temperature (°C)",
    "Wind Speed (m/s)"
  )
)

## tidy data ----

wc_met <- CRHMr::readObsFile('crhm/obs/wolf_creek_forest_hourly_2015_2024.obs',
                             timezone = 'Etc/GMT+7') |> 
  mutate(station = 'Wolf Creek')

rc_met <- readRDS('crhm/obs/russell/russell_upper_stephanie_clearcut2_2005_2008_withgaps.rds') |> 
  select(datetime, t.1 = t, rh.1 = rh, p.1 = p, u.1 = u) |> 
  mutate(station = 'Russell Creek')

mc_met <- CRHMr::readObsFile('crhm/obs/Marmot_Hourly_ArrayMetData_withT_g_1Oct05-30Sept24_update_3Jan2025.obs',
                             timezone = 'Etc/GMT+6') |> 
  select(datetime, t.1 = t.5, rh.1 = rh.5, p.1 = p.5, u.1 = u.8)  |> 
  mutate(station = 'Marmot Creek')

fm_met <- CRHMr::readObsFile('crhm/obs/Fortress_Hourly_ArrayMetData_1Oct2013-30Sept2023_update_13Nov2023.obs',
                             timezone = 'Etc/GMT+6') |> 
  select(datetime, t.1 = t.7, rh.1 = rh.7, p.1 = p.4 , u.1 = u.7) |> 
  mutate(station = 'Fortress Mountain')

## combine dataframes ----

all_met <- rbind(wc_met, rc_met) |> 
  rbind(mc_met) |> 
  rbind(fm_met) 

## average by month ----

all_pcp_monthly_mean <- all_met |> 
  mutate(month = factor(format(datetime, "%b"), levels = month.abb),
         year = year(datetime)) |> 
  group_by(month, year, station) |> 
  summarise(p.1 = sum(p.1)) |> 
  group_by(month, station) |> 
  summarise(p.1 = mean(p.1)) |> 
  pivot_longer(p.1)

all_met_monthly_mean <- all_met |> 
  mutate(month = factor(format(datetime, "%b"), levels = month.abb),
         year = year(datetime)) |> 
  group_by(month, year,station) |> 
  summarise(t.1 = mean(t.1),
            rh.1 = mean(rh.1),
            u.1 = mean(u.1)) |> 
  pivot_longer(t.1:u.1)

monthly_met <- rbind(all_pcp_monthly_mean, all_met_monthly_mean) |> 
  left_join(pretty_names)

## plot monthly met data ----

ggplot(monthly_met, aes(month, value, colour = station, group = station)) +
  geom_point() +
  geom_line() +
  facet_wrap(~name_pretty, scales = 'free') +
  ylab('Monthly Mean') +
  xlab(element_blank()) + 
  labs(colour = 'Station') +
  theme(legend.position = 'bottom')

ggsave('figs/met-normals/monthly_met_normals_all_stns.png', width = 7, height = 5)

# Print date ranges of met
all_met |>
mutate(year = year(datetime)) |> 
group_by(station) |> 
summarise(start = min(year),
          end = max(year))

