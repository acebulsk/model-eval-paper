# Script to plot met data
library(tidyverse)
library(CRHMr)

met_mc <- readObsFile(
  'crhm/obs/Marmot_Hourly_ArrayMetData_withT_g_1Oct05-30Sept24_update_3Jan2025.obs',
  timezone = 'Etc/GMT+6'
) |>
  select(datetime, t.1 = t.5, rh.1 = rh.5, p.1 = p.5, u.1 = u.8) |>
  filter(datetime < '2020-01-01', datetime > '2019-01-01') |> 
  mutate(ppt = cumsum(p.1))

met_mc |>
  pivot_longer(!datetime) |>
  ggplot(aes(datetime, value)) +
  geom_line() +
  facet_grid(rows = vars(name), scales = 'free')

plotly::ggplotly()
