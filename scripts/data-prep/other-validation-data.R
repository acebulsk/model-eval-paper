# Script to bring in other possible validation data (canopy load, unloading from lysimeters, etc)

library(tidyverse)
library(wxlogR)

# wolf creek

wc_wt <- load_CS_1000(
  'data/wolf-creek/weighed_tree/CR800hangingtree_LoadCell.dat'
) |>
  select(datetime = TIMESTAMP, Weight_lbs)

ggplot(wc_wt, aes(datetime, Weight_lbs)) +
  geom_line()

plotly::ggplotly()

# Marmot

jm_path <- 'data/marmot/jm-thesis-data/'

met_mc <- readObsFile(
  'crhm/obs/Marmot_Hourly_ArrayMetData_withT_g_1Oct05-30Sept24_update_3Jan2025.obs',
  timezone = 'Etc/GMT+6'
) |>
  select(
    datetime,
    # t.1 = t.5, rh.1 = rh.5, u.1 = u.8,
    p.1 = p.5
  ) |>
  filter(datetime < '2008-01-01', datetime > '2007-01-01') |>
  pivot_longer(!datetime)

# unloading data

scl_mc_1 <- readxl::read_xls(
  paste0(jm_path, 'unloading data.xls'),
  sheet = 1
) |>
  select(datetime = Date, trough1 = SWE_1)
scl_mc_2 <- readxl::read_xls(
  paste0(jm_path, 'unloading data.xls'),
  sheet = 2
) |>
  select(trough2 = SWE_2)
scl_mc_3 <- readxl::read_xls(
  paste0(jm_path, 'unloading data.xls'),
  sheet = 3
) |>
  select(trough3 = SWE_3)
scl_mc <- cbind(scl_mc_1, scl_mc_2) |> cbind(scl_mc_3)

scl_mc |>
  pivot_longer(!datetime) |>
  rbind(met_mc) |>
  ggplot(aes(datetime, value)) +
  facet_grid(rows = vars(name)) +
  geom_line()

plotly::ggplotly()

# weighed tree data from JM thesis (seems data file is incomplete)

wt1 <- readxl::read_xls(
  paste0(jm_path, '07data/TOWER07.xls'),
  sheet = '115',
  skip = 1
) |>
  select(datetime = ...1, raw_1:net_tree)

wt2 <- readxl::read_xls(paste0(jm_path, '08data/HANGINGTREE08.xls')) |>
  select(datetime = DATETIME, SWE_1:net_1)

wt <- bind_rows(wt2, wt1) |>
  arrange(datetime)

wt |>
  pivot_longer(!datetime) |>
  rbind(met_mc) |>
  ggplot(aes(datetime, value, colour = name)) +
  geom_line()

plotly::ggplotly()
