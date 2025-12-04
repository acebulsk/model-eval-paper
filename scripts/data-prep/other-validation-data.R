# Script to bring in other possible validation data (canopy load, unloading from lysimeters, etc)

library(tidyverse)
library(wxlogR)
library(CRHMr)

# wolf creek

wc_wt <- load_CS_1000(
  'data/wolf-creek/weighed_tree/CR800Series_hangingtree_LoadCell.dat'
  #'data/wolf-creek/weighed_tree/CR800hangingtree_LoadCell.dat' # something wrong with data does not show any ablation
  # 'data/wolf-creek/weighed_tree/CR3000_tree_LoadCell.dat' # fall 2022 to spring 2023 dont have .obs here

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
  mutate(p_cml = cumsum(p.1)) |> 
  filter(datetime < '2008-07-01', datetime > '2007-01-01') |>
  pivot_longer(!datetime)

# subcanopy lysimeter data

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

# canopy load measurements from JM thesis manually transcribed from Table 5.1 
# need to check are valid here

jm_vals <- read.csv('data/marmot/jm-thesis-data/jmacdonald_thesis_table_5.1_ac.csv') |> 
  mutate(StartTime = as.POSIXct(StartTime, '%Y-%m-%d %H:%M:%S', tz = "Etc/GMT+6")) |> 
  select(Date, datetime = StartTime, CanopyLoad_mmSWE)

# QAQC scl data

scl_mc <- cbind(scl_mc_1, scl_mc_2) |> cbind(scl_mc_3) |> 
  mutate(
    datetime = ceiling_date(datetime),
    datetime = force_tz(datetime, 'Etc/GMT+6')
  )

cumulate_non_negative <- function(df, colnum){
  df <- df[complete.cases(df),]
  dU <- df[,colnum] - lag(df[,colnum])
  dU_fltr <- if_else(dU < 0, 0, dU)
  df$dcml <- dU_fltr
  df$value <-
    cumsum(ifelse(is.na(dU_fltr), 0, dU_fltr))
  
  return(df)
}

scl_mc_cml <- scl_mc |> 
  pivot_longer(!datetime) |> 
  arrange(datetime) |> 
  as.data.frame()

scl_mc_cml_splt <- split(scl_mc_cml, scl_mc_cml$name)

scl_mc_cml_no_neg <- lapply(scl_mc_cml_splt, cumulate_non_negative, 3)
scl_mc_cml_no_neg <- do.call("rbind", scl_mc_cml_no_neg)
scl_mc_cml_no_neg$name <- sub("\\..*", "", rownames(scl_mc_cml_no_neg))

# cumulate over events to visualise

scl_mc_cml_no_neg_w_met <- 
  rbind(scl_mc_cml_no_neg |> select(-dcml),
   met_mc |> filter(name == 'p_cml')) |> 
  left_join(jm_vals) |> 
  fill(Date, .direction = 'down') |> 
  group_by(name, Date) |> 
  mutate(value_events = value - first(value))

scl_mc_cml_no_neg_w_met |>
  select(datetime, name, value = value_events) |> 
  ggplot(aes(datetime, value)) +
  facet_grid(rows = vars(name)) +
  geom_line()

plotly::ggplotly()

# Using Canopy Load = Snowfall - throughfall gets close to the values provided in Table 5.1 so confident in using these values.. 

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
