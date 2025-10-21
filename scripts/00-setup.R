# Setup script to define parameters and pull correct model runs
library(tidyverse)
library(CRHMr)

# functions ----
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
    timezone = tz
  )
}

to_long_short <- function(from, to, quality, notes, event_id){
  datetime <- seq(from, to, 900)

  out <- data.frame(datetime, quality, notes, event_id)

  return(out)
}

path <- "crhm/output/"

# LOAD DATA ----

## POWERLINE ----

### obs snow ----

fortress_snow_survey <- CRHMr::readObsFile(
  'data/fortress/snow_survey/Powerline_obs_SWE_2013-23.obs',
  timezone = 'Etc/GMT+6'
) |>
  select(datetime, obs_swe = Powerline_Forest.1) |>
  mutate(station = 'Fortress - Powerline Forest')

fm_cpy_load_obs <- readRDS('data/fortress/weighed_tree/unloading_events_zero_weighed_tree_kg_m2_pre_post_cnpy_snow_fsd_closed_0.88.rds') |> 
  select(datetime, var = name, value = tree_mm) |> 
  mutate(model = 'Obs')

## Marmot ----

mc_cpy_load_obs_jm <- read.csv('data/marmot/jm-thesis-data/jmacdonald_thesis_table_5.1_ac.csv') |> 
  mutate(EndTime = as.POSIXct(EndTime, '%Y-%m-%d %H:%M:%S', tz = "Etc/GMT+6")) |> 
  mutate(
    var = 'cpy_swe',
    model = 'Obs',
    year = '2007-2008'
  ) |> 
    select(datetime = EndTime, var, value = CanopyLoad_mmSWE, model, year)

mc_cpy_load_obs_js <- read_rds('data/marmot/cob-thesis-data/processed_ac/weighed_tree_kg_m2_zero_pre_post_cnpy_snow.rds') |> 
  mutate(
    var = 'cpy_swe',
    model = 'Obs',
    year = '2018-2019'
  ) |> 
  select(datetime, var, value, model, year)

## Wolf Creek ----

wc_cpy_load_obs <- read.csv('data/marmot/jm-thesis-data/jmacdonald_thesis_table_5.1_ac.csv') |> 
  mutate(EndTime = as.POSIXct(EndTime, '%Y-%m-%d %H:%M:%S', tz = "Etc/GMT+6")) |> 
  mutate(
    var = 'cpy_swe',
    model = 'Obs'
  ) |> 
    select(datetime = EndTime, var, value = CanopyLoad_mmSWE, model)

# CRHM OUTPUTS ----

## FORTRESS - POWERLINE ----

### updated crhm (cansnobal) ----

prj <- "fortress_powerline_clearing_snowsurveytransect_cansnobal"
run_tag_updt <- "v_4_0_logan_fix_precip"

crhm_output_new <- read_crhm_obs(path, prj, run_tag_updt, 'Etc/GMT+6') |>
  select(
    datetime,
    swe = SWE.1,
    cpy_swe = m_s_veg.1,
    # rain = hru_rain.1,
    snow = hru_snow.1,
    # pcp = hru_p.1,
    # tf = net_p.1,
    subl = delsub_veg_int.1,
    # evap = delevap_veg_int.1,
    # drip = deldrip_veg_int.1,
    melt = delmelt_veg_int.1,
    unld = delunld_int.1,
    tf = throughfall_snow.1,
    # tf_rf = throughfall_rain.1
  ) |>
  mutate(subl = -subl) |> # flux is wrt canopy and diff from baseline
  pivot_longer(!datetime, names_to = 'var') |>
  mutate(model = 'CP25')

### baseline crhm ----
prj <- "fortress_powerline_clearing_snowsurveytransect_baseline"
run_tag_base <- "v_4_0_logan_fix_precip"

crhm_output_base <- read_crhm_obs(
  path = path,
  prj = prj,
  runtag = run_tag_base,
  tz = 'Etc/GMT+6'
) |>
  select(
    datetime,
    swe = SWE.1,
    cpy_swe = Snow_load.1,
    # rain = hru_rain.1,
    snow = hru_snow.1,
    unld = SUnload.1,
    melt = SUnload_H2O.1,
    # pcp = hru_p.1,
    tf = direct_snow.1,
    subl = Subl_Cpy.1
  ) |>
  pivot_longer(!datetime, names_to = 'var') |>
  mutate(model = 'E10')

# ggplot(crhm_output_base, aes(datetime, SWE.1)) +
#   geom_line()

### combine powerline ----

fortress_obs_sim_swe <- rbind(crhm_output_new, crhm_output_base) |>
  mutate(station = 'Fortress - Powerline Forest')

# snowscale_and_new <- left_join(crhm_output_new,
#                                snow_scale,
#                                by = 'datetime') |>
#   left_join(crhm_output_base) |>
#   mutate(station = 'Powerline Forest')

## MARMOT UPPER FOREST ----

### obs snow ----

marmot_snow_survey <- CRHMr::readObsFile(
  'data/marmot/Marmot_UpperForest_UpperClearing_SWE_2008-23.obs',
  timezone = 'Etc/GMT+6'
) |>
  select(datetime, obs_swe = Upper_Forest.1) |>
  mutate(station = 'Marmot - Upper Forest')

### updated crhm (cansnobal) ----

prj <- "marmot_upper_forest_clearing_snowsurveytransect_cansnobal"
run_tag_updt <- "r4_earlier_starttime"

crhm_output_new <- read_crhm_obs(path, prj, run_tag_updt, 'Etc/GMT+6') |>
  select(
    datetime,
    swe = SWE.1,
    cpy_swe = m_s_veg.1,
    # rain = hru_rain.1,
    snow = hru_snow.1,
    # pcp = hru_p.1,
    # tf = net_p.1,
    subl = delsub_veg_int.1,
    melt = delmelt_veg_int.1,
    # evap = delevap_veg_int.1,
    # drip = deldrip_veg_int.1,
    unld = delunld_int.1,
    tf = throughfall_snow.1
    # tf_rf = throughfall_rain.1
  ) |>
  mutate(subl = -subl) |> # flux is wrt canopy and diff from baseline
  pivot_longer(!datetime, names_to = 'var') |>
  mutate(model = 'CP25')

### baseline crhm ----
prj <- "marmot_upper_forest_clearing_snowsurveytransect_baseline"
run_tag_base <- "r5_add_rain_load_output"

crhm_output_base <- read_crhm_obs(
  path = path,
  prj = prj,
  runtag = run_tag_base,
  tz = 'Etc/GMT+6'
) |>
  select(
    datetime,
    swe = SWE.1,
    cpy_swe = Snow_load.1,
    cpy_rain = rain_load.1,
    # rain = hru_rain.1,
    snow = hru_snow.1,
    melt = SUnload_H2O.1,
    unld = SUnload.1,
    # pcp = hru_p.1,
    tf = direct_snow.1,
    subl = Subl_Cpy.1
  ) |>
  mutate(cpy_swe = cpy_swe + cpy_rain) |> 
  pivot_longer(!datetime, names_to = 'var') |>
  mutate(model = 'E10')

# ggplot(crhm_output_base, aes(datetime, SWE.1)) +
#   geom_line()

### combine marmot ----

marmot_obs_sim_swe <- rbind(crhm_output_new, crhm_output_base) |>
  mutate(station = 'Marmot - Upper Forest')

## WOLF CREEK ----

### Snow survey obs ----

wcf_snow_survey <- readRDS(
  'data/wolf-creek/snow_survey/obs/wcf_snow_survey_stats_1993_2024.rds'
) |>
  select(datetime = date, obs_swe = swe_mean) |>
  mutate(station = 'Wolf Creek - Forest') |>
  filter(datetime > '2015-10-01', datetime < '2022-10-01')

### updated crhm (cansnobal) ----

prj <- "wolf_creek_forest_snowsurveytransect_cansnobal"
run_tag_updt <- "revert_r4"

crhm_output_new <- read_crhm_obs(path, prj, run_tag_updt, 'Etc/GMT+7') |>
  select(
    datetime,
    swe = SWE.1,
    cpy_swe = m_s_veg.1,
    # rain = hru_rain.1,
    snow = hru_snow.1,
    # pcp = hru_p.1,
    # tf = net_p.1,
    subl = delsub_veg_int.1,
    melt = delmelt_veg_int.1,
    # evap = delevap_veg_int.1,
    # drip = deldrip_veg_int.1,
    unld = delunld_int.1,
    tf = throughfall_snow.1
    # tf_rf = throughfall_rain.1
  ) |>
  mutate(subl = -subl) |> # flux is wrt canopy and diff from baseline
  pivot_longer(!datetime, names_to = 'var') |>
  mutate(model = 'CP25')

### baseline crhm ----

prj <- "wolf_creek_forest_snowsurveytransect_baseline"
run_tag_base <- "revert_r4"

crhm_output_base <- read_crhm_obs(
  path = path,
  prj = prj,
  runtag = run_tag_base,
  tz = 'Etc/GMT+7'
) |>
  select(
    datetime,
    swe = SWE.1,
    cpy_swe = Snow_load.1,
    # rain = hru_rain.1,
    snow = hru_snow.1,
    melt = SUnload_H2O.1,
    unld = SUnload.1,
    # pcp = hru_p.1,
    tf = direct_snow.1,
    subl = Subl_Cpy.1
  ) |>
  pivot_longer(!datetime, names_to = 'var') |>
  mutate(model = 'E10')

### combine wolf ----

wcf_obs_sim_swe <- rbind(crhm_output_new, crhm_output_base) |>
  mutate(station = 'Wolf Creek - Forest')

## RUSSELL CREEK ----

### Snow survey obs ----

russell_snow_survey <-
  readRDS('data/russell-creek/russell_upper_stephanie_all_swe_2006_2008.rds') |>
  select(datetime, obs_swe = OG2) |>
  mutate(station = 'Russell - Old Growth')

### updated crhm (cansnobal) ----

prj <- "russell_upper_steph_forest_snowsurveytransect_cansnobal"
run_tag_updt <- "rs_harder_lai_1.93_cc0.86_addtf"

crhm_output_new <- read_crhm_obs(path, prj, run_tag_updt, 'Etc/GMT+8') |>
  select(
    datetime,
    swe = SWE.1,
    cpy_swe = m_s_veg.1,
    # rain = hru_rain.1,
    snow = hru_snow.1,
    # pcp = hru_p.1,
    # tf = net_p.1,
    subl = delsub_veg_int.1,
    melt = delmelt_veg_int.1,
    # evap = delevap_veg_int.1,
    # drip = deldrip_veg_int.1,
    unld = delunld_int.1,
    tf = throughfall_snow.1
    # tf_rf = throughfall_rain.1
  ) |>
  mutate(subl = -subl) |> # flux is wrt canopy and diff from baseline
  pivot_longer(!datetime, names_to = 'var') |>
  mutate(model = 'CP25')

### baseline crhm ----

prj <- "russell_upper_steph_forest_snowsurveytransect_baseline"
run_tag_updt <- "ellis2010unldpars_rs_harder_sbar_8_7"

crhm_output_base <- read_crhm_obs(path, prj, run_tag_updt, 'Etc/GMT+8') |>
  select(
    datetime,
    swe = SWE.1,
    cpy_swe = Snow_load.1,
    # rain = hru_rain.1,
    snow = hru_snow.1,
    melt = SUnload_H2O.1,
    unld = SUnload.1,
    # pcp = hru_p.1,
    tf = direct_snow.1,
    subl = Subl_Cpy.1
  ) |>
  pivot_longer(!datetime, names_to = 'var') |>
  mutate(model = 'E10')

### combine russell ----

zero_start <- as.POSIXct('2007-04-21 19:00', tz = 'Etc/GMT+8')
zero_end <- as.POSIXct('2007-11-10 20:00', tz = 'Etc/GMT+8')

russell_obs_sim_swe <- rbind(crhm_output_new, crhm_output_base) |>
  mutate(
    station = 'Russell - Old Growth',
    value = ifelse(datetime >= zero_start & datetime <= zero_end, NA, value)
  )

# Combine all sites ----

all_sites_mods <-
  rbind(marmot_obs_sim_swe, russell_obs_sim_swe) |>
  rbind(fortress_obs_sim_swe) |>
  rbind(wcf_obs_sim_swe)

all_sites_obs <-
  rbind(marmot_snow_survey, russell_snow_survey, fortress_snow_survey) |>
  rbind(wcf_snow_survey)

all_sites_yrs <- all_sites_mods |>
  group_by(station) |>
  summarise(start = min(year(datetime)), end = max(year(datetime)))
