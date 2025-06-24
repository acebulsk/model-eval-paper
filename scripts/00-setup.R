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
    timezone = tz) 
  
}

path <- "crhm/output/"

# LOAD DATA ----

## POWERLINE ---- 

### obs snow ----

fortress_snow_survey <- CRHMr::readObsFile(
  'data/fortress/snow_survey/Powerline_obs_SWE_2013-23.obs',
  timezone = 'Etc/GMT+6'
) |> select(datetime, obs_swe = Powerline_Forest.1) |> 
  mutate(station = 'Fortress - Powerline Forest')

# Read the file while skipping non-relevant lines
snow_scale <- read.table('data/fortress/snow_survey/Powerline_SommerSnowScale_SWE_29Oct2013-16Oct2023.obs', 
                         skip = 3,       # Skip the first 4 lines (including column names)
                         fill = T,
                         col.names = c("datetime", "Observed_clearing"),  # Assign column names
                         header = FALSE) # Data doesn't have a header row

snow_scale$datetime <- as.POSIXct(as.numeric(snow_scale[, 1]) * 24 * 3600, 
                                  origin = "1899-12-30", tz = "UTC")
snow_scale$datetime <- lubridate::force_tz(snow_scale$datetime, tzone = 'Etc/GMT+6')
snow_scale <- CRHMr::makeRegular(snow_scale, 'Etc/GMT+6')

### updated crhm (cansnobal) ----

prj <- "fortress_powerline_clearing_snowsurveytransect_cansnobal"
run_tag_updt <- "v_4_0_updt_solar_cases"

crhm_output_new <- read_crhm_obs(path, prj, run_tag_updt, 'Etc/GMT+6') |> 
  select(datetime, swe = SWE.1, cpy_swe = m_s_veg.1) |> 
  pivot_longer(!datetime, names_to = 'var') |> 
  mutate(model = 'CP25')

### baseline crhm ----
prj <- "fortress_powerline_clearing_snowsurveytransect_baseline"
run_tag_base <- "v2_w_snobal_bug"

crhm_output_base <- read_crhm_obs(
  path = path,
  prj = prj,
  runtag = run_tag_base,
  tz = 'Etc/GMT+6'
) |>
  select(datetime, swe = SWE.1, cpy_swe = Snow_load.1) |> 
  pivot_longer(!datetime, names_to = 'var') |> 
  mutate(model = 'E10')

# ggplot(crhm_output_base, aes(datetime, SWE.1)) + 
#   geom_line()

### combine powerline ----

fortress_obs_sim_swe <- rbind(crhm_output_new,
                                    crhm_output_base) |> 
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
) |> select(datetime, obs_swe = Upper_Forest.1) |>
  mutate(station = 'Marmot - Upper Forest')

### updated crhm (cansnobal) ----

prj <- "marmot_upper_forest_clearing_snowsurveytransect_cansnobal"
run_tag_updt <- "cancov_0.8_r2"

crhm_output_new <- read_crhm_obs(path, prj, run_tag_updt, 'Etc/GMT+6') |> 
  select(datetime, swe = SWE.1, cpy_swe = m_s_veg.1) |> 
  pivot_longer(!datetime, names_to = 'var') |> 
  mutate(model = 'CP25')

### baseline crhm ----
prj <- "marmot_upper_forest_clearing_snowsurveytransect_baseline"
run_tag_base <- "r2"

crhm_output_base <- read_crhm_obs(
  path = path,
  prj = prj,
  runtag = run_tag_base,
  tz = 'Etc/GMT+6'
) |>
  select(datetime, swe = SWE.1, cpy_swe = Snow_load.1) |> 
  pivot_longer(!datetime, names_to = 'var') |> 
  mutate(model = 'E10')

# ggplot(crhm_output_base, aes(datetime, SWE.1)) + 
#   geom_line()

### combine marmot ----

marmot_obs_sim_swe <- rbind(crhm_output_new,
                                crhm_output_base) |>
  mutate(station = 'Marmot - Upper Forest')

## WOLF CREEK ----

### Snow survey obs ---- 

# snow_survey <- CRHMr::readObsFile(
#   'data/wolf-creek/snow_survey/WolfCreek_Forest_observed_SWE_1999.obs',
#   timezone = 'Etc/GMT+7'
# ) |> select(datetime, obs_swe = Forest_SWE.1)
# 
# ### updated crhm (cansnobal) ----
# 
# prj <- "wolf_creek_forest_snowsurveytransect_cansnobal"
# run_tag_updt <- "v_4_0_r5"
# 
# crhm_output_new <- read_crhm_obs(path, prj, run_tag_updt, 'Etc/GMT+6') |> 
#   select(datetime, CP25 = SWE.1, Simulated_open_new = SWE.2)
# 
# ### baseline crhm ----
# 
# prj <- "wolf_creek_borland_example"
# run_tag_updt <- "r1"
# 
# crhm_output_base <- read_crhm_obs(path, prj, run_tag_updt, 'Etc/GMT+6') |> 
#   select(datetime, E10 = SWE.3)
# 
# ### combine wolf ----
# 
# snowscale_and_new <- left_join(crhm_output_new,
#                                crhm_output_base,
#                                by = 'datetime') |>
#   mutate(station = 'Wolf Creek Forest')
# 
# ggplot(snowscale_and_new |> pivot_longer(starts_with('Simulated_')), aes(datetime, value, colour = name)) +
#   geom_line()
# 

## RUSSELL CREEK ----

### Snow survey obs ---- 

russell_snow_survey <- 
  readRDS('data/russell-creek/russell_upper_stephanie_all_swe_2006_2008.rds') |> 
  select(datetime, obs_swe = OG2)  |>
  mutate(station = 'Russell - US Old Growth')

### updated crhm (cansnobal) ----

prj <- "russell_upper_steph_forest_snowsurveytransect_cansnobal"
run_tag_updt <- "rs_-0.25_lai_1.93_cc0.86"

crhm_output_new <- read_crhm_obs(path, prj, run_tag_updt, 'Etc/GMT+8') |> 
  select(datetime, swe = SWE.1, cpy_swe = m_s_veg.1) |> 
  pivot_longer(!datetime, names_to = 'var') |> 
  mutate(model = 'CP25')

### baseline crhm ----

prj <- "russell_upper_steph_forest_snowsurveytransect_baseline"
run_tag_updt <- "new_obs"

crhm_output_base <- read_crhm_obs(path, prj, run_tag_updt, 'Etc/GMT+8') |> 
  select(datetime, swe = SWE.1, cpy_swe = Snow_load.1) |> 
  pivot_longer(!datetime, names_to = 'var') |> 
  mutate(model = 'E10')

### combine russell ----

russell_obs_sim_swe <- rbind(crhm_output_new,
                                 crhm_output_base) |>
  mutate(station = 'Russell - US Old Growth')

# Combine all sites ----

all_sites_mods <- 
  rbind(marmot_obs_sim_swe, 
        russell_obs_sim_swe) |> 
  rbind(fortress_obs_sim_swe)

all_sites_obs <- 
  rbind(marmot_snow_survey,
        russell_snow_survey,
        fortress_snow_survey)