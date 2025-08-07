# Script to run diagnostic on Upper Steph Forest SWE
library(tidyverse)
source('scripts/00-setup.R')
path <- "crhm/output/"

# snow_survey <- CRHMr::readObsFile(
#   'data/wolf-creek/snow_survey/obs/WolfCreek_Forest_observed_SWE_1999.obs',
#   timezone = 'Etc/GMT+7'
# ) |> pivot_longer(!datetime) |> 
#   mutate(group = 'Snow Survey')

snow_survey <- 
  read_csv('data/wolf-creek/snow_survey/obs/wcf_snow_survey_stats_1993_2024.csv') |> 
  select(datetime = date, swe_mean) |> 
  pivot_longer(!datetime) |> 
  mutate(group = 'Snow Survey') |> 
  filter(datetime > '2015-10-01', datetime < '2022-10-01')

### updated crhm (cansnobal) ----

# prj <- "russell_upper_steph_forest_snowsurveytransect_cansnobal_mod_solar"
# run_tag_updt <- "forest_solar"

prj <- "wolf_creek_forest_snowsurveytransect_cansnobal"
run_tag_updt <- "v_4_0_fix_sensor_hts_airport_fltr_2015_2022_no_h2o_bug"

crhm_output_new <- read_crhm_obs(path, prj, run_tag_updt, 'Etc/GMT+7') |> 
  mutate(ground = throughfall_snow.1 + deldrip_veg_int.1 + delunld_int.1,
         atmos = delsub_veg_int.1 + delevap_veg_int.1)

swe <- crhm_output_new |> 
  select(datetime, WCF = SWE.1, OPEN = SWE.2) |> 
  select(datetime, WCF, OPEN) |> 
  pivot_longer(!datetime) |> mutate(group = 'Simulation')

ggplot(swe, aes(datetime, value, colour = name, linetype = group, shape = group)) + 
  geom_line() +
  geom_point(data = snow_survey) +
  scale_linetype_manual(name = "group", values = c("Simulation" = "solid")) +
  scale_shape_manual(name = "group", values = c("Snow Survey" = 16)) +
  labs(y = 'SWE (mm)',
       x = element_blank())
plotly::ggplotly()

ggsave(
  paste0(
    'figs/crhm-analysis/diag/',
    prj,
    '/',
    run_tag_updt,
    '_russell_upper_stephanie_obs_mod_swe.png'
  ), width = 6, height = 4)

# diag canopy snow processes 

cpy_snow_proc <- crhm_output_new |> 
  select(
    datetime,
    hru_snow.1,
    throughfall_snow.1,
    delunld_int.1,
    delmelt_veg_int.1,
    # deldrip_veg_int.1,
    delsub_veg_int.1,
    # delevap_veg_int.1
    snowmelt_int.1,
    E_s_0_int.1
  )

cpy_snow_proc |> 
  pivot_longer(!datetime) |> 
  filter(datetime > '2015-10-01', datetime < '2016-10-01') |>
  group_by(name) |> 
  mutate(cum_value = cumsum(value),
         value = cum_value - first(cum_value)) |> 
  ggplot(aes(datetime, value, colour = name)) +
  geom_line() +
  facet_grid(rows = vars(name), scales = 'free')
plotly::ggplotly()

met_w_canopy_snow <- crhm_output_new |> select(
  datetime,
  # qsi = QsiS_Var.1,
  t = hru_t.1,
  rh = hru_rh.1,
  u = hru_u.1,
  precip = hru_p.1,
  snow = hru_snow.1,
  # canopy_load_mm = Snow_load.1
  canopy_load_mm = m_s_veg.1
) |> pivot_longer(!datetime)

ggplot(met_w_canopy_snow, aes(datetime, value, colour = name)) +
  geom_line() + facet_grid(rows = vars(name), scales = 'free')

plotly::ggplotly()

# frac atmos vs ground

cpy_snow_proc <- crhm_output_new |> 
  select(
    datetime,
    hru_snow.1,
    throughfall_snow.1,
    delunld_int.1,
    delmelt_veg_int.1,
    # deldrip_veg_int.1,
    delsub_veg_int.1,
    # delevap_veg_int.1
    snowmelt_int.1,
    E_s_0_int.1
  )