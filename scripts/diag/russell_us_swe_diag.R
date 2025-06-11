# Script to run diagnostic on Upper Steph Forest SWE
library(tidyverse)
source('scripts/setup.R')
path <- "crhm/output/"

us_obs1 <- CRHMr::readObsFile('crhm/obs/russell/2006-2007 Upper Stephanie-Windsame.OBS', timezone = 'Etc/GMT+8') |> select(-starts_with('Tg'))
CRHMr::writeObsFile(us_obs1, 'crhm/obs/russell/2006-2007UpperStephanie-Windsame_ac.OBS')

# this one has errors (constant temp at -0.21)
# us_obs2 <- CRHMr::readObsFile('crhm/obs/russell/2006-2007UpperStephanie.OBS', timezone = 'Etc/GMT+8')
# CRHMr::writeObsFile(us_obs2, 'crhm/obs/russell/2006-2007UpperStephanie_ac.OBS')

# this one has gaps/errors filled
us_obs2 <- CRHMr::readObsFile('crhm/obs/russell/2006-2007UpperStephanieA.OBS', timezone = 'Etc/GMT+8') |> select(-starts_with('T_g'))

CRHMr::writeObsFile(us_obs2, 'crhm/obs/russell/2006-2007UpperStephanieA_ac.OBS')

snow_survey <- CRHMr::readObsFile(
  'data/russell-creek/2006-2007 Upper Stephanie-SWE.obs',
  timezone = 'Etc/GMT+8'
) |> select(datetime, forest_swe = SWE.8, open_swe = SWE.1) |>  # 8 is USOG2 2 is CC2
  pivot_longer(!datetime) |> 
  mutate(value = ifelse(value == 0, NA, value), group = 'Snow Survey')

### updated crhm (cansnobal) ----

# prj <- "russell_upper_steph_forest_snowsurveytransect_cansnobal_mod_solar"
# run_tag_updt <- "forest_solar"

prj <- "russell_upper_steph_forest_snowsurveytransect_cansnobal"
run_tag_updt <- "w12"

# prj <- "russell_upper_steph_forest_snowsurveytransect_baseline"
# run_tag_updt <- "actual_r2"
# 
# prj <- "russell_upper_steph_forest_snowsurveytransect_baseline_ac"
# run_tag_updt <- "r4"

crhm_output_new <- read_crhm_obs(path, prj, run_tag_updt, 'Etc/GMT+8')

swe <- crhm_output_new |> 
  select(datetime, forest_swe = SWE.1, open_swe = SWE.2, open_h2o = h2o.2) |> 
  select(datetime, forest_swe, open_swe) |> 
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

swe <- crhm_output_new |> 
  select(datetime, forest_swe = SWE.1, open_swe = SWE.2, precip = hru_p.1, snow = hru_snow.1, rain = hru_rain.1) |> 
  mutate(precip_cml = cumsum(precip), snow_cml = cumsum(snow)) |> 
  select(datetime, forest_swe, open_swe, precip_cml, snow_cml) |> 
  pivot_longer(!datetime) |> mutate(group = 'station')

swe <- crhm_output_new |> 
  select(datetime, forest_swe = SWE.1, open_swe = SWE.2, open_h2o = h2o.2, snow_load = Snow_load.1) |> 
  select(datetime, forest_swe, open_swe, snow_load) |> 
  pivot_longer(!datetime) |> mutate(group = 'Simulation')

ggplot(swe, aes(datetime, value, colour = name)) + 
  geom_line() +
  geom_point(data = snow_survey)
plotly::ggplotly()

met_w_canopy_snow <- crhm_output_new |> select(
  datetime,
  qsi = QsiS_Var.1,
  t = hru_t.1,
  rh = hru_rh.1,
  u = hru_u.1,
  precip = hru_p.1,
  snow = hru_snow.1,
  canopy_load_mm = Snow_load.1
  # canopy_load_mm = m_s_veg.1
) |> pivot_longer(!datetime)

ggplot(met_w_canopy_snow, aes(datetime, value, colour = name)) +
  geom_line() + facet_grid(rows = vars(name), scales = 'free')

plotly::ggplotly()

