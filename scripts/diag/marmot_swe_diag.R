# Script to run diagnostic on Upper Steph Forest SWE
library(tidyverse)
source('scripts/setup.R')
path <- "crhm/output/"

snow_survey <- CRHMr::readObsFile(
  'data/marmot/Marmot_UpperForest_UpperClearing_SWE_2008-23.obs',
  timezone = 'Etc/GMT+6'
) |> select(datetime, forest_swe = Upper_Forest.1) |> 
  pivot_longer(!datetime) |> 
  mutate(group = 'Snow Survey')

### updated crhm (cansnobal) ----

prj <- "marmot_upper_forest_clearing_snowsurveytransect_cansnobal"
run_tag_updt <- "canopy_coverage_0.6"

crhm_output_new <- read_crhm_obs(path, prj, run_tag_updt, 'Etc/GMT+8')

swe <- crhm_output_new |> 
  select(datetime, forest_swe = SWE.1, open_swe = SWE.2) |> 
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

