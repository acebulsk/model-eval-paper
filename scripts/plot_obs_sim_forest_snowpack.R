# SETUP ----
library(tidyverse)
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
run_tag_updt <- "v_4_0_output"

crhm_output_new <- read_crhm_obs(path, prj, run_tag_updt, 'Etc/GMT+6') |> 
  select(datetime, CP25 = SWE.1)

### baseline crhm ----
prj <- "fortress_powerline_clearing_snowsurveytransect_baseline"
run_tag_base <- "v2_w_snobal_bug"

crhm_output_base <- read_crhm_obs(
  path = path,
  prj = prj,
  runtag = run_tag_base,
  tz = 'Etc/GMT+6'
) |>
  select(datetime, E10 = SWE.1)

# ggplot(crhm_output_base, aes(datetime, SWE.1)) + 
#   geom_line()

### combine powerline ----

fortress_obs_sim_swe <-   left_join(crhm_output_new, crhm_output_base, by = 'datetime') |> 
  mutate(station = 'Fortress - Powerline Forest')

snowscale_and_new <- left_join(crhm_output_new,
                               snow_scale,
                               by = 'datetime') |>
  left_join(crhm_output_base) |> 
  mutate(station = 'Powerline Forest')


## MARMOT UPPER FOREST ---- 

### obs snow ----

marmot_snow_survey <- CRHMr::readObsFile(
  'data/marmot/Marmot_UpperForest_UpperClearing_SWE_2008-23.obs',
  timezone = 'Etc/GMT+6'
) |> select(datetime, obs_swe = Upper_Forest.1) |>
  mutate(station = 'Marmot - Upper Forest')

### updated crhm (cansnobal) ----

prj <- "marmot_upper_forest_clearing_snowsurveytransect_cansnobal"
run_tag_updt <- "alpha_0.65"

crhm_output_new <- read_crhm_obs(path, prj, run_tag_updt, 'Etc/GMT+6') |> 
  select(datetime, CP25 = SWE.1)

### baseline crhm ----
prj <- "marmot_upper_forest_clearing_snowsurveytransect_baseline"
run_tag_base <- "r1"

crhm_output_base <- read_crhm_obs(
  path = path,
  prj = prj,
  runtag = run_tag_base,
  tz = 'Etc/GMT+6'
) |>
  select(datetime, E10 = SWE.1)

# ggplot(crhm_output_base, aes(datetime, SWE.1)) + 
#   geom_line()

### combine marmot ----

marmot_obs_sim_swe <- left_join(crhm_output_new,
                               crhm_output_base,
                               by = 'datetime') |>
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
run_tag_updt <- "new_obs5"

crhm_output_new <- read_crhm_obs(path, prj, run_tag_updt, 'Etc/GMT+8') |> 
  select(datetime, CP25 = SWE.1) |>
  mutate(station = 'Russell - US Old Growth')


### baseline crhm ----

prj <- "russell_upper_steph_forest_snowsurveytransect_baseline"
run_tag_updt <- "new_obs"

crhm_output_base <- read_crhm_obs(path, prj, run_tag_updt, 'Etc/GMT+8') |> 
  select(datetime, E10 = SWE.1)

### combine russell ----

russell_obs_sim_swe <- left_join(crhm_output_new, crhm_output_base, by = 'datetime') |>
  mutate(station = 'Russell - US Old Growth')

# PLOT ALL STATIONS ---- 

# SWE ---- 

all_sites_mods <- 
  rbind(marmot_obs_sim_swe, 
        russell_obs_sim_swe) |> 
  rbind(fortress_obs_sim_swe)

all_sites_obs <- 
  rbind(marmot_snow_survey, russell_snow_survey, fortress_snow_survey)

# Plot subcanopy SWE
all_sites_mods |> 
  # rename(Observed = SWE,
  #        Simulated = SWE.1) |> 
  pivot_longer(
   !c(datetime, station)
  ) |> 
  ggplot(aes(x = datetime, y = value, colour = name)) +
  geom_line() +
  geom_point(
    data = all_sites_obs,
    aes(x = datetime, y = obs_swe, colour = "Snow Survey"),
    inherit.aes = FALSE
  ) +
  facet_wrap(~station, nrow = 4, scales = 'free') +
  scale_colour_manual(
    values = c(#"Observed_clearing" = "blue",
               "E10" = "salmon",
               "CP25" = "dodgerblue",
               "Snow Survey" = "black"),
    # labels = c(
    #   "Observed" = "Observed-Clearing",
    #   "Simulated" = "Simulated-Forest"
    # ),
    name = "Legend"
  ) +
  # guides(colour = guide_legend(override.aes = list(
  #   linetype = c(1, 1, 1, 0), # Line styles for the first two, none for points
  #   shape = c(NA, NA, NA, 16)  # Points only for "Snow Survey"
  # ))) +
  ylab(expression(Snow~Water~Equivalent~(kg~m^{-2}))) +
  xlab(element_blank()) +
  theme(legend.position = 'bottom')

ggsave(
  paste0(
    'figs/crhm-analysis/crhm_swe_vs_snow_survey/crhm_swe_vs_snow_survey_vs_snowscale_timeseries_',
    # '_',
    # run_tag_updt,
    '_',
    format(Sys.time(), "%Y-%m-%d_%H-%M-%S"),
    '.png'
  ),
  device = png,
  width = 12,
  height = 6
)

# ggplotly... above breaks for somereason when trying plotly 

snowscale_and_new |> 
  # rename(Observed = SWE,
  #        Simulated = SWE.1) |> 
  pivot_longer(
    !datetime
  ) |> 
  ggplot(aes(x = datetime, y = value, colour = name)) +
  geom_line()

plotly::ggplotly()


# scatter ----

## SWE ----

obs_mod <- left_join(
  all_sites_mods, all_sites_obs, suffix = c('sim', 'obs')
) |> 
  pivot_longer(c(E10, CP25))

ggplot(obs_mod,
       aes(obs_swe, value, colour = name)) +
  geom_point() + geom_abline() +
  ylab(expression(Simulated~Snow~Water~Equivalent~(kg~m^{-2}))) +
  xlab(expression(Observed~Snow~Water~Equivalent~(kg~m^{-2}))) +
  theme(legend.title = element_blank()) +
  facet_wrap(~station, scales = 'free')

ggsave(
  paste0(
    'figs/crhm-analysis/crhm_swe_vs_snow_survey/crhm_swe_vs_snow_survey_scatter_',
    '_',
    format(Sys.time(), "%Y-%m-%d_%H-%M-%S"),
    '.png'
  ),
  device = png,
  width = 7,
  height = 6
)

# error table ----

mod_snow_survey_err_tbl <- obs_mod |> 
  mutate(diff = obs_swe - value) |> 
  group_by(name, station) |> 
  summarise(
    `Mean Bias` = mean(diff, na.rm = T),
    MAE = mean(abs(diff), na.rm = T),
    `RMS Error` = sqrt(mean(diff ^ 2, na.rm = T)),
    # NRMSE = `RMS Error` / (max(obs_swe, na.rm = TRUE) - min(obs_swe, na.rm = TRUE)),
    NRMSE = `RMS Error` / mean(obs_swe, na.rm = T),
    R = cor(obs_swe, value),
    `r^2` = R^2) |> 
  mutate(across(`Mean Bias`:`r^2`, round, digits = 3))
mod_snow_survey_err_tbl
write_csv(mod_snow_survey_err_tbl,
          paste0(
            'tbls/crhm-swe-vs-snowsurvey-errortbl',
            '_',
            format(Sys.time(), "%Y-%m-%d_%H-%M-%S"),
            '.csv'
          ))
