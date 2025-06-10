# SETUP ----
library(tidyverse)
path <- "crhm/output/"

# LOAD DATA ----

## POWERLINE ---- 

### obs snow ----

snow_survey <- CRHMr::readObsFile(
  'data/fortress/snow_survey/Powerline_obs_SWE_2013-23.obs',
  timezone = 'Etc/GMT+6'
) |> select(datetime, obs_swe = Powerline_Forest.1)

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
run_tag_updt <- "v_4_0_r3"

crhm_output_new <- read_crhm_obs(path, prj, run_tag_updt, 'Etc/GMT+6') |> 
  select(datetime, Simulated_forest_new = SWE.1)

### baseline crhm ----
prj <- "fortress_powerline_clearing_snowsurveytransect_baseline"
run_tag_base <- "v2_w_snobal_bug"

crhm_output_base <- read_crhm_obs(
  path = path,
  prj = prj,
  runtag = run_tag_base,
  tz = 'Etc/GMT+6'
) |>
  select(datetime, Simulated_forest_base = SWE.1)

# ggplot(crhm_output_base, aes(datetime, SWE.1)) + 
#   geom_line()

### combine powerline ----

snowscale_and_new <- left_join(crhm_output_new,
                               snow_scale,
                               by = 'datetime') |>
  left_join(crhm_output_base) |> 
  mutate(station = 'Powerline Forest')

## WOLF CREEK ----

### Snow survey obs ---- 

snow_survey <- CRHMr::readObsFile(
  'data/wolf-creek/snow_survey/WolfCreek_Forest_observed_SWE_1999.obs',
  timezone = 'Etc/GMT+7'
) |> select(datetime, obs_swe = Forest_SWE.1)

### updated crhm (cansnobal) ----

prj <- "wolf_creek_forest_snowsurveytransect_cansnobal"
run_tag_updt <- "v_4_0_r5"

crhm_output_new <- read_crhm_obs(path, prj, run_tag_updt, 'Etc/GMT+6') |> 
  select(datetime, Simulated_forest_new = SWE.1, Simulated_open_new = SWE.2)

### baseline crhm ----

prj <- "wolf_creek_borland_example"
run_tag_updt <- "r1"

crhm_output_base <- read_crhm_obs(path, prj, run_tag_updt, 'Etc/GMT+6') |> 
  select(datetime, Simulated_forest_base = SWE.3)

### combine wolf ----

snowscale_and_new <- left_join(crhm_output_new,
                               crhm_output_base,
                               by = 'datetime') |>
  mutate(station = 'Wolf Creek Forest')

ggplot(snowscale_and_new |> pivot_longer(starts_with('Simulated_')), aes(datetime, value, colour = name)) +
  geom_line()


## RUSSELL CREEK ----

### Snow survey obs ---- 

snow_survey <- CRHMr::readObsFile(
  'data/russell-creek/2006-2007 Upper Stephanie-SWE.obs',
  timezone = 'Etc/GMT+8'
) |> select(datetime, obs_swe = Forest_SWE.8) |>  # 8 is USOG2
mutate(obs_swe = ifelse(obs_swe == 0, NA, obs_swe))

### updated crhm (cansnobal) ----

prj <- "russell_upper_steph_forest_snowsurveytransect_cansnobal"
run_tag_updt <- "r3"

crhm_output_new <- read_crhm_obs(path, prj, run_tag_updt, 'Etc/GMT+8') |> 
  select(datetime, Simulated_forest_new = SWE.1)

ggplot(crhm_output_new, aes(datetime, Simulated_forest_new)) + geom_line()

### baseline crhm ----

prj <- "wolf_creek_borland_example"
run_tag_updt <- "r1"

crhm_output_base <- read_crhm_obs(path, prj, run_tag_updt, 'Etc/GMT+6') |> 
  select(datetime, Simulated_forest_base = SWE.3)

### combine russell ----

snowscale_and_new <- crhm_output_new |>
  mutate(station = 'Russell Creek US Old Growth')

ggplot(snowscale_and_new |> pivot_longer(starts_with('Simulated_')), aes(datetime, value, colour = name)) +
  geom_line()

# PLOT NEW and Snow Scale ---- 

# SWE ---- 

# left_join(crhm_output_tree |> 
#             select(datetime, Snow_load.1_15min = Snow_load.1))

# Plot subcanopy SWE
snowscale_and_new |> 
  # rename(Observed = SWE,
  #        Simulated = SWE.1) |> 
  pivot_longer(
   !c(datetime, station)
  ) |> 
  ggplot(aes(x = datetime, y = value, colour = name)) +
  geom_line() +
  geom_point(
    data = snow_survey, 
    aes(x = datetime, y = obs_swe, colour = "Snow Survey"), 
    inherit.aes = FALSE
  ) +
  scale_colour_manual(
    values = c("Observed_clearing" = "blue", 
               "Simulated_forest_base" = "red", 
               "Simulated_forest_new" = "green", 
               "Snow Survey" = "black"),
    # labels = c(
    #   "Observed" = "Observed-Clearing",
    #   "Simulated" = "Simulated-Forest"
    # ),
    name = "Legend"
  ) +
  guides(colour = guide_legend(override.aes = list(
    linetype = c(1, 1, 1, 0), # Line styles for the first two, none for points
    shape = c(NA, NA, NA, 16)  # Points only for "Snow Survey"
  ))) +
  ylab(expression(Snow~Water~Equivalent~(kg~m^{-2}))) +
  xlab(element_blank()) +
  theme(legend.position = 'bottom')

ggsave(
  paste0(
    'figs/crhm-analysis/crhm_swe_vs_snow_survey/crhm_swe_vs_snow_survey_vs_snowscale_timeseries_',
    '_',
    run_tag_updt,
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

mod_snow_survey <- left_join(
  snow_survey,
  snowscale_and_new |> select(datetime, Baseline = Simulated_forest_base, Updated = Simulated_forest_new)
) |> 
  pivot_longer(c(Baseline, Updated))

ggplot(mod_snow_survey,
       aes(obs_swe, value, colour = name)) +
  geom_point() + geom_abline() +
  ylab(expression(Simulated~Snow~Water~Equivalent~(kg~m^{-2}))) +
  xlab(expression(Observed~Snow~Water~Equivalent~(kg~m^{-2}))) +
  theme(legend.title = element_blank())

ggsave(
  paste0(
    'figs/crhm-analysis/crhm_swe_vs_snow_survey/crhm_swe_vs_snow_survey_scatter_',
    '_',
    run_tag_updt,
    '_',
    format(Sys.time(), "%Y-%m-%d_%H-%M-%S"),
    '.png'
  ),
  device = png,
  width = 7,
  height = 6
)

# error table ----

mod_snow_survey_err_tbl <- mod_snow_survey |> 
  mutate(diff = obs_swe - value) |> 
  group_by(name) |> 
  summarise(
    `Mean Bias` = mean(diff, na.rm = T),
    MAE = mean(abs(diff), na.rm = T),
    `RMS Error` = sqrt(mean(diff ^ 2, na.rm = T)),
    # NRMSE = `RMS Error` / (max(obs_swe, na.rm = TRUE) - min(obs_swe, na.rm = TRUE)),
    NRMSE = `RMS Error` / mean(obs_swe, na.rm = T),
    R = cor(obs_swe, value),
    `r^2` = R^2) |> 
  mutate(across(`Mean Bias`:`r^2`, round, digits = 3))

write_csv(mod_snow_survey_err_tbl,
          paste0(
            'tbls/crhm-swe-vs-snowsurvey-errortbl',
            '_',
            run_tag_updt,
            '_',
            format(Sys.time(), "%Y-%m-%d_%H-%M-%S"),
            '.csv'
          ))
