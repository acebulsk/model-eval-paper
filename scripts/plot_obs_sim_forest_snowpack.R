# SETUP ----
library(tidyverse)
source('scripts/00-setup.R')

options(ggplot2.discrete.fill= palette.colors(palette = "R4"))

# PLOT ALL STATIONS ---- 

# SWE ---- 

# Plot subcanopy SWE
all_sites_mods |> 
  filter(var == 'swe') |> 
  ggplot(aes(x = datetime, y = value, colour = model, group = model)) +
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
  width = 8.5,
  height = 6
)

# ggplotly... above breaks for somereason when trying plotly 

# snowscale_and_new |> 
#   # rename(Observed = SWE,
#   #        Simulated = SWE.1) |> 
#   pivot_longer(
#     !datetime
#   ) |> 
#   ggplot(aes(x = datetime, y = value, colour = name)) +
#   geom_line()
# 
# plotly::ggplotly()


# scatter ----

## SWE ----

obs_mod <- all_sites_mods |>
  filter(var == 'swe') |>
  pivot_wider(names_from = model) |>
  left_join(all_sites_obs, suffix = c('sim', 'obs')) |>
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

# swe summary table ----

swe_peak_ann_mean_smry_tbl <- 
  obs_mod |> 
  pivot_wider(names_from = name, values_from = value) |> 
  filter(!is.na(obs_swe)) |> 
  mutate(year = weatherdash::wtr_yr(datetime)) |> 
  rename(OBS = obs_swe) |> 
  pivot_longer(c(OBS, CP25, E10)) |> 
  group_by(name, year,  station) |> 
  summarise(
    mean_swe = mean(value),
    max_swe = max(value)
  ) |> pivot_longer(c(mean_swe, max_swe), names_to = 'stat_name', values_to = 'stat_val')

swe_peak_ann_mean_smry_tbl |> 
  filter(stat_name == 'mean_swe') |> 
  ggplot(aes(year, stat_val, fill = name)) +
  geom_bar(stat = 'identity', position = 'dodge') + 
  facet_wrap(~station, nrow = 4, scale = 'free') +
  ylab(expression(Mean~Annual~SWE~(kg~m^{-2}))) +
  xlab(element_blank()) +
  labs(fill = element_blank()) +
  scale_x_continuous(breaks = swe_peak_ann_mean_smry_tbl$year)

ggsave(
  paste0(
    'figs/crhm-analysis/crhm_swe_vs_snow_survey/crhm_swe_vs_snow_survey_annual_mean',
    # '_',
    # run_tag_updt,
    '_',
    format(Sys.time(), "%Y-%m-%d_%H-%M-%S"),
    '.png'
  ),
  device = png,
  width = 8.5,
  height = 6
)

swe_peak_ann_mean_smry_tbl |> 
  filter(stat_name == 'max_swe') |> 
  ggplot(aes(year, stat_val, fill = name)) +
  geom_bar(stat = 'identity', position = 'dodge') + 
  facet_wrap(~station, nrow = 4, scale = 'free') +
  ylab(expression(Peak~Annual~SWE~(kg~m^{-2}))) +
  xlab(element_blank()) +
  labs(fill = element_blank()) +
  scale_x_continuous(breaks = swe_peak_ann_mean_smry_tbl$year)

ggsave(
  paste0(
    'figs/crhm-analysis/crhm_swe_vs_snow_survey/crhm_swe_vs_snow_survey_annual_peak',
    # '_',
    # run_tag_updt,
    '_',
    format(Sys.time(), "%Y-%m-%d_%H-%M-%S"),
    '.png'
  ),
  device = png,
  width = 8.5,
  height = 6
)

swe_peak_ann_mean_minmax_allyrs_by_stn_tbl <- 
  swe_peak_ann_mean_smry_tbl |> 
  group_by(name, station) |> 
  summarise(max_mean_swe = max(mean_swe),
            min_mean_swe = min(mean_swe),
            max_peak_swe = max(max_swe),
            min_peak_swe = min(max_swe)
            )

swe_peak_ann_mean_minmax_allyrs_tbl <- 
  swe_peak_ann_mean_smry_tbl |> 
  group_by(name) |> 
  summarise(max_mean_swe = max(mean_swe),
            min_mean_swe = min(mean_swe),
            max_peak_swe = max(max_swe),
            min_peak_swe = min(max_swe)
  ) |> 
  filter(name == 'obs_swe')

saveRDS(swe_peak_ann_mean_minmax_allyrs_tbl, 'data/manuscript-dfs/obs_swe_stats_peak_ann_min_max.rds')

# error table ----

## annual mean error stats ----

ann_mod_snow_survey_err  <- 
  obs_mod |> 
  filter(!is.na(obs_swe)) |> 
  mutate(year = weatherdash::wtr_yr(datetime)) |> 
  group_by(year, name, station) |> 
  summarise(mean_obs = mean(obs_swe, na.rm = T),
            mean_sim = mean(value, na.rm = T)) |> 
  mutate(diff = mean_obs - mean_sim) 

ann_mod_snow_survey_err |> 
  ggplot(aes(name, y = diff)) +
  geom_boxplot() +
  facet_wrap(~station, scales = 'free') +
  ylab('Annual Mean Bias (mm)') +
  xlab(element_blank())

ann_mod_snow_survey_err_tbl <- ann_mod_snow_survey_err |> 
  group_by(name, station) |> 
  summarise(
    `Mean Bias` = mean(diff, na.rm = T),
    MAE = mean(abs(diff), na.rm = T),
    `RMS Error` = sqrt(mean(diff ^ 2, na.rm = T)),
    # NRMSE = `RMS Error` / (max(obs_swe, na.rm = TRUE) - min(obs_swe, na.rm = TRUE)),
    NRMSE = `RMS Error` / mean(mean_obs, na.rm = T),
    R = cor(mean_obs, mean_sim, use = 'complete.obs'),
    `r^2` = R^2) |> 
  mutate(across(`Mean Bias`:`r^2`, round, digits = 3)) |> 
  arrange(station)

  ann_mod_snow_survey_err_tbl
  
write_csv(ann_mod_snow_survey_err_tbl,
          paste0(
            'tbls/crhm-swe-vs-snowsurvey-errortbl-annual-mean',
            '_',
            format(Sys.time(), "%Y-%m-%d_%H-%M-%S"),
            '.csv'
          ))

## peak annual error stats ----

mod_snow_survey_err_tbl <- obs_mod |> 
  filter(!is.na(obs_swe)) |> 
  mutate(year = weatherdash::wtr_yr(datetime)) |> 
  group_by(year, name, station) |> 
  summarise(peak_obs = max(obs_swe, na.rm = T),
            peak_sim = max(value, na.rm = T)) |> 
  mutate(diff = peak_obs - peak_sim) |> 
  group_by(name, station) |> 
  summarise(
    `Mean Bias` = mean(diff, na.rm = T),
    MAE = mean(abs(diff), na.rm = T),
    `RMS Error` = sqrt(mean(diff ^ 2, na.rm = T)),
    # NRMSE = `RMS Error` / (max(peak_obs, na.rm = TRUE) - min(peak_obs, na.rm = TRUE)),
    NRMSE = `RMS Error` / mean(peak_obs, na.rm = T),
    R = cor(peak_obs, peak_sim, use = 'complete.obs'),
    `r^2` = R^2) |> 
  mutate(across(`Mean Bias`:`r^2`, round, digits = 3)) |> 
  arrange(station)
mod_snow_survey_err_tbl
write_csv(mod_snow_survey_err_tbl,
          paste0(
            'tbls/crhm-swe-vs-snowsurvey-errortbl',
            '_',
            format(Sys.time(), "%Y-%m-%d_%H-%M-%S"),
            '.csv'
          ))

## time-series error stats ----

mod_snow_survey_err_tbl <- obs_mod |> 
  mutate(diff = obs_swe - value) |> 
  group_by(name, station) |> 
  summarise(
    `Mean Bias` = mean(diff, na.rm = T),
    MAE = mean(abs(diff), na.rm = T),
    `RMS Error` = sqrt(mean(diff ^ 2, na.rm = T)),
    # NRMSE = `RMS Error` / (max(obs_swe, na.rm = TRUE) - min(obs_swe, na.rm = TRUE)),
    NRMSE = `RMS Error` / mean(obs_swe, na.rm = T),
    R = cor(obs_swe, value, use = 'complete.obs'),
    `r^2` = R^2) |> 
  mutate(across(`Mean Bias`:`r^2`, round, digits = 3)) |> 
  arrange(station)
mod_snow_survey_err_tbl

write_csv(mod_snow_survey_err_tbl,
          paste0(
            'tbls/crhm-swe-vs-snowsurvey-errortbl',
            '_',
            format(Sys.time(), "%Y-%m-%d_%H-%M-%S"),
            '.csv'
          ))
