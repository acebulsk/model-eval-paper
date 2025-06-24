# SETUP ----
library(tidyverse)
source('scripts/00-setup.R')

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
  width = 12,
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
