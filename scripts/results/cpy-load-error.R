# Script to compute error in canopy load

## Marmot ----

# obs cpy load from Cob's thesis

mc_cpy_load_sim_js <- marmot_obs_sim_swe |> 
  filter(var == 'cpy_swe') |> 
  select(datetime, var, value, model) |> 
  filter(
    datetime >= '2018-12-14',
    #datetime <= '2019-05-10' # for no influence of mixed rain/snow events
    datetime <= '2019-06-19') |> 
  mutate(year = '2018-2019')

mc_cpy_load_sim_js |> 
  rbind(mc_cpy_load_obs_js) |> 
  ggplot(aes(x = datetime, y = value, colour = model, group = model)) +
  geom_line() +
  scale_colour_manual(
    values = c(#"Observed_clearing" = "blue",
               "E10" = "salmon",
               "CP25" = "dodgerblue",
               "Obs" = "black"),
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
  ylab(expression(Canopy~Load~(kg~m^{-2}))) +
  xlab(element_blank()) +
  theme(legend.position = 'bottom')

plotly::ggplotly(mc_cpy_load_sim_js |> 
  rbind(mc_cpy_load_obs_js) |> 
  ggplot(aes(x = datetime, y = value, colour = model, group = model)) +
  geom_line())

ggsave(
  'figs/final/figure12.png',
  # paste0(
  #   'figs/crhm-analysis/crhm_swe_vs_snow_survey/crhm_swe_vs_snow_survey_annual_peak',
  #   # '_',
  #   # run_tag_updt,
  #   '_',
  #   format(Sys.time(), "%Y-%m-%d_%H-%M-%S"),
  #   '.png'
  # ),
  device = png,
  width = 8.5,
  height = 6
)

# plot just spring events

mc_cpy_load_sim_js |> 
  rbind(mc_cpy_load_obs_js) |> 
  filter(datetime > '2019-05-14') |> 
  ggplot(aes(x = datetime, y = value, colour = model, group = model)) +
  geom_line() +
  scale_colour_manual(
    values = c(#"Observed_clearing" = "blue",
               "E10" = "salmon",
               "CP25" = "dodgerblue",
               "Obs" = "black"),
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
  ylab(expression(Canopy~Load~(kg~m^{-2}))) +
  xlab(element_blank()) +
  theme(legend.position = 'bottom')

ggsave(
  'figs/supplement/cpy_load_2019_spring_events.png',
  # paste0(
  #   'figs/crhm-analysis/crhm_swe_vs_snow_survey/crhm_swe_vs_snow_survey_annual_peak',
  #   # '_',
  #   # run_tag_updt,
  #   '_',
  #   format(Sys.time(), "%Y-%m-%d_%H-%M-%S"),
  #   '.png'
  # ),
  device = png,
  width = 8.5,
  height = 6
)

# Error table

obs_mod_js <- mc_cpy_load_sim_js |>
  pivot_wider(names_from = model) |>
  select(-var) |> 
  left_join(mc_cpy_load_obs_js |> select(datetime, Obs = value)) |>
  pivot_longer(c(E10, CP25)) |> 
  filter(!is.na(Obs)) |> 
  # mutate(year = weatherdash::wtr_yr(datetime)) |> 
  group_by(name) |> 
  mutate(diff = Obs - value) 

mc_cpy_load_obs_mod_err_tbl_js <- obs_mod_js |> 
  group_by(name) |> 
  summarise(
    year = '2018-2019',
    `Mean Bias` = mean(diff, na.rm = T),
    MAE = mean(abs(diff), na.rm = T),
    `RMS Error` = sqrt(mean(diff ^ 2, na.rm = T)),
    # NRMSE = `RMS Error` / (max(obs_swe, na.rm = TRUE) - min(obs_swe, na.rm = TRUE)),
    NRMSE = `RMS Error` / mean(Obs, na.rm = T),
    R = cor(Obs, value, use = 'complete.obs'),
    `r^2` = R^2,
    NSE = 1 - sum((Obs - value)^2, na.rm = TRUE) / sum((Obs - mean(Obs, na.rm = TRUE))^2), # NSE from dingman
    n = n()
  ) |> 
  mutate(across(`Mean Bias`:`r^2`, round, digits = 3)) |> 
  arrange(name)

mc_cpy_load_obs_mod_err_tbl_js

write.csv(mc_cpy_load_obs_mod_err_tbl_js, 'data/manuscript-dfs/cpy-load-error-marmot-js.csv')

# error for spring rain/snow events

rain_events <- c('2019-05-30', '2019-06-13')
rain_snow_events <- c('2019-05-15', '2019-05-23', '2019-06-06')

rain_snow_datetimes <- read_rds('data/marmot/cob-thesis-data/processed_ac/weighed_tree_kg_m2_zero_pre_post_cnpy_snow.rds') |> 
  select(datetime, event_id) |> 
  filter(event_id %in% rain_snow_events) 

rain_snow_stats <- mc_cpy_load_sim_js |> 
  rbind(mc_cpy_load_obs_js) |> 
  left_join(rain_snow_datetimes, by = 'datetime') |> 
  filter(event_id %in% rain_snow_events) |> 
  group_by(event_id, model) |> 
  summarise(
    peak_cpy_load = max(value)
  ) |>   pivot_wider(names_from = model, values_from = peak_cpy_load) |> 
  mutate(
    perc_err_CP25 = 100 * (CP25 - Obs) / Obs,
    perc_err_E10  = 100 * (E10 - Obs) / Obs
  )

saveRDS(rain_snow_stats, 'data/manuscript-dfs/cpy-load-rain-snow-stats.rds')

# fraction of year canopy covered with >x kg m-2 canopy snow

cpy_snow_th <- 2

mc_cpy_load_obs_js_hourly <- mc_cpy_load_obs_js |> 
  mutate(datetime = ceiling_date(datetime, 'hour')) |> 
  group_by(datetime, var, model, year) |> 
  summarise(value = max(value, na.rm = T),
            value = ifelse(is.infinite(value), NA, value))

frac_yr_cpy_snow <- mc_cpy_load_sim_js |>
  pivot_wider(names_from = 'model') |>
  left_join(mc_cpy_load_obs_js_hourly |> pivot_wider(names_from = 'model')) |>
  filter(!is.na(Obs)) |>
  pivot_longer(CP25:Obs) |> 
  group_by(model = name) |>
  summarise(
    total_count = n(),
    count_cpy_snow = sum(value > cpy_snow_th),
    frac_cpy_snow = count_cpy_snow / n()
  )

df_pct_change <- frac_yr_cpy_snow |>
  select(-count_cpy_snow) |> 
  pivot_wider(names_from = model, values_from = frac_cpy_snow) |>
  mutate(
    percent_change_cp25_e10 = ((CP25 - E10) / E10) * 100,
    percent_change_cp25_obs = ((CP25 - Obs) / Obs) * 100,
    percent_change_e10_obs = ((E10 - Obs) / Obs) * 100
)

saveRDS(df_pct_change, 'data/manuscript-dfs/frac-yr-cpy-snow-th-validation.rds')

# Plot obs sim canopy load, just assessing on JM's canopy load obs as cannot reproduce
#  the unloading values given in Table 5.1 using the unloading excel files but we can 
# reproduce the canopy load values

mc_cpy_load_sim_jm <- marmot_obs_sim_swe |> 
  filter(var == 'cpy_swe') |> 
  select(datetime, var, value, model) |> 
  filter(
    datetime >= '2007-02-01',
    datetime <= '2008-02-15') |> 
  mutate(year = '2007-2008')

mc_cpy_load_sim_jm |> 
  ggplot(aes(x = datetime, y = value, colour = model, group = model)) +
  geom_line() +
  geom_point(
    data = mc_cpy_load_obs_jm,
    aes(x = datetime, y = value, colour = model),
    inherit.aes = FALSE
  ) +
  scale_colour_manual(
    values = c(#"Observed_clearing" = "blue",
               "E10" = "salmon",
               "CP25" = "dodgerblue",
               "Obs" = "black"),
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
  ylab(expression(Canopy~Load~(kg~m^{-2}))) +
  xlab(element_blank()) +
  theme(legend.position = 'bottom')

ggsave(
  'figs/final/figure13.png',
  # paste0(
  #   'figs/crhm-analysis/crhm_swe_vs_snow_survey/crhm_swe_vs_snow_survey_annual_peak',
  #   # '_',
  #   # run_tag_updt,
  #   '_',
  #   format(Sys.time(), "%Y-%m-%d_%H-%M-%S"),
  #   '.png'
  # ),
  device = png,
  width = 8.5,
  height = 6
)

# Error table

obs_mod_jm <- mc_cpy_load_sim_jm |>
  pivot_wider(names_from = model) |>
  select(-var) |> 
  left_join(mc_cpy_load_obs_jm |> select(datetime, Obs = value)) |>
  pivot_longer(c(E10, CP25)) |> 
  filter(!is.na(Obs)) |> 
  # mutate(year = weatherdash::wtr_yr(datetime)) |> 
  group_by(name) |> 
  mutate(diff = Obs - value) 

mc_cpy_load_obs_mod_err_tbl_jm <- obs_mod_jm |> 
  group_by(name) |> 
  summarise(
    year = '2007-2008',
    `Mean Bias` = mean(diff, na.rm = T),
    MAE = mean(abs(diff), na.rm = T),
    `RMS Error` = sqrt(mean(diff ^ 2, na.rm = T)),
    # NRMSE = `RMS Error` / (max(obs_swe, na.rm = TRUE) - min(obs_swe, na.rm = TRUE)),
    NRMSE = `RMS Error` / mean(Obs, na.rm = T),
    R = cor(Obs, value, use = 'complete.obs'),
    `r^2` = R^2,
    NSE = 1 - sum((Obs - value)^2, na.rm = TRUE) / sum((Obs - mean(Obs, na.rm = TRUE))^2), # NSE from dingman
    n = n()
  ) |> 
  mutate(across(`Mean Bias`:`r^2`, round, digits = 3)) |> 
  arrange(name)

mc_cpy_load_obs_mod_err_tbl_jm

write.csv(mc_cpy_load_obs_mod_err_tbl_jm, 'data/manuscript-dfs/cpy-load-error-marmot-jm.csv')

# combine JS and JM thesis validations

mc_cpy_load_sim_jm |> 
  rbind(mc_cpy_load_obs_js) |> 
  rbind(mc_cpy_load_sim_js) |> 
  ggplot(aes(x = datetime, y = value, colour = model, group = model)) +
  geom_line(linewidth = 0.5, na.rm = FALSE) +
  geom_point(
    data = mc_cpy_load_obs_jm,
    aes(x = datetime, y = value, colour = model),
    inherit.aes = FALSE
  ) +
  scale_colour_manual(
    values = c(#"Observed_clearing" = "blue",
               "E10" = "salmon",
               "CP25" = "dodgerblue",
               "Obs" = "black"),
    # labels = c(
    #   "Observed" = "Observed-Clearing",
    #   "Simulated" = "Simulated-Forest"
    # ),
    name = "Legend"
  ) +
    scale_x_datetime(
    date_labels = "%b %Y",    # shows "Jan 2008"
    date_breaks = "2 month"   # adjust as needed (e.g., "3 months")
  ) +
  # guides(colour = guide_legend(override.aes = list(
  #   linetype = c(1, 1, 1, 0), # Line styles for the first two, none for points
  #   shape = c(NA, NA, NA, 16)  # Points only for "Snow Survey"
  # ))) +
  ylab(expression(Canopy~Load~(kg~m^{-2}))) +
  xlab(element_blank()) +
  theme(legend.position = 'bottom') +
  facet_wrap(~year, nrow = 2, scales = 'free_x')

ggsave(
  'figs/final/figure15.png',
  # paste0(
  #   'figs/crhm-analysis/crhm_swe_vs_snow_survey/crhm_swe_vs_snow_survey_annual_peak',
  #   # '_',
  #   # run_tag_updt,
  #   '_',
  #   format(Sys.time(), "%Y-%m-%d_%H-%M-%S"),
  #   '.png'
  # ),
  device = png,
  width = 8,
  height = 4.5
)

# avg over the years 

mc_cpy_load_obs_mod_err_tbl_avgyr <- obs_mod_jm |> 
  rbind(obs_mod_js) |> 
  group_by(name) |> 
  summarise(
    year = 'All',
    `Mean Bias` = mean(diff, na.rm = T),
    MAE = mean(abs(diff), na.rm = T),
    `RMS Error` = sqrt(mean(diff ^ 2, na.rm = T)),
    # NRMSE = `RMS Error` / (max(obs_swe, na.rm = TRUE) - min(obs_swe, na.rm = TRUE)),
    NRMSE = `RMS Error` / mean(Obs, na.rm = T),
    R = cor(Obs, value, use = 'complete.obs'),
    `r^2` = R^2,
    NSE = 1 - sum((Obs - value)^2, na.rm = TRUE) / sum((Obs - mean(Obs, na.rm = TRUE))^2), # NSE from dingman
    n = n()
  ) |> 
  mutate(across(`Mean Bias`:`r^2`, round, digits = 3)) |> 
  arrange(name)

mc_cpy_load_obs_mod_err_tbl <- rbind(mc_cpy_load_obs_mod_err_tbl_jm, mc_cpy_load_obs_mod_err_tbl_js) |> 
  rbind(mc_cpy_load_obs_mod_err_tbl_avgyr)

write.csv(mc_cpy_load_obs_mod_err_tbl, 'figs/final/table3.csv', row.names = F)

## Wolf Creek ----

# currently only have data for 2020-2021 and something seems wrong as the tree weigh only accumulates and does not register any ablation

## Fortress Mountain ----

# not really a fair comparison as model is currently run in the Powerline Transect
# so much more shade and reduced winds so we see an underestimation of the ablation
# rates in the new model simulations compared to the weighed tree

# fm_cpy_load_sim <- fortress_obs_sim_swe |> 
#   filter(var == 'cpy_swe') |> 
#   select(datetime, var, value, model) |> 
#   filter(
#     datetime >= '2021-10-01',
#     datetime <= '2023-07-01')

# fm_cpy_load_obs_sim <- rbind(fm_cpy_load_sim, fm_cpy_load_obs)

# ggplot(fm_cpy_load_obs_sim, aes(datetime, value, colour = model)) + geom_line()

# plotly::ggplotly()

# # Error table for fortress
# obs_mod <- fm_cpy_load_sim |>
#   pivot_wider(names_from = model) |>
#   select(-var) |> 
#   left_join(fm_cpy_load_obs |> select(datetime, Obs = value)) |>
#   pivot_longer(c(E10, CP25)) |> 
#   filter(Obs > 0) |> 
#   # mutate(year = weatherdash::wtr_yr(datetime)) |> 
#   group_by(name) |> 
#   mutate(diff = Obs - value) 

# fm_cpy_load_obs_mod_err_tbl <- obs_mod |> 
#   group_by(name) |> 
#   summarise(
#     `Mean Bias` = mean(diff, na.rm = T),
#     MAE = mean(abs(diff), na.rm = T),
#     `RMS Error` = sqrt(mean(diff ^ 2, na.rm = T)),
#     # NRMSE = `RMS Error` / (max(obs_swe, na.rm = TRUE) - min(obs_swe, na.rm = TRUE)),
#     NRMSE = `RMS Error` / mean(Obs, na.rm = T),
#     R = cor(Obs, value, use = 'complete.obs'),
#     `r^2` = R^2,
#     n = n()
#   ) |> 
#   mutate(across(`Mean Bias`:`r^2`, round, digits = 3)) |> 
#   arrange(name)

# fm_cpy_load_obs_mod_err_tbl