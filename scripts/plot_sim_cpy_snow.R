# Plot baseline vs new canopy snow

# SETUP ----
library(tidyverse)

all_sites_cpy_swe <- all_sites_mods |> 
  filter(var == 'cpy_swe')

# Plot canopy snow load

all_sites_cpy_swe |> 
  ggplot(aes(x = datetime, y = value, colour = model, group = model)) +
  geom_line() +
  facet_wrap(~station, nrow = 4, scales = 'free') +
  scale_colour_manual(
    values = c(#"Observed_clearing" = "blue",
      "E10" = "salmon",
      "CP25" = "dodgerblue"),
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
  paste0(
    'figs/crhm-analysis/crhm_cpy_swe/crhm_cpy_swe_baseline_vs_cansnobal_',
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

plotly::ggplotly()

# Plot canopy snow load select periods
wc_fm_mc_start <- as.Date('2016-09-01')
wc_fm_mc_end <- as.Date('2017-06-15')

rc_start <- as.Date('2006-09-01')
rc_end <- as.Date('2007-06-15')

all_sites_cpy_swe |> 
  filter((datetime >= fm_mc_start & datetime <= fm_mc_end & station %in% c('Marmot - Upper Forest', 'Fortress - Powerline Forest', 'Wolf Creek - Forest')) |
         (datetime >= rc_start & datetime <= rc_end & station == 'Russell - US Old Growth')
         ) |> 
  ggplot(aes(x = datetime, y = value, colour = model, group = model)) +
  geom_line() +
  facet_wrap(~station, nrow = 4, scales = 'free_x') +
  scale_colour_manual(
    values = c(#"Observed_clearing" = "blue",
      "E10" = "salmon",
      "CP25" = "dodgerblue"),
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
  paste0(
    'figs/crhm-analysis/crhm_cpy_swe/crhm_cpy_swe_select_yrs_baseline_vs_cansnobal_',
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

plotly::ggplotly()

# fraction of year canopy covered with >x kg m-2 canopy snow

cpy_snow_th <- 2

frac_yr_cpy_snow <- all_sites_cpy_swe |> 
  filter(!is.na(value)) |> 
  mutate(year = year(datetime)) |> 
  group_by(year, model, station) |> 
  summarise(
    count_cpy_snow = sum(value > cpy_snow_th),
    frac_cpy_snow = count_cpy_snow / n()
  )

ggplot(frac_yr_cpy_snow, aes(model, frac_cpy_snow, colour = model)) +
  geom_boxplot() +
  facet_grid(~station) +
  labs(y = expression(Fraction~of~Year~Canopy~Load~">"~2~(kg~m^{-2})),
       x = element_blank()) +
  theme(legend.position = 'none') +
  scale_colour_manual(
    values = c(#"Observed_clearing" = "blue",
      "E10" = "salmon",
      "CP25" = "dodgerblue"))

ggsave(
  paste0(
    'figs/crhm-analysis/crhm_cpy_swe/crhm_frac_canopy_load_grtrthan_baseline_vs_cansnobal_',
    # '_',
    # run_tag_updt,
    '_',
    format(Sys.time(), "%Y-%m-%d_%H-%M-%S"),
    '.png'
  ),
  device = png,
  width = 8.5,
  height = 3.5
)

# mean canopy load per year

mean_ann_cpy_load <- all_sites_cpy_swe |> 
  mutate(year = year(datetime)) |> 
  group_by(year, model, station) |> 
  summarise(
    mean_load = mean(value, na.rm = T)
  )

ggplot(mean_ann_cpy_load, aes(model, mean_load, colour = model)) +
  geom_boxplot() +
  facet_grid(~station) +
  labs(y = expression(Mean~Annual~Canopy~Load~(kg~m^{-2})),
       x = element_blank()) +
  theme(legend.position = 'none') +
  scale_colour_manual(
    values = c(#"Observed_clearing" = "blue",
      "E10" = "salmon",
      "CP25" = "dodgerblue"))

ggsave(
  paste0(
    'figs/crhm-analysis/crhm_cpy_swe/crhm_mean_annual_cpyload_baseline_vs_cansnobal_',
    # '_',
    # run_tag_updt,
    '_',
    format(Sys.time(), "%Y-%m-%d_%H-%M-%S"),
    '.png'
  ),
  device = png,
  width = 8.5,
  height = 3.5
)
