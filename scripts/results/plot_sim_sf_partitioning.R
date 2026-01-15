# Script to combine snowfall partitioning figs into one plot
cols <- c("dodgerblue", 'grey', "#9467BD", "#F2B701", 'salmon')
options(ggplot2.discrete.fill = cols)
options(ggplot2.discrete.colour = cols)

# fraction of snowfall sublimated 

frac_subl_df <- all_sites_mods |> 
  mutate(wat_yr = weatherdash::wtr_yr(datetime)) |> 
  filter(var %in% c('subl', 'snow')) |> 
  pivot_wider(names_from = var) |> 
  group_by(model, wat_yr, station) |> 
  summarise(del_sf = sum(snow, na.rm = T),
            del_subl = sum(subl, na.rm = T),
            frac_of_total_sf = del_subl/del_sf,
            n = n(),
            name = 'Fraction Sublimated (-)') |> 
  filter(n > 1000) |> 
  select(model, wat_yr, station, name, frac_of_total_sf)

saveRDS(frac_subl_df, 'data/manuscript-dfs/frac-subl.rds')

# fraction of snowfall that unloads/throughfalls

frac_unld_df <- all_sites_mods |> 
  mutate(wat_yr = weatherdash::wtr_yr(datetime)) |> 
  filter(var %in% c('unld', 'snow', 'tf')) |> 
  pivot_wider(names_from = var) |> 
  group_by(model, wat_yr, station) |> 
  summarise(del_sf = sum(snow, na.rm = T),
            del_unld = sum(unld, na.rm = T),
            del_tf = sum(tf, na.rm = T),
            del_unld_tf = del_unld + del_tf,
            frac_of_total_sf = del_unld_tf/del_sf,
            n = n(),
          name = 'Fraction Unloaded + Throughfall (-)') |>
  filter(n > 1000) |> 
  select(model, wat_yr, station, name, frac_of_total_sf)

saveRDS(frac_unld_df, 'data/manuscript-dfs/frac-unld-tf.rds')

# fraction of snowfall that melts

frac_melt_df <- all_sites_mods |> 
  mutate(wat_yr = weatherdash::wtr_yr(datetime)) |> 
  filter(var %in% c('melt', 'snow')) |> 
  pivot_wider(names_from = var) |> 
  group_by(model, wat_yr, station) |> 
  summarise(del_sf = sum(snow, na.rm = T),
            del_melt = sum(melt, na.rm = T),
            frac_of_total_sf = del_melt/del_sf,
            n = n(),
          name = 'Fraction Drip (-)') |> 
  filter(n > 1000) |> 
  select(model, wat_yr, station, name, frac_of_total_sf)

saveRDS(frac_melt_df, 'data/manuscript-dfs/frac-drip.rds')

# plot 

rbind(frac_unld_df, frac_melt_df) |> 
  rbind(frac_subl_df) |> 
  mutate(name = factor(name, levels = c('Fraction Sublimated (-)', 'Fraction Unloaded + Throughfall (-)', 'Fraction Drip (-)'))) |> 
  ggplot(aes(model, frac_of_total_sf, colour = model)) +
  geom_boxplot() +
    facet_grid(
    rows = vars(name),
    cols = vars(station),
    switch = "y"
  ) +
  labs(
    y = NULL,
    x = 'Model'
  ) +
  scale_colour_manual(
    values = c(
      "E10"  = "salmon",
      "CP25" = "dodgerblue"
    )
  ) +
  theme(
    legend.position = "none",

    # remove background ONLY for row strips (name)
    strip.background.y = element_blank(),

    # keep background for column strips (station)
    strip.background.x = element_rect(fill = "grey85", colour = NA),

    strip.placement = "outside",
    strip.text.y.left = element_text(size = 11)
  )

ggsave(
  'figs/final/figure12.png',
  # paste0(
  #   'figs/crhm-analysis/crhm_proc_diag/crhm_cpy_snow_melt',
  #   # '_',
  #   # run_tag_updt,
  #   '_',
  #   format(Sys.time(), "%Y-%m-%d_%H-%M-%S"),
  #   '.png'
  # ),
  device = png,
  width = 8.5,
  height = 8
)

ggsave(
  '../phd-thesis/chapters/05-model-eval-paper/figs/final/figure12.png',
  # paste0(
  #   'figs/crhm-analysis/crhm_proc_diag/crhm_cpy_snow_melt',
  #   # '_',
  #   # run_tag_updt,
  #   '_',
  #   format(Sys.time(), "%Y-%m-%d_%H-%M-%S"),
  #   '.png'
  # ),
  device = png,
  width = 8.5,
  height = 8
)

# total snow dispositioning

del_sf_df <- all_sites_mods |> 
  mutate(wat_yr = weatherdash::wtr_yr(datetime)) |> 
  filter(
    var == 'snow',
    model == 'CP25'
  ) |> 
  group_by(wat_yr, station, climate) |> 
  summarise(del_value = sum(value, na.rm = T),
            n = n(),
            name = 'Snowfall') |> 
  filter(n > 1000) |> 
  select(wat_yr, station, name, climate, del_value)

del_subl_df <- all_sites_mods |> 
  mutate(wat_yr = weatherdash::wtr_yr(datetime)) |> 
  filter(
    var == 'subl',
    model == 'CP25'
  ) |> 
  group_by(wat_yr, station, climate) |> 
  summarise(del_value = sum(value, na.rm = T),
            n = n(),
            name = 'Canopy Snow Sublimation') |> 
  filter(n > 1000) |> 
  select(wat_yr, station, name, climate, del_value)

del_unld_df <- all_sites_mods |> 
  mutate(wat_yr = weatherdash::wtr_yr(datetime)) |> 
  filter(
    var == 'unld',
    model == 'CP25'
  ) |> 
  group_by(wat_yr, station, climate) |> 
  summarise(del_value = sum(value, na.rm = T),
            n = n(),
            name = 'Unloading') |> 
  filter(n > 1000) |> 
  select(wat_yr, station, name, climate, del_value)

del_tf_df <- all_sites_mods |> 
  mutate(wat_yr = weatherdash::wtr_yr(datetime)) |> 
  filter(
    var == 'tf',
    model == 'CP25'
  ) |> 
  group_by(wat_yr, station, climate) |> 
  summarise(del_value = sum(value, na.rm = T),
            n = n(),
            name = 'Throughfall') |> 
  filter(n > 1000) |> 
  select(wat_yr, station, name, climate, del_value)

# del_unld_df <- all_sites_mods |> 
#   mutate(wat_yr = weatherdash::wtr_yr(datetime)) |> 
#   filter(
#     var %in% c('unld', 'tf'),
#     model == 'CP25'
#   ) |> 
#   pivot_wider(names_from = var) |> 
#   group_by(wat_yr, station) |> 
#   summarise(
#             del_unld = sum(unld, na.rm = T),
#             del_tf = sum(tf, na.rm = T),
#             del_value = del_unld + del_tf,
#             n = n(),
#             name = 'Unloading + Throughfall') |> 
#   filter(n > 1000) |> 
#   select(wat_yr, station, name, del_value)

del_melt_df <- all_sites_mods |> 
  mutate(wat_yr = weatherdash::wtr_yr(datetime)) |> 
  filter(
    var == 'melt',
    model == 'CP25'
  ) |> 
  group_by(wat_yr, station, climate) |> 
  summarise(del_value = sum(value, na.rm = T),
            n = n(),
            name = 'Canopy Snow Drip') |> 
  filter(n > 1000) |> 
  select(wat_yr, station, name, climate, del_value)

plot_df <- rbind(del_unld_df, del_melt_df) |> 
  rbind(del_subl_df) |> 
  rbind(del_sf_df) |> 
  rbind(del_tf_df) |> 
  mutate(name = factor(name, levels = c('Snowfall', 'Throughfall', 'Unloading', 'Canopy Snow Sublimation', 'Canopy Snow Drip'))) |> 
  ungroup()

plot_df|> 
  # mutate(name = factor(name, levels = c('Fraction Sublimated (-)', 'Fraction Unloaded + Throughfall (-)', 'Fraction Melted (-)'))) |> 
  ggplot(aes(station, del_value, colour = name)) +
  geom_boxplot() +
  # facet_grid(station) +
  labs(y = NULL,
       x = element_blank()) 

plot_df|> 
  group_by(climate, name) |> 
  summarise(del_value = mean(del_value)) |> 
  ggplot(aes(climate, del_value, fill = name)) +
  geom_bar(stat = 'identity', position = 'dodge') +
  # facet_grid(station) +
  labs(
       y = expression(Mean~Water~Equivalent~(kg ~ m^{-2} ~ yr^{-1})),
       x = 'Climate (station)',
      fill = element_blank()) +
  theme(legend.position = 'bottom')

ggsave(
  'figs/final/figure13.png',
  # paste0(
  #   'figs/crhm-analysis/crhm_proc_diag/crhm_cpy_snow_melt',
  #   # '_',
  #   # run_tag_updt,
  #   '_',
  #   format(Sys.time(), "%Y-%m-%d_%H-%M-%S"),
  #   '.png'
  # ),
  device = png,
  width = 8.5,
  height = 7
)
ggsave(
  '../phd-thesis/chapters/05-model-eval-paper/figs/final/figure13.png',
  # paste0(
  #   'figs/crhm-analysis/crhm_proc_diag/crhm_cpy_snow_melt',
  #   # '_',
  #   # run_tag_updt,
  #   '_',
  #   format(Sys.time(), "%Y-%m-%d_%H-%M-%S"),
  #   '.png'
  # ),
  device = png,
  width = 8.5,
  height = 7
)
