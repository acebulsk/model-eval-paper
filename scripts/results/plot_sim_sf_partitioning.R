# Script to combine snowfall partitioning figs into one plot

library(cowplot)

# fraction of snowfall sublimated 

frac_subl_df <- all_sites_mods |> 
  mutate(year = year(datetime)) |> 
  filter(var %in% c('subl', 'snow')) |> 
  pivot_wider(names_from = var) |> 
  group_by(model, year, station) |> 
  summarise(del_sf = sum(snow),
            del_subl = sum(subl),
            value = del_subl/del_sf,
          name = 'Fraction Sublimated (-)') |> 
  select(model, year, station, name, value)

saveRDS(frac_subl_df, 'data/manuscript-dfs/frac-subl.rds')

# fraction of snowfall that unloads/throughfalls

frac_unld_df <- all_sites_mods |> 
  mutate(year = year(datetime)) |> 
  filter(var %in% c('unld', 'snow', 'tf')) |> 
  pivot_wider(names_from = var) |> 
  group_by(model, year, station) |> 
  summarise(del_sf = sum(snow),
            del_unld = sum(unld),
            del_tf = sum(tf),
            del_unld_tf = del_unld + del_tf,
            value = del_unld_tf/del_sf,
          name = 'Fraction Unloaded + Throughfall (-)') |>
    select(model, year, station, name, value)

saveRDS(frac_unld_df, 'data/manuscript-dfs/frac-unld-tf.rds')

# fraction of snowfall that melts

frac_melt_df <- all_sites_mods |> 
  mutate(year = year(datetime)) |> 
  filter(var %in% c('melt', 'snow')) |> 
  pivot_wider(names_from = var) |> 
  group_by(model, year, station) |> 
  summarise(del_sf = sum(snow),
            del_melt = sum(melt),
            value = del_melt/del_sf,
          name = 'Fraction Melted (-)') |> 
  select(model, year, station, name, value)

saveRDS(frac_melt_df, 'data/manuscript-dfs/frac-drip.rds')

# plot 

rbind(frac_unld_df, frac_melt_df) |> 
  rbind(frac_subl_df) |> 
  mutate(name = factor(name, levels = c('Fraction Sublimated (-)', 'Fraction Unloaded + Throughfall (-)', 'Fraction Melted (-)'))) |> 
  ggplot(aes(model, value, colour = model)) +
  geom_boxplot() +
  facet_grid(name~station) +
  labs(y = NULL,
       x = element_blank()) +
  theme(legend.position = 'none') +
  scale_colour_manual(
    values = c(#"Observed_clearing" = "blue",
      "E10" = "salmon",
      "CP25" = "dodgerblue"))

ggsave(
  'figs/final/figure6.png',
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
