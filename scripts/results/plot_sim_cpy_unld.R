# Script to show annual frac of snow that is unloaded out of the canopy to the
# ground. Not apples to apples comparison as HP98 alg has significant unloading
# in the I/P max snow capacity paramaterisations.

# fraction of snowfall that sublimates 

frac_unld_df <- all_sites_mods |> 
  mutate(year = year(datetime)) |> 
  filter(var %in% c('unld', 'snow', 'tf')) |> 
  pivot_wider(names_from = var) |> 
  group_by(model, year, station) |> 
  summarise(del_sf = sum(snow),
            del_unld = sum(unld),
            del_tf = sum(tf),
            del_unld_tf = del_unld + del_tf,
            frac_unld_tf = del_unld_tf/del_sf) 

saveRDS(frac_unld_df, 'data/manuscript-dfs/frac-unld-tf.rds')

ggplot(frac_unld_df, aes(year, frac_unld_tf, colour = model)) +
  geom_point() +
  facet_grid(~station)

ggplot(frac_unld_df, aes(model, frac_unld_tf, colour = model)) +
  geom_boxplot() +
  facet_grid(~station) +
  labs(y = 'Fraction of Snowfall Unloaded + Throughfall (-)',
       x = element_blank()) +
  theme(legend.position = 'none') +
  scale_colour_manual(
    values = c(#"Observed_clearing" = "blue",
      "E10" = "salmon",
      "CP25" = "dodgerblue"))

ggsave(
  'figs/final/figure7.png',
  # paste0(
  #   'figs/crhm-analysis/crhm_proc_diag/crhm_cpy_snow_unld',
  #   # '_',
  #   # run_tag_updt,
  #   '_',
  #   format(Sys.time(), "%Y-%m-%d_%H-%M-%S"),
  #   '.png'
  # ),
  device = png,
  width = 8.5,
  height = 3.5
)
