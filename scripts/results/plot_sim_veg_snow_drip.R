# Script to compute the annual fraction of canopy snow melt/drip Tricky to do
# this exactly as cansnobal combines rainfall and snowmeltwater into the same
# reservoir and thus cannot partition directly. Current work around is to
# compare the melt rate in CP25 to the SUnloadH20 rate in E10. This is apples to
# apples since E10 does not stor liq water in the canopy but differs from how
# CP25 is actually implemented.

# fraction of snowfall that sublimates 

frac_melt_df <- all_sites_mods |> 
  mutate(year = year(datetime)) |> 
  filter(var %in% c('melt', 'snow')) |> 
  pivot_wider(names_from = var) |> 
  group_by(model, year, station) |> 
  summarise(del_sf = sum(snow),
            del_melt = sum(melt),
            frac_melt = del_melt/del_sf) 

saveRDS(frac_melt_df, 'data/manuscript-dfs/frac-drip.rds')

ggplot(frac_melt_df, aes(year, frac_melt, colour = model)) +
  geom_point() +
  facet_grid(~station)

ggplot(frac_melt_df, aes(model, frac_melt, colour = model)) +
  geom_boxplot() +
  facet_grid(~station) +
  labs(y = 'Fraction of Snowfall Melted (-)',
       x = element_blank()) +
  theme(legend.position = 'none') +
  scale_colour_manual(
    values = c(#"Observed_clearing" = "blue",
      "E10" = "salmon",
      "CP25" = "dodgerblue"))

ggsave(
  'figs/final/figure8.png',
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
  height = 3.5
)
