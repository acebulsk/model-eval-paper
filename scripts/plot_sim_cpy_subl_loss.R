# Script to compute the annual fraction of interception losses

# fraction of snowfall that sublimates 

frac_subl_df <- all_sites_mods |> 
  mutate(year = year(datetime)) |> 
  filter(var %in% c('subl', 'snow')) |> 
  pivot_wider(names_from = var) |> 
  group_by(model, year, station) |> 
  summarise(del_sf = sum(snow),
            del_subl = sum(subl),
            frac_subl = del_subl/del_sf) 

ggplot(frac_subl_df, aes(year, frac_subl, colour = model)) +
  geom_point() +
  facet_grid(~station)

ggplot(frac_subl_df, aes(model, frac_subl, colour = model)) +
  geom_boxplot() +
  facet_grid(~station) +
  labs(y = 'Annual Fraction of Snowfall Sublimated (-)',
       x = element_blank()) +
  theme(legend.position = 'none') +
  scale_colour_manual(
    values = c(#"Observed_clearing" = "blue",
      "E10" = "salmon",
      "CP25" = "dodgerblue"))

ggsave(
  paste0(
    'figs/crhm-analysis/crhm_subl_loss/crhm_cpy_snow_loss',
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
