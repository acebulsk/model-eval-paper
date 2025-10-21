# Script to plot cumulative atmospheric snowfall vs subcanopy SWE obs

all_sites_hru_sf <- all_sites_mods |> 
  filter(var == 'snow', model == 'CP25') |>
  mutate(wat_yr = weatherdash::wtr_yr(datetime),
         value = ifelse(is.na(value), 0, value)) |> 
  group_by(station, wat_yr) |> 
  mutate(cml_sf = cumsum(value))

ggplot(all_sites_hru_sf, aes(datetime, cml_sf)) +
  geom_point(
    data = all_sites_obs,
    aes(x = datetime, y = obs_swe),
    colour = "red",    # set points to red
    inherit.aes = FALSE
  ) +
  geom_line(colour = "black") +  # set line to black
  facet_wrap(~station, nrow = 4, scales = "free") +
  labs(
    y = "Snow Water Equivalent (mm)",
    x = NULL,
    colour = NULL
  ) +
  theme(legend.position = "bottom")

ggsave(
  'figs/final/figure3_a.png',
  device = png,
  width = 8.5,
  height = 6
)

plotly::ggplotly(
  
)

# frac of snowfall as subcanopy SWE

all_sites_obs |> 
  mutate()
  group_by()
  
all_sites_hru_sf <- all_sites_mods |> 
  left_join(all_sites_obs) |> 
  filter(var == 'snow', model == 'CP25') |>
  mutate(wat_yr = weatherdash::wtr_yr(datetime),
         value = ifelse(is.na(value), 0, value)) |> 
  group_by(station, wat_yr) |> 
  mutate(cml_sf = cumsum(value)) |> 
  summarise(pk_swe = max(obs_swe, na.rm = TRUE),
            cml_sf_at_pk_swe = cml_sf[which.max(obs_swe)],
            frac_sf = pk_swe/cml_sf_at_pk_swe) 

ggplot(all_sites_hru_sf, aes(station, 1-frac_sf)) +
  geom_boxplot() +
  labs(
    y = 'Fraction of Snowfall Losses at Peak SWE (-)',
    x = element_blank()
  )

ggsave(
  'figs/final/figure3_b.png',
  device = png,
  width = 8.5,
  height = 6
)

all_sites_hru_sf_avg <-  all_sites_hru_sf |> 
  group_by(station) |> 
  summarise(frac_sf = mean(frac_sf),
            frac_loss = 1-frac_sf)

saveRDS(all_sites_hru_sf_avg, 'data/manuscript-dfs/frac_snowfall_as_pk_swe_w_losses.rds')
  