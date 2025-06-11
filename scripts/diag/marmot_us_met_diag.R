# Check russell Upper Steph obs vs mod temp/precip
us_obs <- CRHMr::readObsFile('crhm/obs/Marmot_Hourly_ArrayMetData_withT_g_1Oct05-30Sept24_update_3Jan2025.obs',
                             timezone = 'Etc/GMT+6') |>
  select(datetime, temp = t.5, precip = p.5, wind = u.8) |> 
  pivot_longer(!datetime) |> 
  mutate(group = 'obs')

prj <- "marmot_upper_forest_clearing_snowsurveytransect_cansnobal"
run_tag_updt <- "r2"

us_mod2 <- read_crhm_obs(path, prj, run_tag_updt, 'Etc/GMT+6') |>
  select(datetime, temp = hru_t.1, precip = hru_p.1, wind = hru_u.1) |> 
  pivot_longer(!datetime) |> 
  mutate(group = 'crhm_out')

plotdf <- rbind(us_obs, us_mod2) #|> rbind(us_mod2)

ggplot(plotdf, aes(datetime, value, colour = group)) + geom_line() +
  facet_grid(rows = vars(name))
plotly::ggplotly()
