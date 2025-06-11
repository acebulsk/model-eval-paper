# Check russell Upper Steph obs vs mod temp/precip
us_obs <- CRHMr::readObsFile('crhm/obs/russell/2006-2007UpperStephanieA.OBS',
                             timezone = 'Etc/GMT+8') |>
  select(datetime, temp = t.1, precip = p.1, wind = u.1) |> 
  pivot_longer(!datetime) |> 
  mutate(group = 'obs')

prj <- "russell_upper_steph_forest_snowsurveytransect_baseline_ac"
run_tag_updt <- "r4"

us_mod2 <- read_crhm_obs(path, prj, run_tag_updt, 'Etc/GMT+8') |>
  select(datetime, temp = hru_t.1, precip = hru_p.1, wind = hru_u.1) |> 
  pivot_longer(!datetime) |> 
  mutate(group = 'crhm_out')

plotdf <- rbind(us_obs, us_mod2) #|> rbind(us_mod2)

ggplot(plotdf, aes(datetime, value, colour = group)) + geom_line() +
  facet_grid(rows = vars(name))
plotly::ggplotly()
