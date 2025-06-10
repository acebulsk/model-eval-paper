# Check russell Upper Steph obs vs mod solar
us_obs <- CRHMr::readObsFile('crhm/obs/russell/2006-2007UpperStephanieA.OBS', timezone = 'Etc/GMT+8') |> select(datetime, obs_in = Qsi.1) |> 
  pivot_longer(!datetime)

# modelled solar
prj <- "russell_upper_steph_forest_snowsurveytransect_cansnobal_mod_solar"
run_tag_updt <- "r2"

us_mod1 <- read_crhm_obs(path, prj, run_tag_updt, 'Etc/GMT+8') |> select(datetime, mod = QsiS_Var.1) |> pivot_longer(!datetime)

# crhm output from obs solar
prj <- "russell_upper_steph_forest_snowsurveytransect_cansnobal"
run_tag_updt <- "solar_check"

us_mod2 <- read_crhm_obs(path, prj, run_tag_updt, 'Etc/GMT+8') |> select(datetime, crhm_obs_out = QsiS_Var.1) |> pivot_longer(!datetime)

plotdf <- rbind(us_obs, us_mod1) |> rbind(us_mod2)

ggplot(plotdf, aes(datetime, value, colour = name)) + geom_line()
plotly::ggplotly()
