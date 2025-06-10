# WARNING did not account for canopy snowmelt evap so new module does not perfectly balance
# Canopy snowmelts, then evaps, then runs off...



# LOAD DATA ----

## updated crhm (cansnobal) ----

prj <- "fortress_powerline_clearing_snowsurveytransect_cansnobal"
run_tag_updt <- "v2_cionco_IP_ft_profile_ablate"

path <- list.files(
  paste0(
    "crhm/output/",
    prj
  ),
  pattern = run_tag_updt,
  full.names = T
)

stopifnot(length(path) == 1)

crhm_output_new_mb <- CRHMr::readOutputFile(
  path,
  timezone = 'Etc/GMT+6') |> 
  mutate(
    cml_rain_in = cumsum(hru_rain.1),
    rain_int_tf = intercepted_rain.1 + throughfall_rain.1,
    cml_rain_int_tf = cumsum(rain_int_tf),
    rain_out = (deldrip_veg_int.1 - delmelt_veg_int.1) + throughfall_rain.1 + delevap_veg_int.1,
    cml_rain_out = cumsum(rain_out)
    # cml_net_p = cumsum(net_p.1)
  ) |> 
  select(datetime, cml_rain_in, cml_rain_out, cml_rain_int_tf) |> 
  mutate(group = 'new')

crhm_output_new_mb_long <- crhm_output_new_mb |> 
  pivot_longer(c(cml_rain_in, cml_rain_out, cml_rain_int_tf))

ggplot(crhm_output_new_mb_long, aes(datetime, value, colour = name)) + geom_line()
plotly::ggplotly()

## baseline crhm ----

prj <- "fortress_powerline_clearing_snowsurveytransect_baseline"
run_tag_base <- "v1_r8"

path <- list.files(
  paste0(
    "crhm/output/",
    prj
  ),
  pattern = run_tag_base,
  full.names = T
)

stopifnot(length(path) == 1)

crhm_output_base_mb <- CRHMr::readOutputFile(
  path,
  timezone = 'Etc/GMT+6') |> 
  mutate(
    cml_rain_in = cumsum(hru_rain.1),
    rain_out = (drip_cpy.1 - SUnload_H2O.1) + direct_rain.1 + intcp_evap.1,
    cml_rain_out = cumsum(rain_out)
    # cml_net_p = cumsum(net_p.1)
  ) |> 
  select(datetime, cml_rain_in, cml_rain_out) |> 
  mutate(group = 'base')

crhm_output_base_mb_long <- crhm_output_base_mb |> 
  pivot_longer(c(cml_rain_in, cml_rain_out))

# PLOT NEW and BASE ---- 

plot_df <- rbind(crhm_output_new_mb_long,
                 crhm_output_base_mb_long)

ggplot(plot_df, aes(datetime, value, colour = name, linetype = group)) + geom_line()
