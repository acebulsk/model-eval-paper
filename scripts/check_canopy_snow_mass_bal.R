# LOAD DATA ----

## updated crhm (cansnobal) ----

prj <- "fortress_powerline_clearing_snowsurveytransect_cansnobal"
run_tag_updt <- "v1_r3"

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
    cml_snow_in = cumsum(hru_snow.1),
    snow_out = delunld_int.1 + delmelt_veg_int.1 + throughfall_snow.1 - delsub_veg_int.1,
    cml_snow_out = cumsum(snow_out)
    # cml_net_p = cumsum(net_p.1)
    ) |> 
  select(datetime, cml_snow_in, cml_snow_out) |> 
  mutate(group = 'new')

crhm_output_new_mb_long <- crhm_output_new_mb |> 
  pivot_longer(c(cml_snow_in, cml_snow_out))

ggplot(crhm_output_new_mb_long, aes(datetime, value, colour = name)) + geom_line()
plotly::ggplotly()

## baseline crhm ----

prj <- "fortress_powerline_clearing_snowsurveytransect_baseline"
run_tag_base <- "v1_r4"

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
    cml_snow_in = cumsum(hru_snow.1),
    snow_out = SUnload.1 + SUnload_H2O.1 + direct_snow.1 + Subl_Cpy.1,
    cml_snow_out = cumsum(snow_out)
    # cml_net_p = cumsum(net_p.1)
  ) |> 
  select(datetime, cml_snow_in, cml_snow_out) |> 
  mutate(group = 'base')

crhm_output_base_mb_long <- crhm_output_base_mb |> 
  pivot_longer(c(cml_snow_in, cml_snow_out))

# PLOT NEW and BASE ---- 

plot_df <- rbind(crhm_output_new_mb_long,
                 crhm_output_base_mb_long)

ggplot(plot_df, aes(datetime, value, colour = name, linetype = group)) + geom_line()
