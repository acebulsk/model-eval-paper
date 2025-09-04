# Plot sim canopy snow sublimation 

# Plot  baseline vs new canopy snow

# SETUP ----
library(tidyverse)

# LOAD DATA ----

## updated crhm (cansnobal) ----

prj <- "fortress_powerline_clearing_snowsurveytransect_cansnobal"
run_tag_updt <- "revert_obs"

path <- list.files(
  paste0(
    "crhm/output/",
    prj
  ),
  pattern = run_tag_updt,
  full.names = T
)

stopifnot(length(path) == 1)

crhm_output_new_cpy_snow <- CRHMr::readOutputFile(
  path,
  timezone = 'Etc/GMT+6') |> 
  mutate(cml_subl = cumsum(-delsub_veg_int.1),
         cml_net_p = cumsum(net_p.1)) |> 
  select(datetime, cml_subl, cml_net_p) |> 
  mutate(group = 'new')

crhm_output_new_cpy_snow_long <- crhm_output_new_cpy_snow |> 
  pivot_longer(c(cml_subl, cml_net_p))

## baseline crhm ----
prj <- "fortress_powerline_clearing_snowsurveytransect_baseline"
run_tag_base <- "r2_3"

path <- list.files(
  paste0(
    "crhm/output/",
    prj
  ),
  pattern = run_tag_base,
  full.names = T
)

stopifnot(length(path) == 1)

crhm_output_base_cpy_snow <- CRHMr::readOutputFile(
  path,
  timezone = 'Etc/GMT+6') |> 
  mutate(cml_subl = cumsum(Subl_Cpy.1),
         cml_net_p = cumsum(net_p.1)) |> 
  select(datetime, cml_subl, cml_net_p) |> 
  mutate(group = 'base')

crhm_output_base_cpy_snow_long <- crhm_output_base_cpy_snow |> 
  pivot_longer(c(cml_subl, cml_net_p))


# PLOT NEW and BASE ---- 

plot_df <- rbind(crhm_output_new_cpy_snow_long,
                     crhm_output_base_cpy_snow_long)

# Plot subcanopy SWE
plot_df |> 
  ggplot(aes(x = datetime, y = value, colour = group)) +
  geom_line() +
  facet_grid(rows = vars(name), scales = 'free_y')

plotly::ggplotly()
