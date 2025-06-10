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
  select(datetime, new = m_s_veg.1)

# ggplot(crhm_output_new_cpy_snow, aes(datetime, hru_u.1)) + geom_line()
# plotly::ggplotly()

## baseline crhm ----
prj <- "fortress_powerline_clearing_snowsurveytransect_baseline"
run_tag_base <- "new2"

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
  select(datetime, base = Snow_load.1)

# PLOT NEW and BASE ---- 

plot_df <- left_join(crhm_output_new_cpy_snow,
                     crhm_output_base_cpy_snow,
                               by = 'datetime')

# Plot subcanopy SWE
plot_df |> 
  # rename(Observed = SWE,
  #        Simulated = SWE.1) |> 
  pivot_longer(
    !datetime
  ) |> 
  ggplot(aes(x = datetime, y = value, colour = name)) +
  geom_line() 

plotly::ggplotly()
