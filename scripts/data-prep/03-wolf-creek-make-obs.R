# Script to make CRHM met .obs for wolf creek 

wcf_met <- readRDS('data/wolf-creek/met/alex/wcf_gap_fill_ac.rds')
airport_precip <- readRDS('data/wolf-creek/met/alex/eccc_airport_qaqc_undercatch_corr_ac.rds')
  
# output to CRHM obs
  
  crhm_obs <- met_out |> 
  select(datetime, t = t.low, rh = rh.1, p = ppt, u = u.high) |> 
  filter(datetime > as.POSIXct('2015-10-01 00:00', tz = tz))

CRHMr::writeObsFile(crhm_obs, 'crhm/obs/wolf_creek_forest_hourly_2015_2024.obs')

crhm_obs |> 
  pivot_longer(!datetime) |> 
  ggplot(aes(datetime, value, colour = name)) +
  geom_line() + facet_grid(rows = vars(name), scales = 'free')

hist(crhm_obs$ppt[crhm_obs$ppt>0])
