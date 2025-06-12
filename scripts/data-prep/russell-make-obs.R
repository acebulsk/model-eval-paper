# Script to combine met at Russell Creek Upper stephanie over all years
# selected just for clearcut plot #2
# something wrong with all met on 2005/2006

start_fin_df <- data.frame(
  year = c('0506', '0607', '0708')
)

us_obs2 <- CRHMr::readObsFile('crhm/obs/russell/2006-2007UpperStephanieA.OBS', timezone = 'Etc/GMT+8') |> select(-starts_with('T_g'))

CRHMr::writeObsFile(us_obs2, 'crhm/obs/russell/2006-2007UpperStephanieA_ac.OBS')

# clear cut plot 2 to is hru #2
us_0506 <- 
  CRHMr::readObsFile('crhm/obs/russell/2005-2006 Upper Stephanie.OBS',
                              timezone = 'Etc/GMT+8') |> 
  select(datetime, ends_with(".2")) |> 
  select(datetime, t = t.2, rh = rh.2, p = p.2, u = u.2, Qsi = Qsi.2)
start_fin_df$start[start_fin_df$year == '0506'] <- min(us_0506$datetime) |> as.character()
start_fin_df$end[start_fin_df$year == '0506'] <- max(us_0506$datetime) |> as.character()

# clear cut plot 1 doesnt exist, cc2 is hru#1
us_0607 <- 
  CRHMr::readObsFile('crhm/obs/russell/2006-2007UpperStephanieA.OBS',
                     timezone = 'Etc/GMT+8') |> 
  select(datetime, ends_with(".1")) |> 
  select(datetime, t = t.1, rh = rh.1, p = p.1, u = u.1, Qsi = Qsi.1)
start_fin_df$start[start_fin_df$year == '0607'] <- min(us_0607$datetime) |> as.character()
start_fin_df$end[start_fin_df$year == '0607'] <- max(us_0607$datetime) |> as.character()

# clear cut plot 1 doesnt exist, cc2 is hru#1
# 2007-2008 Upper Stephanie-MOT Prec Wind CorrectA.OBS huge diff in precip
us_0708 <- 
  CRHMr::readObsFile('crhm/obs/russell/2007-2008 Upper Stephanie-MOT Prec Wind CorrectA.OBS',
                     timezone = 'Etc/GMT+8') |> 
  select(datetime, ends_with(".1"))|> 
  select(datetime, t = t.1, rh = rh.1, p = p.1, u = u.1, Qsi = Qsi.1)
start_fin_df$start[start_fin_df$year == '0708'] <- min(us_0708$datetime) |> as.character()
start_fin_df$end[start_fin_df$year == '0708'] <- max(us_0708$datetime) |> as.character()
saveRDS(start_fin_df, 'data/russell-creek/obs_start_fin_dates_by_year.rds')

us_gaps <- #rbind(us_0506, us_0607) |> 
  rbind(us_0607, us_0708)

us_gaps |> 
  mutate(wat_yr = weatherdash::wtr_yr(datetime)) |> 
  pivot_longer(!c(datetime, wat_yr)) |> 
  ggplot(aes(datetime, value, colour = name, group = wat_yr)) +
  geom_line() +
  facet_grid(rows = vars(name), scales = 'free')
plotly::ggplotly()

# create full dataframe

us_full <- data.frame(
  datetime = seq(min(us_0607$datetime), max(us_0708$datetime), by = 'hour')
) |> left_join(us_gaps) |> 
  fill(t:Qsi, .direction = 'down')

saveRDS(us_full,
        'crhm/obs/russell/russell_upper_stephanie_clearcut2_2005_2008_fillgapsdown.rds')

CRHMr::writeObsFile(us_full,
                    'crhm/obs/russell/russell_upper_stephanie_clearcut2_2005_2008_fillgapsdown_ac.obs')

us_full |> 
  pivot_longer(!datetime) |> 
  ggplot(aes(datetime, value, colour = name)) +
  geom_line() +
  facet_grid(rows = vars(name), scales = 'free')
