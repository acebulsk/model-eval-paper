# Gap fill rh, temp, precip at Wolf Creek Forest Station (WCF)

# Using met post 2015 as precip seems to be more stable. Data comes in at 30 min from WCF. 
# Selected met is t/rh from low height, above canopy wind, and weighing gauge preicp
# low wind is also gap filled for use in the ppt undercatch correction
# Gap Fill Priority will be based on R2 between variables at Forest Tower and Airport
# NOTE: data from Rosy is in UTC

library(tidyverse)
library(CRHMr)
library(plotly)

sum_na <- function(x) {
  if (all(is.na(x))) {
    return(NA_real_)
  } else {
    return(sum(x, na.rm = TRUE))
  }
}

tz_in <- 'UTC' # tz from header meta is UTC
tz <- 'Etc/GMT+7' # whitehorse LST
max_gap_fill_linear <- 2
met_start_date <- as.POSIXct('2015-10-01 00:00:00', tz = tz) # weighing gauge precip data looks better after this date at  WC Forest 
met_end_date <- as.POSIXct('2024-10-01 00:00:00', tz = tz)
kmhr_to_ms <- 1000/3600 # 1000 m in a km and 3600 s in hr

# QC ---- 

### PRECIP ----

### MET ----
met_main <- met_main |> 
  select( # rename for strict crhmr functions
    datetime,
    t.low = TA_low,
    t.mid = TA_mid,
    t.high = TA_high,
    rh.low = RH_low,
    rh.mid = RH_mid,
    rh.high = RH_high,
    u.low = US_low,
    u.high = US_high
  ) |>
  left_join(precip) |> 
  mutate(datetime = ceiling_date(datetime, unit = '1 hour')) |> # ceiling ensures the timestamp corresponds to preeceeding records
  group_by(datetime) |> 
  summarise(
    t.low = mean(t.low, na.rm = TRUE),
    t.mid = mean(t.mid, na.rm = TRUE),
    t.high = mean(t.high, na.rm = TRUE),
    rh.low = mean(rh.low, na.rm = TRUE),
    rh.mid = mean(rh.mid, na.rm = TRUE),
    rh.high = mean(rh.high, na.rm = TRUE),
    u.low = mean(u.low, na.rm = TRUE),
    u.high = mean(u.high, na.rm = TRUE),
    ppt = sum_na(ppt)
  ) |> 
  filter(datetime >= met_start_date, datetime <= met_end_date) |> # precip data looks better after this date 
  as.data.frame() # crhmr fns do not work with tibbles

### Solar ----

# plan is to use solar 2 from the new tower/new sensor and then gap fill from
# the start date oct 1 2015 using solar 1 which has a slight bias so will also
# apply regression correction

# below applies rough thresholding QC and agg to hourly from 15 min

# qaqc values from @Fang2019 for marmot creek
# roc_th <- 1450 # W m-2 over 15 min this is higher than glob max so not going to find any spikes not currently applying this... 
glob_min <- 0
glob_max <- 1368 
flatline_timesteps <- 4 # 4 hrs at hourly timestep here

solar1_no_nan <- solar1 |> filter(!is.na(sw_in_14m))

n_over_max <- sum(solar1_no_nan$sw_in_14m > glob_max)
n_under_min <- sum(solar1_no_nan$sw_in_14m < glob_min)
solar1_no_nan$sw_in_14m[solar1_no_nan$sw_in_14m < glob_min] <- glob_min # most negatives are at night so this is valid
solar1_no_nan$sw_in_14m[solar1_no_nan$sw_in_14m > glob_max] <- NA

if(n_over_max > 0){
  warning(paste(n_over_max, 'values are over the shortwave in max threshold.'))
}

if(n_under_min > 0){
  warning(paste(n_under_min, 'values are under the shortwave in min threshold.'))
}

solar1_fltr <- data.frame(datetime = seq(
  solar_mod$datetime[1],
  tail(solar_mod$datetime, n = 1),
  900
)) |>  
  left_join(solar1_no_nan) |> 
  mutate(datetime = ceiling_date(datetime, unit = '1 hour')) |> # ceiling ensures the timestamp corresponds to preeceeding records
  group_by(datetime) |> 
  summarise(
    sw_in_14m = mean(sw_in_14m, na.rm = TRUE)
  ) |> as.data.frame()

solar2_no_nan <- solar2 |> filter(!is.na(sw_in_18m))

n_over_max <- sum(solar2_no_nan$sw_in_18m > glob_max)
n_under_min <- sum(solar2_no_nan$sw_in_18m < glob_min)
solar2_no_nan$sw_in_18m[solar2_no_nan$sw_in_18m < glob_min] <- glob_min # most negatives are at night so this is valid
solar2_no_nan$sw_in_18m[solar2_no_nan$sw_in_18m > glob_max] <- NA

if(n_over_max > 0){
  warning(paste(n_over_max, 'values are over the shortwave in max threshold.'))
}

if(n_under_min > 0){
  warning(paste(n_under_min, 'values are under the shortwave in min threshold.'))
}

solar2_fltr <- data.frame(datetime = seq(
  solar_mod$datetime[1],
  tail(solar_mod$datetime, n = 1),
  900
)) |>
  left_join(solar2_no_nan) |> 
  mutate(datetime = ceiling_date(datetime, unit = '1 hour')) |> # ceiling ensures the timestamp corresponds to preeceeding records
  group_by(datetime) |> 
  summarise(
    sw_in_18m = mean(sw_in_18m, na.rm = TRUE)
  ) |> as.data.frame()

# plot_solar <- rbind(solar2_fltr |> mutate(group = 'fltr') |> select(datetime, value = sw_in_18m, group) |> na.omit(),
#                     solar2 |> mutate(group = 'raw')|> select(datetime, value = sw_in_18m, group)) |> 
#   rbind(solar_mod |> mutate(group = 'mod') |> select(datetime, value = QsiS_Var.1, group))
# 
# ggplot(plot_solar, aes(datetime, value, colour = group, group = group)) + geom_line()
# 
# plotly::ggplotly()

## ECCC Stations ----

# stns <- weathercan::stations() |> filter(station_name %in% c('WHITEHORSE AUTO'),
                                         # interval == 'hour')
# # WHITEHORSE A, 
# wha <- weathercan::weather_dl(
#   stns$station_id,
#   start = '2015-01-01',
#   interval = 'hour',
#   time_disp = "UTC" # returned times are actually UTC, otherwise are returned as UTC even though in local time
# )
# saveRDS(wha, 'data/wolf-creek/met/eccc/weathercan_whitehorse_airport.rds')

wha <- wha_in |> 
  select(datetime = time, t.1 = temp, rh.1 = rel_hum, u.1 = wind_spd, precip.1 = precip_amt) |> 
  mutate(datetime = as.POSIXct(datetime, tz = tz),
         u.1 = u.1 * kmhr_to_ms) |> 
  filter(datetime >= met_start_date, datetime <= met_end_date) |> as.data.frame()

# ggplot(wha |> pivot_longer(!datetime), aes(datetime, value, colour = name)) +
#   geom_line() +
#   facet_grid(rows = vars(name), scales = 'free')
# ggplotly()

# check timezones aligned 

# wha_temp <- wha |> select(datetime, t = t.1) |> mutate(name = 'airport')
# wcf_temp <- met_main |> select(datetime, t = t.low) |> mutate(name = 'wcf')
# 
# rbind(wha_temp, wcf_temp) |> 
#   ggplot(aes(datetime, t, colour = name)) +
#   geom_point() 
# plotly::ggplotly()

# CHECK FILL PRIORITY (primary modelling variables) ----

# run regressions on variables to determine priority of fills 
# based on below relationships:
# AT fill is from mid, high, airport in that order
# RH is mid, airport
# Wind Speed is from Lower (R2 = 0.26), no relationship from the airport - should fill from other stations 
# Precip is no relationship, will direct infill as in Rasouli 2018

logfile <- 'crhm/logs/gap_fill_log.log'

## air temp ---- 

at_low_mid_reg <- CRHMr::regress(
  met_main,
  primary.columns =1,
  met_main,
  secondary.columns =2,
  logfile = logfile
)
at_low_mid_reg

at_low_hi_reg <- CRHMr::regress(
  met_main,
  primary.columns =1,
  met_main,
  secondary.columns =3,
  logfile = logfile
)
at_low_hi_reg

at_low_wha_reg <- CRHMr::regress(
  met_main,
  primary.columns =1,
  wha,
  secondary.columns =1,
  logfile = logfile#,
  # plot = T
)
at_low_wha_reg

## above canopy wind speed ---- 

u_high_low_reg <- CRHMr::regress(
  met_main,
  primary.columns =8,
  met_main,
  secondary.columns =7,
  logfile = logfile,
  forceOrigin = F
  # plot = T
)
u_high_low_reg
CRHMr::regress(
  met_main,
  primary.columns = 8,
  met_main,
  secondary.columns = 7,
  logfile = logfile,
  forceOrigin = F,
  plot = T
)

u_high_eccc_reg <- CRHMr::regress(
  met_main,
  primary.columns =8,
  wha,
  secondary.columns =3,
  logfile = logfile,
  forceOrigin = F
  # plot = T
)
u_high_eccc_reg

CRHMr::regress(
  met_main,
  primary.columns = 8,
  wha,
  secondary.columns =3,
  logfile = logfile,
  forceOrigin = F,
  plot = T
)

## RH ---- 

ea_main_low <- changeRHtoEa(met_main,
                            t.cols = 1,
                            rh.cols = 4,
                            logfile = logfile) |> select(datetime, ea.1 = ea.1)
ea_main_mid <- changeRHtoEa(met_main,
                            t.cols = 2,
                            rh.cols = 5,
                            logfile = logfile)  |> select(datetime, ea.1 = ea.1)

ea_eccc_mid <- changeRHtoEa(wha,
                            t.cols = 1,
                            rh.cols = 2,
                            logfile = logfile)  |> select(datetime, ea.1 = ea.1)

ea_low_mid_reg <- CRHMr::regress(
  ea_main_low,
  primary.columns =1,
  ea_main_mid,
  secondary.columns =1,
  forceOrigin = T,
  logfile = logfile#,
  # plot = T
)
ea_low_mid_reg

CRHMr::regress(
  ea_main_low,
  primary.columns =1,
  ea_main_mid,
  secondary.columns =1,
  forceOrigin = T,
  logfile = logfile,
  plot = T
)

ea_low_wha_reg <- CRHMr::regress(
  ea_main_low,
  primary.columns = 1,
  ea_eccc_mid,
  secondary.columns =1,
  forceOrigin = T,
  logfile = logfile,
  plot =F
)
ea_low_wha_reg

CRHMr::regress(
  ea_main_low,
  primary.columns = 1,
  ea_eccc_mid,
  secondary.columns =1,
  forceOrigin = T,
  logfile = logfile,
  plot = T
)

## Precip ----

CRHMr::regress(
  met_main,
  primary.columns =9,
  wha,
  secondary.columns =4,
  logfile = logfile
)

CRHMr::regress(
  met_main,
  primary.columns =9,
  wha,
  secondary.columns =4,
  logfile = logfile,
  plot = T
)

# PRIORITY on Undercatch Variables ---- 

## airport wind speed ---- 

u_wha_high_reg <- CRHMr::regress(
  wha,
  primary.columns = 3,
  met_main,
  secondary.columns = 8,
  logfile = logfile,
  forceOrigin = F
  # plot = T
)
u_wha_high_reg

CRHMr::regress(
  wha,
  primary.columns = 3,
  met_main,
  secondary.columns = 8,
  logfile = logfile,
  forceOrigin = F,
  plot = T
)

u_wha_low_reg <- CRHMr::regress(
  wha,
  primary.columns = 3,
  met_main,
  secondary.columns = 7,
  logfile = logfile,
  forceOrigin = F
  # plot = T
)
u_wha_low_reg

CRHMr::regress(
  wha,
  primary.columns = 3,
  met_main,
  secondary.columns = 7,
  logfile = logfile,
  forceOrigin = F,
  plot = T
)

## airport RH undercatch ---- 

ea_wha_low_reg <- CRHMr::regress(
  ea_eccc_mid,
  primary.columns = 1,
  ea_main_low,
  secondary.columns = 1,
  logfile = logfile,
  forceOrigin = F
  # plot = T
)
ea_wha_low_reg

CRHMr::regress(
  ea_eccc_mid,
  primary.columns = 1,
  ea_main_low,
  secondary.columns = 1,
  logfile = logfile,
  forceOrigin = F,
  plot = T
)

## airport air temp undercatch ---- 

at_wha_low_reg <- CRHMr::regress(
  wha,
  primary.columns = 1,
  met_main,
  secondary.columns = 1,
  logfile = logfile,
  forceOrigin = F
  # plot = T
)
at_wha_low_reg

CRHMr::regress(
  wha,
  primary.columns = 1,
  met_main,
  secondary.columns = 1,
  logfile = logfile,
  forceOrigin = F,
  plot = T
)

# Regressions for Solar ---- 

# treating solar2 (18m, new sensor) as primary and will fill starting Oct 1 2015
# by solar1 (14m old tower) and
# then crhm mod for now until other stations are
# made available by the whitehorse crew. all at hourly already

# regress new sensor against old
qsi_18_14 <- CRHMr::regress(
  solar2,
  primary.columns = 1,
  solar1,
  secondary.columns = 1,
  logfile = logfile,
  forceOrigin = T
  # plot = T
)
qsi_18_14

# 18 m / new sensor reads a bit lower likely sensor related ... 
CRHMr::regress(
  solar2,
  primary.columns = 1,
  solar1,
  secondary.columns = 1,
  logfile = logfile,
  forceOrigin = T,
  plot = T
)

# regress new sensor against modelled data
qsi_18_mod <- CRHMr::regress(
  solar2,
  primary.columns = 1,
  solar_mod,
  secondary.columns = 1,
  logfile = logfile,
  forceOrigin = T
  # plot = T
)
qsi_18_mod

# 18 m / new sensor reads a bit lower than modelled ... 
CRHMr::regress(
  solar2,
  primary.columns = 1,
  solar_mod,
  secondary.columns = 1,
  logfile = logfile,
  forceOrigin = T,
  plot = T
)

# GAP FILL SHORT GAPS WITH LINEAR INTERP ----

met_main_fill <- met_main |> 
  select(datetime, t.low, rh.low, u.high, ppt)

## Main met fill ----

### Air Temperature + Wind ----


at_u_fill_short <- CRHMr::interpolate(met_main_fill, 
                                  varcols = c(1, 3),
                                  methods = 'linear', 
                                  maxlength = max_gap_fill_linear)

at_still_have_gaps <- findGaps(at_u_fill_short, gapfile = 'crhm/logs/pwl_at_gaps.csv',
                               quiet = F, logfile = logfile)

### Relative Humidity ---- 

# can only fill on ea not RH
ea_fill_short <- CRHMr::interpolate(ea_main_low, 
                                         varcols = 1,
                                         methods = 'linear', 
                                         maxlength = max_gap_fill_linear)

ea_still_have_gaps <- findGaps(ea_fill_short, gapfile = 'crhm/logs/pwl_at_gaps.csv',
                               quiet = F, logfile = logfile)

## Solar ----

qsi_fill_short <- CRHMr::interpolate(solar2_fltr, 
                                      varcols = 1,
                                      methods = 'linear', 
                                      maxlength = max_gap_fill_linear)

qsi_still_have_gaps <- findGaps(qsi_fill_short, gapfile = 'crhm/logs/qsi_gaps.csv',
                               quiet = F, logfile = logfile)
                               
## Secondary fill vars ----

### wolf creek forest tower ----

met_sec_fill <- met_main |> 
  select(datetime, t.mid, t.high, rh.mid, u.low, u.high)

#  air temp + wind
at_u_sec_fill_short <- CRHMr::interpolate(met_sec_fill, 
                                      varcols = c(1, 2, 4, 5),
                                      methods = 'linear', 
                                      maxlength = max_gap_fill_linear)
# rh
ea_sec_fill_short <- CRHMr::interpolate(ea_main_mid, 
                                        varcols = 1,
                                        methods = 'linear', 
                                        maxlength = max_gap_fill_linear) 
# solar from old tower
# full record until sensor removed so below doesnt work
# qsi_old_fill_short <- CRHMr::interpolate(solar1_fltr, 
#                                         varcols = 1,
#                                         methods = 'linear', 
#                                         maxlength = max_gap_fill_linear) 

### whitehorse airport ----

#  air temp + wind
at_u_thrd_fill_short <- CRHMr::interpolate(wha, 
                                          varcols = c(1, 3),
                                          methods = 'linear', 
                                          maxlength = max_gap_fill_linear)
# rh
ea_thrd_fill_short <- CRHMr::interpolate(ea_eccc_mid, 
                                        varcols = 1,
                                        methods = 'linear', 
                                        maxlength = max_gap_fill_linear) 

# Fill Long Gaps on Modelling Vars ----

## air temp ---- 

at_fill_long_gaps <- CRHMr::impute(
    at_u_fill_short,
    primaryCols = 1,
    secondaryObs = at_u_sec_fill_short,
    secondaryCols = 2,
    multipliers = at_low_mid_reg$slope,
    offsets = at_low_mid_reg$intercept,
    logfile = logfile
  )

at_fill_long_gaps <- CRHMr::impute(
  at_fill_long_gaps,
  primaryCols = 1,
  secondaryObs = at_u_sec_fill_short,
  secondaryCols = 3,
  multipliers = at_low_hi_reg$slope,
  offsets = at_low_hi_reg$intercept,
  logfile = logfile
)
  
at_fill_long_gaps <- CRHMr::impute(
  at_u_fill_short,
    primaryCols = 1,
    secondaryObs = wha,
    secondaryCols = 1,
    multipliers = at_low_wha_reg$slope,
    offsets = at_low_wha_reg$intercept,
    logfile = logfile
  )

# only have full record to 2023 currently
at_gaps <- findGaps(at_fill_long_gaps, gapfile = 'crhm/logs/at_gaps.csv',
                    quiet = F, logfile = logfile)

# gap length of 2 left so just fill with linear interp

# at_fill_long_gaps <- CRHMr::interpolate(at_fill_long_gaps, 
#                                       varcols = 1,
#                                       methods = 'linear', 
#                                       maxlength = max_gap_fill_linear)
# at_gaps <- findGaps(at_fill_long_gaps, gapfile = 'crhm/logs/at_gaps.csv',
#                     quiet = F, logfile = logfile)

# check fill 

at_check_df <- at_fill_long_gaps |> 
  select(datetime, t = t.low) |> 
  mutate(group = 'low_filled') |> 
  rbind(wha |> select(datetime, t = t.1) |> mutate(group = 'wha_raw'))# |> 
# rbind(met_main |> select(datetime, t = t.low) |> mutate(group = 'wcf_lo')) |> 
# rbind(met_main |> select(datetime, t = t.mid) |> mutate(group = 'wcf_mid')) |> 
# rbind(met_main |> select(datetime, t = t.high) |> mutate(group = 'wcf_hi'))


# ggplot(at_check_df |> filter(datetime >=
#                                '2015-10-01'),
#        aes(datetime, t, colour = group)) + geom_line()
# plotly::ggplotly()

## air temp (airport for undercatch corr) ---- 

at_wha_fill_long_gaps <- CRHMr::impute(
  at_u_thrd_fill_short,
  primaryCols = 1,
  secondaryObs = at_fill_long_gaps,
  secondaryCols = 1,
  multipliers = at_wha_low_reg$slope,
  offsets = at_wha_low_reg$intercept,
  logfile = logfile
)

at_gaps <- findGaps(at_wha_fill_long_gaps, gapfile = 'crhm/logs/at_wha_gaps.csv',
                    quiet = F, logfile = logfile)

# check fill 

at_check_df <- at_wha_fill_long_gaps |> 
  select(datetime, t = t.1) |> 
  mutate(group = 'wha_fill') |> 
  rbind(wha |> select(datetime, t = t.1) |> mutate(group = 'wha_raw'))# |> 
# rbind(met_main |> select(datetime, t = t.low) |> mutate(group = 'wcf_lo')) |> 
# rbind(met_main |> select(datetime, t = t.mid) |> mutate(group = 'wcf_mid')) |> 
# rbind(met_main |> select(datetime, t = t.high) |> mutate(group = 'wcf_hi'))


# ggplot(at_check_df |> filter(datetime >=
#                                '2015-10-01'),
#        aes(datetime, t, colour = group)) + geom_line()
# plotly::ggplotly()

## wind high ---- 

u_fill_long_gaps <- CRHMr::impute(
  at_u_fill_short,
  primaryCols = 2,
  secondaryObs = at_u_sec_fill_short,
  secondaryCols = 3,
  multipliers = u_high_low_reg$slope,
  offsets = u_high_low_reg$intercept,
  logfile = logfile
)

u_fill_long_gaps <- CRHMr::impute(
  u_fill_long_gaps,
  primaryCols = 1,
  secondaryObs = at_u_thrd_fill_short,
  secondaryCols = 2,
  multipliers = u_high_eccc_reg$slope,
  offsets = u_high_eccc_reg$intercept,
  logfile = logfile
)

u_gaps <- findGaps(u_fill_long_gaps, gapfile = 'crhm/logs/u_gaps.csv',
                    quiet = F, logfile = logfile)
  
# still gaps, fill with avg val for now until other stations are available

u_fill_long_gaps <- u_fill_long_gaps |>
  mutate(u.high = ifelse(is.na(u.high), 0.8, u.high))

u_gaps <- findGaps(u_fill_long_gaps, gapfile = 'crhm/logs/u_gaps.csv',
                   quiet = F, logfile = logfile)

# check fill 

u_check_df <- u_fill_long_gaps |> 
  select(datetime, u = u.high) |> 
  mutate(group = 'u_filled') |> 
  rbind(wha |> select(datetime, u = u.1) |> mutate(group = 'wha_raw')) |> 
# rbind(met_main |> select(datetime, t = t.low) |> mutate(group = 'wcf_lo')) |> 
# rbind(met_main |> select(datetime, t = t.mid) |> mutate(group = 'wcf_mid')) |> 
  rbind(met_main |> select(datetime, u = u.high) |> mutate(group = 'wcf_hi'))


# ggplot(u_check_df |> filter(datetime >=
#                                '2015-10-01'),
#        aes(datetime, u, colour = group)) + geom_line()
# plotly::ggplotly()

## airport wind (for undercatch not used in modelling) ---- 

u_wha_fill_long_gaps <- CRHMr::impute(
  at_u_thrd_fill_short,
  primaryCols = 2,
  secondaryObs = at_u_sec_fill_short,
  secondaryCols = 4,
  multipliers = u_wha_high_reg$slope,
  offsets = u_wha_high_reg$intercept,
  logfile = logfile
)

u_wha_fill_long_gaps <- CRHMr::impute(
  u_wha_fill_long_gaps,
  primaryCols = 1,
  secondaryObs = at_u_sec_fill_short,
  secondaryCols = 3,
  multipliers = u_wha_low_reg$slope,
  offsets = u_wha_low_reg$intercept,
  logfile = logfile
)

u_wha_fill_long_gaps <- u_wha_fill_long_gaps |>
  mutate(u.1 = ifelse(u.1<0, 0, u.1))

u_gaps <- findGaps(u_wha_fill_long_gaps, gapfile = 'crhm/logs/u_wha_gaps.csv',
                   quiet = F, logfile = logfile)

# still gaps, fill with avg val for now until other stations are available

u_wha_fill_long_gaps <- u_wha_fill_long_gaps |>
  mutate(u.1 = ifelse(is.na(u.1), 1, u.1))

u_gaps <- findGaps(u_wha_fill_long_gaps, gapfile = 'crhm/logs/u_wha_gaps.csv',
                   quiet = F, logfile = logfile)

# check fill 

u_check_df <- u_wha_fill_long_gaps |> 
  select(datetime, u = u.1) |> 
  mutate(group = 'u_filled') |> 
  rbind(wha |> select(datetime, u = u.1) |> mutate(group = 'wha_raw')) 


# ggplot(u_check_df |> filter(datetime >=
#                               '2015-10-01'),
#        aes(datetime, u, colour = group)) + geom_line()
# plotly::ggplotly()

## RH ---- 

ea_fill_long_gaps <- CRHMr::impute(
  ea_fill_short,
  primaryCols = 1,
  secondaryObs = ea_sec_fill_short,
  secondaryCols = 1,
  multipliers = ea_low_mid_reg$slope,
  # offsets = ea_low_mid_reg$intercept,
  logfile = logfile
)

ea_fill_long_gaps <- CRHMr::impute(
  ea_fill_long_gaps,
  primaryCols = 1,
  secondaryObs = ea_thrd_fill_short,
  secondaryCols = 1,
  multipliers = ea_low_wha_reg$slope,
  # offsets = ea_low_wha_reg$intercept,
  logfile = logfile
)

ea_gaps <- findGaps(ea_fill_long_gaps, gapfile = 'crhm/logs/ea_gaps.csv',
                    quiet = F, logfile = logfile)

# all full, now back to RH

ea_fill_long_gaps_w_at <- left_join(at_fill_long_gaps, ea_fill_long_gaps)

at_rh_fill_long_gaps <- 
  changeEatoRH(ea_fill_long_gaps_w_at, t.cols = 1, ea.cols = 2,logfile = logfile)

# check fill 

check_df <- at_rh_fill_long_gaps |> 
  select(datetime, rh = rh.1) |> 
  mutate(group = 'fill') |> 
  rbind(wha |> select(datetime, rh = rh.1) |> mutate(group = 'wha_raw')) |> 
  # rbind(met_main |> select(datetime, t = t.low) |> mutate(group = 'wcf_lo')) |> 
  # rbind(met_main |> select(datetime, t = t.mid) |> mutate(group = 'wcf_mid')) |> 
  rbind(met_main |> select(datetime, rh = rh.low) |> mutate(group = 'wcf'))


# ggplot(check_df |> filter(datetime >=
#                               '2015-10-01'),
#        aes(datetime, rh, colour = group)) + geom_line()
# plotly::ggplotly()

## RH (airport for undercatch) ---- 

ea_wha_fill_long_gaps <- CRHMr::impute(
  ea_thrd_fill_short,
  primaryCols = 1,
  secondaryObs = ea_fill_long_gaps,
  secondaryCols = 1,
  multipliers = ea_wha_low_reg$slope,
  offsets = ea_wha_low_reg$intercept,
  logfile = logfile
)

# ea_gaps <- findGaps(ea_wha_fill_long_gaps, gapfile = 'crhm/logs/ea_gaps.csv',
#                     quiet = F, logfile = logfile)

# all full, now back to RH

ea_wha_fill_long_gaps_w_at <- left_join(at_wha_fill_long_gaps, ea_wha_fill_long_gaps)

at_rh_wha_fill_long_gaps <- 
  changeEatoRH(ea_wha_fill_long_gaps_w_at, t.cols = 1, ea.cols = 2,logfile = logfile)

# check fill 

check_df <- at_rh_wha_fill_long_gaps |> 
  select(datetime, rh = rh.1) |> 
  mutate(group = 'fill') |> 
  rbind(wha |> select(datetime, rh = rh.1) |> mutate(group = 'wha_raw'))


# ggplot(check_df |> filter(datetime >=
#                             '2015-10-01'),
#        aes(datetime, rh, colour = group)) + geom_line()
# plotly::ggplotly()

## Precip ----

p_gaps <- findGaps(met_main, gapfile = 'crhm/logs/p_gaps.csv',
                   quiet = F, logfile = logfile)

ppt_fill_long_gaps <- CRHMr::impute(
  met_main,
  primaryCols = 9,
  secondaryObs = wha,
  secondaryCols = 4,
  multipliers = 1,
  logfile = logfile
)

p_gaps <- findGaps(ppt_fill_long_gaps, gapfile = 'crhm/logs/p_gaps.csv',
                    quiet = F, logfile = logfile)

# a few short gaps, just fill with 0s
ppt_fill_long_gaps <- ppt_fill_long_gaps |>
  mutate(ppt = ifelse(is.na(ppt), 0, ppt))

p_gaps <- findGaps(ppt_fill_long_gaps, gapfile = 'crhm/logs/p_gaps.csv',
                   quiet = F, logfile = logfile)

# check fill 

u_check_df <- ppt_fill_long_gaps |> 
  select(datetime, ppt = ppt) |> 
  mutate(group = 'filled') |> 
  rbind(wha |> select(datetime, ppt = precip.1) |> mutate(group = 'wha_raw')) |> 
  # rbind(met_main |> select(datetime, t = t.low) |> mutate(group = 'wcf_lo')) |> 
  # rbind(met_main |> select(datetime, t = t.mid) |> mutate(group = 'wcf_mid')) |> 
  rbind(met_main |> select(datetime, ppt) |> mutate(group = 'wcf_raw'))


# ggplot(u_check_df |> filter(datetime >=
#                               '2015-10-01'),
#        aes(datetime, ppt, colour = group)) + geom_line()
# plotly::ggplotly()

## Precip (airport for undercatch) ----

ppt_wha_fill_long_gaps <- CRHMr::impute(
  wha,
  primaryCols = 4,
  secondaryObs = met_main,
  secondaryCols = 9,
  multipliers = 1,
  logfile = logfile
) |> rename(ppt = precip.1)

p_gaps <- findGaps(ppt_wha_fill_long_gaps, gapfile = 'crhm/logs/p_wha_gaps.csv',
                   quiet = F, logfile = logfile)

# a few short gaps, just fill with 0s
ppt_wha_fill_long_gaps <- ppt_wha_fill_long_gaps |>
  mutate(ppt = ifelse(is.na(ppt), 0, ppt))

p_gaps <- findGaps(ppt_wha_fill_long_gaps, gapfile = 'crhm/logs/p_wha_gaps.csv',
                   quiet = F, logfile = logfile)

# check fill 

u_check_df <- ppt_wha_fill_long_gaps |> 
  select(datetime, ppt = ppt) |> 
  mutate(group = 'filled') |> 
  rbind(wha |> select(datetime, ppt = precip.1) |> mutate(group = 'wha_raw')) 


# ggplot(u_check_df |> filter(datetime >=
#                               '2015-10-01'),
#        aes(datetime, ppt, colour = group)) + geom_line()
# plotly::ggplotly()

## Solar ----

qsi_fill_long_gaps <- CRHMr::impute(
  qsi_fill_short,
  primaryCols = 1,
  secondaryObs = solar1_fltr,
  secondaryCols = 1,
  multipliers = qsi_18_14$slope,
  logfile = logfile
) 

qsi_gaps <- findGaps(qsi_fill_long_gaps, gapfile = 'crhm/logs/qsi_gaps.csv',
                   quiet = F, logfile = logfile)

qsi_fill_long_gaps2 <- CRHMr::impute(
  qsi_fill_long_gaps,
  primaryCols = 1,
  secondaryObs = solar_mod,
  secondaryCols = 1,
  multipliers = qsi_18_mod$slope,
  logfile = logfile
) 

qsi_gaps <- findGaps(qsi_fill_long_gaps2, gapfile = 'crhm/logs/qsi_gaps.csv',
                     quiet = F, logfile = logfile)

# check fill
qsi_check <- rbind(qsi_fill_short |> pivot_longer(!datetime) |> mutate(data = 'short_fill'),
                   qsi_fill_long_gaps |> pivot_longer(!datetime) |> mutate(data = 'long_fill')) |> 
  rbind(qsi_fill_long_gaps2 |> pivot_longer(!datetime) |> mutate(data = 'long_fill2'))

ggplot(qsi_check, aes(datetime, value, colour = data)) + geom_line()
plotly::ggplotly()

# OUTPUT gap filled data ----

## WCF modelling dataset ----
met_out <- 
  # qsi_fill_long_gaps2 |> # qsi has a limited date range
  at_rh_fill_long_gaps |> 
  # left_join(at_rh_fill_long_gaps) |> 
  left_join(u_fill_long_gaps) |> 
  left_join(ppt_fill_long_gaps)

gaps <- findGaps(met_out, gapfile = 'crhm/logs/all_gaps.csv',
                   quiet = F, logfile = logfile)

saveRDS(met_out, 'data/wolf-creek/met/alex/wcf_gap_fill_ac.rds')

## Whitehorse airport precip data for undercatch ----

wha_out <- 
  at_rh_wha_fill_long_gaps |> 
  left_join(u_wha_fill_long_gaps) |> 
  left_join(ppt_wha_fill_long_gaps)

gaps <- findGaps(wha_out, gapfile = 'crhm/logs/all_wha_gaps.csv',
                 quiet = F, logfile = logfile)

saveRDS(wha_out, 'data/wolf-creek/met/alex/eccc_airport_gap_fill_ac.rds')