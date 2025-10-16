# Script to calibrate weighed tree from kg to kg m-2 
# using Jacob Staines throughfall measurements

cpy_load_kg <- read.csv('data/marmot/cob-thesis-data/lysimeters_merged.csv') |> 
  mutate(TIMESTAMP = as.POSIXct(TIMESTAMP, tz = 'Etc/GMT+6')) |> 
  mutate(
    var = 'cpy_swe',
    model = 'Obs'
  ) |> 
    select(datetime = TIMESTAMP, var, value = m1, model)

# Table 2.1 and 4.2 from Staines thesis 2021
event_vals <- read.csv('data/marmot/cob-thesis-data/event-data-staines-table4_2.csv', skip = 1) |> 
  mutate(date = as.POSIXct(date, tz = 'Etc/GMT+6'))

event_deltas <- event_vals |> 
      summarise(across(where(is.numeric), ~ c(NA, diff(.))))

mass_bal <- event_deltas |> 
  mutate(cpy_load = cuml_precip - mean_swe_forest)

# cumulate change in canopy snow load over the events

event_1_start <- event_vals$date[1] |> ceiling_date(unit = '15 minutes')
event_1_end <- event_vals$date[2] |> ceiling_date(unit = '15 minutes')

event_1_datetimes <- data.frame(datetime = seq(event_1_start, event_1_end, by = 60*15))

event_1_load <- event_1_datetimes |> 
  left_join(cpy_load_kg)

event_1_del_load <- max(event_1_load$value) - min(event_1_load$value)

event_2_start <- event_vals$date[2] |> ceiling_date(unit = '15 minutes')
event_2_end <- event_vals$date[3] |> ceiling_date(unit = '15 minutes')

event_2_datetimes <- data.frame(datetime = seq(event_2_start, event_2_end, by = 60*15))

event_2_load <- event_2_datetimes |> 
  left_join(cpy_load_kg)

event_2_del_load <- max(event_2_load$value) - min(event_2_load$value)

# calculate the correction factor after Hedstrom and Pomeroy 1998
event_1_cpy_load_mass_bal <- mass_bal$cpy_load[2]

# kg / m2 = kg * x
# x = [kg /m2] / kg
event_1_corr <- event_1_cpy_load_mass_bal / event_1_del_load

event_2_cpy_load_mass_bal <- mass_bal$cpy_load[3]

# kg / m2 = kg * x
# x = [kg /m2] / kg
event_2_corr <- event_2_cpy_load_mass_bal / event_2_del_load

cpy_load_mm <- cpy_load_kg |> 
  mutate(cpy_load_1 = value * event_1_corr,
         cpy_load_2 = value * event_2_corr,
         cpy_load_avg = value * (event_1_corr + event_2_corr)/2
        )

cpy_load_mm_long_zeroed <- cpy_load_mm |> 
  pivot_longer(c(value, cpy_load_1, cpy_load_2, cpy_load_avg)) |> 
    group_by(name) |> 
    mutate(value = value - first(value)) |> 
    ungroup()

cpy_load_mm_long_zeroed |> 
  ggplot(aes(datetime, value, colour = name)) + geom_line()

plotly::ggplotly()

# zero tree ----
# this was done by defining individual canopy snow load events based 
# on the idea that transpiration losses are minimal when the canopy 
# is snow covered we can zero it pre/post

canopy_snow_events_pre_post <- read.csv('data/marmot/cob-thesis-data/processed_ac/hanging_tree_events_ac.csv') |> 
mutate(from =  as.POSIXct(from, tz = 'Etc/GMT+6'),
       to = as.POSIXct(to, tz = 'Etc/GMT+6'), 
       event_id = as.Date(from, tz = 'Etc/GMT+6'))  |> 
  filter(quality < 3)

canopy_snow_events_pre_post$event_id <- as.Date(canopy_snow_events_pre_post$event_id)

canopy_snow_long_pre_post <- purrr::pmap_dfr(canopy_snow_events_pre_post, to_long_short)

weighed_tree_df_fltr <- cpy_load_mm |>
  select(datetime, value = cpy_load_avg) |> # calibrated weighed tree from above
  left_join(canopy_snow_long_pre_post) |> # this is the df I created to select periods of time where snow is in the canopy and no above canopy precip is occuring
  filter(is.na(event_id) == F)

# zero the tree at the start of each event 

event_start_vals <- weighed_tree_df_fltr |> 
  group_by(event_id) |> 
  summarise(start_time = min(datetime, na.rm = T),
            start_val = first(value)) # grab instrument value at begining of event 

weighed_tree_zeroed_pre_post_cnpy_snow <- weighed_tree_df_fltr |> 
  left_join(event_start_vals, by = c('event_id')) |> 
  mutate(value = value - start_val,
         hours = as.numeric(difftime(datetime, start_time, 'hours'))/(60*60),
         value = case_when(
           value < 0 ~ 0,
           is.na(event_id)~NA,
           T ~ value
         )) |> 
  select(-start_val) |> 
  # rbind(inc_snow) |> 
  filter(is.na(event_id) == F) 

ggplot(weighed_tree_zeroed_pre_post_cnpy_snow, aes(datetime, value)) +
  geom_line() 
plotly::ggplotly()

saveRDS(weighed_tree_zeroed_pre_post_cnpy_snow, 'data/marmot/cob-thesis-data/processed_ac/weighed_tree_kg_m2_zero_pre_post_cnpy_snow.rds')
