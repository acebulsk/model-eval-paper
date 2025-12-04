# Setup script to define parameters and pull correct model runs
library(tidyverse)
library(CRHMr)

# functions ----
read_crhm_obs <- function(path, prj, runtag, tz) {
  fullpath <- list.files(
    paste0(
      path,
      prj
    ),
    pattern = runtag,
    full.names = T
  )

  stopifnot(length(path) == 1)

  crhm_output_new <- CRHMr::readOutputFile(
    fullpath,
    timezone = tz
  )
}

to_long_short <- function(from, to, quality, notes, event_id){
  datetime <- seq(from, to, 900)

  out <- data.frame(datetime, quality, notes, event_id)

  return(out)
}

# NSE from Dingman
nse <- function(obs, sim) {
  1 - sum((obs - sim)^2, na.rm = TRUE) / sum((obs - mean(obs, na.rm=TRUE))^2, na.rm = TRUE)
}

# KGE from Clark 2021
kge <- function(obs, sim) {
  r <- cor(obs, sim, use = "complete.obs")
  alpha <- sd(sim, na.rm = TRUE) / sd(obs, na.rm = TRUE)
  beta <- mean(sim, na.rm = TRUE) / mean(obs, na.rm = TRUE)
  1 - sqrt((beta - 1)^2 + (alpha - 1)^2 + (r - 1)^2)
}

# not a huge diff between event and hourly resampling ... event shows greater range in model perforamnce 
# bootstrapping uses stochastic resampling and likely better for this application than jackknife 
# this function computes the error stats by selectively resampling from the 17 events
bootstrap_event <- function(df, n_boot = 1000) {

  df %>%
    filter(!is.na(observed) & !is.na(value)) %>%
    group_by(name) %>%
    group_modify(~{
      data <- .x
      events <- unique(data$event_id)
      n_events <- length(events)
      
      if(n_events < 2) return(tibble(MB = NA, MB_lower = NA, MB_upper = NA,
                                     MAE = NA, MAE_lower = NA, MAE_upper = NA,
                                     RMSE = NA, RMSE_lower = NA, RMSE_upper = NA))
      
      boot_fun <- function(event_indices) {
        sampled_events <- events[event_indices]
        d_sample <- data %>% filter(event_id %in% sampled_events)
        obs <- d_sample$observed
        sim <- d_sample$value
        diff <- obs - sim
        c(
          MB = mean(diff),
          MAE = mean(abs(diff)),
          RMSE = sqrt(mean(diff^2)),
          NSE = nse(obs, sim),
          KGE = kge(obs, sim)
        )
      }
      
      # sample event indices with replacement
      boot_samples <- replicate(n_boot, boot_fun(sample(1:n_events, n_events, replace = TRUE)))
      tmat <- t(boot_samples)  # transpose so rows = replicates
      colnames(tmat) <- c("MB","MAE","RMSE","NSE","KGE")
      
      tibble(
        MB = mean(tmat[,"MB"]), MB_lower = quantile(tmat[,"MB"], 0.025), MB_upper = quantile(tmat[,"MB"], 0.975),
        MAE = mean(tmat[,"MAE"]), MAE_lower = quantile(tmat[,"MAE"], 0.025), MAE_upper = quantile(tmat[,"MAE"], 0.975),
        RMSE = mean(tmat[,"RMSE"]), RMSE_lower = quantile(tmat[,"RMSE"], 0.025), RMSE_upper = quantile(tmat[,"RMSE"], 0.975),
        NSE = mean(tmat[,"NSE"]), NSE_lower = quantile(tmat[,"NSE"], 0.025), NSE_upper = quantile(tmat[,"NSE"], 0.975),
        KGE = mean(tmat[,"KGE"]), KGE_lower = quantile(tmat[,"KGE"], 0.025), KGE_upper = quantile(tmat[,"KGE"], 0.975)
      )
    })
}

# for snow survey bootstrapping

bootstrap_per_site_model <- function(df, n_boot = 1000) {
  
  df %>%
    group_by(station, name) %>%  # station or site name
    group_by(model = name, add = TRUE) %>%  # model grouping
    group_modify(~{
      data <- .x
      if(nrow(data) < 2) return(tibble(
        MB = NA, MB_lower = NA, MB_upper = NA,
        MAE = NA, MAE_lower = NA, MAE_upper = NA,
        RMSE = NA, RMSE_lower = NA, RMSE_upper = NA,
        NSE = NA, NSE_lower = NA, NSE_upper = NA,
        KGE = NA, KGE_lower = NA, KGE_upper = NA
      ))
      
      boot_fun <- function() {
        d_sample <- data[sample(1:nrow(data), nrow(data), replace = TRUE), ]
        obs <- d_sample$observed
        sim <- d_sample$value
        diff <- obs - sim
        c(
          MB = mean(diff),
          MAE = mean(abs(diff)),
          RMSE = sqrt(mean(diff^2)),
          NSE = nse(obs, sim),
          KGE = kge(obs, sim)
        )
      }
      
      boot_samples <- replicate(n_boot, boot_fun())
      tmat <- t(boot_samples)
      colnames(tmat) <- c("MB","MAE","RMSE","NSE","KGE")
      
      tibble(
        MB = mean(tmat[,"MB"]), MB_lower = quantile(tmat[,"MB"],0.025), MB_upper = quantile(tmat[,"MB"],0.975),
        MAE = mean(tmat[,"MAE"]), MAE_lower = quantile(tmat[,"MAE"],0.025), MAE_upper = quantile(tmat[,"MAE"],0.975),
        RMSE = mean(tmat[,"RMSE"]), RMSE_lower = quantile(tmat[,"RMSE"],0.025), RMSE_upper = quantile(tmat[,"RMSE"],0.975),
        NSE = mean(tmat[,"NSE"]), NSE_lower = quantile(tmat[,"NSE"],0.025), NSE_upper = quantile(tmat[,"NSE"],0.975),
        KGE = mean(tmat[,"KGE"]), KGE_lower = quantile(tmat[,"KGE"],0.025), KGE_upper = quantile(tmat[,"KGE"],0.975)
      )
    }) %>% ungroup()
}

bootstrap_across_sites_model_weighted <- function(df, n_boot = 1000) {
  
  unique_models <- unique(df$name)
  
  results <- map_dfr(unique_models, function(m) {
    df_model <- df %>% filter(name == m)
    unique_stations <- unique(df_model$station)
    
    # Run bootstrap
    boot_samples <- replicate(n_boot, {
      
      # Resample stations with replacement
      sampled_stations <- sample(unique_stations, length(unique_stations), replace = TRUE)
      
      # Compute metrics per site, then average across sites
      site_metrics <- map_dfr(sampled_stations, function(st) {
        site_data <- df_model %>% filter(station == st)
        
        # Resample rows within site
        obs <- sample(site_data$observed, nrow(site_data), replace = TRUE)
        sim <- sample(site_data$value, nrow(site_data), replace = TRUE)
        
        # Remove NA
        valid <- !is.na(obs) & !is.na(sim)
        obs <- obs[valid]
        sim <- sim[valid]
        
        diff <- obs - sim
        
        tibble(
          MB = if(length(diff) > 0) mean(diff) else NA,
          MAE = if(length(diff) > 0) mean(abs(diff)) else NA,
          RMSE = if(length(diff) > 0) sqrt(mean(diff^2)) else NA,
          NSE = if(length(obs) > 1) nse(obs, sim) else NA,
          KGE = if(length(obs) > 1) kge(obs, sim) else NA
        )
      })
      
      # Average metrics across sites for this replicate
      colMeans(site_metrics, na.rm = TRUE)
      
    })
    
    tmat <- t(boot_samples)
    colnames(tmat) <- c("MB","MAE","RMSE","NSE","KGE")
    
    # Return final summary with 95% CI
    tibble(
      model = m,
      MB = mean(tmat[,"MB"], na.rm = TRUE),
      MB_lower = quantile(tmat[,"MB"], 0.025, na.rm = TRUE),
      MB_upper = quantile(tmat[,"MB"], 0.975, na.rm = TRUE),
      MAE = mean(tmat[,"MAE"], na.rm = TRUE),
      MAE_lower = quantile(tmat[,"MAE"], 0.025, na.rm = TRUE),
      MAE_upper = quantile(tmat[,"MAE"], 0.975, na.rm = TRUE),
      RMSE = mean(tmat[,"RMSE"], na.rm = TRUE),
      RMSE_lower = quantile(tmat[,"RMSE"], 0.025, na.rm = TRUE),
      RMSE_upper = quantile(tmat[,"RMSE"], 0.975, na.rm = TRUE),
      NSE = mean(tmat[,"NSE"], na.rm = TRUE),
      NSE_lower = quantile(tmat[,"NSE"], 0.025, na.rm = TRUE),
      NSE_upper = quantile(tmat[,"NSE"], 0.975, na.rm = TRUE),
      KGE = mean(tmat[,"KGE"], na.rm = TRUE),
      KGE_lower = quantile(tmat[,"KGE"], 0.025, na.rm = TRUE),
      KGE_upper = quantile(tmat[,"KGE"], 0.975, na.rm = TRUE)
    )
    
  })
  
  return(results)
}

path <- "crhm/output/"

# LOAD DATA ----

## POWERLINE ----

### obs snow ----

fortress_snow_survey <- CRHMr::readObsFile(
  'data/fortress/snow_survey/Powerline_obs_SWE_2013-23.obs',
  timezone = 'Etc/GMT+6'
) |>
  select(datetime, obs_swe = Powerline_Forest.1) |>
  mutate(station = 'Fortress - Powerline Forest')

fm_cpy_load_obs <- readRDS('data/fortress/weighed_tree/unloading_events_zero_weighed_tree_kg_m2_pre_post_cnpy_snow_fsd_closed_0.88.rds') |> 
  select(datetime, var = name, value = tree_mm) |> 
  mutate(model = 'Obs')

## Marmot ----

mc_cpy_load_obs_jm <- read.csv('data/marmot/jm-thesis-data/jmacdonald_thesis_table_5.1_ac.csv') |> 
  mutate(EndTime = as.POSIXct(EndTime, '%Y-%m-%d %H:%M:%S', tz = "Etc/GMT+6")) |> 
  mutate(
    var = 'cpy_swe',
    model = 'Obs',
    year = '2007-2008'
  ) |> 
    select(datetime = EndTime, var, value = CanopyLoad_mmSWE, model, year)

mc_cpy_load_obs_js <- read_rds('data/marmot/cob-thesis-data/processed_ac/weighed_tree_kg_m2_zero_pre_post_cnpy_snow.rds') |> 
  mutate(
    var = 'cpy_swe',
    model = 'Obs',
    year = '2018-2019'
  ) |> 
  select(datetime, var, value, model, year)

# CRHM OUTPUTS ----

## FORTRESS - POWERLINE ----

### updated crhm (cansnobal) ----

prj <- "fortress_powerline_clearing_snowsurveytransect_cansnobal"
run_tag_updt <- "v_4_0_logan_fix_precip"

crhm_output_new <- read_crhm_obs(path, prj, run_tag_updt, 'Etc/GMT+6') |>
  select(
    datetime,
    swe = SWE.1,
    cpy_swe = m_s_veg.1,
    # rain = hru_rain.1,
    snow = hru_snow.1,
    # pcp = hru_p.1,
    # tf = net_p.1,
    subl = delsub_veg_int.1,
    # evap = delevap_veg_int.1,
    # drip = deldrip_veg_int.1,
    melt = delmelt_veg_int.1,
    unld = delunld_int.1,
    tf = throughfall_snow.1,
    # tf_rf = throughfall_rain.1
  ) |>
  mutate(subl = -subl) |> # flux is wrt canopy and diff from baseline
  pivot_longer(!datetime, names_to = 'var') |>
  mutate(model = 'CP25')

### baseline crhm ----
prj <- "fortress_powerline_clearing_snowsurveytransect_baseline"
run_tag_base <- "v_4_0_logan_fix_precip"

crhm_output_base <- read_crhm_obs(
  path = path,
  prj = prj,
  runtag = run_tag_base,
  tz = 'Etc/GMT+6'
) |>
  select(
    datetime,
    swe = SWE.1,
    cpy_swe = Snow_load.1,
    # rain = hru_rain.1,
    snow = hru_snow.1,
    unld = SUnload.1,
    melt = SUnload_H2O.1,
    # pcp = hru_p.1,
    tf = direct_snow.1,
    subl = Subl_Cpy.1
  ) |>
  pivot_longer(!datetime, names_to = 'var') |>
  mutate(model = 'E10')

# ggplot(crhm_output_base, aes(datetime, SWE.1)) +
#   geom_line()

### combine powerline ----

fortress_obs_sim_swe <- rbind(crhm_output_new, crhm_output_base) |>
  mutate(station = 'Fortress - Powerline Forest') |> 
  filter(datetime < '2023-10-01')

# snowscale_and_new <- left_join(crhm_output_new,
#                                snow_scale,
#                                by = 'datetime') |>
#   left_join(crhm_output_base) |>
#   mutate(station = 'Powerline Forest')

## MARMOT UPPER FOREST ----

### obs snow ----

marmot_snow_survey <- CRHMr::readObsFile(
  'data/marmot/Marmot_UpperForest_UpperClearing_SWE_2008-23.obs',
  timezone = 'Etc/GMT+6'
) |>
  select(datetime, obs_swe = Upper_Forest.1) |>
  mutate(station = 'Marmot - Upper Forest')

### updated crhm (cansnobal) ----

prj <- "marmot_upper_forest_clearing_snowsurveytransect_cansnobal"
run_tag_updt <- "r4_earlier_starttime"

crhm_output_new <- read_crhm_obs(path, prj, run_tag_updt, 'Etc/GMT+6') |>
  select(
    datetime,
    swe = SWE.1,
    cpy_swe = m_s_veg.1,
    # rain = hru_rain.1,
    snow = hru_snow.1,
    # pcp = hru_p.1,
    # tf = net_p.1,
    subl = delsub_veg_int.1,
    melt = delmelt_veg_int.1,
    # evap = delevap_veg_int.1,
    # drip = deldrip_veg_int.1,
    unld = delunld_int.1,
    tf = throughfall_snow.1
    # tf_rf = throughfall_rain.1
  ) |>
  mutate(subl = -subl) |> # flux is wrt canopy and diff from baseline
  pivot_longer(!datetime, names_to = 'var') |>
  mutate(model = 'CP25')

### baseline crhm ----
prj <- "marmot_upper_forest_clearing_snowsurveytransect_baseline"
run_tag_base <- "r5_add_rain_load_output"

crhm_output_base <- read_crhm_obs(
  path = path,
  prj = prj,
  runtag = run_tag_base,
  tz = 'Etc/GMT+6'
) |>
  select(
    datetime,
    swe = SWE.1,
    cpy_swe = Snow_load.1,
    cpy_rain = rain_load.1,
    # rain = hru_rain.1,
    snow = hru_snow.1,
    melt = SUnload_H2O.1,
    unld = SUnload.1,
    # pcp = hru_p.1,
    tf = direct_snow.1,
    subl = Subl_Cpy.1
  ) |>
  mutate(cpy_swe = cpy_swe + cpy_rain) |> 
  pivot_longer(!datetime, names_to = 'var') |>
  mutate(model = 'E10')

# ggplot(crhm_output_base, aes(datetime, SWE.1)) +
#   geom_line()

### combine marmot ----

marmot_sim_swe <- rbind(crhm_output_new, crhm_output_base) |>
  mutate(station = 'Marmot - Upper Forest') 

## WOLF CREEK ----

### Snow survey obs ----

wcf_snow_survey <- readRDS(
  'data/wolf-creek/snow_survey/obs/wcf_snow_survey_stats_1993_2024.rds'
) |>
  select(datetime = date, obs_swe = swe_mean) |>
  mutate(station = 'Wolf Creek - Forest') |>
  filter(datetime > '2015-10-01', datetime < '2022-10-01')

### updated crhm (cansnobal) ----

prj <- "wolf_creek_forest_snowsurveytransect_cansnobal"
run_tag_updt <- "revert_r4"

crhm_output_new <- read_crhm_obs(path, prj, run_tag_updt, 'Etc/GMT+7') |>
  select(
    datetime,
    swe = SWE.1,
    cpy_swe = m_s_veg.1,
    # rain = hru_rain.1,
    snow = hru_snow.1,
    # pcp = hru_p.1,
    # tf = net_p.1,
    subl = delsub_veg_int.1,
    melt = delmelt_veg_int.1,
    # evap = delevap_veg_int.1,
    # drip = deldrip_veg_int.1,
    unld = delunld_int.1,
    tf = throughfall_snow.1
    # tf_rf = throughfall_rain.1
  ) |>
  mutate(subl = -subl) |> # flux is wrt canopy and diff from baseline
  pivot_longer(!datetime, names_to = 'var') |>
  mutate(model = 'CP25')

### baseline crhm ----

prj <- "wolf_creek_forest_snowsurveytransect_baseline"
run_tag_base <- "revert_r4"

crhm_output_base <- read_crhm_obs(
  path = path,
  prj = prj,
  runtag = run_tag_base,
  tz = 'Etc/GMT+7'
) |>
  select(
    datetime,
    swe = SWE.1,
    cpy_swe = Snow_load.1,
    # rain = hru_rain.1,
    snow = hru_snow.1,
    melt = SUnload_H2O.1,
    unld = SUnload.1,
    # pcp = hru_p.1,
    tf = direct_snow.1,
    subl = Subl_Cpy.1
  ) |>
  pivot_longer(!datetime, names_to = 'var') |>
  mutate(model = 'E10')

### combine wolf ----

wcf_obs_sim_swe <- rbind(crhm_output_new, crhm_output_base) |>
  mutate(station = 'Wolf Creek - Forest') |> 
  filter(datetime < '2022-10-01')

## RUSSELL CREEK ----

### Snow survey obs ----

russell_snow_survey <-
  readRDS('data/russell-creek/russell_upper_stephanie_all_swe_2006_2008.rds') |>
  select(datetime, obs_swe = OG2) |>
  mutate(station = 'Russell - Old Growth')

### updated crhm (cansnobal) ----

prj <- "russell_upper_steph_forest_snowsurveytransect_cansnobal"
run_tag_updt <- "rs_harder_lai_1.93_cc0.86_addtf"

crhm_output_new <- read_crhm_obs(path, prj, run_tag_updt, 'Etc/GMT+8') |>
  select(
    datetime,
    swe = SWE.1,
    cpy_swe = m_s_veg.1,
    # rain = hru_rain.1,
    snow = hru_snow.1,
    # pcp = hru_p.1,
    # tf = net_p.1,
    subl = delsub_veg_int.1,
    melt = delmelt_veg_int.1,
    # evap = delevap_veg_int.1,
    # drip = deldrip_veg_int.1,
    unld = delunld_int.1,
    tf = throughfall_snow.1
    # tf_rf = throughfall_rain.1
  ) |>
  mutate(subl = -subl) |> # flux is wrt canopy and diff from baseline
  pivot_longer(!datetime, names_to = 'var') |>
  mutate(model = 'CP25')

### baseline crhm ----

prj <- "russell_upper_steph_forest_snowsurveytransect_baseline"
run_tag_updt <- "ellis2010unldpars_rs_harder_sbar_8_7"

crhm_output_base <- read_crhm_obs(path, prj, run_tag_updt, 'Etc/GMT+8') |>
  select(
    datetime,
    swe = SWE.1,
    cpy_swe = Snow_load.1,
    # rain = hru_rain.1,
    snow = hru_snow.1,
    melt = SUnload_H2O.1,
    unld = SUnload.1,
    # pcp = hru_p.1,
    tf = direct_snow.1,
    subl = Subl_Cpy.1
  ) |>
  pivot_longer(!datetime, names_to = 'var') |>
  mutate(model = 'E10')

### combine russell ----

zero_start <- as.POSIXct('2007-04-21 19:00', tz = 'Etc/GMT+8')
zero_end <- as.POSIXct('2007-11-10 20:00', tz = 'Etc/GMT+8')

russell_obs_sim_swe <- rbind(crhm_output_new, crhm_output_base) |>
  mutate(
    station = 'Russell - Old Growth',
    value = ifelse(datetime >= zero_start & datetime <= zero_end, NA, value)
  )

# Combine all sites ----

all_sites_mods <-
  rbind(marmot_sim_swe, russell_obs_sim_swe) |>
  rbind(fortress_obs_sim_swe) |>
  rbind(wcf_obs_sim_swe)

all_sites_obs <-
  rbind(marmot_snow_survey, russell_snow_survey, fortress_snow_survey) |>
  rbind(wcf_snow_survey)

all_sites_yrs <- all_sites_mods |>
  group_by(station) |>
  summarise(start = min(year(datetime)), end = max(year(datetime)))
