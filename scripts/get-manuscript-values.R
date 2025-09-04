# script to pull values referenced in the manuscript
site_meta <- read.csv('figs/final/table1.csv')

swe_timeseries_err_stats <- read.csv('figs/final/table2.csv')
obs_swe_stats <- readRDS('data/manuscript-dfs/obs_swe_stats_peak_ann_min_max.rds')

frac_sf_as_pk_swe <- readRDS('data/manuscript-dfs/frac_snowfall_as_pk_swe_w_losses.rds')
frac_sf_as_pk_swe_lst <- setNames(frac_sf_as_pk_swe$frac_sf |> round(1), frac_sf_as_pk_swe$station) |> as.list()

fm <- 'Fortress - Powerline Forest'
rc <- 'Russell - Old Growth'
wc <- 'Wolf Creek - Forest'
mc <- 'Marmot - Upper Forest'

cp25_no_rc_mb <- swe_timeseries_err_stats$Mean.Bias[swe_timeseries_err_stats$name == 'CP25' & swe_timeseries_err_stats$station != rc] |> mean()
e10_no_rc_mb <- swe_timeseries_err_stats$Mean.Bias[swe_timeseries_err_stats$name == 'E10' & swe_timeseries_err_stats$station != rc] |> mean()
cp25_mb <- swe_timeseries_err_stats$Mean.Bias[swe_timeseries_err_stats$name == 'CP25' & swe_timeseries_err_stats$station == 'All Station Mean'] 
e10_mb <- swe_timeseries_err_stats$Mean.Bias[swe_timeseries_err_stats$name == 'E10' & swe_timeseries_err_stats$station == 'All Station Mean']

cp25_fm_mb <- swe_timeseries_err_stats$Mean.Bias[swe_timeseries_err_stats$name == 'CP25' & swe_timeseries_err_stats$station == fm]
e10_fm_mb <- swe_timeseries_err_stats$Mean.Bias[swe_timeseries_err_stats$name == 'E10' & swe_timeseries_err_stats$station == fm]
cp25_rc_mb <- swe_timeseries_err_stats$Mean.Bias[swe_timeseries_err_stats$name == 'CP25' & swe_timeseries_err_stats$station == rc]
e10_rc_mb <- swe_timeseries_err_stats$Mean.Bias[swe_timeseries_err_stats$name == 'E10' & swe_timeseries_err_stats$station == rc]
cp25_wc_mb <- swe_timeseries_err_stats$Mean.Bias[swe_timeseries_err_stats$name == 'CP25' & swe_timeseries_err_stats$station == wc]
e10_wc_mb <- swe_timeseries_err_stats$Mean.Bias[swe_timeseries_err_stats$name == 'E10' & swe_timeseries_err_stats$station == wc]
cp25_mc_mb <- swe_timeseries_err_stats$Mean.Bias[swe_timeseries_err_stats$name == 'CP25' & swe_timeseries_err_stats$station == mc]
e10_mc_mb <- swe_timeseries_err_stats$Mean.Bias[swe_timeseries_err_stats$name == 'E10' & swe_timeseries_err_stats$station == mc]

# canopy snow partitioning 
library(dplyr, warn.conflicts = F)

frac_subl <- readRDS('data/manuscript-dfs/frac-subl.rds') |> 
  filter(model == 'CP25') |> 
  group_by(station) |> 
  summarise(frac_subl = median(frac_subl, na.rm = T))

frac_subl_lst <- setNames(frac_subl$frac_subl |> round(1) * 100, frac_subl$station) |> as.list()
