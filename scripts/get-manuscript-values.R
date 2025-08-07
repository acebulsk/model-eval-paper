# script to pull values referenced in the manuscript

obs_swe_stats <- readRDS('data/manuscript-dfs/obs_swe_stats_peak_ann_min_max.rds')

swe_timeseries_err_stats <- read.csv('tbls/crhm-swe-vs-snowsurvey-errortbl_2025-07-23_11-30-23.csv')

fm <- 'Fortress - Powerline Forest'
rc <- 'Russell - US Old Growth'
wc <- 'Wolf Creek - Forest'

cp25_no_rc_mb <- swe_timeseries_err_stats$Mean.Bias[swe_timeseries_err_stats$name == 'CP25' & swe_timeseries_err_stats$station != rc] |> mean()
e10_no_rc_mb <- swe_timeseries_err_stats$Mean.Bias[swe_timeseries_err_stats$name == 'E10' & swe_timeseries_err_stats$station != rc] |> mean()
cp25_mb <- swe_timeseries_err_stats$Mean.Bias[swe_timeseries_err_stats$name == 'CP25'] |> mean()
e10_mb <- swe_timeseries_err_stats$Mean.Bias[swe_timeseries_err_stats$name == 'E10'] |> mean()

cp25_fm_mb <- swe_timeseries_err_stats$Mean.Bias[swe_timeseries_err_stats$name == 'CP25' & swe_timeseries_err_stats$station == fm]
e10_fm_mb <- swe_timeseries_err_stats$Mean.Bias[swe_timeseries_err_stats$name == 'E10' & swe_timeseries_err_stats$station == fm]
cp25_rc_mb <- swe_timeseries_err_stats$Mean.Bias[swe_timeseries_err_stats$name == 'CP25' & swe_timeseries_err_stats$station == rc]
e10_rc_mb <- swe_timeseries_err_stats$Mean.Bias[swe_timeseries_err_stats$name == 'E10' & swe_timeseries_err_stats$station == rc]
cp25_wc_mb <- swe_timeseries_err_stats$Mean.Bias[swe_timeseries_err_stats$name == 'CP25' & swe_timeseries_err_stats$station == wc]
e10_wc_mb <- swe_timeseries_err_stats$Mean.Bias[swe_timeseries_err_stats$name == 'E10' & swe_timeseries_err_stats$station == wc]
