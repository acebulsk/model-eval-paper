# Script to apply undercatch correction on snowfall at Wolf Creek Forest

library(tidyverse)
library(CRHMr)

# met_in <- readRDS('data/wolf-creek/met/alex/wcf_gap_fill_ac.rds') # significant capping/ interception at WCF geonor so using airport precip
met_in <- readRDS('data/wolf-creek/met/alex/eccc_airport_gap_fill_ac.rds') 

# run phils correction

pc_correct <- CRHMr::phaseCorrect(
  obs = met_in,
  Tcol = 1,
  RHcol = 2,
  Ucol = 3,
  Pcol = 4,
  shield = 2 # this is the Smith correction recommended by pomeroy
) |>
  mutate(
    raw = cumsum(ppt),
    corrected = cumsum(phaseCorrectTotalPrecip))

# plot 

pc_correct |> 
  select(datetime, raw, corrected) |> 
  pivot_longer(!datetime) |> 
  ggplot(aes(datetime, value, colour = name)) +
  geom_line() +
  ylab('Cumulative Precipitation (mm)')

# print some stats

diffpc <- max(pc_correct$corrected) - max(pc_correct$raw, na.rm = T)
# diffpc <- max(pc_correct_old$pc_corr_old) - max(pc_correct$pc, na.rm = T)
# diffpc <- max(pc_correct_old$pc_corr_jm) - max(pc_correct$pc_corr)

dt_range <- range(pc_correct$datetime)

print(
  paste(
    'Between',
    dt_range[1],
    'and',
    dt_range[2],
    'there was',
    diffpc,
    'mm of precip added by the undercatch correction.'
  )
)

# writeout data

pc_out <- pc_correct |>
  select(datetime, pc = corrected)

pint_pc_out <- weighingGaugeInterval(pc_out, quiet = F) |>
  rename(ppt = pc_interval)

saveRDS(pint_pc_out, file = 'data/wolf-creek/met/alex/eccc_airport_qaqc_undercatch_corr_ac.rds')
