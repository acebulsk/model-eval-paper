# Script to plot monthly met normals for each of the four sites

library(tidyverse)

options(ggplot2.discrete.colour = palette.colors(palette = "R4"))
options(ggplot2.discrete.fill = palette.colors(palette = "R4"))


pretty_names <- tibble::tibble(
  name = c("p.1", "rh.1", "t.1", "u.1"),
  name_pretty = c(
    "Total Precipitation (mm)",
    "Relative Humidity (%)",
    "Air Temperature (°C)",
    "Wind Speed (m/s)"
  )
)

## tidy data ----

wc_met <- CRHMr::readObsFile(
  'crhm/obs/wolf_creek_forest_hourly_2015_2024.obs',
  timezone = 'Etc/GMT+7'
) |>
  # select(-Qsi.1) |>
  mutate(station = 'Wolf Creek')

rc_met <- readRDS(
  'crhm/obs/russell/russell_upper_stephanie_clearcut2_2005_2008_withgaps.rds'
) |>
  select(datetime, t.1 = t, rh.1 = rh, p.1 = p, u.1 = u) |>
  mutate(station = 'Russell Creek')

mc_met <- CRHMr::readObsFile(
  'crhm/obs/Marmot_Hourly_ArrayMetData_withT_g_1Oct05-30Sept24_update_3Jan2025.obs',
  timezone = 'Etc/GMT+6'
) |>
  select(datetime, t.1 = t.5, rh.1 = rh.5, p.1 = p.5, u.1 = u.8) |>
  mutate(station = 'Marmot Creek')

fm_met <- CRHMr::readObsFile(
  'crhm/obs/Fortress_Hourly_ArrayMetData_1Oct2013-30Sept2023_update_13Nov2023.obs',
  timezone = 'Etc/GMT+6'
) |>
  select(datetime, t.1 = t.7, rh.1 = rh.7, p.1 = p.4, u.1 = u.7) |>
  mutate(station = 'Fortress Mountain')

## adjust wind to above canopy ----

# function to increase wind speed to constant height around canopy from CRHM original typically for open clearing to top
u_veg_ht <- function(z, veg_ht, uz) {
  uz *
    log((veg_ht - 2 / 3 * z) / (0.123 * z)) /
    log((z - 2 / 3 * z) / (0.123 * z))
}

# wolf creek already comes in at above canopy (18 m US sensor)

# russell creek considers as above canopy

# marmot already comes in at above canopy (18 m US sensor)

# fortress
fm_met$u.1 <- u_veg_ht(5.25, 10.5, fm_met$u.1)

## combine dataframes ----

all_met <- rbind(wc_met, rc_met) |>
  rbind(mc_met) |>
  rbind(fm_met)

## average by month ----

# 1) Precipitation: sum per month per year, then summarise across years
all_pcp_monthly <- all_met %>%
  mutate(
    month = factor(format(datetime, "%b"), levels = month.abb),
    year  = year(datetime)
  ) %>%
  group_by(month, year, station) %>%
  summarise(p.1 = sum(p.1, na.rm = TRUE), .groups = "drop_last") %>%
  group_by(month, station) %>%
  summarise(
    mean = mean(p.1, na.rm = TRUE),
    p5   = quantile(p.1, 0.05, na.rm = TRUE),
    p95  = quantile(p.1, 0.95, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(name = "p.1")

# 2) Other meteorological vars: compute stats for each variable
all_met_monthly_vars <- all_met %>%
  mutate(
    month = factor(format(datetime, "%b"), levels = month.abb)
  ) %>%
  group_by(month, station) %>%
  summarise(
    across(
      c(t.1, rh.1, u.1),
      list(mean = ~mean(.x, na.rm = TRUE),
           p5   = ~quantile(.x, 0.05, na.rm = TRUE),
           p95  = ~quantile(.x, 0.95, na.rm = TRUE)
      ),
      .names = "{.col}_{.fn}"
    ),
    .groups = "drop"
  ) %>%
  # pivot longer to get variable / stat columns
  pivot_longer(
    cols = -c(month, station),
    names_to = c("name", "stat"),
    names_sep = "_",
    values_to = "value"
  ) %>%
  pivot_wider(
    names_from = stat,
    values_from = value
  )

# 3) Combine precipitation + other vars into one tidy table
monthly_met <- bind_rows(
  all_pcp_monthly %>% rename(mean = mean, p5 = p5, p95 = p95),
  all_met_monthly_vars  # already has mean, p5, p95 columns
) %>%
  # optional: attach pretty names (assumes pretty_names maps variable -> name_pretty)
  left_join(pretty_names, by = "name")

# Quick sanity checks (optional)
# check for NA in quantiles or strange ordering
stopifnot(all(c("mean","p5","p95") %in% names(monthly_met)))
# ensure p5 <= p95 (if not, swap)
monthly_met <- monthly_met %>%
  mutate(
    pmin = pmin(p5, p95, na.rm = TRUE),
    pmax = pmax(p5, p95, na.rm = TRUE)
  ) %>%
  mutate(p5 = pmin, p95 = pmax) %>%
  select(-pmin, -pmax)

# 4) Plot: ribbon between 5th and 95th, mean line on top
ggplot(monthly_met, aes(month, mean, colour = station, group = station)) +
  geom_ribbon(aes(ymin = p5, ymax = p95, fill = station), alpha = 0.1, colour = NA) +
  geom_line(aes(y = p5), linetype = "solid", alpha = 0.3) +   # 5th percentile
  geom_line(aes(y = p95), linetype = "solid", alpha = 0.3) +   # 95
  geom_line() +
  geom_point() +
  facet_wrap(~name_pretty, scales = "free") +
  labs(x = NULL, y = "Monthly Mean", colour = "Station", fill = "Station") +
  theme(legend.position = "bottom")

# ggsave('figs/met-normals/monthly_met_normals_all_stns.png', width = 7, height = 5)
ggsave('figs/final/figure2.png', width = 7, height = 5)

# Print date ranges of met
all_met |>
  mutate(year = year(datetime)) |>
  group_by(station) |>
  summarise(start = min(year), end = max(year))
