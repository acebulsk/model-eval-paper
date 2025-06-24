# Plot  baseline vs new canopy snow

# SETUP ----
library(tidyverse)

all_sites_cpy_swe <- all_sites_mods |> 
  filter(var == 'cpy_swe')

# Plot subcanopy SWE

all_sites_cpy_swe |> 
  ggplot(aes(x = datetime, y = value, colour = model, group = model)) +
  geom_line() +
  facet_wrap(~station, nrow = 4, scales = 'free') +
  scale_colour_manual(
    values = c(#"Observed_clearing" = "blue",
      "E10" = "salmon",
      "CP25" = "dodgerblue"),
    # labels = c(
    #   "Observed" = "Observed-Clearing",
    #   "Simulated" = "Simulated-Forest"
    # ),
    name = "Legend"
  ) +
  # guides(colour = guide_legend(override.aes = list(
  #   linetype = c(1, 1, 1, 0), # Line styles for the first two, none for points
  #   shape = c(NA, NA, NA, 16)  # Points only for "Snow Survey"
  # ))) +
  ylab(expression(Canopy~Load~(kg~m^{-2}))) +
  xlab(element_blank()) +
  theme(legend.position = 'bottom')

plotly::ggplotly()
