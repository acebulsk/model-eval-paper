# Check crhm input forcing ac vs lf

ac_df <- CRHMr::readObsFile(
  '/home/alex/local-usask/analysis/crhm-analysis/obs/pwl_crhm_modelling_obs.obs',
  timezone = 'Etc/GMT+6'
)  |> 
  filter(datetime > '2021-10-01') |> 
  select(datetime, ppt = ppt.1, u = u.1) |> mutate(pc = cumsum(ppt), group = 'ac')

lf_df <- CRHMr::readObsFile(
  '/home/alex/local-usask/analysis/crhm-analysis/obs/Fortress_Hourly_ArrayMetData_1Oct2013-30Sept2023_update_13Nov2023.obs',
  timezone = 'Etc/GMT+6'
)  |> 
  filter(datetime > '2021-10-01') |>
  select(datetime, ppt = p.5, u = u.7) |> mutate(pc = cumsum(ppt), group = 'lf')

plot_df <- rbind(ac_df, lf_df)

ggplot(plot_df, aes(datetime, ppt, colour = group)) + geom_line()
plotly::ggplotly()
ggplot(plot_df, aes(datetime, pc, colour = group)) + geom_line()
plotly::ggplotly()

left_join(ac_df, lf_df, by = 'datetime') |> 
  ggplot(aes(u.x, u.y)) + geom_point()
