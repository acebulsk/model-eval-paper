# Script to fill gaps in Forest Precip at Wolf Creek

# LOAD DATA ----

## Newer data from Gord ----

# TODO request more years ...  

datetimes <- read_table('data/wolf-creek/met/Wolf_Creek_Forest_2021_Clean/Clean/TV.dat',
                        col_names = 'matlabtime') |>
  mutate(datetime = as.POSIXct((matlabtime - matlab_origin) * 86400,
                               origin = "1970-01-01",
                               tz = "UTC"
  ))

at <- read_table('data/wolf-creek/met/gord/WCF_2021_Clean/Clean/Air_Temp_2m_Avg.dat',
                 col_names = 't', na = 'NaN')

precip_new <- read_table('data/wolf-creek/met/gord/WCF_2021_Clean/Clean/Geonor_precipitation.dat',
                 col_names = 'ppt', na = 'NaN') 

precip_new_df <- cbind(datetimes, precip_new) |> mutate(station_name = 'Forest')

ggplot(precip_new_df, aes(datetime, ppt, colour = station_name)) + geom_line()
plotly::ggplotly()

# WARNING: AlpineForestShrubTundra_precipitation_mm_Gaps_Filled.csv is DAILY
precip_forest <- read_csv('data/wolf-creek/met/kabir2019/Precipitation/Forest_Precip.Rain_mm.csv') |> 
  mutate(datetime = as.POSIXct(Date, format = '%d/%m/%Y %H:%M', tz = tz)) |> 
  select(datetime, ppt = Value) |> 
  mutate(station_name = 'Forest')

precip_alpine <- read_csv('data/wolf-creek/met/kabir2019/Precipitation/Alpine_Precip.Rain._mm.csv') |> 
  mutate(datetime = as.POSIXct(Date, format = '%d/%m/%Y %H:%M', tz = tz)) |> 
  select(datetime, ppt = Value) |> 
  mutate(station_name = 'Alpine')

precip_shrubtund <- read_csv('data/wolf-creek/met/kabir2019/Precipitation/ShrubTundra_Precip.Rain_mm.csv') |> 
  mutate(datetime = as.POSIXct(Date, format = '%d/%m/%Y %H:%M', tz = tz)) |> 
  select(datetime, ppt = Value) |> 
  mutate(station_name = 'ShrubTundra')

precip <- rbind(precip_forest, precip_alpine) |> rbind(precip_shrubtund)

ggplot(precip, aes(datetime, ppt, colour = station_name)) + geom_line()
plotly::ggplotly()