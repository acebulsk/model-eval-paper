# Script to compile wolf creek snow survey data.
# Basic structure: loop over all Excel files in
# ss_paths, extract all sheets from each file, and bind the results together.

library(tidyverse)
library(readxl)

date_index <- 'H7' # switches to 'I7' at Wolf Ck Snow Surveys 2015-2016.xlsx handle this below
survey_index <- 'G9:L33'
date_switch_trigger <- 'data/wolf-creek/snow_survey/obs/WCF_SnowSurveys_raw/Raw Data and Workup//Wolf Ck Snow Surveys 2015-2016.xlsx'

# Load data ----
ss_path <- 'data/wolf-creek/snow_survey/obs/WCF_SnowSurveys_raw/Raw Data and Workup/'
ss_paths <- 
  base::list.files(
    ss_path,
    '^Wolf Ck Snow Surveys*',
    full.names = T
  )

ss_paths <- ss_paths[!grepl("template", ss_paths, ignore.case = TRUE)]

# ss_paths is a character vector of file paths
query_all_excel_ss <- function(ss_paths) {
  map_dfr(ss_paths, function(file_path) {
    print(file_path)
    if(file_path == date_switch_trigger){date_index <<- 'I7'}
    sheet_names <- excel_sheets(file_path)
    
    map_dfr(sheet_names, function(sheet) {
      print(sheet)
      # Safely read H7 cell; returns NA if empty or doesn't exist
      ss_date_raw <- tryCatch({
        read_excel(file_path, sheet = sheet, range = date_index, col_names = FALSE)[[1, 1]]
      }, error = function(e) NA)
      print(ss_date_raw)
      # Parse the date if available
      ss_date <- if (!is.na(ss_date_raw)) {
        parse_date_time(ss_date_raw, orders = c("ymd", "mdy"), quiet = TRUE)
      } else {
        NA
      }
      
      # Read main data
      df <- read_excel(file_path, sheet = sheet, range = survey_index, col_names = F) |>
        select(2, 6) |> 
        setNames(c('depth', 'swe')) |> 
        mutate(across(1:2, as.numeric)) |>  
        mutate(date = ss_date,
               sheet = sheet,
               file = basename(file_path))  # add file name for reference
      
      return(df)
    })
  })
}

all_snow_survey_data <- query_all_excel_ss(ss_paths)

# Plot snow surveys 

all_snow_survey_data |> 
  ggplot(aes(date, swe, group = date)) +
  geom_boxplot(width = 10000)
plotly::ggplotly()

# Compute mean SWE per date

all_ss_smry <- all_snow_survey_data |> 
  mutate(swe = swe * 10) |> ## cm to mm 
  group_by(date) |> 
  summarise(swe_mean = mean(swe, na.rm = T),
            swe_sd = sd(swe, na.rm = T),
            sd_low = swe_mean - swe_sd,
            sd_low = ifelse(sd_low < 0, 0, sd_low),
            sd_hi = swe_mean + swe_sd,
            sheet = first(sheet),
            file = first(file))
  
all_ss_smry |> 
  ggplot(aes(date, swe_mean)) +
  geom_errorbar(aes(ymax = sd_hi, ymin = sd_low)) +
  geom_point()

plotly::ggplotly()

