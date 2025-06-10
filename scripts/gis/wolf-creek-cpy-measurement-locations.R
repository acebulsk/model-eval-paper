# Script to plot wolf creek canopy information

library(readxl)       # For reading Excel files
library(sf)           # For spatial data handling
library(ggplot2)      # For plotting
library(dplyr)
library(maptiles)
library(ggspatial)

# Create weather station sf object (in WGS84) coord from Rasouli 2019
weather_station <- st_sf(
  ID = "Forest", 
  geometry = st_sfc(st_point(c(-134.9528, 60.596)), crs = 4326)
)

# just the WCF plots 
data <- read_excel("data/wolf-creek/canopy/UTF-8SRKFMR-18-20 field data summary-V5.xlsx", sheet = "SRKFMR-18-20 field data summary") |> 
  filter(Site == 'WCF')

data$Easting <- as.numeric(data$Easting)
data$Northing <- as.numeric(data$Northing)

data_clean <- data[!is.na(data$Easting) & !is.na(data$Northing), ]

# Step 2: Inspect to find column names (assuming 'x' and 'y')
head(data)

# Step 3: Convert to sf object (assumes columns are named 'x' and 'y')
points_sf <- st_as_sf(data_clean, coords = c("Easting", "Northing"), crs = 32608)  # UTM Zone 8N

# Step 4: (Optional) Transform to geographic coordinates (lat/lon)
points_latlon <- st_transform(points_sf, crs = 4326) |> 
  select(ID = `Site-Plot`)

points_out <- rbind(weather_station, points_latlon)

sf::write_sf(points_out, 'data/wolf-creek/canopy/wolf_creek_data.gpkg')

# Get a bounding box around your points
bbox <- st_bbox(points_latlon)

# Fetch satellite basemap (e.g., from Esri)
basemap <- get_tiles(points_latlon, provider = "Esri.WorldImagery", crop = TRUE, zoom = 12)

# Plot satellite with your points
ggplot() +
  layer_spatial(basemap) +
  geom_sf(data = points_latlon, color = "red", size = 2) +
  theme_minimal()


# all plots
data <- read_excel("data/wolf-creek/canopy/UTF-8SRKFMR-18-20 field data summary-V5.xlsx", sheet = "SRKFMR-18-20 field data summary")

data$Easting <- as.numeric(data$Easting)
data$Northing <- as.numeric(data$Northing)

data_clean <- data[!is.na(data$Easting) & !is.na(data$Northing), ]

# Step 2: Inspect to find column names (assuming 'x' and 'y')
head(data)

# Step 3: Convert to sf object (assumes columns are named 'x' and 'y')
points_sf <- st_as_sf(data_clean, coords = c("Easting", "Northing"), crs = 32608)  # UTM Zone 8N

# Step 4: (Optional) Transform to geographic coordinates (lat/lon)
points_latlon <- st_transform(points_sf, crs = 4326)

sf::write_sf(points_latlon, 'data/wolf-creek/canopy/wolf_creek_all_plots.gpkg')

# Get a bounding box around your points
bbox <- st_bbox(points_latlon)

# Fetch satellite basemap (e.g., from Esri)
basemap <- get_tiles(points_latlon, provider = "Esri.WorldImagery", crop = TRUE, zoom = 12)

# Plot satellite with your points
ggplot() +
  layer_spatial(basemap) +
  geom_sf(data = points_latlon, color = "red", size = 2) +
  theme_minimal()
