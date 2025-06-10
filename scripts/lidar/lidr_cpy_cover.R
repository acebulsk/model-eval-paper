library(lidR)
library(sf)
library(terra)

 #first clipped 32gb file with pdal pipeline pdal_clip.json
res <- 1

las <- readLAS("data/wolf-creek/snow_survey/canopy/Aug2018_WolfCreek_class_clip_snow_survey.laz")
dtm <- rasterize_terrain(las, algorithm = tin(), res = res)
las_norm <- normalize_height(las, dtm)

first <- filter_first(las_norm)
plot(first, color = "Classification", size = 3, bg = "white")

veg <- filter_poi(first, (ReturnNumber == 1 & Classification != 2 & Z > 2))
# plot(veg, color = "Z", size = 3, bg = "white")

# METHOD 1 cpy returns / total returns 

total_returns <- grid_density(first, res = res)

# plot(las_filtered, color = "Classification", size = 3, bg = "white")

cpy_returns <- grid_density(las_filtered, res = res)

cpy_coverage <- cpy_returns/total_returns

plot(cpy_coverage)

canopy_coverage <- raster::cellStats(cpy_coverage, stat = 'mean', na.rm = TRUE) * 100
canopy_coverage

# Start PNG device
png("data/wolf-creek/snow_survey/canopy/wolf_creek_forest_snow_survey_canopy_canopy_coverage.png", width = 1200, height = 1000, res = 150)
# Plot
plot(cpy_coverage, main = "Canopy Coverage")
# Close the device
dev.off()

write.csv(canopy_coverage, 'data/wolf-creek/snow_survey/canopy/wolf_creek_forest_snow_survey_canopy_coverage.csv')

# METHOD 2 gap fraction (less biased to ground occlusion)

# binary raster: is there a return > 2m?
veg_hits <- grid_metrics(first, ~any(Z > 2), res = res)

# Canopy cover = % of cells with hits above 2m
canopy_cover_pct <-  raster::cellStats(veg_hits, "mean", na.rm = TRUE) * 100
canopy_cover_pct
