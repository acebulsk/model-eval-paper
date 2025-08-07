# Script to compute tree heights near snow survey 
library(lidR)
library(sf)
library(terra)

#first clipped 32gb file with pdal pipeline pdal_clip.json
res <- 1

las <- readLAS("data/wolf-creek/snow_survey/canopy/Aug2018_WolfCreek_class_clip_snow_survey.laz")
dtm <- rasterize_terrain(las, algorithm = tin(), res = res)

# scalar. The distance to the simulated cloth to classify a point cloud into ground and non-ground. The default is 0.5.
ct <- 0.2
# scalar. The distance between particles in the cloth. This is usually set to the average distance of the points in the point cloud. The default value is 0.5.
cr <- 0.5
algo <- csf(sloop_smooth = F, class_threshold = ct, cloth_resolution = cr, rigidness = 1)
las <- classify_ground(las, algo)
las <- normalize_height(las, tin())

window_size <- 1
min_height <- 4

ttops <- locate_trees(las, lmf(ws = window_size, hmin = min_height))

write_sf(ttops |> st_zm(), paste0('data/lidR_canopy_metrics/', plot, '_ttop_points.shp'))

chm <- rasterize_canopy(las, 0.25, pitfree(subcircle = 0.2))

zlim <- c(0, 18)  # Replace min_value and max_value with your desired values
plot <- 'wcf'
# Save the plot for lidr_sd
png(paste0(
  'figs/maps/',
  plot,
  '_canopy_height_lidR.png'
), width = 1000, height = 800, res = 200)
plot(chm, main = ': Height (m)', range = zlim)
dev.off()

# 2d vis
# plot(chm, col = height.colors(50))
# plot(sf::st_geometry(ttops), add = TRUE, pch = 3)
mean_z <- mean(ttops$Z)

saveRDS(mean_z,
        paste0(
          'data/wolf-creek/snow_survey/canopy/',
          plot,
          '_mean_tree_height.rds'
        ))

ggplot(ttops, aes(x =Z)) +
  geom_histogram(binwidth = 0.5, colour = "black", position = 'dodge') +
  geom_vline(xintercept = mean(ttops$Z), color = "red", linetype ="dashed", size = 1) + 
  labs(title = plot,
       y = "Frequency",
       x = "Tree Height (m)")

