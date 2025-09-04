library(sf)
library(tidyverse)
library(tmap)
library(ggrepel)
library(terra)
library(tidyterra)
library(spData)
library(cowplot)

# https://www.cec.org/north-american-environmental-atlas/land-cover-30m-2020/
globcover <- terra::rast('~/Documents/no-backups/land_cover_2020v2_30m_tif/NA_NALCMS_landcover_2020v2_30m/data/NA_NALCMS_landcover_2020v2_30m.tif')

sites <- data.frame(
  site = c('Wolf Creek', 'Russell Creek', 'Fortress Mountain', 'Marmot Creek'),
  lon = c(-135.1497, -126.3090,-115.1983, -115.155154),
  lat = c(60.567, 50.3710, 50.8269, 50.9255877)
)

sites_sf <- sf::st_as_sf(sites, coords = c('lon', 'lat'), crs = 4326)
sites_laea <- st_transform(sites_sf, crs(globcover))

xmin <- -2120000
xmax <- -900864
ymin <- 570000
ymax <- 2547041


# Create terra extent
sites_extent <- ext(xmin, xmax, ymin, ymax)

# Crop raster
land_buf <- crop(globcover, sites_extent)
lvls <- levels(land_buf)[[1]]

lvls$Class_EN <- c(
  NA,                         # 0
  "Needleleaf\n vegetation",               # 1
  "Needleleaf\n vegetation",               # 2
  "Non-needleleaf\n vegetation",# 3
  "Non-needleleaf\n vegetation",# 4
  "Non-needleleaf\n vegetation",# 5
  "Non-needleleaf\n vegetation",# 6
  "Non-needleleaf\n vegetation",# 7
  "Non-needleleaf\n vegetation",# 8
  "Non-needleleaf\n vegetation",# 9
  "Non-needleleaf\n vegetation",#10
  "Non-needleleaf\n vegetation",#11
  "Non-needleleaf\n vegetation",#12
  "Barren",                   #13
  "Water",                  #14
  "Non-needleleaf\n vegetation",#15
  "Barren",                   #16
  "Urban",                    #17
  "Water",                    #18
  "Snow/Ice"                  #19
)

# Assign back to raster
levels(land_buf) <- lvls
cat_colors <- c(
  "Needleleaf\n vegetation" = "#003d00",
  "Non-needleleaf\n vegetation" = "#55a630",
  "Barren" = "#a8abaf",
  "Urban" = "#dc2126",
  "Water" = "#4c70a3",
  "Snow/Ice" = "#ffffff"
)

main_map <- ggplot() +
  geom_spatraster(data = land_buf, aes(fill = Class_EN)) +
  scale_fill_manual(
    values = cat_colors,
    na.value = "transparent", drop = TRUE) +
  geom_sf(data = sites_laea, color = "black", fill = "gold", size = 3, shape = 21) +
  geom_label_repel(
    data = sites_sf,
    aes(geometry = geometry, label = site),
    segment.color = "black",  # change the connecting line color
    stat = "sf_coordinates",
    box.padding = unit(1, "lines"),
    size = 3
  ) +  
  # coord_sf(expand = FALSE) +
  # scale_x_continuous(breaks = seq(-170, -60, by = 30)) +
  # scale_y_continuous(breaks = seq(20, 80, by = 20)) +
  theme_minimal() +
  theme(
    legend.background = element_rect(fill = "#FAEBD7", color = "black", size = 0.5),
    # legend.key = element_rect(fill = "#FAEBD7", color = NA),
    panel.background = element_rect(fill = "#a6cee360", color = NA),  # light blue ocean
    panel.grid.major = element_line(color = "gray80", size = 0.2),
    # Place legend inside
    legend.position = c(0.93, 0.93),         # x = 0-1, y = 0-1 (top-right corner)
    legend.justification = c("right", "top"),
    # panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8)
    # panel.grid.major = element_line(color = "grey80", linetype = "dotted")
  ) + labs(y = element_blank(), x = element_blank(), fill = 'Land Cover')

# Inset map
inset_map <- ggplot() +
  geom_sf(data = world, fill = "grey80", color = "white") +
  geom_sf(data = sites_sf, fill = "gold", color = "black", size = 2, shape = 21) +
  coord_sf(xlim = c(-168, -62), ylim = c(22, 78)) +  # shrink a bit
  theme_void() +
  theme(
    panel.background = element_rect(fill = "white", color = "black", linewidth = 0.8)
  )

final_map <- ggdraw() +
  draw_plot(main_map) +
  draw_plot(inset_map, x = 0.7, y = 0.58, width = 0.16, height = 0.16)

ggsave("figs/final/figure1.png", final_map, width = 7.5, height = 10, bg = "white")


# ggsave('figs/maps/sit_map_globcov.png', width = 7.5, height = 10, bg = 'white')
ggsave('figs/final/figure1.png', width = 7.5, height = 10, bg = 'white')


# OLDER ONE WITH ECOZONE (missing below)

# bg <- read_sf('data/gis/provincial_boundary/lpr_000b16a_e.shp') %>% filter(
#   PRENAME %in% c('British Columbia', 'Yukon', 'Alberta', 'Northwest Territories', 'Saskatchewan')
# ) 
# 
# target_crs <- st_crs(bg)
# 
# sites <- data.frame(
#   site = c('Wolf Creek', 'Russell Creek', 'Fortress Mountain', 'Marmot Creek'),
#   lon = c(-134.95283, -126.3090,-115.1983, -115.155154),
#   lat = c(60.596, 50.3710, 50.8269, 50.9255877),
#   ele = c(750, 1000, 2100, 1850)
# )
# 
# sites_sf <- sf::st_as_sf(sites, coords = c('lon', 'lat'), crs = 4326)
# 
# sf::write_sf(sites_sf, 'data/gis/site_coords.gpkg')
# sf::write_sf(sites_sf, 'data/gis/site_coords.kml')
# write.csv(sites, 'data/gis/site_coords.csv', row.names = F)
# 
# 
# filter_box <- st_bbox(st_buffer(sites_sf, dist = 55000)) |>
#   st_as_sfc()
# 
# # canada <- spData::world %>% dplyr::filter(name_long == "Canada")
# 
# disp_win_wgs84 <- st_sfc(st_point(c(-130, 43)), st_point(c(-122, 67)),
#                          crs = 4326)
# disp_win_trans <- st_transform(disp_win_wgs84, crs = target_crs)
# 
# disp_win_coord <- st_coordinates(disp_win_trans)
# 
# png('figs/site-map-ecozone.jpg', width=6, height=5, units="in", res=300)
# 
# ggplot(data = bg) +
#   geom_sf() +
#   geom_sf(data = sites_sf) +
#   scale_fill_viridis_d(alpha = 0.8, option = "H") +
#   # coord_sf(xlim = c(bb$xmin, bb$xmax), ylim = c(bb$ymin, bb$ymax)) +
#   # coord_sf(xlim = c(-140, -100), ylim = c(0, 20000)) +
#   coord_sf(xlim = disp_win_coord[,'X'], ylim = disp_win_coord[,'Y'],
#            datum = 4326, expand = FALSE) +
#   theme_bw() +
#   geom_label_repel(
#     data = sites_sf,
#     aes(geometry = geometry, label = site),
#     stat = "sf_coordinates",
#     box.padding = unit(2, "lines")
#   ) +
#   theme(axis.title.x = element_blank(),
#         axis.title.y = element_blank())
# 
# dev.off()
# 
# # tmap::tm_shape(bg, bbox = bb) +
# #   tm_fill() +
# #   tm_borders(col = 'grey') +
# #   tm_shape(ecozones) +
# #   tm_fill(col = 'ZONE_NAME') +
# #   tm_shape(sites_sf) +
# #   tm_dots(col = 'red', size = 0.6) +
# #   tm_text("site", size = 2, auto.placement = F, just = 'bottom') +
# #   tm_graticules(n.x = 3, n.y = 4)
# 
# # tmap::tmap_save(map, 'figs/site_map.jpg')
# 
# # ggsave('figs/site_map_ecozone.jpg')
