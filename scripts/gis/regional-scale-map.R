library(sf)
library(tidyverse)
library(tmap)
library(ggrepel)

data(land)

sites <- data.frame(
  site = c('Wolf Creek', 'Russell Creek', 'Fortress Mountain', 'Marmot Creek'),
  lon = c(-135.1497, -126.3090,-115.1983, -115.155154),
  lat = c(60.567, 50.3710, 50.8269, 50.9255877)
)

sites_sf <- sf::st_as_sf(sites, coords = c('lon', 'lat'), crs = st_crs(land))

# sf::write_sf(sites_sf, 'data/gis/site_coords.gpkg')

bb_canusa <- st_bbox(c(
  xmin = -165,
  ymin =  35,
  xmax = -85,
  ymax =  75
), crs = st_crs(4326)) # assuming WGS 84

land_crop <- sf::st_crop(land, bb_canusa) |> 
  select(elevation)

land_df <- as.data.frame(land_crop, xy = TRUE, na.rm = FALSE)
ggplot() +
  geom_raster(data = land_df, aes(x = x, y = y, fill = elevation)) +
  scale_fill_gradientn(
    colors = terrain.colors(100),
    name = "Elevation (m)",
    na.value = "transparent"
  ) +
  geom_sf(data = sites_sf, color = 'red', size = 1.5) +
  geom_label_repel(
    data = sites_sf,
    aes(geometry = geometry, label = site),
    stat = "sf_coordinates",
    box.padding = unit(1, "lines"),
    size = 3
  ) +  
  coord_sf(expand = FALSE) +
  scale_x_continuous(breaks = seq(-170, -60, by = 30)) +
  scale_y_continuous(breaks = seq(20, 80, by = 20)) +
  theme_minimal() +
  theme(
    legend.position = c(0, 0.01),
    legend.justification = c("left", "bottom"),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8)
    # panel.grid.major = element_line(color = "grey80", linetype = "dotted")
  ) +
  labs(x = element_blank(),
       y = element_blank())

ggsave('figs/maps/sit_map_ele.png', width = 5, height = 5, bg = 'white')

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
