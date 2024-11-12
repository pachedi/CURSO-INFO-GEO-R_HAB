

library(sf)
library(spData)
library(ggplot2)
library(cowplot)
library(rcartocolor)

install.packages("rcartocolor")

data("us_states", package = "spData")
north_carolina = read_sf(system.file("shape/nc.shp", package = "sf"))

us_states_2163 = st_transform(us_states, crs = 2163)
north_carolina_2163 = st_transform(north_carolina, crs = 2163)

north_carolina_2163_bb = st_as_sfc(st_bbox(north_carolina_2163))
mainbox <- st_bbox(north_carolina_2163)

# inset panel
inset = ggplot() + 
  geom_sf(data = us_states_2163, fill = "white") + 
  geom_sf(data = north_carolina_2163_bb, fill = NA, color = "yellow", size = 1.2) +
  theme_void()

# main panel
main = ggplot() + 
  geom_sf(data = north_carolina_2163, aes(fill = BIR74)) +
  scale_fill_carto_c(palette = "Mint") +
  geom_sf(data = north_carolina_2163_bb, fill = NA, color = "yellow", size = 1.2) +
  theme_void() +
  theme(legend.position = "none")

# map
#gg_inset_map1 = 
  cowplot::ggdraw() +
  coord_equal(xlim = c(0, 20), ylim = c(0, 20), expand = FALSE) +
  annotation_custom(ggplotGrob(inset), xmin = -5, xmax = 10, ymin = 8, ymax = 20) +
  annotation_custom(ggplotGrob(main), xmin = 0, xmax = 20, ymin = 0, ymax = 10)

  geom_segment(aes(x = 8.9, xend = 19, y = 14.4, yend = 9.2), color = "yellow", linewidth = 1.2) +
  geom_segment(aes(x = 7.5, xend = 1, y = 14.4, yend = 9.2), color = "yellow", linewidth = 1.2)


###############################################################################################

mainbox <- st_bbox(partido_quilmes)

partido_quilmes_bb = st_as_sfc(st_bbox(partido_quilmes))

# inset panel
inset = ggplot() +
  geom_sf(data = caba, fill= "gray", col=NA)+
  geom_sf(data = amba, fill = "white") + 
  geom_sf(data = partido_quilmes_bb, fill = "NA", color = "yellow", lwd = 0.7) +
  theme_void()

# main panel
main = ggplot() + 
  geom_sf(data = radios_quilmes, aes(fill = totalpobl), col=NA) +
  scale_fill_carto_c(palette = "Mint", name="Población\npor radio censal") +
  geom_sf(data = partido_quilmes_bb, fill = NA, color = "yellow", lwd = 1.2) +
  labs(caption = "Fuente: Elaboración propia en base a datos del censo 2010")+
  theme_void()

# map
#gg_inset_map1 = 
  cowplot::ggdraw() +
  coord_equal(xlim = c(0, 20), ylim = c(0, 20), expand = FALSE) +
  annotation_custom(ggplotGrob(inset), xmin = 3.8, xmax = 12, ymin = 10, ymax = 20) +
  annotation_custom(ggplotGrob(main), xmin = -0.5, xmax = 20, ymin = 0, ymax = 9.6) +
  geom_segment(aes(x = 9.5, xend = 11.7, y = 14, yend = 9.3), color = "yellow", linewidth = 1.2) +
  geom_segment(aes(x = 8.9, xend = 3.4, y = 14, yend = 9.2), color = "yellow", linewidth = 1.2)

gg_inset_map1

?geom_segment
