suppressMessages({
  library(tidytransit)
  library(tidyverse)
  library(tmap)
  library(ggplot2)
  library(here)
  library(units)
  library(sf)
  library(leaflet)
  library(tidycensus)
  library(plotly)
  library(tidygraph)
  library(leafsync)
  library(here)})

install.packages("tidycensus")

bondi <- read_gtfs("colectivos.zip")

bondisf <- tidytransit::gtfs_as_sf(bondi, crs = 4326)
bondisf$stops %>% head() 

a <- leaflet(bondisf$shapes) %>% # add data to display
  addProviderTiles(providers$CartoDB.DarkMatter) %>% # add BaseMap
  addPolylines(weight = 1, color = "red") %>% # add lines
  addControl(htmltools::HTML("Route shapes")) # add Title 

b <- leaflet(bondisf$stops) %>% # add data to display
  addProviderTiles(providers$CartoDB.DarkMatter) %>% # add BaseMap
  addCircles(weight = 3, color = "red") %>% # add circles
  addControl(htmltools::HTML("Stop shapes"))

a
b
trip_routes <- bondi$trips %>% 
  full_join(bondi$routes, by = "route_id")

trip_shape <- bondisf$shapes %>% 
  full_join(bondi$trips %>% 
              select(shape_id, trip_id), 
            by = "shape_id")

# Merging the two into one and then taking only one row for each
# unique combination of route_id and shape_id.
route_trip_shape <- trip_shape %>% 
  select(-shape_id) %>% 
  full_join(trip_routes, by = c("trip_id")) %>% 
  group_by(shape_id, route_id) %>% 
  slice(1)

# Route type is not really intuitive - let's fix that
route.shape <- route_trip_shape %>% 
  mutate(route_type = case_when(
    route_type == "0" ~ 'Tram, Streetcar',
    route_type == "1" ~ 'Subway, Metro',
    route_type == "2" ~ 'Rail',
    route_type == "3" ~ 'Bus'
  ))
pal <- leaflet::colorFactor(c("red", "orange", "pink"), domain = route.shape$route_type)

route.shape %>% 
  leaflet::leaflet(data = .) %>% 
  leaflet::addProviderTiles(providers$CartoDB.DarkMatter) %>% 
  leaflet::addPolylines(color = ~pal(route_type), 
                        weight = 3,
                        opacity = 0.9,
                        popup = paste0("Route type: ", route.shape$route_type))

###


Jersey_buffer <- route.shape %>% 
  sf::st_transform(crs = 4326) %>% 
  sf::st_buffer(dist = units::set_units(400, "m")) 

# To union the buffer polygons by route_type
Jersey_buffer_group <- Jersey_buffer %>% 
  group_by(route_type) %>% 
  summarise()

# Buffering POINTS ---------------------------------------
# Just to show that lines & points overlap well
Jersey_stop_buffer <- bondisf$stops %>% 
  st_transform(crs = 4326) %>% 
  st_buffer(dist = units::set_units(400, "m")) %>% 
  st_union()

#################
pal <- colorFactor(palette = c("red", "yellow", "blue"), domain = Jersey_buffer_group$route_type)

leaflet() %>% 
  addProviderTiles(providers$CartoDB.DarkMatter) %>% 
  
  addPolygons(data = Jersey_buffer_group %>% st_transform(crs = 4326), col = ~pal(route_type),
              popup = Jersey_buffer_group$route_type,
              group = "Buffer from Line") %>%
  
  addPolygons(data = Jersey_stop_buffer %>% st_transform(crs = 4326),
              weight = 0.5, 
              color = "white",
              group = "Buffer from Point") %>% 
  addLayersControl(
    overlayGroups = c("Buffer from Line", "Buffer from Point"),
    options = layersControlOptions(collapsed = FALSE)
  )

#####

Jersey_buffer <- route.shape %>% 
  sf::st_transform(crs = 26967) %>% 
  sf::st_buffer(dist = units::set_units(400, "m")) 

# To union the buffer polygons by route_type
Jersey_buffer_group <- Jersey_buffer %>% 
  group_by(route_type) %>% 
  summarise()

leaflet() %>% 
  addProviderTiles(providers$CartoDB.DarkMatter) %>% 
  #addPolygons(data = acs2020c %>% st_transform(crs = 4326), fillOpacity = 0.2, color = "white", weight = 0.5, group = 'Census') %>% 
  addPolygons(data = Jersey_buffer_group %>% st_transform(crs = 4326), fillOpacity = 0.2, color = "yellow", weight  = 0.5, group = 'GTFS')
  
### CENSO 

partidos_gba <- st_read("Codgeo_Buenos_Aires_con_datos/Buenos_Aires_con_datos.shp",
                        crs=4326)


partidos_gba <- st_transform(partidos_gba , crs =4326)

caba <- sf::st_as_sf(caba)

library(geojsonio)
st_write(caba, "caba.shp",crs=4326)

caba <- st_read(caba)

nbi_deptos <- read_sf("https://github.com/martoalalu/clase-geo-salud/raw/990c36ecec182d3f8ff4dffc921c233160cb8d43/data/nbi_deptos.geojson")

class(nbi_deptos)
class(caba)

### transformar multi surf a multipol
pol = st_geometry(caba)
class(pol) = c("XY", "MULTISURFACE", "sfg")
pol
#try(st_cast(pol, "MULTIPOLYGON")) # fails!
st_cast(st_sfc(pol), "MULTIPOLYGON")
caba=st_cast(st_sf(a = 1, st_sfc(pol)), "MULTIPOLYGON")


#######
pol = st_geometry(veinticuatro)
class(pol) = c("XY", "MULTISURFACE", "sfg")
pol
#try(st_cast(pol, "MULTIPOLYGON")) # fails!
st_cast(st_sfc(pol), "MULTIPOLYGON")
veinticuatro=st_cast(st_sf(a = 1, st_sfc(pol)), "MULTIPOLYGON")

#########
pol = st_geometry(cuarenta)
class(pol) = c("XY", "MULTISURFACE", "sfg")
pol
#try(st_cast(pol, "MULTIPOLYGON")) # fails!
st_cast(st_sfc(pol), "MULTIPOLYGON")
cuarenta=st_cast(st_sf(a = 1, st_sfc(pol)), "MULTIPOLYGON")


amba <- bind_rows(caba, veinticuatro)

rmba_40 <- bind_rows(caba, cuarenta)


leaflet() %>% 
  addProviderTiles(providers$CartoDB.DarkMatter) %>% 
  addPolygons(data = rmba_40)

st_crs(amba) <- 4326

st_crs(rmba_40) <- 4326

ggplot()+
  geom_sf(data=caba)
caba

####


buffer_intersect_tract <- rmba_40 %>% 
  # Unify the CRS
  st_transform(crs = st_crs(Jersey_buffer_group)) %>% 
  # Intersection
  st_intersection(Jersey_buffer_group) %>% 
  # Extract bus routes only
  filter(route_type == "Bus")


leaflet() %>% 
  addProviderTiles(providers$CartoDB.DarkMatter) %>% 
  addPolygons(data = rmba_40 %>% st_transform(crs = 4326), fillOpacity = 0.2, color = "white", weight = 0.5, group = 'Census') %>% 
  addPolygons(data = Jersey_buffer_group %>% st_transform(crs = 4326), fillOpacity = 0.2, color = "yellow", weight  = 0.5, group = 'GTFS') %>% 
  addPolygons(data = buffer_intersect_tract %>% st_transform(crs = 4326), group = "Intersection",
               fillOpacity = 0.9, weight = 0.5, opacity = 0.3, color = "grey") %>%     # leaflet takes 4326
  addLayersControl(overlayGroups = c("Census", "GTFS", "Intersection"), options = layersControlOptions(collapsed = FALSE))

geojsonio::geojson_write(amba, geometry = "polygon", crs=4326,
                         file = "amba.geojson")

geojsonio::geojson_write(rmba_40, geometry = "polygon", crs=4326,
                         file = "rmba_40.geojson")

rmba_open <- st_read("rmba.shp")

st_write(rmba_40, "rmba.shp", crs=4326)
st_write(amba, "amba.shp", crs=4326)

st_write(caba, "caba.shp", crs=4326)

cuarenta <- c("Almirante Brown", "Avellaneda", "Berazategui", 
              "Berisso", "Brandsen", "Campana", "Cañuelas", "Ensenada", "Escobar", 
              "Esteban Echeverría", "Exaltación de la Cruz", "Ezeiza", "Florencio Varela", 
              "General Las Heras", "General Rodríguez", "General San Martín", "Hurlingham", 
              "Ituzaingó", "José C. Paz", "La Matanza", "Lanús", "La Plata", "Lomas de Zamora", 
              "Luján", "Marcos Paz", "Malvinas Argentinas", "Moreno", "Merlo", "Morón", "Pilar", 
              "Presidente Perón", "Quilmes", "San Fernando", "San Isidro", "San Miguel", 
              "San Vicente", "Tigre", "Tres de Febrero", "Vicente López", "Zárate")




