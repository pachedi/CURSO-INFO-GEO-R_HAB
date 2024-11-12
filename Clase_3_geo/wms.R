

wfs_regions <- "https://geotematico01.conae.gov.ar/geoserver/wfs"

regions_client <- WFSClient$new(wfs_regions, 
                                serviceVersion = "2.0.0")

datos <- as.data.frame(regions_client$getFeatureTypes(pretty = TRUE))


url <- parse_url(wfs_regions)

url$query <- list(service = "wfs",
                  version = "2.0.0", # opcional
                  request = "GetFeature",
                  typename = "Localidades:Centro_huellas_localidades",
                  srsName = "EPSG:4326"
)

request <- build_url(url)

library(raster)


focos_calor <- read_sf(request)

imagestack <- stack(focos_calor)

ggplot() +
  geom_sf(data= focos_calor)


class(focos_calor)


############
library(leaflet)


mapa_satelite <- "https://geoservicios.conae.gov.ar/geoserver/SACC/wms"

ab <- "https://geoservicios.conae.gov.ar/geoserver/SACC/wms?Getcapabilities"


wfs_regions <- "https://geoservicios.conae.gov.ar/geoserver/SACC/wms"

regions_client <- WMSClient$new(wfs_regions)
                                
dengue <- "https://geoservicios.conae.gov.ar/geoserver/GeoServiciosCONAE/wms"

leaflet() %>% 
  addTiles() %>% 
  setView(lng =  -58.3895861, lat=-34.6107111  , zoom = 9) %>%
  addWMSTiles(
    mapa_satelite,
    layers = "Mosaico_SACC",
    options = WMSTileOptions(format = "image/png", transparent = TRUE)
  ) %>% 
  addWMSTiles(
    dengue,
    layers = "GeoServiciosCONAE:FocosDeCalor",
    options = WMSTileOptions(format = "image/png", transparent = TRUE)
  ) %>% 
  addMarkers(data=com_indi, ~lon, ~lat, popup = ~as.character(nom_com) ,
             clusterOptions = markerClusterOptions())  
  


com_indi <- com_indi %>%
  dplyr::mutate(lon = sf::st_coordinates(.)[,1],
                lat = sf::st_coordinates(.)[,2])
  

?addWMSTiles()

wms_grb <- "https://geo.api.vlaanderen.be/GRB-basiskaart/wms?"
leaflet() %>% 
  setView(lng = 4.287638, lat = 50.703039, zoom = 15) %>%
  addWMSTiles(
    wms_grb,
    layers = "Mosaico_SACC",
    options = WMSTileOptions(format = "image/png", transparent = TRUE)
  )
