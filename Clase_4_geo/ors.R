# install.packages("remotes")
remotes::install_github("GIScience/openrouteservice-r")



url <- "https://api.openrouteservice.org/v2/directions/driving-car?"

options(openrouteservice.url = "http://localhost:8080/ors")
#  start = "8.681495,49.41461"&
#  end = 8.687872,49.420318
  
  headers = c('Accept', 'application/json, application/geo+json, application/gpx+xml, img/png; charset=utf-8')
  
  request <- GET(
  url = url,
  query = list(api_key= "5b3ce3597851110001cf62482569a3c36df84e6cab6cefb4f4925f82",
               start=c("8.681495,49.41461"),
               end=c("8.687872,49.420318"))
)

  #options(timeout=100000)

#request2 <- GET("https://api.openrouteservice.org/v2/directions/driving-car?api_key=5b3ce3597851110001cf62482569a3c36df84e6cab6cefb4f4925f82&start=8.681495,49.41461&end=8.687872,49.420318")
  
  request$status_code

  
  response <- content(request, as = "text", encoding = "UTF-8")
  
  
  request$content
  
  df <- fromJSON(response) 
  
  #data = fromJSON(rawToChar(request$content))
  
  names(df)
  
  #ab <- data$features %>% 
  #  as.data.frame()
  
  
  #df <- fromJSON(response)  
  
  
  #df$features$geometry$coordinates
  
  
  route <- ors_directions(df$features$geometry$coordinates, format="geojson")
  
  df2 <- df$metadata %>% 
    as.data.frame()
  
#########  

  locations <- lapply(c("Heidelberg", "Kraków"), ors_geocode)
  
  
  coordinates <- lapply(locations, function(x) x$features[[1]]$geometry$coordinates)
  
  # find route 
  route <- ors_directions(coordinates, format="geojson")
  

  ###############
  leaflet() %>%
    addTiles() %>%
  addGeoJSON(data, fill=FALSE) %>%
    fitBBox(data$bbox)
  
  options(openrouteservice.url = "http://localhost:8080/ors")  
  
  
  options(openrouteservice.paths = list(directions = "v2/directions",
                                        isochrones = "v2/isochrones",
                                        matrix = "v2/matrix",
                                        geocode = "geocode",
                                        pois = "pois",
                                        elevation = "elevation",
                                        optimization = "optimization"))
  #options(internet.info = 0)
  library(openrouteservice)    
  library(httr)    
 # set_config(use_proxy(url="10.3.100.207",port=8080))
  ors_api_key("5b3ce3597851110001cf62482569a3c36df84e6cab6cefb4f4925f82")  

  
  coordinates <- list(c(8.34234, 48.23424), c(8.34423, 48.26424))
  
  x <- ors_directions(coordinates)  

  leaflet() %>%
    addTiles() %>%
    addGeoJSON(x, fill=FALSE) %>%
    fitBBox(x$bbox)
  
  
  ?ors_directions
  
  library(httr)
  #set_config(
  #  use_proxy(url="181.46.138.71", port="8080", username="user",password="password")
  #)  
  
  
  geometry <- x$routes[[1]]$geometry
  
  df$metadata
  
  
  x <- ors_geocode("Quilmes")
  
  leaflet() %>%
    addTiles() %>%
    addGeoJSON(x) %>%
    fitBBox(x$bbox)
  
  
  #Sys.setenv(ALL_PROXY = "127.0.0.1")
  
  
  Sys.getenv()
  
  
  locations <- lapply(c("Heidelberg", "Kraków"), ors_geocode)
  coordinates <- lapply(locations, function(x) x$features[[1]]$geometry$coordinates)
  
  
  
  
  x$features[[1]]$geometry$coordinates
  ########
  

  library(sf)
  library(tidyverse)
  coordinates <- list(point.lon="-58.381577",point.lat="-34.603755")
  
  y = ors_geocode(location = coordinates,
                  format = "geojson")
  
  ggplot()+
    geom_sf(data=y)
  
  leaflet() %>%
    addTiles() %>%
    addGeoJSON(y) %>%
    fitBBox(y$bbox)
  
  
 # https://api.openrouteservice.org /geocode/reverse? api_key = 5b3ce3597851110001cf62482569a3c36df84e6cab6cefb4f4925f82& point.lon = -58.381577& point.lat = -34.603755
  
  #####################
  
  rm(data_f1)
  rm(apilados)
  
apilados <- c()
for( i in 1:length(y$features)){  
  
  data_f1 <- y$features[[i]] %>%
    unlist() %>% 
    as.data.frame() %>% 
    rename(variables  = 1) %>% 
    mutate(nombres = rownames(.)) %>% 
    pivot_wider(values_from = variables, names_from = nombres) %>% 
    select(1:29)
  
  assign(paste("item_", i, sep=''),data_f1,envir = globalenv())
  
  nombre <- paste0("item_", i)
  
  apilados <- rbind(apilados, get(nombre))
  
  
}
  
?assign

  df <- data.frame(matrix(unlist(y$features[[1]]), nrow=length(y$features), byrow=TRUE))
  
  
  length(y$features)
  
  
coordenadas_x <- c()
coordenadas_y <- c()
etiqueta <- c()
pais <- c()
ciudad <- c()
comuna <- c()
barrio <- c()
  
  for(i in 1:length(y$features)){
    
    # coordenadas  
 coordenadas_x <- append(coordenadas_x,y$features[[i]]$geometry$coordinates[1])
    
 
 coordenadas_y <- append(coordenadas_y,y$features[[i]]$geometry$coordinates[2])
 
    #nombre
  etiqueta <- append(etiqueta,  y$features[[i]]$properties$label)
    
    #pais
    pais <- append(pais,y$features[[i]]$properties$country)
    
    # ciudad
    
  ciudad <- append(ciudad,  y$features[[i]]$properties$region)
    
    # barrio
    
    barrio <- append(barrio,y$features[[i]]$properties$neighbourhood)
    
    #comuna 
    
   comuna <- append(comuna, y$features[[i]]$properties$borough)
    
    
  }


resultado <- data.frame(coordenadas_x, coordenadas_y,
                        etiqueta, pais, ciudad, comuna, barrio)


df <- st_as_sf(x = df,                         
               coords = c("lon", "lat"),
               crs = projcrs)

resultado_sf <- st_as_sf(resultado,
                         coords = c("coordenadas_x", "coordenadas_y"),
                         crs = 4326)

st_write(resultado_sf, "obelisco.shp", crs=4326)


obelisco <- st_read("obelisco.shp", crs=4326)

caba = st_read("../Clase_3_geo/caba_radios/caba.shp")

barrios = st_read("../Clase_3_geo/barrios_caba/barrios_wgs84.shp")

ggplot()+
  geom_sf(data = filter(barrios, BARRIO=="SAN NICOLAS"))+
  geom_sf(data= resultado_sf, col= "red")


resultado <- resultado %>% 
  rename(lng = coordenadas_x, 
         coordenadas_y)

library(htmltools)
leaflet() %>% 
  addTiles() %>% 
  addMarkers(data = obelisco, lng= ~coordenadas_x,
             lat = ~coordenadas_y,
             popup = ~htmlEscape(etiqueta))

#install.packages("mapboxapi")

  # coordenadas  
y$features[[1]]$geometry$coordinates
  
#nombre
y$features[[1]]$properties$label

#pais
y$features[[1]]$properties$country

# ciudad

y$features[[1]]$properties$region

# barrio

y$features$properties$neighbourhood

#comuna 

y$features$properties$borough
