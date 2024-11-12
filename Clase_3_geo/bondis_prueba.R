

url='https://apitransporte.buenosaires.gob.ar/colectivos/feed-gtfs?'


request <- GET(
  url = url,
  query = list(client_id = vclient_id,
               client_secret = vclient_secret)
)

request$status_code

# para zip.
dat <- request$content

con <- file("colectivos.zip", open = "wb")

writeBin(dat, con)

close(con)

zipF<- "./colectivos.zip"
outDir<-"./colectivos"
unzip(zipF,exdir=outDir)

lista <- list.files("./colectivos/")

for(i in lista){
  
  a = read.table(paste0("colectivos/", i), sep= ",", header=T)
  
  assign( i , a ,envir = globalenv())
  
}

# stop_times , stops , trips --> shapes / calendar dates, routes, agency, 

joinear <- left_join(stop_times.txt, stops.txt, by="stop_id")

joinear <- left_join(joinear, trips.txt, by="trip_id")

joinear <- left_join(joinear, routes.txt, by= "route_id")

joinear <- left_join(joinear, agency.txt, by= "agency_id")

unique(joinear$trip_short_name)

names(joinear)



sesenta <- joinear %>% 
  filter(agency_id == "138")

sesenta_2 <- inner_join(sesenta, calendar_dates.txt, by= "service_id")
rm(stop_times.txt)
rm(sesenta_2)
gc()
#names(joinear)

m = leaflet() %>%
  # Add CartoDB background map
  addProviderTiles("CartoDB.DarkMatter") %>%  
  # Add a marker for each stop
  addCircleMarkers(lng= ~ stop_lon, lat= ~stop_lat, data = sesenta,
                   stroke = FALSE, fillOpacity = 0.5, radius =5 ) 
m  # Show the map

sesenta_prueba <- sesenta %>% 
group_by(agency_id,stop_id) %>% summarise(cnt=n(),
                                          lat= stop_lat, lon= stop_lon,
                                          stop_id) %>% 
  unique()


library(knitr)
kable(head(sesenta_prueba))


m = leaflet() %>%
  # Add CartoDB background map
  addProviderTiles("CartoDB.DarkMatter") %>%  
  # Add a marker for each stop
  addCircleMarkers(lng= ~ lon, lat= ~lat, data = sesenta_prueba, stroke = FALSE, 
                   fillOpacity = 0.5, radius =3)
m  # Show the map


install.packages("tidytransit")
install.packages("rlang")
library(tidytransit)
library(rlang)


bondi <- read_gtfs("colectivos.zip")

summary(bondi)
head(bondi$stops)


names(bondi)
validation_result <- attr(bondi, "validation_result")
head(validation_result)

library(sf)

gtfs <- bondi %>% 
  gtfs_as_sf()

agency_names <- gtfs$agency %>% select(agency_id, agency_name)

trips <- gtfs$trips %>%
  # -- remove duplicate trips by filtering by unique shape_id
  distinct(shape_id, .keep_all = TRUE) %>%
  left_join(gtfs$routes, by = "route_id") %>%
  # add agency name column
  left_join(agency_names, by = "agency_id")


trip_shapes_freq <- gtfs$trips %>%
  left_join(gtfs$routes, by = "route_id") %>%
  #filter(agency_id != "BOX") %>%
  left_join(agency_names, by = "agency_id") %>%
  left_join(gtfs$shapes, by = "shape_id") %>%
  # convert from df to sf
  st_as_sf() %>%
  # get trip length
  mutate(trip_length_km = as.integer(st_length(.) / 1000))


unique(trip_shapes_freq$agency_name)

# add frequency of each trip from the 'frequencies' file in the GTFS
trip_shapes_freq <- trip_shapes_freq %>%
  # remove agencies that we don't care about
  dplyr::filter(agency_id == "EXPRESO QUILMES S.A." ) %>%
  left_join(gtfs$frequencies, by = "trip_id") %>%
  # get headway in minutes as it is more intuitive
  mutate(headway_mins = round(headway_secs/60, 0))

class(trip_shapes_freq)

ggplot(data=trip_shapes_freq)+
  geom_sf()

