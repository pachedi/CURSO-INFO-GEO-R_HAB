
#x=T
while(TRUE){
  

url='https://apitransporte.buenosaires.gob.ar/colectivos/vehiclePositionsSimple?'


request <- GET(
  url = url,
  query = list(client_id = client_id,
               client_secret = client_secret)
)

request$status_code

response <- content(request, as = "text", encoding = "UTF-8")


df <- fromJSON(response) %>% 
  data.frame()

cientotreinta <- df %>% 
  filter(route_short_name =="101A")

bondi <- makeIcon(
  iconUrl = "https://www.shareicon.net/download/128x128//2015/10/19/658777_transport_512x512.png",
  iconWidth = 30, iconHeight = 30)


mapa <- leaflet(cientotreinta) %>%
  addTiles() %>%  
  addMarkers(lng =  ~ longitude,
             lat =  ~ latitude )#,
             #icon =bondi)

print(mapa)


Sys.sleep(20)
}
