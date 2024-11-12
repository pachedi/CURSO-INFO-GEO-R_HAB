library(sf)
library(tidyverse)
library(ggrepel)
library(units)

### presentacion
# puntos lineas poligonos
# simple feature
# shape file
# layeres o capas
# hablar de sig
# SRC
# coordenadas geograficas
# formatos, geo json
# fuentes
# tiff
# lat long

caba <- st_read("bases_clase_1/comunas/comunas_wgs84.shp")

ggplot()+
  geom_sf(data= caba, aes(fill=COMUNAS))

class(caba$COMUNAS)

caba <- caba %>% 
  mutate(COMUNAS = as.character(COMUNAS))

class(caba$COMUNAS)

ggplot()+
  geom_sf(data= caba, aes(fill=COMUNAS))

caba <- caba %>% 
  mutate(COMUNAS = factor(COMUNAS, levels= c(1:15)))

ggplot()+
  geom_sf(data= caba, aes(fill=COMUNAS))+
  theme_void()+
  theme(legend.position = "none")+
  labs(title= "Mapa de la CABA por Comuna")


rivadavia <- st_read("https://cdn.buenosaires.gob.ar/datosabiertos/datasets/jefatura-de-gabinete-de-ministros/calles/avenida_rivadavia.geojson")

ggplot()+
  geom_sf(data= caba, aes(fill=COMUNAS))+
  geom_sf(data = rivadavia, col="red", lwd= 1)+
  theme_void()+
  theme(legend.position = "none")+
  labs(title= "Mapa de la CABA por Comuna")

st_point_on_surface(caba$geometry)[[1]]

caba <- caba %>% 
  mutate(centroides = st_point_on_surface(geometry))
  

caba <- caba %>% 
  mutate(lon = map_dbl(geometry, ~st_point_on_surface(.x)[1]),
         lat = map_dbl(geometry, ~st_point_on_surface(.x)[2]))


 st_point_on_surface(caba$geometry)[[1]][2]

?map_dbl
ggplot()+
  geom_sf(data= caba, aes(fill=COMUNAS))+
  geom_sf(data = rivadavia, col="red", lwd= 1)+
  geom_sf(data = caba$centroides)+
  theme_void()+
  theme(legend.position = "none")+
  labs(title= "Mapa de la CABA por Comuna")


ggplot()+
  geom_sf(data= caba, aes(fill=COMUNAS))+
  geom_sf(data = rivadavia, col="red", lwd= 1)+
  geom_label_repel(data= caba, aes(x = lon, y = lat, label=COMUNAS))+
  theme_void()+
  theme(legend.position = "none")+
  labs(title= "Mapa de la CABA por Comuna")


escuelas <- st_read("bases_clase_1/establecimientos_educativos/establecimientos_educativos_wgs84.shp")

ggplot()+
  geom_sf(data=caba, aes(fill=COMUNAS), col=NA)+
  geom_sf(data= escuelas, alpha = 0.3)


escuelas_cantidad <- escuelas %>% 
  group_by(comuna) %>% 
  summarise( cantidad = n())


escuelas_cantidad <- escuelas_cantidad %>% 
  mutate(comuna = as.character(comuna),
    comunas = factor(comuna, levels= c(1:15)))

ggplot(data = escuelas_cantidad)+
  geom_col( aes(x = reorder( comuna, cantidad), 
                y = cantidad, fill= cantidad))+
  scale_fill_viridis_c(name= "Cantidad")+
  scale_y_continuous(breaks = seq(0,350,50))+
  coord_flip()+
  labs(x= "COMUNAS", y= "Cantidad")


escuelas_cantidad <- as.data.frame(escuelas_cantidad) %>% 
  select(-3)

union_caba <- left_join(caba, escuelas_cantidad,
                        by= c("COMUNAS" = "comuna"))


ggplot()+
  geom_sf(data=union_caba, aes(fill= cantidad))+
  geom_label_repel(data= union_caba, aes(x= lon, y= lat,
                                   label = cantidad))+
  scale_fill_viridis_c()+
  theme(legend.position = "none")









