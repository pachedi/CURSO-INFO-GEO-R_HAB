wfs_regions <- "http://geoservicios.indec.gov.ar/geoserver/wfs"
regions_client <- WFSClient$new(wfs_regions, 
                                serviceVersion = "2.0.0")
regions_client$getFeatureTypes(pretty = TRUE)


library(httr)
library(ows4R)

#install.packages("ows4R")
wfs_bwk <- "http://geoservicios.indec.gov.ar/geoserver/wfs"

url <- parse_url(wfs_bwk)

url$query <- list(service = "wfs",
                  #version = "2.0.0", # facultative
                  request = "GetCapabilities"
)
request <- build_url(url)
request

bwk_client <- WFSClient$new(wfs_bwk, 
                            serviceVersion = "2.0.0") #serviceVersion must be provided here

bwk_client

bwk_client$getFeatureTypes(pretty = TRUE)

bwk_client$getCapabilities()


bwk_client$
  getCapabilities()$
  findFeatureTypeByName("geocenso2010:datos_basicos_provincias")$
  getDescription() %>%
  map_chr(function(x){x$getName()})


bwk_client$getFeatureTypes() %>%
  map_chr(function(x){x$getTitle()})


bwk_client$getFeatureTypes() %>%
  map_chr(function(x){x$getName()})


#### buiild

wfs_regions <- "http://geoservicios.indec.gov.ar/geoserver/wfs"
regions_client <- WFSClient$new(wfs_regions, 
                                serviceVersion = "2.0.0")
regions_client$getFeatureTypes(pretty = TRUE)

url <- parse_url(wfs_regions)
url$query <- list(service = "wfs",
                  #version = "2.0.0", # optional
                  request = "GetFeature",
                  typename = "geocenso2010:datos_basicos_provincias",
                  srsName = "EPSG:4326"
)
request <- build_url(url)

bel_regions <- read_sf(request) #Lambert2008


ggplot(bel_regions) +
  geom_sf()







