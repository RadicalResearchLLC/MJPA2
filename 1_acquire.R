# March JPA data from County of Riverside

library(sf)
library(leaflet)
library(tidyverse)
library(htmltools)

wd <- getwd()
MJPA_dir <- paste0(wd, '/boundary/')
cities_dir <- paste0(wd, '/Cities')

source('polygonTest.R')

MJPA <- read_sf(dsn = MJPA_dir ) %>% 
  st_transform("+proj=longlat +ellps=WGS84 +datum=WGS84")

city_names <- c('RIVERSIDE', 'PERRIS', 'MORENO VALLEY')

cities <- read_sf(dsn = cities_dir)  %>% 
  st_transform("+proj=longlat +ellps=WGS84 +datum=WGS84") %>% 
  filter(CITYNAME %in% city_names) %>% 
  mutate(city.name = str_to_title(CITYNAME))

WH.url <- 'https://raw.githubusercontent.com/RadicalResearchLLC/WarehouseMap/main/WarehouseCITY/geoJSON/warehouse.geoJSON'
warehouses <- st_read(WH.url) %>% 
  st_transform("+proj=longlat +ellps=WGS84 +datum=WGS84") %>% 
  filter(county == 'Riverside')

palCity <- colorFactor(palette = c('brown', 'yellow', 'orange'), domain = cities$city.name)

leaflet() %>% 
  addTiles() %>% 
  addProviderTiles(provider = providers$Esri.WorldImagery) %>% 
  setView(lng = -117.25, lat = 33.89, zoom = 12) %>% 
  addPolygons(data = cities,
              stroke = TRUE,
              color = 'blue',
              fillOpacity = 0,
              weight = 3,
              group = 'Adjacent cities',
              label = ~htmlEscape(city.name)
  )  %>% 
  addPolygons(data = MJPA,
                  stroke = TRUE,
                  color = 'black',
                  weight = 5,
                  fillOpacity = 0, 
                  group = 'March JPA') %>% 
  addPolygons(data = warehouses,
              stroke = FALSE,
              color = 'red',
              fillOpacity = 0.5,
              group = 'warehouses',
              label = ~htmlEscape(paste('Parcel', apn, ';', 
                round(shape_area,0), 'sq.ft.', class, year_chr))) %>%
  addPolygons(data = Syc_WH,
              stroke = TRUE,
              weight = 2,
              color = 'red',
              fillOpacity = 0.5,
              label = ~htmlEscape(name),
              group = 'Sycamore Canyon WH') %>% 
  addPolygons(data = WCUP, 
              stroke = TRUE,
              weight = 2,
              color = 'red', 
              fillOpacity = 0.5,
              label = ~htmlEscape(name),
              group = 'West Campus Upper Plateau')
  
leaflet() %>% 
  setView(lng = -117.25, lat = 33.89, zoom = 12) %>% 
  addTiles() %>% 
  addProviderTiles(provider = providers$HERE.trafficFlow)

                 