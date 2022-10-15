# March JPA data from County of Riverside

library(sf)
library(leaflet)
library(tidyverse)
library(htmltools)

wd <- getwd()
MJPA_dir <- paste0(wd, '/boundary/')
cities_dir <- paste0(wd, '/Cities')
app_dir <- paste0(wd, '/MarchJPA')

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

warehouses_narrow <- warehouses %>% 
  select(apn, geometry) %>% 
  rename(name = apn, geom = geometry)



buff_proj_1000 <- WCUP %>% 
  rbind(Syc_WH) %>% 
  rbind(warehouses_narrow) %>% 
  st_buffer(dist = 304)
buff_proj_800 <- WCUP %>%
  rbind(Syc_WH) %>% 
  rbind(warehouses_narrow) %>% 
  st_buffer(dist = 243.8)

palCity <- colorFactor(palette = c('brown', 'yellow', 'orange'), domain = cities$city.name)

leaflet() %>% 
  addTiles() %>% 
  addProviderTiles(provider = providers$Esri.WorldImagery, 
                   group = 'Imagery') %>% 
  addLayersControl(baseGroups = c('Basemap', 'Imagery'),
                   overlayGroups = c('City boundaries', 'MJPA boundary',
                                     'Existing warehouses',
                                     'Under construction warehouses',
                                     'West Campus Upper Plateau',
                                     '800 foot buffer - City policy',
                                     '1000 foot buffer - WRCOG GNG'),
                   options = layersControlOptions(collapsed = FALSE)) %>% 
  hideGroup(c('800 foot buffer - City policy',
              '1000 foot buffer - WRCOG GNG')) %>% 
  setView(lng = -117.25, lat = 33.87, zoom = 12) %>% 
  addPolygons(data = cities,
              stroke = TRUE,
              color = 'blue',
              fillOpacity = 0,
              weight = 3,
              group = 'City boundaries',
              label = ~htmlEscape(city.name)
  )  %>% 
  addPolygons(data = MJPA,
                  stroke = TRUE,
                  color = 'black',
                  weight = 5,
                  fillOpacity = 0, 
                  group = 'MJPA boundary') %>% 
  addPolygons(data = warehouses,
              stroke = FALSE,
              color = 'red',
              fillOpacity = 0.5,
              group = 'Existing warehouses',
              label = ~htmlEscape(paste('Parcel', apn, ';', 
                round(shape_area,0), 'sq.ft.', class, year_chr))) %>%
  addPolygons(data = Syc_WH,
              stroke = TRUE,
              weight = 2,
              color = 'red',
              fillOpacity = 0.5,
              label = ~htmlEscape(name),
              group = 'Under construction warehouses') %>% 
  addPolygons(data = buff_proj_800,
              stroke = FALSE,
              color = 'brown',
              fillOpacity = 0.3,
              group = '800 foot buffer - City policy') %>% 
  addPolygons(data = buff_proj_1000,
              stroke = FALSE,
              color = 'brown',
              fillOpacity = 0.3,
              group = '1000 foot buffer - WRCOG GNG') %>% 
  addPolygons(data = WCUP, 
              stroke = TRUE,
              weight = 2,
              color = 'red', 
              fillOpacity = 0.5,
              label = ~htmlEscape(name),
              group = 'West Campus Upper Plateau')  

setwd(app_dir)
save.image('.RData')


                 