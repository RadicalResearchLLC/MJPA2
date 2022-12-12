# March JPA data from County of Riverside

library(sf)
library(leaflet)
library(tidyverse)
library(htmltools)
library(leaflet.extras)

wd <- getwd()
MJPA_dir <- paste0(wd, '/boundary/')
cities_dir <- paste0(wd, '/Cities')
app_dir <- paste0(wd, '/MarchJPA')
city_receptors <- paste0(wd, '/City_Sensitive_receptors')

source('polygonTest.R')

MJPA <- read_sf(dsn = MJPA_dir ) %>% 
  st_transform("+proj=longlat +ellps=WGS84 +datum=WGS84")

city_names <- c('RIVERSIDE', 'PERRIS', 'MORENO VALLEY')

cities <- read_sf(dsn = cities_dir)  %>% 
  st_transform("+proj=longlat +ellps=WGS84 +datum=WGS84") %>% 
  filter(CITYNAME %in% city_names) %>% 
  mutate(city.name = str_to_title(CITYNAME))

RivCO_cdp <- read_sf(dsn = 'C:/Dev/MJPA2/RivCo_CDP/Census_2010_Designated_Places.geojson') %>% 
  st_transform("+proj=longlat +ellps=WGS84 +datum=WGS84") %>% 
  filter(PLACE_NAME %in% c('Mead Valley', 'March ARB'))  

WH.url <- 'https://raw.githubusercontent.com/RadicalResearchLLC/WarehouseMap/main/WarehouseCITY/geoJSON/finalParcels.geojson'
warehouses <- st_read(WH.url) %>% 
  st_transform("+proj=longlat +ellps=WGS84 +datum=WGS84") %>% 
  filter(county == 'Riverside')

warehouses_narrow <- warehouses %>% 
  select(apn, geometry, floorSpace.sq.ft, shape_area) %>% 
  rename(name = apn, geom = geometry, area = shape_area) %>% 
  mutate(type = 'existing') %>% 
  select(name, type, floorSpace.sq.ft, geom, area ) %>% 
  mutate(jurisdiction = '')

WCUP$shape <- st_area(WCUP)

WCUP <- WCUP %>% 
  mutate(type = 'WCUP',
         floorSpace.sq.ft = round(as.numeric(0.5*10.764*shape),0),
         area = round(as.numeric(10.764*shape), 0)) %>% 
  select(-shape) 

WH_uCons$shape <- st_area(WH_uCons)

WH_uCons <- WH_uCons %>% 
  mutate(type = 'under construction',
         floorSpace.sq.ft = round(as.numeric(0.5*10.764*shape), 0),
         area = round(as.numeric(10.764*shape), 0)) %>% 
  select(-shape) 

JPA <- bind_rows(WCUP, WH_uCons) %>%
  mutate(jurisdiction = 
           case_when(
             name == 'Sycamore Canyon 1' ~ 'Riverside',
             name == 'Sycamore Canyon 2' ~ 'Riverside',
             TRUE ~ 'MarchJPA')) #%>%

MJPA2 <- st_union(MJPA) %>% 
  st_transform("+proj=longlat +ellps=WGS84 +datum=WGS84")

areaValue <- st_area(MJPA2)

MJPA3 <- st_as_sf(MJPA2) %>% 
  mutate(name = 'MarchJPA',
         #convert m^2 to ft^2
         area = as.numeric(areaValue*10.7639)) %>% 
  rename(geometry = x) 

Unincorp_RivCo1 <- st_union(RivCO_cdp) %>% 
  st_as_sf() %>% 
  st_transform("+proj=longlat +ellps=WGS84 +datum=WGS84") %>% 
  rename(geometry = x) 

Unincorp_RivCo2 <- Unincorp_RivCo1 %>% 
  st_difference(MJPA3) %>% 
  mutate(name = 'Unincorporated RivCo')

areaValue2 <- st_area(Unincorp_RivCo2)

Unincorp_RivCo2 <- Unincorp_RivCo2 %>% 
  mutate(area = as.numeric(areaValue2*10.7639))

jurisdictions <- cities %>% 
  select(city.name, SHAPESTAre, geometry) %>% 
  rename(name = city.name, area = SHAPESTAre) %>% 
  rbind(MJPA3, Unincorp_RivCo2) %>% 
  arrange(name)

rm(ls = MJPA, MJPA2, MJPA3, RivCO_cdp, Unincorp_RivCo1, Unincorp_RivCo2, cities)

juris_warehouses <-  warehouses_narrow %>% 
  st_intersects(jurisdictions)

for(i in 1:length(juris_warehouses)) {
  print(juris_warehouses[[i]])
  #print(city_names$city[[juris_warehouses[[i]]]])
  print(length(juris_warehouses[[i]]))
  ifelse(length(juris_warehouses[[i]]) == 0,
         warehouses_narrow$jurisdiction[[i]] == '',
         ifelse(length(juris_warehouses[[i]]) == 1,
                {warehouses_narrow$jurisdiction[[i]] = jurisdictions$name[[juris_warehouses[[i]]]]},
                {warehouses_narrow$jurisdiction[[i]] = 'more than one city'})
  )
  
}  

warehouses_tidy <- warehouses_narrow %>% 
  filter(jurisdiction != '') %>% 
  mutate(jurisdiction = 
           case_when(
             name == '297080015' ~ 'Unincorporated RivCo',
             name == '297080016' ~ 'Unincorporated RivCo',
             name == '297100095' ~ 'MarchJPA',
             name == '297160005' ~ 'MarchJPA',
             name == '297200004' ~ 'MarchJPA',
             name == '294180055' ~ 'Perris',
             name == '294180031' ~ 'Perris',
             name == '294190080'~ 'Perris',
             name == '294210060'~ 'Perris',
             name == '291020023'~ 'Moreno Valley',
             TRUE ~ jurisdiction
           )) %>% 
  arrange(jurisdiction)

buff_proj_1000 <- JPA %>% 
  rbind(warehouses_tidy) %>% 
  st_buffer(dist = 304)

buff_proj_800 <- JPA %>%
  rbind(warehouses_tidy) %>% 
  st_buffer(dist = 243.8)

receptors <- sf::st_read(dsn = city_receptors)
leaflet() %>% 
  addTiles() %>% 
  addWebGLHeatmap(data = receptors, size = 200, units = 'm',
                  opacity = 0.3, intensity = 1)


palJuris <- colorFactor(palette = c('red', 'orange', 'brown', 'blue', 'gold'), 
                        domain = jurisdictions$name)
palWarehouseJuris <- colorFactor(palette = c('red', 'orange', 'brown', 'blue', 'gold'), 
                                 domain = warehouses_tidy$jurisdiction)
leaflet() %>% 
  addTiles() %>% 
  addProviderTiles(provider = providers$Esri.WorldImagery, 
    group = 'Imagery') %>% 
  setView(lng = -117.24, lat = 33.875, zoom = 11) %>% 
  addLayersControl(baseGroups = c('Basemap', 'Imagery'),
                   overlayGroups = c('Jurisdictions', 
                                     'Existing Warehouses',
                                     'Planned Warehouses',
                                     '800 foot buffer',
                                     'Residential'),
                   options = layersControlOptions(collapsed = FALSE)
                   ) %>% 
  hideGroup(c('800 foot buffer', 'Jurisdictions')) %>% 
  addMapPane('Jurisdictions', zIndex = 390) %>% 
  addMapPane('800 foot buffer', zIndex = 395) %>% 
  addMapPane('Existing Warehouses', zIndex = 410) %>% 
  addMapPane('Planned Warehouses', zIndex = 420) %>% 
  addPolygons(data = jurisdictions,
              color = ~palJuris(name),
              stroke = FALSE,
              fillOpacity = 0.2,
              label = ~htmlEscape(name),
              group = 'Jurisdictions',
              options = pathOptions(pane = 'Jurisdictions')) %>%
  addPolygons(data = buff_proj_800,
              color = 'grey',
              stroke = FALSE,
              fillOpacity = 0.7,
              group = '800 foot buffer',
              options = pathOptions(pane = '800 foot buffer'))  %>%
  addPolygons(data = warehouses_tidy,
              color = ~palWarehouseJuris(jurisdiction),
              stroke = TRUE,
              weight = 2,
              fillOpacity = 0.8,
              group = 'Existing Warehouses',
              label = ~htmlEscape(paste(jurisdiction, name, 
                type, floorSpace.sq.ft, 'sq.ft.')),
              options = pathOptions(pane = 'Existing Warehouses')) %>% 
  addPolygons(data = JPA,
              color = ~palWarehouseJuris(jurisdiction),
              stroke = TRUE,
              weight = 2,
              fillOpacity = 0.8,
              group = 'Planned Warehouses',
              label = ~htmlEscape(paste(name, 
                      type, floorSpace.sq.ft, 'sq.ft.')),
              options = pathOptions(pane = 'Planned Warehouses')
  ) %>% 
  addLegend(data = warehouses_tidy,
            pal = palWarehouseJuris,
            title = 'Jurisdiction',
            values = ~(jurisdiction))

rm(ls = warehouses_narrow, juris_warehouses, warehouses, WCUP, bloom_proj, WH_uCons)

setwd(wd)
save.image('.RData')
setwd(app_dir)
save.image('.RData')


                 