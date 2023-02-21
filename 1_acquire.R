# March JPA data from County of Riverside
# Created October 2022
# Modified February 2023
# Created by MCM

library(sf)
library(leaflet)
library(tidyverse)
library(htmltools)
library(leaflet.extras)
library(googlesheets4)

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
  filter(PLACE_NAME %in% c('Mead Valley', 'March ARB', 'Woodcrest',
                           'Good Hope', 'Lake Mathews', 'El Sobrante',
                           'Meadowbrook', 'Lakeview', 'Nuevo', 'Romoland', 
                           'Homeland', 'Highgrove'))  

WH.url <- 'https://raw.githubusercontent.com/RadicalResearchLLC/WarehouseMap/main/WarehouseCITY/geoJSON/finalParcels.geojson'
warehouses <- st_read(WH.url) %>% 
  st_transform("+proj=longlat +ellps=WGS84 +datum=WGS84") %>% 
  filter(county == 'Riverside')

warehouses_narrow <- warehouses %>% 
  select(apn, geometry, floorSpace.sq.ft, shape_area, year_built) %>% 
  rename(name = apn, geom = geometry, area = shape_area) %>% 
  mutate(type = 'existing') %>% 
  select(name, type, floorSpace.sq.ft, geom, area, year_built) %>% 
  mutate(jurisdiction = '')


shapeArea <- st_area(planned215_60)

planned <- planned215_60 %>% 
  mutate(type = 'Planned',
         floorSpace.sq.ft = round(as.numeric(0.5*10.764*shapeArea),0),
         footprint =        round(as.numeric(1*10.764*shapeArea), 0)) 

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
  st_intersects(jurisdictions, sparse = TRUE)

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
             name == '297100095' ~ 'March JPA',
             name == '297160005' ~ 'March JPA',
             name == '297200004' ~ 'March JPA',
             name == '294180055' ~ 'Perris',
             name == '294180031' ~ 'Perris',
             name == '294190080'~ 'Perris',
             name == '294210060'~ 'Perris',
             name == '291020023'~ 'Moreno Valley',
             name == '247170022' ~ 'Riverside',
             name == '255120036'~ 'Riverside',
             name == '255120014'~ 'Riverside',
             name == '257240002'~ 'Riverside',
             name == '257240001'~ 'Riverside',
             TRUE ~ jurisdiction
           )) %>% 
  arrange(jurisdiction) %>% 
  mutate(jurisdiction = ifelse(jurisdiction == 'MarchJPA', 'March JPA', jurisdiction))


missingMoVal <- sf::st_read('C:/Dev/MJPA2/jurisdictions/moValmissing.geojson') %>% 
  select(Name, geometry) %>% 
  rename(geom = geometry, name = Name)

March215_60_sheet <- read_sheet('https://docs.google.com/spreadsheets/d/1u7JJYoxl5lE-oXJHEt5kqICmLug6SDh5sYSrSNud7Cs/edit#gid=0',
                                sheet = 'March215_60') %>% 
  janitor::clean_names()

planned215_60_full <- planned215_60 %>% 
  bind_rows(missingMoVal) %>% 
  full_join(March215_60_sheet, by = c('name' = 'building_id')) %>% 
  rename(buildingID = 'name', project_size_sq_ft = size_sq_ft) %>% 
  select(-x14, -jurisdiction_enviro_docs)

shapeArea <- st_area(planned215_60_full)

planned_tidy <- planned215_60_full %>% 
  mutate(type = 'Planned',
         area =  round(as.numeric(10.764*shapeArea), -3)) %>% 
  mutate(floorSpace.sq.ft = 0.5*area) %>% 
  select(buildingID, type, floorSpace.sq.ft, area, jurisdiction, geom) %>% 
  rename(name = buildingID) %>% 
  mutate(year_built = '2025')


names(planned_tidy)
names(warehouses_tidy)

buff_proj_800 <- planned_tidy %>%
  rbind(warehouses_tidy) %>% 
  st_buffer(dist = 243.8)

palJuris <- colorFactor(palette = c('red', 'orange', 'brown', 'blue', 'green', 'yellow'), 
                        domain = jurisdictions$name)
palWarehouseJuris <- colorFactor(palette = c('red', 'orange', 'brown', 'blue', 'green', 'yellow'), 
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
                                     '800 foot buffer'),
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
  addPolygons(data = planned_tidy,
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

table4app <- planned215_60_full %>% 
  st_set_geometry(value = NULL) %>% 
  select(project, buildingID, project_size_sq_ft, jurisdiction, stage_pending_approved, 
         developer, year, docs) %>%
  arrange(desc(project_size_sq_ft)) %>% 
  rename('project size (sq.ft.)' = project_size_sq_ft, 'CEQA step/type' = stage_pending_approved) %>% 
  mutate(link = paste0('<a href=', docs, '>', 'Enviromental document', '</a>')) %>% 
  select(-docs) #%>% 

JurisdictionTotals <- warehouses_tidy %>% 
  st_set_geometry(value=NULL) %>% 
  group_by(jurisdiction) %>% 
  summarize(number = n(), footprint = round(sum(area), -5))

plannedTotals <- planned215_60_full %>% 
  st_set_geometry(value = NULL) %>% 
  mutate(footprint = round(as.numeric(1*10.764*shapeArea), 0)) %>% 
  select(jurisdiction, footprint) %>% 
  group_by(jurisdiction) %>% 
  summarize(planned.number = n(), planned.footprint = round(sum(footprint), -5))

summaryTable <- full_join(JurisdictionTotals, plannedTotals) %>% 
  mutate(total.Number = number + planned.number, 
         total.Footprint = footprint + planned.footprint)

summarySummary <- summaryTable %>% 
  summarize(number = sum(number), footprint = sum(footprint),
            planned.number = sum(planned.number), planned.footprint = sum(planned.footprint),
            total.Number = sum(total.Number), total.Footprint = sum(total.Footprint),
            .groups = 'drop') %>% 
  mutate(jurisdiction = 'March 215/60 Corridor Total')

summaryTable <- bind_rows(summaryTable, summarySummary) %>% 
  arrange(desc(total.Footprint)) %>% 
  rename('Current warehouse count' = number, 
         'Current warehouse footprint (sq.ft.)' = footprint,
         'Planned warehouse count' = planned.number,
         'Planned warehouse footprint (sq.ft.)' = planned.footprint,
         'Future warehouse count' = total.Number,
         'Future warehouse footprint (sq.ft.)' = total.Footprint)
  
rm(ls = warehouses_narrow, juris_warehouses, warehouses, WCUP, bloom_proj, WH_uCons)
rm(ls = receptors, summarySummary, plannedTotals)
setwd(wd)
save.image('.RData')
setwd(app_dir)
save.image('.RData')


                 