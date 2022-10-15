## Data analysis for school warehouse proximity
## First created September 2022
## Created by Mike McCarthy, PhD, Radical Research LLC


palDPM <- colorQuantile(palette = 'Greys', domain = SoCalEJ$DieselPM_P, n = 5)

leaflet() %>% 
  addTiles() %>% 
  setView(lat = 34, lng = -117.60, zoom = 9) %>%
  addPolygons(data = SoCalEJ, 
             stroke = FALSE,
             fillColor = ~palDPM(DieselPM_P),
             fillOpacity = 0.5) %>%
  addPolygons(data = inland_parcels, 
              color = '#A94915',
              weight = 1,
              fillOpacity = 0.4,
              group = 'Warehouses') %>% 
 # addPolygons(data = schools,
  #            color = 'purple',
  #            fillOpacity = 0.8,
  #            group = 'Schools') %>% 
  addLegend(data = SoCalEJ, 
           pal = palDPM, 
           title = 'Diesel PM (%) - CalEnviroScreen', 
           values = ~DieselPM_P) #%>% 

schools <- mutate(schools, school.row = row_number())  

#buff_schools_1000 <- schools %>% 
#  st_buffer(dist = 304) 
#buff_schools_2000 <- schools %>% 
#  st_buffer(dist = 608)  
#buff_schools_3000 <- schools %>% 
#  st_buffer(dist = 912) 

buff_warehouses_1000 <- inland_parcels %>% 
  st_buffer(dist = 304) 
#buff_warehouses_2000 <- inland_parcels %>% 
#  st_buffer(dist = 608)  
buff_warehouses_3000 <- inland_parcels %>% 
  st_buffer(dist = 912) 

setwd('C:/Dev/SchoolWarehouse/')
getwd()
source('polygonTest.R')

#st_crs(projects_sf) <- 4326
buff_proj_1000 <- WCUP %>% 
  st_buffer(dist = 304)
buff_proj_3000 <- WCUP %>% 
  st_buffer(dist = 912)

buff_syc_1000 <- Syc_WH %>% 
  st_buffer(dist = 304)
buff_syc_3000 <- Syc_WH %>% 
  st_buffer(dist = 912)

#st_crs(projects_sf) <- 4326
buff_bloom_1000 <- bloom_proj %>% 
  st_buffer(dist = 304)
buff_bloom_3000 <- bloom_proj %>% 
  st_buffer(dist = 912)


#check bloomington
leaflet() %>% 
  addTiles() %>% 
  addProviderTiles(providers$Esri.WorldImagery, group = 'Imagery') %>%
  addLayersControl(baseGroups = c('Basemap', 'Imagery')) %>% 
  addPolygons(data = buff_bloom_1000, 
              color = '#A94915',
              weight = 0.1,
              fillOpacity = 0.1) %>% 
  addPolygons(data = buff_bloom_3000, 
              color = '#A94915',
              weight = 0.2,
              fillOpacity = 0.2) %>% 
  addPolygons(data = schools,
              color = 'purple',
              fillOpacity = 0.8,
              stroke = FALSE,
              group = 'Schools') %>% 
  addPolygons(data = bloom_proj, 
              color = '#A94915',
              weight = 0.8,
              fillOpacity = 0.8)# %>% 

#Bloomington graphics

leaflet() %>% 
  addTiles() %>% 
  setView(lng = -117.409354, lat = 34.054098, zoom = 13) %>% 
  addPolygons(data = buff_warehouses_3000,
              color = '#A94915',
              weight = 0.1,
              fillOpacity = 0.1) %>% 
  addPolygons(data = buff_warehouses_1000,
              color = '#A94915',
              weight = 0.2,
             fillOpacity = 0.2) %>% 
   addPolygons(data = buff_bloom_3000, 
              color = '#5F1003',
              weight = 0.1,
              fillOpacity = 0.1) %>% 
  addPolygons(data = buff_bloom_1000, 
              color = '#5F1003',
              weight = 0.2,
              fillOpacity = 0.2) %>% 
    addPolygons(data = inland_parcels, 
              color = '#8D3312',
              weight = 0.8,
              fillOpacity = 0.8,
              group = 'Warehouses') %>% 
  addPolygons(data = bloom_proj, 
              color = '#5F1003',
              weight = 0.8,
              fillOpacity = 0.8) %>% 
  addPolygons(data = schools,
              color = 'purple',
              fillOpacity = 0.8,
              stroke = FALSE,
              group = 'Schools') 

# No Bloomington warehouse map  
  
  leaflet() %>% 
    addTiles() %>% 
    setView(lng = -117.409354, lat = 34.054098, zoom = 13) %>% 
    addPolygons(data = buff_warehouses_3000,
                color = '#A94915',
                weight = 0.1,
                fillOpacity = 0.1) %>% 
    addPolygons(data = buff_warehouses_1000,
              color = '#A94915',
              weight = 0.2,
              fillOpacity = 0.2) %>% 
    addPolygons(data = inland_parcels, 
                color = '#8D3312',
                weight = 0.8,
                fillOpacity = 0.8,
                group = 'Warehouses') %>% 
    addPolygons(data = schools,
                color = 'purple',
                fillOpacity = 0.8,
                stroke = FALSE,
                group = 'Schools') 
  

school1000 <- st_intersects(buff_schools_1000, inland_parcels) 
#school2000 <- st_intersects(buff_schools_2000, inland_parcels) 
#school3000 <- st_intersects(buff_schools_3000, inland_parcels)

school.calej <- st_intersects(schools, SoCalEJ)

inland_parcels <- inland_parcels %>% 
  mutate(rowParcels = row_number())

#school_unnest1 <- tibble(school1000) %>% 
#  mutate(school.row = row_number()) %>% 
#  unnest(cols = school1000) %>%
#  rename(rowParcels = school1000) %>% 
#  left_join(inland_parcels, by = ('rowParcels')) %>% 
#  select(school.row, rowParcels, shape_area, floorSpace.sq.ft) %>% 
#  group_by(school.row) %>% 
#  summarize(count1000 = n(), footprint1000 = sum(shape_area, na.rm=T), 
#            building.sq.ft1000 = sum(floorSpace.sq.ft, na.rm=T))

#school_unnest2 <- tibble(school2000) %>% 
#  mutate(school.row = row_number()) %>% 
#  unnest(cols = school2000) %>%
#  rename(rowParcels = school2000) %>% 
#  left_join(inland_parcels) %>% 
#  select(school.row, rowParcels, shape_area, floorSpace.sq.ft) %>% 
#  group_by(school.row) %>% 
#  summarize(count2000 = n(), footprint2000 = sum(shape_area, na.rm=T), 
#            building.sq.ft2000 = sum(floorSpace.sq.ft, na.rm=T))

#school_unnest3 <- tibble(school3000) %>% 
#  mutate(school.row = row_number()) %>% 
#  unnest(cols = school3000) %>%
#  rename(rowParcels = school3000) %>% 
#  left_join(inland_parcels) %>% 
#  select(school.row, rowParcels, shape_area, floorSpace.sq.ft) %>% 
#  group_by(school.row) %>% 
#  summarize(count3000 = n(), footprint3000 = sum(shape_area, na.rm=T), 
#            building.sq.ft3000 = sum(floorSpace.sq.ft, na.rm=T))

SoCalEJ <- mutate(SoCalEJ, rowTract = row_number())

schoolEJ <- tibble(school.calej) %>% 
  mutate(school.row = row_number()) %>% 
  unnest(cols = school.calej) %>%
  rename(rowTract = school.calej) %>% 
  left_join(SoCalEJ) %>% 
  select(school.row, Tract, DieselPM_P, CIscoreP) %>% 
  group_by(school.row) %>% 
  summarize(countTract = n(), DieselPM_P = mean(DieselPM_P, na.rm=T), 
            CIscoreP = mean(CIscoreP, na.rm=T))

schoolsFull <- schools %>% 
  left_join(school_unnest1) %>% 
  left_join(school_unnest2) %>% 
  left_join(school_unnest3) %>% 
  left_join(schoolEJ) %>% 
  mutate(above80DPM = ifelse(DieselPM_P >= 80, 1, 0),
         warehouse1000 = ifelse(is.na(count1000), 0, 1),
         warehouse2000 = ifelse(is.na(count2000), 0, 1),
         warehouse3000 = ifelse(is.na(count3000), 0, 1)) %>% 
  mutate(countBad = (above80DPM + warehouse1000 + warehouse2000 + warehouse3000)) %>%
  data.frame() %>% 
  arrange(desc(countBad), desc(DieselPM_P))

write.csv(schoolsFull, 'Detailed_schools.csv')

summaryStats <- schoolsFull %>% 
  data.frame() %>% 
  summarize(schools1000ft =  sum(warehouse1000),
            schools2000ft = sum(warehouse2000),
            schools3000ft = sum(warehouse3000),
            schools80DPM = sum(above80DPM),
            schoolCount = n())

write.csv( summaryStats, 'summaryStats.csv')
getwd()


leaflet() %>% 
  addTiles() %>% 
  setView(lng = -117.409354, lat = 34.054098, zoom = 11) %>% 
  addPolygons(data = buff_warehouses_3000,
              color = '#A94915',
              weight = 0.1,
              fillOpacity = 0.1) %>% 
  addPolygons(data = buff_warehouses_1000,
              color = '#A94915',
              weight = 0.2,
              fillOpacity = 0.2) %>% 
  addPolygons(data = SoCalEJ, 
              stroke = FALSE,
              fillColor = ~palDPM(DieselPM_P),
              fillOpacity = 0.5) %>%
  addPolygons(data = inland_parcels, 
              color = '#A94915',
              weight = 1,
              fillOpacity = 0.4,
              group = 'Warehouses') %>% 
  # addPolygons(data = schools,
  #            color = 'purple',
  #            fillOpacity = 0.8,
  #            group = 'Schools') %>% 
  addLegend(data = SoCalEJ, 
            pal = palDPM, 
            title = 'Diesel PM (%) - CalEnviroScreen', 
            values = ~DieselPM_P) #%>% 


## March JPA close-up map

leaflet() %>% 
  addTiles() %>% 
  addProviderTiles(providers$Esri.WorldImagery, group = 'Imagery') %>%
  addLayersControl(baseGroups = c('Basemap', 'Imagery')) %>% 
  setView(lng = -117.305518, lat = 33.90736, zoom = 13) %>% 
  # addPolygons(data = buff_warehouses_3000,
  #             color = '#A94915',
  #              weight = 0.1,
  #              fillOpacity = 0.1) %>% 
  # addPolygons(data = buff_warehouses_1000,
  #              color = '#A94915',
  #              weight = 0.2,
  #             fillOpacity = 0.2) %>% 
  addPolygons(data = buff_warehouses_3000,
              color = '#CF7820',
              weight = 0.1,
              fillOpacity = 0.1) %>% 
  addPolygons(data = buff_proj_3000, 
              color = '#CF7820',
              weight = 0.1,
              fillOpacity = 0.1) %>% 
  addPolygons(data = buff_syc_3000, 
              color = '#CF7820',
              weight = 0.1,
              fillOpacity = 0.1) %>% 
  #addPolygons(data = buff_proj_1000, 
  #            color = '#CF7820',
  #            weight = 0.1,
  #            fillOpacity = 0.1) %>% 
  addPolygons(data = inland_parcels, 
              color = '#A94915',
              weight = 0.7,
              fillOpacity = 0.8,
              group = 'Warehouses') %>% 
  addPolygons(data = WCUP, 
              color = '#5F1003',
              weight = 0.7,
              fillOpacity = 0.8) %>% 
  addPolygons(data = Syc_WH, 
              color = '#5F1003',
              weight = 0.7,
              fillOpacity = 0.8) %>% 
  addPolygons(data = schools,
              color = 'purple',
              fillOpacity = 0.7,
              stroke = FALSE,
              group = 'Schools') 

leaflet() %>% 
  addTiles() %>% 
  addProviderTiles(providers$Esri.WorldImagery, group = 'Imagery') %>%
  addLayersControl(baseGroups = c('Basemap', 'Imagery')) %>% 
  setView(lng = -117.305518, lat = 33.90736, zoom = 13) %>% 
  addPolygons(data = buff_warehouses_3000,
              color = '#CF7820',
              weight = 0.1,
              fillOpacity = 0.1) %>% 
  addPolygons(data = buff_syc_3000, 
              color = '#CF7820',
              weight = 0.1,
              fillOpacity = 0.1) %>% 
  addPolygons(data = inland_parcels, 
              color = '#A94915',
              weight = 0.7,
              fillOpacity = 0.8,
              group = 'Warehouses') %>% 
    addPolygons(data = Syc_WH, 
              color = '#5F1003',
              weight = 0.7,
              fillOpacity = 0.8) %>% 
  addPolygons(data = schools,
              color = 'purple',
              fillOpacity = 0.7,
              stroke = FALSE,
              group = 'Schools') 


leaflet() %>% 
  #addTiles() %>% 
  addProviderTiles(providers$Esri.WorldImagery, group = 'Imagery') %>%
  addLayersControl(baseGroups = c('Basemap', 'Imagery')) %>% 
  setView(lng = -117.305518, lat = 33.90736, zoom = 11) %>% 
  addPolygons(data = SoCalEJ, 
              stroke = FALSE,
              fillColor = ~palDPM(DieselPM_P),
              fillOpacity = 0.5) %>%
  addPolygons(data = inland_parcels, 
              color = 'red',
              weight = 0.7,
              fillOpacity = 0.4,
              group = 'Warehouses') %>% 
  addPolygons(data = Syc_WH, 
              color = 'red',
              weight = 0.7,
              fillOpacity = 0.4) %>%
  addPolygons(data = WCUP, 
              color = 'red',
              weight = 0.7,
              fillOpacity = 0.4) %>%
  addPolygons(data = bloom_proj,
              color = 'red',
              weight = 0.7,
              fillOpacity = 0.4) %>%
  addLegend(data = SoCalEJ, 
            pal = palDPM, 
            title = 'Diesel PM (%) - CalEnviroScreen', 
            values = ~DieselPM_P)
 
 
