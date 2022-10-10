## create polygons for Bloomington Option 2 Site Plan
## Created September 2022
## created by Mike McCarthy

library(sf)
library(tidyverse)
library(leaflet)

#building 1 - bottom right - 710k sq.ft.

b1 <- rbind(c(-117.400892, 34.052100), 
            c(-117.400926, 34.048686),
            c(-117.404977, 34.048708),
            c(-117.404999, 34.052125),
            c(-117.400892, 34.052100))
(mp1 <- st_multipoint(b1))
#building 2 - middle - 1.25M sq.ft.
b2 <- rbind(c(-117.409354, 34.054098),
            c(-117.405195, 34.054019),
            c(-117.405206, 34.048729),
            c(-117.409350, 34.048681),
            c(-117.409354, 34.054098))
(mp2 <- st_multipoint(b2))
#building 3 - left - 750k sq.ft.
b3 <- rbind(c(-117.41379, 34.05580),
            c(-117.4096,   34.05580),
            c(-117.4096,   34.052210),
            c(-117.41379,  34.052210),
            c(-117.41379,  34.05580))

(mp3 <- st_multipoint(b3))
b4 <- rbind(c(-117.4139, 34.055779),
            c(-117.416015, 34.055779),
            c(-117.416015, 34.048702),
            c(-117.4139, 34.048702),
            c(-117.4139, 34.050485),
            c(-117.4117, 34.050485),
            c(-117.4117, 34.051375),
            c(-117.40955, 34.051375),
            c(-117.40955, 34.05217),
            c(-117.4139, 34.05217),
            c(-117.4139, 34.055779))
(mp4 <- st_multipoint(b4))
crs <- st_crs(4326)
pol1 <- st_sf(name = 'Bloomington 1', geom = st_sfc(st_polygon(list(mp1))), crs = crs)
pol2 <- st_sf(name = 'Bloomington 2', geom = st_sfc(st_polygon(list(mp2))), crs = crs)
pol3 <- st_sf(name = 'Bloomington 3', geom = st_sfc(st_polygon(list(mp3))), crs = crs)
pol4 <- st_sf(name = 'Bloomington 4', geom = st_sfc(st_polygon(list(mp4))), crs = crs)

bloom_proj <- rbind(pol1, pol2, pol3, pol4)

#ggplot() +
#  geom_sf(data = bloom_proj)

leaflet() %>% 
  addTiles() %>% 
  addPolygons(data = bloom_proj)

## Sycamore Canyon warehouses
# Warehouse 1 - 200k sq. ft. above mini-storage

sc1 <- rbind(c(-117.313744, 33.918513),
             c(-117.310906, 33.918513),
             c(-117.310688, 33.918834),
             c(-117.311060, 33.918834),
             c(-117.311060, 33.919558),
             c(-117.311847, 33.92031),
             c(-117.312206, 33.920296),
             c(-117.313835, 33.91987),
             c(-117.313744, 33.918513))
polSC1 <- st_polygon(list(sc1))
sc2 <- rbind(c(-117.310387, 33.917642),
             c(-117.30967, 33.920514),
             c(-117.309896, 33.921082),
             c(-117.309325, 33.9213),
             c(-117.3084, 33.9213),
             c(-117.3077, 33.9205),
             c(-117.30745, 33.918026),
             c(-117.310387, 33.917642))
polSC2 <- st_polygon(list(sc2))
VB <- rbind(c(-117.26336, 33.88068),
            c(-117.26499, 33.87962),
            c(-117.26698, 33.88069),
            c(-117.262535, 33.87028), 
            c(-117.255926, 33.87019),
            c(-117.26336, 33.88068))

Syc1 <- st_sf(name = 'Sycamore Canyon 1', geom = st_sfc(st_polygon(list(sc1))), crs = crs)
Syc2 <- st_sf(name = 'Sycamore Canyon 2', geom = st_sfc(st_polygon(list(sc2))), crs = crs)
VB1 <- st_sf(name = 'Veterans Industrial Park 1', geom = st_sfc(st_polygon(list(VB))), crs = crs)
Syc_WH <- rbind(Syc1, Syc2, VB1)

WCUP_MW1 <- rbind(c(-117.312157, 33.90384),
                  c(-117.305518, 33.90384),
                  c(-117.305518, 33.90698),
                  c(-117.312157, 33.90698),
                  c(-117.312157, 33.90384))

WCUP_MW2 <- rbind(c(-117.312157, 33.90736),
                  c(-117.305518, 33.90736),
                  c(-117.305518, 33.91013),
                  c(-117.311916, 33.91013),
                  c(-117.311916, 33.909389),
                  c(-117.312157, 33.907547),
                  c(-117.312157, 33.90736))

WCUP_MW3 <- rbind(c(-117.304892, 33.91013),
                  c(-117.301669, 33.91013),
                  c(-117.301669, 33.907),
                  c(-117.302827, 33.90736),
                  c(-117.304892, 33.90736),
                  c(-117.304892, 33.91013))

wcup_RW1 <- rbind(c(-117.311916, 33.9104),
                  c(-117.310255, 33.9104),
                  c(-117.310255, 33.91176),
                  c(-117.311916, 33.91176),
                  c(-117.311916, 33.9104))
wcup_RW2 <- rbind(c(-117.31024, 33.9104),
                  c(-117.307706, 33.9104),
                  c(-117.307706, 33.91176),
                  c(-117.31024, 33.91176),
                  c(-117.31024, 33.9104))
wcup_RW3 <- rbind(c(-117.3077, 33.9104),
               c(-117.304892, 33.9104),
               c(-117.304892, 33.911650),
               c(-117.306486, 33.91176),
               c(-117.3077, 33.91176),
               c(-117.3077, 33.9104)) 

wcup_RW4 <- rbind(c(-117.30488, 33.9104),
                  c(-117.302431, 33.9104),
                  c(-117.302431, 33.91165),
                  c(-117.30488, 33.91165),
                  c(-117.30488, 33.9104)) 

wcup_RW5 <- rbind(c(-117.304892, 33.90698),
                  c(-117.30283, 33.90698),
                  c(-117.30283, 33.90567),
                  c(-117.3038, 33.904598),
                  c(-117.304892, 33.904598),
                  c(-117.304892, 33.90698))

wcup_RW6 <- rbind(c(-117.307383, 33.903533),
                  c(-117.308933, 33.903533),
                  c(-117.308933, 33.901235),
                  c(-117.3076,  33.901235),
                  c(-117.307383, 33.9015),
                  c(-117.307383, 33.903533))

wcup_RW7 <- rbind(c(-117.308938, 33.903533),
                  c(-117.310932, 33.903533),
                  c(-117.310932, 33.901235),
                  c(-117.308938, 33.901235),
                  c(-117.308938, 33.903533))

WCUP1 <- st_sf(name = 'West Campus Mega 1', geom = st_sfc(st_polygon(list(WCUP_MW1))), crs = crs)
WCUP2 <- st_sf(name = 'West Campus Mega 2', geom = st_sfc(st_polygon(list(WCUP_MW2))), crs = crs)
WCUP3 <- st_sf(name = 'West Campus Mega 3', geom = st_sfc(st_polygon(list(WCUP_MW3))), crs = crs)
WCUP4 <- st_sf(name = 'West Campus reg 1', geom = st_sfc(st_polygon(list(wcup_RW1))), crs = crs)
WCUP5 <- st_sf(name = 'West Campus reg 2', geom = st_sfc(st_polygon(list(wcup_RW2))), crs = crs)
WCUP6 <- st_sf(name = 'West Campus reg 3', geom = st_sfc(st_polygon(list(wcup_RW3))), crs = crs)
WCUP7 <- st_sf(name = 'West Campus reg 4', geom = st_sfc(st_polygon(list(wcup_RW4))), crs = crs)
WCUP8 <- st_sf(name = 'West Campus reg 5', geom = st_sfc(st_polygon(list(wcup_RW5))), crs = crs)
WCUP9 <- st_sf(name = 'West Campus reg 6', geom = st_sfc(st_polygon(list(wcup_RW6))), crs = crs)
WCUP10 <- st_sf(name = 'West Campus reg 7', geom = st_sfc(st_polygon(list(wcup_RW7))), crs = crs)

WCUP <- rbind(WCUP1, WCUP2, WCUP3, WCUP4, WCUP5, WCUP6, WCUP7, WCUP8, WCUP9, WCUP10)


#projects <- data.frame(buildings, option)
#projects$geometry <- st_sfc(geo_test, sf_column_name = 'geometry', epsg = 4326)
#st_sfc()
#projects_sf <- st_as_sf(projects)


leaflet() %>% 
  addTiles() %>% 
  addPolygons(data = WCUP, color = 'red',
              stroke = FALSE, fillOpacity = 0.5) %>% 
  addPolygons(data = Syc_WH, color = 'red',
              stroke = FALSE, fillOpacity = 0.5)

  #st_transform("+proj=longlat +ellps=WGS84 +datum=WGS84") 

rm(ls = wcup_RW1, wcup_RW2, wcup_RW3, wcup_RW4,
   wcup_RW5, wcup_RW6, wcup_RW7, WCUP_MW1, WCUP_MW2, WCUP_MW3)
rm(ls = sc1, sc2, Syc1, Syc2, pol1, pol2, pol3, pol4, VB1)
rm(ls = b1, b2, b3, b4)
rm(ls = WCUP1, WCUP2, WCUP3, WCUP4,
   WCUP5, WCUP6, WCUP7, WCUP8, WCUP9, WCUP10)
