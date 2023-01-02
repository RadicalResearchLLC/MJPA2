## create polygons for Bloomington Option 2 Site Plan
## Created September 2022
## created by Mike McCarthy

library(sf)
library(tidyverse)
library(leaflet)

WH.url <- 'https://raw.githubusercontent.com/RadicalResearchLLC/WarehouseMap/main/WarehouseCITY/geoJSON/finalParcels.geojson'
warehouses <- st_read(WH.url) %>% 
  st_transform("+proj=longlat +ellps=WGS84 +datum=WGS84") %>% 
  filter(county == 'Riverside')

##Bloomington Project
#building 1 - bottom right - 710k sq.ft.
b1 <- rbind(c(-117.400892, 34.052100), 
            c(-117.400926, 34.048686),
            c(-117.404977, 34.048708),
            c(-117.404999, 34.052125),
            c(-117.400892, 34.052100))

#building 2 - middle - 1.25M sq.ft.
b2 <- rbind(c(-117.409354, 34.054098),
            c(-117.405195, 34.054019),
            c(-117.405206, 34.048729),
            c(-117.409350, 34.048681),
            c(-117.409354, 34.054098))

#building 3 - left - 750k sq.ft.
b3 <- rbind(c(-117.41379, 34.05580),
            c(-117.4096,   34.05580),
            c(-117.4096,   34.052210),
            c(-117.41379,  34.052210),
            c(-117.41379,  34.05580))

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

crs <- st_crs(4326)
pol1 <- st_sf(name = 'Bloomington 1', geom = st_sfc(st_polygon(list(b1))), crs = crs)
pol2 <- st_sf(name = 'Bloomington 2', geom = st_sfc(st_polygon(list(b2))), crs = crs)
pol3 <- st_sf(name = 'Bloomington 3', geom = st_sfc(st_polygon(list(b3))), crs = crs)
pol4 <- st_sf(name = 'Bloomington 4', geom = st_sfc(st_polygon(list(b4))), crs = crs)

bloom_proj <- rbind(pol1, pol2, pol3, pol4)

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

sc2 <- rbind(c(-117.310387, 33.917642),
             c(-117.30967, 33.920514),
             c(-117.309896, 33.921082),
             c(-117.309325, 33.9213),
             c(-117.3084, 33.9213),
             c(-117.3077, 33.9205),
             c(-117.30745, 33.918026),
             c(-117.310387, 33.917642))
# VIP215 
VB <- rbind(c(-117.26336, 33.88068),
            c(-117.26499, 33.87962),
            c(-117.26698, 33.88069),
            c(-117.262535, 33.87028), 
            c(-117.255926, 33.87019),
            c(-117.26336, 33.88068))
#JPA South Campus buildings
SoCam1 <- rbind(c(-117.31376, 33.88484),
                c(-117.31142, 33.88484),
                c(-117.31142, 33.88228),
                c(-117.31376, 33.88228),
                c(-117.31376, 33.88484))
SoCam2 <- rbind(c(-117.31373, 33.8850),
                c(-117.31142, 33.8850),
                c(-117.31153, 33.8875),
                c(-117.31209, 33.88754),
                c(-117.31373, 33.88754),
                c(-117.31373, 33.8850))
SoCam3 <- rbind(c(-117.29843, 33.88159),
                c(-117.29843, 33.88326),
                c(-117.29753, 33.88326),
                c(-117.29609, 33.88397),
                c(-117.29506, 33.88395),
                c(-117.29403, 33.88366),
                c(-117.29403, 33.88159),
                c(-117.29843, 33.88159))
SoCam4 <- rbind(c(-117.29403, 33.88159),
               c(-117.29403, 33.88366),
               c(-117.29291, 33.88388),
               c(-117.29206, 33.88382),
               c(-117.29173, 33.88388),
               c(-117.29085, 33.88404),
               c(-117.28980, 33.88398),
               c(-117.28978, 33.88207),
               c(-117.29113, 33.88218),
               c(-117.29403, 33.88159))
SoCam5 <- rbind(c(-117.28978, 33.88207),
                c(-117.28980, 33.88398),
                c(-117.28942, 33.88426),
                c(-117.28898, 33.88430),
                c(-117.28818, 33.88474),
                c(-117.28784, 33.884785),
                c(-117.28770, 33.88419),
                c(-117.28770, 33.88184),
                c(-117.28906, 33.88184),
                c(-117.28978, 33.88207))
SoCam6 <- rbind(c(-117.307015, 33.88603),
                c(-117.30699, 33.88519),
                c(-117.30702, 33.88431),
                c(-117.30473, 33.88415),
                c(-117.30537, 33.88547),
                c(-117.30545, 33.88603),
                c(-117.307015, 33.88603))
SoCam7 <- rbind(c(-117.29557, 33.88113),
                c(-117.29413, 33.88113),
                c(-117.29413, 33.88052),
                c(-117.29552, 33.87881),
                c(-117.29557, 33.88113))
SoCam8 <- rbind(c(-117.2938, 33.88127),
                c(-117.29254, 33.88160),
                c(-117.29335, 33.88018),
                c(-117.2938, 33.88033),
                c(-117.29376, 33.88075),
                c(-117.2938, 33.88127))

Syc1 <- st_sf(name = 'Sycamore Hills Distribution Center Bldg 1', geom = st_sfc(st_polygon(list(sc1))), crs = crs)
Syc2 <- st_sf(name = 'Sycamore Hills Distribution Center Bldg 2', geom = st_sfc(st_polygon(list(sc2))), crs = crs)
VB1 <- st_sf(name = "Veteran's Industrial Park", geom = st_sfc(st_polygon(list(VB))), crs = crs)
SoCamp1 <- st_sf(name = 'South Campus Reg 1', geom = st_sfc(st_polygon(list(SoCam1))), crs = crs)
SoCamp2 <- st_sf(name = 'South Campus Bldg. F and G', geom = st_sfc(st_polygon(list(SoCam2))), crs = crs)
SoCamp3 <- st_sf(name = 'South Campus Reg3', geom = st_sfc(st_polygon(list(SoCam3))), crs = crs)
SoCamp4 <- st_sf(name = 'South Campus Reg4', geom = st_sfc(st_polygon(list(SoCam4))), crs = crs)
SoCamp5 <- st_sf(name = 'South Campus Bldg. H and I', geom = st_sfc(st_polygon(list(SoCam5))), crs = crs)
SoCamp6 <- st_sf(name = 'South Campus Bldg 1', geom = st_sfc(st_polygon(list(SoCam6))), crs = crs)
SoCamp7 <- st_sf(name = 'South Campus Bldg 2', geom = st_sfc(st_polygon(list(SoCam7))), crs = crs)
SoCamp8 <- st_sf(name = 'South Campus Bldg 3', geom = st_sfc(st_polygon(list(SoCam8))), crs = crs)


WH_uCons <- rbind(Syc1, Syc2, VB1, SoCamp1, SoCamp2, SoCamp3, SoCamp4, SoCamp5, SoCamp6, SoCamp7, SoCamp8)

#West Campus Buildings

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

WLC_SP <- rbind(c(-117.139088, 33.939),
                c(-117.139116, 33.9353),
                c(-117.1564, 33.93525),
                c(-117.15628, 33.92135),
                c(-117.15192, 33.92135),
                c(-117.15199, 33.91782),
                c(-117.14506, 33.91761),
                c(-117.14506, 33.91382),
                c(-117.14592, 33.91382),
                c(-117.14592, 33.9102),
                c(-117.13992, 33.9102),
                c(-117.13992, 33.91469),
                c(-117.13746, 33.91215),
                c(-117.13299, 33.9098),
                c(-117.12567, 33.9098),
                c(-117.12567, 33.9149),
                c(-117.11688, 33.9149),
                c(-117.11688, 33.9098),
                c(-117.0950,  33.9098),
                c(-117.09921, 33.9163),
                c(-117.10384, 33.9194),
                c(-117.12107, 33.9283),
                c(-117.12455, 33.9326),
                c(-117.12631, 33.9366),
                c(-117.12955, 33.9379),
                c(-117.13344, 33.9381),
                c(-117.139088, 33.939))

WLC <- st_sf(name = 'World Logistics Center', geom = st_sfc(st_polygon(list(WLC_SP))), crs = crs)

Stoneridge1 <- rbind(c(-117.17, 33.8017),
                     c(-117.17, 33.8263),
                     c(-117.16404, 33.83012),
                     c(-117.15904, 33.83012),
                     c(-117.15904, 33.82956),
                     c(-117.15634, 33.82956),
                     c(-117.15634, 33.82036),
                     c(-117.1587, 33.82036),
                     c(-117.16508, 33.81203),
                     c(-117.16508, 33.8017),
                     c(-117.17, 33.8017))
StoneridgeCC <- st_sf(name = 'Stoneridge Commerce Center', geom = st_sfc(st_polygon(list(Stoneridge1))), crs = crs)

SPerris <- rbind(c(-117.21718, 33.7791),
                 c(-117.21715, 33.7723),
                 c(-117.20877, 33.7723),
                 c(-117.20877, 33.7784),
                 c(-117.2103, 33.77915),
                 c(-117.21718, 33.7791))

SPerris1 <- st_sf(name = 'South Perris Industrial Project', geom = st_sfc(st_polygon(list(SPerris))), crs = crs)

MValTC1 <- rbind(c(-117.15697, 33.93539),
                 c(-117.15697, 33.93203),
                 c(-117.16534, 33.93203),
                 c(-117.16534, 33.93539),
                 c(-117.15697, 33.93539))
MValTC <- st_sf(name = 'Moreno Valley Trade Center', geom = st_sfc(st_polygon(list(MValTC1))), crs = crs)

RiderPat1 <- rbind(c(-117.25714, 33.830075),
                   c(-117.257168, 33.82667),
                   c(-117.25285, 33.82667),
                   c(-117.25289, 33.83015),
                   c(-117.25714, 33.830075))
RiderPat <- st_sf(name = 'Rider and Patterson Business Center', geom = st_sfc(st_polygon(list(RiderPat1))), crs = crs)
KnoxBP1D <- rbind(c(-117.27, 33.85878),
                  c(-117.27, 33.85566),
                  c(-117.2658, 33.85566),
                  c(-117.2658, 33.85878),
                  c(-117.27, 33.85878))
KnoxBP1E <- rbind(c(-117.27303, 33.85878),
                  c(-117.27026, 33.85878),
                  c(-117.27026, 33.85566),
                  c(-117.27303, 33.85566),
                  c(-117.27303, 33.85878))
KnoxBPd <- st_sf(name = 'Knox Business Park Bldg. D', geom = st_sfc(st_polygon(list(KnoxBP1D))), crs = crs)
KnoxBPe <- st_sf(name = 'Knox Business Park Bldg. E', geom = st_sfc(st_polygon(list(KnoxBP1E))), crs = crs)
RamonaGate1 <- rbind(c(-117.24808, 33.84428),
                     c(-117.24412, 33.84428),
                     c(-117.24407, 33.83964),
                     c(-117.24808, 33.83964),
                     c(-117.24808, 33.84428))
RamonaGate <- st_sf(name = 'Ramona Gateway', geom = st_sfc(st_polygon(list(RamonaGate1))), crs = crs)

HeacockCC1 <- rbind(c(-117.243385, 33.8954),
                    c(-117.23941, 33.8954),
                    c(-117.23941, 33.89116),
                    c(-117.24007, 33.89116),
                    c(-117.24007, 33.89112),
                    c(-117.24242, 33.89096),
                    c(-117.243385, 33.89096),
                    c(-117.243385, 33.8954))
HeacockCC <- st_sf(name = 'Heacock Commerce Center', geom = st_sfc(st_polygon(list(HeacockCC1))), crs = crs)

OLC1 <- rbind(c(-117.22584, 33.84817),
              c(-117.2198, 33.84817),
              c(-117.2198, 33.84486),
              c(-117.221686, 33.84486),
              c(-117.221686, 33.84572),
              c(-117.22584, 33.84572),
              c(-117.22584, 33.84817))
OLC3 <- st_sf(name = 'OLC3 Warehouse', geom = st_sfc(st_polygon(list(OLC1))), crs = crs)
DukeWH1 <- rbind(c(-117.25245, 33.85813),
                 c(-117.24921, 33.85812),
                 c(-117.24835, 33.85703),
                 c(-117.24835, 33.8556),
                 c(-117.25245, 33.8556),
                 c(-117.25245, 33.85813)) 
DukeWH <- st_sf(name = 'Duke Warehouse', geom = st_sfc(st_polygon(list(DukeWH1))), crs = crs)

OleanderBP1 <- rbind(c(-117.27017, 33.86598),
                     c(-117.27017, 33.85904),
                     c(-117.2750, 33.85904),
                     c(-117.2750, 33.86598),
                     c(-117.27017, 33.86598))
OleanderBP <- st_sf(name = 'Oleander Business Park', geom = st_sfc(st_polygon(list(OleanderBP1))), crs = crs)
MapesCC1 <- rbind(c(-117.22355, 33.76115),
                  c(-117.21938, 33.76115),
                  c(-117.21938, 33.75779),
                  c(-117.22355, 33.75779),
                  c(-117.22355, 33.76115))
MapesCC <- st_sf(name = 'Mapes Commerce Center', geom = st_sfc(st_polygon(list(MapesCC1))), crs = crs)

March1LP1 <- rbind(c(-117.26212, 33.87015),
                   c(-117.26212, 33.869),
                   c(-117.26134, 33.86734),
                   c(-117.25925, 33.86734),
                   c(-117.25928, 33.86909),
                   c(-117.25708, 33.86908),
                   c(-117.25706, 33.869785),
                   c(-117.25484, 33.869785),
                   c(-117.25519, 33.87007),
                   c(-117.26212, 33.87015))
March1LP <- st_sf(name = 'First March Logistics Project', geom = st_sfc(st_polygon(list(March1LP1))), crs = crs)
HarvillBC1 <- rbind(c(-117.2482, 33.81935),
                    c(-117.24623, 33.81935),
                    c(-117.24623, 33.81599),
                    c(-117.2482, 33.81599),
                    c(-117.2482, 33.81935))
HarvillBC <-  st_sf(name = 'Harvill Business Center', geom = st_sfc(st_polygon(list(HarvillBC1))), crs = crs)
SeatonCajalco1 <- rbind(c(-117.2615, 33.83725),
                        c(-117.25942, 33.8373),
                        c(-117.25947, 33.83412),
                        c(-117.26153, 33.83412),
                        c(-117.2615, 33.83725))
SeatonCajalcoIP <-  st_sf(name = 'Seaton and Cajalco Industrial Project', geom = st_sfc(st_polygon(list(SeatonCajalco1))), crs = crs)
PerrisCC1 <- rbind(c(-117.23022, 33.84434),
                   c(-117.22634, 33.84434),
                   c(-117.22634, 33.84283),
                   c(-117.23041, 33.84286),
                   c(-117.23022, 33.84357),
                   c(-117.23022, 33.84434))
PerrisCC <- st_sf(name = 'Perris Valley Commerce Center', geom = st_sfc(st_polygon(list(PerrisCC1))), crs = crs)
HarvillRider1 <- rbind(c(-117.24768, 33.8336),
                       c(-117.24641, 33.8306),
                       c(-117.24869, 33.8306),
                       c(-117.24992, 33.8336),
                       c(-117.24768, 33.8336))
HarvillRider <- st_sf(name = 'Harvill and Rider' , geom = st_sfc(st_polygon(list(HarvillRider1))), crs = crs)
RedlandsAveWest <- rbind(c(-117.22135, 33.8281),
                         c(-117.217525, 33.8281),
                         c(-117.217525, 33.82586),
                         c(-117.21963, 33.82586),
                         c(-117.22135, 33.8281))
RedlandsAveWestIP <- st_sf(name = 'Redlands Ave West Industrial Project' , geom = st_sfc(st_polygon(list(RedlandsAveWest))), crs = crs)
PerrisMorganIPP1 <- rbind(c(-117.23039, 33.83726),
                            c(-117.22637, 33.83718),
                            c(-117.2263, 33.83391),
                            c(-117.22715, 33.83396), 
                            c(-117.22715, 33.83639),
                            c(-117.23039, 33.83639),
                            c(-117.23039, 33.83726))
PerrisMorganIPP <- st_sf(name = 'Perris Morgan Industrial Park Project' , 
                         geom = st_sfc(st_polygon(list(PerrisMorganIPP1))), crs = crs)
PlacentiaLP1 <- rbind(c(-117.24923, 33.82491),
                      c(-117.2467, 33.82491),
                      c(-117.24614, 33.82367),
                      c(-117.24611, 33.82328),
                      c(-117.24886, 33.82328),
                      c(-117.24923, 33.82491)) 
PlacentiaLP <- st_sf(name = 'Placentia Logistics Project', 
                     geom = st_sfc(st_polygon(list(PlacentiaLP1))), crs = crs)
RedlandsAveEastIP1 <- rbind(c(-117.21728, 33.82712),
                            c(-117.21521, 33.82713),
                            c(-117.21521, 33.82487),
                            c(-117.21728, 33.82487),
                            c(-117.21728, 33.82712))
RedlandsAveEastIP <- st_sf(name = 'Redlands Ave East Industrial Project', 
                     geom = st_sfc(st_polygon(list(RedlandsAveEastIP1))), crs = crs)
MuranakaWP1 <- rbind(c(-117.26999, 33.86246),
                     c(-117.26672, 33.86246),
                     c(-117.26672, 33.86068),
                     c(-117.26999, 33.86068),
                     c(-117.26999, 33.86246))
MuranakaWP <- st_sf(name = 'Muranaka Warehouse Project', 
                           geom = st_sfc(st_polygon(list(MuranakaWP1))), crs = crs)
RamonaIndianWP1 <- rbind(c(-117.23023, 33.84641),
                         c(-117.22628, 33.84641),
                         c(-117.22628, 33.8457),
                         c(-117.2272, 33.8457),
                         c(-117.2272, 33.84473),
                         c(-117.23009, 33.84481),
                         c(-117.23022, 33.84491),
                         c(-117.23023, 33.84641))
RamonaIndianWP <- st_sf(name = 'Ramona Indian Warehouse Project', 
                    geom = st_sfc(st_polygon(list(RamonaIndianWP1))), crs = crs)
MeridianD1Gateway1 <- rbind(c(-117.24406, 33.88078),
                            c(-117.24977, 33.87817),
                            c(-117.2506, 33.87663),
                            c(-117.24949, 33.87494),
                            c(-117.24485, 33.87692),
                            c(-117.24398, 33.877),
                            c(-117.24406, 33.88078))
MeridianD1Gateway <- st_sf(name = 'Meridian D1 Gateway Aviation Project', 
                        geom = st_sfc(st_polygon(list(MeridianD1Gateway1))), crs = crs)
ramonaBrennanWP1 <- rbind(c(-117.24161, 33.84437),
                          c(-117.23945, 33.84437),
                          c(-117.23945, 33.84302),
                          c(-117.24161, 33.84302),
                          c(-117.24161, 33.84437))
ramonaBrennanWP <- st_sf(name = 'Ramona and Brennan Warehouse Project', 
                            geom = st_sfc(st_polygon(list(ramonaBrennanWP1))), crs = crs)
MoValBC1 <- rbind(c(-117.27864, 33.91879),
                  c(-117.27686, 33.91883),
                  c(-117.27686, 33.91707),
                  c(-117.27864, 33.91713),
                  c(-117.27864, 33.91879))
MoValBC <- st_sf(name = 'Moreno Valley Business Center', 
                         geom = st_sfc(st_polygon(list(MoValBC1))), crs = crs)
FirstHarleyKI1 <- rbind(c(-117.21952, 33.85874),
                       c(-117.217, 33.85872),
                       c(-117.217, 33.8574),
                       c(-117.21952,33.8574),
                       c(-117.21952, 33.85874))
FirstHarleyKI <- st_sf(name = 'First Harley Knox Industrial', 
                 geom = st_sfc(st_polygon(list(FirstHarleyKI1))), crs = crs)
Operon1 <- rbind(c(-117.2345, 33.8571),
                 c(-117.23003, 33.8571),
                 c(-117.23003, 33.85645),
                 c(-117.2345, 33.85645),
                 c(-117.2345, 33.8571))
OperonHKI <- st_sf(name = 'Operon HKI', 
                       geom = st_sfc(st_polygon(list(Operon1))), crs = crs)
HarleyKnoxIP1 <- rbind(c(-117.22389, 33.85875),
                       c(-117.22175, 33.85875),
                       c(-117.22175, 33.85743),
                       c(-117.22389, 33.85743),
                       c(-117.22389, 33.85875))
HarleyKnoxIP <- st_sf(name = 'Harley Knox Industrial Project', 
                   geom = st_sfc(st_polygon(list(HarleyKnoxIP1))), crs = crs)
EdgemontCC1 <- rbind(c(-117.28047, 33.92048),
                     c(-117.279, 33.92046),
                     c(-117.279, 33.91878),
                     c(-117.28047, 33.91878),
                     c(-117.28047, 33.92048))
EdgemontCC2 <- rbind(c(-117.28147, 33.92049),
                     c(-117.281, 33.92049),
                     c(-117.281, 33.91985),
                     c(-117.28147, 33.91985),
                     c(-117.28147, 33.92049))
EdgemontCC <- st_sf(name = 'Edgemont Commerce Center 1', 
              geom = st_sfc(st_polygon(list(EdgemontCC1))), crs = crs)
EdgemontCC_2 <- st_sf(name = 'Edgemont Commerce Center 2', 
                    geom = st_sfc(st_polygon(list(EdgemontCC2))), crs = crs)
Chartwell1 <- rbind(c(-117.21887, 33.83002),
                    c(-117.21748, 33.83002),
                    c(-117.2175, 33.82817),
                    c(-117.21887, 33.82817),
                    c(-117.21887, 33.83002))
Chartwell <- st_sf(name = 'Chartwell Warehouse', 
                      geom = st_sfc(st_polygon(list(Chartwell1))), crs = crs)
Old215BP1 <- rbind(c(-117.28835, 33.92423),
                   c(-117.28665, 33.92422),
                   c(-117.28836, 33.92814),
                   c(-117.28835, 33.92423))
Old215BP <- st_sf(name = 'Old 215 Business Park Project', 
                 geom = st_sfc(st_polygon(list(Old215BP1))), crs = crs)
MarlboroughNGLI1 <- rbind(c(-117.3333, 33.99743),
                         c(-117.33112, 33.99741),
                         c(-117.33112, 33.99636),
                         c(-117.33174, 33.99636),
                         c(-117.333, 33.99679),
                         c(-117.3333, 33.99679),
                         c(-117.3333, 33.99743))
MarlboroughNGLI <- st_sf(name = 'Marlborough Northgate Warehouse', 
                  geom = st_sfc(st_polygon(list(MarlboroughNGLI1))), crs = crs)
Phelan1 <- rbind(c(-117.24373, 33.8554),
                 c(-117.24261, 33.8554),
                 c(-117.24261, 33.85374),
                 c(-117.24373, 33.85374),
                 c(-117.24373, 33.8554))
Phelan <- st_sf(name = 'Phelan Warehouse', 
                         geom = st_sfc(st_polygon(list(Phelan1))), crs = crs)
SeatonPerry1 <- rbind(c(-117.26581, 33.85005),
                      c(-117.26162, 33.85005),
                      c(-117.26162, 33.84926),
                      c(-117.26581, 33.84926),
                      c(-117.26581, 33.85005))
SeatonPerry <- st_sf(name = 'Seaton and Perry Industrial Project', 
                geom = st_sfc(st_polygon(list(SeatonPerry1))), crs = crs)
MajesticFreeway17 <- rbind(c(-117.26124, 33.85719),
                              c(-117.25804, 33.85721),
                              c(-117.25734, 33.85563),
                              c(-117.26124, 33.85565),
                              c(-117.26124, 33.85719))
MajesticFreewayBP17 <- st_sf(name = 'Majestic Freeway Business Park 17', 
                             geom = st_sfc(st_polygon(list(MajesticFreeway17))), crs = crs)
MajesticFreeway18 <- rbind(c(-117.2637, 33.85877),
                           c(-117.26296, 33.85877),
                           c(-117.26296, 33.8579),
                           c(-117.2616, 33.8579),
                           c(-117.2616, 33.85565),
                           c(-117.2637, 33.85565),
                           c(-117.2637, 33.85877))
MajesticFreewayBP18 <- st_sf(name = 'Majestic Freeway Business Park 18', 
                             geom = st_sfc(st_polygon(list(MajesticFreeway18))), crs = crs)
MajesticFreeway14 <- rbind(c(-117.26132, 33.85055),
                           c(-117.25957, 33.85055),
                           c(-117.25879, 33.85076),
                           c(-117.25737, 33.84848),
                           c(-117.2613, 33.84848),
                           c(-117.26132, 33.85055))
MajesticFreewayBP14 <- st_sf(name = 'Majestic Freeway Business Park 14', 
                             geom = st_sfc(st_polygon(list(MajesticFreeway14))), crs = crs)
MajesticFreeway13 <- rbind(c(-117.25924, 33.8483),
                           c(-117.2573, 33.8483),
                           c(-117.2573, 33.84484),
                           c(-117.25924, 33.84484),
                           c(-117.25924, 33.8483))
MajesticFreewayBP13 <- st_sf(name = 'Majestic Freeway Business Park 13', 
                             geom = st_sfc(st_polygon(list(MajesticFreeway13))), crs = crs)
WilsonAveP1 <- rbind(c(-117.21294, 33.82861),
                     c(-117.21018, 33.82866),
                     c(-117.2096, 33.82736),
                     c(-117.21294, 33.82736),
                     c(-117.21294, 33.82861))
WilsonAveP <- st_sf(name = 'Wilson Avenue Project', 
                             geom = st_sfc(st_polygon(list(WilsonAveP1))), crs = crs)

planned215_60 <- rbind(SPerris1, WLC, StoneridgeCC, MValTC, RiderPat, KnoxBPd, KnoxBPe, RamonaGate,
                       HeacockCC, OLC3, DukeWH, OleanderBP, MapesCC, March1LP, SeatonPerry,
                       HarvillBC,SeatonCajalcoIP, PerrisCC, HarvillRider, RedlandsAveWestIP, PerrisMorganIPP,
                       PlacentiaLP, RedlandsAveEastIP, MuranakaWP, RamonaIndianWP, MeridianD1Gateway,
                       ramonaBrennanWP, MoValBC, FirstHarleyKI, OperonHKI, HarleyKnoxIP,
                       EdgemontCC, EdgemontCC_2, Chartwell, Old215BP, MarlboroughNGLI, Phelan,
                       MajesticFreewayBP17, MajesticFreewayBP18, MajesticFreewayBP14, MajesticFreewayBP13, WilsonAveP)
WCUP_narrow <- select(WCUP, name, geom)
WH_uCons_narrow <- select(WH_uCons, name, geom)

planned215_60 <- bind_rows(planned215_60, WCUP_narrow, WH_uCons_narrow)

leaflet() %>% 
  addTiles() %>% 
  addProviderTiles(providers$CartoDB.Positron, group = 'Basemap') %>% 
  addProviderTiles(provider = providers$Esri.WorldImagery, 
                   group = 'Imagery') %>% 
  addLayersControl(baseGroups = c('Basemap', 'Imagery'),
                   
                   options = layersControlOptions(collapsed = FALSE)) %>%
  setView(lng = -117.24, lat = 33.875, zoom = 11.5) %>% 
  addPolygons(data = planned215_60,
              color = 'purple',
              weight = 1) %>% 
  addPolygons(data = warehouses,
              color= 'red',
              fillOpacity = 0.2,
              weight = 1)

rm(ls = wcup_RW1, wcup_RW2, wcup_RW3, wcup_RW4,
   wcup_RW5, wcup_RW6, wcup_RW7, WCUP_MW1, WCUP_MW2, WCUP_MW3)
rm(ls = sc1, sc2, Syc1, Syc2, pol1, pol2, pol3, pol4, VB1)
rm(ls = b1, b2, b3, b4)
rm(ls = WCUP1, WCUP2, WCUP3, WCUP4,
   WCUP5, WCUP6, WCUP7, WCUP8, WCUP9, WCUP10)
rm(crs, polSC1, polSC2, VB)
rm(SoCam4, SoCam5, SoCam6,SoCam7, SoCam8)
rm(SoCam1, SoCam2, SoCam3, SoCamp1, SoCamp2, SoCamp3, SoCamp4, SoCamp5, SoCamp6, SoCamp7, SoCamp8)
rm(DukeWH, DukeWH1, HarvillBC, HarvillBC1, HeacockCC, HeacockCC1)
rm(KnoxBP1D, KnoxBP1E, KnoxBPd, KnoxBPe, MapesCC, MapesCC1, March1LP, March1LP1, MValTC, MValTC1)
rm(OleanderBP, OleanderBP1, PerrisCC, PerrisCC1, RiderPat, RiderPat1, SeatonCajalco1, SeatonCajalcoIP, SPerris,SPerris1)
rm(OLC1, OLC3, RamonaGate, RamonaGate1, StoneridgeCC, Stoneridge1, WLC, WLC_SP )
rm(Chartwell, Chartwell1, EdgemontCC, EdgemontCC_2, EdgemontCC1, EdgemontCC2, FirstHarleyKI, FirstHarleyKI1)
rm(HarleyKnoxIP, HarleyKnoxIP1, HarvillRider, HarvillRider1, MarlboroughNGLI, MarlboroughNGLI1, MeridianD1Gateway, MeridianD1Gateway1)
rm(MoValBC, MoValBC1, MuranakaWP1, MuranakaWP, Old215BP, Old215BP1, Operon1, OperonHKI, PerrisMorganIPP, PerrisMorganIPP1)
rm(RamonaGate, RamonaGate1, RamonaIndianWP, RamonaIndianWP1, PlacentiaLP, PlacentiaLP1, Phelan, Phelan1)
rm(ramonaBrennanWP, ramonaBrennanWP1, RedlandsAveEastIP, RedlandsAveEastIP1, RedlandsAveWest, RedlandsAveWestIP)
rm(MajesticFreeway13, MajesticFreeway14, MajesticFreeway17, MajesticFreeway18)
rm(MajesticFreewayBP13, MajesticFreewayBP14, MajesticFreewayBP17, MajesticFreewayBP18, SeatonPerry, SeatonPerry1)
rm(WCUP, WCUP_narrow, WH_uCons, WH_uCons_narrow, WilsonAveP, WilsonAveP1)

#sf::st_write(bloom_proj, 'bloomington.geojson')
