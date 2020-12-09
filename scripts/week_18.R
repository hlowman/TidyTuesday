# Tidy Tuesday Dec 8th
# Introduction to Spatial Mapping in R
# Annie Holt


# R has good tools for spatial mapping; great for repeating analysis, efficient data exploration, and keeping track of steps you take
# see section on additional packages/resources for material not covered during live training session

# load libraries
library(tidyverse)
library(sf) #simple feature
library(ggspatial) #additional customization functions
library(mapview) #interactive maps


#### READING OR CREATING SPATIAL DATA ####

# California Stream Condition Index example data
# includes lat/long fields which we will use make the data spatial
csci_smc <- read_csv("csci_smc_example.csv")

# importing a shapefile
# note coordinate system, called CRS "Coordinate Reference System"
# shapefiles come with CRS already associated with them
smc_sheds <- st_read("spatial_mapping/smc_sheds.shp")

# coordinate reference system as way in which spatial data are flattened from round/3D Earth so that you can draw them on a 2-D surface
# often have different flatting methods, and thus differently defined coordinate systems

# in a bit more detail...
# components of CRS: 
  # coordinate system, the x y grid and how you define where a point is located
  # horizontal and vertical units to define grid
  # datum: modeled version of shape of earth
  # projection information: mathematical equation for flattening 

# you typically have your *geographic coordinate system* (like latitude/longitude) *projected* to a localized coordinate system to minimize distortion (like California Albers)

# when using 'sf' package, can refer to CRS with EPSG code
# Common CRS:
# California Albers, crs = 3310 (good option for mapping in CA)
# WGS84, crs = 4326 (commonly used as a default)

# making lat/long file into spatial object 
# choose x/y fields and projected coordinate reference system
csci_point <- st_as_sf(csci_smc,
                       coords = c("longitude", "latitude"),
                       crs = 4326) #project as WGS84

# convert to different Coordinate Reference System CRS
# chose CA Albers/NAD83 since I know data is in CA and want to minimize local distortion 
csci_point_albers <- st_transform(csci_point, crs = 3310)

# confirm CRS changed
st_crs(csci_point_albers)


#### MAPPING ####

# can plot spatial data with ggplot, geom_sf()
# geom_sf() requires input data to be spatial
ggplot()+
  geom_sf(data = smc_sheds)+
  geom_sf(data = csci_point_albers, #add additional spatial layers, recommend that layers have same CRS
          aes(color = csci), alpha = 0.7)+ #can customize colors and transparency like in other ggplot graphing
  scale_color_viridis_c(option = "viridis")+
  labs(color = "CSCI Score")+
  ggtitle("Biological Condition in SMC Watersheds")+
  theme_bw()
  

#### SPATIAL OPERATIONS ####

# spatial join, where you are attributing data based on spatial relationship
# in this case, say we want to associate each point with the watershed it falls in
# can add features of a polygon layer to a point layer
# this sometimes takes a bit of run time, default is left join 

csci_sheds <- st_join(csci_point_albers, smc_sheds)

# can see that we now have fields from sheds shapefile in our point shapefile 

# potential workflow for looking at stream condition just in Los Angeles watershed area:

# create polygon shapefile just for LA watershed by filtering 
la <- smc_sheds %>% 
  filter(SMC_Name == "Los Angeles")

# ask where polygon and points intersect, keep only points from y that are in x
# this is like a clip/cropping process

la_csci <- st_intersection(la, csci_point_albers)

# st_disjoint is the opposite


#### ADDITIONAL MAPPING ####

ggplot()+
  geom_sf(data = la)+
  geom_sf(data = la_test,
          aes(color = csci), alpha = 0.7)+
  scale_color_viridis_c(option = "viridis")+
  labs(color = "CSCI Score")+
  ggtitle("Biological Condition in Los Angeles")+
  theme_grey() 

# leveraging a few annotation functions from 'ggspatial' package
ggplot()+
  geom_sf(data = la)+
  geom_sf(data = la_csci,
          aes(color = csci), alpha = 0.7)+
  scale_color_viridis_c(option = "viridis")+
  annotation_scale(location = "bl")+ #add scale
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering)+ #add north arrow and alter location
  labs(color = "CSCI Score")+
  # coord_sf(xlim = c(), ylim = c(), expand = FALSE)+ #can alter your lat/long limits 
  xlab("Longitude (NAD83)")+ #good to note your CRS 
  ylab("Latitude")+
  ggtitle("Biological Condition in Los Angeles")+
  theme_bw()

# save
ggsave("map.png", width = 8, height = 8, units = "in")


#### EXPORTING SHAPEFILES ####

# example, exporting LA watershed boundary shapefile
# will export .shp file and associated files with spatial information (.dbf, .prj, .shx)

st_write(obj = la, "la_smc.shp")


#### INTERACTIVE MAPS ####

# can quickly make dynamic map with sf object using 'mapview' package
# great for data exploration 

# interactive map of csci scores in LA area
mapview::mapview(la_csci,
                 zcol='csci')+ # color based on variable value
  mapview(la) #can add another shapefile with '+'
# change basemap on the left of interactive window


# more editing and customization examples
mapview(la_csci,
        zcol='csci',
        at = c(0 , 0.8, 1) #define breakpoints
        )+
  mapview(la, 
          color = "white", #border colors
          lwd = 5, #border size
          col.regions = "grey", #region colors
          legend = FALSE)


#### ADDITIONAL RESOURCES ####

# 'USA boundaries' package has boundary shapefiles available for download

# install.packages('USABoundaries')

#to install associated data after installing package
# install.packages('devtools')
# library(devtools)
# devtools::install_github("ropensci/USAboundariesData")

library(USAboundaries)

# get state data, add projection
ca <- USAboundaries::us_states(resolution = "high", states = "california") #can choose multiple states with: states = c("", "")
# check CRS with st_crs()
# transform to NAD 83
ca_albers <- st_transform(ca, crs = 3310)

# or get county data
ca_county <- us_counties(states = "california", resolution = "high")

la_county_albers <- st_transform(ca_county, crs = 3310) %>% 
  filter(name == "Los Angeles") #choose just LA county for example

ggplot()+
  geom_sf(data = la_county_albers)+
  geom_sf(data = csci_point_albers)

# AND THERES MORE!

# 'leaflet' package also has great tools for spatial mapping, interactive maps

# resource below has good breakdown of additional packages and examples, including leaflet
# https://bookdown.org/nicohahn/making_maps_with_r5/docs/introduction.html

# 'prism' package as spatial climate data you can download!
# https://rpubs.com/collnell/get_prism

# End of script.