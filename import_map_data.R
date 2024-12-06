rm(list = ls()) #clear environment

# install packages --------------------------------------------------------

library(tidyverse) #for everything
library(marmap) #for noaa bathy
library(mapdata) #for basemaps
library(sf) #for all mapping stuff
library(tigris) #for census data
library(ncdf4) #for nedcdf files
library(terra) #for raster stuff

sf_use_s2(FALSE)

# load census data --------------------------------------------------------

#import high resolution data of southern new england (used for main masp)
RI.sf <- states() %>%
  filter(STUSPS %in% c('RI', 'MA', "CT")) %>%
  erase_water(area_threshold = 0.9)

saveRDS(RI.sf, file="Rdata/census_RI.rds")

#import high resolution data of RI only
RI.sf <- states() %>%
  filter(STUSPS %in% c('RI')) %>%
  erase_water(area_threshold = 0.5)

saveRDS(RI.sf, file="Rdata/census_RI_highres.rds")

#import data for all of new england
NE.sf <- states(cb=TRUE) %>%
  filter(STUSPS %in% c('RI', 'MA', "CT", "ME", "NH", "VT","NY", "NJ","PA")) %>%
  erase_water(area_threshold = 0.98)

saveRDS(NE.sf, file="/Users/ilanajacobs/SURFO/AGU_Data/Map/census_NE.rds")

#import coarse resolution data of the united states (used for inset map)
USA.sf <- states(cb=TRUE)

saveRDS(USA.sf, file="/Users/ilanajacobs/SURFO/AGU_Data/Map/census_USA.rds")


#we can also import roads, but honestly I haven't been using this
#use a variable to control this import
import_roads <- TRUE

if(import_roads){
  
  RI_roads.sf <- primary_secondary_roads(state=c("RI"))
  MA_roads.sf <- primary_secondary_roads(state=c("MA"))
  
  RI_roads.sf <- rbind_tigris(RI_roads.sf, MA_roads.sf)
  
  saveRDS(RI_roads.sf, file="Rdata/RI_roads.rds")
  
  prov_roads.sf <- roads(state="RI", county="Providence")
  saveRDS(prov_roads.sf, file="Rdata/prov_roads.rds")
  
  major_roads.sf <- primary_roads()
  
  saveRDS(major_roads.sf, file="Rdata/major_roads.rds") 
}

# load topo data ----------------------------------------------------------

#topo for broader new england region
#set coordinates
xmin=-76
xmax=-67
ymin=39
ymax=45

NE_topo <- getNOAA.bathy(lon1 = xmin, lon2 = xmax, lat1 = ymin, lat2 = ymax, resolution = 1)

# convert bathymetry to data frame
NE_topo.df = fortify.bathy(NE_topo)

saveRDS(NE_topo.df,file="/Users/ilanajacobs/SURFO/AGU_Data/Map/newengland_topobathy.rds")

eastcoast_topo <- getNOAA.bathy(lon1 = -85, lon2 = 68, lat1 = 30, lat2 = 45, resolution = 4)

eastcoast_topo = fortify.bathy(eastcoast_topo)

saveRDS(eastcoast_topo,file="/Users/ilanajacobs/SURFO/AGU_Data/Map/eastcoast_topobathy.rds")

# convert bathymetry to data frame

world_topo <- getNOAA.bathy(lon1 = -180, lon2 = 180, lat1 = -90, lat2 = 90, resolution = 16)

# convert bathymetry to data frame
world_topo = fortify.bathy(world_topo)

saveRDS(world_topo,file="/Users/ilanajacobs/SURFO/AGU_Data/Map/world_topobathy.rds")


# import narragansett bay DEM ---------------------------------------------

#set coordinates for narragansett bay region
xmin=-72
xmax=-70
ymin=41
ymax=42

#download high resolution topographic data for narragansett bay
nb_topo <- getNOAA.bathy(lon1 = xmin, lon2 = xmax, lat1 = ymin, lat2 = ymax, resolution = 0.01)

# convert bathymetry to data frame
nb_topo = fortify.bathy(nb_topo)


#make raster for later
nb_topo <- rast(nb_topo, crs="epsg:4326")

nc <- nc_open("/Users/ilanajacobs/SURFO/AGU_Data/Map/narragansett_bay.nc")

#summarize that data
print(nc)

#get dimensions
x <- ncvar_get(nc, "x", verbose = FALSE)
y <- ncvar_get(nc, "y", verbose = FALSE)
z <- ncvar_get(nc, "Band1", verbose = FALSE)

#get fill value and replace those with NA
fillvalue <- ncatt_get(nc, "Band1", "_FillValue")$value
z[z == fillvalue] <- NA

#close the connection
nc_close(nc)

# transpose to lat (rows), lon (cols)
z.tmp <- t(z)

#convert to raster
r <- rast(z.tmp,
          extent=ext(min(x), max(x), min(y), max(y)),
          crs = "+init=epsg:26919")

# flip on vertical axis
r <- flip(r, direction = "vertical") 

#convert to lat long
r <- project(r, "epsg:4326")

#check to make sure this is right
plot(r)

#convert to dataframe for export
df <- as.data.frame(r, xy=TRUE)

colnames(df) <- c("x","y","z")

saveRDS(df, "/Users/ilanajacobs/SURFO/AGU_Data/Map/narragansett_highres.rds")

r <- aggregate(r, fact=4)

nb_topo <- project(nb_topo, r, align_only=TRUE)

#combine with the existing narragansett topobathy data
cover <- mosaic(r, nb_topo, fun="first")

#convert to dataframe for export
df <- as.data.frame(cover, xy=TRUE)

colnames(df) <- c("x","y","z")

saveRDS(df, "/Users/ilanajacobs/SURFO/AGU_Data/Map/narragansett_lowres.rds")
