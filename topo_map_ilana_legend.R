rm(list = ls()) #clear environment

# install packages --------------------------------------------------------

library(tidyverse) #for everything
library(marmap) #for noaa bathy
library(mapdata) #for basemaps
library(sf) #for all mapping stuff
library(metR) #for contour functions. better than ggplot
library(ggspatial) #for north arrow, distance scale, etc.
library(ggnewscale) #for separate fill scales by layer
library(ggsci) #for color pallets
library(tigris) #for census data
library(cowplot) #for ggdraw (inset)
library(ggrepel) #for labels
library(dplyr)
library(lwgeom)

# some global stuff -------------------------------------------------------

#coordinate for main map
xmin=-71.45
xmax=-71.4
ymin=41.53
ymax=41.57

#xmin=-71.55
#xmax=-70.95
#ymin=41.4
#ymax=41.9

sf_use_s2(FALSE)

# import topography data --------------------------------------------------

#import topography data
topobathy.df <- readRDS("/Users/ilanajacobs/SURFO/Rdata/narragansett_highres.rds")
NE_topo.df <- readRDS("/Users/ilanajacobs/SURFO/Rdata/newengland_topobathy.rds")

#crop the data to our coordinate limits defined above
#cropping is important for the color scales - otherwise becomes hard to read the map
topobathy.df <- topobathy.df %>%
  filter(between(x, xmin-0.001, xmax+0.001),
         between(y, ymin-0.001, ymax+0.001))

#make a subset that is only the underwater data
bathy.df <- topobathy.df %>%
  filter(z<0)

# import census (i.e. land) data ------------------------------------------

#import census data
RI.sf <- readRDS("/Users/ilanajacobs/SURFO/Rdata/census_RI_highres.rds")
USA.sf <- readRDS("/Users/ilanajacobs/SURFO/Rdata/census_USA.rds")

# import buoy data --------------------------------------------------------
#this is specific to my project

buoy_coords <- data.frame(
  lon = c(-71.41479),  # Longitudes of the buoys
  lat = c(41.551)     # Latitudes of the buoys
)
#import buoy stations
#locations.df <- readRDS("Rdata/location_info.rds") %>%
 # filter(station!="PB")

#convert to sf object
#stations.sf <- st_as_sf(locations.df,coords=c("longitude.degrees","latitude.degrees"),crs="+proj=longlat +datum=WGS84")

#remove NOAA
#stations.sf <- stations.sf %>%
 # filter(type=="Water Quality") %>%
 # mutate(region_3=factor(region_3, levels=c("EXCLUDED", "Upper Bay", "Mid Bay", "Lower Bay")))

sites<- read_sf("/Users/ilanajacobs/SURFO/Map/Aquaculture_FoxIsland.shp")

# configure_graphing ------------------------------------------------------

#colors we want
land_color="grey70"
water_color="slategray1"

mytheme <- list(
  theme_bw(),
  scale_shape_manual(values=c(21:26)),
  labs(x=NULL, y=NULL),
  theme(legend.background = element_blank(),
        legend.box.background = element_rect(fill="black",colour = "white"),
        panel.grid=element_blank()
        #panel.background = element_rect(fill = water_color)
        )
)

#theme for the map inset. this just removes the axis ticks
insettheme <- list(
  theme(plot.background=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank())
)

# Prepare a Legend -----------------------------------------------------------

# Combine legend data with existing data
buoy_coords$type <- "AquaTroll 500"
sites$type <- "Aquaculture Sites"

# Bind all legend-relevant data into one dataframe
buoy_coords$type <- "AquaTroll 500"
legend_data <- data.frame(
  lon = c(-71.48, -71.48),  # Placeholder coordinates for legend items
  lat = c(41.54, 41.53),    # Placeholder coordinates for legend items
  type = c("AquaTroll 500", "Aquaculture Sites") # Correct labels
)


# make a map of narragansett bay ------------------------------------------

# Update the map

p1a <- ggplot() +
  mytheme+
  #plot the underwater bathymetry
  geom_contour_fill(data = bathy.df, aes(x=x, y=y, z=z),
                    binwidth=1.25, show.legend = FALSE)+
  scale_fill_viridis_c(option = "mako", alpha = .85)+
  geom_point(data = buoy_coords,
             aes(x = lon, y = lat),
             color = "black",
             size = 3,
             shape = 21,
             fill = "yellow",
             alpha = 0.8) +
  geom_sf(data=RI.sf, fill=land_color)+
  theme(legend.position=c(0.85,0.85))+
  annotation_scale(location = "br", width_hint = 0.5, text_col="black") +
  geom_sf(data = sites,
          fill = "hotpink", size=4, alpha=.65)+
  annotation_north_arrow(location = "tl", which_north = "true",
                         pad_x = unit(0.1, "in"), pad_y = unit(0.1, "in"),
                         style = north_arrow_fancy_orienteering)+
  labs(fill="Region",shape="Region")+
  coord_sf(xlim = c(xmin, xmax), ylim = c(ymin, ymax), expand = FALSE)

p1a <- ggplot() + 
  mytheme + 
  # plot the underwater bathymetry
  geom_contour_fill(data = bathy.df, aes(x = x, y = y, z = z), binwidth = 1.25, show.legend = FALSE) + 
  scale_fill_viridis_c(option = "mako", alpha = .85) + 
  geom_point(data = buoy_coords, aes(x = lon, y = lat), color = "black", size = 3, shape = 21, fill = "yellow", alpha = 0.8) + 
  geom_sf(data = RI.sf, fill = land_color) + 
  theme(legend.position = c(0.85, 0.85)) + 
  annotation_scale(location = "br", width_hint = 0.5, text_col = "black") + 
  geom_sf(data = sites, fill = "hotpink", size = 4, alpha = .65) + 
  annotation_north_arrow(location = "tl", which_north = "true", 
                         pad_x = unit(0.1, "in"), pad_y = unit(0.1, "in"), 
                         style = north_arrow_fancy_orienteering) + 
  labs(fill = "Region", shape = "Region") + 
  coord_sf(xlim = c(xmin, xmax), ylim = c(ymin, ymax), expand = FALSE)  # apply initial zoom

# 1. Filter the `sites` shapefile to select the specific aquaculture site based on the attribute (e.g., site name)
site_geom <- sites %>% 
  filter(Name == "Whilden")  # Replace with actual site name

# 2. Calculate the perimeter (boundary) of the farm's geometry
farm_perimeter <- st_length(st_boundary(site_geom))  # This gives the perimeter length

# 3. Get the area of the selected farm from the attribute table
selected_farm <- site_geom %>% 
  st_transform(crs = 4326)  # Transform to appropriate CRS if needed for area calculation

site_area <- st_area(selected_farm)  # Get the area of the farm

# 4. Calculate the buffer radius (15 times the square root of the area)
buffer_radius <- sqrt(as.numeric(site_area)) * 15  # sqrt(area) * 15 for buffer radius

# 5. Create the buffer (15 times the perimeter length)
buffer <- st_buffer(site_geom, dist = buffer_radius)

# 6. Add the buffer to your plot
p1a <- p1a + 
  geom_sf(data = buffer, fill = "white", alpha=0.5, color = "white", size = 1, linetype = "dashed")

# Reapply zoom after adding the buffer
p1a <- p1a + coord_sf(xlim = c(xmin, xmax), ylim = c(ymin, ymax), expand = FALSE)

# Display the plot with the buffer and zoom
p1a


ggsave(p1a, file="/Users/ilanajacobs/SURFO/Map/extent_map.png")


# p1a <- p1a +
#   # Add legend items (invisible points for legend purposes)
#   geom_point(data = legend_data, 
#              aes(x = lon, y = lat, shape = type, fill = type), 
#              size = 4, alpha = 0) +  # Invisible points used only for legend
#   # Define shapes and fills with labels for the legend
#   scale_shape_manual(
#     values = c("AquaTroll 500" = 21, "Aquaculture Sites" = 22),
#     labels = c("AquaTroll 500" = "AquaTroll 500", 
#                "Aquaculture Sites" = "Aquaculture Sites")
#   ) +
#   scale_fill_manual(
#     values = c("AquaTroll 500" = "yellow", "Aquaculture Sites" = "hotpink"),
#     labels = c("AquaTroll 500" = "AquaTroll 500", 
#                "Aquaculture Sites" = "Aquaculture Sites")
#   ) +
  # Customize legend position and appearance
#   theme(
#     legend.position = c(0.2, 0.1),  # Adjust legend position within the map
#     legend.background = element_rect(fill = "white", color = "black"), # White background with black border
#     legend.key.size = unit(0.8, "lines"),  # Reduce legend key size
#     legend.text = element_text(size = 8, color = "black"),  # Smaller legend text
#     legend.title = element_text(size = 9, color = "black", face = "bold", hjust = 0.5)  # Smaller, bold, centered title
#   ) +
#   guides(
#     shape = guide_legend(
#       title = "Legend",  # Add a title for the legend
#       override.aes = list(size = 4, color = "black", 
#                           fill = c("hotpink", "yellow"))
#     ),
#     fill = "none"  # Avoid duplicate legends for fill
#   )
# 
# p1a

# make a map of new england -----------------------------------------------

p2 <- ggplot() +
  mytheme+
  #add USA land data
  geom_sf(data=USA.sf, fill="grey")+
  geom_rect(aes(xmin=xmin,ymin=ymin,xmax=xmax,ymax=ymax),fill = NA, color = "red",linewidth = 0.45)+
  coord_sf(xlim = c(-70.5, -71.8), ylim = c(41.25, 42.25), expand = FALSE)

p2

# combine plots -----------------------------------------------------------

final <- ggdraw() +
  draw_plot(p1a) +
  draw_plot(p2+insettheme,
            height = 0.25,
            x = 0.3,
            y = 0.7)+
  theme_classic()

#to save time, the plot does not render by default.
final

ggsave(final, file="/Users/ilanajacobs/SURFO/Map/sites_map.png")


# bigger map --------------------------------------------------------------

#coordinate for main map
xmin_NE=-75
xmax_NE=-68
ymin_NE=40.5
ymax_NE=44

#crop the topography data again
topobathy.df <- NE_topo.df %>%
  filter(between(x, xmin_NE-0.1, xmax_NE+0.1),
         between(y, ymin_NE-0.1, ymax_NE+0.1))

#define min and max altitude for the contour breaks
minZ <- min(topobathy.df$z)
maxZ <- max(topobathy.df$z)

ggplot() +
  mytheme+
  geom_contour_fill(data = topobathy.df, aes(x=x, y=y, z=z), breaks=seq(0,maxZ,50),
                    show.legend = FALSE, color="black", linewidth=0.1)+
  scale_fill_viridis_c(option="rocket", direction=-1)+
  new_scale_fill()+
  geom_contour_fill(data = topobathy.df, aes(x=x, y=y, z=z), breaks=seq(0,minZ,-20),
                    show.legend = FALSE, color="black", linewidth=0.1)+
  scale_fill_viridis_c(option="mako")+
  geom_sf(data=USA.sf, fill="black", alpha=0.2)+
  geom_sf(data=USA.sf, fill=NA, color="black")+
  geom_rect(aes(xmin=xmin,ymin=ymin,xmax=xmax,ymax=ymax),fill = NA, color = "black",linewidth = 1)+
  coord_sf(xlim = c(xmin_NE, xmax_NE), ylim = c(ymin_NE, ymax_NE), expand = FALSE)


# RI map --------------------------------------------------------------

#coordinate for main map
xmin_NE=-72.5
xmax_NE=-68
ymin_NE=40.5
ymax_NE=43

#crop the topography data again
topobathy.df <- NE_topo.df %>%
  filter(between(x, xmin_NE-0.1, xmax_NE+0.1),
         between(y, ymin_NE-0.1, ymax_NE+0.1))

#define min and max altitude for the contour breaks
minZ <- min(topobathy.df$z)
maxZ <- max(topobathy.df$z)

ggplot() +
  mytheme+
  geom_contour_fill(data = topobathy.df, aes(x=x, y=y, z=z), breaks=seq(0,maxZ,50),
                    show.legend = FALSE, color="grey40", linewidth=0.1)+
  scale_fill_viridis_c(option="rocket", direction=-1)+
  new_scale_fill()+
  geom_contour_fill(data = topobathy.df, aes(x=x, y=y, z=z), breaks=seq(0,minZ,-20),
                    show.legend = FALSE, color="grey40", linewidth=0.1)+
  scale_fill_viridis_c(option="mako")+
  geom_sf(data=USA.sf, fill="grey40", alpha=0.2)+
  geom_sf(data=USA.sf, fill=NA, color="grey40")+
  geom_rect(aes(xmin=xmin,ymin=ymin,xmax=xmax,ymax=ymax),fill = NA, color = "black",linewidth = 1)+
  coord_sf(xlim = c(xmin_NE, xmax_NE), ylim = c(ymin_NE, ymax_NE), expand = FALSE)

#ggsave(file="figures/new_england_map.png")

