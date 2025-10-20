#Maggie Phillips
#Zoo800
#HW 7

library(sf)
library(tidyverse)
library(ggtext)

##-----------below is Maggie's code to make a map of study sites-------------##

#read in my shapefile and csv with coords
Wisc<- st_read("Wisconsin_State_Boundary_24K.shp") #downloaded Wisc boundary from DNR
coordinates<- read.csv("df_sites.csv") #csv of site lat/longs
coords_df<- data.frame(coordinates) #sf obj conversion didn't work til I made this a df

crs_info <- st_crs(Wisc) # seeing what CRS the shapefile is in

# Print the CRS information
print(crs_info)

#need to make coords df into a sf obj for mapping
sf_coords<- st_as_sf(coords_df, 
                     coords= c("LONG_approx", #long NEEDS to come first!
                               "LAT_approx"),
                     crs= 4326) #this is for WGS84 for lat/long
#converted the coordinates to point geometry

#making sure the coordinates and shapefile have the same crs
sf_coords <- st_transform(sf_coords, st_crs(Wisc))

#separating stream out from other monitoring types so that it can be seen on map
stream_sites <- sf_coords[sf_coords$Monitoring == "Stream", ]
other_sites <- sf_coords[sf_coords$Monitoring != "Stream", ]


#trying this bc some points are almost exactly overlapping, so it's hard to see
#didn't end up using this, but good to know it exists
sf_coords_jittered <- sf::st_jitter(sf_coords, amount = 500)


ggplot() +
  geom_sf(data = Wisc, fill = "gray", color = "black") + #grey fill and black border
  geom_sf(data = other_sites,
          aes(fill = Monitoring), #filling circles by monitoring type
          shape= 21, size = 5, alpha = 0.7) + #cirlce shape. 0.7 opacity
  
  # Plot Stream sites last (on top)
  #streams are right next to each other--basically show up as one point
  geom_sf(data = stream_sites,
          aes(fill = Monitoring),
          shape= 21, size = 5, alpha = 0.9) + #drawing this layer last so it's on top
  
  #using markdown syntax to insert line breaks, control line height, and bold the figure caption
  labs(
    caption = "<span style='line-height:0.9'><b>Figure 1.</b> Wisconsin Discovery Farm research stations representing<br>farms across a range of crop rotations, tillage, and manure practices.</span>"
  )+
  theme_minimal()+ 
  theme(
    plot.caption = element_markdown( #using markdown so that I can more easily bold "figure 1" and format caption
      size= 12, #caption text size
      hjust= 0, #justify left
    ),
    legend.title = element_text(size = 14), #increasing legend title and text size
    legend.text = element_text(size = 12),
    panel.grid = element_blank(),
    legend.position = c(0, 0.025),  # bottom left corner (tweak as needed)
    legend.justification = c(0, 0),  # anchor legend to bottom-left corner  
    axis.text = element_blank(), #googled this. getting rid of gridlines and lat/long   
    axis.ticks = element_blank(),  #getting rid of tick marks         
    axis.title = element_blank() #getting rid of axis title
  )