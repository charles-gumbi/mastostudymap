###### Bonginkosi Gumbi PhD study map ###### 
##### Eswatini 2017 Landuse map ###### 

##### Loading libraries ####
library(raster)
library(SDMTools)
library(rgeos)
library(GGally)
library(reshape2)
library(rgdal)
library(dplyr)
library(RSAGA)
library(Rcpp)
library(ggplot2)
library(rasterVis)
library(rgeos)
library(mapview)
library(leaflet)
library(broom)
library(dplyr)
library(data.table)
library(tidyverse)
library(raster)
library(sf)
library(rgdal)
library(ggspatial)
library(cowplot)
library(gridExtra)
library(ggpubr)

# Setting seed for reproducibility
set.seed(100)
# Setting plotting theme
theme_set(theme_bw())

# Importing the landcover map from land2016
eswatini_map <- raster('~/Documents/Phd_dessetation/Study_map/land2016')
plot(eswatini_map)

# convert the eswatini map into a ratser tif format
# save eswatini map as a raster file
writeRaster (eswatini_map, 'eswatini.tiff', format = 'GTiff')
GDALinfo('~/Documents/Phd_dessetation/Study_map/land2016')
eswatini_raster <- raster("~/Documents/Manuscript/Ires/swaziland_landcover/land2016/eswatini.tif")

table(values(eswatini_raster))  
## Landuse classifcation: 
## 1. Bare area
## 2. Bushland
## 3. Cropland plantation
## 4. Cropland small scale
## 5. Forest
## 6. Grasslands 
## 7. Riverine vegetation  
## 8. Urban areas 
## 9. Water bodies
## 10.Sugarcane

# Reclassifying map to focuss on the only four landuse covers 
unique(eswatini_raster)

# Creating a vector of old and new classes
vec <- c(1,2,3,4,5,6,7,8,9,10, 
         1,2,3,1,2,2,2,8,2,2)

# Creating a reclass matrix by turning our vector into a matrix
reclass <- matrix(vec, ncol=2, byrow=F)
eswatini_raster_rcls <- reclassify(eswatini_raster, reclass)
unique(eswatini_raster_rcls)
table(values(eswatini_raster_rcls)) 
# Ploting map
plot(eswatini_raster_rcls)

writeRaster (eswatini_raster_rcls, 'eswatini_map.tiff', format = 'GTiff')

eswatini_raster <- raster('~/Documents/Phd_dessetation/Study_map/land2016/eswatini_map.tif')

# Getting summary of the raster 
summary(eswatini_raster)

# This way you can take all cells into account
summary(eswatini_raster, maxsamp = ncell(eswatini_raster))

# Built-in raster function for plotting
plot(eswatini_raster)

# converting the reclassified raster into a dataframe 
eswatini_raster_rcl_df <- as.data.frame(eswatini_raster, xy = TRUE, na.rm = TRUE)

# information goes lost
# Note that this is a raster function, see ?as.data.frame
# Check the structure of this new object
str(eswatini_raster_rcl_df)

plot(eswatini_raster)

eswatini_raster_rcl_df <- eswatini_raster_rcl_df %>%
  mutate(landuse = case_when(eswatini_map <= '1' ~ 'Rural',
                             eswatini_map <= '2' ~ 'Savanna',
                             eswatini_map <= '3' ~ 'Sugarcane',
                             eswatini_map <= '8' ~ 'Urban')) 
 ggplot() +
  geom_raster(data = eswatini_raster_rcl_df, aes(x=x, y=y, fill = landuse)) + 
  scale_fill_brewer(name = 'landuse', palette ='BrBG', na.value = 'white') +
   #scale_fill_manual(values = terrain.colors(4)) + 
  geom_sf(data = sites, aes(color = habitat, shape = habitat), size = 1) + 
  theme_bw() +
  theme(axis.title = element_blank(),
        text = element_text(size = 12,  family= 'serif'), 
        strip.text.x = element_text(colour = 'black', size = 12),
        strip.text.y = element_text(colour = 'black', size = 12),
        axis.text.x = element_text(size = 12, color = 'black'),
        axis.text.y = element_text(size = 12, color = 'black'),
        legend.title = element_text(colour = 'black', size = 12),
        panel.grid = element_blank()) +
  annotation_scale(location = 'bl', width_hint = 0.5) +
  annotation_north_arrow(location ='bl', which_north = "true", 
                         pad_x = unit(0.75, "cm"), pad_y = unit(0.75, "cm"),
                         style = north_arrow_fancy_orienteering) + 
  coord_sf(expand = FALSE, datum = st_crs(crs_to_use)) 

dev.off()

##### Reading in and converting vegetation data in to spatial object #########
# Read in the sampling sites
sites <- read_csv('~/Documents/Phd_dessetation/Study_map/data/loc.csv')

# Turning the sites into a spatial object 
crs_to_use <- st_crs(eswatini_raster)
sites <- st_as_sf(sites, coords = c("easting", "northing"), crs = crs_to_use)

########### Make sure all CRS are the same ############
crs(site_spatial)
crs(eswatini_raster)
world <- st_as_sf(world, coords = c("easting", "northing"), crs = crs_to_use)


library("ggplot2")
theme_set(theme_bw())
library("sf")

library("rnaturalearth")
library("rnaturalearthdata")

africa <- ne_countries(continent = 'africa', scale = 'large', returnclass = 'sf')
class(africa)
plot(africa)











# Renaming landsuses and plotting 


  
ggplot(aes(data = eswatini_raster_rcl_df, x=x, y=y, fill = eswatini_map)) + 
  geom_raster() +
  scale_fill_brewer(name = 'eswatini_map', palette ='BrBG', na.value = 'white') +
  coord_quickmap(expand = FALSE) 

# Reading in and converting sampling point into spatial object ##############
sampling_sites <- read_csv('~/Desktop/database/analysis/points.csv')

# Turning the sampling points into a spatial object(sampling_point_spatial) 
crs_to_use <- st_crs(eswatini_raster_rcls)
sampling_point_spatial <- st_as_sf(sampling_sites, coords = c("latitude", "longitude"), 
                                  crs = crs_to_use)
# Renaming landsuses and plotting 
eswatini_raster_rcl_df <- eswatini_raster_rcl_df%>% 
  mutate(landuse = 
           case_when(eswatini <= "1" ~ "Savanna", 
                     eswatini <= "2" ~ "Rural", 
                     eswatini <= "3" ~ "Sugarcane",
                     eswatini <= "8" ~ "Urban"))
  

ggplot(data = eswatini_raster_rcl_df, aes(x=x, y=y, fill = landuse)) + 
  geom_raster() +
  scale_fill_brewer(name = "landuse", palette ='BrBG', na.value = 'white') +
  geom_sf(data = sampling_point_spatial,  color = "black", size = 2, fill = NA) +
  coord_sf(expand = FALSE)
  
  
geom_text(data = points, aes(x = X, y = Y, label = plotID) , color = 'black', 
            nudge_x = 0, nudge_y = -40, size = 3, inherit.aes = TRUE) +
  theme_bw() +
  theme(axis.title = element_blank(),
        text = element_text(size = 12,  family= 'serif'), 
        legend.position = 'top',
        panel.grid = element_blank()) +
  annotation_scale(location = 'bl', width_hint = 0.5) +
  annotation_north_arrow(location ='bl', which_north = "true", 
                         pad_x = unit(0.75, "cm"), pad_y = unit(0.75, "cm"),
                         style = north_arrow_fancy_orienteering) +
  coord_sf(expand = FALSE)





























