library(maps)
library(tidyverse)
library(usmap)
library(raster)
library(sp)
library(terra)
library(ggplot2)
library(sf)
getwd()

# Replace with your full directory, rather than the one I used
wmas <- st_read("C:/Users/lkp0038/OneDrive - Auburn University/Auburn/MS/ICP 2.0/icp1/icp1/ICP_PC_Overlap/WMAs.shp")

ggplot(wmas) +
  geom_sf() +
  theme_minimal() +
  ggtitle("WMAs Shapefile")

#Replace with your directory again
tidy_BirdLevel2_PointVisits <- read.csv("C:/Users/lkp0038/OneDrive - Auburn University/Auburn/MS/ICP 2.0/icp1/icp1/Tidy Tables/tidy_BirdLevel2_PointVisits.csv",
                                        header = T)

# Convert the data frame to an sf object
points_sf <- st_as_sf(tidy_BirdLevel2_PointVisits, 
                      coords = c("Longitude", "Latitude"), crs = 4326)

# If necessary, transform points to match the CRS of the WMAs
points_sf <- st_transform(points_sf, st_crs(wmas))

# Plot
library(ggplot2)
ggplot() +
  geom_sf(data = wmas, fill = 'lightblue', color = 'blue') +
  geom_sf(data = points_sf, color = 'red', size = 0.3) +
  theme_minimal() +
  ggtitle("WMAs with ICP 1 Bird Observation Points")


# Perform spatial intersection
intersections <- st_intersects(points_sf, wmas)

# Count points within WMAs
num_points_within_wmas <- sum(lengths(intersections) > 0)
num_points_within_wmas

