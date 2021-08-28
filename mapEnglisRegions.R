#!/usr/bin/env Rscript
#
# Experiment to create maps for the data.
#
# Based on: https://datatricks.co.uk/creating-maps-in-r
#


# Load packages -----------------------------------------------------------

library(maps)
library(mapdata)
library(maptools)
library(rgdal)
library(ggmap)
library(ggplot2)
library(rgeos)
library(broom)
library(plyr)


# Load map data -----------------------------------------------------------


# Download the 9 layer English regions from:
#
# https://geoportal.statistics.gov.uk/datasets/regions-december-2015-full-clipped-boundaries-in-england/explore?location=52.964930%2C-2.000000%2C6.84
#
# or
#
# https://geoportal.statistics.gov.uk/datasets/regions-december-2017-full-clipped-boundaries-in-england/explore?location=52.950000%2C-2.000000%2C6.75
#
# Finer level maps are available.
#
# Save the contents of the zip file into Data/UKRegions/2015 or Data/UKRegions/2017.
#

# Read in the Shapefiles
#shapefile <- readOGR(dsn="./Data/UKRegions/2015/")
shapefile <- readOGR(dsn="./Data/UKRegions/2017/")

# Get the map data
mapdata <- tidy(shapefile)

# id   Region     (This is based on looking at the map below)
# 0    North East
# 1    North West
# 2    Yorkshire and The Humber
# 3    East Midlands
# 4    West Midlands
# 5    East of England
# 6    London
# 7    South East
# 8    South West

# Plot the map data
ggplot() +
  geom_polygon(data = mapdata, aes(x = long, y = lat, group = group, fill=id),
               colour="black",size = 0.25) +
  coord_fixed(1) + # This gives the map a 1:1 aspect ratio
  theme_minimal()

# A minimalist version of the map
ggplot() +
  geom_polygon(data = mapdata, aes(x = long, y = lat, group = group, fill=id),
               colour="black",size = 0.25) +
  coord_fixed(1) + # This gives the map a 1:1 aspect ratio
  theme_minimal() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = 'none') +
  theme(axis.title.x=element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) +
  theme(axis.title.y=element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())

