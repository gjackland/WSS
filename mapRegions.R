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
library(tidyr)

# Load map data -----------------------------------------------------------


# Download the 9 layer English regions from:
#
# https://geoportal.statistics.gov.uk/datasets/regions-december-2015-full-clipped-boundaries-in-england/explore?location=52.964930%2C-2.000000%2C6.84
#
# or
#
# https://geoportal.statistics.gov.uk/datasets/regions-december-2017-full-clipped-boundaries-in-england/explore?location=52.950000%2C-2.000000%2C6.75
#
# The UK polygon data is available from:
#
# https://geoportal.statistics.gov.uk/datasets/nuts-level-1-january-2018-super-generalised-clipped-boundaries-in-the-united-kingdom/explore?location=54.650000%2C-3.250000%2C5.63
#
# Finer level maps are available.
#
# Save the contents of the zip file into Data/UKRegions/2015 or Data/UKRegions/2017.
#
# Data is provided under an Office for National Statistics licensed under the
# Open Government Licence v.3.0 and contains OS data © Crown copyright and
# database 2015, 2017, 2018.
#
# https://www.ons.gov.uk/methodology/geography/licences
#

# You need to read the right type of shape file for the map you want to plot.
#
# Read in the Shapefiles
#eng_shapefile <- readOGR(dsn="./Data/UKRegions/2015/")
eng_shapefile <- readOGR(dsn="./Data/UKRegions/2017/")
uk_shapefile <- readOGR(dsn="./Data/UKRegions/2018-UK")

# Get the map data
mapdata <- tidy(eng_shapefile)
ukmapdata <- tidy(uk_shapefile)

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

# Map Region to id for the English regions
regions2id <- tibble("North East"="0",
                    "North West"="1",
                    "Yorkshire and The Humber"="2",
                    "East Midlands"="3",
                    "West Midlands"="4",
                    "East of England"="5",
                    "London"="6",
                    "South East"="7",
                    "South West"="8",
                    "Wales"="9",
                    "Scotland"="10",
                    "NI"="11")

# Plot the map data for English regions - check the above mapping corresponds
ggplot() +
  geom_polygon(data = mapdata, aes(x = long, y = lat, group = group, fill=id),
               colour="black",size = 0.25) +
  coord_fixed(1) + # This gives the map a 1:1 aspect ratio
  theme_minimal()

# Plot the map data for uk - check the above mapping corresponds
ggplot() +
  geom_polygon(data = ukmapdata, aes(x = long, y = lat, group = group, fill=id),
               colour="black",size = 0.25) +
  coord_fixed(1) + # This gives the map a 1:1 aspect ratio
  theme_minimal()

# A minimalist version of the English map
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

# Now plot actual data - this is for the English regions only
keepcols <- names(rat)[2:ncol(rat)]
keepdate <- max(rat$date)
title_date <- format(keepdate,"%d/%m/%y")

rat %>% pivot_longer(cols=keepcols, names_to = "regions", values_to = "values") %>%
        filter(regions %in% c("North East","North West","Yorkshire and The Humber",
                              "East Midlands","West Midlands","London","East of England",
                              "South East","South West") & date == keepdate) %>%
        mutate(id = recode(regions,!!!regions2id) )  %>%  # from stackoverflow 31296092
        inner_join(mapdata, by = "id") %>%
        mutate(totals = scale(values)) %>%
        ggplot() +
        geom_polygon(aes(x = long, y = lat, group = group, fill = values),
                    colour="black",size = 0.25) +
        coord_fixed(1) + # This gives the map a 1:1 aspect ratio
        theme_minimal() +
  theme(axis.title.x=element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) +
  theme(axis.title.y=element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  ggtitle(paste0("R Values for ", title_date))

# This plot is for all the UK regions
keepcols <- names(rat)[2:ncol(rat)]
keepdate <- max(rat$date)
title_date <- format(keepdate,"%d/%m/%y")

rat %>% pivot_longer(cols=keepcols, names_to = "regions", values_to = "values") %>%
  filter(regions %in% c("North East","North West","Yorkshire and The Humber",
                        "East Midlands","West Midlands","London","East of England",
                        "South East","South West","Scotland","Wales","NI") & date == keepdate) %>%
  mutate(id = recode(regions,!!!region2id) )  %>%  # stackoverflow 31296092
  inner_join(ukmapdata, by = "id") %>%
  mutate(totals = scale(values)) %>%
  ggplot() +
  geom_polygon(aes(x = long, y = lat, group = group, fill = values),
               colour="black",size = 0.25) +
  coord_fixed(1) + # This gives the map a 1:1 aspect ratio
  theme_minimal() +
  theme(axis.title.x=element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) +
  theme(axis.title.y=element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  ggtitle(paste0("R Values for ", title_date))
