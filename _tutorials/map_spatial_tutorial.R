library(dplyr)
library(sf)
library(sfdep)
library(sp)
library(spdep)
library(readr)
library(ggplot2)
library(ggthemes)

# import data for municipalities

data_municipalities <- read_csv("../_data/data_municipalities.csv")

# import a shapefile for municipalities

map_municipalities <- st_read("../_data/shapefile/map_municipalities.shp")

# plot the borders of municipalities

plot(st_geometry(map_municipalities))

# check if municipality codes in the datafile and the map are the same

which(!map_municipalities$mncplty_c %in% data_municipalities$municipality_code)
which(!data_municipalities$municipality_code %in% map_municipalities$mncplty_c )

# OK - all are the same

# lets add the data to the map object

map_municipalities_with_data <- 
  map_municipalities %>% 
  left_join(data_municipalities, 
            by = join_by(mncplty_c == municipality_code))

# and make a visualization for a selected variable -- e.g. percent_vaccinated

ggplot(map_municipalities_with_data, 
       aes(fill = percent_vaccinated)) +
  geom_sf() +
  ggthemes::theme_map() +
  scale_fill_continuous()

#-------------------------------------------
# import a shapefile for historical partitions of Poland

map_partitions <- st_read("../_data/shapefile/map_partitions.shp")

# lets plot historical borders between partitions

ggplot() +
  geom_sf(data = map_partitions, 
          aes(fill = partition)) +
  ggthemes::theme_map()
  
# lets try to overlay the two maps

ggplot() +
  geom_sf(data = map_municipalities_with_data, 
          aes(fill = percent_vaccinated)) +
  ggthemes::theme_map() +
  scale_fill_continuous() +
  geom_sf(data = map_partitions, 
          size = 2, 
          color = "red",
          fill = NA)




#---------------------------------------------------------------
# creating spatial weight matrix - contiguity (i.e. common border)
municipalities_neighbours <- sfdep::st_contiguity(map_municipalities)

# lets check its structure
glimpse(municipalities_neighbours)

# converting a list of neighbours into a matrix
spatial_weights <- nb2mat(municipalities_neighbours)

# you can check if it is row standardized by default
summary(rowSums(spatial_weights))

# OK

# based on that we can create a variable which is a spatial lag of 
# percent vaccinated (i.e. average value based on the neighbours of 
# each municipality)

map_municipalities_with_data$splag_percent_vaccinated <- 
  spatial_weights %*% as.matrix(map_municipalities_with_data$percent_vaccinated)

