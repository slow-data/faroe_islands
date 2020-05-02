#===========================================
# @Project: Faroe Islands
# @Name: get_data
# @author: jprimav
# @date: 2020/05
#===========================================

rm(list=ls())

# load libraries
library(rgdal) # version 1.2.18
library(readr) # version 1.3.1
library(dplyr) # version 0.8.0.1

sessionInfo()

# R version 3.5.3 (2019-03-11)
# Platform: x86_64-w64-mingw32/x64 (64-bit)
# Running under: Windows >= 8 x64 (build 9200)

# Faroe island OpenStreetMap extract downloaded from http://glunimore.geofabrik.de/index.html and stored in the /shapes subfolder of the project.
# All needed shapes will be read with readOGR function and reprojected to Web Mercator (EPSG: 3857).

# Read buildings
gis_osm_buildings_a_free_1 <- readOGR(dsn = "../shapes/faroe-islands-latest-free.shp", layer = "gis_osm_buildings_a_free_1")
gis_osm_buildings_a_free_1 <- spTransform(gis_osm_buildings_a_free_1, CRS=CRS("+init=epsg:3857"))

# Read natural (areas)
gis_osm_natural_a_free_1 <- readOGR(dsn = "../shapes/faroe-islands-latest-free.shp", layer = "gis_osm_natural_a_free_1")
gis_osm_natural_a_free_1 <- spTransform(gis_osm_natural_a_free_1, CRS=CRS("+init=epsg:3857"))

# Extract beaches (areas)
beach_a <- gis_osm_natural_a_free_1[gis_osm_natural_a_free_1$fclass=="beach",]

# Read natural (points or lines)
gis_osm_natural_free_1 <- readOGR(dsn = "../shapes/faroe-islands-latest-free.shp", layer = "gis_osm_natural_free_1")
gis_osm_natural_free_1 <- spTransform(gis_osm_natural_free_1, CRS=CRS("+init=epsg:3857"))

# Extract beaches (points or lines)
beach <- gis_osm_natural_free_1[gis_osm_natural_free_1$fclass=="beach",]

# Read places (areas)
gis_osm_places_a_free_1 <- readOGR(dsn = "../shapes/faroe-islands-latest-free.shp", layer = "gis_osm_places_a_free_1")
gis_osm_places_a_free_1 <- spTransform(gis_osm_places_a_free_1, CRS=CRS("+init=epsg:3857"))

# I downloaded Wikipedia table with 18 main islands of Faroe and some information, stored in /data subfolder
# Read Wikipedia table
main_islands_wiki <- read_delim("../data/main_islands_wiki.csv", ";", 
                                escape_double = FALSE, trim_ws = TRUE)

# From wikipedia we see 18 main islands. In OSM places layer we have more than 18 polygons, 
# Let's see where/what these "extra" polygons are

asin <- gis_osm_places_a_free_1[gis_osm_places_a_free_1$osm_id==693808541,]
plot(gis_osm_places_a_free_1, col = "white")
plot(asin, col = "red", add=T) # can't see it...
plot(asin, col = "red") 
plot(gis_osm_places_a_free_1, col = "green", add=T) # probably included in other polygons

steyrur <- gis_osm_places_a_free_1[gis_osm_places_a_free_1$osm_id==772533423,]
plot(gis_osm_places_a_free_1, col = "white")
plot(steyrur, col = "red", add=T) # can't see them...
plot(steyrur, col = "red") 
plot(gis_osm_places_a_free_1, col = "green", add=T) # probably included in other islands

krunan1 <- gis_osm_places_a_free_1[gis_osm_places_a_free_1$osm_id==780606940,]
plot(gis_osm_places_a_free_1, col = "white")
plot(krunan1, col = "red", add=T) # can't see them...
plot(krunan1, col = "red") 
plot(gis_osm_places_a_free_1, col = "green", add=T) # probably included in other islands

krunan2 <- gis_osm_places_a_free_1[gis_osm_places_a_free_1$osm_id==780606941,]
plot(gis_osm_places_a_free_1, col = "white")
plot(krunan2, col = "red", add=T) # can't see them...
plot(krunan2, col = "red") 
plot(gis_osm_places_a_free_1, col = "green", add=T) # probably included in other islands

hasskoraberg <- gis_osm_places_a_free_1[gis_osm_places_a_free_1$osm_id==780606943,]
plot(gis_osm_places_a_free_1, col = "white")
plot(hasskoraberg, col = "red", add=T) # can't see them...
plot(hasskoraberg, col = "red") 
plot(gis_osm_places_a_free_1, col = "green", add=T) # probably included in other islands

flesin <- gis_osm_places_a_free_1[gis_osm_places_a_free_1$osm_id==782661526,]
plot(gis_osm_places_a_free_1, col = "white")
plot(flesin, col = "red", add=T) # can't see them...
plot(flesin, col = "red") 
plot(gis_osm_places_a_free_1, col = "green", add=T) # probably included in other islands

noname <- gis_osm_places_a_free_1[is.na(gis_osm_places_a_free_1$name),]
plot(gis_osm_places_a_free_1, col = "white")
plot(noname, col = "red", add=T) # can't see them...
plot(noname, col = "red") 
plot(gis_osm_places_a_free_1, col = "green", add=T) # probably included in other islands

# they all seem already included in the main 18 islands, so let's remove them
gis_osm_places_a_free_1 <- gis_osm_places_a_free_1[(!gis_osm_places_a_free_1$osm_id %in% c(693808541,
                                                                                         772533423,
                                                                                         780606940,
                                                                                         780606941,
                                                                                         780606943,
                                                                                         782661526)) &
                                                     (!is.na(gis_osm_places_a_free_1$name)),]


# Koltur island have two polygons, let's check if we can union them
Koltur1 <- gis_osm_places_a_free_1[gis_osm_places_a_free_1$osm_id==2066799,]
Koltur2 <- gis_osm_places_a_free_1[gis_osm_places_a_free_1$osm_id==19072581,]
plot(gis_osm_places_a_free_1, col = "white")
plot(Koltur1, col = "red", add=T)
plot(Koltur2, col = "red", add=T)
plot(Koltur1, col = "red")
plot(Koltur2, col = "green", add=T) # looks like a replication...

# Let's remove one duplication of Koltur
gis_osm_places_a_free_1 <- gis_osm_places_a_free_1[gis_osm_places_a_free_1$osm_id != 19072581,]

# Let's check now
nrow(gis_osm_places_a_free_1@data) # got the 18 main islands!
plot(gis_osm_places_a_free_1, col = "white") # looks good

# Add names without special characters in data so I can join with wiki table
names(gis_osm_places_a_free_1@data)[5] <- "name_spec_char"

gis_osm_places_a_free_1@data$name <- c("Nolsoy",
                                       "Hestur",
                                       "Koltur",
                                       "Mykines",
                                       "Stora Dimun",
                                       "Litla Dimun",
                                       "Svinoy",
                                       "Streymoy",
                                       "Vagar",
                                       "Suduroy",
                                       "Kalsoy",
                                       "Vidoy",
                                       "Fugloy",
                                       "Eysturoy",
                                       "Sandoy",
                                       "Skuvoy",
                                       "Kunoy",
                                       "Bordoy")

# Join with wiki table
gis_osm_places_a_free_1@data <- dplyr::left_join(gis_osm_places_a_free_1@data, main_islands_wiki)

# Count buildings in each island
building_loc <- over(gis_osm_buildings_a_free_1, gis_osm_places_a_free_1, fn = NULL)
building_cnt <- as.data.frame(table(as.character(building_loc$osm_id)))
names(building_cnt) <- c("osm_id", "building_freq")
building_cnt$osm_id <- as.character(building_cnt$osm_id)
gis_osm_places_a_free_1$osm_id <- as.character(gis_osm_places_a_free_1$osm_id)
gis_osm_places_a_free_1@data <- dplyr::left_join(gis_osm_places_a_free_1@data, building_cnt)
gis_osm_places_a_free_1$osm_id <- as.factor(gis_osm_places_a_free_1$osm_id)
gis_osm_places_a_free_1$building_freq <- coalesce(gis_osm_places_a_free_1$building_freq, 0L)
gis_osm_places_a_free_1@data$building_density <- gis_osm_places_a_free_1$building_freq/gis_osm_places_a_free_1$area_km2

# count beaches in each island
beach_loc <- over(beach, gis_osm_places_a_free_1, fn = NULL)
beach_a_loc <- over(beach_a, gis_osm_places_a_free_1, fn = NULL) 
beach_cnt <- as.data.frame(table(c(as.character(beach_loc$osm_id), as.character(beach_a_loc$osm_id))))
names(beach_cnt) <- c("osm_id", "beach_freq")
beach_cnt$osm_id <- as.character(beach_cnt$osm_id)
gis_osm_places_a_free_1$osm_id <- as.character(gis_osm_places_a_free_1$osm_id)
gis_osm_places_a_free_1@data <- dplyr::left_join(gis_osm_places_a_free_1@data, beach_cnt)
gis_osm_places_a_free_1$osm_id <- as.factor(gis_osm_places_a_free_1$osm_id)
gis_osm_places_a_free_1$beach_freq <- coalesce(gis_osm_places_a_free_1$beach_freq, 0L)
gis_osm_places_a_free_1@data$beach_density <- gis_osm_places_a_free_1$beach_freq/gis_osm_places_a_free_1$area_km2

writeOGR(gis_osm_places_a_free_1, "../shapes/main18_islands", "main18_islands", driver="ESRI Shapefile")
