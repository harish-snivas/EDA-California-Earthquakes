library(tidyverse)
library(dplyr)
library(sf)
library(ggplot2)
library(rayshader)
library(sp)
library(maps)
library(maptools)

#Importing datset into the project
#setwd("./dataset")
#file_list <- list.files()
#earthquakes <- map_dfr(file_list,read_csv)
#saveRDS(earthquakes, "earthquakes.rds")

#earthquake data fro west coast over last 40 years
earthquakes <- readRDS("dataset/earthquakes.rds")

# surveying data to look for patterns of interest for california
places <- unique(tolower(earthquakes$place))

#pattterns in data to filter out california records in the data
patterns <- c(", ca", "california")


#filtered dataset with only earthquakes in the state of california
california_earthquakes <- filter(earthquakes, grepl(paste(patterns, collapse="|"), tolower(place)))

#old earthquakes dataset is no longer required.
rm(earthquakes)

#Great	8 or more
#Major	7 - 7.9
#Strong	6 - 6.9
#Moderate	5 - 5.9
#Light	4 - 4.9
#Minor	3 -3.9
#Low 2.5 - 3

#binning magnitudes based on severity
breaks <- c(2.5,3,4,5,6,7,8,50)
tags <- rev(c("Great (8+)", "Major (7-8)", "Strong (6-7)", "Moderate (5-6)", "Light (4-5)", "Minor (3-4)", "Low (2.5-3)"))

california_earthquakes <- mutate(california_earthquakes, severity = cut(california_earthquakes$mag, breaks = breaks, include.lowest = TRUE, right = TRUE, labels = tags),
                                                year = format(time, format = "%Y"))
#summarizing based on count in each bin
summary1 <- california_earthquakes %>%
              group_by(year, severity) %>%
                summarize(count = n())

#graphical bar plot for magnitude-frequency of earthquakes.
ggplot(california_earthquakes, aes(x = severity)) +
  geom_bar(fill="bisque",color="grey",alpha=0.3) +
  stat_count(geom="text", aes(label=stat(count))) +
  labs(x='Severity', y = "Frequency") +
  theme_minimal() + coord_flip()


# reading in shape file for california
california_map <- st_read("./CA_Counties/CA_Counties_TIGER2016.shp")


# latlong2fips <- function(latitude, longitude) {
#   url <-  "https://geo.fcc.gov/api/census/block/find?format=json&latitude=%f&longitude=%f"
#   #url <- "https://geo.fcc.gov/api/census/area?lat=%f&lon=%f&format=json"
#   url <- sprintf(url, latitude, longitude)
#   json <- RCurl::getURL(url)
#   json <- RJSONIO::fromJSON(json)
#   #as.character(json$County['FIPS'])
#   return(json$County["FIPS"])
# }

# json <- latlong2fips(36.3855, -121.5905)
# json

############Dont run beyond this, under construction ###################
#sample_dataset <- california_earthquakes[1:1000, ]

# FIPS <- data.frame(FIPS = mapply(latlong2fips, sample_dataset$latitude, sample_dataset$longitude))
# 
# sample_dataset["GEOID"] <- FIPS
# 
# combined_data <- left_join(sample_dataset, california_map, by = "GEOID")
# 
# combined_data <- st_sf(combined_data)
# 
# ggplot(combined_data) +
#   geom_sf(aes(fill = mag))




# The single argument to this function, pointsDF, is a data.frame in which:
#   - column 1 contains the longitude in degrees (negative in the US)
#   - column 2 contains the latitude in degrees

latlong2county <- function(pointsDF) {
  # Prepare SpatialPolygons object with one SpatialPolygon
  # per county
  counties <- map('county', fill=TRUE, col="transparent", plot=FALSE)
  IDs <- sapply(strsplit(counties$names, ":"), function(x) x[1])
  counties_sp <- map2SpatialPolygons(counties, IDs=IDs,
                                     proj4string=CRS("+proj=longlat +datum=WGS84"))
  
  # Convert pointsDF to a SpatialPoints object 
  pointsSP <- SpatialPoints(pointsDF, 
                            proj4string=CRS("+proj=longlat +datum=WGS84"))
  
  # Use 'over' to get _indices_ of the Polygons object containing each point 
  indices <- over(pointsSP, counties_sp)
  
  # Return the county names of the Polygons object containing each point
  countyNames <- sapply(counties_sp@polygons, function(x) x@ID)
  countyNames[indices]
}

latlong2county(data.frame(x = c(-121.5905), y = c(36.38550)))


df_counties <- data.frame(county = latlong2county(data.frame(x = california_earthquakes$longitude, y = california_earthquakes$latitude)))

df_counties <- data.frame(lapply(df_counties, as.character), stringsAsFactors=FALSE)
df_counties$ID <- seq.int(nrow(df_counties))

california_earthquakes$counties <- df_counties$county

california_map <- california_map %>%
                    mutate(counties = paste('california', tolower(NAME), sep = ","))

california_earthquakes <- read_csv("california_earthquakes.csv")

summary2 <- california_earthquakes %>%
  group_by(name) %>%
  summarize(count = n(), mag_max = max(mag))

california_map <- right_join(summary2, california_map, by = c("name" = "NAME"))

california_map <- st_sf(california_map)

ggplot(california_map) +
     geom_sf(aes(fill = count))

gg_ca <- ggplot(california_map) +
  geom_sf(aes(fill = count)) +
  scale_fill_viridis_c("Frequency") +
  ggtitle("Frequency of earthquakes by county") +
  theme_bw()

plot_gg(gg_ca, multicore = TRUE, width = 6 ,height=2.7, fov = 70)
render_depth(focallength=100,focus=0.72)


