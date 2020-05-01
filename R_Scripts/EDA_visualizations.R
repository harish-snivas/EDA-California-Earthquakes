library(tidyverse)
library(dplyr)
library(sf)
library(tmap)
library(ggplot2)
library(mapview)

#Start.....Go to step 12.

#Step 1 - Importing datset into the project
#setwd("./dataset")
#file_list <- list.files()
#earthquakes <- map_dfr(file_list,read_csv)
#saveRDS(earthquakes, "earthquakes.rds")

#Step 2 - earthquake data fro west coast over last 40 years
earthquakes <- readRDS("dataset/earthquakes.rds")

#Step 3 - surveying data to look for patterns of interest for california
places <- unique(tolower(earthquakes$place))

#Step 4 - pattterns in data to filter out california records in the data
patterns <- c(", ca", "california")


#Step 5 - filtered dataset with only earthquakes in the state of california
california_earthquakes <- filter(earthquakes, grepl(paste(patterns, collapse="|"), tolower(place)))

#Step 6 - old earthquakes dataset is no longer required.
rm(earthquakes)

#Great	8 or more
#Major	7 - 7.9
#Strong	6 - 6.9
#Moderate	5 - 5.9
#Light	4 - 4.9
#Minor	3 -3.9
#Low 2.5 - 3

#Step 9 - binning magnitudes based on severity
breaks <- c(2.5,3,4,5,6,7,8,50)
tags <- rev(c("Great (8+)", "Major (7-8)", "Strong (6-7)", "Moderate (5-6)", "Light (4-5)", "Minor (3-4)", "Low (2.5-3)"))

california_earthquakes <- mutate(california_earthquakes, severity = cut(california_earthquakes$mag, breaks = breaks, include.lowest = TRUE, right = TRUE, labels = tags),
                                               year = format(time, format = "%Y"))

#california_earthquakes <- read_csv("california_earthquakes.csv")

#Step 10 - summarizing based on count in each bin
summary1 <- california_earthquakes %>%
              group_by(year, severity) %>%
                summarize(count = n())

#Step 11 - graphical bar plot for magnitude-frequency of earthquakes.
ggplot(california_earthquakes, aes(x = severity)) +
  geom_bar(fill="bisque",color="grey",alpha=0.3) +
  stat_count(geom="text", aes(label=stat(count))) +
  labs(x='Severity', y = "Frequency") +
  theme_minimal() + coord_flip()


#Step 12 - reading in shape file for california. Skipping next steps.... Go to Step 17 after this.
california_map <- st_read("./CA_Counties/CA_Counties_TIGER2016.shp")

#us_zip_codes <- st_read("./cb_2015_us_zcta510_500k/cb_2015_us_zcta510_500k.shp")

#Step 13 - The file containing county names for each latitude and longitude in the dataset
coords<- read_csv("coords.csv")

#Step 14 - Next few steps involves some data cleaning to get unique county names for each lat, long
coords <- coords %>%
      filter(friendly_type == "county")

coords <- coords %>%
            group_by(latitude, longitude, name) %>%
              summarize(count = n())

counties <- california_map$NAME

coords <- coords %>%
            filter((name %in% counties))


#Step 15 - joining the two tables based on latitude and longitude.
california_earthquakes <- inner_join(california_earthquakes, coords, by = c("latitude", "longitude") )%>%
                                select(.,-"count")

#Step 16 - writing the result to a csv file
write_csv(california_earthquakes, "california_earthquakes.csv")

#Step 17 - reading the cleaned data set
california_earthquakes <- read_csv("california_earthquakes.csv")

#Steo 18 - summarizing earthquakes by county in california
summary2 <- california_earthquakes %>%
              group_by(name) %>%
                summarize(count = n(), mag_max = max(mag))

#Step 19 - Join the california_map df to summary2 to tag the values to the county
california_map <- right_join(summary2, california_map, by = c("name" = "NAME"))

#Step 20 - Convert the joined df to sf object to plot it.
california_map <- st_sf(california_map)

plot(st_geometry(california_map))

summary3 <- california_earthquakes %>%
  group_by(name) %>%
  mutate(mag_max = max(mag)) %>%
  filter(mag %in% mag_max)

#summary3 <- st_as_sf(x = summary3, coords = c("longitude", "latitude"),crs = st_crs(california_map))

#Step 21 - Plotting choropleths of magnitude and frequency 
tm_shape(california_map) + tm_fill(col = "mag_max")
tm_shape(california_map) + tm_fill(col = "count")

#Step 22 - Cholorplet of frequency distribution and scatter plot of maximum magnitude in county
mymap <- mapview(california_map, 
                 zcol = "count", 
                 homebutton = FALSE) +
        mapview(summary3, 
          zcol = "mag_max", 
          legend = TRUE, 
          alpha = 0.5, cex = 3, 
          col.regions = "orange",
          homebutton = FALSE,
          xcol = "longitude",
          ycol = "latitude") 


#Step 23 - loading mapdeck for barplots in 3d
library(mapdeck)

set_token(Sys.getenv("MAPBOX"))
key = "your-key-here"

mapdeck(token = key)


#Step 24 - basic interactive plot in 3d
mapdeck( token = key, style = mapdeck_style("dark"), pitch = 45 ) %>%
  add_grid(
    data = california_earthquakes
    , lat = "latitude"
    , lon = "longitude"
    , cell_size = 5000
    , elevation_scale = 50
    , layer_id = "grid_layer"
  )


#Step 25 - Frwequncy of each earthquake by county add severity. 
mapdeck(token = key, style = mapdeck_style("dark")) %>%
  add_polygon(
    data = st_transform(california_map, crs = 4326),
    polyline = NULL,
    layer = "polygon_layer",
    fill_colour = "name",
    elevation = "count",
    legend = T
  )
