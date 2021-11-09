# 
# BAN400 - R PROGRAMMING FOR DATA SCIENCE
# LECTURE: MAKING MAPS
# 

# In this lecture we will look at various techniques for making maps in R. We
# will start straight away by looking at some of the complicating factors that
# may arise when working with this kind of data, and then go into some simpler
# examples after that, to show that it does not always have to be all that
# complicated.

# EXAMPLE 1: The 1854 cholera outbreak ---------
#
# This example is presented in "Modern Data Science with R" by Baumer, Kaplan
# and Horton. The sp and rgdal-packages have been replaced by the sf-package.
library(mdsr)        # Companion R package to the book, containing data
library(sf)          # For spatial data, co-authoured by NHH Prof. Roger Bivand 
library(ggmap)       # For drawing static maps as ggplots
library(tidyverse)   # Data wrangling etc
plot(CholeraDeaths)  # Simple plot of the data

# When working with spatial data we typically need to deal with shape-files.
# These files are more complicated than what we are used to, and we need
# specialized functions to read them. Administrative units (countries, regions
# etc) typically publish shapefiles for their borders. This is not a simple
# format though.

# Download the file "rtwilson.com/downloads/SnowGIS_SHP.zip" and extract to your
# working directory. This is the cholera data as shapefiles:
folder <- "SnowGIS_SHP/SnowGIS_SHP"     # The folder containing the shapefiles.
list.files(folder)                      # List the files in the folders
st_layers(folder)                       # List the "layers" in this folder (sets 
                                        # of shapefiles).

# We will begin by loading in the "Cholera_Deaths"-layer
ColeraDeaths <- read_sf(folder, layer = "Cholera_Deaths")
summary(ColeraDeaths)

# We have 250 spatial points, and for each point we have an ID-column (that
# seems to only contain zeros) as well as a Count column that contains the
# number of victims at that address. The Geomety column contains the definitions
# of the points, but this could also be the circumference of a country for
# example.

# Extract the coordinates of the points and plot in a coordinate system:
cholera_coords <- as_tibble(st_coordinates(ColeraDeaths))
ggplot(cholera_coords) +
  geom_point(aes(x = X, y = Y)) +
  coord_quickmap()

# This is still just a very basic plot of the locations of the points in our
# data set on a coordinate system with some sort of logintude and latitude on
# the axes. We want to overlay this on a map. We can download this map as a
# ggplot using the ggmap-package. In order to do that, we need to specify the
# square that we want to download, but note that the coordinates in the data is
# not in the lat/lon-format:
cholera_coords

# The unit here is meters, and not the usual degree/minute/second format. We
# must transform the coordinates, but from what? We don't always know, because
# there are many possible projections (read
# https://en.wikipedia.org/wiki/Map_projection if you want, but this is a very
# deep rabbit hole). 

# The book that presents this example lists three commonly used projections:
# - EPSG:4326   (Also known as WGS84), standard for GPS systems and Google earth.
# - EPSG:3857   Used by OpenStreetMap, Google Maps and other services.
# - EPSG:27700  Often used in Britain.

# The map-tiles that we will download later however come in the EPSG:4326-format,
# so let us try to convert the cholera data to that format:
cholera_latlon <- 
  CholeraDeaths %>% 
  st_transform(st_crs("+init=epsg:4326")) %>% 
  st_coordinates %>% 
  as_tibble %>% 
  mutate(Count = ColeraDeaths$Count)     # Add the counts back in to the data

# Define the box and download map:
london <- make_bbox(cholera_latlon$X, 
                    cholera_latlon$Y, 
                    f = .05)
m <- get_map(london, zoom = 17, source = "osm")
ggmap(m)

# Add the cholera data to the map:
ggmap(m) +
  geom_point(aes(x = X, y = Y, size = Count), data = as_tibble(cholera_latlon))

# The points are a little bit off, unfortunately, and this is because we have
# not figured out what kind of coordinate system the original data has. In fact,
# in this case the data set does not come with information about which
# coordinate system was used to encode this data. We have information that this
# in fact is in the EPSG:27700-format, but this is something that we have to
# figure our from the source every time. 

# Check the system; no info unfortunately
st_crs(CholeraDeaths)$input

# Set coordinate system fisrst, then transform and plot.
cholera_latlon <- 
  CholeraDeaths %>% 
  st_set_crs("+init=epsg:27700") %>% 
  st_transform(st_crs("+init=epsg:4326")) %>% 
  st_coordinates %>% 
  as_tibble %>% 
  mutate(Count = ColeraDeaths$Count)

ggmap(m) +
  geom_point(aes(x = X, y = Y, size = Count), data = cholera_latlon)

# Voila!

# EXAMPLE 2: Water sources in Africa --------------

# These packages contains shapefiles for the countries of the worls
library(rnaturalearth)
library(rnaturalearthdata)
library(rgeos)

water <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-05-04/water.csv")
world <- ne_countries(scale = "medium", returnclass = "sf")

tanzania <- world[world$name == "Tanzania",]

tan_data <- 
  water %>% 
  filter(country_name == "Tanzania") %>% 
  filter(lat_deg < 60)                   # Some weird data points

# Plot the country
ggplot(st_geometry(tanzania)) +
  geom_sf()

# Add the water sources
ggplot(st_geometry(tanzania)) +
  geom_sf() +
  geom_point(aes(x = lon_deg, y = lat_deg), 
             data = tan_data) 

# Make a nice map
library(firatheme)                # My favourite ggplot theme: https://github.com/vankesteren/firatheme
ggplot(st_geometry(tanzania)) +
  geom_sf(fill = "#00000050") +
  geom_point(aes(x = lon_deg, y = lat_deg, colour = water_source), 
             data = tan_data) +
  xlab("") +
  ylab("") +
  labs(colour = "") +
  ggtitle("Water sources in Tanzania") +
  theme_fira() +
  scale_colour_fira(na.value = "darkred") +
  theme(axis.line = element_blank(),
        panel.grid.major = element_line(colour = "#00000020", 
                                        inherit.blank = FALSE),
        axis.text = element_text(colour = "#00000050"))

# Downloaded shapefiles for other features of Tanzania here:
# https://mapcruzin.com/free-tanzania-arcgis-maps-shapefiles.htm
# (Just made a Google search for Tanzania shapefiles)

tan_roads <- read_sf("tanzania_roads")
tan_cities <- 
  read_sf("tanzania_places") %>% 
  filter(type == "city") %>% 
  select(name, geometry) %>% 
  drop_na() %>% 
  mutate(X = st_coordinates(.$geometry)[,1]) %>% 
  mutate(Y = st_coordinates(.$geometry)[,2])
tan_waterways <- read_sf("tanzania_waterways")

ggplot(st_geometry(tanzania)) +
  geom_sf(fill = "#00000050") +
  geom_point(aes(x = lon_deg, y = lat_deg, colour = water_source), 
             data = tan_data) +
  xlab("") +
  ylab("") +
  labs(colour = "") +
  ggtitle("Water sources in Tanzania") +
  theme_fira() +
  scale_colour_fira(na.value = "darkred") +
  theme(axis.line = element_blank(),
        panel.grid.major = element_line(colour = "#00000020", 
                                        inherit.blank = FALSE),
        axis.text = element_text(colour = "#00000050")) +
  geom_sf(data = st_geometry(tan_roads), colour = "#00000050") +
  geom_point(aes(x = X, y = Y), data = tan_cities, size = 2) +
  geom_text(aes(x = X, y = Y, label = name), 
            data = tan_cities, 
            colour = "#999999", 
            position = "right")

# EXAMPLE 3: Water sources in Africa, continued ------------


