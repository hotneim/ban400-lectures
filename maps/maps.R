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

london <- make_bbox(cholera_latlon$X, 
                    cholera_latlon$Y, 
                    f = .05)
m <- get_map(london, zoom = 17, source = "osm")

ggmap(m) +
  geom_point(aes(x = X, y = Y, size = Count), data = cholera_latlon)

# Voila!

# EXAMPLE 2: Water sources in Africa --------------

# These packages contains shapefiles for the countries of the world (as well as a lot of other information about the countries)
library(rnaturalearth)
library(rnaturalearthdata)
library(rgeos)

# This is a data set that is published on the "Tidy Tuesday"-project, a Github
# repository that publishes a new data set every week for the online data
# science community to analyze. This data set contains information on water sources in the world, but mainly in Africa.
water <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-05-04/water.csv")

# Here, we make a data frame for all the countries of the world. This requires all of the three packages above to be loaded.
world <- ne_countries(scale = "medium", returnclass = "sf")

# Let us concentrate on just one country, we filter out all other
tanzania <- world[world$name == "Tanzania",]

# We do the same for the water sources data. When making the plots later we will see that there are a couple of strange data points with latitude > 60. These are obviously not in Tanzania, so  just filter them out.
tan_data <- 
  water %>% 
  filter(country_name == "Tanzania") %>% 
  filter(lat_deg < 60)                   # Some weird data points

# We make a simple plot of the borders of the country using the plotting method geom_sf() that is especially built for these kind of shape-objects. 
ggplot(st_geometry(tanzania)) +
  geom_sf()

# Add the water sources in the normal way. The coordinate systems appears to be
# compatible in this case, and the figure is in any case not so sensitive to
# such discrepancies when we are working on such a large area, and not on the
# street level as we did above.
ggplot(st_geometry(tanzania)) +
  geom_sf() +
  geom_point(aes(x = lon_deg, y = lat_deg), 
             data = tan_data) 

# Make a nice map. I have used the fira-theme, becuase it is very nice. You need
# to install this yourself by following the link below, and that also requires
# the installation of a font. I you want to use another theme, replace (or take
# out) theme_fira() and scale_color_fira() below.
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
# (Just made a Google search for Tanzania shapefiles). Read them in in the same way as for the London-example. The places-file includes a column on type, so we filter it down to just the major cities. We make the coordinates as separate columns and plot these "manually" so that we can plot the names of the cities as well as indicating their positions:
tan_roads <- read_sf("tanzania_roads")
tan_cities <- 
  read_sf("tanzania_places") %>% 
  filter(type == "city") %>% 
  select(name, geometry) %>% 
  drop_na() %>% 
  mutate(X = st_coordinates(.$geometry)[,1]) %>% 
  mutate(Y = st_coordinates(.$geometry)[,2])
tan_waterways <- read_sf("tanzania_waterways")

# We add the roads, waterways and cities to the map. Takes quite a lot of time
# to create, but the result is really nice!
ggplot(st_geometry(tanzania)) +
  geom_sf(fill = "#00000050") +
  geom_point(aes(x = lon_deg, y = lat_deg, colour = water_source), 
             data = tan_data,
             alpha = .5) +
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
            nudge_y = 0.3) +
  geom_sf(data = st_geometry(tan_waterways), colour = "darkblue", alpha = .3)

# EXAMPLE 3: Water sources in Africa, continued ------------

# Another important type of plots is to color countries according to some
# variable. We have all seen this kind of plots in the pandemic.

# We start by filtering the "world" data frame above to include the countries of
# Africa, and the information about this in contained in the "continent"
# variable. We need to join the data on the geometry of the countries with the
# water data later, so we need the country names to match exactly. A bit further
# down we have an anti_join() for finding mismatches, and we fix them here with
# a recode:
africa <- 
  world %>% 
  filter(continent == "Africa") %>% 
  select(admin, geometry) %>% 
  mutate(admin = recode(.$admin, 
                        "Democratic Republic of the Congo" = "Congo - Kinshasa",
                        "Republic of Congo" = "Congo - Brazzaville",
                        "United Republic of Tanzania" = "Tanzania")) %>% 
  rename(country_name = admin)

# We make a basic plot of the countries and see that it looks reasonable:
ggplot(africa, aes(geometry = geometry)) +
  geom_sf() 

# There is a binary variable in the water source data that indicates whether
# there is water available at the time of the visit. Let us calculate the share
# of active water sources for each country:
water_share <- 
  water %>% 
  group_by(country_name) %>% 
  summarise(share_status = mean(status_id == "y")) 

# We use the anti_jon to chech which countries in the water-data that does not
# have matches in the africa data. Identifies that the two Congos and Tanzania
# were spelled differently, fixed that above when filtering down the africa
# data.
anti_join(water_share, africa)

# We join, first a full join to get both the water data and the geometry data in the dame column, and then a semi-join to filter out all countries that are not in the Africa data (there are a few non-African countries in the water data).
water_africa <- 
  full_join(water_share, africa) %>% 
  semi_join(africa) 

# Basic plot, where we map the fill-aesthetic to the share-variable:
ggplot(water_africa) +
  geom_sf(aes(geometry = geometry, fill = share_status)) 

# Make a prettier plot. Again, drop the firatheme if you have not bothered to
# install it.
ggplot(water_africa) +
  geom_sf(aes(geometry = geometry, fill = share_status)) +
  scale_fill_gradient(
    low = "white",
    high = "red") +
  ggtitle("Share of active water sources") +
  labs(fill = "") +
  theme_fira() +
  scale_colour_fira(na.value = "darkred") +
  theme(axis.line = element_blank(),
        panel.grid.major = element_line(colour = "#00000020", 
                                        inherit.blank = FALSE),
        axis.text = element_text(colour = "#00000050"))

