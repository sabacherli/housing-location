
# ------------------------------------------------------------------------------
# SET UP
# ------------------------------------------------------------------------------

# Clean the environment
rm(list = ls())

# Create a function to install missing packages and load all required packages
ipack <- function(pack) {
  create.pkg <- pack[!(pack %in% installed.packages()[, "Package"])]
  if (length(create.pkg))
    install.packages(create.pkg, dependencies = TRUE)
  sapply(pack, require, character.only = TRUE)
}

# Specifcy packages and install and load them
# [loaded in reverse order, meaning functions from early packages may be masked by functions from latter packages]
packages <- c("pryr", "tictoc",  # profiling
              "smoothr", "pastecs", "Rfast", "data.table",  # miscellaneous
              "osmdata", "rgdal", "tmap", "sf", "tidyverse")  # fundamentals
ipack(packages)

mem_used()


# ------------------------------------------------------------------------------
# EXPLORING THE DATA SET
# ------------------------------------------------------------------------------

# Import the data and get an overview
df1 <- fread("/Users/samu_hugo/Desktop/Code/Data/prices.csv") %>% 
  glimpse()

# Select and rename relevant variables
df2 <- df1 %>% 
  select(address = ADDRESS,
         neighbourhood = NEIGHBORHOOD,
         borough = BOROUGH,
         block = BLOCK,
         lot = LOT,
         zip = `ZIP CODE`,
         residential = `RESIDENTIAL UNITS`,
         commercial = `COMMERCIAL UNITS`,
         units = `TOTAL UNITS`,
         tax = `TAX CLASS AT TIME OF SALE`,
         building_code = `BUILDING CLASS AT TIME OF SALE`,
         sqft = `GROSS SQUARE FEET`,
         built = `YEAR BUILT`,
         price = `SALE PRICE`,
         sale = `SALE DATE`,
         long = Longitude,
         lat = Latitude) %>% 
  glimpse()

# Change classes of variables and calculate the age of buildings
df3 <- df2 %>% 
  mutate(price = as.numeric(price),
         borough = as.factor(borough),
         neighbourhood = as.factor(neighbourhood),
         tax = as.factor(tax),
         zip = as.factor(zip),
         lot = as.factor(lot),
         block = as.factor(block),
         sqft = as.numeric(gsub(",", "", sqft)),
         year = as.numeric(substr(sale, 7, 10)),
         age = 2019 - built)

# Check variables for measurement error and bad data quality
df3 %>% 
  summary()

# Filter out bad data
df4 <- df3 %>% 
  filter(!is.na(long),
         !is.na(lat),
         !is.na(sqft),
         !is.na(residential),
         sqft != 0,
         price != 0,
         built != 0)

# Only take residential properties transacted in 2018 into account
df5 <- df4 %>% 
  filter(year == 2018,
         tax %in% c(1,2),
         commercial == 0)

# Import the building class category descriptions
df.dictionary <- fread("/Users/samu_hugo/Desktop/Code/Data/dictionary.csv") %>% 
  glimpse()

df.dictionary <- df.dictionary %>% 
  mutate(building_code = `Building Code`,
         building_type = as.factor(Description)) %>%
  select(-c(`Building Code`, Description)) %>% 
  glimpse()

# Joing the description to the building codes
df6 <- df5 %>% 
  left_join(df.dictionary, by = c("building_code")) %>% 
  mutate(building_code = as.factor(building_code))

# Check number of observations per building category left
df6 %>% 
  group_by(building_code) %>% 
  count(building_type) %>% 
  arrange(desc(n)) %>% 
  View("Building Types")

# Create overarching building categories and filter out garages
df7 <- df6 %>% 
  mutate(building_class = as.factor(substr(building_code, 0, 1))) %>% 
  filter(building_class != "G")

# Check number of buildings with multiple apartments
df7 %>% 
  count(residential) %>% 
  arrange(desc(n)) %>%
  View("Number of buildings")

# Check for observations where number of residential units still doesn't match the total units
df7 %>% 
  filter(residential != units) %>% 
  arrange(residential) %>% 
  View("Number of units")

# Filter out buildings where units and residential doesn't match, convert square
# feet to square metres and calculate price per sqm, price per m2, and price per unit
df8 <- df7 %>% 
  filter(residential == units) %>% 
  mutate(m2 = sqft / 10.764,
         price_m2 = price / m2,
         price_sqft = price / sqft,
         price_unit = price / units)

# Check if the price distribution makes sense
summary(df8$price_m2)
pdf("plot-prices-before.pdf") 
plot(density(df8$price_m2, bw = "sj"), main = "Price per square metre")  # plot empirical probability density function with sj smoothing bandwidth

# Filter out observations before the first local minimum deemed to be private transactions aimed at reducing property taxes
d <- density(df8$price_m2, bw = "sj")  # calculate the density
ts_y <- ts(d$y)  # make it a time series
tp <- turnpoints(ts_y)  # calculate turning points (extrema)
points(d$x[tp$tppos], d$y[tp$tppos], col = "red")  # plot turning points
dev.off()
df9 <- df8 %>% 
  filter(price_m2 > d$x[tp$tppos][1])

# Check price distribution again
summary(df9$price_m2)
pdf("plot-prices-after.pdf")
plot(density(df9$price_m2, bw = "sj"), main = "Price per square metre")
dev.off()

# Check if the most expensive properties are reasonable
df9 %>% 
  top_n(25, price_m2) %>% 
  arrange(desc(price_m2)) %>% 
  View("Expensive properties")

# Create an sf object and change to a equidistant projection with metres as units
sf.nyc <- df9 %>% 
  st_as_sf(coords = c("long", "lat"), crs = 4326) %>% 
  st_transform(32118)

# Import NYC boroughs and change to a equidistant projection with metres as units
sf.boroughs <- st_as_sf(readOGR(dsn = "/Users/samu_hugo/Desktop/Code/Data/boroughs/",
                                layer = "geo_export_a814b278-dd58-459f-b871-28e2dd794f22"),
                        crs = 4326) %>% 
  st_transform(32118)

# Check the spatial distribution of the prices (~25sec)
tic("Mapping")
  tmap_mode("plot")
  pdf("map-prices.pdf")
  tm_shape(sf.boroughs) + 
    tm_polygons(alpha = 0.1, border.alpha = 0.4) +
    tm_layout(title = "2018 Property Prices, NYC", title.position = c("left", "top"),
              frame = F) + 
    tm_legend(title.size = 1.2, text.size = 0.8, position = c(0.05, 0.7)) + 
    tm_shape(sf.nyc) +
    tm_dots(size = 0.1, col = "price_m2", title = "Price per m2", alpha = 0.4, 
            style = "quantile", border.lwd = NA, border.col = NA,
            palette = viridis::magma(n = 20, 
                                     begin = 0.1, 
                                     end = 0.9, 
                                     direction = -1))
toc()

# Clean up environment
rm(list = setdiff(ls(), c("sf.nyc", "sf.boroughs")))

# Create buffers around each observation
sf.nyc_400 <- st_buffer(sf.nyc, 400)
sf.nyc_800 <- st_buffer(sf.nyc, 800)
sf.nyc_2000 <- st_buffer(sf.nyc, 2000)


# ------------------------------------------------------------------------------
# EDUCATION DATA 
# ------------------------------------------------------------------------------

# Import SAT scores and school directory
df.sat <- fread("/Users/samu_hugo/Desktop/Code/Data/education.csv")
df.schools <- fread("/Users/samu_hugo/Desktop/Code/Data/schools.csv")

# Get an overview of the SAT data set
df.sat %>% 
  glimpse()

# Rename variables and filter out schools without quality indicators
df.sat2 <- df.sat %>% 
  select(id = DBN,
         school = `SCHOOL NAME`, 
         n = `Num of SAT Test Takers`, 
         reading = `SAT Critical Reading Avg. Score`, 
         math = `SAT Math Avg. Score`, 
         writing = `SAT Writing Avg. Score`) %>% 
  filter(n != "s") %>% 
  mutate(n = as.numeric(n),
         math = as.numeric(math),
         reading = as.numeric(reading),
         writing = as.numeric(writing))

# Get an overview of the school data set
df.schools %>% 
  glimpse()

# Select and rename the schools
df.schools2 <- df.schools %>% 
  select(id = `ATS SYSTEM CODE`,
         location_string = `Location 1`,
         school_type = LOCATION_CATEGORY_DESCRIPTION)

# Create an sf object with geocoded school locations
sf.schools <- df.schools2 %>% 
  mutate(coordinates = str_match(location_string, "\\((.*?)\\)")[,2]) %>% 
  mutate(X = as.numeric(substr(coordinates, 1, regexpr(",", coordinates) - 1))) %>% 
  mutate(Y = as.numeric(substr(coordinates, regexpr(",", coordinates) + 1, nchar(coordinates)))) %>% 
  filter(!is.na(X) & !is.na(Y)) %>% 
  st_as_sf(coords = c("Y", "X"), crs = 4326) %>% 
  st_transform(32118) %>% 
  select(-c(location_string, coordinates))

# Join SAT scores and schools data set
sf.school_quality <- sf.schools %>% 
  inner_join(df.sat2, by = "id")

# Calculate distance to closest high school
sf.nyc$d_highschool <- st_distance(sf.nyc, st_combine(sf.school_quality)) %>% 
  as.numeric() %>% 
  `/`(100)
sf.nyc$d_highschool_unit <- "100m"

# Join appropriate school and quality indicators (~1min)
tic("Add school quality")
  index <- st_distance(sf.nyc, sf.school_quality) %>% 
    as.data.frame() %>% 
    mutate_all(function(x) as.numeric(as.character(x))) %>% 
    as.matrix() %>% 
    rowMins(value = F)
  sf.nyc <- sf:::cbind.sf(sf.nyc, sf.school_quality[index, ] %>% 
                            as.data.frame() %>% 
                            select(-c(id, geometry)))
toc()

# Plot high schools only
tmap_mode("plot")
pdf("map-highschools.pdf")
tm_shape(sf.boroughs) + 
  tm_polygons(alpha = 0.1, border.alpha = 0.4) +
  tm_layout(title = "High schools, NYC", title.position = c("left", "top"),
            frame = F) + 
  tm_legend(title.size = 1.2, text.size = 0.8, position = c(0.05, 0.7)) + 
  tm_shape(sf.nyc) +
  tm_dots(size = 0.1, col = "price_m2", title = "Price per square metre", alpha = 0.4, 
          style = "quantile", border.lwd = NA, border.col = NA,
          palette = viridis::magma(n = 20, 
                                   begin = 0.1, 
                                   end = 0.9, 
                                   direction = -1)) +
  tm_shape(sf.school_quality) + 
  tm_dots(size = 0.1, col = "blue")
dev.off
# Plotting elementary schools only
pdf("map-elementary.pdf")
tm_shape(sf.boroughs) + 
  tm_polygons(alpha = 0.1, border.alpha = 0.4) + 
  tm_layout(title = "Elementary schools, NYC", title.position = c("left", "top"),
            frame = F) + 
  tm_legend(title.size = 1.2, text.size = 0.8, position = c(0.05, 0.7)) + 
  tm_shape(sf.nyc) +
  tm_dots(size = 0.1, col = "price_m2", title = "Price per square metre", alpha = 0.4, 
          style = "quantile", border.lwd = NA, border.col = NA,
          palette = viridis::magma(n = 20, 
                                   begin = 0.1, 
                                   end = 0.9, 
                                   direction = -1)) +
  tm_shape(sf.schools %>% filter(school_type == "Elementary")) + 
  tm_dots(size = 0.1, col = "green")
dev.off()


# ------------------------------------------------------------------------------
# CRIME DATA 
# ------------------------------------------------------------------------------

# Create sf object out of csv
sf.crime <- fread("/Users/samu_hugo/Desktop/Code/Data/crime.csv") %>% 
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>% 
  st_transform(32118)

# Count number of shootings within 10min walking distance
sf.nyc$n_crime <- st_contains(sf.nyc_800, sf.crime) %>% 
  map(length) %>% 
  unlist()

# Plot shootings
tmap_mode("plot")
pdf("map-shootings.pdf")
tm_shape(sf.boroughs) + 
  tm_polygons(alpha = 0.1, border.alpha = 0.4) +
  tm_layout(title = "Shootings, NYC", title.position = c("left", "top"),
            frame = F) + 
  tm_legend(title.size = 1.2, text.size = 0.8, position = c(0.05, 0.7)) + 
  tm_shape(sf.nyc) +
  tm_dots(size = 0.1, col = "price_m2", title = "Price per square metre", alpha = 0.4, 
          style = "quantile", border.lwd = NA, border.col = NA,
          palette = viridis::magma(n = 20, 
                                   begin = 0.1, 
                                   end = 0.9, 
                                   direction = -1)) +
  tm_shape(sf.crime) +
  tm_dots(size = 0.1, col = "blue")
dev.off()


# ------------------------------------------------------------------------------
# SUBWAY STATIONS 
# ------------------------------------------------------------------------------

# Create sf object out of csv
sf.subway <- fread("/Users/samu_hugo/Desktop/Code/Data/subway.csv") %>% 
  mutate(coordinates = str_match(the_geom, "\\((.*?)\\)")[,2]) %>% 
  mutate(X = as.numeric(substr(coordinates, 1, regexpr(" ", coordinates) - 1))) %>% 
  mutate(Y = as.numeric(substr(coordinates, regexpr(" ", coordinates) + 1, nchar(coordinates)))) %>% 
  filter(!is.na(X) & !is.na(Y)) %>% 
  st_as_sf(coords = c("X", "Y"), crs = 4326) %>% 
  st_transform(32118)

# Calculate distance to closest subway stations
sf.nyc$d_subway <- st_distance(sf.nyc, st_combine(sf.subway)) %>% 
  as.numeric() %>% 
  `/`(100)
sf.nyc$d_subway_unit <- "100m"

# Plot subway stations
tmap_mode("plot")
pdf("map-subway.pdf")
tm_shape(sf.boroughs) + 
  tm_polygons(alpha = 0.1, border.alpha = 0.4) +
  tm_layout(title = "Subway stations, NYC", title.position = c("left", "top"),
            frame = F) + 
  tm_legend(title.size = 1.2, text.size = 0.8, position = c(0.05, 0.7)) + 
  tm_shape(sf.nyc) +
  tm_dots(size = 0.1, col = "price_m2", title = "Price per square metre", alpha = 0.4, 
          style = "quantile", border.lwd = NA, border.col = NA,
          palette = viridis::magma(n = 20, 
                                   begin = 0.1, 
                                   end = 0.9, 
                                   direction = -1)) +
  tm_shape(sf.subway) +
  tm_dots(size = 0.1, col = "blue")
dev.off()


# ------------------------------------------------------------------------------
# NOISE 
# ------------------------------------------------------------------------------

# Create sf object out of csv
sf.noise <- fread("/Users/samu_hugo/Desktop/Code/Data/noise.csv") %>% 
  filter(substr(`Created Date`, 7, 10) == "2018" & `Complaint Type` == "Noise - Residential") %>% 
  filter(`Resolution Description` %in% c("The Police Department responded to the complaint and took action to fix the condition.",
                                         "The Police Department made an arrest in response to the complaint.")) %>% 
  filter(!is.na(Longitude) & !is.na(Latitude)) %>% 
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>% 
  st_transform(32118)

# Count number of noise complaints within 5min walking distance
sf.nyc$n_noise <- st_contains(sf.nyc_400, sf.noise) %>% 
  map(length) %>% 
  unlist()

# Plot noise complaints
tmap_mode("plot")
pdf("map-noise.pdf")
tm_shape(sf.boroughs) + 
  tm_polygons(alpha = 0.1, border.alpha = 0.4) +
  tm_layout(title = "Noise complaints, NYC", title.position = c("left", "top"),
            frame = F) + 
  tm_legend(title.size = 1.2, text.size = 0.8, position = c(0.05, 0.7)) + 
  tm_shape(sf.nyc) +
  tm_dots(size = 0.1, col = "price_m2", title = "Price per square metre", alpha = 0.4, 
          style = "quantile", border.lwd = NA, border.col = NA,
          palette = viridis::magma(n = 20, 
                                   begin = 0.1, 
                                   end = 0.9, 
                                   direction = -1)) +
  tm_shape(sf.noise) +
  tm_dots(size = 0.02, col = "blue", alpha = 0.2)
dev.off()


# ------------------------------------------------------------------------------
# AIR 
# ------------------------------------------------------------------------------

# Create an sf object
df.air <- fread("/Users/samu_hugo/Desktop/Code/Data/air.csv") %>%
  filter(year_description == "2013" & geo_type_name == "Borough") %>% 
  select(-indicator_data_id, -indicator_id) %>% 
  pivot_wider(names_from = name, values_from = data_valuemessage)

sf.nyc$id <- 1:nrow(sf.nyc)
l.boroughs <- st_contains(sf.boroughs, sf.nyc)
sf.nyc$borough <- case_when(sf.nyc$id %in% l.boroughs[[1]] ~ sf.boroughs$boro_name[1],
                            sf.nyc$id %in% l.boroughs[[2]] ~ sf.boroughs$boro_name[2],
                            sf.nyc$id %in% l.boroughs[[3]] ~ sf.boroughs$boro_name[3],
                            sf.nyc$id %in% l.boroughs[[4]] ~ sf.boroughs$boro_name[4],
                            sf.nyc$id %in% l.boroughs[[5]] ~ sf.boroughs$boro_name[5])

# Check that observations were assigned property
tmap_mode("plot")
pdf("map-boroughs.pdf")
tm_shape(sf.boroughs) +
  tm_polygons(alpha = 0.1, border.alpha = 0.4, border.col = "black") +
  tm_shape(sf.nyc) +
  tm_dots(size = 0.04, col = "borough", alpha = 1, shape = 15,
          palette = viridis::magma(n = 5, 
                                   begin = 0.2, 
                                   end = 0.8, 
                                   direction = -1))
dev.off()

# Join the air pollution data to the price data
sf.nyc <- sf.nyc %>% 
  left_join(df.air %>% select(measure = Measure, 
                              boro_id = geo_entity_id, 
                              borough = geo_entity_name,
                              NOx = `Boiler Emissions- Total NOx Emissions`,
                              PM2.5 = `Boiler Emissions- Total PM2.5 Emissions`,
                              SO2 = `Boiler Emissions- Total SO2 Emissions`) %>% 
              mutate(borough = as.character(borough)), 
            by = c("borough"))


# ------------------------------------------------------------------------------
# RESTARURANTS 
# ------------------------------------------------------------------------------

# Get restuaurant locations from OSM
sf.restaurants <- getbb('New York City', format_out = "sf_polygon") %>% 
  opq() %>%
  add_osm_feature(key = 'amenity', value = 'restaurant') %>% 
  osmdata_sf() %>% 
  trim_osmdata(bb_poly = getbb('New York City', format_out = "sf_polygon"))

# Get points that do not overlap with polygons
sf.restaurants_points <- sf.restaurants$osm_points %>% 
  filter(lengths(st_touches(., sf.restaurants$osm_polygons)) == 0) %>% 
  st_geometry()

# Get the centroids of the polygons
sf.restaurants_centroids <- sf.restaurants$osm_polygons %>% 
  st_centroid() %>% 
  st_geometry()

# Bind the two sf point objects together
sf.restaurants <- c(sf.restaurants_points, sf.restaurants_centroids) %>% 
  st_transform(32118)
rm(list = c("sf.restaurants_points", "sf.restaurants_centroids"))

# Count number of restaurants within 5m walking distance
sf.nyc$n_restaurants <- st_contains(sf.nyc_400, sf.restaurants) %>% 
  map(length) %>% 
  unlist()

# Plot restaurants
tmap_mode("plot")
pdf("map-restaurants.pdf")
tm_shape(sf.boroughs) + 
  tm_polygons(alpha = 0.1, border.alpha = 0.4) +
  tm_layout(title = "Restaurants, NYC", title.position = c("left", "top"),
            frame = F) + 
  tm_legend(title.size = 1.2, text.size = 0.8, position = c(0.05, 0.7)) + 
  tm_shape(sf.nyc) +
  tm_dots(size = 0.1, col = "price_m2", title = "Price per square metre", alpha = 0.4, 
          style = "quantile", border.lwd = NA, border.col = NA,
          palette = viridis::magma(n = 20, 
                                   begin = 0.1, 
                                   end = 0.9, 
                                   direction = -1)) +
  tm_shape(sf.restaurants) +
  tm_dots(size = 0.04, border.col = "blue")
dev.off()


# ------------------------------------------------------------------------------
# CAFES 
# ------------------------------------------------------------------------------

# Get cafe locations from OSM
sf.cafes <- getbb('New York City', format_out = "sf_polygon") %>% 
  opq() %>%
  add_osm_feature(key = 'amenity', value = 'cafe') %>% 
  osmdata_sf() %>% 
  trim_osmdata(bb_poly = getbb('New York City', format_out = "sf_polygon"))

# Get points that do not overlap with polygons
sf.cafes_points <- sf.cafes$osm_points %>% 
  filter(lengths(st_touches(., sf.cafes$osm_polygons)) == 0) %>% 
  st_geometry()

# Get the centroids of the polygons
sf.cafes_centroids <- sf.cafes$osm_polygons %>% 
  st_centroid() %>% 
  st_geometry()

# Bind the two sf point objects together
sf.cafes <- c(sf.cafes_points, sf.cafes_centroids) %>% 
  st_transform(32118)
rm(list = c("sf.cafes_points", "sf.cafes_centroids"))

# Count number of cafes within 5m walking distance
sf.nyc$n_cafes <- st_contains(sf.nyc_400, sf.cafes) %>% 
  map(length) %>% 
  unlist()

# Plot cafes
tmap_mode("plot")
pdf("map-cafes.pdf")
tm_shape(sf.boroughs) + 
  tm_polygons(alpha = 0.1, border.alpha = 0.4) +
  tm_layout(title = "Cafes, NYC", title.position = c("left", "top"),
            frame = F) + 
  tm_legend(title.size = 1.2, text.size = 0.8, position = c(0.05, 0.7)) + 
  tm_shape(sf.nyc) +
  tm_dots(size = 0.1, col = "price_m2", title = "Price per square metre", alpha = 0.4, 
          style = "quantile", border.lwd = NA, border.col = NA,
          palette = viridis::magma(n = 20, 
                                   begin = 0.1, 
                                   end = 0.9, 
                                   direction = -1)) +
  tm_shape(sf.cafes) +
  tm_dots(size = 0.04, col = "blue", alpha = 1)
dev.off()


# ------------------------------------------------------------------------------
# BARS 
# ------------------------------------------------------------------------------

# Get bar locations from OSM
sf.bars <- getbb('New York City', format_out = "sf_polygon") %>% 
  opq() %>%
  add_osm_feature(key = 'amenity', value = 'bar') %>% 
  osmdata_sf() %>% 
  trim_osmdata(bb_poly = getbb('New York City', format_out = "sf_polygon"))

# Get points that do not overlap with polygons
sf.bars_points <- sf.bars$osm_points %>% 
  filter(lengths(st_touches(., sf.bars$osm_polygons)) == 0) %>% 
  st_geometry()

# Get the centroids of the polygons
sf.bars_centroids <- sf.bars$osm_polygons %>% 
  st_centroid() %>% 
  st_geometry()

# Bind the two sf point objects together
sf.bars <- c(sf.bars_points, sf.bars_centroids) %>% 
  st_transform(32118)
rm(list = c("sf.bars_points", "sf.bars_centroids"))

# Count number of bars within 5m walking distance
sf.nyc$n_bars <- st_contains(sf.nyc_400, sf.bars) %>% 
  map(length) %>% 
  unlist()

# Plot bars
tmap_mode("plot")
pdf("map-bars.pdf")
tm_shape(sf.boroughs) + 
  tm_polygons(alpha = 0.1, border.alpha = 0.4) +
  tm_layout(title = "Bars, NYC", title.position = c("left", "top"),
            frame = F) + 
  tm_legend(title.size = 1.2, text.size = 0.8, position = c(0.05, 0.7)) + 
  tm_shape(sf.nyc) +
  tm_dots(size = 0.1, col = "price_m2", title = "Price per square metre", alpha = 0.4, 
          style = "quantile", border.lwd = NA, border.col = NA,
          palette = viridis::magma(n = 20, 
                                   begin = 0.1, 
                                   end = 0.9, 
                                   direction = -1)) +
  tm_shape(sf.bar) +
  tm_dots(size = 0.04, col = "blue", alpha = 1)
dev.off()


# ------------------------------------------------------------------------------
# CINEMAS 
# ------------------------------------------------------------------------------

# Get cinema locations from OSM
sf.cinema <- getbb('New York City', format_out = "sf_polygon") %>% 
  opq() %>%
  add_osm_feature(key = 'amenity', value = 'cinema') %>% 
  osmdata_sf() %>% 
  trim_osmdata(bb_poly = getbb('New York City', format_out = "sf_polygon"))

# Calculate distance to closest cinema
sf.nyc$d_cinema <- st_distance(sf.nyc, st_combine(st_transform(sf.cinema$osm_polygons,  32118))) %>% 
  as.numeric() %>% 
  `/`(100)
sf.nyc$d_cinema_unit <- "100m"

# Plot cinemas
tmap_mode("plot")
pdf("map-cinemas.pdf")
tm_shape(sf.boroughs) + 
  tm_polygons(alpha = 0.1, border.alpha = 0.4) +
  tm_layout(title = "Cinemas, NYC", title.position = c("left", "top"),
            frame = F) + 
  tm_legend(title.size = 1.2, text.size = 0.8, position = c(0.05, 0.7)) + 
  tm_shape(sf.nyc) +
  tm_dots(size = 0.1, col = "price_m2", title = "Price per square metre", alpha = 0.4, 
          style = "quantile", border.lwd = NA, border.col = NA,
          palette = viridis::magma(n = 20, 
                                   begin = 0.1, 
                                   end = 0.9, 
                                   direction = -1)) +
  tm_shape(st_transform(sf.cinema$osm_points, 32118)) +
  tm_dots(size = 0.2, col = "blue", alpha = 1)
dev.off()


# ------------------------------------------------------------------------------
# AIRPORTS
# ------------------------------------------------------------------------------

# Get airport geometries from OSM
sf.airport <- getbb('New York City', format_out = "sf_polygon") %>% 
  opq() %>%
  add_osm_feature(key = 'aeroway', value = 'aerodrome') %>% 
  osmdata_sf() %>% 
  trim_osmdata(bb_poly = getbb('New York City', format_out = "sf_polygon"))

# Calculate distance to closest airport
sf.nyc$d_airport <- st_distance(sf.nyc, st_combine(st_transform(sf.airport$osm_polygons,  32118))) %>% 
  as.numeric() %>% 
  `/`(100)
sf.nyc$d_airport_unit <- "100m"

# Plot airports
tmap_mode("plot")
pdf("map-airports.pdf")
tm_shape(sf.boroughs) + 
  tm_polygons(alpha = 0.1, border.alpha = 0.4) +
  tm_layout(title = "Airports, NYC", title.position = c("left", "top"),
            frame = F) + 
  tm_legend(title.size = 1.2, text.size = 0.8, position = c(0.05, 0.7)) + 
  tm_shape(sf.nyc) +
  tm_dots(size = 0.1, col = "price_m2", title = "Price per square metre", alpha = 0.4, 
          style = "quantile", border.lwd = NA, border.col = NA,
          palette = viridis::magma(n = 20, 
                                   begin = 0.1, 
                                   end = 0.9, 
                                   direction = -1)) +
  tm_shape(st_transform(sf.airport$osm_polygons, 32118)) +
  tm_polygons(border.col = "blue", border.alpha = 1)
dev.off()


# ------------------------------------------------------------------------------
# FITNESS CENTRES
# ------------------------------------------------------------------------------

# Get fitness locations from OSM
sf.sport1 <- getbb('New York City', format_out = "sf_polygon") %>% 
  opq() %>%
  add_osm_feature(key = 'leisure', value = 'fitness_centre') %>% 
  osmdata_sf() %>% 
  trim_osmdata(bb_poly = getbb('New York City', format_out = "sf_polygon"))

sf.sport2 <- getbb('New York City', format_out = "sf_polygon") %>% 
  opq() %>%
  add_osm_feature(key = 'sport', value = 'fitness') %>% 
  osmdata_sf() %>% 
  trim_osmdata(bb_poly = getbb('New York City', format_out = "sf_polygon"))

# Calculate distance to closest fitness
sf.nyc$d_fitness <- st_distance(sf.nyc, st_transform(st_combine(c(st_geometry(sf.sport1$osm_points), st_geometry(sf.sport2$osm_points))), 32118)) %>% 
  as.numeric() %>% 
  `/`(100)
sf.nyc$d_fitness_unit <- "100m"

# Plot fitness centres
tmap_mode("plot")
pdf("map-fitness.pdf")
tm_shape(sf.boroughs) + 
  tm_polygons(alpha = 0.1, border.alpha = 0.4) +
  tm_layout(title = "Fitness centres, NYC", title.position = c("left", "top"),
            frame = F) + 
  tm_legend(title.size = 1.2, text.size = 0.8, position = c(0.05, 0.7)) + 
  tm_shape(sf.nyc) +
  tm_dots(size = 0.1, col = "price_m2", title = "Price per square metre", alpha = 0.4, 
          style = "quantile", border.lwd = NA, border.col = NA,
          palette = viridis::magma(n = 20, 
                                   begin = 0.1, 
                                   end = 0.9, 
                                   direction = -1)) +
  tm_shape(st_transform(st_combine(c(st_geometry(sf.sport1$osm_points), st_geometry(sf.sport2$osm_points))), 32118)) +
  tm_dots(size = 0.1, col = "blue", alpha = 1)
dev.off()


# ------------------------------------------------------------------------------
# PARKS  (takes ~45min)
# ------------------------------------------------------------------------------

# Get park areas from OSM
sf.parks <- getbb('New York City', format_out = "sf_polygon") %>% 
  opq() %>%
  add_osm_feature(key = 'leisure', value = 'park') %>% 
  osmdata_sf() %>% 
  trim_osmdata(bb_poly = getbb('New York City', format_out = "sf_polygon"))

# Remove parks with area smaller than 2500 sqm
sf.parks2 <- st_transform(sf.parks$osm_polygons, 32118)  # reproject geometry to ensure area accuracy
sf.parks3 <- drop_crumbs(sf.parks2, 2500)  # drop parks with area smaller than 50 by 50 metres

# Check the first 10 properties visually
sf.nyc_2000_10 <- sf.nyc_2000[1:10, ]
sf.parks_10 <- st_combine(st_intersection(sf.nyc_2000_10, sf.parks3))

# Plot park areas
tmap_mode("plot")
pdf("map-parks.pdf")
tm_shape(sf.boroughs) + 
  tm_polygons(alpha = 0.1, border.alpha = 0.4) +
  tm_layout(title = "Park areas, NYC", title.position = c("left", "top"),
            frame = F) + 
  tm_legend(title.size = 1.2, text.size = 0.8, position = c(0.05, 0.7)) + 
  tm_shape(sf.nyc) +
  tm_dots(size = 0.1, col = "price_m2", title = "Price per square metre", alpha = 0.4, 
          style = "quantile", border.lwd = NA, border.col = NA,
          palette = viridis::magma(n = 20, 
                                   begin = 0.1, 
                                   end = 0.9, 
                                   direction = -1)) + 
  tm_shape(sf.parks$osm_polygons) +
  tm_polygons(col = "darkgreen", alpha = 0.8, border.alpha = 0) + 
  tm_shape(sf.nyc_2000_10) +
  tm_polygons(alpha = 0.7) +
  tm_shape(sf.parks_10) +
  tm_polygons(col = "blue") +
  tm_shape(st_centroid(sf.nyc_2000_10)) +
  tm_dots(col = "red", size = 0.1)
dev.off()

# Calculate intersection areas on entire data set (~41min)
tic("Park area")
  area <- rep(NA, nrow(sf.nyc_2000))
  for (x in 1:nrow(sf.nyc_2000)) {
    area[x] <- (st_area(st_combine(st_intersection(sf.nyc_2000[x, ], sf.parks3))))
  }
toc()

# Check if calculations are plausible
max_area <- pi * 2000^2  # area of the 2km buffer zone
all(area < max_area)

# Add areas to the data set in km2
sf.nyc$a_park <- area / 1000000
sf.nyc$a_park_unit <- "km2"


# ------------------------------------------------------------------------------
# PLAYGROUNDS
# ------------------------------------------------------------------------------

# Get playground locations from OSM
sf.playground <- getbb('New York City', format_out = "sf_polygon") %>% 
  opq() %>%
  add_osm_feature(key = 'leisure', value = 'playground') %>% 
  osmdata_sf() %>% 
  trim_osmdata(bb_poly = getbb('New York City', format_out = "sf_polygon"))

# Get points that do not overlap with polygons
sf.playground_points <- sf.playground$osm_points %>% 
  filter(lengths(st_touches(., sf.playground$osm_polygons)) == 0) %>% 
  st_geometry()

# Get the centroids of the polygons
sf.playground_centroids <- sf.playground$osm_polygons %>% 
  st_centroid() %>% 
  st_geometry()

# Bind the two sf point objects together
sf.playground <- c(sf.playground_points, sf.playground_centroids) %>% 
  st_transform(32118) %>% 
  st_combine()
rm(list = c("sf.playground_centroids", "sf.playground_points"))

# Calculate distance to closest playground
sf.nyc$d_playground <- st_distance(sf.nyc, sf.playground) %>% 
  as.numeric() %>% 
  `/`(100)
sf.nyc$d_playground_unit <- "100m"

# Plot playgrounds
tmap_mode("plot")
pdf("map-playgrounds.pdf")
tm_shape(sf.boroughs) + 
  tm_polygons(alpha = 0.1, border.alpha = 0.4) +
  tm_layout(title = "Playgrounds, NYC", title.position = c("left", "top"),
            frame = F) + 
  tm_legend(title.size = 1.2, text.size = 0.8, position = c(0.05, 0.7)) + 
  tm_shape(sf.nyc) +
  tm_dots(size = 0.1, col = "price_m2", title = "Price per square metre", alpha = 0.4, 
          style = "quantile", border.lwd = NA, border.col = NA,
          palette = viridis::magma(n = 20, 
                                   begin = 0.1, 
                                   end = 0.9, 
                                   direction = -1)) +
  tm_shape(sf.playground) +
  tm_dots(size = 0.1, col = "blue", alpha = 1) +
dev.off()


# ------------------------------------------------------------------------------
# RAIL 
# ------------------------------------------------------------------------------

# Get rail lines from OSM
sf.lightrail <- getbb('New York City', format_out = "sf_polygon") %>% 
  opq() %>%
  add_osm_feature(key = 'railway', value = 'light_rail') %>% 
  osmdata_sf() %>% 
  trim_osmdata(bb_poly = getbb('New York City', format_out = "sf_polygon"))

sf.rail <- getbb('New York City', format_out = "sf_polygon") %>% 
  opq() %>%
  add_osm_feature(key = 'railway', value = 'rail') %>% 
  osmdata_sf() %>% 
  trim_osmdata(bb_poly = getbb('New York City', format_out = "sf_polygon"))

sf.railways <- st_combine(c(st_combine(sf.lightrail$osm_lines), st_combine(sf.rail$osm_lines))) %>%  # join both osm data sets
  st_transform(32118)

# Calculate distance to railways for each house
sf.nyc$d_railway <- st_distance(sf.nyc, sf.railways) %>% 
  as.numeric() %>% 
  `/`(100)
sf.nyc$d_railway_unit <- "100m"

# Plot railways
tmap_mode("plot")
pdf("map-railways.pdf")
tm_shape(sf.boroughs) + 
  tm_polygons(alpha = 0.1, border.alpha = 0.4) +
  tm_layout(title = "Railways, NYC", title.position = c("left", "top"),
            frame = F) + 
  tm_legend(title.size = 1.2, text.size = 0.8, position = c(0.05, 0.7)) + 
  tm_shape(sf.nyc) +
  tm_dots(size = 0.1, col = "price_m2", title = "Price per square metre", alpha = 0.4, 
          style = "quantile", border.lwd = NA, border.col = NA,
          palette = viridis::magma(n = 20, 
                                   begin = 0.1, 
                                   end = 0.9, 
                                   direction = -1)) +
  tm_shape(sf.railways) +
  tm_lines(lwd = 1, col = "blue")
dev.off()


# ------------------------------------------------------------------------------
# DEPARTMENT STORES & SHOPPING MALLS
# ------------------------------------------------------------------------------

# Get the department store locations from OSM
sf.department_stores <- getbb('New York City', format_out = "sf_polygon") %>% 
  opq() %>%
  add_osm_feature(key = 'shop', value = 'department_store') %>% 
  osmdata_sf() %>% 
  trim_osmdata(bb_poly = getbb('New York City', format_out = "sf_polygon"))

# Get the mall locations from OSM
sf.shopping_malls <- getbb('New York City', format_out = "sf_polygon") %>% 
  opq() %>%
  add_osm_feature(key = 'shop', value = 'mall') %>% 
  osmdata_sf() %>% 
  trim_osmdata(bb_poly = getbb('New York City', format_out = "sf_polygon"))

# Get the centroids of the polygons
sf.shopping_malls_centroids <- sf.shopping_malls$osm_polygons %>% 
  st_centroid() %>% 
  st_geometry() %>% 
  st_transform(32118)

# Transform the department store data and join it with the shopping mall data
sf.shopping <- sf.department_stores$osm_points %>% 
  st_geometry() %>% 
  st_transform(32118) %>% 
  c(sf.shopping_malls_centroids) %>% 
  st_combine()

# Calculate distance to the nearest department store or shopping mall
sf.nyc$d_shopping <- st_distance(sf.nyc, sf.shopping) %>% 
  as.numeric() %>% 
  `/`(100)
sf.nyc$d_shopping_unit <- "100m"

# Plot shopping centres
tmap_mode("plot")
pdf("map-shopping.pdf")
tm_shape(sf.boroughs) + 
  tm_polygons(alpha = 0.1, border.alpha = 0.4) +
  tm_layout(title = "Shopping centres, NYC", title.position = c("left", "top"),
            frame = F) + 
  tm_legend(title.size = 1.2, text.size = 0.8, position = c(0.05, 0.7)) + 
  tm_shape(sf.nyc) +
  tm_dots(size = 0.1, col = "price_m2", title = "Price per square metre", alpha = 0.4, 
          style = "quantile", border.lwd = NA, border.col = NA,
          palette = viridis::magma(n = 20, 
                                   begin = 0.1, 
                                   end = 0.9, 
                                   direction = -1)) +
  tm_shape(sf.shopping) +
  tm_dots(size = 0.1, col = "blue", alpha = 1)
dev.off()


# ------------------------------------------------------------------------------
# GROCERY STORE 
# ------------------------------------------------------------------------------

# Get the convenience store locations from OSM
sf.convenience <- getbb('New York City', format_out = "sf_polygon") %>% 
  opq() %>%
  add_osm_feature(key = 'shop', value = 'convenience') %>% 
  osmdata_sf() %>% 
  trim_osmdata(bb_poly = getbb('New York City', format_out = "sf_polygon"))

# Get the convenience store locations from OSM
sf.supermarket <- getbb('New York City', format_out = "sf_polygon") %>% 
  opq() %>%
  add_osm_feature(key = 'shop', value = 'supermarket') %>% 
  osmdata_sf() %>% 
  trim_osmdata(bb_poly = getbb('New York City', format_out = "sf_polygon"))

# Get points that do not overlap with polygons
sf.supermarket_points <- sf.supermarket$osm_points %>% 
  filter(lengths(st_touches(., sf.supermarket$osm_polygons)) == 0) %>% 
  st_geometry()

# Get the centroids of the polygons
sf.supermarket_centroids <- sf.supermarket$osm_polygons %>% 
  st_centroid() %>% 
  st_geometry()

# Bind the two sf point objects together
sf.supermarket <- c(sf.supermarket_points, sf.supermarket_centroids) %>% 
  st_transform(32118)
rm(list = c("sf.supermarket_centroids", "sf.supermarket_points"))

# Get points that do not overlap with polygons
sf.convenience_points <- sf.convenience$osm_points %>% 
  filter(lengths(st_touches(., sf.convenience$osm_polygons)) == 0) %>% 
  st_geometry()

# Get the centroids of the polygons
sf.convenience_centroids <- sf.convenience$osm_polygons %>% 
  st_centroid() %>% 
  st_geometry()

# Bind the two sf point objects together
sf.convenience <- c(sf.convenience_points, sf.convenience_centroids) %>% 
  st_transform(32118)
rm(list = c("sf.convenience_centroids", "sf.convenience_points"))

# Calculate distance to closest supermarket
sf.nyc$d_supermarket <- st_distance(sf.nyc, st_combine(sf.supermarket)) %>% 
  as.numeric() %>% 
  `/`(100)
sf.nyc$d_supermarket_unit <- "100m"

# Calculate distance to closest convenience store
sf.nyc$d_convenience <- st_distance(sf.nyc, st_combine(sf.convenience)) %>% 
  as.numeric() %>% 
  `/`(100)
sf.nyc$d_convenience_unit <- "100m"

# Plot grocery stores
tmap_mode("plot")
pdf("map-grocery-stores.pdf")
tm_shape(sf.boroughs) + 
  tm_polygons(alpha = 0.1, border.alpha = 0.4) +
  tm_layout(title = "Grocery stores, NYC", title.position = c("left", "top"),
            frame = F) + 
  tm_legend(title.size = 1.2, text.size = 0.8, position = c(0.05, 0.7)) + 
  tm_shape(sf.nyc) +
  tm_dots(size = 0.1, col = "price_m2", title = "Price per square metre", alpha = 0.4, 
          style = "quantile", border.lwd = NA, border.col = NA,
          palette = viridis::magma(n = 20, 
                                   begin = 0.1, 
                                   end = 0.9, 
                                   direction = -1)) +
  tm_shape(sf.supermarket) +
  tm_dots(size = 0.1, col = "blue", alpha = 1) + 
  tm_shape(sf.convenience) +
  tm_dots(size = 0.1, col = "green", alpha = 1)
dev.off()


# ------------------------------------------------------------------------------
# STOCK EXCHANGE 
# ------------------------------------------------------------------------------

# Define location of stock exchange
sf.exchange <- st_set_crs(st_geometry(st_point(c(-74.011265, 40.706877))), 4326) %>% 
  st_transform(32118)

# Calculate distance to stock exchange
sf.nyc$d_exchange <- st_distance(sf.nyc, sf.exchange) %>% 
  as.numeric() %>% 
  `/`(100)
sf.nyc$d_exchange_unit <- "100m"

# Plot stock exchange
tmap_mode("plot")
pdf("map-exchange.pdf")
tm_shape(sf.boroughs) + 
  tm_polygons(alpha = 0.1, border.alpha = 0.4) +
  tm_layout(title = "Stock Exchange, NYC", title.position = c("left", "top"),
            frame = F) + 
  tm_legend(title.size = 1.2, text.size = 0.8, position = c(0.05, 0.7)) + 
  tm_shape(sf.nyc) +
  tm_dots(size = 0.1, col = "price_m2", title = "Price per square metre", alpha = 0.4, 
          style = "quantile", border.lwd = NA, border.col = NA,
          palette = viridis::magma(n = 20, 
                                   begin = 0.1, 
                                   end = 0.9, 
                                   direction = -1)) +
  tm_shape(sf.exchange) +
  tm_dots(size = 0.6, col = "green", alpha = 1, shape = 13)
dev.off()


# ------------------------------------------------------------------------------
# ROADS
# ------------------------------------------------------------------------------

# Get road data from OSM
sf.motorway <- getbb('New York City', format_out = "sf_polygon") %>% 
  opq() %>%
  add_osm_feature(key = 'highway', value = 'motorway') %>% 
  osmdata_sf() %>% 
  trim_osmdata(bb_poly = getbb('New York City', format_out = "sf_polygon"))
sf.motorway <- st_combine(sf.motorway$osm_lines) %>% 
  st_transform(32118)

sf.motorway_link <- getbb('New York City', format_out = "sf_polygon") %>% 
  opq() %>%
  add_osm_feature(key = 'highway', value = 'motorway_link') %>% 
  osmdata_sf() %>% 
  trim_osmdata(bb_poly = getbb('New York City', format_out = "sf_polygon"))
sf.motorway_link <- st_combine(sf.motorway_link$osm_lines) %>% 
  st_transform(32118)

sf.trunkroad <- getbb('New York City', format_out = "sf_polygon") %>% 
  opq() %>%
  add_osm_feature(key = 'highway', value = 'trunk') %>% 
  osmdata_sf() %>% 
  trim_osmdata(bb_poly = getbb('New York City', format_out = "sf_polygon"))
sf.trunkroad <- st_combine(sf.trunkroad$osm_lines) %>% 
  st_transform(32118)

sf.mainroad <- getbb('New York City', format_out = "sf_polygon") %>% 
  opq() %>%
  add_osm_feature(key = 'highway', value = 'primary') %>% 
  osmdata_sf() %>% 
  trim_osmdata(bb_poly = getbb('New York City', format_out = "sf_polygon"))
sf.mainroad <- st_combine(sf.mainroad$osm_lines) %>% 
  st_transform(32118)

# Calculate distance to different roads for each house
sf.nyc$d_highway <- st_distance(sf.nyc, sf.motorway) %>% 
  as.numeric() %>% 
  `/`(100)
sf.nyc$d_highway_unit <- "100m"

sf.nyc$d_highway_link <- st_distance(sf.nyc, sf.motorway_link) %>% 
  as.numeric() %>% 
  `/`(100)
sf.nyc$d_highway_link_unit <- "100m"

sf.nyc$d_trunkroad <- st_distance(sf.nyc, sf.trunkroad) %>% 
  as.numeric() %>% 
  `/`(100)
sf.nyc$d_trunkroad_unit <- "100m"

sf.nyc$d_mainroad <- st_distance(sf.nyc, sf.mainroad) %>% 
  as.numeric() %>% 
  `/`(100)
sf.nyc$d_mainroad_unit <- "100m"

# Plot roads
tmap_mode("plot")
pdf("map-roads.pdf")
tm_shape(sf.boroughs) + 
  tm_polygons(alpha = 0.1, border.alpha = 0.4) +
  tm_layout(title = "Highways, NYC", title.position = c("left", "top"),
            frame = F) + 
  tm_legend(title.size = 1.2, text.size = 0.8, position = c(0.05, 0.7)) + 
  tm_shape(sf.nyc) +
  tm_dots(size = 0.1, col = "price_m2", title = "Price per square metre", alpha = 0.4, 
          style = "quantile", border.lwd = NA, border.col = NA,
          palette = viridis::magma(n = 20, 
                                   begin = 0.1, 
                                   end = 0.9, 
                                   direction = -1)) +
  tm_shape(sf.motorway) +
  tm_lines(lwd = 1, col = "blue") +
  tm_shape(sf.motorway_link) +
  tm_lines(lwd = 1, col = "green") +
  tm_shape(sf.mainroad) +
  tm_lines(lwd = 1, col = "red") +
  tm_shape(sf.trunkroad) +
  tm_lines(lwd = 1, col = "yellow")
dev.off()


# ------------------------------------------------------------------------------
# WATERFRONT
# ------------------------------------------------------------------------------

# Import waterfront shapefile
sf.waterfront <- st_as_sf(readOGR(dsn = "/Users/samu_hugo/Desktop/Code/Data/waterfront/",
                                  layer = "WAM_publiclyownedwaterfront"),
                          crs = 4326) %>% 
  st_transform(32118)
sf.waterfront <- sf.waterfront %>% 
  filter(st_is_valid(sf.waterfront) == T)

# Calculate distance to waterfront access
sf.nyc$d_waterfront <- st_distance(sf.nyc, st_combine(sf.waterfront)) %>% 
  as.numeric() %>% 
  `/`(100)
sf.nyc$d_waterfront_unit <- "100m"

# Plot waterfront acces
tmap_mode("plot")
pdf("map-waterfront.pdf")
tm_shape(sf.boroughs) + 
  tm_polygons(alpha = 0.1, border.alpha = 0.4) +
  tm_layout(title = "Waterfront access, NYC", title.position = c("left", "top"),
            frame = F) + 
  tm_legend(title.size = 1.2, text.size = 0.8, position = c(0.05, 0.7)) + 
  tm_shape(sf.nyc) +
  tm_dots(size = 0.1, col = "price_m2", title = "Price per square metre", alpha = 0.4, 
          style = "quantile", border.lwd = NA, border.col = NA,
          palette = viridis::magma(n = 20, 
                                   begin = 0.1, 
                                   end = 0.9, 
                                   direction = -1)) +
  tm_shape(sf.waterfront) +
  tm_polygons(border.col = "blue")
dev.off()


# ------------------------------------------------------------------------------
# Saving the data set
# ------------------------------------------------------------------------------

# Remove observations with NAs remaining (1 observation)
sf.nyc <- sf.nyc[!rowSums(is.na(sf.nyc)) > 0, ]

# Saving data set
save(sf.nyc, file = "/Users/samu_hugo/Desktop/Code/data-variables.RData")
