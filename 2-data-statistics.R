
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
packages <- c("tictoc", "pryr",  # profiling
              "MASS", "nngeo", "purrr", # miscellaneous
              "rgdal", "spdep", "tmap", "sf", "dplyr")  # fundamentals
ipack(packages) ; rm(ipack, packages)

# ------------------------------------------------------------------------------
# SPATIAL WEIGHTS MATRIX
# ------------------------------------------------------------------------------

# Import the data set
load("/Users/samu_hugo/Desktop/Code/data-variables.RData")

# Calculate a distance threshold
threshold <- st_nn(sf.nyc, sf.nyc, k = 2, returnDist = T)[[2]] %>% unlist() %>% max()

# Calculate cricles around observations
sf.nyc_t <- st_buffer(sf.nyc, threshold)
sf.nyc_2t <- st_buffer(sf.nyc, 2*threshold)

# Import NYC boroughs and change to a equidistant projection with metres as units
sf.boroughs <- st_as_sf(readOGR(dsn = "/Users/samu_hugo/Desktop/Code/Data/boroughs/",
                                layer = "geo_export_a814b278-dd58-459f-b871-28e2dd794f22"),
                        crs = 4326) %>% 
  st_transform(32118)

# Visually inspect the spatial relationships
tmap_mode("view")
tm_shape(sf.boroughs) + 
  tm_polygons(alpha = 0, border.alpha = 0.4) +
  tm_shape(sf.nyc_t[1:10,]) +
  tm_polygons(border.col = "green", alpha = 0, zindex = 999) +
  tm_shape(sf.nyc_2t[1:10,]) +
  tm_polygons(border.col = "blue", alpha = 0, zindex = 999) + 
  tm_shape(sf.nyc) +
  tm_dots(size = 0.06, col = "price_m2", title = "Price per m2", alpha = 0.4, 
          style = "quantile", border.lwd = NA, border.col = NA,
          palette = viridis::magma(n = 20, 
                                   begin = 0.1, 
                                   end = 0.9, 
                                   direction = -1))

# Calculating distances up to a threshold (~42sec)
tic("Calculating distances up to a threshold")
  dnn <- dnearneigh(sf.nyc, 0, round(threshold, 0))
  dlist <- nbdists(dnn, st_coordinates(sf.nyc))
toc()

# Creating an inverse distance matrix (~24sec)
tic("Inverse distance matrix")
  id.list <- lapply(dlist, function(x) 1/x)
  id.listw <- nb2listw(dnn, glist = id.list, style = "W") ; id.listw
toc()

# Creating an exponential distance matrix (~24sec)
tic("Negative exponential distance matrix")
  exp.list <- lapply(dlist, function(x) exp(-x))
  exp.listw <- nb2listw(dnn, glist = exp.list, style = "W")
toc()

# Plot a correlogram for k-nearest neighbours (~11min)
tic("Correlogram")
  knn10.nb <- knn2nb(knearneigh(sf.nyc, k = 10))
  pdf("/Users/samu_hugo/Desktop/Code/Plots/plot-correlogram.pdf") 
    plot(sp.correlogram(neighbours = knn10.nb,
                        var = as.vector(sf.nyc$price_m2),
                        order = 10,
                        method = "C",  # for Geary's C
                        style = "W",
                        randomisation = T,
                        zero.policy = T),
         main = "Correlogram Geary's C - Price per square metre")
    plot(sp.correlogram(neighbours = knn10.nb,
                        var = as.vector(sf.nyc$price_m2),
                        order = 10,
                        method = "I",  # for Moran's I
                        style = "W",
                        randomisation = T,
                        zero.policy = T),
         main = "Correlogram Moran's I - Price per square metre")
  dev.off()
toc()

# Creating an knn weights matrix (~25sec)
tic("knn 6 Weights matrix")
  knn6.nb <- knn2nb(knearneigh(sf.nyc, k = 6))  # create nb object with 5 nearest neighbours
  knn6.listw <- nb2listw(knn6.nb, style = "W")
toc()

# Kernel weights
# ··············································································

u = lapply(dlist, function(x) x/(round(threshold, 0)))

# Creating a gaussian distance matrix (~20sec)
tic("Gaussian distance matrix")
  gau.list <- lapply(u, function(z) exp(-0.5*(z^2)))
  gau.listw <- nb2listw(dnn, glist = gau.list, style = "W")
toc()

# Creating an epanechnikov distance matrix (~24sec)
tic("Epanechnikov distance matrix")
  epa.list <- lapply(u, function(z) (3/4)*(1 - z^2))
  epa.listw <- nb2listw(dnn, glist = epa.list, style = "W")
toc()

# Save all listw objects
rm(list = setdiff(ls(), grep('listw$', ls(), value = T)))
save.image(file = "/Users/samu_hugo/Desktop/Code/data-listw.RData")

# ------------------------------------------------------------------------------
# DESCRIPTIVE STATISTICS
# ------------------------------------------------------------------------------

# Load the listw objects and the variables
load(file = "/Users/samu_hugo/Desktop/Code/data-listw.RData")
load("/Users/samu_hugo/Desktop/Code/data-variables.RData")

# Check for global spatial correlation (~10min)
tic("Global Moran's I")
  gmi.stats <- map(grep('listw$', ls(), value = T), 
              function(x) moran.test(sf.nyc$price_m2, get(x), zero.policy = T))
  ggc.stats <- map(grep('listw$', ls(), value = T), 
              function(x) geary.test(sf.nyc$price_m2, get(x), zero.policy = T))
toc()
print(grep('listw$', ls(), value = T)) ; gmi.stats ; ggc.stats  # print the weights names and the values


# Check for local spatial autocorrelation (~90sec)
tic("Local Moan's I")
  # Local autocorrelation with adjusted p-values by the bonferroni method
  price.lmi.stats <- localmoran(as.vector(sf.nyc$price_m2),
                                id.listw,
                                zero.policy = T,
                                alternative = "greater",
                                p.adjust.method = "bonferroni")
  
  # Add the values to the data.frame and create factor variables indicating significance
  sf.stats <- sf.nyc %>% 
    dplyr::select(price_m2) %>% 
    mutate(lmi.level = price.lmi.stats[, 1],
           lmi.significance = case_when(price.lmi.stats[, 5] < 0.01 ~ "p < 0.01",
                                        price.lmi.stats[, 5] < 0.02 & 
                                          price.lmi.stats[, 5] > 0.01 ~ "p < 0.02",
                                        price.lmi.stats[, 5] < 0.05 & 
                                          price.lmi.stats[, 5] > 0.02 ~ "p < 0.05",
                                        TRUE ~ "not significant") %>% 
             as.factor())
  
  # Import NYC boroughs and change to a equidistant projection with metres as units
  sf.boroughs <- st_as_sf(readOGR(dsn = "/Users/samu_hugo/Desktop/Code/Data/boroughs/",
                                  layer = "geo_export_a814b278-dd58-459f-b871-28e2dd794f22"),
                          crs = 4326) %>% 
    st_transform(32118)
  
  # Plot the local Moran's I in levels and significance to detect outliers and local clusters
  p.levels.stats <- tm_shape(sf.boroughs) + 
    tm_polygons(alpha = 0.1, border.alpha = 0.4) +
    tm_layout(title.position = c("left", "top"), frame = F) + 
    tm_legend(title.size = 1.2, text.size = 0.8, position = c(0.05, 0.7)) +
    tm_shape(sf.stats) +
    tm_layout(title = "New York, Price per square metre", frame = F, title.position = c("center", "top")) +
    tm_dots(col = "lmi.level", title = "Local Moran's I Levels", palette = "plasma", style = "order")
  
  p.significance.stats <- tm_shape(sf.boroughs) + 
    tm_polygons(alpha = 0.1, border.alpha = 0.4) +
    tm_layout(title.position = c("left", "top"), frame = F) + 
    tm_legend(title.size = 1.2, text.size = 0.8, position = c(0.05, 0.7)) + 
    tm_shape(sf.stats) +
    tm_layout(title = "New York, Price per square metre", frame = F, title.position = c("center", "top")) +
    tm_dots(col = "lmi.significance", title = "Local Moran's I Significance", 
            palette = c("not significant" = "black",
                        "p < 0.01" = "yellow",
                        "p < 0.02" = "orange",
                        "p < 0.05" = "red"), legend.show = F) +
    tm_add_legend(type = "fill", 
                  col = c("black", "yellow", "orange", "red"),
                  labels = levels(sf.stats$lmi.significance),
                  title = "Local Moran's I")
  pdf("/Users/samu_hugo/Desktop/Code/Plots/map-local-moran.pdf") 
    tmap_mode("plot")
    tmap_arrange(p.levels.stats, p.significance.stats)
  dev.off()
toc()

# Moran's scatterplot plotted manually for greater flexibility (~2sec)
tic("Moran's scatterplot")
  sf.stats$price_scaled <- scale(sf.stats$price_m2)
  sf.stats$price_lagged <- lag.listw(id.listw, sf.stats$price_scaled)
  pdf("/Users/samu_hugo/Desktop/Code/Plots/plot-scatterplot.pdf") 
  plot(x = sf.stats$price_scaled, y = sf.stats$price_lagged, main = "Moran Scatterplot", 
       pch = 19, cex = 0.5, col = rgb(0,0,0,0.08),
       xlab = "House prices (scaled)",
       ylab = "Spatially lagged house prices (scaled)",
       xlim = c(-1, 12),
       ylim = c(-1, 4))
  abline(h = 0, v = 0, lty = 3)
  abline(lm(sf.stats$price_lagged ~ sf.stats$price_scaled), lty = 3, lwd = 2, col = "green")
  dev.off()
toc()

# Calculate and plot local Getis Ord statistics to analyse hot and cold spots (~30sec)
tic("Getis-Ord")
  localGO <- localG(sf.nyc$price_m2, id.listw, zero.policy = T)
  sf.stats$local.go <- localGO
  tmap_mode("plot")
  pdf("/Users/samu_hugo/Desktop/Code/Plots/map-getis-ord.pdf") 
  tm_shape(sf.boroughs) + 
    tm_polygons(alpha = 0.1, border.alpha = 0.4) +
    tm_legend(title.size = 1.2, text.size = 0.8, position = c(0.05, 0.7)) + 
    tm_shape(sf.stats) +
    tm_layout(title = "Getis-Ord", frame = F, title.position = c("center", "top")) +
    tm_dots(col = "local.go", title = "Local Getis-Ord", palette = "plasma", 
            style = "order")
  dev.off()
toc()

# Save the descriptive statistics
rm(list = setdiff(ls(), grep('.stats$', ls(), value = T)))
save.image(file = "/Users/samu_hugo/Desktop/Code/data-stats.RData")