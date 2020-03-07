
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
              "knitr", "kableExtra", "pastecs",  # latex
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
tm_basemap(leaflet::providers$Esri.WorldTopoMap) +
  tm_shape(sf.boroughs) + 
  tm_polygons(alpha = 0, border.alpha = 0.4) +
  tm_shape(sf.nyc_t[1:10,]) +
  tm_polygons(border.col = "red", alpha = 0, zindex = 999) +
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

# # Plot a correlogram for k-nearest neighbours (~32min)
# tic("Correlogram")
#   knn10.nb <- knn2nb(knearneigh(sf.nyc, k = 10))
#   pdf("/Users/samu_hugo/Desktop/Code/Plots/Statistics/plot-correlogram.pdf") 
#     plot(sp.correlogram(neighbours = knn10.nb,
#                         var = as.vector(sf.nyc$price_m2),
#                         order = 10,
#                         method = "C",  # for Geary's C
#                         style = "W",
#                         randomisation = T,
#                         zero.policy = T),
#          main = "Correlogram Geary's C - Price per square metre")
#     plot(sp.correlogram(neighbours = knn10.nb,
#                         var = as.vector(sf.nyc$price_m2),
#                         order = 10,
#                         method = "I",  # for Moran's I
#                         style = "W",
#                         randomisation = T,
#                         zero.policy = T),
#          main = "Correlogram Moran's I - Price per square metre")
#   dev.off()
# toc()
# 
# # Creating an knn weights matrix (~25sec)
# tic("knn 6 Weights matrix")
#   knn6.nb <- knn2nb(knearneigh(sf.nyc, k = 6))  # create nb object with 5 nearest neighbours
#   knn6.listw <- nb2listw(knn6.nb, style = "W")
# toc()

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
# SPATIAL AUTOCORRELATION
# ------------------------------------------------------------------------------

# Load the data set and the listw objects
load(file = "/Users/samu_hugo/Desktop/Code/data-listw.RData")
load(file = "/Users/samu_hugo/Desktop/Code/data-specification.RData")

# Classify the variables
locational_variables <- c("d_highschool", "math", "d_subway", "d_cinema", "d_fitness", 
                          "a_park", "d_playground", "d_railway", "d_shopping", 
                          "d_supermarket", "d_convenience", "d_exchange",
                          "d_highway", "d_mainroad", "d_waterfront",  "sum_n_rcb")
neighbourhood_variables <- c("pc_neighbourhood")
structural_variables <- c("building_class", "age")
numerical_variables_sum <- c("age", "math", "pc_neighbourhood", 
                             "sum_n_rcb", "d_subway", "d_railway", 
                             "d_highway", "d_mainroad","d_exchange", "d_highschool",
                             "d_supermarket", "d_convenience", "d_fitness", "d_cinema", 
                             "d_playground", "a_park", "d_waterfront", "d_shopping")
categorical_variables <- c("building_class")
dependent_variable.t <- c("price_m2.t")
dependent_variable <- c("price_m2")

# Descriptive statistics of the data set
options(scipen = 9999)
desc.stat <- df.nyc %>% 
  dplyr::select(dependent_variable, numerical_variables_sum) %>% 
  stat.desc(basic = T) %>% 
  slice(c(1,4,5,8,9,13))
rownames(desc.stat) <- c("Number of observations", "Minimum", "Maximum", "Median", "Mean", "Standard deviation")
desc.stat %>% 
  t() %>% 
  round(2) %>% 
  kable("latex", caption = "Descriptive statistics of the numerical variables included in the processed data set. In addition, there are two categorical variables, \textit{Boroughs} and \textit{Building class}, with five and four levels respectively.", booktabs = T) %>%
  kable_styling(latex_options = c("striped", "HOLD_position"))


# ------------------------------------------------------------------------------
# DESCRIPTIVE STATISTICS
# ------------------------------------------------------------------------------

# Load the listw objects and the variables
load("/Users/samu_hugo/Desktop/Code/data-listw.RData")
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
tic("Local Moran's I")
  # Local autocorrelation with adjusted p-values by the bonferroni method
  price.lmi.stats.bonf <- localmoran(as.vector(sf.nyc$price_m2),
                                id.listw,
                                zero.policy = T,
                                alternative = "greater",
                                p.adjust.method = "bonferroni")
  
  # Local autocorrelation without adjusted p-values
  price.lmi.stats.none <- localmoran(as.vector(sf.nyc$price_m2),
                                id.listw,
                                zero.policy = T,
                                alternative = "greater",
                                p.adjust.method = "none")
  
  # Add the values to the data.frame and create factor variables indicating significance
  sf.stats.bonf <- sf.nyc %>% 
    dplyr::select(price_m2) %>% 
    mutate(lmi.level = price.lmi.stats.bonf[, 1],
           lmi.significance = case_when(price.lmi.stats.bonf[, 5] < 0.01 ~ "p < 0.01",
                                        price.lmi.stats.bonf[, 5] < 0.01 & 
                                          price.lmi.stats.bonf[, 5] > 0.05 ~ "p < 0.05",
                                        price.lmi.stats.bonf[, 5] < 0.05 & 
                                          price.lmi.stats.bonf[, 5] > 0.1 ~ "p < 0.1",
                                        TRUE ~ "not significant") %>% 
             as.factor())
  
  sf.stats.none <- sf.nyc %>% 
    dplyr::select(price_m2) %>% 
    mutate(lmi.level = price.lmi.stats.none[, 1],
           lmi.significance = case_when(price.lmi.stats.none[, 5] < 0.01 ~ "p < 0.01",
                                        price.lmi.stats.none[, 5] < 0.01 & 
                                          price.lmi.stats.none[, 5] > 0.05 ~ "p < 0.05",
                                        price.lmi.stats.none[, 5] < 0.05 & 
                                          price.lmi.stats.none[, 5] > 0.1 ~ "p < 0.1",
                                        TRUE ~ "not significant") %>% 
             as.factor())
  
  # Import NYC boroughs and change to a equidistant projection with metres as units
  sf.boroughs <- st_as_sf(readOGR(dsn = "/Users/samu_hugo/Desktop/Code/Data/boroughs/",
                                  layer = "geo_export_a814b278-dd58-459f-b871-28e2dd794f22"),
                          crs = 4326) %>% 
    st_transform(32118)
  
  # Plot the local Moran's I in levels and significance to detect outliers and local clusters
  tmap_mode("plot")
  pdf("/Users/samu_hugo/Desktop/Code/Plots/Statistics/map-local-moran-levels.pdf")
    tm_shape(sf.boroughs) + 
      tm_polygons(alpha = 0.1, border.alpha = 0.4) +
      tm_layout(title.size = 2, title = "Local Moran's I", title.position = c(0.1, 0.95), frame = F) + 
      tm_legend(title.size = 1.6, text.size = 1, position = c(0.1, 0.70), height = 10) +
      tm_shape(sf.stats.bonf) +
      tm_dots(col = "lmi.level", title = "Levels", palette = "plasma", style = "order")
  dev.off()
  
  tmap_mode("plot")
  pdf("/Users/samu_hugo/Desktop/Code/Plots/Statistics/map-local-moran-significance-bonferroni.pdf")
    tm_shape(sf.boroughs) + 
      tm_polygons(alpha = 0.1, border.alpha = 0.4) +
      tm_layout(title.size = 2, title = "Local Moran's I", title.position = c(0.1, 0.95), frame = F) + 
      tm_legend(title.size = 1.6, text.size = 1, position = c(0.1, 0.70), height = 10) + 
      tm_shape(sf.stats.bonf) +
      tm_dots(col = "lmi.significance", 
              palette = c("not significant" = "black",
                          "p < 0.01" = "red",
                          "p < 0.05" = "orange",
                          "p < 0.10" = "yellow"), legend.show = F) +
      tm_add_legend(type = "fill", 
                    col = c("black", "red", "orange", "yellow"),
                    labels = c("Not significant", "p < 0.01", "p < 0.05", "p < 0.1"),
                    title = "Significance (Bonferroni adjustment)")
  dev.off()
  
  tmap_mode("plot")
  pdf("/Users/samu_hugo/Desktop/Code/Plots/Statistics/map-local-moran-significance-none.pdf")
  tm_shape(sf.boroughs) + 
    tm_polygons(alpha = 0.1, border.alpha = 0.4) +
    tm_layout(title.size = 2, title = "Local Moran's I", title.position = c(0.1, 0.95), frame = F) + 
    tm_legend(title.size = 1.6, text.size = 1, position = c(0.1, 0.70), height = 10) + 
    tm_shape(sf.stats.none) +
    tm_dots(col = "lmi.significance", 
            palette = c("not significant" = "black",
                        "p < 0.01" = "red",
                        "p < 0.05" = "orange",
                        "p < 0.10" = "yellow"), legend.show = F) +
    tm_add_legend(type = "fill", 
                  col = c("black", "red", "orange", "yellow"),
                  labels = c("Not significant", "p < 0.01", "p < 0.05", "p < 0.1"),
                  title = "Significance (No adjustment)")
  dev.off()

  # Moran's scatterplot plotted manually for greater flexibility (~2sec)
  sf.stats.none$price_scaled <- scale(sf.stats.none$price_m2)
  sf.stats.none$price_lagged <- lag.listw(id.listw, sf.stats.none$price_scaled)
  tmap_mode("plot")
  pdf("/Users/samu_hugo/Desktop/Code/Plots/Statistics/plot-moran-scatterplot.pdf") 
  plot(x = sf.stats.none$price_scaled, y = sf.stats.none$price_lagged, main = "Moran Scatterplot", 
       pch = 19, cex = 0.5, col = rgb(0,0,0,0.08),
       xlab = "House prices (scaled)",
       ylab = "Spatially lagged house prices (scaled)",
       xlim = c(-1, 12),
       ylim = c(-1, 4))
  abline(h = 0, v = 0, lty = 3)
  abline(lm(sf.stats.none$price_lagged ~ sf.stats.none$price_scaled), lty = 3, lwd = 2, col = "green")
  dev.off()

  # Calculate and plot local Getis Ord statistics to analyse hot and cold spots (~30sec)
  localGO <- localG(sf.nyc$price_m2, id.listw, zero.policy = T)
  sf.stats.none$local.go <- localGO
  tmap_mode("plot")
  pdf("/Users/samu_hugo/Desktop/Code/Plots/Statistics/map-getis-ord.pdf")
  tm_shape(sf.boroughs) + 
    tm_polygons(alpha = 0.1, border.alpha = 0.4) +
    tm_legend(title.size = 1.2, text.size = 0.8, position = c(0.05, 0.7)) + 
    tm_shape(sf.stats.none) +
    tm_layout(title = "Getis-Ord", frame = F, title.position = c("center", "top")) +
    tm_dots(col = "local.go", title = "Local Getis-Ord", palette = "plasma", 
            style = "order")
  dev.off()
toc()

# Save the descriptive statistics
rm(list = setdiff(ls(), grep('.stats', ls(), value = T)))
save.image(file = "/Users/samu_hugo/Desktop/Code/data-stats.RData")
