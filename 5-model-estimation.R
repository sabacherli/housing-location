
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
packages <- c("tictoc", "pryr", "Metrics",  # profiling & testing
              "MASS", "nngeo", "purrr", "mgcv", # miscellaneous
              "spmoran", "tmap", "sf", "rgdal", "spdep", "dplyr")  # fundamentals
ipack(packages) ; rm(ipack, packages)

# Specify the inverse Box-Cox transformation
bcUntransform <- function(yt, lambda = bc$lambda) {
  if (lambda == 0) {
    return(exp(y))
  } else {
    return(exp(log(lambda * yt + 1) / lambda))
  }
}


# ------------------------------------------------------------------------------
# PREPERATION
# ------------------------------------------------------------------------------

# Load data and listw objects
load(file = "/Users/samu_hugo/Desktop/Code/data-variables.RData")
load(file = "/Users/samu_hugo/Desktop/Code/data-specification.RData")
load(file = "/Users/samu_hugo/Desktop/Code/data-boxcox.RData")
# load(file = "/Users/samu_hugo/Desktop/Code/data-listw.RData")

# cutoff <- 20000
# sf.nyc <- sf.nyc %>% filter(price_m2 < cutoff)
# df.nyc <- df.nyc %>% filter(price_m2 < cutoff)

# Specify variables of interest
numerical_variables_sum <- c("age", "math", "n_crime", "n_noise", 
                           "sum_n_rcb", "d_subway", "d_railway", 
                           "d_highway", "d_mainroad","d_exchange", "d_highschool",
                           "d_supermarket", "d_convenience", "d_fitness", "d_cinema", 
                           "d_playground", "a_park", "d_waterfront", "d_shopping")
categorical_variables <- c("neighbourhood", "building_class")
dependent_variable <- c("price_m2")
dependent_variable.t <- c("price_m2.t")

# # Determine levels with most observations for reference levels
df.nyc %>%
  select_(.dots = categorical_variables) %>%
  map(., function(x) count(x)) %>%
  lapply(., function(x) as.data.frame(x) %>% top_n(1, freq))

# Scale the numerical values for appropriate interpretation of the intercept
df.nyc <- df.nyc %>%
  mutate_at(vars(numerical_variables_sum), ~ scale(., center = T, scale = F)) %>%
  mutate(neighbourhood = relevel(neighbourhood, ref = "FLUSHING-NORTH"),
         building_class = relevel(building_class, ref = "A"))


# ------------------------------------------------------------------------------
# OLS
# ------------------------------------------------------------------------------

# Ordinary least squares
tic("OLS estimation")
  ols.form <- as.formula(paste(dependent_variable.t, 
                               paste(c(numerical_variables_sum, categorical_variables), 
                                     collapse = "+"), sep = "~"))
  
  ols <- lm(ols.form, df.nyc)
toc()
options(max.print = 100) ; summary(ols)
pdf("/Users/samu_hugo/Desktop/Code/Plots/plot-ols-check.pdf")
  par(mfrow = c(2,2)) ; plot(ols, pch = 16, col = rgb(0,0,0,0.1))
dev.off()

# Calculate mean absolute percentage error of OLS
df.nyc$ols_fitted.t <- ols$fitted.values
df.nyc$ols_fitted <- bcUntransform(ols$fitted.values)
df.nyc$ols_residuals.t <- ols$residuals
df.nyc$ols_residuals <- df.nyc$price_m2 - df.nyc$ols_fitted
df.nyc$ols_ape.t <- abs(df.nyc$price_m2.t - df.nyc$ols_fitted.t) / df.nyc$price_m2.t
df.nyc$ols_ape <- abs(df.nyc$price_m2 - df.nyc$ols_fitted) / df.nyc$price_m2

mean(df.nyc$ols_ape) ; mean(df.nyc$ols_ape.t) ; median(df.nyc$ols_ape) ; median(df.nyc$ols_ape.t)


# ------------------------------------------------------------------------------
# GAM
# ------------------------------------------------------------------------------

# Generalised additive model (~2min)
tic("GAM estimation")
  gam.form <- as.formula(paste(dependent_variable.t, 
                               paste(c(categorical_variables, 
                                       map_chr(numerical_variables_sum, 
                                               function(x) paste0("s(", x, ", bs = 'ps')"))), 
                                     collapse = "+"), sep = "~"))
  gam <- mgcv::gam(gam.form, family = gaussian(link = identity), data = df.nyc)
toc()

# Calculate mean absolute percentage error of the GAM
df.nyc$gam_fitted.t <- gam$fitted.values
df.nyc$gam_fitted <- bcUntransform(gam$fitted.values)
df.nyc$gam_residuals.t <- gam$residuals
df.nyc$gam_residuals <- df.nyc$price_m2 - df.nyc$gam_fitted
df.nyc$gam_ape.t <- abs(df.nyc$price_m2.t - df.nyc$gam_fitted.t) / df.nyc$price_m2.t
df.nyc$gam_ape <- abs(df.nyc$price_m2 - df.nyc$gam_fitted) / df.nyc$price_m2

mean(df.nyc$gam_ape) ; mean(df.nyc$gam_ape.t) ; median(df.nyc$gam_ape) ; median(df.nyc$gam_ape.t)

# Model plots for diagnostics
par(mfrow = c(2,2))
mgcv::gam.check(gam, type = "response")


# ------------------------------------------------------------------------------
# ESF
# ------------------------------------------------------------------------------

# Calculate moran's eigenvectors with different the best performing kernel
meig_f.exp <- meigen_f(st_coordinates(sf.nyc), "exp")
colnames(meig_f.exp$sf) <- as.character(paste0("sev", 1:ncol(meig_f.exp$sf)))
esf.df.exp <- cbind(df.nyc, meig_f.exp$sf)

# Create the GAM form with the best performing moran eigenvector kernel smoother (see previous script)
esf.exp.gam.form <- as.formula(paste(dependent_variable.t, 
                                     paste(c(colnames(meig_f.exp$sf), 
                                             categorical_variables, 
                                             map_chr(numerical_variables_sum, 
                                                     function(x) paste0("s(", x, ", bs = 'ps')"))), 
                                           collapse = "+"), sep = "~"))

# Estimate the ESF model
esf.exp.gam <- mgcv::gam(esf.exp.gam.form, family = gaussian(), esf.df.exp)

# Calculate mean absolute percentage error of the ESF GAM
df.nyc$esf_fitted.t <- esf.exp.gam$fitted.values
df.nyc$esf_fitted <- bcUntransform(esf.exp.gam$fitted.values)
df.nyc$esf_residuals.t <- esf.exp.gam$residuals
df.nyc$esf_residuals <- df.nyc$price_m2 - df.nyc$esf_fitted
df.nyc$esf_ape.t <- abs(df.nyc$price_m2.t - df.nyc$esf_fitted.t) / df.nyc$price_m2.t
df.nyc$esf_ape <- abs(df.nyc$price_m2 - df.nyc$esf_fitted) / df.nyc$price_m2

mean(df.nyc$esf_ape) ; mean(df.nyc$esf_ape.t) ; median(df.nyc$esf_ape) ; median(df.nyc$esf_ape.t)

# Model plots for diagnostics
par(mfrow = c(2,2))
mgcv::gam.check(esf.exp.gam, type = "response")

# ESF OUTLIERS
# ------------------------------------------------------------------------------

# Take a closer look at the outliers
sf.outliers <- cbind(df.nyc, cooks_distance = cooks.distance(esf.exp.gam)) %>% 
  mutate(outlier_colour = ifelse(cooks_distance > 4 / nrow(df.nyc), "red", "black"),
         extreme_outlier_colour = ifelse(cooks_distance %in% sort(cooks_distance, decreasing = T)[1:500], "green", "black"),
         outlier = ifelse(cooks_distance < 4 / nrow(df.nyc), F, T)) %>% 
  st_as_sf(coords = c("X", "Y"), crs = 32118)

par(mfrow = c(1,1))
hist(sf.outliers %>% 
       filter(outlier == T) %>% 
       as.data.frame() %>% 
       dplyr::select(price_m2) %>% 
       t() %>% 
       as.vector(), 
     20000, xlim = c(0, 5000), main = "Histogram of Cook's Distance outliers", xlab = "Price per sqm (cut off)")

# Import NYC boroughs and change to a equidistant projection with metres as units
sf.boroughs <- st_as_sf(readOGR(dsn = "/Users/samu_hugo/Desktop/Code/Data/boroughs/",
                                layer = "geo_export_a814b278-dd58-459f-b871-28e2dd794f22"),
                        crs = 4326) %>% 
  st_transform(32118)

# Check the spatial distribution of the prices
tmap_mode("plot")
pdf("/Users/samu_hugo/Desktop/Code/Plots/map-outliers.pdf")
tm_shape(sf.boroughs) +
  tm_polygons(alpha = 0.1, border.alpha = 0.4) +
  tm_layout(title = "2018 Outliers, NYC", title.position = c("left", "top"),
            frame = F) +
  tm_legend(title.size = 1.2, text.size = 0.8, position = c(0.05, 0.7)) +
  tm_shape(sf.outliers %>% filter(outlier == T)) +
  tm_dots(size = 0.1, col = "cooks_distance", title = "Cooks distance", alpha = 0.4, 
          style = "quantile", border.lwd = NA, border.col = NA,
          palette = viridis::magma(n = 5, 
                                   begin = 0.1, 
                                   end = 0.9, 
                                   direction = -1))
dev.off()

# Remove outliers
meig_f.inlier <- meigen_f(st_coordinates(sf.outliers %>% filter(outlier == F)), "exp")
colnames(meig_f.inlier$sf) <- as.character(paste0("sev", 1:ncol(meig_f.inlier$sf)))
esf.sf.inlier <- cbind(sf.outliers %>% filter(outlier == F), meig_f.inlier$sf)
esf.exp.inliers <- mgcv::gam(esf.exp.gam.form, family = gaussian(), esf.sf.inlier)

# Calculate mean absolute percentage error of the ESF GAM
sf.inliers <- sf.outliers %>% filter(outlier == F)
sf.inliers$inliers_fitted.t <- esf.exp.inliers$fitted.values
sf.inliers$inliers_fitted <- bcUntransform(esf.exp.inliers$fitted.values)
sf.inliers$inliers_residuals.t <- esf.exp.inliers$residuals
sf.inliers$inliers_residuals <- sf.inliers$price_m2 - sf.inliers$inliers_fitted
sf.inliers$inliers_ape.t <- abs(sf.inliers$price_m2.t - sf.inliers$esf_fitted.t) / sf.inliers$price_m2.t
sf.inliers$inliers_ape <- abs(sf.inliers$price_m2 - sf.inliers$esf_fitted) / sf.inliers$price_m2

mean(sf.inliers$inliers_ape) ; mean(sf.inliers$inliers_ape.t) ; median(sf.inliers$inliers_ape) ; median(sf.inliers$inliers_ape.t)

# Model plots for diagnostics
pdf("/Users/samu_hugo/Desktop/Code/Plots/plot-outlier-residuals.pdf")
  par(mfrow = c(2,2))
  mgcv::gam.check(esf.exp.inliers, type = "response")
dev.off()


# ------------------------------------------------------------------------------
# LOCATION ESTIMATION
# ------------------------------------------------------------------------------

# Calculate the locational price
locational_variables <- c("d_highschool", "math", "d_subway", "d_cinema", "d_fitness", 
                          "a_park", "d_playground", "d_railway", "d_shopping", 
                          "d_supermarket", "d_convenience", "d_exchange",
                          "d_highway", "d_mainroad", "d_waterfront",  "sum_n_rcb") %>% 
  map_chr(function(x) paste0("'s(", x, ")'"))
neighbourhood_variables <- c("'s(n_noise)'", "'s(n_crime)'", "neighbourhood")
structural_variables <- c("building_class", "'s(age)'")
eigenvector_variables <- paste0("sev", 1:200)

# Predict the variable values by term
df.prediction <- predict(esf.exp.inliers, esf.sf.inlier, type = "terms") %>% 
  as.data.frame()

# Calculate the contribution by variable type
sf.inliers <- sf.inliers %>% 
  mutate(locational_value = df.prediction %>% select_(.dots = locational_variables) %>% rowSums(),
         neighbourhood_value = df.prediction %>% select_(.dots = neighbourhood_variables) %>% rowSums(),
         structural_value = df.prediction %>% select_(.dots = structural_variables) %>% rowSums(),
         eigenvector_value = df.prediction %>% select_(.dots = eigenvector_variables) %>% rowSums(),
         intercept_value = esf.exp.inliers$coefficients[1]) %>% 
  st_as_sf(coords = c("X", "Y"), crs = 32118)

sf.inliers %>% 
  as.data.frame() %>% 
  dplyr::select(vars_select(names(sf.inliers), ends_with('_value'))) %>% 
  summary()

# Plot the individual variable categories
tmap_mode("plot")
pdf("/Users/samu_hugo/Desktop/Code/Plots/map-meta-value.pdf")
# Locational value
tm_shape(sf.boroughs) +
  tm_polygons(alpha = 0.1, border.alpha = 0.4) +
  tm_layout(title = "2018 Locational Value, NYC", title.position = c("left", "top"),
            frame = F) +
  tm_legend(title.size = 1.2, text.size = 0.8, position = c(0.05, 0.7)) +
  tm_shape(sf.inliers) +
  tm_dots(size = 0.1, col = "locational_value", title = "Locational value", alpha = 0.4, 
          style = "quantile", border.lwd = NA, border.col = NA,
          palette = viridis::magma(n = 5, 
                                   begin = 0.1, 
                                   end = 0.9, 
                                   direction = -1))

# Neighbourhood value
tm_shape(sf.boroughs) +
  tm_polygons(alpha = 0.1, border.alpha = 0.4) +
  tm_layout(title = "2018 Neighbourhood Value, NYC", title.position = c("left", "top"),
            frame = F) +
  tm_legend(title.size = 1.2, text.size = 0.8, position = c(0.05, 0.7)) +
  tm_shape(sf.inliers) +
  tm_dots(size = 0.1, col = "neighbourhood_value", title = "Neighbourhood value", alpha = 0.4, 
          style = "quantile", border.lwd = NA, border.col = NA,
          palette = viridis::magma(n = 5, 
                                   begin = 0.1, 
                                   end = 0.9, 
                                   direction = -1))

# Structural value
tm_shape(sf.boroughs) +
  tm_polygons(alpha = 0.1, border.alpha = 0.4) +
  tm_layout(title = "2018 Structural Value, NYC", title.position = c("left", "top"),
            frame = F) +
  tm_legend(title.size = 1.2, text.size = 0.8, position = c(0.05, 0.7)) +
  tm_shape(sf.inliers) +
  tm_dots(size = 0.1, col = "structural_value", title = "Structural value", alpha = 0.4, 
          style = "quantile", border.lwd = NA, border.col = NA,
          palette = viridis::magma(n = 5, 
                                   begin = 0.1, 
                                   end = 0.9, 
                                   direction = -1))

# Eigenvector value
tm_shape(sf.boroughs) +
  tm_polygons(alpha = 0.1, border.alpha = 0.4) +
  tm_layout(title = "2018 Eigenvector Value, NYC", title.position = c("left", "top"),
            frame = F) +
  tm_legend(title.size = 1.2, text.size = 0.8, position = c(0.05, 0.7)) +
  tm_shape(sf.inliers) +
  tm_dots(size = 0.1, col = "eigenvector_value", title = "Eigenvector value", alpha = 0.4, 
          style = "quantile", border.lwd = NA, border.col = NA,
          palette = viridis::magma(n = 5, 
                                   begin = 0.1, 
                                   end = 0.9, 
                                   direction = -1))

# Price value
tm_shape(sf.boroughs) +
  tm_polygons(alpha = 0.1, border.alpha = 0.4) +
  tm_layout(title = "2018 Price per sqm, NYC", title.position = c("left", "top"),
            frame = F) +
  tm_legend(title.size = 1.2, text.size = 0.8, position = c(0.05, 0.7)) +
  tm_shape(sf.inliers) +
  tm_dots(size = 0.1, col = "price_m2", title = "Eigenvector value", alpha = 0.4, 
          style = "quantile", border.lwd = NA, border.col = NA,
          palette = viridis::magma(n = 5, 
                                   begin = 0.1, 
                                   end = 0.9, 
                                   direction = -1))
dev.off()










# Create a data frame setting all locational values to their respective medians
df.median <- df.nyc %>%
  select(c(locational_variables, neighbourhood_variables, structural_variables)) %>% 
  mutate_at(vars(numerical_variables_sum), ~ median(.)) %>%
  mutate_at(vars(categorical_variables), ~ levels(.)[1]) 

# Create a data frame setting all locational values to their respective medians
df.median_location <- cbind(df.nyc, meig_f.exp$sf) %>%
  mutate_at(vars(c(locational_variables)), ~ median(.))

# Create a data frame with all locational variables set to their respective mean
sf.median_location <- df.nyc %>% 
  mutate(structural_value.t = predict(esf.exp.gam, df.median_location),
         neighbourhood_value.t = esf_fitted.t - structural_value.t - locational_value.t,
         locational_value.t = (esf_fitted.t -  - structural_value.t - neighbourhood_value.t)) %>% 
  st_as_sf(coords = c("X", "Y"), crs = 32118)

moran.test(sf.location_average$location_quality, listw = id.listw)

# Import NYC boroughs and change to a equidistant projection with metres as units
sf.boroughs <- st_as_sf(readOGR(dsn = "/Users/samu_hugo/Desktop/Code/Data/boroughs/",
                                layer = "geo_export_a814b278-dd58-459f-b871-28e2dd794f22"),
                        crs = 4326) %>% 
  st_transform(32118)

# Check the spatial distribution of the prices
tmap_mode("plot")
pdf("/Users/samu_hugo/Desktop/Code/Plots/map-locational-value.pdf")
  tm_shape(sf.boroughs) +
    tm_polygons(alpha = 0.1, border.alpha = 0.4) +
    tm_layout(title = "2018 Locational Value, NYC", title.position = c("left", "top"),
              frame = F) +
    tm_legend(title.size = 1.2, text.size = 0.8, position = c(0.05, 0.7)) +
    tm_shape(sf.nyc) +
    tm_dots(size = 0.1, col = "eigenvector_value", title = "Locational value in percentage", alpha = 0.4, 
            style = "quantile", border.lwd = NA, border.col = NA,
            palette = viridis::magma(n = 5, 
                                     begin = 0.1, 
                                     end = 0.9, 
                                     direction = -1))
dev.off()

hist(sf.location_average$location_quality, 1000, main = "Histogram of location quality")
hist(sf.location_average$mean_location.t, 1000, main = "Histogram of build quality")
hist(sf.location_average$price_m2.t, 1000, main = "Histogram of house prices")



price_m2.t ~ sf.location_average$location_quality