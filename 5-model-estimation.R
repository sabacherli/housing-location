
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
packages <- c("tictoc", "pryr", "Metrics", "lmtest",  # profiling & testing
              "MASS", "nngeo", "purrr", "mgcv", "tidyselect",  # miscellaneous
              "knitr", "kableExtra",  # latex
              "mgcv", "spmoran", "tmap", "sf", "rgdal", "spdep", "dplyr", "plyr")  # fundamentals
ipack(packages) ; rm(ipack, packages)

# Specify the inverse Box-Cox transformation
bcUntransform <- function(yt, lambda = bc$lambda) {
  if (lambda == 0) {
    return(exp(y))
  } else {
    return(exp(log(lambda * yt + 1) / lambda))
  }
}

# Define a function to calculate the difference between the most valuable and least valuable observation per meta variables
minmax <- function(x) {
  for (j in 1:(length(x) - 1)) {
    print(paste(colnames(x)[j], "Max premium: ", (max(x[,j]) - median(x[,j])) / median(x[,j])))
    print(paste(colnames(x)[j], "Min discount: ", (min(x[,j]) - median(x[,j])) / median(x[,j])))
  }
}

iqr <- function(x) {
  for (j in 1:(length(x) - 1)) {
    print(paste(colnames(x)[j], "75th premium: ", (quantile(x[,j], 0.75) - median(x[,j])) / median(x[,j])))
    print(paste(colnames(x)[j], "25th discount: ", (quantile(x[,j], 0.25) - median(x[,j])) / median(x[,j])))
  }
}

# Import NYC boroughs and change to a equidistant projection with metres as units
sf.boroughs <- st_as_sf(readOGR(dsn = "/Users/samu_hugo/Desktop/Code/Data/boroughs/",
                                layer = "geo_export_a814b278-dd58-459f-b871-28e2dd794f22"),
                        crs = 4326) %>% 
  st_transform(32118)


# ------------------------------------------------------------------------------
# PREPERATION
# ------------------------------------------------------------------------------

# Load data and listw objects
load(file = "/Users/samu_hugo/Desktop/Code/data-variables.RData")
load(file = "/Users/samu_hugo/Desktop/Code/data-specification.RData")
load(file = "/Users/samu_hugo/Desktop/Code/data-boxcox.RData")
load(file = "/Users/samu_hugo/Desktop/Code/data-listw.RData")

# Specify variables of interest
numerical_variables_sum <- c("age", "math", "pc_neighbourhood",
                             "sum_n_rcb", "d_subway", "d_railway", "d_airport",
                             "d_highway", "d_mainroad","d_exchange", "d_highschool",
                             "d_supermarket", "d_convenience", "d_fitness", "d_cinema", 
                             "d_playground", "a_park", "d_waterfront", "d_shopping")
categorical_variables <- c("building_class")
dependent_variable <- c("price_m2")
dependent_variable.t <- c("price_m2.t")

# # Subset the data
# lower <- 0
# upper <- 300000
# sf.nyc <- sf.nyc %>% filter(price_m2 < upper & price_m2 > lower)
# df.nyc <- df.nyc %>% filter(price_m2 < upper & price_m2 > lower)


# Determine levels with most observations for reference levels
df.nyc %>%
  select_(.dots = categorical_variables) %>%
  map(., function(x) count(x)) %>%
  lapply(., function(x) as.data.frame(x) %>% top_n(1, freq))

# Scale the numerical values for appropriate interpretation of the intercept
df.nyc <- df.nyc %>%
  mutate_at(vars(numerical_variables_sum), ~ scale(., center = T, scale = F)) %>%
  mutate(district = as.factor(district),
         zip = relevel(zip, ref = "11234"),
         borough = relevel(borough, ref = "Queens"),
         neighbourhood = relevel(neighbourhood, ref = "FLUSHING-NORTH"),
         district = relevel(district, ref = 27),
         building_class = relevel(building_class, ref = "A"))

# ------------------------------------------------------------------------------
# GAM
# ------------------------------------------------------------------------------

# Generalised additive model with spatial smoothing
form.gam <- as.formula(paste(dependent_variable.t, 
                             paste(c(categorical_variables,
                                     map_chr(numerical_variables_sum, 
                                             function(x) paste0("s(", x, ", k = 10, bs = 'cr')"))), 
                                   collapse = "+"), sep = "~"))
gam <- gam(form.gam, family = gaussian(link = identity), data = df.nyc)

# Check the significance of the explanatory variables
gam %>% summary()

# Model plots for diagnostics
pdf("/Users/samu_hugo/Desktop/Code/Plots/Results/map-results-diagnostics.pdf")
par(mfrow = c(2,2))
mgcv::gam.check(gam, type = "response", pch = 16, cex = 0.2, breaks = 1000)
dev.off()

# Check spatial correlation remaining in the residuals
moran.test(gam$residuals, epa.listw)

# Recalculate mean absolute percentage error of the GAM
df.nyc$gam_fitted.t <- gam$fitted.values
df.nyc$gam_fitted <- bcUntransform(gam$fitted.values)
df.nyc$gam_residuals.t <- gam$residuals
df.nyc$gam_residuals <- df.nyc$price_m2 - df.nyc$gam_fitted
df.nyc$gam_ape.t <- abs(df.nyc$price_m2.t - df.nyc$gam_fitted.t) / df.nyc$price_m2.t
df.nyc$gam_ape <- abs(df.nyc$price_m2 - df.nyc$gam_fitted) / df.nyc$price_m2

median(df.nyc$gam_ape)


# ------------------------------------------------------------------------------
# LOCATION ESTIMATION
# ------------------------------------------------------------------------------

# Calculate the locational price
locational_variables <- c("math", "sum_n_rcb", "d_subway", "d_railway", "d_airport", 
                          "d_highway", "d_mainroad", "d_exchange", "d_highschool", 
                          "d_supermarket", "d_convenience", "d_fitness", "d_cinema", 
                          "d_playground", "a_park", "d_waterfront", "d_shopping") %>% 
  map_chr(function(x) paste0("'s(", x, ")'"))
neighbourhood_variables <- c("'s(pc_neighbourhood)'")
structural_variables <- c("building_class", "'s(age)'")

# Predict the variable values by term
df.prediction <- predict(gam, df.nyc, type = "terms") %>% 
  as.data.frame()
df.prediction %>% 
  head(5) %>% 
  t() %>% 
  kable("latex", caption = "Summary output of the locational, structural, and neighbourhood values with Queens and One family houses as categorical reference values and the respective median as numerical reference values.", booktabs = T) %>%
  kable_styling(latex_options = c("striped", "HOLD_position"))

# Calculate the contribution by variable type
sf.gam <- df.nyc %>% 
  mutate(locational_value = df.prediction %>% select_(.dots = locational_variables) %>% rowSums(),
         neighbourhood_value = df.prediction %>% select_(.dots = neighbourhood_variables) %>% rowSums(),
         structural_value = df.prediction %>% select_(.dots = structural_variables) %>% rowSums(),
         intercept_value = gam$coefficients[1]) %>% 
  st_as_sf(coords = c("X", "Y"), crs = 32118)

# Untransformed variable space
sf.untransformed <- sf.gam %>% 
  transmute(locational_value = bcUntransform(locational_value + intercept_value),
         neighbourhood_value = bcUntransform(neighbourhood_value + intercept_value),
         structural_value = bcUntransform(structural_value + intercept_value),
         observed_value = price_m2,
         predicted_value = gam_fitted,
         residual_value = gam_residuals)


# ------------------------------------------------------------------------------
# PLOTTING
# ------------------------------------------------------------------------------

# Plot the individual variable categories
tmap_mode("plot")
pdf("/Users/samu_hugo/Desktop/Code/Plots/Results/map-results-locational-value.pdf")
# Locational value
tm_shape(sf.boroughs) +
  tm_polygons(alpha = 0.1, border.alpha = 0.4) +
  tm_layout(title = "Locational values", title.size = 2, title.position = c(0.1, 0.95),
            frame = F, legend.hist.width = 0.25, legend.hist.height = 0.25) +
  tm_legend(title.size = 1.6, text.size = 1, position = c(0.1, 0.425)) +
  tm_shape(sf.untransformed) +
  tm_dots(size = 0.065, col = "locational_value",  title = "Price per sqm", alpha = 1, 
          style = "quantile", border.lwd = NA, border.col = NA, legend.hist = T,
          palette = viridis::magma(n = 5, 
                                   begin = 0.1, 
                                   end = 0.9, 
                                   direction = -1))
dev.off()

# Neighbourhood value
pdf("/Users/samu_hugo/Desktop/Code/Plots/Results/map-results-neighbourhood-value.pdf")
tm_shape(sf.boroughs) +
  tm_polygons(alpha = 0.1, border.alpha = 0.4) +
  tm_layout(title = "Neighbourhood values", title.size = 2, title.position = c(0.1, 0.95),
            frame = F, legend.hist.width = 0.25, legend.hist.height = 0.25) +
  tm_legend(title.size = 1.6, text.size = 1, position = c(0.1, 0.425)) +
  tm_shape(sf.untransformed) +
  tm_dots(size = 0.065, col = "neighbourhood_value",  title = "Price per sqm", alpha = 1, 
          style = "quantile", border.lwd = NA, border.col = NA, legend.hist = T,
          palette = viridis::magma(n = 5, 
                                   begin = 0.1, 
                                   end = 0.9, 
                                   direction = -1))
dev.off()

# Structural value
pdf("/Users/samu_hugo/Desktop/Code/Plots/Results/map-results-structural-value.pdf")
tm_shape(sf.boroughs) +
  tm_polygons(alpha = 0.1, border.alpha = 0.4) +
  tm_layout(title = "Structural values", title.size = 2, title.position = c(0.1, 0.95),
            frame = F, legend.hist.width = 0.25, legend.hist.height = 0.25) +
  tm_legend(title.size = 1.6, text.size = 1, position = c(0.1, 0.425)) +
  tm_shape(sf.untransformed) +
  tm_dots(size = 0.065, col = "structural_value",  title = "Price per sqm", alpha = 1, 
          style = "quantile", border.lwd = NA, border.col = NA, legend.hist = T,
          palette = viridis::magma(n = 5, 
                                   begin = 0.1, 
                                   end = 0.9, 
                                   direction = -1))
dev.off()

# Price value
pdf("/Users/samu_hugo/Desktop/Code/Plots/Results/map-results-observed-value.pdf")
tm_shape(sf.boroughs) +
  tm_polygons(alpha = 0.1, border.alpha = 0.4) +
  tm_layout(title = "Observed values", title.size = 2, title.position = c(0.1, 0.95),
            frame = F, legend.hist.width = 0.25, legend.hist.height = 0.25) +
  tm_legend(title.size = 1.6, text.size = 1, position = c(0.1, 0.425)) +
  tm_shape(sf.untransformed) +
  tm_dots(size = 0.065, col = "observed_value", title = "Price per sqm", alpha = 1, 
          style = "quantile", border.lwd = NA, border.col = NA, legend.hist = T,
          palette = viridis::magma(n = 5, 
                                   begin = 0.1, 
                                   end = 0.9, 
                                   direction = -1))
dev.off()

# Predicted value
pdf("/Users/samu_hugo/Desktop/Code/Plots/Results/map-results-predicted-value.pdf")
tm_shape(sf.boroughs) +
  tm_polygons(alpha = 0.1, border.alpha = 0.4) +
  tm_layout(title = "Predicted values", title.size = 2, title.position = c(0.1, 0.95),
            frame = F, legend.hist.width = 0.25, legend.hist.height = 0.25) +
  tm_legend(title.size = 1.6, text.size = 1, position = c(0.1, 0.425)) +
  tm_shape(sf.untransformed) +
  tm_dots(size = 0.065, col = "predicted_value", title = "Price per sqm", alpha = 1, 
          style = "quantile", border.lwd = NA, border.col = NA, legend.hist = T,
          palette = viridis::magma(n = 5, 
                                   begin = 0.1, 
                                   end = 0.9, 
                                   direction = -1))
dev.off()

# Residuals
pdf("/Users/samu_hugo/Desktop/Code/Plots/Results/map-results-residual-value.pdf")
tm_shape(sf.boroughs) +
  tm_polygons(alpha = 0.1, border.alpha = 0.4) +
  tm_layout(title = "Residual values", title.size = 2, title.position = c(0.1, 0.95),
            frame = F, legend.hist.width = 0.25, legend.hist.height = 0.25) +
  tm_legend(title.size = 1.6, text.size = 1, position = c(0.1, 0.425)) +
  tm_shape(sf.untransformed) +
  tm_dots(size = 0.065, col = "residual_value", title = "Price per sqm", alpha = 1, 
          style = "quantile", border.lwd = NA, border.col = NA, legend.hist = T,
          palette = viridis::magma(n = 5, 
                                   begin = 0.1, 
                                   end = 0.9, 
                                   direction = -1))
dev.off()


# ------------------------------------------------------------------------------
# RESULTS
# ------------------------------------------------------------------------------

# Check the significance of the explanatory variables
gam %>% summary()

# Check spatial correlation remaining in the residuals
moran.test(gam$residuals, epa.listw)

# Median and mean APE
median(df.nyc$gam_ape)
mean(df.nyc$gam_ape)

# Summary of the meta variables
sf.untransformed %>% 
  as.data.frame() %>% 
  select(-geometry) %>% 
  summary() %>% 
  kable("latex", caption = "Summary output of the locational, structural, and neighbourhood values with Queens and One family houses as categorical reference values and the respective median as numerical reference values.", booktabs = T) %>%
  kable_styling(latex_options = c("striped", "HOLD_position"))

# Calculate the respective premiums of the meta variables
sf.untransformed %>% 
  as.data.frame() %>% 
  minmax()

# Calculate the respective premiums of the meta variables
sf.untransformed %>% 
  as.data.frame() %>% 
  iqr()

# Fit an ordinary least squares regression with the aggregated values
summary(lm(observed_value ~ locational_value, data = sf.untransformed))
