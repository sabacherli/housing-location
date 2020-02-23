# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# IMPORTANCE OF QUALITY AND VIEW
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------

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
# INLIERS
# ------------------------------------------------------------------------------

# Generalised additive model with spatial smoothing
form.gam <- as.formula(paste(dependent_variable.t, 
                             paste(c(categorical_variables,
                                     map_chr(numerical_variables_sum, 
                                             function(x) paste0("s(", x, ", k = 10, bs = 'cr')"))), 
                                   collapse = "+"), sep = "~"))
gam <- gam(form.gam, family = gaussian(link = identity), data = df.nyc)

# Remove all the outliers from the data set
df.nyc <- cbind(df.nyc, cooks_distance = cooks.distance(gam)) %>% 
  mutate(outlier_colour = ifelse(cooks_distance > 4 / nrow(df.nyc), "red", "black"),
         extreme_outlier_colour = ifelse(cooks_distance %in% sort(cooks_distance, decreasing = T)[1:500], "green", "black"),
         outlier = ifelse(cooks_distance < 4 / nrow(df.nyc), F, T))

df.inlier <- df.nyc %>% 
  filter(outlier == F)

sf.outlier <- df.nyc  %>% 
  st_as_sf(coords = c("X", "Y"), crs = 32118)

gam <- gam(form.gam, family = gaussian(link = identity), data = df.inlier)

# Check the significance of the explanatory variables
gam %>% summary()

# Model plots for diagnostics
pdf("/Users/samu_hugo/Desktop/Code/Plots/Results/map-inliers-diagnostics.pdf")
par(mfrow = c(2,2))
mgcv::gam.check(gam, type = "response", pch = 16, cex = 0.2, breaks = 1000)
dev.off()

# Recalculate mean absolute percentage error of the GAM
df.inlier$gam_fitted.t <- gam$fitted.values
df.inlier$gam_fitted <- bcUntransform(gam$fitted.values)
df.inlier$gam_residuals.t <- gam$residuals
df.inlier$gam_residuals <- df.inlier$price_m2 - df.inlier$gam_fitted
df.inlier$gam_ape.t <- abs(df.inlier$price_m2.t - df.inlier$gam_fitted.t) / df.inlier$price_m2.t
df.inlier$gam_ape <- abs(df.inlier$price_m2 - df.inlier$gam_fitted) / df.inlier$price_m2

mean(df.inlier$gam_ape)


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
df.prediction <- predict(gam, df.inlier, type = "terms") %>% 
  as.data.frame()
df.prediction %>% 
  head(5) %>% 
  t() %>% 
  kable("latex", caption = "Summary output of the locational, structural, and neighbourhood values with Queens and One family houses as categorical reference values and the respective median as numerical reference values.", booktabs = T) %>%
  kable_styling(latex_options = c("striped", "HOLD_position"))

# Calculate the contribution by variable type
sf.gam <- df.inlier %>% 
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

# Plot the outliers
tmap_mode("plot")
pdf("/Users/samu_hugo/Desktop/Code/Plots/Results/map-outliers.pdf")
tm_shape(sf.boroughs) +
  tm_polygons(alpha = 0.1, border.alpha = 0.4) +
  tm_layout(title = "Outlier values", title.size = 2, title.position = c(0.1, 0.95),
            frame = F, legend.hist.width = 0.25, legend.hist.height = 0.25) +
  tm_legend(title.size = 1.6, text.size = 1, position = c(0.1, 0.425)) +
  tm_shape(sf.outlier %>% filter(outlier == T)) +
  tm_dots(size = 0.065, col = "cooks_distance", title = "Cook's distance", alpha = 1, 
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

# Median APE
median(df.inlier$gam_ape)

# Summary of the meta variables
sf.untransformed %>% 
  as.data.frame() %>% 
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



# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# INCREASED ACCURACY BY ESF
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------

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
              "MASS", "nngeo", "purrr", "mgcv", "tidyselect",  # miscellaneous
              "spmoran", "tmap", "sf", "rgdal", "spdep", "dplyr", "plyr")  # fundamentals
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
# ESF
# ------------------------------------------------------------------------------

# Calculate moran's eigenvectors with different the best performing kernel
meig_f.exp <- meigen_f(st_coordinates(sf.nyc), "gau")
colnames(meig_f.exp$sf) <- as.character(paste0("sev", 1:ncol(meig_f.exp$sf)))
df.esf <- cbind(df.nyc, meig_f.exp$sf)

# Estimate the ESF model
form.esf <- as.formula(paste(dependent_variable.t, 
                             paste(c(colnames(meig_f.exp$sf), 
                                     categorical_variables, 
                                     map_chr(numerical_variables_sum, 
                                             function(x) paste0("s(", x, ", k = 10, bs = 'cr')"))), 
                                   collapse = "+"), sep = "~"))
esf <- mgcv::gam(form.esf, family = gaussian(), df.esf)

# Calculate mean absolute percentage error of the ESF GAM
df.esf$esf_fitted.t <- esf$fitted.values
df.esf$esf_fitted <- bcUntransform(esf$fitted.values)
df.esf$esf_residuals.t <- esf$residuals
df.esf$esf_residuals <- df.esf$price_m2 - df.esf$esf_fitted
df.esf$esf_ape.t <- abs(df.esf$price_m2.t - df.esf$esf_fitted.t) / df.esf$price_m2.t
df.esf$esf_ape <- abs(df.esf$price_m2 - df.esf$esf_fitted) / df.esf$price_m2

median(df.esf$esf_ape)
