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
packages <- c("tictoc", "pryr", "lmtest", "car", "normtest",  # profiling & testing
              "Matrix",  "MASS", "tidyr", "ggplot2", "purrr",  # miscellaneous
              "rgdal", "spmoran", "spdep", "sf", "dplyr", "tmap")  # fundamentals
ipack(packages) ; rm(ipack, packages)
mem_used()


# ------------------------------------------------------------------------------
# MULTICOLLINEARITY
# ------------------------------------------------------------------------------

# Load matrix
load(file = "/Users/samu_hugo/Desktop/Code/data-variables.RData")

# Specify variables of interest
numeric_variables <- c("age", "math", "reading", "writing",  "SO2", "NOx", "PM2.5", 
                       "n_crime", "n_noise", "n_restaurants", "n_cafes", "n_bars", 
                       "d_airport", "d_subway", "d_railway", "d_highway", 
                       "d_highway_link", "d_trunkroad", "d_mainroad","d_exchange", 
                       "d_supermarket", "d_convenience", "d_fitness", "d_cinema", 
                       "d_playground", "a_park", "d_waterfront", "d_shopping")
categorical_variables <- c("neighbourhood", "building_class")
dependent_variable <- c("price_m2")

# Check which explanatory variables are highly correlated
sf.nyc %>% 
  as.data.frame() %>% 
  select_(.dots = numeric_variables) %>% 
  cor() %>% 
  as.data.frame() %>% 
  pivot_longer(cols = everything(), names_to = "variable2", values_to = "correlation") %>% 
  mutate(variable1 = rep(numeric_variables, each = length(numeric_variables))) %>% 
  select(variable1, variable2, correlation) %>% 
  mutate(correlation = abs(correlation)) %>% 
  filter(correlation != 1) %>% 
  arrange(desc(correlation)) %>% 
  filter(row_number() %% 2 == 0) %>% 
  View("Correlation Matrix")

# Create principle components for the correlated variable groups
F1 <- sf.nyc %>% 
  as.data.frame() %>% 
  select(math, reading, writing) %>% 
  prcomp()
summary(F1)
F1$rotation

F2 <- sf.nyc %>% 
  as.data.frame() %>%  
  select(NOx, PM2.5, SO2) %>% 
  prcomp()
summary(F2)
F2$rotation

F3 <- sf.nyc %>% 
  as.data.frame() %>%  
  select(n_bars, n_cafes, n_restaurants) %>% 
  prcomp()
summary(F3)
F3$rotation

F4 <- sf.nyc %>% 
  as.data.frame() %>%  
  select(d_highway, d_highway_link) %>% 
  prcomp()
summary(F4)
F4$rotation

# Add the principle components to the data set and aggregate restaurants, bars, cafes as an alternative approach
df.nyc <- sf.nyc %>% 
  as.data.frame() %>% 
  mutate(pc_sat = F1$x[, 1],
         pc_d_highways = F4$x[, 1],
         pc_n_rcb = F3$x[, 1],
         pc_air = F2$x[, 1],
         sum_n_rcb = n_restaurants + n_cafes + n_bars) %>% 
  cbind(st_coordinates(sf.nyc)) ; rm(F1, F2, F3, F4)

# Specify variables of interest
numeric_variables_pca <- c("age", "pc_sat", "pc_air", "n_crime", "n_noise", 
                           "pc_n_rcb", "d_airport", "d_subway", "d_railway", 
                           "pc_d_highways", "d_mainroad","d_exchange", 
                           "d_supermarket", "d_convenience", "d_fitness", "d_cinema", 
                           "d_playground", "a_park", "d_waterfront", "d_shopping")

numeric_variables_sum <- c("age", "math", "PM2.5", "n_crime", "n_noise", 
                           "sum_n_rcb", "d_airport", "d_subway", "d_railway", 
                           "d_highway", "d_mainroad","d_exchange", 
                           "d_supermarket", "d_convenience", "d_fitness", "d_cinema", 
                           "d_playground", "a_park", "d_waterfront", "d_shopping")

# Check if the multicollinearity has improved with the PCA approach
df.nyc %>% 
  as.data.frame() %>% 
  select_(.dots = numeric_variables_pca) %>% 
  cor() %>% 
  as.data.frame() %>% 
  pivot_longer(cols = everything(), names_to = "variable2", values_to = "correlation") %>% 
  mutate(variable1 = rep(numeric_variables_pca, each = length(numeric_variables_pca))) %>% 
  select(variable1, variable2, correlation) %>% 
  mutate(correlation = abs(correlation)) %>% 
  filter(correlation != 1) %>% 
  arrange(desc(correlation)) %>% 
  filter(row_number() %% 2 == 0) %>% 
  View("Correlation Matrix")

# Check if the multicollinearity has improved with the SUM approach
df.nyc %>% 
  as.data.frame() %>% 
  select_(.dots = numeric_variables_sum) %>% 
  cor() %>% 
  as.data.frame() %>% 
  pivot_longer(cols = everything(), names_to = "variable2", values_to = "correlation") %>% 
  mutate(variable1 = rep(numeric_variables_sum, each = length(numeric_variables_sum))) %>% 
  select(variable1, variable2, correlation) %>% 
  mutate(correlation = abs(correlation)) %>% 
  filter(correlation != 1) %>% 
  arrange(desc(correlation)) %>% 
  filter(row_number() %% 2 == 0) %>% 
  View("Correlation Matrix")

# Analyse the unexpected positive correlation between subway and airport, and subway and fitness centres
sf.cor <- df.nyc %>% 
  mutate(color = ifelse(borough == "Staten Island", "red", "blue"))
plot(sf.cor$d_airport, sf.cor$d_subway, col = sf.cor$col, pch = 19, 
     main = "Scatterplot", 
     xlab = "Airport distance", ylab = "Subway distance")
legend(10, 200, legend = c("Staten Island", "Rest of New York City"), 
       box.col = "white", pch = 19, col = c("red", "blue")) ; rm(sf.cor)

# Drop both distance to airport and air pollution, the latter due to collinearity
# with population density and consequently the number of restaurant, cafes, and bars
numeric_variables_sum <- c("age", "math", "n_crime", "n_noise", 
                           "sum_n_rcb", "d_subway", "d_railway", 
                           "d_highway", "d_mainroad","d_exchange",
                           "d_supermarket", "d_convenience", "d_fitness", "d_cinema", 
                           "d_playground", "a_park", "d_waterfront", "d_shopping")


# ------------------------------------------------------------------------------
# HETEROSCEDASTICITY (BOX-COX TRANSFORMATION)
# ------------------------------------------------------------------------------

# Determine the functional form
ols.form <- as.formula(paste(dependent_variable, 
                             paste(c(numeric_variables_sum, categorical_variables), 
                                   collapse = "+"), sep = "~"))

# Estimate an OLS model
ols <- lm(ols.form, df.nyc)

# Test if errors have heteroskedasticity
# ··············································································

# Calculate a Breush-Pagan test
bptest(ols)

# Calculate a non constant variance test
ncvTest(ols)

# Plot the residuals versus the levels
plot(ols$fitted.values, ols$residuals, xlab = "Fitted values", ylab = "Residuals", 
     main = "Heteroscedasticity without Box-Cox transformation")

# Test if the errors are normally distributed
# ··············································································

# Plot the density of residuals
plot(density(ols$residuals), pch = 19, cex = 0.5, main = "OLS Residuals")

# Plot a QQ plot
qqPlot(ols$residuals, main = "Q-Q Plot", ylab = "Residuals", pch = 19, cex = 0.5)

# Calculate a Jarque-Bera tests (skewness & kurtosis of residuals)
jb.norm.test(ols$residuals)

# Rectify both violations with the Box-Cox transformation
# ··············································································

# Calculate the power transformation power and test if transformation is required
summary(bc <- powerTransform(ols.form, data = df.nyc))

# Transform the dependent variable
df.nyc$price_m2.transformed <- bcPower(df.nyc$price_m2, coef(bc, round = T))

# Adjust the formula
dependent_variable <- c("price_m2.transformed")
ols.form <- as.formula(paste(dependent_variable, 
                             paste(c(numeric_variables_sum, categorical_variables), 
                                   collapse = "+"), sep = "~"))
ols <- lm(ols.form, df.nyc)

# Check if the transformation was successful
summary(powerTransform(ols.form, data = df.nyc))

# Plot the residuals versus the levels
plot(ols$fitted.values, ols$residuals, xlab = "Fitted values", ylab = "Residuals", 
     main = "Homoscedasticity with Box-Cox Transformation")

# ------------------------------------------------------------------------------
# NON-LINEARITY (BOX-TIDWELL TRANSFORMATION)
# ------------------------------------------------------------------------------

# Plot the independent variables against the dependent variable to detect non-linear relationships
df.nyc %>% 
  as.data.frame() %>% 
  select_(.dots = c(numeric_variables_sum, dependent_variable)) %>% 
  gather(-price_m2.transformed, key = "predictors", value = "value") %>%
  ggplot(aes(x = value, y = price_m2.transformed)) +
  geom_point() +
  facet_wrap(~ predictors, scales = "free")

# Generalised additive model (~2min)
tic("GAM estimation")
  gam.form <- as.formula(paste(dependent_variable, 
                               paste(c(categorical_variables, 
                                       map_chr(numeric_variables_sum, 
                                               function(x) paste0("s(", x, ")"))), 
                                     collapse = "+"), sep = "~"))
  gam <- mgcv::gam(gam.form, family = gaussian(), data = df.nyc)
toc()
options(max.print = 100) ; summary(gam) ; mgcv::gam.check(gam) ; plot(gam)

# Specify the inverse Box-Cox transformation
bcUntransform <- function(yt, lambda = bc$lambda) {
  if (lambda == 0) {
    return(exp(y))
  } else {
    return(exp(log(lambda * yt + 1) / lambda))
  }
}

# Calculate mean absolute percentage error using GAM
ape.gam <- abs(bcUntransform(predict(gam, df.nyc)) - df.nyc$price_m2) / df.nyc$price_m2
mape.gam <- mean(ape.gam) ; mape.gam ; median(ape.gam)
plot(ape.gam)

# Calculate mean absolute percentage error using OLS
ape.ols <- abs(bcUntransform(predict(ols, df.nyc)) - df.nyc$price_m2) / df.nyc$price_m2
mape.ols <- mean(ape.ols) ; mape.ols ; median(ape.ols)
plot(ape.ols, pch = 19, cex = 0.5, col = rgb(0,0,0,0.1), ylim = c(0, 1))

# Calculate the locational price
locational_variables <- c("d_highschool", "d_cinema", "d_airport", "d_fitness", 
                          "d_playground", "d_railway", "d_shopping", "d_exhange",
                          "d_highway", "d_mainroad", "d_waterfront", "a_park", 
                          "d_subway", "sum_n_rcb", "d_supermarket")
df.location <- df.nyc %>% mutate_at(vars(setdiff(names(.), c(locational_variables, 
                                                             categorical_variables))), ~ 0)
loc_var <- bcUntransform(predict(ols, df.location)) / bcUntransform(predict(ols, df.nyc))

# Plot the density of residuals
plot(density(ols$residuals), pch = 19, cex = 0.5, main = "OLS Residuals")

# Plot a QQ plot
qqPlot(ols$residuals, main = "Q-Q Plot", ylab = "Residuals", pch = 19, cex = 0.5)

# Calculate a Jarque-Bera tests (skewness & kurtosis of residuals)
jb.norm.test(ols$residuals)

# Import NYC boroughs and change to a equidistant projection with metres as units
sf.boroughs <- st_as_sf(readOGR(dsn = "/Users/samu_hugo/Desktop/Code/Data/boroughs/",
                                layer = "geo_export_a814b278-dd58-459f-b871-28e2dd794f22"),
                        crs = 4326) %>% 
  st_transform(32118)

# Plot the old buildings on the map
tm_shape(sf.boroughs) + 
  tm_polygons(alpha = 0.1, border.alpha = 0.4) +
  tm_layout(title = "Buildings old than 100 years, \n (transacted in 2018)", title.position = c("left", "top"),
            frame = F) + 
  tm_legend(title.size = 1.2, text.size = 0.8, position = c(0.05, 0.7)) + 
  sf.nyc %>% 
  filter(age > 100) %>% 
  tm_shape() +
  tm_dots(size = 0.1, col = "price_m2", title = "Price per m2", alpha = 0.4, 
          style = "quantile", border.lwd = NA, border.col = NA,
          palette = viridis::magma(n = 20, 
                                   begin = 0.1, 
                                   end = 0.9, 
                                   direction = -1))

# Check the age price per sqm relationship
plot(df.nyc$age, df.nyc$price_m2.transformed, main = "Building age vs Price per sqm", 
     xlab = "Age", ylab = "Price per sqm", pch = 19, cex = 0.5, col = rgb(0,0,0,0.2))

# Plotting the age of the 100 most expensive buildings transacted in 2018
df.nyc %>% 
  top_n(100, price_m2) %>% 
  select("Age" = age, "Price per sqm" = price_m2) %>% 
  plot(pch = 19, main = "Relationship between age and price per sqm (100 most expensive properties)")

# Percentage of old buildings among the most expensive buildings
df.nyc %>% 
  top_n(100, price_m2) %>% 
  count(age > 80)

# Percentage of new buildings among the most expensive buildings
df.nyc %>% 
  top_n(100, price_m2) %>% 
  count(age < 20)


# ------------------------------------------------------------------------------
# SPATIAL CORRELATION (S2SLS / ESF)
# ------------------------------------------------------------------------------

# Load the listw objects
load(file = "/Users/samu_hugo/Desktop/Code/data-listw.RData")

# Check the amount of spatial correlation left in the residuals of an OLS regression (~3.5min)
tic("Spatial correlation in OLS residuals")
  ols.form <- as.formula(paste(dependent_variable, 
                           paste(c(numeric_variables_sum, categorical_variables), 
                                 collapse = "+"), sep = "~"))
  ols <- lm(ols.form, df.nyc)
  lm.test <- map(grep('listw$', ls(), value = T), 
                 function(x) lm.morantest(ols, get(x), zero.policy = T))
toc()

# Calculate moran's eigenvectors with different kernel models
meig_f.exp <- meigen_f(st_coordinates(sf.nyc), "exp")
colnames(meig_f.exp$sf) <- as.character(paste0("sev", 1:ncol(meig_f.exp$sf)))
esf.df.exp <- cbind(df.nyc, meig_f.exp$sf)

meig_f.gau <- meigen_f(st_coordinates(sf.nyc), "gau")
colnames(meig_f.gau$sf) <- as.character(paste0("sev", 1:ncol(meig_f.gau$sf)))
esf.df.gau <- cbind(df.nyc, meig_f.gau$sf)

meig_f.sph <- meigen_f(st_coordinates(sf.nyc), "sph")
colnames(meig_f.sph$sf) <- as.character(paste0("sev", 1:ncol(meig_f.sph$sf)))
esf.df.sph <- cbind(df.nyc, meig_f.sph$sf)

# Estimate the residuals
esf.exp.form <- as.formula(paste(dependent_variable, 
                         paste(c(numeric_variables_sum, categorical_variables, 
                                 colnames(meig_f.exp$sf)),  collapse = "+"), sep = "~"))
esf.gau.form <- as.formula(paste(dependent_variable, 
                                 paste(c(numeric_variables_sum, categorical_variables, 
                                         colnames(meig_f.gau$sf)),  collapse = "+"), sep = "~"))
esf.sph.form <- as.formula(paste(dependent_variable, 
                                 paste(c(numeric_variables_sum, categorical_variables, 
                                         colnames(meig_f.sph$sf)),  collapse = "+"), sep = "~"))
esf.exp <- lm(esf.exp.form, esf.df.exp)
esf.gau <- lm(esf.gau.form, esf.df.gau)
esf.sph <- lm(esf.sph.form, esf.df.sph)

# Check for the remaining spatial correlation in the residuals (~18min)
tic("Check residual spatial correlation")
  lm.morantest(esf.exp, id.listw, alternative = "two.sided")  # I = 0.043 with p < 2.2e-16
  lm.morantest(esf.gau, id.listw)  # I = 0.080 with p < 2.2e-16
  lm.morantest(esf.sph, id.listw)  # I = 0.018 with p < 2.2e-16
  lm.morantest(esf.exp, gau.listw)  # I = 0.004 with p < 2.2e-16
  lm.morantest(esf.gau, gau.listw)  # I = 0.036 with p < 2.2e-16
  lm.morantest(esf.sph, gau.listw, alternative = "two.sided")  # I = -0.016 with p < 2.2e-16
  lm.morantest(esf.exp, b2.listw)  # I = 0.033 with p < 2.2e-16
  lm.morantest(esf.gau, b2.listw)  # I = 0.072 with p < 2.2e-16
  lm.morantest(esf.sph, b2.listw)  # I = 0.0046 with p < 2.2e-16
  lm.morantest(esf.exp, t3.listw)  # I = 0.031 with p < 2.2e-16
  lm.morantest(esf.gau, t3.listw)  # I = 0.070 with p < 2.2e-16
  lm.morantest(esf.sph, t3.listw)  # I = 0.003 with p < 2.2e-16
  lm.morantest(esf.exp, knn3.listw)  # I = 0.343 with p < 2.2e-16
  lm.morantest(esf.gau, knn3.listw)  # I = 0.365 with p < 2.2e-16
  lm.morantest(esf.sph, knn3.listw)  # I = 0.316 with p < 2.2e-16
  lm.morantest(esf.exp, knn6.listw)  # I = 0.332 with p < 2.2e-16
  lm.morantest(esf.gau, knn6.listw)  # I = 0.356 with p < 2.2e-16
  lm.morantest(esf.sph, knn6.listw)  # I = 0.304 with p < 2.2e-16
  lm.morantest(esf.exp, knn10.listw)  # I = 0.303 with p < 2.2e-16
  lm.morantest(esf.gau, knn10.listw)  # I = 0.330 with p < 2.2e-16
  lm.morantest(esf.sph, knn10.listw)  # I = 0.271 with p < 2.2e-16
toc()

# Calculate spatial two stage least square models with differnt spatial weights matrices (~1min)
tic("Calculate S2SLS models")
  stsls.m <- map(grep('listw$', ls(), value = T), 
                 function(x) stsls(ols.form, df.nyc, get(x), zero.policy = T))
toc()

# Check for the remaining spatial correlation in the residuals (~18min)
tic("Check S2SLS residuals")
  stsls.m <- map(grep('listw$', ls(), value = T), 
                 function(x) moran.test(ols.form, df.nyc, get(x), zero.policy = T))
  moran.test(stsls.id$residuals, id.listw)  # I = 0.235 with p < 2.2e-16
  moran.test(stsls.gau$residuals, gau.listw)  # I = 0.226 with p < 2.2e-16
  moran.test(stsls.b2$residuals, b2.listw)  # I = 0.246 with p < 2.2e-16
  moran.test(stsls.t3$residuals, t3.listw)  # I = 0.240 with p < 2.2e-16
  moran.test(stsls.knn3$residuals, knn3.listw)  # I = 0.448 with p < 2.2e-16
  moran.test(stsls.knn6$residuals, knn6.listw)  # I = 0.460 with p < 2.2e-16
  moran.test(stsls.knn10$residuals, knn10.listw)  # I = 0.441 with p < 2.2e-16
toc()

