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
packages <- c("tictoc", "lmtest", "car", "normtest", "nortest", "tseries",  # profiling & testing
              "Matrix",  "MASS", "tidyr", "ggplot2", "purrr", "splines", "mgcv",  # miscellaneous
              "knitr", "kableExtra",  # latex
              "rgdal", "spmoran", "spdep", "sf", "dplyr", "tmap")  # fundamentals
ipack(packages) ; rm(ipack, packages)


# ------------------------------------------------------------------------------
# MULTICOLLINEARITY
# ------------------------------------------------------------------------------

# Load data set
load(file = "/Users/samu_hugo/Desktop/Code/data-variables-full.RData")

# Specify variables of interest
numerical_variables <- c("age", "math", "reading", "writing",  "SO2", "NOx", "PM2.5", 
                       "n_crime", "n_noise", "n_restaurants", "n_cafes", "n_bars", 
                       "d_airport", "d_subway", "d_railway", "d_highway", 
                       "d_highway_link", "d_trunkroad", "d_mainroad","d_exchange", 
                       "d_supermarket", "d_convenience", "d_fitness", "d_cinema", 
                       "d_playground", "a_park", "d_waterfront", "d_shopping")
categorical_variables <- c("building_class")
dependent_variable <- c("price_m2")

# Check which explanatory variables are highly correlated
sf.nyc %>% 
  as.data.frame() %>% 
  select_(.dots = numerical_variables) %>% 
  cor() %>% 
  as.data.frame() %>% 
  pivot_longer(cols = everything(), names_to = "variable2", values_to = "correlation") %>% 
  mutate(variable1 = rep(numerical_variables, each = length(numerical_variables))) %>% 
  dplyr::select(variable1, variable2, correlation) %>% 
  mutate(correlation = abs(correlation)) %>% 
  filter(correlation != 1) %>% 
  arrange(desc(correlation)) %>% 
  filter(row_number() %% 2 == 0) %>% 
  View("Correlation Matrix")

  # Generate latex output
  sf.nyc %>% 
    as.data.frame() %>% 
    select_(.dots = numerical_variables) %>% 
    cor() %>% 
    as.data.frame() %>% 
    pivot_longer(cols = everything(), names_to = "variable2", values_to = "correlation") %>% 
    mutate(variable1 = rep(numerical_variables, each = length(numerical_variables))) %>% 
    dplyr::select(variable1, variable2, correlation) %>% 
    mutate(correlation = abs(correlation)) %>% 
    filter(correlation != 1) %>% 
    arrange(desc(correlation)) %>% 
    filter(row_number() %% 2 == 0) %>% 
    top_n(20, correlation) %>% 
    kable("latex", caption = "The 20 most highly correlated variables in the data set.", booktabs = T) %>%
    kable_styling(latex_options = c("striped", "HOLD_position"))

# Create principle components for the correlated variable groups
F1 <- sf.nyc %>% 
  as.data.frame() %>% 
  dplyr::select(math, reading, writing) %>% 
  prcomp()
summary(F1)
F1$rotation

F2 <- sf.nyc %>% 
  as.data.frame() %>%  
  dplyr::select(NOx, PM2.5, SO2) %>% 
  prcomp()
summary(F2)
F2$rotation

F3 <- sf.nyc %>% 
  as.data.frame() %>%  
  dplyr::select(n_bars, n_cafes, n_restaurants) %>% 
  prcomp()
summary(F3)
F3$rotation

F4 <- sf.nyc %>% 
  as.data.frame() %>%  
  dplyr::select(d_highway, d_highway_link) %>% 
  prcomp()
summary(F4)
F4$rotation

F5 <- sf.nyc %>% 
  as.data.frame() %>%  
  dplyr::select(n_crime, n_noise) %>% 
  prcomp()
summary(F5)
F5$rotation

# Add the principle components to the data set and aggregate restaurants, bars, cafes as an alternative approach
df.nyc <- sf.nyc %>% 
  as.data.frame() %>% 
  mutate(pc_sat = F1$x[, 1],
         pc_d_highways = F4$x[, 1],
         pc_n_rcb = F3$x[, 1],
         pc_air = F2$x[, 1],
         pc_neighbourhood = F5$x[, 1],
         sum_n_rcb = n_restaurants + n_cafes + n_bars) %>% 
  cbind(st_coordinates(sf.nyc)) ; rm(F1, F2, F3, F4, F5)

# Specify variables of interest
numerical_variables_pca <- c("age", "pc_sat", "pc_air", "pc_neighbourhood",
                           "pc_n_rcb", "d_airport", "d_subway", "d_railway", 
                           "pc_d_highways", "d_mainroad","d_exchange", 
                           "d_supermarket", "d_convenience", "d_fitness", "d_cinema", 
                           "d_playground", "a_park", "d_waterfront", "d_shopping")

numerical_variables_sum <- c("age", "math", "PM2.5", "pc_neighbourhood",
                           "sum_n_rcb", "d_airport", "d_subway", "d_railway", 
                           "d_highway", "d_mainroad","d_exchange", 
                           "d_supermarket", "d_convenience", "d_fitness", "d_cinema", 
                           "d_playground", "a_park", "d_waterfront", "d_shopping") ; rm(numerical_variables)

# Check if the multicollinearity has improved with the PCA approach
df.nyc %>% 
  as.data.frame() %>% 
  select_(.dots = numerical_variables_pca) %>% 
  cor() %>% 
  as.data.frame() %>% 
  pivot_longer(cols = everything(), names_to = "variable2", values_to = "correlation") %>% 
  mutate(variable1 = rep(numerical_variables_pca, each = length(numerical_variables_pca))) %>% 
  dplyr::select(variable1, variable2, correlation) %>% 
  mutate(correlation = abs(correlation)) %>% 
  filter(correlation != 1) %>% 
  arrange(desc(correlation)) %>% 
  filter(row_number() %% 2 == 0) %>% 
  View("Correlation Matrix")

# Check if the multicollinearity has improved with the SUM approach
df.nyc %>% 
  as.data.frame() %>% 
  select_(.dots = numerical_variables_sum) %>% 
  cor() %>% 
  as.data.frame() %>% 
  pivot_longer(cols = everything(), names_to = "variable2", values_to = "correlation") %>% 
  mutate(variable1 = rep(numerical_variables_sum, each = length(numerical_variables_sum))) %>% 
  dplyr::select(variable1, variable2, correlation) %>% 
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

  # Save plot
  pdf("/Users/samu_hugo/Desktop/Code/Plots/Statistics/plot-subway-airport-scatterplot.pdf")
    sf.cor <- df.nyc %>% 
      mutate(color = ifelse(borough == "Staten Island", "red", "blue"))
    plot(sf.cor$d_airport, sf.cor$d_subway, col = sf.cor$col, pch = 19, 
         main = "Scatterplot", 
         xlab = "Airport distance", ylab = "Subway distance")
    legend(10, 200, legend = c("Staten Island", "Rest of New York City"), 
           box.col = "white", pch = 19, col = c("red", "blue")) ; rm(sf.cor)
  dev.off()

# Drop air pollution due to collinearity with population density and consequently 
# the number of restaurant, cafes, and bars
numerical_variables_pca <- c("age", "pc_sat", "pc_neighbourhood",
                             "pc_n_rcb", "d_airport", "d_subway", "d_railway", 
                             "pc_d_highways", "d_mainroad","d_exchange", 
                             "d_supermarket", "d_convenience", "d_fitness", "d_cinema", 
                             "d_playground", "a_park", "d_waterfront", "d_shopping")

numerical_variables_sum <- c("age", "math", "pc_neighbourhood",
                             "sum_n_rcb", "d_airport", "d_subway", "d_railway", 
                             "d_highway", "d_mainroad","d_exchange", 
                             "d_supermarket", "d_convenience", "d_fitness", "d_cinema", 
                             "d_playground", "a_park", "d_waterfront", "d_shopping")

# Drop observations from Staten Island
sf.nyc <- sf.nyc %>% filter(boro_id != 5)  # Remove Staten Island
df.nyc <- df.nyc %>% filter(boro_id != 5)  # Remove Staten Island

# Check if the multicollinearity has improved with the both approach
df.nyc %>% 
  as.data.frame() %>% 
  select_(.dots = numerical_variables_sum) %>% 
  cor() %>% 
  as.data.frame() %>% 
  pivot_longer(cols = everything(), names_to = "variable2", values_to = "correlation") %>% 
  mutate(variable1 = rep(numerical_variables_sum, each = length(numerical_variables_sum))) %>% 
  dplyr::select(variable1, variable2, correlation) %>% 
  mutate(correlation = abs(correlation)) %>% 
  filter(correlation != 1) %>% 
  arrange(desc(correlation)) %>% 
  filter(row_number() %% 2 == 0) %>% 
  View("Correlation Matrix")

df.nyc %>% 
  as.data.frame() %>% 
  select_(.dots = numerical_variables_pca) %>% 
  cor() %>% 
  as.data.frame() %>% 
  pivot_longer(cols = everything(), names_to = "variable2", values_to = "correlation") %>% 
  mutate(variable1 = rep(numerical_variables_pca, each = length(numerical_variables_pca))) %>% 
  dplyr::select(variable1, variable2, correlation) %>% 
  mutate(correlation = abs(correlation)) %>% 
  filter(correlation != 1) %>% 
  arrange(desc(correlation)) %>% 
  filter(row_number() %% 2 == 0) %>% 
  View("Correlation Matrix")


# ------------------------------------------------------------------------------
# HETEROSKEDASTICITY & NON-NORMALITY (BOX-COX TRANSFORMATION)
# ------------------------------------------------------------------------------

# Estimate an OLS model
form.ols <- as.formula(paste(dependent_variable,
                             paste(c(numerical_variables_pca, categorical_variables),
                                   collapse = "+"), sep = "~"))
ols <- lm(form.ols, df.nyc)
plot(ols$fitted.values, abs(ols$residuals), pch = 16, col = rgb(0,0,0,1), cex = 0.1)
# Calculate heteroskedasticity by hand
df.het <- bind_cols(residuals = abs(ols$residuals), df.nyc)
residuals <- "residuals"
form.het <- as.formula(paste(residuals,
                             paste(c(numerical_variables_pca, categorical_variables),
                                   collapse = "+"), sep = "~"))
het <- lm(form.het, df.het)
summary(het)

# Test if the errors are normally distributed
# ··············································································
# Plot a QQ plot
qqPlot(ols$residuals, main = "Normal Q-Q Plot", ylab = "Residuals", pch = 19, cex = 0.5)

# Plot a histogram of the residuals
hist(ols$residuals, main = "Histogram of OLS residuals", 1000)

# Calculate a Jarque-Bera test and third and fourth moments
jb.norm.test(ols$residuals)  # normality tests for large sample sizes will always be confidently rejected
skewness.norm.test(ols$residuals)
kurtosis.norm.test(ols$residuals)

# Calculate the Anderson-Darling test (Shapiro-Wilk is not conductable in R due to the large sample size)
ad.test(ols$residuals)


# Test if errors have heteroskedasticity
# ··············································································
# Plot the residuals against the levels
plot(ols$fitted.values, ols$residuals, xlab = "Fitted values", ylab = "Residuals",
     main = "Inspection of heteroskedasticity", pch = 19, col = rgb(0,0,0,0.1),
     cex = 0.5)

# Calculate the studentized Breush-Pagan (Koenker-Bassett) test and non constant variance test
ncvTest(ols)

# Reduce the issue of non-normality and heteroskedasticity with a Box-Cox transformation
# ··············································································
# Calculate the power transformation power and test if transformation is required
summary(bc <- powerTransform(form.ols, data = df.nyc))  # lambda = 0.18

# Transform the dependent variable and adjust the ols model
df.nyc$price_m2.t <- bcPower(df.nyc$price_m2, coef(bc, round = T))
dependent_variable.t <- c("price_m2.t")
ols.t.form <- as.formula(paste(dependent_variable.t,
                             paste(c(numerical_variables_pca, categorical_variables),
                                   collapse = "+"), sep = "~"))
ols.t <- lm(ols.t.form, df.nyc)

# Check if the transformation was successful
summary(powerTransform(ols.t.form, data = df.nyc))

# Compare statistics before and after the transformation
pdf("/Users/samu_hugo/Desktop/Code/Plots/Statistics/plot-boxcox-qq.pdf")
  par(mfrow = c(2, 1))
  qqPlot(ols$residuals, main = "Normal Q-Q Plot", ylab = "Original Residuals", pch = 19, cex = 0.5)
  qqPlot(ols.t$residuals, main = "Normal Q-Q Plot", ylab = "Box-cox Residuals", pch = 19, cex = 0.5)
dev.off()
pdf("/Users/samu_hugo/Desktop/Code/Plots/Statistics/plot-boxcox-heteroskedasticity.pdf")
  par(mfrow = c(2, 1))
  plot(ols$fitted.values, ols$residuals, xlab = "Original fitted values", ylab = "Residuals",
       main = "Heteroskedasticity", pch = 19, col = rgb(0,0,0,0.1),
       cex = 0.5)
  plot(ols.t$fitted.values, ols.t$residuals, xlab = "Box-cox fitted values", ylab = "Residuals",
       main = "Heteroskedasticity", pch = 19, col = rgb(0,0,0,0.1),
       cex = 0.5)
dev.off()
pdf("/Users/samu_hugo/Desktop/Code/Plots/Statistics/plot-boxcox-histogram-prices.pdf")
  par(mfrow = c(3, 1))
  hist(df.nyc$price_m2, main = "Histogram", xlab = "Original price per sqm", breaks = 1000)
  hist(df.nyc$price_m2.t, main = "Histogram", xlab = "Box-cox price per sqm", breaks = 1000)
  hist(log(df.nyc$price_m2), main = "Histogram", xlab = "Log price per sqm", breaks = 1000)
dev.off()
pdf("/Users/samu_hugo/Desktop/Code/Plots/Statistics/plot-boxcox-histogram-residuals.pdf")
  par(mfrow = c(2, 1))
  hist(ols$residuals, main = "Residual distribution", xlab = "Original residual", breaks = 1000)
  hist(ols.t$residuals, main = "Residual distribution", xlab = "Box-cox residual", breaks = 1000)
dev.off()

# Compare tests statistics for improvements
jb.norm.test(ols.t$residuals)
skewness.norm.test(ols.t$residuals)
kurtosis.norm.test(ols.t$residuals)
ad.test(ols.t$residuals)
ncvTest(ols.t)

# Show that inference based on normality tests is inappropriate
# ··············································································
# Type I error: Failure to reject the null hypothesis with small sample sizes
set.seed(1)
shapiro.test(rlnorm(20, 0, .4))

# Type 2 error: False rejection of the null hyptothesis with large sample sizes
set.seed(1)
ad.test(rt(500000, 200))

# Save the lambda
save(bc, file = "/Users/samu_hugo/Desktop/Code/data-boxcox.RData")

# ------------------------------------------------------------------------------
# NON-LINEARITY 
# ------------------------------------------------------------------------------

# Plot the independent variables against the dependent variable to detect non-linear relationships
pdf("/Users/samu_hugo/Desktop/Code/Plots/Nonlinearity/plot-variable-scatterplots.pdf") 
  df.nyc %>% 
    as.data.frame() %>% 
    select_(.dots = c(numerical_variables_sum, dependent_variable.t)) %>% 
    gather(-price_m2.t, key = "predictors", value = "value") %>%
    ggplot(aes(x = value, y = price_m2.t)) +
    geom_point(shape = ".", size = 0.15, colour = rgb(0,0,0,0.1)) +
    facet_wrap(~ predictors, scales = "free")
dev.off()

# White test for nonlinearity
set.seed(1)
white.test(df.nyc %>% dplyr::select(numerical_variables_sum) %>% as.matrix(), 
           df.nyc %>% dplyr::select(dependent_variable.t) %>% t() %>% as.vector())

  # Generate latex output
  set.seed(1)
  white.test(df.nyc %>% dplyr::select(numerical_variables_sum) %>% as.matrix(), 
             df.nyc %>% dplyr::select(dependent_variable.t) %>% t() %>% as.vector())

# Plotting numerical variable histograms, scatterplots, and penalised splines
y <- df.nyc %>% dplyr::select(dependent_variable.t) %>% t() %>% as.vector()  # select the dependent variable
for (x in 1:length(numerical_variables_sum)) {
  variable <- numerical_variables_sum[x] 
  path <- paste0("/Users/samu_hugo/Desktop/Code/Plots/Nonlinearity/Penalised Splines/plot-ps-", variable, ".pdf")  # create the path to save the plot to
  x <- df.nyc %>% dplyr::select(variable) %>% t() %>% as.vector()  # select the independent variable
  pdf(path)
    plot(y ~ x, pch = 16, col = rgb(0,0,0,0.05), xlab = variable, ylab = "Price per sqm (transformed)",
         main = "Scatterplot with penalized B-splines")
    gam <- gam(y ~ s(x, bs = 'ps'))
    lines(x[order(x)], gam$fitted.values[order(x)], 
          col = "blue", lwd = 2, type = "l", pch = 16)  # add predicted points from the spline
  dev.off()
}

# Plotting numerical variable histograms, scatterplots, and 10 knot cubic regression splines
y <- df.nyc %>% dplyr::select(dependent_variable.t) %>% t() %>% as.vector()  # select the dependent variable
for (x in 1:length(numerical_variables_sum)) {
  variable <- numerical_variables_sum[x] 
  path <- paste0("/Users/samu_hugo/Desktop/Code/Plots/Nonlinearity/Cubic splines/plot-cs-", variable, ".pdf")  # create the path to save the plot to
  x <- df.nyc %>% dplyr::select(variable) %>% t() %>% as.vector()  # select the independent variable
  pdf(path)
  plot(y ~ x, pch = 16, col = rgb(0,0,0,0.05), xlab = variable, ylab = "Price per sqm (transformed)",
       main = "Scatterplot with 10 knot cubic splines")
  s <- bs(x, k = 10)  # calculate the 10 knot cubic spline 
  m <- lm(y ~ s)
  lines(x[order(x)], m$fitted.values[order(x)], 
        col = "blue", lwd = 2, type = "l", pch = 16)  # add predicted points from the spline
  dev.off()
}

# Save the dataset with transformed values
sf.nyc <- df.nyc %>% st_as_sf()
save(df.nyc, file = "/Users/samu_hugo/Desktop/Code/data-specification.RData")
save(sf.nyc, file = "/Users/samu_hugo/Desktop/Code/data-variables.RData")

