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
              "rgdal", "spmoran", "spdep", "sf", "dplyr", "tmap")  # fundamentals
ipack(packages) ; rm(ipack, packages)


# ------------------------------------------------------------------------------
# MULTICOLLINEARITY
# ------------------------------------------------------------------------------

# Load data set
load(file = "/Users/samu_hugo/Desktop/Code/data-variables.RData")

# Specify variables of interest
numerical_variables <- c("age", "math", "reading", "writing",  "SO2", "NOx", "PM2.5", 
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
numerical_variables_pca <- c("age", "pc_sat", "pc_air", "n_crime", "n_noise", 
                           "pc_n_rcb", "d_airport", "d_subway", "d_railway", 
                           "pc_d_highways", "d_mainroad","d_exchange", 
                           "d_supermarket", "d_convenience", "d_fitness", "d_cinema", 
                           "d_playground", "a_park", "d_waterfront", "d_shopping")

numerical_variables_sum <- c("age", "math", "PM2.5", "n_crime", "n_noise", 
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

# Drop both distance to airport and air pollution, the latter due to collinearity
# with population density and consequently the number of restaurant, cafes, and bars
numerical_variables_sum <- c("age", "math", "n_crime", "n_noise", 
                           "sum_n_rcb", "d_subway", "d_railway", 
                           "d_highway", "d_mainroad","d_exchange", "d_highschool",
                           "d_supermarket", "d_convenience", "d_fitness", "d_cinema", 
                           "d_playground", "a_park", "d_waterfront", "d_shopping")


# ------------------------------------------------------------------------------
# HETEROSKEDASTICITY & NON-NORMALITY (BOX-COX TRANSFORMATION)
# ------------------------------------------------------------------------------

# Determine the functional form
ols.form <- as.formula(paste(dependent_variable, 
                             paste(c(numerical_variables_sum, categorical_variables), 
                                   collapse = "+"), sep = "~"))

# Estimate an OLS model
ols <- lm(ols.form, df.nyc)

# Test if the errors are normally distributed
# ··············································································
# Plot a QQ plot
qqPlot(ols$residuals, main = "Q-Q Plot", ylab = "Residuals", pch = 19, cex = 0.5)

# Plot a histogram of the residuals
hist(ols$residuals, main = "Histogram of OLS residuals", 100)

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
bptest(ols, studentize = T)
ncvTest(ols)

# Reduce the issue of non-normality and heteroskedasticity with a Box-Cox transformation
# ··············································································
# Calculate the power transformation power and test if transformation is required
summary(bc <- powerTransform(ols.form, data = df.nyc))  # lambda = 0.39

# Transform the dependent variable and adjust the ols model
df.nyc$price_m2.t <- bcPower(df.nyc$price_m2, coef(bc, round = T))
dependent_variable.t <- c("price_m2.t")
ols.t.form <- as.formula(paste(dependent_variable.t, 
                             paste(c(numerical_variables_sum, categorical_variables), 
                                   collapse = "+"), sep = "~"))
ols.t <- lm(ols.t.form, df.nyc)

# Check if the transformation was successful
summary(powerTransform(ols.t.form, data = df.nyc))

# Compare the QQ plots
pdf("/Users/samu_hugo/Desktop/Code/Plots/plot-ols-qqplots.pdf") 
  par(mfrow = c(2, 1))
  qqPlot(ols$residuals, main = "QQ Plot (before Box-Cox)", ylab = "Residuals", pch = 19, cex = 0.5)
  qqPlot(ols.t$residuals, main = "QQ Plot (after Box-Cox)", ylab = "Residuals", pch = 19, cex = 0.5)
dev.off()

# Compare the heteroskedasticity plots
pdf("/Users/samu_hugo/Desktop/Code/Plots/plot-ols-heteroskedasticity.pdf") 
  par(mfrow = c(2, 1))
  plot(ols$fitted.values, ols$residuals, xlab = "Fitted values", ylab = "Residuals", 
       main = "Heteroskedasticity without Box-Cox transformation", pch = 19, col = rgb(0,0,0,0.1),
       cex = 0.5)
  plot(ols.t$fitted.values, ols.t$residuals, xlab = "Fitted values", ylab = "Residuals", 
       main = "Heteroskedasticity with Box-Cox Transformation", pch = 19, col = rgb(0,0,0,0.1),
       cex = 0.5)
dev.off()

# Compare the histograms
pdf("/Users/samu_hugo/Desktop/Code/Plots/plot-ols-histograms.pdf") 
  par(mfrow = c(3, 1))
  hist(df.nyc$price_m2, main = "Without transformation", xlab = "Transformed price per sqm")
  hist(df.nyc$price_m2.t, main = "Box-Cox transformation", xlab = "Transformed price per sqm")
  hist(log(df.nyc$price_m2), main = "Logarithmic transformation", xlab = "Transformed price per sqm")
dev.off()

# Compare tests statistics for improvements
jb.norm.test(ols.t$residuals)
skewness.norm.test(ols.t$residuals)
kurtosis.norm.test(ols.t$residuals)
ad.test(ols.t$residuals)
bptest(ols.t)
ncvTest(ols.t)

# Show that inference based on normality tests is inappropriate
# ··············································································
# Type I error: Failure to reject the null hypothesis with small sample sizes
set.seed(1)
shapiro.test(rlnorm(20,0,.4))

# Type 2 error: False rejection of the null hyptothesis with large sample sizes
set.seed(1)
ad.test(rt(500000, 200))

# Save the lambda
save(bc, file = "/Users/samu_hugo/Desktop/Code/data-boxcox.RData")

# ------------------------------------------------------------------------------
# NON-LINEARITY 
# ------------------------------------------------------------------------------

# Plot the independent variables against the dependent variable to detect non-linear relationships
pdf("/Users/samu_hugo/Desktop/Code/Plots/plot-correlations.pdf") 
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

# Plotting numerical variable histograms, scatterplots, and splines
y <- df.nyc %>% dplyr::select(dependent_variable.t) %>% t() %>% as.vector()  # select the dependent variable
for (x in 1:length(numerical_variables_sum)) {
  variable <- numerical_variables_sum[x] 
  path <- paste0("/Users/samu_hugo/Desktop/Code/Plots/plot-nonlinearity-", variable, ".pdf")  # create the path to save the plot to
  x <- df.nyc %>% dplyr::select(variable) %>% t() %>% as.vector()  # select the independent variable
  pdf(path)
    par(mfrow = c(1,2))
    hist(x, 100, main = paste("Histogram of ", variable))
    plot(y~x, pch = 16, col = rgb(0,0,0,0.05), xlab = variable, ylab = "Price per sqm (transformed)",
         main = "Scatterplot with a 10 knot spline")
    s <- bs(x, k = 10)  # calculate the 10 knot spline 
    m <- lm(y ~ s)
    lines(x[order(x)], predict(m, newdata = list(x = seq(min(x), max(x), 1)))[order(x)], 
          col = "blue", lwd = 2, type = "l", pch = 16, cex = 0.5)  # ddd predicted points from the spline
  dev.off()
}

# Save the dataset with transformed values
save(df.nyc, file = "/Users/samu_hugo/Desktop/Code/data-specification.RData")

# ------------------------------------------------------------------------------
# SPATIAL AUTOCORRELATION
# ------------------------------------------------------------------------------

# Load the data set and the listw objects
load(file = "/Users/samu_hugo/Desktop/Code/data-listw.RData")

# Check the amount of spatial correlation left in the residuals of the GAM (~3.5min)
tic("Spatial correlation in GAM residuals")
  gam.form <- as.formula(paste(dependent_variable.t, 
                               paste(c(categorical_variables, 
                                       map_chr(numerical_variables_sum, 
                                               function(x) paste0("s(", x, ", bs = 'ps')"))), 
                                     collapse = "+"), sep = "~"))
  gam <- mgcv::gam(gam.form, family = gaussian(), data = df.nyc)
  gam.moran <- map(grep('listw$', ls(), value = T), 
                 function(x) moran.test(gam$residuals, get(x), zero.policy = T)) ; print(grep('listw$', ls(), value = T)) ; gam.moran
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

# Create the GAM forms with the moran eigenvectors
esf.exp.gam.form <- as.formula(paste(dependent_variable.t, 
                                     paste(c(colnames(meig_f.exp$sf), 
                                             categorical_variables, 
                                             map_chr(numerical_variables_sum, 
                                                     function(x) paste0("s(", x, ", bs = 'ps')"))), 
                                           collapse = "+"), sep = "~"))

esf.gau.gam.form <- as.formula(paste(dependent_variable.t, 
                                     paste(c(colnames(meig_f.gau$sf), 
                                             categorical_variables, 
                                             map_chr(numerical_variables_sum, 
                                                     function(x) paste0("s(", x, ", bs = 'ps')"))), 
                                           collapse = "+"), sep = "~"))

esf.sph.gam.form <- as.formula(paste(dependent_variable.t, 
                                     paste(c(colnames(meig_f.sph$sf), 
                                             categorical_variables, 
                                             map_chr(numerical_variables_sum, 
                                                     function(x) paste0("s(", x, ", bs = 'ps')"))), 
                                           collapse = "+"), sep = "~"))

# Estimate the models
esf.exp.gam <- mgcv::gam(esf.exp.gam.form, family = gaussian(), esf.df.exp)
esf.gau.gam <- mgcv::gam(esf.gau.gam.form, family = gaussian(), esf.df.gau)
esf.sph.gam <- mgcv::gam(esf.sph.gam.form, family = gaussian(), esf.df.sph)

# Check for the remaining spatial correlation in the residuals (~30min)
tic("Check residual spatial correlation")
  options(scipen = 99999)
  esf.moran <- matrix(NA, 
                      nrow = length(grep('listw$', ls(), value = T)), 
                      ncol = length(grep('.gam$', ls(), value = T)))
  rownames(esf.moran) <- grep('listw$', ls(), value = T)
  colnames(esf.moran) <- grep('.gam$', ls(), value = T)
  for (i in 1:length(grep('listw$', ls(), value = T))) {
    for (j in 1:length(grep('.gam$', ls(), value = T))) {
      esf.moran[i, j] <- moran.test(get(grep('.gam$', ls(), value = T)[j])$residuals,
                                    get(grep('listw$', ls(), value = T)[i]), 
                                    alternative = "two.sided")$estimate[1] %>% 
        unname() %>% round(., 4)
    }
  } ; esf.moran
toc()
