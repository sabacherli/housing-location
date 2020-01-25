
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
packages <- c("pryr",  # profiling & testing
              "MASS",  # miscellaneous
              "tmap", "sf", "rgdal", "spdep", "dplyr")  # fundamentals
ipack(packages) ; rm(ipack, packages)

mem_used()


# ------------------------------------------------------------------------------
# MODEL ESTIMATION
# ------------------------------------------------------------------------------

colnames(sf.nyc2)
colnames(sf.nyc3)

X <- sf.nyc3 %>% 
  as.data.frame() %>% 
  select(price, m2, age, land, math, crime, subway, noise, cinema, fitness, park,
         playground, railway, restaurants_cafes_bars, shopping, groceries, exchange,
         highway, mainroad, waterfront) %>% 
  scale(center = T, scale = F) %>% 
  cbind(sf.nyc3 %>% as.data.frame() %>% select(borough, class))

m <- price ~ m2 ~ borough + class + age + land + math + crime + subway + noise + 
  cinema + fitness + park + playground + railway + restaurants_cafes_bars +
  shopping + groceries + exchange + highway + mainroad + waterfront
ols <- lm(price ~ m2 + borough + class + age + land + math + crime + subway + noise + 
            cinema + fitness + park + playground + railway + restaurants_cafes_bars +
            shopping + groceries + exchange + highway + mainroad + waterfront, data = X)

lm_tests <- lm.LMtests(ols, W.listw, test = "all")

# ------------------------------------------------------------------------------
# NONLINEAR ESTIMATION
# ------------------------------------------------------------------------------

# Generalised additive model (~2min)
tic("GAM estimation")
  gam.form <- as.formula(paste(dependent_variable.t, 
                               paste(c(categorical_variables, 
                                       map_chr(numeric_variables_sum, 
                                               function(x) paste0("s(", x, ", k = 10)"))), 
                                     collapse = "+"), sep = "~"))
  gam <- mgcv::gam(gam.form, family = gaussian(), data = df.nyc)
toc()
options(max.print = 100) 
summary(gam)
mgcv::gam.check(gam, data = df.nyc, method = "REML")
plot(gam, resid = T)

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
                          "d_playground", "d_railway", "d_shopping", "d_exchange",
                          "d_highway", "d_mainroad", "d_waterfront", "a_park", 
                          "d_subway", "sum_n_rcb", "d_supermarket")
df.location <- cbind(df.nyc, meig_f.exp$sf) %>% 
  mutate_at(vars(setdiff(names(.), c(locational_variables, categorical_variables, "X", "Y"))), ~ 0)

sf.location <- sf.nyc %>% 
  mutate(location_value = bcUntransform(predict(esf.exp.r, df.location)),
         fitted_value = bcUntransform(esf.exp.r$fitted.values),
         residuals = price_m2 - fitted_value) %>% 
  filter(residuals >= 0)

moran.test(sf.location$location.value, listw = id.listw)

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
  tm_shape(sf.location) +
  tm_dots(size = 0.1, col = "location_value", title = "Price per m2", alpha = 0.4, 
          style = "quantile", border.lwd = NA, border.col = NA,
          palette = viridis::magma(n = 20, 
                                   begin = 0.1, 
                                   end = 0.9, 
                                   direction = -1))
dev.off()

hist(sf.location$location_value, 10000, main = "Histogram of locational value", xlim = c(0, 10000))
hist(sf.location$price_m2, 10000, main = "Histogram of locational value")

