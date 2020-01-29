
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
              "MASS", "nngeo", "purrr", "spatialreg", "mgcv", # miscellaneous
              "spmoran", "tmap", "sf", "rgdal", "spdep", "dplyr")  # fundamentals
ipack(packages) ; rm(ipack, packages)


# ------------------------------------------------------------------------------
# MODEL COMPARISON
# ------------------------------------------------------------------------------

tic("Model comparison: ")

# Load data and listw objects
load(file = "/Users/samu_hugo/Desktop/Code/data-variables.RData")
load(file = "/Users/samu_hugo/Desktop/Code/data-specification.RData")
load(file = "/Users/samu_hugo/Desktop/Code/data-listw.RData")

# Specify variables of interest
numerical_variables_sum <- c("age", "math", "n_crime", "n_noise", 
                           "sum_n_rcb", "d_subway", "d_railway", 
                           "d_highway", "d_mainroad","d_exchange", "d_highschool",
                           "d_supermarket", "d_convenience", "d_fitness", "d_cinema", 
                           "d_playground", "a_park", "d_waterfront", "d_shopping")
categorical_variables <- c("neighbourhood", "building_class")
dependent_variable.t <- c("price_m2.t")

# WEIGHTS MATRIX
threshold <- st_nn(sf.nyc[1:1000, ], sf.nyc[1:1000, ], k = 2, returnDist = T)[[2]] %>% unlist() %>% max()
dnn <- dnearneigh(sf.nyc[1:1000, ], 0, round(threshold, 0))
dlist <- nbdists(dnn, st_coordinates(sf.nyc[1:1000, ]))
id.list <- lapply(dlist, function(x) 1/x)
id.listw <- nb2listw(dnn, glist = id.list, style = "W") ; id.listw


# OLS
ols.form <- as.formula(paste(dependent_variable.t, 
                             paste(c(numerical_variables_sum, categorical_variables), 
                                   collapse = "+"), sep = "~"))
ols <- lm(ols.form, df.nyc[1:1000, ]) ; mean(abs(ols$residuals))


# POLY OLS
pols.form <- price_m2.t ~ building_class + neighbourhood + poly(a_park, 2) + poly(age, 2) + log(d_cinema) +
  poly(d_convenience, 2) + log(d_exchange) + poly(d_fitness, 3) + log(d_highway) + 
  log(d_mainroad) + poly(d_playground, 2) + log(d_railway) + poly(d_shopping, 3) +
  log(d_subway) + poly(d_supermarket, 3) + d_waterfront + poly(math, 2) + poly(n_crime, 2) +
  poly(n_noise, 3) + poly(sum_n_rcb, 3)
pols <- lm(pols.form, df.nyc[1:1000, ]) ; mean(abs(pols$residuals))


# GAM
gam.form <- as.formula(paste(dependent_variable.t, 
                             paste(c(categorical_variables, 
                                     map_chr(numerical_variables_sum, 
                                             function(x) paste0("s(", x, ", bs = 'ps')"))), 
                                   collapse = "+"), sep = "~"))
gam <- mgcv::gam(gam.form, family = gaussian(), data = df.nyc[1:1000, ]) ; mean(abs(gam$residuals))


# S2SLS
stsls.form <- as.formula(paste(dependent_variable.t, 
                               paste(c(categorical_variables, 
                                       numerical_variables_sum), 
                                     collapse = "+"), sep = "~"))
stsls <- spatialreg::stsls(formula = stsls.form, data = df.nyc[1:1000,], listw = id.listw) ; mean(abs(stsls$residuals))


# POLY STSLS
pstsls.form <- price_m2.t ~ building_class + neighbourhood + poly(a_park, 2) + poly(age, 2) + log(d_cinema) +
  poly(d_convenience, 2) + log(d_exchange) + poly(d_fitness, 3) + log(d_highway) + 
  log(d_mainroad) + poly(d_playground, 2) + log(d_railway) + poly(d_shopping, 3) +
  log(d_subway) + poly(d_supermarket, 3) + d_waterfront + poly(math, 2) + poly(n_crime, 2) +
  poly(n_noise, 3) + poly(sum_n_rcb, 3)
pstsls <- stsls(pstsls.form, data = df.nyc[1:1000, ], listw = id.listw) ; mean(abs(pstsls$residuals))


# MESS
mess.form <- as.formula(paste(dependent_variable.t,
                              paste(c(categorical_variables,
                                      numerical_variables_sum),
                                    collapse = "+"), sep = "~"))
mess <- spatialreg::lagmess(formula = mess.form, data = df.nyc[1:1000,], listw = id.listw, control = list()) ; mean(abs(residuals(mess)))


# POLY MESS
pmess.form <- price_m2.t ~ building_class + neighbourhood + poly(a_park, 2) + poly(age, 2) + log(d_cinema) +
  poly(d_convenience, 2) + log(d_exchange) + poly(d_fitness, 3) + log(d_highway) + 
  log(d_mainroad) + poly(d_playground, 2) + log(d_railway) + poly(d_shopping, 3) +
  log(d_subway) + poly(d_supermarket, 3) + d_waterfront + poly(math, 2) + poly(n_crime, 2) +
  poly(n_noise, 3) + poly(sum_n_rcb, 3)
pmess <- spatialreg::lagmess(formula = pmess.form, data = df.nyc[1:1000,], listw = id.listw) ; mean(abs(residuals(pmess)))


# GSTSLS
gstsls.form <- as.formula(paste(dependent_variable.t, 
                                paste(c(categorical_variables, 
                                        numerical_variables_sum), 
                                      collapse = "+"), sep = "~"))
gstsls <- spatialreg::gstsls(formula = gstsls.form, data = df.nyc[1:1000,], listw = id.listw, control = list()) ; mean(abs(gstsls$residuals))


# POLY GSTSLS
pgstsls.form <- price_m2.t ~ building_class + neighbourhood + poly(a_park, 2) + poly(age, 2) + log(d_cinema) +
  poly(d_convenience, 2) + log(d_exchange) + poly(d_fitness, 3) + log(d_highway) + 
  log(d_mainroad) + poly(d_playground, 2) + log(d_railway) + poly(d_shopping, 3) +
  log(d_subway) + poly(d_supermarket, 3) + d_waterfront + poly(math, 2) + poly(n_crime, 2) +
  poly(n_noise, 3) + poly(sum_n_rcb, 3)
pgstsls <- spatialreg::gstsls(formula = pgstsls.form, data = df.nyc[1:1000,], listw = id.listw) ; mean(abs(pgstsls$residuals))


# ESF
meig_f.exp <- meigen_f(st_coordinates(sf.nyc[1:1000, ]), "exp")
colnames(meig_f.exp$sf) <- as.character(paste0("sev", 1:ncol(meig_f.exp$sf)))
esf.df.exp <- cbind(df.nyc[1:1000, ], meig_f.exp$sf)
esf.exp.gam.form <- as.formula(paste(dependent_variable.t, 
                                     paste(c(colnames(meig_f.exp$sf), 
                                             categorical_variables, 
                                             map_chr(numerical_variables_sum, 
                                                     function(x) paste0("s(", x, ", bs = 'ps')"))), 
                                           collapse = "+"), sep = "~"))
esf.exp.gam <- mgcv::gam(esf.exp.gam.form, family = gaussian(), esf.df.exp) ; mean(abs(esf.exp.gam$residuals))

toc()