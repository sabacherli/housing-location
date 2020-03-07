
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
packages <- c("tictoc", "pryr", "Metrics", "car", "lmtest", "normtest", "nortest", "tseries",  # profiling & testing
              "MASS", "nngeo", "purrr", "spatialreg", "mgcv", "testit", # miscellaneous
              "knitr", "kableExtra",  # latex
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
# PREPARATION
# ------------------------------------------------------------------------------

# Load data and listw objects
load(file = "/Users/samu_hugo/Desktop/Code/data-variables.RData")
load(file = "/Users/samu_hugo/Desktop/Code/data-specification.RData")
load(file = "/Users/samu_hugo/Desktop/Code/data-boxcox.RData")

# Specify variables of interest
numerical_variables_sum <- c("age", "math", "pc_neighbourhood",
                             "sum_n_rcb", "d_subway", "d_railway", "d_airport",
                             "d_highway", "d_mainroad","d_exchange", "d_highschool",
                             "d_supermarket", "d_convenience", "d_fitness", "d_cinema", 
                             "d_playground", "a_park", "d_waterfront", "d_shopping")
categorical_variables <- c("building_class")
dependent_variable <- c("price_m2")
dependent_variable.t <- c("price_m2.t")

# WEIGHTS MATRIX (for subsample)
threshold <- st_nn(sf.nyc[which(sf.nyc$borough == "Bronx"), ], sf.nyc[which(sf.nyc$borough == "Bronx"), ], k = 2, returnDist = T)[[2]] %>% unlist() %>% max()
dnn <- dnearneigh(sf.nyc[which(sf.nyc$borough == "Bronx"), ], 0, round(threshold, 0) + 1)
dlist <- nbdists(dnn, st_coordinates(sf.nyc[which(sf.nyc$borough == "Bronx"), ]))

# Creating an inverse distance matrix
id.list <- lapply(dlist, function(x) 1/x)
id.listw <- nb2listw(dnn, glist = id.list, style = "W")

# Creating an exponential distance matrix 
exp.list <- lapply(dlist, function(x) exp(-x))
exp.listw <- nb2listw(dnn, glist = exp.list, style = "W")

u = lapply(dlist, function(x) x/(round(threshold, 0)))

# Creating a gaussian distance matrix
gau.list <- lapply(u, function(z) exp(-0.5*(z^2)))
gau.listw <- nb2listw(dnn, glist = gau.list, style = "W")

# Creating an epanechnikov distance matrix
epa.list <- lapply(u, function(z) (3/4)*(1 - z^2))
epa.listw <- nb2listw(dnn, glist = epa.list, style = "W")

# Releveling
df.nyc <- df.nyc %>%
  mutate(district = as.factor(district),
         zip = relevel(zip, ref = "11234"),
         borough = relevel(borough, ref = "Queens"),
         neighbourhood = relevel(neighbourhood, ref = "FLUSHING-NORTH"),
         district = relevel(district, ref = 27),
         building_class = relevel(building_class, ref = "A"))

price_m2.subset <- df.nyc[which(df.nyc$borough == "Bronx"), ]$price_m2
price_m2.subset.t <- df.nyc[which(df.nyc$borough == "Bronx"), ]$price_m2.t

# ------------------------------------------------------------------------------
# MODELS
# ------------------------------------------------------------------------------

# OLS
form.ols <- as.formula(paste(dependent_variable.t, 
                             paste(c(numerical_variables_sum, categorical_variables), 
                                   collapse = "+"), sep = "~"))
model.ols <- lm(form.ols, df.nyc[which(df.nyc$borough == "Bronx"), ])
mape.ols <- median(abs(price_m2.subset - bcUntransform(model.ols$fitted.values))/price_m2.subset)
moran.ols <- map(grep('listw$', ls(), value = T), 
                     function(x) moran.test(model.ols$residuals, get(x), zero.policy = T))
ad.ols <- ad.test(model.ols$residuals)$statistic %>% unname()


# POLY OLS
form.pols <- price_m2.t ~ building_class  + poly(a_park, 2) + poly(age, 2) + log(d_cinema) +
  poly(d_convenience, 2) + log(d_exchange) + poly(d_fitness, 3) + log(d_highway) + 
  log(d_mainroad) + poly(d_playground, 2) + log(d_railway) + poly(d_shopping, 3) +
  log(d_subway) + poly(d_supermarket, 3) + d_waterfront + poly(math, 2) + poly(n_crime, 2) +
  poly(n_noise, 3) + poly(sum_n_rcb, 3)
model.pols <- lm(form.pols, df.nyc[which(df.nyc$borough == "Bronx"), ])
mape.pols <- median(abs(price_m2.subset - bcUntransform(model.pols$fitted.values))/price_m2.subset)
moran.pols <- map(grep('listw$', ls(), value = T), 
                     function(x) moran.test(model.pols$residuals, get(x), zero.policy = T)) 
ad.pols <- ad.test(model.pols$residuals)$statistic %>% unname()

# GAM PS
form.gam.ps <- as.formula(paste(dependent_variable.t, 
                             paste(c(categorical_variables,
                                     map_chr(numerical_variables_sum, 
                                             function(x) paste0("s(", x, ", bs = 'ps')"))), 
                                   collapse = "+"), sep = "~"))
model.gam.ps <- mgcv::gam(form.gam.ps, family = gaussian(), data = df.nyc[which(df.nyc$borough == "Bronx"), ])
mape.gam.ps <- median(abs(price_m2.subset - bcUntransform(model.gam.ps$fitted.values))/price_m2.subset)
moran.gam.ps <- map(grep('listw$', ls(), value = T), 
                     function(x) moran.test(model.gam.ps$residuals, get(x), zero.policy = T))
ad.gam.ps <- ad.test(model.gam.ps$residuals)$statistic %>% unname()

# GAM CS
form.gam.cs <- as.formula(paste(dependent_variable.t, 
                                   paste(c(categorical_variables,
                                           map_chr(numerical_variables_sum, 
                                                   function(x) paste0("s(", x, ", k = 10, bs = 'cs')"))), 
                                         collapse = "+"), sep = "~"))
model.gam.cs <- mgcv::gam(form.gam.cs, family = gaussian(), data = df.nyc[which(df.nyc$borough == "Bronx"), ])
mape.gam.cs <- median(abs(price_m2.subset - bcUntransform(model.gam.cs$fitted.values))/price_m2.subset)
moran.gam.cs <- map(grep('listw$', ls(), value = T), 
                     function(x) moran.test(model.gam.cs$residuals, get(x), zero.policy = T))
ad.gam.cs <- ad.test(model.gam.cs$residuals)$statistic %>% unname()


# GEOGAM PS
form.geogam.ps <- as.formula(paste(dependent_variable.t, 
                                paste(c(categorical_variables,
                                        "s(X, Y)",
                                        map_chr(numerical_variables_sum, 
                                                function(x) paste0("s(", x, ", bs = 'ps')"))), 
                                      collapse = "+"), sep = "~"))
model.geogam.ps <- mgcv::gam(form.geogam.ps, family = gaussian(), data = df.nyc[which(df.nyc$borough == "Bronx"), ])
mape.geogam.ps <- median(abs(price_m2.subset - bcUntransform(model.geogam.ps$fitted.values))/price_m2.subset)
moran.geogam.ps <- map(grep('listw$', ls(), value = T), 
                    function(x) moran.test(model.geogam.ps$residuals, get(x), zero.policy = T))
ad.geogam.ps <- ad.test(model.geogam.ps$residuals)$statistic %>% unname()

# GEOGAM CS
form.geogam.cs <- as.formula(paste(dependent_variable.t, 
                                      paste(c(categorical_variables,
                                              "s(X, Y)",
                                              map_chr(numerical_variables_sum, 
                                                      function(x) paste0("s(", x, ", k = 10, bs = 'cs')"))), 
                                            collapse = "+"), sep = "~"))
model.geogam.cs <- mgcv::gam(form.geogam.cs, family = gaussian(), data = df.nyc[which(df.nyc$borough == "Bronx"), ])
mape.geogam.cs <- median(abs(price_m2.subset - bcUntransform(model.geogam.cs$fitted.values))/price_m2.subset)
moran.geogam.cs <- map(grep('listw$', ls(), value = T), 
                          function(x) moran.test(model.geogam.cs$residuals, get(x), zero.policy = T))
ad.geogam.cs <- ad.test(model.geogam.cs$residuals)$statistic %>% unname()


# S2SLS
form.stsls <- as.formula(paste(dependent_variable.t, 
                               paste(c(categorical_variables, 
                                       numerical_variables_sum), 
                                     collapse = "+"), sep = "~"))
model.stsls <- spatialreg::stsls(formula = form.stsls, data = df.nyc[which(df.nyc$borough == "Bronx"), ], listw = exp.listw)
mape.stsls <- median(abs(price_m2.subset - bcUntransform(price_m2.subset.t - model.stsls$residuals))/price_m2.subset)
moran.stsls <- map(grep('listw$', ls(), value = T), 
                     function(x) moran.test(model.stsls$residuals, get(x), zero.policy = T))
ad.stsls <- ad.test(model.stsls$residuals)$statistic %>% unname()

# POLY STSLS
form.pstsls <- price_m2.t ~ building_class + poly(a_park, 2) + poly(age, 2) + log(d_cinema) +
  poly(d_convenience, 2) + log(d_exchange) + poly(d_fitness, 3) + log(d_highway) + 
  log(d_mainroad) + poly(d_playground, 2) + log(d_railway) + poly(d_shopping, 3) +
  log(d_subway) + poly(d_supermarket, 3) + d_waterfront + poly(math, 2) + poly(n_crime, 2) +
  poly(n_noise, 3) + poly(sum_n_rcb, 3)
model.pstsls <- stsls(form.pstsls, data = df.nyc[which(df.nyc$borough == "Bronx"), ], listw = exp.listw)
mape.pstsls <- median(abs(price_m2.subset - bcUntransform(price_m2.subset.t - model.pstsls$residuals))/price_m2.subset)
moran.pstsls <- map(grep('listw$', ls(), value = T), 
                     function(x) moran.test(model.pstsls$residuals, get(x), zero.policy = T))
ad.pstsls <- ad.test(model.pstsls$residuals)$statistic %>% unname()


# MESS
form.mess <- as.formula(paste(dependent_variable.t,
                              paste(c(categorical_variables,
                                      numerical_variables_sum),
                                    collapse = "+"), sep = "~"))
model.mess <- spatialreg::lagmess(formula = form.mess, data = df.nyc[which(df.nyc$borough == "Bronx"), ], listw = exp.listw, control = list())
mape.mess <- median(abs(price_m2.subset - bcUntransform(price_m2.subset.t - residuals(model.mess)))/price_m2.subset)
moran.mess <- map(grep('listw$', ls(), value = T), 
                     function(x) moran.test(residuals(model.mess), get(x), zero.policy = T))
ad.mess <- ad.test(residuals(model.mess))$statistic %>% unname()

# POLY MESS
form.pmess <- price_m2.t ~ building_class + poly(a_park, 2) + poly(age, 2) + log(d_cinema) +
  poly(d_convenience, 2) + log(d_exchange) + poly(d_fitness, 3) + log(d_highway) + 
  log(d_mainroad) + poly(d_playground, 2) + log(d_railway) + poly(d_shopping, 3) +
  log(d_subway) + poly(d_supermarket, 3) + d_waterfront + poly(math, 2) + poly(n_crime, 2) +
  poly(n_noise, 3) + poly(sum_n_rcb, 3)
model.pmess <- spatialreg::lagmess(formula = form.pmess, data = df.nyc[which(df.nyc$borough == "Bronx"), ], listw = exp.listw)
mape.pmess <- median(abs(price_m2.subset - bcUntransform(price_m2.subset.t - residuals(model.pmess)))/price_m2.subset)
moran.pmess <- map(grep('listw$', ls(), value = T), 
                     function(x) moran.test(residuals(model.pmess), get(x), zero.policy = T))
ad.pmess <- ad.test(residuals(model.pmess))$statistic %>% unname()


# # GSTSLS
# form.gstsls <- as.formula(paste(dependent_variable.t, 
#                                 paste(c(categorical_variables, 
#                                         numerical_variables_sum), 
#                                       collapse = "+"), sep = "~"))
# model.gstsls <- spatialreg::gstsls(formula = form.gstsls, data = df.nyc[which(df.nyc$borough == "Bronx"), ], listw = exp.listw, control = list())
# mape.gstsls <- median(abs(price_m2.subset - bcUntransform(model.gstsls$fitted.values))/price_m2.subset)
# moran.gstsls <- map(grep('listw$', ls(), value = T), 
#                      function(x) moran.test(model.gstsls$residuals, get(x), zero.policy = T))
# ad.gstsls <- ad.test(model.gstsls$residuals)$statistic %>% unname()
# 
# # POLY GSTSLS
# form.pgstsls <- price_m2.t ~ building_class + poly(a_park, 2) + poly(age, 2) + log(d_cinema) +
#   poly(d_convenience, 2) + log(d_exchange) + poly(d_fitness, 3) + log(d_highway) + 
#   log(d_mainroad) + poly(d_playground, 2) + log(d_railway) + poly(d_shopping, 3) +
#   log(d_subway) + poly(d_supermarket, 3) + d_waterfront + poly(math, 2) + poly(n_crime, 2) +
#   poly(n_noise, 3) + poly(sum_n_rcb, 3)
# model.pgstsls <- spatialreg::gstsls(formula = form.pgstsls, data = df.nyc[which(df.nyc$borough == "Bronx"), ], listw = exp.listw)
# mape.pgstsls <- median(abs(price_m2.subset - bcUntransform(model.pgstsls$fitted.values))/price_m2.subset)
# moran.pgstsls <- map(grep('listw$', ls(), value = T), 
#                    function(x) moran.test(model.pgstsls$residuals, get(x), zero.policy = T))
# ad.pgstsls <- ad.test(model.pgstsls$residuals)$statistic %>% unname()


# ESF
meig_f.gau <- meigen_f(st_coordinates(sf.nyc[which(df.nyc$borough == "Bronx"), ]), "gau")
colnames(meig_f.gau$sf) <- as.character(paste0("sev", 1:ncol(meig_f.gau$sf)))
esf.df.gau <- cbind(df.nyc[which(df.nyc$borough == "Bronx"), ], meig_f.gau$sf)
form.esf.gau.gam <- as.formula(paste(dependent_variable.t, 
                                     paste(c(colnames(meig_f.gau$sf), 
                                             categorical_variables, 
                                             map_chr(numerical_variables_sum, 
                                                     function(x) paste0("s(", x, ", bs = 'cs')"))), 
                                           collapse = "+"), sep = "~"))
model.esf <- mgcv::gam(form.esf.gau.gam, family = gaussian(), esf.df.gau)
mape.esf <- median(abs(price_m2.subset - bcUntransform(model.esf$fitted.values))/price_m2.subset)
moran.esf <- map(grep('listw$', ls(), value = T), 
                 function(x) moran.test(model.esf$residuals, get(x), zero.policy = T))
ad.esf <- ad.test(model.esf$residuals)$statistic %>% unname()

# ------------------------------------------------------------------------------
# COMPARISON
# ------------------------------------------------------------------------------

# Print spatial autocorrelation in residuals
moran <- as.data.frame(matrix(nrow = length(grep('^moran.', ls(), value = F)), ncol = length(grep('listw$', ls(), value = F)) + 1))
colnames(moran) <- c("Method", "Epanechnikov", "Exponential", "Gaussian", "Inverse")
for (x in 1:length(grep('^moran.', ls(), value = F))) {
  moran[x, 1] = ls()[grep('^moran.', ls(), value = F)[x]]
  for (y in 1:length(grep('.listw$', ls(), value = F))) {
    moran[x, y + 1] <- get(ls()[grep('^moran.', ls(), value = F)[x]])[[y]]$estimate[1] %>% unname() %>% round(4) %>% abs()
  }
} ; moran %>% arrange(Epanechnikov) %>% 
  kable("latex", caption = "Descriptive statistics of the numerical variables included in the processed data set used to analyse the pricing implications of housing location for New York City. In addition, there are two categorical variables, \textit{Boroughs} and \textit{Building class}, with five and four levels respectively.", booktabs = T) %>%
  kable_styling(latex_options = c("striped", "HOLD_position"))

# Print MAPE, Heteroskedasticity, Normaility
comp <- as.data.frame(matrix(nrow = length(grep('^model.', ls(), value = F)), ncol = 4))
for (x in 1:length(grep('^model.', ls(), value = F))) {
  comp[x, 1] = ls()[grep('^model.', ls(), value = F)[x]]
  comp[x, 2] = round(get(ls()[grep('^mape.', ls(), value = F)[x]]), 4)
  comp[x, 3] = ifelse(has_error(round(bptest(get(ls()[grep('^model.', ls(), value = F)[x]]))$statistic %>% unname(), 4), silent = T),
                      NA, 
                      round(bptest(get(ls()[grep('^model.', ls(), value = F)[x]]))$statistic %>% unname(), 4))
  comp[x, 4] = round(get(ls()[grep('^ad.', ls(), value = F)[x]]), 4)
} ; comp
comp[6,3] <- round(bptest(model.mess$lmobj)$statistic %>% unname(), 4)  # manually add missing ones where possible
comp[8,3] <- round(bptest(model.pmess$lmobj)$statistic %>% unname(), 4)
comp %>% dplyr::rename(model = V1, mape = V2, bp = V3, ad = V4) %>% arrange(mape) %>% 
  kable("latex", caption = "Descriptive statistics of the numerical variables included in the processed data set used to analyse the pricing implications of housing location for New York City. In addition, there are two categorical variables, \textit{Boroughs} and \textit{Building class}, with five and four levels respectively.", booktabs = T) %>%
  kable_styling(latex_options = c("striped", "HOLD_position"))

