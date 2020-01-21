
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
              "spdep", "dplyr")  # fundamentals
ipack(packages)

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
