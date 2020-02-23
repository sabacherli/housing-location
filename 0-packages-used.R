
# Create a function to install missing packages and load all required packages
ipack <- function(pack) {
  create.pkg <- pack[!(pack %in% installed.packages()[, "Package"])]
  if (length(create.pkg))
    install.packages(create.pkg, dependencies = TRUE)
  sapply(pack, require, character.only = TRUE)
}

# Specifcy packages and install and load them
# [loaded in reverse order, meaning functions from early packages may be masked by functions from latter packages]
packages <- c("dplyr", "stringr", "NCmisc")
ipack(packages)

checkPacks <- function(path){
  
  ## get all R files in your directory
  files <- list.files(path)[str_detect(list.files(path), ".R$")]
  
  ## extract all functions and which package they are from 
  ## using NCmisc::list.functions.in.file
  funs <- unlist(lapply(paste0(path, "/", files), list.functions.in.file))
  packs <- funs %>% names()
  
  ## "character" functions such as reactive objects in Shiny
  characters <- packs[str_detect(packs, "^character")]
  
  ## user defined functions in the global environment
  globals <- packs[str_detect(packs, "^.GlobalEnv")]
  
  ## functions that are in multiple packages' namespaces 
  multipackages <- packs[str_detect(packs, ", ")]
  
  ## get just the unique package names from multipackages
  mpackages <- multipackages %>%
    str_extract_all(., "[a-zA-Z0-9]+") %>%
    unlist() %>%
    unique()
  mpackages <- mpackages[!mpackages %in% c("c", "package")]
  
  ## functions that are from single packages
  packages <- packs[str_detect(packs, "package:") & !packs %in% multipackages] %>%
    str_replace(., "[0-9]+$", "") %>%
    str_replace(., "package:", "") 
  
  ## unique packages
  packages_u <- packages %>%
    unique() %>%
    union(., mpackages)
  
  return(list(packs = packages_u, tb = table(packages)))
  
}

checkPacks("/Users/samu_hugo/Desktop/Code/Packages/")
