rm(list = ls())

get_R_name <- function(filename){
  dir <- paste0("C:/Users/danil/R projects/Appartment Rent in Kyiv 2026/", filename, ".R")
  return(dir)
}

source(get_R_name("01_import"), echo = TRUE)
source(get_R_name("02_data_validation"), echo = TRUE)
source(get_R_name("03_data_vizualization"), echo = TRUE)

