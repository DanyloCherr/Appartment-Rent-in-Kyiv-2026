library(here)
library(nortest)
rm(list = ls())


get_R_name <- function(filename){
  dir <- paste0(here(), "/scripts/", filename, ".R")
  print(dir)
  return(dir)
}

is_markdown = FALSE # Для звіту прибираємо зайве виведення

dev.new()
pdf(file = NULL)
sink("NULL")

source(get_R_name("01_import"), echo = !(is_markdown))
source(get_R_name("02_data_validation"), echo = !(is_markdown))
source(get_R_name("03_data_vizualization"), echo = !(is_markdown))
source(get_R_name("04_data1_regression"), echo = !(is_markdown))
source(get_R_name("041_K1_data1_regression"), echo = !(is_markdown))
# source(get_R_name("042_K1_data1_regression"), echo = !(is_markdown))
# source(get_R_name("043_K1_data1_regression"), echo = !(is_markdown))
source(get_R_name("044(q)_K1_data1_regression"), echo = !(is_markdown))
source(get_R_name("044(q)_K2_data1_regression"), echo = !(is_markdown))
source(get_R_name("041_K2_data1_regression"), echo = !(is_markdown))
source(get_R_name("041_K3_data1_regression"), echo = !(is_markdown))
source(get_R_name("044(q)_K3_data1_regression"), echo = !(is_markdown))
source(get_R_name("041_ind_data1_regression"), echo = !(is_markdown))
source(get_R_name("044(q)_ind_data1_regression"), echo = !(is_markdown))
source(get_R_name("05_data2_regression"), echo = !(is_markdown)) 
source(get_R_name("051_K1_data2_regression"), echo = !(is_markdown)) 
source(get_R_name("051_K2_data2_regression"), echo = !(is_markdown)) 
source(get_R_name("051_K3_data2_regression"), echo = !(is_markdown)) 
source(get_R_name("051_ind_data2_regression"), echo = !(is_markdown)) 

source(get_R_name("100_population_modeling"), echo = !(is_markdown))

sink()
dev.off()

