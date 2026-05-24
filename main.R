library(here)
rm(list = ls())


get_R_name <- function(filename){
  dir <- paste0(here(), "/", filename, ".R")
  print(dir)
  return(dir)
}

is_markdown = FALSE # Для звіту прибираємо зайве виведення

pdf(file = NULL)

source(get_R_name("01_import"), echo = !(is_markdown))
source(get_R_name("02_data_validation"), echo = !(is_markdown))
source(get_R_name("03_data_vizualization"), echo = !(is_markdown))
source(get_R_name("04_data1_regression"), echo = !(is_markdown))
source(get_R_name("041_K1_data1_regression"), echo = !(is_markdown))
source(get_R_name("042_K1_data1_regression"), echo = !(is_markdown))
source(get_R_name("043_K1_data1_regression"), echo = !(is_markdown))
source(get_R_name("041_K2_data1_regression"), echo = !(is_markdown))

dev.off()

