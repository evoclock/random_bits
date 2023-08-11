# Function to load tidyverse
# dealing with the tidyverse/linux incompatibility as per usual
load_required_packages = function() {
  required_packages = c("readr", "tibble", "dplyr", "tidyr", "magrittr", "purrr",
                         "forcats", "stringr", "lubridate", "ggplot2")
  for (pkg in required_packages){
    library(pkg, character.only = TRUE)
  }
}
load_required_packages()
