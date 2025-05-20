# Pacman for packages install/load
if(!require("pacman")){
  install.packages("pacman")
  library(pacman)
}
p_load(char = c("ggplot2",
                "tidyverse",
                "stringr",
                # "janitor",
                "tidyr",
                "fastDummies",
                # "readr",
                # "gtrendsR",
                # "scales",
                # "crimeutils",
                # "haven",
                # "zoo",
                # "callr",
                "readxl",
                "writexl",
                "data.table",
                "dplyr",
                # "stringi",
                "lubridate",
                "fixest",
                "kableExtra",
                # "lemon",
                "openxlsx",
                "readxl",
                #"tidymodels",
                "texreg",
                "estimatr",
                "missForest",
                "psych",
                "RColorBrewer",
                "ggthemes",
                "excel.link", # to read password protected excel files
                "ri2" # to conduct randomization inference
                ))

