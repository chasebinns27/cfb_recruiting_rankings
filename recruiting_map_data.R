#libraries and installs
#install.packages("cfbfastR")
library(cfbfastR)
library(dplyr)
#install.packages("purrr")
library(purrr)

#API Instructions
?register_cfbd

#Use API Key
Sys.setenv(CFBD_API_KEY = "+qVRXH8S2h094wGSnY0PKcNnF8WR8WyEeMPsKWWezwy9PFyw2NWYAQM8PjC5+csU")

#2020 Recruiting Pull Test
cfbd_recruiting_player(2020, recruit_type = "HighSchool")

#Years
years <- c(2012, 2013, 2014, 2015, 2016, 2017,
           2018, 2019)

recruiting_pull_function <- function(year) {
  cfbd_recruiting_player(year, recruit_type = "HighSchool")
}

#pull recruiting data using map_dfr
#check out Collibra api code
recruiting_data <- map_dfr(years, recruiting_pull_function) %>%
  filter(ranking < 301) %>%
  count(state_province, year)
#may want to explore inserting data into dataframe in R
