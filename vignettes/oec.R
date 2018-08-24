## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----countries, eval = TRUE----------------------------------------------
library(oec)

country_codes

## ----products, eval = TRUE-----------------------------------------------
hs92

## ----get_data, eval = TRUE-----------------------------------------------
# What does Chile exchange with Argentina?  
# year 1980 - SITC (4 characters)
get_data("chl", "arg", 1980)

## ----get_data2, eval = FALSE---------------------------------------------
#  # What does Chile exchange with Argentina?
#  # year 2015 - HS07 (4 and 6 characters)
#  get_data("chl", "arg", 2015, "hs07")

## ----get_data2_2, eval = FALSE-------------------------------------------
#  # What does Chile exchange with Argentina?
#  # years 2010 and 2015 - HS07 (4 and 6 characters)
#  get_data("chl", "arg", c(2010,2015), "hs07")

## ----get_data3, eval = TRUE, error=TRUE----------------------------------
# What does Chile exchange with Argentina?  
# year 2000 - HS02 (4 and 6 characters)
get_data("chl", "arg", 2000, "hs02")

## ----get_data4, eval = TRUE, error = TRUE--------------------------------
# What does Andorra exchange with France?  
# year 2015 - SITC (4 characters)
get_data("and", "fra", 2015)

## ----get_data5, eval = TRUE----------------------------------------------
# What does Andorra exchange with France?  
# year 2015 - HS07 (4 and 6 characters)
get_data("and", "fra", 2015, "hs07")

