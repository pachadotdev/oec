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

hs96

hs02

hs07

sitc

## ----getdata, eval = TRUE------------------------------------------------
# What does Chile exchange with China?  
# year 2015 - SITC (4 characters)
getdata("chl", "chn", 2015)

## ----getdata2, eval = TRUE-----------------------------------------------
# What does Chile exchange with China?  
# year 2015 - HS07 (4 and 6 characters)
getdata("chl", "chn", 2015, "hs07")

## ----getdata2_2, eval = TRUE---------------------------------------------
# What does Chile exchange with China?  
# years 2010 and 2015 - HS07 (4 and 6 characters)
getdata("chl", "chn", c(2010,2015), "hs07")

## ----getdata3, eval = TRUE, error=TRUE-----------------------------------
# What does Chile exchange with China?  
# year 2000 - HS02 (4 and 6 characters)
getdata("chl", "chn", 2000, "hs02")

## ----getdata4, eval = TRUE, error = TRUE---------------------------------
# What does Andorra exchange with France?  
# year 2015 - SITC (4 characters)
getdata("and", "fra", 2015)

## ----getdata5, eval = TRUE-----------------------------------------------
# What does Andorra exchange with France?  
# year 2015 - HS07 (4 and 6 characters)
getdata("and", "fra", 2015, "hs07")

