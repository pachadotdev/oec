## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----countries-----------------------------------------------------------
library(oec)

country_codes

## ----products------------------------------------------------------------
hs92

hs96

hs02

hs07

sitc

## ----getdata-------------------------------------------------------------
# What does Chile exchange with China?  
# year 2015 - SITC (4 characters)
getdata("chl", "chn", 2015)

## ----getdata2, eval = F--------------------------------------------------
#  # What does Chile exchange with China?
#  # year 2015 - HS07 (4 and 6 characters)
#  getdata("chl", "chn", 2015, "hs92")
#  
#  # What does Chile exchange with China?
#  # year 2015 - HS96 (4 and 6 characters)
#  getdata("chl", "chn", 2015, "hs96")
#  
#  # What does Chile exchange with China?
#  # year 2015 - HS02 (4 and 6 characters)
#  getdata("chl", "chn", 2015, "hs02")
#  
#  # What does Chile exchange with China?
#  # year 2015 - HS07 (4 and 6 characters)
#  getdata("chl", "chn", 2015, "hs07")

## ----getdata3------------------------------------------------------------
chl_chn_2015_sitc

## ----getdata4, eval = F--------------------------------------------------
#  # What does Chile exchange with China?
#  # year 2000 - HS07 (4 and 6 characters)
#  getdata("chl", "chn", 2000, "hs02")

## ----getdata5, eval = F--------------------------------------------------
#  # What does Andorra exchange with France?
#  # year 2015 - SITC (4 characters)
#  getdata("and", "fra", 2015)

## ----getdata6, eval = F--------------------------------------------------
#  # What does Andorra exchange with France?
#  # year 2015 - HS07 (4 and 6 characters)
#  getdata("and", "fra", 2015, "hs07")

## ----getdata_batch, eval = F---------------------------------------------
#  # What does Chile exchange with China?
#  # years 2010 to 2015 - SITC (4 characters)
#  getdata_batch("chl", "chn", 2010, 2015)

## ----getdata_batch2, eval = F--------------------------------------------
#  # What does Chile exchange with China?
#  # years 2010 to 2015 - SITC (4 characters)
#  getdata("chl", "chn", 2010)
#  getdata("chl", "chn", 2011)
#  getdata("chl", "chn", 2012)
#  getdata("chl", "chn", 2013)
#  getdata("chl", "chn", 2014)
#  getdata("chl", "chn", 2015)
#  
#  data <- bind_rows(chl_chn_2010_sitc, chl_chn_2011_sitc, chl_chn_2012_sitc,
#                    chl_chn_2013_sitc, chl_chn_2014_sitc, chl_chn_2015_sitc)

## ----getdata_batch3, eval = F--------------------------------------------
#  # What does Chile exchange with China?
#  # years 2010 to 2015 - HS92 (4 and 6 characters)
#  getdata_batch("chl", "chn", 2010, 2015, "hs92")
#  
#  # What does Chile exchange with China?
#  # years 2010 to 2015 - HS96 (4 and 6 characters)
#  getdata_batch("chl", "chn", 2010, 2015, "hs96")
#  
#  # What does Chile exchange with China?
#  # years 2010 to 2015 - HS02 (4 and 6 characters)
#  getdata_batch("chl", "chn", 2010, 2015, "hs02")
#  
#  # What does Chile exchange with China?
#  # years 2010 to 2015 - HS07 (4 and 6 characters)
#  getdata_batch("chl", "chn", 2010, 2015, "hs07")

## ----getdata_batch4, eval = F--------------------------------------------
#  # What does Chile exchange with China?
#  # years 2010, 2012 and 2014 - HS07 (4 and 6 characters)
#  getdata_batch("chl", "chn", 2010, 2014, "hs07", 2)

