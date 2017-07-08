###########################
# How to use the package? #
###########################

# 1: Install the package

# 1.1: Install from CRAN (recommended)
install.packages("oec")

# 1.2 Install from Github
#install.packages("devtools") # if needed
library(devtools)
install_github("pachamaltese/oec/r_package")

##############################
# Use package's functions #
##############################

# 2.1: Load the package
library(oec)

# 2.2: Explore the countries list if you are not sure about the countries codes
countries_list

# treemap() examples

# 3.1: What does Chile export to China? (2015) (HS92 4 characters)
treemap("chl", "chn", "exports", 2015)
treemap("chl", "chn", "exports", 2015, 1) # equivalent to last command

# treemap_interval() examples

# 4.1: What does Chile export to China? (2010-2015) (HS92 4 characters)
treemap_interval("chl", "chn", "exports", 2010, 2015)
treemap_interval("chl", "chn", "exports", 2010, 2014, 1, 1, 1) # equivalent to last command

# network() examples

# 5.1: What are the export opportunities of Chile? (2014, trade with China) (HS92 4 characters)
network("chl", "chn", 2015)
network("chl", "chn", 2015, 1) # equivalent to last command

# network_interval() examples

# 6.1: What are the export opportunities of Chile? (2011-2014, trade with China) (HS92 4 characters)
network_interval("chl", "chn", 2010, 2015)
network_interval("chl", "chn", 2010, 2015, 1, 1) # equivalent to last command

# getdata() examples

# 7.1: Download trade between Chile and China in the year 2014 from OEC's API (HS92 4 characters)
getdata("chl", "chn", 2015)
getdata("chl", "chn", 2015, 1) # equivalent to last command

# 7.2: Download trade between Chile and China in the year 2014 from OEC's API (SITC rev2 4 characters)
getdata("chl", "chn", 2015, 2)

# 7.3: Download trade between Chile and China in the year 2014 from from OEC's API (HS92 6 characters)
getdata("chl", "chn", 2015, 3)

# getdata_interval() examples

# 8.1: Download trade between Chile and China in the years 2010-2015 from OEC's API (HS92 4 characters)
getdata_interval("chl", "chn", 2010, 2015)
getdata_interval("chl", "chn", 2010, 2015, 1, 1) # equivalent to last command

# 8.1: Download trade between Chile and China in the years 2010, 2012 and 2014 from OEC's API (HS92 4 characters)
getdata_interval("chl", "chn", 2010, 2014, 1, 2)

# 8.2: Download trade between Chile and China in the years 2010, 2012 and 2014 from OEC's API (SITC rev2 4 characters)
getdata_interval("chl", "chn", 2010, 2014, 2, 2)

# 8.3: Download trade between Chile and China in the years 2010, 2012 and 2014 from OEC's API (HS92 6 characters)
getdata_interval("chl", "chn", 2010, 2014, 3, 2)
