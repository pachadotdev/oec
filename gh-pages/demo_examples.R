### Examples ###

# if needed #
#install.packages("devtools")
library(devtools)
install_github("pachamaltese/oec/cran")

setwd("/myfolder") # change this to your working directory
library(oec)

# see the countries list if you are not sure about the country code
countries_list

# download trade data from OEC's API (HS92 4 characters product list)
# for Chile and China in the year 2014
getdata("chl", "chn", 2014)
# is the same as
getdata("chl", "chn", 2014, 1)

# download trade data from OEC's API (SITC rev.2 4 characters product list)
# for Chile and China in the year 2014
getdata("chl", "chn", 2014, 2)

# download trade data from OEC's API (HS92 6 characters product list)
# for Chile and China in the year 2014
getdata("chl", "chn", 2014, 3)

##############################

# download trade data from OEC's API (HS92 4 characters product list)
# for Chile and China in the years 2010 to 2014
getdata_interval("chl", "chn", 2011, 2014)
# is the same as
getdata_interval("chl", "chn", 2011, 2014, 1, 1)

# download trade data from OEC's API (HS92 4 characters product list)
# for Chile and China in the years 2010, 2012 and 2014
getdata_interval("chl", "chn", 2011, 2014, 1, 2)

# download trade data from OEC's API (SITC rev.2 4 characters product list)
# for Chile and China in the years 2010, 2012 and 2014
getdata_interval("chl", "chn", 2011, 2014, 2, 2)

# download trade data from OEC's API (HS92 6 characters product list)
# for Chile and China in the years 2010, 2012 and 2014
getdata_interval("chl", "chn", 2011, 2014, 3, 2)

##############################

# visualize trade data from OEC's API (HS92 4 characters product list)
# for Chile and China in the year 2014
treemap("chl", "chn", "exports", 2014, 1)
# is the same as
treemap("chl", "chn", "exports", 2014)

##############################

# visualize trade data from OEC's API (HS92 4 characters product list)
# for exports from Chile to China in the year 2014
treemap_interval("chl", "chn", "exports", 2011, 2014, 1, 1, 1)
# is the same as
treemap_interval("chl", "chn", "exports", 2011, 2014)

##############################

# visualize trade data from OEC's API (HS92 4 characters product list)
# for exports from Chile to China in the year 2014
network("chl", "chn", 2014, 1)
# is the same as
network("chl", "chn", 2014)

##############################

# visualize trade data from OEC's API (HS92 4 characters product list)
# for exports from Chile to China in the year 2014
network_interval("chl", "chn", 2011, 2014, 1, 1)
# is the same as
network_interval("chl", "chn", 2011, 2014)
