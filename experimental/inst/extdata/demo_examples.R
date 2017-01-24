### Examples ###

# if needed #
#install.packages("devtools")
library(devtools)
install_github("pachamaltese/oec/cran")

setwd("/myfolder") # change this to your working directory
library(oec)

### Demo with files obtained from OEC's API ###

# see the countries list if you are not sure about the country code
countries_list

# download trade data from OEC's API (HS92 6 characters product lists)
getdata("chl", "all", 6, 2014)

# download trade data from OEC's API (HS92 8 characters product lists)
getdata("chl", "all", 8, 2014)

# download trade data from OEC's API (SITC rev.2 4 characters product lists)
getdata("chl", "all", 4, 2014)

# download trade data from OEC's API (HS92 6 characters product lists)
# for the years 2010, 2012 and 2014
getdata_interval("chl", "chn", 4, 2010, 2014, 2)

# treemap
# treemap() calls getdata() and its not needed to run getdata() first
treemap("chl", "chn", "exports", 6, 2014)

# treemap_interval
# treemap_interval() calls getdata_interval() and its not needed to run getdata_interval() first
treemap_interval("chl", "chn", "exports", 6, 2010, 2014, 2, 1)

# network
# network() calls getdata() and its not needed to run getdata() first
network("chl", "chn", 6, 2014)
