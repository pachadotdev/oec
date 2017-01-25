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
getdata("chl", "chn", 2014)
# is the same as
getdata("chl", "all", 2014, 6)

# download trade data from OEC's API (HS92 8 characters product list)
getdata("chl", "all", 2014, 8)

# download trade data from OEC's API (SITC rev.2 4 characters product list)
getdata("chl", "all", 2014, 4)

# the function getdata_interval() is quite new, avoid it

# download trade data from OEC's API (HS92 6 characters product lists)
# for the years 2010 to 2014
getdata_interval("chl", "chn", 2010, 2014)
# is the same as
getdata_interval("chl", "chn", 2010, 2014, 6, 1)

# download trade data from OEC's API (SITC rev.2 4 characters product list)
# for the years 2010, 2012 and 2014
getdata_interval("chl", "chn", 4, 2010, 2014, 2)

# treemap
# treemap() calls getdata() and its not needed to run getdata() first
treemap("chl", "chn", "exports", 2014)
# is the same as
treemap("chl", "chn", "exports", 2014, 6)

# network
# network() calls getdata() and its not needed to run getdata() first
network("chl", "chn", 6, 2014)
