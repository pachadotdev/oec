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

# download trade data from OEC's API (hs92 6 characters product lists)
getdata("chl", "all", 6, 2014)

# download trade data from OEC's API (hs92 6 characters product lists)
getdata("chl", "all", 6, 2014)

# treemap
# treemap() calls getdata() and it is not needed to run getdata() first
treemap("chl", "chn", "exports", 6, 2014)

# network
# network() calls getdata() and it is not needed to run getdata() first
network("chl", "chn", 6, 2014)

# network.comparison
# network_comparison() calls getdata() and it is not needed to run getdata() first
network_comparison("chl", "chn", 6, 2010, 2014)
