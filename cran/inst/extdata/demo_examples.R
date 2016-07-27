### Examples ###

# if needed #
install.packages("devtools")
library(devtools)
install_github("pachamaltese/oec")

setwd("/myfolder") # change this to your working directory
library(oec)

### Demo with files obtained from OEC's API ###

# see the countries list if you are not sure about the country code
countries_list

# download trade data from OEC's API (hs92 6 characters product lists)
getdata("chl", "all", 6, 2014)

# treemap
# treemap() calls getdata() and it is not needed to run getdata() first
treemap("chl", "all", "exports", 6, 2014)

# network
# network() calls getdata() and it is not needed to run getdata() first
network("chl", "all", 6, 2014)

# network.compare
# network.compare() calls getdata() and it is not needed to run getdata() first
network.compare("chl", "all", 6, 1995, 2014)
