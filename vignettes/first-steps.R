## ----functions1, eval = FALSE--------------------------------------------
#  # Install the package from CRAN (recommended)
#  install.packages("oec")
#  
#  # Or install the package from Github (this option or CRAN option, not both)
#  #install.packages("devtools") ## if needed
#  library(devtools)
#  install_github("observatory-economic-complexity/oec-r")
#  
#  # Load the package
#  library(oec)
#  
#  # Explore the countries list
#  countries_list

## ----functions2, eval = FALSE--------------------------------------------
#  # What does Andorra export? (2015, HS92 4 characters)
#  getdata("and", "all", 2015)
#  
#  # What does Andorra export? (2014 and 2015, HS92 4 characters)
#  getdata_interval("and", "all", 2014, 2015)

