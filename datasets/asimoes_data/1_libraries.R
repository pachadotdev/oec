# Open the project datasets.Rproj one level up to run this script

###########################
# 1. Libraries
###########################

# 1.1: define the libraries to use
libraries <- c("data.table", "jsonlite")

# 1.2: this is the function to download and or load libraries on the fly
download_and_or_load <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

# 1.3: use the function from step 2
download_and_or_load(libraries)

rm(libraries,download_and_or_load)

