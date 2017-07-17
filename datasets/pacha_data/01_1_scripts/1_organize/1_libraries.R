# Open the project datasets.Rproj two levels up to run this script

# 1: define the libraries to use
libraries = c("data.table","dplyr","dtplyr","tidyr","bit64")

# 2: this is the function to download and or load libraries on the fly
download_and_or_load <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

# 3: use the function from step 2
download_and_or_load(libraries)

rm(libraries,download_and_or_load)
