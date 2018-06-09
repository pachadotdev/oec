context("test-oec.R")

test_that("getdata connects to the API and returns a valid tibble after valid input", {
  if (has_internet()) {
    # Bilateral trade Chile-Argentina (SITC, 1980)
    test_data <- getdata("chl", "arg", 1980)
    expect_is(test_data, "tbl")
    expect_is(test_data, "data.frame")
    expect_output(str(test_data), "30 variables")
  } else {
    skip("No internet connection")
  }
})

test_that("getdata connects to the API and returns an error after invalid input", {
  if (has_internet()) {
    expect_error(getdata("chl", "arg", 1980, "hs92"), 
                 "Provided that you requested HS92 data please verify that the data you are requesting is contained within the years 1992-2016.")
  } else {
    skip("No internet connection")
  }
})
