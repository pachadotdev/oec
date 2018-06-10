context("test-oec.R")

test_that("getdata connects to the API and returns a valid tibble after valid input", {
  if (curl::has_internet()) {
    # Bilateral trade Chile-Argentina (SITC, 1980)
    test_data <- getdata("chl", "arg", 1980)
    expect_is(test_data, "tbl")
    expect_is(test_data, "data.frame")
    expect_output(str(test_data), "30 variables")
  } else {
    skip("No internet connection")
  }
})

test_that("getdata connects to the API and returns a valid tibble after valid input", {
  if (curl::has_internet()) {
    # Multilateral trade Chile-World (SITC, 1980)
    test_data <- getdata("chl", "all", 1980)
    expect_is(test_data, "tbl")
    expect_is(test_data, "data.frame")
    expect_output(str(test_data), "30 variables")
  } else {
    skip("No internet connection")
  }
})

test_that("getdata connects to the API and returns an error after invalid input", {
  if (curl::has_internet()) {
    # Bilateral trade Chile-Argentina (HS92, 1980) - Error message
    expect_error(getdata("chl", "arg", 1980, "hs92"), 
                 "Provided that you requested HS92 data please
           verify that the data you are requesting is
           contained within the years 1992-2016.")
  } else {
    skip("No internet connection")
  }
})

test_that("getdata connects to the API and returns an error after invalid input", {
  if (curl::has_internet()) {
    # Bilateral trade Chile-Argentina (HS96, 1980) - Error message
    expect_error(getdata("chl", "arg", 1980, "hs96"), 
                 "Provided that you requested HS96 data please
           verify that the data you are requesting is
           contained within the years 1996-2016.")
  } else {
    skip("No internet connection")
  }
})

test_that("getdata connects to the API and returns an error after invalid input", {
  if (curl::has_internet()) {
    # Bilateral trade Chile-Argentina (HS02, 1980) - Error message
    expect_error(getdata("chl", "arg", 1980, "hs02"), 
                 "Provided that you requested HS02 data please
           verify that the data you are requesting is
           contained within the years 2002-2016.")
  } else {
    skip("No internet connection")
  }
})

test_that("getdata connects to the API and returns an error after invalid input", {
  if (curl::has_internet()) {
    # Bilateral trade Chile-Argentina (HS07, 1980) - Error message
    expect_error(getdata("chl", "arg", 1980, "hs07"), 
                 "Provided that you requested HS07 data please
           verify that the data you are requesting is
           contained within the years 2007-2016.")
  } else {
    skip("No internet connection")
  }
})
