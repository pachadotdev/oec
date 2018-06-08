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

test_that("getdata connects to the API and returns an error after invalid input", {
  if (curl::has_internet()) {
    expect_error(getdata("chl", "arg", 1980, "hs92"), "all\\(years >= 1992\\) is not TRUE")
  } else {
    skip("No internet connection")
  }
})
