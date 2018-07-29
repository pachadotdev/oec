context("test-oec.R")

test_that("get_countrycode works properly for single matching", {
  test_country_1 <- get_countrycode("Chile")
  test_country_2 <- get_countrycode("CHILE")
  test_country_3 <- get_countrycode("Chil")
  
  expect_output(str(test_country_1), "chl")
  expect_output(str(test_country_2), "chl")
  expect_output(str(test_country_3), "chl")
})

test_that("get_countrycode works properly for multiple matching", {
  test_country_1 <- get_countrycode("Germany")
  test_country_2 <- get_countrycode("GERMANY")
  
  expect_is(test_country_1, "tbl")
  expect_is(test_country_2, "tbl")
  expect_output(str(test_country_1), "2 variables")
  expect_output(str(test_country_2), "2 variables")
})