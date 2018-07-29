library(crul)
library(testthat)

# make a stub
stub_request("get", "https://atlas.media.mit.edu/attr/country/") %>%
  to_return(status = 200)

# check that it's in the stub registry
stub_registry()

# make the request
z <- crul::HttpClient$new(url = "https://atlas.media.mit.edu/")$get("attr/country/")

# run tests (nothing returned means it passed)
expect_is(z, "HttpResponse")
expect_equal(z$status_code, 200)
