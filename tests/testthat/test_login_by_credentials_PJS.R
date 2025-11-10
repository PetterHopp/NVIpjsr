# library(NVIpjsr)
library(testthat)
# library(checkmate)


test_that("errors for login_by_credentials_PJS", {

  linewidth <- options("width")
  options(width = 80)

  expect_error(login_by_credentials_PJS(dbinterface = "ODBC"),
               regexp = "Variable 'dbinterface': Must be element of set")

  options(width = unlist(linewidth))
})
