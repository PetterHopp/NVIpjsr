library(testthat)
library(checkmate)
# library(NVIpjsr)

test_that("retrieve_PJSdata", {
  # skip if no connection to 'FAG' have been established
  skip_if_not(dir.exists(NVIdb::set_dir_NVI("FAG")))

  # Read PJSdata using build_query_hensikt
  selection_parameters <- set_disease_parameters(hensikt2select = c("02001"),
                                                 FUN =  build_query_hensikt)
  PJSdata <- retrieve_PJSdata(year = 2015,
                              selection_parameters = selection_parameters)
  expect_data_frame(PJSdata[[1]], nrows = 0)
  expect_data_frame(PJSdata[[2]], nrows = 0)

  # Read PJSdata using build_query_one_disease
  selection_parameters <- set_disease_parameters(analytt2select = c("100201060101"),
                                                 FUN =  build_query_one_disease)
  PJSdata <- retrieve_PJSdata(year = 2020,
                              selection_parameters = selection_parameters)
  expect_data_frame(PJSdata[[1]], nrows = 17)
  expect_data_frame(PJSdata[[2]], nrows = 0)

  # Read PJSdata using build_query_outbreak
  selection_parameters <- set_disease_parameters(utbrudd2select = c("10"),
                                                 hensikt2select = "0100104010",
                                                 analytt2select = "041503020401",
                                                 FUN =  build_query_outbreak)
  PJSdata <- retrieve_PJSdata(year = 2023,
                              selection_parameters = selection_parameters)
  expect_data_frame(PJSdata[[1]], nrows = 205)
  expect_data_frame(PJSdata[[2]], nrows = 0)
)

})


# test_that("errors for read_eos_data", {
#
#   linewidth <- options("width")
#   options(width = 80)
#
#   # expect_error(read_eos_data(eos_table = NA, from_path = tempdir()),
#   #              regexp = "Variable 'eos_table': May not be NA")
#   #
#   # expect_error(read_eos_data(eos_table = "filename.csv", from_path = tempdir()),
#   #              regexp = "File\n * does not exist:",
#   #              fixed = TRUE)
#
#   options(width = unlist(linewidth))
# })
