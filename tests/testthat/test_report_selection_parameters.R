# library(NVIpjsr)
library(testthat)
library(checkmate)

test_that("set disease parameters by direct input", {
  # skip if no connection to 'FAG' have been established
  skip_if_not(dir.exists(NVIdb::set_dir_NVI("FAG")))

  # Reads translation table for PJS codes
  PJS_codes_2_text <- read_PJS_codes_2_text()
  selection_parameters <- set_disease_parameters(hensikt2select = c("0100108018", "0100109003", "0100111003", "0800109"),
                                       analytt2select = c("01220104%", "1502010235"),
                                       metode2select = c("070070", "070231", "010057", "060265"),
                                       FUN = build_query_one_disease)

  selection <- report_selection_parameters(year = 2024,
                              selection_parameters = selection_parameters)

  expect_data_frame(selection, ncols = 4, nrows = 11, any.missing = FALSE)
  expect_identical(colnames(selection),
                   c("Status", "Variable", "Kode", "Beskrivelse"))
  expect_identical(selection$Kode,
                   c("2024", "0100108018", "0100109003", "0100111003", "0800109",
                   "01220104%", "1502010235",
                   "070070", "070231", "010057", "060265"))
  expect_identical(selection$Variable,
                   c("År", "Hensikt", "Hensikt", "Hensikt", "Hensikt",
                     "Analytt", "Analytt", "Metode", "Metode", "Metode", "Metode"))
  expect_identical(selection$Status,
                   c("Selektert", "Selektert", "Selektert", "Selektert", "Selektert",
                     "Selektert", "Selektert", "Selektert", "Selektert", "Selektert",
                     "Selektert"))


    selection <- report_selection_parameters(year = 2024,
                                           selection_parameters = selection_parameters,
                              additional_parameters = list(metode2delete = c("060026", "010001")))

  expect_data_frame(selection, ncols = 4, nrows = 13, any.missing = FALSE)
  expect_identical(selection$Kode,
                   c("2024", "0100108018", "0100109003", "0100111003", "0800109",
                   "01220104%", "1502010235",
                   "070070", "070231", "010057", "060265",
                   "060026", "010001"))
  expect_identical(selection$Variable,
                   c("År", "Hensikt", "Hensikt", "Hensikt", "Hensikt",
                     "Analytt", "Analytt", "Metode", "Metode", "Metode", "Metode",
                     "Metode", "Metode"))
  expect_identical(selection$Status,
                   c("Selektert", "Selektert", "Selektert", "Selektert", "Selektert",
                     "Selektert", "Selektert", "Selektert", "Selektert", "Selektert",
                     "Selektert", "Ekskludert", "Ekskludert"))

})

