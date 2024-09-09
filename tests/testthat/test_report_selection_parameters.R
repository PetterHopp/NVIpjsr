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
                                           selection_parameters = selection_parameters,
                                           translation_table = PJS_codes_2_text)

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


  selection <- report_selection_parameters(year = 2023,
                                           selection_parameters = selection_parameters,
                                           additional_parameters = list(metode2delete = c("060026", "010001")),
                                           translation_table = PJS_codes_2_text)

  expect_data_frame(selection, ncols = 4, nrows = 13, any.missing = FALSE)
  expect_identical(selection$Kode,
                   c("2023", "0100108018", "0100109003", "0100111003", "0800109",
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

  selection_parameters <- set_disease_parameters(hensikt2select = c("0100108018", "0100109003", "0100111003"),
                                                 hensikt2delete = c("0800109"),
                                                 utbrudd2select = "22",
                                                 analytt2select = c("01220104%", "1502010235"),
                                                 analytt2delete = "0601",
                                                 metode2select = "010001",
                                                 art2select = c("03%", NA),
                                                 include_missing_art = "always",
                                                 FUN = build_query_one_disease)
  selection <- report_selection_parameters(year = c(2022:2024),
                                           selection_parameters = selection_parameters,
                                           translation_table = PJS_codes_2_text)

  expect_data_frame(selection, ncols = 4, nrows = 11, any.missing = FALSE)
  expect_identical(colnames(selection),
                   c("Status", "Variable", "Kode", "Beskrivelse"))
  expect_identical(selection$Kode,
                   c("2022-2024", "0100108018", "0100109003", "0100111003", "22",
                     "01220104%", "1502010235",
                     "010001", "03%", "0800109", "0601"))
  expect_identical(selection$Variable,
                   c("År", "Hensikt", "Hensikt", "Hensikt", "Utbrudd",
                     "Analytt", "Analytt", "Metode", "Art", "Hensikt", "Analytt"))
  expect_identical(selection$Status,
                   c("Selektert", "Selektert", "Selektert", "Selektert", "Selektert",
                     "Selektert", "Selektert", "Selektert", "Selektert", "Ekskludert",
                     "Ekskludert"))


  selection_parameters <- set_disease_parameters(analytt2select = c("01220104%", "1502010235"),
                                                 FUN = build_query_one_disease)
  selection <- report_selection_parameters(year = c(2022),
                                           selection_parameters = selection_parameters,
                                           translation_table = PJS_codes_2_text)

  expect_data_frame(selection, ncols = 4, nrows = 3, any.missing = FALSE)
  expect_identical(colnames(selection),
                   c("Status", "Variable", "Kode", "Beskrivelse"))
  expect_identical(selection$Kode,
                   c("2022", "01220104%", "1502010235"))
  expect_identical(selection$Variable,
                   c("År", "Analytt", "Analytt"))
  expect_identical(selection$Status,
                   c("Selektert", "Selektert", "Selektert"))

  selection_parameters <- set_disease_parameters(hensikt2select = "0100108018",
                                                 FUN = build_query_hensikt)
  selection <- report_selection_parameters(year = c(2020),
                                           selection_parameters = selection_parameters,
                                           translation_table = PJS_codes_2_text)

  expect_data_frame(selection, ncols = 4, nrows = 2, any.missing = FALSE)
  expect_identical(colnames(selection),
                   c("Status", "Variable", "Kode", "Beskrivelse"))
  expect_identical(selection$Kode,
                   c("2020", "0100108018"))
  expect_identical(selection$Variable,
                   c("År", "Hensikt"))
  expect_identical(selection$Status,
                   c("Selektert", "Selektert"))


})


test_that("report disease parameters using parameter file", {
  # Reads translation table for PJS codes
  PJS_codes_2_text <- read_PJS_codes_2_text()

  writeLines(
    c('hensikt2select <- c("0100108018", "0100109003", "0100111003", "0800109")',
      'utbrudd2select <- NULL',
      'metode2select <- c("070070", "070231", "010057", "060265")',
      'analytt2select <- c("01220104%", "1502010235")'),
    con = file.path(tempdir(), "PD.R")
  )

  selection <- report_selection_parameters(year = 2022,
                                           selection_parameters = file.path(tempdir(), "PD.R"),
                                           translation_table = PJS_codes_2_text)

  expect_data_frame(selection, ncols = 4, nrows = 11, any.missing = FALSE)
  expect_identical(colnames(selection),
                   c("Status", "Variable", "Kode", "Beskrivelse"))
  expect_identical(selection$Kode,
                   c("2022", "0100108018", "0100109003", "0100111003", "0800109",
                     "01220104%", "1502010235",
                     "070070", "070231", "010057", "060265"))
  expect_identical(selection$Variable,
                   c("År", "Hensikt", "Hensikt", "Hensikt", "Hensikt",
                     "Analytt", "Analytt", "Metode", "Metode", "Metode", "Metode"))
  expect_identical(selection$Status,
                   c("Selektert", "Selektert", "Selektert", "Selektert", "Selektert",
                     "Selektert", "Selektert", "Selektert", "Selektert", "Selektert",
                     "Selektert"))
})



test_that("errors for report_selection_parameters", {
  linewidth <- options("width")
  options(width = 80)

  # Reads translation table for PJS codes
  PJS_codes_2_text <- read_PJS_codes_2_text()

  expect_error(report_selection_parameters(year = NA,
                                           selection_parameters = list("analytt2select" = c("01220104%", "1502010235"),
                                                                       "FUN" = build_query_one_disease),
                                           additional_parameters = NULL,
                                           translation_table = PJS_codes_2_text),
               regexp = "Variable 'year': Contains missing values")

  expect_error(report_selection_parameters(year = 2022,
                                           selection_parameters = NA,
                                           additional_parameters = NULL,
                                           translation_table = PJS_codes_2_text),
               regexp = "Variable 'selection_parameters': One of the following must apply:")

  expect_error(report_selection_parameters(year = 2022,
                                           selection_parameters = list("analytt2select" = c("01220104%", "1502010235"),
                                                                       "FUN" = build_query_one_disease),
                                           additional_parameters = NA,
                                           translation_table = PJS_codes_2_text),
               regexp = "Variable 'additional_parameters': Must be of type 'list'")

  expect_error(report_selection_parameters(year = 2022,
                                           selection_parameters = list("analytt2select" = c("01220104%", "1502010235"),
                                                                       "FUN" = build_query_one_disease),
                                           additional_parameters = NULL,
                                           translation_table = "PJS_codes_2_text"),
               regexp = "Variable 'translation_table': Must be of type 'data.frame'")


  options(width = unlist(linewidth))
})
