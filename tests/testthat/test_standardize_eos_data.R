# library(NVIpjsr)
library(testthat)
library(checkmate)

# Read data with saksnr
  eos_rapp <- NVIdb::login_by_credentials("EOS", dbinterface = "odbc")
  proveresultat_ila <- DBI::dbGetQuery(eos_rapp,
                                        "select * from proveresultat_ila_historikk where År = 2022")
  DBI::dbDisconnect(eos_rapp)

test_that("standardize_eos_data", {
  # skip if no connection to 'FAG' have been established
  skip_if_not(dir.exists(NVIdb::set_dir_NVI("FAG")))

  # PJS_codes_2_text <- read_PJS_codes_2_text()

  ila <- standardize_eos_data(proveresultat_ila, breed_to_species = FALSE)

  # Check transformation of colnames
  expect_equal(base::setdiff(colnames(ila), tolower(colnames(proveresultat_ila))),
               c("lopenr", "mottatt", "aar", "ansvarlig_seksjon", "innsendelsenr",
                 "rekvirenttype", "mt_avdelingnr", "eier_lokalitettype",
                 "eier_lokalitetnr", "eier_lokalitet", "regnr", "annen_aktortype",
                 "innehavernr", "innehaver", "ant_und_ila", "sist_overfort"))

  expect_equal(base::setdiff(tolower(colnames(proveresultat_ila)), colnames(ila)),
               c("id", "mottatt_dato", "år", "sek", "inr", "rekvirent_type",
                 "mt_nr", "eier_type", "lok_nr", "lok_navn", "reg_nr",
                 "annen_aktør_type", "innehaver_nr", "innehaver_navn",
                 "antall_undersokte_prover", "sist_oppdatert"))

  # Check transformation of numbers and dates
  expect_class(ila$mottatt, classes = "Date")
  expect_class(ila$sist_overfort, classes = "Date")
  expect_class(ila$lopenr, classes = "numeric")
  expect_class(ila$aar, classes = "numeric")
  expect_class(ila$ant_und_ila, classes = "numeric")

  # Check for correction of doubble registrations of MT-district
  expect_equal(nrow(proveresultat_ila[which(proveresultat_ila[, "Inr"] %in% c(421, 2322)), ]), 4)
  expect_equal(nrow(ila[which(ila[, "innsendelsenr"] %in% c(421, 2322)), ]), 2)
})

test_that("errors for read_eos_data", {

  linewidth <- options("width")
  options(width = 80)

  expect_error(standardize_eos_data(data = NA),
               regexp = "Variable 'data': Must be of type 'data.frame', not 'logical'")

  expect_error(standardize_eos_data(data = proveresultat_ila,
                                    standardize_colnames = "TRUE"),
               regexp = "Variable 'standardize_colnames': Must be of type 'logical flag'",
               fixed = TRUE)

  expect_error(standardize_eos_data(data = proveresultat_ila,
                                    breed_to_species = "TRUE"),
               regexp = "Variable 'breed_to_species': Must be of type 'logical flag'",
               fixed = TRUE)

  expect_error(standardize_eos_data(data = proveresultat_ila,
                                    adjust_n_examined = "TRUE"),
               regexp = "Variable 'adjust_n_examined': Must be of type 'logical flag'",
               fixed = TRUE)

  expect_error(standardize_eos_data(data = proveresultat_ila,
                                    delete_redundant = "TRUE"),
               regexp = "Variable 'delete_redundant': Must be of type 'logical flag'",
               fixed = TRUE)

  expect_error(standardize_eos_data(data = proveresultat_ila,
                                    standardize_colnames = "TRUE"),
               regexp = "Variable 'standardize_colnames': Must be of type 'logical flag'",
               fixed = TRUE)

  expect_error(standardize_eos_data(data = proveresultat_ila,
                                    standardize_colnames = "TRUE"),
               regexp = "Variable 'standardize_colnames': Must be of type 'logical flag'",
               fixed = TRUE)

  expect_error(standardize_eos_data(data = proveresultat_ila,
                                    standardize_colnames = "TRUE"),
               regexp = "Variable 'standardize_colnames': Must be of type 'logical flag'",
               fixed = TRUE)

  options(width = unlist(linewidth))
})
