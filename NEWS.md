# NVIpjsr 0.2.0.9000 - (2025-##-##)

## New features:

- `add_PJS_code_description` will now translate "delpr_forbehandlingkode" when the arguments `new_column` = `"auto"` and/or `PJS_variable_type` = `"auto"`.


## Bug fixes:

- 


## Other changes:

-


## BREAKING CHANGES:

-


# NVIpjsr 0.2.0 - (2025-02-07)

## New features:

- Created `report_selection_parameters` which generates a data frame with the selection parameters used when selecting from PJS, for reporting selection parameters to an Excel file or others.

- `add_PJS_code_description` will now translate "annen_aktor_rollekode", "delpr_provematerialekode" and "delpr_provematerialetype" when the arguments `new_column` = `"auto"` and/or `PJS_variable_type` = `"auto"`.


## Bug fixes:

- `set_disease_parameters` now keeps the names of the elements if using named list in the `selection_statement`.

- `set_disease_parameters` now handles double spaces before and after =, <- when reading disease parameters from a file.

- `retrieve_PJSdata` now don't get an error if all list elements in the `selection_parameters$selection_statement` is unnamed.

- `select_PJSdata_for_value` now don't get an error when the input data has 0 rows.


## Other changes:

- Created startup message when `NVIdb` is attached after `NVIpjsr`.

- Improved examples in help for several functions.


# NVIpjsr 0.1.2 - (2024-08-20)

## Bug fixes:

- removed error for `retrieve_PJSdata` when no additional arguments.


# NVIpjsr 0.1.1 - (2024-08-20)

## Bug fixes:

- `retrieve_PJSdata` now accepts additional arguments in dots.

- The build_query-functions now correctly calls `NVIpjsr::build_sql_select_year` and `NVIpjsr::build_sql_select_code`.

- `choose_PJS_levels` now correctly calls `NVIpjsr::PJS_levels`.

- `select_PJSdata_for_value` now correctly calls `NVIpjsr::PJS_levels`.


## Other changes:

- `NVIpjsr` now uses the internal pipe "|>" and therefore requires R >= 4.1.0.

- simplified code for some functions.


# NVIpjsr 0.1.0 - (2024-05-03)

## First release

Tools for retrieving, standardising, wrangling, preparing and reporting PJS data
and EOS data. `NVIpjsr` was created by separating out PJS-functions from `NVIdb`.
The following functions were copied from `NVIdb v0.11.3`:

- `add_PJS_code_description` Translates PJS codes to descriptive text

- `build_query_hensikt` Builds query for selecting data for hensikt from PJS

- `build_query_one_disease` Builds query for selecting data for one disease from PJS

- `build_query_outbreak` Builds query to select data for a disease outbreak from PJS

- `build_sql_select_code` Builds sql modules to be included in select statements for selecting code

- `build_sql_select_year` Builds sql modules to be included in select statements for selecting year(s)

- `choose_PJS_levels` Choose columns from specified PJS-levels

- `copy_PJS_code_2_text` Copy translation table for PJS codes to descriptive text

- `exclude_from_PJSdata` Exclude rows from PJS-data

- `login_by_credentials_PJS` Log in to PJS using stored credentials

- `read_eos_data` Read EOS data from RaData

- `read_PJS_code_2_text` Read translation table for PJS codes to descriptive text

- `retrieve_PJSdata` 	Retrieves data from PJS

- `select_PJSdata_for_value` Selects a subset of PJSdata based on code values

- `set_disease_parameters` Sets disease selection parameters

- `standardize_eos_data` Standardising EOS-data

- `standardize_PJSdata` Standardising PJS-data

- `transform_code_combinations` Transform combinations of code values into new values
