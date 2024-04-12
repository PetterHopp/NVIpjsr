# NVIpjsr 0.0.0.9000 (2024-##-##)

## First release

Tools for retrieving, standardising, wrangling, preparing and reporting PJS data
and EOS data. The following functions are copied from NVIdb v0.11.3:

- `add_PJS_code_description` Manage translation of PJS codes to descriptive text

- `build_query_hensikt` Builds query for selecting data for hensikt from PJS

- `build_query_one_disease` Builds query for selecting data for one disease from PJS

- `build_query_outbreak` Builds query to select data for a disease outbreak from PJS

- `build_sql_select_code` Builds sql modules to be included in select statements for selecting code

- `build_sql_select_year` Builds sql modules to be included in select statements for selecting year(s)

- `choose_PJS_levels` Choose columns from specified PJS-levels

- `copy_PJS_code_2_text` Copy translation table for PJS codes to descriptive text

- `exclude_from_PJSdata` Exclude rows from PJS-data

- `login_by_credentials_PJS` Log in to PJS using stored credentials

- `login_by_input_PJS` Log in to PJS using manual input of credentials

- `login_PJS` Log in to PJS

- `read_eos_data` Read EOS data from RaData

- `read_PJS_code_2_text` Read translation table for PJS codes to descriptive text

- `retrieve_PJSdata` 	Retrieves data from PJS

- `select_PJSdata_for_value` Selects a subset of PJSdata based on code values

- `set_disease_parameters` Sets disease selection parameters

- `standardize_eos_data` Standardising EOS-data

- `standardize_PJSdata` Standardising PJS-data

- `transform_code_combinations` Transform combinations of code values into new values
