# NVIpjsr: Tools to facilitate working with PJS data <img src="man/figures/NVIpjsr_logo.png" align="right" height="138" />

<!-- README.md is generated from README.Rmd. Please edit that file -->

-   [Overview](#overview)
-   [Installation](#installation)
-   [Usage](#usage)
-   [Copyright and license](#copyright-and-license)
-   [Contributing](#contributing)

## Overview

`NVIpjsr`tools for retrieving, standardising, wrangling, preparing and
reporting PJS data and EOS data. NVIpjsr was created by separating out
PJS-functions from NVIdb. Further development of and creation of new
PJS-functions will take place within NVIpjsr.

`NVIpjsr` is part of `NVIverse`, a collection of R-packages with tools
to facilitate data management and data reporting at the Norwegian
Veterinary Institute (NVI). The `NVIverse` consists of the following
packages: `NVIconfig`, `NVIdb`, `NVIpjsr`, `NVIspatial`, `NVIpretty`,
`NVIbatch`, `OKplan`, `OKcheck`, `NVIcheckmate`, `NVIpackager`,
`NVIrpackages`. See [Contribute to
NVIpjsr](https://github.com/NorwegianVeterinaryInstitute/NVIpjsr/blob/main/CONTRIBUTING.md)
for more information.

## Installation

`NVIpjsr` is available at
[GitHub](https://github.com/NorwegianVeterinaryInstitute). To install
`NVIpjsr` you will need:

-   R version &gt; 4.1.0
-   R package `remotes`
-   Rtools version 4.0, 4.2, 4.3 or 4.4 depending on R version

First install and attach the `remotes` package.

    install.packages("remotes")
    library(remotes)

To install (or update) the `NVIpjsr` package without vignettes, run the
following code:

    remotes::install_github("NorwegianVeterinaryInstitute/NVIpjsr",
        upgrade = FALSE,
        build = TRUE,
        build_vignettes = FALSE)

To install (or update) the `NVIpjsr` package with vignettes, you will
need to first install some additional R-packages needed to build the
vignettes. Check README below in the section [Vignettes](#vignettes) to
see which vignettes are available. To install the package with the
vignettes, first install the packages: `knitr`, `rmarkdown`, `R.rsp`,
and `NVIrpackages` (from GitHub) if they are missing. If you don’t use
R-studio, you will also need to install Pandoc. Then run the following
code:

    remotes::install_github("NorwegianVeterinaryInstitute/NVIpjsr",
        upgrade = FALSE,
        build = TRUE,
        build_vignettes = TRUE)

## Usage

The `NVIpjsr` package needs to be attached.

    library(NVIpjsr)

`NVIpjsr` tools for retrieving, standardising, wrangling, preparing and
reporting PJS data and EOS data. NVIpjsr was created by separating out
PJS-functions from NVIdb. Further development of and creation of new
PJS-functions will take place within NVIpjsr.

#### Further documentation

##### Help

The full list of all available functions and datasets can be accessed by
typing

    help(package = "NVIpjsr")

##### Vignettes

Consult the vignettes for task-oriented help.

    vignette(package = "NVIpjsr")

Vignettes in package `NVIpjsr`:

-   Contribute to NVIpjsr (html)  
-   NVIpjsr reference manual (pdf)  
-   Retrieve and standardise PJS-data (html)

##### NEWS

Please check the
[NEWS](https://github.com/NorwegianVeterinaryInstitute/NVIpjsr/blob/main/NEWS)
for information on new features, bug fixes and other changes.

## Copyright and license

Copyright (c) 2024 Norwegian Veterinary Institute.  
Licensed under the BSD\_3\_clause License. See
[License](https://github.com/NorwegianVeterinaryInstitute/NVIpjsr/blob/main/LICENSE)
for details.

## Contributing

Contributions to develop `NVIpjsr` is highly appreciated. There are
several ways you can contribute to this project: ask a question, propose
an idea, report a bug, improve the documentation, or contribute code.
See [Contribute to
NVIpjsr](https://github.com/NorwegianVeterinaryInstitute/NVIpjsr/blob/main/CONTRIBUTING.md)
for more information.

------------------------------------------------------------------------

<!-- Code of conduct -->

Please note that the NVIpjsr project is released with a [Contributor
Code of
Conduct](https://github.com/NorwegianVeterinaryInstitute/NVIpjsr/blob/main/CODE_OF_CONDUCT.md).
By contributing to this project, you agree to abide by its terms.
