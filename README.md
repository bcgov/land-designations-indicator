
<!-- README.md is generated from README.Rmd. Please edit that file -->

[![img](https://img.shields.io/badge/Lifecycle-Stable-97ca00)](https://github.com/bcgov/repomountie/blob/master/doc/lifecycle-badges.md)

# Analysis of land designations that contribute to conservation

This repository contains R code that calculates summaries of the amount
of land designated in B.C. that contributes to conservation. It supports
this [Environmental Reporting BC
indicator](http://www.env.gov.bc.ca/soe/indicators/land/land-designations.html).

## Usage

### R packages required:

Other than `bcmaps` and `envreportutils`, all required packages are
available from CRAN

-   bcmaps (install with `devtools::install_github("bcgov/bcmaps")`)
-   envreportutils (install with
    `devtools::install_github("bcgov/envreportutils")`)
-   geojsonio
-   rmapshaper
-   feather
-   readr
-   dplyr
-   tidyr
-   sf
-   ggplot2
-   ggpolypath
-   magrittr
-   ggthemes
-   svglite

### Data

The inputs required to run this analysis can be obtained by downloading
the data via the [B.C. Data
Catalogue](https://catalogue.data.gov.bc.ca/dataset/3eedf0da-0c1d-4917-aff5-1eb8f59736bc).
They are released under the [Open Government Licence - British
Columbia](http://www2.gov.bc.ca/gov/content?id=A519A56BC2BF44E4A008B33FCF527F61):

-   `designatedlands.gpkg` - the land designations layer
-   `lands_bec.gpkg` - the land designations layer intersected with BEC.
-   `lands_eco.gpkg`- the land designations layer intersected with
    ecosections.

Place these in the `data` folder of this repository. You will also
require `BEC_BIOGEOCLIMATIC_POLY.gdb` from
[here](https://catalogue.data.gov.bc.ca/dataset/biogeoclimatic-ecosystem-classification-bec-map)
and `ERC_ECOSECTIONS_SP.gdb` from
[here](https://catalogue.data.gov.bc.ca/dataset/ecosections-ecoregion-ecosystem-classification-of-british-columbia)

The [`data-raw`](data-raw) folder contains a file
[`sources.csv`](data-raw/sources.csv) that lists all of the source land
designation layers used in the analysis.

### Running the analysis

-   Run the `01_clean.R` file to prepare the data. Note that this will
    take a long time (several hours)
-   Run the `02_analysis.R` file to calculate the summaries.
-   Summary csv files are copied to the `out` directory, and summary
    tabular objects for use in the [land designations Shiny
    app](https://github.com/bcgov/land-designations-shinyapp) are copied
    to the `out-shiny` directory.
-   Run the `03_output_static.R` script to create outputs (png maps and
    graphs etc).
-   Run the `04_output_shiny.R` scrip to generate the objects required
    for the shiny app. This will also copy the files from `out-shiny` to
    the `../land-designations-shinyapp/app/data` folder, assuming the
    folder exists in same parent directory as this repository

### Designated Lands Python script:

Input data was created by running the python script
[here](https://github.com/bcgov/designatedlands). Please note that the
code there is still being developed to make it more efficient.

It was run using the following commands:

    ## Download the Biogeoclimatic Zone layer from here: 
    ## https://catalogue.data.gov.bc.ca/dataset/biogeoclimatic-ecosystem-classification-bec-map
    ## and store it in your working directory

    ## Download the Ecosection layer from here: 
    ## https://catalogue.data.gov.bc.ca/dataset/ecosections-ecoregion-ecosystem-classification-of-british-columbia
    ## and store it in your working directory

    python designatedlands.py create_db

    python designatedlands.py load

    python designatedlands.py process

    python designatedlands.py dump --out_file=designatedlands.gpkg

    python designatedlands.py overlay BEC_BIOGEOCLIMATIC_POLY.gdb --in_layer=WHSE_FOREST_VEGETATION_BEC_BIOGEOCLIMATIC_POLY_polygon --new_layer_name=bec

    python designatedlands.py overlay ERC_ECOSECTIONS_SP.gdb --in_layer=WHSE_TERRESTRIAL_ECOLOGY_ERC_ECOSECTIONS_SP_polygon --new_layer_name=eco

    python designatedlands.py dump --out_table=bec_overlay --out_file=lands_bec.gpkg --aggregate_fields="bc_boundary,designation,category,zone,subzone,variant,phase,map_label"

    python designatedlands.py dump --out_table=eco_overlay --out_file=lands_eco.gpkg --aggregate_fields="bc_boundary,designation,category,parent_ecoregion_code,ecosection_code,ecosection_name"

## Getting Help or Reporting an Issue

To report bugs/issues/feature requests, please file an
[issue](https://github.com/bcgov-c/land-designations-indicator/issues/).

## How to Contribute

If you would like to contribute, please see our
[CONTRIBUTING](CONTRIBUTING.md) guidelines.

Please note that this project is released with a [Contributor Code of
Conduct](CODE_OF_CONDUCT.md). By participating in this project you agree
to abide by its terms.

## Licence

    Copyright 2016 Province of British Columbia

    Licensed under the Apache License, Version 2.0 (the "License");
    you may not use this file except in compliance with the License.
    You may obtain a copy of the License at 

       http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software
    distributed under the License is distributed on an "AS IS" BASIS,
    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
    See the License for the specific language governing permissions and
    limitations under the License.

This repository is maintained by [Environmental Reporting
BC](http://www2.gov.bc.ca/gov/content?id=FF80E0B985F245CEA62808414D78C41B).
Click [here](https://github.com/bcgov/EnvReportBC) for a complete list
of our repositories on GitHub.
