# Copyright 2021 Province of British Columbia
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
# http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and limitations under the License.

library(dplyr)
library(tidyr)
library(purrr)
library(readr)
library(stringr)
library(ggplot2)
library(lubridate)
library(glue)
library(assertr)
library(sf)
library(bcmaps)
library(bcdata)
library(rmapshaper)
library(geojsonio)

# Clip BEC to BC outline ----------------------------------------------------
# We'll need simplified becs for plotting later
# NOTE: geojson doesn't have CRS so have to remind R that CRS is BC Albers
#       (It will warn that it's not transforming)
message("Clip BEC to BC outline")


if (!exists("ld_m")) load("tmp/raw_data_vect.RData") #load vector data


geojson_write(ld_m, file = "data/ld_m.geojson")
#geojson_write(bc, file = "data/bc.geojson")


system(glue("mapshaper-xl data/ld_m.geojson ",
            "-simplify 50% ",
            "-o data/ld_m_simp.geojson"))
