# Copyright 2016 Province of British Columbia
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


library(rgdal)
library(sp)
library(rgeos)
library(raster)
library(rmapshaper)
library(ggplot2)
library(ggthemes)
library(dplyr)
library(tidyr)
library(feather)
library(bcmaps)
library(readr)

source("fun.R")

dir.create("out", showWarnings = FALSE)

## BC Summary
bc_ld_summary <- ld_t@data %>%
  group_by(category) %>%
  summarize(area_des_ha = sum(area) * 1e-4) %>%
  mutate(percent_des = (area_des_ha * 1e4) / sum(bc_bound_trim$SHAPE_Area) * 100) %>%
  mutate_if(is.numeric, round, digits = 2) %>%
  write_feather("out/bc_ld_summary.feather")

################################################################################
# BEC

bec_cat_summary <- bec_ld_t@data %>%
  group_by(map_label, category = factor(category)) %>%
  summarize(area_des = sum(shape_area, na.rm = TRUE)) %>%
  right_join(group_by(bec_t@data, MAP_LABEL, ZONE, ZONE_NAME, SUBZONE, SUBZONE_NAME,
                      VARIANT, VARIANT_NAME) %>%
               summarize(bec_area = sum(bec_area, na.rm = TRUE)),
             by = c("map_label" = "MAP_LABEL")) %>%
  mutate(percent_des = area_des / bec_area * 100,
         area_des_ha = area_des * 1e-4) %>%
  complete(nesting(map_label, ZONE, ZONE_NAME, SUBZONE, SUBZONE_NAME,
                   VARIANT, VARIANT_NAME, bec_area), category,
           fill = list(area_des = 0, area_des_ha = 0, percent_des = 0)) %>%
  mutate(category = as.character(category)) %>%
  rename(MAP_LABEL = map_label) %>%
  write_feather("out/ld_bec_summary.feather")

gg_ld_x_bec <- gg_fortify(ld_x_bec_simp) %>% write_feather("out/gg_ld_bec.feather")

################################################################################
# Ecoregions

eco_cat_summary <- eco_ld_t@data %>%
  group_by(CRGNCD = parent_ecoregion_code, category = factor(category)) %>%
  summarize(area_des = sum(shape_area, na.rm = TRUE)) %>%
  complete(CRGNCD, category,
           fill = list(area_des = 0)) %>%
  mutate(category = as.character(category)) %>%
  right_join(select(ecoregions_t@data, CRGNCD, ecoreg_area),
             by = "CRGNCD") %>%
  mutate(percent_des = area_des / ecoreg_area * 100,
         area_des_ha = area_des * 1e-4) %>%
  write_feather("out/ld_ecoreg_summary.feather")

