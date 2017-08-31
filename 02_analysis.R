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

library(dplyr)
library(tidyr)
library(feather)
library(readr)
library(sf)

source("fun.R")

dir.create("out", showWarnings = FALSE)
dir.create("out-shiny", showWarnings = FALSE)

## BC Summary
bc_ld_summary <- ld_t %>%
  st_set_geometry(NULL) %>%
  group_by(category) %>%
  filter_non_designated() %>%
  summarize(area_des_ha = as.numeric(sum(calc_area)) * 1e-4) %>%
  mutate(percent_des = (area_des_ha * 1e4) / as.numeric(sum(ld_t$calc_area)) * 100) %>%
  mutate_if(is.numeric, round, digits = 2) %>%
  write_feather("out-shiny/bc_ld_summary.feather") %>%
  write_csv("out/bc_land_designations_summary.csv")

################################################################################
# BEC

bec_cat_summary <- bec_ld %>% st_set_geometry(NULL) %>%
  filter_non_designated() %>%
  group_by(map_label, category = factor(category)) %>%
  summarize(area_des = sum(calc_area, na.rm = TRUE)) %>%
  right_join(group_by(st_set_geometry(bec_t, NULL), MAP_LABEL, ZONE, ZONE_NAME,
                      SUBZONE, SUBZONE_NAME, VARIANT, VARIANT_NAME) %>%
               summarize(bec_area = sum(bec_area, na.rm = TRUE)),
             by = c("map_label" = "MAP_LABEL")) %>%
  mutate(percent_des = as.numeric(area_des / bec_area * 100),
         area_des_ha = area_des * 1e-4) %>%
  complete(nesting(map_label, ZONE, ZONE_NAME, SUBZONE, SUBZONE_NAME,
                   VARIANT, VARIANT_NAME, bec_area), category,
           fill = list(area_des = 0, area_des_ha = 0, percent_des = 0)) %>%
  mutate(category = as.character(category)) %>%
  rename(MAP_LABEL = map_label) %>%
  write_feather("out-shiny/ld_bec_summary.feather") %>%
  write_csv("out/bc_bgc_land_designations_summary.csv")

bec_zone_cat_summary <- bec_cat_summary %>%
  group_by(ZONE, ZONE_NAME, category) %>%
  summarize(area_designated_ha = sum(area_des_ha, na.rm = TRUE),
            zone_area_ha = sum(bec_area, na.rm = TRUE) * 1e-4,
            percent_designated = as.numeric(area_designated_ha / zone_area_ha * 100)) %>%
  mutate_if(is.numeric, round, digits = 2) %>%
  arrange(ZONE, category) %>%
  write_csv("out/bc_bgc_zone_land_designations_summary.csv")


bec_zone_sizes <- bec_t%>% st_set_geometry(NULL) %>%
  group_by(ZONE) %>%
  summarise(area = sum(bec_area, na.rm = TRUE) * 1e-4)
################################################################################
# Ecoregions and ecosections ## TODO - add ecosection_name back in

ecosec_sizes <- eco_ld %>% st_set_geometry(NULL) %>%
  group_by(parent_ecoregion_code, ecosection_name,
           ecosection_code) %>%
  summarize(ecosec_area = sum(calc_area, na.rm = TRUE))

ecoreg_sizes <- ecosec_sizes %>%
  group_by(ecoregion_code = parent_ecoregion_code) %>%
  summarize(ecoreg_area = sum(ecosec_area, na.rm = TRUE))

ecosection_cat_summary <- eco_ld %>% st_set_geometry(NULL) %>%
  filter_non_designated() %>%
  right_join(ecosec_sizes, by = c("parent_ecoregion_code", "ecosection_name",
                                  "ecosection_code")) %>%
  complete(nesting(parent_ecoregion_code, ecosection_name,
                   ecosection_code, ecosec_area), category) %>%
  group_by(category, parent_ecoregion_code, ecosection_code, ecosection_name) %>%
  summarize(area_des = sum(calc_area, na.rm = TRUE),
            ecosec_area = sum(ecosec_area, na.rm = TRUE),
            percent_des = as.numeric(area_des / ecosec_area * 100),
            area_des_ha = area_des * 1e-4) %>%
  write_csv("out/bc_ecosections_land_designations_summary.csv")

ecoregion_cat_summary <- ecosection_cat_summary %>%
  group_by(ecoregion_code = parent_ecoregion_code, category) %>%
  summarize(area_des = sum(area_des, na.rm = TRUE)) %>%
  right_join(ecoreg_sizes, by = "ecoregion_code") %>%
  mutate(percent_des = as.numeric(area_des / ecoreg_area * 100),
            area_des_ha = area_des * 1e-4) %>%
  left_join(select(bcmaps::ecoregions@data, ecoregion_code = CRGNCD, ecoregion_name = CRGNNM)) %>%
  mutate(ecoregion_name = tools::toTitleCase(tolower(ecoregion_name))) %>%
  select(ecoregion_code, ecoregion_name, category, everything()) %>%
  write_feather("out-shiny/ld_ecoreg_summary.feather") %>%
  write_csv("out/bc_ecoregions_land_designations_summary.csv")
