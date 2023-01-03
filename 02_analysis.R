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

## Land Designations Indicator - 01_analysis.R

library(dplyr)
library(tidyverse)
library(sf)
library(RStoolbox)

rm(list = ls())

#If the vector data is not in the working memory, load it in.
if (!exists("ld")) load("tmp/raw_data_vect.RData")

#If out and out-shiny folders don't yet exist, create them.
if(!dir.exists('out')) dir.create("out", showWarnings = FALSE)
if(!dir.exists('out-shiny'))dir.create("out-shiny", showWarnings = FALSE)

# # Set tolerance for simplifying geometries.
# my_dTolerance = 1500

# Assign a regional district name to each polygon of land designation gpkg.
# bc_reg = bcmaps::regional_districts() %>% dplyr::select(DISTRICT_NAME)
bc_reg = bcmaps::nr_districts() %>% dplyr::select(DISTRICT_NAME)

ld_with_reg = ld %>%
  st_join(bc_reg, st_intersects)
#Started at 2:10 PM earliest... finished ~ 2:35 PM

# Calculate the area of each designation, grouped by district. Only keep the largest 50 designations for each district-restriction type-restriction number combination.
ld_areas_with_reg = ld_with_reg %>%
  #Pivot table longer, combining into one column the 3 industries' max restrictions.
  pivot_longer(cols = c(ends_with('_max')), names_to = 'max_rest_ind_type', values_to = 'max_rest_ind_value') %>%
  #Remove rows with 'max_rest_ind_value' equal to 0 (i.e. there was no such restriction for that parcel of land)
  filter(max_rest_ind_value != 0) %>%
  #Calculate the area of each individual parcel. We'll sum these later.
  mutate(individual_area = st_area(.)) %>%
  st_drop_geometry() %>%
  dplyr::select(DISTRICT_NAME,designations_planarized_id,
                max_rest_ind_type,max_rest_ind_value,
                individual_area) %>%
  distinct() %>%
  group_by(DISTRICT_NAME,max_rest_ind_type,max_rest_ind_value) %>%
  arrange(desc(individual_area)) %>%
  slice(1:10)

write_csv(ld_areas_with_reg, "land_designations_webapp_3col/land_designations_area_with_district.csv")

#Find area/proportion for each restriction level/type

ld_sum = ld_with_reg %>%
  #Pivot table longer, combining into one column the 3 industries' max restrictions.
  pivot_longer(cols = c(ends_with('_max')), names_to = 'max_rest_ind_type', values_to = 'max_rest_ind_value') %>%
  #Remove rows with 'max_rest_ind_value' equal to 0 (i.e. there was no such restriction for that parcel of land)
  #filter(max_rest_ind_value != 0) %>%
  #Calculate the area of each individual parcel. We'll sum these later.
  mutate(individual_area = st_area(.)) %>%
  st_drop_geometry() %>%
  #Sum together the
  group_by(max_rest_ind_type, max_rest_ind_value) %>%
  summarise(area_by_rest_type_and_level_ha = round(sum(individual_area, na.rm=T) / 10000, digits = 2),
         parcels_in_rest_type_and_level = n()) %>%
  #Calculate percentage of BC surface area in hectares for each type / level.
  mutate(perc_bc_ha = round(area_by_rest_type_and_level_ha/bc_size_ha*100, digits=1)) %>%
  rename(restriction = max_rest_ind_value) %>%
  ungroup()

ld_regdist_sum = ld_with_reg %>%
  #Pivot table longer, combining into one column the 3 industries' max restrictions.
  pivot_longer(cols = c(ends_with('_max')), names_to = 'max_rest_ind_type', values_to = 'max_rest_ind_value') %>%
  #Remove rows with 'max_rest_ind_value' equal to 0 (i.e. there was no such restriction for that parcel of land)
  #filter(max_rest_ind_value != 0) %>%
  #Calculate the area of each individual parcel. We'll sum these later.
  mutate(individual_area = st_area(.)) %>%
  st_drop_geometry() %>%
  #Sum together the
  group_by(max_rest_ind_type, max_rest_ind_value,DISTRICT_NAME) %>%
  summarise(area_by_rest_type_and_level_ha = round(sum(individual_area, na.rm=T) / 10000, digits = 2),
            parcels_in_rest_type_and_level = n()) %>%
  #Calculate percentage of BC surface area in hectares for each type / level.
  mutate(perc_bc_ha = round(area_by_rest_type_and_level_ha/bc_size_ha*100, digits=1)) %>%
  rename(restriction = max_rest_ind_value) %>%
  ungroup()

rest_key <- tribble(
  ~ restriction, ~ r_level, ~ category,
  NA , "None", "None",
  1 , "Full", "Parks & Protected Areas",
  2 , "High", "Other Protected Lands",
  3 , "Medium", "Resource Exclusion Areas",
  4 , "Low", "Managed Areas",
  5 , "Very Low", "Other"
)

cons_area_all <- ld_sum %>%
  left_join(rest_key)

cons_area_by_regdist = ld_regdist_sum %>%
  left_join(rest_key)

# split the land designations planarized gpkg into simplified layers for each industry.
# ld_forest = ld %>%
#   dplyr::select(-mine_restrictions,-og_restrictions,-mine_restriction_max,-og_restriction_max,-map_tile) %>%
#   filter(forest_restriction_max != 0) %>%
#   mutate(forest_restriction_max = as.factor(forest_restriction_max))
# ld_forest_s = sf::st_simplify(ld_forest, preserveTopology = T, dTolerance = my_dTolerance)
# ld_forest_m = ld_forest_s %>% st_make_valid() %>%
#   group_by(designations,forest_restrictions,forest_restriction_max) %>%
#   summarise()
# select subset of cities for plotting - choose largest city in each natural resource district

message("Add nr districts regions")

ld_cities <- st_intersection(bcmaps::nr_districts(), bcmaps::bc_cities()) %>%
  group_by(DISTRICT_NAME) %>%
  slice_max(POP_2000, n=1) %>%
  dplyr::select(DISTRICT_NAME, NAME, POP_2000, geometry)

save(ld_with_reg, file = 'tmp/ld_with_reg.RData')
save(ld_cities, cons_area_all, cons_area_by_regdist, file = "tmp/clean.RData")

#
# ggplot() + geom_sf(data = ld_forest_m,
#                    aes(fill = forest_restriction_max), col = 'transparent') +
#   scale_fill_brewer(palette = 'Greens')

