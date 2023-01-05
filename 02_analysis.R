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
if(!exists("ld_overlapping")){
  ld_overlapping = st_read('data/designatedlands.gpkg',
                           layer = 'designations_overlapping')
}

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

#For each natural resource district, crop the overlapping land designations sf object to its bounding box,
# then find the area of all land designations within that district (i.e. trimming away pieces of a land designation
# that fall outside the border of the district)
ld_overlapping_with_district = bc_reg %>%
  split(.$DISTRICT_NAME) %>%
  map( ~ {
    st_intersection(
      st_crop(ld_overlapping,st_as_sfc(st_bbox(.x))),
      .x
    )
  })
#The above operation takes about 10 minutes.

# Visual check of the spatial operation.
ggplot() +
  geom_sf(data = bc_reg[bc_reg$DISTRICT_NAME == '100 Mile House Natural Resource District',],
          size = 3) +
  geom_sf(data = ld_overlapping_with_district[[1]], fill = 'blue')

# Calculate area of each type of designation, e.g. "Ungulate Winter Range", "Wildlife Habitat Area",
# and also by restriction level.
overlapping_ld_areas_by_district = ld_overlapping_with_district %>%
  bind_rows() %>%
  mutate(ld_area = st_area(.)) %>%
  st_drop_geometry() %>%
  pivot_longer(cols = c('forest_restriction','mine_restriction','og_restriction'),
               names_to = 'industry',
               values_to = 'restriction_level') %>%
  filter(restriction_level != 0) %>%
  group_by(DISTRICT_NAME, designation, industry, restriction_level) %>%
  summarise(ld_area = as.numeric(sum(ld_area, na.rm=T))) %>%
  ungroup()

# Area of largest 10 designations for each natural resource district-restriction type-restriction level combination.
overlapping_ld_areas_by_district = overlapping_ld_areas_by_district %>%
  mutate(ld_area = ld_area/1000000) %>%
  mutate(industry = case_when(
    industry == 'forest_restriction' ~ "Forestry Restrictions",
    industry == 'mine_restriction' ~ "Mining Restrictions",
    industry == 'og_restriction' ~ "Oil/Gas Restrictions"
  )) %>%
  #Temporary?: drop the restriction level field, and only keep top 10 largest by restriction type and district name.
  group_by(DISTRICT_NAME,industry,designation,restriction_level) %>%
  arrange(desc(ld_area)) %>%
  slice(1:10) %>%
  # dplyr::select(-max_rest_ind_value) %>%
  # mutate(is_restricted = is.numeric(individual_area)) %>%
  # mutate(is_restricted = replace(is_restricted,
  #                               T,
  #                               "Yes")) %>%
  pivot_wider(names_from = industry,
              values_from = restriction_level,
              values_fill = 0) %>%
  dplyr::rename(`District Name` = DISTRICT_NAME,
                `Designation` = designation,
                # `Restriction Type` = max_rest_ind_type,
                `Area (km²)` = ld_area) %>%
  ungroup() %>%
  arrange(desc(`Area (km²)`))

# Add entries for province scale
overlapping_ld_areas_by_district = bind_rows(
  overlapping_ld_areas_by_district,
  overlapping_ld_areas_by_district %>%
    group_by(Designation,`Forestry Restrictions`,`Mining Restrictions`,`Oil/Gas Restrictions`) %>%
    summarise(`Area (km²)` = sum(`Area (km²)`)) %>%
    arrange(desc(`Area (km²)`)) %>%
    mutate(`District Name` = "Provincial")
)

# Convert restriction levels from numbers to text labels, convert designation data labels
# to readable titles.

overlapping_ld_areas_by_district = overlapping_ld_areas_by_district%>%
  mutate(across(ends_with('Restrictions'), ~ case_when(
    .x == 0 ~ "None",
    .x == 1 ~ "Low",
    .x == 2 ~ "Medium",
    .x == 3 ~ "High",
    .x == 4 ~ "Full",
    .x == 5 ~ "Full"
  ))) %>%
  mutate(Designation = case_when(
    Designation == 'uwr_conditional_harvest' ~ 'Ungulate Winter Range (conditional harvest)',
    Designation == 'great_bear_ebm_area' ~ 'Great Bear Rainforest Ecosystem-Based Management',
    Designation == 'muskwa_kechika_special_mgmt' ~ 'Muskwa-Kechika Management Area',
    Designation == 'park_provincial' ~ 'Provincial Park',
    Designation == 'great_bear_fisheries_watersheds' ~ 'Great Bear Rainforest Important Fisheries Watersheds',
    Designation == 'atlin_taku_fra' ~ 'Atlin-Taku Forest Retention Area',
    Designation == 'uwr_no_harvest' ~ 'Ungulate Winter Range (no harvest)',
    Designation == 'lrmp_hg' ~ 'Land and Resource Management Plan',
    Designation == 'wha_conditional_harvest' ~ 'Wildlife Habitat Area (conditional harvest)',
    Designation == 'muskwa_kechika_special_wildland' ~ 'Muskwa-Kechika Management Area',
    Designation == 'park_conservancy' ~ 'Park Conservancy',
    Designation == 'designated_area' ~ "Old Growth - Designated Area",
    Designation == 'vqo_modify' ~ 'Visual Quality Objective (modified)',
    Designation == 'fsw' ~ 'Fisheries Sensitive Watersheds',
    Designation == 'vqo_retain' ~ 'Visual Quality Objective (retained)',
    Designation == 'vqo_partretain' ~ 'Visual Quality Objective (partially retained)',
    Designation == 'wha_no_harvest' ~ 'Wildlife Habitat Area (no harvest)',
    Designation == 'ogma_legal' ~ 'Old Growth Management Areas (legal)',
    Designation == 'park_national' ~ 'National Park',
    Designation == 'community_watershed' ~ 'Community Watershed',
    Designation == 'wildland_area' ~ 'Wildland Area',
    Designation == 'mineral_reserve' ~ "Mineral Reserve",
    Designation == 'vqo_maxmodify' ~ 'Visual Quality Objective (max modify)',
    Designation == 'biodiv_mining_tourism_areas' ~ 'Biodiversity and Mining Tourism Areas',
    Designation == 'flathead' ~ 'Elk Flathead Natural Area',
    Designation == 'wildlife_management_area' ~ 'Wildlife Management Area',
    Designation == 'rec_site_med' ~ 'Recreation Site (Medium)',
    Designation == 'rec_site_high' ~ 'Recreation Site (High)',
    Designation == 'park_protectedarea' ~ 'Park & Protected Area',
    Designation == 'great_bear_grizzly_class1' ~ 'Grizzly Bear Habitat (High Suitability)',
    Designation == 'vqo_preserve' ~ 'Visual Quality Objective (Preserve)',
    Designation == 'ngo_fee_simple' ~ 'NGO Conservation Areas - Fee Simple',
    Designation == 'park_er' ~ 'Park & Ecological Reserve',
    Designation == 'lakes_corridors' ~ 'Lake Corridors',
    Designation == 'great_bear_grizzly_class2' ~ 'Grizzly Bear Habitat (Moderately High Suitability)',
    Designation == 's_chilcotin_mta' ~ 'South Chilcotin MTA',
    Designation == 'private_conservation_lands_admin' ~ 'Private Conservation Lands',
    Designation == 'creston_valley_wma' ~ 'Creston Valley Wildlife Management Area',
    Designation == 'nlhaxten_cayoosh_wildland_area' ~ 'Nlhaxten/Cayoosh Wildland Area',
    Designation == 'park_recreationarea' ~ 'Park Recreation Area',
    Designation == 'national_wildlife_area' ~ 'National Wildlife Area',
    Designation == 'migratory_bird_sanctuary' ~ 'Migratory Bird Sanctuary',
    T ~ Designation
  ))

write_csv(overlapping_ld_areas_by_district,
          'land_designations_webapp_3col/www/overlapping_land_designations_with_district.csv')

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

