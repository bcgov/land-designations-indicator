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
library(tidyr)
library(ggplot2)
library(purrr)
library(sf)
library(bcmaps)
# library(RStoolbox)

# Read in data from the python geoprocessing script.
if (!exists("ld")) ld = read_sf('data/designatedlands.gpkg',
                                layer = 'designations_planarized')
if(!exists("ld_overlapping")){
  ld_overlapping = st_read('data/designatedlands.gpkg',
                           layer = 'designations_overlapping')
}

#If out and out-shiny folders don't yet exist, create them.
if(!dir.exists('out')) dir.create("out", showWarnings = FALSE)
if(!dir.exists('out-shiny'))dir.create("out-shiny", showWarnings = FALSE)

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

# Calculate area of each type of designation, e.g. "Ungulate Winter Range", "Wildlife Habitat Area",
# split by natural resource district, industry type, and restriction level.
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

# Convert area from square meters to square kilometers and pivot industry restrictions columns wider.
overlapping_ld_areas_by_district = overlapping_ld_areas_by_district %>%
  mutate(ld_area = ld_area/1000000) %>%
  mutate(industry = case_when(
    industry == 'forest_restriction' ~ "Forestry Restrictions",
    industry == 'mine_restriction' ~ "Mining Restrictions",
    industry == 'og_restriction' ~ "Oil/Gas Restrictions"
  )) %>%
  group_by(DISTRICT_NAME,industry,designation,restriction_level) %>%
  arrange(desc(ld_area)) %>%
  slice(1:10) %>%
  pivot_wider(names_from = industry,
              values_from = restriction_level,
              values_fill = 0) %>%
  dplyr::rename(`District Name` = DISTRICT_NAME,
                `Designation` = designation,
                `Area (km²)` = ld_area) %>%
  ungroup() %>%
  arrange(desc(`Area (km²)`))

# Add entries for province scale (i.e. sum area of each district, split by designations and industry type)
overlapping_ld_areas_by_district = bind_rows(
  overlapping_ld_areas_by_district,
  overlapping_ld_areas_by_district %>%
    group_by(Designation,`Forestry Restrictions`,`Mining Restrictions`,`Oil/Gas Restrictions`) %>%
    summarise(`Area (km²)` = sum(`Area (km²)`)) %>%
    arrange(desc(`Area (km²)`)) %>%
    mutate(`District Name` = "Provincial")
)

# Read in 'sources_designations' file to convert short designation names to readable labels.
designation_labels = read.csv('data-raw/sources_designations.csv')

overlapping_ld_areas_by_district = overlapping_ld_areas_by_district %>%
  left_join(designation_labels %>%
            dplyr::select(name, designation) %>%
              rename(Designation = designation))

# Three designation types are not listed in the 'sources_designations' file. These are:
# 1. wildland_area
# 2. rec_site_med
# 3. rec_site_high
# For now, replace those with labels.
overlapping_ld_areas_by_district = overlapping_ld_areas_by_district %>%
  mutate(name = case_when(
    Designation == 'wildland_area' ~ 'Wildland Area',
    Designation == 'rec_site_med' ~ 'Recreation Site (Medium)',
    Designation == 'rec_site_high' ~ 'Recreation Site (High)',
    T ~ name
  ))

# Convert industry restriction levels from numeric to text labels.
overlapping_ld_areas_by_district = overlapping_ld_areas_by_district %>%
  mutate(across(ends_with('Restrictions'), ~ case_when(
    .x == 0 ~ "None",
    .x == 1 ~ "Low",
    .x == 2 ~ "Medium",
    .x == 3 ~ "High",
    .x == 4 ~ "Full",
    .x == 5 ~ "Full"
  )))

write.csv(overlapping_ld_areas_by_district,
          'land_designations_webapp/www/overlapping_land_designations_with_district.csv',
          row.names = F)

save(ld_with_reg, file = 'tmp/ld_with_reg.RData')
