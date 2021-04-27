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


if (!exists("ld_m")) load("tmp/raw_data_vect.RData")

dir.create("out", showWarnings = FALSE)
dir.create("out-shiny", showWarnings = FALSE)


#Find area/proportion for each restriction level/type

ld_m_sum <- ld_m %>%
  mutate(total_area = st_area(geom)) %>%
  st_set_geometry(NULL) %>%
  group_by(mine_restriction) %>%
  summarize(N=length(mine_restriction_id),
          total_area_ha = round(sum(as.numeric(total_area)) / 10000, digits=2),
          .groups = "drop")%>%
  mutate(perc_bc_ha = round(total_area_ha/bc_size_ha*100, digits=1)) %>%
  add_column(restriction_type = "Mining", .before="mine_restriction")%>%
  rename(restriction = mine_restriction)


ld_f_sum <- ld_f %>%
  mutate(total_area = st_area(geom)) %>%
  st_set_geometry(NULL) %>%
  group_by(forest_restriction) %>%
  summarize(N=length(forest_restriction_id),
            total_area_ha = round(sum(as.numeric(total_area)) / 10000, digits=2),
            .groups = "drop")%>%
  mutate(perc_bc_ha = round(total_area_ha/bc_size_ha*100, digits=1))%>%
  add_column(restriction_type = "Forestry", .before="forest_restriction")%>%
  rename(restriction = forest_restriction)

ld_o_sum <- ld_o %>%
  mutate(total_area = st_area(geom)) %>%
  st_set_geometry(NULL) %>%
  group_by(og_restriction) %>%
  summarize(N=length(og_restriction_id),
            total_area_ha = round(sum(as.numeric(total_area)) / 10000, digits=2),
            .groups = "drop")%>%
  mutate(perc_bc_ha = round(total_area_ha/bc_size_ha*100, digits=1))%>%
  add_column(restriction_type = "Oil & Gas", .before="og_restriction") %>%
  rename(restriction = og_restriction)


rest_key <- tribble(
  ~ restriction, ~ r_level, ~ category,
  NA , "None", "None",
  1 , "Full", "Parks & Protected Areas",
  2 , "High", "Other Protected Lands",
  3 , "Medium", "Resource Exclusion Areas",
  4 , "Low", "Managed Areas"
)

cons_area_all <- rbind(ld_f_sum, ld_m_sum, ld_o_sum) %>%
  left_join(rest_key)

# select subset of cities for plotting - choose largest city in each natural resource district

message("Add nr districts regions")
ld_cities <- st_intersection(bc_nr_dist, bc_cities) %>%
  group_by(DISTRICT_NAME) %>%
  slice_max(POP_2000, n=1) %>%
  dplyr::select(DISTRICT_NAME, NAME, POP_2000, geometry)




save(ld_f_sum, ld_m_sum, ld_o_sum, ld_cities, cons_area_all, file = "tmp/clean.RData")


