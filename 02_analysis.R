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
library(feather)
library(bcmaps)
library(cleangeo)

source("fun.R")

dir.create("out", showWarnings = FALSE)

# ld <- readRDS("tmp/mock_spatial.rds")
ld_agg <- readRDS("tmp/mock_spatial_agg.rds")
ld_agg_simp <- readRDS("tmp/mock_spatial_agg_simp.rds")

ecoreg <- readRDS("tmp/ecoregions_t.rds")
ecoreg_simp <- readRDS("tmp/ecoregions_t_simp.rds")

bec_zone <- readRDS("tmp/bec_zone.rds")
bec_zone_simp <- readRDS("tmp/bec_zone_simp.rds")

## Intersect land designations with ecoregions and summarize
ld_x_ecoreg <- raster::intersect(ecoreg, ld_agg) %>%
  clgeo_Clean()

ld_x_ecoreg$area <- rgeos::gArea(ld_x_ecoreg, byid = TRUE)

ld_agg$area <- gArea(ld_agg, byid = TRUE)

ld_ecoreg_summary <- ld_x_ecoreg@data %>%
  group_by(CRGNCD, cons_cat) %>%
  summarise(area_des = sum(area, na.rm = TRUE)) %>%
  left_join(group_by(ecoreg@data, CRGNCD) %>%
              summarize(ecoreg_area = sum(area, na.rm = TRUE)), by = "CRGNCD") %>%
  mutate(percent_des = area_des / ecoreg_area * 100,
         area_des_ha = area_des * 1e-4) %>%
  bind_rows(ld_agg@data %>%
              group_by(cons_cat) %>%
              summarise(area_des = sum(area, na.rm = TRUE)) %>%
              mutate(CRGNCD = "BC",
                     percent_des = area_des / bc_area(units = "m2") * 100,
                     area_des_ha = area_des * 1e-4)) %>%
  write_feather("out/ld_ecoreg_summary.feather")

# Intersect simplified versions for mapping display
ld_x_ecoreg_simp <- raster::intersect(ld_agg_simp, ecoreg_simp) %>%
  clgeo_Clean()

gg_ld_x_ecoreg <- gg_fortify(ld_x_ecoreg_simp) %>% write_feather("out/gg_ld_ecoreg.feather")
gg_ecoreg <- gg_fortify(ecoreg_simp) %>% write_feather("out/gg_ecoreg.feather")

# ecoreg_cds <- unique(gg_ecoreg$CRGNCD)
#
# lapply(ecoreg_cds, gg_ld_ecoreg, gg_ld_x_ecoreg, gg_ecoreg)

################################################################################
# BEC

## Intersect land designations with BEC and summarize
ld_x_bec <- raster::intersect(bec_zone, ld_agg) %>%
  clgeo_Clean()

ld_x_bec$area <- rgeos::gArea(ld_x_bec, byid = TRUE)

ld_agg$area <- gArea(ld_agg, byid = TRUE)

ld_bec_summary <- ld_x_bec@data %>%
  group_by(ZONE, cons_cat) %>%
  summarise(area_des = sum(area, na.rm = TRUE)) %>%
  left_join(group_by(bec_zone@data, ZONE) %>%
              summarize(bec_area = sum(area, na.rm = TRUE)), by = "ZONE") %>%
  mutate(percent_des = area_des / bec_area * 100,
         area_des_ha = area_des * 1e-4) %>%
  bind_rows(ld_agg@data %>%
              group_by(cons_cat) %>%
              summarise(area_des = sum(area, na.rm = TRUE)) %>%
              mutate(ZONE = "BC",
                     percent_des = area_des / bc_area(units = "m2") * 100,
                     area_des_ha = area_des * 1e-4)) %>%
  write_feather("out/ld_bec_summary.feather")

# Intersect simplified versions for mapping display
ld_x_bec_simp <- raster::intersect(ld_agg_simp, bec_zone_simp) %>%
  clgeo_Clean()

gg_ld_x_bec <- gg_fortify(ld_x_bec_simp) %>% write_feather("out/gg_ld_bec.feather")
gg_bec <- gg_fortify(bec_zone_simp) %>% write_feather("out/gg_bec.feather")




####################################
# ecoregions_list <- lapply(ecoreg$CRGNCD, function(x) {
#   ecoreg[ecoreg$CRGNCD == x,]
# })
#
# names(ecoregions_list) <- ecoreg$CRGNCD
#
# # This takes about 30 minutes
# # ld_ecoreg_clip_list <- lapply(ecoregions_list, function(x) {
# #   rmapshaper::ms_clip(ld_agg, x)
# # })
#
# names(ld_ecoreg_clip_list) <- ecoreg$CRGNCD
#
# lapply(names(ld_ecoreg_clip_list), plot_ecoreg_land_des, ecoregions_list, ld_ecoreg_clip_list)

