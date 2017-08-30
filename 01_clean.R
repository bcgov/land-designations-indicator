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

# library(rgdal)
# library(sp)
# library(rgeos)
# library(raster)
library(bcmaps)
library(geojsonio)
library(rmapshaper)
library(feather)
library(readr)
library(dplyr)
library(tidyr)
library(sf)

source("fun.R")

dir.create("tmp", showWarnings = FALSE)
dir.create("out", showWarnings = FALSE)
dir.create("out-shiny", showWarnings = FALSE)

bc_bound_trim_rds <- "tmp/bc_bound_trim.rds"
bc_bound_trim <- tryCatch(readRDS(bc_bound_trim_rds), error = function(e) {
  bc_bound_trim <- read_sf("data/BC_Boundary.gdb")
  bc_bound_trim <- transform_bc_albers(bc_bound_trim)
  saveRDS(bc_bound_trim, bc_bound_trim_rds)
  bc_bound_trim
})

## Clip Ecoregions to BC Boundary
ecoreg_t_rds <- "tmp/ecoreg_t.rds"
ecoregions_t <- tryCatch(readRDS(ecoreg_t_rds), error = function(e) {
  m_ecoregions <- c("HCS", "IPS", "OPS", "SBC", "TPC")
  eco_t <- st_intersection(st_as_sf(ecoregions[!ecoregions$CRGNCD %in% m_ecoregions,]),
                     bc_bound_trim)
  eco_t <- fix_geo_problems(eco_t)
  eco_t$ecoreg_area <- st_area(eco_t)
  saveRDS(eco_t, file = ecoreg_t_rds)
  eco_t
})

## Clip bgc to BC boundary
bec_t_rds <- "tmp/bec_t.rds"
bec_t <- tryCatch(readRDS(bec_t_rds), error = function(e) {
  bec <- read_sf("data/BEC_BIOGEOCLIMATIC_POLY.gdb")
  bec <- transform_bc_albers(bec)
  bec_t <- clip_only(bec, bc_bound_trim)
  bec_t <- fix_geo_problems(bec_t)
  bec_t$bec_area <- st_area(bec_t)
  saveRDS(bec_t, file = bec_t_rds)
  bec_t
})

## Get full land designations file
ld_t_rds <- "tmp/ld_t.rds"
ld_t <- tryCatch(readRDS(ld_t_rds), error = function(e) {
  ld_t <- read_sf("data/designatedlands.gpkg") %>%
    filter(bc_boundary == "bc_boundary_land_tiled") %>%
    select(-bc_boundary) %>%
    mutate(calc_area = st_area(.))
  saveRDS(ld_t, ld_t_rds)
  ld_t
})

## Load BEC x land designations
bec_ld_rds <- "tmp/bec_ld_t.rds"
bec_ld <- tryCatch(readRDS(bec_ld_rds), error = function(e) {
  bec_ld <- read_sf("data/lands_bec.gpkg") %>%
    filter(bc_boundary == "bc_boundary_land_tiled") %>%
    select(-bc_boundary) %>%
    mutate(calc_area = st_area(.))
  saveRDS(bec_ld, bec_ld_rds)
  bec_ld
})

## Load Ecosections x land designations
eco_ld_rds <- "tmp/eco_ld_t.rds"
eco_ld <- tryCatch(readRDS(eco_ld_rds), error = function(e) {
  eco_ld <- read_sf("data/lands_eco.gpkg") %>%
    filter(bc_boundary == "bc_boundary_land_tiled") %>%
    select(-bc_boundary) %>%
    mutate(calc_area = st_area(.))
  saveRDS(eco_ld, eco_ld_rds)
  eco_ld
})

# Simplification and aggregation for visualizations -----------------------

## Simplify bc boundary, fortify for ggplot and write to feather file
bc_bound_simp_feather <- "out-shiny/gg_bc_bound.feather"
bc_bound_simp <- tryCatch(read_feather(bc_bound_simp_feather), error = function(e) {
  rmapshaper::ms_simplify(bc_bound_trim, keep = 0.001) %>%
    as("Spatial") %>%
    gg_fortify() %>%
    write_feather(bc_bound_simp_feather)
})

## Create simplified versions of ecoregions for leaflet map
eco_leaflet_rds <- "out-shiny/ecoregions_t_leaflet.rds"
ecoregions_t_simp_leaflet <- tryCatch(readRDS(eco_leaflet_rds), error = function(e) {
  eco_t_simp_leaflet <- ms_simplify(ecoregions_t[,c("CRGNCD", "CRGNNM")], 0.003) %>%
    fix_geo_problems() %>%
    st_set_crs(3005) %>%
    st_transform(4326) %>%
    mutate(CRGNNM = tools::toTitleCase(tolower(as.character(CRGNNM))))
  saveRDS(as(eco_t_simp_leaflet, "Spatial"), eco_leaflet_rds)
  eco_t_simp_leaflet
})

## Create simplified versions of ecoregions for visualization
ecoregions_t_simp_rds <- "tmp/ecoregions_t_simp.rds"
ecoregions_t_simp <- tryCatch(readRDS(ecoregions_t_simp_rds), error = function(e) {
  eco_t_simp <- ms_simplify(ecoregions_t, 0.01) %>%
    fix_geo_problems()
  saveRDS(eco_t_simp, ecoregions_t_simp_rds)
  eco_t_simp
})

gg_ecoreg <- gg_fortify(as(ecoregions_t_simp, "Spatial")) %>% write_feather("out-shiny/gg_ecoreg.feather")

## Simplify BEC pologyons for use in display
bec_zone_rds <- "tmp/bec_zone.rds"
bec_zone <- tryCatch(readRDS(bec_zone_rds), error = function(e) {
  bec_zone <- group_by(bec_t, ZONE) %>%
    summarize() %>%
    fix_geo_problems()
  bec_zone$zone_area <- st_area(bec_zone)
  saveRDS(bec_zone, bec_zone_rds)
  bec_zone
})

bec_zone_simp_rds <- "tmp/bec_zone_simp.rds"
bec_zone_simp <- tryCatch(readRDS(bec_zone_simp_rds), error = function(e) {
  bec_zone$zone_area <- as.numeric(bec_zone$zone_area) ## Of class units, need as numeric
  bec_zone_simp <- bec_zone %>%
    ms_simplify(keep = 0.005) %>%
    fix_geo_problems()
  saveRDS(bec_zone_simp, bec_zone_simp_rds)
  bec_zone_simp
})

gg_bec <- as(bec_zone_simp, "Spatial") %>%
  gg_fortify() %>%
  write_feather("out-shiny/gg_bec.feather")

## Further simplification for BEC leaflet map
bec_zone_leaflet_rds <- "out-shiny/bec_leaflet.rds"
bec_zone_leaflet <- tryCatch(readRDS(bec_zone_leaflet_rds), error = function(e) {
  bec_zone_leaflet <- bec_zone_simp %>%
    ms_simplify(0.1) %>%
    fix_geo_problems() %>%
    st_transform(4326)
  bec_zone_leaflet$ZONE <- as.character(bec_zone_leaflet$ZONE)
  saveRDS(as(bec_zone_leaflet, "Spatial"), bec_zone_leaflet_rds)
  bec_zone_leaflet
})

## Aggregate the full Land Designations file
ld_agg_rds <- "tmp/ld_agg.rds"
ld_agg <- tryCatch(readRDS(ld_agg_rds), error = function(e) {
    ld_agg <- ld_t %>%
    filter_non_designated() %>%
    group_by(category, designation) %>%
    summarise()
  saveRDS(ld_agg, ld_agg_rds)
  ld_agg
})

## Aggreate by category
ld_agg_cat_rds <- "tmp/ld_agg_cat.rds"
ld_agg_cat <- tryCatch(readRDS(ld_agg_cat_rds), error = function(e) {
  ld_agg_cat <- ld_t %>%
    filter_non_designated() %>%
    fix_geo_problems() %>%
    group_by(category) %>%
    summarise()
  saveRDS(ld_agg_cat, ld_agg_cat_rds)
  ld_agg_cat
})

## Simplify the provincial-scale categories
ld_simp_rds <- "tmp/ld_simp.rds"
ld_simp <- tryCatch(readRDS(ld_simp), error = function(e) {
  ld_simp <- mapshaper_apply(ld_agg_cat, "category", ms_simplify, keep = 0.01) %>%
    fix_geo_problems()
  saveRDS(ld_simp, ld_simp_rds)
  ld_simp
})

## Save simplified ld object as ggplot-able feather file
gg_ld <- as(ld_simp, "Spatial") %>%
  gg_fortify() %>%
  write_feather("out/gg_ld_simp.feather")

## Simplify a bit more for shiny plotting
ld_simp_more <- ms_simplify(ld_simp, keep = 0.05, explode = TRUE, keep_shapes = TRUE) %>%
  fix_geo_problems() %>%
  group_by(category) %>%
  summarise()

## Intersect simplified ld with simplified bec to get viz object:
ld_bec_simp <- st_intersection(bec_zone_simp, ld_simp_more) %>%
  st_collectionextract("POLYGON") %>%
  select(ZONE, category) %>%
  fix_geo_problems()

gg_ld_bec <- as(ld_bec_simp, "Spatial") %>%
  gg_fortify() %>%
  write_feather("out-shiny/gg_ld_bec_new.feather")

## Intersect simplified ld with simplified ecoregions to get viz object:
ld_ecoreg_simp <- st_intersection(ecoregions_t_simp, ld_simp_more) %>%
  st_collectionextract("POLYGON") %>%
  select(CRGNCD, category) %>%
  fix_geo_problems()

gg_ld_ecoreg <- as(ld_ecoreg_simp, "Spatial") %>%
  fix_geo_problems() %>%
  gg_fortify() %>%
  write_feather("out-shiny/gg_ld_ecoreg_new.feather")
