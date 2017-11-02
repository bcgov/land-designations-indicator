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

library(sf)
library(bcmaps)
library(dplyr)
library(feather)
library(rmapshaper)

source("fun.R")

dir.create("tmp", showWarnings = FALSE)

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

## Simplify bc boundary, fortify for ggplot and write to feather file
bc_bound_simp_feather <- "tmp/gg_bc_bound.feather"
bc_bound_simp <- tryCatch(read_feather(bc_bound_simp_feather), error = function(e) {
  rmapshaper::ms_simplify(bc_bound_trim, keep = 0.001) %>%
    as("Spatial") %>%
    gg_fortify() %>%
    write_feather(bc_bound_simp_feather)
})

## Aggregate the full Land Designations file
ld_agg_rds <- "tmp/ld_agg.rds"
ld_agg <- tryCatch(readRDS(ld_agg_rds), error = function(e) {
  ld_agg <- ld_t %>%
    filter_non_designated() %>%
    group_by(category, designation) %>%
    summarise() %>%
    ungroup()
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

## Simplify the provincial-scale categories for plotting
ld_simp_rds <- "tmp/ld_simp.rds"
ld_simp <- tryCatch(readRDS(ld_simp_rds), error = function(e) {
  ld_simp <- mapshaper_apply(ld_agg_cat, "category", ms_simplify, keep = 0.01) %>%
    fix_geo_problems()
  saveRDS(ld_simp, ld_simp_rds)
  ld_simp
})

## Save simplified ld object as ggplot-able feather file
gg_ld <- as(ld_simp, "Spatial") %>%
  gg_fortify() %>%
  write_feather("tmp/gg_ld_simp.feather")

## Process the file with overlapping polygons:
ld_overlaps <- read_sf("data/designatedlands_overlaps.gpkg") %>%
  st_collection_extract(ld_overlaps, "POLYGON")

ld_overlaps_clean <- ld_overlaps %>%
  st_make_valid() %>%
  group_by(designation, designation_name, bc_boundary) %>%
  summarize()

# Convert input gpkg to shp and use mapshaper to clean it up and aggregate it.
# Then zip up the inputs for the release.
if (nzchar(Sys.which("ogr2ogr")) && nzchar(Sys.which("mapshaper"))) {
  system("ogr2ogr -f 'ESRI Shapefile' data/designatedlands.shp data/designatedlands.gpkg")
  system("mapshaper data/designatedlands.shp -filter '\"bc_boundary_land_tiled\".indexOf(bc_bound_1) > -1' -clean -dissolve designatio copy-fields=category -o out/designatedlands.shp")
  files_to_zip <- normalizePath(
    c(list.files("out", pattern = "designatedlands\\.(shp|dbf|prj|shx)$", full.names = TRUE),
      file.path("data", c("designatedlands.gpkg", "lands_bec.gpkg", "lands_eco.gpkg"))
    ))
  zip("out/land_designations_shp.zip", files_to_zip)
}
