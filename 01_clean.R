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
library(bcmaps)
library(geojsonio)
library(rmapshaper)
library(feather)

library(dplyr)
library(sf)

source("fun.R")

dir.create("tmp", showWarnings = FALSE)

bc_bound_trim_rds <- "tmp/bc_bound_trim.rds"
bc_bound_trim <- tryCatch(readRDS(bc_bound_trim_rds), error = function(e) {
  bc_bound_trim <- readOGR("data/BC_Boundary.gdb", stringsAsFactors = FALSE)
  bc_bound_trim <- transform_bc_albers(bc_bound_trim)
  saveRDS(bc_bound_trim, bc_bound_trim_rds)
  bc_bound_trim
})

## Simplify bc boundary, fortify for ggplot and write to feather file
bc_bound_simp_feather <- "out/gg_bc_bound.feather"
bc_bound_simp <- tryCatch(read_feather(bc_bound_simp_feather), error = function(e) {
  rmapshaper::ms_simplify(bc_bound_trim, keep = 0.001) %>%
  gg_fortify() %>%
  write_feather(bc_bound_simp_feather)
})

## Clip Ecoregions to BC Boundary
ecoreg_t_rds <- "tmp/ecoreg_t.rds"
ecoregions_t <- tryCatch(readRDS(ecoreg_t_rds), error = function(e) {
  m_ecoregions <- c("HCS", "IPS", "OPS", "SBC", "TPC")
  eco_t <- clip_only(ecoregions[!ecoregions$CRGNCD %in% m_ecoregions,],
                     bc_bound_trim)
  eco_t <- fix_geo_problems(eco_t)
  eco_t$ecoreg_area <- gArea(eco_t, byid = TRUE)
  saveRDS(eco_t, file = ecoreg_t_rds)
  eco_t
})

## Create simplified versions of ecoregions for visualization
ecoregions_t_simp_rds <- "tmp/ecoregions_t_simp.rds"
ecoregions_t_simp <- tryCatch(readRDS(ecoregions_t_simp_rds), error = function(e) {
  eco_t_simp <- ms_simplify(ecoregions_t, 0.01) %>%
    fix_geo_problems()
  saveRDS(eco_t_simp, ecoregions_t_simp_rds)
  eco_t_simp
})

gg_ecoreg <- gg_fortify(ecoregions_t_simp) %>% write_feather("out/gg_ecoreg.feather")

eco_leaflet_rds <- "out/ecoregions_t_leaflet.rds"
ecoregions_t_simp_leaflet <- tryCatch(readRDS(eco_leaflet_rds), error = function(e) {
  eco_t_simp_leaflet <- ms_simplify(ecoregions_t[,c("CRGNCD", "CRGNNM")], 0.003) %>%
    fix_geo_problems() %>%
    spTransform(CRSobj = CRS("+init=epsg:4326"))
  eco_t_simp_leaflet$CRGNCD <- as.character(ecoregions_t_simp_leaflet$CRGNCD)
  eco_t_simp_leaflet$CRGNNM <- tools::toTitleCase(tolower(as.character(ecoregions_t_simp_leaflet$CRGNNM)))
  eco_t_simp_leaflet$rmapshaperid <- NULL
  saveRDS(ecoregions_t_simp_leaflet, eco_leaflet_rds)
  eco_t_simp_leaflet
})

## Clip bgc to BC boundary
bec_t_rds <- "tmp/bec_t.rds"
bec_t <- tryCatch(readRDS(bec_t_rds), error = function(e) {
  bgc <- readOGR("data/BEC_BIOGEOCLIMATIC_POLY.gdb", stringsAsFactors = FALSE)
  bgc <- transform_bc_albers(bgc)
  bec_t <- clip_only(bgc, bc_bound_trim)
  bec_t <- fix_geo_problems(bec_t)
  bec_t$bec_area <- gArea(bec_t, byid = TRUE)
  saveRDS(bec_t, file = bec_t_rds)
  bec_t
})

bec_zone_rds <- "tmp/bec_zone.rds"
bec_zone <- tryCatch(readRDS(bec_zone_rds), error = function(e) {
  bec_zone <- raster::aggregate(bec_t, by = "ZONE") %>%
    fix_geo_problems()
  bec_zone$zone_area <- gArea(bec_zone, byid = TRUE)
  saveRDS(bec_zone, bec_zone_rds)
  bec_zone
})

## Simplify BEC pologyons for use in display
bec_zone_simp_rds <- "tmp/bec_zone_simp.rds"
bec_zone_simp <- tryCatch(readRDS(bec_zone_simp_rds), error = function(e) {
  bec_zone_simp <- ms_simplify(bec_zone, keep = 0.005) %>%
    fix_geo_problems()
  saveRDS(bec_zone_simp, bec_zone_simp_rds)
  bec_zone_simp
})

gg_bec <- gg_fortify(bec_zone_simp) %>% write_feather("out/gg_bec.feather")

bec_zone_leaflet_rds <- "out/bec_leaflet.rds"
bec_zone_leaflet <- tryCatch(readRDS(bec_zone_leaflet_rds), error = function(e) {
  bec_zone_leaflet <- ms_simplify(bec_zone_simp, 0.1) %>%
    fix_geo_problems() %>%
    spTransform(CRSobj = CRS("+init=epsg:4326"))
  bec_zone_leaflet$ZONE <- as.character(bec_zone_leaflet$ZONE)
  bec_zone_leaflet$rmapshaperid <- NULL
  saveRDS(bec_zone_leaflet, bec_zone_leaflet_rds)
  bec_zone_leaflet
})

## Get full land designations file
ld_t_rds <- "tmp/ld_t.rds"
ld_t <- tryCatch(readRDS(ld_t_rds), error = function(e) {
  ld <- readOGR("data/conservationlands.gdb", stringsAsFactors = FALSE) %>%
    fix_geo_problems()
  ld_t <- ld[ld$bc_boundary == "bc_boundary_land_tiled" &
               ld$category != "" & !is.na(ld$category), ] %>%
    fix_geo_problems()
  ld_t$area <- rgeos::gArea(ld_t, byid = TRUE)
  saveRDS(ld_t, ld_t_rds)
  ld_t
})

## Simplify provincial coverage
# ld_test <- ld_sf[ld_sf$map_tile %in% unique(ld_t$map_tile)[1:10], ]
# system.time(ld_t_simp <- mapshaper_apply(ld_test, "map_tile", ms_simplify, keep_shapes = TRUE))

## Load BEC x land designations
bec_ld_rds <- "tmp/bec_ld_t.rds"
bec_ld_t <- tryCatch(readRDS(bec_ld_rds), error = function(e) {
  bec_ld <- readOGR("data/lands_bec.gdb", stringsAsFactors = FALSE) %>%
    fix_geo_problems()
  bec_ld_t <- bec_ld[bec_ld$bc_boundary == "bc_boundary_land_tiled" &
                       bec_ld$category != "" & !is.na(bec_ld$category), ] %>%
    fix_geo_problems()
  saveRDS(bec_ld_t, bec_ld_rds)
  bec_ld_t
})

# Make some aggregated and simplified products from bec_ld:
bec_ld_agg_rds <- "tmp/bec_ld_agg.rds"
bec_ld_agg <- tryCatch(readRDS(bec_ld_agg_rds), error = function(e) {
  bec_ld_agg <- raster::aggregate(bec_ld_t,
                                by = c("category", "zone", "subzone", "variant",
                                       "map_label")) %>%
  fix_geo_problems()
  saveRDS(bec_ld_agg, bec_ld_agg_rds)
  bec_ld_agg
})

bec_ld_agg_zone_rds <- "tmp/bec_ld_agg_zone.rds"
bec_ld_agg_zone <- tryCatch(readRDS(bec_ld_agg_zone_rds), error = function(e) {
  bec_ld_agg_zone <- raster::aggregate(bec_ld_agg, by = c("zone", "category")) %>%
    fix_geo_problems()
  saveRDS(bec_ld_agg_zone, bec_ld_agg_zone_rds)
  bec_ld_agg_zone
})

## Load Ecosections x land designations (using sf)
eco_ld_sf_rds <- "tmp/sf/eco_ld_t.rds"
eco_ld_t <- tryCatch(readRDS(eco_ld_sf_rds), error = function(e) {
  eco_ld_t <- st_read("data/lands_eco.gdb", stringsAsFactors = FALSE) %>%
    filter(bc_boundary == "bc_boundary_land_tiled",
           !is.na(category),
           category != "") %>%
    select(eco_ld_t, designation, map_tile, category, parent_ecoregion_code,
           ecosection_name, ecosection_code, shape_area, SHAPE) %>%
    fix_geo_problems()
  saveRDS(eco_ld_t, eco_ld_sf_rds)
  eco_ld_t
})

eco_ld_rds <- "tmp/eco_ld_t.rds"
eco_ld_t <- tryCatch(readRDS(eco_ld_rds), error = function(e) {
  eco_ld <- readOGR("data/lands_eco.gdb", stringsAsFactors = FALSE) %>%
    fix_geo_problems()
  eco_ld_t <- eco_ld[eco_ld$bc_boundary == "bc_boundary_land_tiled" &
                       eco_ld$category != "" & !is.na(eco_ld$category),
                     c("designation", "map_tile", "category",
                       "parent_ecoregion_code", "ecosection_name",
                       "ecosection_code", "shape_area")] %>%
    fix_geo_problems()
  saveRDS(eco_ld_t, eco_ld_rds)
  eco_ld_t
})

ecosec_ld_agg_rds <- "tmp/ecosec_ld_agg.rds"
ecosec_ld_agg <- tryCatch(readRDS(ecosec_ld_agg_rds), error = function(e) {
  ecosec_ld_agg <- raster::aggregate(eco_ld_t, by = c("category",
                                                   "parent_ecoregion_code",
                                                   "ecosection_name",
                                                   "ecosection_code")) %>%
    fix_geo_problems()
  saveRDS(ecosec_ld_agg, ecosec_ld_agg_rds)
  ecosec_ld_agg
})

ecoreg_ld_agg_rds <- "tmp/ecoreg_ld_agg.rds"
ecoreg_ld_agg <- tryCatch(readRDS(ecoreg_ld_agg_rds), error = function(e) {
  ecoreg_ld_agg <- raster::aggregate(ecosec_ld_agg, by = c("category",
                                                      "parent_ecoregion_code")) %>%
    fix_geo_problems()
  saveRDS(ecoreg_ld_agg, ecoreg_ld_agg_rds)
  ecoreg_ld_agg
})

# system.time(ld_ecosec_simp <- mapshaper_apply(ecosec_ld_agg, "parent_ecoregion_code", ms_simplify,
#                                               keep = 0.005, keep_shapes = TRUE,
#                                               parallel = FALSE, recombine = TRUE))

ld_ecoreg_simp_rds <- "tmp/ld_ecoreg_simp.rds"
ld_ecoreg_simp <- tryCatch(readRDS(ld_ecoreg_simp_rds), error = function(e) {
  ld_ecoreg_simp <- mapshaper_apply(ecoreg_ld_agg, "parent_ecoregion_code", ms_simplify,
                                  keep = 0.005, keep_shapes = TRUE,
                                  parallel = FALSE, recombine = TRUE) %>%
  fix_geo_problems() %>%
  raster::aggregate(by = c("category","parent_ecoregion_code"))
  saveRDS(ld_ecoreg_simp, ld_ecoreg_simp_rds)
  ld_ecoreg_simp
})

ld_ecoreg_simp_more <- ms_simplify(ld_ecoreg_simp, keep = 0.05, keep_shapes = TRUE) %>%
  fix_geo_problems()

gg_ld_ecoreg <- gg_fortify(ld_ecoreg_simp_more) %>%
  rename(CRGNCD = parent_ecoregion_code) %>%
  select(-rmapshaperid) %>%
  write_feather("out/gg_ld_ecoreg.feather")





library(ggplot2)
library(ggpolypath)
ggplot(gg_ld_ecoreg, aes(x = long, y = lat, group = group)) +
  geom_polypath(aes(fill = category)) +
  coord_fixed()

####################

library(ggplot2)
library(ggpolypath)
ggplot(gg_bec_ld[gg_bec_ld$zone == "BWBS", ], aes(x = long, y = lat, group = group)) +
  geom_polypath(aes(fill = category)) +
  coord_fixed()

library(readr)

cat_summary <- bec_ld_t@data %>%
  group_by(category) %>%
  summarize(area_prot_ha = sum(shape_area) * 1e-4) %>%
  mutate(percent_prot = (area_prot_ha * 1e4) / sum(bc_bound_trim$SHAPE_Area) * 100) %>%
  mutate_if(is.numeric, round, digits = 2) %>%
  arrange(category) %>%
  write_csv("out/designation_categories_summary.csv")

bec_zone_cat_summary <- bec_ld_t@data %>%
  group_by(zone, category) %>%
  summarize(area_designated_ha = sum(shape_area) * 1e-4) %>%
  left_join(bec_zone@data, by = c("zone" = "ZONE")) %>%
  mutate(zone_area_ha = area * 1e-4,
         percent_designated = area_designated_ha / zone_area_ha * 100) %>%
  select(-area) %>%
  mutate_if(is.numeric, round, digits = 2) %>%
  arrange(zone, category) %>%
  write_csv("out/designation_categories_by_bec_zone.csv")
