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
library(parallel)

source("fun.R")

dir.create("tmp", showWarnings = FALSE)

bc_bound_trim <- readOGR("data/BC_Boundary.gdb")

## Get the spatial file in - read from saved rds if exists, otherwise process raw file
cl_rds <- "tmp/cl.rds"
cl_clip_rds <- "tmp/cl_clip.rds"
if (!file.exists(cl_rds)) {
  cl <- readOGR("data/conservationlands.gdb", "conservationlands", stringsAsFactors = FALSE)

  cl <- fix_geo_problems(cl)

  saveRDS(cl, cl_rds)
} else {
  cl <- readRDS(cl_rds)
}

cl_t <- cl[cl$bc_boundary == "bc_boundary_land_tiled", ]

## Clip to BC Boundary
if (!file.exists(cl_clip_rds)) {
  system.time(cl_clip <- parallel_apply(cl, "category",
                            raster::intersect,
                            y = bc_bound_hres,
                            recombine = TRUE)) # 1.5 hrs

  system.time(cl_clip2 <- parallel_apply(cl, "category",
                            rmapshaper::ms_clip,
                            clip = bc_bound_hres,
                            recombine = TRUE))

  saveRDS(cl_clip, cl_clip_rds)
} else {
  cl_clip <- readRDS(cl_clip_rds)
}

## Aggregate by rollup group
cl_agg_rds <- "tmp/cl_agg.rds"
if (!file.exists(cl_agg_rds)) {
  cl_agg <- aggregate(cl_clip[, "rollup"], by = list(cons_cat = cl_clip$rollup), FUN = max)
  saveRDS(cl_agg, cl_agg_rds)
} else {
  cl_agg <- readRDS(cl_agg_rds)
}
#
# mock_ld_agg_simp <- geojson_json(mock) %>%
#   ms_simplify(keep = 0.05, keep_shapes = TRUE) %>% # explode = TRUE if want to keep all shapes
#   ms_dissolve(field = "cons_cat") %>%
#   geojson_sp() %>%
#   fix_self_intersect() %>%
#   transform_bc_albers()

################################################################################

cl_ecosections <- readOGR("data/eco_test.gdb", "ecosections_conservationlands", stringsAsFactors = FALSE)

cl_ecosections <- fix_geo_problems(cl_ecosections)

cl_ecosections_t <- cl_ecosections[cl_ecosections$bc_boundary == "bc_boundary_land_tiled",]
cl_ecosections_t$des_area <- gArea(cl_ecosections_t, byid = TRUE)

library(dplyr)

## Ecoregions:
m_ecoregions <- c("HCS", "IPS", "OPS", "SBC", "TPC")
system.time(t_egoregion_covers <- gCovers(ecoregions, bc_bound_trim, byid = TRUE, returnDense = TRUE))
system.time(t_egoregion_contains <- gContains(ecoregions, bc_bound_trim, byid = TRUE, returnDense = FALSE))
## load ecoregions data from bcmaps package
data("ecoregions")

## Extract the terrestrial and marine portions of GPB into separate objects
gpb_terrestrial <- ms_clip(ecoregions[ecoregions$CRGNCD == "GPB",],
                           bc_bound_hres)
## Fix it up:
gpb_terrestrial <- fix_self_intersect(gpb_terrestrial)

## Add terrestrial portion of GPB back to terrestrial ecoregions
ecoregions_t <- rbind(ecoregions[!ecoregions$CRGNCD %in% c("GPB", m_ecoregions), ],
                      gpb_terrestrial[, setdiff(names(gpb_terrestrial), "rmapshaperid")])

## Calcualte the area of the polygons
ecoregions_t$area <- gArea(ecoregions_t, byid = TRUE)

## Create simplified versions for visualization
ecoregions_t_simp <- ms_simplify(ecoregions_t, 0.05) %>%
  fix_self_intersect()
ecoregions_t_simp_leaflet <- ms_simplify(ecoregions_t[,c("CRGNCD", "CRGNNM")], 0.01) %>%
  fix_self_intersect() %>%
  spTransform(CRSobj = CRS("+init=epsg:4326"))
ecoregions_t_simp_leaflet$CRGNCD <- as.character(ecoregions_t_simp_leaflet$CRGNCD)
ecoregions_t_simp_leaflet$CRGNNM <- tools::toTitleCase(tolower(as.character(ecoregions_t_simp_leaflet$CRGNNM)))
ecoregions_t_simp_leaflet$rmapshaperid <- NULL

###############################################################################
## Biogeoclimatic zones
if (!file.exists("data/BEC_POLY/BEC_POLY_polygon.shp.shp")) {
  unzip("data/BCGW_78757263_1474302658533_4244.zip", exdir = "data")
}

bgc <- readOGR("data/BEC_BIOGEOCLIMATIC_POLY.gdb", stringsAsFactors = FALSE)

## Clip bgc to BC boundary
bec_t <- clip_only(bgc, bc_bound_trim)
bec_t <- fix_geo_problems(bec_t)
bec_t$area <- gArea(bec_t, byid = TRUE)

bec_zone <- raster::aggregate(bec_t, by = "ZONE")
bec_zone <- fix_self_intersect(bec_zone)
bec_zone$area <- gArea(bec_zone, byid = TRUE)

## Simplify BEC pologyons for use in display
bec_zone_simp <- ms_simplify(bec_zone, keep = 0.005)
## Repair orphaned holes.
bec_zone_simp <- fix_self_intersect(bec_zone_simp)

bec_zone_leaflet <- ms_simplify(bec_zone_simp, 0.1) %>%
  fix_self_intersect() %>%
  spTransform(CRSobj = CRS("+init=epsg:4326"))
bec_zone_leaflet$ZONE <- as.character(bec_zone_leaflet$ZONE)
bec_zone_leaflet$rmapshaperid <- NULL

saveRDS(mock, "tmp/mock_spatial.rds")
saveRDS(mock_ld_agg, "tmp/mock_spatial_agg.rds")
saveRDS(mock_ld_agg_simp, "tmp/mock_spatial_agg_simp.rds")

saveRDS(ecoregions_t, "tmp/ecoregions_t.rds")
saveRDS(ecoregions_t_simp, "tmp/ecoregions_t_simp.rds")
saveRDS(ecoregions_t_simp_leaflet, "out/ecoregions_t_leaflet.rds")

saveRDS(bec_t, "tmp/bec_t.rds")
saveRDS(bec_zone, "tmp/bec_zone.rds")
saveRDS(bec_zone_simp, "tmp/bec_zone_simp.rds")
saveRDS(bec_zone_leaflet, "out/bec_leaflet.rds")

gg_fortify(bc_bound) %>% write_feather("out/gg_bc_bound.feather")
