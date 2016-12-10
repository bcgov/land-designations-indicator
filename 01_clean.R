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

bc_bound_trim <- readOGR("data/BC_Boundary.gdb", stringsAsFactors = FALSE)
bc_bound_trim <- transform_bc_albers(bc_bound_trim)

## Simplify bc boundary, fortify for ggplot and write to feather file
bc_bound_simp <- rmapshaper::ms_simplify(bc_bound_trim, keep = 0.001)
gg_fortify(bc_bound_simp) %>%
  write_feather("out/gg_bc_bound.feather")

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
ecoregions_t_simp <- ms_simplify(ecoregions_t, 0.01) %>%
  fix_geo_problems()
gg_ecoreg <- gg_fortify(ecoregions_t_simp) %>% write_feather("out/gg_ecoreg.feather")

ecoregions_t_simp_leaflet <- ms_simplify(ecoregions_t[,c("CRGNCD", "CRGNNM")], 0.003) %>%
  fix_geo_problems() %>%
  spTransform(CRSobj = CRS("+init=epsg:4326"))
ecoregions_t_simp_leaflet$CRGNCD <- as.character(ecoregions_t_simp_leaflet$CRGNCD)
ecoregions_t_simp_leaflet$CRGNNM <- tools::toTitleCase(tolower(as.character(ecoregions_t_simp_leaflet$CRGNNM)))
ecoregions_t_simp_leaflet$rmapshaperid <- NULL

saveRDS(ecoregions_t_simp, "tmp/ecoregions_t_simp.rds")
saveRDS(ecoregions_t_simp_leaflet, "out/ecoregions_t_leaflet.rds")

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

bec_zone <- raster::aggregate(bec_t, by = "ZONE") %>%
  fix_geo_problems()
bec_zone$zone_area <- gArea(bec_zone, byid = TRUE)
saveRDS(bec_zone, "tmp/bec_zone.rds")

## Simplify BEC pologyons for use in display
bec_zone_simp <- ms_simplify(bec_zone, keep = 0.005)
## Repair orphaned holes.
bec_zone_simp <- fix_geo_problems(bec_zone_simp)
saveRDS(bec_zone_simp, "tmp/bec_zone_simp.rds")
gg_bec <- gg_fortify(bec_zone_simp) %>% write_feather("out/gg_bec.feather")

bec_zone_leaflet <- ms_simplify(bec_zone_simp, 0.1) %>%
  fix_geo_problems() %>%
  spTransform(CRSobj = CRS("+init=epsg:4326"))
bec_zone_leaflet$ZONE <- as.character(bec_zone_leaflet$ZONE)
bec_zone_leaflet$rmapshaperid <- NULL
saveRDS(bec_zone_leaflet, "out/bec_leaflet.rds")

unzip("data/land_designations_bec_overlay.zip", exdir = "data")
bec_ld <- readOGR("data/bec.gdb", stringsAsFactors = FALSE) %>%
  fix_geo_problems()
bec_ld_t <- bec_ld[bec_ld$bc_boundary == "bc_boundary_land_tiled", ]
saveRDS(bec_ld_t, "tmp/bec_ld_t.rds")

bec_ld_geojson <- geojson_json(bec_ld_t)

bec_ld_t@data %>%
  filter(category %in% c("01_PPA", "02_Protected_Other")) %>%
  group_by(zone) %>%
  summarize(area_prot = sum(shape_area)) %>%
  left_join(bec_zone@data, by = c("zone" = "ZONE")) %>%
  mutate(percent_prot = area_prot / zone_area) %>%
  arrange(percent_prot)

## Get the spatial file in - read from saved rds if exists, otherwise process raw file
# cl_rds <- "tmp/cl.rds"
# cl_clip_rds <- "tmp/cl_clip.rds"
# if (!file.exists(cl_rds)) {
#   cl <- readOGR("data/conservationlands.gdb", "conservationlands", stringsAsFactors = FALSE)
#
#   cl <- fix_geo_problems(cl)
#
#   saveRDS(cl, cl_rds)
# } else {
#   cl <- readRDS(cl_rds)
# }
#
# cl_t <- cl[cl$bc_boundary == "bc_boundary_land_tiled", ]

## Clip to BC Boundary
# if (!file.exists(cl_clip_rds)) {
#   system.time(cl_clip <- parallel_apply(cl, "category",
#                             raster::intersect,
#                             y = bc_bound_hres,
#                             recombine = TRUE)) # 1.5 hrs
#
#   system.time(cl_clip2 <- parallel_apply(cl, "category",
#                             rmapshaper::ms_clip,
#                             clip = bc_bound_hres,
#                             recombine = TRUE))
#
#   saveRDS(cl_clip, cl_clip_rds)
# } else {
#   cl_clip <- readRDS(cl_clip_rds)
# }

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

saveRDS(mock, "tmp/mock_spatial.rds")
saveRDS(mock_ld_agg, "tmp/mock_spatial_agg.rds")
saveRDS(mock_ld_agg_simp, "tmp/mock_spatial_agg_simp.rds")

