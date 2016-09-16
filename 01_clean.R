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
library(bcmaps)
library(geojsonio)
library(rmapshaper)
library(feather)
library(cleangeo)

source("fun.R")

dir.create("tmp", showWarnings = FALSE)

## The CARTS database is downloadable from the Canadian Council on
## Ecological Areas here: http://www.ccea.org/carts/
carts_zip <- "data/CARTS_31122015_Download.zip"
unzip(carts_zip, exdir = "data", overwrite = TRUE)
carts <- readOGR("data/CARTS_Update_31122015.gdb", "CARTS_Update_31122015_WithoutQc", stringsAsFactors = FALSE)
bc_carts <- carts[carts$BIOME == "T" & carts$LOC_E == "British Columbia", ]

## Mock four categories:
set.seed(42)
bc_carts$cons_cat <- sample(c("A", "B", "C", "D"),
                            size = nrow(bc_carts@data), replace = TRUE)
bc_carts <- transform_bc_albers(bc_carts)
mock <- bc_carts[,"cons_cat"]
mock_ld_agg <- raster::aggregate(mock, by = "cons_cat")
mock_ld_agg <- clgeo_Clean(mock_ld_agg)

mock_ld_agg_simp <- geojson_json(mock) %>%
  ms_simplify(keep = 0.05, keep_shapes = TRUE) %>% # explode = TRUE if want to keep all shapes
  ms_dissolve(field = "cons_cat") %>%
  geojson_sp() %>%
  clgeo_Clean() %>%
  transform_bc_albers()

################################################################################

## Ecoregions:
m_ecoregions <- c("HCS", "IPS", "OPS", "SBC", "TPC")

## load ecoregions data from bcmaps package
data("ecoregions")

## Extract the terrestrial and marine portions of GPB into separate objects
gpb_terrestrial <- ms_clip(ecoregions[ecoregions$CRGNCD == "GPB",],
                           bc_bound_hres)
## Fix it up:
gpb_terrestrial <- clgeo_Clean(gpb_terrestrial)

## Add terrestrial portion of GPB back to terrestrial ecoregions
ecoregions_t <- rbind(ecoregions[!ecoregions$CRGNCD %in% c("GPB", m_ecoregions), ],
                      gpb_terrestrial[, setdiff(names(gpb_terrestrial), "rmapshaperid")])

## Calcualte the area of the polygons
ecoregions_t$area <- gArea(ecoregions_t, byid = TRUE)

## Create simplified versions for visualization
ecoregions_t_simp <- ms_simplify(ecoregions_t, 0.05) %>%
  clgeo_Clean()
ecoregions_t_simp_leaflet <- ms_simplify(ecoregions_t[,c("CRGNCD", "CRGNNM")], 0.01) %>%
  clgeo_Clean() %>%
  spTransform(CRSobj = CRS("+init=epsg:4326"))
ecoregions_t_simp_leaflet$CRGNCD <- as.character(ecoregions_t_simp_leaflet$CRGNCD)
ecoregions_t_simp_leaflet$CRGNNM <- tools::toTitleCase(tolower(as.character(ecoregions_t_simp_leaflet$CRGNNM)))
ecoregions_t_simp_leaflet$rmapshaperid <- NULL

###############################################################################
## Biogeoclimatic zones
if (!file.exists("data/BEC_BIOGEOCLIMATIC_POLY.gdb")) {
  unzip("data/BCGW_78757263_1473884441500_9556.zip", exdir = "data")
}

bgc <- readOGR("data/BEC_BIOGEOCLIMATIC_POLY.gdb",
               "WHSE_FOREST_VEGETATION_BEC_BIOGEOCLIMATIC_POLY_polygon",
               stringsAsFactors = FALSE)
writeOGR(bgc, "data", "bec", driver = "ESRI Shapefile")

unlink(paste0("data/", c("bc_bound.geojson", "bec_clip*")))
geojsonio::geojson_write(bc_bound_hres, file = "data/bc_bound.geojson") # bc_bound_hres is from bcmaps package
system("mapshaper data/bec.shp -clip data/bc_bound.geojson -explode -o data/bec_clip.shp")

## Check for validity of bec polygons
bec_t <- readOGR("data", "bec_clip", stringsAsFactors = FALSE)
bec_t <- clgeo_Clean(bec_t)
bec_t$area <- gArea(bec_t, byid = TRUE)

bec_zone <- raster::aggregate(bec_t, by = "ZONE")
bec_zone <- clgeo_Clean(bec_zone)
bec_zone$area <- gArea(bec_zone, byid = TRUE)

## Simplify BEC pologyons for use in display
bec_zone_simp <- ms_simplify(bec_zone, keep = 0.005, keep_shapes = TRUE)
# unlink("data/bec_zone*")
# writeOGR(bec_zone, "data", "bec_zone", "ESRI Shapefile")
# system("mapshaper data/bec_zone.shp -simplify 0.01 keep-shapes -o data/bec_zone_simp.shp")
# bec_zone_simp <- readOGR("data", "bec_zone_simp", stringsAsFactors = FALSE)
## Repair orphaned holes
bec_zone_simp <- clgeo_Clean(bec_zone_simp)

bec_zone_leaflet <- ms_simplify(bec_zone_simp, 0.1, explode = TRUE) %>%
  ms_dissolve(field = "ZONE") %>%
  clgeo_Clean() %>%
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
