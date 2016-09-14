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
mock_ld_agg <- fix_self_intersect(mock_ld_agg)

mock_ld_agg_simp <- geojson_json(mock) %>%
  ms_simplify(keep = 0.05, keep_shapes = TRUE) %>% # explode = TRUE if want to keep all shapes
  ms_dissolve(field = "cons_cat") %>%
  geojson_sp() %>%
  fix_self_intersect() %>%
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
gpb_terrestrial <- fix_self_intersect(gpb_terrestrial)

## Add terrestrial portion of GPB back to terrestrial ecoregions
ecoregions_t <- rbind(ecoregions[!ecoregions$CRGNCD %in% c("GPB", m_ecoregions), ],
                      gpb_terrestrial[, setdiff(names(gpb_terrestrial), "rmapshaperid")])

## Calcualte the area of the polygons
ecoregions_t$area <- gArea(ecoregions_t, byid = TRUE)

## Create simplified versions for visualization
ecoregions_t_simp <- ms_simplify(ecoregions_t, 0.05) %>%
  fix_self_intersect()
ecoregions_t_simp_leaflet <- ms_simplify(ecoregions_t[,"CRGNCD"], 0.01) %>%
  fix_self_intersect() %>%
  spTransform(CRSobj = CRS("+init=epsg:4326"))
ecoregions_t_simp_leaflet$CRGNCD <- as.character(ecoregions_t_simp_leaflet$CRGNCD)

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
bec_t <- fix_self_intersect(bec_t)

## Simplify BEC pologyons for use in display
bec_t$poly_id <- row.names(bec_t)
unlink("data/bec_t*")
writeOGR(bec_t, "data", "bec_t", "ESRI Shapefile")
system("mapshaper data/bec_t.shp -simplify 0.01 keep-shapes -o data/bec_t_simp.shp")
bec_t_simp <- readOGR("data", "bec_t_simp", stringsAsFactors = FALSE)
## Repair orphaned holes
bec_t_simp <- fix_self_intersect(bec_t_simp)

## Put area back in m2
bec_t$area <- gArea(bec_t, byid = TRUE)
bec_t_simp$area <- bec_t$area

## Create a map of bec zones
bec_zone_simp <- raster::aggregate(bec_t_simp, by = "ZONE")
bec_zone_simp$area <- gArea(bec_zone_simp, byid = TRUE)

foo <- ms_simplify(bec_zone_simp, 0.05, keep_shapes = TRUE, explode = TRUE)

saveRDS(mock, "tmp/mock_spatial.rds")
saveRDS(mock_ld_agg, "tmp/mock_spatial_agg.rds")
saveRDS(mock_ld_agg_simp, "tmp/mock_spatial_agg_simp.rds")
saveRDS(ecoregions_t, "tmp/ecoregions_t.rds")
saveRDS(ecoregions_t_simp, "tmp/ecoregions_t_simp.rds")
saveRDS(ecoregions_t_simp_leaflet, "out/ecoregions_t_leaflet.rds")
bc_bound %>%
  gg_fortify() %>%
  write_feather("out/gg_bc_bound.feather")
