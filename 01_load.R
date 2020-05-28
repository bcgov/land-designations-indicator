# Copyright 2020 Province of British Columbia
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


# load data required for indicator


library(sf)
library(readr)
library(raster)
library(tidyverse)
#library(bcmaps)
#library(dplyr)
#library(feather)
#library(rmapshaper)


if(!dir.exists("tmp")) dir.create("tmp", showWarnings = FALSE)


# download data files


# use drake to pull data set ? test this set up and save to data folder

# temporary data source
data.dir = file.path("data/outputs/outputs")





#list.files(data.dir)

tif <- list.files(data.dir, pattern = ".tif", full.names = TRUE)

# read in rasters and name

# option 1: stack
tif.stack <- stack(tif)


# option 2: list ?
tifs <- lapply(tif, function(t){
  raster(file.path(data.dir, t))
})

names(tifs) <- tif


# set key for restriction level

rest_key <- tribble(
  ~ value, ~ rlevel,
  0 , "None",
  1 , "Low",
  2 , "Medium",
  3 , "High",
  4 , "Full"
)



# temp raster
#forest <- raster(file.path(data.dir, "forest_restriction.tif"))
#og <- raster(file.path(data.dir, "og_restriction.tif"))
#mine <- raster(file.path(data.dir, "mine_restriction.tif"))



# read in base data

bc_bound_trim_rds <- "tmp/bc_bound_trim.rds"
bc_bound_trim <- tryCatch(readRDS(bc_bound_trim_rds), error = function(e) {
  bc_bound_trim <- read_sf("data/BC_Boundary.gdb")
  bc_bound_trim <- transform_bc_albers(bc_bound_trim)
  saveRDS(bc_bound_trim, bc_bound_trim_rds)
  bc_bound_trim
})


## Get full land designations file
ld_t_rds <- "tmp/ld_t.rds"
ld_t <- tryCatch(readRDS(ld_t_rds), error = function(e) {
  ld_t <- st_read(file.path(data.dir, "designatedlands.gpkg", layer = "designatedlands" ))# %>%
  #ld_t <- read_sf("data/designatedlands.gpkg") %>%
    #filter(bc_boundary == "bc_boundary_land_tiled") %>%
    #select(-bc_boundary) %>%
    #mutate(calc_area = st_area(.))
  #saveRDS(ld_t, ld_t_rds)
  #ld_t
})



st_layers(file.path(data.dir, "designatedlands.gpkg"))


f_r <- d

Driver: GPKG
Available layers:
  layer_name geometry_type features fields
1    designatedlands                 332369      9
2 forest_restriction                 391156      3
3     og_restriction                 343951      3
4   mine_restriction                  58035      3


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
ld_overlaps_rds <- "tmp/ld_overlaps.rds"
ld_overlaps <- tryCatch(readRDS(ld_overlaps_rds), error = function(e) {

  ## Read file with overlaps and designation attributes retained.
  ## category is empty, so populate with lookup table created from
  ## ld_t
  ld_overlaps <- read_sf("data/designatedlands_overlaps.gpkg") %>%
    st_collection_extract("POLYGON") %>%
    st_make_valid() %>%
    select(-category) %>%
    mutate(area = st_area(.),
           designation = gsub("ol_", "", designation),
           designation = gsub("(c01_park_national).+", "\\1", designation)) %>%
    left_join(st_set_geometry(ld_t, NULL) %>%
                distinct(designation, category), by = "designation")

  saveRDS(ld_overlaps, ld_overlaps_rds)
  ld_overlaps
})

# Convert input gpkg to shp and use mapshaper to clean it up and aggregate it.
# Then zip up the inputs for the release.
if (nzchar(Sys.which("ogr2ogr")) && nzchar(Sys.which("mapshaper"))) {
  system("ogr2ogr -f 'ESRI Shapefile' data/designatedlands.shp data/designatedlands.gpkg")
  system("mapshaper data/designatedlands.shp -filter '\"bc_boundary_land_tiled\".indexOf(bc_bound_1) > -1' -clean -dissolve designatio copy-fields=category -o out/designatedlands.shp")
  files_to_zip <- normalizePath(
    c(list.files("out", pattern = "designatedlands\\.(shp|dbf|prj|shx)$", full.names = TRUE),
      file.path("data", c("designatedlands.gpkg", "lands_bec.gpkg", "lands_eco.gpkg"))
    ))
  zip("out/land_designations.zip", files = files_to_zip, flags = "-j9X")
}
