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


## Land Designations Indicator - 01_load.R

library(sf)
library(readr)
library(raster)
library(tidyverse)
library(bcmaps)
library(rmapshaper)

# Bring in land designations layer (.gpkg)
data.ld = file.path("data/designatedlands.gpkg")
data.dir = file.path("data")
ld = read_sf('data/designatedlands.gpkg')

# ld_m <- st_read(dsn = data.ld, layer="mine_restriction")
# ld_f <- st_read(dsn = data.ld, layer="forest_restriction")
# ld_o <- st_read(dsn = data.ld, layer="og_restriction")

# # Bring in industry rasters - mining, forestry, o&g
# forest <- raster(file.path(data.dir, "forest_restriction.tif"))
# og <- raster(file.path(data.dir, "og_restriction.tif"))
# mine <- raster(file.path(data.dir, "mine_restriction.tif"))

# Load required layers from bcmaps
bc_boundary <- bc_bound_hres(class="sf", ask= FALSE, force=FALSE)

bc_size_km <- bc_area(what="land", units = "km2")

bc_size_ha <- bc_area(what="land", units = "ha")

bc_cities <- bc_cities(class= "sf", ask= FALSE, force=FALSE)

bc_nr_dist <- nr_districts(class="sf", ask= FALSE, force=FALSE)


if (!exists("tmp")) dir.create("tmp", showWarnings = FALSE)

#save vector objs
# save(bc_boundary, bc_size_km, bc_size_ha, bc_cities, ld_f, ld_o, ld_m, file = "tmp/raw_data_vect.RData")
save(bc_boundary, bc_nr_dist, bc_size_km, bc_size_ha, bc_cities, ld, file = "tmp/raw_data_vect.RData")

#save(forest, mine, og, file = "tmp/raw_data_ras.RData")

#example map :)
ggplot()+
  geom_sf(data=bc_nr_dist, aes(fill=DISTRICT_NAME))+
  geom_sf(data=bc_cities, size = 2, shape = 21)+
  geom_sf(data=bc_boundary,fill='transparent') +
  theme(legend.position = 'none')

