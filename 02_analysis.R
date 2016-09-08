library(rgdal)
library(sp)
library(rgeos)
library(raster)
library(rmapshaper)
library(ggplot2)
library(ggthemes)
library(dplyr)
library(feather)

ld <- readRDS("tmp/mock_spatial.rds")
ld_agg <- readRDS("tmp/mock_spatial_agg.rds")
ld_agg_simp <- readRDS("tmp/mock_spatial_agg_simp.rds")
ecoreg <- readRDS("tmp/ecoregions_t.rds")
ecoreg_simp <- readRDS("tmp/ecoregions_t_simp.rds")

## Intersect land designations with ecoregions and summarize
ld_x_ecoreg <- raster::intersect(ecoreg, ld_agg)

ld_x_ecoreg$area <- rgeos::gArea(ld_x_ecoreg, byid = TRUE)

ld_ecoreg_summary <- ld_x_ecoreg@data %>%
  group_by(CRGNCD, cons_cat) %>%
  summarise(area_des = sum(area, na.rm = TRUE)) %>%
  left_join(group_by(ecoreg@data, CRGNCD) %>%
              summarize(ecoreg_area = sum(area, na.rm = TRUE)), by = "CRGNCD") %>%
  mutate(percent_des = area_des / ecoreg_area * 100,
         area_des_ha = area_des * 1e-4) %>%
  write_feather("ld_ecoreg_summary.feather")

# Intersect simplified versions for mapping display
ld_x_ecoreg_simp <- raster::intersect(ld_agg_simp, ecoreg_simp)

gg_ld_x_ecoreg <- gg_fortify(ld_x_ecoreg_simp) %>% write_feather("gg_ld_ecoreg.feather")
gg_ecoreg <- gg_fortify(ecoreg_simp) %>% write_feather("gg_ecoreg.feather")

ecoreg_cds <- unique(gg_ecoreg$CRGNCD)

lapply(ecoreg_cds, gg_ld_ecoreg, gg_ld_x_ecoreg, gg_ecoreg)



####################################
ecoregions_list <- lapply(ecoreg$CRGNCD, function(x) {
  ecoreg[ecoreg$CRGNCD == x,]
})

names(ecoregions_list) <- ecoreg$CRGNCD

# This takes about 30 minutes
# ld_ecoreg_clip_list <- lapply(ecoregions_list, function(x) {
#   rmapshaper::ms_clip(ld_agg, x)
# })

names(ld_ecoreg_clip_list) <- ecoreg$CRGNCD

lapply(names(ld_ecoreg_clip_list), plot_ecoreg_land_des, ecoregions_list, ld_ecoreg_clip_list)

