library(rgdal)
library(rgeos)
library(raster)
library(bcmaps)
library(rmapshaper)
library(parallel)
library(ggplot2)
library(ggpolypath)
library(ggthemes)
library(dplyr)

source("fun.R")


## Get the spatial file in - read from saved rds if exists, otherwise process raw file
cl_rds <- "tmp/cl.rds"
cl_clip_rds <- "tmp/cl_clip.rds"
if (!file.exists(cl_rds)) {
  cl <- readOGR("data/conservation_lands", "conservation_lands", stringsAsFactors = FALSE)

  cl <- fix_geo_problems(cl)

  saveRDS(cl, cl_rds)
} else {
  cl <- readRDS(cl_rds)
}

cat_rollup_table <- unique(cl@data[, c("category", "rollup")])

if (!file.exists(cl_clip_rds)) {
  cl_u <- gUnaryUnion(cl, id = cl$category)
  cl_u_clip <- gIntersection(cl_u, bc_bound_hres, byid = TRUE)
  row.names(cl_u_clip) <- sapply(strsplit(row.names(cl_u_clip), " "), `[`, 1)
  cl_u_clip <- SpatialPolygonsDataFrame(cl_u_clip, cat_rollup_table, match.ID = "category")
  cl_u_clip$area <- gArea(cl_u_clip, byid = TRUE)
  saveRDS(cl_u_clip, cl_clip_rds)
} else {
  cl_u_clip <- readRDS(cl_clip_rds)
}

eco_x_cl_rds <- "tmp/ecoreg_x_cl.rds"
if (!file.exists(eco_x_cl_rds)) {
  ecoregions_t <- readRDS("tmp/ecoregions_t.rds")
  system.time(eco_cl_int <- parallel_apply(cl_u_clip, "category",
                                           raster::intersect,
                                           y = ecoregions_t,
                                           recombine = TRUE)) # This took 13 hours
  eco_cl_int$area <- gArea(eco_cl_int, byid = TRUE)
  saveRDS(eco_cl_int, eco_x_cl_rds)
} else {
  eco_cl_int <- readRDS(eco_x_cl_rds)
}

eco_cl_summary <- eco_cl_int@data %>%
  group_by(CRGNNM, rollup) %>%
  summarize(area_ha = sum(area, na.rm = TRUE) * 1e-4,
            percent = sum(area, na.rm = TRUE) / mean(area.2, na.rm = TRUE) * 100)

## Simplify the aggregated designation rollup polygons
cl_simp_rds <- "tmp/cl_simp.rds"
if (!file.exists(cl_simp_rds)) {

  cl_simp <- parallel_apply(cl_u_clip, "category", ms_simplify,
                                keep = 0.01, keep_shapes = TRUE,
                                recombine = TRUE)

  cl_simp <- fix_geo_problems(cl_simp)

  saveRDS(cl_simp, cl_simp_rds)
} else {
  cl_simp <- readRDS(cl_simp_rds)
}

simp_zone_intersect <- parallel_apply(cl_simp, "category",
                                      raster::intersect,
                                      y = bec_zone_simp,
                                      recombine = TRUE)

simp_zone_intersect$area <- gArea(simp_zone_intersect, byid = TRUE)

test_simp_zone_summmary <- simp_zone_intersect@data %>%
  group_by(ZONE, rollup) %>%
  summarize(area_ha = sum(area, na.rm = TRUE) * 1e-4,
            percent = sum(area, na.rm = TRUE) / mean(area.1, na.rm = TRUE) * 100)

summary <- cl_u_clip@data %>%
  group_by(rollup) %>%
  summarise(area_designated = sum(area, na.rm = TRUE),
            percent_of_bc = round(area_designated / gArea(bc_bound_hres) * 100, 2))
readr::write_csv(summary, "out/total_provincial_summary.csv")


## Convert to ggplot2-friendly data frame, and plot

gg_bc <- gg_fortify(bc_bound_hres)

gg_cl <- gg_fortify(cl_simp)

cols <- c("#004529", "#238443", "#addd8e", "#f7fcb9")
labels = c("01_PPA"                    = "Parks and Protected Areas",
           "02_Protected_Other"        = "Other Protected Lands",
           "03_Exclude_1_2_Activities" = "Exclude 1 or 2 Actitivies",
           "04_Managed"                = "Managed Lands")

ggplot(gg_bc, aes(x = long, y = lat, group = group)) +
  geom_polygon(colour = "grey70", fill = "grey80") +
  geom_polypath(data = gg_cl, aes(x = long, y = lat, group = group, fill = rollup)) +
  scale_fill_manual(name = "Land Designation Category",
                    values = cols, labels = labels) +
  coord_fixed() +
  theme_map()

