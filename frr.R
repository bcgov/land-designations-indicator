library(rgdal)
library(raster)
library(bcmaps)
library(rmapshaper)
library(parallel)
library(ggplot2)
library(ggthemes)

source("fun.R")

## Get the spatial file in - read from saved rds if exists, otherwise process raw file
frr_rds <- "tmp/frr.rds"
if (!file.exists(frr_rds)) {
  frr <- readOGR("data/rr_forAndy.gdb", "rr_restriction", stringsAsFactors = FALSE)

  frr2 <- frr[!is.na(frr$rr_restriction_rollup), ] # Remove non-designated areas
  frr2 <- fix_self_intersect(frr2)

  frr2 <- transform_bc_albers(frr2)
  saveRDS(frr2, frr_rds)
  rm(frr)
} else {
  frr2 <- readRDS(frr_rds)
}

## Aggregate by rollup group
frr_agg_rds <- "tmp/frr_agg.rds"
if (!file.exists(frr_agg_rds)) {
  frr_agg <- aggregate(frr2[, "rr_restriction_rollup"], by = list(cons_cat = frr2$rr_restriction_rollup), FUN = max)
  saveRDS(frr_agg, "tmp/frr_agg.rds")
} else {
  frr_agg <- readRDS(frr_agg_rds)
}

ld_agg <- frr_agg
rm(frr2, frr_agg)

## Simplify the aggregated designation rollup polygons
frr_agg_list_simp_rds <- "tmp/frr_agg_list_simp.rds"
if (!file.exists(frr_agg_list_simp_rds)) {

  # Split into a list so objects passed into ms_simplify are smaller. Also can be parallelized this way
  ld_agg_list <- lapply(ld_agg$cons_cat[ld_agg$cons_cat != "06_No Special Restriction"], function(x) {
    ld_agg[ld_agg$cons_cat == x,]
  })

  ## Do the simplification in Parallel:
  no_cores <- detectCores() - 1 # Use one less than max cores so we have computing capacity to do other things
  cl <- makeCluster(no_cores)

  ld_agg_list_simp <- parLapply(cl, ld_agg_list, ms_simplify, keep = 0.01, keep_shapes = TRUE)
  stopCluster(cl)

  ld_agg_list_simp <- lapply(ld_agg_list_simp, fix_self_intersect)

  saveRDS(ld_agg_list_simp, frr_agg_list_simp_rds)
} else {
  ld_agg_list_simp <- readRDS(frr_agg_list_simp_rds)
}

## Recombine list elements into one simplified sp object
ld_agg_simp <- ld_agg_list_simp[[1]]
for (i in 2:length(ld_agg_list_simp)) {
  ld_agg_simp <- rbind(ld_agg_simp, ld_agg_list_simp[[i]])
}

## Convert to ggplot2-friendly data frame, and plot
gg_ld_agg <- gg_fortify(ld_agg_simp)

ggplot(gg_ld_agg, aes(x = long, y = lat, group = group, fill = cons_cat)) +
  geom_polygon() +
  coord_fixed() +
  theme_map()

