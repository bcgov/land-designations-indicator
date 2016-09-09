file.copy(paste0("out/", c("gg_ecoreg.feather", "gg_ld_ecoreg.feather", "ld_ecoreg_summary.feather",
                           "ecoregions_t_leaflet.rds")),
          "../land-designations-shinyapp/data/", overwrite = TRUE)
