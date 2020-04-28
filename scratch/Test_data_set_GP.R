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

# initial data exploration


library(sf)
library(readr)
library(bcmaps)
library(dplyr)
library(feather)
library(rmapshaper)
library(ggplot2)
library(ggpolypath)
library(envreportutils) #theme_soe_facet & png_retina
library(magrittr) # %>%
library(feather) #read in feather file
library(ggthemes)




source("fun.R")

## Get full land designations file
  ld_t <- read_sf("data/designatedlands.gpkg") %>%
    # filter(bc_boundary == "bc_boundary_land_tiled") %>%
    #  select(-bc_boundary) %>%
    mutate(calc_area = st_area(.))



c("forest_restriction", "og_restriction", "mine_restriction" )


## BC Summary
bc_ld_summary <- ld_t %>%
  st_set_geometry(NULL) %>%
  group_by(forest_restriction) %>%
  summarize(area_des_ha = as.numeric(sum(calc_area)) * 1e-4) %>%
  mutate(percent_des = (area_des_ha * 1e4) / as.numeric(sum(ld_t$calc_area)) * 100) %>%
  mutate_if(is.numeric, round, digits = 2) %>%
 # write_feather("out-shiny/bc_ld_summary.feather") %>%
  write_csv("out/bc_land_designations_summary.csv")



## plot BC map with forestry catergories
forest <- ld_t %>%
  filter(!forest_restriction == "") %>%
  select(designation, forest_restriction)



ld_map <- ggplot(data = ld_t) +
  geom_sf(aes(fill = forest_restriction))


  geom_polypath(aes(fill = forest_restriction)) #+





    scale_fill_manual(values = des_cols, labels = cat_labels_full, name = NULL) +
  coord_fixed(expand = FALSE) +
  theme_map() +
  theme(legend.position="none",
        legend.text = element_text(size = 12))
#  guides(guide = "none")


    ## bgc facet plot
    bgcfacetplot <- ggplot(ld_t, aes(x = ZONE_NAME, y = percent_designated, fill = category)) +
      geom_col() +
      facet_wrap(~rollup, nrow=3, labeller = labeller(rollup = lab)) +
      scale_fill_manual(values = des_cols, guide =FALSE) +
      coord_flip() +
      labs(x = "Biogeoclimatic Zone\n", y = "Percent Designated") +
      scale_y_continuous(expand = c(0, 0), breaks = seq(0, 65, 5), limits = c(0, 65)) +
      theme_soe_facet() +
      theme(panel.grid.major.x = element_line(colour = "grey90"),
            panel.grid.minor.x = element_line(colour = "grey90"),
            panel.grid.major.y = element_blank(),
            strip.placement = "top",
            strip.text.x = element_text(size = 13),
            axis.title = element_text(size = 14),
            axis.text = element_text(size = 10),
            plot.margin = unit(c(2,2,1,1), "lines"))
    plot(bgcfacetplot)




