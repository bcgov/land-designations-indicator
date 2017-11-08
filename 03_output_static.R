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

library(ggplot2)
library(ggpolypath)
library(envreportutils) #theme_soe_facet & png_retina
library(magrittr) # %>%
library(feather) #read in feather file
library(ggthemes)

# ggplot(gg_ld_bec, aes(x = long, y = lat, group = group)) +
#   geom_polypath(aes(fill = category)) +
#   coord_fixed()
#
# ggplot(gg_ld_ecoreg, aes(x = long, y = lat, group = group)) +
#   geom_polypath(aes(fill = category)) +
#   coord_fixed()

## Static Outputs

## @knitr pre

## colour palette
des_cols <- c("01_PPA"                    = "#004529",
              "02_Protected_Other"        = "#006837",
              "03_Exclude_1_2_Activities" = "#41ab5d",
              "04_Managed"                = "#d9f0a3")

## category labels for legend
cat_labels_full <- c("01_PPA" = "Parks & Protected Areas",
                "02_Protected_Other" = "Other Protected Lands",
                "03_Exclude_1_2_Activities" = "Resource Exclusion Areas",
                "04_Managed" = "Spatially Managed Areas")

## function to roll-up the two protected (sub)categories
rollup_category <- function(category) {
  factor(ifelse(category %in% c("01_PPA", "02_Protected_Other"),
                "Prot", category),
         levels = c("04_Managed", "03_Exclude_1_2_Activities", "Prot"), ordered = TRUE)
}

## @knitr pre end

## Static BC Summary map

## read in datafiles
ld_df <- read_feather("out-shiny/gg_ld_ecoreg.feather")
bc_map <-  read_feather("out-shiny/gg_bc_bound.feather")

## @knitr map

## plot BC map with 3 categories
ld_map <- ggplot(ld_df, aes(x = long, y = lat, group = group)) +
  geom_polypath(data = bc_map, fill = "grey80", colour = "gray80") +
  geom_polypath(aes(fill = category)) +
  scale_fill_manual(values = des_cols, labels = cat_labels_full, name = NULL) +
  coord_fixed(expand = FALSE) +
  theme_map() +
  theme(legend.position="none",
        legend.text = element_text(size = 12))
#  guides(guide = "none")

## @knitr map end

plot(ld_map)

## print BC Summary map to PNG at retina quality
png_retina(filename = "out/bc_ld_map.png",
    width = 500, height = 500, units = "px")
ld_map
dev.off()




## Static bar chart for provincial summary by category
## read in datafile
bcsum <- read.csv("out/bc_land_designations_summary.csv", stringsAsFactors = FALSE)


## @knitr bcsummary

## roll-up two protected categories
bcsum$rollup <- rollup_category(bcsum$category)

## category labels for legend
cat_labels <- c("01_PPA" = "Parks & Protected Areas",
                "02_Protected_Other" = "Other Protected Lands")
                # "03_Exclude_1_2_Activities" = "Resource Exclusion Areas",
                # "04_Managed" = "Managed Areas")

## setting factors for ordering bars
bcsum$category <- factor(bcsum$category, levels = c("04_Managed",
                                                    "03_Exclude_1_2_Activities",
                                                    "02_Protected_Other",
                                                    "01_PPA"))
## bc summary bar plot
bcsumplot <- ggplot(bcsum, aes(x = rollup, y = percent_des, fill = category)) +
  geom_col(width = .6) +
  scale_fill_manual(breaks = c("01_PPA","02_Protected_Other"), values = des_cols, labels = cat_labels) +
  scale_x_discrete(labels = c("04_Managed" = "Spatially\nManaged\nAreas",
                              "03_Exclude_1_2_Activities" = "Resource\nExclusion\nAreas",
                              "Prot" = "Protected\nLands")) +
  guides(fill = guide_legend(title = NULL)) +
  coord_flip() +
  labs(x = "", y = "Percent of British Columbia Designated") +
  scale_y_continuous(expand = c(0, 0), breaks = seq(0, 25, 5), limits = c(0, 25)) +
  theme_soe() +
  theme(panel.grid.major.x = element_line(colour = "grey90"),
        panel.grid.minor.x = element_line(colour = "grey90"),
        panel.grid.major.y = element_blank(),
       legend.position = "top",
    #    legend.direction = "vertical",
  #     legend.position = c(.85, .72),
        legend.background = element_blank(),
        legend.text = element_text(size = 12),
        axis.title = element_text(size = 14),
       axis.text = element_text(size = 12),
       plot.margin = unit(c(3,3,2,1), "lines"))
 plot(bcsumplot)

## @knitr bcsummary end

## print BC Summary plot to PNG at retina quality
svg_px("./out/bc_sum_plot.svg", width = 500, height = 500)
bcsumplot
dev.off()


## Static facet bar chart for summary by bgc and category
##read in datafile
bgc <- read.csv("out/bc_bgc_zone_land_designations_summary.csv", stringsAsFactors = FALSE)

## @knitr bgc_summary

##roll-up two protected categories
bgc$rollup <- rollup_category(bgc$category)

#facet labels
lab <- c("04_Managed" = "Spatially Managed Areas",
         "03_Exclude_1_2_Activities" = "Resource Exclusion Areas",
         "Prot" = "Protected Lands")

## order bars within facets
bgc <- bgc  %>%
    order_df("ZONE_NAME", "percent_designated", fun = max)

## bgc facet plot
bgcfacetplot <- ggplot(bgc, aes(x = ZONE_NAME, y = percent_designated, fill = category)) +
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

## @knitr bgc_summary end

## print BGC facet plot to PNG at retina quality
png_retina(filename = "out/bgc_facet_plot.png",
    width = 900, height = 700, units = "px")
bgcfacetplot
dev.off()

## Static facet bar chart for summary by ecoregion and category
##read in datafile
eco <- read.csv("out/bc_ecoregions_land_designations_summary.csv", stringsAsFactors = FALSE)

## @knitr ecoreg_summary

##roll-up two protected categories
eco$rollup <- rollup_category(eco$category)

#facet labels
lab <- c("04_Managed" = "Spatially\nManaged\nAreas",
         "03_Exclude_1_2_Activities" = "Resource\nExclusion\nAreas",
         "Prot" = "Protected\nLands")

eco <- eco  %>%
  order_df("ecoregion_name", "percent_des", fun = max)

##ecoregion facet plot
ecofacetplot <- ggplot(eco, aes(x = ecoregion_name, y = percent_des, fill = category)) +
  geom_col() +
  facet_wrap(~rollup, nrow=1, labeller = labeller(rollup = lab)) +
  scale_fill_manual(values = des_cols, guide =FALSE) +
  coord_flip() +
  labs(x = "Ecoregion\n", y = "Percent Designated") +
  scale_y_continuous(expand = c(0, 0), breaks = seq(20, 100, 20), limits = c(0, 100)) +
  theme_soe_facet() +
  theme(panel.grid.major.x = element_line(colour = "grey90"),
        panel.grid.minor.x = element_line(colour = "grey90"),
        panel.grid.major.y = element_blank(),
        strip.placement = "top",
        strip.text.x = element_text(size = 13),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 9),
        plot.margin = unit(c(2,4,1,4), "lines"))
 plot(ecofacetplot)

## @knitr ecoreg_summary end

## print facet plot to PNG at retina quality
png_retina(filename = "out/ecoregion_facet_plot.png",
    width = 900, height = 700, units = "px")
ecofacetplot
dev.off()

## Summarize overlaps file to get terrestrial areas of the different designations falling under
## categories 1 and 2:
ld_overlaps_summary <- ld_overlaps %>%
  st_set_geometry(NULL) %>%
  group_by(designation, designation_name, bc_boundary) %>%
  summarize(area = sum(area))

ld_overlaps_summary_protected <- ld_overlaps_summary %>%
  filter(bc_boundary == "bc_boundary_land_tiled") %>%
  group_by(designation) %>%
  summarize(area = sum(area)) %>%
  mutate(percent_bc = 100 * (area / sum(st_area(bc_bound_trim))),
         des_number = gsub("^c(\\d\\d).+", "\\1", designation),
         designation = gsub("^c\\d\\d_ol_", "", designation),
         area_ha = units::set_units(area, ha)) %>%
  filter(des_number < 16) %>%
  select(designation, area_ha, percent_bc)
write_csv(ld_overlaps_summary_protected, "out/land_designations_ppa_summary_2017-11-07.csv")

