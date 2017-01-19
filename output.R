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
library(envreportutils) #theme_soe_facet

files_list <- list.files("out-shiny", full.names = TRUE)
file.copy(from = files_list, to = "../land-designations-shinyapp/app/data", overwrite = TRUE)

ggplot(gg_ld_bec, aes(x = long, y = lat, group = group)) +
  geom_polypath(aes(fill = category)) +
  coord_fixed()

ggplot(gg_ld_ecoreg, aes(x = long, y = lat, group = group)) +
  geom_polypath(aes(fill = category)) +
  coord_fixed()

## Static facet bar chart for summary by category

##read in datafile
bgc <- read.csv("out/bc_bgc_zone_land_designations_summary.csv", stringsAsFactors = FALSE)

des_cols <- c("01_PPA"                    = "#00441b",
              "02_Protected_Other"        = "#006d2c",
              "03_Exclude_1_2_Activities" = "#a6d96a",
              "04_Managed"                = "#fdbf6f")



##function to roll-up two protected categories
rollup_category <- function(category) {
  factor(ifelse(category %in% c("01_PPA", "02_Protected_Other"),
                "Prot", category),
         levels = c("04_Managed", "03_Exclude_1_2_Activities", "Prot"), ordered = TRUE)
}

##roll-up two protected categories
bgc$rollup <- rollup_category(bgc$category)

#facet labels
lab <- c("04_Managed" = "Managed Areas",
         "03_Exclude_1_2_Activities" = "Exclude 1 or 2 Activities",
         "Prot" = "Protected Lands")

##facet plot
facetplot <- ggplot(bgc, aes(x = ZONE, y = percent_designated, fill = category)) +
  geom_col() +
  facet_wrap(~rollup, nrow=3, labeller = labeller(rollup = lab)) +
  scale_fill_manual(values = des_cols) +
 #  coord_flip() +
  labs(x = "Biogeoclimatic Zone\n", y = "Percent Designated") +
  scale_y_continuous(expand = c(0, 0), breaks = seq(0, 80, 10), limits = c(0, 80)) +
  theme_soe_facet() +
  theme(panel.grid.major.y = element_line(colour = "grey90"),
        panel.grid.minor.y = element_line(colour = "grey90"),
        panel.grid.major.x = element_blank(),
        strip.placement = "top")
#        plot.margin = unit(c(2,0,1,0), "lines"))
plot(facetplot)

