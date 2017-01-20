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
library(magrittr) # %>%

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

bgc$bec_nms <- bgc$ZONE
bgc$bec_nms[bgc$bec_nms == "BAFA"] <- "Boreal Altai Fescue Alpine"
bgc$bec_nms[bgc$bec_nms == "SWB"] <- "Spruce—Willow—Birch"
bgc$bec_nms[bgc$bec_nms == "BWBS"] <- "Boreal White & Black Spruce"
bgc$bec_nms[bgc$bec_nms ==  "ESSF"] <- "Engelmann Spruce—Subalpine Fir"
bgc$bec_nms[bgc$bec_nms == "CMA"] <- "Coastal Mountain-heather Alpine"
bgc$bec_nms[bgc$bec_nms ==  "SBS"] <-  "Sub-Boreal Spruce"
bgc$bec_nms[bgc$bec_nms ==  "MH"] <-  "Mountain Hemlock"
bgc$bec_nms[bgc$bec_nms ==  "CWH"] <- "Coastal Western Hemlock"
bgc$bec_nms[bgc$bec_nms ==  "ICH"] <- "Interior Cedar—Hemlock"
bgc$bec_nms[bgc$bec_nms ==  "IMA"] <- "Interior Mountain-heather Alpine"
bgc$bec_nms[bgc$bec_nms ==  "SBPS"] <- "Sub-Boreal Pine—Spruce"
bgc$bec_nms[bgc$bec_nms ==  "MS"] <- "Montane Spruce"
bgc$bec_nms[bgc$bec_nms ==  "IDF"] <- "Interior Douglas-fir"
bgc$bec_nms[bgc$bec_nms ==  "BG"] <- "Bunchgrass"
bgc$bec_nms[bgc$bec_nms ==  "PP"] <- "Ponderosa Pine"
bgc$bec_nms[bgc$bec_nms ==  "CDF"] <- "Coastal Douglas-fir"

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
         "03_Exclude_1_2_Activities" = "Resource Exclusion Areas",
         "Prot" = "Protected Lands")

bgc <- bgc  %>%
    order_df("bec_nms", "percent_designated", fun = max)

##facet plot
facetplot <- ggplot(bgc, aes(x = bec_nms, y = percent_designated, fill = category)) +
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
        axis.text = element_text(size = 11),
        plot.margin = unit(c(2,2,1,1), "lines"))
plot(facetplot)

## print facet plot to PNG
png(filename = "out/bgc_facet_plot.png",
    width = 900, height = 700, units = "px")
facetplot
dev.off()

