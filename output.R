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
library(feather) #read in feather file
library(ggthemes)

files_list <- list.files("out-shiny", full.names = TRUE)
file.copy(from = files_list, to = "../land-designations-shinyapp/app/data", overwrite = TRUE)

ggplot(gg_ld_bec, aes(x = long, y = lat, group = group)) +
  geom_polypath(aes(fill = category)) +
  coord_fixed()

ggplot(gg_ld_ecoreg, aes(x = long, y = lat, group = group)) +
  geom_polypath(aes(fill = category)) +
  coord_fixed()

## Static Outputs

## colour palette
des_cols <- c("01_PPA"                    = "#00441b",
              "02_Protected_Other"        = "#006d2c",
              "03_Exclude_1_2_Activities" = "#a6d96a",
              "04_Managed"                = "#fdbf6f")


## Static BC Sumamry map

## read in datafiles
ld_df <- read_feather("out-shiny/gg_ld_ecoreg.feather")
bc_map <-  read_feather("out-shiny/gg_bc_bound.feather")

ld_map <- ggplot(ld_df, aes(x = long, y = lat, group = group)) +
  geom_polypath(data = bc_map, fill = "grey80", colour = "gray80") +
  geom_polypath(aes(fill = category)) +
  scale_fill_manual(values = des_cols) +
  coord_fixed(expand = FALSE) +
  theme_map() +
  guides(fill = "none")
plot(ld_map)


## print BC Summary map to PNG
png(filename = "out/bc_ld_map.png",
    width = 500, height = 500, units = "px")
ld_map
dev.off()

## function to roll-up the two protected (sub)categories
rollup_category <- function(category) {
  factor(ifelse(category %in% c("01_PPA", "02_Protected_Other"),
                "Prot", category),
         levels = c("04_Managed", "03_Exclude_1_2_Activities", "Prot"), ordered = TRUE)
}


## Static bar chart for provincial summary by category
## read in datafile
bcsum <- read.csv("out/bc_land_designations_summary.csv", stringsAsFactors = FALSE)

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
  geom_col() +
  scale_fill_manual(breaks = c("01_PPA","02_Protected_Other"), values = des_cols, labels = cat_labels) +
  scale_x_discrete(labels = c("04_Managed" = "Managed\nAreas",
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
       plot.margin = unit(c(2,3,1,1), "lines"))
plot(bcsumplot)

## print BC Summary plot to PNG
png(filename = "out/bc_sum_plot.png",
    width = 500, height = 500, units = "px")
bcsumplot
dev.off()


## Static facet bar chart for summary by bgc and category
##read in datafile
bgc <- read.csv("out/bc_bgc_zone_land_designations_summary.csv", stringsAsFactors = FALSE)

## Adding full BGC names to dataframe
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


##roll-up two protected categories
bgc$rollup <- rollup_category(bgc$category)

#facet labels
lab <- c("04_Managed" = "Managed Areas",
         "03_Exclude_1_2_Activities" = "Resource Exclusion Areas",
         "Prot" = "Protected Lands")

## order bars within facets
bgc <- bgc  %>%
    order_df("bec_nms", "percent_designated", fun = max)

## bgc facet plot
bgcfacetplot <- ggplot(bgc, aes(x = bec_nms, y = percent_designated, fill = category)) +
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
plot(bgcfacetplot)

## print BGC facet plot to PNG
png(filename = "out/bgc_facet_plot.png",
    width = 900, height = 700, units = "px")
bgcfacetplot
dev.off()

# ## Static facet bar chart for summary by ecoregion and category
# ##read in datafile
# eco <- read.csv("out/bc_ecoregions_land_designations_summary.csv", stringsAsFactors = FALSE)
#
# ## Adding full Ecoregion names to dataframe
# eco$eco_nms <- eco$ecoregion_code
# bgc$bec_nms[bgc$bec_nms == "BAFA"] <- "Boreal Altai Fescue Alpine"
# bgc$bec_nms[bgc$bec_nms == "SWB"] <- "Spruce—Willow—Birch"
# bgc$bec_nms[bgc$bec_nms == "BWBS"] <- "Boreal White & Black Spruce"
# bgc$bec_nms[bgc$bec_nms ==  "ESSF"] <- "Engelmann Spruce—Subalpine Fir"
# bgc$bec_nms[bgc$bec_nms == "CMA"] <- "Coastal Mountain-heather Alpine"
# bgc$bec_nms[bgc$bec_nms ==  "SBS"] <-  "Sub-Boreal Spruce"
# bgc$bec_nms[bgc$bec_nms ==  "MH"] <-  "Mountain Hemlock"
# bgc$bec_nms[bgc$bec_nms ==  "CWH"] <- "Coastal Western Hemlock"
# bgc$bec_nms[bgc$bec_nms ==  "ICH"] <- "Interior Cedar—Hemlock"
# bgc$bec_nms[bgc$bec_nms ==  "IMA"] <- "Interior Mountain-heather Alpine"
# bgc$bec_nms[bgc$bec_nms ==  "SBPS"] <- "Sub-Boreal Pine—Spruce"
# bgc$bec_nms[bgc$bec_nms ==  "MS"] <- "Montane Spruce"
# bgc$bec_nms[bgc$bec_nms ==  "IDF"] <- "Interior Douglas-fir"
# bgc$bec_nms[bgc$bec_nms ==  "BG"] <- "Bunchgrass"
# bgc$bec_nms[bgc$bec_nms ==  "PP"] <- "Ponderosa Pine"
# bgc$bec_nms[bgc$bec_nms ==  "CDF"] <- "Coastal Douglas-fir"
#
# TPC = "Transitional Pacific",
# HCS = "Hecate Continental Shelf",
# COG = "Coastal Gap",
# EHM = "Eastern Hazelton Mountains",
# OPS = "Outer Pacific Shelf",
# NRM = "Northern Canadian Rocky Mountains",
# OKH = "Okanogan Highland",
# CMI = "Chugach Mountains and Icefields",
# STE = "St Elias Mountains",
# BOU = "Boundary Ranges",
# YSL = "Yukon Southern Lakes",
# NUP = "Northern Alberta Upland",
# PEM = "Pelly Mountains",
# LIB = "Liard Basin",
# HHI = "Hyland Highland",
# HSL = "Hay-Slave Lowland",
# BMP = "Boreal Mountains and Plateaus",
# MPL = "Muskwa Plateau",
# CAU = "Central Alberta Upland",
# CRM = "Central Canadian Rocky Mountains",
# PRB = "Peace River Basin",
# OMM = "Omineca Mountains",
# SKM = "Skeena Mountains",
# FAB = "Fraser Basin",
# NRA = "Nass Ranges",
# SBC = "Sub-Arctic Pacific",
# FAP = "Fraser Plateau",
# WRA = "Western Continental Ranges",
# GWH = "Gwaii Haanas",
# CHR = "Chilcotin Ranges",
# SRT = "Southern Rocky Mountain Trench",
# IPS = "Inner Pacific Shelf",
# ITR = "Interior Transition Ranges",
# PAC = "Pacific Ranges",
# TOP = "Thompson-Okanagan Plateau",
# NCM = "Northern Columbia Mountains",
# NCR = "Northern Cascade Ranges",
# EVI = "Eastern Vancouver Island",
# PTR = "Purcell Transitional Ranges",
# NCD = "Northern Continental Divide",
# LOM = "Lower Mainland",
# WVI = "Western Vancouver Island",
# GPB = "Georgia-Puget Basin",
# YSH = "Yukon-Stikine Highlands",
# COH = "Columbia Highlands",
# SBF = "Selkirk-Bitterroot Foothills",
# SAU = "Southern Alberta Upland",
# ECR = "Eastern Continental Ranges"
#
# ##roll-up two protected categories
# bgc$rollup <- rollup_category(bgc$category)
#
# #facet labels
# lab <- c("04_Managed" = "Managed Areas",
#          "03_Exclude_1_2_Activities" = "Resource Exclusion Areas",
#          "Prot" = "Protected Lands")
#
# bgc <- bgc  %>%
#   order_df("bec_nms", "percent_designated", fun = max)
#
# ##facet plot
# facetplot <- ggplot(bgc, aes(x = bec_nms, y = percent_designated, fill = category)) +
#   geom_col() +
#   facet_wrap(~rollup, nrow=3, labeller = labeller(rollup = lab)) +
#   scale_fill_manual(values = des_cols, guide =FALSE) +
#   coord_flip() +
#   labs(x = "Biogeoclimatic Zone\n", y = "Percent Designated") +
#   scale_y_continuous(expand = c(0, 0), breaks = seq(0, 65, 5), limits = c(0, 65)) +
#   theme_soe_facet() +
#   theme(panel.grid.major.x = element_line(colour = "grey90"),
#         panel.grid.minor.x = element_line(colour = "grey90"),
#         panel.grid.major.y = element_blank(),
#         strip.placement = "top",
#         strip.text.x = element_text(size = 13),
#         axis.title = element_text(size = 14),
#         axis.text = element_text(size = 11),
#         plot.margin = unit(c(2,2,1,1), "lines"))
# plot(facetplot)
#
# ## print facet plot to PNG
# png(filename = "out/bgc_facet_plot.png",
#     width = 900, height = 700, units = "px")
# facetplot
# dev.off()
#
