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

library(tidyverse)
library(sf)
library(raster)
library(RColorBrewer)
library(envreportutils)

rm(list = ls())

#Double check that a directory called 'out' exists - if not, create it.
if (!exists("out")) dir.create("out", showWarnings = FALSE)
if (!exists("land_designations_webapp/www")) dir.create("land_designations_webapp/www", showWarnings = FALSE)
if (!exists("cons_area_all")) load("tmp/clean.RData")
if (!exists("ld_with_reg")) load("tmp/ld_with_reg.RData")
if (!exists('bc_regs_small')) bc_regs = read_sf('tmp/bc_regdists.gpkg')
# if (!exists("ld")) load("tmp/raw_data_vect.RData")

# bc_regs = bcmaps::regional_districts()
# bc_regs_small = st_simplify(bc_regs, dTolerance = 2500)
# write_sf(bc_regs_small, 'tmp/bc_regdists.gpkg')

#Grey colour
my_grey = '#C5C5C5'
# bc_regs = bcmaps::regional_districts()
bc_regdists_area = bc_regs %>%
  mutate(regdist_area = as.numeric(st_area(.))) %>%
  st_drop_geometry() %>%
  dplyr::select(ADMIN_AREA_NAME, regdist_area) %>%
  distinct()

bc_boundary = bcmaps::bc_bound()
bc_total_surface_area = as.numeric(st_area(bc_boundary %>% summarise()))

#Stacked plot of proportion of BC area for each restriction category and level.

cons_area_all = cons_area_all %>%
  mutate(plot_cat = paste0(category," (",r_level,")")) %>%
  mutate(plot_cat = replace(plot_cat, plot_cat == "NA (NA)", NA)) %>%
  mutate(plot_cat = factor(plot_cat, levels = c("Parks & Protected Areas (Full)",
                                                "Other Protected Lands (High)",
                                                "Resource Exclusion Areas (Medium)",
                                                "Managed Areas (Low)",
                                                "Other (Very Low)",
                                                NA)))

# cons_area_all = cons_area_all %>%
#   mutate(r_level = factor(r_level, levels = c(NA,"Very Low","Low","Medium","High","Full")))

#Get a vector that organizes the 3 industries in terms of high-to-low area as % of all BC.
industry_ordering_vec = cons_area_all %>%
  mutate(max_rest_ind_type = str_to_upper(str_replace_all(max_rest_ind_type,"_"," "))) %>%
  filter(restriction != 0) %>%
  group_by(max_rest_ind_type) %>%
  summarise(total_area = sum(perc_bc_ha)) %>%
  arrange(desc(total_area)) %>%
  pull(max_rest_ind_type)

#Industry-specific barplots of proportion of BC area by restriction type.
forestry_barplot = cons_area_all %>%
  filter(max_rest_ind_type == "forest_restriction_max") %>%
  mutate(max_rest_ind_type = str_to_upper(str_replace_all(max_rest_ind_type,"_"," "))) %>%
  # mutate(max_rest_ind_type = factor(max_rest_ind_type, levels = industry_ordering_vec)) %>%
  ggplot(aes(x=as.numeric(perc_bc_ha),
             y= plot_cat,fill=plot_cat)) +
  geom_col(col='black') +
  scale_fill_brewer(palette = 'Greens', na.value = my_grey, direction = -1) +
  guides(fill = guide_legend(reverse = T))+
  xlab("Proportion of Land Designations by Industry in B.C.") + ylab(NULL) +
  theme_soe() +
  theme(legend.position="bottom",
        legend.title=element_blank(),
        axis.text.y = element_blank())

mining_barplot = cons_area_all %>%
  filter(max_rest_ind_type == "mine_restriction_max") %>%
  mutate(max_rest_ind_type = str_to_upper(str_replace_all(max_rest_ind_type,"_"," "))) %>%
  # mutate(max_rest_ind_type = factor(max_rest_ind_type, levels = industry_ordering_vec)) %>%
  ggplot(aes(x=as.numeric(perc_bc_ha),
             y= plot_cat,fill=plot_cat)) +
  geom_col(col='black') +
  scale_fill_brewer(palette = 'Purples', na.value = my_grey, direction = -1) +
  guides(fill = guide_legend(reverse = T))+
  xlab("Proportion of Land Designations by Industry in B.C.") + ylab(NULL) +
  theme_soe() +
  theme(legend.position="bottom",
        legend.title=element_blank(),
        axis.text.y = element_blank())

og_barplot = cons_area_all %>%
  filter(max_rest_ind_type == "og_restriction_max") %>%
  mutate(max_rest_ind_type = str_to_upper(str_replace_all(max_rest_ind_type,"_"," "))) %>%
  # mutate(max_rest_ind_type = factor(max_rest_ind_type, levels = industry_ordering_vec)) %>%
  ggplot(aes(x=as.numeric(perc_bc_ha),
             y= plot_cat,fill=plot_cat)) +
  geom_col(col='black') +
  scale_fill_brewer(palette = 'Oranges', na.value = my_grey, direction = -1) +
  guides(fill = guide_legend(reverse = T))+
  xlab("Proportion of Land Designations by Industry in B.C.") + ylab(NULL) +
  theme_soe() +
  theme(legend.position="bottom",
        legend.title=element_blank(),
        axis.text.y = element_blank())

ggsave('out/forestry_barplot.jpeg',forestry_barplot, width = 6, height = 4)
ggsave('out/mining_barplot.jpeg',mining_barplot, width = 6, height = 4)
ggsave('out/og_barplot.jpeg',og_barplot, width = 6, height = 4)

# prop_des = cons_area_all %>%
#   mutate(max_rest_ind_type = str_to_upper(str_replace_all(max_rest_ind_type,"_"," "))) %>%
#   mutate(max_rest_ind_type = factor(max_rest_ind_type, levels = industry_ordering_vec)) %>%
#   # mutate(palette_color = case_when(
#   #   str_detect(max_rest_ind_type, "FOREST") ~ "Greens",
#   #   str_detect(max_rest_ind_type, "MINE") ~ "Blues",
#   #   str_detect(max_rest_ind_type, "OG ") ~ "Oranges"
#   # )) %>%
#   ggplot(aes(x=as.numeric(perc_bc_ha),
#              y= max_rest_ind_type,fill=plot_cat)) +
#   geom_bar(position="fill", stat="identity",col='black') +
#   scale_fill_brewer(aes(palette = palette_color), na.value = my_grey, direction = -1) +
#   guides(fill = guide_legend(reverse = T))+
#   xlab("Proportion of Land Designations by Industry in B.C.") + ylab(NULL) +
#   theme_soe() +
#   theme(legend.position="bottom",
#         legend.title=element_blank())
#
# prop_des

# ggsave('out/prop_des.jpeg', prop_des, width = 7, height = 4, units = 'in')

# Static ggplot maps for each industry type.
forest_plot = ld_with_reg %>%
  filter(forest_restriction_max != 0) %>%
  mutate(forest_restriction_max = as.factor(forest_restriction_max)) %>%
  ggplot() +
  geom_sf(data = bc_boundary, fill = 'grey', color = 'transparent') +
  geom_sf(aes(fill = forest_restriction_max),
          col = 'transparent') +
  # geom_sf(data = bc_regs, fill = 'transparent') +
  scale_fill_brewer(palette = 'Greens') +
  theme_minimal() +
  theme(legend.position = 'none',
        panel.grid = element_blank(),
        axis.text = element_blank())

ggsave('out/forest_restriction_plot.jpeg',forest_plot, width = 6, height = 8, units = 'in')
magick::image_read('out/forest_restriction_plot.jpeg') %>%
  image_scale('x1000') %>%
  image_crop('700x600+45+200') %>%
  image_write('out/forest_restriction_plot.jpeg')

mine_plot = ld_with_reg %>%
  filter(mine_restriction_max != 0) %>%
  mutate(mine_restriction_max = as.factor(mine_restriction_max)) %>%
  ggplot() +
  geom_sf(data = bc_boundary, fill = 'grey', color = 'transparent') +
  geom_sf(aes(fill = mine_restriction_max),
          col = 'transparent') +
  # geom_sf(data = bc_regs, fill = 'transparent') +
  scale_fill_brewer(palette = 'Purples') +
  theme_minimal() +
  theme(legend.position = 'none',
        panel.grid = element_blank(),
        axis.text = element_blank())

ggsave('out/mine_restriction_plot.jpeg',mine_plot, width = 6, height = 8, units = 'in')
magick::image_read('out/mine_restriction_plot.jpeg') %>%
  image_scale('x1000') %>%
  image_crop('700x600+45+200') %>%
  image_write('out/mine_restriction_plot.jpeg')

og_plot = ld_with_reg %>%
  filter(og_restriction_max != 0) %>%
  mutate(og_restriction_max = as.factor(og_restriction_max)) %>%
  ggplot() +
  geom_sf(data = bc_boundary, fill = 'grey', color = 'transparent') +
  geom_sf(aes(fill = og_restriction_max),
          col = 'transparent') +
  # geom_sf(data = bc_regs, fill = 'transparent') +
  scale_fill_brewer(palette = 'Oranges') +
  theme_minimal() +
  theme(legend.position = 'none',
        panel.grid = element_blank(),
        axis.text = element_blank())

ggsave('out/og_restriction_plot.jpeg',og_plot, width = 6, height = 8, units = 'in')
magick::image_read('out/og_restriction_plot.jpeg') %>%
  image_scale('x1000') %>%
  image_crop('700x600+45+200') %>%
  image_write('out/og_restriction_plot.jpeg')

# Copy the provincial-scale JPEGs (n=6) we just made to the R Shiny www/ folder.
list.files(path = 'out/', pattern = "[^des]\\.jpeg") %>%
  map( ~ {
    file.copy(from = paste0('out/',.x),to = paste0('land_designations_webapp/www/',.x))
  })

# Summarise data for each industry type, regional district, and restriction level.
area_by_regdist_and_industry_and_restriction_value = ld_with_reg %>%
  mutate(area = as.numeric(st_area(.))) %>%
  st_drop_geometry() %>%
  pivot_longer(cols = c("forest_restriction_max","mine_restriction_max","og_restriction_max"),
               names_to = 'industry_name',
               values_to = 'max_restriction_value') %>%
  group_by(industry_name, ADMIN_AREA_NAME, max_restriction_value) %>%
  summarise(area = sum(area, na.rm=T),
            .groups = 'drop') %>%
  left_join(bc_regdists_area) %>%
  mutate(prop_regdist_area = area/regdist_area) %>%
  ungroup()

area_by_regdist_and_industry_and_restriction_value

write.csv(area_by_regdist_and_industry_and_restriction_value,'land_designations_webapp/www/ld_choro.csv',
          row.names = F)

# For each industry and regional district, make a high-resolution map piece.
for(industry in c("forest_restriction_max","mine_restriction_max","og_restriction_max")){
  for(region_name in c(unique(bc_regs$ADMIN_AREA_NAME),NA)){

    print(paste0("Working on: ",industry, " - ",region_name))

    the_region = bc_regs %>% filter(ADMIN_AREA_NAME %in% region_name)

    choro_dat = ld_with_reg %>%
      pivot_longer(cols = c("forest_restriction_max","mine_restriction_max","og_restriction_max"),
                   names_to = 'industry_name',
                   values_to = 'max_restriction_value') %>%
      filter(industry_name == industry) %>%
      filter(ADMIN_AREA_NAME == region_name) %>%
      filter(max_restriction_value != 0) %>%
      mutate(max_restriction_value = as.factor(max_restriction_value))

    g = ggplot() +
      geom_sf(data = the_region, col = 'black', fill = 'transparent') +
      geom_sf(aes(fill = max_restriction_value),
              col = 'transparent',
              data = choro_dat) +
      theme_minimal() +
      theme(legend.position = 'none',
            panel.grid = element_blank(),
            axis.text = element_blank())
    if(industry == "forest_restriction_max"){
      g = g + scale_fill_brewer(palette = 'Greens')
    } else if(industry == "mine_restriction_max"){
      g = g + scale_fill_brewer(palette = 'Purples')
    } else if(industry == "og_restriction_max"){
      g = g + scale_fill_brewer(palette = 'Oranges')
    }

    ggsave(filename = paste0('tmp/regdist_figs/',industry,"_",region_name,".jpeg"),
           plot = g, width = 6, height = 4, dpi = 150)
    file.copy(from = paste0('tmp/regdist_figs/',industry,"_",region_name,".jpeg"),
              to = paste0('land_designations_webapp/www/',industry,"_",region_name,".jpeg"))
  }
}
