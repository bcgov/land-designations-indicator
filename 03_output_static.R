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

# Load in packages to be used in this script.
library(tidyverse)
library(sf)
library(raster)
library(RColorBrewer)
library(envreportutils)
library(bcmaps)
library(magick)

# Remove objects in the environment - metaphorically cleaning the top of your desk before working.
rm(list = ls())

#Double check that directories 'out' and 'land_designations_webapp/www' exists - if not, create them.
if (!exists("out")) dir.create("out", showWarnings = FALSE)
if (!exists("land_designations_webapp/www")) dir.create("land_designations_webapp/www", showWarnings = FALSE)
if (!exists("cons_area_all")) load("tmp/clean.RData") # Load in data
if (!exists("ld_with_reg")) load("tmp/ld_with_reg.RData") #Load in data

#If your local machine does not yet have a simplified version of the natural resource districts, do the following:
if(!file.exists('tmp/bc_regdists.gpkg')){

  #This code doesn't run to completion if you run it from the 'if' on line 26... strange.
  bc_regs = nr_districts()

  bc_regs_small = st_simplify(bc_regs, dTolerance = 2500)

  write_sf(bc_regs_small, 'tmp/bc_regdists.gpkg')
  write_sf(bc_regs_small, 'land_designations_webapp_3col/www/bc_regdists.gpkg')

  rm(bc_regs_small); rm(bc_regs)
}

#Conversely, if your machine already has these districts as a geopackage, load them in.
if (!exists('bc_regs')) bc_regs = read_sf('tmp/bc_regdists.gpkg')
# if (!exists("ld")) load("tmp/raw_data_vect.RData")


### Setting or calculating values to be used in the rest of the code.

# Grey colour
my_grey = '#C5C5C5'

# Area of each of the natural resource districts.
bc_regdists_area = bc_regs %>%
  mutate(regdist_area = as.numeric(st_area(.))) %>%
  st_drop_geometry() %>%
  dplyr::select(DISTRICT_NAME, regdist_area) %>%
  distinct()

# Download a map of BC using the {bcmaps} package.
bc_boundary = bc_bound()

# Optional: Combine restriction levels 'Full' and 'Protected' into one level.
mapping_data = ld_with_reg %>%
  # filter(DISTRICT_NAME == "South Island Natural Resource District") %>%
  mutate(across(ends_with('_max'), ~ case_when(
    .x == 0 ~ "None",
    .x == 1 ~ "Low",
    .x == 2 ~ "Medium",
    .x == 3 ~ "High",
    .x == 4 ~ "Full",
    .x == 5 ~ "Full"
  ))) %>%
  ungroup() %>%
  mutate(across(ends_with('_max'), ~ replace(.x, .x == "None", NA))) %>%
  mutate(across(ends_with('_max'), ~ factor(.x,
                                        levels = c("Full","High","Medium",
                                                   "Low","None"))))

# Static ggplot maps at provincial scale for each industry type.
# We use the {magick} package to rescale and crop the ggplot images.
forest_plot = mapping_data %>%
  ggplot() +
  geom_sf(data = bc_boundary, fill = 'transparent', color = 'black') +
  geom_sf(aes(fill = forest_restriction_max),
          col = 'transparent') +
  scale_fill_brewer(palette = 'Greens', direction = -1, na.value = 'white') +
  theme_minimal() +
  theme(legend.position = 'none',
        panel.grid = element_blank(),
        axis.text = element_blank())

ggsave('out/forest_restriction_plot.jpeg',forest_plot, width = 6, height = 8, units = 'in')
image_read('out/forest_restriction_plot.jpeg') %>%
  image_scale('x1000') %>%
  image_crop('700x600+45+200') %>%
  image_write('out/forest_restriction_plot.jpeg')

mine_plot = mapping_data %>%
  ggplot() +
  geom_sf(data = bc_boundary, fill = 'transparent', color = 'black') +
  geom_sf(aes(fill = mine_restriction_max),
          col = 'transparent') +
  scale_fill_brewer(palette = 'Purples', direction = -1, na.value = 'white') +
  theme_minimal() +
  theme(legend.position = 'none',
        panel.grid = element_blank(),
        axis.text = element_blank())

ggsave('out/mine_restriction_plot.jpeg',mine_plot, width = 6, height = 8, units = 'in')

image_read('out/mine_restriction_plot.jpeg') %>%
  image_scale('x1000') %>%
  image_crop('700x600+45+200') %>%
  image_write('out/mine_restriction_plot.jpeg')

og_plot = mapping_data %>%
  ggplot() +
  geom_sf(data = bc_boundary, fill = 'transparent', color = 'black') +
  geom_sf(aes(fill = og_restriction_max),
          col = 'transparent') +
  scale_fill_brewer(palette = 'Oranges', direction = -1, na.value = 'white') +
  theme_minimal() +
  theme(legend.position = 'none',
        panel.grid = element_blank(),
        axis.text = element_blank())

ggsave('out/og_restriction_plot.jpeg',og_plot, width = 6, height = 8, units = 'in')

image_read('out/og_restriction_plot.jpeg') %>%
  image_scale('x1000') %>%
  image_crop('700x600+45+200') %>%
  image_write('out/og_restriction_plot.jpeg')

### Copy the provincial-scale JPEGs (n=3) we just made to the R Shiny www/ folder.

# First, delete any old versions of these jpegs that we have in that folder...
file.remove(list.files(path = 'land_designations_webapp_3col/www/',
                       pattern = '_plot\\.jpeg',full.names = T))

list.files(path = 'out/', pattern = "[^des]\\.jpeg") %>%
  map( ~ {
    file.copy(from = paste0('out/',.x),to = paste0('land_designations_webapp_3col/www/',.x))
  })

# Summarise total area by industry type, natural resource district, and restriction level.
area_by_regdist_and_industry_and_restriction_value = ld_with_reg %>%
  mutate(area = as.numeric(st_area(.))) %>%
  st_drop_geometry() %>%
  pivot_longer(cols = c("forest_restriction_max","mine_restriction_max","og_restriction_max"),
               names_to = 'industry_name',
               values_to = 'max_restriction_value') %>%
  group_by(industry_name, DISTRICT_NAME, max_restriction_value) %>%
  summarise(area = sum(area, na.rm=T)) %>%
  group_by(industry_name,DISTRICT_NAME) %>%
  mutate(total_district_area = sum(area,na.rm=T)) %>%
  mutate(proportional_area = area/total_district_area) %>%
  ungroup()

# Add 3 rows - one for each industry at the scale of the entire province
bc_reg_dat = area_by_regdist_and_industry_and_restriction_value %>%
  bind_rows(
    area_by_regdist_and_industry_and_restriction_value %>%
      group_by(industry_name, max_restriction_value) %>%
      summarise(area = sum(area,na.rm=T)) %>%
      group_by(industry_name) %>%
      mutate(bc_area = sum(area)) %>%
      ungroup() %>%
      mutate(proportional_area = area/bc_area) %>%
      mutate(DISTRICT_NAME = "Provincial")
  ) %>%
  #Rename restriction levels from numeric levels to character labels.
  mutate(max_restriction_value = case_when(
    max_restriction_value == 0 ~ "None",
        max_restriction_value == 1 ~ "Low",
        max_restriction_value == 2 ~ "Medium",
        max_restriction_value == 3 ~ "High",
        max_restriction_value == 4 ~ "Full",
        max_restriction_value == 5 ~ "Protected"
  )) %>%
  #Optional: collapse categories 4 and 5 into one single category called "Full Restriction"
    mutate(max_restriction_value = replace(max_restriction_value, max_restriction_value == "Protected", "Full")) %>%
    group_by(industry_name,max_restriction_value,DISTRICT_NAME) %>%
  summarise(proportional_area = sum(proportional_area,na.rm=T)) %>%
  ungroup() %>%
  mutate(max_restriction_value = factor(max_restriction_value,
                                        levels = c("Full","High","Medium",
                                                   "Low","None")))

# Write these summary data to the shiny app's /www folder.
write.csv(bc_reg_dat,'land_designations_webapp_3col/www/bc_reg_dat.csv',
          row.names = F)

# Calculate the number of designations per district.
num_des_per_reg = ld_with_reg %>%
  st_drop_geometry() %>%
  filter(!duplicated(designations_planarized_id)) %>%
  count(DISTRICT_NAME, name = 'number_designations')

write.csv(num_des_per_reg, 'land_designations_webapp_3col/www/number_designations_per_district.csv', row.names = F)


# # #  JPEGs for each Natural Resource district
# Clear out images currently in tmp/regdist_figs, tmp/regdist_svgs, and
# land_designations_webapp_3col/www/regdist_figs

file.remove(list.files(path = './tmp/regdist_figs/',full.names = T))
file.remove(list.files(path = './tmp/regdist_svgs/',full.names = T))
file.remove(list.files(path = './land_designations_webapp_3col/www/regdist_figs/',full.names = T))

# For each industry and district, make a high-resolution map piece.
for(industry in c("forest_restriction_max","mine_restriction_max","og_restriction_max")){
  for(district_name in unique(bc_regs$DISTRICT_NAME)){

    print(paste0("Working on: ",industry, " - ",district_name))

    the_district = bc_regs %>% filter(DISTRICT_NAME %in% district_name)

    district_dat = ld_with_reg %>%
      pivot_longer(cols = c("forest_restriction_max","mine_restriction_max","og_restriction_max"),
                   names_to = 'industry_name',
                   values_to = 'max_restriction_value') %>%
      filter(industry_name == industry) %>%
      filter(DISTRICT_NAME == district_name) %>%
      filter(max_restriction_value != 0) %>%
      mutate(max_restriction_value = as.factor(max_restriction_value))

    #Trim edges using the district polygon.
    district_dat = sf::st_intersection(district_dat, the_district)

    g = ggplot() +
      geom_sf(aes(fill = max_restriction_value),
              col = 'transparent',
              data = district_dat) +
      geom_sf(data = the_district, col = 'black', fill = 'transparent') +
      ggspatial::annotation_scale() +
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

    ggsave(filename = paste0('tmp/regdist_figs/',industry,"_",district_name,".jpeg"),
           plot = g, width = 4, height = 4, dpi = 200)
    file.copy(from = paste0('tmp/regdist_figs/',industry,"_",district_name,".jpeg"),
              to = paste0('land_designations_webapp_3col/www/regdist_figs/',industry,"_",district_name,".jpeg"))
  }
}

library(magick)
list.files(path = 'tmp/regdist_figs/') %>%
  map( ~ {
    image_read(paste0('tmp/regdist_figs/',.x)) %>%
      image_scale('x350') %>%
      image_convert(format = 'svg') %>%
      image_write(paste0('tmp/regdist_svgs/',str_replace(.x,"\\jpeg","svg")))
  })
