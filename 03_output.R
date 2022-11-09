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
library(rgdal)
library(rasterVis)
library(RColorBrewer)
library(envreportutils)
library(jpeg)
library(ggpubr)
library(sf)
library(plotly)
library(patchwork)
library(leaflet)
library(leaflet.extras)

if (!exists("cons_area_all")) load("tmp/clean.RData") #tidy data
if (!exists("ld_m")) load("tmp/raw_data_vect.RData") #load vector data
# if (!exists("forest")) load("tmp/raw_data_ras.RData") #load raster data

bbox_coords = matrix(sf::st_bbox(bc_regs), ncol=4) %>% as_tibble() %>% setNames(c("xmin","ymin","xmax","ymax"))
center = st_centroid(st_as_sfc(sf::st_bbox(bc_regs))) %>% st_coordinates()

bc_regs = bcmaps::regional_districts()

forest_img_path = 'out/forest_restriction_plot.jpeg'
forest_image = magick::image_read('out/forest_restriction_plot.jpeg')
mine_image = readJPEG('out/mine_restriction_plot.jpeg')
og_image = readJPEG('out/og_restriction_plot.jpeg')


### Just pure ggplot2. Works but is not practical for flexdashboard / crosstalk...
### Can't be converted to plotly with ggplotly...

#### a) Either annotation_custom or geom_image work.
g = ggplot() +
  # annotation_custom(grid::rasterGrob(forest_image,
  #                                    x = 0.497,
  #                                    width = 1.04,
  #                                    height = 1.6,
  #                                    interpolate = T)) +
  # background_image(forest_image) +
  ggimage::geom_image(aes(x=x,y=y,
                          image = image),
                      height = 1500,
                      width = 1000,
                      size = 1,
                      data = data.frame(x = center[1],
                                        y = center[2],
                                        image = forest_img_path)) +
  geom_sf(data = bc_regs, col = 'blue', fill = 'transparent')

### Pure plotly. Can get the regional districts but no image...
library(htmlwidgets)
library(htmltools)
fillOpacity = function(., alpha = 0.5) {
  css = sprintf("<style> .js-fill { fill-opacity: %s !important; } </style>", alpha)
  prependContent(., HTML(css))
}
plot_ly(
  bc_regs,
        line = list(
          color = 'green',
          fillColor = 'transparent'
        )
        ) %>%
  # add_trace(
  #   # type = 'polygon',
  #   ) %>%
  layout(
    images = list(
      list(
        # Add images
        # source = "out/forest_restriction_plot.jpeg",
        souce = 'https://www.nj.com/somerset/2017/02/senior_cat_is_a_kitten_at_heart.html',
        xref = "x",
        yref = "y",
        x = 0.2,
        y = 3,
        sizex = 2,
        sizey = 2,
        sizing = "stretch",
        opacity = 0.4,
        layer = "below"
      )
    )
  ) %>%
  fillOpacity(alpha = 0)

### Pure leaflet.
forest_image = data.frame(x = center[1], y = center[2],
                          image = forest_img_path)
bc_regs_t = bc_regs %>% st_transform(crs = 4326)

bbox_coords_t = matrix(sf::st_bbox(bc_regs_t), ncol=4) %>% as_tibble() %>% setNames(c("xmin","ymin","xmax","ymax"))

leaflet() %>%
  addPolygons(data = bc_regs %>% st_transform(crs = 4326)) %>%
  #leaflet::addMapPane(name = 'testpane', zIndex = 400) %>%
  htmlwidgets::onRender(jsCode = paste0("
      function(el, x) {
        console.log(this);
        var myMap = this;
        var imageUrl = 'https://www.nj.com/somerset/2017/02/senior_cat_is_a_kitten_at_heart.html';
        var imageBounds = [[", bbox_coords_t$xmin,
                               ", ",
                               bbox_coords_t$ymin,
                               "], [",
                               bbox_coords_t$xmax,
                               ", ",
                               bbox_coords_t$ymax,
                               "]];

        L.imageOverlay(imageUrl, imageBounds).addTo(myMap);
      }
      "))


# Trying with patchwork
g = ggplot(bc_regs) +
  inset_element(p = readJPEG(forest_img_path),
                left = 0.5,
                right = 0.5,
                top = 0.5,
                bottom = 0.5)

g = g + geom_sf(data = bc_regs, col = 'blue', fill = 'transparent')

print(g)

ggplotly(g)

plot_ly() %>%
plotly::add_image(forest_image) %>%
  plotly::add_choropleth(bc_regs %>% st_transform(crs = 4326))

#stacked plot forestry

cons_area_all$category <-factor(cons_area_all$category, c("None", "Managed Areas",
                                                          "Resource Exclusion Areas",
                                                          "Other Protected Lands",
                                                          "Parks & Protected Areas"))


cat.color <- c("Forestry" = "darkgreen",
               "Oil & Gas" = "darkorange3",
               "Mining" = "deeppink4")


prop_des<- ggplot(cons_area_all, aes(x=perc_bc_ha, y=restriction_type, fill=restriction_type,
                                     alpha=category)) +
  geom_bar(width = 0.9, stat="identity") +
  scale_fill_manual(values = cat.color, guide=FALSE) +
  scale_alpha_manual(values = c(
    "None"=0.1,
    "Parks & Protected Areas"=1,
    "Other Protected Lands"=0.8,
    "Resource Exclusion Areas"=0.6,
    "Managed Areas"=0.4)) +
  #scale_x_continuous(expand = c(0,0)) +
  guides(alpha = guide_legend(reverse = TRUE))+
  xlab("Proportion of Land Designations by Industry in B.C.(%)") + ylab(NULL) +
  theme_soe() +
  theme(legend.position="bottom",
        legend.title=element_blank())

plot(prop_des)

#map outputs

for_theme<- rasterTheme(region=brewer.pal(5, "Greens"))
for_ras<-levelplot(forest$forest_restriction, par.settings = for_theme)

mine_theme<- rasterTheme(region=brewer.pal(5, "Purples"))
mine_ras<-levelplot(mine$mine_restriction, par.settings = mine_theme)

og_theme<- rasterTheme(region=brewer.pal(5, "Oranges"))
og_ras<-levelplot(og$og_restriction, par.settings = og_theme)

forest.sp <- rasterToPolygons(forest)

for_ras_gg <- ggplot()+
  geom_raster(data=forest, aes(fill=attributes))+
  scale_fill_manual(values=for_theme) +
  theme(axis.title=element_blank())+
  coord_quickmap()

for_df <- as.data.frame(forest, xy=TRUE)

for_plot <- ggplot()+
  geom_raster(data=forest, aes(x=x, y=y, fill=attributes))

#save outputs

if (!exists("tmp")) dir.create("tmp", showWarnings = FALSE)
save(prop_des, mine_ras, og_ras, for_ras, for_ras_gg,
     file = "tmp/plots.RData")


if (!exists("out")) dir.create("out", showWarnings = FALSE)


svg_px("./out/prop_des.svg", width = 700, height = 400)
plot(prop_des)
dev.off()

png_retina(filename = "./out/prop_des.png", width = 700, height = 400,
           units = "px", type = "cairo-png", antialias = "default")
plot(prop_des)
dev.off()


svg_px("./out/for_ras.svg", width = 700, height = 900)
plot(for_ras)
dev.off()

png_retina(filename = "./out/for_ras.png", width = 700, height = 400,
           units = "px", type = "cairo-png", antialias = "default")
plot(for_ras)
dev.off()

svg_px("./out/mine_ras.svg", width = 700, height = 900)
plot(mine_ras)
dev.off()

png_retina(filename = "./out/mine_ras.png", width = 700, height = 400,
           units = "px", type = "cairo-png", antialias = "default")
plot(mine_ras)
dev.off()

svg_px("./out/og_ras.svg", width = 700, height = 900)
plot(og_ras)
dev.off()

png_retina(filename = "./out/og_ras.png", width = 700, height = 400,
           units = "px", type = "cairo-png", antialias = "default")
plot(og_ras)
dev.off()

