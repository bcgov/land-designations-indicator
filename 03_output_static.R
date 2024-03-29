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

if (!exists("cons_area_all")) load("tmp/clean.RData")
if (!exists("ld_m")) load("tmp/raw_data_vect.RData")


#stacked plot forestry

cons_area_all$plot_cat <-factor(cons_area_all$Category, c("None", "Managed Areas",
                                                          "Resource Exclusion Areas",
                                                          "Other Protected Lands",
                                                          "Parks & Protected Areas"))

cons.order <- unique(cons_area_all$plot_cat)
cons.cols<-5
cons.pal <- rev(brewer.pal(cons.cols, "Purples"))
names(cons.pal) <- cons.order

prop_des<- ggplot(cons_area_all, aes(fill=plot_cat, x=perc_bc_ha, y=restriction_type,
                                     order=plot_cat)) +
  geom_bar(position="fill", stat="identity") +
  scale_fill_manual(values = cons.pal) +
  guides(fill = guide_legend(reverse = TRUE))+
  xlab("Proportion of Land Designations by Industry in B.C.") + ylab(NULL) +
  theme_soe() +
  theme(legend.position="bottom",
        legend.title=element_blank())

plot(prop_des)


plot(ld_m)
ld_f_map <- ggplot(ld_m, (aes(group=mine_restriction)))
plot(ld_f_map)
#save outputs

if (!exists("out")) dir.create("out", showWarnings = FALSE)


svg_px("./out/prop_des.svg", width = 700, height = 400)
plot(prop_des)
dev.off()

png_retina(filename = "./out/prop_des.png", width = 700, height = 400,
           units = "px", type = "cairo-png", antialias = "default")
plot(prop_des)
dev.off()





