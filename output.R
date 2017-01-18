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

files_list <- list.files("out-shiny", full.names = TRUE)
file.copy(from = files_list, to = "../land-designations-shinyapp/app/data", overwrite = TRUE)

ggplot(gg_ld_bec, aes(x = long, y = lat, group = group)) +
  geom_polypath(aes(fill = category)) +
  coord_fixed()

ggplot(gg_ld_ecoreg, aes(x = long, y = lat, group = group)) +
  geom_polypath(aes(fill = category)) +
  coord_fixed()
