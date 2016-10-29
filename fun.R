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

gg_fortify <- function(x) {
  if (!require("maptools")) stop("maptools is not installed")
  if (!requireNamespace("ggplot2")) stop("ggplot2 is not installed.")
  if (!requireNamespace("dplyr")) stop("dplyr is not installed.")
  x@data$ggid <- rownames(x@data)
  x_points <- ggplot2::fortify(x, region = "ggid")
  x_df <- dplyr::left_join(x_points, x@data, by = c("id" = "ggid"))
  x_df
}

plot_ecoreg_land_des <- function(x, ecoreg_list, poly_list) {
  ecoreg_outline <- ecoreg_list[[x]]
  plot(ecoreg_outline, main = tools::toTitleCase(tolower(ecoreg_outline$CRGNNM[1])))
  polys <- poly_list[[x]]
  if (!is.null(polys)) {
    plot(polys, col = polys$cons_cat, add = TRUE)
  }
}

gg_ld_ecoreg <- function(ecoreg_cd, ld_df, ecoreg_df) {
  ld_df_sub <- ld_df[ld_df$CRGNCD == ecoreg_cd,]
  ecoreg_df_sub <- ecoreg_df[ecoreg_df$CRGNCD == ecoreg_cd, ]
  ggplot(ld_df_sub, aes(x = long, y = lat, group = group)) +
    geom_polygon(data = ecoreg_df_sub, fill = "grey85", colour = "gray40") +
    geom_polygon(aes(fill = cons_cat)) +
    ggtitle(tools::toTitleCase(tolower(ecoreg_df_sub$CRGNNM[1]))) +
    coord_fixed() +
    theme_map()
}



#' Split a large SpatialPolygonsDataFrame into a list and apply a mapshaper function
#' to each element in Parallel
#'
#' @param spdf
#' @param column
#' @param fun
#' @param ...
#' @param recombine
#'
#' @return list of SPDFs if recombine is FALSE, a SPDF if TRUE
#' @export
#'
#' @examples
#' library(bcmaps)
#' foo <- parallel_mapshaper(ecoprovinces, "CPRVNCCD", ms_simplify, keep_shapes = TRUE, recombine = FALSE)
#' lapply(foo, plot)
#' bar <- recombine_spatial_list(foo)
parallel_apply <- function(spdf, column, fun = ms_simplify, ..., recombine = FALSE) {

  spdf_list <- split_on_attribute(spdf, column)

  ## Do the simplification in Parallel:
  no_cores <- detectCores() - 1 # Use one less than max cores so we have computing capacity to do other things
  cl <- makeCluster(no_cores)

  on.exit(stopCluster(cl))

  spdf_list_out <- parLapply(cl, spdf_list, fun, ...)

  if (!recombine) return(spdf_list_out)

  combine_spatial_list(spdf_list_out)
}

split_on_attribute <- function(spdf, column) {
  lapply(spdf[[column]], function(x) {
    spdf[spdf[[column]] == x,]
  })
}

#' Combine a list of Spatial objects into one
#'
#' @param splist a list of Spatial objects, all of the same type
#' @param ... arguments passed on to \code{link[raster]{bind}}
#'
#' @return a Spatial object of the same class as the inputs
#' @export
combine_spatial_list <- function(splist, ...) {
  ## Recombine list elements into one sp object
  do.call(raster::bind, splist, ...)
}
