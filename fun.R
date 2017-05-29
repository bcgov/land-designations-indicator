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
#' to each element (optionally in parallel)
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
#' foo <- mapshaper_apply(ecoprovinces, "CPRVNCCD", ms_simplify, keep_shapes = TRUE, recombine = FALSE)
#' lapply(foo, plot)
#' bar <- recombine_spatial_list(foo)
mapshaper_apply <- function(spdf, column, fun = ms_simplify, ..., parallel = FALSE, recombine = TRUE) {
  if (parallel && !require("parallel")) stop("library 'parallel' not available")

  spdf_list <- split_on_attribute(spdf, column)

  if (parallel) {
    ## Do the simplification in Parallel:
    no_cores <- detectCores() - 1 # Use one less than max cores so we have computing capacity to do other things
    cl <- makeCluster(no_cores)

    on.exit(stopCluster(cl))

    clusterEvalQ(cl, library(rmapshaper))

    spdf_list_out <- parLapply(cl, spdf_list, fun, ...)
  } else {
    spdf_list_out <- lapply(spdf_list, fun, ...)
  }

  if (!recombine) return(spdf_list_out)

  combine_spatial_list(spdf_list_out)
}

mapshaper_apply_sf <- function(sf, column, fun = ms_simplify, ..., parallel = FALSE, recombine = TRUE) {
    if (parallel && !require("parallel")) stop("library 'parallel' not available")

    sf_list <- split_on_attribute(sf, column)
    gj_list <- lapply(sf_list, geojsonio::geojson_json)

    if (parallel) {
      ## Do the simplification in Parallel:
      no_cores <- parallel::detectCores() - 1 # Use one less than max cores so we have computing capacity to do other things
      cl <- parallel::makeCluster(no_cores)

      on.exit(parallel::stopCluster(cl))

      parallel::clusterEvalQ(cl, library(rmapshaper))

      geojson_list_out <- parallel::parLapply(cl, gj_list, fun, ...)
    } else {
      geojson_list_out <- lapply(gj_list, fun, ...)
    }

    sf_list_out <- lapply(geojson_list_out, sf::read_sf)

    if (!recombine) return(sf_list_out)

    dplyr::bind_rows(sf_list_out)
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

#' Clip one layer on a boundary layer, ensuring that only the features that overlap
#' are actually clipped to save processing time
#'
#' @param x the target layer
#' @param bc the boundary layer
#'
#' @return A spatial object of the same time as x
#' @export
#'
#' @examples
clip_only <- function(x, bc) {
  UseMethod("clip_only")
}

clip_only.Spatial <- function(x, bc) {
  ## First check if there are any that are completely outside and ditch if so:
  intersects <- rgeos::gIntersects(x, bc, byid = TRUE, returnDense = TRUE)
  intersects <- as.logical(colSums(intersects))
  x <- x[intersects, ]

  # Find the features in x that are not fully covered by the boundary
  covers <- rgeos::gCoveredBy(bc, x, byid = TRUE, returnDense = TRUE)
  covers <- as.logical(colSums(covers))
  clipped <- rmapshaper::ms_clip(x[covers, ], bc)
  rbind(x[!covers, ], clipped[, names(x)])
}

#' @importFrom sf st_intersects st_covered_by
clip_only.sf <- function(x, bc) {
  ## First check if there are any that are completely outside and ditch if so:
  intersects <- sf::st_intersects(x, bc, sparse = FALSE)
  intersects <- as.logical(rowSums(intersects))
  x <- x[intersects, ]

  ## Find the features in x that are not fully covered by the boundary
  ## and clip them
  covers <- sf::st_covered_by(bc, x, sparse = FALSE)
  covers <- as.logical(colSums(covers))
  clipped <- rmapshaper::ms_clip(geojsonio::geojson_json(x[covers, ]),
                                 geojsonio::geojson_json(bc))
  clipped <- sf::read_sf(clipped)

  ## Set the crs of the clipped as it is lost when going through geojson
  suppressWarnings(sf::st_crs(clipped) <- sf::st_crs(x)[[2]])

  ## Make sure the clipped layer and the non-clipped layer have the
  ## same geometry column
  names(clipped)[names(clipped) == attr(clipped, "sf_column")] <- attr(x, "sf_column")
  attr(clipped, "sf_column") <- attr(x, "sf_column")

  rbind(x[!covers, ], clipped[, setdiff(names(clipped), "rmapshaperid")])
}

get_latest_release <- function(repo_path) {
  if (!require(httr)) stop("You need the 'httr' package to use this function", call. = FALSE)

  repo_path <- gsub("^/+|/+$", "", repo_path)

  ret <- GET(paste0("https://api.github.com/repos/", repo_path, "/releases/latest"))
  stop_for_status(ret)
  content(ret)
}

list_release_assets <- function(assets_url) {
  if (!require(httr)) stop("You need the 'httr' package to use this function", call. = FALSE)

  ret <- GET(assets_url)
  stop_for_status(ret)
  content(ret)
}

get_asset <- function(asset, overwrite = TRUE) {
  if (!require(httr)) stop("You need the 'httr' package to use this function", call. = FALSE)

  ret <- GET(asset$url, write_disk(paste0("data/",asset$name), overwrite = overwrite), progress())
  stop_for_status(ret)
  ret
}

download_assets <- function(assets, dest_dir = "data", overwrite = TRUE) {
  ret <- lapply(assets, get_asset, overwrite = overwrite)

  if (length(ret) == 1L) return(ret[[1]])
  ret
}

list_asset_files <- function(assets) {
  vapply(assets, function(x) x$content, FUN.VALUE = character(1))
}

get_data <- function(repo = "bcgov/land-designations-indicator", dest_dir = "data", overwrite = TRUE) {
  the_release <- get_latest_release(repo_path = repo)
  the_assets <- list_release_assets(the_release$assets_url)
  asset_files <- download_assets(the_assets, overwrite = overwrite)
  list_asset_files(asset_files)
}
