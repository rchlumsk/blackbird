#' Snap pour points to channels
#'
#' @description Pour points describe the outlets of sub-basins within a DEM. To use
#' the pour points to delineate catchments, they must align with the drainage
#' network. This function snaps (forces the locations) of pour points to the
#' channels.
#' @param pourpoint_file file path for shapefile containing pour points.
#' @param flow_acc_file file path to flow accumulation raster.
#' @param snapped_pourpoint_file File name for snapped pour points.
#' @param snap_dist Maximum snap distance in map units.
#' @param ... Additional parameters for \pkg{whitebox} function \code{wbt_snap_pour_points}.
#'
#' @return Returns \code{TRUE} when successful.
#' @examples
#'
#'
#' @importFrom raster raster compareCRS
#' @importFrom whitebox wbt_snap_pour_points
#' @importFrom sf st_crs st_write
#' @export bb_wbt_pourpoints
bb_wbt_pourpoints <- function(pourpoint_file = NULL, flow_acc_file, snapped_pourpoint_file,
                              snap_dist = NULL, ...) {

  # pourpoint_file
  # flow_acc_file
  # snapped_pourpoint_file
  # snap_dist = pourpoint_snap_dist

  ## xxx to do - update this to only use the single pourpoint file path

  bb_wbt_check_whitebox()
  exe_location <- whitebox::wbt_init()
  if (!file.exists(flow_acc_file)) {
    stop("Error: flow accumulation file does not exist")
  }
  if (missing(pourpoint_file)) {
    stop("Error: value for pp_sf missing")
  }
  if (is.null(snap_dist)) {
    stop("Error: value for snap_dist missing")
  }
  if (!st_crs(sf::st_read(pourpoint_file)) == st_crs(terra::rast(flow_acc_file))) {
    stop("Error: pour points and flow accumulation grid have different crs")
  }
  message("bb_wbt: Snapping pour points to stream network")
  # st_write(pp_sf, fn_pp, quiet = TRUE, delete_dsn = TRUE)
  whitebox::wbt_snap_pour_points(pourpoint_file, flow_acc_file, snapped_pourpoint_file, snap_dist, ...)
  return(TRUE)
}
