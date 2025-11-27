#' @title Remove internal holes from polygons
#'
#' @description Removes internal holes from polygon vectors
#'
#' @param pourpoint_file file path for shapefile containing pour points.
#' @param flow_acc_file file path to flow accumulation raster.
#' @param snapped_pourpoint_file File name for snapped pour points.
#' @param snap_dist Maximum snap distance in map units.
#'
#' @return Returns \code{TRUE} when successful.
#' @examples
#'
#'
#' @importFrom raster raster compareCRS
#' @importFrom whitebox wbt_snap_pour_points
#' @importFrom sf st_crs st_write
#' @export bb_wbt_removepolyholes
bb_wbt_removepolyholes <- function(inputvector = NULL, outputvector=NULL, returnvector=FALSE) {

  bb_wbt_check_whitebox()
  exe_location <- whitebox::wbt_init()
  if (missing(inputvector)) {
    stop("Error: value for inputvector is missing")
  }

  if (is.null(outputvector) & !returnvector) {
    stop("Must provide output path or set returnvector=TRUE")
  }

  if (is.character(inputvector)) {
    if (!file.exists(inputvector)) {
      stop("Error: input vector file does not exist")
    }
  } else {
    if ("sf" %in% class(inputpolygon) ) {
      tf <- tempfile(fileext = ".shp")
      inputpolygon <- write_sf(obj=inputpolygon, dsn=tf)
      inputpolygon <- tf
    } else if ("SpatVector" %in% class(inputpolygon) ) {
      tf <- tempfile(fileext = ".shp")
      inputpolygon <- writeVector(inputpolygon, tf)
      inputpolygon <- tf
    } else {
      stop("inputpolygon must be of class sf/SpatVector or a shapefile file path")
    }
  }

  if (is.null(outputvector)) {
    outputvector <- tempfile(fileext = ".shp")
  }

  message("bb_wbt: Filling internal polygon holes")
  # st_write(pp_sf, fn_pp, quiet = TRUE, delete_dsn = TRUE)
  whitebox::wbt_remove_polygon_holes(inputvector, outputvector)

  if (returnvector) {
    vv <- sf::read_sf(outputvector)
    return(vv)
  } else {
    return(TRUE)
  }
}
