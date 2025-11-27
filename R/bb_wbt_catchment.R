#' Delineate catchment boundaries
#'
#' @param fn_pp_snap Name of file containing snapped pour points
#' @param fn_flowdir Name of file containing flow accumulations.
#' @param fn_catchment_ras Raster file to contain delineated catchment.
#' @param fn_catchment_vec Vector file to contain delineated catchment.
#' @param return_vector If \code{TRUE} (the default) a vector of the catchment will be returned.
#'
#' @return If \code{return_vector == TRUE} a vector of the catchment is returned. Otherwise
#' nothing is returned.
#'
#' @examples
#' # xxx to do
#'
#'
#' @importFrom terra rast
#' @importFrom whitebox wbt_watershed wbt_raster_to_vector_polygons wbt_unnest_basins
#' @importFrom sf st_crs write_sf st_read
#' @importFrom magrittr %>%
#' @export bb_wbt_catchment
bb_wbt_catchment <- function(fn_pp_snap, fn_flowdir, fn_catchment_ras,
                             fn_catchment_vec, return_vector = FALSE) {

  # fn_pp_snap=snapped_streamnodes_file
  # fn_flowdir = flow_dir_file
  # fn_catchment_ras, fn_catchment_vec

  bb_wbt_check_whitebox()
  exe_location <- whitebox::wbt_init()
  if (!file.exists(fn_pp_snap)) {
    stop("Error: file containing snapped pour points does not exist")
  }
  if (!file.exists(fn_flowdir)) {
    stop("Error: input flow direction file does not exist")
  }

  message("bb_wbt: Delineating catchment boundaries")
  if (st_crs(st_read(fn_pp_snap)) !=  st_crs(rast(fn_flowdir))) {
    stop("Error: pour points and flow direction grid have different crs")
  }
  whitebox::wbt_watershed(d8_pntr = fn_flowdir, pour_pts = fn_pp_snap,
                          output = fn_catchment_ras)
  whitebox::wbt_raster_to_vector_polygons(fn_catchment_ras, fn_catchment_vec)
  # rewrite raster with exact values (avoid fuzziness in catchment raster IDs)
  # catchment_ras <- raster(fn_catchment_ras)
  # whitebox::wbt_vector_polygons_to_raster(fn_catchment_vec,fn_catchment_ras,field = "VALUE",base=fn_catchment_ras)
  catchment_vec <- st_read(fn_catchment_vec) %>% st_as_sf()
  if (is.na(st_crs(catchment_vec))) {
    sf::st_crs(catchment_vec) <- st_crs(rast(fn_catchment_ras))
    write_sf(catchment_vec, fn_catchment_vec)
  }
  if (return_vector) {
    return(st_read(fn_catchment_vec))
  } else {
    return(TRUE)
  }
}
