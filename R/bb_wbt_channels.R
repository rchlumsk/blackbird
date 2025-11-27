#' Generate stream network
#'
#' @param fn_flowacc File name for flow accumulation grid.
#' @param fn_flowdir File name for flow direction grid.
#' @param fn_channel_ras File name for raster version of channel network.
#' @param fn_channel_vec File name for vector version of channel networks.
#' @param threshold Threshold for channel initiation.
#' @param ... Other parameters for \pkg{whitebox} function \code{wbt_extract_streams}
#' @return Returns a \pkg{sf} vector object of the stream channels.
#'
#' @author Dan Moore
#'
#'
#' @examples
#'
#'
#' @importFrom terra rast
#' @importFrom whitebox wbt_extract_streams wbt_raster_streams_to_vector
#' @importFrom sf st_crs write_sf
#' @importFrom stats step
#' @export bb_wbt_channels
bb_wbt_channels <- function(fn_flowacc, fn_flowdir,
                            fn_channel_ras, fn_channel_vec,
                            threshold = NULL, ...) {
  bb_wbt_check_whitebox()
  exe_location <- whitebox::wbt_init()
  if (!file.exists(fn_flowacc)) {
    stop("Error: input flow accumulation file does not exist")
  }

  if (!file.exists(fn_flowdir)) {
    stop("Error: input flow direction file does not exist")
  }
  if (is.null(threshold)) {
    step("Error: threshold for channel initiation not specified")
  }

  message("bb_wbt: Generating stream network")
  whitebox::wbt_extract_streams(fn_flowacc, fn_channel_ras, threshold = threshold, ...)
  whitebox::wbt_raster_streams_to_vector(fn_channel_ras, fn_flowdir, fn_channel_vec)
  channel_vec <- st_read(fn_channel_vec)
  # if(is.na(st_crs(channel_vec))) {
    sf::st_crs(channel_vec) <- st_crs(rast(fn_channel_ras))
    write_sf(channel_vec, fn_channel_vec)
  # }
  return(channel_vec)
}
