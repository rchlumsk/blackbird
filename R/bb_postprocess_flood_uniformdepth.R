#' @title Create HAND floodmap with uniform depths in catchments
#'
#' @description
#' Create floodmap using HAND method with uniform depths in catchments
#'
#' @param hand_raster HAND raster (or file path)
#' @param uniformdepth single channel water depth (in metres)
#' @param workingfolder folder to read inputs from and write outputs to for blackbird operations
#'
#' @return \item{depth_raster}{returns raster of depth in flooded areas}
#'
#' @details
#'
#'
#' @examples
#' library(raster)
#' library(sf)
#'
#' dem <- raster(system.file("extdata", "dem_Galt_10m.tif", package="blackbird"))
#' rivershp <- sf::read_sf(system.file("extdata", "river_main_clipped.shp", package="blackbird"))
#'
#' myworkingfolder <- tempdir()
#' hand_raster <- bb_preprocess_hand(dem, rivershp, workingfolder=myworkingfolder)
#'
#' myflooddepth <- 4.3 # metres
#'
#' flood_raster <- bb_postprocess_flood_uniformdepth(hand_raster=hand_raster, uniformdepth=5.4)
#'
#'
#' @importFrom sf st_write
#' @importFrom terra lapp
#' @export bb_hand_flood_uniformdepth
bb_postprocess_flood_uniformdepth <- function(hand_raster=NULL, uniformdepth=1.0, workingfolder=NULL) {

  # check inputs
  if (is.null(hand_raster)) {stop("hand_raster is required")}
  if (!is.finite(uniformdepth) | uniformdepth<=0) {
    stop(print("Paramter uniformdepth must be a finite positive value."))
  }

  flood_raster <- terra::lapp(c(hand_raster),fun=function(r1){
    ifelse(!is.na(r1),
           ifelse(uniformdepth >= r1, uniformdepth-r1, NA)
           ,NA)})

  return(flood_raster)
}
