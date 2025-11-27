#' Delineate catchment boundaries
#'
#' @param inputraster File path for a raster to clip
#' @param inputpolygon File path for a polygon to use as mask in clipping
#' @param outputfile Output file to write the resulting clipped raster to
#' @param maintain_dimensions whether to maintain the dimensions of the original raster, passed to \code{wbt_clip_raster_to_polygon}
#' @param compress_rasters whether to compress the resulting raster, passed to \code{wbt_clip_raster_to_polygon}
#' @param return_raster If \code{TRUE} (the default) a raster object will be returned, otherwise a file path to created raster is returned.
#'
#' @return If \code{return_raster == TRUE} a clipped raster object is returned. Otherwise, \code{TRUE}.
#'
#' @examples
#' print("TO DO")
#'
#'
#' @importFrom raster compareCRS
#' @importFrom terra rast writeVector crs writeRaster vect
#' @importFrom whitebox wbt_clip_raster_to_polygon
#' @importFrom sf write_sf st_read write_sf
#'
#' @export bb_wbt_clip_raster_to_polygon
bb_wbt_clip_raster_to_polygon <- function(inputraster=NULL, inputpolygon=NULL, outputfile=NULL,
                             maintain_dimensions=FALSE, compress_rasters=FALSE,
                             return_raster=FALSE) {

  stopifnot(!is.null(inputraster),!is.null(inputpolygon))

  bb_wbt_check_whitebox()
  exe_location <- whitebox::wbt_init()
  exe_location <- wbt_exe_path(shell_quote = FALSE)
  if (is.character(inputraster)) {
    if (!file.exists(inputraster)) {
      stop("Error: file containing inputraster does not exist")
    }
  } else {

    if ("SpatRaster" %in% class(inputraster)) {
      # raster input directly, write to temporary file
      tf <- tempfile(fileext=".tif")
      writeRaster(inputraster, tf)
      inputraster <- tf
    } else {
      stop("inputraster must be of class SpatRaster or a raster file path.")
    }
  }

  if (is.character(inputpolygon)) {
    if (!file.exists(inputpolygon)) {
      stop("Error: inputpolygon file does not exist")
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

  if (is.null(outputfile)) {
    outputfile <- tempfile(fileext = ".tif")
  }

  tempoutputfile <- tempfile(fileext = ".tif")

  # message("bb_wbt: Clipping raster to polygon")
  # crs_pg <- sf::st_crs(st_read(inputpolygon))$epsg
  # crs_rr <- sf::st_crs(raster(inputraster))$epsg

  # crs_pg <- raster::crs(st_read(inputpolygon))@projargs
  # crs_rr <- raster::crs(raster(inputraster))@projargs

  # crs_rr <- raster::crs(raster(inputraster))
  # if (crs_pg != crs_rr) {
  if (!compareCRS(vect(inputpolygon), rast(inputraster))){
    stop("Error: Input raster and polygons have different crs")
  }

  whitebox::wbt_clip_raster_to_polygon(input=inputraster,
                             polygons=inputpolygon,
                             output=tempoutputfile,
                             maintain_dimensions = maintain_dimensions,
                             compress_rasters = compress_rasters,
                             verbose_mode = FALSE)

  # fix crs on clipped raster
  rr <- rast(tempoutputfile)
  crs(rr) <- terra::crs(rast(inputraster))
  terra::writeRaster(x=rr, filename=outputfile, overwrite=TRUE)
  unlink(tempoutputfile) # memorycleanup

  if (return_raster) {
    rr <- terra::rast(outputfile)
    return(rr)
  } else {
    return(outputfile)
  }
}
