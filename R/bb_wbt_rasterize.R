#' Convert vector to raster
#'
#' @param inputvector file path for a vector to rasterize
#' @param field field name as character to use for raster values
#' @param baseraster file path for raster to use as base
#' @param outputfile Output file to write the resulting raster to
#' @param nodata set background value to nodata (if \code{FALSE} is set to 0.0)
#' @param return_raster If \code{TRUE} (the default) a raster of the rasterized catchments will be returned.
#'
#' @return If \code{return_raster == TRUE} a raster of the rasterized catchments will be returned.
#' Otherwise, \code{TRUE}.
#'
#' @examples
#' print("TO DO")
#'
#'
#' @importFrom raster compareCRS
#' @importFrom terra rast writeVector crs writeRaster vect
#' @importFrom whitebox wbt_clip_raster_to_polygon
#' @importFrom sf st_crs write_sf st_read write_sf
#'
#' @export bb_wbt_clip_raster_to_polygon
bb_wbt_rasterize <- function(inputvector=NULL, field=NULL, baseraster=NULL,
                             outputfile=NULL, nodata=TRUE, return_raster=TRUE) {

  stopifnot(!is.null(inputvector),!is.null(baseraster),!is.null(field))

  bb_wbt_check_whitebox()
  exe_location <- whitebox::wbt_init()

  if (is.character(inputvector)) {
    if (!file.exists(inputvector)) {
      stop("Error: input vector file does not exist")
    }
  } else {
    if ("sf" %in% class(inputvector) ) {
      tf <- tempfile(fileext = ".shp")
      inputvector <- write_sf(obj=inputvector, dsn=tf)
      inputvector <- tf
    } else if ("SpatVector" %in% class(inputvector) ) {
      tf <- tempfile(fileext = ".shp")
      inputvector <- writeVector(inputvector, tf)
      inputvector <- tf
    } else {
      stop("inputvector must be of class sf/SpatVector or a shapefile file path")
    }
  }

  if (is.character(baseraster)) {
    if (!file.exists(baseraster)) {
      stop("Error: file containing snapped pour points does not exist")
    }
  } else {

    if ("SpatRaster" %in% class(baseraster)) {
      # raster input directly, write to temporary file
      tf <- tempfile(fileext=".tif")
      writeRaster(baseraster, tf)
      baseraster <- tf
    } else {
      stop("baseraster must be of class SpatRaster or a raster file path.")
    }
  }

  # check that the supplied field is in inputvector
  if (field %notin% colnames(read_sf(inputvector))) {
    stop(sprintf("field name '%s' must be in inputvector",field))
  }

  if (is.null(outputfile)) {
    outputfile <- tempfile(fileext = ".tif")
  }

  tempoutputfile <- tempfile(fileext = ".tif")

  if (!compareCRS(vect(inputvector), rast(baseraster))) {
    stop("Error: Input vector and base raster have different crs")
  }

  # set verbose_mode to FALSE in most cases
  whitebox::wbt_vector_polygons_to_raster(input=inputvector,
                             output=tempoutputfile,
                             field=field,
                             nodata=nodata,
                             base=baseraster,
                             verbose_mode = FALSE)

  # fix crs on clipped raster
  rr <- rast(tempoutputfile)
  crs(rr) <- terra::crs(rast(baseraster))
  terra::writeRaster(x=rr, filename=outputfile, overwrite=TRUE)
  unlink(tempoutputfile) # memorycleanup

  if (return_raster) {
    rr <- terra::rast(outputfile) #re-read so that source file is updated
    return(rr)
  } else {
    return(TRUE)
  }
}
