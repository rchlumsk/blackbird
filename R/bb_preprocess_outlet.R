#' @title Creates an outlet file for the rivershp creation based on flow accumulation
#'
#' @description
#' Determines the outlet position based on the flow accumulation raster and creates an outlet shapefile with one point.
#'
#' @param lastindex The position from the flowaccumulation raster to use in (indicates which last position to use in finding the position)
#' @param return_shp whether to return the processed outlet as an sf object
#' @param workingfolder folder to read inputs from and write outputs to for blackbird operations
#' @param overwrite whether to overwrite an existing file (default TRUE)
#'
#' @return {\code{TRUE} if processed properly, or the dem if return_raster is TRUE}
#
#' @details
#'
#'
#' @examples
#'
#'
#' @importFrom terra rast writeRaster crs res
#' @importFrom sf write_sf
#' @export bb_preprocess_outlet
bb_preprocess_outlet <- function(lastindex=1,
                    return_shp=FALSE, workingfolder=NULL, overwrite=TRUE) {

  r <- bb_get_flowaccraster(workingfolder)
  vals <- terra::values(r, mat = FALSE)
  valid <- which(!is.na(vals)) # Remove NAs

  # Cell index of Nth highest value
  nth_cell <- valid[order(vals[valid], decreasing = TRUE)[lastindex]]

  xy <- terra::xyFromCell(r, nth_cell)

  ss <- sf::st_as_sf(
    data.frame(
      fid = 1,
      name="outlet",
      rank = lastindex,
      flowacc = vals[nth_cell],
      x = xy[1],
      y = xy[2]
    ),
    coords = c("x", "y"),
    crs = terra::crs(r)
  )

  # write to file
  write_sf(ss,bb_get_outletshp(workingfolder, returnobject=FALSE))

  if (return_shp) {
    return(ss)
  } else {
    return(TRUE)
  }
}
