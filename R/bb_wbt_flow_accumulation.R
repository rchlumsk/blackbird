#' Creates flow accumulation grid file
#'
#' @param fn_dem_ns File name of dem with sinks removed.
#' @param fn_flowacc File name for flow accumulation grid to be created.
#' @param return_raster If \code{TRUE} (the default), the flow accumulation
#' grid will be returned as a raster object, in addition to being written to
#' \option{fn_flowacc}. If \code{FALSE}, the output file will still be created
#' but a \code{NULL} value is returned.
#'
#'
#' @return If \code{return_raster = TRUE}, the flow accumulation
#' grid will be returned as a raster object, otherwise \code{NULL} is returned.
#'
#' @examples
#'
#'
#' @importFrom raster raster
#' @importFrom whitebox wbt_d8_flow_accumulation
#' @export bb_wbt_flow_accumulation
bb_wbt_flow_accumulation <- function(fn_dem_ns, fn_flowacc, return_raster = FALSE) {
  bb_wbt_check_whitebox()
  exe_location <- whitebox::wbt_init()
  if (!file.exists(fn_dem_ns)) {
    stop("Error: input sink-free dem file does not exist")
  }

  message("bb_wbt: Creating flow accumulation grid")
  whitebox::wbt_d8_flow_accumulation(fn_dem_ns, fn_flowacc)

  if (return_raster) {
    return(raster(fn_flowacc))
  } else {
    return(TRUE)
  }
}
