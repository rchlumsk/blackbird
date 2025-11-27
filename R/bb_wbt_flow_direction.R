#' Creates flow direction grid file
#'
#' @param fn_dem_ns File name of dem with sinks removed.
#' @param fn_flowdir File name for flow direction grid to be created.
#' @param return_raster Should a raster object be returned?
#'
#' @return If \code{return_raster = TRUE} (the default), the flow direction
#' grid will be returned as a raster object, in addition to being written to
#' \option{fn_flowdir}. If \code{return_raster = FALSE}, the output file will still be created
#' but a \code{NULL} value is returned.
#'
#' @examples
#'
#'
#' @importFrom raster raster
#' @importFrom whitebox wbt_d8_pointer
#' @export bb_wbt_flow_direction
bb_wbt_flow_direction <- function(fn_dem_ns, fn_flowdir, method="d8", return_raster = FALSE) {
  bb_wbt_check_whitebox()
  exe_location <- whitebox::wbt_init()
  if (!file.exists(fn_dem_ns)) {
    stop("Error: input sink-free dem file does not exist")
  }
  message("bb_wbt: Creating flow direction grid")

  if (method=="d8") {
     whitebox::wbt_d8_pointer(fn_dem_ns, fn_flowdir)
  } else if (method=="dinf") {
     whitebox::wbt_d_inf_pointer(fn_dem_ns, fn_flowdir)
    warning("resulting flow direction file will not work with native flow accumulation layer")
  } else {
    stop(sprintf("invalid method: %s", method))
  }

  if (return_raster) {
    return(raster(fn_flowdir))
  } else {
    return(TRUE)
  }
}
