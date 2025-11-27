#' @title Write Blackbird model input files in netcdf format
#'
#' @description
#' Writes model input files to Blackbird netcdf format.
#'
#' @details
#' Writes the netcdf file required as inputs to Blackbird compiled executable. The relevant GIS raster information
#' from the project is automatically collected from the workingfolder location (i.e. catchment raster, HAND raster,
#' HAND ID raster) and written with the appropriate dimensions into netcdf format. DHAND information is written if
#' the \code{bbopt$use_dhand} argument is \code{TRUE}.
#'
#' File is written into the main folder of the workingfolder location.
#'
#' @param bbopt blackbird options object
#' @param filename name of the netcdf file to write to (including .nc extension)
#' @param workingfolder workingfolder
#'
#' @return {returns \code{TRUE} if run successfully}
#
#' @examples
#' # to do
#'
#'
#' @importFrom terra ext res
#' @importFrom ncdf4 ncdim_def ncvar_def nc_create ncvar_put ncatt_put nc_close
#' @importFrom sf st_crs
#' @export bb_write_inputs_netcdf
bb_write_inputs_netcdf <- function(bbopt=NULL, filename="bb_inputs.nc", workingfolder=NULL) {
  # Read input
  c_from_s <- blackbird::bb_get_catchmentsfromstreamnodesraster(workingfolder)
  hand <- blackbird::bb_get_handraster(workingfolder)
  handid <- blackbird::bb_get_handpourpointIDraster(workingfolder)
  depths <- bbopt$dhand_Hseq # seq(from=0, to=9, by=0.05) # manually set for now?
  nd <- length(depths)

  if (bbopt$use_dhand) {
    dhand <- vector("list", nd)
    dhandid <- vector("list", nd)
    for(i in seq_along(depths)) {
      dhand[[i]] <- blackbird::bb_get_dhandraster(workingfolder, depth=depths[i], filetype="depthraster")
      dhandid[[i]] <- blackbird::bb_get_dhandraster(workingfolder, depth=depths[i], filetype="idraster")
    }
  }

  # Read dimensions, resolution, etc.
  extent <- ext(c_from_s)
  resolution <- res(c_from_s)
  xvals <- seq(from=extent$xmin, to=extent$xmax-0.01, by=resolution[1])
  yvals <- seq(from=extent$ymax-0.01, to=extent$ymin, by=-resolution[2])
  nx <- length(xvals)
  ny <- length(yvals)

  # Define nc dimensions
  east_dim <- ncdim_def("easting", "meters", xvals)
  north_dim <- ncdim_def("northing", "meters", yvals)
  depth_dim <- ncdim_def("depth", "meters", depths)

  # Define nc variables
  var_c_from_s <- ncvar_def("catchments_streamnodes", "", list(east_dim, north_dim), NAflag(c_from_s), longname="Blackbird Catchments From Streamnodes Raster")
  var_hand <- ncvar_def("hand", "meters", list(east_dim, north_dim), NAflag(hand), longname="Blackbird HAND Raster")
  var_handid <- ncvar_def("handid", "", list(east_dim, north_dim), NAflag(handid), longname="Blackbird HAND Pourpoint ID Raster")

  if (bbopt$use_dhand) {
    var_dhand <- ncvar_def("dhand", "meters", list(east_dim, north_dim, depth_dim), NAflag(dhand[[1]]), "Blackbird DHAND Rasters")
    var_dhandid <- ncvar_def("dhandid", "", list(east_dim, north_dim, depth_dim), NAflag(dhandid[[1]]), "Blackbird DHAND Pourpoint ID Rasters")
  }

  # Create nc
  if (bbopt$use_dhand) {
    ncnew <- nc_create(paste0(workingfolder, "/", filename), list(var_c_from_s, var_hand, var_handid, var_dhand, var_dhandid), force_v4=TRUE)
  } else {
    ncnew <- nc_create(paste0(workingfolder, "/", filename), list(var_c_from_s, var_hand, var_handid), force_v4=TRUE)
  }

  print(paste("The file has", ncnew$nvars, "variables"))
  print(paste("The file has", ncnew$ndim, "dimensions"))

  # Convert SpatRasters to matrices
  c_from_s_data <- t(as.matrix(c_from_s, wide=TRUE))
  hand_data <- t(as.matrix(hand, wide=TRUE))
  handid_data <- t(as.matrix(handid, wide=TRUE))

  if (bbopt$use_dhand) {
    dhand_data <- array(NAflag(dhand[[1]]), dim=c(nx, ny, nd))
    dhandid_data <- array(NAflag(dhandid[[1]]), dim=c(nx, ny, nd))
    for (i in seq_along(dhand)) {
      dhand_data[,,i] <- t(as.matrix(dhand[[i]], wide=TRUE))
      dhandid_data[,,i] <- t(as.matrix(dhandid[[i]], wide=TRUE))
    }
  }

  # Write data to nc
  ncvar_put(ncnew, var_c_from_s, c_from_s_data)
  ncvar_put(ncnew, var_hand, hand_data)
  ncvar_put(ncnew, var_handid, handid_data)
  # dhand_data <- aperm(dhand_data, c(2, 1, 3))
  # dhandid_data <- aperm(dhandid_data, c(2, 1, 3))
  if (bbopt$use_dhand) {
    ncvar_put(ncnew, var_dhand, dhand_data)
    ncvar_put(ncnew, var_dhandid, dhandid_data)
  }

  ncatt_put(ncnew, 0, "Product", "Blackbird_InputNC")
  ncatt_put(ncnew, 0, "License", "Provided by Heron Hydrologic under an MIT license")
  ncatt_put(ncnew, 0, "EPSG", paste("Projected coordinate system with EPSG code",sf::st_crs(c_from_s)$epsg))

  # Close nc file
  nc_close(ncnew)

  print("NetCDF file created successfully.")
  return(TRUE)
}
