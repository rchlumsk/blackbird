
### ~~~~~~~~~~~~~~~~~~~~
### BASIC UTILITIES ----
### ~~~~~~~~~~~~~~~~~~~~

#' @title Pads string with spaces, either right or left justified
#'
#' @description
#' Pad string with spaces, justified on either the left or right
#'
#' @param string Text string
#' @param width Number of characters total, including desired spaces
#' @param just 'r' for right, 'l' for left
#'
#' @return {Padded string}
#'
#' @examples
#' # Returns "   To the right"
#' stringpad('To the right', 15, just='r')
#'
#' # Returns "Padded    "
#' stringpad('Padded', 10, just='l')
stringpad <- function(string, width, just='r')
{
  slength <- nchar(string)
  padlength <- width - slength
  #-- Break if string is longer than the pad width
  if (padlength < 0) {
    stop('String exceeds total width')
  }
  #-- return a padded string
  if (just == 'r') {
    return(paste0(strrep(' ', padlength), string))
  }
  else if (just == 'l') {
    return(paste0(string, strrep(' ', padlength)))
  }
}

#' @title substring from the Left
#'
#' @description Returns n characters from the left side of the supplied string x.
#'
#' @param x a string to manipulate
#' @param n number of characters to remove from the left side of the string
#' @seealso \code{\link{bb_substrRight}} for using n characters from right side of string
#'
#' \code{\link{bb_substrMRight}} for removing n characters from the right side of a string
#'
#' @examples
#'
#' bb_substrLeft("hello world",3)
#' # returns "hel"
#'
#' @export bb_substrLeft
bb_substrLeft <- function(x, n)
{
  substr(x, 1,n)
}


#' @title substring minus characters from the Left
#'
#' @description
#' Returns a string x with n characters removed from the left side
#' of the string.
#'
#' @param x a string to manipulate
#' @param n number of characters to remove from the left side of the string
#' @seealso \code{\link{bb_substrRight}} for using n characters from the right
#' side of string,
#'
#' \code{\link{bb_substrLeft}} for using n characters from the left side of
#' string
#'
#' \code{\link{bb_substrMRight}} for removing n characters from the right side of
#' a string
#' @examples
#'
#' bb_substrMLeft("hello world",3)
#' # returns "lo world"
#'
#' @export bb_substrMLeft
bb_substrMLeft <- function(x, n)
{
  substr(x, n+1,nchar(x))
}


#' @title substring minus characters from the Right
#'
#' @description
#' Returns a string x with n characters removed from the right
#' side of the string.
#'
#' @param x a string to manipulate
#' @param n number of characters to remove from the right side of the string
#' @seealso \code{\link{bb_substrRight}} for using n characters from the right
#' side of string
#'
#' \code{\link{bb_substrLeft}} for using n characters from the left side of
#' string
#'
#' \code{\link{bb_substrMLeft}} for removing n characters from the left side of a
#' string
#' @examples
#'
#' bb_substrMRight("hello world",3)
#' # returns "hello wo"
#'
#' @export bb_substrMRight
bb_substrMRight <- function(x, n)
{
  substr(x, 1,nchar(x)-n)
}


#' @title substring from the Right
#'
#' @description
#' Returns n characters from the right side of the supplied string
#' x.
#'
#' @param x a string to manipulate
#' @param n number of characters to use from the right side of the string
#' @seealso \code{\link{bb_substrLeft}} for using n characters from the left side
#' of string
#'
#' \code{\link{bb_substrMRight}} for removing n characters from the right side of
#' a string
#'
#' \code{\link{bb_substrMLeft}} for removing n characters from the left side of a
#' string
#' @examples
#'
#' bb_substrRight("hello world",3)
#' # returns "rld"
#'
#' @export bb_substrRight
bb_substrRight <- function(x, n)
{
  substr(x, nchar(x)-n+1, nchar(x))
}

#' Pipe operator
#'
#' See \code{magrittr::\link[magrittr:pipe]{\%>\%}} for details.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
#' @param lhs A value or the magrittr placeholder.
#' @param rhs A function call using the magrittr semantics.
#' @return The result of calling `rhs(lhs)`.
NULL


#' @title \%notin\% operator
#'
#' @description
#' Syntax for the opposite of \%in\%
#'
#' @param x values to be matched
#' @param table the values to be matched against
#'
#' @examples
#' seq(1,5,1) %notin% seq(3,7,1)
#'
#' @export %notin%
"%notin%" <- function(x, table) match(x, table, nomatch = 0) == 0


#' @title Transparent colour
#'
#' @description Adds a transparency to a given colour
#'
#' @param color color to be modified
#' @param percent percent transparency
#' @param name an optional name for the color
#'
#' @return colour transparent colour (as hexadecimal color code)
#' @importFrom grDevices col2rgb rgb
#' @export tcol
#'
#' @examples
#' tcol("white", percent=75)
#'
tcol <- function(color, percent = 50, name = NULL) {
  #      color = color name
  #    percent = % transparency
  #       name = an optional name for the color

  ## Get RGB values for named color
  rgb.val <- col2rgb(color)

  ## Make new color using input color as base and alpha set by transparency
  t.col <- rgb(red=rgb.val[1], blue=rgb.val[2], green=rgb.val[3],
               alpha = (100 - percent) * 255 / 100,
               names = name,
               maxColorValue = 255)

  return(t.col)
}


#' @title Check for finite values and return zero if infinite or NaN
#'
#' @description
#' Check for finite values, and return zero if infinite or NaN
#'
#' @details
#' Performs \code{is.finite} to determine whether to return \code{x} as is,
#' or return zero. Strings and other non-numeric types may return 0 or
#' result in an error (see example). Logicals are returned as they are.
#'
#' @param x value (likely numeric) for checking
#'
#' @return {finite value}
#'
#' @examples
#' finiteorzero(1/5)
#' finiteorzero(1/0)
#' finiteorzero("string")
#' finiteorzero(TRUE) # converts to numeric
#'
#' finiteorzero(c(1,2.5,1/0,Inf,NaN,pi))
#'
#'
#' @rdname finiteorzero
finiteorzero <- function(x)
{
  # if (is.finite(x)) {
  #   return(x)
  # } else {
  #   return(0)
  # }
  x[!is.finite(x)] <- 0
  return(x)
}

#' @rdname finiteorzero
finiteorone <- function(x)
{
  # if (is.finite(x)) {
  #   return(x)
  # } else {
  #   return(1)
  # }
  x[!is.finite(x)] <- 1
  return(x)
}


#' @title Tokenize string
#'
#' @description
#' Tokenizes a string into a vector, removing anything after a comment
#'
#' @param tt Text string
#'
#' @return {vectorized string}
#'
#' @examples
#' bb_tokenize(":Command sample string being tokenized # the comment won't be read")
#'
#' @export bb_tokenize
bb_tokenize <- function(tt) {

  if (is.na(tt)) {tt <- ""}
  ss <- unlist(strsplit(trimws(tt),split="\\,|\\s+|\\,\\s+|\\s+\\,\\s+"))

    # take only ss items before a comment char
    if (any(bb_substrLeft(ss,1) == "#")) {
      if (which(bb_substrLeft(ss,1) == "#")[1] > 1) {
        ss <- ss[1:(which(bb_substrLeft(ss,1) == "#")[1]-1)]
      } else {
        ss <- NULL
      }
    }
  return(ss)
}


#' @title Get N closet values in vector
#'
#' @description
#' Return N closest values from a vector to a specified value
#'
#' @param x a numeric vector
#' @param value function will return n values that are closet to this parameter
#' @param n how many values to return
#'
#' @return {vector of values from \code{x} that are closet to \code{value}}
#'
#' @examples
#' bb_closest(x=seq(1,10), value=6.4, n=3)
#'
#' @export bb_closest
bb_closest <- function(x, value, n=1){
    x[order(abs(x-value))][1:n]
}

#' @title Get N closet values in vector of depths
#'
#' @description
#' Return up to 2 closest values from a depth vector to a specified value
#'
#' @details
#' Here, we don't want to return values that are outside the range of another returned value.
#' For example, for a vector of depths c(10,12,14) and a depth of 10.1, we don't want to return
#' 12 and anything higher than 12, even if n is more than 2. This is applicable when the depth
#' value (\code{value}) is outside the range of vector \code{x}.
#'
#' In this function, the order is also preserved.
#'
#' @param x an ordered numeric vector (of depths)
#' @param value specific depth value to use in searching for closet vector values
#'
#' @return {vector of depth values from \code{x} that are closet to a depth of \code{value}}
#'
#' @examples
#' xx <- c(seq(1,5),seq(6,10,2),seq(11,17,3))
#'
#' # normal scenario - returns two depths to bound the depth value
#' bb_closestdepths(x=xx, value=6.4)
#'
#' # example where depth value is outside the range, will only return one value
#' bb_closestdepths(x=xx, value=20)
#'
#' # example where depth value is exactly equal to a value in x, returns just that value
#' bb_closestdepths(x=xx, value=10)
#'
#' # example where depth value is exactly in between two values in x - favours lower number first
#' bb_closestdepths(x=xx, value=7)
#'
#' @export bb_closestdepths
bb_closestdepths <- function(x, value){
  if (value %in% x) {
    # one value matches exactly
    # will check for this outside of this function, but still good to include
    # xxx check this, seems to not work with returning the exact value unless treated as factor
    return(value)
  }

  dd <- x[order(abs(x-value))]
  x1 <- dd[dd < value][1]
  x2 <- dd[dd>value][1]

  if (is.na(x1) & is.na(x2)) {
    # both values NA for some reason
    return(NA)
  } else if (is.na(x1)) {
    # small value NA, return only larger
    return(x2)
  } else if (is.na(x2)) {
    # large value is NA, return only smaller
    return(x1)
  } else {
    # return both
    return(c(x1,x2))
  }

  # n=2
  # if (length(x) == n) {
  #   return(x)
  # }
  # dd <- x[order(abs(x-value))][1:n]
  # if (max(x) %in% dd) {
  #   ddm1 <- dd[2:length(dd)]
  #   dd <- c(dd[1],ddm1[ddm1>=max(x)])
  # } else if (min(x) %in% dd) {
  #   ddm1 <- dd[2:length(dd)]
  #   dd <- c(dd[1],ddm1[ddm1<=min(x)])
  # }
  # dd <- dd[order(dd)]
  # return(dd)
}

#' @title Chunk a sequence into n segments
#'
#' @description
#' Converts a vector sequence into n approximately equal size segments
#'
#' @param x vector input
#'
#' @return {list of size n}
#'
#' @examples
#' chunk_seq(seq(1,20),3)
#'
#' @seealso contribution on stack overflow for this function \url{https://stackoverflow.com/questions/3318333/split-a-vector-into-chunks}
#'
#' @export chunk_seq
chunk_seq <- function(x,n) {
  split(x, cut(seq_along(x), n, labels = FALSE))
}


#' @title Extend a series to resolution dx
#'
#' @description
#' Extends a series to the desired resolution by interpolating between provided points
#'
#' @param xx vector of horizontal stations
#' @param zz vector of elevations
#' @param nn vector of roughness values
#' @param dx desired horizontal spacing
#' @param method method of interpolation for roughness values (constant or linear), used in \link{\code{approx}} function
#'
#' @return {list of objects for xx, zz, nn}
#'
#' @examples
#' chunk_seq(seq(1,20),3)
#'
#' @export extend_series
extend_series <- function(x,z,n,dx,method="constant") {
  ## extend series to resolution of dx in between all points ----
  xx <- seq(x[1],x[2],by=dx)
  for (i in 2:(length(x)-1)) {
    xx <- c(xx,seq(x[i]+dx,x[i+1],by=dx))
  }
  zz <- approx(x=x,y=z,xout=xx)$y
  nn <- approx(x=x,y=n,xout=xx,method=method)$y
  return(list("xx"=xx,"zz"=zz,"nn"=nn))
}

#' @title Calculate mid point of two points
#' @param p1 first point as set of coordinates (dimension 2,1)
#' @param p2 second point in same format
#' @return {mid point as set of coordinates}
#' @seealso this [Stack Overflow thread](https://stackoverflow.com/questions/56771058/perpendicular-lines-at-regular-intervals-along-lines-with-multiple-nodes)
#' @noRd
mid_point <- function(p1,p2) {
    return(c(p1[1] + (p2[1] - p1[1]) / 2,p1[2] + (p2[2] - p1[2]) / 2))
}

#' @title Calculate slope between two points
#' @param p1 first point as set of coordinates (dimension 2,1)
#' @param p2 second point in same format
#' @return {mid point as set of coordinates}
#' @seealso this [Stack Overflow thread](https://stackoverflow.com/questions/56771058/perpendicular-lines-at-regular-intervals-along-lines-with-multiple-nodes)
#' @noRd
slope <- function(p1,p2) {
    return((p2[2] - p1[2]) / (p2[1] - p1[1]))
}

#' @title Return two minimum values from specific data frame
#' @param spts Two column data frame with the second column named 'dist'
#' @return {spts returned with two rows corresponding to two min values}
#' @noRd
get_2_mins <- function(spts) {
    min_ind <- which(spts$dist == min(spts$dist))
    min_ind <- c(min_ind,
                 which(spts$dist == min(spts$dist[-min_ind])))
    return(min_ind)
}

#' @title Calculate perpindicular points as set of coordinates
#' @param n1 first point as set of coordinates (dimension 2,1)
#' @param n2 second point in same format
#' @param xsection_halflength the length of half the cross-section (i.e. distance from mid point to end) in metres
#' @param mp mid point as set of coordinates (optional)
#' @return {data frame with two points and XY columns that represent a perpindicular line to n1 and n2}
#' @seealso this [Stack Overflow thread](https://stackoverflow.com/questions/56771058/perpendicular-lines-at-regular-intervals-along-lines-with-multiple-nodes)
#' @noRd
calc_perp_points <- function(n1,n2,xsection_halflength,mp=NULL) {

  if (n1[1]==n2[1] & n1[2]==n2[2]) {
    stop("n1 and n2 must be different points")
  }

  # Calculate mid point
  if (is.null(mp)) {
    mp <- mid_point(n1,n2)  # or use snapped_streamnode?
  }

  if (n1[2] == n2[2]) { # correction for zero slope (flat line)
    p1 <- cbind(mp[1], mp[2]-xsection_halflength)
    p2 <- cbind(mp[1], mp[2]+xsection_halflength)

  } else if (n1[1] == n2[1]) { # correction for vertical line
    p1 <- cbind(mp[1]-xsection_halflength, mp[2])
    p2 <- cbind(mp[1]+xsection_halflength, mp[2])

  } else {
    # Calculate slope
    m <- slope(n1,n2)
    new_m <- -1.0 / m
    # Calculate points xsection_halflength units away in x direction at mid_point
    xlen <- sqrt((xsection_halflength^2)/(1+new_m^2))
    ylen <- sqrt((xsection_halflength^2)/(1/(new_m^2)+1))
    p1 <- c(mp[1]+xlen, mp[2]+(xlen*new_m))
    p2 <- c(mp[1]-xlen, mp[2]-(xlen*new_m))
  }
  df <- data.frame(rbind(p1,p2))
  colnames(df) <- c("X","Y")
  return(df)
}

#' @title Calculate perpindicular points as set of coordinates
#' @param n1 first point as set of coordinates (dimension 2,1)
#' @param n2 second point in same format
#' @param n3 third point (first of new set) from a second line
#' @param n4 second point (of second set) in same format
#' @param xsection_halflength the length of half the cross-section (i.e. distance from mid point to end) in metres
#' @param mp mid point as set of coordinates (optional)
#' @details
#'   This function serves the same purpose as calc_perp_points, but in some instances when creating cross-sections,
#'   we end up sampling two lines and need to determine a perpindicular direction to these two lines. Here, this function
#'   takes an average slope of the two lines in questions (from c(n1,n2) and c(n3,n4)) and provides a perpindicular line
#'   to that averaged slope.
#' @return {data frame with two points and XY columns that represent a perpindicular line to n1 and n2}
#' @seealso this [Stack Overflow thread](https://stackoverflow.com/questions/56771058/perpendicular-lines-at-regular-intervals-along-lines-with-multiple-nodes)
#' @noRd
calc_perp_points_avglsope <- function(n1,n2,n3,n4,xsection_halflength,mp=NULL) {

  ## first point
  if (n1[1]==n2[1] & n1[2]==n2[2]) {
    stop("n1 and n2 must be different points")
  }

  # Calculate mid point
  if (is.null(mp)) {
    mp <- mid_point(n1,n2)  # or use snapped_streamnode?
  }

  if (n1[2] == n2[2]) { # correction for zero slope (flat line)
    p1 <- cbind(mp[1], mp[2]-xsection_halflength)
    p2 <- cbind(mp[1], mp[2]+xsection_halflength)
    m1 <- 0

  } else if (n1[1] == n2[1]) { # correction for vertical line
    p1 <- cbind(mp[1]-xsection_halflength, mp[2])
    p2 <- cbind(mp[1]+xsection_halflength, mp[2])
    m1 <- NA

  } else {
    # Calculate slope
    m1 <- slope(n1,n2)
  }

  ## second point
  if (n3[1]==n4[1] & n3[2]==n4[2]) {
    stop("n3 and n4 must be different points")
  }

  # Calculate mid point
  if (is.null(mp)) {
    mp <- mid_point(n3,n4)  # or use snapped_streamnode?
  }

  if (n3[2] == n4[2]) { # correction for zero slope (flat line)
    m2 <- 0

  } else if (n3[1] == n4[1]) { # correction for vertical line
    m2 <- NA

  } else {
    # Calculate slope
    m2 <- slope(n3,n4)
    new_m2 <- -1.0 / m2
  }

  if (is.na(m1) | is.na(m2)) {

    # take average slope from average of points
    ## way to check that taking these points make sense based on min distance? to do xxx
    n5 <- c(mean(c(n1[1],n3[1])), mean(c(n2[1],n4[1])))
    n6 <- c(mean(c(n1[2],n3[2])), mean(c(n2[2],n4[2])))

    # Calculate slope
    m <- slope(n5,n6)
    new_m <- -1.0 / m
    # Calculate points xsection_halflength units away in x direction at mid_point
    xlen <- sqrt((xsection_halflength^2)/(1+new_m^2))
    ylen <- sqrt((xsection_halflength^2)/(1/(new_m^2)+1))
    p1 <- c(mp[1]+xlen, mp[2]+(xlen*new_m))
    p2 <- c(mp[1]-xlen, mp[2]-(xlen*new_m))
  } else {

    m <- mean(c(m1,m2))

    b1 <- calc_intercept(n1,m1)
    # Calculate inverse reciprocal of slope
    new_m <- -1.0 / m
    xlen <- sqrt((xsection_halflength^2)/(1+new_m^2))
    ylen <- sqrt((xsection_halflength^2)/(1/(new_m^2)+1))
    p1 <- c(mp[1]+xlen, mp[2]+(xlen*new_m))
    p2 <- c(mp[1]-xlen, mp[2]-(xlen*new_m))
  }
  df <- data.frame(rbind(p1,p2))
  colnames(df) <- c("X","Y")
  return(df)
}



#' @title Convert TIFF to PNG
#' @description
#' Converts raster tiff files to png format for web display along with json metadata file.
#' @param tiff file path for tiff file or directory with tiff files
#' @param png_dir directory to write png files to (tempdir if not provided)
#' @return png_dir directory of png files if successful
#' @importFrom terra rast
#' @importFrom rjson toJSON
bb_tiff_to_png <- function(tiff_path, png_dir=NULL) {
# library(terra)
# library(rjson)
  # function to normalize raster to [0,255]
  normalize_png <- function(x){255*(x-val_bounds[1])/(val_bounds[2]-val_bounds[1])}

  # Modify these
  # geotiff_dir <- "../GeoTiffs" # path to geotiff files
  # png_dir <- "../PNGs" # path to save png files

  # check if input is directory or single tiff
  if (dir.exists(tiff_path)) {
    geotiff_dir <- tiff_path
  } else {
    geotiffs <- tiff_path
  }

  # set png_dir if not provided
  if (is.null(png_dir)) {
    png_dir <- tempdir()
  }

  # Initialize data frame for holding metadata
  geotiffs <- list.files(geotiff_dir)

  for(tif in geotiffs) {
    rr <- terra::rast(paste(geotiff_dir, tif, sep="/"))
    val_bounds <- range(rr[!is.na(rr)]) # extract values to matrix form with [] notation and take out NA values
    rr_norm <- normalize_png(rr)

    # writes to graphic object as file instead of plot display in RStudio
    png_path <- paste(png_dir, sub(".tif", ".png", tif), sep="/")
    png(png_path, width=ncol(rr_norm), height=nrow(rr_norm))
    colfunc <- colorRampPalette(c("white", "darkblue"))
    col <- colfunc(500)
    par(bg=NA, mar=c(0,0,0,0), oma=c(0,0,0,0))
    plot.new()
    plot.window(xlim=c(xmin(rr_norm), xmax(rr_norm)), ylim=c(ymin(rr_norm), ymax(rr_norm)), xaxs="i",yaxs="i")
    terra::plot(rr_norm, maxcell = ncell(rr_norm), col = col, colNA = NA,
                axes = FALSE, legend=FALSE, box=FALSE, add=TRUE)
    dev.off() # release graphic object

    # writes metadata json file for png
    json_path <- sub(".png", ".json", png_path)
    extents <- matrix(list(), nrow = 2, ncol = 1)
    extents[[1,1]] <- c(ymax(rr_norm), xmin(rr_norm))
    extents[[2,1]] <- c(ymin(rr_norm), xmax(rr_norm))
    metadata <- list(extents = extents, flow_rate = strsplit(sub(".tif", "", tif), "_")[[1]][4], min_depth = val_bounds[1], max_depth = val_bounds[2])
    write(toJSON(metadata, 1), json_path)
  }

  # Write colour data to json
  rgb <- col2rgb(col)
  col_data <- array(data = NA, dim = c(500))
  for(i in 1:500) {
    col_data[i] <- list(rgb[,i])
  }
  write(toJSON(col_data, 1), paste(png_dir, "colours.json", sep="/"))
}

#' @title Blackbird dynamic unload for reinstall
#' @description
#' Unloads blackbird dll for re-installation of package
#' @return {\code{TRUE} if successful}
#' @noRd
bb_dynunload <- function() {
  dyn.unload(system.file("libs", "x64", "blackbird.dll", package = "blackbird"))
  return(TRUE)
}


#' @title Get breaks in index vector
#' @description
#' Determines breakpoints in the index vector where jumps occur (i.e. where wetted area is discontinuous).
#' @param x numeric index vector (likely ind from blackbird)
#' @return breakpoints vector of breakpoints in x. Will at least contain the bounds.
get_breakpoints <- function(x) {
  return(c(1,which(diff(x)!=1),length(x)))
}

#' @title Truncate or pad vector to specific length
#' @details
#' Used in downslope tracing algorithm for fuzzy HAND#'
#' @param x vector to truncate or pad
#' @param maxcolsize max length of vector
#' @return {vector of length maxcolsize}
#' @noRd
truncate_pad <- function(x,maxcolsize) {
  # truncates to maxcolsize or pads to maxcolsize, depending on length of x

  if (length(x) >= maxcolsize) {
    x <- x[1:maxcolsize]
  } else {
    x <- c(x,rep(-1,maxcolsize-length(x))) # pad with -1
  }
  return(x)
}

#' @title Return rows of matrix which contain any vector elements
#' @details
#' Used in downslope tracing algorithm for fuzzy HAND
#' @param mat matrix of values
#' @param vec vector of values to look for
#' @return {vector of \code{mat} rows which contain any matching elements in \code{vec}}
#' @noRd
contains_element <- function(mat, vec) {
  row_has_element <- apply(mat, 1, function(row) any(row %in% vec))
  return(which(row_has_element)) # Returns row indices that match
}

#' @title Return only elements of x also found in y
#' @param x vector
#' @param y second vector
#' @return {vector of values that are also found in y}
#' @noRd
common_elements <- function(x, y) {
  return(x[which(x %in% y)])
}

### ~~~~~~~~~~~~~~~~~~~~~~~~~
### METRICS -----------------
### ~~~~~~~~~~~~~~~~~~~~~~~~~

#' @title Metrics for evaluating flood rasters
#'
#' @description
#' Metrics for computing
#'
#' @details
#' All functions use the function \code{bb_get_object} to retrieve files/objects.
#' Standard names for blackbird files can be found with a \code{\link{bb_get_fileinfo}}.
#'
#' @param rrsim raster from simulated model (SpatRaster or matrix form)
#' @param rrobs raster from observation (SpatRaster or matrix form)
#'
#' @return Returns numeric value or data frame of numeric metrics
#'
#' @name bb_metric


#' @rdname bb_metric
bb_metric_abserror <- function(rrsim,rrobs) {
  # sum of absolute error in each cell

  if (class(rrobs) %in% "numeric") {

    # check extents first
    if (length(rrobs) != length(rrsim)) {
      stop("vectors must be of the same length")
    }

  } else {
    if (terra::ext(rrobs) != terra::ext(rrsim)){
      stop("rasters need the same extents")
    }
    # convert to matrix
    rrobs <-  terra::as.matrix(rrobs)
    rrsim <- terra::as.matrix(rrsim)
  }

  # set NA to zero here to avoid missing calculations where one is NA, other has value
  rrobs[is.na(rrobs)] <- 0
  rrsim[is.na(rrsim)] <- 0

  # total abs error
  diff <- abs(rrobs-rrsim)
  val <- sum(diff[!is.na(diff)])
  return(val)
}

#' @rdname bb_metric
bb_metric_mae <- function(rrsim,rrobs) {
  # mean absolute error

  if (class(rrobs) %in% "numeric") {

    # check extents first
    if (length(rrobs) != length(rrsim)) {
      stop("vectors must be of the same length")
    }

  } else {
    if (terra::ext(rrobs) != terra::ext(rrsim)){
      stop("rasters need the same extents")
    }
    # convert to matrix
    rrobs <-  terra::as.matrix(rrobs)
    rrsim <- terra::as.matrix(rrsim)
  }

  # set NA to zero here to avoid missing calculations where one is NA, other has value
  rrobs[is.na(rrobs)] <- 0
  rrsim[is.na(rrsim)] <- 0

  # total abs error
  diff <- abs(rrobs-rrsim)
  val <- mean(diff[!is.na(diff)])
  return(val)
}

#' @rdname bb_metric
bb_metric_sserror <- function(rrsim,rrobs) {
  # sum of squared errors

  if (class(rrobs) %in% "numeric") {

    # check extents first
    if (length(rrobs) != length(rrsim)) {
      stop("vectors must be of the same length")
    }

  } else {
    if (terra::ext(rrobs) != terra::ext(rrsim)){
      stop("rasters need the same extents")
    }
    # convert to matrix
    rrobs <-  terra::as.matrix(rrobs)
    rrsim <- terra::as.matrix(rrsim)
  }

  # set NA to zero here to avoid missing calculations where one is NA, other has value
  rrobs[is.na(rrobs)] <- 0
  rrsim[is.na(rrsim)] <- 0

  # total sum squares error
  diff <- (rrobs-rrsim)^2
  val <- sum(diff[!is.na(diff)])
  return(val)
}

#' @rdname bb_metric
bb_metric_rmse <- function(rrsim,rrobs) {
  # root mean sum of squared errors

  if (class(rrobs) %in% "numeric") {

    # check extents first
    if (length(rrobs) != length(rrsim)) {
      stop("vectors must be of the same length")
    }

  } else {
    if (terra::ext(rrobs) != terra::ext(rrsim)){
      stop("rasters need the same extents")
    }
    # convert to matrix
    rrobs <-  terra::as.matrix(rrobs)
    rrsim <- terra::as.matrix(rrsim)
  }

  # set NA to zero here to avoid missing calculations where one is NA, other has value
  rrobs[is.na(rrobs)] <- 0
  rrsim[is.na(rrsim)] <- 0

  # total sum squares error
  diff <- (rrobs-rrsim)^2
  val <- sqrt(mean(diff[!is.na(diff)]))
  return(val)
}

#' @rdname bb_metric
bb_metric_normsserror <- function(rrsim,rrobs) {
  # sum of normalized squared errors
  # NSE-type metric to evaluate 1D model against an average depth raster

  if (class(rrobs) %in% "numeric") {

    # check extents first
    if (length(rrobs) != length(rrsim)) {
      stop("vectors must be of the same length")
    }

  } else {
    if (terra::ext(rrobs) != terra::ext(rrsim)){
      stop("rasters need the same extents")
    }
    # convert to matrix
    rrobs <-  terra::as.matrix(rrobs)
    rrsim <- terra::as.matrix(rrsim)
  }

  mean_rrobs <- mean(rrobs[!is.na(rrobs)])

  # set NA to zero here to avoid missing calculations where one is NA, other has value
  rrobs[is.na(rrobs)] <- 0
  rrsim[is.na(rrsim)] <- 0

  # create layer equal to avg depth in all 2d areas
  # see how layer does relative to that (NSE approach?)
  rrobs_benchmark <- rrobs
  rrobs_benchmark[rrobs_benchmark != 0] <- mean_rrobs

  # total normalized sum squares error
  diff <- (rrobs-rrsim)^2
  diff_benchmark <- (rrobs-rrobs_benchmark)^2
  val <- 1-sum(diff[!is.na(diff)])/sum(diff_benchmark[!is.na(diff_benchmark)])

  return(val)
}

#' @rdname bb_metric
bb_metric_hitmetrics <- function(rrsim,rrobs) {
  # binary hit/miss/falsealarm metrics

  if (class(rrobs) %in% "numeric") {

    # check extents first
    if (length(rrobs) != length(rrsim)) {
      stop("vectors must be of the same length")
    }

  } else {
    if (terra::ext(rrobs) != terra::ext(rrsim)){
      stop("rasters need the same extents")
    }
    # convert to matrix
    rrobs <-  terra::as.matrix(rrobs)
    rrsim <- terra::as.matrix(rrsim)
  }

  # set NA to zero here to avoid missing calculations where one is NA, other has value
  rrobs[is.na(rrobs)] <- 0
  rrsim[is.na(rrsim)] <- 0

  # hit calculations
  ind_2d_wet <- which(rrobs>0)
  ind_2d_dry <- which(is.na(rrobs) | rrobs==0)
  ind_1d_wet <- which(rrsim>0)
  ind_1d_dry <- which(is.na(rrsim) | rrsim==0)

  hits_wet <- length(which(ind_1d_wet %in% ind_2d_wet))
  hits_dry <- length(which(ind_1d_dry %in% ind_2d_dry))
  hits <- hits_wet + hits_dry
  falsealarms <- length(which(ind_1d_wet %in% ind_2d_dry))
  misses <- length(which(ind_1d_dry %in% ind_2d_wet))

  pod <- hits/(hits+misses)
  # far <- falsealarms/(hits+falsealarms)  ## change to hits / hits + falsealarms
  far <- hits/(hits+falsealarms)  ## updated metric
  csi <- hits/(sum(hits,misses,falsealarms))
  bias <- (hits+falsealarms)/(hits+misses)
  return(data.frame(hits_wet,hits_dry,hits,falsealarms,misses,pod,far,csi,bias))
}

#' @rdname bb_metric
bb_metric_hitmetrics_thresh <- function(rrsim,rrobs,depth_threshold=0.1) {
  # binary hit/miss/falsealarm metrics with threshold to reduce impact of small depths

  if (class(rrobs) %in% "numeric") {

    # check extents first
    if (length(rrobs) != length(rrsim)) {
      stop("vectors must be of the same length")
    }

  } else {
    if (terra::ext(rrobs) != terra::ext(rrsim)){
      stop("rasters need the same extents")
    }
    # convert to matrix
    rrobs <-  terra::as.matrix(rrobs)
    rrsim <- terra::as.matrix(rrsim)
  }

  # set NA to zero here to avoid missing calculations where one is NA, other has value
  rrobs[is.na(rrobs)] <- 0
  rrsim[is.na(rrsim)] <- 0

  # hit calculations with threshold
  # depth_threshold <- 0.1 # 10cm?
  ind_2d_wet <- which(rrobs>0)
  ind_2d_dry <- which(is.na(rrobs) | rrobs==0)
  ind_1d_wet <- which(rrsim>0)
  ind_1d_dry <- which(is.na(rrsim) | rrsim==0)

  vec_wet2d <- rrobs[ind_2d_wet] / rrobs[ind_2d_wet]
  wet2d_lessthresh <- length(vec_wet2d[rrobs[ind_2d_wet]<depth_threshold])
  vec_wet2d[rrobs[ind_2d_wet]<depth_threshold] <- rrobs[ind_2d_wet][rrobs[ind_2d_wet]<depth_threshold]/depth_threshold
  vec_wet1d <- rrsim[ind_1d_wet] / rrsim[ind_1d_wet]
  wet1d_lessthresh <- length(vec_wet1d[rrsim[ind_1d_wet]<depth_threshold])
  vec_wet1d[rrsim[ind_1d_wet]<depth_threshold] <- rrsim[ind_1d_wet][rrsim[ind_1d_wet]<depth_threshold]/depth_threshold

  hits_wet <- length(which(ind_1d_wet %in% ind_2d_wet)) # same
  hits_dry <- length(which(ind_1d_dry %in% ind_2d_dry)) # same
  hits <- hits_wet + hits_dry
  # falsealarms <- length(which(ind_1d_wet %in% ind_2d_dry))
  falsealarms <- sum(vec_wet1d[which(ind_1d_wet %in% ind_2d_dry)])
  # misses <- length(which(ind_1d_dry %in% ind_2d_wet))
  misses <- sum(vec_wet2d[which(ind_2d_wet %in% ind_1d_dry)])

  pod <- hits/(hits+misses)
  # far <- falsealarms/(hits+falsealarms)
  far <- hits/(hits+falsealarms) # updated metric
  csi <- hits/(sum(hits,misses,falsealarms))
  bias <- (hits+falsealarms)/(hits+misses)
  return(data.frame(hits_wet,hits_dry,hits,falsealarms,misses,pod,far,csi,bias))

  # counts of how many wet cells less than threshold here, if needed
  # return(data.frame(wet2d_lessthresh,wet1d_lessthresh,hits_wet,hits_dry,hits,falsealarms,misses,pod,far,csi,bias))
}

#' @rdname bb_metric
bb_metric_confusionmetrics <- function(rrsim,rrobs) {
  # binary hit/miss/falsealarm metrics

  if (class(rrobs) %in% "numeric") {

    # check extents first
    if (length(rrobs) != length(rrsim)) {
      stop("vectors must be of the same length")
    }

  } else {
    if (terra::ext(rrobs) != terra::ext(rrsim)){
      stop("rasters need the same extents")
    }
    # convert to matrix
    rrobs <-  terra::as.matrix(rrobs)
    rrsim <- terra::as.matrix(rrsim)
  }

  # set NA to zero here to avoid missing calculations where one is NA, other has value
  rrobs[is.na(rrobs)] <- 0
  rrsim[is.na(rrsim)] <- 0

  # hit calculations
  ind_2d_wet <- which(rrobs>0)
  ind_2d_dry <- which(is.na(rrobs) | rrobs==0)
  ind_1d_wet <- which(rrsim>0)
  ind_1d_dry <- which(is.na(rrsim) | rrsim==0)

  P <- length(ind_2d_wet)
  N <- length(ind_2d_dry)
  PP <- length(ind_1d_wet)
  PN <- length(ind_1d_dry)
  TP <- length(which(ind_1d_wet %in% ind_2d_wet))
  TN <- length(which(ind_1d_dry %in% ind_2d_dry))
  FN <- length(which(ind_1d_dry %in% ind_2d_wet))
  FP <- length(which(ind_1d_wet %in% ind_2d_dry))
  TPR <- TP/P
  TNR <- TN/N
  PPV <- TP/PP
  NPV <- TN/PN
  FNR <- FN/P
  FPR <- FP/N
  FOR <- FN/PN
  FDR <- FP/PP

  # various scores of interest
  BA <- (TPR+TNR)/2
  F1 <- 2*PPV*TPR/(PPV+TPR)
  FM <- sqrt(PPV*TPR)
  MCC <- sqrt(TPR*TNR*PPV*NPV)-sqrt(FNR*FPR*FPR*FDR)
  TS <- TP/(TP+FN+FP)
  HITRATE_WING <- TP / (TP + FN)
  FAR_WING <- FP / (TP + FN)
  CSI_WING <- TP / (TP + FN + FP)
  EB_WING <- FP/FN # error bias, as defined in Wing 2017

  return(data.frame(TP,TN,FN,FP,BA,F1,FM,MCC,TS,HITRATE_WING,FAR_WING,CSI_WING,EB_WING))
}


### ~~~~~~~~~~~~~~~~~~~~~~~~~
### GEOSPATIAL UTILITIES ----
### ~~~~~~~~~~~~~~~~~~~~~~~~~

#' @title Sample points along a spatial line
#'
#' @description
#' Samples regularly-spaced points along a polyline
#'
#' @param lineshp line as sf object on which to generate points
#' @param p1 a point to start measuring from as sf object (optional)
#' @param p2 a point to measure to as sf object (optional)
#' @param snapdist distance to use in snapping points to lineshp (if snapdist is not \code{NA})
#'
#' @return {Returns distance in metres from p1 to p2, or of entire lineshp if no points provided}
#'
#' @details
#' The points are snapped to the line only if the \code{snapdist} parameter is not provided as \code{NA},
#' otherwise they are snapped with \code{snapdist} as a tolerance (in metres).
#'
#' If \code{p1} and \code{p2} are not provided, then the length of the line is returned without splitting.
#' Note that both \code{p1} and \code{p2} must be provided in order to split the line. If you wish to use a
#' single point and compute that distance, consider passing either p1 or p2 as an endpoint determined by
#' \code{st_cast(lineshp, "POINT")} and passing the appropriate endpoint.
#'
#' @examples
#' library(sf)
#' rivershp <- sf::read_sf(system.file("extdata", "riverline_NB.shp", package="blackbird"))
#' rivershp <- bb_preprocess_rivershp(rivershp, return_shp=TRUE)
#'
#' nodes <- bb_sample_linepoints(st_zm(rivershp), pointdist=200)
#'
#' bb_calc_linedist(rivershp, p1=nodes[2,], p2=nodes[3,])
#'
#' @importFrom sf as write_sf st_as_sf st_length st_cast st_snap st_buffer st_collection_extract st_intersects
#' @importFrom lwgeom st_split
#' @export bb_sample_linepoints
bb_calc_linedist <- function(lineshp=NULL, p1=NULL, p2=NULL, snapdist=NA) {

  if (is.null(lineshp)) {
    stop("lineshp is a required argument")
  }

  if ("sf" %notin% class(lineshp)) {
    stop("lineshp should be of class sf")
  }

  if (!is.null(p1) & "sf" %notin% class(p1)) {
    stop("p1 should be of class sf")
  }

  if (!is.null(p2) & "sf" %notin% class(p1)) {
    stop("p2 should be of class sf")
  }

  if (!is.null(p1) & is.null(p2) | !is.null(p2) & is.null(p1)) {
    stop("p1 and p2 must both be provided to measure distance.")
  }

  # basic checks and processing
  lineshp <- st_zm(lineshp)
  if ("geometry" %notin% names(lineshp)) {
    st_geometry(lineshp) <- "geometry"
  }
  if (!is.null(p1) & "geometry" %notin% names(p1)) {
    st_geometry(p1) <- "geometry"
  }
  if (!is.null(p2) & "geometry" %notin% names(p2)) {
    st_geometry(p2) <- "geometry"
  }

  # are lines on same rivershp segment?
  if (nrow(lineshp) > 1) {
    # print("TO DO")
    warning("multiple features in lineshp, TO DO - bb_calc_linedist. Won't work for defined points")
  }

  # plot(lineshp$geometry)
  # plot(p1$geometry, add=TRUE,col='red')
  # plot(p2$geometry, add=TRUE,col='blue')
  # plot(temp, add=TRUE, lwd=4)

  if (is.null(p1) & is.null(p2)) {
     result <- st_zm(lineshp) %>%
      st_length() %>%
      as.numeric() %>%
       sum()
  } else {

    # general snap of points
    ## neglect, assuming that points provided are already generated from bb_sample_linepoints
    if (!is.na(snapdist)) {

      if (!st_intersects(lineshp, p1, sparse=FALSE)) {
        p1 <- st_snap(p1,lineshp,tolerance=snapdist)
      }
      if (!st_intersects(lineshp, p2, sparse=FALSE)) {
        p2 <- st_snap(p2,lineshp,tolerance=snapdist)
      }
    }

    # snap points
    tol = 1e-6
    base::units(tol)="m"
    p1_snap <- st_cast(st_buffer(p1$geometry, tol+st_distance(p1$geometry,lineshp$geometry)),"LINESTRING") %>%
      st_as_sf()
    p2_snap <- st_cast(st_buffer(p2$geometry, tol+st_distance(p2$geometry,lineshp$geometry)),"LINESTRING") %>%
      st_as_sf()

    # check if intersects
    if (!st_intersects(p1_snap, lineshp, sparse=FALSE)){stop("check that p1 is close to lineshp, failed to snap")}
    if (!st_intersects(p2_snap, lineshp, sparse=FALSE)){stop("check that p2 is close to lineshp, failed to snap")}

    # split parts
    # splits into 3 parts each time - first segment, a buffer segment that is vanishing small, and a third segment
    # on the other side of the intersecting point
    parts = st_collection_extract(st_split(lineshp$geometry, p1_snap$x),"LINESTRING") %>%
      st_as_sf()

    # if (nrow(parts) > 3) {
    #   stop("split resulted in many linestrings, something went wrong")
    # }


    # nextpart <- NA
    # # check which line segment is between p1 and p2
    # ## check which segment intersects with p2, and take that one
    # ## the next segment will be either 1 or 3, the opposite of which is grabbed now
    # if (st_intersects(p2_snap, parts[1,], sparse=FALSE)) {
    #   parts <- parts[1,]
    #   nextpart <- 3
    # } else if (st_intersects(p2_snap, parts[3,], sparse=FALSE)) {
    #   parts <- parts[3,]
    #   nextpart <- 1
    # } else {
    #   stop("error in splitting line, neither segment intersects with p2")
    # }

    parts2 = st_collection_extract(st_split(parts$x, p2_snap$x),"LINESTRING") %>%
      st_as_sf()

    # check_intersects <- function(x) {
    #   if (st_intersects(x,p1_snap,sparse=FALSE) & st_intersects(x,p2_snap,sparse=FALSE)) {
    #     return(TRUE)
    #   }
    # }

    ## check if any segments intersect with both on their own
    returnshp <- NULL
    for (i in 1:nrow(parts2)) {
      if (st_intersects(parts2[i,],p1_snap,sparse=FALSE) & st_intersects(parts2[i,],p2_snap,sparse=FALSE)) {
        returnshp <- parts2[i,]
        break
      }
    }

    lentol <- 1e-2

    ## check for 5 part split - three big segments, 2 mini segments sandwiched in
    if (is.null(returnshp)) {
      if (nrow(parts2) == 5 & as.numeric(st_length(parts[2,])) < lentol & as.numeric(st_length(parts[4,])) < lentol) {
        temp <- st_union(parts2[2:4,])
        if (st_intersects(temp,p1_snap,sparse=FALSE) & st_intersects(temp,p2_snap,sparse=FALSE)) {
          returnshp <- temp
        }
      }
    }

    ## check if combining other parts somehow merges into 1
    if (is.null(returnshp)) {
      # plengths <- as.numeric(st_length(parts2))
      for (i in 2:(nrow(parts2)-1)) {
        checklen <- which(as.numeric(st_length(parts2[c((i-1),(i+1)),])) < lentol)

        if (length(checklen) != 0) {
          temp <- bb_utility_merge_lineshp(st_union(parts2[c((i-1),(i+1))[checklen],], parts2[i,]))

          if (st_intersects(temp,p1_snap,sparse=FALSE) & st_intersects(temp,p2_snap,sparse=FALSE)) {
            returnshp <- temp
          }
        }
        if (!is.null(returnshp)) {break}
      }
    }

    if (is.null(returnshp)) {
      warning("Some error in intersecting points against lineshp, returning NULL")
      result <- NULL
    } else {
      # get distance between points
      result <- st_length(returnshp) %>%
        as.numeric()
    }
  }
  return(result)
}

#' @title Get epsg code of spatial object
#'
#' @description
#' Wrapper for getting the epsg code from sf objects with the \code{sf::st_crs} function.
#'
#' @param x sf object with geometry definition (named geometry)
#'
#' @return \item{epsg}{coordinate reference system EPSG identifier}
#'
#'
#' @examples
#' library(sf)
#' rivershp <- sf::read_sf(system.file("extdata", "riverline_NB.shp", package="blackbird"))
#'
#' epsg(rivershp)
#'
#' @importFrom sf st_crs
epsg <- function(x) {
  st_crs(x$geometry)$epsg
}


#' @title Sample points along a spatial line
#'
#' @description
#' Samples regularly-spaced points along a polyline
#'
#' @param lineshp line as sf object on which to generate points
#' @param pointdist distance between generated points in metres
#' @param writefile file name to write sf point object to (optional)
#' @param overwrite whether to overwrite the writefile if it already exists
#'
#' @return \item{TRUE}{returns \code{TRUE} if executed successfully}
#'
#' @details
#' The sf object is only written to disk if the provided \code{writefile} argument is not \code{NA}.
#' If it is provided, the function returns \code{TRUE}. Otherwise, an \code{sf} object containing new points is returned.
#'
#'
#' @examples
#' library(sf)
#' rivershp <- sf::read_sf(system.file("extdata", "riverline_NB.shp", package="blackbird"))
#'
#' nodes <- bb_sample_linepoints(lineshp=rivershp, pointdist=200)
#'
#'
#' @importFrom sf as write_sf st_as_sf st_length st_line_sample st_geometry st_cast
#' @importFrom sp spsample
#' @export bb_sample_linepoints
bb_sample_linepoints <- function(lineshp=NULL, pointdist=100, firstpoint=FALSE, lastpoint=FALSE,
                                 min_end_offset=0,
                                 writefile=NA, overwrite=TRUE) {

  # @importFrom rgeos gLength

  if (is.null(lineshp)) {
    stop("lineshp is a required argument")
  }

  if ("sf" %notin% class(lineshp)) {
    stop("lineshp should be of class sf")
  }

  if (nrow(lineshp) >1) {
    stop("multiple features in lineshp, you must use bb_sample_linepoints_multiple")
  }

  # TO DO - add parameter for points density
  # detect raster resolution and have default point density to match raster resolution
  ## e.g. 10m raster gets point density of every 10m, since more is not helpful

  # sp and rgeos approach - works ok for multipolyline
  # lineshp2 <- as(st_zm(lineshp), "Spatial")
  # numOfPoints  <-  rgeos::gLength(lineshp2) / pointdist
  # linepoints <- sp::spsample(lineshp2, n = numOfPoints, type = "regular")
  # linepoints <- st_as_sf(linepoints)

  # sf approach
  lineshp_length <- as.numeric(st_length(lineshp))
  lineshp2 <- st_zm(lineshp)
  # numOfPoints  <-  ceiling(as.numeric(st_length(lineshp2)) / pointdist)
  linepoints <- st_line_sample(x=lineshp2, density=1/pointdist)
  linepoints <- st_cast(linepoints, "POINT")
  if (length(linepoints)<=1) {
    # get halfway point if nothing sampled
    linepoints <- st_line_sample(x=lineshp2, density=1/pointdist, sample=0.5)
    linepoints <- st_cast(linepoints, "POINT")
  }
  linepoints <- st_as_sf(linepoints)
  linepoints$geometry <- linepoints$x
  st_geometry(linepoints) <- "geometry"
  linepoints$pointid <- seq(1,nrow(linepoints))
  linepoints <- linepoints[,c("pointid","geometry")]

  if (firstpoint) {
    sample_coord <- min_end_offset/lineshp_length # offset from endpoint by set distance, scaled 0-1
    newpoints <- st_line_sample(x=lineshp2, density=1/pointdist, sample=c(sample_coord))
    newpoints <- st_cast(newpoints, "POINT")
    newpoints <- st_as_sf(newpoints)
    newpoints$geometry <- newpoints$x
    st_geometry(newpoints) <- "geometry"
    newpoints$pointid <- 0
    newpoints <- newpoints[,c("pointid","geometry")]

    linepoints <- rbind(newpoints, linepoints)
    linepoints$pointid <- seq(1,nrow(linepoints))
  }

  if (lastpoint) {
    sample_coord <- 1 - min_end_offset/lineshp_length # offset from endpoint by set distance, scaled 0-1
    newpoints <- st_line_sample(x=lineshp2, density=1/pointdist, sample=c(sample_coord))
    newpoints <- st_cast(newpoints, "POINT")
    newpoints <- st_as_sf(newpoints)
    newpoints$geometry <- newpoints$x
    st_geometry(newpoints) <- "geometry"
    newpoints$pointid <- nrow(linepoints)+1
    newpoints <- newpoints[,c("pointid","geometry")]
    linepoints <- rbind(linepoints, newpoints)
    linepoints$pointid <- seq(1,nrow(linepoints))
  }

  ## add conversion for multilinestring to single linestring
  # newlineshp <- lineshp %>%
  #   st_zm() %>%
  #   st_cast("LINESTRING")
  #
  # glen <- newlineshp %>%
  #   st_length() %>%
  #   sum() %>%
  #   as.numeric()
  #
  # sampling_seq <- seq(from=pointdist,glen, by=pointdist)
  #
  # if (firstpoint) {
  #   sampling_seq <- c(0,sampling_seq)
  # }
  # if (lastpoint) {
  #   sampling_seq <- c(sampling_seq,glen)
  # }
  #
  # linepoints <-  newlineshp %>%
  #   sf::st_line_sample(x=newlineshp$geometry, sample = sampling_seq/glen) %>%
  #   st_as_sf() %>%
  #   st_cast(to="POINT")
  #
  # nrow(linepoints)
  #
  # plot(linepoints)


  if (!is.na(writefile)) {

    st_write(linepoints, writefile, overwrite=overwrite)

    message("sampled points written to file")
    return(TRUE)
  } else {
    return(linepoints)
  }
}

#' @title Sample points along multiple spatial lines
#'
#' @description
#' Samples regularly-spaced points along a set of sf lines
#'
#' @param lineshp line(s) as sf object on which to generate points
#' @param pointdist distance between generated points in metres
#' @param firstpoint include the first point on the line in the sample (boolean)
#' @param lastpoint include the last point on the line in the sample (boolean)
#' @param writefile file name to write sf point object to (optional)
#' @param add_info_cols character vector of column names from lineshp to include with each set of point features
#' @param overwrite whether to overwrite the writefile if it already exists
#'
#' @return \item{TRUE}{returns \code{TRUE} if executed successfully}
#'
#' @details
#' The sf object is only written to disk if the provided \code{writefile} argument is not \code{NA}.
#' If it is provided, the function returns \code{TRUE}. Otherwise, an \code{sf} object containing new points is returned.
#'
#' \code{add_info_cols} is used to associate the river reach IDs (reachID column for
#' routing product input shapefile data) to the sampled linepoints. If left as NULL,
#' the sampled points will only contain geometric data.
#'
#' @examples
#' library(sf)
#' rivershp <- sf::read_sf(system.file("extdata", "nithburg_river_EPSG3161.shp", package="blackbird"))
#'
#' nodes <- bb_sample_linepoints_multiple(lineshp=rivershp, pointdist=200)
#'
#'
#' @importFrom sf as write_sf st_as_sf st_length st_line_sample st_geometry st_cast
#' @importFrom sp spsample
#' @export bb_sample_linepoints_multiple
bb_sample_linepoints_multiple <- function(lineshp=NULL, pointdist=100, firstpoint=FALSE, lastpoint=FALSE,
                                          min_end_offset=0,
                                 writefile=NA, add_info_cols=NULL, overwrite=TRUE) {

  # lineshp=rivershp
  # pointdist=1000
  # firstpoint=TRUE
  # lastpoint=TRUE
  # min_end_offset=12
  # writefile=NA
  # add_info_cols=NULL
  # overwrite=TRUE

  # @importFrom rgeos gLength

  if (is.null(lineshp)) {
    stop("lineshp is a required argument")
  }

  if ("sf" %notin% class(lineshp)) {
    stop("lineshp should be of class sf")
  }

  if (nrow(lineshp) < 1) {
    stop("no features detected in lineshp")
  }

  # setup features
  linepoints_all <- st_sf(st_sfc())
  st_crs(linepoints_all) <- st_crs(lineshp)

  # check add_info_cols
  if (!is.null(add_info_cols)) {
    if (any(add_info_cols %notin% colnames(lineshp))) {
      warning("The following columns not found in lineshp:\n",
              paste0(add_info_cols[which(add_info_cols %notin% colnames(lineshp))]),sep="\n")
      add_info_cols <- add_info_cols[which(add_info_cols %in% colnames(lineshp))]
      if (length(add_info_cols) == 0) {
        add_info_cols <- NULL
        warning("No matching columns found in add_info_cols and lineshp. No attribute data will be assigned to sampled points.")
      }
    }
  }

  # TO DO - replace with more clever lapply or similar for sf

  for (i in 1:nrow(lineshp)) {

    lineshp_segment <- bb_utility_merge_lineshp(lineshp[i,])
    # lineshp_segment <- st_line_sample(lineshp[i,])

    linepoints <- bb_sample_linepoints(lineshp=lineshp_segment,
                    pointdist=pointdist,
                    firstpoint=firstpoint,
                    lastpoint=lastpoint,
                    min_end_offset=min_end_offset,
                    writefile=NA,
                    overwrite=overwrite)

    ## add properties to linepoints if add_cols is not NULL
    if (!is.null(add_info_cols)) {
      for (j in 1:length(add_info_cols)) {
        linepoints[[add_info_cols[j]]] <- lineshp[i, add_info_cols[j]][[add_info_cols[j]]]
      }
    }

    linepoints_all <- rbind(linepoints_all,
                            linepoints)
  }

  # remove duplicates - actual duplicates or is there rounding error?
  # linepoints <-
  #   linepoints_all %>%
  #   dplyr::distinct(geometry, .keep_all = TRUE)
  linepoints <- linepoints_all

  # assign pointid to each
  linepoints$pointid <- seq(1,nrow(linepoints))


  ## attempts at using lapply instead of loop
  ## https://stackoverflow.com/questions/48505551/use-apply-with-a-simple-features-sf-function

  # lapply_sf <- function(sf, ...){
  #   ls_sf <- lapply(1:nrow(sf), function(x, sf) {sf[x,]}, sf)
  #   result <- lapply(ls_sf, bb_sample_linepoints, ...)
  #   return(result)
  # }
  #
  # temp <- lapply_sf(lineshp)
  #
  # apply(lineshp, 1, bb_sample_linepoints,
  #                 pointdist=poinstdist,
  #                 firstpoint=firstpoint,
  #        lastpoint=lastpoint,
  #        writefile=NA,
  #        overwrite=overwrite)

  if (!is.na(writefile)) {

    st_write(linepoints, writefile, overwrite=overwrite)

    message("sampled points written to file")
    return(TRUE)
  } else {
    return(linepoints)
  }
}


#' @title Recast hydrologically-conditioned reach points as line feature
#'
#' @description
#' Takes the snapped pour points from the HAND pre-processing, and outputs
#' a hydrologically-corrected river shapefile
#'
#' @param snapped_pourpoints_hand shapefile (or file path) of snapped pour points from HAND pre-processing
#' @param workingfolder folder to write secondary spatial outputs to
#' @param overwrite whether to overwrite the writefile if it already exists
#'
#' @return {Returns an sf object of the hydrologically-corrected river shapefile}
#'
#' @examples
#' print("TO DO XXX")
#'
#'
#' @importFrom sf as write_sf st_as_sf st_length st_line_sample st_geometry st_cast st_crs
#' @export bb_utility_recast_rivershp
bb_utility_recast_rivershp <- function(snapped_pourpoints_hand=NULL, workingfolder=NULL, overwrite=TRUE) {

  # find all items in workingfolder if provided
  if (!is.null(workingfolder)) {
    if (is.null(snapped_pourpoints_hand)) {
      snapped_pourpoints_hand <- bb_get_snappedpourpointshand(workingfolder=workingfolder,returnobject=FALSE)
    }
  }

  # go through items and check
  if (is.null(snapped_pourpoints_hand)) { stop("snapped_pourpoints_hand must be provided")}

  # check that items are appropriate classes, convert to file path
  if (is.character(snapped_pourpoints_hand)) {
    snapped_pourpoints_hand <- sf::read_sf(snapped_pourpoints_hand)
  } else {
    if ("sf" %notin% class(snapped_pourpoints_hand)) {
      stop("snapped_pourpoints_hand must be an sf object or a file path to one")
    }
  }

  points <- snapped_pourpoints_hand$geometry
  n <- length(points) -1

  linestrings <- lapply(X = 1:n, FUN = function(x) {
    pair <- st_combine(c(points[x], points[x + 1]))
    line <- st_cast(pair, "LINESTRING")
    return(line)
  })

  multilinestring <- st_multilinestring(do.call("rbind", linestrings))

  # convert to sf
  hydrivershp <- st_geometry(multilinestring) %>%
    st_as_sf()
  st_geometry(hydrivershp) <- "geometry"

  # add crs
  st_crs(hydrivershp) <- st_crs(snapped_pourpoints_hand)

  # cast to single linestring
  hydrivershp <- bb_utility_merge_lineshp(hydrivershp)

  # save to workingfolder (if provided)
  if (!is.null(workingfolder)) {
    write_sf(hydrivershp,
             bb_get_hydrivershp(workingfolder,returnobject = FALSE),
             delete_dsn = overwrite)
  }

  return(hydrivershp)
}


#' @title Merge linestring to single feature
#'
#' @description
#' Takes an sf linestring (or multilinestring) and combines (merges) it into a single feature,
#' rather than a set of multiple contiguous features.
#'
#' @details
#' Assumes that the sf line feature has points that are overlapping, such that removing duplicates
#' and redrawing the line feature as a LINESTRING will properly recreate the line. If points
#' are offset between MULTILINESTRING features and not overlapping, this function may not create
#' the expected line feature or return an error.
#'
#' @param lineshp shapefile (or file path) of feature to process
#'
#' @return {Returns an sf object of the processed line feature}
#'
#' @examples
#' library(sf)
#' rivershp <- sf::read_sf(system.file("extdata", "riverline_NB.shp", package="blackbird"))
#' bb_utility_merge_lineshp(rivershp)
#'
#' hydrivershp <- bb_get_hydrivershp(workingfolder)
#'
#' @importFrom sf read_sf st_cast st_zm
#' @importFrom dplyr summarise distinct select
#' @export bb_utility_merge_lineshp
bb_utility_merge_lineshp <- function(lineshp=NULL) {

  # TO FIX - can't get the function to keep lineshp as a LINESTRING instead of MULTILINESTRING
  # see vignette for example

  if (is.null(lineshp)) {stop("lineshp is required")}

  if (is.character(lineshp)) {
    lineshp <- sf::read_sf(lineshp)
  } else if ("sf" %notin% class(lineshp)) {
    stop("lineshp must be of class sf")
  }

  if ("geometry" %notin% names(lineshp)) {
    sf::st_geometry(lineshp) <- "geometry"
  }

  # uses the approach from SO here
  ## https://stackoverflow.com/questions/69175360/is-there-a-way-to-merge-contiguous-multilinestrings-sf-object-in-r

  # take one geometry
  # lineshp <- lineshp$geometry %>% st_as_sf()


  ## new solution
  new_lineshp <-
    lineshp %>%
    st_zm() %>%
    st_cast("POINT", warn=FALSE) %>%
    select(geometry) %>%
    dplyr::distinct() %>%
    summarise(do_union=FALSE) %>%
    st_cast("LINESTRING")
    # as("Spatial") %>% head(20)
    # sp::remove.duplicates()

  # head(sf_ml,20)


  ## start of old code using igraph ~~~~

  # @importFrom igraph graph_from_adj_list components

  # sf_ml <- lineshp %>% st_zm() %>% st_cast("LINESTRING")
  #
  # my_idx_touches <- st_touches(sf_ml)
  # my_igraph <- igraph::graph_from_adj_list(my_idx_touches)
  # my_components <- igraph::components(my_igraph)$membership
  #
  # if (any(my_components != 1)) {
  #   warning("discontinuities found in lineshp, returning NULL")
  #   return(NULL)
  # }
  #
  #  sf_ml2 <-  sf_ml %>%
  #   # st_as_sf() %>%
  #   group_by(section = as.character({{my_components}})) %>%
  #   summarise() %>%
  #   select(-section)
  #
  # # set geometry attribute to 'geometry' (instead of x)
  # st_geometry(sf_ml2) <- "geometry"

  # sf_ml2
  # plot(sf_ml2, lwd = 2)

  ## end of old code using igraph ~~~~

  return(new_lineshp)
}


#' @title Simplify river features to main channel
#'
#' @description
#' Takes an sf linestring (or multilinestring) and removes the small segments that are assumed
#' to be just the end forks connecting to other basins.
#'
#' @details
#' TBD
#'
#' @param lineshp shapefile (or file path) of feature to process
#'
#' @return {Returns an sf object of the processed line feature}
#'
#' @examples
#' library(sf)
#' rivershp <- sf::read_sf(system.file("extdata", "riverline_NB.shp", package="blackbird"))
#' bb_utility_chopends_lineshp(rivershp)
#'
#' @importFrom sf read_sf st_cast st_zm
#' @importFrom dplyr summarise distinct select
#' @export bb_utility_merge_lineshp
bb_utility_chopends_lineshp <- function(lineshp=NULL) {

  # TO FIX - can't get the function to keep lineshp as a LINESTRING instead of MULTILINESTRING
  # see vignette for example

  # if (is.null(lineshp)) {stop("lineshp is required")}
  #
  # if (is.character(lineshp)) {
  #   lineshp <- sf::read_sf(lineshp)
  # } else if ("sf" %notin% class(lineshp)) {
  #   stop("lineshp must be of class sf")
  # }
  #
  # if ("geometry" %notin% names(lineshp)) {
  #   sf::st_geometry(lineshp) <- "geometry"
  # }
  #
  # # uses the approach from SO here
  # ## https://stackoverflow.com/questions/69175360/is-there-a-way-to-merge-contiguous-multilinestrings-sf-object-in-r
  #
  # # take one geometry
  # # lineshp <- lineshp$geometry %>% st_as_sf()
  #
  #
  # ## new solution
  #
  # # new_lineshp <-
  #   lineshp %>%
  #   st_zm() %>%
  #   st_cast("POINT", warn=FALSE) %>%
  #   select(geometry) %>%
  #   dplyr::distinct() %>%
  #   summarise(do_union=FALSE) %>%
  #   st_cast("LINESTRING")
  #   # as("Spatial") %>% head(20)
  #   # sp::remove.duplicates()
  #
  # # head(sf_ml,20)
  #
  #
  # ## start of old code using igraph ~~~~
  #
  # # @importFrom igraph graph_from_adj_list components
  #
  # # sf_ml <- lineshp %>% st_zm() %>% st_cast("LINESTRING")
  # #
  # # my_idx_touches <- st_touches(sf_ml)
  # # my_igraph <- igraph::graph_from_adj_list(my_idx_touches)
  # # my_components <- igraph::components(my_igraph)$membership
  # #
  # # if (any(my_components != 1)) {
  # #   warning("discontinuities found in lineshp, returning NULL")
  # #   return(NULL)
  # # }
  # #
  # #  sf_ml2 <-  sf_ml %>%
  # #   # st_as_sf() %>%
  # #   group_by(section = as.character({{my_components}})) %>%
  # #   summarise() %>%
  # #   select(-section)
  # #
  # # # set geometry attribute to 'geometry' (instead of x)
  # # st_geometry(sf_ml2) <- "geometry"
  #
  # # sf_ml2
  # # plot(sf_ml2, lwd = 2)
  #
  # ## end of old code using igraph ~~~~
  #
  # return(new_lineshp)
}

#' @title Match raster extents
#'
#' @description
#' Matches the extent of raster x to raster y
#'
#' @param x raster you wish to adjust the extents of
#' @param y raster you wish to use for matching the extents of
#'
#' @return {processed raster with the extent of y}
#'
#' @examples
#'
#' @importFrom terra ext crop extend
#' @export bb_match_raster_extents
bb_match_raster_extents <- function(x=NULL, y=NULL) {

    if (ext(y) != ext(x)) {
    x <- x %>%
      terra::extend(x=., y=ext(y), fill=NA)  %>%
      terra::crop(x=., y=ext(y))
  }
  return(x)
}

#' @title Match raster extents by resampling
#'
#' @description
#' Matches the extent of raster x to raster y by resampling
#'
#' @param x raster you wish to adjust the extents of
#' @param y raster you wish to use for matching the extents of
#' @param method passed onto \code{terra::resample(method=method)}
#'
#' @return {processed raster with the extent of y}
#'
#' @examples
#'
#' @importFrom terra ext resample
#' @export bb_match_raster_extents_resample
bb_match_raster_extents_resample <- function(x=NULL, y=NULL, method="bilinear") {

  if (ext(y) != ext(x)) {
    x <- x %>%
      resample(x=., y=y, method=method)
  }
  return(x)
}

#' @title Check extents of two rasters
#'
#' @description
#' Check the extents of two rasters
#'
#' @param x first raster you wish to check the extents
#' @param y second raster
#'
#' @return {\code{TRUE} if extents match, else \code{FALSE}}
#'
#' @examples
#'
#' @importFrom terra ext
#' @export bb_check_extents
bb_check_extents <- function(x=NULL, y=NULL) {

  if (ext(y) != ext(x)) {
    return(FALSE)
  } else {
    return(TRUE)
  }
}

#' @title Calculate distance from long/lat
#'
#' @description Calculates distance between points based on a set of long/lat coordinates.
#'
#' @details
#' Calculates distance in metres based on the longitude and latitude of two or more sets of points.
#'
#' The function uses either the Haversine or Vincenty Sphere methods to calculate the distances.
#'
#' @note
#' Function is based on modifications from the \href{https://cran.r-project.org/package=geosphere}{geosphere package}
#' scripts for \code{distHaversine} and \code{distVincentySphere}.
#'
#' @param p1 longitude/latitude of point(s); can be a vector of two numbers, or a matrix of 2 columns (long/lat).
#' @param p2 second point in same format as \code{p1}
#' @param method calculation method as either \code{haversine} (default) or {vincentysphere}
#' @param r radius of the Earth in metres (default 6378137)
#'
#' @return a vector of calculated distances (length of vector based on input)
#'
#' @examples
#' # calculate distance from Engineering 2 (p1) to Graduate House (p2) at the University of Waterloo
#' p1 <- c(-80.5402891965711,43.47088594350457)
#' p2 <- c(-80.54096577853629,43.46976096704924)
#' dist_lonlat(p1, p2)
#'
#' # distance from University of Waterloo to Windsor
#' p2 <- c(-83.02099905916948,42.283371378771555)
#' dist_lonlat(p1, p2)
#'
dist_lonlat <- function(p1, p2, method="haversine", r=6378137) {
  toRad <- pi / 180
  p1 <- matrix(p1,ncol=2) * toRad
  if (missing(p2)) {
    p2 <- p1[-1, ,drop=FALSE]
    p1 <- p1[-nrow(p1), ,drop=FALSE]
  } else {
    p2 <- matrix(p2,ncol=2)  * toRad
  }

  p = cbind(p1[,1], p1[,2], p2[,1], p2[,2], as.vector(r))


  if (tolower(method)=="haversine") {
    dLat <- p[,4]-p[,2]
    dLon <- p[,3]-p[,1]
    a <- (sin(dLat/2))^2 + cos(p[,2]) * cos(p[,4]) * (sin(dLon/2))^2
    # to avoid values of 'a' that are a sliver above 1
    # which may occur at antipodes
    # https://stackoverflow.com/questions/45889616/why-does-disthaversine-return-nan-for-some-pairs-of-coordinates#
    a <- pmin(a, 1)
    dist <- as.vector(2 * atan2(sqrt(a), sqrt(1-a)) * p[,5])
  } else if (tolower(method)=="vincentysphere") {
    lon1 <- p[,1]
    lat1 <- p[,2]
    lon2 <- p[,3]
    lat2 <- p[,4]
    x <- sqrt((cos(lat2) * sin(lon1-lon2))^2 + (cos(lat1) * sin(lat2) - sin(lat1) * cos(lat2) * cos(lon1-lon2))^2)
    y <- sin(lat1) * sin(lat2) + cos(lat1) * cos(lat2) * cos(lon1-lon2)
    dist <- as.vector( p[,5] * atan2(x, y) )
  } else {
    warning(sprintf("dist_lonlat: Unrecognized method %s; should be 'haversine' or 'vincentysphere'.",method))
    dist <- NULL
  }

  return(dist)
}

#' @title Calculate distance from projected points
#'
#' @description Calculates Euclidean distance between points.
#'
#' @param p1 point or set of points as sf object
#' @param p2 second point in same format as \code{p1}
#'
#' @return a vector of calculated distances (length of vector based on input)
#'
#' @examples
#' # xxx to do
#'
dist_projected <- function(p1, p2) {
  # uses euclidean distance to compute distance between projected points
  x1 <- st_coordinates(p1)
  x2 <- st_coordinates(p2)
  dist <- sqrt((x1[,1]-x2[,1])^2+(x1[,2]-x2[,2])^2)
  return(dist)
}

#' @title Calculate distance from coordinates
#'
#' @description Calculates Euclidean distance between coordinates supplied explicitly.
#'
#' @param x1 xy coordinates of point 1
#' @param x2 xy coordinates of point 2
#'
#' @return a vector of calculated distances (length of vector based on input)
#'
#' @examples
#' # xxx to do
#'
dist_coordinates <- function(x1, x2) {
  # uses euclidean distance to compute distance between projected points
  # x1 <- st_coordinates(p1)
  # x2 <- st_coordinates(p2)
  dist <- sqrt((x1[,1]-x2[,1])^2+(x1[,2]-x2[,2])^2)
  return(dist)
}

#' @title EPSG code used for area calculations
#'
#' Returns the EPSG code that is used in reprojecting rasters for area calculations
#'
#' @export bb_crs4calc
bb_crs4calc <- function() {
  # EPSG code of projection used in calculating areas
  # corresponds to WGS 84
  return(4326)
}

#' @title Create a blank sf object
#'
#' Returns a blank sf class object, used in initializing some blackbird classes
#'
#' @importFrom sf st_sf
#' @importFrom sfc st_sfc
#' @export bb_blanksf
bb_blanksf <- function() {
  return(st_sf(st_sfc()))
}

#' @title Get dem resolution
#'
#' Returns resolution of the dem raster as a numeric (single value, assumes square).
#'
#' @importFrom terra res
#' @export bb_get_demres
bb_get_demres <- function(bbopt) {
  dem <- bb_get_demraster(workingfolder=bbopt$workingfolder,returnobject = TRUE)
  return(terra::res(dem)[1])
}

#' @title Compare netcdf input file
#'
#' @description
#' Compares the input netcdf file to the contents of the workingfolder to evlaute consistency.
#'
#' @return {returns \code{TRUE} if run successfully}
#'
#' @importFrom terra ext res
#' @importFrom ncdf4 ncdim_def ncvar_def nc_create ncvar_put ncatt_put nc_close
#' @importFrom sf st_crs
#' @export bb_compare_netcdf
bb_compare_netcdf <- function(bbopt=NULL, filename="bb_ncdf.nc", workingfolder=NULL) {
  ncfile <- nc_open(paste0(workingfolder, "/", filename))

  # Function to compare data, handling missing values
  compare_matrices <- function(original, nc_data, var_name) {
    # Replace NA with missing value for comparison
    original[is.na(original)] <- mv
    nc_data[is.na(nc_data)] <- mv

    if (identical(t(original), nc_data)) {
      cat(sprintf("✅ %s matches NetCDF data!\n", var_name))
    } else {
      cat(sprintf("❌ %s does NOT match NetCDF data!\n", var_name))
      print(summary(as.vector(t(original) - nc_data)))  # Print summary of differences
    }
  }

  # Define missing value from NetCDF file
  mv <- -999

  easting_vals <- ncvar_get(ncfile, "easting")
  northing_vals <- ncvar_get(ncfile, "northing")
  depth_vals <- ncvar_get(ncfile, "depth")  # May not exist for some variables

  c_from_s_nc <- ncvar_get(ncfile, "c_from_s")
  c_from_s_raster <- blackbird::bb_get_catchmentsfromstreamnodesraster(workingfolder)
  c_from_s_data <- as.matrix(c_from_s_raster, wide=TRUE)
  rm(c_from_s_raster)
  compare_matrices(c_from_s_data, c_from_s_nc, "c_from_s")
  rm(c_from_s_data)
  rm(c_from_s_nc)

  hand_nc <- ncvar_get(ncfile, "hand")
  hand_raster <- blackbird::bb_get_handraster(workingfolder)
  hand_data     <- as.matrix(hand_raster, wide=TRUE)
  rm(hand_raster)
  compare_matrices(hand_data, hand_nc, "hand")
  rm(hand_data)
  rm(hand_nc)

  handid_nc <- ncvar_get(ncfile, "handid")
  handid_raster <- blackbird::bb_get_handpourpointIDraster(workingfolder)
  handid_data   <- as.matrix(handid_raster, wide=TRUE)
  rm(handid_raster)
  compare_matrices(handid_data, handid_nc, "handid")
  rm(handid_data)
  rm(handid_nc)

  # should get from bbopt ideally
  if (bbopt$use_dhand) {
    depths <- bbopt$dhand_Hseq #  seq(0, 9, 0.05)
    nd <- length(depths)

    # not sure best way to handle here if DHAND not used, just skip?
    dhand_nc <- ncvar_get(ncfile, "dhand")
    dhandid_nc <- ncvar_get(ncfile, "dhandid")

    nc_close(ncfile)

    for (i in seq_along(depths)) {
      depth_str <- sprintf("%.2f", depths[i])
      dhand_raster <- blackbird::bb_get_dhandraster(workingfolder, depth=depths[i], filetype="depthraster")
      dhand_data <- as.matrix(dhand_raster, wide=TRUE)
      rm(dhand_raster)
      compare_matrices(dhand_data, dhand_nc[,,i], paste("dhand at depth", depth_str))
      rm(dhand_data)

      dhandid_raster <- blackbird::bb_get_dhandraster(workingfolder, depth=depths[i], filetype="idraster")
      dhandid_data <- as.matrix(dhandid_raster, wide=TRUE)
      rm(dhandid_raster)
      compare_matrices(dhandid_data, dhandid_nc[,,i], paste("dhandid at depth", depth_str))
      rm(dhandid_data)
    }
    rm(dhand_nc)
    rm(dhandid_nc)
  }

  cat("Verification complete.\n")
  return(TRUE)
}

#' @title Calculate adjacent cells of a raster
#' @details
#' Used in downslope tracing algorithm for fuzzy HAND
#' @param x cells (by index) to evaluate adjacent cells of
#' @param rr raster to evaluate
#' @param globalsearch boolean for whether searching globally
#' @return {matrix of adjacent cell indices for each x}
#' @seealso this [Stack Overflow thread](https://stackoverflow.com/questions/56771620/get-values-from-adjacent-cells-raster-r)
#' @noRd
adj <- function(x, rr, globalsearch) {
  nc <- ncol(rr)
  ngb <- c(-nc-1, -nc, -nc+1, -1, +1, +nc-1, +nc, +nc+1)
  # x is cell ID, ngb=nbrs
  a <- x + ngb
  # the below line is not strictly needed, so it should be faster without it
  # a[a < 1 | a > ncell(rr)] <- NA
  if (!globalsearch) {
    col <- colFromCell(rr, x)
    if (col == 1) {
      a[c(1,4,6)] <- NA
    } else if (col==nc) {
      a[c(3,5,8)] <- NA
    }
  }
  a
}

#' @title Calculate downslope DInf weight vector
#' @details
#' Used in downslope tracing algorithm for fuzzy HAND.
#' Requires to be used with \code{downslope_dinf_ind} to get direction of two weights
#' @param dir numeric direction [0..360]
#' @param ww weight to apply to cell (default 1.0)
#' @return {vector of two weights}
#' @seealso this [Stack Overflow thread](https://stackoverflow.com/questions/56771620/get-values-from-adjacent-cells-raster-r)
#' @noRd
downslope_dinf <- function(dir,ww=1) {
  ccdirs <- c("N", "NE", "E", "SE","S","SW","W","NW","N")
  ccvec <- seq(0,360,45)
  if (dir!=360) {
    ind <- c(which(ccvec > dir)[1]-1,which(ccvec > dir)[1])
    w1 <- (ccvec[ind[2]]-dir)/45*ww
    w2 <- (dir-ccvec[ind[1]])/45*ww
    # cc <- ccdirs[ind]
  } else {
    w1=ww
    w2=0
    ind <- c(1,2)
  }
  df <- data.frame(t(c(w1,w2)))
  return(df)
}

#' @title Calculate downslope DInf weight vector direction
#' @details
#' Used in downslope tracing algorithm for fuzzy HAND.
#' Requires to be used with \code{downslope_dinf} to get weights of vector
#' @param dir numeric direction [0..360]
#' @return {vector of two index directions}
#' @seealso this [Stack Overflow thread](https://stackoverflow.com/questions/56771620/get-values-from-adjacent-cells-raster-r)
#' @noRd
downslope_dinf_ind <- function(dir) {
  ccdirs <- c("N", "NE", "E", "SE","S","SW","W","NW","N")
  ccvec <- seq(0,360,45)
  if (dir!=360) {
    ind <- c(which(ccvec > dir)[1]-1,which(ccvec > dir)[1])
  } else {
    ind <- c(1,2)
  }
  # correct ind to N
  ind[which(ind>8)] <- ind[which(ind>8)]-8
  df <- data.frame(t(ind))
  return(df)
}


#' @title Checks reachID on nearest rivershp for snapped_streamnodes
#' @details
#' Used in the bb_preprocess_streamnodes_forcatchments as part of iterative snapping
#' Returns the index ID of snapped_streamnodes where the reachID has changed,
#' indicating that the snap distance provided by \code{bbopt$pourpoint_snapdist} has allowed
#' the point to erroneously snap to another river branch
#' @param snapped_streamnodes snapped_streamnodes
#' @param rivershp rivershp
#' @return {index of snapped_streamnodes that have a changed reachID, \code{numeric(0)} if none}
#' @noRd
check_reachIDs = function(snapped_streamnodes, rivershp) {
  nearest_ids <- st_nearest_feature(snapped_streamnodes, rivershp)
  snapped_streamnodes$reachID_check <- rivershp[nearest_ids,]$reachID
  if (any(snapped_streamnodes$reachID != snapped_streamnodes$reachID_check) ) {
    message("Some nodes have new reachIDs, reducing snap distance for some streamnodes")
    return(which(snapped_streamnodes$reachID != snapped_streamnodes$reachID_check))
  } else {
    return(numeric(0))
  }
}


#' @title Checks sf geometry types and drop inconsistent geometries
#' @details
#' Useful in checking an sf object to ensure all geometries are of the same type,
#' especially after an intersection or other function that may produce
#' different geometry types than the initial base object. Used in the
#' \code{\link{bb_preprocess_generate_rivershp}} function after
#' an \code{\link{st_intersection}} operation.
#' @param x sf object
#' @param keepclass geometry type to keep, all other will be dropped (if any)
#' @return {x sf object with only the keepclass geometry types}
#' @importFrom sf st_geometry_type
bb_drop_inconsistent_geomtypes = function(x, keeptype="LINESTRING") {
  tt <- st_geometry_type(x)
  ind <- which(tt != keeptype)
  message(sprintf("dropping %i geometries with bb_drop_inconsistent_geomtypes", length(ind)))
  if (length(ind)>0) {
    x <- x[-ind,]
  }
  return(x)
}



### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### BLACKBIRD SUPPORT UTILITIES ----
### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' @title Generate supported boundary condition types
#'
#' Generates the vector of supported boundary condition methods (\code{bb_boundarycondition$bctype})
#'
#' @return {supported methods for option \code{bb_boundarycondition$bctype}}
#'
#' @export bb_get_boundary_types
bb_get_boundary_types <- function() {
  return(c("normal_depth","set_wsl","set_depth"))
}

#' @title Generate supported average friction slope methods
#'
#' Generates the vector of supported methods for calculating average friction slope (Sf_Avg) values (\code{bbopt$friction_slope_method})
#'
#' @return {supported methods for option \code{bbopt$friction_slope_method}}
#'
#' @export bb_get_friction_methods
bb_get_friction_methods <- function() {
  return(c("average_conveyance","average_friction","geometric_friction",
           "harmonic_friction","reach_friction"))
}

#' @title Generate supported Manning composite methods
#'
#' Generates the vector of supported methods for calculating composite Manning's roughness coefficient (\code{bbopt$Manning_composite_method})
#'
#' @return {supported methods for option \code{bbopt$Manning_composite_method}}
#'
#' @export bb_get_composite_Manning_methods
bb_get_composite_Manning_methods <- function() {
  return(c("equal_force","weighted_average_area","weighted_average_wetperimeter",
           "weighted_average_conveyance","equal_velocity","blended_nc"))
}

#' @title Generate supported Cross-section conveyance methods
#'
#' Generates the vector of supported methods for calculating conveyance in cross-section (xsection) class streamnodes.
#'
#' @return {supported methods for option \code{bbopt$xsection_conveyance_method}}
#'
#' @export bb_get_xsection_conveyance_methods
bb_get_xsection_conveyance_methods <- function() {
  # overbank_conveyance - calculate conveyance in LOB, Main, and ROB and sum. Uses single Manning roughness value in each
  # default_conveyance - uses zones grouped by roughness to compute N conveyance terms
  # coordinate_conveyance - uses each set of points in computing N roughness terms, where N is the number of xx coordinates -1
  # discretized_conveyance - discretizes the domain by bbopt$dx and computes properties based on that

  return(c("overbank_conveyance","default_conveyance","coordinate_conveyance","discretized_conveyance",
           "areaweighted_conveyance_onecalc","areaweighted_conveyance")
)
}

#' @title Generate supported catchment conveyance methods
#'
#' Generates the vector of supported methods for calculating conveyance in catchment class streamnodes.
#'
#' @return {supported methods for option \code{bbopt$catchment_conveyance_method}}
#'
#' @export bb_get_catchment_conveyance_methods
bb_get_catchment_conveyance_methods <- function() {
  # overbank_conveyance - calculate conveyance in LOB, Main, and ROB and sum. Uses input raster or buffer parameter to generate the LOB/Main/ROB raster
  # discretized_conveyance - discretizes the domain by each raster in the input dem resolution
  # aggfactor_conveyance - passes a filter to aggregate Area and WetPerimeter by a factor of dem resolution * bbopt$aggfactor_conveyance_factor
  # roughzone_conveyance - sums conveyance after grouping each discontiguous roughness zone and computing conveyance in each.
  #                   number of conveyance pieces N will be equal to the number of unique roughness values in the domain
  # contigroughzone_conveyance - groups by contiguous roughness zones only
  # areaweighted_conveyance_onecalc - sums Ai/ni over the entire area, then multiplies (A/P)^(2/3) (actually Vf/Af as proxy for A/P)

  # currently supported ones

  return(c("discretized_conveyance","areaweighted_conveyance_onecalc",
           "roughzone_conveyance","blended_conveyance"))
}

#' @title Generate supported post-processing interpolation methods
#'
#' @description
#' Generates the vector of supported methods for post-processing and interpolating model results.
#'
#' @details
#' These are the methods that are supported in the \code{bb_options} parameter \code{interpolation_postproc_method}.
#' Supported methods include:
#'
#' 1. catchment-hand - HAND layer with constant depth applied over each catchment (streamnode) area.
#' 2. catchment-dhand - DHAND layers with constant depth applied over each catchment (streamnode) area.
#' 3. interp-hand - HAND layer with depth interpolated along rivershp and applied using HAND.
#' 4. interp-dhand - DHAND layer(s) with depth interpolated along rivershp and applied using DHAND.
#'
#'
#' @return {supported methods for option \code{bbopt$catchment_conveyance_method}}
#'
#' @export bb_get_catchment_conveyance_methods
bb_get_interp_postproc_methods <- function() {
    return(c("catchment-hand","catchment-dhand",
           "interp-hand","interp-dhand","interp-dhand-WSLcorr"))
}

#' @title Generate supported model types methods
#'
#' Generates the vector of supported model types for checking purposes.
#'
#' @return {supported methods for option \code{bbopt$modeltype}}
#'
#' @export bb_get_modeltypes
bb_get_modeltypes <- function() {
  # message("bb_get_modeltypes: note that unsteadyflow is not yet supported.")
  return(c("hand-manning","steadyflow"))
}

#' @title Generate supported catchment (reach) integration methods
#'
#' Generates the vector of supported catchment integration types for checking purposes.
#'
#' @return {supported methods for option \code{bbopt$catchment_integration_method}}
#'
#' @export bb_get_catchmentintegrationtypes
bb_get_catchmentintegrationtypes <- function() {
  return(c("effective_length","reach_length"))
}

#' @title Generate supported regime types
#'
#' Generates the vector of supportedregime types for checking purposes.
#'
#' @return {supported methods for option \code{bbopt$modeltype}}
#'
#' @export bb_get_regimetypes
bb_get_regimetypes <- function() {

  # message("bb_get_modeltypes: note that unsteadyflow is not yet supported.")
  return(c("subcritical","supercritical","mixed"))
}

#' @title Generate empty table for flow input
#'
#' @description
#' Returns an empty table that can be infilled with flow values and provided
#' to \code{\link{bb_hyd_compute_profile}}.
#'
#' @param geometry geom object inputted
#'
#' @return {empty flow table}
#'
#' @examples
#'
#'
#' @export bb_generate_flow_table
bb_generate_flow_table <- function(geometry=NULL) {

  sdf <- geometry$get_streamnodeList_as_dataframe()
  sdf$Flows <- NA

  return(sdf)
}


#' @title Get standard blackbird file information
#'
#' @description
#' Returns the standard file information for blackbird files.
#'
#' @details
#' Used in the \code{\link{bb_get}} functions
#' and other blackbird functions that write standard files to workingfolder.
#'
#' @param objectname name of the object for which to retrieve information
#'
#' @return {Returns a data frame with the objectname, filename, and filetype,
#'   either for a specific objectname or the entire data frame if \code{objectname=NULL}.
#'   Returns \code{NULL} if the objectname is provided but not recognized.}
#'
#' @examples
#' # retrieve the name of the hand raster
#' bb_get_fileinfo("handraster")
#'
#' @export bb_get_fileinfo
bb_get_fileinfo <- function(objectname=NULL) {

  sdf <- data.frame(matrix(ncol=3,nrow=48,byrow=TRUE,
                           data=c(
                             c("rivershp",                    "bb_rivershp.shp","sf"),
                             # c("genrivershp",                 "bb_generated_rivershp.shp","sf"),

                             # c("rivercatch",                  "bb_rivercatch.shp","sf"),
                             c("demraster",                   "bb_dem.tif","SpatRaster"),
                             c("demcondraster",               "bb_dem_conditioned.tif","SpatRaster"),
                             c("dembreachraster",             "bb_dem_breached.tif","SpatRaster"),
                             c("handraster",                  "bb_hand.tif","SpatRaster"),
                             c("flowaccraster",               "bb_flow_accumulation.tif","SpatRaster"),
                             c("flowdirraster",               "bb_flow_direction.tif","SpatRaster"),
                             c("flowdirdinfraster",           "bb_flow_dinf_direction.tif","SpatRaster"),
                             c("sloperaster",                 "bb_slope.tif","SpatRaster"),
                             c("reachlengthraster",           "bb_reach_length.tif","SpatRaster"),

                             c("euclideandistraster",         "bb_euclideandist.tif","SpatRaster" ),
                             c("euclideandistcondraster",     "bb_euclideandist_conditioned.tif","SpatRaster" ),
                             c("euclideandistflowaccraster",  "bb_euclideandist_flow_accumulation.tif","SpatRaster" ),
                             c("euclideandistflowdirraster",  "bb_euclideandist_flow_direction.tif","SpatRaster" ),

                             c("pourpointshandshp",           "bb_pourpoints_hand.shp","sf"),
                             c("snappedpourpointshandshp",    "bb_snapped_pourpoints_hand.shp","sf"),
                             c("handpourpointIDraster",       "bb_hand_pourpoint_id.tiff","SpatRaster"),

                             c("demcoarseraster",             "bb_dem_coarse.tif","SpatRaster"),
                             c("demcoarsecondraster",         "bb_demcoarse_conditioned.tif","SpatRaster"),
                             c("coarseflowaccraster",         "bb_coarse_flow_accumulation.tif","SpatRaster"),
                             c("coarseflowdirraster",         "bb_coarse_flow_direction.tif","SpatRaster"),
                             c("pourpointshydrivershp",           "bb_pourpoints_hydriver.shp","sf"),
                             c("snappedpourpointshydrivershp",    "bb_snapped_pourpoints_hydriver.shp","sf"),

                             c("catchmentshandshp",           "bb_catchmentshand.shp","sf"),
                             c("catchmentshandraster",        "bb_catchmentshand.tif","SpatRaster"),
                             c("zdrainageraster",             "bb_zdrainage.tif","SpatRaster"),
                             c("streamnodesforcatchmentsshp", "bb_streamnodes_forcatchments.shp","sf"),
                             c("snappedstreamnodesforcatchmentsshp",  "bb_snapped_streamnodes_forcatchments.shp","sf"),
                             c("catchmentsfromstreamnodesshp","bb_catchments_fromstreamnodes.shp","sf"),
                             c("xsectionsfromstreamnodesshp", "bb_xsections_fromstreamnodes.shp","sf"),
                             c("catchmentsfromstreamnodesraster","bb_catchments_fromstreamnodes.tif","SpatRaster"),
                             c("manningsnraster",             "bb_manningsn.tif","SpatRaster"),
                             c("hydrivershp",                 "bb_hydcorrected_rivershp.shp","sf"),
                             c("slrivershp",                  "bb_slrivershp.shp","sf"),

                             c("catchmentlistrdata",          "bb_catchmentlist.rds","rds"),
                             c("xsectionlistrdata",           "bb_xsectionlist.rds","rds"),
                             c("geometryrdata",               "bb_geometry.rds","rds"),
                             c("boundaryconditionrdata",      "bb_bc.rds","rds"),
                             c("bboptionsrdata",              "bb_options.rds","rds"),

                             c("hydraulicoutput",             "bb_hydraulic_output.csv","csv"),

                             c("allids",                      "bb_allids_dinf.csv","csv"),
                             c("validcells",                  "bb_validcells_dinf.csv","csv"),
                             c("handfuzzyraster",             "bb_hand_fuzzy.tif","SpatRaster"),
                             c("catchmentsrasterstack",       "bb_catchments_fromstreamnodes_rasterstack.tif","SpatRaster"),

                             c("depthraster",                 "bb_results_depth.tif","SpatRaster"),
                             c("velocityraster",              "bb_results_velocity.tif","SpatRaster"),
                             c("depthxvelocityraster",        "bb_results_depthxvelocity.tif","SpatRaster"),
                             c("hazardraster",                "bb_results_hazards.tif","SpatRaster")
                           )))
  colnames(sdf) <- c("objectname","filename","filetype")

  if (is.null(objectname)) {
    # message("objectname is NULL, returning entire standard file info data frame")
    result <- sdf
  } else if (objectname %in% sdf$objectname) {
    result <- sdf[sdf$objectname == objectname,]
    return(result)
  } else {
    stop(sprintf("objectname %s not recognized",objectname))
  }
  return(result)
}

#' @title Retrieval functions for standard files in the blackbird package
#'
#' @description
#' File retrieval functions for standard files in the blackbird package.
#'
#' @details
#' All functions use the function \code{bb_get_object} to retrieve files/objects.
#' Standard names for blackbird files can be found with a \code{\link{bb_get_fileinfo}}.
#'
#' @param workingfolder the working folder where the file should be found
#' @param returnobject if \code{TRUE} (default), the object is returned directly; otherwise the full file path is returned.
#'
#' @return Returns the object in R or full file path if found, otherwise returns \code{NULL}
#'
#' @name bb_get

#' @importFrom terra rast
#' @importFrom sf read_sf
#' @export
bb_get_object <- function(objectname=NULL, workingfolder=NULL, returnobject=TRUE, include_wf=TRUE) {

  if (is.null(objectname)) {
    stop("objectname is required")
  }

  if (is.null(workingfolder)) {
    stop("Cannot retrieve files when workingfolder is NULL")
  }

  df <- bb_get_fileinfo(objectname)
  if (is.null(df)) {
    stop(sprintf("%s not recognized as objectname in bb_get_fileinfo",objectname))
  }

  # check that file exists
  if (include_wf) {
    tf <- file.path(workingfolder, df$filename)
  } else {
    tf <- df$filename
  }

  result <- tf

  if (!is.null(result)) {
    if (returnobject) {

      if (!file.exists(result)) {
        warning(sprintf("%s not found in file path %s, returning NULL",objectname,tf))
        result <- NULL
      } else {
        if (df$filetype == "SpatRaster") {
          result <- terra::rast(tf)
          # result <- raster::raster(tf)
        } else if (df$filetype == "sf") {
          result <- read_sf(tf)
        } else if (df$filetype == "csv") {
          result <- read.csv(tf)
        } else if (df$filetype == "rds") {
          result <- readRDS(tf)
        } else {
          warning(sprintf("unrecognized file type %s, returning NULL",df$filetype))
          result <- NULL
        }
      }
    }
  }
  return(result)
}

#' @rdname bb_get
bb_get_rivershp <- function(workingfolder=NULL, returnobject=TRUE, include_wf=TRUE) {
  objectname <- "rivershp"
  result <- bb_get_object(objectname, workingfolder, returnobject, include_wf)
  return(result)
}

# #' @rdname bb_get
# bb_get_genrivershp <- function(workingfolder=NULL, returnobject=TRUE, include_wf=TRUE) {
#   objectname <- "genrivershp"
#   result <- bb_get_object(objectname, workingfolder, returnobject, include_wf)
#   return(result)
# }

# #' @rdname bb_get
#' bb_get_rivercatch <- function(workingfolder=NULL, returnobject=TRUE, include_wf=TRUE) {
#'   objectname <- "rivercatch"
#'   result <- bb_get_object(objectname, workingfolder, returnobject, include_wf)
#'   return(result)
#' }

#' @rdname bb_get
bb_get_handraster <- function(workingfolder=NULL, returnobject=TRUE, include_wf=TRUE) {
  objectname <- "handraster"
  result <- bb_get_object(objectname, workingfolder, returnobject, include_wf)
  return(result)
}

#' @rdname bb_get
bb_get_demraster <- function(workingfolder=NULL, returnobject=TRUE, include_wf=TRUE) {
  objectname <- "demraster"
  result <- bb_get_object(objectname, workingfolder, returnobject, include_wf)
  return(result)
}

#' @rdname bb_get
bb_get_demcondraster <- function(workingfolder=NULL, returnobject=TRUE, include_wf=TRUE) {
  objectname <- "demcondraster"
  result <- bb_get_object(objectname, workingfolder, returnobject, include_wf)
  return(result)
}

#' @rdname bb_get
bb_get_dembreachraster <- function(workingfolder=NULL, returnobject=TRUE, include_wf=TRUE) {
  objectname <- "dembreachraster"
  result <- bb_get_object(objectname, workingfolder, returnobject, include_wf)
  return(result)
}

#' @rdname bb_get
bb_get_flowaccraster <- function(workingfolder=NULL, returnobject=TRUE, include_wf=TRUE) {
  objectname <- "flowaccraster"
  result <- bb_get_object(objectname, workingfolder, returnobject, include_wf)
  return(result)
}

#' @rdname bb_get
bb_get_flowdirraster <- function(workingfolder=NULL, returnobject=TRUE, include_wf=TRUE) {
  objectname <- "flowdirraster"
  result <- bb_get_object(objectname, workingfolder, returnobject, include_wf)
  return(result)
}

#' @rdname bb_get
bb_get_flowdirdinfraster <- function(workingfolder=NULL, returnobject=TRUE, include_wf=TRUE) {
  objectname <- "flowdirdinfraster"
  result <- bb_get_object(objectname, workingfolder, returnobject, include_wf)
  return(result)
}

#' @rdname bb_get
bb_get_sloperaster <- function(workingfolder=NULL, returnobject=TRUE, include_wf=TRUE) {
  objectname <- "sloperaster"
  result <- bb_get_object(objectname, workingfolder, returnobject, include_wf)
  return(result)
}

#' @rdname bb_get
bb_get_reachlengthraster <- function(workingfolder=NULL, returnobject=TRUE, include_wf=TRUE) {
  objectname <- "reachlengthraster"
  result <- bb_get_object(objectname, workingfolder, returnobject, include_wf)
  return(result)
}

#' @rdname bb_get
bb_get_euclideandistraster <- function(workingfolder=NULL, returnobject=TRUE, include_wf=TRUE) {
  objectname <- "euclideandistraster"
  result <- bb_get_object(objectname, workingfolder, returnobject, include_wf)
  return(result)
}

#' @rdname bb_get
bb_get_euclideandistcondraster <- function(workingfolder=NULL, returnobject=TRUE, include_wf=TRUE) {
  objectname <- "euclideandistcondraster"
  result <- bb_get_object(objectname, workingfolder, returnobject, include_wf)
  return(result)
}

#' @rdname bb_get
bb_get_euclideandistflowdirraster <- function(workingfolder=NULL, returnobject=TRUE, include_wf=TRUE) {
  objectname <- "euclideandistflowdirraster"
  result <- bb_get_object(objectname, workingfolder, returnobject, include_wf)
  return(result)
}

#' @rdname bb_get
bb_get_euclideandistflowaccraster <- function(workingfolder=NULL, returnobject=TRUE, include_wf=TRUE) {
  objectname <- "euclideandistflowaccraster"
  result <- bb_get_object(objectname, workingfolder, returnobject, include_wf)
  return(result)
}

#' @rdname bb_get
bb_get_pourpointshand <- function(workingfolder=NULL, returnobject=TRUE, include_wf=TRUE) {
  objectname <- "pourpointshandshp"
  result <- bb_get_object(objectname, workingfolder, returnobject, include_wf)
  return(result)
}

#' @rdname bb_get
bb_get_snappedpourpointshand <- function(workingfolder=NULL, returnobject=TRUE) {
  objectname <- "snappedpourpointshandshp"
  result <- bb_get_object(objectname, workingfolder, returnobject)
  return(result)
}

#' @rdname bb_get
bb_get_handpourpointIDraster <- function(workingfolder=NULL, returnobject=TRUE, include_wf=TRUE) {
  objectname <- "handpourpointIDraster"
  result <- bb_get_object(objectname, workingfolder, returnobject, include_wf)
  return(result)
}

#' @rdname bb_get
bb_get_demcoarseraster <- function(workingfolder=NULL, returnobject=TRUE, include_wf=TRUE) {
  objectname <- "demcoarseraster"
  result <- bb_get_object(objectname, workingfolder, returnobject, include_wf)
  return(result)
}

#' @rdname bb_get
bb_get_demcoarsecondraster <- function(workingfolder=NULL, returnobject=TRUE, include_wf=TRUE) {
  objectname <- "demcoarsecondraster"
  result <- bb_get_object(objectname, workingfolder, returnobject, include_wf)
  return(result)
}

#' @rdname bb_get
bb_get_coarseflowaccraster <- function(workingfolder=NULL, returnobject=TRUE, include_wf=TRUE) {
  objectname <- "coarseflowaccraster"
  result <- bb_get_object(objectname, workingfolder, returnobject, include_wf)
  return(result)
}

#' @rdname bb_get
bb_get_coarseflowdirraster <- function(workingfolder=NULL, returnobject=TRUE, include_wf=TRUE) {
  objectname <- "coarseflowdirraster"
  result <- bb_get_object(objectname, workingfolder, returnobject, include_wf)
  return(result)
}

#' @rdname bb_get
bb_get_pourpointshydriver <- function(workingfolder=NULL, returnobject=TRUE, include_wf=TRUE) {
  objectname <- "pourpointshydrivershp"
  result <- bb_get_object(objectname, workingfolder, returnobject, include_wf)
  return(result)
}

#' @rdname bb_get
bb_get_snappedpourpointshydriver <- function(workingfolder=NULL, returnobject=TRUE) {
  objectname <- "snappedpourpointshydrivershp"
  result <- bb_get_object(objectname, workingfolder, returnobject)
  return(result)
}

#' @rdname bb_get
bb_get_catchmentshandshp <- function(workingfolder=NULL, returnobject=TRUE) {
  objectname <- "catchmentshandshp"
  result <- bb_get_object(objectname, workingfolder, returnobject)
  return(result)
}

#' @rdname bb_get
bb_get_catchmentshandraster <- function(workingfolder=NULL, returnobject=TRUE) {
  objectname <- "catchmentshandraster"
  result <- bb_get_object(objectname, workingfolder, returnobject)
  return(result)
}

#' @rdname bb_get
bb_get_zdrainageraster <- function(workingfolder=NULL, returnobject=TRUE) {
  objectname <- "zdrainageraster"
  result <- bb_get_object(objectname, workingfolder, returnobject)
  return(result)
}

#' @rdname bb_get
bb_get_streamnodesforcatchmentsshp <- function(workingfolder=NULL, returnobject=TRUE) {
  objectname <- "streamnodesforcatchmentsshp"
  result <- bb_get_object(objectname, workingfolder, returnobject)
  return(result)
}

#' @rdname bb_get
bb_get_snappedstreamnodesforcatchmentsshp <- function(workingfolder=NULL, returnobject=TRUE) {
  objectname <- "snappedstreamnodesforcatchmentsshp"
  result <- bb_get_object(objectname, workingfolder, returnobject)
  return(result)
}

#' @rdname bb_get
bb_get_catchmentsfromstreamnodesraster <- function(workingfolder=NULL, returnobject=TRUE) {
  objectname <- "catchmentsfromstreamnodesraster"
  result <- bb_get_object(objectname, workingfolder, returnobject)
  return(result)
}

#' @rdname bb_get
bb_get_catchmentsfromstreamnodesshp <- function(workingfolder=NULL, returnobject=TRUE) {
  objectname <- "catchmentsfromstreamnodesshp"
  result <- bb_get_object(objectname, workingfolder, returnobject)
  return(result)
}

#' @rdname bb_get
bb_get_catchmentlistrdata <- function(workingfolder=NULL, returnobject=TRUE) {
  objectname <- "catchmentlistrdata"
  result <- bb_get_object(objectname, workingfolder, returnobject)
  return(result)
}

#' @rdname bb_get
bb_get_xsectionsfromstreamnodesshp <- function(workingfolder=NULL, returnobject=TRUE) {
  objectname <- "xsectionsfromstreamnodesshp"
  result <- bb_get_object(objectname, workingfolder, returnobject)
  return(result)
}

#' @rdname bb_get
bb_get_xsectionlistrdata <- function(workingfolder=NULL, returnobject=TRUE) {
  objectname <- "xsectionlistrdata"
  result <- bb_get_object(objectname, workingfolder, returnobject)
  return(result)
}

#' @rdname bb_get
bb_get_geometryrdata <- function(workingfolder=NULL, returnobject=TRUE) {
  objectname <- "geometryrdata"
  result <- bb_get_object(objectname, workingfolder, returnobject)
  return(result)
}

#' @rdname bb_get
bb_get_boundaryconditionrdata <- function(workingfolder=NULL, returnobject=TRUE) {
  objectname <- "boundaryconditionrdata"
  result <- bb_get_object(objectname, workingfolder, returnobject)
  return(result)
}

#' @rdname bb_get
bb_get_bboptionsrdata <- function(workingfolder=NULL, returnobject=TRUE) {
  objectname <- "bboptionsrdata"
  result <- bb_get_object(objectname, workingfolder, returnobject)
  return(result)
}

#' @rdname bb_get
bb_get_manningsnraster <- function(workingfolder=NULL, returnobject=TRUE) {
  objectname <- "manningsnraster"
  result <- bb_get_object(objectname, workingfolder, returnobject)
  return(result)
}

#' @rdname bb_get
bb_get_hydrivershp <- function(workingfolder=NULL, returnobject=TRUE, include_wf=TRUE) {
  objectname <- "hydrivershp"
  result <- bb_get_object(objectname, workingfolder, returnobject, include_wf)
  return(result)
}

#' @rdname bb_get
bb_get_slrivershp <- function(workingfolder=NULL, returnobject=TRUE, include_wf=TRUE) {
  objectname <- "slrivershp"
  result <- bb_get_object(objectname, workingfolder, returnobject, include_wf)
  return(result)
}

#' @rdname bb_get
bb_get_allids <- function(workingfolder=NULL, returnobject=TRUE, include_wf=TRUE) {
  objectname <- "allids"
  result <- bb_get_object(objectname, workingfolder, returnobject, include_wf)
  return(result)
}

#' @rdname bb_get
bb_get_validcells <- function(workingfolder=NULL, returnobject=TRUE, include_wf=TRUE) {
  objectname <- "validcells"
  result <- bb_get_object(objectname, workingfolder, returnobject, include_wf)
  return(result)
}

#' @rdname bb_get
bb_get_fuzzyhandraster <- function(workingfolder=NULL, returnobject=TRUE, include_wf=TRUE) {
  objectname <- "handfuzzyraster"
  result <- bb_get_object(objectname, workingfolder, returnobject, include_wf)
  return(result)
}

#' @rdname bb_get
bb_get_catchmentstack <- function(workingfolder=NULL, returnobject=TRUE, include_wf=TRUE) {
  objectname <- "catchmentsrasterstack"
  result <- bb_get_object(objectname, workingfolder, returnobject, include_wf)
  return(result)
}

#' @rdname bb_get
bb_get_results_hydraulic_output <- function(workingfolder=NULL, returnobject=TRUE) {
  objectname <- "hydraulicoutput"
  result <- bb_get_object(objectname,
                          file.path(workingfolder, "results"),
                          returnobject)
  return(result)
}

#' @rdname bb_get
bb_get_results_depthraster <- function(workingfolder=NULL, returnobject=TRUE) {
  objectname <- "depthraster"
  result <- bb_get_object(objectname,
                          file.path(workingfolder, "results"),
                          returnobject)
  return(result)
}

#' @rdname bb_get
bb_get_results_velocityraster <- function(workingfolder=NULL, returnobject=TRUE) {
  objectname <- "velocityraster"
  result <- bb_get_object(objectname,file.path(workingfolder, "results"), returnobject)
  return(result)
}

#' @rdname bb_get
bb_get_results_depthxvelocityraster <- function(workingfolder=NULL, returnobject=TRUE) {
  objectname <- "depthxvelocityraster"
  result <- bb_get_object(objectname, file.path(workingfolder, "results"), returnobject)
  return(result)
}

#' @rdname bb_get
bb_get_results_hazardraster <- function(workingfolder=NULL, returnobject=TRUE) {
  objectname <- "hazardraster"
  result <- bb_get_object(objectname,file.path(workingfolder, "results"), returnobject)
  return(result)
}

#' @rdname bb_get
#' @importFrom terra rast
bb_get_dhandraster <- function(workingfolder, returnobject=TRUE, include_wf=TRUE, depth=NULL, filetype="depthraster") {
  # type as one of depthraster or idraster

  # special case for getting dhand rasters

  if (is.null(workingfolder)) {
    stop("Cannot retrieve files when workingfolder is NULL")
  }

  if (is.null(depth)) {
    stop("Cannot retrive DHAND files when depth is NULL")
  }

  if (filetype == "depthraster") {
    df <- file.path("dhand",sprintf("bb_dhand_depth_%0.4fm.tif",depth))
  } else if (filetype=="idraster") {
    df <- file.path("dhand",sprintf("bb_dhand_pourpoint_id_depth_%0.4fm.tif",depth))
  } else {
    stop(sprintf("unrecognized filetype: %s",filetype))
  }

  # check that file exists
  if (include_wf) {
    tf <- file.path(workingfolder, df)
  } else {
    tf <- df
  }

  result <- tf

  if (!is.null(result)) {
    if (returnobject) {

      if (!file.exists(result)) {
        warning(sprintf("%s not found in file path %s, returning NULL",objectname,tf))
        result <- NULL
      } else {
        ## assume dhand here is always a raster of some sort
          result <- terra::rast(tf)
      }
    }
  }
  return(result)
}

#' @rdname bb_get
#' @importFrom terra rast
bb_get_results_raster <- function(workingfolder, returnobject=TRUE, include_wf=TRUE, flowprofile=NULL, modeltype=NULL, preprocmethod=NULL, interpmethod=NULL, filetype="depthraster") {
  # type as one of depthraster, velocityraster, dvprodraster, hazardraster

  # special case for getting results raster

  if (is.null(workingfolder)) {stop("Cannot get files when workingfolder is NULL")}
  if (is.null(flowprofile)) {stop("Cannot get results files when depth is NULL")}
  if (is.null(interpmethod)) {stop("Cannot get results files when interpmethod is NULL")}

  if (filetype == "depthraster") {
    df <- file.path("results",sprintf("bb_results_%s_%s_%s_%s_depth.tif",flowprofile,modeltype,preprocmethod,interpmethod))
  } else if (filetype=="velocityraster") {
    df <- file.path("results",sprintf("bb_results_%s_%s_%s_%s_velocity.tif",flowprofile,modeltype,preprocmethod,interpmethod))
  } else if (filetype=="dvprodraster") {
    df <- file.path("results",sprintf("bb_results_%s_%s_%s_%s_depthvelocity.tif",flowprofile,modeltype,preprocmethod,interpmethod))
  } else if (filetype=="hazardraster") {
    df <- file.path("results",sprintf("bb_results_%s_%s_%s_%s_hazard.tif",flowprofile,modeltype,preprocmethod,interpmethod))
  } else {
    stop(sprintf("unrecognized filetype: %s",filetype))
  }

  # check that file exists
  if (include_wf) {
    tf <- file.path(workingfolder, df)
  } else {
    tf <- df
  }
  result <- tf
  if (!is.null(result)) {
    if (returnobject) {

      if (!file.exists(result)) {
        warning(sprintf("%s not found in file path %s, returning NULL",objectname,tf))
        result <- NULL
      } else {
        ## assume dhand here is always a raster of some sort
        result <- terra::rast(tf)
      }
    }
  }
  return(result)
}

#' @title Write sf objects to geojson format
#'
#' @description
#' Writes sf object to geojson format at the specified output location
#'
#' @details
#' All functions use the function \code{bb_get_object} to retrieve files/objects.
#' Same files names are used though with the geojson extension.
#' Specific handling for some spatial objects in alignment with BlackbirdView.
#'
#' @param bbopt blackbird options folder
#' @param outputfolder folder where the file should be written to (defaults to \code{<workingfolder>}/model/GIS_files)
#'
#' @return Returns \code{TRUE} if written successfully
#'
#' @name bb_write_geojson
bb_write_geojson <- function(ss=NULL, bbopt=NULL, filename=NULL, outputfolder=NULL) {
  if (is.null(ss)) {stop("ss is required, may be NULL")}
  if (is.null(bbopt)) {stop("bbopt is required")}
  if (is.null(filename)) {stop("filename is required")}

  ss <- st_transform(ss, 4326)

  if (is.null(outputfolder)) {
    outputfolder <- file.path(bbopt$workingfolder,"model/GIS_files")
  }

  outputpath <- file.path(outputfolder, sprintf("%s.geojson",filename))
  sf::st_write(ss,dsn=outputpath,driver = "GeoJSON",delete_dsn = TRUE)
}

#' @rdname bb_write_geojson
#' @importFrom sf st_write st_transform
bb_write_rivershp_geojson <- function(bbopt=NULL,outputfolder=NULL) {
  if (is.null(bbopt)) {stop("bbopt is required")}
  ss <- bb_get_rivershp(bbopt$workingfolder)
  bb_write_geojson(ss,bbopt,"bb_rivershp",outputfolder)
  return(TRUE)
}

#' @rdname bb_write_geojson
#' @importFrom sf st_write st_transform
bb_write_snappedstreamnodes_geojson <- function(bbopt=NULL,outputfolder=NULL) {
  if (is.null(bbopt)) {stop("bbopt is required")}
  ss <- bb_get_snappedstreamnodesforcatchmentsshp(bbopt$workingfolder)
  bb_write_geojson(ss,bbopt,"bb_snapped_streamnodes_forcatchments",outputfolder)
  return(TRUE)
}

#' @rdname bb_write_geojson
#' @importFrom sf st_write st_transform
bb_write_xsections_geojson <- function(bbopt=NULL,outputfolder=NULL) {
  if (is.null(bbopt)) {stop("bbopt is required")}
  ss <- bb_get_xsectionsfromstreamnodesshp(bbopt$workingfolder)
  bb_write_geojson(ss,bbopt,"bb_xsections_fromstreamnodes",outputfolder)
  return(TRUE)
}

#' @rdname bb_write_geojson
#' @importFrom sf st_write st_transform
bb_write_catchmentstreamnodes_geojson <- function(bbopt=NULL,outputfolder=NULL, include_results=FALSE, hydraulic_output=NULL) {
  if (is.null(bbopt)) {stop("bbopt is required")}
  ss <- bb_get_catchmentsfromstreamnodesshp(bbopt$workingfolder)

  if (!include_results) {
    bb_write_geojson(ss,bbopt,"bb_catchments_fromstreamnodes",outputfolder)
  } else {

    if (is.null(hydraulic_output)) {
      hydraulic_output <- bb_get_results_hydraulic_output(bbopt$workingfolder)
    }

    hydraulic_output$pointid <- hydraulic_output$nodeID
    hydraulic_output <- hydraulic_output[,c("pointid","Depth","WSL","Flow","flowprofile")]

    if (length(unique(hydraulic_output$flowprofile))==1) {
      ss <- left_join(ss,hydraulic_output,by="pointid")
    } else {
      # hydraulic_output %>% pivot_wider(names_from="flowprofile")
      # work on pivot_wider function
      hydraulic_output <- hydraulic_output[hydraulic_output$flowprofile == hydraulic_output$flowprofile[1],]
      ss <- left_join(ss,hydraulic_output,by="pointid")
    }

    bb_write_geojson(ss,bbopt,"bb_catchments_fromstreamnodes",outputfolder)
  }

  return(TRUE)
}

### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### HYDRAULIC CALCULATION UTILITIES ----
### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


#' @title Calculate hydraulic properties in preprocessing for reach-integrated streamnodes
#'
#' @description
#' Calculate hydraulic properties in preprocessing for reach-integrated streamnodes
#'
#' @param workingfolder folder to read inputs from and write outputs to for blackbird operations
#' @param return_raster whether to return the processed dem as SpatRaster object
#' @param overwrite whether to overwrite an existing file (default TRUE)
#'
#' @return {\code{preproc_table} if processed properly, or the dem if return_raster is TRUE}
#
#' @details
#' This function is called in both serial and parallel processing of hydraulic properties from the
#' geometry class functions.
#'
#' @examples
#' # IOU examples
#' @export bb_compute_preproc_hydprops
bb_compute_preproc_hydprops = function(i, bbopt, preproc_table, a, sdf,
                                    catchment, dem, hand, handid, dhands, dhandsid, manningsn, reachlength,
                                    catchmentstack=NULL, applyfuzzy=FALSE) {



  ## skip all this, just using a single depth for each reach

  # interpolation, load the hand ID field
  # handid <- bb_get_handpourpointIDraster(workingfolder,returnobject = TRUE)
  # sppi <- spp[spp$cpointid == uni[i],]
  # sppi$pointid <- as.integer(sppi$pointid)
  # sppidepths <- matrix(NA,nrow=nrow(sppi),ncol=length(bbopt$Hseq))
  # seqelev <- seq(from=sppi$elev[1], to=sppi$elev[nrow(sppi)], length.out=nrow(sppi))
  #
  # for (ii in 1:length(bbopt$Hseq)) {
  #   if (bbopt$Hseq[ii] == 0) {
  #     sppidepths[,ii] <- 0
  #     next
  #   }
  #   max_change <- abs((seqelev-sppi$elev)/bbopt$Hseq[ii])
  #   Ct <- rep(NA,nrow(sppi))
  #   for (j in 1:nrow(sppi)) {
  #     # Ct[j] <- calc_Ct(max_change[j], bbopt$postproc_elev_corr_threshold)
  #     Ct[j] <- calc_Ct(max_change[j], bbopt$postproc_elev_corr_threshold)
  #   }
  #   sppidepths[,ii] <- bbopt$Hseq[ii] + (seqelev-sppi$elev)*Ct
  # }
  # preproc_table <- .self$streamnodeList[[i]]$depthdf
  #

  ## get subset for specific nodeID
  if (!applyfuzzy) {
    ind2 <- which(catchment == sdf$nodeID[i])
  } else {
    ind2 <- which(catchmentstack[i,] == sdf$nodeID[i])
  }

  for (j in 1:length(bbopt$Hseq)) {

    if (bbopt$Hseq[j]==0) {
      # skip all calculations as all the properties we can determine will be zero
      # preproc table already prefilled with zeros
      preproc_table[j,]$alpha <- 1
      preproc_table[j,]$alpha_areaconv <- 1
      preproc_table[j,]$alpha_roughconv <- 1
      preproc_table[j,]$alpha_disconv <- 1
      preproc_table[j,]$Manning_Composite <- mean(manningsn,na.rm=TRUE)
      preproc_table[j,]$Length_Effective <- sdf$us_reach_length1[i]
      # set roughness values to non-zero values
      preproc_table[j,]$Manning_Composite <- 0.035
      preproc_table[j,]$nc_equalforce  <- 0.035
      preproc_table[j,]$nc_equalvelocity   <- 0.035
      preproc_table[j,]$nc_wavgwp  <- 0.035
      preproc_table[j,]$nc_wavgarea   <- 0.035
      preproc_table[j,]$nc_wavgconv   <- 0.035
      next
    }

    if (bbopt$use_dhand) {
      handrr <- (dhands[j,])[ind2]
      handidrr <- as.integer( (dhandsid[j,])[ind2])
    } else {
      handrr <- hand[ind2]
      handidrr <- as.integer(handid[ind2])
    }

    ## if getting values for each spp, could look like this
    # pointid from catchment was somehow not lining up here
    # from other definition of ind2
    # ind2 <- which(handidrr %in% sppi$pointid)

    # for (jj in 1:length(sppi$pointid)) {
    #   depthrr[handidrr == sppi$pointid[jj]] <-
    #     sppidepths[jj,j]
    #
    #   # print(sprintf("for jj=%i, sppi$pointid=%i, %i cells match or %i cells match",
    #   #               jj,sppi$pointid[jj],
    #   #               length(which(handidrr==sppi$pointid[jj])),
    #   #               length(depthrr[handidrr == sppi$pointid[jj]])))
    # }

    dirr <- bbopt$Hseq[j]-handrr # use single depth here
    dirr[dirr<0] <- 0

    # Aif_rr = dirr*0
    Aif_rr <- dirr*0+(a^2)
    # Aif_rr[dirr<=0] <- 0
    Atf_rr <- Aif_rr
    Vif_rr <- Aif_rr*dirr

    Rhi_rr <- Vif_rr / Aif_rr
    Ki_rr <- (1/manningsn[ind2])*Vif_rr*(Rhi_rr^(2.0/3.0))

    Vfi_ni_rr <-  Vif_rr/manningsn[ind2]
    Ki3_Vif2_ratio <-  (Ki_rr^3) / ((Vif_rr/a)^2)

    Af <- sum(Aif_rr,na.rm = TRUE)
    Atf <- Af
    Vf <- sum(Vif_rr,na.rm = TRUE)
    Kisum <- sum(Ki_rr,na.rm = TRUE)

    if (bbopt$catchment_integration_method == "effective_length") {
      Leff_K_prod <-  Ki_rr * reachlength[ind2]
      Leff <- (1/Kisum)*sum(Leff_K_prod,na.rm = TRUE)
      # correct Leff if at a junction
      if (sdf$us_reach_length2[i] != -99) {
        Leff = sdf$us_reach_length2[i]
      }
      # correct Leff if NA
      if (!is.finite(Leff)) {
        Leff <- sdf$us_reach_length1[i]
      }
    } else if (bbopt$catchment_integration_method == "reach_length") {
      Leff <- sdf$us_reach_length1[i]
    }

    # // if (catchment_conveyance_method == "discretized_conveyance") {
    # // do nothing, use K as computed above and summed in each cell. Just compute alpha
    sum_Vf_a = Vf/a
    K_Total_disconv = Kisum
    alpha_disconv = finiteorone( (sum_Vf_a^2) / (K_Total_disconv^3) * sum(Ki3_Vif2_ratio,na.rm = TRUE))

    # // } else if (catchment_conveyance_method == "areaweighted_conveyance_onecalc") {
    K_Total_areaconv = sum(Vfi_ni_rr,na.rm=TRUE) * Vf^(2/3) / Af^(2/3) #  // total conveyance, m3/s
    # // NumericVector alpha =NumericVector::create(1.0); // alpha set to one by definition in method
    alpha_areaconv = 1.0

    # // } else if (catchment_conveyance_method == "roughzone_conveyance") {
    # // get unique roughness values in catchment, sum conveyance there with areaweighted approach
    uniqueManningsn = unique(manningsn[ind2])
    K = 0.0
    alpha = 0.0
    for (kk in 1:length(uniqueManningsn)) {
      # // kkmanningsn = manningsn[cmanningsn == uniqueManningsn[kk]];
      # // logical subet that meets the criteria of
      # LogicalVector res1 = manningsn == uniqueManningsn[kk];
      # LogicalVector res2 = catchment == nodeID;
      # LogicalVector res3 = res1 & res2;
      # // NumericVector kkmanningsn = manningsn[manningsn == uniqueManningsn[kk] && catchment == nodeID];

      cmanningsn <- manningsn[ind2]
      res3 <- which(cmanningsn==uniqueManningsn[kk])
      # res3 <- res1[which(res1 %in% ind2)] # merge criteria
      kkmanningsn = cmanningsn[res3]
      kkAif_rr = Aif_rr[res3]
      kkVif_rr = Vif_rr[res3]
      kkVfi_ni_rr = Vfi_ni_rr[res3]
      kkAf = sum(kkAif_rr,na.rm=TRUE) #  // total flooded area, m2
      kkVf = sum(kkVif_rr,na.rm=TRUE) #  // total flooded volume, m3
      Ki = finiteorzero(sum(kkVfi_ni_rr,na.rm=TRUE)*kkVf^(2/3)/ kkAf^(2.0/3.0))
      K = K + Ki
      alpha = alpha + finiteorzero(Ki^3/kkVf^2)
    }
    alpha =finiteorone(alpha*Vf^2/K^3.0)
    K_Total_roughconv = K
    alpha_roughconv = alpha

    if (bbopt$catchment_conveyance_method == "discretized_conveyance") {
      K = K_Total_disconv
      alpha = alpha_disconv
    } else if (bbopt$catchment_conveyance_method == "areaweighted_conveyance_onecalc") {
      K = K_Total_areaconv
      alpha = alpha_areaconv
    } else if (bbopt$catchment_conveyance_method == "roughzone_conveyance") {
      K = K_Total_roughconv
      alpha = alpha_roughconv
    }

    # // }
    # // std::cout << "conveyance calculations are ok" << "\n";


    # // hydraulic properties for 1D ---
    # // normalized by effective reach length Leff (varies with stage) to normalize when in dividing to get 1D properties
    A1D = finiteorzero(Vf / Leff) # ;  // equivalent cross-sectional area
    P1D = finiteorzero(Af / Leff) # ;  // equivalent wetted perimeter
    Rh1D = finiteorzero(A1D / P1D) # ; // equivalent hydraulic radius
    K1D = finiteorzero(K / Leff) # ;   // equivalent conveyance
    T1D = finiteorzero(Atf / Leff) # ; // equivalent top width
    HyD1D = finiteorzero(A1D / T1D) # ;// equivalent hydraulic radius
    K_roughzone_1D = finiteorzero(K_Total_roughconv / Leff) # ;    // equivalent conveyance from roughness zone conveyance
    K_disc_1D = finiteorzero(K_Total_disconv / Leff) # ;           // equivalent conveyance from discretized conveyance
    K_areaweighted_1D = finiteorzero(K_Total_areaconv / Leff) # ;  // equivalent conveyance from area weighted conveyance


    # // alpha check
    if (alpha < 1.0 | alpha > 5) {
      warning(sprintf("alpha computed as %2g for catchment %i, should be bounded by approx [1.0, 5.0]. Alpha set to boundary value.",alpha,sdf$nodeID[i])) # ; // may need to change %2g to %f
      if (alpha < 1.0) {
        alpha = 1.0;
      } else if (alpha > 5.0) {
        alpha = 5.0;
      }
    }

    # // composite mannings n check
    # NumericVector nc = NumericVector::create(0);

    # if (Af == 0 ) {
    #   // below results in an error if land cover does not completely cover dem
    #   // take area-weighted average where hand==0 (will have no wet cells to compute with zero depth otherwise)
    #   NumericVector cAi_ni_prod = cAi_rr*cmanningsn;
    #   sum(as<NumericVector>(na_omit(cAi_rr)[chand <= 0]));
    #   nc = cpp_finite_or_zero( sum(as<NumericVector>(na_omit(cAi_ni_prod)[chand <= 0])) / NumericVector::create(sum(as<NumericVector>(na_omit(cAi_rr)[chand <= 0]))) );
    #   nc_equalforce = nc;
    #   nc_equalvelocity = nc;
    #   nc_wavgwp = nc;
    #   nc_wavgarea = nc;
    #   nc_wavgconv = nc;
    # } else {

    # // could add more methods here, such as equal force but based on Vf not Af xxx
    # // note that we use Vf in place of area when using catchment-based versions of equations
    # // and Af in place of perimeter
    # //if (manning_composite_method == "equal_force") {
    # // equivalent method - total force equation
    Aif_ni_prod_eqf = Aif_rr * manningsn[ind2]^2
    nc_equalforce = finiteorzero(sqrt(sum(Aif_ni_prod_eqf,na.rm=TRUE)/Af) )

    # //} else if (manning_composite_method == "weighted_average_area") {
    #   // simple area-weighted average
    #   // uses volume in catchment approach
    Vif_ni_prod_waa = Vif_rr * manningsn[ind2]
    nc_wavgarea = finiteorzero(sum(Vif_ni_prod_waa,na.rm=TRUE) / Vf)

    # //} else if (manning_composite_method == "weighted_average_wetperimeter") {
    #   // simple perimeter-weighted average
    #   // uses area in catchment approach
    Aif_ni_prod = Aif_rr * manningsn[ind2]
    nc_wavgwp = finiteorzero(sum(Aif_ni_prod) / Af)

    # //} else if (manning_composite_method == "weighted_average_conveyance") {
    # // simple area-weighted average
    Ki_ni_prod = Ki_rr * manningsn[ind2]
    nc_wavgconv = finiteorzero(sum(Ki_ni_prod,na.rm=TRUE) / Kisum)

    # //} else if (manning_composite_method == "equal_velocity") {
    Aif_ni_prod_eqvel = Aif_rr * manningsn[ind2]^1.5
    nc_equalvelocity = finiteorzero( (sum(Aif_ni_prod_eqvel,na.rm=TRUE)/ Af)^(2.0/3.0) )
    # //}

    if (bbopt$Manning_composite_method == "equal_force") {
      nc = nc_equalforce
    } else if (bbopt$Manning_composite_method == "weighted_average_area") {
      nc = nc_wavgarea
    } else if (bbopt$Manning_composite_method == "weighted_average_wetperimeter") {
      nc = nc_wavgwp
    } else if (bbopt$Manning_composite_method == "weighted_average_conveyance") {
      nc = nc_wavgconv
    } else if (bbopt$Manning_composite_method == "equal_velocity") {
      nc = nc_equalvelocity
    } else {
      warning("Unrecognized bbopt - Manning_composite_method. Using default (equal_force)");
      nc = nc_equalforce;
    }
    # }
    # // std::cout << "nc calc ok" << "\n";


    if (nc > max(manningsn[ind2],na.rm=TRUE) | nc < min(manningsn[ind2],na.rm=TRUE)) {
      warning(sprintf("composite Mannings n is outside of bounds for catchment %s, setting to boundary value", sdf$nodeID[i]))
      if (nc > max(manningsn[ind2],na.rm=TRUE)) {
        nc <- max(manningsn[ind2],na.rm=TRUE)
      } else if (nc < min(manningsn[ind2],na.rm=TRUE)) {
        nc <- min(manningsn[ind2],na.rm=TRUE)
      }
    }

    preproc_table$Area[j]         <- A1D
    preproc_table$WetPerimeter[j] <- P1D
    preproc_table$HRadius[j]      <- Rh1D
    preproc_table$K_Total[j] <- K1D
    preproc_table$alpha[j] <- alpha
    preproc_table$Manning_Composite[j] <- nc
    preproc_table$Length_Effective[j] <- Leff
    preproc_table$TopWidth[j] <- T1D
    preproc_table$HydDepth[j] <- HyD1D
    preproc_table$K_Total_areaconv[j] <- K_areaweighted_1D
    preproc_table$K_Total_disconv[j] <- K_disc_1D
    preproc_table$K_Total_roughconv[j] <- K_roughzone_1D
    preproc_table$alpha_areaconv[j] <- alpha_areaconv
    preproc_table$alpha_disconv[j] <- alpha_disconv
    preproc_table$alpha_roughconv[j] <- alpha_roughconv
    preproc_table$nc_equalforce[j] <- nc_equalforce
    preproc_table$nc_equalvelocity[j] <- nc_equalvelocity
    preproc_table$nc_wavgwp[j] <- nc_wavgwp
    preproc_table$nc_wavgarea[j] <- nc_wavgarea
    preproc_table$nc_wavgconv[j] <- nc_wavgconv

  }
  return(preproc_table)
}




# to do xxx - add shear stress, shear power, etc. calculations

#' @title Support functions for hydraulic calculations in the blackbird package
#'
#' Support functions used in the blackbird package for calculating various hydraulic properties.
#' Includes calculations for conveyance, friction slope, energy calculation, and others.
#'
#' Note that some functions may have multiple versions for calcualting a hydraulic parameter,
#' such as conveyance or friction slope. To view the function calculation, type the function name
#' into the R console with no brackets to view the function code (e.g., 'energy_calc').
#'
#' @param Z water surface elevation (m)
#' @param y water depth (m)
#' @param v water average velocity (m/s)
#' @param n Manning's n value (weighted for channel)
#' @param Rh hydraulic radius (m)
#' @param Q flow (m3/s)
#' @param K channel conveyance
#'
#' Other parameters and various outputs as per function usage.
#'
#' @name bb_support
energy_calc <- function(Z,y,v,alpha,g=9.81) {
  # total energy in the channel
  return(Z+y+v^2/2/g*alpha)
}

#' @rdname bb_support
sf_calc <- function(n,v,Rh) {
  # friction slope from Manning's equation
  return((n*v*Rh^(3/2))^2)
}

#' @rdname bb_support
sf_calc2 <- function(Q,K) {
  # friction slope from conveyance and flow
  return((Q/K)^2)
}

#' @rdname bb_support
conv_calc <- function(n,A,Rh) {
  # conveyance
  return(max((1/n)*A*Rh^(2/3),0,na.rm=T))
}

#' @rdname bb_support
vhead_calc <- function(alpha,v,g) {
  # velocity head
  return(alpha*v^2/2/g)
}

#' @rdname bb_support
wetted_perimeter <- function(zz,ind,dx) {
  # wetted perimeter
  # zz, ind, dx provided within other functions - see compute_profile or normal_depth for an example

  # find breakpoints within ind
  bp <- get_breakpoints(ind)
  wp <- rep(NA,length(bp)-1)
  for (i in 1:(length(bp)-1)) {
    vv <- wetted_perimeter_vec(zz,ind[bp[i]:bp[(i+1)]],dx)
    wp[i] <- sum(vv)
  }

  # vv <- wetted_perimeter_vec(zz,ind,dx)
  return(sum(wp))
  # return(max(sum(((zz[ind][-1] - zz[ind][-length(ind)])^2 + dx^2)^0.5),0,na.rm=TRUE))
}

#' @rdname bb_support
# #' @importFrom zoo rollmean
wetted_perimeter_vec <- function(zz,ind,dx) {
  # wetted perimeter as a vector
  # zzmean <- rollmean(zz,k=3,na.pad=TRUE,align='center')
  # zzmean[is.na(zzmean)] <- zz[is.na(zzmean)]
  # vv <- (((zzmean[-1] - zzmean[-length(zzmean)])^2 + dx^2)^0.5)
  # notind <- which(seq(1,length(vv)) %notin% ind)
  # vv[notind] <- 0
  # sum(vv)

  # wetted perimeter calculated for each zz point
  # each point represents the wetted perimeter of channel from -0.5dx to +0.5dx around point
  # endpoints from 0 to +0.5dx and from [max(xx)-0.5dx] to max(xx)
  wp <- rep(NA,length(zz))
  for (i in 2:(length(wp)-1)) {
    wp[i] <- sqrt((0.5*dx)^2 + (0.5*(zz[i]-zz[i-1]))^2) + sqrt((0.5*dx)^2 + (0.5*(zz[i+1]-zz[i]))^2)
  }
  wp[1] <- sqrt(dx^2 + (0.5*(zz[1]-zz[2]))^2)
  wp[length(wp)] <- sqrt(dx^2 + (0.5*(zz[length(wp)-1]-zz[length(wp)]) )^2)
  # correct for ind (where depth is positive)
  notind <- which(seq(1,length(wp)) %notin% ind)
  wp[notind] <- 0
  return(wp)
}

#' @rdname bb_support
froude_calc <- function(v,g,d) {
  # froude number
  return(finiteorzero(v/sqrt(g*d)))
}

#' @rdname bb_support
total_energy <- function(mm,i,bbopt) {
  # specifically for bb_hyd_compute_profile critical depth solver
  te <- energy_calc(Z=mm[i,]$Min_Elev,
                    y=mm[i,]$Depth,
                    v=mm[i,]$Velocity,
                    alpha=mm[i,]$alpha,
                    g=bbopt$g)
  return(te)
}

#' @rdname bb_support
get_total_energy <- function(H,mm,geometry,i,previousmm,bbopt) {
  # specifically for bb_hyd_compute_profile critical depth solver
  # mmm <- mm
  mm[i,] <- geometry$streamnodeList[[i]]$compute_profile_next(
Flow=mm[i,]$Flow, WSL=H, mm=mm[i,], prevmm = previousmm, bbopt = bbopt)
  return(total_energy(mm,i,bbopt=bbopt))
}

#' @rdname bb_support
calc_Ct <- function(x,tt) {
  # compute the correction term in post-processing elevation correction
  # x is the max_change in depth as a fraction, tt is the threshold (bbopt$postproc_elev_corr_threshold)
  if (tt==0) {
    # exception for 0 threshold, correction is not applied at all (Ct=0, nullifying elevation correction)
    return(0)
  } else {
    # tolerance for depth correction >0, compute
    return(1 - min((max(x - tt, 0))/tt, 1))
  }
}

#' @rdname bb_support
bb_hydraulic_output_emptydf <- function(nrow=1) {

  mm <- bb_hydraulic_output_emptydf_propsonly(nrow=nrow)

  # add additional properties that are only determined at runtime in hydraulic model
  mm$Length_EffectiveAdjusted <-
  mm$Length_EnergyLoss <-
  mm$WS_err <-
    mm$K_err <-
    mm$cp_iterations <-
    mm$Depth_Critical <- NA
  # should move Energy_total, velocity, headloss, etc to this part too,
  # avoid lots of NA columns in hydraulic tables

  # # empty data frame for hydraulic_output calculations
  # # xxx to do - add nodetype here for checking nodetype while determining Leff to use

  return(mm)
}

#' @rdname bb_support
bb_hydraulic_output_emptydf_propsonly <- function(nrow=1) {
  # empty data frame for hydraulic_output calculations
  mm <- data.frame(matrix(NA,nrow=nrow,ncol=69))
  # xxx to do - add nodetype here for checking nodetype while determining Leff to use
  colnames(mm) <- c("nodeID","reachID","downnodeID","upnodeID1","upnodeID2","stationname","station", "reach_length_DS",
                    "reach_length_US1","reach_length_US2",
                    "Flow","Flow_LOB","Flow_Main","Flow_ROB","Min_Elev","WSL","Depth",
                    "HydDepth","HydDepth_LOB","HydDepth_Main","HydDepth_ROB",
                    "TopWidth","TopWidth_LOB","TopWidth_Main","TopWidth_ROB",
                    "Velocity","Velocity_LOB","Velocity_Main","Velocity_ROB","K_Total",
                    "K_LOB","K_Main","K_ROB","alpha","Area","Area_LOB","Area_Main","Area_ROB",
                    "HRadius","HRadius_LOB","HRadius_Main","HRadius_ROB","WetPerimeter","WetPerimeter_LOB",
                    "WetPerimeter_Main","WetPerimeter_ROB","Energy_total","Velocity_head","Froude",
                    "Sf","Sf_Avg","Sbed",
                    "Length_Effective","Head_Loss","Manning_LOB","Manning_Main","Manning_ROB","Manning_Composite",
                    "K_Total_areaconv","K_Total_roughconv","K_Total_disconv",
                    "alpha_areaconv","alpha_roughconv","alpha_disconv",
                    "nc_equalforce","nc_equalvelocity","nc_wavgwp","nc_wavgarea","nc_wavgconv")
  return(mm)
}


### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### PARALLEL SUPPORT UTILITIES ----
### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' @title Cleanup parallel computing clusters
#'
#' @description
#' Cleans up residual parallel clusters.
#'
#' @seealso this thread on [Stack Overflow](https://stackoverflow.com/questions/64519640/error-in-summary-connectionconnection-invalid-connection)
#'
#' @return {Returns \code{TRUE} if executed properly}
#'
#' @examples
#' bb_unregister_dopar()
#'
#' @importFrom foreach foreach
#' @export bb_unregister_dopar
bb_unregister_dopar <- function() {
  env <- foreach:::.foreachGlobals
  rm(list=ls(name=env), pos=env)
  return(TRUE)
}


#' @title Pre-process depth tables in parallel
#'
#' @description
#' Computes the depth properties curve for each streamnode as a pre-processing step using parallel computation.
#'
#' @param bbgeom blackbird geometry object
#' @param bbopt blackbird options object
#'
#' @return \item{TRUE}{returns \code{TRUE} if computed successfully}
#
#' @details
#'
#'
#' @examples
#' # TO DO
#'
#'
#' @importFrom parallel makeCluster
#' @importFrom foreach foreach
#' @importFrom doParallel registerDoParallel
#' @export bb_preprocess_parellel_depthdf
bb_preprocess_parellel_depthdf <- function(bbgeom=NULL, bbopt=NULL) {

  if (is.null(bbgeom)) {
    stop("bbgeom is required")
  }
  if (is.null(bbopt)) {
    stop("bbopt is required")
  }

  ## setup parallel set
  totalCores = detectCores()
  usableCores <- totalCores - bbopt$reserve_cores
  message(sprintf("Using %i cores of %i available",usableCores,totalCores))

  ## divide up jobs
  len_nodes <- bbgeom$get_streamnodelist_length()
  chunks <- chunk_seq(seq(1,len_nodes),usableCores)

  ## check Hseq
  seqH <- bbopt$Hseq

  # check that seqQ is non-empty
  if (length(seqH) <= 1) {
    stop("multiple flow points are required to pre-process depth tables,\nplease check the dH/minH/maxH/Hseq blackbird options")
  }

  # initialize cluster
  cluster <- makeCluster(usableCores, outfile="Log.txt", setup_strategy = "sequential")
  registerDoParallel(cluster)

  on.exit(stopCluster(cluster))
  on.exit(registerDoSEQ())
  on.exit(bb_unregister_dopar())

  # run parallel code
  foreach(cc = chunks) %dopar% {
    for (i in as.numeric(unlist(cc))) {

      mm <- bbgeom$streamnodeList[[i]]$generate_initial_hydraulic_profile(nrow=length(seqH))
      mm$Depth <- seqH
      mm$WSL <- mm$Min_Elev + mm$Depth

      for (j in 1:nrow(mm)) {
        mm[j,] <- bbgeom$streamnodeList[[i]]$compute_basic_depth_properties(
                                  WSL=mm[j,]$WSL,
                                  mm=mm[j,],
                                  bbopt=bbopt)
      }
      bbgeom$streamnodeList[[i]]$depthdf <- mm
    }
  }
  stopCluster(cluster)
  registerDoSEQ()
  bb_unregister_dopar()
  closeAllConnections()

  return(TRUE)
}


#' @title Pre-process min elevations in parallel
#'
#' @description
#' Computes the minimum elevation in each streamnode as a pre-processing step using parallel computation.
#'
#' @param bbgeom blackbird geometry object
#' @param bbopt blackbird options object
#'
#' @return \item{TRUE}{returns \code{TRUE} if computed successfully}
#
#' @examples
#' bb_preprocess_parellel_minelev() # to do xxx
#'
#'
#' @importFrom parallel makeCluster
#' @importFrom foreach foreach
#' @importFrom doParallel registerDoParallel
#' @export bb_preprocess_parellel_depthdf
bb_preprocess_parellel_minelev <- function(bbgeom=NULL, bbopt=NULL) {

  if (is.null(bbgeom)) {
    stop("bbgeom is required")
  }
  if (is.null(bbopt)) {
    stop("bbopt is required")
  }

  ## setup parallel set
  totalCores = detectCores()
  usableCores <- totalCores - bbopt$reserve_cores
  message(sprintf("Using %i cores of %i available",usableCores,totalCores))

  ## divide up jobs
  len_nodes <- bbgeom$get_streamnodelist_length()
  chunks <- chunk_seq(seq(1,len_nodes),usableCores)

  # register cluster
  cluster <- makeCluster(usableCores, outfile="Log.txt", setup_strategy = "sequential")
  registerDoParallel(cluster)

  on.exit(stopCluster(cluster))
  on.exit(registerDoSEQ())
  on.exit(bb_unregister_dopar())

  # run parallel code
  foreach(cc = chunks) %dopar% {
    for (i in as.numeric(unlist(cc))) {
      bbgeom$streamnodeList[[i]]$calc_min_elev(set=TRUE)
    }
  }

  stopCluster(cluster)
  registerDoSEQ()
  bb_unregister_dopar()
  closeAllConnections()

  return(TRUE)
}


