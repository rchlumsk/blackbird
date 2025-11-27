#' @title Read HEC-RAS geometry file to xsection
#'
#' Reads in a basic (xsection-based) geometry file to blackbird xsectionList object.
#'
#' @details
#' Currently only works for single reach networks. Assigns nodeID and connections based on riverstation.
#'
#' This is meant to be a convenience function for simple examples, and not a generic tool for reading all HEC-RAS geometry files.
#'
#' @param geomfile HEC-RAS geometry file path
#'
#' @return \item{xsectionList}{List of blackbird-class xsection streamnode objects}
#'
#' @examples
#' # IOU an example
#'
#' @export bb_read_rasgeom
bb_read_rasgeom <- function(geomfile=NULL) {

  if (!file.exists(geomfile)) {
    stop("geomfile must be a valid file path.")
  }

  tt <- readLines(geomfile)

  # get number of xsections in file
  num_sections <- length(grep("Type RM Length",tt))

  xsectionList <- list()

  ## local function
  bb_tokenize_rasfile <- function(tt) {

    if (is.na(tt)) {tt <- ""}

    # local function, added equal sign here
    ss <- unlist(strsplit(trimws(tt),split="\\,|\\s+|\\,\\s+|\\s+\\,\\s+|="))

    # remove all blank ones
    ss <- ss[ss!=""]

    return(ss)
  }

  ## blank out old xsection data
  riverstation <- NA
  nodeID <- NA
  reachlen_left <- NA
  reachlen_main <- NA
  reachlen_right <- NA
  Manning_LOB <- -1
  Manning_Main <- -1
  Manning_ROB <- -1
  xx <- NA
  zz <- NA
  nn <- NA
  xxnn <- NA
  desc <- NA
  bankstn_left <- NA
  bankstn_right <- NA
  num_points_xxzz <- NA
  num_points_nn <- NA
  contr_coeff <- NA
  exp_coeff <- NA

  pp <- as.integer(1) # tracking number of xsections, default for nodeID

  for (i in 1:length(tt)) {
    ss <- bb_tokenize_rasfile(tt[i])

    # print(i)

    ## parse line by line
    if (is.null(ss) | length(ss) == 0) {
      # comment or blank line
    } else if (ss[1]=="Type" & ss[2]=="RM" & ss[3]=="Length" & ss[7]=="1") {  ## start of new xsection

      riverstation <- as.numeric(ss[8])
      nodeID <- pp
      reachlen_left <- as.numeric(ss[9])
      reachlen_main <- as.numeric(ss[10])
      reachlen_right <- as.numeric(ss[11])
    } else if (ss[1]=="BEGIN" & ss[2]=="DESCRIPTION:") {
      node_desc <- tt[i+1]
      i <- i+1
    } else if(ss[1] == "#Sta/Elev") {
      num_points_xxzz <- as.numeric(ss[2])

      ## build out xx and zz
      kk <- 1
      xx <- rep(NA,num_points_xxzz)
      zz <- rep(NA,num_points_xxzz)

      while (kk < num_points_xxzz+1) {
        i <- i+1
        ss <- bb_tokenize_rasfile(tt[i])

        if (ss[1] == "#Mann"){
          break
        } else {
          ss <- read.fwf(geomfile,skip=i-1,n=1,widths=rep(8,10)) # special reading for fixed width format
          if (length(ss)!=10) {stop(sprintf("Error reading fixed width format file at line %i",i))}
          xx[kk:(kk+4)] <- as.numeric(ss[c(1,3,5,7,9)])
          zz[kk:(kk+4)] <- as.numeric(ss[c(2,4,6,8,10)])
          kk <- kk+5
        }
      }

      # trim off the end
      xx <- xx[!is.na(xx)]
      zz <- zz[!is.na(zz)]

    } else if(ss[1] == "#Mann") {
      ## currently just handling the horizontal variation case
      num_points_nn <- as.numeric(ss[2])

      ## build out nn
      kk <- 1
      xxnn <- rep(NA,num_points_nn) # xx stns for nn values
      nn <- rep(NA,num_points_nn)

      while (kk < num_points_nn+1) {
        i <- i+1
        ss <- bb_tokenize_rasfile(tt[i])

        if (ss[1] == "Bank"){
          break
        } else {

          ss <- read.fwf(geomfile,skip=i-1,n=1,widths=rep(8,10)) # special reading for fixed width format
          if (length(ss)!=10) {stop(sprintf("Error reading fixed width format file at line %i",i))}
          xxnn[kk:(kk+2)] <- as.numeric(ss[c(1,4,7)])
          nn[kk:(kk+2)] <- as.numeric(ss[c(2,5,8)])
          kk <- kk+3

          # xxnn[kk:(kk+2)] <- as.numeric(ss[c(1,4,7)])
          # nn[kk:(kk+2)] <- as.numeric(ss[c(2,5,8)])
          # kk <- kk+3
        }
      }

      # trim off the end
      xxnn <- xxnn[!is.na(xxnn)]
      nn <- nn[!is.na(nn)]

      # special declaration if exactly three values provided
      if (length(nn) == 3) {
        # handle as specifying LOB, Main, ROB roughness values
        Manning_LOB <- nn[1]
        Manning_Main <- nn[2]
        Manning_ROB <- nn[3]
      }

      ## add nn to xx/zz table
      nn_new <- rep(NA,length(xx))

      ## get new xxnn aligned with closest xx
      for (kk in 1:length(xxnn)) {
        nn_new[which.min(abs(xx - xxnn[kk]))] <- nn[kk]
      }
      nn <- nn_new
      rm(nn_new)

      ## infill nn (constant until changes)
      ### xxx eventually bring in bbopt to reflect how this is handled (interpolated or constant until change)
      manningsn_currentval <- nn[1]
      for (kk in 2:length(nn)) {
        if (is.na(nn[kk])) {
          nn[kk] <- manningsn_currentval
        } else {
          manningsn_currentval <- nn[kk]
        }
      }
      rm(manningsn_currentval)

    } else if(ss[1] == "Bank" & ss[2]=="Sta") {
      bankstn_left <- as.numeric(ss[3])
      bankstn_right <- as.numeric(ss[4])

    } else if (ss[1] == "Exp/Cntr") {

      exp_coeff <- as.numeric(ss[2])
      contr_coeff <- as.numeric(ss[3])

      ## calculate downnodeID
      if (nodeID < num_sections) {
        downnodeID <- nodeID+1
      } else {
        downnodeID <- -1
      }

      if (nodeID == 1) {
        upnodeID1 <- as.integer(-1)
      } else {
        upnodeID1 <- as.integer(nodeID-1)
      }

      ## triggers and of xsection, create and append to list
      xs <- blackbird::xsection(
        nodetype="xsection",
        xx=xx,
        zz=zz,
        Manning=nn,
        lbs_xx =bankstn_left,
        rbs_xx=bankstn_right,
        Manning_LOB=Manning_LOB,
        Manning_ROB=Manning_ROB,
        Manning_Main=Manning_Main,
        ds_length_LOB=reachlen_left,
        ds_length_Main = reachlen_main,
        ds_length_ROB = reachlen_right,
        nodeID = nodeID,
        station= riverstation,
        stationname=as.character(riverstation),
        downnodeID = as.integer(downnodeID),
        upnodeID1 =upnodeID1,
        expansion_coeff = exp_coeff,
        contraction_coeff = contr_coeff
      )
      xsectionList <- append(xsectionList,xs)
      rm(xs)

      # increment nodeID
      pp <- as.integer(pp+1)

      ## blank out old xsection data
      riverstation <- NA
      nodeID <- NA
      reachlen_left <- NA
      reachlen_main <- NA
      reachlen_right <- NA
      Manning_LOB <- -1
      Manning_Main <- -1
      Manning_ROB <- -1
      xx <- NA
      zz <- NA
      nn <- NA
      xxnn <- NA
      desc <- NA
      bankstn_left <- NA
      bankstn_right <- NA
      num_points_xxzz <- NA
      num_points_nn <- NA
      contr_coeff <- NA
      exp_coeff <- NA

    } else {
      # message(sprintf("Unrecognized command %s on line %i",ss[1],i))
    }
  }
  return(xsectionList)
}

