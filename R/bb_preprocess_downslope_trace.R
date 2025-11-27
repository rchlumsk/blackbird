#' @title Computes downstream pourpoint IDs using DInf Downslope Tracing
#'
#' @description
#' Performs the d-infinity downslope tracing to determine plausible downstream cells.
#' This is used to support fuzzy HAND calculations.
#'
#' @param bbopt blackbird options object
#' @param maxhand max value to filter cells to evaluate from the HAND raster
#' @param maxiter maximum iterations per cell (as percentage of DEM length)
#' @param maxcol maximum number of pourpoint elements to store for each raster cell
#' @param wwthreshold minimum threshold to store points for fuzzy HAND [0..1]
#'
#' @return {\code{TRUE} if processed properly}
#
#' @details
#'
#'
#' @examples
#'
#' # IOU examples
#'
#' @importFrom terra rast writeRaster crs res
#' @importFrom raster raster extract rasterize
#' @export bb_preprocess_downslope_trace
bb_preprocess_downslope_trace <- function(bbopt=NULL,
                          maxhand=40,maxiterpercent=0.3,
                          maxcol=50, wwthreshold=0.005) {

  ## constants to declare
  # maxiter <- 5e4 # max iterations in loop
  # maxcol <- 50
  # wwthreshold <- 0.005 # 0.5% threshold on keeping the pour point ID



  if (is.null(bbopt)) {
    stop("bbopt is required")
  }

  workingfolder <- bbopt$workingfolder

  dem <- raster::raster(bb_get_demcondraster(workingfolder,returnobject = FALSE))
  dinf_path <- bb_get_flowdirdinfraster(workingfolder,returnobject = FALSE)   # "MadRiverProject/bb_DInf_pointer.tif"
  dinf <- raster::raster(dinf_path)
  hand <- bb_get_handraster(workingfolder)
  dem2 <- dem # temporary

  # define pourpoint ID raster, with just pour point IDs
  pourpoints <- bb_get_snappedpourpointshand(workingfolder)
  rpp <- raster::rasterize(pourpoints,y=dem, field="pointid")
  rpp <- as.vector(rpp)
  rpp[is.na(rpp)] <- -1 # set NA to-1 for easier processing
  rpp <- as.integer(rpp)

  # sample elev at pourpoints
  pourpoints$elev <- raster::extract(dem2,pourpoints)
  rm(dem2)

  # make all cells between thalweg cells also a thalweg cell? plug the potential diagonal leaks

  dem <- as.vector(dem)
  dem[is.na(dem)] <- -1

  hand <- as.vector(hand)
  hand[is.na(hand)] <- -1

  dinf <- as.vector(dinf)
  dinf[is.na(dinf)] <- -1 # already -1 as NA

  # scale maxiter to percentage of dem length
  maxiter <- ceiling(maxiterpercent*length(dem))

  ## compute the ds data frame in full first ----

  # get all downslope values
  validcells <- which(dinf!=-1 & dem != -1 & hand != -1 & hand<maxhand )
  length(validcells)

  allds <- matrix(as.numeric(t(sapply(dinf[validcells], FUN=downslope_dinf))),ncol=2)
  alldsind <- matrix(as.integer(t(sapply(dinf[validcells], FUN=downslope_dinf_ind))),ncol=2)

  # get values of all adjacent cells
  alldirs <- t(sapply(validcells, adj, rr=raster(dinf_path), globalsearch=FALSE))
  colnames(alldirs) <- c("NW", "N", "NE", "W", "E", "SW", "S", "SE")
  # resort to ccdirs
  ccdirs <-  c("N", "NE", "E", "SE","S","SW","W","NW")
  alldirs <- alldirs[,ccdirs]
  alldirs <- matrix(as.integer(alldirs),ncol=8)

  allcells2eval <- validcells



  # for local storage - not needed with cpp
  # allids <- rep(list(NA), length(dinf))
  allids <- matrix(NA,nrow=length(allcells2eval),ncol=maxcol) # store max of 50 pointIDs
  allrww <- matrix(NA,nrow=length(allcells2eval),ncol=maxcol) # store weights for testing

  # querying for later - moved outside of loop
  ind3 <- which(rpp!=-1)

  ## start calculations ----
  emptydem <- dem
  emptydem[emptydem>0] <- 0

  ## note on indices
  # ii tracking from 1... length(allcells2eval)
  # cells tracking from 1...length(dinf), (and all other rasters)

  for (ii in 1:length(allcells2eval)) {

    cells <- allcells2eval[ii]        # tracking on length dinf
    ind <- which(validcells == cells) # tracking row in alldir, etc

    # if (which(cells==allcells2eval) %% 100 ==0) {
    #   print(sprintf("Evaluating cell %i of %i",
    #                 which(cells==allcells2eval) ,
    #                 length(allcells2eval)))
    # }

    # print(sprintf("Evaluating cell %i of %i",
    #               which(cells==allcells2eval) ,
    #               length(allcells2eval)))



    if (rpp[cells]==-1) { # not a pour point cell already, run calcs

      ## reset reval and rww
      reval <- emptydem # dem*0      # binary, has raster cell been evaluated this iteration
      rww <- emptydem # dem*0        # weight raster
      rww[rww<0] <- 0 # just zero or >0 in rww

      # should be uncessary, but force all rww to be zero
      if (any(rww>0)) {
        rww[rww>0] <- 0
      }

      ## first cell calculation ----
      rww[cells] <- 1 # update weights in first cell
      reval[cells] <- 1
      ds <- allds[ind,] # leave as 1 from allds
      ad <- alldirs[ind,]
      updatecells <- ad[alldsind[ind,]] # filter ad to columns in ds calc using indices
      rww[updatecells] <- ds # update weights in those cells

      if (!is.finite(rww[cells])) {
        stop(sprintf("stop1: rww is %s",rww[cells]))
      }

      ## iterate for other cells ----
      i <- 2
      # trackcells <- rep(NA,maxiter) # just for debugging, can comment out for operations
      # trackcells[1] <- cells    # just for debugging, can comment out for operations

      ## filter update cells that are not rpp
      evalcells <- as.numeric(updatecells)

      while (i < maxiter) {

        if (i >= maxiter) {
          # print("Iteration counter hitting maxiter")
          break
        }

        if (length(evalcells)>0) {
          for (jj in 1:length(evalcells)) {

            if (rww[evalcells[jj]] != 0 & reval[evalcells[jj]] != 1 & rpp[evalcells[jj]]==-1 & dinf[evalcells[jj]] != -1 & (evalcells[jj] %in% validcells)) {
              cells <- evalcells[jj]
              break
            } else if (jj==length(evalcells)) {
              # nothing viable found in eval list
              # print("nothing viable found in eval list, break")
              i <- maxiter+1
              break # this break is not working?
            }
          }

          # purge list
          evalcells <- evalcells[-(1:jj)]

          if (i < maxiter) {
            # trackcells[i] <- cells # just for debugging, can comment out for operations

            reval[cells] <- 1

            # update index
            ind <- which(validcells == cells)
            ds <- allds[ind,]*rww[cells]
            ad <- alldirs[ind,]
            updatecells <- ad[alldsind[ind,]] # filter ad to columns in ds calc
            rww[updatecells] <- ds + rww[updatecells] # update weights in those cells, adding any existing weights
            # update the evalcells
            evalcells <- c(evalcells, updatecells)
          }

        } else {
          i <- maxiter+1
          break
        }

        i <- i+1
      }

      ## collect all ppIDs at cells with rww > 0
      ind1 <- which(rww!=0)
      ind2 <- which(rww!=-1)
      indgrab <- Reduce(intersect, list(ind1,ind2,ind3))

      # indgrab <- which(rww!=0 & rww!= -1 & rpp != -1 )
      if (length(indgrab)==0) {
        print(sprintf("for %i, indgrab length is zero",ii))
      }

      # grab and sort by weights
      rppind <- rpp[indgrab]
      ww <- rww[indgrab]
      rppind <- rppind[order(ww, decreasing=TRUE)]
      ww <- ww[order(ww,decreasing = TRUE)]

      # if using threshold to cutoff
      ww <- ww/sum(ww) # force weights to sum to 1
      indgrab <- which(ww>wwthreshold)
      rppind <- rppind[indgrab]
      ww <- ww[indgrab]

      # grab range along each reach, need reach info
      ## skipping for now


      if (length(indgrab)>ncol(allids)) {
        print(sprintf("for %i, indgrab length is greater than ncol(allids) of %i",ii,ncol(allids)))
        # consider sorting by rww and taking top x
      }

      allids[ii,] <- truncate_pad(rppind,ncol(allids))
      allrww[ii,] <- truncate_pad(ww,ncol(allids))
      # rm(rppind)
      # rm(ww)

    } else { # cell is already a pourpoint, assign its own value
      allids[ii,] <- truncate_pad(rpp[cells],ncol(allids))
      allrww[ii,] <- truncate_pad(1,ncol(allids))
    }
  }

  write.csv(allids,
            bb_get_allids(workingfolder,returnobject=FALSE),
            quote=F,row.names=F)

  write.csv(validcells,
            bb_get_validcells(workingfolder,returnobject=FALSE),
            quote=F,row.names=F)

  return(TRUE)
}

## code for getting range of each reach (if being used)
# tt <- pourpoints[pourpoints$pointid %in% rppind,]
# reaches <- tt$reachID %>% unique()
# finalrpp <- matrix(NA,nrow=1,ncol=length(reaches)*2) # store 2 points for each reach
#
# for (j in 1:length(reaches)) {
#   finalrpp[((j-1)*2+1):(j*2)] <- range(tt[tt$reachID==reaches[j],]$pointid)
# }
