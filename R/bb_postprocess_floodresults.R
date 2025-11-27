#' @title Process flood results to produce rasters
#'
#' @description
#' Create a series of results as rasters for depth, velocity, and depth-velocity product.
#'
#' @param hand HAND raster (or file path)
#' @param catchments_streamnodes catchments sf object used in hydraulic calculations (or file path)
#' @param hydraulic_output table of hydraulic_output results, obtained from \code{\link{compute_profile}}
#' @param flow_profiles vector of flow profiles to run computations for
#' @param subset_streamnodes a vector of streamnodes IDs to run postprocessing for
#' @param dhand_method how to find the nearest dhand raster (interpolate or floor)
#' @param workingfolder folder to read inputs from and write outputs to for blackbird operations
#' @param flow_profiles character vector of flow profile names to compute (compute all in hydraulic results otherwise)
#' @param overwrite boolean whether to overwrite existing results file
#' @param return_raster boolean whether to return a SpatRaster with function call
#' @param write_raster boolean to write raster to file#'
#' @param output_file specific file name to write to, if \code{write_raster=TRUE} (writes into the workingfolder/results folder regardless)
#'
#' @return {returns \code{TRUE} if run successfully}
#'
#' @details
#' The results are computed based on the HAND raster for depths. Additional results are computed based
#' on the class properties to determine hydraulic properties in each grid cell.
#'
#' \code{flow_profiles} used to specify which flow profile results have results produced for. If NULL, the full set of
#' flow_profiles from the hydraulic_output object is used.
#'
#' The interpolation scheme is passed through the bbopt$interpolation_postproc_method and can be one of:
#' 1. catchment-hand - will use the HAND raster and apply a catchment-averaged depth within each streamnode catchment
#' 2. interp-hand - will interpolate depths along the river lines, and apply the interpolated depth based on HAND values
#' 3. catchment-dhand - will use the Dynamic HAND raster (possibly interpolated between pre-processed DHAND rasters at the closest depths)
#'     to apply catchment-averaged depth within each streamnode catchment
#' 4. interp-dhand - will interpolate depths along the river lines, and use the (possibly interpolated) Dynamic HAND rasters to
#'     determine depths in each cell
#'
#'
#' @examples
#' library(terra)
#' library(sf)
#'
#' # TO DO XXX
#'
#' @importFrom sf st_write
#' @importFrom terra lapp
#' @export bb_postprocess_floodresults
bb_postprocess_floodresults <- function(bbmodel=NULL,
                                        hydraulic_output=NULL,
                                        flow_profiles=NULL,
                                        subset_streamnodes=NULL,
                                        dhand_method="interpolate",
                                        applyfuzzy=FALSE,
                                        usefuzzyhand=FALSE,
                                        overwrite=TRUE,
                                        return_raster=FALSE,
                                        skip_extent_checks=TRUE,
                                        write_raster=TRUE,
                                        output_file=NULL) {

  # flow_profiles=NULL
  # subset_streamnodes=NULL
  # dhand_method="interpolate"
  # applyfuzzy=FALSE
  # usefuzzyhand=FALSE
  # overwrite=TRUE
  # return_raster=FALSE
  # skip_extent_checks=TRUE
  # write_raster=TRUE
  # output_file=NULL

  hand=NULL
  catchments_streamnodes=NULL
  # dhand_method="interpolate"
  # flow_profiles=NULL
  # overwrite=TRUE
  # return_raster=FALSE
  # write_raster =TRUE
  # # hydraulic_output=NULL
  # # bbgeometry=bbgeom
  # # bbopt=bbopt

  if (return_raster & length(flow_profiles)>1) {
    stop("return_raster is only valid with a single flow profile")
  }

  if (is.null(bbmodel)) {
    stop("bbmodel is required")
  }

  bbgeom <- bbmodel$bbgeo
  bbopt <- bbmodel$bbopt
  workingfolder <- bbopt$workingfolder

  # check inputs
  # find all items in workingfolder if provided
  if (is.null(workingfolder)) {
    stop("workingfolder is required")
  }
  # find the required pieces
  if (!is.null(workingfolder)) {
    if (is.null(catchments_streamnodes)) {
      catchments_streamnodes <- bb_get_catchmentsfromstreamnodesshp(workingfolder)
    }
    if (is.null(hand)) {
      if (!usefuzzyhand) {
        hand <- bb_get_handraster(workingfolder)
      } else {
        hand <- bb_get_fuzzyhandraster(workingfolder)
      }
    }
    if (is.null(hydraulic_output)) {
      hydraulic_output <- bb_get_results_hydraulic_output(workingfolder)
    }
    if ( length(bbopt$interpolation_postproc_method %>% grep("interp-",x=.))>=1 | applyfuzzy) {
      # interpolation, load the hand ID field
      handid <- bb_get_handpourpointIDraster(workingfolder,returnobject = TRUE)
      spp <- bb_get_snappedpourpointshand(workingfolder,returnobject = TRUE)
       # xxx update spp in HAND processing to include reachID in order to handle junctions
      spp$depth <- NA # final depths with WSL corr
      spp$depth_interp <- NA # interpolated (or uninterpolated) depths from streamnode results in hydraulic_output
    }

    if (length(bbopt$interpolation_postproc_method %>% grep("-dhand",x=.))>=1 ) {
      # DHAND, will need the catchment_raster
      catchment_raster <- bb_get_catchmentsfromstreamnodesraster(workingfolder,returnobject = TRUE)
    }

    if (applyfuzzy) {
      # cs <- terra::rast("MadRiverProject/bb_catchmentraster_stack.tiff")
      cs <- bb_get_catchmentstack(workingfolder)
    }
  }

  if (is.null(catchments_streamnodes)) {stop("catchments_streamnodes is required")}
  if (is.null(hydraulic_output)) {stop("hydraulic_output is required")}

  if (is.null(flow_profiles)) {
    flow_profiles <- unique(hydraulic_output$flowprofile)
  }

  if (is.null(subset_streamnodes)) {
    subset_streamnodes <- catchments_streamnodes$pointid
  }
  if (any(subset_streamnodes %notin% catchments_streamnodes$pointid)) {
    stop("Some subset streamnodes not found in catchments_streamnodes, please check")
  }

  if ( length(bbopt$interpolation_postproc_method %>% grep("interp-",x=.))>=1 ) {
    # subset spp based on subset_streamnodes
    spp <- spp[spp$cpointid %in% subset_streamnodes,]
  }

  # check that flow_profiles exist in model and hydraulic results
  if (any(flow_profiles %notin% colnames(bbmodel$bbfp$flowdf))) {
    stop("flow_profiles provided must exist in bbmodel")
  }
  if (any(flow_profiles %notin% hydraulic_output$flowprofile)) {
    stop("flow_profiles provided must exist in bbmodel")
  }

  # check postproc method
  if (bbopt$interpolation_postproc_method %notin% bb_get_interp_postproc_methods()) {
    stop(sprintf("bbopt$interpolation_postproc_method supplied (%s) is not supported.",
                 bbopt$interpolation_postproc_method))
  }

  # check hydraulic output and catchments_streamnodes
  # if (nrow(catchments_streamnodes)*length(flow_profiles) != nrow(hydraulic_output)) {
  #   stop("Mismatch in number of rows between catchments streamnodes and hydraulic output, cannot postprocess data.")
  # }

  ## loop for each flow_profiles
  read_dhands_already <- FALSE
  for (jj in 1:length(flow_profiles)) {

    depth_raster <- NULL
    hdf <- hydraulic_output[hydraulic_output$flowprofile == flow_profiles[jj],]

    if (nrow(catchments_streamnodes) == nrow(hdf)) {
      catchments_streamnodes$depth <- hdf$Depth
    } else {
      stop("Error in matching number of rows in hydraulic output subset and catchments_streamnodes")
    }

    if (bbopt$interpolation_postproc_method == "catchment-hand") { # catchment-hand ----

      if (!applyfuzzy) {
        catchmentdepth <- bb_wbt_rasterize(
          inputvector = catchments_streamnodes,
          field="depth",
          # baseraster=hand,
          baseraster=bb_get_handraster(bbopt$workingfolder,returnobject = FALSE),
          outputfile = tempfile(fileext = ".tif")
        )
        # catchmentdepth <- rast(catchmentdepth) # now returning an object?
        depth_raster <- hand
        values(depth_raster) <- cpp_postprocess_depth(postproc_method = bbopt$interpolation_postproc_method, hand = terra::as.matrix(hand),
                                                      catchmentdepth_ = terra::as.matrix(catchmentdepth))
        names(depth_raster) <- "depth_raster"
      } else {
        # applyfuzzy==TRUE

        # replace each cs with depth value
        depth_raster <- hand
        depth_raster[!is.na(depth_raster)] <- 0 # set to zero for max function usage
        dr <- as.vector(depth_raster)

        # find max depth based on overlapping catchment boundaries
        ## note that this considers the fuzzy downstream tracing through catchment boundaries
        for (i in 1:nrow(catchments_streamnodes)) {
          rrv <- as.vector(cs[[i]])
          ind <- which(rrv>0)
          dd <- apply(catchments_streamnodes$depth[i] - hand[ind],1,FUN=function(x){max(x,0)})
          dr[ind] <- pmax(dr[ind], dd, na.rm=TRUE)
        }
        # assign values and NA
        values(depth_raster) <- dr
        names(depth_raster) <- "depth_raster"
        depth_raster[depth_raster==0] <- NA
      }

    } else if (bbopt$interpolation_postproc_method == "catchment-dhand") { # catchment-dhand ----

      # for each catchment, interpolate closest DHAND raster clips and write to temporary directory
      td <- tempdir()
      if (length(list.files(td))>0) {unlink(list.files(td,full.names = TRUE),recursive = TRUE)}
      for (i in 1:nrow(catchments_streamnodes)) {

        if (catchments_streamnodes$pointid[i] %in% subset_streamnodes) {
          if (catchments_streamnodes$depth[i] %in% bbopt$dhand_Hseq) {
            # get lucky, the depth is exactly in Hseq
            # use the closest dhand raster
            hand_raster <- bb_get_dhandraster(workingfolder=bbopt$workingfolder,returnobject=TRUE,depth = catchments_streamnodes$depth[i],filetype = "depthraster")
            hand <- terra::lapp(c(catchment_raster,hand_raster), fun=function(r1,r2) {
              r2[is.na(r1[])] <- NA
              r2[r1 != catchments_streamnodes$pointid[i]] <- NA
              return(r2)
            })
          } else {
            closest_depths <- bb_closestdepths(x=bbopt$dhand_Hseq, value=catchments_streamnodes$depth[i])

            if (dhand_method=="interpolate") {
              if (length(closest_depths) == 2) {
                if (file.exists(bb_get_dhandraster(workingfolder=bbopt$workingfolder,returnobject=FALSE,depth =closest_depths[1],filetype = "depthraster"))) {
                  dhand1 <- bb_get_dhandraster(workingfolder=bbopt$workingfolder,returnobject=TRUE,depth = closest_depths[1],filetype = "depthraster")
                } else {
                  stop(sprintf("Looking for file that was not found:\n %s",bb_get_dhandraster(workingfolder=bbopt$workingfolder,returnobject=FALSE,depth = closest_depths[1],filetype = "depthraster")))
                }
                if (file.exists(bb_get_dhandraster(workingfolder=bbopt$workingfolder,returnobject=FALSE,depth =closest_depths[2],filetype = "depthraster"))) {
                  dhand2 <- bb_get_dhandraster(workingfolder=bbopt$workingfolder,returnobject=TRUE,depth = closest_depths[2],filetype = "depthraster")
                } else {
                  stop(sprintf("Looking for file that was not found:\n %s",bb_get_dhandraster(workingfolder=bbopt$workingfolder,returnobject=FALSE,depth = closest_depths[1],filetype = "depthraster")))
                }

                hand <- terra::lapp(c(dhand1,dhand2,catchment_raster),fun=function(r1,r2,r3) {
                  # closest_depths[1],closest_depths[2],mm$Depth,
                  d1 <- closest_depths[1]
                  d2 <- closest_depths[2]
                  d <- catchments_streamnodes$depth[i]
                  # overlay with catchment raster and set other cells to NA
                  r1[is.na(r3[])] <- NA
                  r1[r3 != catchments_streamnodes$pointid[i]] <- NA
                  r2[is.na(r3[])] <- NA
                  r2[r3 != catchments_streamnodes$pointid[i]] <- NA
                  # calculate linearly interpolated dhand raster
                  rr <- r1 * (d1-d)/(d1-d2) + r2 * (d-d2)/(d1-d2)
                  return(rr)
                })
              } else {
                # add exception for trying to find closest values to the boundary
                warning(sprintf("Depth of %.4f is outside the bounds of bbopt$dhand_Hseq. Using closest available dhand, though results should be re-run with more dhand rasters to cover this depth",catchments_streamnodes$depth[i]))
                if (file.exists(bb_get_dhandraster(workingfolder=bbopt$workingfolder,returnobject=FALSE,depth =closest_depths[1],filetype = "depthraster"))) {
                  hand_raster <- bb_get_dhandraster(workingfolder=bbopt$workingfolder,returnobject=TRUE,depth = closest_depths[1],filetype = "depthraster")
                  hand <- terra::lapp(c(catchment_raster,hand_raster),fun=function(r1,r2) {
                    r2[is.na(r1[])] <- NA
                    r2[r1 != catchments_streamnodes$pointid[i]] <- NA
                    return(r2)
                  })
                } else {
                  stop(sprintf("Looking for file that was not found:\n %s",bb_get_dhandraster(workingfolder=bbopt$workingfolder,returnobject=FALSE,depth = closest_depths[1],filetype = "depthraster")))
                }
              }
            } else if (dhand_method == "floor") {

              if (file.exists(bb_get_dhandraster(workingfolder=bbopt$workingfolder,returnobject=FALSE,depth =closest_depths[1],filetype = "depthraster"))) {
                hand_raster <- bb_get_dhandraster(workingfolder=bbopt$workingfolder,returnobject=TRUE,depth = closest_depths[1],filetype = "depthraster")
                hand <- terra::lapp(c(catchment_raster,hand_raster),fun=function(r1,r2) {
                  r2[is.na(r1[])] <- NA
                  r2[r1 != catchments_streamnodes$pointid[i]] <- NA
                  return(r2)
                })
                handid_raster <- bb_get_dhandraster(workingfolder=bbopt$workingfolder,returnobject=TRUE,depth = closest_depths[1],filetype = "idraster")
                handid <- terra::lapp(c(catchment_raster,handid_raster),fun=function(r1,r2) {
                  r2[is.na(r1[])] <- NA
                  r2[r1 != catchments_streamnodes$pointid[i]] <- NA
                  return(r2)
                })
              } else {
                stop(sprintf("Looking for file that was not found:\n %s",bb_get_dhandraster(workingfolder=bbopt$workingfolder,returnobject=FALSE,depth = closest_depths[1],filetype = "depthraster")))
              }
            }
          }

          # write DHAND to temporary folder
          terra::writeRaster(hand, filename=sprintf("%s/dhand_temp_streamnode_%i.tiff",td,catchments_streamnodes$pointid[i]))
        }
      }

      # stitch dhand rasters back together
      r <- list.files(path = td, pattern = "dhand_temp_streamnode_*", full.names = TRUE)
      r <- lapply(r, terra::rast)
      hand <- terra::merge(terra::sprc(r))
      rm(r)
      tfhand <- tempfile(fileext=".tif")
      terra::writeRaster(hand,filename=tfhand)

      # xxx cleanup between raster and terra to do here
      # writing hand to file for bb_wbt_rasterize, could be passed direct if terra based
      catchmentdepth <- bb_wbt_rasterize(
        inputvector = catchments_streamnodes,
        field="depth",
        baseraster=tfhand,
        outputfile = tempfile(fileext = ".tif")
      )
      catchmentdepth <- rast(catchmentdepth)
      hand <- rast(tfhand)

      depth_raster <- terra::lapp(c(catchmentdepth,hand),fun=function(r1, r2){
        ifelse(!is.na(r1) & !is.na(r2),
               ifelse(r1>=r2, r1-r2, NA), NA)})

    } else if (bbopt$interpolation_postproc_method == "interp-hand") { # interp-hand ----

      # use the hand pixel IDs to build depths for catchment depths
      # catchments_streamnodes$depth <- hydraulic_output$Depth

      sdf <- bbgeom$get_streamnodeList_as_dataframe()

      if (nrow(sdf) != nrow(catchments_streamnodes)) {
        stop("Mismatch in sdf from bbgeom and catchments_streamnodes, cannot use")
      }
      catchments_streamnodes$upnodeID1 <- sdf$upnodeID1
      catchments_streamnodes$upnodeID2 <- sdf$upnodeID2
      catchments_streamnodes$ds_reach_length <- sdf$ds_reach_length
      catchments_streamnodes$us_reach_length1 <- sdf$us_reach_length1

      ## build depths in each catchment
      for (i in 1:nrow(catchments_streamnodes)) {

        if (catchments_streamnodes$pointid[i] %in% subset_streamnodes) {

          if (catchments_streamnodes$upnodeID1[i] == -1) {
            # headwater node, nothing to interpolate on one side of node
            # just skip interpolation and map depth across all spp nodes within catchment
            sppi <- spp
            sppi$within <- as.numeric(sf::st_within(spp, catchments_streamnodes[i,]))
            sppi <- sppi %>% filter(within==1)
            sppi$depth <- catchments_streamnodes$depth[i]

            ## correct depths based on elevation at sppi nodes
            seqelev <- seq(from=sppi$elev[1], to=sppi$elev[nrow(sppi)], length.out=nrow(sppi))

            ## find max change in depth (from change in elev)
            max_change <- abs((seqelev-sppi$elev)/sppi$depth)

            if (max(max_change)>0.5) {
              warning(sprintf("max change >0.5 detected at streamnode nodeID=%i",sppi$pointid[1]))
            }

            ## Ct is correction on the elevation change
            # Ct = 1 if max_change is 50% or less (<=bbopt$postproc_elev_corr_threshold, default 0.5)
            # 0 < Ct < 1 if max change > bbopt$postproc_elev_corr_threshold (default 0.5)
            # Ct = 0 (i.e. depths are as from HAND) if max_change >= 100% (>=1)
            Ct <- rep(NA,nrow(sppi))
            for (j in 1:nrow(sppi)) {
              Ct[j] <- calc_Ct(max_change[j], bbopt$postproc_elev_corr_threshold)
            }

            # calculate interpolated depths
            sppi$depth <- sppi$depth + (seqelev-sppi$elev)*Ct


          } else if (length(which(catchments_streamnodes$downid == catchments_streamnodes$pointid[i]))>1) {
            # junction catchment with multiple sets of reaches within it
            sppi <- spp
            sppi$within <- as.numeric(sf::st_within(spp, catchments_streamnodes[i,]))
            sppi <- sppi %>% filter(within==1)
            sppi$depth <- catchments_streamnodes$depth[i]

            # sorting in descending order not needed here

            ## trying to do more here - for now just leave as constant depth
            # determine depth at junction, weighted average of depth and length
            # convention: depth 1 at downstream end, depth 2 at one upstream reach segment, depth 3 at the other
            depth1 <- catchments_streamnodes[i,]$depth
            depth2 <- catchments_streamnodes[catchments_streamnodes$pointid == catchments_streamnodes$upnodeID1[i],]$depth
            depth3 <- catchments_streamnodes[catchments_streamnodes$pointid == catchments_streamnodes$upnodeID2[i],]$depth
            L1 <- catchments_streamnodes[i,]$us_reach_length1
            L2 <- catchments_streamnodes[catchments_streamnodes$pointid == catchments_streamnodes$upnodeID1[i],]$ds_reach_length
            L3 <- catchments_streamnodes[catchments_streamnodes$pointid == catchments_streamnodes$upnodeID2[i],]$ds_reach_length
            depth_junction <- (depth1*L1+depth2*L2+depth3*L3)/sum(L1,L2,L3)

            if (is.finite(depth_junction)) {
              # simply apply the junction depth at all non-current reachID nodes, if any
              sppi[sppi$reachID != catchments_streamnodes$reachID[i],]$depth <- depth_junction
            }

          } else {
            # convention - basin 1 is downstream of basin i (not longer used), basin 2 = basin i, and basin 3 is upstream of basin i

            # upstream and downstream depths
            depth2 <- catchments_streamnodes$depth[i]
            # depth1 <- catchments_streamnodes[catchments_streamnodes$pointid == catchments_streamnodes$downid[i],]$depth
            depth3 <- catchments_streamnodes[catchments_streamnodes$pointid == catchments_streamnodes$upnodeID1[i],]$depth

            # get upstream reach lengths
            L2 <- catchments_streamnodes$us_reach_length1[i]

            # filter to current catchment
            sppi <- spp
            sppi$within <- as.numeric(sf::st_within(spp, catchments_streamnodes[i,]))
            sppi <- sppi %>% filter(within==1)

            if (nrow(sppi) == 0) {
              stop(sprintf("No points found in catchment pointid=%i",catchments_streamnodes$pointid[i]))
            } else if (nrow(sppi)==1) {
              # exception if only one point in catchment - just apply depth as is
              sppi$depth <- catchments_streamnodes$depth[i]
            } else {
              # ensure points are in descending order
              if (sppi$downID[1] != sppi$pointid[2]) {
                sppi <- sppi[order(-sppi$pointid),]
                if (sppi$downID[1] != sppi$pointid[2]) {
                  stop("issue in ordering of pointid, downid does not follow convention with pointid")
                }
              }

              # calculate chainage, assume most DS HAND point is on or very close to the streamnode
              sppi$chainage <- seq(L2,0,length.out=nrow(sppi))
              sppi$depth <- depth2 + (depth3-depth2)/L2*sppi$chainage
            }

            ## correct depths based on elevation at sppi nodes
            seqelev <- seq(from=sppi$elev[1], to=sppi$elev[nrow(sppi)], length.out=nrow(sppi))

            ## find max change in depth (from change in elev)
            max_change <- abs((seqelev-sppi$elev)/sppi$depth)

            if (max(max_change)>0.5) {
              warning(sprintf("max change >0.5 detected at streamnode nodeID=%i",sppi$pointid[1]))
            }

            ## Ct is correction on the elevation change
            # Ct = 1 if max_change is 50% or less (<=bbopt$postproc_elev_corr_threshold, default 0.5)
            # 0 < Ct < 1 if max change > bbopt$postproc_elev_corr_threshold (default 0.5)
            # Ct = 0 (i.e. depths are as from HAND) if max_change >= 100% (>=1)
            Ct <- rep(NA,nrow(sppi))
            for (j in 1:nrow(sppi)) {
              Ct[j] <- calc_Ct(max_change[j], bbopt$postproc_elev_corr_threshold)
            }

            # calculate interpolated depths
            sppi$depth <- sppi$depth + (seqelev-sppi$elev)*Ct

            # plot(sppi$elev, type='l', ylim=c(454, 459),lwd=2)
            # lines(sppi$depth+sppi$elev, col='blue',lty=1,lwd=2)
            # lines(seqelev, col='black', lty=2,lwd=2)
            # lines(sppi$depth2+sppi$elev, col='cyan',lty=2,lwd=2)
            # legend('left',
            #        c('Elev','Interp Elev','HAND WSL','Interp WSL'),
            #        col=c('black','black','blue','cyan'),
            #        lty=c(1,2,1,2),
            #        lwd=c(2,2,2,2), inset=0.03)

          }

          # get depths back to main spp
          spp[spp$pointid %in% sppi$pointid,]$depth <- sppi$depth
          rm(sppi)
        }
      }

      # ggplot(spp)+
      #     geom_sf(aes(colour=depth))

      depth_raster <- hand
      values(depth_raster) <- cpp_postprocess_depth(postproc_method = bbopt$interpolation_postproc_method,
                                                    hand = terra::as.matrix(hand), handid_ = terra::as.matrix(handid), spp_ = as.data.frame(spp))
      names(depth_raster) <- "depth_raster"

    } else if (bbopt$interpolation_postproc_method == "interp-dhand") { # interp-dhand ----

      # for each catchment, interpolate closest DHAND raster clips and write to temporary directory
      # also find closest DHAND id raster, clip for catchment and write that to file

      td <- tempdir()
      if (length(list.files(td))>0) {unlink(list.files(td,full.names = TRUE),recursive = TRUE)}
      for (i in 1:nrow(catchments_streamnodes)) {

        if (catchments_streamnodes$pointid[i] %in% subset_streamnodes) {

          if (catchments_streamnodes$depth[i] %in% bbopt$dhand_Hseq) {
            # get lucky, the depth is exactly in Hseq
            # use the closest dhand raster
            hand_raster <- bb_get_dhandraster(workingfolder=bbopt$workingfolder,returnobject=TRUE,depth = catchments_streamnodes$depth[i],filetype = "depthraster")
            hand <- terra::lapp(c(catchment_raster,hand_raster),fun=function(r1,r2) {
              r2[is.na(r1[])] <- NA
              r2[r1 != catchments_streamnodes$pointid[i]] <- NA
              return(r2)
            })
            handid_raster <- bb_get_dhandraster(workingfolder=bbopt$workingfolder,returnobject=TRUE,depth = catchments_streamnodes$depth[i],filetype = "idraster")
            # xxx update all subsetting to be based on ind, much like the preproc?
            handid <- terra::lapp(c(catchment_raster,handid_raster),fun=function(r1,r2) {
              r2[is.na(r1[])] <- NA
              r2[r1 != catchments_streamnodes$pointid[i]] <- NA
              return(r2)
            })
          } else {
            closest_depths <- bb_closestdepths(x=bbopt$dhand_Hseq, value=catchments_streamnodes$depth[i])

            if (dhand_method=="interpolate") {
              if (length(closest_depths) == 2) {
                if (file.exists(bb_get_dhandraster(workingfolder=bbopt$workingfolder,returnobject=FALSE,depth =closest_depths[1],filetype = "depthraster"))) {
                  dhand1 <- bb_get_dhandraster(workingfolder=bbopt$workingfolder,returnobject=TRUE,depth = closest_depths[1],filetype = "depthraster")
                } else {
                  stop(sprintf("Looking for file that was not found:\n %s",bb_get_dhandraster(workingfolder=bbopt$workingfolder,returnobject=FALSE,depth = closest_depths[1],filetype = "depthraster")))
                }
                if (file.exists(bb_get_dhandraster(workingfolder=bbopt$workingfolder,returnobject=FALSE,depth =closest_depths[2],filetype = "depthraster"))) {
                  dhand2 <- bb_get_dhandraster(workingfolder=bbopt$workingfolder,returnobject=TRUE,depth = closest_depths[2],filetype = "depthraster")
                } else {
                  stop(sprintf("Looking for file that was not found:\n %s",bb_get_dhandraster(workingfolder=bbopt$workingfolder,returnobject=FALSE,depth = closest_depths[1],filetype = "depthraster")))
                }

                hand <- terra::lapp(c(dhand1,dhand2,catchment_raster),fun=function(r1,r2,r3) {
                  # closest_depths[1],closest_depths[2],mm$Depth,
                  d1 <- closest_depths[1]
                  d2 <- closest_depths[2]
                  d <- catchments_streamnodes$depth[i]
                  # overlay with catchment raster and set other cells to NA
                  r1[is.na(r3[])] <- NA
                  r1[r3 != catchments_streamnodes$pointid[i]] <- NA
                  r2[is.na(r3[])] <- NA
                  r2[r3 != catchments_streamnodes$pointid[i]] <- NA
                  # calculate linearly interpolated dhand raster
                  rr <- r1 * (d1-d)/(d1-d2) + r2 * (d-d2)/(d1-d2)
                  return(rr)
                })
              } else {
                # add exception for trying to find closest values to the boundary
                warning(sprintf("Depth of %.4f is outside the bounds of bbopt$dhand_Hseq. Using closest available dhand, though results should be re-run with more dhand rasters to cover this depth",catchments_streamnodes$depth[i]))
                if (file.exists(bb_get_dhandraster(workingfolder=bbopt$workingfolder,returnobject=FALSE,depth =closest_depths[1],filetype = "depthraster"))) {
                  hand_raster <- bb_get_dhandraster(workingfolder=bbopt$workingfolder,returnobject=TRUE,depth = closest_depths[1],filetype = "depthraster")
                  hand <- terra::lapp(c(catchment_raster,hand_raster),fun=function(r1,r2) {
                    r2[is.na(r1[])] <- NA
                    r2[r1 != catchments_streamnodes$pointid[i]] <- NA
                    return(r2)
                  })
                } else {
                  stop(sprintf("Looking for file that was not found:\n %s",bb_get_dhandraster(workingfolder=bbopt$workingfolder,returnobject=FALSE,depth = closest_depths[1],filetype = "depthraster")))
                }
              }
            } else if (dhand_method == "floor") {

              if (file.exists(bb_get_dhandraster(workingfolder=bbopt$workingfolder,returnobject=FALSE,depth =closest_depths[1],filetype = "depthraster"))) {
                hand_raster <- bb_get_dhandraster(workingfolder=bbopt$workingfolder,returnobject=TRUE,depth = closest_depths[1],filetype = "depthraster")
                hand <- terra::lapp(c(catchment_raster,hand_raster),fun=function(r1,r2) {
                  r2[is.na(r1[])] <- NA
                  r2[r1 != catchments_streamnodes$pointid[i]] <- NA
                  return(r2)
                })
                handid_raster <- bb_get_dhandraster(workingfolder=bbopt$workingfolder,returnobject=TRUE,depth = closest_depths[1],filetype = "idraster")
                handid <- terra::lapp(c(catchment_raster,handid_raster),fun=function(r1,r2) {
                  r2[is.na(r1[])] <- NA
                  r2[r1 != catchments_streamnodes$pointid[i]] <- NA
                  return(r2)
                })
              } else {
                stop(sprintf("Looking for file that was not found:\n %s",bb_get_dhandraster(workingfolder=bbopt$workingfolder,returnobject=FALSE,depth = closest_depths[1],filetype = "depthraster")))
              }
            }
          }

          # write DHAND rasters to temporary folder
          terra::writeRaster(hand,filename=sprintf("%s/dhand_temp_streamnode_%i.tiff",td,catchments_streamnodes$pointid[i]))
          terra::writeRaster(handid,filename=sprintf("%s/dhand_temp_streamnodeID_%i.tiff",td,catchments_streamnodes$pointid[i]))
        }
      }

      # stitch dhand rasters back together
      r <- list.files(path = td, pattern = "dhand_temp_streamnode_*", full.names = TRUE)
      r <- lapply(r, terra::rast)
      hand <- terra::merge(terra::sprc(r))
      rm(r)
      r <- list.files(path = td, pattern = "dhand_temp_streamnodeID_*", full.names = TRUE)
      r <- lapply(r, terra::rast)
      handid <- terra::merge(terra::sprc(r))
      rm(r)

      # now perform interpolation along channels
      sdf <- bbgeom$get_streamnodeList_as_dataframe()

      if (nrow(sdf) != nrow(catchments_streamnodes)) {
        stop("Mismatch in sdf from bbgeom and catchments_streamnodes, cannot use")
      }
      catchments_streamnodes$upnodeID1 <- sdf$upnodeID1
      catchments_streamnodes$upnodeID2 <- sdf$upnodeID2
      catchments_streamnodes$ds_reach_length <- sdf$ds_reach_length
      catchments_streamnodes$us_reach_length1 <- sdf$us_reach_length1

      ## build depths in each catchment
      for (i in 1:nrow(catchments_streamnodes)) {

        if (catchments_streamnodes$pointid[i] %in% subset_streamnodes) {

          if (catchments_streamnodes$upnodeID1[i] == -1) {
            # headwater node, nothing to interpolate on one side of node
            # just skip interpolation and map depth across all spp nodes within catchment
            sppi <- spp
            sppi$within <- as.numeric(sf::st_within(spp, catchments_streamnodes[i,]))
            sppi <- sppi %>% filter(within==1)
            sppi$depth <- catchments_streamnodes$depth[i]

            ## correct depths based on elevation at sppi nodes
            seqelev <- seq(from=sppi$elev[1], to=sppi$elev[nrow(sppi)], length.out=nrow(sppi))

            ## find max change in depth (from change in elev)
            max_change <- abs((seqelev-sppi$elev)/sppi$depth)

            if (max(max_change)>0.5) {
              warning(sprintf("max change >0.5 detected at streamnode nodeID=%i",sppi$pointid[1]))
            } else {
              # message(sprintf("max change at node ID= %i is %.4f ",sppi$pointid[1], max(max_change) ))
            }

            ## Ct is correction on the elevation change
            # Ct = 1 if max_change is 50% or less (<=bbopt$postproc_elev_corr_threshold, default 0.5)
            # 0 < Ct < 1 if max change > bbopt$postproc_elev_corr_threshold (default 0.5)
            # Ct = 0 (i.e. depths are as from HAND) if max_change >= 100% (>=1)
            Ct <- rep(NA,nrow(sppi))
            for (j in 1:nrow(sppi)) {
              # Ct[j] <- calc_Ct(max_change[j], bbopt$postproc_elev_corr_threshold)
              Ct[j] <- calc_Ct(max_change[j], bbopt$postproc_elev_corr_threshold)
            }

            # calculate interpolated depths
            sppi$depth <- sppi$depth + (seqelev-sppi$elev)*Ct

            # message(sprintf("elevations at node %i changed by average of %.4f", catchments_streamnodes$pointid[i], mean((seqelev-sppi$elev)*Ct)))


            # plot(sppi$elev, type='l', ylim=c(454, 459),lwd=2)
            # lines(sppi$depth+sppi$elev, col='blue',lty=1,lwd=2)
            # lines(seqelev, col='black', lty=2,lwd=2)
            # lines(sppi$depth2+sppi$elev, col='cyan',lty=2,lwd=2)
            # legend('left',
            #        c('Elev','Interp Elev','HAND WSL','Interp WSL'),
            #        col=c('black','black','blue','cyan'),
            #        lty=c(1,2,1,2),
            #        lwd=c(2,2,2,2), inset=0.03)

          } else if (length(which(catchments_streamnodes$downid == catchments_streamnodes$pointid[i]))>1) {
            # junction catchment with multiple sets of reaches within it
            sppi <- spp
            # xxx simplify this to a lookup since spp has pointIDs already, dont need a spatial operation here
            # maybe create another one nad intersect
            sppi$within <- as.numeric(sf::st_within(spp, catchments_streamnodes[i,]))
            sppi <- sppi %>% filter(within==1)
            sppi$depth <- catchments_streamnodes$depth[i]

            # sorting in descending order not needed here

            ## trying to do more here - for now just leave as constant depth
            # determine depth at junction, weighted average of depth and length
            # convention: depth 1 at downstream end, depth 2 at one upstream reach segment, depth 3 at the other
            depth1 <- catchments_streamnodes[i,]$depth
            depth2 <- catchments_streamnodes[catchments_streamnodes$pointid == catchments_streamnodes$upnodeID1[i],]$depth
            depth3 <- catchments_streamnodes[catchments_streamnodes$pointid == catchments_streamnodes$upnodeID2[i],]$depth
            L1 <- catchments_streamnodes[i,]$us_reach_length1
            L2 <- catchments_streamnodes[catchments_streamnodes$pointid == catchments_streamnodes$upnodeID1[i],]$ds_reach_length
            L3 <- catchments_streamnodes[catchments_streamnodes$pointid == catchments_streamnodes$upnodeID2[i],]$ds_reach_length
            depth_junction <- (depth1*L1+depth2*L2+depth3*L3)/sum(L1,L2,L3)

            if (is.finite(depth_junction)) {
              # simply apply the junction depth at all non-current reachID nodes, if any
              sppi[sppi$reachID != catchments_streamnodes$reachID[i],]$depth <- depth_junction
            }

            # # interpolate for each reach, alternatively

          } else {
            # convention - basin 1 is downstream of basin i (not longer used), basin 2 = basin i, and basin 3 is upstream of basin i

            # upstream and downstream depths
            depth2 <- catchments_streamnodes$depth[i]
            # depth1 <- catchments_streamnodes[catchments_streamnodes$pointid == catchments_streamnodes$downid[i],]$depth
            depth3 <- catchments_streamnodes[catchments_streamnodes$pointid == catchments_streamnodes$upnodeID1[i],]$depth

            # get upstream reach lengths
            L2 <- catchments_streamnodes$us_reach_length1[i]

            # filter to current catchment
            sppi <- spp
            sppi$within <- as.numeric(sf::st_within(spp, catchments_streamnodes[i,]))
            sppi <- sppi %>% filter(within==1)

            if (nrow(sppi) == 0) {
              stop(sprintf("No points found in catchment pointid=%i",catchments_streamnodes$pointid[i]))
            } else if (nrow(sppi)==1) {
              # exception if only one point in catchment - just apply depth as is
              sppi$depth <- catchments_streamnodes$depth[i]
            } else {
              # ensure points are in descending order
              if (sppi$downID[1] != sppi$pointid[2]) {
                sppi <- sppi[order(-sppi$pointid),]
                if (sppi$downID[1] != sppi$pointid[2]) {
                  stop("issue in ordering of pointid, downid does not follow convention with pointid")
                }
              }

              # calculate chainage, assume most DS HAND point is on or very close to the streamnode
              sppi$chainage <- seq(L2,0,length.out=nrow(sppi))
              sppi$depth <- depth2 + (depth3-depth2)/L2*sppi$chainage
            }

            ## correct depths based on elevation at sppi nodes
            seqelev <- seq(from=sppi$elev[1], to=sppi$elev[nrow(sppi)], length.out=nrow(sppi))

            ## find max change in depth (from change in elev)
            max_change <- abs((seqelev-sppi$elev)/sppi$depth)

            if (max(max_change)>0.5) {
              warning(sprintf("max change >0.5 detected at streamnode nodeID=%i",sppi$pointid[1]))
            } else {
              # message(sprintf("max change at node ID= %i is %.4f ",sppi$pointid[1], max(max_change) ))
            }

            ## Ct is correction on the elevation change
            # Ct = 1 if max_change is 50% or less (<=bbopt$postproc_elev_corr_threshold, default 0.5)
            # 0 < Ct < 1 if max change > bbopt$postproc_elev_corr_threshold (default 0.5)
            # Ct = 0 (i.e. depths are as from HAND) if max_change >= 100% (>=1)
            Ct <- rep(NA,nrow(sppi))
            for (j in 1:nrow(sppi)) {
              Ct[j] <- calc_Ct(max_change[j], bbopt$postproc_elev_corr_threshold)
            }

            # calculate interpolated depths
            sppi$depth <- sppi$depth + (seqelev-sppi$elev)*Ct

            # message(sprintf("elevations at node %i changed by average of %.4f", catchments_streamnodes$pointid[i], mean((seqelev-sppi$elev)*Ct)))

            # plot(sppi$elev, type='l', ylim=c(454, 459),lwd=2)
            # lines(sppi$depth+sppi$elev, col='blue',lty=1,lwd=2)
            # lines(seqelev, col='black', lty=2,lwd=2)
            # lines(sppi$depth2+sppi$elev, col='cyan',lty=2,lwd=2)
            # legend('left',
            #        c('Elev','Interp Elev','HAND WSL','Interp WSL'),
            #        col=c('black','black','blue','cyan'),
            #        lty=c(1,2,1,2),
            #        lwd=c(2,2,2,2), inset=0.03)
          }

          # get depths back to main spp
          spp[spp$pointid %in% sppi$pointid,]$depth <- sppi$depth
          # rm(sppi)
        }
      }

      # write.csv(spp[,c("pointid","depth")], sprintf("depth_check_%.2f.csv", bbopt$postproc_elev_corr_threshold),
      #           row.names=FALSE,quote=FALSE)

      ## create depth raster from spp by reclassifying handids as depth
      rcl_tbl <- spp %>%
        select(pointid, depth) %>%
        sf::st_drop_geometry() %>%
        as.data.frame()

      catchmentdepth <- terra::classify(x=handid,rcl=rcl_tbl)

      depth_raster <- terra::lapp(c(catchmentdepth,hand),fun=function(r1, r2){
        ifelse(!is.na(r1) & !is.na(r2),
               ifelse(r1>=r2, r1-r2, NA), NA)})

    }  else if (bbopt$interpolation_postproc_method == "interp-dhand-WSLcorr") { # interp-dhand-WSLcorr ----

      ## pseudocode:
      # get all dhand and dhandid layers in a matrix (dimension of xx by lenght(bbopt$dhand_Hseq))
      # create depth raster matrix (of all zeros) with length based on dimensions of subsetnodes

      ## for each streamnode in subset:
      # compute the sppi depths with interpolated depths for given streamnode j
      # adjust depths for WSL corr
      ## for each sppi pointid:
      # grab DHAND segment corresponding to interpolated depth (not WSL corr depth)
      # compute depths
      # update depths in depth matrix

      ## final step
      # overlay depth raster matrix


      # Read in relevant raster files and prepare them for data operations
      catchment_raster <- terra::rast(bb_get_catchmentsfromstreamnodesraster(workingfolder,returnobject = FALSE))
      catchment <- terra::as.matrix(catchment_raster)

      uni <- subset_streamnodes

      ind <- which(catchment %in% uni & !is.na(catchment))
      # notind <- seq(1,length(catchment))[seq(1,length(catchment)) %notin% ind]
      catchment <- catchment[ind]

      # get range of depths, avoid reading in all layers to save processing
      # max(hdf$Depth)
      # do later, would involve checking the WSL corr too
      # read all in now to keep things simple

      # dhand processing (read in and subset)
      if (!read_dhands_already) {
        # redo only if not already read. Limits the heaviest procesing step for multiple profiles

        dhands <- array(data=NA, dim=c(length(bbopt$dhand_Hseq), length(catchment)))
        dhandsid <- array(data=NA, dim=c(length(bbopt$dhand_Hseq), length(catchment)))
        if (bbopt$use_dhand) {
          # dhands <- array(data=NA, dim=c(length(bbopt$Hseq), length(catchment)))
          for(i in 1:length(bbopt$dhand_Hseq)) {

            f_dhand <- bb_get_dhandraster(workingfolder=bbopt$workingfolder,returnobject=FALSE,depth = bbopt$dhand_Hseq[i],filetype = "depthraster")

            if (file.exists(f_dhand)) {
              hand_raster <- terra::rast(f_dhand)

              if (!skip_extent_checks) {
                if (terra::ext(hand_raster) != terra::ext(catchment_raster)) {
                  stop("need to crop/extend dhand raster to match extents of catchment raster before proceeding")
                }
              }

              hand_raster <- terra::as.matrix(hand_raster)
              dhands[i,] <- hand_raster[ind]  # hand_raster[catchment %in% uni]
              dhandsid[i,] <- terra::as.matrix(bb_get_dhandraster(workingfolder=bbopt$workingfolder,returnobject=TRUE,depth = bbopt$dhand_Hseq[i],filetype = "idraster"))[ind]
              rm(hand_raster)
            } else {
              stop(sprintf("Looking for file that was not found:\n %s",f_dhand))
            }
          }

          # check for any NA values in dhands
          for (i in 1:length(bbopt$dhand_Hseq)) {
            if (any(is.na(dhands[i,]))) {
              warning(sprintf("%i NA value(s) found in dhands of length %i with depth of %.3f, check extents and values",
                              length(which(is.na(dhands[i,]))),
                              length(dhands[i,]),
                              bbopt$dhand_Hseq[i]))
            }
          }

          # check for any NA values in dhandsid
          for (i in 1:length(bbopt$dhand_Hseq)) {
            if (any(is.na(dhandsid[i,]))) {
              warning(sprintf("%i NA value(s) found in dhandsid of length %i with depth of %.3f, check extents and values",
                              length(which(is.na(dhandsid[i,]))),
                              length(dhandsid[i,]),
                              bbopt$dhand_Hseq[i]))
            }
          }

        } else {
          stop("Postproc specified to use dhand but bbopt$use_dhand=FALSE. Check this discrepancy")
        }
        read_dhands_already <- TRUE
      }
      rm(catchment_raster) # keep around for clipping other rasters? and for depth map

      print("read dhands")

      # now perform interpolation along channels
      sdf <- bbgeom$get_streamnodeList_as_dataframe()

      if (nrow(sdf) != nrow(catchments_streamnodes)) {
        stop("Mismatch in sdf from bbgeom and catchments_streamnodes, cannot use")
      }
      catchments_streamnodes$upnodeID1 <- sdf$upnodeID1
      catchments_streamnodes$upnodeID2 <- sdf$upnodeID2
      catchments_streamnodes$ds_reach_length <- sdf$ds_reach_length
      catchments_streamnodes$us_reach_length1 <- sdf$us_reach_length1

      ## NOTE - cant subset as we need the info from other depths to interpolate, even if not mapping in that particular catchment/reach
      # subset based on subset_streamnodes (aka uni)
      # catchments_streamnodes <- catchments_streamnodes[catchments_streamnodes$pointid %in% uni,]

      ## build depths in each catchment
      for (i in 1:nrow(catchments_streamnodes)) {

        if (catchments_streamnodes$pointid[i] %in% uni) {

          if (catchments_streamnodes$upnodeID1[i] == -1) {
            # headwater node, nothing to interpolate on one side of node
            # just skip interpolation and map depth across all spp nodes within catchment
            sppi <- spp[spp$cpointid == catchments_streamnodes$pointid[i],]
            sppi$depth_interp <- catchments_streamnodes$depth[i]

            ## correct depths based on elevation at sppi nodes
            seqelev <- seq(from=sppi$elev[1], to=sppi$elev[nrow(sppi)], length.out=nrow(sppi))

            ## find max change in depth (from change in elev)
            max_change <- abs((seqelev-sppi$elev)/sppi$depth_interp)

            if (max(max_change)>0.5) {
              warning(sprintf("max change >0.5 detected at streamnode nodeID=%i",sppi$pointid[1]))
            }
            ## Ct is correction on the elevation change
            # Ct = 1 if max_change is 50% or less (<=bbopt$postproc_elev_corr_threshold, default 0.5)
            # 0 < Ct < 1 if max change > bbopt$postproc_elev_corr_threshold (default 0.5)
            # Ct = 0 (i.e. depths are as from HAND) if max_change >= 100% (>=1)
            Ct <- rep(NA,nrow(sppi))
            for (j in 1:nrow(sppi)) {
              # Ct[j] <- calc_Ct(max_change[j], bbopt$postproc_elev_corr_threshold)
              Ct[j] <- calc_Ct(max_change[j], bbopt$postproc_elev_corr_threshold)
            }

            # calculate interpolated depths
            sppi$depth <- sppi$depth_interp + (seqelev-sppi$elev)*Ct

            # message(sprintf("elevations at node %i changed by average of %.4f", catchments_streamnodes$pointid[i], mean((seqelev-sppi$elev)*Ct)))

            # plot(sppi$elev, type='l', ylim=c(454, 459),lwd=2)
            # lines(sppi$depth+sppi$elev, col='blue',lty=1,lwd=2)
            # lines(seqelev, col='black', lty=2,lwd=2)
            # lines(sppi$depth2+sppi$elev, col='cyan',lty=2,lwd=2)
            # legend('left',
            #        c('Elev','Interp Elev','HAND WSL','Interp WSL'),
            #        col=c('black','black','blue','cyan'),
            #        lty=c(1,2,1,2),
            #        lwd=c(2,2,2,2), inset=0.03)

          } else if (length(which(catchments_streamnodes$downid == catchments_streamnodes$pointid[i]))>1) {
            # junction catchment with multiple sets of reaches within it
            sppi <- spp[spp$cpointid == catchments_streamnodes$pointid[i],]
            sppi$depth_interp <- catchments_streamnodes$depth[i]

            # sorting in descending order not needed here

            ## trying to do more here - for now just leave as constant depth
            # determine depth at junction, weighted average of depth and length
            # convention: depth 1 at downstream end, depth 2 at one upstream reach segment, depth 3 at the other
            depth1 <- catchments_streamnodes[i,]$depth
            depth2 <- catchments_streamnodes[catchments_streamnodes$pointid == catchments_streamnodes$upnodeID1[i],]$depth
            depth3 <- catchments_streamnodes[catchments_streamnodes$pointid == catchments_streamnodes$upnodeID2[i],]$depth
            L1 <- catchments_streamnodes[i,]$us_reach_length1
            L2 <- catchments_streamnodes[catchments_streamnodes$pointid == catchments_streamnodes$upnodeID1[i],]$ds_reach_length
            L3 <- catchments_streamnodes[catchments_streamnodes$pointid == catchments_streamnodes$upnodeID2[i],]$ds_reach_length
            depth_junction <- (depth1*L1+depth2*L2+depth3*L3)/sum(L1,L2,L3)

            if (is.finite(depth_junction)) {
              # simply apply the junction depth at all non-current reachID nodes, if any
              sppi[sppi$reachID != catchments_streamnodes$reachID[i],]$depth <- depth_junction
            }

          } else {
            # convention - basin 1 is downstream of basin i (not longer used), basin 2 = basin i, and basin 3 is upstream of basin i

            # upstream and downstream depths
            depth2 <- catchments_streamnodes$depth[i]
            # depth1 <- catchments_streamnodes[catchments_streamnodes$pointid == catchments_streamnodes$downid[i],]$depth
            depth3 <- catchments_streamnodes[catchments_streamnodes$pointid == catchments_streamnodes$upnodeID1[i],]$depth

            # get upstream reach lengths
            L2 <- catchments_streamnodes$us_reach_length1[i]

            # filter to current catchment
            sppi <- spp[spp$cpointid == catchments_streamnodes$pointid[i],]
            sppi$depth_interp <- catchments_streamnodes$depth[i]

            if (nrow(sppi) == 0) {
              stop(sprintf("No points found in catchment pointid=%i",catchments_streamnodes$pointid[i]))
            } else if (nrow(sppi)==1) {
              # exception if only one point in catchment - just apply depth as is
              sppi$depth_interp <- catchments_streamnodes$depth[i]
            } else {
              # ensure points are in descending order
              if (sppi$downID[1] != sppi$pointid[2]) {
                sppi <- sppi[order(-sppi$pointid),]
                if (sppi$downID[1] != sppi$pointid[2]) {
                  stop("issue in ordering of pointid, downid does not follow convention with pointid")
                }
              }

              # calculate chainage, assume most DS HAND point is on or very close to the streamnode
              sppi$chainage <- seq(L2,0,length.out=nrow(sppi))
              sppi$depth_interp <- depth2 + (depth3-depth2)/L2*sppi$chainage
            }

            ## correct depths based on elevation at sppi nodes
            seqelev <- seq(from=sppi$elev[1], to=sppi$elev[nrow(sppi)], length.out=nrow(sppi))

            ## find max change in depth (from change in elev)
            max_change <- abs((seqelev-sppi$elev)/sppi$depth_interp)

            if (max(max_change)>0.5) {
              warning(sprintf("max change >0.5 detected at streamnode nodeID=%i",sppi$pointid[1]))
            } else {
              # message(sprintf("max change at node ID= %i is %.4f ",sppi$pointid[1], max(max_change) ))
            }

            ## Ct is correction on the elevation change
            # Ct = 1 if max_change is 50% or less (<=bbopt$postproc_elev_corr_threshold, default 0.5)
            # 0 < Ct < 1 if max change > bbopt$postproc_elev_corr_threshold (default 0.5)
            # Ct = 0 (i.e. depths are as from HAND) if max_change >= 100% (>=1)
            Ct <- rep(NA,nrow(sppi))
            for (j in 1:nrow(sppi)) {
              Ct[j] <- calc_Ct(max_change[j], bbopt$postproc_elev_corr_threshold)
            }

            # calculate interpolated depths
            sppi$depth <- sppi$depth_interp + (seqelev-sppi$elev)*Ct

            # message(sprintf("elevations at node %i changed by average of %.4f", catchments_streamnodes$pointid[i], mean((seqelev-sppi$elev)*Ct)))

            # plot(sppi$elev, type='l', ylim=c(454, 459),lwd=2)
            # lines(sppi$depth+sppi$elev, col='blue',lty=1,lwd=2)
            # lines(seqelev, col='black', lty=2,lwd=2)
            # lines(sppi$depth2+sppi$elev, col='cyan',lty=2,lwd=2)
            # legend('left',
            #        c('Elev','Interp Elev','HAND WSL','Interp WSL'),
            #        col=c('black','black','blue','cyan'),
            #        lty=c(1,2,1,2),
            #        lwd=c(2,2,2,2), inset=0.03)
          }

          # get depths back to main spp
          spp[spp$pointid %in% sppi$pointid,]$depth_interp <- sppi$depth_interp
          spp[spp$pointid %in% sppi$pointid,]$depth <- sppi$depth
          # rm(sppi)
        }
      }
      rm(sppi)

      print("computed all depth for reaches")

      # main depth matrix
      depthmm <- catchment*0

      # compute all depths for specific point within reaches
      for (i in spp$pointid) {
        if (i %% 100 == 0 ) {
          print(sprintf("on row %i of %i",i,nrow(spp)))
        }
        closest_depth <- max(bb_closestdepths(x=bbopt$dhand_Hseq, value=spp$depth_interp[i]))
        dhand_ind <- which(bbopt$dhand_Hseq == closest_depth)

        # candidate to pass to cpp??
        depthmm[ dhandsid[dhand_ind,] == spp$pointid[i]] <-
          spp$depth[i] - dhands[dhand_ind,][dhandsid[dhand_ind,] == spp$pointid[i]]
      }

      print("computed all depth raster values")

      # get depths back into final raster
      depth_raster <- terra::rast(bb_get_catchmentsfromstreamnodesraster(workingfolder,returnobject = FALSE))*0
      depth_raster[ind] <- depthmm
      depth_raster[depth_raster<0] <- 0 # negative depth correction (when dhat < Hi)
      depth_raster[depth_raster==0] <- NA # set zero depth to NA

    } else {
      stop(sprintf("Unrecognized bbopt$interpolation_postproc_method option: %s",
                   bbopt$interpolation_postproc_method))
    }

    if (!is.null(depth_raster)) {
      # write depth raster as main flood result depth raster
      if (bbopt$use_dhand) {
        preprocmethod_string="dhand"
      } else {
        preprocmethod_string="hand"
      }

      if (write_raster) {

        if (is.null(output_file)) {
          depthrr_file <- bb_get_results_raster(workingfolder,returnobject=FALSE, flowprofile=flow_profiles[jj],
                                                modeltype=bbopt$modeltype,
                                                preprocmethod=preprocmethod_string,
                                                interpmethod=bbopt$interpolation_postproc_method, filetype="depthraster")
        } else {
          depthrr_file <- sprintf("%s/results/%s",bbopt$workingfolder,output_file)
        }

        if (!dir.exists(sprintf("%s/results/",bbopt$workingfolder))) {dir.create(sprintf("%s/results/",bbopt$workingfolder))}
        # depthrr_file <- bb_get_results_depthraster(workingfolder=workingfolder, returnobject=FALSE)
        terra::writeRaster(depth_raster, depthrr_file, overwrite=overwrite)
        message(sprintf("Depth raster written to file (%s).",depthrr_file))
      }

      if (return_raster) {
        return(depth_raster)
      }

    } else {
      stop("Error occurred in computing the depth raster")
    }

  }

  # check for velocity request
  # if (bbopt$output_velocity) {
  #   message("velocity raster not available yet!")
  # }
  #
  # if (bbopt$output_depthvelocityproduct) {
  #   message("depth-velocity raster not available yet!")
  #   if (!bbopt$output_velocity) {
  #     # compute velocity raster first
  #     message("Will need to computing velocity raster (as part of depth-velocity product)")
  #   }
  #   # compute depth-velocity product
  # }

  return(TRUE)
}
