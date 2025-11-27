
### ~~~~~~~~~~~~~~~~~~
### SETUP CLASSES ----
### ~~~~~~~~~~~~~~~~~~

## Import classes from other packages ##
##
## Import SpatRaster from raster package to use in class definition

# require(raster)
require(sf)
require(Rcpp)
require(terra)

# RasterLayer <- getFromNamespace(".__C__RasterLayer", "raster")
# sf <- getFromNamespace("sf", "sf")
## end of importing classes from other packages


### ~~~~~~~~~~~~~~~~~~~~~
### STREAMNODE CLASS ----
### ~~~~~~~~~~~~~~~~~~~~~

#' Stream node class
#'
#' streamnode:
#' A reference class to use as basic unit of calculation in 1D model and contains static hydraulic properties.
#' Is the parent class for catchmentnode, xsection, xsectionspatial, structurenode, and customnode.
#'
#' @field nodeID ID of streamnode
#' @field nodetype type of node ('xsection','catchment','bridge')
#' @field downnodeID ID of the downstream node
#' @field upnodeID1 ID of the upstream node
#' @field upnodeID2 ID of the second upstream node if applicable
#' @field stationname name of the stream node as character
#' @field station station location along profile as numeric value
#' @field reachID the reachID corresponding to the river reach on which the streamnode is defined
#' @field isjunction boolean whether streamnode is a junction
#' @field contraction_coeff contraction coefficient for use in energy loss calculation
#' @field expansion_coeff expansion coefficient for use in energy loss calculation
#' @field ds_reach_length length in metres between streamnode and next downstream one
#' @field us_reach_length1 length in metres between streamnode and next upstream one (or junction)
#' @field us_reach_length2 length in metres between streamnode and next upstream one, averaging upstream branches (used only with junction nodes)
#' @field catchmentshp a shapefile as sf of the catchment boundary (obsolete)
#' @field catchmentnode a shapefile as sf of the streamnode location (xxx TO DO to streamnodeshp, would be more appropriate) (obsolete)
#' @field reachshp reach sf object (obsolete)
#' @field depthdf preprocessed depth-parameter table of values
#' @field handraster a raster of HAND (height above nearest drainage) values as a file path
#' @field min_elev minimum elevation of streamnode used in calculations
#' @field bed_slope average channel slope within streamnode (used in hand-manning calculations)
#' @field roughmult multiplier on roughness applied at particular streamnode (not including global multiplier)
streamnode <- setRefClass("streamnode",
    contains=c("sf"),

  field=list(nodeID="integer", nodetype="character", downnodeID="integer", upnodeID1="integer",upnodeID2="integer",
             stationname="character",station="numeric",reachID="integer",isjunction="logical",
             contraction_coeff="numeric",expansion_coeff="numeric",
             ds_reach_length="numeric", us_reach_length1="numeric", us_reach_length2="numeric",
             catchmentshp="sf", catchmentnode="sf", reachshp="sf",
             depthdf="data.frame", handraster="character", min_elev="numeric",bed_slope="numeric",roughmult="numeric"),
  method = list(initialize =
                  function(..., nodeID=as.integer(-1), nodetype="", downnodeID=as.integer(-1),upnodeID1=as.integer(-1),upnodeID2=as.integer(-1),
                           stationname=as.character(station),station=-99,reachID=as.integer(-99),isjunction=FALSE,
                           contraction_coeff=0.1,expansion_coeff=0.3,
                           ds_reach_length=-99, us_reach_length1=-99, us_reach_length2=-99,
                           catchmentshp=st_sf(st_sfc()),catchmentnode=st_sf(st_sfc()),
                           reachshp=st_sf(st_sfc()), depthdf=data.frame(), handraster="", min_elev=-99, bed_slope=-99,roughmult=1.0)
                  {
                    callSuper(..., nodeID=nodeID, nodetype=nodetype, downnodeID=downnodeID,upnodeID1=upnodeID1,upnodeID2=upnodeID2,
                              stationname = stationname, station = station, reachID=reachID,isjunction=isjunction,
                              contraction_coeff=contraction_coeff,expansion_coeff=expansion_coeff,
                              ds_reach_length=ds_reach_length, us_reach_length1=us_reach_length1,  us_reach_length2=us_reach_length2,
                              catchmentshp = catchmentshp, catchmentnode=catchmentnode,
                              reachshp=reachshp, depthdf=depthdf, handraster=handraster, min_elev=min_elev, bed_slope=bed_slope,roughmult=roughmult)
                  },

                # creates the initial mm profile
                generate_initial_hydraulic_profile = function(nrow=1) {

                  # initialize hydraulic data for this streamnode
                  mm <- bb_hydraulic_output_emptydf(nrow=nrow)

                  mm$Min_Elev <- .self$min_elev

                  mm$stationname <- .self$stationname
                  mm$station <- .self$station
                  mm$reach_length_DS <- .self$ds_reach_length
                  mm$reach_length_US1 <- .self$us_reach_length1
                  mm$reach_length_US2 <- .self$us_reach_length2

                  return(mm)
                },

                # pre-process calculations for various depths
                compute_preprocessed_depthdf = function(bbopt) {
                  if (class(bbopt) != "bb_options") {
                    stop("bbopt must be of class bb_options")
                  }

                  # seqH <- bbopt$Hseq

                  # check that seqQ is non-empty
                  if (length(bbopt$Hseq) <= 1) {
                    stop("multiple flow points are required to pre-process depth tables,\nplease check the dH/minH/maxH/Hseq blackbird options")
                  }

                  mm <- .self$bb_hydraulic_output_emptydf_propsonly(nrow=length(bbopt$Hseq))
                  mm$Depth <- bbopt$Hseq
                  mm$WSL <- mm$Min_Elev + mm$Depth

                  for (i in 1:nrow(mm)) {
                    mm[i,] <- .self$compute_basic_depth_properties(
                      WSL=mm[i,]$WSL,
                      mm=mm[i,],
                      bbopt=bbopt)
                  }
                  .self$depthdf <- mm
                },
                # compute the normal depth with a provided Flow and Slope
                # update to just use mm, Slope, init_WSL, bbopt
                compute_normal_depth = function(Flow=NA,Slope=NA,init_WSL=NA,bbopt) {

                  if (is.na(Flow)) {
                    stop("Flow is a required parameter.")
                  }
                  if (class(Flow) != "numeric") {
                    stop("Flow must be of class numeric.")
                  }
                  if (is.na(Slope)) {
                    stop("Slope is a required parameter.")
                  }
                  if (class(Slope) != "numeric") {
                    stop("Slope must be of class numeric.")
                  }
                  if (class(bbopt) != "bb_options") {
                    stop("bbopt must be of class bb_options")
                  }

                  # consider evaluating this once and avoid multiple times in this calculation
                  # min_elev <- .self$min_elev

                  ### Calculation routine
                  mm <- .self$generate_initial_hydraulic_profile()
                  # update to use provided mm instead?

                  if (is.na(init_WSL) | init_WSL==-99) {

                    if (.self$nodetype == "xsection") {
                      # estimate initial WSL from velocity of 3m/s and Flow rate
                      area_req <- Flow/3

                      # function to minimize distance to area_req
                      ff =function(x) {abs(area_req - .self$calculate_flow_area(x))}

                      rr <- optim(par=c(.self$min_elev+2),
                            fn=ff,
                            method="Brent",
                            lower=.self$min_elev,
                            upper=.self$min_elev+1000) # arbitrary limit of 1000m depth

                    if (rr$par > .self$min_elev & rr$par < .self$min_elev+1000) {
                      init_WSL <- rr$par
                    } else {
                      init_WSL <- .self$min_elev+1
                      warning("Optimizer for initial WSL failed to converge")
                    }

                    ## if catchment ...

                    } else {
                      # xxx TO DO - build out functionality for catchment as well
                      init_WSL <- mm$Min_Elev + 1 # initial point as 1m depth, could be improved
                    }
                  }

                  mm <- .self$compute_profile(Flow=Flow, WSL=init_WSL, mm=mm, bbopt=bbopt)
                  # override Sf for normal depth
                  mm$Sf <- mm$Sf_Avg <- Slope

                  # error difference in
                  # errdiff <- rep(NA,bbopt$iteration_limit_nd)
                  err_lag2 <- NA # rep(NA,bbopt$iteration_limit_nd)
                  err_lag1 <- NA # rep(NA,bbopt$iteration_limit_nd)
                  prevWSL_lag1 <- NA
                  prevWSL_lag2 <- NA
                  # prevWSL_lag1 <- rep(NA,bbopt$iteration_limit_nd)
                  # prevWSL_lag2 <- rep(NA,bbopt$iteration_limit_nd)

                  # print("initial profile computed")

                  for (j in 1:(bbopt$iteration_limit_nd)) {

                    if (j == 1) {
                      prevWSL_lag2 <- NA
                      prevWSL_lag1 <- mm$WSL
                      max_depth_change <- 50 # arbitarily high number
                    } else {
                      prevWSL_lag2 <- prevWSL_lag1
                      prevWSL_lag1 <- mm$WSL
                      # max_depth_change <- 0.5*mm$Depth
                      max_depth_change <- 0.5*(0.5*(prevWSL_lag2+prevWSL_lag1-2*mm$Min_Elev)) # avg of last two depths
                    }

                    if (!(bbopt$silent_cp)) {
                      if (j == 1 | j %% 10 == 0) {
                        print(sprintf("normal depth: iteration %i: -----",j))
                      }
                    }

                    if (j>=2) {
                      mm <- .self$compute_profile(Flow=Flow, WSL=mm$WSL, mm=mm, bbopt=bbopt)
                      # override Sf for normal depth
                      mm$Sf <- mm$Sf_Avg <- Slope
                    }

                    RHS <- mm$K_Total*sqrt(Slope)

                    # assume the next computed WSL based on the ratio of Flow/RHS
                    comp_WSL <- mm$Min_Elev + mm$Depth*Flow/RHS

                    # checks/modifications against Min_Elev
                    if (comp_WSL <= mm$Min_Elev) {
                      if (mm$Flow == 0) {
                        comp_WSL <- mm$Min_Elev
                      } else {
                        # set as min elev + 0.1
                        comp_WSL <- max(mm$Min_Elev+0.1, comp_WSL)
                      }
                    }

                    # errdiff[j] <- Flow-RHS
                    err_lag2 <- err_lag1
                    err_lag1 <- comp_WSL - prevWSL_lag1 # Flow-RHS
                    err_diff <- err_lag2 - err_lag1
                    assum_diff <- prevWSL_lag2 - prevWSL_lag1

                    if (abs(err_lag1) > bbopt$tolerance_nd) {

                      # terminate if iteration limit reached
                      if (j >= bbopt$iteration_limit_nd) {
                        warning("Iteration limit on normal depth calculation exceeded, terminating normal depth calculation.")
                        return(mm)
                      }

                      # get new WSL for next iteration
                      if ( j == 1) {
                        # setting based on second trial rules
                        proposed_WSL <- mm$WSL + 0.7*(err_lag1) # bbopt$next_WSL_split_cp

                        if (abs(proposed_WSL - mm$WSL) > max_depth_change) {
                          mm$WSL <- mm$WSL + (proposed_WSL-mm$WSL)*(max_depth_change/abs(proposed_WSL-mm$WSL))
                        } else {
                          mm$WSL <- proposed_WSL
                        }
                      } else {

                        # aligned algorithm here with bb_hyd_compute_profile

                        if (abs(err_diff) < 0.003 | j > (bbopt$iteration_limit_nd/2) | j>20) {
                          # for small error differences, secant method can fail
                          # take average of prevWSL_lag1 and prevWSL_lag2
                          # xxx to do - consider rate limitation basedon depth here too?
                          proposed_WSL <- mean(c(comp_WSL,prevWSL_lag1))

                          # change by half of max_depth_change in direction of comp_WSL if massive swing
                          if (abs(comp_WSL-prevWSL_lag1)>max_depth_change) {
                            proposed_WSL <- prevWSL_lag1 +0.5*max_depth_change*((comp_WSL-prevWSL_lag1)/abs(comp_WSL-prevWSL_lag1))
                          }

                          mm$WSL <- proposed_WSL

                        } else {
                          # secant method
                          proposed_WSL <- prevWSL_lag2 - err_lag2*assum_diff/err_diff

                          if (abs(proposed_WSL - mm$WSL) > max_depth_change) {
                            mm$WSL <- mm$WSL + (proposed_WSL-mm$WSL)*(max_depth_change/abs(proposed_WSL-mm$WSL))
                          } else {
                            mm$WSL <- proposed_WSL
                          }
                        }
                      }
                      mm$Depth <- mm$WSL-mm$Min_Elev

                    } else {
                      if (!(bbopt$silent_nd)) {
                        print("Normal depth estimated successfully.")
                      }
                      return(mm)
                    }
                  }
                },
                # use the interpolated tables from .self$depthdf to return mm from WSL
                compute_basic_depth_properties_interpolation = function(WSL=NULL, mm=NULL, bbopt=NULL) {
                  # use to retrieve interpolation from given WSL

                  # check that depthdf exists
                  if (nrow(.self$depthdf) == 0) {
                    stop("depthdf has not been computed, please run compute_preprocessed_depthdf first.")
                  }

                  # check that bbopt$use_preproc is TRUE
                  if (!bbopt$use_preproc) {
                    stop("use_prepreoc is set to FALSE in blackbird options, please set to TRUE.")
                  }

                  # check that supplied mm and lookup mm match in min elev and station
                  if (mm$stationname != .self$depthdf$stationname[1] |
                      round(mm$Min_Elev,4) != round(.self$depthdf$Min_Elev[1],4) |
                      round(mm$reach_length_US1,4) != round(.self$depthdf$reach_length_US1[1],4)) {
                    stop("check properties in interpolation, they do not match those in the provided mm structure")
                  }

                  # specify fields in mm to interpolate
                  # should include only those in the derived hydraulic properties now
                  names_interp <- c("Depth","K_Total","alpha","Area","HRadius","WetPerimeter",
                                    "Manning_Composite","Length_Effective","HydDepth","TopWidth",
                                    "K_Total_areaconv","K_Total_disconv","K_Total_roughconv",
                                    "alpha_areaconv","alpha_disconv","alpha_roughconv",
                                    "nc_equalforce","nc_equalvelocity","nc_wavgwp","nc_wavgarea","nc_wavgconv")

                  if (WSL < min(.self$depthdf$WSL) | WSL > max(.self$depthdf$WSL)) {

                    if (bbopt$interp_extraplotion_method == "stoponerror") {
                      stop(sprintf("WSL provided (%.2f) is outside of the range in depthdf [%.2f, %.2f]",
                                   WSL, min(.self$depthdf$WSL), max(.self$depthdf$WSL)))
                    } else {
                      # extrapolate
                      # warning(sprintf("WSL provided (%.2f) is outside of the range in depthdf [%.2f, %.2f] in calculating flow %.2f,\nExtrapolating to continue computation.",
                      #                 WSL, min(.self$depthdf$WSL), max(.self$depthdf$WSL),mm$Flow))

                      # perform linear extrapolation on each column in names_interp
                      # use specific number of end points as defined by bbopt$num_extrapolation_points
                      if (bbopt$num_extrapolation_points >= length(.self$depthdf$WSL) | bbopt$num_extrapolation_points < 0) {
                        nxp <- length(.self$depthdf$WSL)-1
                      } else {
                        nxp <- bbopt$num_extrapolation_points
                      }

                      if (WSL < min(.self$depthdf$WSL)) {
                        indpoints <- seq(1,nxp)
                      } else {
                        indpoints <- seq(length(.self$depthdf$WSL)-nxp,length(.self$depthdf$WSL))
                      }
                      lmx <- .self$depthdf$WSL[indpoints]
                      mm$Depth <- WSL-mm$Min_Elev[1]
                      for (i in 2:length(names_interp)) {
                        lmy <- .self$depthdf[indpoints,names_interp[i]]
                        fit <- lm(lmy~lmx)
                        mm[1, names_interp[i]] <- as.numeric(fit$coefficients[1]+fit$coefficients[2]*WSL)
                      }
                    }
                  } else {
                    # WSL is within the range of depthdf
                    # perform interpolation on each column in names_interp
                    for (i in 1:length(names_interp)) {

                      # fix for some series that have NaN values
                      if (length(which(is.finite(.self$depthdf[,names_interp[i]])))==1) {
                        warning(sprintf(""))
                        mm[1, names_interp[i]] <- mean(.self$depthdf[,names_interp[i]],na.rm=TRUE)
                      } else {
                        mm[1, names_interp[i]] <-
                          approx(x=.self$depthdf$WSL, y=.self$depthdf[,names_interp[i]], xout=WSL,
                                 rule=1, na.rm=TRUE)$y
                      }
                    }
                  }

                  # adjustments based on options

                  # define the adjusted effective length
                  mm$Length_EffectiveAdjusted <- mm$Length_Effective # until it gets assigned elsewhere

                  ## roughness multiplier
                  if (bbopt$roughness_multiplier != 1) {
                    mm$K_Total <- mm$K_Total / bbopt$roughness_multiplier
                    mm$Manning_Composite <- mm$Manning_Composite * bbopt$roughness_multiplier
                  }
                  if (.self$roughmult != 1) {
                    mm$K_Total <- mm$K_Total / .self$roughmult
                    mm$Manning_Composite <- mm$Manning_Composite * .self$roughmult
                  }

                  if (bbopt$enforce_delta_Leff) { # xxx add check here that it is a catchment, probably don't want to do this for pre-defined xsection lengths

                    # xxx to do - add a warning when enforce_delta_Leff gets used and boundary hit

                    if (mm$reach_length_US2 != -99) {
                      reach_length <- mm$reach_length_US2
                    } else {
                        reach_length <- mm$reach_length_US1
                    }
                    if (mm$Length_Effective < reach_length*(1-bbopt$delta_reachlength)) {

                      message(sprintf("Enforcing Leff on node with nodeID %i",mm$nodeID))

                      mm$Length_EffectiveAdjusted <- reach_length*(1-bbopt$delta_reachlength)
                      Leff_ratio <- mm$Length_EffectiveAdjusted / mm$Length_Effective

                      if (bbopt$catchment_integration_method == "effective_length") {
                        # update properties yes based on updated Leff
                        mm$Area <- mm$Area * (mm$Length_EffectiveAdjusted / mm$Length_Effective)
                        mm$WetPerimeter <- mm$WetPerimeter * (mm$Length_EffectiveAdjusted / mm$Length_Effective)
                        mm$K_Total <- mm$K_Total * (mm$Length_EffectiveAdjusted / mm$Length_Effective)
                        mm$TopWidth <- mm$TopWidth * (mm$Length_EffectiveAdjusted / mm$Length_Effective)
                        mm$HydDepth <- mm$HydDepth * (mm$Length_EffectiveAdjusted / mm$Length_Effective)
                      }

                    } else if (mm$Length_Effective > reach_length*(1+bbopt$delta_reachlength)) {
                      message(sprintf("Enforcing Leff on node with nodeID %i",mm$nodeID))

                      mm$Length_EffectiveAdjusted  <- reach_length*(1+bbopt$delta_reachlength)
                      Leff_ratio <- mm$Length_EffectiveAdjusted  / mm$Length_Effective

                      if (bbopt$catchment_integration_method == "effective_length") {
                        # update properties yes based on updated Leff
                        mm$Area <- mm$Area * (mm$Length_EffectiveAdjusted / mm$Length_Effective)
                        mm$WetPerimeter <- mm$WetPerimeter * (mm$Length_EffectiveAdjusted / mm$Length_Effective)
                        mm$K_Total <- mm$K_Total * (mm$Length_EffectiveAdjusted / mm$Length_Effective)
                        mm$TopWidth <- mm$TopWidth * (mm$Length_EffectiveAdjusted / mm$Length_Effective)
                        mm$HydDepth <- mm$HydDepth * (mm$Length_EffectiveAdjusted / mm$Length_Effective)
                      }
                    }
                  }

                  ## assumes calculations on Leff
                  # xxx to do - cleanup with the preprocessing code
                  if (bbopt$catchment_integration_method != "effective_length") {
                    # correct based on ratio of reach length and not effective length
                    if (mm$reach_length_US2 != -99) {
                      reach_length <- mm$reach_length_US2
                    } else {
                        reach_length <- mm$reach_length_US1
                    }
                    mm$Area <- mm$Area * mm$Length_Effective / reach_length
                    mm$WetPerimeter <- mm$WetPerimeter * mm$Length_Effective / reach_length
                    mm$K_Total <- mm$K_Total * mm$Length_Effective / reach_length
                    mm$TopWidth <- mm$TopWidth * mm$Length_Effective / reach_length
                    mm$HydDepth <- mm$HydDepth * mm$Length_Effective / reach_length
                  }

                  return(mm)
                },
                # takes mm as a single row
                 compute_profile = function(Flow=NULL, WSL=NULL, mm=NULL, bbopt=NULL) {

                  if (nrow(mm) > 1) {
                    stop("mm should be supplied as a single row")
                  }

                  # xxx to do - add some rm statements to clear memory as not needed
                  mm$Flow <- Flow
                  mm$WSL <- WSL

                  # add exception for zero flow
                  if (mm$Flow == 0 | mm$Flow <= bbopt$tolerance_nd) {
                    ss <- c("In computing hydraulic profile, flow is found to be zero or within tolerance of zero.",
                            "Setting depth estimate to zero.") %>%
                      warning()
                    mm$WSL <- mm$Min_Elev
                  }

                  if (!is.finite(mm$WSL)) {
                    stop("WSL not finite, something diverged.")
                  }

                  # add exception for WSL less than min elev
                  if (mm$WSL < mm$Min_Elev) {
                    mm$WSL <- mm$Min_Elev
                  }

                  # calculate Depth from WSL
                  mm$Depth <- mm$WSL - mm$Min_Elev

                  # print(sprintf("previous WSL is %.2f, Sf is %.2e",
                  #               mm$WSL, mm$Sf))

                  if (bbopt$use_preproc) {
                    # interpolate from depthdf
                    mm <- .self$compute_basic_depth_properties_interpolation(
                      WSL=mm$WSL,
                      mm=mm,
                      bbopt=bbopt)
                  } else {
                    mm <- .self$compute_basic_depth_properties(WSL=mm$WSL,
                                                               mm=mm,
                                                               bbopt=bbopt)
                  }

                  # now compute the flow-driven properties
                  mm <- .self$compute_basic_flow_properties(Flow=Flow,
                                                        mm=mm,
                                                        bbopt=bbopt)
                  return(mm)
                },

                compute_profile_next = function(Flow=NULL, WSL=NULL, mm=NULL, prevmm=NULL, bbopt=NULL) {

                  ## replace with initial configuration call
                  mm <- .self$compute_profile(Flow=Flow, WSL=WSL, mm=mm, bbopt=bbopt)

                  ## updated Sf if needed based on catchment integration method
                  if (bbopt$catchment_integration_method != "effective_length") {
                    # correct based on ratio of reach length and not effective length
                    if (mm$reach_length_US2 != -99) {
                      reach_length <- mm$reach_length_US2
                    } else {
                      reach_length <- mm$reach_length_US1
                    }
                    mm$Sf <- mm$Sf * (mm$Length_Effective / reach_length)^2
                  }

                  ## compute average friction slope
                  if (bbopt$friction_slope_method == "average_conveyance") {
                    if (any(unlist(lapply(c(mm$Flow,prevmm$Flow,mm$K_Total,prevmm$K_Total),function(x) {
                      if(is.na(x) | is.nan(x) | is.infinite(x)){
                        return(TRUE)
                      } else {
                        return(FALSE)
                        }
                    } )))) {
                      # use average friction instead if NA or NaN
                      mm$Sf_Avg <- mean(c(mm$Sf,prevmm$Sf),na.rm=TRUE)
                    } else {
                      # use normal conveyance approach
                      mm$Sf_Avg <- ((mm$Flow+prevmm$Flow)/(mm$K_Total+prevmm$K_Total))^2
                    }
                  } else if (bbopt$friction_slope_method == "average_friction") {
                    mm$Sf_Avg <- mean(c(mm$Sf,prevmm$Sf),na.rm=TRUE)
                  } else if (bbopt$friction_slope_method == "geometric_friction") {
                    mm$Sf_Avg <- sqrt(mm$Sf*prevmm$Sf)
                  } else if (bbopt$friction_slope_method == "harmonic_friction") {
                    mm$Sf_Avg <- 2*(mm$Sf*prevmm$Sf)/(mm$Sf+prevmm$Sf)
                  } else if (bbopt$friction_slope_method == "reach_friction") {
                    if (bbopt$regimetype=="subcritical") {
                      mm$Sf_Avg <- prevmm$Sf
                    } else {
                      mm$Sf_Avg <- mm$Sf
                    }
                  } else if (bbopt$friction_slope_method == "ds_friction") {
                      mm$Sf_Avg <- prevmm$Sf
                  } else if (bbopt$friction_slope_method == "us_friction") {
                      mm$Sf_Avg <- mm$Sf
                  } else {
                    stop("Unrecognized bbopt$friction_slope_method. Check against bb_get_friction_methods()")
                  }

                  # calculate head loss, he
                  loss_coeff <- 0
                  if (prevmm$Velocity_head > mm$Velocity_head) {
                    # downstream velocity head greater than upstream  velocity
                    #   water contracting moving downstream, use contraction coeff
                    loss_coeff <- .self$contraction_coeff
                  } else {
                    # downstream velocity less than upstream velocity,
                    #   water expanding moving downstream, use expansion coeff
                    loss_coeff <- .self$expansion_coeff
                  }

                  if (bbopt$Leff_method == "average_Leff") {
                    mm$Length_EnergyLoss <- mean(c(mm$Length_EffectiveAdjusted,prevmm$Length_EffectiveAdjusted))
                  } else if (bbopt$Leff_method == "ds_Leff") {
                    mm$Length_EnergyLoss <- prevmm$Length_EffectiveAdjusted
                  } else if (bbopt$Leff_method == "us_Leff") {
                    mm$Length_EnergyLoss <- mm$Length_EffectiveAdjusted
                  } else {
                    mm$Length_EnergyLoss <- mm$Length_EffectiveAdjusted # us_Leff
                  }

                  mm$Head_Loss <- mm$Length_EnergyLoss*mm$Sf_Avg + loss_coeff*abs(mm$alpha*mm$Velocity^2/2/bbopt$g - prevmm$alpha*prevmm$Velocity^2/2/bbopt$g)

                  return(mm)
                })
)


### ~~~~~~~~~~~~~~~~~~~~
### CATCHMENT CLASS ----
### ~~~~~~~~~~~~~~~~~~~~

#' Catchment Object
#'
#' catchment:
#' A reference class to create a catchment with all relevant data and static hydraulic properties.
#' This class captures a geospatial catchment and includes all properties required for hydraulic
#' calculations.
#'
#'  note - the fields below should be ideally consistent with the hand pre-processing steps
#'
#'  possibly to add to catchment class
#'  - zdrainage
#'  - rivershp
#'  - snapped streamnode
#'
#'
#' @field demraster an unconditioned digital elevation model (DEM) as a file path
#' @field demcond a hydrologically conditioned DEM as a file path
#' @field nraster a raster of Mannings n values within the catchment boundary as a file path
#' @field catchmentraster a raster of overall catchments, where the catchment object is defined by its nodeID in the raster
catchment <- setRefClass("catchment",
   contains=c("streamnode"), # "sf"
  field=list(demraster="character",demcond="character",nraster="character",catchmentraster="character"),

  ## xxx TO DO - http://adv-r.had.co.nz/R5.html
  ## split the methods into a separate file (perhaps a methods .R file for each class?)

  method = list(initialize =
                  function(..., demraster="",demcond="",nraster="",catchmentraster="")
                  {
                    require(sf)

                    callSuper(..., demraster = demraster, demcond = demcond, nraster=nraster,catchmentraster=catchmentraster) # add nodetype="catchment" ?? xxx
                  },

  calc_min_elev = function(set=TRUE) {

    # xxx to do - replace with terra

    catchment_raster <- raster::raster(.self$catchmentraster)
    hand_raster <- raster::raster(.self$handraster)
    dem_raster <- raster::raster(.self$demraster)

    if (!bb_check_extents(catchment_raster,dem_raster)) {
      stop("Need to have raster extents match exactly for calc_min_elev")
    }

    # if using dhand would use dhand with depth zero, equivalent to hand raster
    hand <- raster::overlay(catchment_raster,hand_raster,fun=function(r1,r2) {
      r2[is.na(r1[])] <- NA
      r2[r1 != .self$nodeID] <- NA
      return(r2)
    })

    dem <- raster::overlay(catchment_raster,dem_raster,fun=function(r1,r2) {
      r2[is.na(r1[])] <- NA
      r2[r1 != .self$nodeID] <- NA
      return(r2)
    })

    handmin <- raster::cellStats(hand, stat='min',na.rm=TRUE)
    if (handmin != 0) {
      warning(sprintf("minimum HAND value in station %s is greater than zero (%.2e)",.self$stationname, handmin))
      # set to greater of handmin and 0.1m
      handmin <- max(handmin, 0.1)
    }

    newrr <- raster::overlay(dem,hand,fun=function(dem,hand) {ifelse(hand <= handmin, dem, NA)})

    # set this as the mean (basin-averaged hand=0 elevation) or min (min elevation of all hand=0 values)
    return_elev <- mean(raster::as.matrix(newrr), na.rm=TRUE)

    if (set) {.self$min_elev <- return_elev}

    return(return_elev)
  },

  # compute the basic hydraulic properties related to depth only
  # uses an existing mm that is provided
  # this function uses raster calcs, and properties can be pre-processed
  compute_basic_depth_properties = function(WSL=NULL, mm=NULL, bbopt=NULL) {

    ## xxx function is obsolete, use the cpp code instead
    stop("function is obsolete, use the cpp code instead")

    ## add equivalent of top width, froude number, hydraulic depth to catchment-based calculation

    if (nrow(mm) > 1) {
      stop("mm should be supplied as a single row")
    }

    if (bbopt$catchment_conveyance_method != "areaweighted_conveyance") {
      stop("Catchment conveyance methods other than 'areaweighted_conveyance' are not supported.")
    }

    workingfolder <- bbopt$workingfolder

    # xxx to do - add some rm statements to clear memory as not needed
    mm$WSL <- WSL

    # could create issues if min elev is lower outside of flooded area?
    # mm$Min_Elev <- .self$min_elev
    # need a more efficient way to do this and avoid calling get_min_elev unnecessarily!!!
    # XXX TO DO
    # somehow pass the initial mm on to next function

    mm$Depth <- mm$WSL - mm$Min_Elev

    # need to reproject rasters into a geographic CS for calculation of areas
    # crs4calcs <- bb_crs4calc()

    # read in actual rasters and get local catchment ones
    ## clean up the use of bb_get or .self stored paths
    ## perhaps the bb_get is easier after all, and easier than storing in bbgeom explicitly
    # catchment_raster <- raster::raster(.self$catchmentraster)
    # hand_raster <- raster::raster(.self$handraster)
    # dem_raster <- raster::raster(.self$demraster)
    # manningsn_raster <- raster::raster(.self$nraster)
    catchment_raster <- bb_get_catchmentsfromstreamnodesraster(workingfolder,returnobject = TRUE)
    # hand_raster <- bb_get_handraster(workingfolder = workingfolder, returnobject = TRUE)
    # dem_raster <- bb_get_demraster(workingfolder = workingfolder, returnobject = TRUE)
    # manningsn_raster <- bb_get_manningsnraster(workingfolder = workingfolder, returnobject = TRUE)
    # reachlength_raster <- bb_get_reachlengthraster(workingfolder, returnobject = TRUE)
    # slope_raster <- bb_get_sloperaster(workingfolder,returnobject = TRUE)




    # ccr <- raster::overlay(catchment_raster,fun=function(r1) {
    #   r1[is.na(r1[])] <- NA
    #   r1[r1 != .self$nodeID] <- NA
    #   return(r1)
    # })

    ## xxx add terra::trim to each block here, trim to non-NA area after processing
    # will make other steps much faster
    # not needed for catchment_raster though

    dem_raster <- bb_get_demraster(workingfolder = workingfolder, returnobject = TRUE)
    dem <- raster::overlay(catchment_raster,dem_raster,fun=function(r1,r2) {
      r2[is.na(r1[])] <- NA
      r2[r1 != .self$nodeID] <- NA
      # r2 <- terra::trim(r2)
      return(r2)
    })
    dem <- terra::trim(dem) # trim NA values
    # use dem extent for cropping other rasters
    rm(dem_raster) # clean up memory immediately

    if (bbopt$use_dhand) {
      ## use dhand method

      # assume bbopt$dhand_Hseq is accurate to files in folder
      # find closest depths to Hseq
      if (mm$Depth %in% bbopt$dhand_Hseq) {
        # no interpolation required, just use the right file

        if (file.exists(bb_get_dhandraster(workingfolder=bbopt$workingfolder,returnobject=FALSE,depth = mm$Depth,filetype = "depthraster"))) {
          hand_raster <- bb_get_dhandraster(workingfolder=bbopt$workingfolder,returnobject=TRUE,depth = mm$Depth,filetype = "depthraster")
        } else {
          stop(sprintf("Looking for file that was not found:\n %s",bb_get_dhandraster(workingfolder=bbopt$workingfolder,returnobject=FALSE,depth = mm$Depth,filetype = "depthraster")))
        }

      } else {
        # find the two closest files, if just one use the closest and throw a warning
        closest_depths <- bb_closestdepths(x=bbopt$dhand_Hseq, value=mm$Depth)

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

          hand_raster <- raster::overlay(dhand1,dhand2,catchment_raster,fun=function(r1,r2,r3) {
            # closest_depths[1],closest_depths[2],mm$Depth,
            d1 <- closest_depths[1]
            d2 <- closest_depths[2]
            d <- mm$Depth
            # overlay with catchment raster and set other cells to NA
            r1[is.na(r3[])] <- NA
            r1[r3 != .self$nodeID] <- NA
            r2[is.na(r3[])] <- NA
            r2[r3 != .self$nodeID] <- NA
            # calculate linearly interpolated dhand raster
            rr <- r1 * (d1-d)/(d1-d2) + r2 * (d-d2)/(d1-d2)
            return(rr)
          })
        } else {
          # add exception for trying to find closest values to the boundary
          warning("Depth of %.4f is outside the bounds of bbopt$dhand_Hseq. Using closest available dhand, though results should be re-run with more dhand rasters to cover this depth")
          if (file.exists(bb_get_dhandraster(workingfolder=bbopt$workingfolder,returnobject=FALSE,depth =closest_depths[1],filetype = "depthraster"))) {
            hand_raster <- bb_get_dhandraster(workingfolder=bbopt$workingfolder,returnobject=TRUE,depth = closest_depths[1],filetype = "depthraster")
          } else {
            stop(sprintf("Looking for file that was not found:\n %s",bb_get_dhandraster(workingfolder=bbopt$workingfolder,returnobject=FALSE,depth = closest_depths[1],filetype = "depthraster")))
          }
        }
      }

    } else {
      ## use hand raster method without dhand
      hand_raster <- bb_get_handraster(workingfolder = workingfolder, returnobject = TRUE)
    }
    hand <- raster::overlay(catchment_raster,hand_raster,fun=function(r1,r2) {
      r2[is.na(r1[])] <- NA
      r2[r1 != .self$nodeID] <- NA
      # r2 <- terra::trim(r2)
      return(r2)
    })
    hand <- terra::crop(hand, dem)
    rm(hand_raster) # clean up memory immediately

    manningsn_raster <- bb_get_manningsnraster(workingfolder = workingfolder, returnobject = TRUE)
    nraster <- raster::overlay(catchment_raster,manningsn_raster,fun=function(r1,r2) {
      r2[is.na(r1[])] <- NA
      r2[r1 != .self$nodeID] <- NA
      # r2 <- ifelse(!is.na(r1) & !is.na(r2), r2, NA)
      # r2 <- terra::trim(r2)
      return(r2)
    })
    nraster <- terra::crop(nraster, dem)
    rm(manningsn_raster) # clean up memory immediately


    # xxx add option to skip reachlength calculation if method is hand-manning
    # xxx decide if using effective reachlength or physical reach length from gis
    # if (!bbopt$modeltype == "hand-manning") {
    reachlength_raster <- bb_get_reachlengthraster(workingfolder, returnobject = TRUE)
    reachlengthr <- raster::overlay(catchment_raster,reachlength_raster,fun=function(r1,r2) {
      r2[is.na(r1[])] <- NA
      r2[r1 != .self$nodeID] <- NA
      # r2 <- ifelse(!is.na(r1) & !is.na(r2), r2, NA)
      # r2 <- terra::trim(r2)
      return(r2)
    })
    reachlengthr <- terra::crop(reachlengthr, dem)
    rm(reachlength_raster) # clean up memory immediately
    # }

    slope_raster <- bb_get_sloperaster(workingfolder,returnobject = TRUE)
    sloper <- raster::overlay(catchment_raster,slope_raster,fun=function(r1,r2) {
      r2[is.na(r1[])] <- NA
      r2[r1 != .self$nodeID] <- NA
      # r2 <- ifelse(!is.na(r1) & !is.na(r2), r2, NA)
      # r2 <- terra::trim(r2)
      return(r2)
    })
    sloper <- terra::crop(sloper, dem)
    rm(slope_raster) # clean up memory immediately

    rm(catchment_raster) # clean up memory immediately

    ## check extents to see if everything still aligned?

    # checks against options
    if (bbopt$catchment_conveyance_method %notin% bb_get_catchment_conveyance_methods()) {
      warning(sprintf("Unrecognized bbopt - catchment_conveyance_method '%s'. Using default (discretized_conveyance)",
                      bbopt$catchment_conveyance_method))
      bbopt$catchment_conveyance_method <- "discretized_conveyance"
    }

    # check bbopt$Manning_composite_method for recognized methods
    if (bbopt$Manning_composite_method %notin% bb_get_composite_Manning_methods()) {
      warning(sprintf("Unrecognized bbopt - Manning_composite_method '%s'. Using default (equal_velocity)",
                      bbopt$Manning_composite_method))
      bbopt$Manning_composite_method <- "equal_velocity"
    }



    # project to crs4calcs
    # dem <- raster::projectRaster(dem,crs=crs4calcs)
    # hand <- raster::projectRaster(hand,crs=crs4calcs)
    # nraster <- raster::projectRaster(nraster,crs=crs4calcs)
    # reachlengthr <- raster::projectRaster(reachlengthr,crs=crs4calcs)
    # sloper <- raster::projectRaster(sloper,crs=crs4calcs)


    ### basic cell properties ----
    a <- res(dem)
    if (a[1] == a[2]) { # check for square dem one more time
      a <- a[1]
    } else {
      stop("dem must be square resolution")
    }

    Ai_rr <- a*sqrt(a^2*(1+(tan(sloper))^2))      # area of each cell
    dimin_rr <- max(hand - 0.5*a*tan(sloper),0)   # min depth for flooding
    dimax_rr <- hand + 0.5*a*tan(sloper)          # depth for complete inundation
    di_rr <- max(mm$Depth - dimin_rr,0)           # depth in cell (relative to dmin)

    # assume projected raster in units of metres
    # flooded area of each cell
    Aif_rr <- raster::overlay(di_rr,dimin_rr,dimax_rr,Ai_rr,sloper,
                              fun=function(di_rr,dimin_rr,dimax_rr,Ai_rr,sloper) {
                                ifelse(di_rr <= dimin_rr, 0,  # no water
                                       ifelse(di_rr >= dimax_rr, Ai_rr, # completely inundated
                                              pmin(a*sqrt((di_rr/tan(sloper))^2),Ai_rr) # xxx need to fix this line for mid areas // changed to using pmin for element-wise
                                       ))
                              })
    # flooded volume in each cell
    Vif_rr <- raster::overlay(di_rr,dimin_rr,dimax_rr,Ai_rr,sloper,
                              fun=function(di_rr,dimin_rr,dimax_rr,Ai_rr,sloper) {
                                ifelse(di_rr <= dimin_rr, 0,  # no water
                                       ifelse(di_rr >= dimax_rr, 0.5*a^2*(dimax_rr-dimin_rr)+a^2*(di_rr-dimax_rr),  # completely inundated
                                              0.5*a^2*(dimax_rr-dimin_rr) # xxx need to fix this line for mid areas
                                       ))
                              })

    Af <- sum(Aif_rr[!is.na(Aif_rr)]) # total flooded area, m2
    Vf <- sum(Vif_rr[!is.na(Vif_rr)]) # total flooded volume, m3

    ### hydraulic properties from rasters ----

    # Pi_rr <- Aif_rr / a   # wetted perimeter in each cell
    # P <- sum(Pi_rr[!is.na(Pi_rr)]) # total wetted perimeter
    Rhi_rr <- Vif_rr / Aif_rr # hydraulic radius in each cell

    # conveyance calculations
    Vif_nn_rr <- raster::overlay(nraster,Vif_rr,
                                 fun=function(nraster,Vif_rr) {nraster*Vif_rr})
    K <- sum(Vif_nn_rr[!is.na(Vif_nn_rr)])*(Vf/Af)^(2/3) # total conveyance from areaweighted_conveyance method

    # compute individual conveyances, and correct individual conveyance based on ratio of K/K_distsum
    # (xxx if needed??)
    Ki_rr <- (1/nraster)*Vif_rr*(Rhi_rr)^(2/3) # conveyance in each cell
    K_distsum <- sum(Ki_rr[!is.na(Ki_rr)]) # total conveyance summed from individual values, m3/s
    Ki_rr <- Ki_rr*(K/K_distsum)

    # xxx reflect this change in the cpp version
    # compute Leff as conveyance-weighted value
    if (bbopt$modeltype != "hand-manning") {
      Leff_K_prod <- Ki_rr * reachlengthr  # reachlengthr = Li
      Leff <- ifelse(mm$Depth == 0, as.numeric(sf::st_length(.self$reachshp)), (1/K)*sum(Leff_K_prod[!is.na(Leff_K_prod)]))
    } else {
      Leff <- as.numeric(sf::st_length(.self$reachshp))
    }
    # temp1 <- (Rhi_rr)
    # temp2 <- (Rhi_rr)^(1)
    # temp3 <- (Rhi_rr)^(2)
    # temp4 <- (Rhi_rr)^(2/3)

    ### hydraulic properties for 1D ----

    A1D <- finiteorzero(Vf / Leff)
    P1D <- finiteorzero(Af / Leff) # or P/Leff?
    Rh1D <- finiteorzero(A1D / P1D)
    K1D <- finiteorzero(K / Leff)

    # alpha calculation and check
    Ki3_Vif2_ratio <- Ki_rr^3 / Vif_rr^2
    Ki3_Vif2_ratio[Ki3_Vif2_ratio ==0] <- NA

    # update with area-weighted method
    alpha <- 1 # finiteorone((Vf^2 / K^3) * sum(Ki3_Vif2_ratio[!is.na(Ki3_Vif2_ratio)]))

    # xxx check alpha value, computation here yields a very low value (nearly 0)

    # if (alpha < 1.0 | alpha > 4) {
    #   warning(sprintf("alpha computed as %2g, should be bounded by approx [1.0, 4.0]. Alpha set to boundary value.",alpha))
    #   if (alpha < 1.0) {alpha <- 1.0}
    #   if (alpha > 4.0) {alpha <- 4.0}
    # }


    # composite mannings n check
    # Pi_ni_prod <- Pi_rr * nraster^2
    # nc <- finiteorzero(sqrt(sum(Pi_ni_prod[!is.na(Pi_ni_prod)])) / sqrt(sum(Pi_rr[!is.na(Pi_rr)])))

    if (Af == 0 | mm$Depth == 0) {
      # message("For a depth or Af of 0, using an area-weighted average of Mannings n where cells have a HAND value of 0")

      Ai_ni_prod <- raster::overlay(Ai_rr,nraster,hand,fun=function(Ar,nr,hr) {
        r1 <- Ar*nr
        r1[hr > 0] <- NA
        return(r1) })
      Ai_hand0 <- raster::overlay(Ai_rr,hand,fun=function(Ar,hr) {
        Ar[hr >0] <- NA
        return(Ar) })
      nc <- finiteorzero( sum(Ai_ni_prod[!is.na(Ai_ni_prod)]) / sum(Ai_hand0[!is.na(Ai_hand0)]) )
    } else {
      # xxx could add others here, but really just want to support weighted_average_area
      # weighted_average_wetperimeter
      # weighted_average_conveyance
      if (bbopt$Manning_composite_method == "equal_force") {
        # equivalent method - total force equation
        Aif_ni_prod <- Aif_rr * nraster^2
        nc <- finiteorzero(sqrt(sum(Aif_ni_prod[!is.na(Aif_ni_prod)])) / sqrt(Af))

      } else if (bbopt$Manning_composite_method == "weighted_average_area") {
        # simple area-weighted average
        Aif_ni_prod <- Aif_rr * nraster
        nc <- finiteorzero(sum(Aif_ni_prod[!is.na(Aif_ni_prod)]) / Af)

      } else if (bbopt$Manning_composite_method == "equal_velocity") {
        Aif_ni_prod <- Aif_rr * nraster^(1.5)
        nc <- finiteorzero( (sum(Aif_ni_prod[!is.na(Aif_ni_prod)]) / Af)^(2/3) )
      } else {
        stop("Unrecognized bbopt - Manning_composite_method.")
      }
    }

    # reality check on nc just in case
    if (nc < cellStats(nraster,min) | nc > cellStats(nraster,max) ){
      warning(sprintf("composite Mannings n computed as %2g, should be bounded by landuse raster min and max values [%2g, %2g]. Value set to boundary value.",
                      nc,cellStats(nraster,min),cellStats(nraster,max)))
      if (nc < cellStats(nraster,min)) {nc <- cellStats(nraster,min)}
      if (nc > cellStats(nraster,max)) {nc <- cellStats(nraster,max)}
    }

    ### assign values to mm ----
    mm$Area <- A1D
    mm$WetPerimeter <- P1D
    mm$HRadius <- Rh1D
    mm$K_Total <- K1D
    mm$alpha <- alpha
    mm$Manning_Composite <- nc
    mm$Length_Effective <- Leff

    mm$TopWidth <- 123 ## xxx to update
    mm$HydDepth <- 1.23 ## xxx to update

    # calculate bed slope (for Manning method)
    if (bbopt$modeltype == "hand-manning") {
      mm$Sbed <- .self$bed_slope
    }

    # get dmin, dmax each cell
    # mm$Depth, hand, slope

    # # get depth in each catchment cell
    # temprr <- raster::overlay(hand,fun=function(r1) {ifelse(r1 < mm$Depth, r1, NA)})
    # depthrr <- mm$Depth-temprr
    # rm(temprr)
    # depthrr <- raster::projectRaster(depthrr,crs=crs4calcs)
    #
    #
    # # demcond <- raster::projectRaster(demcond,crs=crs4calcs)
    # nraster <- raster::projectRaster(nraster,crs=crs4calcs)
    #
    # # get catchment flooded 'area'
    # ## area
    # cellsizerr <- raster::area(depthrr,na.rm=TRUE)
    # cellsizerr <- cellsizerr * 1e6 # convert to m2
    # cell_size <- cellsizerr[!is.na(cellsizerr)]
    # flooded_area_m2 <- max(as.numeric(length(cell_size)*median(cell_size)),0)
    #
    # # reasonable estimate of reach_length as sqrt(area) (assuming area is square)
    # # reach_length <- sqrt(flooded_area_m2) #metres
    # #
    # # xxx make it a function of method in the options tag
    # # currently assume subcritical and moving upstream, therefore using upstream reach length
    # reach_length <- .self$us_reach_length1
    # # to do - update to consider multiple upstream reach lengths for junctions
    #
    # ## catchment flooded volume
    # # volumerr <- (cellsizerr * depthrr)
    # volumerr <- raster::overlay(depthrr,cellsizerr,fun=function(r1, r2){ifelse(!is.na(r1) & !is.na(r2), r1*r2, NA)})
    # # plot(volumerr)
    # volume_m3 <- sum(volumerr[!is.na(volumerr)])
    #
    # mm$Area <- volume_m3/reach_length
    # # individual flood area is cellsizerr
    #
    # mm$WetPerimeter <- flooded_area_m2 / reach_length
    # # to do xxx note: fix for wetted area based on slope of each grid cell from Zheng et al. 2018 (river channel and geometry...)
    #
    # # mm$HRadius <- finiteorzero(volume_m3 / flooded_area_m2) # note: reach length cancels out
    # mm$HRadius <- finiteorzero(mm$Area / mm$WetPerimeter) # equivalent
    #
    # # consider if this should be a sum of hradius rr instead, better practice than using totals?
    # # hradiusrr <- raster::overlay(volumerr,cellsizerr,fun=function(r1, r2){ifelse(!is.na(r1) & !is.na(r2), r1/r2, NA)})
    # # mm$HRadius <-
    # # sum(hradiusrr[!is.na(hradiusrr)]) / reach_length
    # # produces different result
    #
    #
    # # correct nraster
    # # nraster <- overlay(nraster,cellsizerr,fun=function(r1, r2){ifelse(!is.na(r1) & !is.na(r2), r1, NA)})
    # # volumerr <- overlay(depthrr,cellsizerr,fun=function(r1, r2){ifelse(!is.na(r1) & !is.na(r2), r1*r2, NA)})
    # nraster_corr <- raster::resample(nraster, cellsizerr)  # xxx switch to terra?
    #
    # conveyrr <- raster::overlay(nraster_corr,cellsizerr,volumerr,
    #                     fun=function(r1n, r2a, r3v){
    #                       ifelse(!is.na(r1n) & !is.na(r2a) &!is.na(r3v),
    #                              (1/r1n)*(r2a)*((r3v/r2a)^2/3),
    #                              NA)})
    # mm$K_Total <- sum(conveyrr[!is.na(conveyrr)],na.rm=TRUE) / reach_length
    #
    # # compute alpha based on conveyance
    # # alpharr <- flowrr*(velocityrr^2)  / (mm$Flow*mm$Velocity^2)
    #
    # # alpha = sum(Q*velocity^2) / (Qtotal*velocitytotal^2)
    # # alpharr <- flowrr*(velocityrr^2)  / (mm$Flow*mm$Velocity^2)
    # # mm$alpha <- finiteorzero(sum(alpharr[!is.na(alpharr)]))
    # # mm$alpha <- (mm$Flow_LOB*mm$Velocity_LOB^2 + mm$Flow_Main*mm$Velocity_Main^2 + mm$Flow_ROB*mm$Velocity_ROB^2) / (mm$Flow*mm$Velocity^2)
    #
    # alpharr <- raster::overlay(volumerr/reach_length,conveyrr/reach_length,
    #                     fun=function(r1a, r2c){
    #                       ifelse(!is.na(r1a) & !is.na(r2c),
    #                              r2c^3/r1a^2,
    #                              NA)})
    # mm$alpha <- sum(alpharr[!is.na(alpharr)],na.rm=TRUE)*(mm$Area^2)/(mm$K_Total^3)
    # mm$alpha <- finiteorone(mm$alpha)
    #
    # # try composite Manning's n based on conveyance-weighted average
    # # more stable than using the wetted perimeter equation
    # manningcomprr <- raster::overlay(conveyrr,nraster_corr,
    #                                  fun=function(r1, r2){
    #                                    ifelse(!is.na(r1) & !is.na(r2),
    #                                           r1*r2,
    #                                           NA)})
    # mm$Manning_Composite <- finiteorzero((sum(manningcomprr[!is.na(manningcomprr)]))/(sum(conveyrr[!is.na(conveyrr)])))
    #
    # # exception for zero flow sitation - add in non-zero Mannings n for interpolation purposes
    # if (mm$Manning_Composite == 0 ) {
    #   mm$Manning_Composite <- mean(nraster_corr[!is.na(nraster_corr)])
    # }

    # get average upslope flowpath length
    # temporarily xxx just use reach length, eventually replace with an average flowpath calculation
    # that is weighted by flow in each cell
    # mm$Length_Effective <- reach_length

    return(mm)
  },

  # compute the basic hydraulic properties for flow
  # (uses initial properties with mm from compute_basic_depth_properties)
  # note that these properties in this function rely on non-raster calcs
  # and are not pre-processed
  compute_basic_flow_properties = function(Flow=NULL, mm=NULL, bbopt=NULL) {

    if (nrow(mm) > 1) {stop("mm should be supplied as a single row")}

    mm$Flow <- Flow
    mm$Velocity <- finiteorzero(mm$Flow/mm$Area)
    mm$Velocity_head <- vhead_calc(mm$alpha,mm$Velocity,bbopt$g)
    mm$Energy_total <- mm$Velocity_head + mm$WSL
    mm$Froude <- froude_calc(mm$Velocity,bbopt$g,mm$HydDepth)
    mm$Sf <- finiteorzero((mm$Flow/mm$K_Total)^2)

    # if (mm$Sf > 1.0) {
    #   warning(sprintf("in catchment compute_basic_flow_properties: Sf with %.4e exceeds 1.0, likely divergence in calculation",mm$Sf))
    # }
    return(mm)
  })

)

### ~~~~~~~~~~~~~~~~~~~~
### XSECTION CLASS ----
### ~~~~~~~~~~~~~~~~~~~~

#' Cross-Section Object
#'
#' xsection:
#' A reference class to create a cross-section with all relevant data and static hydraulic properties.
#' This class is consistent with a regularly defined cross-section, and is not strictly tied to linear geospatial features
#' other than a point and station reference on the supplied river shapefile.
#'
#' @field xx horizontal coordinates of 1D section
#' @field zz vertical elevation coordinates of 1D section, corresponding to xx
#' @field Manning vector of Manning's n values corresponding to xx (currently not used in blackbird)
#' @field Manning_LOB Manning's n value for the left overbank area
#' @field Manning_Main Manning's n value for the main channel area
#' @field Manning_ROB Manning's n value for the right overbank area
#' @field lbs_xx horizontal co-ordindate of the left bank station
#' @field rbs_xx horizontal co-ordindate of the right bank station
#' @field ds_length_LOB downstream length of the left channel overbank
#' @field ds_length_Main downstream length of the main channel (to the next cross-section)
#' @field ds_length_ROB downstream length of the right channel overbank
#' @field xsectionshp file path to shapefile of cross-section (assume that shapefile has multiple sections with an ID column)
xsection <- setRefClass("xsection",
    contains=c("streamnode"),
    field=list(xx="numeric",zz="numeric",Manning="numeric",
               Manning_LOB="numeric",Manning_Main="numeric",Manning_ROB="numeric",
               lbs_xx="numeric",rbs_xx="numeric",ds_length_LOB="numeric",ds_length_Main="numeric",
               ds_length_ROB="numeric",contraction_coeff="numeric",expansion_coeff="numeric",xsectionshp="character"),
    method = list(initialize =
    function(...,
             xx=c(0),zz=c(0),Manning=c(0),
             Manning_LOB=-1.0,Manning_Main=-1.0,Manning_ROB=-1.0,
             lbs_xx=0,rbs_xx=0,ds_length_LOB=0,ds_length_Main=0,
             ds_length_ROB=0, contraction_coeff=0.1,expansion_coeff=0.3,xsectionshp="")

  {
      callSuper(..., xx = xx,zz=zz,Manning=Manning,
                Manning_LOB=Manning_LOB,Manning_Main=Manning_Main,Manning_ROB=Manning_ROB,
                lbs_xx=lbs_xx,rbs_xx=rbs_xx,ds_length_LOB=ds_length_LOB,ds_length_Main=ds_length_Main,
                ds_length_ROB=ds_length_ROB,contraction_coeff=contraction_coeff,expansion_coeff=expansion_coeff,
                xsectionshp=xsectionshp)
    },


  ## consider adding methods in separate file,
  ### these long methods can be added to the class after to keep this file clean

  calc_min_elev = function(set=TRUE, confine_to_bankstations=FALSE) {

    if (!confine_to_bankstations) {
      return_elev <- min(.self$zz)
    } else {
      ind <- which(.self$xx >= .self$lbs_xx & .self$xx <= .self$rbs_xx)
      return_elev <-min(.self$zz[ind])
    }

    # consider checking for outlying depressions away from middle of channel section?

    if (set) { .self$min_elev <- return_elev}
    return(return_elev)
  },

  # compute the basic hydraulic properties related to depth only
  # uses an existing mm that is provided
  # this function uses xsection calcs, and properties can be pre-processed
  compute_basic_depth_properties = function(WSL=NULL, mm=NULL, bbopt=NULL) {

    if (nrow(mm) > 1) {
      stop("mm should be supplied as a single row")
    }

    # check properties and options
    if (bbopt$xs_use_obcalcs & (.self$Manning_LOB <= 0 | .self$Manning_Main <= 0 | .self$Manning_ROB <= 0)) {
      warning("Cannot use bbopt$xs_use_obcalcs=TRUE with any of Manning_LOB,Manning_Main, or Manning_ROB not set.\nSetting bbopt$xs_use_obcalcs to FALSE")
      bbopt$xs_use_obcalcs <- FALSE
    }

    if (!bbopt$xs_use_obcalcs & (.self$Manning_LOB > 0 & .self$Manning_Main > 0 & .self$Manning_ROB > 0) ) {
      warning("bbopt$xs_use_obcalcs set to FALSE, but Manning values in each region are defined.\nPlease set bbopt$xs_use_obcalcs=TRUE if you wish to use these Manning region values in the calculation.")
    }

    # check if valid bbopt$xsection_conveyance_method provided
    if (bbopt$xsection_conveyance_method %notin% bb_get_xsection_conveyance_methods() ) {
      warning(sprintf("Unsupported xsection conveyance method: %s. Defaulting to 'default_conveyance'."))
      bbopt$xsection_conveyance_method <- "default_conveyance"
    }

    # check if bbopt$xsection_conveyance_method is "overbank_conveyance" and overbank roughness values not provided
    if (bbopt$xsection_conveyance_method == "overbank_conveyance" & (.self$Manning_LOB <= 0 | .self$Manning_Main <= 0 | .self$Manning_ROB <= 0)) {
      warning("bbopt$xsection_conveyance_method set as 'overbank_conveyance' but one or more Manning values for bank sections not provided. Defaulting to 'default_conveyance'.")
      bbopt$xsection_conveyance_method <- "default_conveyance"
    }

    # check bbopt$Manning_composite_method for recognized methods
    if (bbopt$Manning_composite_method %notin% bb_get_composite_Manning_methods()) {
      warning(sprintf("Unrecognized bbopt - Manning_composite_method '%s'. Using default (equal_velocity)",
                      bbopt$Manning_composite_method))
      bbopt$Manning_composite_method <- "equal_velocity"
    }

    # check Manning values if bbopt$Manning_enforce_values == TRUE
    if (bbopt$Manning_enforce_values & any(.self$Manning_LOB <= 0 | .self$Manning_Main <= 0 | .self$Manning_ROB <= 0)) {
      warning("bbopt$Manning_enforce_values is TRUE but one or more Manning values for bank sections not defined.\nSetting bbopt$Manning_enforce_values to FALSE")
      bbopt$Manning_enforce_values <- FALSE
    }

    # check for consistency with areaweighted_conveyance and manning composite method
    if (bbopt$xsection_conveyance_method == "areaweighted_conveyance" & bbopt$Manning_composite_method != "weighted_average_area") {
      warning("conveyance method 'areaweighted_conveyance' should be used with the Manning composite method 'weighted_average_area' for consistency.")
    }

    if (bbopt$roughness_multiplier <= 0 | is.null(bbopt$roughness_multiplier) | is.na(bbopt$roughness_multiplier)) {
      stop("bbopt$roughness_multiplier must be a positive value.")
    }

    # xxx to do - add some rm statements to clear memory as not needed

    mm$WSL <- WSL
    mm$Depth <- mm$WSL - mm$Min_Elev

    ## check method and setup xx, zz, nn accordingly
    if (bbopt$xsection_conveyance_method != "discretized_conveyance") {

      # here, each xx represents the section to the right of it

      xx <- .self$xx
      zz <- .self$zz
      nn <- .self$Manning

      depth <- mm$WSL - zz
      ind <- c(which(depth>0)-1, which(depth>0),which(depth>0)[length(which(depth>0))])
      ind <- sort(unique(ind))
      notind <- which(seq(1,length(depth)) %notin% ind)

      # define limits for left / main / right sections of channel
      ind_lob <-  which(xx < .self$lbs_xx)
      ind_main <- which(xx >= .self$lbs_xx & xx < .self$rbs_xx)
      ind_rob <-  which(xx >= .self$rbs_xx)

      ind_lob <-  ind_lob[which(ind_lob %in% ind)]
      ind_main <- ind_main[which(ind_main %in% ind)]
      ind_rob <-  ind_rob[which(ind_rob %in% ind)]

    } else { # bbopt$xsection_conveyance_method == "discretized_conveyance"

      # here, each xx treated as a point at exact xx location

      ## interpolate series (for computational purposes)
      min_elev <- mm$Min_Elev
      min_dist <- min(.self$xx)
      max_dist <- max(.self$xx) # take shorter of the two series

      xx <- seq(from=min_dist,to=max_dist,by=bbopt$dx)
      N <- length(xx)
      zz <- approx(x=.self$xx,y=.self$zz,xout=xx)$y
      if (bbopt$Manning_interpolation_method %in% c("constant","linear")) {
        nn <- approx(x=.self$xx,y=.self$Manning,xout=xx,
                     method=bbopt$Manning_interpolation_method)$y
      } else {
        stop(sprintf("compute_basic_depth_properties: unrecognized Manning_interpolation_method option '%s'",
                     bbopt$Manning_interpolation_method))
      }

      depth <- mm$WSL - zz
      ind <- which(depth>0)
      notind <- which(seq(1,length(depth)) %notin% ind)

      ind_lob <- which(depth>0 & xx <= .self$lbs_xx)
      ind_main <- which(depth>0 & xx > .self$lbs_xx & xx < .self$rbs_xx)
      ind_rob <- which(depth>0 & xx >= .self$rbs_xx)
    }

    # adjust roughness values to ensure all roughness values in areas reflect those values
    if (bbopt$Manning_enforce_values & .self$Manning_LOB > 0 & .self$Manning_Main > 0 & .self$Manning_ROB > 0) {
      nn[ind_lob] <- .self$Manning_LOB
      nn[ind_rob] <- .self$Manning_ROB
      nn[ind_main] <- .self$Manning_Main
    }

    # adjust roughness if using multiplier
    if (bbopt$roughness_multiplier != 1) {
      nn <- nn * bbopt$roughness_multiplier
    }
    if (.self$roughmult != 1) {
      nn <- nn * .self$roughmult
    }

    # print("compute initial profile - series interpolated")

    ## compute flow Area
    if (bbopt$xsection_conveyance_method != "discretized_conveyance") {

      Area_vec <- rep(NA,length(xx))
      Area_vec[length(xx)] <- 0
      for (i in 1:(length(xx)-1)) {
        if (mm$WSL < min(zz[i],zz[i+1])) {
          Area_vec[i] <- 0
        } else {
          # rectangular portion above zmax
          Area_vec[i] <-  max( (mm$WSL-max(zz[i],zz[i+1]))*(xx[i+1]-xx[i]),0)

          # wedge portion
          if (mm$WSL > max(zz[i+1],zz[i])) {
            Area_vec[i] <- Area_vec[i] + 0.5*(xx[i+1]-xx[i])*abs(zz[i+1]-zz[i])
          } else {
            # partial submergence of wedge
            Area_vec[i] <- Area_vec[i] + 0.5*(xx[i+1]-xx[i])*(mm$WSL-min(zz[i],zz[i+1]))^2/(abs(zz[i]-zz[i+1]))
          }
        }
      }
      # enforce zero area in notind (should already be handled above for this method)
      Area_vec[notind] <- 0
      Area_vec[which(Area_vec<0)] <- 0

    } else { # if (bbopt$xsection_conveyance_method == "discretized_conveyance")
      Area_vec <- depth*bbopt$dx
      Area_vec[notind] <- 0
      Area_vec[which(Area_vec<0)] <- 0
      # check
      # as.numeric(sum(depth[ind])*bbopt$dx)
      # sum(Area_vec)
      # mm$Area
    }
    mm$Area_LOB <- sum(Area_vec[ind_lob])
    mm$Area_Main <- sum(Area_vec[ind_main])
    mm$Area_ROB <- sum(Area_vec[ind_rob])
    mm$Area <- sum(mm$Area_LOB, mm$Area_Main, mm$Area_ROB)

    ## calculate Top Width, Wetted Perimeter, Hydraulic Radius
    if (bbopt$xsection_conveyance_method != "discretized_conveyance") {

      TopWidth_vec <- rep(0,length(xx))
      WetPer_vec   <- rep(0,length(xx))

      for (i in 1:(length(xx)-1)) {
        if (mm$WSL < min(zz[i],zz[i+1])) {
          TopWidth_vec[i] <- 0
          WetPer_vec[i] <- 0

        } else {

          # wedge portion
          if (mm$WSL > max(zz[i+1],zz[i])) {
            TopWidth_vec[i] <- finiteorzero(xx[i+1]-xx[i])
            WetPer_vec[i] <- finiteorzero(sqrt((xx[i+1]-xx[i])^2+(zz[i+1]-zz[i])^2))
          } else {
            # partial submergence of wedge
            # h <- mm$WSL-min(zz[i],zz[i+1])
            TopWidth_vec[i] <- finiteorzero(abs((xx[i+1]-xx[i])*(mm$WSL-min(zz[i],zz[i+1]))/(zz[i+1]-zz[i])))
            WetPer_vec[i] <- finiteorzero(sqrt(TopWidth_vec[i]^2 + (mm$WSL-min(zz[i],zz[i+1]))^2))
          }
        }
      }
      # enforce zero properties in notind (should already be handled above for this method)
      TopWidth_vec[notind] <- 0
      TopWidth_vec[TopWidth_vec<0] <- 0
      WetPer_vec[notind] <- 0
      WetPer_vec[WetPer_vec<0] <- 0

      ## add special handling for banks where land boundary touched
      if (mm$WSL > zz[1]) {
        # add left side boundary vertical height
        WetPer_vec[1] <- WetPer_vec[1] + mm$WSL-zz[1]
      }
      if (mm$WSL > zz[(length(zz))]) {
        # add right side boundary vertical height
        WetPer_vec[length(WetPer_vec)] <- WetPer_vec[length(WetPer_vec)] + mm$WSL-zz[length(WetPer_vec)]
      }

      mm$TopWidth <- sum(TopWidth_vec[ind])
      mm$TopWidth_LOB <- sum(TopWidth_vec[ind_lob])
      mm$TopWidth_Main <- sum(TopWidth_vec[ind_main])
      mm$TopWidth_ROB <- sum(TopWidth_vec[ind_rob])

      mm$WetPerimeter_LOB <- sum(WetPer_vec[ind_lob])
      mm$WetPerimeter_ROB <- sum(WetPer_vec[ind_rob])
      mm$WetPerimeter_Main <- sum(WetPer_vec[ind_main])

    } else { # bbopt$xsection_conveyance_method == "discretized_conveyance"

      mm$TopWidth <- length(ind)*bbopt$dx
      mm$TopWidth_LOB <- length(ind_lob)*bbopt$dx
      mm$TopWidth_Main <- length(ind_main)*bbopt$dx
      mm$TopWidth_ROB <- length(ind_rob)*bbopt$dx

      mm$WetPerimeter_LOB <- wetted_perimeter(zz,ind_lob,bbopt$dx)
      mm$WetPerimeter_ROB <- wetted_perimeter(zz,ind_rob,bbopt$dx)
      mm$WetPerimeter_Main <- wetted_perimeter(zz,ind_main,bbopt$dx)
      WetPer_vec <- wetted_perimeter_vec(zz,ind,bbopt$dx)

      # check
      # wetted_perimeter(zz,ind,bbopt$dx)
      # mm$WetPerimeter
      # sum(WetPer_vec)

      ## add special handling for banks where land boundary touched
      if (mm$WSL > zz[1]) {
        # add left side boundary vertical height
        WetPer_vec[1] <- WetPer_vec[1] + mm$WSL-zz[1]
        mm$WetPerimeter_LOB <- mm$WetPerimeter_LOB + mm$WSL-zz[1]
      }
      if (mm$WSL > zz[length(zz)]) {
        # add right side boundary vertical height
        WetPer_vec[length(WetPer_vec)] <- WetPer_vec[length(WetPer_vec)] + mm$WSL-zz[length(WetPer_vec)]
        mm$WetPerimeter_ROB <- mm$WetPerimeter_ROB + mm$WSL-zz[length(WetPer_vec)]
      }
    }
    mm$WetPerimeter <- sum(mm$WetPerimeter_LOB,mm$WetPerimeter_Main,mm$WetPerimeter_ROB)

    mm$HRadius <- max(mm$Area/mm$WetPerimeter,0,na.rm=T)
    mm$HRadius_Main <- max(mm$Area_Main/mm$WetPerimeter_Main,0,na.rm=T)
    mm$HRadius_LOB <- max(mm$Area_LOB/mm$WetPerimeter_LOB,0,na.rm=T)
    mm$HRadius_ROB <- max(mm$Area_ROB/mm$WetPerimeter_ROB,0,na.rm=T)
    HRadius_vec <- finiteorzero(Area_vec/WetPer_vec)
    mm$HydDepth <- mm$Area/mm$TopWidth
    mm$HydDepth_LOB <- mm$Area_LOB/mm$TopWidth_LOB
    mm$HydDepth_Main <- mm$Area_Main/mm$TopWidth_Main
    mm$HydDepth_ROB <- mm$Area_ROB/mm$TopWidth_ROB

    ## compute conveyance as a vector and sum for each section (depending on bbopt$xsection_conveyance_method)
    K_vec <- (1/nn)*Area_vec*HRadius_vec^(2/3)
    K_vec[notind] <- 0
    K_vec[which(K_vec<0)] <- 0

    ## conveyance calculation
    if (bbopt$xsection_conveyance_method == "overbank_conveyance") {
      ## use the overbank roughness values directly, calculate conveyance in each LOB/Main/ROB directly
      mm$K_LOB <- (1/.self$Manning_LOB)*mm$Area_LOB*(mm$HRadius_LOB^(2/3))
      mm$K_Main <- (1/.self$Manning_Main)*mm$Area_Main*(mm$HRadius_Main^(2/3))
      mm$K_ROB <- (1/.self$Manning_ROB)*mm$Area_ROB*(mm$HRadius_ROB^(2/3))
      mm$K_Total <- sum(mm$K_LOB,mm$K_Main,mm$K_ROB)
      mm$alpha <- (mm$Area^2/mm$K_Total^3)*(finiteorzero((mm$K_LOB^3/mm$Area_LOB^2)) + finiteorzero(mm$K_Main^3/mm$Area_Main^2) + finiteorzero(mm$K_ROB^3/mm$Area_ROB^2))

    } else if (bbopt$xsection_conveyance_method == "default_conveyance") {

      ## classify roughness zones
      roughness_zones <- rep(NA,(length(xx)))
      roughness_zones[1] <- 1
      for (i in 2:(length(xx))) {
        if (nn[i] != nn[i-1]) {
          roughness_zones[i] <- roughness_zones[i-1]+1
        } else {
          roughness_zones[i] <- roughness_zones[i-1]
        }
      }
      roughness_zones[c(ind_main,ind_rob)] <- roughness_zones[c(ind_main,ind_rob)]+1
      roughness_zones[ind_rob] <- roughness_zones[ind_rob]+1

      # compute conveyance in each roughness zone
      conv_vec <- rep(0,length(xx))
      manning_vec <- nn # rep(0,length(xx))
      area_vec <- Area_vec
      wp_vec <- WetPer_vec

      ## aggregate within vectors by roughness zone
      for (rz in unique(roughness_zones)) {
        # rz=1

        # first instance of roughness zone index
        allzones <- which(roughness_zones == rz)
        firstind <- allzones[1]
        otherzones <- allzones[-which(allzones %in% firstind)]

        area_vec[firstind] <- sum(area_vec[allzones])
        area_vec[otherzones] <- 0

        wp_vec[firstind] <- sum(wp_vec[allzones])
        wp_vec[otherzones] <- 0

        # manning_vec[firstind] <- mean(manning_vec[allzones],na.rm=TRUE)
        # manning_vec[allzones] <- manning_vec[firstind]

        conv_vec[firstind] <- finiteorzero(conv_calc(n=manning_vec[firstind],
                                              A=area_vec[firstind],
                                              Rh=area_vec[firstind]/wp_vec[firstind]))
      }

      # update vectors based on calcs here
      WetPer_vec <- wp_vec
      K_vec <- conv_vec
      Area_vec <- area_vec
      HRadius_vec <- finiteorzero(Area_vec/WetPer_vec)

      mm$K_LOB <- sum(conv_vec[ind_lob])
      mm$K_Main <- sum(conv_vec[ind_main])
      mm$K_ROB <- sum(conv_vec[ind_rob])
      mm$K_Total <- sum(mm$K_LOB,mm$K_Main,mm$K_ROB)
      mm$alpha <- (mm$Area^2/mm$K_Total^3)*(finiteorzero((mm$K_LOB^3/mm$Area_LOB^2)) + finiteorzero(mm$K_Main^3/mm$Area_Main^2) + finiteorzero(mm$K_ROB^3/mm$Area_ROB^2))

    } else if (bbopt$xsection_conveyance_method == "coordinate_conveyance") {

      ## calculate and sum conveyance in each coordinate

      # use each section as is, do not aggregate if no change to manning
      conv_vec <- rep(NA,length(.self$xx))
      # manning_vec <- rep(NA,length(.self$xx))
      manning_vec <- nn
      conv_vec <- finiteorzero(conv_calc(n=manning_vec,
                                         A=Area_vec,
                                         Rh=Area_vec/WetPer_vec))
      conv_vec[notind] <- 0
      conv_vec[length(.self$xx)] <- 0

      mm$K_LOB <- sum(conv_vec[ind_lob])
      mm$K_Main <- sum(conv_vec[ind_main])
      mm$K_ROB <- sum(conv_vec[ind_rob])
      mm$K_Total <- sum(mm$K_LOB,mm$K_Main,mm$K_ROB)
      # mm$alpha <- (mm$Area^2/mm$K_Total^3)*(mm$K_LOB^3/mm$Area_LOB^2 + mm$K_Main^3/mm$Area_Main^2 + mm$K_ROB^3/mm$Area_ROB^2)
      mm$alpha <- (mm$Area^2/mm$K_Total^3)*(finiteorzero((mm$K_LOB^3/mm$Area_LOB^2)) + finiteorzero(mm$K_Main^3/mm$Area_Main^2) + finiteorzero(mm$K_ROB^3/mm$Area_ROB^2))

      # ## checks
      # sum(conv_vec)
      # sum(mm$K_LOB,mm$K_Main,mm$K_ROB) #  mm$K_Total <-
      # mm$K_LOB
      # mm$K_Main
      # mm$K_ROB
      #
      # rbind(.self$xx, .self$Manning, manning_vec, conv_vec)

    } else if (bbopt$xsection_conveyance_method == "areaweighted_conveyance") {

      # uses special approach in Blackbird to modify Manning's equation,
      # sum the area-weighted Ai/ni in each section and then multiply by (A/P)^(2/3)

      sum_vec <- Area_vec / nn
      manning_vec <- nn
      # mm$K_Total <- finiteorzero(sum(sum_vec[ind])*(mm$Area/mm$WetPerimeter)^(2/3))
      mm$K_LOB <- finiteorzero(sum(sum_vec[ind_lob])*(mm$Area_LOB/mm$WetPerimeter_LOB)^(2/3))
      mm$K_Main <- finiteorzero(sum(sum_vec[ind_main])*(mm$Area_Main/mm$WetPerimeter_Main)^(2/3))
      mm$K_ROB <- finiteorzero(sum(sum_vec[ind_rob])*(mm$Area_LOB/mm$WetPerimeter_ROB)^(2/3))
      mm$K_Total <- sum(mm$K_LOB,mm$K_Main,mm$K_ROB)
      # mm$alpha <- (mm$Area^2/mm$K_Total^3)*(mm$K_LOB^3/mm$Area_LOB^2 + mm$K_Main^3/mm$Area_Main^2 + mm$K_ROB^3/mm$Area_ROB^2)
      mm$alpha <- (mm$Area^2/mm$K_Total^3)*(finiteorzero((mm$K_LOB^3/mm$Area_LOB^2)) + finiteorzero(mm$K_Main^3/mm$Area_Main^2) + finiteorzero(mm$K_ROB^3/mm$Area_ROB^2))

    } else if (bbopt$xsection_conveyance_method == "areaweighted_conveyance_onecalc") {

      # uses special approach in Blackbird to modify Manning's equation,
      # sum the area-weighted Ai/ni and then multiply by (A/P)^(2/3) for the entire section

      sum_vec <- Area_vec / nn
      manning_vec <- nn
      mm$K_Total <- finiteorzero(sum(sum_vec[ind])*(mm$Area/mm$WetPerimeter)^(2/3))
      # assign other conveyances based on weighted average with flow area
      mm$K_LOB <- mm$K_Total*mm$Area_LOB/mm$Area
      mm$K_ROB <- mm$K_Total*mm$Area_ROB/mm$Area
      mm$K_Main <- mm$K_Total*mm$Area_Main/mm$Area
      mm$alpha <- 1 # by definition with this method

    } else if (bbopt$xsection_conveyance_method == "discretized_conveyance") {
      ## calculate and sum conveyance in each point discretized by bbopt$dx
      mm$K_LOB <- sum(K_vec[ind_lob])
      mm$K_Main <- sum(K_vec[ind_main])
      mm$K_ROB <- sum(K_vec[ind_rob])
      mm$K_Total <- sum(mm$K_LOB,mm$K_Main,mm$K_ROB)
      manning_vec <- nn
      # mm$alpha <- (mm$Area^2/mm$K_Total^3)*(mm$K_LOB^3/mm$Area_LOB^2 + mm$K_Main^3/mm$Area_Main^2 + mm$K_ROB^3/mm$Area_ROB^2)
      mm$alpha <- (mm$Area^2/mm$K_Total^3)*(finiteorzero((mm$K_LOB^3/mm$Area_LOB^2)) + finiteorzero(mm$K_Main^3/mm$Area_Main^2) + finiteorzero(mm$K_ROB^3/mm$Area_ROB^2))

    }

    # compute effective reach length from conveyance weighted average
    # xxx need to update this to consider junction nodes
    mm$Length_Effective <- (.self$ds_length_LOB*mm$K_LOB + .self$ds_length_Main*mm$K_Main + .self$ds_length_ROB*mm$K_ROB)/ (mm$K_Total)

    # define the adjusted effective length
    # used in calcs, though will not be changed for xsection
    mm$Length_EffectiveAdjusted <- mm$Length_Effective # until it gets assigned elsewhere

    ## compute composite roughness in each of LOB, Main, ROB, and overall composite roughness
    if (bbopt$xsection_conveyance_method == "overbank_conveyance") {
      mm$Manning_LOB <- .self$Manning_LOB * bbopt$roughness_multiplier
      mm$Manning_Main <- .self$Manning_Main * bbopt$roughness_multiplier
      mm$Manning_ROB <- .self$Manning_ROB * bbopt$roughness_multiplier

      mm$Manning_LOB <- .self$Manning_LOB * .self$roughmult
      mm$Manning_Main <- .self$Manning_Main * .self$roughmult
      mm$Manning_ROB <- .self$Manning_ROB * .self$roughmult

      if (bbopt$Manning_composite_method == "equal_force") {
        # equivalent method - total force equation
        # [(1/P) sum(n_i^(2) * Pi)]^(1/2), also called Pavlovski method
          mm$Manning_Composite <- sqrt((1/mm$WetPerimeter)*sum(mm$WetPerimeter_LOB*mm$Manning_LOB^2,
                                                               mm$WetPerimeter_Main*mm$Manning_Main^2,
                                                               mm$WetPerimeter_ROB*mm$Manning_ROB^2))

      } else if (bbopt$Manning_composite_method == "weighted_average_area") {
        # simple area-weighted average
        # sum(area*mannings n) / sum(area)
          mm$Manning_Composite <- sum(mm$Area_LOB*mm$Manning_LOB,
                                      mm$Area_Main*mm$Manning_Main,
                                      mm$Area_ROB*mm$Manning_ROB) / mm$Area

      } else if (bbopt$Manning_composite_method == "weighted_average_wetperimeter") {
        # simple area-weighted average
        # sum(area*mannings n) / sum(area)
        mm$Manning_Composite <- sum(mm$WetPerimeter_LOB*mm$Manning_LOB,
                                    mm$WetPerimeter_Main*mm$Manning_Main,
                                    mm$WetPerimeter_ROB*mm$Manning_ROB) / mm$Area

      } else if (bbopt$Manning_composite_method == "weighted_average_conveyance") {
        # simple conveyance-weighted average
        # sum(K*mannings n) / sum(K)

          mm$Manning_Composite <- sum(mm$K_LOB*mm$Manning_LOB,
                                      mm$K_Main*mm$Manning_Main,
                                      mm$K_ROB*mm$Manning_ROB) / mm$K_Total

      } else if (bbopt$Manning_composite_method == "equal_velocity") {
        # [(1/P) sum(n_i^(3/2) * Pi)]^(2/3), also called Horton or Einstein method
          mm$Manning_Composite <- ((1/mm$WetPerimeter)*sum(mm$WetPerimeter_LOB*mm$Manning_LOB^(3/2),
                                                           mm$WetPerimeter_Main*mm$Manning_Main^(3/2),
                                                           mm$WetPerimeter_ROB*mm$Manning_ROB^(3/2)))^(2/3)
      }

    } else {

      # compute Manning roughness in each section based on the composite method provided
      if (bbopt$Manning_composite_method == "equal_force") {
        # equivalent method - total force equation
        # [(1/P) sum(n_i^(2) * Pi)]^(1/2), also called Pavlovski method

        mm$Manning_LOB <- sqrt((1/mm$WetPerimeter_LOB)*sum(WetPer_vec[ind_lob]*manning_vec[ind_lob]^2))
        mm$Manning_Main <- sqrt((1/mm$WetPerimeter_Main)*sum(WetPer_vec[ind_main]*manning_vec[ind_main]^2))
        mm$Manning_ROB <- sqrt((1/mm$WetPerimeter_ROB)*sum(WetPer_vec[ind_rob]*manning_vec[ind_rob]^2))
        mm$Manning_Composite <- sqrt((1/mm$WetPerimeter)*sum(WetPer_vec[ind]*manning_vec[ind]^2))

      } else if (bbopt$Manning_composite_method == "weighted_average_area") {
        # simple area-weighted average
        # sum(area*mannings n) / sum(area)

        mm$Manning_LOB <- sum(Area_vec[ind_lob]*manning_vec[ind_lob]) / sum(Area_vec[ind_lob])
        mm$Manning_Main <- sum(Area_vec[ind_main]*manning_vec[ind_main]) / sum(Area_vec[ind_main])
        mm$Manning_ROB <- sum(Area_vec[ind_rob]*manning_vec[ind_rob]) / sum(Area_vec[ind_rob])
        mm$Manning_Composite <- sum(Area_vec[ind]*manning_vec[ind]) / sum(Area_vec[ind])

      } else if (bbopt$Manning_composite_method == "weighted_average_wetperimeter") {
        # simple area-weighted average
        # sum(area*mannings n) / sum(area)

        mm$Manning_LOB <- sum(WetPer_vec[ind_lob]*manning_vec[ind_lob]) / sum(WetPer_vec[ind_lob])
        mm$Manning_Main <- sum(WetPer_vec[ind_main]*manning_vec[ind_main]) / sum(WetPer_vec[ind_main])
        mm$Manning_ROB <- sum(WetPer_vec[ind_rob]*manning_vec[ind_rob]) / sum(WetPer_vec[ind_rob])
        mm$Manning_Composite <- sum(WetPer_vec[ind]*manning_vec[ind]) / sum(WetPer_vec[ind])

      } else if (bbopt$Manning_composite_method == "weighted_average_conveyance") {
        # simple conveyance-weighted average
        # sum(K*mannings n) / sum(K)

        mm$Manning_LOB <- sum(K_vec[ind_lob]*manning_vec[ind_lob]) / sum(K_vec[ind_lob])
        mm$Manning_Main <- sum(K_vec[ind_main]*manning_vec[ind_main]) / sum(K_vec[ind_main])
        mm$Manning_ROB <- sum(K_vec[ind_rob]*manning_vec[ind_rob]) / sum(K_vec[ind_rob])
        mm$Manning_Composite <- sum(K_vec[ind]*manning_vec[ind]) / sum(K_vec[ind])

      } else if (bbopt$Manning_composite_method == "equal_velocity") {
        # [(1/P) sum(n_i^(3/2) * Pi)]^(2/3), also called Horton or Einstein method
        mm$Manning_LOB <- ((1/mm$WetPerimeter_LOB)*sum(WetPer_vec[ind_lob]*manning_vec[ind_lob]^(3/2)))^(2/3)
        mm$Manning_Main <- ((1/mm$WetPerimeter_Main)*sum(WetPer_vec[ind_main]*manning_vec[ind_main]^(3/2)))^(2/3)
        mm$Manning_ROB <- ((1/mm$WetPerimeter_ROB)*sum(WetPer_vec[ind_rob]*manning_vec[ind_rob]^(3/2)))^(2/3)
        mm$Manning_Composite <- ((1/mm$WetPerimeter)*sum(WetPer_vec[ind]*manning_vec[ind]^(3/2)))^(2/3)
      }
    }

    return(mm)
  },

  # compute the basic hydraulic properties for flow
  # (uses initial properties with mm from compute_basic_depth_properties)
  # note that these properties in this function rely on non-raster calcs
  # and are not pre-processed
  compute_basic_flow_properties = function(Flow=NULL, mm=NULL, bbopt=NULL) {

    if (nrow(mm) > 1) {
      stop("mm should be supplied as a single row")
    }

    mm$Flow <- Flow
    mm$Flow_LOB <- mm$Flow*mm$K_LOB/mm$K_Total
    mm$Flow_Main <- mm$Flow*mm$K_Main/mm$K_Total
    mm$Flow_ROB <- mm$Flow*mm$K_ROB/mm$K_Total

    mm$Velocity <- finiteorzero(mm$Flow/mm$Area)
    mm$Velocity_LOB <- max(mm$Flow_LOB/mm$Area_LOB,0,na.rm=T)
    mm$Velocity_Main <- max(mm$Flow_Main/mm$Area_Main,0,na.rm=T)
    mm$Velocity_ROB <- max(mm$Flow_ROB/mm$Area_ROB,0,na.rm=T)

    mm$Velocity_head <- vhead_calc(mm$alpha,mm$Velocity,bbopt$g)
    mm$Energy_total <- mm$Velocity_head + mm$WSL
    mm$Froude <- froude_calc(mm$Velocity,bbopt$g,mm$HydDepth)
    mm$Sf <- finiteorzero((mm$Flow/mm$K_Total)^2)

    # if (mm$Sf > 1.0) {
    #   ## silent out for now?? Only if final Sf is >1
    #   warning(sprintf("in xsection compute_basic_flow_properties: Sf with %.4e exceeds 1.0, likely divergence in calculation",mm$Sf))
    # }
    return(mm)
  },

  calculate_flow_area = function(WSL=NULL) {

    ## basic version of code from compute_basic_depth_properties
    min_elev <- .self$min_elev
    min_dist <- min(.self$xx)
    max_dist <- max(.self$xx) # take shorter of the two series
    xx <- seq(from=min_dist,to=max_dist,by=bbopt$dx)
    N <- length(xx)
    zz <- approx(x=.self$xx,y=.self$zz,xout=xx)$y
    depth <- WSL - zz
    ind <- which(depth>0)
    Area_vec <- depth*bbopt$dx
    return(sum(Area_vec[ind]))
  }  )


)

# xxx add function to plot section -----

### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

### ~~~~~~~~~~~~~~~~~~~
### GEOMETRY CLASS ----
### ~~~~~~~~~~~~~~~~~~~

#' Geometry Object
#'
#' geom:
#' A reference class to create a geometry object from a collection of cross-sections of class xsection
#'
#' @field geomname name of the geometry object
#' @field streamnodeList a list of streamnode objects, which contains all of the cross-sections, catchments, and relevant data
bb_geometry <- setRefClass("bb_geometry", field = list(geomname = "character",
                       streamnodeList = "list",
                       rivershp="sf",
                       riverreachList = "list"),
  method = list(

    initialize =
      function(..., geomname="mygeometry",
               streamnodeList=list(),
               rivershp=st_sf(st_sfc()),
               riverreachList=list())
      {
        callSuper(..., geomname = geomname,
                  streamnodeList = streamnodeList,
                  rivershp=rivershp,
                  riverreachList=riverreachList)
      },

    get_streamnodelist_length = function() {
      return(length(.self$streamnodeList))
    },

    get_riverreachList_length = function() {
      return(length(.self$riverreachList))
    },

    compute_reach_lengths = function() {
      # len_nodes <- length(.self$streamnodeList)
      len_nodes <- .self$get_streamnodelist_length()

      ## recompute reach lengths from clipped reachshp objects
      ## calculate reach lengths (length of upstream area within catchment)
      # set as max of reachshp and demres for junction nodes
      # for (i in 1:len_nodes) {
      #
      #   # check that reachID is valid and found in reachshp
      #   if (.self$streamnodeList[[i]]$reachID %notin% .self$streamnodeList[[i]]$reachshp$reachID) {
      #     stop(sprintf("reachID not found in reachshp for streamnode with pointID %i",
      #                  .self$streamnodeList[[i]]$nodeID))
      #   }
      #
      #   if (nrow(.self$streamnodeList[[i]]$reachshp) == 1) {
      #     # simple case (all streamnodes will have an upstream length)
      #     .self$streamnodeList[[i]]$us_reach_length1 <- .self$streamnodeList[[i]]$reachshp$length
      #   } else {
      #     # junction - grab the right segment of downstream river length up to junction
      #     .self$streamnodeList[[i]]$us_reach_length1 <-
      #       .self$streamnodeList[[i]]$reachshp[.self$streamnodeList[[i]]$reachshp$reachID == .self$streamnodeList[[i]]$reachID,]$length
      #
      #     # calculate upstream reach length 2 as average of upstream options
      #     .self$streamnodeList[[i]]$us_reach_length2 <-
      #       .self$streamnodeList[[i]]$us_reach_length1 +
      #       mean(.self$streamnodeList[[i]]$reachshp[.self$streamnodeList[[i]]$reachshp$reachID %notin% .self$streamnodeList[[i]]$reachID,]$length)
      #   }
      # }

      sdf <- .self$get_streamnodeList_as_dataframe()

      ## calculate downstream reach lengths
      for (i in 1:len_nodes) {

        # get info of the downnodeID into temp
        ds_node_index <- which(sdf$nodeID == sdf$downnodeID[i])
        temp <- sdf[ds_node_index,]

        if (nrow(temp)==0) {
          # no downstream node, keep as -99
          .self$streamnodeList[[i]]$ds_reach_length <- -99
        } else if (nrow(temp) == 1) {
          if (temp$upnodeID2 == -1) {
            .self$streamnodeList[[i]]$ds_reach_length <- temp$us_reach_length1
          } else {
            # junction - need to get the right reach length
            if (temp$upnodeID1 == sdf$nodeID[i]) {
              .self$streamnodeList[[i]]$ds_reach_length <- temp$us_reach_length1
            } else {
              .self$streamnodeList[[i]]$ds_reach_length <- temp$us_reach_length2
            }
          }
        } else {
          stop("unexpected number of matching downstream IDs")
        }
      }

      ## recompute stations from reach lengths
      ## xxx TO DO - add check that stationing has the downstream end as 0m, max length at upstream end
      sdf <- .self$get_streamnodeList_as_dataframe()
      sdf$station <- -99

      reaches <- unique(sdf$reachID)

      for (j in 1:length(reaches)) {
        sdff <- sdf[sdf$reachID == reaches[j],]

        startid <- sdff[which(sdff$upnodeID1 %notin% sdff$nodeID),]$nodeID
        sdff[sdff$nodeID == startid,]$station <-
           sdff[sdff$nodeID == startid,]$us_reach_length1
        lastid <- startid

        if (nrow(sdff) > 1) {
          for (i in 1:(nrow(sdff)-1)) {
            startid <- sdff[sdff$nodeID == startid,]$downnodeID

            if (startid %in% sdff$nodeID) {
              sdff[sdff$nodeID == startid,]$station <-
             sdff[sdff$nodeID == startid,]$us_reach_length1+
              sdff[sdff$nodeID == lastid,]$station
            } else {
              warning(sprintf("ending loop, nodeID %s not found in sdff",startid))
              break
            }
            lastid <- startid
          }
        } else if (nrow(sdff == 1)) {
          sdff$station <- sdff$us_reach_length1
        } else {
          stop("gg$compute_reach_lengths: found an empty reach ID")
        }

        # update in sdf
        sdf[sdf$reachID == reaches[j],]$station <-
          sdff$station
      }

      # update station in geometry object
      for (i in 1:len_nodes) {
        .self$streamnodeList[[i]]$station <-
          sdf$station[i]
      }

      message(sprintf("Reach lengths and stations for geometry '%s' have been updated", .self$geomname))
      return(TRUE)
    },

    compute_min_elevations_all = function(confine_to_bankstations=FALSE, checkelev=FALSE) {
      ## xxx could add subsetNodeIDs as an input here as well, for now using it to filter xsection and catchment nodes

      ## check which nodes are xsection and catchment
      sdf <- .self$get_streamnodeList_as_dataframe()
      catchmentnodes <- sdf[sdf$nodetype == "catchment",]$nodeID
      xsectionnodes <- sdf[sdf$nodetype == "xsection",]$nodeID

      if (any(sdf$nodetype %notin% c("catchment","xsection"))) {
        stop("unhandled streamnode types found")
      }

      ## compute xsection min elevations
      if (length(xsectionnodes)>0) {
        for (nodeID in xsectionnodes) {
          ii <- which(sdf$nodeID == nodeID)
            # message(sprintf("Computing minimum elevations for xsection streamnode %i of %i", i,len_nodes))
            .self$streamnodeList[[ii]]$calc_min_elev(set=TRUE,confine_to_bankstations=confine_to_bankstations)
        }
        message(sprintf("computed %i min elevations for xsection nodes", length(xsectionnodes)))
      }

      if (length(catchmentnodes)>0) {
        subsetNodeIDs <- catchmentnodes

        # Assumes that all catchments in subset or geometry have the same catchment, hand, and dem raster.
        # Read in raster data
        catchment_raster <- terra::rast(.self$streamnodeList[[1]]$catchmentraster)
        hand_raster <- terra::rast(.self$streamnodeList[[1]]$handraster)
        dem_raster <- terra::rast(.self$streamnodeList[[1]]$demraster)

        # if (!bb_check_extents(catchment_raster,dem_raster)) {
        #   stop("Need to have raster extents match exactly for compute_min_elevations")
        # }

        # Convert raster data to matrices
        catchment <- terra::as.matrix(catchment_raster)
        hand <- terra::as.matrix(hand_raster)
        dem <- terra::as.matrix(dem_raster)

        # Use Rcpp to calculate average minimum elevation for each catchment
        ## note: fails if catchment has no HAND values == 0
        return_elevs <- cpp_min_elev(catchment, hand, dem, subsetNodeIDs)

        if (any(!is.finite(return_elevs))) {
          tt <- which(!is.finite(as.numeric(return_elevs)))

          # calculate average min elev for each catchment
          for (i in tt) {
            ind1 <- which(catchment == subsetNodeIDs[i])
            ind2 <- which(hand == min(hand[ind1],na.rm=TRUE))
            ind3 <- common_elements(ind1,ind2)
            return_elevs[i] <- mean(dem[ind3],na.rm=TRUE)
            message(sprintf("Min elevation for streamnode %i was initially not finite, and was recalculated",subsetNodeIDs[i]))
          }
        }

        if (any(!is.finite(return_elevs))) {
          stop(sprintf("failed to compute min elevations at the following stream nodes:\n%s",
                       paste(names(return_elevs)[which(!is.finite(return_elevs))], collapse=", ")))
        }

        # len_nodes <- length(.self$streamnodeList)
        # len_nodes <- .self$get_streamnodelist_length()
        len_nodes <- ifelse(is.null(subsetNodeIDs), .self$get_streamnodelist_length(), length(subsetNodeIDs))

        ## assign min elevations for each streamnode
        for (i in 1:.self$get_streamnodelist_length()) {
          if(is.null(subsetNodeIDs) || .self$streamnodeList[[i]]$nodeID %in% subsetNodeIDs) {
            # message(sprintf("Assigning minimum elevations for catchment streamnode %i of %i", i,len_nodes))
            .self$streamnodeList[[i]]$min_elev <- return_elevs[as.character(.self$streamnodeList[[i]]$nodeID)]
          }
        }
      }

      if (checkelev) {
        for (i in 1:nrow(sdf)) {
          upnode1 <- sdf$upnodeID1[i]
          upnode2 <- sdf$upnodeID2[i]

          if (upnode1 >0) {
            if (abs(sdf$min_elev[i]-sdf[sdf$nodeID==upnode1,]$min_elev) > 10) {
              warning(sprintf("Change in min_elev from streamnode pointid %i to %i exceeds 5m (%.2fm, %.2f to %.2f), please check min_elev attributes",
                              as.integer(upnode1),sdf$nodeID[i],abs(sdf$min_elev[i]-sdf[sdf$nodeID==upnode1,]$min_elev), sdf[sdf$nodeID==upnode1,]$min_elev, sdf$min_elev[i]))
            }
          }
          if (upnode2 >0) {
            if (abs(sdf$min_elev[i]-sdf[sdf$nodeID==upnode2,]$min_elev) > 10) {
              warning(sprintf("Change in min_elev from streamnode pointid %i to %i exceeds 5m (%.2fm, %.2f to %.2f), please check min_elev attributes",
                              as.integer(upnode2),sdf$nodeID[i], abs(sdf$min_elev[i]-sdf[sdf$nodeID==upnode2,]$min_elev), sdf[sdf$nodeID==upnode2,]$min_elev, sdf$min_elev[i]))
            }
          }
        }
      }

      message(sprintf("Minimum elevations for geometry '%s' have been updated", .self$geomname))
      return(TRUE)
    },

    set_min_elevations = function(x) {
      # x as a numeric vector of min elevations
      # len_nodes <- length(.self$streamnodeList)
      len_nodes <- .self$get_streamnodelist_length()

      if (length(x) != len_nodes) {
        stop("Length of vector x and number of streamnodes must be consistent")
      }

      ## set min elevations for each streamnode
      for (i in 1:len_nodes) {
        .self$streamnodeList[[i]]$min_elev <- x[i]
      }

      message(sprintf("Minimum elevations for geometry '%s' have been updated", .self$geomname))
      return(TRUE)
    },

    update_roughmult_by_nodeID = function(nodeIDs=NULL,roughmult=1.0) {

      sdf <- .self$get_streamnodeList_as_dataframe()

      # check that all nodeIDs are valid
      if (any(nodeIDs %notin% sdf$nodeID)) {
        stop("All nodeIDs must be valid!")
      }

      ind <- which(sdf$nodeID %in% nodeIDs)

      for (i in ind) {
        .self$streamnodeList[[i]]$roughmult <- roughmult
      }
    },

    compute_bed_slope_all = function(subsetNodeIDs=NULL) {
      len_nodes <- .self$get_streamnodelist_length()
      sdf <- .self$get_streamnodeList_as_dataframe()

      if (is.null(subsetNodeIDs)) {
        subsetNodeIDs <- sdf$nodeID
      }

      # Assumes that all catchments in subset or geometry have the same catchment, hand, and dem raster.
      # Read in raster data
      catchment_raster <- terra::rast(.self$streamnodeList[[1]]$catchmentraster)
      hand_raster <- terra::rast(.self$streamnodeList[[1]]$handraster)
      # dem_raster <- terra::rast(.self$streamnodeList[[1]]$demraster)
      slope_raster <- terra::rast(bb_get_sloperaster(workingfolder,returnobject = FALSE))

      # if (!bb_check_extents(catchment_raster,dem_raster)) {
      #   stop("Need to have raster extents match exactly for compute_min_elevations")
      # }

      # Convert raster data to matrices
      catchment <- terra::as.matrix(catchment_raster)
      hand <- terra::as.matrix(hand_raster)
      sloper <- terra::as.matrix(slope_raster)

      # Use Rcpp to calculate average minimum elevation for each catchment
      ## note: fails if catchment has no HAND values == 0
      return_slopes <- cpp_bed_slope(catchment, hand, sloper, subsetNodeIDs)

      if (any(!is.finite(return_slopes))) {
        tt <- which(!is.finite(as.numeric(return_slopes)))

        # calculate average min elev for each catchment
        for (i in tt) {
          ind1 <- which(catchment == subsetNodeIDs[i])
          return_slopes[i] <- mean(sloper[which(hand==min(hand[ind1],na.rm=TRUE))])
          warning(sprintf("Slope for streamnode %i was initially not finite, and was recalculated",subsetNodeIDs[i]))
        }
      }

      if (any(!is.finite(return_slopes))) {
       stop(sprintf("failed to compute bed slopes at the following stream nodes:\n%s",
            paste(names(return_slopes)[which(!is.finite(return_slopes))], collapse=", ")))
      }

      len_nodes <- ifelse(is.null(subsetNodeIDs), .self$get_streamnodelist_length(), length(subsetNodeIDs))

      ## assign bed slope for each streamnode
      for (i in 1:.self$get_streamnodelist_length()) {
        if(is.null(subsetNodeIDs) || .self$streamnodeList[[i]]$nodeID %in% subsetNodeIDs) {
          .self$streamnodeList[[i]]$bed_slope <- return_slopes[as.character(.self$streamnodeList[[i]]$nodeID)]
        }
      }

      message(sprintf("Bed slope for geometry '%s' have been updated", .self$geomname))
      return(TRUE)
    },

    compute_preprocessing_tables = function(bbopt, subsetNodeIDs = NULL, skip_extent_checks=FALSE,
                                                runparallel=FALSE, applyfuzzy=FALSE, usefuzzyhand=FALSE) {

      if (runparallel & applyfuzzy) {
        stop("not yet configured to run fuzzy in parallel")
      }

      # Check bbopt validity
      if (class(bbopt) != "bb_options") {
        stop("bbopt must be of class bb_options")
      }

      # check that seqH is non-empty
      if (length(bbopt$Hseq) <= 1) {
        stop("multiple flow points are required to pre-process depth tables,\nplease check the bbopt$Hseq blackbird options")
      }
      # check that dhand_seqH is not empty
      if (bbopt$use_dhand & length(bbopt$dhand_Hseq) <= 1) {
        stop("Multiple DHAND rasters required to use DHAND functionality.\nCheck bbopt$dhand_Hseq.")
      }

      # Set working folder
      workingfolder <- bbopt$workingfolder

      ## check which nodes are xsection and catchment
      sdf <- .self$get_streamnodeList_as_dataframe()
      if (is.null(subsetNodeIDs)) {subsetNodeIDs <- sdf$nodeID}
      sdf <- sdf[sdf$nodeID %in% subsetNodeIDs,]
      catchmentnodes <- sdf[sdf$nodetype == "catchment" & sdf$nodeID %in% subsetNodeIDs,]$nodeID
      xsectionnodes <- sdf[sdf$nodetype == "xsection" & sdf$nodeID %in% subsetNodeIDs,]$nodeID

      if (any(sdf$nodetype %notin% c("catchment","xsection"))) {
        stop("unhandled streamnode types found")
      }

      ## compute xsection preprocessing tables
      # xxx add subsection for xsection, to do
      if (length(xsectionnodes)>0) {
        for (nodeID in xsectionnodes) {
          ii <- which(sdf$nodeID == nodeID)
          message(sprintf("Computing preprocessing tables for .self$streamnodeList[[i]] i=%i",ii))
          .self$streamnodeList[[ii]]$compute_preprocessed_depthdf(bbopt)
        }
      }

      if (length(catchmentnodes)>0) {

        # various checks
        # xxx might make sense to move these elsewhere?

        # check if valid bbopt$catchment_conveyance_method provided
        if (bbopt$catchment_conveyance_method %notin% bb_get_catchment_conveyance_methods() ) {
          warning(sprintf("Unsupported catchment conveyance method: %s. Defaulting to 'areaweighted_conveyance_onecalc'."))
          bbopt$catchment_conveyance_method <- "areaweighted_conveyance_onecalc"
        }

        # check bbopt$Manning_composite_method for recognized methods
        if (bbopt$Manning_composite_method %notin% bb_get_composite_Manning_methods()) {
          warning(sprintf("Unrecognized bbopt - Manning_composite_method '%s'. Using default (equal_velocity)",
                          bbopt$Manning_composite_method))
          bbopt$Manning_composite_method <- "equal_velocity"
        }

        # check for consistency with areaweighted_conveyance and manning composite method
        if (bbopt$catchment_conveyance_method == "areaweighted_conveyance_onecalc" & bbopt$Manning_composite_method != "weighted_average_area") {
          warning("conveyance method 'areaweighted_conveyance_onecalc' should be used with the Manning composite method 'weighted_average_area' for consistency.")
        }

        # Read in relevant raster files and prepare them for data operations
        catchment_raster <- terra::rast(bb_get_catchmentsfromstreamnodesraster(workingfolder,returnobject = FALSE))
        catchment <- terra::as.matrix(catchment_raster)
        # rm(catchment_raster) # keep around for clipping other rasters?

        uni <- subsetNodeIDs

        print("read catchment_raster")

        ## use hand raster method without dhand
        # pull hand in any case to check na values for determining ind
        hand <- NULL
        if (!usefuzzyhand) {
          hand_raster <- bb_get_handraster(workingfolder = workingfolder, returnobject = TRUE)
        } else {
          hand_raster <- bb_get_fuzzyhandraster(workingfolder)
        }
        if (!skip_extent_checks & terra::ext(hand_raster) != terra::ext(catchment_raster)) {
          stop("need to crop/extend hand raster to match extents of catchment raster before proceeding")
        }
        hand <- terra::as.matrix(hand_raster)
        # hand <- hand[catchment %in% uni & !is.na(catchment)]
        rm(hand_raster)

        # valid indices to be used for 'cropping'
        # assumes everything is of the same extent - likely worth checking, else have to crop/expand to same extent
        if (!applyfuzzy) {
          ind <- which(catchment %in% uni & !is.na(catchment) & !is.na(hand))
        } else {
          # using catchment stack instead of single one
          # catchment_rasterstack <- terra::rast("MadRiverProject/bb_catchmentraster_stack.tiff") # xxx make fetch functions for this
          catchment_rasterstack <- bb_get_catchmentstack(bbopt$workingfolder)
          catchments_streamnodes <- bb_get_catchmentsfromstreamnodesshp(bbopt$workingfolder)

          ind <- c()

          # find all indices where catchment exists
          for (i in 1:nrow(catchments_streamnodes)) {
            if (catchments_streamnodes$pointid[i] %in% uni) {
              # if in uni, append with additional non-NA vector
              ind <- c(ind,which(as.vector(catchment_rasterstack[[i]])==catchments_streamnodes$pointid[i]))
            }
          }
          ind <- intersect(unique(ind), which(!is.na(hand)))
        }

        hand <- hand[ind]

        # filter catchment raster
        catchment <- catchment[ind]

        # filter all catchmentraster_stack
        catchrs=NULL
        if (applyfuzzy) {
          # convert raster stack to matrix
          catchrs <- array(data=NA, dim=c(nrow(catchments_streamnodes), length(ind)))
          for (i in 1:nrow(catchments_streamnodes)) {
            rr <- catchment_rasterstack[[i]]
            catchrs[i,] <- as.vector(rr)[ind]
          }
          rm(catchments_streamnodes)
        }
        # xxx consider how dhands and newind play together here?

        # handid raster
        handid <- NULL
        handid_raster <- bb_get_handpourpointIDraster(workingfolder = workingfolder, returnobject = TRUE)
        if (terra::ext(handid_raster) != terra::ext(catchment_raster)) {
          stop("need to crop/extend hand raster to match extents of catchment raster before proceeding")
        }
        handid <- terra::as.matrix(handid_raster)
        # hand <- hand[catchment %in% uni & !is.na(catchment)]
        rm(handid_raster)
        handid <- handid[ind]

        # dhand processing (if used)
        dhands <- array(data=NA, dim=c(length(bbopt$Hseq), length(catchment)))
        dhandsid <- array(data=NA, dim=c(length(bbopt$Hseq), length(catchment)))
        if (bbopt$use_dhand) {
          # dhands <- array(data=NA, dim=c(length(bbopt$Hseq), length(catchment)))
          for(i in 1:length(bbopt$Hseq)) {
            # print(sprintf("i is %i",i))

            d <- bbopt$Hseq[i]
            ## use dhand method

            # assume bbopt$dhand_Hseq is accurate to files in folder
            # find closest depths to Hseq
            if (d %in% bbopt$dhand_Hseq) {
              # print("found in dhand_Hseq")
              # no interpolation required, just use the right file

              f_dhand <- bb_get_dhandraster(workingfolder=bbopt$workingfolder,returnobject=FALSE,depth = d,filetype = "depthraster")

              if (file.exists(f_dhand)) {
                hand_raster <- terra::rast(f_dhand)

                if (terra::ext(hand_raster) != terra::ext(catchment_raster)) {
                  stop("need to crop/extend dhand raster to match extents of catchment raster before proceeding")
                }

                hand_raster <- terra::as.matrix(hand_raster)
                dhands[i,] <- hand_raster[ind]  # hand_raster[catchment %in% uni]
                dhandsid[i,] <- terra::as.matrix(bb_get_dhandraster(workingfolder=bbopt$workingfolder,returnobject=TRUE,depth = d,filetype = "idraster"))[ind]
                rm(hand_raster)
              } else {
                stop(sprintf("Looking for file that was not found:\n %s",f_dhand))
              }

            } else {
              # find the two closest files, if just one use the closest and throw a warning
              closest_depths <- bb_closestdepths(x=bbopt$dhand_Hseq, value=d)
              f_dhand1 <- bb_get_dhandraster(workingfolder=bbopt$workingfolder,returnobject=FALSE,depth =closest_depths[1],filetype = "depthraster")
              if (terra::ext(terra::rast(f_dhand1)) != terra::ext(catchment_raster)) {
                stop("need to crop/extend dhand raster to match extents of catchment raster before proceeding")
              }

              if (length(closest_depths) == 2) {
                f_dhand2 <- bb_get_dhandraster(workingfolder=bbopt$workingfolder,returnobject=FALSE,depth =closest_depths[2],filetype = "depthraster")

                if (file.exists(f_dhand1)) {
                  dhand1_raster <- terra::rast(f_dhand1)

                  if (terra::ext(dhand1_raster) != terra::ext(catchment_raster)) {
                    stop("need to crop/extend dhand raster to match extents of catchment raster before proceeding")
                  }

                  dhand1 <- terra::as.matrix(dhand1_raster)
                  dhand1 <- dhand1[ind] # dhand1[catchment %in% uni]
                  rm(dhand1_raster)
                } else {
                  stop(sprintf("Looking for file that was not found:\n %s",f_dhand1))
                }
                if (file.exists(f_dhand2)) {
                  dhand2_raster <- terra::rast(f_dhand2)
                  dhand2 <- terra::as.matrix(dhand2_raster)
                  dhand2 <- dhand2[ind] #  dhand2[catchment %in% uni]
                  rm(dhand2_raster)
                } else {
                  stop(sprintf("Looking for file that was not found:\n %s",f_dhand2))
                }
                d1 <- closest_depths[1]
                d2 <- closest_depths[2]
                # xxx alternative here would be to just use the min DHAND, not interpolate
                dhands[i,] <- dhand1*((d1 - d)/(d1 - d2)) + dhand2*((d - d2)/(d1 - d2))
                rm(dhand1)
                rm(dhand2)
              } else {
                # add exception for trying to find closest values to the boundary
                warning("Depth is outside the bounds of bbopt$dhand_Hseq. Using closest available dhand, though results should be re-run with more dhand rasters to cover this depth")
                if (file.exists(f_dhand1)) {
                  hand_raster <- terra::rast(f_dhand1)
                  if (terra::ext(hand_raster) != terra::ext(catchment_raster)) {
                    stop("need to crop/extend dhand raster to match extents of catchment raster before proceeding")
                  }
                  hand_raster <- terra::as.matrix(hand_raster)
                  dhands[i,] <- hand_raster[ind]   # hand_raster[catchment %in% uni]
                  rm(hand_raster)
                } else {
                  stop(sprintf("Looking for file that was not found:\n %s",f_dhand1))
                }
              }
              dhandsid[i,] <- terra::as.matrix(bb_get_dhandraster(workingfolder=bbopt$workingfolder,returnobject=TRUE,depth = max(closest_depths),filetype = "idraster"))[ind]
            }
          }

          # # check for any NA values in dhands
          for (i in 1:length(bbopt$Hseq)) {
            if (any(is.na(dhands[i,]))) {
              warning(sprintf("%i NA value(s) found in dhands of length %i with depth of %.3f, check extents and values",
                              length(which(is.na(dhands[i,]))),
                              length(dhands[i,]),
                                      bbopt$Hseq[i]))
            }
          }

          # check for any NA values in dhandsid
          for (i in 1:length(bbopt$Hseq)) {
            if (any(is.na(dhandsid[i,]))) {
              warning(sprintf("%i NA value(s) found in dhandsid of length %i with depth of %.3f, check extents and values",
                              length(which(is.na(dhandsid[i,]))),
                              length(dhandsid[i,]),
                              bbopt$Hseq[i]))
            }
          }

        }

        print("read dhands")

        # option added to skip extent checks, as that is quite slow

        dem_raster <- terra::rast(bb_get_demraster(workingfolder = workingfolder, returnobject = FALSE))
        if (!skip_extent_checks & terra::ext(dem_raster) != terra::ext(catchment_raster)) {
          stop("need to crop/extend dem raster to match extents of catchment raster before proceeding")
        }

        dem <- terra::as.matrix(dem_raster)
        # dem <- dem[catchment %in% uni & !is.na(catchment)]
        dem <- dem[ind]
        a <- terra::res(dem_raster)
        rm(dem_raster)
        if (a[1] == a[2]) { # check for square dem one more time
          a <- a[1]
        } else {
          stop("dem must be square resolution")
        }

        manningsn_raster <- terra::rast(bb_get_manningsnraster(workingfolder = workingfolder, returnobject = FALSE))
        if (!skip_extent_checks & terra::ext(manningsn_raster) != terra::ext(catchment_raster)) {
          stop("need to crop/extend manningsn raster to match extents of catchment raster before proceeding")
        }

        manningsn <- terra::as.matrix(manningsn_raster)
        # manningsn <- manningsn[catchment %in% uni & !is.na(catchment)]
        manningsn <- manningsn[ind]
        rm(manningsn_raster)

        reachlength <- rep(NA,length(hand))
        # if (bbopt$modeltype != "hand-manning") {
        reachlength_raster <- terra::rast(bb_get_reachlengthraster(workingfolder, returnobject = FALSE))
        if (!skip_extent_checks & terra::ext(reachlength_raster) != terra::ext(catchment_raster)) {
          stop("need to crop/extend reachlength raster to match extents of catchment raster before proceeding")
        }
        reachlength <- terra::as.matrix(reachlength_raster)
        # reachlength <- reachlength[catchment %in% uni & !is.na(catchment)]
        reachlength <- reachlength[ind]
        rm(reachlength_raster)
        # }

        # slope_raster <- terra::rast(bb_get_sloperaster(workingfolder,returnobject = FALSE))
        # if (!skip_extent_checks & terra::ext(slope_raster) != terra::ext(catchment_raster)) {
        #   stop("need to crop/extend slope raster to match extents of catchment raster before proceeding")
        # }
        # slope <- terra::as.matrix(slope_raster)
        # # slope <- slope[catchment %in% uni & !is.na(catchment)]
        # slope <- slope[ind]
        # rm(slope_raster)

        print("read reach length")

        # final check on missing values in inputs before processing
        ## subset further if needed
        newind <- seq(1,length(dem))
        for (rr in list(catchment,dem,hand,manningsn,reachlength)) {
          if (any(is.na(rr))) {
            # message(sprintf("NA value found in one or more layers"))
            temp <- which(!is.na(reachlength))
            newind <- newind[which(newind %in% temp)]
          }
        }

        # subset from dhands if needed to avoid NA values
        if (bbopt$use_dhand) {
          for (i in 1:length(bbopt$Hseq)) {
            if (any(is.na(dhands[i,]))) {
              message(sprintf("NA value found in dhands with depth of %.3f, check extents and values", bbopt$Hseq[i]))
              temp <- which(!is.na(dhands[i,]))
              newind <- newind[which(newind %in% temp)]
            }
          }
        }

        # subset if needed
        if (length(newind) < length(dem)) {
          catchment <- catchment[newind]
          dem <- dem[newind]
          hand <- hand[newind]
          handid <- handid[newind]
          manningsn <- manningsn[newind]
          reachlength <- reachlength[newind]
          # slope <- slope[newind]

          if (bbopt$use_dhand) {
            dhands2 <- array(data=NA, dim=c(length(bbopt$Hseq), length(newind)))
            dhandsid2 <- array(data=NA, dim=c(length(bbopt$Hseq), length(newind)))
            for (i in 1:length(bbopt$Hseq)) {
              dhands2[i,] <- dhands[i,newind]
              dhandsid2[i,] <- dhandsid[i,newind]
            }
            dhands <- dhands2
            dhandsid <- dhandsid2
            rm(dhands2)
            rm(dhandsid2)
          }

          if (applyfuzzy) {
            catchrs2 <- array(data=NA, dim=c(nrow(catchrs), length(newind)))
            for (i in 1:nrow(catchrs2)) {
              catchrs2[i,] <- catchrs[i,newind]
            }
            catchrs <- catchrs2
            rm(catchrs2)
          }
        }


        # xxx also check dhandsid? Should be conssitent with dhands

        # if (!bbopt$use_dhand) {
        #   hand <- hand[ind]
        # } else {
        #   for(i in 1:length(bbopt$Hseq)) {
        #     dhands[i] <- dhands[i,ind]
        #   }
        # }

        print("generating initial profiles")

        if (runparallel) {

          ## manage directories
          tmd <- tempdir()
          cwd <- getwd()
          setwd(tmd)

          ## cleanup all blackbird_preproc_ csv files (just in case there are some here)
          list.files(pattern="blackbird_preproc*") %>%
            unlink()

          ## setup parallel set
          require(parallel)
          require(doParallel)
          totalCores = detectCores()
          usableCores <- totalCores - bbopt$reserve_cores
          message(sprintf("Using up to %i cores of %i available",min(length(uni),usableCores),totalCores))

          ## divide up jobs
          # len_nodes <- bbgeom$get_streamnodelist_length()
          chunks <- chunk_seq(uni,usableCores)

          # initialize cluster
          cluster <- makeCluster(usableCores, outfile="Log.txt", setup_strategy = "sequential")
          registerDoParallel(cluster)

          on.exit(stopCluster(cluster))
          on.exit(registerDoSEQ())
          on.exit(bb_unregister_dopar())
          # on.exit(setwd(cwd)) # reset working directory if error occurs

          sdf <- .self$get_streamnodeList_as_dataframe()

          # run parallel code
          foreach(cc = chunks,
                  .packages='blackbird',
                  .export=c(".self") # .self not automatically exported for some reason
                  ) %dopar% {

                    uni2 <- as.numeric(unlist(cc))
                    len_nodes <- length(uni2) # .self$get_streamnodelist_length()

                    # index of specific entries in streamnodeList to compute
                    indsdf2 <- which(sdf$nodeID %in% uni2)

                    for(i in indsdf2) {
                      .self$streamnodeList[[i]]$depthdf <- .self$streamnodeList[[i]]$generate_initial_hydraulic_profile(nrow=length(bbopt$Hseq))
                      .self$streamnodeList[[i]]$depthdf$Depth <- bbopt$Hseq
                      .self$streamnodeList[[i]]$depthdf$WSL <- .self$streamnodeList[[i]]$depthdf$Min_Elev + .self$streamnodeList[[i]]$depthdf$Depth

                      .self$streamnodeList[[i]]$depthdf$nodeID <- sdf$nodeID[i]
                      .self$streamnodeList[[i]]$depthdf$reachID <- sdf$reachID[i]
                      .self$streamnodeList[[i]]$depthdf$downnodeID <- sdf$downnodeID[i]
                      .self$streamnodeList[[i]]$depthdf$upnodeID1 <- sdf$upnodeID1[i]
                      .self$streamnodeList[[i]]$depthdf$upnodeID2 <- sdf$upnodeID2[i]
                      .self$streamnodeList[[i]]$depthdf$Area <- 0
                      .self$streamnodeList[[i]]$depthdf$WetPerimeter <- 0
                      .self$streamnodeList[[i]]$depthdf$HRadius <- 0
                      .self$streamnodeList[[i]]$depthdf$K_Total <- 0
                      .self$streamnodeList[[i]]$depthdf$alpha <- 0
                      .self$streamnodeList[[i]]$depthdf$Manning_Composite <- 0
                      .self$streamnodeList[[i]]$depthdf$Length_Effective <- 0
                      .self$streamnodeList[[i]]$depthdf$TopWidth <- 0
                      .self$streamnodeList[[i]]$depthdf$HydDepth <- 0
                      .self$streamnodeList[[i]]$depthdf$K_Total_areaconv <- 0
                      .self$streamnodeList[[i]]$depthdf$K_Total_disconv <- 0
                      .self$streamnodeList[[i]]$depthdf$K_Total_roughconv <- 0
                      .self$streamnodeList[[i]]$depthdf$alpha_areaconv <- 0
                      .self$streamnodeList[[i]]$depthdf$alpha_disconv <- 0
                      .self$streamnodeList[[i]]$depthdf$alpha_roughconv <- 0
                      .self$streamnodeList[[i]]$depthdf$nc_equalforce <- 0
                      .self$streamnodeList[[i]]$depthdf$nc_equalvelocity <- 0
                      .self$streamnodeList[[i]]$depthdf$nc_wavgwp <- 0
                      .self$streamnodeList[[i]]$depthdf$nc_wavgarea <- 0
                      .self$streamnodeList[[i]]$depthdf$nc_wavgconv <- 0

                      preproc_table <- .self$streamnodeList[[i]]$depthdf

                      # call R function to compute properties
                      preproc_table <- bb_compute_preproc_hydprops(i, bbopt, preproc_table, a, sdf,
                                                                   catchment, dem, hand, handid, dhands, dhandsid, manningsn, reachlength)

                      ## consider passing preproc_table right back to .self instead of writing to file, likely slowing things down

                      # write out to file (safer than storing directly back in to object, but could try that too)
                      write.csv(preproc_table,
                                sprintf("blackbird_preproc_table_nodeID_%i.csv",sdf$nodeID[i]),
                                quote=FALSE,row.names=FALSE)

                    }
                  }
          stopCluster(cluster)
          registerDoSEQ()
          bb_unregister_dopar()
          closeAllConnections()

          print("parallel clusters stopped")

          ## function to bring everything back in to .self
          ff <- list.files(pattern="blackbird_preproc*")
          ffnodeIDs <- as.numeric(bb_substrMLeft(bb_substrMRight(ff,4),31))
          sdf <- .self$get_streamnodeList_as_dataframe()
          indsdf <- which(sdf$nodeID %in% ffnodeIDs)

          for (i in 1:length(ff)) {
            ppt <- read.csv(ff[i])

            # assign depthdf table from ppt into geometry (.self) object at right index
            .self$streamnodeList[[which(sdf$nodeID == ffnodeIDs[i])]]$depthdf <- ppt
          }

          ## cleanup all blackbird_preproc_ csv files
          list.files(pattern="blackbird_preproc*") %>%
            unlink()

          setwd(cwd)

        } else {

          ## running serial for subset of nodes

          # Generate initial hydraulic profile for each streamnode
          # len_all_nodes <- .self$get_streamnodelist_length()
          len_nodes <- length(uni) # .self$get_streamnodelist_length()
          sdf <- .self$get_streamnodeList_as_dataframe()

          # index of specific entries in streamnodeList to compute
          indsdf <- which(sdf$nodeID %in% uni)

          for(i in indsdf) {
            # reachshp_lens[i] <- as.numeric(sf::st_length(.self$streamnodeList[[i]]$reachshp))
            # if(as.integer(.self$get_streamnodeList_as_dataframe()["nodeID"][[1]][i]) %notin% uni) {
            #   next
            # }
            .self$streamnodeList[[i]]$depthdf <- .self$streamnodeList[[i]]$generate_initial_hydraulic_profile(nrow=length(bbopt$Hseq))
            .self$streamnodeList[[i]]$depthdf$Depth <- bbopt$Hseq
            .self$streamnodeList[[i]]$depthdf$WSL <- .self$streamnodeList[[i]]$depthdf$Min_Elev + .self$streamnodeList[[i]]$depthdf$Depth

            .self$streamnodeList[[i]]$depthdf$nodeID <- sdf$nodeID[i]
            .self$streamnodeList[[i]]$depthdf$reachID <- sdf$reachID[i]
            .self$streamnodeList[[i]]$depthdf$downnodeID <- sdf$downnodeID[i]
            .self$streamnodeList[[i]]$depthdf$upnodeID1 <- sdf$upnodeID1[i]
            .self$streamnodeList[[i]]$depthdf$upnodeID2 <- sdf$upnodeID2[i]
            .self$streamnodeList[[i]]$depthdf$Area <- 0
            .self$streamnodeList[[i]]$depthdf$WetPerimeter <- 0
            .self$streamnodeList[[i]]$depthdf$HRadius <- 0
            .self$streamnodeList[[i]]$depthdf$K_Total <- 0
            .self$streamnodeList[[i]]$depthdf$alpha <- 0
            .self$streamnodeList[[i]]$depthdf$Manning_Composite <- 0
            .self$streamnodeList[[i]]$depthdf$Length_Effective <- 0
            .self$streamnodeList[[i]]$depthdf$TopWidth <- 0
            .self$streamnodeList[[i]]$depthdf$HydDepth <- 0
            .self$streamnodeList[[i]]$depthdf$K_Total_areaconv <- 0
            .self$streamnodeList[[i]]$depthdf$K_Total_disconv <- 0
            .self$streamnodeList[[i]]$depthdf$K_Total_roughconv <- 0
            .self$streamnodeList[[i]]$depthdf$alpha_areaconv <- 0
            .self$streamnodeList[[i]]$depthdf$alpha_disconv <- 0
            .self$streamnodeList[[i]]$depthdf$alpha_roughconv <- 0
            .self$streamnodeList[[i]]$depthdf$nc_equalforce <- 0
            .self$streamnodeList[[i]]$depthdf$nc_equalvelocity <- 0
            .self$streamnodeList[[i]]$depthdf$nc_wavgwp <- 0
            .self$streamnodeList[[i]]$depthdf$nc_wavgarea <- 0
            .self$streamnodeList[[i]]$depthdf$nc_wavgconv <- 0

            preproc_table <- .self$streamnodeList[[i]]$depthdf

            # call R function to compute properties
            preproc_table <- bb_compute_preproc_hydprops(i, bbopt, preproc_table, a, sdf,
                                                         catchment, dem, hand, handid, dhands, dhandsid, manningsn, reachlength,
                                                         catchrs, applyfuzzy=applyfuzzy)

            .self$streamnodeList[[indsdf[i]]]$depthdf <- preproc_table

          }
        }

      }
      .self$check_preprocessing_tables()
      return(TRUE)
    },

    update_preproc_tables_bymethod = function(bbopt=NULL) {

      if (is.null(bbopt)) {stop("bbopt is required")}
      if (!bbopt$use_preproc) {stop("This function assumes that preprocessing is used and depthdf tables have been generated")}

      # check specified methods
      if (bbopt$catchment_conveyance_method %notin% bb_get_catchment_conveyance_methods()) {
        stop("bbopt$catchment_conveyance_method must be a valid method (check bb_get_catchment_conveyance_methods() )")
      }
      if (bbopt$Manning_composite_method %notin% bb_get_composite_Manning_methods()) {
        stop("bbopt$catchment_conveyance_method must be a valid method (check bb_get_composite_Manning_methods() )")
      }

      sdf <- .self$get_streamnodeList_as_dataframe()

      for (i in 1:nrow(sdf)) {

        if (.self$streamnodeList[[i]]$nodetype == "catchment") {

          # check conveyance methods, update
          if (bbopt$catchment_conveyance_method == "discretized_conveyance") {
            .self$streamnodeList[[i]]$depthdf$K_Total <- .self$streamnodeList[[i]]$depthdf$K_Total_disconv
            .self$streamnodeList[[i]]$depthdf$alpha <- .self$streamnodeList[[i]]$depthdf$alpha_disconv
          } else if (bbopt$catchment_conveyance_method == "areaweighted_conveyance_onecalc") {
            .self$streamnodeList[[i]]$depthdf$K_Total <- .self$streamnodeList[[i]]$depthdf$K_Total_areaconv
            .self$streamnodeList[[i]]$depthdf$alpha <- .self$streamnodeList[[i]]$depthdf$alpha_areaconv
          } else if (bbopt$catchment_conveyance_method == "roughzone_conveyance") {
            .self$streamnodeList[[i]]$depthdf$K_Total <- .self$streamnodeList[[i]]$depthdf$K_Total_roughconv
            .self$streamnodeList[[i]]$depthdf$alpha <- .self$streamnodeList[[i]]$depthdf$alpha_roughconv
          }

          else if (bbopt$catchment_conveyance_method == "blended_conveyance") {
            ### blended code (not added to bbopt and elsewhere, yet)
            # note that if blending, would need to call this method in every iteration where weights change

            if (length(bbopt$blended_conveyance_weights) != 3) {
              stop("length of bbopt$blended_conveyance_weights must be 3.")
            }
            if (sum(bbopt$blended_conveyance_weights)!=1.0) {
              stop("sum of bbopt$blended_conveyance_weights must be 1.0")
            }
            # sorted highest to lowest
            .self$streamnodeList[[i]]$depthdf$K_Total <-
              .self$streamnodeList[[i]]$depthdf$K_Total_disconv*bbopt$blended_conveyance_weights[1]+
              .self$streamnodeList[[i]]$depthdf$K_Total_roughconv*bbopt$blended_conveyance_weights[2]+
              .self$streamnodeList[[i]]$depthdf$K_Total_areaconv*bbopt$blended_conveyance_weights[3]
            .self$streamnodeList[[i]]$depthdf$alpha <-
              .self$streamnodeList[[i]]$depthdf$alpha_disconv*bbopt$blended_conveyance_weights[1]+
              .self$streamnodeList[[i]]$depthdf$alpha_roughconv*bbopt$blended_conveyance_weights[2]+
              .self$streamnodeList[[i]]$depthdf$alpha_areaconv*bbopt$blended_conveyance_weights[3]
          }

          # check manning composite methods, update
          if (bbopt$Manning_composite_method == "equal_force") {
            .self$streamnodeList[[i]]$depthdf$Manning_Composite <- .self$streamnodeList[[i]]$depthdf$nc_equalforce
          } else if (bbopt$Manning_composite_method == "weighted_average_area") {
            .self$streamnodeList[[i]]$depthdf$Manning_Composite <- .self$streamnodeList[[i]]$depthdf$nc_wavgarea
          } else if (bbopt$Manning_composite_method == "weighted_average_wetperimeter") {
            .self$streamnodeList[[i]]$depthdf$Manning_Composite <- .self$streamnodeList[[i]]$depthdf$nc_wavgwp
          } else if (bbopt$Manning_composite_method == "weighted_average_conveyance") {
            .self$streamnodeList[[i]]$depthdf$Manning_Composite <- .self$streamnodeList[[i]]$depthdf$nc_wavgconv
          } else if (bbopt$Manning_composite_method == "equal_velocity") {
            .self$streamnodeList[[i]]$depthdf$Manning_Composite <- .self$streamnodeList[[i]]$depthdf$nc_equalvelocity
          } else if (bbopt$Manning_composite_method == "blended_nc") {
            if (length(bbopt$blended_nc_weights) != 5) {
              stop("length of bbopt$blended_nc_weights must be 5.")
            }
            if (sum(bbopt$blended_nc_weights)!=1.0) {
              stop("sum of bbopt$blended_nc_weights must be 1.0")
            }
            # sorted highest to lowest
            .self$streamnodeList[[i]]$depthdf$Manning_Composite <-
              .self$streamnodeList[[i]]$depthdf$nc_equalforce*bbopt$blended_nc_weights[1]+
              .self$streamnodeList[[i]]$depthdf$nc_equalvelocity*bbopt$blended_nc_weights[2]+
              .self$streamnodeList[[i]]$depthdf$nc_wavgwp*bbopt$blended_nc_weights[3]+
              .self$streamnodeList[[i]]$depthdf$nc_wavgarea*bbopt$blended_nc_weights[4]+
              .self$streamnodeList[[i]]$depthdf$nc_wavgconv*bbopt$blended_nc_weights[5]
          }
        }
      }

      return(TRUE)
    },

    # return a list of depthdf (preprocessed) tables
    get_preprocessing_tables = function() {
      len_nodes <- .self$get_streamnodelist_length()
      df <- lapply(seq(1:len_nodes), FUN=function(x) .self$streamnodeList[[x]]$depthdf)
      return(df)
    },

    # return a subsetted list of depthdf (preprocessed) tables
    get_preprocessing_tables_subset = function(subsetNodeIDs=NULL) {
      sdf <- .self$get_streamnodeList_as_dataframe()
      indss <- which(sdf$nodeID %in% subsetNodeIDs)
      len_nodes <- length(indss)
      if (length(indss)==0) {return(NULL)}
      df <- lapply(indss, FUN=function(x) .self$streamnodeList[[x]]$depthdf)
      return(df)
    },

    # set depthdf for each streamnode using list x (likely obtained from a previous call of get_preprocessing_tables)
    set_preprocessing_tables = function(x) {
      len_nodes <- .self$get_streamnodelist_length()
      for (i in 1:len_nodes) {
        .self$streamnodeList[[i]]$depthdf <- x[[i]]
      }
      message(sprintf("Pre-processing tables for geometry '%s' have been updated", .self$geomname))
      return(TRUE)
    },

    check_preprocessing_tables = function() {
      len_nodes <- .self$get_streamnodelist_length()
      issuecount <- 0
      sdf <- .self$get_streamnodeList_as_dataframe()
      for (i in 1:nrow(sdf)) {
        x <- .self$streamnodeList[[i]]$depthdf$Area

        if (!all(x == cummax(x))) {
          issuecount <- issuecount+1
          warning(sprintf("Equivalent cross-sectional flow Area for streamnode %i (index %i) is not monotonically increasing, check for errors",
                          sdf$nodeID[i], i))
        }
        # other checks to make in preprocessing? xxx
      }
      if (issuecount==0) {
        message("No issues found in geometry preprocessing tables")
      } else {
        message(sprintf("Found %i possible issues in preprocessing tables",issuecount))
      }

    },

    check_streamnode_properties = function() {
      # xxx add check of changes in min bed elevation
      len_nodes <- .self$get_streamnodelist_length()
      issuecount <- 0

      for (i in 1:len_nodes) {
        x <- .self$streamnodeList[[i]]$ds_reach_length

        if (length(x) != 1) {
          issuecount <- issuecount+1
          warning(sprintf("ds_reach_length for streamnode %i (index %i) is missing, check for errors",
                          .self$streamnodeList[[i]]$nodeID, i))
        }

        x <-  .self$streamnodeList[[i]]$us_reach_length1
        if (length(x) != 1) {
          issuecount <- issuecount+1
          warning(sprintf("us_reach_length1 for streamnode %i (index %i) is missing, check for errors",
                          .self$streamnodeList[[i]]$nodeID, i))
        }

        x <-  .self$streamnodeList[[i]]$us_reach_length2
        if (length(x) != 1) {
          issuecount <- issuecount+1
          warning(sprintf("us_reach_length2 for streamnode %i (index %i) is missing, check for errors",
                          .self$streamnodeList[[i]]$nodeID, i))
        }



      }
      if (issuecount==0) {
        message("No issues found in geometry streamnodes")
      } else {
        message(sprintf("Found %i possible issues in geometry streamnodes",issuecount))
      }

    },

    get_preproc_properties = function(depth=1.0,prop=c("Area")) {
      # get table of properties from preproc across all nodes
      # useful in diagnotics
      # for a specific depth, get any number of propreties into the same dataframe for all streamnodes

      len_nodes <- .self$get_streamnodelist_length()
      sdf <- .self$get_streamnodeList_as_dataframe()
      proptable <- sdf[,c("nodeID","nodetype","reachID","ds_reach_length","us_reach_length1")]

      for (j in 1:length(prop)) {
        proptable[[prop[j]]] <- NA
      }

      for (i in 1:nrow(sdf)) {
        x <- .self$streamnodeList[[i]]$depthdf

        if (depth %notin% x$Depth) {
          stop(sprintf("Depth not found in streamnode ID=%i",x$nodeID[1]))
        }
        if (any(prop %notin% colnames(x))) {
          stop(sprintf("Property %s not found in streamnode ID=%i",prop,x$nodeID[1]))
        }

        for (j in 1:length(prop)) {
          proptable[[prop[j]]][i] <- x[x$Depth==depth,][[prop[j]]]
        }
      }
      return(proptable)
    },

    get_stations = function() {
      # len_nodes <- length(.self$streamnodeList)
      len_nodes <- .self$get_streamnodelist_length()
      stations <- rep(-1,len_nodes)

      for (i in 1:len_nodes) {
        stations[i] <- .self$streamnodeList[[i]]$station
      }
      return(stations)
    },

    get_upstream_areas = function(bbopt=NULL) {

      # len_nodes <- length(.self$streamnodeList)
      len_nodes <- .self$get_streamnodelist_length()
      sdf <- .self$get_streamnodeList_as_dataframe()
      sdf$upstream_areas <- -99  # m2
      # sdf$areas <-  -99          # m2

      catchments_streamnodes <- bb_get_catchmentsfromstreamnodesshp(workingfolder = bbopt$workingfolder)
      catchments_streamnodes$areas <- as.numeric(sf::st_area(catchments_streamnodes))
      catchments_streamnodes$nodeID <- catchments_streamnodes$pointid
      catchments_streamnodes <- catchments_streamnodes[,c("nodeID","areas")]
      catchments_streamnodes <-  sf::st_drop_geometry(catchments_streamnodes)

      sdf <- left_join(sdf,catchments_streamnodes, by="nodeID")

      # check for any missing references, set to -1
      # should be fixed in preprocess_catchments_sf but here again just in case
      if (any(sdf$downnodeID %notin% sdf$nodeID)) {
        sdf[sdf$downnodeID %notin% sdf$nodeID,]$downnodeID <- -1
      }
      if (any(sdf$upnodeID1 %notin% sdf$nodeID)) {
        sdf[sdf$upnodeID1 %notin% sdf$nodeID,]$upnodeID1 <- -1
      }
      if (any(sdf$upnodeID2 %notin% sdf$nodeID)) {
        sdf[sdf$upnodeID2 %notin% sdf$nodeID,]$upnodeID2 <- -1
      }

      links<-data.frame(nodeID=sdf$nodeID,downnodeID=sdf$downnodeID)
      links<-subset.data.frame(links,downnodeID>=0) # get rid of -1

      net <- igraph::graph_from_data_frame(d=links, vertices=sdf, directed=TRUE)
      egon <- igraph::ego(net,order=100, nodes=V(net),mode="in")
      count=1
      for (i in 1:nrow(sdf)){
        nodeID = sdf$nodeID[i]
        up <- subset.data.frame(sdf, nodeID %in% as_ids(egon[[i]]))
        sdf$upstream_areas[i] <- sum(up$areas)
        count=count+1
      }

      return(sdf)
    },

    get_streamnodeList_as_dataframe = function() {
      # len_nodes <- length(.self$streamnodeList)
      len_nodes <- .self$get_streamnodelist_length()
      streamnodedf <- data.frame(matrix(NA,nrow=len_nodes,ncol=15))

      ## XXX TO DO - add reach name here

      colnames(streamnodedf) <- c("nodeID","nodetype","downnodeID","upnodeID1","upnodeID2","stationname",
                                  "station","reachID","ds_reach_length","us_reach_length1","us_reach_length2",
                                  "contraction_coeff","expansion_coeff","min_elev","bed_slope")

      for (i in 1:len_nodes) {
        streamnodedf[i,] <- c(.self$streamnodeList[[i]]$nodeID,
                              .self$streamnodeList[[i]]$nodetype,
                              .self$streamnodeList[[i]]$downnodeID,
                              .self$streamnodeList[[i]]$upnodeID1,
                              .self$streamnodeList[[i]]$upnodeID2,
                              .self$streamnodeList[[i]]$stationname,
                              .self$streamnodeList[[i]]$station,
                              .self$streamnodeList[[i]]$reachID,
                              .self$streamnodeList[[i]]$ds_reach_length,
                              .self$streamnodeList[[i]]$us_reach_length1,
                              .self$streamnodeList[[i]]$us_reach_length2,
                              .self$streamnodeList[[i]]$contraction_coeff,
                              .self$streamnodeList[[i]]$expansion_coeff,
                              .self$streamnodeList[[i]]$min_elev,
                              .self$streamnodeList[[i]]$bed_slope
                              )
      }
      streamnodedf$nodeID <- as.integer(streamnodedf$nodeID)
      streamnodedf$station <- as.numeric(streamnodedf$station)
      streamnodedf$ds_reach_length <- as.numeric(streamnodedf$ds_reach_length)
      streamnodedf$us_reach_length1 <- as.numeric(streamnodedf$us_reach_length1)
      streamnodedf$us_reach_length2 <- as.numeric(streamnodedf$us_reach_length2)
      streamnodedf$min_elev <- as.numeric(streamnodedf$min_elev)
      streamnodedf$bed_slope <- as.numeric(streamnodedf$bed_slope)

      return(streamnodedf)
    },

    write_preprocessed_depthdf = function(bbopt=NULL, modelname=NULL) {

      workingfolder <- bbopt$workingfolder

      if (is.null(modelname)) {
        temp <- unlist(strsplit(workingfolder,"/"))
        modelname <- temp[length(temp)]
      }

      outputfile <- file.path(workingfolder,"model",sprintf("%s.bbg",modelname))
      # xxx replace this path with one that comes from the bb_get_object function
      if (!dir.exists(file.path(workingfolder,"model"))) {
        dir.create(file.path(workingfolder,"model"))
      }
      fc <- file(outputfile,open='w+')
      sdf <- .self$get_streamnodeList_as_dataframe()

      # xxx update to write only the properties found in bb_hydraulic_output_emptydf_propsonly

      # check that depthdf is not null before proceeding

      if (bbopt$use_preproc & !is.null(.self$streamnodeList[[1]]$depthdf)) {
        writeLines("\n:PreprocessedHydraulicTables", fc)

        for (i in 1:.self$get_streamnodelist_length()) {

          if (is.null(.self$streamnodeList[[i]]$depthdf)) {
            stop("NULL depthdf table found, please complete pre-processing to ensure all streamnodes are processed.")
          }

          writeLines(sprintf("\n  :PreprocHydTable %s # streamnode nodeID", .self$streamnodeList[[i]]$nodeID), fc)
          depthdf <- .self$streamnodeList[[i]]$depthdf

          if (nrow(depthdf)==0) {
            next
          }

          # to do xxx - get the basic properties into the depthdf tables during preprocessing of tables
          depthdf$nodeID <- .self$streamnodeList[[i]]$nodeID
          depthdf$reachID <- .self$streamnodeList[[i]]$reachID
          depthdf$downnodeID <- .self$streamnodeList[[i]]$downnodeID
          depthdf$upnodeID1 <- .self$streamnodeList[[i]]$upnodeID1
          depthdf$upnodeID2 <- .self$streamnodeList[[i]]$upnodeID2

          writeLines(paste(c("    :Attributes",colnames(depthdf)),collapse="  "),fc)
          for (j in 1:nrow(depthdf)) {
            writeLines(paste(c("    ",depthdf[j,]), collapse="  "),fc)
          }
          writeLines("  :EndPreprocHydTable", fc)
        }
        writeLines("\n:EndPreprocessedHydraulicTables", fc)
        # writeLines(sprintf("\n:RedirectToFile %s_preprocessed_hydraulic_tables.bbg",modelname), fc)
      }
      close(fc)
      return(TRUE)
    },

    read_preprocessed_depthdf = function(bbopt=NULL, modelname=NULL) {

      workingfolder <- bbopt$workingfolder

      if (is.null(workingfolder)) {stop("workingfolder must not be NULL")}

      bbg <- list.files(file.path(workingfolder,"model"),pattern="*.bbg",full.names = TRUE)[grep(list.files(file.path(workingfolder,"model")), pattern="*.bbg")]

      if (length(bbg) > 1 & !is.null(modelname)) {
        # warning("Multiple bbg files found, modelname is required to distinguish")
        if (file.exists(sprintf("%s/model/%s.bbg",workingfolder,modelname))) {
          bbg <- sprintf("%s/model/%s.bbg",workingfolder,modelname)
        } else {
          stop(sprintf("%s/%s.bbg does not exist",modelname,workingfolder))
        }
      } else if (length(bbg) == 0) {
        stop(sprintf("No files found in %s",workingfolder))
      }

      sdf <- .self$get_streamnodeList_as_dataframe()

      # read bbg file ---
      tt <- readLines(bbg)

      # parse bbg line by line
      i <- 0
      while (i <= length(tt)) {
        i <- i+1
        ss <- bb_tokenize(tt[i])

        if (is.null(ss) | length(ss) == 0) {
          # comment or blank line
        } else if (ss[1]==":PreprocessedHydraulicTables") {

          while (i <= length(tt)) {
            i <- i+1
            ss <- bb_tokenize(tt[i])

            if (is.null(ss) | length(ss) == 0) {
              # comment
            } else if (ss[1]==":EndPreprocessedHydraulicTables") {
              break
            } else if (ss[1]==":PreprocHydTable") {
              if ( (as.integer(ss[2]) %notin%  sdf$nodeID)) {
                stop(sprintf("Error reading :PreprocHydTable - nodeID %i not found in streamnodeList; please ensure that the :Streamnodes command appears before the :PreprocessedHydraulicTables command",ss[2]))
              }

              pp_nodeID <- as.integer(ss[2])

              # xxx make consistent with write
              # ppht <- bb_hydraulic_output_emptydf_propsonly(nrow=1)
              ppht <- bb_hydraulic_output_emptydf(nrow=1)
              jj <- 0 # tracking ppht index

              while (i <= length(tt)) {
                i <- i+1
                ss <- bb_tokenize(tt[i])

                if (ss[1]==":EndPreprocHydTable") {
                  break
                } else if (is.null(ss) | length(ss) == 0) {
                  # comment
                } else if (ss[1]==":Attributes") {
                  if (ss[2] != "nodeID" & ss[3] != "reachID" & length(ss) != (ncol(ppht)+1) ) {
                    stop(sprintf("Error reading :PreprocHydTable - expect %i attributes, beginining with nodeID and reachID columns",ncol(ppht)))
                  }
                } else if (length(ss) == ncol(ppht)) {
                  jj <- jj+1
                  ppht[jj,] <- ss
                } else {
                  warning(sprintf("Unrecognized command or incorrect syntax %s in :PreprocHydTable block on line %i of %s.bbg",ss[1],i,modelname))
                }
              }

              # make some columns numeric and store in geometry
              for (vv in c("nodeID", "reachID", "downnodeID", "upnodeID1", "upnodeID2")) {
                ppht[[vv]] <- as.integer(ppht[[vv]])
              }

              for (vv in c("station", "reach_length_DS", "reach_length_US1",
                           "reach_length_US2", "Flow", "Flow_LOB", "Flow_Main", "Flow_ROB",
                           "Min_Elev", "Depth", "WSL", "Velocity", "Velocity_LOB", "Velocity_Main",
                           "Velocity_ROB", "K_Total", "K_LOB", "K_Main", "K_ROB", "alpha",
                           "Area", "Area_LOB", "Area_Main", "Area_ROB", "HRadius", "HRadius_LOB",
                           "HRadius_Main", "HRadius_ROB", "WetPerimeter", "WetPerimeter_LOB",
                           "WetPerimeter_Main", "WetPerimeter_ROB", "Energy_total", "Velocity_head",
                           "Froude", "Sf", "Sf_Avg", "Length_Effective", "Head_Loss", "Manning_Composite",
                           "K_Total_areaconv","K_Total_roughconv","K_Total_disconv",
                           "alpha_areaconv","alpha_roughconv","alpha_disconv",
                           "nc_equalforce","nc_equalvelocity","nc_wavgwp","nc_wavgarea","nc_wavgconv")) {
                ppht[[vv]] <- suppressWarnings(as.numeric(ppht[[vv]]))
              }

              if (any(ppht$nodeID[1] != ppht$nodeID)) {
                stop(sprintf("Error in reading preprocessed hydraulic table for nodeID %i: all nodeIDs must be the same within the table",ppht$nodeID[1]))
              }

              # store in geometry
              for (kk in 1:length(.self$streamnodeList)) {
                if (ppht$nodeID[1] == .self$streamnodeList[[kk]]$nodeID) {
                  .self$streamnodeList[[kk]]$depthdf <- ppht
                  break
                } else if (ppht$nodeID[1] != .self$streamnodeList[[kk]]$nodeID & kk == length(.self$streamnodeList)) {
                  stop(sprintf("Unrecognized nodeID %i in preprocessed hydraulic table; ensure that nodeIDs in :Streamnodes and :PreprocessedHydraulicTables are aligned."))
                }
              }

            } else {
              warning(sprintf("Unrecognized command or incorrect syntax %s in :PreprocessedHydraulicTables block on line %i of %s.bbg",ss[1],i,modelname))
            }
          }

        } else {
          warning(sprintf("Unrecognized command %s on line %i of %s.bbg",ss[1],i,modelname))
        }
      }

      message(sprintf("geometry object %s updated with preproc depthdf",.self$geomname))
      return(TRUE)
    }
  ))

### ~~~~~~~~~~~~~~~~~~~~~
### RIVER REACH CLASS ----
### ~~~~~~~~~~~~~~~~~~~~~

#' River reach class
#'
#' riverreach:
#' A reference class to use as unit of river length for computation. By definition, each
#' reach may have an upstream or downstream connection(s), but will itself be a single
#' section of river.
#'
#' @field reachID identifying number for the reach
#' @field reachname name of the stream node as character
#' @field reachlength length of the river reach (in metres)
#' @field reachshp sf object of river reach geometry
riverreach <- setRefClass("riverreach",
    contains=c("sf"),
  field=list(reachID="integer",reachname="character",
             reachlength="numeric",reachshp="sf"),
  method = list(initialize =
                  function(..., reachID=as.integer(-99),reachname=as.character("-99"),
                          reachlength=-99, reachshp=st_sf(st_sfc()))
                  {
                    callSuper(..., reachID = reachID, reachname=reachname,
                              reachlength=reachlength, reachshp=reachshp)
                  })
)


### ~~~~~~~~~~~~~~~~~~~~~~~
### FLOW PROFILE CLASS ----
### ~~~~~~~~~~~~~~~~~~~~~~~

#' Flow profile class
#'
#' bb_flowprofile:
#' A reference class that contains one or more sets of flows for all nodes in the Blackbird model.
#'
#' @field profiletype the type of flow profile (steady or unsteady)
#' @field flowdf a data frame of nodeID in first column and flow profiles by name and flow values (units always in cms) in other columns

# other fields to add once we get to unsteady flow! start date, timestep, etc.

bb_flowprofile <- setRefClass("bb_flowprofile",
  field=list(profiletype="character",
             flowdf="data.frame"),
  method = list(initialize =
                  function(..., profiletype="steady",
                         flowdf=data.frame("nodeID"=1,"flowprofile1"=1))
                  {
                    callSuper(..., profiletype=profiletype,
                              flowdf=flowdf)
                  },

                check_flowprofile = function() {
                  # check that nodeID and flowvalue have consistent dimensions

                  if (any(.self$flowdf[,2:ncol(flowdf)] < 0)) {
                    warning("flow values should be positive.")
                  }
                  if ("nodeID" %notin% colnames(.self$flowdf)[1]) {
                    stop("first column in flowdf must be nodeID")
                  }
                  if (any(.self$flowdf$nodeID <= 0) | "integer" %notin% class(.self$flowdf$nodeID)){
                    stop("flowdf$nodeID must be positive and of class integer")
                  }

                  message("flow profile check completed")
                })
)

### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### BOUNDARY CONDITION CLASS ----
### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' Boundary Condition Object
#'
#' bc:
#' A reference class to create a boundary conditions at a particular location in the model.
#' Note that currently only subcritical mode with normal depth boundary condition at the downstream-most
#' cross-section in the model is supported.
#'
#' @field riverstation name of the cross-section where the boundary condition is applied (character type)
#' @field nodeID ID of streamnode
#' @field stationname name of the stream node as character
#' @field station station location where the boundary condition is applied along profile (numeric type)
#' @field reach name of the reach
#' @field location description of the boundary condition location as either "downstream", "upstream"
#' @field bctype the type of boundary condition ("normal_depth", "set_wsl", "set_depth")
#' @field bcvalue the value supplied with the boundary condition (slope for normal depth or wsl/depth accordingly)
#' @field init_WSL the initial WSL value passed to boundary condition, used in normal depth calculation
bb_boundarycondition <- setRefClass("bb_boundarycondition",

    # is the location actually needed to be specified as upstream or downstream? Can likely remove
    # xxx add nodeID here

    field=list(nodeID="integer",stationname="character",station="numeric",reach="character",location="character",
               bctype="character",bcvalue="numeric",init_WSL="numeric"),
    method = list(initialize =
                    function(..., nodeID=as.integer(-1),stationname="",station=-99,reach="",location="downstream",
                             bctype="normal_depth",bcvalue=0.001,init_WSL=-99)
                    {
                      callSuper(..., nodeID=nodeID,stationname = stationname, station = station, reach=reach, location=location,
                                bctype=bctype, bcvalue=bcvalue,init_WSL=init_WSL)
                    })
)


### ~~~~~~~~~~~~~~~~~~~
### BOUNDARY CONDITION LIST CLASS ----
### ~~~~~~~~~~~~~~~~~~~

#' Boundary Condition List Object
#'
#' bclist:
#' xxx
#' A reference class to create a geometry object from a collection of cross-sections of class xsection
#'
#' @field geomname name of the geometry object
#' @field streamnodeList a list of streamnode objects, which contains all of the cross-sections, catchments, and relevant data
bb_boundaryconditionlist <- setRefClass("bb_boundaryconditionlist", field = list(bcname = "character",
                                                       bclist = "list"),
   method = list(

     initialize =
       function(..., bcname="mybc",
                bclist=list())
       {
         callSuper(..., bcname = bcname,
                   bclist = bclist)
       },

     get_bclist_length = function() {
       return(length(.self$bclist))
     },

     get_bclist_dataframe = function() {

       lenlist <- .self$get_bclist_length()
       bcdf <- data.frame(matrix(NA,nrow=lenlist,ncol=6))

       colnames(bcdf) <- c("nodeID","reach","location","bctype","bcvalue","init_WSL")

       for (i in 1:lenlist) {
         bcdf[i,] <-   c(.self$bclist[[i]]$nodeID,
                         .self$bclist[[i]]$reach,
                         .self$bclist[[i]]$location,
                         .self$bclist[[i]]$bctype,
                         .self$bclist[[i]]$bcvalue,
                         .self$bclist[[i]]$init_WSL
         )
       }
       bcdf$nodeID <- as.integer(bcdf$nodeID)
       bcdf$bcvalue <- as.numeric(bcdf$bcvalue)
       bcdf$init_WSL <- as.numeric(bcdf$init_WSL)
       return(bcdf)
     }
                             )
)

## xxx add in methods for extracting sdf like propreties from list

### ~~~~~~~~~~~~~~~~~~
### OPTIONS CLASS ----
### ~~~~~~~~~~~~~~~~~~

#' Blackbird Options Object
#'
#' bb_options: A reference class to define the computational parameters and
#' physical constants used in the blackbird package. The parameters are defined
#' with defaults when an instance of the options object is generated, however,
#' these may be modified as needed.
#'
#' @details
#' to add to options: method silent output
#'
#' blended_nc_weights: in the order of decreasing roughness (generally):
#'  \code{c(nc_equalforce,nc_wavgarea,nc_wavgwp,nc_wavgconv,nc_equalvelocity)}
#'
#' blended_conveyance_weights: in order of decreasing conveyance:
#' \code{c(discretized_conv, roughzone_conveyance, areaweighted_conveyance)}
#'
#' @field workingfolder the workingfolder where files are stored
#' @field modeltype type of model being solved (steadyflow, unsteadyflow)
#' @field regimetype type of regime (subcritical, supercritical, mixed)
#' @field g gravitational constant (N/kg)
#' @field Froude_threshold threshold in Froude number for supercritical flow
#' @field sample_linepoints_dist distance (m) to use in sampling line points in HAND calculation
#' @field removesinks_dist distance (m) to use in removesinks algorithms
#' @field pourpoint_snap_dist distance (m) to use in snapping pour points
#' @field dx resolution for interpolating cross-section data, defined by the
#'   horizontal space between interpolated points (m)
#' @field Hseq sequence of depth values to use in preprocessing depthdf table
#'   explicitly (can be used for irregular intervals)
#' @field use_preproc whether to use pre-processed tables in computational run
#' @field use_euclidean whether to use the euclidean distance raster in preprocessing (default \code{FALSE})
#' @field interp_extraplotion_method extrapolation method to use in
#'   interpolation functions if WSL is outside of range
#' @field num_extrapolation_points number of points to use in extrapolation of hydraulic properties beyond max range
#' @field xs_use_obcalcs whether to use overbank values directly (i.e.
#'   Manning_LOB, Manning_ROB) (TRUE) or to use vector values and determine
#'   overbank-averaged values from conveyeance where possible (FALSE, default)
#' @field friction_slope_method method for calculating friction slope (default \code{average_conveyance})
#' @field xsection_conveyance_method conveyance method to use in xsection-based
#'   streamnodes
#' @field catchment_conveyance_method conveyance method to use in
#'   catchment-based streamnodes
#' @field Leff_method method of determining the effective reach length at runtime for catchment-based nodes
#' @field enforce_delta_Leff boolean whether to enforce a maximum change in the effective reach length from reach length
#' @field delta_reachlength numeric of allowable fraction change in reach length in Leff; Leff will be bounded by deviation in calculations
#' @field tolerance_cp tolerance limit for water surface calculations in
#'   compute_profile function (m)
#' @field iteration_limit_cp iteration limit for calculations between
#'   cross-sections in compute_profile
#' @field next_WSL_split_cp coefficient in assigning the next iteration of water
#'   surface elevations from the difference in previous WSL to calculated WSL
#' @field tolerance_nd tolerance limit for normal depth calculation in comparing
#'   flow to Manning's equation terms (m3/s)
#' @field iteration_limit_nd iteration limit for normal depth calculations
#' @field next_WSL_split_nd coefficient in assigning next water surface depth in
#'   normal depth calculation based on ratio of Manning's equation terms
#' @field silent_cp level of output in compute_profile function; if true,
#'   minimal output is produced
#' @field silent_nd level of output in normal_depth function; if true, minimal
#'   output is produced
#' @field max_RHSQ_ratio ratio of RHS over Q in normal depth calculation, which
#'   prevents massive swings in depth estimates
#' @field min_RHSQ_ratio ratio of RHS over Q in normal depth calculation, which
#'   prevents massive swings in depth estimates
#' @field reserve_cores number of cores to reserve when running parallel
#'   computations (default 2)
#' @field use_dhand boolean whether to use DHAND in preprocessing and
#'   postprocessing
#' @field dhand_Hseq sequence of depth values to use in dhand processing
#' @field flooded_cell_method method to use in calculating flooded cell areas
#'   and volumes ("simple","slopecorrected")
#' @field Manning_interpolation_method method to use in interpolating roughness
#'   vector (one of "constant","linear")
#' @field Manning_composite_method method to use in calculating the composite
#'   Mannings n (one of "equal_force", "equal_velocity", or "weighted_average")
#' @field Manning_enforce_values enforces Manning values in left, right and main
#'   channel components if Manning values defined as such
#'   @field catchment_integration_method method to use in integrating catchment properties
#'   into 1D form (use either "effective_length" or "reach_length")
#' @field interpolation_postproc_method method to use in post-processing depth
#'   results to raster (one of "catchment_hand", "catchment_dhand",
#'   "interp_hand", or "interp_dhand")
#' @field postproc_elev_corr_threshold threshold as fraction of depth change in elevation correction
#' @field roughness_multiplier calibration parameter on roughness that is
#'   applied in model runs (not preprocessing, default=1.0)
#' @field blended_conveyance_weights weights to be applied in blended_conveyance
#'   method
#' @field blended_nc_weights weights to be applied in blended_nc method
#' @field output_velocity boolean whether to compute velocity raster in
#'   post-processing
#' @field output_depthvelocityproduct boolean whether to compute depth-velocity
#'   product raster in post-processing
#'
bb_options <- setRefClass("bb_options",
field=list(workingfolder="character",
modeltype="character",regimetype="character",
g="numeric",
Froude_threshold="numeric",
sample_linepoints_dist="numeric",removesinks_dist="numeric",pourpoint_snap_dist="numeric",
dx="numeric",Hseq="numeric",
use_preproc="logical",
use_euclidean="logical",
interp_extraplotion_method="character",
num_extrapolation_points="numeric",
xs_use_obcalcs="logical",
friction_slope_method="character",
xsection_conveyance_method="character",catchment_conveyance_method="character",
Leff_method="character",
enforce_delta_Leff="logical",delta_reachlength="numeric",
tolerance_cp="numeric",iteration_limit_cp="numeric",next_WSL_split_cp="numeric",
tolerance_nd="numeric",iteration_limit_nd="numeric",next_WSL_split_nd="numeric",
silent_cp="logical",silent_nd="logical", max_RHSQ_ratio="numeric", min_RHSQ_ratio="numeric",
reserve_cores="numeric",use_dhand="logical",dhand_Hseq="numeric",
flooded_cell_method="character",
Manning_interpolation_method="character",
Manning_composite_method="character",Manning_enforce_values="logical",
catchment_integration_method="character",
interpolation_postproc_method="character",
postproc_elev_corr_threshold="numeric",
roughness_multiplier="numeric",
blended_conveyance_weights="numeric",blended_nc_weights="numeric",
output_velocity="logical",output_depthvelocityproduct="logical"),
method = list(initialize =
    function(..., workingfolder="",
             modeltype="steadyflow",regimetype="subcritical",
             g=9.81,
             Froude_threshold=0.94,
             sample_linepoints_dist=-1,removesinks_dist=-1,pourpoint_snap_dist=-1,
             dx=0.1,Hseq=c(0,5,10),
             use_preproc=FALSE,use_euclidean=FALSE,interp_extraplotion_method="stoponerror",
             num_extrapolation_points=20,
             xs_use_obcalcs=FALSE,
             friction_slope_method="us_friction",
             xsection_conveyance_method="default_conveyance",catchment_conveyance_method="roughzone_conveyance",
             Leff_method="us_length",
             enforce_delta_Leff=FALSE,delta_reachlength=0.3,
             tolerance_cp=0.003,iteration_limit_cp=50,next_WSL_split_cp=0.7,
             tolerance_nd=0.001,iteration_limit_nd=50,next_WSL_split_nd=0.4,
             silent_cp=FALSE,silent_nd=FALSE, max_RHSQ_ratio=2, min_RHSQ_ratio=0.5,
             reserve_cores=2,use_dhand=FALSE,dhand_Hseq=c(0,5,10),
             flooded_cell_method="slopecorrected",
             Manning_interpolation_method="linear",Manning_enforce_values=TRUE,
             catchment_integration_method="effective_length",
             Manning_composite_method="equal_force",interpolation_postproc_method="interp-hand",
             postproc_elev_corr_threshold=0.0,
             roughness_multiplier=1.0,
             blended_conveyance_weights=c(0.333,0.333,0.334),
             blended_nc_weights=rep(0.2,5),
             output_velocity=FALSE,output_depthvelocityproduct=FALSE)
    {
      callSuper(..., workingfolder=workingfolder,
                modeltype=modeltype, regimetype=regimetype,
                g=g,
                Froude_threshold=Froude_threshold,
                sample_linepoints_dist=sample_linepoints_dist,removesinks_dist=removesinks_dist,pourpoint_snap_dist=pourpoint_snap_dist,
                dx=dx,Hseq=Hseq,xs_use_obcalcs=xs_use_obcalcs,
                friction_slope_method=friction_slope_method,
                xsection_conveyance_method=xsection_conveyance_method,catchment_conveyance_method=catchment_conveyance_method,
                Leff_method=Leff_method,
                enforce_delta_Leff=enforce_delta_Leff,delta_reachlength=delta_reachlength,
                use_preproc=use_preproc,use_euclidean=use_euclidean,interp_extraplotion_method=interp_extraplotion_method,
                num_extrapolation_points=num_extrapolation_points,
                tolerance_cp=tolerance_cp,iteration_limit_cp=iteration_limit_cp,next_WSL_split_cp=next_WSL_split_cp,
                tolerance_nd=tolerance_nd,iteration_limit_nd=iteration_limit_nd,next_WSL_split_nd=next_WSL_split_nd,
                silent_cp=silent_cp,silent_nd=silent_nd,max_RHSQ_ratio=max_RHSQ_ratio, min_RHSQ_ratio=min_RHSQ_ratio,
                reserve_cores=reserve_cores,use_dhand=use_dhand,dhand_Hseq=dhand_Hseq,
                flooded_cell_method=flooded_cell_method,
                Manning_interpolation_method=Manning_interpolation_method,
                Manning_composite_method=Manning_composite_method,Manning_enforce_values=Manning_enforce_values,
                catchment_integration_method=catchment_integration_method,
                interpolation_postproc_method=interpolation_postproc_method,
                postproc_elev_corr_threshold=postproc_elev_corr_threshold,
                roughness_multiplier=roughness_multiplier,
                blended_conveyance_weights=blended_conveyance_weights,
                blended_nc_weights=blended_nc_weights,
                output_velocity=output_velocity,output_depthvelocityproduct=output_depthvelocityproduct)
    },

  check_options = function() {

    message("Start of bboptions check ---- ")

    # warning that velocity still needed if depth-velocity requested
    if (!.self$output_velocity & .self$output_depthvelocityproduct) {
      message("Will not output velocity in postprocessing but velocity will still be computed as part of the depth-velocity product.")
    }

    # check for consistency with areaweighted_conveyance and manning composite method
    if (.self$xsection_conveyance_method == "areaweighted_conveyance" & .self$Manning_composite_method != "weighted_average_area") {
      warning("conveyance method 'areaweighted_conveyance' should be used with the Manning composite method 'weighted_average_area' for consistency.")
    }

    if (.self$catchment_conveyance_method != "areaweighted_conveyance") {
      warning("The 'areaweighted_conveyance' method is strongly recommended, please see supporting blackbird literature.")
    }

    if (.self$roughness_multiplier <=0 | is.na(.self$roughness_multiplier) | is.null(.self$roughness_multiplier)) {
      warning("bbopt$bbopt$roughness_multiplier must be a positive value.")
    }
    if (.self$roughness_multiplier > 100) {
      warning("bbopt$roughness_multiplier value is extremely high (>100), please check.")
    }

    # check that dy dx dothers >0

    # want to ensure that snap dist is smaller than sample distance
    if (.self$sample_linepoints_dist < .self$pourpoint_snap_dist) {
      warning("Ideally want to have the snap distance less than sample distance (want bbopt$sample_linepoints_dist > bbopt$pourpoint_snap_dist)")
    }

    message("End of bboptions check ---- ")
  },

  get_defaults = function() {

    # message("Start of get_defaults ---- ")
    demres <- bb_get_demres(.self)

    # xxx to do
    # add options to bb_options and update here

    ## defaults for distances in wbt algorithms

    .self$sample_linepoints_dist <- demres[1]*3
    .self$removesinks_dist <- demres[1]*5
    .self$pourpoint_snap_dist <-  demres[1]*3

    message("bbopt defaults set successfully! ~~~~~~")
  })
)

# to add xxx - add automatic option for calculating distance offset defaults
# add distance offset defaults


### ~~~~~~~~~~~~~~~~~~
### MODEL CLASS ----
### ~~~~~~~~~~~~~~~~~~

#' Blackbird Model Object
#'
#' bb_model:
#' A reference class to define the overall model in Blackbird. Contains the model options, geometry, and flows.
#'
#' @field bbopt a Blackbird options object
#' @field bbgeo a Blackbird geometry object
#' @field bbfp a Blackbird flow profile object
#' @field bbbc a boundary conditions list class object
bb_model <- setRefClass("bb_model",
  contains=c("sf"),
  field=list(bbopt="bb_options",
             bbgeo="bb_geometry",
             bbfp="bb_flowprofile",
             bbbc="bb_boundaryconditionlist"
             ),
  method = list(initialize =
                  function(..., bbopt=bb_options(),bbgeo=bb_geometry(),
                           bbfp=bb_flowprofile(),bbbc=list(bb_boundarycondition())){
                    callSuper(..., bbopt=bbopt,bbgeo=bbgeo,bbfp=bbfp,bbbc=bbbc)
                  })
)
