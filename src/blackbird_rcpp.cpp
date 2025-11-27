#include <Rcpp.h>
using namespace Rcpp;

//' Returns value if finite, 0 if not
//' @param value The value to be considered.
//' @return value if finite, 0 if not.
//' @noRd
// [[Rcpp::export]]
NumericVector cpp_finite_or_zero(NumericVector value) {
  return is_finite(value)[0] ? value : NumericVector::create(0.0);
}

//' Returns value if finite, 1 if not
//' @param value The value to be considered.
//' @return value if finite, 1 if not.
//' @noRd
// [[Rcpp::export]]
NumericVector cpp_finite_or_one(NumericVector value) {
 return is_finite(value)[0] ? value : NumericVector::create(1.0);
}

//' Utility function to extract row from NumericMatrix
 //' @param x NumericMatrix
 //' @param y indicating yth row to extract
 //' @return NumericVector of the yth row
 //' @noRd
 // [[Rcpp::export]]
 NumericVector getRowNM(NumericMatrix x, int y) {
   int cols = x.ncol(); // Get the number of columns
   NumericVector row(cols); // Create a vector to store the row

   for (int j = 0; j < cols; j++) {
     row[j] = x(y, j); // Extract the yth row
   }

   return row;
 }

//' Utility function to extract row from IntegerMatrix
//' @param x IntegerMatrix
//' @param y indicating yth row to extract
//' @return IntegerVector of the yth row
//' @noRd
// [[Rcpp::export]]
IntegerVector getRowIM(IntegerMatrix x, int y) {
 int cols = x.ncol(); // Get the number of columns
 IntegerVector row(cols); // Create a vector to store the row

 for (int j = 0; j < cols; j++) {
   row[j] = x(y, j); // Extract the yth row
 }

 return row;
}

// [[Rcpp::export]]
IntegerVector dropValues(IntegerVector x, IntegerVector valuesToRemove) {
  IntegerVector result;

  for (int i = 0; i < x.size(); i++) {
    if (is_true(all(x[i] != valuesToRemove))) {
      result.push_back(x[i]); // Keep elements not in valuesToRemove
    }
  }

  return result;
}


// [[Rcpp::export]]
IntegerVector drop_first(IntegerVector x) {
  return IntegerVector(x.begin() + 1, x.end());
}


// [[Rcpp::export]]
bool containsIV(IntegerVector vec, int value) {
  return std::find(vec.begin(), vec.end(), value) != vec.end();
}


// [[Rcpp::export]]
IntegerVector getIndicesGreaterThanZero(NumericVector vec) {
  IntegerVector indices;

  for (int i = 0; i < vec.size(); i++) {
    if (vec[i] > 0) {
      indices.push_back(i); // Zero-based indexing
    }
  }

  return indices;
}

// [[Rcpp::export]]
IntegerVector getCommonElements(IntegerVector vec1, IntegerVector vec2) {
  std::unordered_set<int> set1(vec1.begin(), vec1.end());
  IntegerVector common;

  for (int val : vec2) {
    if (set1.find(val) != set1.end()) {
      common.push_back(val);
    }
  }

  return common;
}

// [[Rcpp::export]]
int minInt(int a, int b) {
  return (a < b) ? a : b; // Returns the smaller of the two
}

// [[Rcpp::export]]
int getPositionIV(IntegerVector vec, int value) {
  IntegerVector::iterator it = std::find(vec.begin(), vec.end(), value);

  if (it != vec.end()) {
    return std::distance(vec.begin(), it); // Returns position (zero-based index)
  } else {
    return -1; // Returns -1 if value not found
  }
}

// [[Rcpp::export]]
IntegerVector getIndicesNotEqual(IntegerVector vec, int value) {
  IntegerVector indices; // Store matching indices

  for (int i = 0; i < vec.size(); i++) {
    if (vec[i] != value) {
      indices.push_back(i); // Zero-based indexing
    }
  }

  return indices;
}


// [[Rcpp::export]]
NumericVector sumNumericVectors(NumericVector v1, NumericVector v2) {
  if (v1.size() != v2.size()) {
    stop("Vectors must be of the same length");
  }

  return v1 + v2; // Element-wise addition
}


// pads with NA values, and stores in specific rowIndex, and truncates if values is too long
// [[Rcpp::export]]
NumericMatrix storeRowElements(NumericMatrix mat, int rowIndex, NumericVector values) {
  int cols = mat.ncol(); // Get the number of columns

  if (rowIndex < 0 || rowIndex >= mat.nrow()) {
    stop("Invalid row index");
  }

  // Ensure correct length: truncate if too long, pad with NA if too short
  NumericVector adjustedValues(cols, NA_REAL); // Initialize with NA values
  int fillSize = minInt(values.size(), cols); // Determine valid range

  for (int j = 0; j < fillSize; j++) {
    adjustedValues[j] = values[j]; // Copy elements
  }

  // Store values in the specified row of the matrix
  for (int j = 0; j < cols; j++) {
    mat(rowIndex, j) = adjustedValues[j];
  }

  return mat;
}

// [[Rcpp::export]]
IntegerMatrix storeRow(IntegerMatrix mat, IntegerVector vec, int rowIndex) {
  int cols = mat.ncol();
  if (rowIndex >= mat.nrow() || rowIndex < 0) {
    stop("Invalid row index.");
  }

  // Rcout << "ready to start padding vector"<< std::endl;

  // Ensure the vector is padded if needed
  IntegerVector paddedVec = clone(vec);
  while (paddedVec.size() < cols) {
    paddedVec.push_back(-1);  // Add -1 padding
  }

  // Rcout << "done padding, now assigning"<< std::endl;

  for (int j = 0; j < cols; j++) {
    mat(rowIndex, j) = paddedVec[j];  // Assign values from NumericVector to IntegerMatrix row
  }

  return mat;
}

//' Minimum elevation of a set of catchments
//' @param catchment The matrix form of a catchment raster.
//' @param hand The matrix form of a hand raster.
//' @param dem The matrix form of a dem raster.
//' @param subset A subset of catchment nodeIDs to calculate for (nullable).
//' @return A named vector of catchment minimum elevations.
//' @noRd
// [[Rcpp::export]]
NumericVector cpp_min_elev(IntegerVector catchment, NumericVector hand, NumericVector dem, Nullable<IntegerVector> subset) {
  // Grab given subset or unique catchments
  IntegerVector uni;
  if(subset.isNull()) {
    uni = setdiff(catchment, IntegerVector::create(NA_INTEGER));
  } else {
    uni = clone(subset);
  }

  // Filter out all values that are not associated with a catchment
  dem = dem[in(catchment, uni)];
  hand = hand[in(catchment, uni)];
  catchment = catchment[in(catchment, uni)];

  // Initialize data structures for holding catchment data
  NumericVector totals(uni.length(), 0);
  IntegerVector counts(uni.length(), 0);
  totals.names() = uni;
  counts.names() = uni;

  // Calculate the total elevation of the catchment at a hand value of 0 and
  // the number of pixels that fall under those conditions to get the average.
  for(int i = 0; i < catchment.length(); i++) {
    int catch_num = catchment[i];
    if(hand[i] == 0) {
      totals[std::to_string(catch_num)] = as<double>(totals[std::to_string(catch_num)]) + dem[i];
      counts[std::to_string(catch_num)] = as<int>(counts[std::to_string(catch_num)]) + 1;
    }
  }
  NumericVector return_elevs = totals/(NumericVector)counts;
  return_elevs.names() = uni;

  return return_elevs;
}

//' Slope of a set of catchments
 //' @param catchment The matrix form of a catchment raster.
 //' @param hand The matrix form of a hand raster.
 //' @param slope The matrix form of a slope raster.
 //' @param subset A subset of catchment nodeIDs to calculate for (nullable).
 //' @return A named vector of catchment bed slopes.
 //' @noRd
 // [[Rcpp::export]]
 NumericVector cpp_bed_slope(IntegerVector catchment, NumericVector hand, NumericVector slope, Nullable<IntegerVector> subset) {
   // Grab given subset or unique catchments
   IntegerVector uni;
   if(subset.isNull()) {
     uni = setdiff(catchment, IntegerVector::create(NA_INTEGER));
   } else {
     uni = clone(subset);
   }

   // Filter out all values that are not associated with a catchment
   slope = slope[in(catchment, uni)];
   hand = hand[in(catchment, uni)];
   catchment = catchment[in(catchment, uni)];

   // Initialize data structures for holding catchment data
   NumericVector totals(uni.length(), 0);
   IntegerVector counts(uni.length(), 0);
   totals.names() = uni;
   counts.names() = uni;

   // Calculate the total slope of the catchment at a hand value of 0 and
   // the number of pixels that fall under those conditions to get the average.
   for(int i = 0; i < catchment.length(); i++) {
     int catch_num = catchment[i];
     if(hand[i] == 0) {
       totals[std::to_string(catch_num)] = as<double>(totals[std::to_string(catch_num)]) + slope[i];
       counts[std::to_string(catch_num)] = as<int>(counts[std::to_string(catch_num)]) + 1;
     }
   }
   NumericVector return_slopes = totals/(NumericVector)counts;
   return_slopes.names() = uni;

   return return_slopes;
 }


//' Preprocessing tables of a set of catchments
//' @param a The dimensions of the dem.
//' @param reachshp_lens An array of lengths of the reach shape object.
//' @param seqH An array of heights to be processed.
//' @param uni An array of unique catchments to process.
//' @param catchment The matrix form of a catchment raster.
//' @param dem The matrix form of a dem raster.
//' @param reghand The matrix form of a regular hand raster.
//' @param dhands The DataFrame of interpolated hand matrices.
//' @param manningsn The matrix form of a manningsn raster.
//' @param reachlength The matrix form of a reachlength raster.
//' @param slope The matrix form of a slope raster.
//' @param preprocessing_tables A list of existing preprocessing tables.
//' @param nodeIDs An array of nodeIDs corresponding to each entry of preprocessing_tables.
//' @param catchment_conveyance_method String containing the catchment conveyance method to be used.
//' @param manning_composite_method String containing the manning composite method to be used.
//' @param catchment_integration_method String containing the method for catchment integration (reach length or Leff).
//' @param use_dhand Logical whether to use DHAND (otherwise uses HAND).
//'
//' @return A list of updated preprocessing tables.
//' @noRd
// [[Rcpp::export]]
void cpp_preprocessing_tables_old(double a, NumericVector reachshp_lens, NumericVector reachshp_lens2, NumericVector seqH, IntegerVector uni, IntegerVector catchment, NumericVector dem, NumericVector reghand, NumericMatrix dhands,
                                   NumericVector manningsn, NumericVector reachlength, NumericVector slope, List preprocessing_tables, IntegerVector nodeIDs, String flooded_cell_method,
                                   String catchment_conveyance_method, String manning_composite_method, String catchment_integration_method, LogicalVector use_dhand) {


  // std::cout << "starting cpp code "<< "\n";


  // one of reghand or dhands must be provided
  /*
  if(reghand.isNull() && dhands.isNull()) {
    stop("Either reghand or dhands must be provided.");
  }
  */


  // If filtering within rcpp, uncomment the following block and change the parameter "IntegerVector uni" to "Nullable<IntegerVector> subset"
  /*
   IntegerVector uni;
   if(subset.isNull()) {
   uni = nodeIDs;
   } else {
   uni = clone(subset);
   }
   dem = dem[in(catchment, uni)];
   if(!reghand.isNull()) {
   reghand = reghand[in(catchment, uni)];
   } else {
   for(int i = 0; i < dhands.length(); i++) {
   dhands[i] = dhands[i][in(catchment, uni)];
   }
   }
   manningsn = manningsn[in(catchment, uni)];
   reachlength = reachlength[in(catchment, uni)];
   slope = slope[in(catchment, uni)];
   catchment = catchment[in(catchment, uni)];
   */


  // std::cout << "starting j loop "<< "\n";

  for(int j = 0; j < seqH.length(); j++) {
    double d = seqH[j];
    NumericVector hand;
    if(use_dhand[0]) { //use_dhand==TRUE
      //hand = as<NumericVector>(as<List>(dhands)[j]); // xxx this might be the problematic line!
      //hand = as<NumericVector>(dhands(j,_)); // xxx this might be the problematic line!
      hand = dhands(j,_);
    } else {
      hand = reghand;
    }
    // std::cout << "hand selected appropriately for depth of "<< std::to_string(d) << "\n";

    // std::cout << "dim of hand is " << std::to_string(hand.length())  << "\n";


    // std::cout << "a is " << std::to_string(a) << " and dim of slope is " << std::to_string(slope.length())  << "\n";
    // std::cout << "a is " << std::to_string(a) << " and dim of reghand is " << std::to_string(reghand.length())  << "\n";
    //std::cout << "a is " << std::to_string(a) << " and dim of slope is " << std::to_string(slope.length())  << "\n";


    // calculate areas and volumes of flooded cells with simple approach (flat cells, no slope correction)
    NumericVector Ai_rr = pow(a,2.0)+ slope*0;   // area of each cell, simply a^2


    // std::cout << "finished Ai_rr calc"<< "\n";

    NumericVector di_rr = pmax(d - hand,0);       // depth in cell
    // flooded area of each cell
    NumericVector Aif_rr = ifelse(di_rr <= 0,
                                  0, // no water
                                  Ai_rr);
    NumericVector Atif_rr = Aif_rr; // top area of each cell, a^2 if inundated else 0
    NumericVector Vif_rr = pow(a,2.0) * di_rr; // flooded volume in each cell
    NumericVector yy = a*tan(slope);     // cell height

    // std::cout << "finished yy calc"<< "\n";

    NumericVector handmin = hand-0.5*yy; // bottom hand value accounting for slope


    // std::cout << "finished handmin calc"<< "\n";

    NumericVector handmax = hand+0.5*yy; // top hand value accounting for slope

    //  std::cout << "finished handmax calc"<< "\n";


    if (flooded_cell_method == "slopecorrected") {

      // correct slope to zero where handmin <0
      slope = ifelse(handmin>0,slope,0);

      // redo calcs
      yy = a*tan(slope);     // cell height
      handmin = hand-0.5*yy; // bottom hand value accounting for slope
      handmax = hand+0.5*yy; // top hand value accounting for slope

      Ai_rr = pow(a,2.0)*sqrt(1+pow(atan(slope),2.0)); // slope corrected area

      di_rr = pmax(d-handmin,0); // cell depth relative to bottom, handmin

      // flooded area of each cell (akin to wetted perimeter)
      Aif_rr = ifelse(di_rr <= 0,
                      0, // no water
                      ifelse(di_rr >= yy,
                             Ai_rr, // fully inundated
                             Ai_rr*(di_rr/yy) ) // partially inundated, take as ratio of depth/yy
                        );

      // top area of each cell (akin to top width)
      Atif_rr = ifelse(di_rr <= 0,
                       0, // no water
                       ifelse(di_rr >= yy,
                              pow(a,2.0)+ slope*0, // fully inundated
                              pow(a,2.0)*(di_rr/yy) ) // partially inundated, take as ratio of depth/yy
      );

      // flooded volume in each cell (akin to cross-sectional area)
      Vif_rr = ifelse(di_rr <= 0,
                        0, // no water
                        ifelse(di_rr >= yy,
                               0.5*pow(a,2.0)*yy + (di_rr-yy)*pow(a,2.0), // fully inundated, xxx correct in a moment
                               0.5*pow(a,2.0)*di_rr ) // partially inundated, take as ratio of depth/yy, xxx correct soon
      );

    }

    //  std::cout << "finished slopecorr calc"<< "\n";

    // ---- hydraulic properties from rasters ----
    // NumericVector Pi_rr = Aif_rr / a; // wetted perimeter in each cell
    NumericVector Rhi_rr = Vif_rr / Aif_rr; // hydraulic radius in each cell
    NumericVector Ki_rr = (1/manningsn)*Vif_rr*pow(Rhi_rr, 2.0/3.0); // conveyance in each cell
    NumericVector Vfi_ni_rr = Vif_rr/manningsn; // flooded volume/manningsn in each cell
    NumericVector Leff_K_prod = Ki_rr * reachlength; // reachlength = Li
    //NumericVector Vif3_Ki2_ratio = pow(Vif_rr/a, 3.0) / pow(Ki_rr, 2.0);
    NumericVector Ki3_Vif2_ratio = pow(Ki_rr, 3.0) / pow(Vif_rr/a, 2.0);

    //Vif3_Ki2_ratio[Vif3_Ki2_ratio ==0] = NA_REAL;

    for(int k = 0; k < nodeIDs.length(); k++) {
      // Get nodeID and skip if not in subset if applicable
      int nodeID = nodeIDs[k];
     // std::cout << "In loop, k=" << std::to_string(k) << " and nodeID is " << std::to_string(nodeID) << "\n";
      /*if(is_false(all(in(IntegerVector::create(nodeID), uni)))) {
        continue;
      }
       */
      // Filter matrices for catchment
      NumericVector chand = hand[catchment == nodeID];
      NumericVector cmanningsn = manningsn[catchment == nodeID];
      NumericVector cAi_rr = Ai_rr[catchment == nodeID];
      NumericVector cAif_rr = Aif_rr[catchment == nodeID];
      NumericVector cAtif_rr = Atif_rr[catchment == nodeID];
      NumericVector cVif_rr = Vif_rr[catchment == nodeID];
      NumericVector cKi_rr = Ki_rr[catchment == nodeID];
      NumericVector cVfi_ni_rr = Vfi_ni_rr[catchment == nodeID];
      NumericVector cLeff_K_prod = Leff_K_prod[catchment == nodeID];
      // NumericVector cVif3_Ki2_ratio = Vif3_Ki2_ratio[catchment == nodeID];
      NumericVector cKi3_Vif2_ratio = Ki3_Vif2_ratio[catchment == nodeID];
      //std::cout << "cKi3_Vif2_ratio is ok" << "\n";

      // Totals for each catchment
      NumericVector Af = NumericVector::create( sum(na_omit(cAif_rr)) ); // total flooded area, m2
      NumericVector Atf = NumericVector::create( sum(na_omit(cAtif_rr)) ); // total flooded top area, m2
      NumericVector Vf = NumericVector::create( sum(na_omit(cVif_rr)) ); // total flooded volume, m3
      NumericVector Kisum = NumericVector::create( sum(na_omit(cKi_rr)) ); // total conveyance, m3/s
      NumericVector K = NumericVector::create(0.0);
      NumericVector alpha =NumericVector::create(1.0);

      // calculate effective reach length (conveyance-weighted only)
      NumericVector Leff = ifelse(LogicalVector::create(d == 0), reachshp_lens[k], (1/Kisum)*sum(na_omit(cLeff_K_prod)));
      // override effective reach length calculation at junctions
      if (reachshp_lens2[k] != -99) {
        Leff = reachshp_lens2[k];
      }

      // new ones to stash results for all methods
      NumericVector K_Total_areaconv = NumericVector::create(0.0);
      NumericVector K_Total_disconv = NumericVector::create(0.0);
      NumericVector K_Total_roughconv = NumericVector::create(0.0);
      NumericVector alpha_areaconv = NumericVector::create(0.0);
      NumericVector alpha_disconv = NumericVector::create(0.0);
      NumericVector alpha_roughconv = NumericVector::create(0.0);
      NumericVector nc_equalforce = NumericVector::create(0.0);
      NumericVector nc_equalvelocity = NumericVector::create(0.0);
      NumericVector nc_wavgwp = NumericVector::create(0.0);
      NumericVector nc_wavgarea = NumericVector::create(0.0);
      NumericVector nc_wavgconv = NumericVector::create(0.0);
      //std::cout << "catchment totals ok" << "\n";

      // if (catchment_conveyance_method == "discretized_conveyance") {
        // do nothing, use K as computed above and summed in each cell. Just compute alpha
        NumericVector sum_Vf_a = Vf/a;
        K_Total_disconv = Kisum;
        alpha_disconv = cpp_finite_or_one((pow(sum_Vf_a,2.0) / pow(K_Total_disconv,3.0)) * sum(as<NumericVector>(na_omit(cKi3_Vif2_ratio))));

      // } else if (catchment_conveyance_method == "areaweighted_conveyance_onecalc") {
        K_Total_areaconv = NumericVector::create( sum(na_omit(cVfi_ni_rr)) )*pow(Vf, 2.0/3.0) / pow(Af, 2.0/3.0); // total conveyance, m3/s
        // NumericVector alpha =NumericVector::create(1.0); // alpha set to one by definition in method
        alpha_areaconv = 1.0;

      // } else if (catchment_conveyance_method == "roughzone_conveyance") {
        // get unique roughness values in catchment, sum conveyance there with areaweighted approach
        NumericVector uniqueManningsn = unique(cmanningsn);
        K = 0.0;
        alpha = 0.0;
        // std::cout << "length of uniqueManningsn is " << std::to_string(uniqueManningsn.length()) << "\n";
        for(int kk = 0; kk < uniqueManningsn.length(); kk++) {
          // kkmanningsn = manningsn[cmanningsn == uniqueManningsn[kk]];
          // logical subet that meets the criteria of
          LogicalVector res1 = manningsn == uniqueManningsn[kk];
          LogicalVector res2 = catchment == nodeID;
          LogicalVector res3 = res1 & res2;
          // NumericVector kkmanningsn = manningsn[manningsn == uniqueManningsn[kk] && catchment == nodeID];
          NumericVector kkmanningsn = manningsn[res3];
          NumericVector kkAif_rr = Aif_rr[res3];
          NumericVector kkVif_rr = Vif_rr[res3];
          NumericVector kkVfi_ni_rr = Vfi_ni_rr[res3];
          NumericVector kkAf = NumericVector::create( sum(na_omit(kkAif_rr)) ); // total flooded area, m2
          NumericVector kkVf = NumericVector::create( sum(na_omit(kkVif_rr)) ); // total flooded volume, m3
          NumericVector Ki = cpp_finite_or_zero(NumericVector::create( sum(na_omit(kkVfi_ni_rr)) )*pow(kkVf, 2.0/3.0) / pow(kkAf, 2.0/3.0));
          K += Ki;
          alpha += cpp_finite_or_zero(pow(Ki,3.0)/pow(kkVf,2.0));
        }
        alpha =cpp_finite_or_one(alpha*pow(Vf,2.0)/pow(K, 3.0));
        K_Total_roughconv = K;
        alpha_roughconv = alpha;

        if (catchment_conveyance_method == "discretized_conveyance") {
          K = K_Total_disconv;
          alpha = alpha_disconv;
        } else if (catchment_conveyance_method == "areaweighted_conveyance_onecalc") {
          K = K_Total_areaconv;
          alpha = alpha_areaconv;
        } else if (catchment_conveyance_method == "roughzone_conveyance") {
          K = K_Total_roughconv;
          alpha = alpha_roughconv;
        }

      // }
      // std::cout << "conveyance calculations are ok" << "\n";


      // hydraulic properties for 1D ----
      // normalized by effective reach length Leff (varies with stage) to normalize when in dividing to get 1D properties
      NumericVector A1D = cpp_finite_or_zero(Vf / Leff);  // equivalent cross-sectional area
      NumericVector P1D = cpp_finite_or_zero(Af / Leff);  // equivalent wetted perimeter
      NumericVector Rh1D = cpp_finite_or_zero(A1D / P1D); // equivalent hydraulic radius
      NumericVector K1D = cpp_finite_or_zero(K / Leff);   // equivalent conveyance
      NumericVector T1D = cpp_finite_or_zero(Atf / Leff); // equivalent top width
      NumericVector HyD1D = cpp_finite_or_zero(A1D / T1D);// equivalent hydraulic radius
      NumericVector K_roughzone_1D = cpp_finite_or_zero(K_Total_roughconv / Leff);    // equivalent conveyance from roughness zone conveyance
      NumericVector K_disc_1D = cpp_finite_or_zero(K_Total_disconv / Leff);           // equivalent conveyance from discretized conveyance
      NumericVector K_areaweighted_1D = cpp_finite_or_zero(K_Total_areaconv / Leff);  // equivalent conveyance from area weighted conveyance

      // // only normalize by Leff here
      // reverse in computation if needed
      // if (catchment_integration_method == "effective_length") {
      //   // continue
      // } else if (catchment_integration_method == "reach_length") {
      //   // normalized by physical reachlength (static property) in dividing to get 1D properties
      //   A1D = cpp_finite_or_zero(Vf / reachshp_lens[k]);  // equivalent cross-sectional area
      //   P1D = cpp_finite_or_zero(Af / reachshp_lens[k]);  // equivalent wetted perimeter
      //   Rh1D = cpp_finite_or_zero(A1D / P1D);             // equivalent hydraulic radius
      //   K1D = cpp_finite_or_zero(K / reachshp_lens[k]);   // equivalent conveyance
      //   T1D = cpp_finite_or_zero(Atf / reachshp_lens[k]); // equivalent top width
      //   HyD1D = cpp_finite_or_zero(A1D / T1D);            // equivalent hydraulic radius
      //   K_roughzone_1D = cpp_finite_or_zero(K_Total_roughconv / reachshp_lens[k]);    // equivalent conveyance from roughness zone conveyance
      //   K_disc_1D = cpp_finite_or_zero(K_Total_disconv / reachshp_lens[k]);           // equivalent conveyance from discretized conveyance
      //   K_areaweighted_1D = cpp_finite_or_zero(K_Total_areaconv / reachshp_lens[k]);  // equivalent conveyance from area weighted conveyance
      // } else {
      //   stop("Catchment integration method not recognized");
      // }


      // std::cout << "1D vector totals ok" << "\n";

      // alpha check
      if (alpha[0] < 1.0 || alpha[0] > 5) {
        warning("alpha computed as %2g for catchment %i, should be bounded by approx [1.0, 5.0]. Alpha set to boundary value.",alpha,nodeID); // may need to change %2g to %f
        if (alpha[0] < 1.0) {
          alpha[0] = 1.0;
        } else if (alpha[0] > 5.0) {
          alpha[0] = 5.0;
        }
      }

      // composite mannings n check
      NumericVector nc = NumericVector::create(0);

      if (Af[0] == 0 || d == 0) {
        // below results in an error if land cover does not completely cover dem
        // take area-weighted average where hand==0 (will have no wet cells to compute with zero depth otherwise)
        NumericVector cAi_ni_prod = cAi_rr*cmanningsn;
        sum(as<NumericVector>(na_omit(cAi_rr)[chand <= 0]));
        nc = cpp_finite_or_zero( sum(as<NumericVector>(na_omit(cAi_ni_prod)[chand <= 0])) / NumericVector::create(sum(as<NumericVector>(na_omit(cAi_rr)[chand <= 0]))) );
        nc_equalforce = nc;
        nc_equalvelocity = nc;
        nc_wavgwp = nc;
        nc_wavgarea = nc;
        nc_wavgconv = nc;
      } else {

        // could add more methods here, such as equal force but based on Vf not Af xxx

        // note that we use Vf in place of area when using catchment-based versions of equations
        // and Af in place of perimeter
        //if (manning_composite_method == "equal_force") {
          // equivalent method - total force equation
          NumericVector cAif_ni_prod_eqf = cAif_rr * pow(cmanningsn,2.0);
          nc_equalforce = cpp_finite_or_zero(sqrt(sum(na_omit(cAif_ni_prod_eqf))/Af) );

        //} else if (manning_composite_method == "weighted_average_area") {
          // simple area-weighted average
          // uses volume in catchment approach
          NumericVector cVif_ni_prod_waa = cVif_rr * cmanningsn;
          nc_wavgarea = cpp_finite_or_zero(sum(na_omit(cVif_ni_prod_waa)) / Vf);

        //} else if (manning_composite_method == "weighted_average_wetperimeter") {
          // simple perimeter-weighted average
          // uses area in catchment approach
          NumericVector cAif_ni_prod = cAif_rr * cmanningsn;
          nc_wavgwp = cpp_finite_or_zero(sum(na_omit(cAif_ni_prod)) / Af);

        //} else if (manning_composite_method == "weighted_average_conveyance") {
          // simple area-weighted average
          NumericVector cKi_ni_prod = cKi_rr * cmanningsn;
          nc_wavgconv = cpp_finite_or_zero(sum(na_omit(cKi_ni_prod)) / Kisum);

        //} else if (manning_composite_method == "equal_velocity") {
          NumericVector cAif_ni_prod_eqvel = cAif_rr * pow(cmanningsn, 1.5);
          nc_equalvelocity = cpp_finite_or_zero( pow(sum(na_omit(cAif_ni_prod_eqvel)) / Af, 2.0/3.0) );
        //}

        if (manning_composite_method == "equal_force") {
          nc = nc_equalforce;
        } else if (manning_composite_method == "weighted_average_area") {
          nc = nc_wavgarea;
        } else if (manning_composite_method == "weighted_average_wetperimeter") {
          nc = nc_wavgwp;
        } else if (manning_composite_method == "weighted_average_conveyance") {
          nc = nc_wavgconv;
        } else if (manning_composite_method == "equal_velocity") {
          nc = nc_equalvelocity;
        } else {
          warning("Unrecognized bbopt - Manning_composite_method. Using default (equal_force)");
          nc = nc_equalforce;
        }
      }
      // std::cout << "nc calc ok" << "\n";

      // reality check on nc just in case
      double mn_min = min(na_omit(cmanningsn));
      double mn_max = max(na_omit(cmanningsn));
      if (nc[0] < mn_min || nc[0] > mn_max ){
        warning("composite Mannings n computed as %2g for catchment %i, should be bounded by landuse raster min and max values [%2g, %2g].",
                        nc,nodeID,mn_min,mn_max);
        /*if (nc[0] < mn_min) {
          nc[0] = mn_min;
        }
        if (nc[0] > mn_max) {
          nc[0] = mn_max;
        }
         */
      }
      // std::cout << "nc check ok" << "\n";


      // assign values to depthdf ----
      // need to update list assigned to numeric 0 value in classes function in order to allow assignment here
      // std::cout << "made all the way to assigning variables" << "\n";
      DataFrame tempdf = as<DataFrame>(preprocessing_tables[k]);
      // std::cout << "A1D is " << std::to_string(A1D[0]) << " and j is " << std::to_string(j) << " and k is " << std::to_string(k) << "\n";
      as<NumericVector>(tempdf["Area"])[j] = A1D[0];
      as<NumericVector>(tempdf["WetPerimeter"])[j] = P1D[0];
      as<NumericVector>(tempdf["HRadius"])[j] = Rh1D[0];
      as<NumericVector>(tempdf["K_Total"])[j] = K1D[0];
      as<NumericVector>(tempdf["alpha"])[j] = alpha[0];
      as<NumericVector>(tempdf["Manning_Composite"])[j] = nc[0];
      as<NumericVector>(tempdf["Length_Effective"])[j] = Leff[0];
      as<NumericVector>(tempdf["TopWidth"])[j] = T1D[0];
      as<NumericVector>(tempdf["HydDepth"])[j] = HyD1D[0];
      as<NumericVector>(tempdf["K_Total_areaconv"])[j] = K_areaweighted_1D[0];
      as<NumericVector>(tempdf["K_Total_disconv"])[j] = K_disc_1D[0];
      as<NumericVector>(tempdf["K_Total_roughconv"])[j] = K_roughzone_1D[0];
      as<NumericVector>(tempdf["alpha_areaconv"])[j] = alpha_areaconv[0];
      as<NumericVector>(tempdf["alpha_disconv"])[j] = alpha_disconv[0];
      as<NumericVector>(tempdf["alpha_roughconv"])[j] = alpha_roughconv[0];
      as<NumericVector>(tempdf["nc_equalforce"])[j] = nc_equalforce[0];
      as<NumericVector>(tempdf["nc_equalvelocity"])[j] = nc_equalvelocity[0];
      as<NumericVector>(tempdf["nc_wavgwp"])[j] = nc_wavgwp[0];
      as<NumericVector>(tempdf["nc_wavgarea"])[j] = nc_wavgarea[0];
      as<NumericVector>(tempdf["nc_wavgconv"])[j] = nc_wavgconv[0];
      // std::cout << "done assigning" << "\n";
    }
  }
}

//' Postprocesses depths using different methods.
   //' @param postproc_method String containing the post processing method to be used.
   //' @param hand The matrix form of a hand raster.
   //' @param catchmentdepth The matrix form of a catchmentdepth raster.
   //' @param handid The matrix form of a hand pour point ID raster.
   //' @param spp The snapped pour points as a DataFrame
   //' @return A raster of depths in matrix form.
   //' @noRd
   // [[Rcpp::export]]
NumericMatrix cpp_postprocess_depth(String postproc_method, NumericMatrix hand, Nullable<NumericMatrix> catchmentdepth_ = R_NilValue, Nullable<IntegerMatrix> handid_ = R_NilValue,
                                    Nullable<DataFrame> spp_ = R_NilValue) {
  // Initialize various matrices
  NumericMatrix catchmentdepth;
  IntegerMatrix handid;
  DataFrame spp;
  NumericVector depths;
  if(postproc_method == "catchment-hand") {
    if(catchmentdepth_.isNull()) {
      stop("Error: catchmentdepth_ must be provided for postproc_method 'catchment-hand'");
    } else {
      // If method is catchment-hand then catchmentdepth is provided as an input
      catchmentdepth = as<NumericMatrix>(catchmentdepth_);
    }
  } else if(postproc_method == "interp-hand") {
    if(spp_.isNull()) {
      stop("Error: spp_ must be provided for postproc_method 'interp-hand'");
    } else if(handid_.isNull()) {
      stop("Error: handid_ must be provided for postproc_method 'interp-hand'");
    } else {
      // If method is interp-hand then catchmentdepth is built from handid and spp
      handid = as<IntegerMatrix>(handid_);
      spp = as<DataFrame>(spp_);
      depths = as<NumericVector>(spp["depth"]);
    }
  }

  // Initialize matrix to hold resulting depths
  NumericMatrix res(hand.nrow(), hand.ncol());
  // For each cell in hand, assign a corresponding depth value to res
  NumericVector cd(1);
  for(int i = 0; i < hand.nrow()*hand.ncol(); i++) {
    if(postproc_method == "catchment-hand") {
      if(!NumericMatrix::is_na(catchmentdepth[i]) && !NumericMatrix::is_na(hand[i]) && catchmentdepth[i] >= hand[i]){
        //Rcout << "res real\n";
        res[i] = catchmentdepth[i] - hand[i];
      } else {
        //Rcout << "res na\n";
        res[i] = NA_REAL;
      }
    } else if(postproc_method == "interp-hand") {
      if(handid[i] != NA_INTEGER && !NumericVector::is_na(depths[handid[i] - 1]) && !NumericMatrix::is_na(hand[i]) && depths[handid[i] - 1] >= hand[i]) {
        //Rcout << "na\n";
        res[i] = depths[handid[i] - 1] - hand[i];
      } else {
        res[i] = NA_REAL;
      }
    }
  }

  return res;
}


//' Postprocesing a hand vector of values for depths
//' @param x A single hand (or dhand) value.
//' @param d A single value of depth.
//'
//' @return A list of updated preprocessing tables.
//' @noRd
// [[Rcpp::export]]
double cpp_hand_calc(NumericVector x, NumericVector d) {
 /*
  hand_calc <- function(x=NA,d) {
  return( ifelse(is.finite(x),
  ifelse(d>=x,d-x,NA),        # return NA or 0 for no wetting? NA for now
  NA))}
  */
 LogicalVector res1 = is_finite(x)[0];
 LogicalVector res2 = d[0]>=x[0];
 LogicalVector res3 = res1 & res2;
 if(res3) {
   return d[0]-x[0];
 } else {
   return NA_REAL;
 }
}


//' Postprocesing a hand vector of values for depths
//' @param a The dimensions of the dem.
//' @param reachshp_lens An array of lengths of the reach shape object.
//' @param seqH An array of heights to be processed.
//' @param uni An array of unique catchments to process.
//' @param catchment The matrix form of a catchment raster.
//'
//' @return A list of updated preprocessing tables.
//' @noRd
// [[Rcpp::export]]
void cpp_postprocess_hand_vector(NumericVector depth_raster_mm,
                                 IntegerVector spp_pointid, NumericVector spp_depth,
                                 IntegerVector handid_mm, NumericVector hand_mm) {


 /*
 hand_calc <- function(x=NA,d) {
   return( ifelse(is.finite(x),
                  ifelse(d>=x,d-x,NA), # return NA or 0 for no wetting? NA for now
                  NA))}
  for (i in 1:nrow(spp)) {
    ind <- which(handid_mm == spp$pointid[i])
    depth_raster_mm[ind] <- vapply(hand_mm[ind],FUN=hand_calc,d=spp$depth[i], FUN.VALUE = numeric(1))
  }
  */


 for(int i = 0; i < spp_pointid.length(); i++) {
  // IntegerVector chandid_mm = handid_mm[handid_mm == spp_pointid[i]];
  NumericVector chand_mm = hand_mm[handid_mm == spp_pointid[i]];
  NumericVector depth_raster_mm_subset = depth_raster_mm[handid_mm == spp_pointid[i]];
   for(int j = 0; j < depth_raster_mm_subset.length(); j++) {
     depth_raster_mm_subset[j] = cpp_hand_calc(chand_mm[j], spp_depth[i]);
   }
 }
}




//' Processing for downslope Dinf method for fuzzy hand
//' @param manningsn The matrix form of a manningsn raster.
//' @return NumericMatrix of pour point IDs for each row of allcells2eval
//' @noRd
// [[Rcpp::export]]
IntegerMatrix cpp_downslope_processing(IntegerVector allcells2eval,
                              IntegerVector validcells,
                              NumericVector emptydem,
                              IntegerVector rpp,
                              NumericVector dinf,
                              NumericMatrix allds,
                              IntegerMatrix alldsind,
                              IntegerMatrix alldirs,
                              int maxiter,
                              int maxcolsize) {

  Rcout << "starting function!!" << std::endl;



  // switch allids to IntegerMatrix?

  // int matcolsize = 50;
 IntegerMatrix allids(allcells2eval.length(), maxcolsize); // Initialize with NA values

 // Rcout << "numeric matrix created" << std::endl;

  for(int ii=0; ii < allcells2eval.length(); ii++) {

    Rcout << "loop ii is: " << std::to_string(ii) <<   std::endl;


   int cells = allcells2eval[ii];

   // Rcout << "got cells defined " << std::to_string(cells) <<   std::endl;

   // Rcout << "rpp[ii] is " << std::to_string(rpp[ii]) <<   std::endl;

   if(rpp[ii] == -1) {

     // Rcout << "assign reval" << std::endl;

     NumericVector reval = emptydem;
     NumericVector rww  = emptydem;

     rww[cells] = 1;
     reval[cells] = 1;

     // Rcout << "building ds" << std::endl;

     NumericVector ds = getRowNM(allds,ii);
     IntegerVector ad = getRowIM(alldirs,ii);
     IntegerVector dsind = getRowIM(alldsind,ii);

     // Rcout << "Got row fetches" <<    std::endl;

     IntegerVector updatecells = ad[dsind];
     rww[updatecells] = ds;

     int i = 0;
     IntegerVector evalcells = updatecells;

     Rcout << "entering while loop" << std::endl;

     while (i <= maxiter) {

       if(i == maxiter) {
         Rcout << "maxiter limit reached" << std::endl;
         break;
       }

       if(evalcells.length()==0){
         Rcout << "no cells left to evaluate" << std::endl;
         break;
       }


       bool breakwhile = false;

       Rcout << "entering jj loop with ii= " << std::to_string(ii) << " and i is " << std::to_string(i) << " and cells is " << std::to_string(cells) << std::endl;

       IntegerVector evalcells2 = evalcells;

       for(int jj=0; jj <= evalcells.length(); jj++) {

         Rcout << "in jj loop with jj= " << std::to_string(jj) << " and evalcells.length() is " << std::to_string(evalcells.length()) << " and evalcells2.length() is " << std::to_string(evalcells2.length()) << std::endl;

         evalcells2 = drop_first(evalcells2); // remove first element,about to be used or discarded
         if (rww[evalcells[jj]] != 0 && reval[evalcells[jj]] != 1 && rpp[evalcells[jj]] != -1 && dinf[evalcells[jj]] != -1) {
           cells = evalcells[jj];
           Rcout << "about to purge jj seqeval" << std::endl;
           /*IntegerVector   seqeval = {0,jj-1};
           evalcells = dropValues(evalcells,seqeval); // purge evalcells list
           evalcells2 = drop_first(evalcells2); // remove first element, about to be used as cells
           Rcout << "seqeval purged" << std::endl;
            */
           Rcout << "breaking out of evalcells block!" << std::endl;
           break;
         } else if (evalcells2.length() == 0) {
           Rcout << "hit max length of evalcells, breaking for loop for jj"<< std::endl;
             // nothing viable found in eval list
             breakwhile=true;
             break;
         } else {
           Rcout << "no jj for loop conditions met" << std::endl;
         }
       }

       Rcout << "exited jj for loop"<< std::endl;


       Rcout << "length of evalcells2 is " << std::to_string(evalcells2.length()) << std::endl;

       evalcells = evalcells2;

       Rcout << "Assigned evalcells"<< " and breakwhile is "<< std::to_string(breakwhile) << std::endl;

       if (breakwhile) {
         Rcout << "breaking while loop"<< std::endl;
         break;
       }

       Rcout << "cells is "<< std::to_string(cells) << std::endl;

       reval[cells] = 1;

       // Rcout << "about to do containsIV" << std::endl;

       if (containsIV(validcells,cells)) {

         NumericVector ds = getRowNM(allds,ii);
         IntegerVector ad = getRowIM(alldirs,ii);
         IntegerVector dsind = getRowIM(alldsind,ii);
         IntegerVector updatecells = ad[dsind];
         rww[updatecells] = ds;

         Rcout << "getting cell position" << std::endl;

         int cellsint =getPositionIV(allcells2eval,cells);

         ds = getRowNM(allds,cellsint) * rww[cells];
         ad = getRowIM(alldirs,cellsint);
         dsind = getRowIM(alldsind,cells);
         updatecells = ad[dsind];
         rww[updatecells] = sumNumericVectors(ds,rww[updatecells]);

         // update evalcells with updatecells, only adding unique values
         for (int kk=0; kk < updatecells.length(); kk++) {
           if (!containsIV(evalcells,updatecells[kk])) {
             evalcells.push_back(updatecells[kk]); // Append a single value
           }
         }
       }
       Rcout << "incrementing i" << std::endl;
       i++; // Increment the while counter
     }

     Rcout << "out of while loop, storing results into allids" << std::endl;

     // Rcout << "cells is " << std::to_string(cells) << " and rpp[cells] is " << std::to_string(rpp[cells]) << " and ii is " << std::to_string(ii) << std::endl;

    // get some portion of pourpoints saved which(rww!=0 & rpp )
    // storeRowElements(allids, ii, rpp[cells]);


    IntegerVector rppind = getIndicesNotEqual(rpp,-1);
    IntegerVector rwwg0 = getIndicesGreaterThanZero(rww);
    IntegerVector indgrab = getCommonElements(rppind, rwwg0);

    Rcout << "length of rppind is " << std::to_string(rppind.length()) << " and length of rwwg0 is " << std::to_string(rwwg0.length()) << std::endl;

    Rcout << "assigned rpp[indgrab] with length " << std::to_string(indgrab.length()) << std::endl;

    if (indgrab.length()==0) {
      indgrab = rppind; // temporary for testing
    }

    if (indgrab.length() > 0 ) {

      if (indgrab.length() > maxcolsize) {
        Rcout << "For ii=" << std::to_string(ii) << ", more than maxcolsize of " << std::to_string(maxcolsize) << " elements found, only maxcolsize will be stored" << std::endl;
      }
      Rcout << "storing indgrab with length > 0" << std::endl;
      storeRow(allids, rpp[indgrab], ii);
    }



   } else {

     Rcout << "out of while loop, storing just rpp for specific pourpoint into allids" << std::endl;

     // cell is already a pourpoint, assign its own value
     // allids[[cells]] <- rpp[cells]
     // storeRowElements(allids, ii, rpp[cells]);
   }
  }

  return(allids);
}


