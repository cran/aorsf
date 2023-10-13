/*-----------------------------------------------------------------------------
 This file is part of aorsf.
 Author: Byron C Jaeger
 aorsf may be modified and distributed under the terms of the MIT license.
#----------------------------------------------------------------------------*/

#include <RcppArmadillo.h>
#include "TreeSurvival.h"
#include "Coxph.h"
#include "utility.h"
// #include "NodeSplitStats.h"

 using namespace arma;
 using namespace Rcpp;

 namespace aorsf {

 TreeSurvival::TreeSurvival() { }

 TreeSurvival::TreeSurvival(double leaf_min_events,
                            double split_min_events,
                            arma::vec* unique_event_times,
                            arma::vec* pred_horizon){

  this->leaf_min_events = leaf_min_events;
  this->split_min_events = split_min_events;
  this->unique_event_times = unique_event_times;
  this->pred_horizon = pred_horizon;

 }

 TreeSurvival::TreeSurvival(arma::uword n_obs,
                            arma::uvec& rows_oobag,
                            std::vector<double>& cutpoint,
                            std::vector<arma::uword>& child_left,
                            std::vector<arma::vec>& coef_values,
                            std::vector<arma::uvec>& coef_indices,
                            std::vector<arma::vec>& leaf_pred_indx,
                            std::vector<arma::vec>& leaf_pred_prob,
                            std::vector<arma::vec>& leaf_pred_chaz,
                            std::vector<double>& leaf_summary,
                            arma::vec* pred_horizon) :
 Tree(rows_oobag, cutpoint, child_left, coef_values, coef_indices, leaf_summary),
 leaf_pred_indx(leaf_pred_indx),
 leaf_pred_prob(leaf_pred_prob),
 leaf_pred_chaz(leaf_pred_chaz),
 pred_horizon(pred_horizon){

  find_rows_inbag(n_obs);

 }

 void TreeSurvival::resize_leaves(arma::uword new_size) {

  leaf_pred_indx.resize(new_size);
  leaf_pred_prob.resize(new_size);
  leaf_pred_chaz.resize(new_size);
  leaf_summary.resize(new_size);

 }

 double TreeSurvival::compute_max_leaves(){

  n_events_inbag = sum(w_inbag % y_inbag.col(1));

  // find maximum number of leaves for this tree
  // there are four ways to have maximal tree size:
  vec max_leaves_4ways = {
   //  1. every leaf node has exactly leaf_min_obs,
   n_obs_inbag / leaf_min_obs,
   //  2. every leaf node has exactly leaf_min_events,
   n_events_inbag / leaf_min_events,
   //  3. every leaf node has exactly split_min_obs - 1,
   n_obs_inbag / (split_min_obs - 1),
   //  4. every leaf node has exactly split_min_events-1
   n_events_inbag / (split_min_events - 1)
  };

  // number of nodes total in binary tree is 2*L - 1,
  // where L is the number of leaf nodes in the tree.
  // (can prove by induction)
  double max_leaves = std::ceil(max(max_leaves_4ways));

  return(max_leaves);

 }

 bool TreeSurvival::is_col_splittable(uword j){

  uvec::iterator i;

  // initialize as 0 but do not make comparisons until x_first_value
  // is formally defined at the first instance of status == 1
  double x_first_value=0;

  bool x_first_undef = true;

  for (i = rows_node.begin(); i != rows_node.end(); ++i) {

   // if event occurred for this observation
   // column is only splittable if X is non-constant among
   // observations where an event occurred.
   if(y_inbag.at(*i, 1) == 1){

    if(x_first_undef){

     x_first_value = x_inbag.at(*i, j);
     x_first_undef = false;

    } else {

     if(x_inbag.at(*i, j) != x_first_value){
      return(true);
     }

    }

   }

  }

  if(verbosity > 3){
   // # nocov start
   mat x_print = x_inbag.rows(rows_node);
   mat y_print = y_inbag.rows(rows_node);

   uvec rows_event = find(y_print.col(1) == 1);
   x_print = x_print.rows(rows_event);

   Rcout << "  --- Column " << j << " was sampled but ";
   Rcout << " unique values of column " << j << " are ";
   Rcout << unique(x_print.col(j)) << std::endl;
   // # nocov end
  }

  return(false);

 }

 bool TreeSurvival::is_node_splittable_internal(){

  double n_risk = sum(w_node);
  double n_events = sum(y_node.col(1) % w_node);

  return(n_events >= 2*leaf_min_events &&
         n_risk   >= 2*leaf_min_obs &&
         n_events >=   split_min_events &&
         n_risk   >=   split_min_obs);

 }

 void TreeSurvival::find_all_cuts(){

  vec y_status = y_node.unsafe_col(1);

  // assume no valid cutpoints at first
  cuts_all.resize(0);

  uword i, j, k;

  uvec::iterator it, it_min, it_max;

  double n_events = 0, n_risk = 0;

  // stop at end-1 b/c we access it+1 in lincomb_sort
  for(it = lincomb_sort.begin(); it < lincomb_sort.end()-1; ++it){

   n_events += y_status[*it] * w_node[*it];
   n_risk += w_node[*it];

   // If we want to make the current value of lincomb a cut-point, we need
   // to make sure the next value of lincomb isn't equal to this current value.
   // Otherwise, we will have the same value of lincomb in both groups!

   if(lincomb[*it] != lincomb[*(it+1)]){

    if( n_events >= leaf_min_events &&
        n_risk   >= leaf_min_obs ) {

     if(verbosity > 2){
      // # nocov start
      Rcout << std::endl;
      Rcout << "  -- lower cutpoint: "        << lincomb(*it) << std::endl;
      Rcout << "     - n_events, left node: " << n_events << std::endl;
      Rcout << "     - n_risk, left node:   " << n_risk   << std::endl;
      Rcout << std::endl;
      // # nocov end
     }

     break;

    }

   }

  }

  it_min = it;

  if(it == lincomb_sort.end()-1) {

   if(verbosity > 2){
    // # nocov start
    Rcout << "   -- Could not find a valid cut-point" << std::endl;
    // # nocov end
   }

   return;

  }

  // j = number of steps we have taken forward in lincomb
  j = it - lincomb_sort.begin();

  // reset before finding the upper limit
  n_events=0, n_risk=0;

  // stop at beginning+1 b/c we access it-1 in lincomb_sort
  for(it = lincomb_sort.end()-1; it >= lincomb_sort.begin()+1; --it){

   n_events += y_status[*it] * w_node[*it];
   n_risk   += w_node[*it];

   if(lincomb[*it] != lincomb[*(it-1)]){

    if( n_events >= leaf_min_events &&
        n_risk   >= leaf_min_obs ) {

     // the upper cutpoint needs to be one step below the current
     // it value, because we use x <= cp to determine whether a
     // value x goes to the left node versus the right node. So,
     // if it currently points to 3, and the next value down is 2,
     // then we want to say the cut-point is 2 because then all
     // values <= 2 will go left, and 3 will go right. This matters
     // when 3 is the highest value in the vector.

     --it;

     if(verbosity > 2){
      // # nocov start
      Rcout << std::endl;
      Rcout << "  -- upper cutpoint: " << lincomb(*it) << std::endl;
      Rcout << "     - n_events, right node: " << n_events    << std::endl;
      Rcout << "     - n_risk, right node:   " << n_risk      << std::endl;
      Rcout << std::endl;
      // # nocov end
     }

     break;

    }

   }

  }

  it_max = it;

  // k = n steps from beginning of sorted lincomb to current it
  k = it - lincomb_sort.begin();

  if(j > k){

   if(verbosity > 2) {
    // # nocov start
    Rcout << "Could not find valid cut-points" << std::endl;
    // # nocov end
   }

   return;

  }

  // only one valid cutpoint
  if(j == k){

   cuts_all = {j};
   return;

  }

  i = 0;
  uvec output_middle(k-j);

  for(it = it_min+1;
      it < it_max; ++it){
   if(lincomb[*it] != lincomb[*(it+1)]){
    output_middle[i] = it - lincomb_sort.begin();
    i++;
   }
  }

  output_middle.resize(i);

  uvec output_left = {j};
  uvec output_right = {k};

  cuts_all = join_vert(output_left, output_middle, output_right);

 }

 arma::uword TreeSurvival::find_safe_mtry(){

  uword safer_mtry = mtry;

  if(lincomb_type == LC_NEWTON_RAPHSON){

   // Need 3:1 ratio of unweighted events:predictors
   uword n_events_total = sum(y_node.col(1));

   while(n_events_total / safer_mtry < 3){
    --safer_mtry;
    if(safer_mtry == 0) break;
   }

  }

  return(safer_mtry);

 }

 double TreeSurvival::compute_split_score(){

  double result=0;

  switch (split_rule) {

  case SPLIT_LOGRANK: {
   result = compute_logrank(y_node, w_node, g_node);
   break;
  }

  case SPLIT_CONCORD: {
   result = compute_cstat(y_node, w_node, g_node, true);
   break;
  }

  }

  return(result);

 }

 void TreeSurvival::sprout_leaf(uword node_id){

  if(verbosity > 2){
   // # nocov start
   Rcout << "-- sprouting node " << node_id << " into a leaf";
   Rcout << " (N = " << sum(w_node) << ")";
   Rcout << std::endl;
   Rcout << std::endl;
   // # nocov end
  }


  // reserve as much size as could be needed (probably more)
  mat leaf_data(y_node.n_rows, 3);

  uword person = 0;

  // find the first unique event time
  while(y_node.at(person, 1) == 0 && person < y_node.n_rows){
   person++;
  }


  // person corresponds to first event or last censor time
  leaf_data.at(0, 0) = y_node.at(person, 0);

  // if no events in this node:
  // should this case even occur? consider removing
  if(person == y_node.n_rows){

   vec temp_surv(1, fill::ones);
   vec temp_chf(1, fill::zeros);

   leaf_pred_indx[node_id] = leaf_data.col(0);
   leaf_pred_prob[node_id] = temp_surv;
   leaf_pred_chaz[node_id] = temp_chf;
   leaf_summary[node_id] = 0.0;

   return;

  }

  double temp_time = y_node.at(person, 0);

  uword i = 1;

  // find the rest of the unique event times
  for( ; person < y_node.n_rows; person++){

   if(temp_time != y_node.at(person, 0) && y_node.at(person, 1) == 1){

    leaf_data.at(i, 0) = y_node.at(person,0);
    temp_time = y_node.at(person, 0);
    i++;

   }

  }

  leaf_data.resize(i, 3);

  // reset for kaplan meier loop
  person = 0; i = 0;
  double n_risk = sum(w_node);
  double temp_surv = 1.0;
  double temp_haz = 0.0;

  do {

   double n_events   = 0;
   double n_risk_sub = 0;
   temp_time = y_node.at(person, 0);

   while(y_node.at(person, 0) == temp_time){

    n_risk_sub += w_node.at(person);
    n_events += y_node.at(person, 1) * w_node.at(person);

    if(person == y_node.n_rows-1) break;

    person++;

   }

   // only do km if a death was observed

   if(n_events > 0){

    temp_surv = temp_surv * (n_risk - n_events) / n_risk;

    temp_haz = temp_haz + n_events / n_risk;

    leaf_data.at(i, 1) = temp_surv;
    leaf_data.at(i, 2) = temp_haz;
    i++;

   }

   n_risk -= n_risk_sub;

  } while (i < leaf_data.n_rows);


  if(verbosity > 3){
   // # nocov start
   mat tmp_mat = join_horiz(y_node, w_node);
   print_mat(tmp_mat, "time & status & weights in this node", 10, 10);
   print_mat(leaf_data, "leaf_data (showing up to 5 rows)", 5, 5);
   // # nocov end
  }

  leaf_pred_indx[node_id] = leaf_data.col(0);
  leaf_pred_prob[node_id] = leaf_data.col(1);
  leaf_pred_chaz[node_id] = leaf_data.col(2);
  leaf_summary[node_id] = compute_mortality(leaf_data);

 }

 double TreeSurvival::compute_mortality(arma::mat& leaf_data){

  double result = 0;
  uword i=0, j=0;

  for( ; i < (*unique_event_times).size(); i++){


   while((*unique_event_times)[i] > leaf_data.at(j, 0) &&
          j < (leaf_data.n_rows-1)) {
    j++;
   }

   result += leaf_data.at(j, 2);

  }

  return(result);

 }

 // double TreeSurvival::compute_mortality(arma::mat& leaf_data){
 //
 //  double result = 0;
 //  uword i=0, j=0;
 //
 //  for( ; i < (*unique_event_times).size(); i++){
 //
 //   if((*unique_event_times)[i] >= leaf_data.at(j, 0) &&
 //      j < (leaf_data.n_rows-1)) {j++;}
 //
 //   result += leaf_data.at(j, 2);
 //
 //  }
 //
 //  return(result);
 //
 // }

 void TreeSurvival::predict_value(arma::mat* pred_output,
                                  arma::vec* pred_denom,
                                  PredType pred_type,
                                  bool oobag){

  uvec pred_leaf_sort = sort_index(pred_leaf, "ascend");

  uvec::iterator it = pred_leaf_sort.begin();

  if(verbosity > 2){
   // # nocov start
   uvec tmp_uvec = find(pred_leaf < max_nodes);

   if(tmp_uvec.size() == 0){
    Rcout << pred_leaf<< std::endl;
    Rcout << "max_nodes: " << max_nodes << std::endl;
   }

   Rcout << "   -- N preds expected: " << tmp_uvec.size() << std::endl;
   // # nocov end
  }

  uword leaf_id = pred_leaf[*it];

  // default for risk or survival at time 0
  double pred_t0 = 1;

  // default otherwise
  if (pred_type == PRED_CHAZ ||
      pred_type == PRED_MORTALITY) {
   pred_t0 = 0;
  }

  uword i, j;

  uword n_preds_made = 0;

  vec leaf_times, leaf_values;

  vec temp_vec((*pred_horizon).size());

  double temp_dbl = pred_t0;
  bool break_loop = false;

  for(; ;) {


   // copies of leaf data using same aux memory
   leaf_times = vec(leaf_pred_indx[leaf_id].begin(),
                    leaf_pred_indx[leaf_id].size(),
                    false);

   switch (pred_type) {

   case PRED_RISK: case PRED_SURVIVAL: {

    leaf_values = vec(leaf_pred_prob[leaf_id].begin(),
                      leaf_pred_prob[leaf_id].size(),
                      false);

    break;

   }

   case PRED_CHAZ: {

    leaf_values = vec(leaf_pred_chaz[leaf_id].begin(),
                      leaf_pred_chaz[leaf_id].size(),
                      false);

    break;

   }

   case PRED_MORTALITY: {

    temp_vec.fill(leaf_summary[leaf_id]);

    break;

   }

   default:
    Rcout << "Invalid pred type; R will crash";
    break;

   }

   // don't reset i in the loop b/c leaf_times ascend
   i = 0;

   if(pred_type != PRED_MORTALITY){

    for(j = 0; j < (*pred_horizon).size(); j++){

     // t is the current prediction time
     double t = (*pred_horizon)[j];

     // if t < t', where t' is the max time in this leaf,
     // then we may find a time t* such that t* < t < t'.
     // If so, prediction should be anchored to t*.
     // But, there may be multiple t* < t, and we want to
     // find the largest t* that is < t, so we find the
     // first t** > t and assign t* to be whatever came
     // right before t**.
     if(t < leaf_times.back()){

      for(; i < leaf_times.size(); i++){

       // we found t**
       if (leaf_times[i] > t){

        if(i == 0)
         // first leaf event occurred after prediction time
         temp_dbl = pred_t0;
        else
         // t* is the time value just before t**, so use i-1
         temp_dbl = leaf_values[i-1];

        break;

       } else if (leaf_times[i] == t){
        // pred_horizon just happens to equal a leaf time
        temp_dbl = leaf_values[i];

        break;

       }

      }

     } else {
      // if t > t' use the last recorded prediction
      temp_dbl = leaf_values.back();

     }

     temp_vec[j] = temp_dbl;

    }

   }

   if(pred_type == PRED_RISK) temp_vec = 1 - temp_vec;

   (*pred_output).row(*it) += temp_vec.t();
   n_preds_made++;
   if(oobag) (*pred_denom)[*it]++;

   // Rcout << "npreds: " << n_preds_made << ", ";
   // Rcout << "*it: " << (*it) << std::endl;

   // in case the last obs has a unique leaf assignment
   if(it == pred_leaf_sort.end()-1) break;

   for(; ;){

    ++it;
    if (it == pred_leaf_sort.end()-1){
     // we've reached the final value of pred_leaf
     // check to see if it's the same leaf as the obs before:
     if (leaf_id == pred_leaf[*it]){
      // if it is, add the value to the pred_output, and be done
      (*pred_output).row(*it) += temp_vec.t();
      n_preds_made++;
      if(oobag) (*pred_denom)[*it]++;
      break_loop = true;
      break;
     }

    }

    if(leaf_id != pred_leaf[*it]) break;

    (*pred_output).row(*it) += temp_vec.t();
    n_preds_made++;
    if(oobag) (*pred_denom)[*it]++;

    // Rcout << "npreds: " << n_preds_made << ", ";
    // Rcout << "*it (inner loop): " << (*it) << std::endl;

   }

   if(break_loop) break;

   leaf_id = pred_leaf(*it);

   // case 3: we've finished out-of-bag predictions
   if(leaf_id == max_nodes) break;

  }

  if(verbosity > 2){
   // # nocov start
   Rcout << "   -- N preds made: " << n_preds_made;
   Rcout << std::endl;
   Rcout << std::endl;
   // # nocov end
  }


 }

 double TreeSurvival::compute_prediction_accuracy_internal(arma::vec& preds){

  return compute_cstat(y_oobag, w_oobag, preds, true);

 }


 } // namespace aorsf

