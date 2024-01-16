//  Forest.cpp

#include <RcppArmadillo.h>
#include "ForestSurvival.h"
#include "TreeSurvival.h"

#include <memory>

using namespace arma;
using namespace Rcpp;

namespace aorsf {

ForestSurvival::ForestSurvival() { }

ForestSurvival::ForestSurvival(double leaf_min_events,
                               double split_min_events,
                               arma::vec& pred_horizon){

 this->leaf_min_events = leaf_min_events;
 this->split_min_events = split_min_events;
 this->pred_horizon = pred_horizon;

}



void ForestSurvival::load(
  arma::uword n_tree,
  arma::uword n_obs,
  std::vector<arma::uvec>& forest_rows_oobag,
  std::vector<std::vector<double>>& forest_cutpoint,
  std::vector<std::vector<arma::uword>>& forest_child_left,
  std::vector<std::vector<arma::vec>>& forest_coef_values,
  std::vector<std::vector<arma::uvec>>& forest_coef_indices,
  std::vector<std::vector<arma::vec>>& forest_leaf_pred_indx,
  std::vector<std::vector<arma::vec>>& forest_leaf_pred_prob,
  std::vector<std::vector<arma::vec>>& forest_leaf_pred_chaz,
  std::vector<std::vector<double>>& forest_leaf_summary,
  PartialDepType pd_type,
  std::vector<arma::mat>& pd_x_vals,
  std::vector<arma::uvec>& pd_x_cols,
  arma::vec& pd_probs
) {

 this->n_tree    = n_tree;
 this->pd_type   = pd_type;
 this->pd_x_vals = pd_x_vals;
 this->pd_x_cols = pd_x_cols;
 this->pd_probs  = pd_probs;

 if(verbosity > 2){
  Rcout << "---- loading forest from input list ----";
  Rcout << std::endl << std::endl;
 }

 // Create trees
 trees.reserve(n_tree);

 for (uword i = 0; i < n_tree; ++i) {
  trees.push_back(
   std::make_unique<TreeSurvival>(n_obs,
                                  forest_rows_oobag[i],
                                  forest_cutpoint[i],
                                  forest_child_left[i],
                                  forest_coef_values[i],
                                  forest_coef_indices[i],
                                  forest_leaf_pred_indx[i],
                                  forest_leaf_pred_prob[i],
                                  forest_leaf_pred_chaz[i],
                                  forest_leaf_summary[i],
                                  &pred_horizon)
  );
 }

 if(n_thread > 1){
  // Create thread ranges
  equalSplit(thread_ranges, 0, n_tree - 1, n_thread);
 }


}

// growInternal() in ranger
void ForestSurvival::plant() {

 this->unique_event_times = find_unique_event_times(data->get_y());

 trees.reserve(n_tree);

 for (arma::uword i = 0; i < n_tree; ++i) {
  trees.push_back(std::make_unique<TreeSurvival>(leaf_min_events,
                                                 split_min_events,
                                                 &unique_event_times,
                                                 &pred_horizon));
 }

}

void ForestSurvival::resize_pred_mat_internal(arma::mat& p,
                                              arma::uword n){

  p.zeros(n, pred_horizon.size());

}

std::vector<std::vector<arma::vec>> ForestSurvival::get_leaf_pred_indx() {

 std::vector<std::vector<arma::vec>> result;

 result.reserve(n_tree);

 for (auto& tree : trees) {
  auto& temp = dynamic_cast<TreeSurvival&>(*tree);
  result.push_back(temp.get_leaf_pred_indx());
 }

 return result;

}

std::vector<std::vector<arma::vec>> ForestSurvival::get_leaf_pred_prob() {

 std::vector<std::vector<arma::vec>> result;

 result.reserve(n_tree);

 for (auto& tree : trees) {
  auto& temp = dynamic_cast<TreeSurvival&>(*tree);
  result.push_back(temp.get_leaf_pred_prob());
 }

 return result;

}

std::vector<std::vector<arma::vec>> ForestSurvival::get_leaf_pred_chaz() {

 std::vector<std::vector<arma::vec>> result;

 result.reserve(n_tree);

 for (auto& tree : trees) {
  auto& temp = dynamic_cast<TreeSurvival&>(*tree);
  result.push_back(temp.get_leaf_pred_chaz());
 }

 return result;

}

void ForestSurvival::resize_oobag_eval(){

 uword n_evals = find_max_eval_steps();

 oobag_eval.resize(n_evals, pred_horizon.size());

}

void ForestSurvival::compute_prediction_accuracy_internal(
  arma::mat& y,
  arma::vec& w,
  arma::mat& predictions,
  arma::uword row_fill
) {

 if(oobag_eval_type == EVAL_R_FUNCTION){

  // initialize function from tree object
  // (Functions can't be stored in C++ classes, but Robjects can)
  Rcpp::Function f_oobag_eval = Rcpp::as<Rcpp::Function>(oobag_R_function);
  Rcpp::NumericMatrix y_ = Rcpp::wrap(y);
  Rcpp::NumericVector w_ = Rcpp::wrap(w);

  for(uword i = 0; i < oobag_eval.n_cols; ++i){
   vec p = predictions.unsafe_col(i);
   Rcpp::NumericVector p_ = Rcpp::wrap(p);
   Rcpp::NumericVector R_result = f_oobag_eval(y_, w_, p_);
   oobag_eval(row_fill, i) = R_result[0];
  }
  return;
 }

 bool pred_is_risklike = true;
 if(pred_type == PRED_SURVIVAL || pred_type == PRED_TIME){
  pred_is_risklike = false;
 }

 for(arma::uword i = 0; i < oobag_eval.n_cols; ++i){
  vec p = predictions.col(i);
  oobag_eval(row_fill, i) = compute_cstat_surv(y, w, p, pred_is_risklike);
 }


}


}


