/*-----------------------------------------------------------------------------
 This file is part of aorsf.
 Author: Byron C Jaeger
 aorsf may be modified and distributed under the terms of the MIT license.
#----------------------------------------------------------------------------*/

#ifndef TREE_H_
#define TREE_H_

#include "Data.h"
#include "globals.h"
#include "utility.h"

 namespace aorsf {

 class Tree {

 public:

  Tree();

  // Create from loaded forest
  Tree(arma::uvec& rows_oobag,
       std::vector<double>& cutpoint,
       std::vector<arma::uword>& child_left,
       std::vector<arma::vec>& coef_values,
       std::vector<arma::uvec>& coef_indices,
       std::vector<double>& leaf_summary);

  virtual ~Tree() = default;

  // deleting the copy constructor
  Tree(const Tree&) = delete;
  // deleting the copy assignment operator
  Tree& operator=(const Tree&) = delete;

  void init(Data* data,
            int seed,
            arma::uword mtry,
            bool sample_with_replacement,
            double sample_fraction,
            PredType pred_type,
            // double leaf_min_events,
            double leaf_min_obs,
            VariableImportance vi_type,
            double vi_max_pvalue,
            SplitRule split_rule,
            // double split_min_events,
            double split_min_obs,
            double split_min_stat,
            arma::uword split_max_cuts,
            arma::uword split_max_retry,
            LinearCombo lincomb_type,
            double lincomb_eps,
            arma::uword lincomb_iter_max,
            bool   lincomb_scale,
            double lincomb_alpha,
            arma::uword lincomb_df_target,
            arma::uword lincomb_ties_method,
            Rcpp::RObject lincomb_R_function,
            Rcpp::RObject oobag_R_function,
            EvalType oobag_eval_type,
            int verbosity);


  virtual void resize_leaves(arma::uword new_size);

  void sample_rows();

  void sample_cols();

  virtual bool is_col_splittable(arma::uword j);

  bool is_node_splittable(arma::uword node_id);

  virtual bool is_node_splittable_internal();

  virtual void find_all_cuts();

  virtual uword find_safe_mtry();

  virtual double compute_split_score();

  void sample_cuts();

  double find_best_cut();

  void sprout_leaf(arma::uword node_id);

  virtual void sprout_leaf_internal(arma::uword node_id) = 0;

  virtual double compute_max_leaves();

  void grow(arma::vec* vi_numer,
            arma::uvec* vi_denom);

  void predict_leaf(Data* prediction_data,
                    bool oobag);

  void predict_leaf(Data* prediction_data,
                    bool oobag,
                    arma::vec& pd_x_vals,
                    arma::uvec& pd_x_cols);

  void predict_value(arma::mat& pred_output,
                     arma::vec& pred_denom,
                     PredType pred_type,
                     bool oobag);

  virtual arma::uword predict_value_internal(arma::uvec& pred_leaf_sort,
                                             arma::mat& pred_output,
                                             arma::vec& pred_denom,
                                             PredType pred_type,
                                             bool oobag) = 0;

  void negate_coef(arma::uword pred_col);

  void compute_oobag_vi(arma::vec* vi_numer, VariableImportance vi_type);

  void compute_dependence(Data* prediction_data,
                          std::vector<std::vector<arma::mat>>& result,
                          PartialDepType pd_type,
                          std::vector<arma::mat>& pd_x_vals,
                          std::vector<arma::uvec>& pd_x_cols,
                          arma::vec& oobag_denom,
                          bool oobag);

  std::vector<arma::uvec>& get_coef_indices() {
   return(coef_indices);
  }

  arma::uvec& get_rows_oobag() {
   return(rows_oobag);
  }

  arma::uvec& get_rows_inbag(){
   return(rows_inbag);
  }


  std::vector<arma::vec>& get_coef_values() {
   return(coef_values);
  }

  std::vector<double>& get_leaf_summary(){
   return(leaf_summary);
  }

  std::vector<double>& get_cutpoint(){
   return(cutpoint);
  }

  std::vector<arma::uword>& get_child_left(){
   return(child_left);
  }

  arma::uvec& get_pred_leaf(){
   return(pred_leaf);
  }

  arma::uvec& get_cuts_all(){
   return(cuts_all);
  }

  arma::uvec& get_cuts_sampled(){
   return(cuts_sampled);
  }

  // helper function used to help test tree functions in R
  void set_x_inbag(arma::mat x){
   this->x_inbag = x;
  }

  void set_y_inbag(arma::mat y){
   this->y_inbag = y;
  }

  void set_w_inbag(arma::vec w){
   this->w_inbag = w;
  }

  void set_x_node(arma::mat x){
   this->x_node = x;
  }

  void set_y_node(arma::mat y){
   this->y_node = y;
  }

  void set_w_node(arma::vec w){
   this->w_node = w;
  }

  void set_rows_node(arma::uvec rows){
   this->rows_node = rows;
  }

  void set_rows_oobag(arma::uvec rows){
   this->rows_oobag = rows;
  }

  void set_lincomb(arma::vec lc){
   this->lincomb = lc;
  }

  void set_lincomb_sort(arma::uvec lc_sort){
   this->lincomb_sort = lc_sort;
  }

  void set_leaf_min_obs(double value){
   this->leaf_min_obs = value;
  }

  void set_verbosity(int value){
   this->verbosity = value;
  }

  void set_seed(int value){
   random_number_generator.seed(seed);
  }

  void set_split_max_cuts(uword value){
   this->split_max_cuts = value;
  }

  void set_split_rule(SplitRule value){
   this->split_rule = value;
  }

  void find_rows_inbag(arma::uword n_obs);

  virtual arma::mat glm_fit();
  virtual arma::mat glmnet_fit();
  virtual arma::mat user_fit();

  virtual uword get_n_col_vi()=0;

  virtual void predict_value_vi(arma::mat& pred_values)=0;

 protected:

  void resize_oobag_data();

  // pointers to variable importance in forest
  arma::vec* vi_numer;
  arma::uvec* vi_denom;

  // // pointers to partial dependence inputs in forest
  // PartialDepType pd_type;
  // std::vector<arma::mat>* pd_x_vals;
  // std::vector<arma::uvec>* pd_x_cols;

  // Pointer to original data
  Data* data;

  arma::uword n_cols_total;
  arma::uword n_rows_total;

  arma::uword n_rows_inbag;

  double n_obs_inbag;
  double n_events_inbag;

  double max_nodes;
  double max_leaves;


  // views of data
  arma::mat x_inbag;
  arma::mat x_oobag;
  arma::mat x_node;

  arma::vec x_oobag_restore;

  arma::mat y_inbag;
  arma::mat y_oobag;
  arma::mat y_node;

  // the 'w' is short for 'weights'
  arma::vec w_inbag;
  arma::vec w_oobag;
  arma::vec w_node;

  // g_node indicates where observations will go when this node splits
  // 0 means go down to left node, 1 means go down to right node
  // the 'g' is short for 'groups'
  arma::uvec g_node;

  int seed;

  arma::uword mtry;

  bool sample_with_replacement;
  double sample_fraction;

  // what type of predictions you compute
  PredType pred_type;

  // variable importance
  VariableImportance vi_type;
  double vi_max_pvalue;

  // Random number generator
  std::mt19937_64 random_number_generator;

  // tree growing members
  // double leaf_min_events;
  double leaf_min_obs;

  // node split members
  SplitRule split_rule;
  // double split_min_events;
  double split_min_obs;
  double split_min_stat;
  arma::uword split_max_cuts;
  arma::uword split_max_retry;

  // linear combination members
  LinearCombo   lincomb_type;
  arma::vec     lincomb;
  arma::uvec    lincomb_sort;
  double        lincomb_eps;
  arma::uword   lincomb_iter_max;
  bool          lincomb_scale;
  double        lincomb_alpha;
  arma::uword   lincomb_df_target;
  arma::uword   lincomb_ties_method;
  Rcpp::RObject lincomb_R_function;

  // allow customization of oobag prediction accuracy
  Rcpp::RObject oobag_R_function;
  EvalType oobag_eval_type;

  int verbosity;


  // all possible cut-points for a linear combination
  uvec cuts_all;
  // a sampled subset of cutpoints.
  uvec cuts_sampled;

  // which rows of data are held out while growing the tree
  arma::uvec rows_inbag;
  arma::uvec rows_oobag;
  arma::uvec rows_node;
  arma::uvec cols_node;


  // predicted leaf node
  arma::uvec pred_leaf;

  // which node each inbag observation is currently in.
  arma::uvec node_assignments;

  // cutpoints used to split the nodes
  std::vector<double> cutpoint;

  // left child nodes (right child is left + 1)
  std::vector<arma::uword> child_left;

  // coefficients for linear combinations;
  // one row per variable (mtry rows), one column per node
  // leaf nodes have all coefficients=0
  std::vector<arma::vec> coef_values;
  // std::vector<arma::vec> coef_values;

  // indices of the predictors used by a node
  std::vector<arma::uvec> coef_indices;
  // std::vector<arma::uvec> coef_indices;

  // leaf values (only in leaf nodes)
  std::vector<double> leaf_summary;

  virtual double compute_prediction_accuracy(arma::mat& preds);

  virtual double compute_prediction_accuracy_internal(arma::mat& preds) = 0;

 };

 } // namespace aorsf

#endif /* TREE_H_ */
