/*-----------------------------------------------------------------------------
 This file is part of aorsf.
Author: Byron C Jaeger
aorsf may be modified and distributed under the terms of the MIT license.
#----------------------------------------------------------------------------*/

#ifndef UTILITY_H
#define UTILITY_H

#include <armadillo>
#include <Rcpp.h>
#include "globals.h"


 namespace aorsf {

 /**
  * Split sequence start..end in num_parts parts with sizes as equal as possible.
  * @param result Result vector of size num_parts+1. Ranges for the parts are then result[0]..result[1]-1, result[1]..result[2]-1, ..
  * @param start minimum value
  * @param end maximum value
  * @param num_parts number of parts
  * @note: this function is taken directly from ranger.
  */
 void equalSplit(std::vector<uint>& result, uint start, uint end, uint num_parts);

 arma::vec find_unique_event_times(arma::mat& y);


 void print_mat(arma::mat& x,
                std::string label,
                arma::uword max_cols,
                arma::uword max_rows);

 void print_vec(arma::vec& x,
                std::string label,
                arma::uword max_elem);

 void print_uvec(arma::uvec& x,
                 std::string label,
                 arma::uword max_elem);


 /**
  * Convert a unsigned integer to string
  * @param number Number to convert
  * @return Converted number as string
  */
 std::string uintToString(uint number);

 /**
  * Beautify output of time.
  * @param seconds Time in seconds
  * @return Time in days, hours, minutes and seconds as string
  */
 std::string beautifyTime(uint seconds);

 static void chkIntFn(void *dummy) {
  R_CheckUserInterrupt();
 }

 inline bool checkInterrupt() {
  return (R_ToplevelExec(chkIntFn, NULL) == FALSE);
 }

 double compute_logrank(arma::mat& y,
                        arma::vec& w,
                        arma::uvec& g);

 double compute_cstat(arma::mat& y,
                      arma::vec& w,
                      arma::vec& p,
                      bool pred_is_risklike);

 double compute_cstat(arma::mat& y,
                      arma::vec& w,
                      arma::uvec& g,
                      bool pred_is_risklike);

 }

#endif /* UTILITY_H */
