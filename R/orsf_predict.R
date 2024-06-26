

#' Prediction for ObliqueForest Objects
#'
#' Compute predicted values from an oblique random forest. Predictions
#'   may be returned in aggregate (i.e., averaging over all the trees)
#'   or tree-specific.
#'
#' @param object `r roxy_describe_ObliqueForest(trained = TRUE)`.
#'
#' @param new_data a `r roxy_data_allowed()` to compute predictions in.
#'
#' @param pred_type (_character_) the type of predictions to compute. Valid
#'   options for survival are:
#'
#'   - 'risk' : probability of having an event at or before `pred_horizon`.
#'   - 'surv' : 1 - risk.
#'   - 'chf': cumulative hazard function
#'   - 'mort': mortality prediction
#'   - 'time': survival time prediction
#'
#'  For classification:
#'
#'  - 'prob': probability for each class
#'  - 'class': predicted class
#'
#'  For regression:
#'
#'  - 'mean': predicted mean, i.e., the expected value
#'
#' @param pred_horizon (_double_) Only relevent for survival forests.
#'   A value or vector indicating the time(s) that predictions will be
#'   calibrated to. E.g., if you were predicting risk of incident heart
#'   failure within the next 10 years, then `pred_horizon = 10`.
#'   `pred_horizon` can be `NULL` if `pred_type` is `'mort'`, since
#'   mortality predictions are aggregated over all event times
#'
#'
#' @param pred_aggregate (_logical_) If `TRUE` (the default), predictions
#'   will be aggregated over all trees by taking the mean. If `FALSE`, the
#'   returned output will contain one row per observation and one column
#'   for each tree. If the length of `pred_horizon` is two or more and
#'   `pred_aggregate` is `FALSE`, then the result will be a list of such
#'   matrices, with the i'th item in the list corresponding to the i'th
#'   value of `pred_horizon`.
#'
#' @param pred_simplify (_logical_) If `FALSE` (the default), predictions
#'   will always be returned in a numeric matrix or a list of numeric matrices.
#'   If `TRUE`, predictions may be simplified to a vector, e.g., if `pred_type`
#'   is `'mort'` for survival or `'class'` for classification, or an array of
#'   matrices if `length(pred_horizon) > 1`.
#'
#' @param oobag (_logical_) If `FALSE` (the default), predictions will
#'   be computed using all trees for each observation. If `TRUE`, then
#'   out-of-bag predictions will be computed. This input parameter should
#'   only be set to `TRUE` if `new_data` is `NULL`.
#'
#' @param na_action `r roxy_na_action_header("new_data")`
#'
#'   - `r roxy_na_action_fail("new_data")`
#'   - `r roxy_na_action_pass("new_data")`
#'   - `r roxy_na_action_omit("new_data")`
#'   - `r roxy_na_action_impute_meanmode('new_data')`. To clarify,
#'     the mean and mode used to impute missing values are from the
#'     training data of `object`, not from `new_data`.
#'
#' @param boundary_checks (_logical_) if `TRUE`, `pred_horizon` will be
#'  checked to make sure the requested values are less than the maximum
#'  observed time in `object`'s training data. If `FALSE`, these checks
#'  are skipped.
#'
#' @param n_thread `r roxy_n_thread_header("computing predictions")`
#'
#' @inheritParams orsf
#'
#' @param ... `r roxy_dots()`
#'
#' @return a `matrix` of predictions. Column `j` of the matrix corresponds
#'   to value `j` in `pred_horizon`. Row `i` of the matrix corresponds to
#'   row `i` in `new_data`.
#'
#' @details
#'
#' `new_data` must have the same columns with equivalent types as the data
#'   used to train `object`. Also, factors in `new_data` must not have levels
#'   that were not in the data used to train `object`.
#'
#' `pred_horizon` values should not exceed the maximum follow-up time in
#'   `object`'s training data, but if you truly want to do this, set
#'   `boundary_checks = FALSE` and you can use a `pred_horizon` as large
#'   as you want. Note that predictions beyond the maximum follow-up time
#'   in the `object`'s training data are equal to predictions at the
#'   maximum follow-up time, because `aorsf` does not estimate survival
#'   beyond its maximum observed time.
#'
#' If unspecified, `pred_horizon` may be automatically specified as the value
#'   used for `oobag_pred_horizon` when `object` was created (see [orsf]).
#'
#'
#' @export
#'
#' @includeRmd Rmd/orsf_predict_examples.Rmd
#'
predict.ObliqueForest <- function(object,
                                  new_data = NULL,
                                  pred_type = NULL,
                                  pred_horizon = NULL,
                                  pred_aggregate = TRUE,
                                  pred_simplify = FALSE,
                                  oobag = FALSE,
                                  na_action = NULL,
                                  boundary_checks = TRUE,
                                  n_thread = NULL,
                                  verbose_progress = NULL,
                                  ...){

 # catch any arguments that didn't match and got relegated to ...
 # these arguments are mistaken input names since ... isn't used.
 check_dots(list(...), .f = predict.ObliqueForest)

 object$predict(
  new_data = new_data,
  pred_type = pred_type,
  pred_horizon = pred_horizon,
  pred_aggregate = pred_aggregate,
  pred_simplify = pred_simplify,
  oobag = oobag,
  na_action = na_action %||% object$na_action,
  boundary_checks = boundary_checks,
  n_thread = n_thread %||% object$n_thread,
  verbose_progress = verbose_progress %||% object$verbose_progress
 )

}

