## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.height = 5, 
  fig.width = 7
)

## ----setup--------------------------------------------------------------------

library(aorsf)
library(survival)
library(SurvMetrics)


## -----------------------------------------------------------------------------

fit <- orsf(data = pbc_orsf, 
            formula = Surv(time, status) ~ . - id,
            oobag_pred_type = 'surv',
            oobag_pred_horizon = 3500)

hist(fit$pred_oobag, 
     main = 'Ensemble out-of-bag survival predictions at t=3,500')


## -----------------------------------------------------------------------------

# what function is used to evaluate out-of-bag predictions?
fit$eval_oobag$stat_type

# what is the output from this function?
fit$eval_oobag$stat_values


## -----------------------------------------------------------------------------

fit <- orsf(data = pbc_orsf,
            formula = Surv(time, status) ~ . - id,
            n_tree = 50,
            oobag_pred_horizon = 3500,
            oobag_eval_every = 1)

plot(
 x = seq(1, 50, by = 1),
 y = fit$eval_oobag$stat_values, 
 main = 'Out-of-bag C-statistic computed after each new tree is grown.',
 xlab = 'Number of trees grown',
 ylab = fit$eval_oobag$stat_type
)


## -----------------------------------------------------------------------------

oobag_fun_brier <- function(y_mat, s_vec){

 # output is numeric vector of length 1
 as.numeric(
  SurvMetrics::Brier(
   object = Surv(time = y_mat[, 1], event = y_mat[, 2]), 
   pre_sp = s_vec,
   # t_star in Brier() should match oob_pred_horizon in orsf()
   t_star = 3500
  )
 )
 
}

## -----------------------------------------------------------------------------

oobag_fun_brier(y_mat = fit$data[, c('time', 'status')],
                s_vec = fit$pred_oobag)


## -----------------------------------------------------------------------------

fit <- orsf(data = pbc_orsf,
            formula = Surv(time, status) ~ . - id,
            n_tree = 50,
            oobag_pred_horizon = 3500,
            oobag_fun = oobag_fun_brier,
            oobag_eval_every = 1)

plot(
 x = seq(1, 50, by = 1),
 y = fit$eval_oobag$stat_values, 
 main = 'Out-of-bag error computed after each new tree is grown.',
 sub = 'For the Brier score, lower values indicate more accurate predictions',
 xlab = 'Number of trees grown',
 ylab = "Brier score"
)


## -----------------------------------------------------------------------------

oobag_fun_tdep_cstat <- function(y_mat, s_vec){

 as.numeric(
  SurvMetrics::Cindex(
   object = Surv(time = y_mat[, 1], event = y_mat[, 2]), 
   predicted = s_vec,
   t_star = 3500
  )
 )

}

fit <- orsf(data = pbc_orsf,
            formula = Surv(time, status) ~ . - id,
            n_tree = 50,
            oobag_pred_horizon = 3500,
            oobag_fun = oobag_fun_tdep_cstat,
            oobag_eval_every = 1)

plot(
 x = seq(50),
 y = fit$eval_oobag$stat_values, 
 main = 'Out-of-bag time-dependent AUC\ncomputed after each new tree is grown.',
 xlab = 'Number of trees grown',
 ylab = "AUC at t = 3,500"
)


## -----------------------------------------------------------------------------

# Helper code to make sure your oobag_fun function will work with aorsf

# time and status values
test_time <- seq(from = 1, to = 5, length.out = 100)
test_status <- rep(c(0,1), each = 50)

# y-matrix is presumed to contain time and status (with column names)
y_mat <- cbind(time = test_time, status = test_status)
# s_vec is presumed to be a vector of survival probabilities
s_vec <- seq(0.9, 0.1, length.out = 100)

# see 1 in the checklist above
names(formals(oobag_fun_tdep_cstat)) == c("y_mat", "s_vec")

test_output <- oobag_fun_tdep_cstat(y_mat = y_mat, s_vec = s_vec)

# test output should be numeric
is.numeric(test_output)
# test_output should be a numeric value of length 1
length(test_output) == 1


## -----------------------------------------------------------------------------

fit_tdep_cstat <- orsf(data = pbc_orsf,
                 formula = Surv(time, status) ~ . - id,
                 n_tree = 500,
                 oobag_pred_horizon = 3500,
                 oobag_fun = oobag_fun_tdep_cstat,
                 importance = 'negate')

fit_tdep_cstat$importance


