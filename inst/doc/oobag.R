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
library(survivalROC)


## -----------------------------------------------------------------------------

fit <- orsf(data = pbc_orsf, 
            formula = Surv(time, status) ~ . - id,
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

 # risk = 1 - survival 
 r_vec <- 1 - s_vec

 # mean of the squared differences between predicted and observed risk
 mean( (y_mat[, 'status'] - r_vec)^2 )
 
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

oobag_fun_sroc <- function(y_mat, s_vec){

 score <- survivalROC::survivalROC(
  Stime = y_mat[, 'time'],
  status = y_mat[, 'status'],
  # risk = 1 - survival
  marker = 1 - s_vec,
  # important!! Make sure this matches the time you used in orsf
  predict.time = 3500,
  # nearest neighbor estimation for censoring
  method = "NNE",
  # value taken from ?survivalROC examples
  span = 0.25 * nrow(y_mat)^(-0.20)
 )
 
 # oobag_fun needs to return a numeric value of length 1
 score$AUC

}

fit <- orsf(data = pbc_orsf,
            formula = Surv(time, status) ~ . - id,
            n_tree = 50,
            oobag_pred_horizon = 3500,
            oobag_fun = oobag_fun_sroc,
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
names(formals(oobag_fun_sroc)) == c("y_mat", "s_vec")

test_output <- oobag_fun_sroc(y_mat = y_mat, s_vec = s_vec)

# test output should be numeric
is.numeric(test_output)
# test_output should be a numeric value of length 1
length(test_output) == 1


## -----------------------------------------------------------------------------

fit_sroc <- orsf(data = pbc_orsf,
                 formula = Surv(time, status) ~ . - id,
                 n_tree = 50,
                 oobag_pred_horizon = 3500,
                 oobag_fun = oobag_fun_sroc,
                 importance = 'negate')

fit_sroc$importance


## -----------------------------------------------------------------------------

pred_oobag <- fit$pred_oobag

pred_oobag[1:5, ]


