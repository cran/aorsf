## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.height = 5, 
  fig.width = 7
)

## ----setup, warning=FALSE-----------------------------------------------------

library(aorsf)
library(survival)
library(SurvMetrics)


## -----------------------------------------------------------------------------

fit <- orsf(data = pbc_orsf, 
            formula = Surv(time, status) ~ . - id,
            oobag_pred_type = 'surv',
            n_tree = 5,
            oobag_pred_horizon = 2000)

hist(fit$pred_oobag, 
     main = 'Out-of-bag survival predictions at t=2,000')


## -----------------------------------------------------------------------------

# what function is used to evaluate out-of-bag predictions?
fit$eval_oobag$stat_type

# what is the output from this function?
fit$eval_oobag$stat_values


## -----------------------------------------------------------------------------

fit <- orsf(data = pbc_orsf,
            formula = Surv(time, status) ~ . - id,
            n_tree = 20,
            tree_seeds = 2,
            oobag_pred_type = 'surv',
            oobag_pred_horizon = 2000,
            oobag_eval_every = 1)

plot(
 x = seq(1, 20, by = 1),
 y = fit$eval_oobag$stat_values, 
 main = 'Out-of-bag C-statistic computed after each new tree is grown.',
 xlab = 'Number of trees grown',
 ylab = fit$eval_oobag$stat_type
)

lines(x=seq(1, 20), y = fit$eval_oobag$stat_values)


## -----------------------------------------------------------------------------

oobag_brier_surv <- function(y_mat, w_vec, s_vec){

 # output is numeric vector of length 1
 as.numeric(
  SurvMetrics::Brier(
   object = Surv(time = y_mat[, 1], event = y_mat[, 2]), 
   pre_sp = s_vec,
   # t_star in Brier() should match oob_pred_horizon in orsf()
   t_star = 2000
  )
 )
 
}

## -----------------------------------------------------------------------------

oobag_brier_surv(y_mat = pbc_orsf[,c('time', 'status')],
                 s_vec = fit$pred_oobag)


## -----------------------------------------------------------------------------

# instead of copy/pasting the modeling code and then modifying it,
# you can just use orsf_update.

fit_brier <- orsf_update(fit, oobag_fun = oobag_brier_surv)

plot(
 x = seq(1, 20, by = 1),
 y = fit_brier$eval_oobag$stat_values, 
 main = 'Out-of-bag error computed after each new tree is grown.',
 sub = 'For the Brier score, lower values indicate more accurate predictions',
 xlab = 'Number of trees grown',
 ylab = "Brier score"
)

lines(x=seq(1, 20), y = fit_brier$eval_oobag$stat_values)


