## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(aorsf)

## -----------------------------------------------------------------------------

time_fast <- system.time(
 expr = orsf(pbc_orsf, 
             formula = time+status~. -id, 
             n_tree = 5)
)

time_net <- system.time(
 expr = orsf(pbc_orsf, 
             formula = time+status~. -id, 
             control = orsf_control_survival(method = 'net'), 
             n_tree = 5)
)

# unspecified control is much faster
time_net['elapsed'] / time_fast['elapsed']


## -----------------------------------------------------------------------------

# automatically pick number of threads based on amount available

orsf(pbc_orsf, 
     formula = time+status~. -id, 
     n_tree = 5,
     n_thread = 0)


## -----------------------------------------------------------------------------

orsf(pbc_orsf, 
     formula = time+status~., 
     n_thread = 0, 
     n_tree = 5, 
     n_retry = 0,
     oobag_pred_type = 'none', 
     importance = 'none',
     split_min_events = 20, 
     leaf_min_events = 10,
     split_min_stat = 10)


## -----------------------------------------------------------------------------

verbose_fit <- orsf(pbc_orsf, 
                    formula = time+status~. -id, 
                    n_tree = 5, 
                    verbose_progress = TRUE)


## -----------------------------------------------------------------------------

fit_spec <- orsf(pbc_orsf, 
                 formula = time+status~. -id, 
                 control = orsf_control_survival(method = 'net'), 
                 n_tree = 2000,
                 no_fit = TRUE)

# how much time it takes to estimate training time:
system.time(
 time_est <- orsf_time_to_train(fit_spec, n_tree_subset = 5)
)

# the estimated training time:
time_est


