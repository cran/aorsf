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
             control = orsf_control_fast(), 
             n_tree = 5)
)

time_net <- system.time(
 expr = orsf(pbc_orsf, 
             formula = time+status~. -id, 
             control = orsf_control_net(), 
             n_tree = 5)
)

# control_fast() is much faster
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
     na_action = 'na_impute_meanmode',
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


