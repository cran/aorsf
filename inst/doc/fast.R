## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(aorsf)

## -----------------------------------------------------------------------------

data("flchain", package = 'survival')

flc <- flchain
# do this to avoid orsf() throwing an error about time to event = 0
flc <- flc[flc$futime > 0, ]
# modify names 
names(flc)[names(flc) == 'futime'] <- 'time'
names(flc)[names(flc) == 'death'] <- 'status'


## -----------------------------------------------------------------------------
head(flc)

## -----------------------------------------------------------------------------

time_fast <- system.time(
 expr = orsf(flc, time+status~., na_action = 'na_impute_meanmode',
             control = orsf_control_fast(), n_tree = 10)
)

time_net <- system.time(
 expr = orsf(flc, time+status~., na_action = 'na_impute_meanmode',
             control = orsf_control_net(), n_tree = 10)
)

# control_fast() is much faster
time_net['elapsed'] / time_fast['elapsed']


## -----------------------------------------------------------------------------

time_1_thread <- system.time(
 expr = orsf(flc, time+status~., na_action = 'na_impute_meanmode',
             n_thread = 1, n_tree = 500)
)

time_5_thread <- system.time(
 expr = orsf(flc, time+status~., na_action = 'na_impute_meanmode',
             n_thread = 5, n_tree = 500)
)

time_auto_thread <- system.time(
 expr = orsf(flc, time+status~., na_action = 'na_impute_meanmode',
             n_thread = 0, n_tree = 500)
)

# 5 threads and auto thread are both about 3 times faster than one thread

time_1_thread['elapsed'] / time_5_thread['elapsed']
time_1_thread['elapsed'] / time_auto_thread['elapsed']


## -----------------------------------------------------------------------------

time_lightweight <- system.time(
 expr = orsf(flc, time+status~., na_action = 'na_impute_meanmode',
             n_thread = 0, n_tree = 500, n_retry = 0,
             oobag_pred_type = 'none', importance = 'none',
             split_min_events = 20, leaf_min_events = 10,
             split_min_stat = 10)
)

# about two times faster than auto thread with defaults
time_auto_thread['elapsed'] / time_lightweight['elapsed']


## -----------------------------------------------------------------------------

verbose_fit <- orsf(flc, time+status~., 
                    na_action = 'na_impute_meanmode',
                    n_thread = 0, 
                    n_tree = 500, 
                    verbose_progress = TRUE)


