## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.height = 5, 
  fig.width = 7
)

## -----------------------------------------------------------------------------

library(aorsf)

set.seed(329)

orsf_fit <- orsf(data = pbc_orsf, 
                 formula = Surv(time, status) ~ . - id)

orsf_fit


## ----eval=FALSE---------------------------------------------------------------
#  library(dplyr)
#  
#  orsf_fit <- pbc_orsf |>
#   select(-id) |>
#   orsf(formula = Surv(time, status) ~ .)
#  

## -----------------------------------------------------------------------------

orsf_vi_negate(orsf_fit)


## -----------------------------------------------------------------------------
  
orsf_vi_permute(orsf_fit)
  

## -----------------------------------------------------------------------------

orsf_vi_anova(orsf_fit)


## -----------------------------------------------------------------------------

orsf_net <- orsf(data = pbc_orsf, 
                 formula = Surv(time, status) ~ . - id, 
                 control = orsf_control_net(),
                 n_tree = 50)


## -----------------------------------------------------------------------------

# tracking how long it takes to fit 50 glmnet trees
print(
 t1 <- system.time(
  orsf(data = pbc_orsf, 
       formula = Surv(time, status) ~ . - id, 
       control = orsf_control_net(),
       n_tree = 50)
 )
)

# and how long it takes to fit 50 cph trees
print(
 t2 <- system.time(
  orsf(data = pbc_orsf, 
       formula = Surv(time, status) ~ . - id, 
       control = orsf_control_cph(),
       n_tree = 50)
 )
)

t1['elapsed'] / t2['elapsed']



