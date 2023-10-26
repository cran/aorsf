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
                 n_tree = 5,
                 formula = Surv(time, status) ~ . - id)

orsf_fit


## ----eval=FALSE---------------------------------------------------------------
#  library(dplyr)
#  
#  orsf_fit <- pbc_orsf |>
#   select(-id) |>
#   orsf(formula = Surv(time, status) ~ .,
#        n_tree = 5)
#  

## -----------------------------------------------------------------------------

orsf_vi_negate(orsf_fit)


## -----------------------------------------------------------------------------
  
orsf_vi_permute(orsf_fit)
  

## -----------------------------------------------------------------------------

orsf_vi_anova(orsf_fit)


## ----eval=FALSE---------------------------------------------------------------
#  
#  orsf_net <- orsf(data = pbc_orsf,
#                   formula = Surv(time, status) ~ . - id,
#                   control = orsf_control_net())
#  

