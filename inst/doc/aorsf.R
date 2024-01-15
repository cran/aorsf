## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.height = 5, 
  fig.width = 7
)

## -----------------------------------------------------------------------------
library(aorsf)

## -----------------------------------------------------------------------------
# An oblique classification RF
penguin_fit <- orsf(data = penguins_orsf, formula = species ~ .)

penguin_fit


## -----------------------------------------------------------------------------
# An oblique regression RF
bill_fit <- orsf(data = penguins_orsf, formula = bill_length_mm ~ .)

bill_fit


## -----------------------------------------------------------------------------
# An oblique survival RF
pbc_fit <- orsf(data = pbc_orsf, 
                n_tree = 5,
                formula = Surv(time, status) ~ . - id)

pbc_fit


## ----eval=FALSE---------------------------------------------------------------
#  
#  library(dplyr)
#  
#  pbc_fit <- pbc_orsf |>
#   select(-id) |>
#   orsf(formula = Surv(time, status) ~ .,
#        n_tree = 5)
#  

## -----------------------------------------------------------------------------

orsf_vi_negate(pbc_fit)


## -----------------------------------------------------------------------------
  
orsf_vi_permute(penguin_fit)
  

## -----------------------------------------------------------------------------

orsf_vi_anova(bill_fit)


## ----eval=FALSE---------------------------------------------------------------
#  
#  orsf_net <- orsf(data = pbc_orsf,
#                   formula = Surv(time, status) ~ . - id,
#                   control = orsf_control_survival(method = 'net'))
#  

