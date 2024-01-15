## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>", 
  fig.height = 5, 
  fig.width = 7,
  warning = FALSE,
  message = FALSE
)


## -----------------------------------------------------------------------------
library(aorsf)
library(ggplot2)

## -----------------------------------------------------------------------------

set.seed(329)

index_train <- sample(nrow(penguins_orsf), 150) 

penguins_orsf_train <- penguins_orsf[index_train, ]
penguins_orsf_test <- penguins_orsf[-index_train, ]

fit_clsf <- orsf(data = penguins_orsf_train, 
                 formula = species ~ .)


## -----------------------------------------------------------------------------

pred_spec <- list(flipper_length_mm = c(190, 210))

pd_oob <- orsf_pd_oob(fit_clsf, pred_spec = pred_spec)

pd_oob


## -----------------------------------------------------------------------------

sum(pd_oob[flipper_length_mm == 190, mean])


## -----------------------------------------------------------------------------

sum(pd_oob[flipper_length_mm == 190, medn])


## -----------------------------------------------------------------------------

set.seed(329)

index_train <- sample(nrow(penguins_orsf), 150) 

penguins_orsf_train <- penguins_orsf[index_train, ]
penguins_orsf_test <- penguins_orsf[-index_train, ]

fit_regr <- orsf(data = penguins_orsf_train, 
                 formula = bill_length_mm ~ .)


## -----------------------------------------------------------------------------

pred_spec <- list(flipper_length_mm = c(190, 210))

pd_new <- orsf_pd_new(fit_regr, 
                      pred_spec = pred_spec,
                      new_data = penguins_orsf_test)

pd_new


## -----------------------------------------------------------------------------

pred_spec = pred_spec_auto(species, island, body_mass_g)

pd_new <- orsf_pd_new(fit_regr, 
                      pred_spec = pred_spec,
                      new_data = penguins_orsf_test)

pd_new


## -----------------------------------------------------------------------------

pd_new <- orsf_pd_new(fit_regr, 
                      expand_grid = FALSE,
                      pred_spec = pred_spec,
                      new_data = penguins_orsf_test)

pd_new


## -----------------------------------------------------------------------------

custom_pred_spec <- data.frame(species = 'Adelie', 
                               island = 'Biscoe')

pd_new <- orsf_pd_new(fit_regr, 
                      pred_spec = custom_pred_spec,
                      new_data = penguins_orsf_test)

pd_new


## -----------------------------------------------------------------------------

set.seed(329)

index_train <- sample(nrow(pbc_orsf), 150) 

pbc_orsf_train <- pbc_orsf[index_train, ]
pbc_orsf_test <- pbc_orsf[-index_train, ]

fit_surv <- orsf(data = pbc_orsf_train, 
                 formula = Surv(time, status) ~ . - id,
                 oobag_pred_horizon = 365.25 * 5)


## -----------------------------------------------------------------------------
pd_train <- orsf_pd_inb(fit_surv, pred_spec = list(bili = 1:5))
pd_train

## -----------------------------------------------------------------------------
pd_train <- orsf_pd_inb(fit_surv, pred_spec_auto(bili))
pd_train

## -----------------------------------------------------------------------------

pd_train <- orsf_pd_inb(fit_surv, pred_spec_auto(bili),
                        pred_horizon = seq(500, 3000, by = 500))
pd_train


## -----------------------------------------------------------------------------
# a rare case of modify_in_place = TRUE
orsf_update(fit_surv, 
            data = pbc_orsf, 
            modify_in_place = TRUE)

fit_surv


## -----------------------------------------------------------------------------

pd_sex_tv <- orsf_pd_oob(fit_surv, 
                         pred_spec = pred_spec_auto(sex),
                         pred_horizon = seq(365, 365*5))

ggplot(pd_sex_tv) +
 aes(x = pred_horizon, y = mean, color = sex) + 
 geom_line() +
 labs(x = 'Time since baseline',
      y = 'Expected risk')


## -----------------------------------------------------------------------------

library(data.table)

ratio_tv <- pd_sex_tv[
 , .(ratio = mean[sex == 'm'] / mean[sex == 'f']), by = pred_horizon
]

ggplot(ratio_tv, aes(x = pred_horizon, y = ratio)) + 
 geom_line(color = 'grey') + 
 geom_smooth(color = 'black', se = FALSE) + 
 labs(x = 'time since baseline',
      y = 'ratio in expected risk for males versus females')


## -----------------------------------------------------------------------------
pd_smry <- orsf_summarize_uni(fit_surv, n_variables = 4)

pd_smry

## -----------------------------------------------------------------------------
head(as.data.table(pd_smry))

## -----------------------------------------------------------------------------

pred_spec = pred_spec_auto(bili, edema, trt)

pd_bili_edema <- orsf_pd_oob(fit_surv, pred_spec)

ggplot(pd_bili_edema) + 
 aes(x = bili, y = medn, col = trt, linetype = edema) + 
 geom_line() + 
 labs(y = 'Expected predicted risk')


## -----------------------------------------------------------------------------

library(survival)

pbc_orsf$edema_05 <- ifelse(pbc_orsf$edema == '0.5', 'yes', 'no')

fit_cph <- coxph(Surv(time,status) ~ edema_05 * bili, 
                 data = pbc_orsf)

anova(fit_cph)


## ----echo = FALSE-------------------------------------------------------------

# drop so it won't interfere with downstream code

pbc_orsf$edema_05 <- NULL


## -----------------------------------------------------------------------------

vint_scores <- orsf_vint(fit_surv, verbose_progress = TRUE)

vint_scores[1:5]


## -----------------------------------------------------------------------------
fit_cph <- coxph(Surv(time,status) ~ albumin * protime, 
                 data = pbc_orsf)

anova(fit_cph)

fit_cph <- update(fit_cph, . ~ stage * protime)

anova(fit_cph)

fit_cph <- update(fit_cph, . ~ copper * protime)

anova(fit_cph)

## -----------------------------------------------------------------------------

pred_spec <- list(flipper_length_mm = c(190, 210))

ice_oob <- orsf_ice_oob(fit_clsf, pred_spec = pred_spec)

ice_oob


## ----eval=FALSE---------------------------------------------------------------
#  
#  ice_oob %>%
#   .[flipper_length_mm == 190] %>%
#   .[id_row == 1] %>%
#   .[['pred']] %>%
#   sum()
#  

## ----echo=FALSE---------------------------------------------------------------
1

## -----------------------------------------------------------------------------

pred_spec <- list(flipper_length_mm = c(190, 210))

ice_new <- orsf_ice_new(fit_regr, 
                        pred_spec = pred_spec,
                        new_data = penguins_orsf_test)

ice_new


## -----------------------------------------------------------------------------

pred_spec = pred_spec_auto(species, island, body_mass_g)

ice_new <- orsf_ice_new(fit_regr, 
                        pred_spec = pred_spec,
                        new_data = penguins_orsf_test)

ice_new


## -----------------------------------------------------------------------------

ice_new <- orsf_ice_new(fit_regr, 
                        expand_grid = FALSE,
                        pred_spec = pred_spec,
                        new_data = penguins_orsf_test)

ice_new


## -----------------------------------------------------------------------------

custom_pred_spec <- data.frame(species = 'Adelie', 
                               island = 'Biscoe')

ice_new <- orsf_ice_new(fit_regr, 
                        pred_spec = custom_pred_spec,
                        new_data = penguins_orsf_test)

ice_new


## -----------------------------------------------------------------------------
ice_train <- orsf_ice_inb(fit_surv, pred_spec = list(bili = 1:5))
ice_train

## -----------------------------------------------------------------------------
ice_train <- orsf_ice_inb(fit_surv, pred_spec_auto(bili))
ice_train

## -----------------------------------------------------------------------------

ice_train <- orsf_ice_inb(fit_surv, pred_spec_auto(bili),
                          pred_horizon = seq(500, 3000, by = 500))
ice_train


## -----------------------------------------------------------------------------

pred_spec <- list(bili = seq(1, 10, length.out = 25))

ice_oob <- orsf_ice_oob(fit_surv, pred_spec, boundary_checks = FALSE)

ice_oob
                     

## -----------------------------------------------------------------------------

ice_oob[, pred_subtract := rep(pred[id_variable==1], times=25)]
ice_oob[, pred := pred - pred_subtract]


## -----orsf_ice----------------------------------------------------------------

ggplot(ice_oob, aes(x = bili, 
                    y = pred, 
                    group = id_row)) + 
 geom_line(alpha = 0.15) + 
 labs(y = 'Change in predicted risk') +
 geom_smooth(se = FALSE, aes(group = 1))


