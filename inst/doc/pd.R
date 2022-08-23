## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>", 
  fig.height = 5, 
  fig.width = 7
)

library(aorsf)
library(ggplot2)


## -----------------------------------------------------------------------------

library(aorsf)

pred_horizon <- 365.25 * 5

set.seed(329730)

index_train <- sample(nrow(pbc_orsf), 150) 

pbc_orsf_train <- pbc_orsf[index_train, ]
pbc_orsf_test <- pbc_orsf[-index_train, ]

fit <- orsf(data = pbc_orsf_train, 
            formula = Surv(time, status) ~ . - id,
            oobag_pred_horizon = pred_horizon)

fit


## -----------------------------------------------------------------------------

pd_inb <- orsf_pd_inb(fit, pred_spec = list(bili = 1:5))

pd_inb


## -----------------------------------------------------------------------------

pd_oob <- orsf_pd_oob(fit, pred_spec = list(bili = 1:5))

pd_oob


## -----------------------------------------------------------------------------

pd_test <- orsf_pd_new(fit, 
                       new_data = pbc_orsf_test, 
                       pred_spec = list(bili = 1:5))

pd_test


## -----------------------------------------------------------------------------

set.seed(329730)

fit <- orsf(pbc_orsf, 
            Surv(time, status) ~ . -id,
            oobag_pred_horizon = pred_horizon)


## -----------------------------------------------------------------------------

pd_sex <- orsf_pd_oob(fit, pred_spec = list(sex = c("m", "f")))

pd_sex


## -----------------------------------------------------------------------------

pd_sex_tv <- orsf_pd_oob(fit, pred_spec = list(sex = c("m", "f")),
                         pred_horizon = seq(365, 365*5))

ggplot(pd_sex_tv, aes(x = pred_horizon, y = mean, color = sex)) + 
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

pd_two_vars <-  
 orsf_pd_oob(fit,
             pred_spec = list(sex = c("m", "f"), bili = 1:5),
             expand_grid = FALSE)

pd_two_vars


## -----------------------------------------------------------------------------

pd_smry <- orsf_summarize_uni(fit)

pd_smry


## -----------------------------------------------------------------------------

head(as.data.table(pd_smry))


## -----------------------------------------------------------------------------

pred_spec = list(bili = seq(1, 5, length.out = 20),
               edema = levels(pbc_orsf_train$edema),
               trt = levels(pbc_orsf$trt))

pd_bili_edema <- orsf_pd_oob(fit, pred_spec)

library(ggplot2)

ggplot(pd_bili_edema, aes(x = bili, y = medn, col = trt, linetype = edema)) + 
 geom_line() + 
 labs(y = 'Expected predicted risk')


## -----------------------------------------------------------------------------

library(survival)

pbc_orsf$edema_05 <- ifelse(pbc_orsf$edema == '0.5', 'yes', 'no')

fit_cph <- coxph(Surv(time,status) ~ edema_05 * bili, 
                 data = pbc_orsf)

anova(fit_cph)


## ---- echo = FALSE------------------------------------------------------------

# in case pbc_orsf is used in downstream docs

pbc_orsf$edema_05 <- NULL


## -----------------------------------------------------------------------------

pred_spec <- list(bili = seq(1, 10, length.out = 25))

ice_oob <- orsf_ice_oob(fit, pred_spec, boundary_checks = FALSE)

ice_oob
                     

## -----------------------------------------------------------------------------

ice_oob[, pred := pred - pred[bili==1], by = id_row]


## ---- -orsf_ice---------------------------------------------------------------

library(ggplot2)

ggplot(ice_oob, aes(x = bili, 
                    y = pred, 
                    group = id_row)) + 
 geom_line(alpha = 0.15) + 
 labs(y = 'Change in predicted risk') +
 geom_smooth(se = FALSE, aes(group = 1))


