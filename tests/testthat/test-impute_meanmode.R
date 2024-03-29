

pbc_miss <- as.data.table(survival::pbc) %>%
 .[status > 0, status := status - 1] %>%
 .[, stage := factor(stage, ordered = TRUE)] %>%
 .[, trt := factor(trt,
                   levels = c(1, 2),
                   labels = c('d_penicill_main',
                              'placebo'))] %>%
 collapse::ftransformv(vars = c(ascites, hepato, spiders, edema),
                       FUN = factor)

pbc_miss$id <- NULL

fit_miss <- orsf(pbc_miss,
                 tree_seeds = seeds_standard,
                 n_tree = n_tree_test,
                 formula = time + status ~ .,
                 na_action = 'impute_meanmode')

test_that(
 desc = "missingness rule is passed to pd / vi functions and retained in object",
 code = {

  skip_on_cran()
  pd_miss <- orsf_pd_oob(fit_miss, pred_spec_auto(ascites))
  vi_miss <- orsf_vi(fit_miss, importance = 'permute')

  expect_equal(fit_miss$na_action, 'impute_meanmode')

 }
)


impute_values <- c(fit_miss$get_means(),
                   fit_miss$get_modes())

pbc_imputed <- data_impute(data = pbc_miss,
                           cols = names(pbc_miss),
                           values = impute_values)

test_that(
 desc = 'imputation does not modify column types',
 code = {
  for(i in seq(ncol(pbc_imputed))){
   expect_equal(typeof(pbc_imputed[[i]]), typeof(pbc_miss[[i]]))
  }
 }
)


fit_imputed <- orsf(pbc_imputed,
                    tree_seeds = seeds_standard,
                    n_tree = n_tree_test,
                    formula = time + status ~ .)

test_that(
 desc = "imputed integers are floor(mean)",
 code = {
  expect_equal(
   pbc_imputed$chol[is.na(pbc_miss$chol)][1],
   floor(mean(pbc_miss$chol, na.rm = TRUE))
  )
 }
)

test_that(
 desc = "imputed doubles are mean",
 code = {
  expect_equal(
   pbc_imputed$alk.phos[is.na(pbc_miss$alk.phos)][1],
   mean(pbc_miss$alk.phos, na.rm = TRUE)
  )
 }
)

test_that(
 desc = "imputed modes",
 code = {
  expect_equal(
   as.integer(pbc_imputed$stage[is.na(pbc_miss$stage)][1]),
   as.integer(impute_values['stage'])
  )
 }
)


test_that(
 desc = 'imputation does not modify user-facing data',
 code = {
  expect_equal(
   sum(complete.cases(fit_miss$data)),
   sum(complete.cases(pbc_miss))
  )
 }
)

# Note: eval_oobag is not the same on ubuntu,
# maybe because fit_imputed x is scaled using imputed values?

test_that(
 desc = 'fit with impute is identical to fit on imputed',
 code = {
  skip_on_os(os = 'linux')
  skip_on_cran()
  expect_equal_leaf_summary(fit_miss, fit_imputed)
  expect_equal(fit_miss$n_obs, fit_imputed$n_obs)
 }
)

test_that(
 desc = "imputation does not coerce columns to new types",
 code = {
  for(i in names(pbc_miss)){
   expect_equal(
    typeof(pbc_miss[[i]]),
    typeof(fit_miss$data[[i]])
   )
  }
 }
)


test_that(
 desc = "integer cols imputed by coercing imputed value to integer",
 code = {
  chol_na <- collapse::whichNA(pbc_miss$chol)
  expect_equal(
   pbc_imputed$chol[chol_na],
   rep(as.integer(impute_values['chol']), length(chol_na))
  )
 }
)

test_that(
 desc = "factor cols imputed with the level corresponding to stored int",
 code = {
  trt_na <- collapse::whichNA(pbc_miss$trt)
  expect_true(
   all(pbc_imputed$trt[trt_na] == levels(pbc_miss$trt)[impute_values['trt']])
  )
 }
)

test_that(
 desc = "doubles imputed with the stored double",
 code = {
  alk_na <- collapse::whichNA(pbc_miss$alk.phos)
  expect_true(
   all(pbc_imputed$alk.phos[alk_na] == impute_values["alk.phos"])
  )
 }
)

test_that(
 desc = 'dimensions of predicted output match expectations',
 code = {

  pred_omit <- predict(fit_miss,
                       new_data = pbc_miss,
                       na_action = 'omit')

  expect_equal(
   nrow(pred_omit),
   sum(complete.cases(pbc_miss))
  )

  pred_pass <- predict(fit_miss,
                       new_data = pbc_miss,
                       na_action = 'pass')

  expect_equal(
   complete.cases(pred_pass),
   complete.cases(pbc_miss)
  )

  pred_impute <- predict(fit_miss,
                         new_data = pbc_miss,
                         na_action = 'impute_meanmode')

  expect_equal(
   sum(complete.cases(pred_impute)),
   nrow(pbc_miss)
  )

 }
)
