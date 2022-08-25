xfit <- function(ds,
                 x,
                 y,
                 a,
                 out_name,
                 K = 5,
                 seed = rnorm(1),
                 case_only = FALSE,
                 control_only = FALSE,
                 mthd = c('superlearner', 'lasso', 'parametric'),
                 ps_fit = FALSE,
                 outcome_family = gaussian(),
                 predict_both_arms = FALSE,
                 ncores = parallel::detectCores()-1,
                 ...) {
  if (outcome_family$family == 'binomial') {
    ps_fit <- TRUE
  }
  set.seed(seed)
  n <- nrow(ds)
  foldn <- rep(1:K, ceiling(n/K))[1:n]
  dsf <- ds %>%
    mutate(fold = sample(foldn))

  test_l <- pbapply::pblapply(1:K, function(i) {
    # browser()
    # message(glue('Fitting fold {i}...'))
    train_ds <- dsf %>%
      filter(!!sym("fold") != i)
    test_ds <- dsf %>%
      filter(!!sym("fold") == i)

    if (case_only) {
      train_ds <- train_ds %>%
        filter(!!sym(a) == 1)

      fit <- fold_fit(x = x,
                      y = y,
                      train_data = train_ds,
                      test_data = test_ds,
                      mthd = mthd,
                      ps_fit = ps_fit,
                      ...)
      test_ds %>%
        mutate(!!out_name := fit$yhat)
    } else if (control_only) {
      train_ds <- train_ds %>%
        filter(!!sym(a) == 0)

      fit <- fold_fit(x = x,
                      y = y,
                      train_data = train_ds,
                      test_data = test_ds,
                      mthd = mthd,
                      ps_fit = ps_fit,
                      ...)
      test_ds %>%
        mutate(!!out_name := fit$yhat)
    } else if (predict_both_arms) {
      test_ds0 <- test_ds %>%
        mutate(!!sym(a) := 0)
      test_ds1 <- test_ds %>%
        mutate(!!sym(a) := 1)

      fit <- fold_fit(x = x,
                      y = y,
                      train_data = train_ds,
                      test_data0 = test_ds0,
                      test_data1 = test_ds1,
                      mthd = mthd,
                      ps_fit = ps_fit,
                      ...)
      test_ds %>%
        mutate(!!glue('{out_name}0') := fit$yhat0,
               !!glue('{out_name}1') := fit$yhat1)
    } else {
      fit <- fold_fit(x = x,
                      y = y,
                      train_data = train_ds,
                      test_data = test_ds,
                      mthd = mthd,
                      ps_fit = ps_fit,
                      ...)
      test_ds %>%
        mutate(!!out_name := fit$yhat)
    }







    # }
  }, cl = ncores)

  return(test_l %>% bind_rows())
}

