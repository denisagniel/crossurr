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
                 # both_arms = FALSE,
                    ...) {
  if (outcome_family$family == 'binomial') {
    ps_fit <- TRUE
  }
  set.seed(seed)
  n <- nrow(ds)
  foldn <- rep(1:K, ceiling(n/K))[1:n]
  dsf <- ds %>%
    mutate(fold = sample(foldn))
  test_l <- map(1:K, function(i) {
    # browser()
    # print(glue('Fitting fold {i}...'))
    train_ds <- dsf %>%
      filter(fold != i)
    if (case_only) {
      train_ds <- train_ds %>%
        filter(!!sym(a) == 1)
    } else if (control_only) {
      train_ds <- train_ds %>%
        filter(!!sym(a) == 0)
    }
    test_ds <- dsf %>%
      filter(fold == i)

    fold_fit <- fold_fit(x = x,
                         y = y,
                         train_data = train_ds,
                         test_data = test_ds,
                         mthd = mthd,
                         ps_fit = ps_fit,
                         ...)
      test_ds %>%
        mutate(!!out_name := fold_fit$yhat)



    # }
  })
  test_l %>%
    bind_rows
}

