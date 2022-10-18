xfit_sl <- function(ds,
                          xvars,
                          yname,
                        out_name,
                          K = 5,
                          learners = c("SL.mean",
                                       "SL.glmnet",
                                       "SL.ranger",
                                       "SL.gbm",
                                       "SL.svm",
                                       "SL.ridge"),
                          seed = rnorm(1),
                        case_only = FALSE,
                        control_only = FALSE,
                    both_arms = FALSE,
                        aname = NULL,
                        ...) {
  if (!inherits(yname, 'quosure')) {
    yn <- enquo(yname)
  } else yn <- yname
  if (!inherits(aname, 'quosure')) {
    an <- enquo(aname)
  } else an <- aname
  set.seed(seed)
  n <- nrow(ds)
  foldn <- rep(1:K, ceiling(n/K))[1:n]
  dsf <- ds %>%
    mutate(fold = sample(foldn))
  test_l <- map(1:K, function(i) {
    # browser()
    print(glue('Fitting fold {i}...'))
    train_ds <- dsf %>%
      filter(!!sym("fold") != i)
    if (case_only) {
      train_ds <- train_ds %>%
        filter(!!an == 1)
    } else if (control_only) {
      train_ds <- train_ds %>%
        filter(!!an == 0)
    }
    test_ds <- dsf %>%
      filter(!!sym("fold") == i)

    slf <- superlearn_fold(xvars = xvars,
                           yname = yn,
                           learners = learners,
                           train_data = train_ds,
                           test_data = test_ds,
                           ...)
    if (both_arms) {
      out_name1 <- glue('{out_name}1')
      out_name0 <- glue('{out_name}0')
      test_ds %>%
        mutate(!!out_name1 := predict(slf, newdata = test_ds %>%
                                        mutate(!!(an) == 1))$pred[,1],
               !!out_name0 := predict(slf, newdata = test_ds %>%
                                        mutate(!!(an) == 0))$pred[,1],)
    } else {
      test_ds %>%
        mutate(!!out_name := slf$SL.predict)
    }

  })
  out_ds <- test_l %>%
    bind_rows
  if (nrow(out_ds) != n) browser()
  out_ds
}

superlearn_fold <- function(xvars = NULL,
                            yname = NULL,
                            train_data = NULL,
                            test_data = NULL,
                            learners = c("SL.mean",
                                         "SL.glmnet",
                                         "SL.ranger",
                                         "SL.gbm",
                                         "SL.svm",
                                         "SL.ridge"),
                            ...) {
  if (!inherits(yname, 'quosure')) {
    yn <- enquo(yname)
  } else yn <- yname
  test_x <- test_data %>%
    select(all_of(xvars))
  sl_fit <- super_learn(xvars = xvars,
                        yname = yn,
                        data = train_data,
                        newX = test_x,
                        learners = learners,...)
  print(sl_fit)
  sl_fit
}