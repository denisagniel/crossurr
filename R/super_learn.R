super_learn <- function(x = NULL,
                        y = NULL,
                        data = NULL,
                        learners = c("SL.mean",
                                     "SL.glmnet",
                                     "SL.ranger",
                                     "SL.gbm",
                                     "SL.svm",
                                     "SL.ridge"),
                        ps_fit = FALSE,
                        ...) {
  # if (!inherits(yname, 'quosure')) {
  #   yn <- enquo(yname)
  # } else yn <- yname

  if (!is.null(data)) {
    x <- data %>%
      select(all_of(x))
    y <- data %>%
      pull(!!sym(y))
  }
  if (ps_fit) {
    sl <- SuperLearner::SuperLearner(Y = y,
                                     X = x,
                                     SL.library = learners,
                                     family = binomial,
                                     ...)
  } else {
    sl <- SuperLearner::SuperLearner(Y = y,
                                     X = x,
                                     SL.library = learners,
                                     ...)
  }

  sl$yhat <- sl$SL.predict
  sl
}