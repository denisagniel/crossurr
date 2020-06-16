super_learn <- function(x = NULL,
                        y = NULL,
                        xvars = NULL,
                        yname = NULL,
                        data = NULL,
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

  if (!is.null(data)) {
    x <- data %>%
      select(all_of(xvars))
    y <- data %>%
      pull(!!yn)
  }
  SuperLearner::SuperLearner(Y = y,
                             X = x,
                             SL.library = learners,
                             ...)
}