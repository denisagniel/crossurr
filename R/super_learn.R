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
  if (!is.null(data)) {
    x <- data %>%
      select(vars(xvars))
    y <- data %>%
      pull(yname)
  }
  SuperLearner::SuperLearner(Y = y,
                             X = x,
                             SL.library = learners,
                             ...)
}