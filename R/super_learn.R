#'@import SuperLearner
#'@keywords internal

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
                        newX0 = NULL,
                        newX1 = NULL,
                        newX = NULL,
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
                                     family = binomial(),
                                     newX = newX,
                                     ...)
    sl$yhat <- sl$SL.predict
  } else if (is.null(newX0) & is.null(newX1)) {
    sl <- SuperLearner::SuperLearner(Y = y,
                                     X = x,
                                     SL.library = learners,
                                     newX = newX,
                                     ...)
    sl$yhat <- sl$SL.predict
  } else {
    sl0 <- SuperLearner::SuperLearner(Y = y,
                                      X = x,
                                      SL.library = learners,
                                      newX = newX0,
                                      ...)
    sl1 <- SuperLearner::SuperLearner(Y = y,
                                      X = x,
                                      SL.library = learners,
                                      newX = newX1,
                                      ...)
    sl <- sl1
    sl$yhat1 <- sl1$SL.predict
    sl$yhat0 <- sl0$SL.predict
  }

  return(sl)
}