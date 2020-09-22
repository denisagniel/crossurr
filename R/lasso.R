lasso <- function(x = NULL,
                        y = NULL,
                        xvars = NULL,
                        yname = NULL,
                        data = NULL,
                  newX = NULL,
                  relax = TRUE,
                  ps_fit = FALSE,
                        ...) {
  # if (!inherits(yname, 'quosure')) {
  #   yn <- enquo(yname)
  # } else yn <- yname

  if (!is.null(data)) {
    x <- data %>%
      select(all_of(x)) %>%
      as.matrix
    y <- data %>%
      pull(!!sym(y))
  }
  if (ps_fit) {
    lfit <- glmnet::cv.glmnet(x = x, y = y, relax = relax, familiy = 'binomial', ...)
  } else {
    lfit <- glmnet::cv.glmnet(x = x, y = y, relax = relax, ...)
  }

  if (!is.null(newX)) {
    yhat <- predict(lfit, newx = newX, type = 'response', s = 'lambda.min')
    lfit$yhat <- yhat
  }

  lfit
}