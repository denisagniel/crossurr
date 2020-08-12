lasso <- function(x = NULL,
                        y = NULL,
                        xvars = NULL,
                        yname = NULL,
                        data = NULL,
                  newX = NULL,
                  relax = TRUE,
                        ...) {
  if (!inherits(yname, 'quosure')) {
    yn <- enquo(yname)
  } else yn <- yname

  if (!is.null(data)) {
    x <- data %>%
      select(all_of(xvars)) %>%
      as.matrix
    y <- data %>%
      pull(!!yn)
  }
  lfit <- glmnet::cv.glmnet(x = x, y = y, relax = relax)
  if (!is.null(newX)) {
    yhat <- predict(lfit, newx = newX, type = 'response', s = 'lambda.min')
    lfit$yhat <- yhat
  }

  lfit
}