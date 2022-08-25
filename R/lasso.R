#' lasso
#'
#' @importFrom glmnet predict.glmnet cv.glmnet
#' @importFrom stats predict
#' @keywords internal
lasso <- function(x = NULL,
                        y = NULL,
                        data = NULL,
                  newX = NULL,
                  newX0 = NULL,
                  newX1 = NULL,
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
    lfit <- glmnet::cv.glmnet(x = x, y = y, relax = relax, grouped=FALSE, familiy = 'binomial', ...)
  } else {
    lfit <- glmnet::cv.glmnet(x = x, y = y, relax = relax, grouped=FALSE, ...)
  }

  if (!is.null(newX)) {
    yhat <- predict(lfit, newx = newX, type = 'response', s = 'lambda.min')
    lfit$yhat <- yhat
  }
  if (!is.null(newX0)) {
    yhat0 <- predict(lfit, newx = newX0, type = 'response', s = 'lambda.min')
    lfit$yhat0 <- yhat0
  }
  if (!is.null(newX1)) {
    yhat1 <- predict(lfit, newx = newX1, type = 'response', s = 'lambda.min')
    lfit$yhat1 <- yhat1
  }

  return(lfit)
}