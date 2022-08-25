#'@importFrom SIS SIS
#'@importFrom stats predict
sis <- function(x = NULL,
                  y = NULL,
                  data = NULL,
                  newX = NULL,
                  newX0 = NULL,
                  newX1 = NULL,
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
    sfit <- SIS::SIS(x = x, y = y, family = 'binomial')
  } else {
    sfit <- SIS::SIS(x = x, y = y)
  }

  if (!is.null(newX)) {
    yhat <- predict(sfit, newx = newX, type = 'response', lambda = sfit$lambda)
    sfit$yhat <- yhat
  }
  if (!is.null(newX0)) {
    yhat0 <- predict(sfit, newx = newX0, type = 'response', lambda = sfit$lambda)
    sfit$yhat0 <- yhat0
  }
  if (!is.null(newX1)) {
    yhat1 <- predict(sfit, newx = newX1, type = 'response', lambda = sfit$lambda)
    sfit$yhat1 <- yhat1
  }

  sfit
}