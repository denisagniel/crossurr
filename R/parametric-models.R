#'Ordinary Least Squares
#'
#'@keywords internal
#'@importFrom glue glue
#'@importFrom stats lm as.formula binomial predict glm
ols <- function(x = NULL,
                  y = NULL,
                  data = NULL,
                  test_data = NULL,
                test_data0 = NULL,
                test_data1 = NULL,
                  ...) {
  fm <- glue::glue('y ~ {paste(x, sep = "", collapse = " + ")}')
  ols_fit <- lm(as.formula(fm), data = data)
  if (!is.null(test_data)) {
    yhat <- predict(ols_fit, newdata = test_data)
    ols_fit$yhat <- yhat
  }
  if (!is.null(test_data0)) {
    yhat0 <- predict(ols_fit, newdata = test_data0)
    ols_fit$yhat0 <- yhat0
  }
  if (!is.null(test_data1)) {
    yhat1 <- predict(ols_fit, newdata = test_data1)
    ols_fit$yhat1 <- yhat1
  }
  ols_fit
}

logistic <- function(x = NULL,
                y = NULL,
                data = NULL,
                test_data = NULL,
                test_data0 = NULL,
                test_data1 = NULL,
                ...) {
  fm <- glue::glue('{y} ~ {paste(x, sep = "", collapse = " + ")}')
  logit_fit <- glm(as.formula(fm), data = data, family = binomial)
  if (!is.null(test_data)) {
    yhat <- predict(logit_fit, newdata = test_data, type = 'response')
    logit_fit$yhat <- yhat
  }
  if (!is.null(test_data0)) {
    yhat0 <- predict(logit_fit, newdata = test_data0, type = 'response')
    logit_fit$yhat0 <- yhat0
  }
  if (!is.null(test_data1)) {
    yhat1 <- predict(logit_fit, newdata = test_data1, type = 'response')
    logit_fit$yhat1 <- yhat1
  }

  logit_fit
}