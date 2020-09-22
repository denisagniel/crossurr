ols <- function(x = NULL,
                  y = NULL,
                  data = NULL,
                  test_data = NULL,
                  ...) {
  fm <- glue::glue('y ~ {paste(x, sep = "", collapse = " + ")}')
  ols_fit <- lm(as.formula(fm), data = data)
  if (!is.null(test_data)) {
    yhat <- predict(ols_fit, newdata = test_data)
    ols_fit$yhat <- yhat
  }

  ols_fit
}

logistic <- function(x = NULL,
                y = NULL,
                data = NULL,
                test_data = NULL,
                ...) {
  fm <- glue::glue('{y} ~ {paste(x, sep = "", collapse = " + ")}')
  logit_fit <- glm(as.formula(fm), data = data, family = binomial)
  if (!is.null(test_data)) {
    yhat <- predict(logit_fit, newdata = test_data, type = 'response')
    logit_fit$yhat <- yhat
  }

  logit_fit
}