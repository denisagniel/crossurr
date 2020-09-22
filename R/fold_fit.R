fold_fit <- function(
  x = NULL,
  y = NULL,
  train_data = NULL,
  test_data = NULL,
  method = c('superlearner', 'lasso', 'ols', 'logistic'),
  ps_fit = FALSE,
  ...) {

  if (method == 'superlearner') {
    test_x <- test_data %>%
      select(all_of(x))
    fit <- super_learn(
      x = x,
      y = y,
      data = train_data,
      newX = test_x,
      ps_fit = ps_fit,
      ...)
  } else if (method == 'lasso') {
    test_x <- test_data %>%
      select(all_of(x)) %>%
      as.matrix
    fit <- lasso(
      x = x,
      y = y,
      data = train_data,
      newX = test_x,
      learners = learners,
      ps_fit = ps_fit,
      family = NULL,
      ...)
  } else if (method == 'ols') {
    fit <- ols(x = x,
               y = y,
               data = train_data,
               test_data = test_data,
               ...)
  } else if (method == 'logistic') {
    fit <- logistic(x = x,
               y = y,
               data = train_data,
               test_data = test_data,
               ...)
  }
}