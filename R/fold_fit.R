fold_fit <- function(
  x = NULL,
  y = NULL,
  train_data = NULL,
  test_data = NULL,
  mthd = c('superlearner', 'lasso', 'ols', 'logistic'),
  ps_fit = FALSE,
  ...) {

  if (mthd == 'superlearner') {
    test_x <- test_data %>%
      select(all_of(x))
    fit <- super_learn(
      x = x,
      y = y,
      data = train_data,
      newX = test_x,
      ps_fit = ps_fit,
      ...)
  } else if (mthd == 'lasso') {
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
      # family = NULL,
      ...)
  } else if (mthd == 'ols') {
    fit <- ols(x = x,
               y = y,
               data = train_data,
               test_data = test_data,
               ...)
  } else if (mthd == 'logistic') {
    fit <- logistic(x = x,
               y = y,
               data = train_data,
               test_data = test_data,
               ...)
  }
}