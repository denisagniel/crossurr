fold_fit <- function(
  x = NULL,
  y = NULL,
  train_data = NULL,
  test_data = NULL,
  test_data0 = NULL,
  test_data1 = NULL,
  mthd = c('superlearner', 'lasso', 'ols', 'logistic'),
  ps_fit = FALSE,
  learners=NULL,
  ...) {

  if (!is.null(test_data)) {
    if (mthd == 'superlearner') {
      test_x <- test_data %>%
        select(all_of(x))
      fit <- super_learn(
        x = x,
        y = y,
        data = train_data,
        newX0 = NULL,
        newX1 = NULL,
        newX = test_x,
        ps_fit = ps_fit,
        learners = learners,
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
    } else if (mthd == 'sis') {
      test_x <- test_data %>%
        select(all_of(x)) %>%
        as.matrix
      fit <- sis(
        x = x,
        y = y,
        data = train_data,
        newX = test_x,
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
  } else {
    if (mthd == 'superlearner') {
      test_x0 <- test_data0 %>%
        select(all_of(x))
      test_x1 <- test_data1 %>%
        select(all_of(x))
      fit0 <- super_learn(
        x = x,
        y = y,
        data = train_data,
        newX0 = test_x0,
        newX1 = test_x1,
        ps_fit = ps_fit,
        ...)

    } else if (mthd == 'lasso') {
      test_x0 <- test_data0 %>%
        select(all_of(x)) %>%
        as.matrix
      test_x1 <- test_data1 %>%
        select(all_of(x)) %>%
        as.matrix
      fit <- lasso(
        x = x,
        y = y,
        data = train_data,
        newX0 = test_x0,
        newX1 = test_x1,
        ps_fit = ps_fit,
        # family = NULL,
        ...)
    } else if (mthd == 'sis') {
      test_x0 <- test_data0 %>%
        select(all_of(x)) %>%
        as.matrix
      test_x1 <- test_data1 %>%
        select(all_of(x)) %>%
        as.matrix
      fit <- sis(
        x = x,
        y = y,
        data = train_data,
        newX0 = test_x0,
        newX1 = test_x1,
        ps_fit = ps_fit,
        # family = NULL,
        ...)
    } else if (mthd == 'ols') {
      fit <- ols(x = x,
                 y = y,
                 data = train_data,
                 test_data0 = test_data0,
                 test_data1 = test_data1,
                 ...)
    } else if (mthd == 'logistic') {
      fit <- logistic(x = x,
                      y = y,
                      data = train_data,
                      test_data0 = test_data0,
                      test_data1 = test_data1,
                      ...)

    }
  }

}