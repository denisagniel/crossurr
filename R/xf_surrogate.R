#'
#'@param ds a \code{data.frame}.
#'@param x names any other covariates (eg. age, sex, etc). Default is \code{NULL}.
#'@param s names surrogates
#'@param y names outcome
#'@param a treatment variable name (eg. groups). Expect a binary variable made of \code{1}s and \code{0}s.
#'@param K number of folds for cross validation. Default is \code{5}
#'@param outcome_learners Default is \code{NULL}
#'@ps_learners only used for the superlearner
#'@interaction_model TRUE
#'@param trim_at 0.05
#'@param outcome_family gaussian
#'@param Default is \code{'superlearner'}
#'@param Number of perturbations. Default is \code{0} which means asymptotics
#'
#'@export
xf_surrogate <- function(ds,
                         x = NULL,
                         s,
                         y,
                         a,
                         K = 5,
                         outcome_learners = NULL,
                         ps_learners = NULL,
                         interaction_model = TRUE,
                         trim_at = 0.05,
                         outcome_family = gaussian(),
                         mthd = 'superlearner',
                         n_ptb = 0,
                         ...) {
  if (is.null(x)) {
    delta_s_fit <- xfit_dr(ds = ds,
                           x = s,
                           y = y,
                           a = a,
                           K = K,
                           outcome_learners = outcome_learners,
                           ps_learners = ps_learners,
                           interaction_model = interaction_model,
                           trim_at = trim_at,
                           outcome_family = outcome_family,
                           mthd = mthd, ...)
    ybar1 <- ds %>%
      filter(!!sym(a) == 1) %>%
      mutate(ybar = !!sym(y)) %>%
      pull(ybar) %>% mean()
    ybar0 <- ds %>%
      filter(!!sym(a) == 0) %>%
      mutate(ybar = !!sym(y)) %>%
      pull(ybar) %>% mean()

  } else {
    delta_s_fit <- xfit_dr(ds = ds,
                           x = c(x, s),
                           y = y,
                           a = a,
                           K = K,
                           outcome_learners = outcome_learners,
                           ps_learners = ps_learners,
                           interaction_model = interaction_model,
                           trim_at = trim_at,
                           outcome_family = outcome_family,
                           mthd = mthd, ...)
    delta_fit <- xfit_dr(ds = ds,
                         x = x,
                         y = y,
                         a = a,
                         K = K,
                         outcome_learners = outcome_learners,
                         ps_learners = ps_learners,
                         interaction_model = interaction_model,
                         trim_at = trim_at,
                         outcome_family = outcome_family,
                         mthd = mthd, ...)
    deltahat <- delta_fit$estimate
    u1 <- delta_fit$observation_data[[1]]$u_i
  }
  n <- nrow(ds)



  u2 <- delta_s_fit$observation_data[[1]]$u_i

  deltahat_s <- delta_s_fit$estimate

  if (n_ptb > 0) {
    g_ptb <- purrr::map(1:n_ptb, function(i) sqrt(12)*rbeta(n, shape1 = 1,
                                                     shape2 = 1) - sqrt(12)/2 + 1)
    ptb_ds <- purrr::map(g_ptb, function(g) {
      deltahat_g <- mean(u1*g)
      deltahat_sg <- mean(u2*g)
      R_g <- 1 - deltahat_sg/deltahat_g
      return(tibble(
        R_g,
        deltahat_sg,
        deltahat_g
      ))
    }) %>%
      bind_rows()
    ptb_out <- ptb_ds %>%
      summarise(R_qci_l = quantile(R_g, 0.025),
                R_qci_h = quantile(R_g, 0.975),
                R_ptb_se = sd(R_g)) %>%
      mutate(ptb_ds = list(ptb_ds))
  }
  sigmasq <- mean(u1^2/deltahat^2 + u2^2*deltahat_s^2/deltahat^4 - 2*u1*u2*deltahat_s/deltahat^3)
  sigmasq_diff <- mean(u1^2 + u2^2 - 2*u1*u2)
  out <- tibble(
    R = 1 - deltahat_s/deltahat,
    R_se = sqrt(sigmasq/length(u1)),
    deltahat_s,
    deltahat_s_se = sqrt(mean(u2^2)/length(u2)),
    deltahat,
    deltahat_se = sqrt(mean(u1^2)/length(u1)),
    delta_diff = deltahat - deltahat_s,
    dd_se = sqrt(sigmasq_diff)/length(u1)
  )
  if (n_ptb > 0) {
    out %>%
      full_join(ptb_out, by = character())
  } else out
}