xf_surrogate <- function(ds,
                         x,
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
  n <- nrow(ds)


  u1 <- delta_fit$observation_data[[1]]$u_i
  u2 <- delta_s_fit$observation_data[[1]]$u_i
  deltahat <- delta_fit$estimate
  deltahat_s <- delta_s_fit$estimate

  if (n_ptb > 0) {
    g_ptb <- map(1:n_ptb, function(i) sqrt(12)*rbeta(n, shape1 = 1,
                                                     shape2 = 1) - sqrt(12)/2 + 1)
    ptb_ds <- map(g_ptb, function(g) {
      deltahat_g <- mean(u1*g)
      deltahat_sg <- mean(u2*g)
      R_g <- 1 - deltahat_sg/deltahat_g
      return(tibble(
        R_g,
        deltahat_sg,
        deltahat_g
      ))
    })
    ptb_out <- ptb_ds %>%
      bind_rows() %>%
      summarise(R_qci_l = quantile(R_g, 0.025),
                R_qci_h = quantile(R_g, 0.975),
                R_ptb_se = sd(R_g))
  }
  sigmasq <- mean(u1^2/deltahat^2 + u2^2*deltahat_s^2/deltahat^4 - 2*u1*u2*deltahat_s/deltahat^3)
  out <- tibble(
    R = 1 - deltahat_s/deltahat,
    R_se = sqrt(sigmasq/length(u1)),
    deltahat_s, deltahat
  )
  if (n_ptb > 0) {
    out %>%
      cbind(ptb_out)
  } else out
}