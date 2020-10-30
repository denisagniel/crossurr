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

  u1 <- delta_fit$observation_data[[1]]$u_i
  u2 <- delta_s_fit$observation_data[[1]]$u_i
  deltahat <- delta_fit$estimate
  deltahat_s <- delta_s_fit$estimate
  sigmasq <- mean(u1^2/deltahat^2 + u2^2*deltahat_s^2/deltahat^4 - 2*u1*u2*deltahat_s/deltahat^3)
  tibble(
    R = 1 - deltahat_s/deltahat,
    R_se = sqrt(sigmasq/length(u1)),
    deltahat_s, deltahat
  )
}