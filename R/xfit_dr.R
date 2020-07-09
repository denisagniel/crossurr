xfit_dr <- function(ds,
                    xvars,
                    yname,
                    aname,
                    K = 5,
                    outcome_learners,
                    ps_learners,
                    interaction_model = TRUE,
                    trim_at = 0.05,
                    outcome_family = gaussian(),
                    method = 'superlearner',
                    ...) {
  yn <- enquo(yname)
  an <- enquo(aname)
  if (interaction_model) {
    if (method == 'superlearner') {
      mu0 <- xfit_sl(ds = ds,
                   xvars = xvars,
                   yname = yn,
                   K = K,
                   out_name = 'mu0',
                   learners = outcome_learners,
                   control_only = TRUE,
                   aname = an,
                   family = outcome_family,
                   ...) %>%
      select(-fold)
    mu1 <- xfit_sl(ds = ds,
                   xvars = xvars,
                   yname = yn,
                   K = K,
                   out_name = 'mu1',
                   learners = outcome_learners,
                   case_only = TRUE,
                   aname = an,
                   family = outcome_family,
                   ...) %>%
      select(-fold)
    } else if (method == 'lasso') {
      mu0 <- xfit_lasso(ds = ds,
                     xvars = xvars,
                     yname = yn,
                     K = K,
                     out_name = 'mu0',
                     control_only = TRUE,
                     aname = an,
                     family = outcome_family,
                     ...) %>%
        select(-fold)
      mu1 <- xfit_lasso(ds = ds,
                     xvars = xvars,
                     yname = yn,
                     K = K,
                     out_name = 'mu1',
                     case_only = TRUE,
                     aname = an,
                     family = outcome_family,
                     ...) %>%
        select(-fold)
    } else stop('currently only methods "lasso" and "superlearner" are supported. please select one of those.')
  } else {
    if (method == 'superlearner') {
      mu <- xfit_sl(ds = ds,
                    xvars = c(xvars, rlang::as_name(an)),
                    yname = yn,
                    K = K,
                    out_name = 'mu',
                    learners = outcome_learners,
                    both_arms = TRUE,
                    aname = an,
                    family = outcome_family,
                    ...) %>%
        select(-fold)
    } else if (method == 'lasso') {
      mu <- xfit_lasso(ds = ds,
                    xvars = c(xvars, rlang::as_name(an)),
                    yname = yn,
                    K = K,
                    out_name = 'mu',
                    both_arms = TRUE,
                    aname = an,
                    family = outcome_family,
                    ...) %>%
        select(-fold)
    } else stop('currently only methods "lasso" and "superlearner" are supported. please select one of those.')

  }
  if (method == 'superlearner') {
    ps <- xfit_sl(ds = ds,
                  xvars = xvars,
                  yname = an,
                  K = K,
                  out_name = 'pi',
                  learners = ps_learners,
                  family = binomial(), ...) %>%
      select(-fold)
  } else if (method == 'lasso') {
    ps <- xfit_lasso(ds = ds,
                  xvars = xvars,
                  yname = an,
                  K = K,
                  out_name = 'pi',
                  learners = ps_learners,
                  family = binomial(), ...) %>%
      select(-fold)
  } else stop('currently only methods "lasso" and "superlearner" are supported. please select one of those.')

  if (trim_at != 0) {
    ps <- ps %>%
      mutate(pi = case_when(pi < trim_at ~ trim_at,
                            pi > 1 - trim_at ~ 1 - trim_at,
                            TRUE ~ pi))
  }
  if (!interaction_model) {
    out_ds <- mu %>%
      inner_join(ps) %>%
      mutate(u_i = mu1 - mu0 +
               (!!an)*((!!yn) - mu1)/pi -
               (1-(!!an))*((!!yn)-mu0)/(1-pi))
  } else {
    out_ds <- mu0 %>%
      inner_join(mu1) %>%
      inner_join(ps) %>%
      mutate(u_i = mu1 - mu0 +
               (!!an)*((!!yn) - mu1)/pi -
               (1-(!!an))*((!!yn)-mu0)/(1-pi))
  }

  out_ds %>%
    summarise(estimate = mean(u_i),
           # sigmasq = mean(u_i^2),
           se = sqrt(mean(u_i^2))/sqrt(nrow(out_ds)),
           observation_data = list(out_ds))
}