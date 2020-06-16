xfit_dr <- function(ds,
                    xvars,
                    yname,
                    aname,
                    K = 5,
                    outcome_learners,
                    ps_learners,
                    trim_at = 0.05,...) {
  yn <- enquo(yname)
  an <- enquo(aname)
  mu0 <- xfit_sl(ds = ds,
                      xvars = xvars,
                      yname = yn,
                      out_name = 'mu0',
                      learners = outcome_learners,
                      control_only = TRUE,
                      aname = an,...)
  mu1 <- xfit_sl(ds = ds,
                     xvars = xvars,
                     yname = yn,
                     out_name = 'mu1',
                     learners = outcome_learners,
                     case_only = TRUE,
                     aname = an,...)
  ps <- xfit_sl(ds = ds,
                    xvars = xvars,
                    yname = an,
                    out_name = 'pi',
                    learners = ps_learners,
                    family = binomial(), ...)
  if (trim_at != 0) {
    ps <- ps %>%
      mutate(pi = case_when(pi < trim_at ~ trim_at,
                            pi > 1 - trim_at ~ 1 - trim_at,
                            TRUE ~ pi))
  }
  out_ds <- mu0 %>%
    inner_join(mu1) %>%
    inner_join(ps) %>%
    mutate(u_i = mu1 - mu0 +
             a*(y - mu1)/pi -
             (1-a)*(y-mu0)/(1-pi))
  out_ds %>%
    summarise(estimate = mean(u_i),
           # sigmasq = mean(u_i^2),
           se = sqrt(mean(u_i^2))/sqrt(n))
}