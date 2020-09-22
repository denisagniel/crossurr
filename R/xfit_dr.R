xfit_dr <- function(ds,
                    x,
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
  if(mthd == 'parametric') {
    out_mthd <- 'ols'
    ps_mthd <- 'logistic'
  } else {
    out_mthd <- ps_mthd <- mthd
  }
  if (interaction_model) {
    mu0 <- xfit(ds = ds,
                x = x,
                y = y,
                a = a,
                out_name = 'mu0',
                K = K,
                control_only = TRUE,
                method = out_mthd,
                learners = outcome_learners) %>%
      select(-fold)
    mu1 <- xfit(ds = ds,
                x = x,
                y = y,
                a = a,
                out_name = 'mu1',
                K = K,
                case_only = TRUE,
                method = out_mthd,
                learners = outcome_learners)%>%
      select(-fold)
  } else {
    mu <- xfit(ds = ds,
               x = c(x, a),
               y = y,
               a = a,
               out_name = 'mu',
               K = K,
               method = out_mthd,
               learners = outcome_learners)%>%
      select(-fold)
  }
    ps <- xfit(ds = ds,
                  x = x,
                  y = a,
                  K = K,
               method = ps_mthd,
                  out_name = 'pi',
                  learners = ps_learners,
               ps_fit = TRUE,
               ...) %>%
    select(-fold)

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
               (!!sym(a))*((!!sym(y)) - mu1)/pi -
               (1-(!!sym(a)))*((!!sym(y))-mu0)/(1-pi),
             u_i1 = mu1 + (!!sym(a))*((!!sym(y)) - mu1)/pi,
             u_i0 = mu_0 + (1-(!!sym(a)))*((!!sym(y))-mu0)/(1-pi))
  } else {
    out_ds <- mu0 %>%
      inner_join(mu1) %>%
      inner_join(ps) %>%
      mutate(u_i = mu1 - mu0 +
               (!!sym(a))*((!!sym(y)) - mu1)/pi -
               (1-(!!sym(a)))*((!!sym(y))-mu0)/(1-pi),
             u_i1 = mu1 + (!!sym(a))*((!!sym(y)) - mu1)/pi,
             u_i0 = mu0 + (1-(!!sym(a)))*((!!sym(y))-mu0)/(1-pi))
  }

  out_ds %>%
    summarise(estimate = mean(u_i),
              E_Y1 = mean(u_i1),
              E_Y0 = mean(u_i0),
           # sigmasq = mean(u_i^2),
           se = sqrt(mean(u_i^2))/sqrt(nrow(out_ds)),
           observation_data = list(out_ds %>%
                                     select(u_i,
                                            mu1,
                                            mu0,
                                            pi)))
}