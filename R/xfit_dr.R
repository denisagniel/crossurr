xfit_dr <- function(ds,
                    x,
                    y,
                    a,
                    K = 5,
                    outcome_learners = NULL,
                    ps_learners = outcome_learners,
                    interaction_model = TRUE,
                    trim_at = 0.05,
                    outcome_family = gaussian(),
                    mthd = 'superlearner',
                    ncores = parallel::detectCores()-1,
                    ...) {
  if(mthd == 'parametric') {
    out_mthd <- 'ols'
    ps_mthd <- 'logistic'
  } else {
    out_mthd <- ps_mthd <- mthd
  }
  if (mthd == 'cal') {
    cal_fit <- RCAL::ate.regu.cv(fold = c(K, K),
                                 nrho = c(11, 11),
                                 y = ds %>% pull(y),
                                 tr = ds %>% pull(a),
                                 x = ds %>% select(any_of(x)) %>% as.matrix
                                 )
    out_ds <- ds %>%
      mutate(mu1 = cal_fit$mfo[,2],
             mu0 = cal_fit$mfo[,1],
             pi1 = cal_fit$mfp[,2],
             pi0 = cal_fit$mfp[,1])
  } else {
    if (interaction_model) {
      mu0 <- xfit(ds = ds,
                  x = x,
                  y = y,
                  a = a,
                  out_name = 'mu0',
                  K = K,
                  control_only = TRUE,
                  mthd = out_mthd,
                  outcome_family = outcome_family,
                  learners = outcome_learners,
                  ncores = ncores, ...) %>%
        select(-fold)
      mu1 <- xfit(ds = ds,
                  x = x,
                  y = y,
                  a = a,
                  out_name = 'mu1',
                  K = K,
                  case_only = TRUE,
                  mthd = out_mthd,
                  outcome_family = outcome_family,
                  learners = outcome_learners,
                  ncores = ncores, ...)%>%
        select(-fold)
    } else {
      mu <- xfit(ds = ds,
                 x = c(x, a),
                 y = y,
                 a = a,
                 out_name = 'mu',
                 K = K,
                 mthd = out_mthd,
                 outcome_family = outcome_family,
                 learners = outcome_learners,
                 predict_both_arms = TRUE,
                 ncores = ncores, ...)%>%
        select(-fold)
    }
    ps <- xfit(ds = ds,
               x = x,
               y = a,
               K = K,
               mthd = ps_mthd,
               out_name = 'pi',
               learners = ps_learners,
               ps_fit = TRUE,
               outcome_family = outcome_family,
               ncores = ncores, ...) %>%
      select(-fold)

    if (trim_at != 0) {
      ps <- ps %>%
        mutate(pi1 = case_when(pi < trim_at ~ trim_at,
                               pi > 1 - trim_at ~ 1 - trim_at,
                               TRUE ~ pi),
               pi0 = 1 - pi1)
    } else {
      ps <- ps %>%
        mutate(pi1 = pi,
               pi0 = 1 - pi1)
    }
    # browser()
    if (!interaction_model) {
      out_ds <- mu %>%
        inner_join(ps, by = colnames(ds))
    } else {
      out_ds <- mu0 %>%
        inner_join(mu1, by = colnames(ds)) %>%
        inner_join(ps, by = colnames(ds))
    }
  }

  #browser()
  out_ds <- out_ds %>%
    mutate(u_i = mu1 - mu0 +
             (!!sym(a))*((!!sym(y)) - mu1)/pi1 -
             (1-(!!sym(a)))*((!!sym(y))-mu0)/pi0,
           u_i1 = mu1 + (!!sym(a))*((!!sym(y)) - mu1)/pi1,
           u_i0 = mu0 + (1-(!!sym(a)))*((!!sym(y))-mu0)/pi0,
           om_u_i = mu1 - mu0,
           ipw_u_i = (!!sym(a))*(!!sym(y))/pi1 -
             (1-(!!sym(a)))*(!!sym(y))/pi0)
  n <- nrow(out_ds)
  out_ds %>%
    summarise(estimate = mean(u_i),
              E_Y1 = mean(u_i1),
              E_Y0 = mean(u_i0),
              # sigmasq = mean(u_i^2),
              se = sqrt(mean(u_i^2))/sqrt(n),
              om_est = mean(om_u_i),
              om_se = sqrt(mean(om_u_i^2)/n),
              ipw_est = mean(ipw_u_i),
              ipw_se = sqrt(mean(ipw_u_i^2)/n),
              risk_ratio = E_Y1/E_Y0,
              rr_se = sqrt(mean(u_i1^2/E_Y0^2 + E_Y1^2/E_Y0^4*u_i0^2 - 2*E_Y1/E_Y0^3*u_i1*u_i0)/n),
              observation_data = list(out_ds %>%
                                        select(u_i,
                                               mu1,
                                               mu0,
                                               pi1,
                                               pi0,
                                               a,
                                               y)))
}