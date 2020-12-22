xfr_surrogate <- function(ds,
                          x = NULL,
                          s,
                          y,
                          a,
                          epsilon = NULL,
                          max_splits = 50,
                          K = 5,
                          outcome_learners = NULL,
                          ps_learners = NULL,
                          interaction_model = TRUE,
                          trim_at = 0.05,
                          outcome_family = gaussian(),
                          mthd = 'superlearner',
                          n_ptb = 0,
                          verbose = FALSE,
                          ...) {
  if (is.null(epsilon)) {
    epsilon <- 0.001*sd(ds %>% pull(y))
  }

  ii <- 0
  all_ptb <- list()
  soln_l <- list()
  old_l <- 0
  old_h <- 0

  while(ii < max_splits) {
    ii <- ii + 1
    this_res <- xf_surrogate(
      ds = ds,
      x = x,
      s = s,
      y = y,
      a = a,
      K = K,
      outcome_learners = outcome_learners,
      ps_learners = ps_learners,
      interaction_model = interaction_model,
      trim_at = trim_at,
      outcome_family = outcome_family,
      mthd = mthd,
      n_ptb = n_ptb,
      seed = ii,
      ...)

    all_ptb[[ii]] <- this_res$ptb_ds[[1]]
    soln_l[[ii]] <- this_res %>% select(-ptb_ds)
    current_l <- all_ptb %>% bind_rows %>%
      summarise_all(~quantile(., 0.025))
    current_h <- all_ptb %>% bind_rows %>%
      summarise_all(~quantile(., 0.975))
    diff_l <- abs(current_l - old_l)
    diff_h <- abs(current_h - old_h)
    if (all(diff_l < epsilon) & all(diff_h < epsilon)) {
      break
    } else {
      old_l <- current_l
      old_h <- current_h
    }
    if (verbose) {
      print(tibble(
      iteration = ii,
      R_cil = old_l$R_g,
      R_cih = old_h$R_g,
      D_cil = old_l$deltahat_g,
      D_cih = old_h$deltahat_g,
      Ds_cil = old_l$deltahat_sg,
      Ds_cih = old_h$deltahat_sg,
      dl = max(unlist(diff_l)),
      dh = max(unlist(diff_h))
    ))
    }
  }

  soln_path <- soln_l %>% bind_rows

  soln_path %>%
         summarise(
             Rm = median(R),
             R_se0 = sqrt(median(R_se^2 + (R - median(R))^2)),
             R_cil0 = Rm - 1.96*R_se0,
             R_cih0 = Rm + 1.96*R_se0,
             R_qcil = current_l$R_g,
             R_qcih = current_h$R_g,
             Dm = median(deltahat),
             D_se0 = sqrt(median(deltahat_se^2 + (deltahat - median(deltahat))^2)),
             D_cil0 = Dm - 1.96*D_se0,
             D_cih0 = Dm + 1.96*D_se0,
             D_qcil = current_l$deltahat_g,
             D_qcih = current_h$deltahat_g,
             Dsm = median(deltahat_s),
             Ds_se0 = sqrt(median(deltahat_s_se^2 + (deltahat_s - median(deltahat_s))^2)),
             Ds_cil0 = Dsm - 1.96*Ds_se0,
             Ds_cih0 = Dsm + 1.96*Ds_se0,
             Ds_qcil = current_l$deltahat_sg,
             Ds_qcih = current_h$deltahat_sg
           ) %>%
    mutate(soln_path = list(soln_path))
}