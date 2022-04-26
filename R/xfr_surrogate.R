#'A function for ...
#'
#'
#'@param ds a \code{data.frame}.
#'@param x names any other covariates (eg. age, sex, etc). Default is \code{NULL}.
#'@param s names surrogates
#'@param y names outcome
#'@param a treatment variable name (eg. groups). Expect a binary variable made of \code{1}s and \code{0}s.
#'@param epsilon tuning parameter to control the accuracy of iterative estimation of confidence intervals.
#'@param max_splits maximum number of data splits to perform when computing confidence intervals.
#'@param K number of folds for cross validation. Default is \code{5}
#'@param outcome_learners Default is \code{NULL}
#'@param ps_learners only used for the superlearner
#'@param interaction_model logical indicating wether an interaction model should be considered.
#'Default is \code{TRUE}
#'@param trim_at Default is \code{0.05}
#'@param outcome_family Default is \code{'gaussian'}. Other choice is \code{'binomial'} for binary outcome.
#'@param mthd Regression method. Default is \code{'superlearner'}, other choice
#'is \code{'lasso'}.
#'@param n_ptb Number of perturbations. Default is \code{0} which means asymptotics
#'@param ... additional parameters (in particular for super_learner)
#'
#'@importFrom purrr map
#'
#'@export
xfr_surrogate <- function(ds,
                          x = NULL,
                          s,
                          y,
                          a,
                          epsilon = NULL,
                          splits = 50,
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
    this_res <- map_df(1:splits, ~xf_surrogate(
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
      seed = .,
      ...))



  this_res %>%
         summarise(
             Rm = median(R),
             R_se0 = sqrt(median(R_se^2 + (R - median(R))^2)),
             R_cil0 = Rm - 1.96*R_se0,
             R_cih0 = Rm + 1.96*R_se0,
             Dm = median(deltahat),
             D_se0 = sqrt(median(deltahat_se^2 + (deltahat - median(deltahat))^2)),
             D_cil0 = Dm - 1.96*D_se0,
             D_cih0 = Dm + 1.96*D_se0,
             Dsm = median(deltahat_s),
             Ds_se0 = sqrt(median(deltahat_s_se^2 + (deltahat_s - median(deltahat_s))^2)),
             Ds_cil0 = Dsm - 1.96*Ds_se0,
             Ds_cih0 = Dsm + 1.96*Ds_se0,
           )
}