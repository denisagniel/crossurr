#'A function for ...
#'
#'
#'@param ds a \code{data.frame}.
#'@param x names any other covariates (eg. age, sex, etc). Default is \code{NULL}.
#'@param s names surrogates
#'@param y names outcome
#'@param a treatment variable name (eg. groups). Expect a binary variable made of \code{1}s and \code{0}s.
#'@param epsilon tuning parameter to control the accuracy of iterative estimation of confidence intervals.
#'@param splits number of data splits to perform when computing confidence intervals.
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
#'@importFrom purrr map map_df
#'@importFrom stats median gaussian
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
             Rm = median(!!sym("R")),
             R_se0 = sqrt(median(!!sym("R_se")^2 + (!!sym("R") - median(!!sym("R")))^2)),
             R_cil0 = !!sym("Rm") - 1.96*!!sym("R_se0"),
             R_cih0 = !!sym("Rm") + 1.96*!!sym("R_se0"),
             Dm = median(!!sym("deltahat")),
             D_se0 = sqrt(median(!!sym("deltahat_se")^2 + (!!sym("deltahat") - median(!!sym("deltahat")))^2)),
             D_cil0 = !!sym("Dm") - 1.96*!!sym("D_se0"),
             D_cih0 = !!sym("Dm") + 1.96*!!sym("D_se0"),
             Dsm = median(!!sym("deltahat_s")),
             Ds_se0 = sqrt(median(!!sym("deltahat_s_se")^2 + (!!sym("deltahat_s") - median(!!sym("deltahat_s")))^2)),
             Ds_cil0 = !!sym("Dsm") - 1.96*!!sym("Ds_se0"),
             Ds_cih0 = !!sym("Dsm") + 1.96*!!sym("Ds_se0"),
           )
}