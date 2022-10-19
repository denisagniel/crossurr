#' A function for estimating the proportion of treatment effect explained using repeated cross-fitting.
#'
#'
#'@param ds a \code{data.frame}.
#'@param x names of all covariates in \code{ds} that should be included to control for confounding (eg. age, sex, etc). Default is \code{NULL}.
#'@param s names of surrogates in \code{ds}.
#'@param y name of the outcome in \code{ds}.
#'@param a treatment variable name (eg. groups). Expect a binary variable made of \code{1}s and \code{0}s.
#'@param splits number of data splits to perform.
#'@param K number of folds for cross-fitting. Default is \code{5}.
#'@param outcome_learners string vector indicating learners to be used for estimation of the outcome function (e.g., \code{"SL.ridge"}). See the SuperLearner package for details.
#'@param ps_learners string vector indicating learners to be used for estimation of the propensity score function (e.g., \code{"SL.ridge"}). See the SuperLearner package for details.
#'@param interaction_model logical indicating whether outcome functions for treated and control should be estimated separately. Default is \code{TRUE}.
#'@param trim_at threshold at which to trim propensity scores. Default is \code{0.05}.
#'@param outcome_family default is \code{'gaussian'} for continuous outcomes. Other choice is \code{'binomial'} for binary outcomes.
#'@param mthd selected regression method. Default is \code{'superlearner'}, which uses the \code{SuperLearner} package for estimation. Other choices include \code{'lasso'} (which uses \code{glmnet}), \code{'sis'} (which uses \code{SIS}), \code{'cal'} (which uses \code{RCAL}).
#'@param n_ptb Number of perturbations. Default is \code{0} which means asymptotic standard errors are used.
#'@param ... additional parameters (in particular for super_learner)
#'
#' @return a \code{tibble} with columns: \itemize{
#'    \item \code{Rm}: estimate of the proportion of treatment effect explained, computed as the median over the repeated splits.
#'    \item \code{R_se0} standard error for the PTE, accounting for the variability due to splitting.
#'    \item \code{R_cil0} lower confidence interval value for the PTE.
#'    \item \code{R_cih0} upper confidence interval value for the PTE.
#'    \item \code{Dm}: estimate of the overall treatment effect,  computed as the median over the repeated splits.
#'    \item \code{D_se0} standard error for the overall treatment effect, accounting for the variability due to splitting.
#'    \item \code{D_cil0} lower confidence interval value for the overall treatment effect.
#'    \item \code{D_cih0} upper confidence interval value for the overall treatment effect.
#'    \item \code{Dsm}: estimate of the residual treatment effect, computed as the median over the repeated splits.
#'    \item \code{Ds_se0} standard error for the residual treatment effect, accounting for the variability due to splitting.
#'    \item \code{Ds_cil0} lower confidence interval value for the residual treatment effect.
#'    \item \code{Ds_cih0} upper confidence interval value for the residual treatment effect.
#'    }
#'
#'@examples
#'
#' Delta <- 2.25
#' Delta_s <- Delta*0.5
#'
#' n <- 100
#' Delta <- 2.25
#' Delta_s <- Delta*0.5
#' q <- 2
#' p <- 20
#' sig <- 1/sqrt(p)
#' sig_s<- 3
#' Sigma <- matrix(0.2, p, p) + (1-0.2)*diag(p)
#'
#' x1 <- runif(n, -2, 5)
#' x2 <- rbinom(n, prob = 0.5, size = 1)
#' x <- cbind(x1, x2)
#' a <- rbinom(n, prob = plogis(-x1 + 2*x1*x2), size = 1)
#' s_spm <- matrix(rep(1:0, c(10, p - 10)), n, p, byrow = TRUE)
#' s_1 <-  1.5 + (x1 + x2)*s_spm + matrix(rnorm(n*p), n, p)*sig_s
#' s_0 <- 2 + x2*s_spm - x1*x2 + matrix(rnorm(n*p), n, p)*sig_s
#' s <- s_1*a + (1-a)*s_0
#' y_1 <- Delta_s + x[,1] + x[,2] + rowMeans(s_1[,1:15]) + rnorm(n, sd = sig)
#' y_0 <- x[,1] + x[,2] + rowMeans(s_0[,1:15]) + rnorm(n, sd = sig)
#' y <- y_1*a + (1-a)*y_0
#' dsi <- tibble(
#'  id = 1:n,
#'  a, y
#')
#' sds <- tibble(
#'  id = rep(1:n, p),
#'  s = c(s),
#'  sn = glue('s.{rep(1:p, each = n)}')
#')
#'xds <- tibble(
#'  id = rep(1:n, 2),
#'  x = c(x),
#'  xn = glue('x.{rep(1:2, each = n)}')
#') %>%
#'  tidyr::spread(xn, x)
#'
#'ds <- dsi %>%
#'  inner_join(sds)
#'wds <- ds %>%
#'  tidyr::spread(sn, s) %>%
#'  inner_join(xds)
#'
#'
#' lasso_est <- xfr_surrogate(ds = wds,
#' x = paste('x.', 1:q, sep =''),
#' s = paste('s.', 1:p, sep =''),
#' a = 'a',
#' y = 'y',
#' splits = 2,
#' K = 2,
#' trim_at = 0.01,
#' mthd = 'lasso',
#' ncores = 1)
#'
#'@importFrom purrr map map_df
#'@importFrom stats median gaussian
#'@importFrom tidyr spread
#'
#'@export
xfr_surrogate <- function(ds,
                          x = NULL,
                          s,
                          y,
                          a,
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
             R_se0 = sqrt(median((!!sym("R_se"))^2 + (!!sym("R") - median(!!sym("R")))^2)),
             R_cil0 = !!sym("Rm") - 1.96*!!sym("R_se0"),
             R_cih0 = !!sym("Rm") + 1.96*!!sym("R_se0"),
             Dm = median(!!sym("deltahat")),
             D_se0 = sqrt(median((!!sym("deltahat_se"))^2 + (!!sym("deltahat") - median(!!sym("deltahat")))^2)),
             D_cil0 = !!sym("Dm") - 1.96*!!sym("D_se0"),
             D_cih0 = !!sym("Dm") + 1.96*!!sym("D_se0"),
             Dsm = median(!!sym("deltahat_s")),
             Ds_se0 = sqrt(median((!!sym("deltahat_s_se"))^2 + (!!sym("deltahat_s") - median(!!sym("deltahat_s")))^2)),
             Ds_cil0 = !!sym("Dsm") - 1.96*!!sym("Ds_se0"),
             Ds_cih0 = !!sym("Dsm") + 1.96*!!sym("Ds_se0"),
           )
}