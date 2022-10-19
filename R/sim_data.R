#' A simple function to simulate example data.
#'
#'
#'
#'@importFrom stats gaussian quantile rnorm rbeta sd
#'@importFrom tibble tibble
#'@importFrom dplyr pull mutate
#'@importFrom tidyr spread
#'
sim_data <- function(n, p, q) {
   Delta <- 2.25
   Delta_s <- Delta*0.5

   Delta <- 2.25
   Delta_s <- Delta*0.5
   sig <- 1/sqrt(p)
   sig_s<- 3
   Sigma <- matrix(0.2, p, p) + (1-0.2)*diag(p)

   x1 <- runif(n, -2, 5)
   x2 <- rbinom(n, prob = 0.5, size = 1)
   x <- cbind(x1, x2)
   a <- rbinom(n, prob = plogis(-x1 + 2*x1*x2), size = 1)
   s_spm <- matrix(rep(1:0, c(10, p - 10)), n, p, byrow = TRUE)
   s_1 <-  1.5 + (x1 + x2)*s_spm + matrix(rnorm(n*p), n, p)*sig_s
   s_0 <- 2 + x2*s_spm - x1*x2 + matrix(rnorm(n*p), n, p)*sig_s
   s <- s_1*a + (1-a)*s_0
   y_1 <- Delta_s + x[,1] + x[,2] + rowMeans(s_1[,1:15]) + rnorm(n, sd = sig)
   y_0 <- x[,1] + x[,2] + rowMeans(s_0[,1:15]) + rnorm(n, sd = sig)
   y <- y_1*a + (1-a)*y_0
   dsi <- tibble(
    id = 1:n,
    a, y
  )
   sds <- tibble(
    id = rep(1:n, p),
    s = c(s),
    sn = glue('s.{rep(1:p, each = n)}')
  )
  xds <- tibble(
    id = rep(1:n, 2),
    x = c(x),
    xn = glue('x.{rep(1:2, each = n)}')
  ) %>%
    tidyr::spread(xn, x)

  ds <- dsi %>%
    inner_join(sds)
  wds <- ds %>%
    tidyr::spread(sn, s) %>%
    inner_join(xds)

  wds
}