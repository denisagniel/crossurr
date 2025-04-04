# crossurr <img src="man/figures/logo.png" align="right" height="139" alt="crossurr hex logo" />

<!-- badges: start -->
[![CRAN status](https://www.r-pkg.org/badges/version/crossurr)](https://CRAN.R-project.org/package=crossurr)
[![R-CMD-check](https://github.com/denisagniel/crossurr/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/denisagniel/crossurr/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

`crossurr` is an `R` package that implements cross-fitting for 
doubly robust evaluation of high-dimensional surrogate markers.

You can use these methods to determine how much of the overall treatment effect 
is explained by a (possibly high-dimensional) set of surrogate markers.

More details on the method is available in Agniel D, Hejblum BP, Thiébaut R & 
Parast L (2022), "Doubly robust evaluation of high-dimensional surrogate 
markers", *Biostatistics* <doi:10.1093/biostatistics/kxac020>. 

The main functions of this package are `xf_surrogate` and `xfr_surrogate`