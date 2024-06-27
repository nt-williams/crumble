
<!-- README.md is generated from README.Rmd. Please edit that file -->

> crumble (verb): break or fall apart into small fragments

# crumble

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/crumble)](https://CRAN.R-project.org/package=crumble)
[![License: GPL
v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)

<!-- badges: end -->

### Installation

``` r
remotes::install_github("nt-williams/crumble")
```

### Example

``` r
library(crumble)
library(mlr3extralearners)

data(weight_behavior, package = "mma")

weight_behavior <- na.omit(weight_behavior)

set.seed(453675476)

crumble(
    data = weight_behavior,
    trt = "sports", 
    outcome = "bmi",
    covar = c("age", "sex", "tvhours"),
    mediators = c("exercises", "overweigh"),
    moc = "snack", 
    d0 = \(data, trt) factor(rep(1, nrow(data)), levels = c("1", "2")), 
    d1 = \(data, trt) factor(rep(2, nrow(data)), levels = c("1", "2")), 
    learners = c("mean", "glm", "earth", "ranger"), 
    nn_module = sequential_module(),
    control = crumble_control(crossfit_folds = 1L, zprime_folds = 5L)
)
#> ✔ Permuting Z-prime variables... 5/5 tasks [317ms]
#> ✔ Fitting outcome regressions... 1/1 folds [19.7s]           
#> ✔ Computing alpha n density ratios... 1/1 folds [7.5s]                 
#> ✔ Computing alpha r density ratios... 1/1 folds [7.2s]
#>                                                 
#> ══ Results `crumble()`═══════════════════════════════════════════════
#> 
#> ── E[Y(d1) - Y(d0)] 
#>       Estimate: 1.0453
#>     Std. error: 0.2312
#>         95% CI: (0.5922, 1.4985)
#> 
#> ── Path: A -> Y 
#>       Estimate: 0.039
#>     Std. error: 0.0112
#>         95% CI: (0.0171, 0.0609)
#> 
#> ── Path: A -> Z -> Y 
#>       Estimate: -0.0176
#>     Std. error: 0.0111
#>         95% CI: (-0.0393, 0.0041)
#> 
#> ── Path: A -> Z -> M -> Y 
#>       Estimate: 0.0051
#>     Std. error: 0.0064
#>         95% CI: (-0.0074, 0.0177)
#> 
#> ── Path: A -> M -> Y 
#>       Estimate: 1.0199
#>     Std. error: 0.2392
#>         95% CI: (0.5511, 1.4887)
#> 
#> ── Intermediate Confounding 
#>       Estimate: -0.0011
#>     Std. error: 0.0143
#>         95% CI: (-0.0291, 0.0269)
```
