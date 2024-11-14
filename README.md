
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

*crumble* implements a modern, unified estimation strategy (Liu et al.
2024) for common mediation estimands: natural effects (Pearl 2022),
organic effects (Lok 2015), interventional effects (Vansteelandt and
Daniel 2017), recanting twins (Vo et al. 2024), in causal inference in
combination with modified treatment policies. It makes use of recent
advancements in “Riesz-learning” to estimate a set of required nuisance
parameters using deep learning. The result is a software package that is
capable of estimating mediation effects with binary, categorical,
continuous, or multivariate exposures with high-dimensional mediators
and mediator-outcome confounders using machine learning.

### Installation

``` r
remotes::install_github("nt-williams/crumble")
```

### Features

| Feature                   | Status  |
|---------------------------|:-------:|
| Recanting twins           |    ✓    |
| Natural effects           |    ✓    |
| Organic effects           |    ✓    |
| Interventional effects    |    ✓    |
| Modified treatment Policy |    ✓    |
| Static intervention       |    ✓    |
| Dynamic intervention      |    ✓    |
| Continuous treatment      |    ✓    |
| Binary treatment          |    ✓    |
| Categorical treatment     |    ✓    |
| Multivariate treatment    |    ✓    |
| Missingness in treatment  |         |
| Continuous outcome        |    ✓    |
| Binary outcome            |    ✓    |
| Censored outcome          |    ✓    |
| Survey weights            | Planned |
| Super learner             |    ✓    |
| Clustered data            | Planned |
| Parallel processing       |    ✓    |
| GPU support               |    ✓    |
| Progress bars             |    ✓    |

### Example(s)

``` r
library(crumble)
library(mlr3extralearners)

data(weight_behavior, package = "mma")

weight_behavior <- na.omit(weight_behavior)

set.seed(2345)
```

##### Recanting twins

``` r
crumble(
    data = weight_behavior,
    trt = "sports", 
    outcome = "bmi",
    covar = c("age", "sex", "tvhours"),
    mediators = c("exercises", "overweigh"),
    moc = "snack",
    d0 = \(data, trt) factor(rep(1, nrow(data)), levels = c("1", "2")), 
    d1 = \(data, trt) factor(rep(2, nrow(data)), levels = c("1", "2")), 
    effect = "RT",
    learners = c("mean", "glm", "earth", "ranger"), 
    nn_module = sequential_module(),
    control = crumble_control(crossfit_folds = 1L, epochs = 20L)
)
#> ✔ Permuting Z-prime variables... 1/1 tasks [2.5s]
#> ✔ Fitting outcome regressions... 1/1 folds [25.6s]             
#> ✔ Computing alpha n density ratios... 1/1 folds [39.7s]        
#> ✔ Computing alpha r density ratios... 1/1 folds [41.6s]        
#> 
#> ══ Results `crumble()` ═════════════════════════════════════════
#> 
#> ── E[Y(d1) - Y(d0)] 
#>       Estimate: 1.0537
#>     Std. error: 0.3009
#>         95% CI: (0.4639, 1.6435)
#> 
#> ── Path: A -> Y 
#>       Estimate: 0.0366
#>     Std. error: 0.1842
#>         95% CI: (-0.3245, 0.3976)
#> 
#> ── Path: A -> Z -> Y 
#>       Estimate: -0.0202
#>     Std. error: 0.0238
#>         95% CI: (-0.0668, 0.0264)
#> 
#> ── Path: A -> Z -> M -> Y 
#>       Estimate: -6e-04
#>     Std. error: 0.0099
#>         95% CI: (-0.02, 0.0189)
#> 
#> ── Path: A -> M -> Y 
#>       Estimate: 1.0506
#>     Std. error: 0.2162
#>         95% CI: (0.627, 1.4743)
#> 
#> ── Intermediate Confounding 
#>       Estimate: -0.0127
#>     Std. error: 0.0261
#>         95% CI: (-0.0638, 0.0384)
```

##### Natural effects

``` r
crumble(
    data = weight_behavior,
    trt = "sports", 
    outcome = "bmi",
    covar = c("age", "sex", "tvhours"),
    mediators = c("exercises", "overweigh"),
    d0 = \(data, trt) factor(rep(1, nrow(data)), levels = c("1", "2")), 
    d1 = \(data, trt) factor(rep(2, nrow(data)), levels = c("1", "2")), 
    effect = "N",
    learners = c("mean", "glm", "earth", "ranger"), 
    nn_module = sequential_module(),
    control = crumble_control(crossfit_folds = 1L, epochs = 20L)
)
#> ✔ Fitting outcome regressions... 1/1 folds [10.6s]             
#> ✔ Computing alpha n density ratios... 1/1 folds [53.1s]        
#> 
#> ══ Results `crumble()` ═════════════════════════════════════════
#> 
#> ── E[Y(d1) - Y(d0)] 
#>       Estimate: 1.0289
#>     Std. error: 0.28
#>         95% CI: (0.48, 1.5777)
#> 
#> ── Natural Direct Effect 
#>       Estimate: 0.0165
#>     Std. error: 0.1717
#>         95% CI: (-0.3201, 0.3531)
#> 
#> ── Natural Indirect Effect 
#>       Estimate: 1.0124
#>     Std. error: 0.2178
#>         95% CI: (0.5856, 1.4393)
```

##### Organic effects

``` r
crumble(
    data = weight_behavior,
    trt = "sports", 
    outcome = "bmi",
    covar = c("age", "sex", "tvhours"),
    mediators = c("exercises", "overweigh"),
    d0 = \(data, trt) factor(rep(1, nrow(data)), levels = c("1", "2")), 
    d1 = \(data, trt) factor(rep(2, nrow(data)), levels = c("1", "2")), 
    effect = "O",
    learners = c("mean", "glm", "earth", "ranger"), 
    nn_module = sequential_module(),
    control = crumble_control(crossfit_folds = 1L, epochs = 20L)
)
#> ✔ Fitting outcome regressions... 1/1 folds [10.7s]             
#> ✔ Computing alpha n density ratios... 1/1 folds [48.2s]        
#> 
#> ══ Results `crumble()` ═════════════════════════════════════════
#> 
#> ── Organic Direct Effect 
#>       Estimate: 0.011
#>     Std. error: 0.1772
#>         95% CI: (-0.3364, 0.3584)
#> 
#> ── Organic Indirect Effect 
#>       Estimate: 1.0278
#>     Std. error: 0.2231
#>         95% CI: (0.5904, 1.4651)#> 
```

##### Randomized interventional effects

``` r
crumble(
    data = weight_behavior,
    trt = "sports", 
    outcome = "bmi",
    covar = c("age", "sex", "tvhours"),
    mediators = c("exercises", "overweigh"),
    moc = "snack",
    d0 = \(data, trt) factor(rep(1, nrow(data)), levels = c("1", "2")), 
    d1 = \(data, trt) factor(rep(2, nrow(data)), levels = c("1", "2")), 
    effect = "RI",
    learners = c("mean", "glm", "earth", "ranger"), 
    nn_module = sequential_module(),
    control = crumble_control(crossfit_folds = 1L, epochs = 20L)
)
#> ✔ Permuting Z-prime variables... 1/1 tasks [2s]
#> ✔ Fitting outcome regressions... 1/1 folds [14.2s]             
#> ✔ Computing alpha r density ratios... 1/1 folds [1m 23.2s]     
#> 
#> ══ Results `crumble()` ═════════════════════════════════════════
#> 
#> ── Randomized Direct Effect 
#>       Estimate: 0.0162
#>     Std. error: 0.1774
#>         95% CI: (-0.3315, 0.364)
#> 
#> ── Randomized Indirect Effect 
#>       Estimate: 1.0304
#>     Std. error: 0.2296
#>         95% CI: (0.5805, 0.4662)
```

#### References

<div id="refs" class="references csl-bib-body hanging-indent"
entry-spacing="0">

<div id="ref-liu2024general" class="csl-entry">

Liu, Richard, Nicholas T Williams, Kara E Rudolph, and Iván Dı́az. 2024.
“General Targeted Machine Learning for Modern Causal Mediation
Analysis.” *arXiv Preprint arXiv:2408.14620*.

</div>

<div id="ref-lok2015" class="csl-entry">

Lok, Judith J. 2015. “Organic Direct and Indirect Effects with
Post-Treatment Common Causes of Mediator and Outcome.”
<https://doi.org/10.48550/ARXIV.1510.02753>.

</div>

<div id="ref-pearl2022" class="csl-entry">

Pearl, Judea. 2022. “Direct and Indirect Effects.” In, 373–92. ACM.
<https://doi.org/10.1145/3501714.3501736>.

</div>

<div id="ref-vansteelandt2017" class="csl-entry">

Vansteelandt, Stijn, and Rhian M. Daniel. 2017. “Interventional Effects
for Mediation Analysis with Multiple Mediators.” *Epidemiology* 28 (2):
258–65. <https://doi.org/10.1097/ede.0000000000000596>.

</div>

<div id="ref-vo2024" class="csl-entry">

Vo, Tat-Thang, Nicholas Williams, Richard Liu, Kara E. Rudolph, and Ivan
Dıaz. 2024. “Recanting Twins: Addressing Intermediate Confounding in
Mediation Analysis.” <https://doi.org/10.48550/ARXIV.2401.04450>.

</div>

</div>
