
<!-- README.md is generated from README.Rmd. Please edit that file -->

# demspaces

<!-- badges: start -->

[![Travis build
status](https://travis-ci.com/andybega/demspaces.svg?branch=master)](https://travis-ci.com/andybega/demspaces)
[![Codecov test
coverage](https://codecov.io/gh/andybega/demspaces/branch/master/graph/badge.svg)](https://codecov.io/gh/andybega/demspaces?branch=master)
<!-- badges: end -->

demspaces contains helper functions for the Closing Spaces project. See
also the
[andybega/closing-spaces](https://github.com/andybega/closing-spaces)
repo.

## Installation

Since this package is specific to the democratic spaces project, it will
not end up on CRAN. Install from [GitHub](https://github.com/) instead,
with:

``` r
# install.packages("remotes")
remotes::install_github("andybega/demspaces")
```

## Example

``` r
library("demspaces")

data("states")

mdl   <- ds_logistic_reg("v2x_veracc_osp", states)
#> Warning in ds_logistic_reg("v2x_veracc_osp", states): Discarding 40
#> incomplete feature set cases
#> Warning in ds_logistic_reg("v2x_veracc_osp", states): Discarding 40
#> incomplete outcome set cases
preds <- predict(mdl, new_data = states)
head(preds)
#>          outcome from_year   for_years gwcode       p_up    p_same
#> 1 v2x_veracc_osp      1970 1971 - 1972      2 0.02847696 0.9609060
#> 2 v2x_veracc_osp      1971 1972 - 1973      2 0.02074426 0.9687526
#> 3 v2x_veracc_osp      1972 1973 - 1974      2 0.02582880 0.9616610
#> 4 v2x_veracc_osp      1973 1974 - 1975      2 0.02612461 0.9628875
#> 5 v2x_veracc_osp      1974 1975 - 1976      2 0.02514729 0.9648053
#> 6 v2x_veracc_osp      1975 1976 - 1977      2 0.02246814 0.9625945
#>       p_down
#> 1 0.01061705
#> 2 0.01050317
#> 3 0.01251021
#> 4 0.01098790
#> 5 0.01004742
#> 6 0.01493733
```

## Meta

There is a Makefile to help with stuff. E.g. to open that static docs:

``` bash
make opendocs
```

To add new models, see the `add_new_model.R` script. It will use a
template to setup the R and test files.
