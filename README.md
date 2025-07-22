
<!-- README.md is generated from README.Rmd. Please edit that file -->

# wowi: Utilities for detecting statistically significant spatial clusters of high acute malnutrition rates using SaTScan’s Bernoulli spatial-scan model

<!-- badges: start -->  
[![Project Status: WIP – Initial development is in progress, but there
has not yet been a stable, usable release suitable for the
public.](https://www.repostatus.org/badges/latest/wip.svg)](https://www.repostatus.org/#wip)  
[![Codecov test
coverage](https://codecov.io/gh/nutspatial/wowi/graph/badge.svg)](https://app.codecov.io/gh/nutspatial/wowi)  
[![R-CMD-check](https://github.com/nutspatial/wowi/actions/workflows/R-CMD-check.yaml/badge.svghttps://github.com/nutspatial/wowi/actions/workflows/R-CMD-check.yaml/badge.svghttps://github.com/nutspatial/wowi/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/nutspatial/wowi/actions/workflows/R-CMD-check.yaml)  
<!-- badges: end -->

Child acute malnutrition can lead to death if not identified and treated
in time. Driven by a combination of diverse factors, it often exhibits
spatial variation. The levels of acute malnutrition are commonly
measured through surveys that are representative of the area of
interest. These survey results are then used to inform programme
responses — to find and treat affected children. To that end, programme
managers require actionable insights on where acute malnutrition is most
prevalent. This is essential for prioritising interventions, especially
when resources are limited.

`wowi` - an expression meaning *“where”* in Elómwè, a local language
spoken in central-northern Mozambique - provides convenient utilities
for this purpose. It identifies locations across the survey area where
acute malnutrition is significantly high (hotspots) or low (coldspots),
and unlikely to be due to chance alone.

`wowi` is a wrapper package built on top of the
[`rsatscan`](https://cran.r-project.org/web/packages/rsatscan/index.html)
package, which enables the use of the
[`SaTScan`](https://www.satscan.org) software from within R. While
`rsatscan` provides general-purpose functionality, wowi was specifically
made for acute malnutrition analysis, tailoring the tools to the needs
of nutrition-focused spatial investigations.

To use `wowi`, you must have SaTScan installed on your machine, along
with the [`mwana`](https://nutriverse.io/mwana/) R package for
preprocessing anthropometric data.

## Installation

`wowi` is not yet on CRAN but can be installed through:

``` r
pak::pak(pkg = "nutspatial/wowi")
```

## Citation

If you use `wowi` package in your work, please cite using the suggested
citation provided by a call to `citation()` function as follows:

``` r
citation("wowi") 
#> To cite wowi in publications use:
#> 
#>   model wUfdssscohamruSBs (2025). _Tomás Zaba_. R package version
#>   0.0.0, <https://github.com/nutspatial/wowi>.
#> 
#> A BibTeX entry for LaTeX users is
#> 
#>   @Manual{,
#>     title = {Tomás Zaba},
#>     author = {wowi: Utilities for detecting statistically significant spatial clusters of high acute malnutrition rates using SaTScan's Bernoulli spatial-scanning model},
#>     year = {2025},
#>     note = {R package version 0.0.0},
#>     url = {https://github.com/nutspatial/wowi},
#>   }
```
