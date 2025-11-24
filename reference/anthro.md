# Sample data set of district-level SMART surveys with geographical coordinates

`anthro` is a SMART survey-generated data conducted in nine districts in
Uganda.

## Usage

``` r
anthro
```

## Format

A tibble of 2,934 rows and 17 columns.

|              |                                                                        |
|--------------|------------------------------------------------------------------------|
| **Variable** | **Description**                                                        |
| *district*   | Location in which the survey was undertaken                            |
| *cluster*    | Primary sampling unit                                                  |
| *sex*        | Sex; "1" = boys, "2" = girls                                           |
| *age*        | Calculated age in months with two decimal places                       |
| *weight*     | Weight in kilograms                                                    |
| *height*     | Height in centimetres                                                  |
| *oedema*     | Oedema; "n" = no oedema, "y" = with oedema                             |
| *muac*       | Mid upper-arm circumference in millimetres                             |
| *y*          | Geographical coordinates: Latitude                                     |
| *x*          | Geographical coordinates: Longitude                                    |
| *precision*  | Estimated spatial accuracy of the recorded GPS coordinates, in meters. |

## Source

anonymous

## Examples

``` r
anthro
#> # A tibble: 2,934 × 11
#>    district cluster   sex   age weight height oedema  muac     y     x precision
#>    <chr>      <dbl> <dbl> <dbl>  <dbl>  <dbl> <chr>  <dbl> <dbl> <dbl>     <dbl>
#>  1 Kotido        15     2 48.9    15.9  108.  n        128  2.93  34.1         8
#>  2 Kaabong      161     1 43.8    15    106.  n        128  3.65  34.1         4
#>  3 Kaabong      161     1 37.0    13.6   91.9 n        149 NA     NA          NA
#>  4 Kaabong      161     1 NA       7    100   n        150 NA     NA          NA
#>  5 Kaabong      160     1 22.5     8.6   74.2 n        119  3.65  34.1         4
#>  6 Kaabong      160     1 32.1    13.4   88.1 n        163 NA     NA          NA
#>  7 Kaabong      160     2  6.97    6.3   62.7 n        114  3.65  34.1         5
#>  8 Kaabong      161     2 NA      14.3  117.  n        143  3.63  34.0         5
#>  9 Kaabong      160     2  6.05    6.8   63.1 n        131  3.65  34.0         5
#> 10 Kaabong      160     2 48       9    103.  n        138  3.65  34.0         5
#> # ℹ 2,924 more rows
```
