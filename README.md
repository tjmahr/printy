
<!-- README.md is generated from README.Rmd. Please edit that file -->
printy
======

Over the years, I've written a lot of one-off functions for formatting numbers in RMarkdown documents. This packages collects them in a single location.

Installation
------------

You can install printy from github with:

``` r
# install.packages("devtools")
devtools::install_github("tjmahr/printy")
```

Examples
--------

Print a number with n digits of precision. R numbers lose precision when converted to strings. This function converts the numbers to strings and keeps precision. (It's a wrapper for `sprintf()`.)

``` r
library(dplyr, warn.conflicts = FALSE)
library(printy)
test_cor <- cor(mtcars[, 1:4]) 

# Typical loss of trailing zeroes
test_cor[1:4, 3] %>% round(2) %>% as.character()
#> [1] "-0.85" "0.9"   "1"     "0.79"

test_cor[1:4, 3] %>% fmt_fix_digits(2)
#> [1] "-0.85" "0.90"  "1.00"  "0.79"
```

Don't print a leading zero on numbers that are bounded between −1 and 1, such as correlations or *p*-values.

``` r
fmt_leading_zero(c(-0.3, 0.4, 1))
#> [1] "-.3" ".4"  "1"

# Todo: This functions obliterates 0's
fmt_leading_zero(c(0, 0.0))
#> [1] "" ""
```

Format negative numbers with a minus sign.

``` r
fmt_minus_sign(c(1, 2, -3, -0.4, -pi))
#> [1] "1"                       "2"                      
#> [3] "&minus;3"                "&minus;0.4"             
#> [5] "&minus;3.14159265358979"
```

Putting it all together.

``` r
fmt_correlation <- function(xs, digits = 2) {
  xs %>% fmt_fix_digits(digits) %>% fmt_leading_zero() %>% fmt_minus_sign()
}

test_cor %>% 
  as.data.frame() %>% 
  tibble::as_tibble() %>% 
  tibble::rownames_to_column(".rowname") %>% 
  mutate_at(vars(-.rowname), fmt_correlation) %>% 
  rename(` ` = .rowname) %>% 
  knitr::kable(align = "lrrrr")
```

|      |   mpg|   cyl|  disp|    hp|
|------|-----:|-----:|-----:|-----:|
| mpg  |  1.00|  −.85|  −.85|  −.78|
| cyl  |  −.85|  1.00|   .90|   .83|
| disp |  −.85|   .90|  1.00|   .79|
| hp   |  −.78|   .83|   .79|  1.00|
