
<!-- README.md is generated from README.Rmd. Please edit that file -->

# printy

Over the years, I’ve written a lot of one-off functions for formatting
numbers in RMarkdown documents. This packages collects them in a single
location.

## Installation 📚

You can install printy from github with:

``` r
# install.packages("remotes")
remotes::install_github("tjmahr/printy")
```

## Formatters ✍

`fmt_fix_digits()` prints a number with n digits of precision. R numbers
lose precision when converted to strings. This function converts the
numbers to strings and keeps precision. (It’s a wrapper for
`sprintf()`.)

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

`fmt_leading_zero()` removes a leading zero on numbers that are bounded
between −1 and 1, such as correlations or *p*-values.

``` r
fmt_leading_zero(c(-0.3, 0.4, 1))
#> [1] "-.3" ".4"  "1"
```

`fmt_minus_sign()` formats negative numbers with a minus sign.

``` r
fmt_minus_sign(c(1, 2, -3, -0.4, -pi))
#> [1] "1"                       "2"                      
#> [3] "&minus;3"                "&minus;0.4"             
#> [5] "&minus;3.14159265358979"
```

Putting it all together: Print a correlation matrix with 2 digits, no
leading zero and with minus signs.

``` r
fmt_correlation <- function(xs, digits = 2) {
  xs %>% fmt_fix_digits(digits) %>% fmt_leading_zero() %>% fmt_minus_sign()
}

test_cor %>% 
  as.data.frame() %>% 
  tibble::rownames_to_column(".rowname") %>% 
  tibble::as_tibble() %>% 
  mutate_at(vars(-.rowname), fmt_correlation) %>% 
  rename(` ` = .rowname) %>% 
  knitr::kable(align = "lrrrr")
```

|      |  mpg |  cyl | disp |   hp |
|:-----|-----:|-----:|-----:|-----:|
| mpg  | 1.00 | −.85 | −.85 | −.78 |
| cyl  | −.85 | 1.00 |  .90 |  .83 |
| disp | −.85 |  .90 | 1.00 |  .79 |
| hp   | −.78 |  .83 |  .79 | 1.00 |

### *p*-values 🎣

`fmt_p_value()` formats *p*-values with *n* digits of precision, with no
leading zero, and with very small values being printed with a `<` sign.

``` r
p <- c(1, 0.1, 0.01, 0.001, 0.0001)
fmt_p_value(p, digits = 2)
#> [1] "1.00"  ".10"   ".01"   "< .01" "< .01"
fmt_p_value(p, digits = 3)
#> [1] "1.000"  ".100"   ".010"   ".001"   "< .001"
```

`fmt_p_value_md()` formats *p*-values in markdown with nice defaults.

-   Use 3 digits of precision for values less than .06
-   Otherwise, use 2 digits of precision.
-   Include *p* in markdown

``` r
p <- c(1, 0.1, 0.06, 0.059, 0.051, 0.01, 0.001, 0.0001)
fmt_p_value_md(p)
#> [1] "*p*&nbsp;> .99"  "*p*&nbsp;= .10"  "*p*&nbsp;= .06"  "*p*&nbsp;= .059"
#> [5] "*p*&nbsp;= .051" "*p*&nbsp;= .010" "*p*&nbsp;= .001" "*p*&nbsp;< .001"
```

These render as: *p* &gt; .99, *p* = .10, *p* = .06, *p* = .059, *p* =
.051, *p* = .010, *p* = .001, *p* &lt; .001.

### Experimental formatters 🧪

`fmt_effect_md()` is an experimental function for getting model effects
formatted in markdown. You give the function a model, an effect and a
string listing the quantities you want.

``` r
model <- lm(breaks ~ wool * tension, warpbreaks) 
summary(model)
#> 
#> Call:
#> lm(formula = breaks ~ wool * tension, data = warpbreaks)
#> 
#> Residuals:
#>      Min       1Q   Median       3Q      Max 
#> -19.5556  -6.8889  -0.6667   7.1944  25.4444 
#> 
#> Coefficients:
#>                Estimate Std. Error t value Pr(>|t|)    
#> (Intercept)      44.556      3.647  12.218 2.43e-16 ***
#> woolB           -16.333      5.157  -3.167 0.002677 ** 
#> tensionM        -20.556      5.157  -3.986 0.000228 ***
#> tensionH        -20.000      5.157  -3.878 0.000320 ***
#> woolB:tensionM   21.111      7.294   2.895 0.005698 ** 
#> woolB:tensionH   10.556      7.294   1.447 0.154327    
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> Residual standard error: 10.94 on 48 degrees of freedom
#> Multiple R-squared:  0.3778, Adjusted R-squared:  0.3129 
#> F-statistic: 5.828 on 5 and 48 DF,  p-value: 0.0002772
```

``` r
# default to: b (beta), e (error), s (statistic), p (p value)
fmt_effect_md(model, "woolB", "besp")
#> [1] "*b*&nbsp;= &minus;16.33, SE&nbsp;= 5.16, *t*&nbsp;= &minus;3.17, *p*&nbsp;= .003"
```

*b* = −16.33, SE = 5.16, *t* = −3.17, *p* = .003

``` r
# Just a subset of them
fmt_effect_md(model, "woolB", terms = "bp")
#> [1] "*b*&nbsp;= &minus;16.33, *p*&nbsp;= .003"
```

*b* = −16.33, *p* = .003

``` r
# B for labeled b
fmt_effect_md(model, "woolB", terms = "Bp", b_lab = "Wool B")
#> [1] "*b*<sub>Wool B</sub>&nbsp;= &minus;16.33, *p*&nbsp;= .003"
```

*b*<sub>Wool B</sub> = −16.33, *p* = .003

``` r
# i for interval
fmt_effect_md(model, "woolB", terms = "bi")
#> [1] "*b*&nbsp;= &minus;16.33, 95% CI&nbsp;= [&minus;26.70, &minus;5.96]"
```

*b* = −16.33, 95% CI = \[−26.70, −5.96\]

``` r
# S for statistic with df
fmt_effect_md(model, "woolB", terms = "bSp")
#> [1] "*b*&nbsp;= &minus;16.33, *t*(48)&nbsp;= &minus;3.17, *p*&nbsp;= .003"
```

*b* = −16.33, *t*(48) = −3.17, *p* = .003

``` r
# extra digits (except for p-values; those go through `fmt_p_value_md()`)
fmt_effect_md(model, "woolB", terms = "bep", digits = 6)
#> [1] "*b*&nbsp;= &minus;16.333333, SE&nbsp;= 5.157299, *p*&nbsp;= .003"
```

*b* = −16.333333, SE = 5.157299, *p* = .003

These are the currently supported models:

-   `lm()`
-   `lme4::lmer()`

For lme4 models, Wald confidence intervals are provided. For *p*-values,
the Kenwood–Roger approximation for the degrees of freedom is used by
default. We can also choose a [method supported by the parameters
package](https://easystats.github.io/parameters/reference/p_value.lmerMod.html).

``` r
library(lme4)
#> Loading required package: Matrix
data(Machines, package = "nlme")

m <- lmer(score ~ 1 + Machine + (Machine | Worker), data = Machines)

# Default is Kenward
fmt_effect_md(m, "MachineB", terms = "beSp")
#> [1] "*b*&nbsp;= 7.97, SE&nbsp;= 2.42, *t*(5)&nbsp;= 3.29, *p*&nbsp;= .022"
fmt_effect_md(m, "MachineB", terms = "beSp", p_value_method = "kenward")
#> [1] "*b*&nbsp;= 7.97, SE&nbsp;= 2.42, *t*(5)&nbsp;= 3.29, *p*&nbsp;= .022"

# Note the infinite degrees of freedom for Wald
fmt_effect_md(m, "MachineB", terms = "beSp", p_value_method = "wald")
#> [1] "*b*&nbsp;= 7.97, SE&nbsp;= 2.42, *t*(Inf)&nbsp;= 3.29, *p*&nbsp;< .001"

# This example doesn't find differences between Satterthwaite and Kenward
fmt_effect_md(m, "MachineB", terms = "beSp", p_value_method = "satterthwaite")
#> [1] "*b*&nbsp;= 7.97, SE&nbsp;= 2.42, *t*(5)&nbsp;= 3.29, *p*&nbsp;= .022"
```

We can also format effects from `glmer()` models. `"S"` is not supported
because the model summary uses *z* statistics, not *t* statistics.

``` r
gm1 <- glmer(
  cbind(incidence, size - incidence) ~ period + (1 | herd),
  data = cbpp, 
  family = binomial
)

round(coef(summary(gm1)), 3)
#>             Estimate Std. Error z value Pr(>|z|)
#> (Intercept)   -1.398      0.231  -6.048    0.000
#> period2       -0.992      0.303  -3.272    0.001
#> period3       -1.128      0.323  -3.495    0.000
#> period4       -1.580      0.422  -3.743    0.000

fmt_effect_md(gm1, "period2", terms = "bespi")
#> [1] "*b*&nbsp;= &minus;0.99, SE&nbsp;= 0.30, *z*&nbsp;= &minus;3.27, *p*&nbsp;= .001, 95% CI&nbsp;= [&minus;1.59, &minus;0.40]"

# Don't use S here
fmt_effect_md(gm1, "period2", terms = "beSp")
#> Error in get_terms.glmerMod(model, effect, terms, ci_width = ci_width, : S is not supported for glmer models
```

## Skeletons 🦴

I use `fmt_` for formatting functions. The other convention in the
package is `skel_` to plug values into a formatting skeleton.

`skel_conf_interval_pair()` creates a confidence interval from two
numbers.

``` r
skel_conf_interval_pair(c(1, 2))
#> [1] "[1, 2]"
```

`skel_conf_interval()` is the vectorized version. It is suitable for
working on columns of numbers.

``` r
model <- lm(breaks ~ wool * tension, warpbreaks) 

ci_starts <- confint(model)[, 1] %>% 
  fmt_fix_digits(2) %>% 
  fmt_minus_sign()

ci_ends <- confint(model)[, 2] %>% 
  fmt_fix_digits(2) %>% 
  fmt_minus_sign()

skel_conf_interval(ci_starts, ci_ends)
#> [1] "[37.22, 51.89]"               "[&minus;26.70, &minus;5.96]" 
#> [3] "[&minus;30.93, &minus;10.19]" "[&minus;30.37, &minus;9.63]" 
#> [5] "[6.45, 35.78]"                "[&minus;4.11, 25.22]"
```

`skel_stat_n_value_pair()` creates *t*-test-like or correlation-like
statistic from a vector of two numbers.

``` r
skel_stat_n_value_pair(c("20", "2.0"))
#> [1] "t(20)&nbsp;= 2.0"
skel_stat_n_value_pair(c("39", ".98"), stat = "*r*")
#> [1] "*r*(39)&nbsp;= .98"
```

`skel_se()` and `skel_ci()` are shorthand functions to help with inline
reporting.

``` r
skel_se(c(10, 4))
#> [1] "SE&nbsp;= 10" "SE&nbsp;= 4"

skel_ci(c("[1, 2]"))
#> [1] "95% CI&nbsp;= [1, 2]"

skel_ci(c("[1, 2]"), ci_width = 90)
#> [1] "90% CI&nbsp;= [1, 2]"
```

## Formatting tables from lme4 models 🖇

One thing I’ve had to do a lot is summarize mixed effects models fit
with lme4. This package provides `pretty_lme4_ranefs()` which creates a
dataframe random effect variances and covariances like those printed by
`summary()`.

For example, we can fit the model.

``` r
library(lme4)
model <- lmer(Reaction ~ Days + (Days | Subject), sleepstudy)
summary(model)
#> Linear mixed model fit by REML ['lmerMod']
#> Formula: Reaction ~ Days + (Days | Subject)
#>    Data: sleepstudy
#> 
#> REML criterion at convergence: 1743.6
#> 
#> Scaled residuals: 
#>     Min      1Q  Median      3Q     Max 
#> -3.9536 -0.4634  0.0231  0.4634  5.1793 
#> 
#> Random effects:
#>  Groups   Name        Variance Std.Dev. Corr
#>  Subject  (Intercept) 612.10   24.741       
#>           Days         35.07    5.922   0.07
#>  Residual             654.94   25.592       
#> Number of obs: 180, groups:  Subject, 18
#> 
#> Fixed effects:
#>             Estimate Std. Error t value
#> (Intercept)  251.405      6.825  36.838
#> Days          10.467      1.546   6.771
#> 
#> Correlation of Fixed Effects:
#>      (Intr)
#> Days -0.138
```

`pretty_lme4_ranefs()` creates the following dataframe.

``` r
pretty_lme4_ranefs(model)
#>      Group   Parameter Variance    SD Correlations &nbsp;
#> 1  Subject (Intercept)   612.10 24.74         1.00 &nbsp;
#> 2   &nbsp;        Days    35.07  5.92          .07   1.00
#> 3 Residual      &nbsp;   654.94 25.59       &nbsp; &nbsp;
```

Which in markdown renders as

``` r
knitr::kable(
  pretty_lme4_ranefs(model), 
  align = c("l", "l", "r", "r", "r")
)
```

| Group    | Parameter   | Variance |    SD | Correlations |      |
|:---------|:------------|---------:|------:|-------------:|:-----|
| Subject  | (Intercept) |   612.10 | 24.74 |         1.00 |      |
|          | Days        |    35.07 |  5.92 |          .07 | 1.00 |
| Residual |             |   654.94 | 25.59 |              |      |

Here’s a dumb model with a lot going on in the random effects.

``` r
model <- lmer(mpg ~ wt * hp + (drat | gear) + (hp * cyl | am), mtcars)
#> boundary (singular) fit: see ?isSingular
model
#> Linear mixed model fit by REML ['lmerMod']
#> Formula: mpg ~ wt * hp + (drat | gear) + (hp * cyl | am)
#>    Data: mtcars
#> REML criterion at convergence: 152.7432
#> Random effects:
#>  Groups   Name        Std.Dev. Corr             
#>  gear     (Intercept) 1.556809                  
#>           drat        0.166292 -1.00            
#>  am       (Intercept) 1.940271                  
#>           hp          0.004055 -0.96            
#>           cyl         0.456219 -0.98  0.93      
#>           hp:cyl      0.001508  0.95 -0.94 -0.99
#>  Residual             2.113554                  
#> Number of obs: 32, groups:  gear, 3; am, 2
#> Fixed Effects:
#> (Intercept)           wt           hp        wt:hp  
#>    48.98745     -7.80904     -0.12118      0.02737  
#> optimizer (nloptwrap) convergence code: 0 (OK) ; 0 optimizer warnings; 1 lme4 warnings

knitr::kable(
  pretty_lme4_ranefs(model), 
  align = c("l", "l", "r", "r", "r", "r", "r", "r", "r")
)
```

| Group    | Parameter   | Variance |   SD | Correlations |      |      |      |
|:---------|:------------|---------:|-----:|-------------:|-----:|-----:|-----:|
| am       | (Intercept) |     3.76 | 1.94 |         1.00 |      |      |      |
|          | hp          |     0.00 | 0.00 |         −.96 | 1.00 |      |      |
|          | cyl         |     0.21 | 0.46 |         −.98 |  .93 | 1.00 |      |
|          | hp:cyl      |     0.00 | 0.00 |          .95 | −.94 | −.99 | 1.00 |
| gear     | (Intercept) |     2.42 | 1.56 |         1.00 |      |      |      |
|          | drat        |     0.03 | 0.17 |        −1.00 | 1.00 |      |      |
| Residual |             |     4.47 | 2.11 |              |      |      |      |
