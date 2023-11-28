bootstrapping
================
ASHLEY ROMO
2023-11-26

Load key packages.

``` r
library(tidyverse)
library(modelr)

set.seed(1)
```

## Generate a relevant sample

``` r
n_samp = 250

#every residual is drawn from a normal distribution with mean 0 and sd 1
sim_df_const = 
  tibble(
    x = rnorm(n_samp, 1, 1),
    error = rnorm(n_samp, 0, 1),
    y = 2 + 3 * x + error
  )

# every residual depends on x (x values close to zero will have smaller errors and x values close to one will have larger errors)
sim_df_nonconst = 
  sim_df_const |> 
  mutate(
    error = error * 0.75 * x, 
    y = 2 + 3 * x + error
  )

sim_df_const |> 
  ggplot(aes(x = x, y = y)) + 
  geom_point()
```

<img src="bootstrapping_files/figure-gfm/unnamed-chunk-3-1.png" width="90%" />

``` r
sim_df_nonconst |> 
  ggplot(aes(x = x, y = y)) +
  geom_point()
```

<img src="bootstrapping_files/figure-gfm/unnamed-chunk-3-2.png" width="90%" />

fit some linear models

``` r
sim_df_const |> 
  lm(y ~ x, data = _) |> 
  broom::tidy()
```

    ## # A tibble: 2 × 5
    ##   term        estimate std.error statistic   p.value
    ##   <chr>          <dbl>     <dbl>     <dbl>     <dbl>
    ## 1 (Intercept)     1.98    0.0981      20.2 3.65e- 54
    ## 2 x               3.04    0.0699      43.5 3.84e-118

``` r
sim_df_nonconst |> 
  lm(y ~ x, data = _) |> 
  broom::tidy() 
```

    ## # A tibble: 2 × 5
    ##   term        estimate std.error statistic   p.value
    ##   <chr>          <dbl>     <dbl>     <dbl>     <dbl>
    ## 1 (Intercept)     1.93    0.105       18.5 1.88e- 48
    ## 2 x               3.11    0.0747      41.7 5.76e-114

``` r
# lm assumes the assumptions of linear regression: aka that there is constant variance across the domain even when the data truly does not have constant variance 
# we need to do in the setting of nonconstant variance is deal with it in a way that allows us to get accurate confidence intervals despite the fact that the generating process is not what we assumed theoretically
```

## Draw and analyze a bootstrap sample

Start with a little function

``` r
boot_sample = function(df) {
  
  sample_frac(df, replace = TRUE)
}
```

Let’s see how this works

``` r
sim_df_nonconst |> 
  boot_sample() |> 
  ggplot(aes(x = x, y = y)) +
  geom_point(alpha = 0.5) +
  stat_smooth(method = "lm")
```

    ## `geom_smooth()` using formula = 'y ~ x'

<img src="bootstrapping_files/figure-gfm/unnamed-chunk-6-1.png" width="90%" />
