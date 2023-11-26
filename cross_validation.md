cross validation
================
ASHLEY ROMO
2023-11-18

Load key packages.

``` r
library(tidyverse)
library(modelr)

set.seed(1)
```

## Nonlinear data and CV

``` r
nonlin_df = 
  tibble(
    id = 1:100,
    x = runif(100, 0, 1),
    y = 1 - 10 * (x - .3) ^ 2 + rnorm(100, 0, 0.3)
  )

nonlin_df |> 
  ggplot(aes(x = x, y = y)) + 
  geom_point()
```

<img src="cross_validation_files/figure-gfm/unnamed-chunk-3-1.png" width="90%" />

Do the train / test split.

``` r
#sample_n(dataset, size #)
train_df = sample_n(nonlin_df, 80)

#anti_join of whatever is in the nonlin_df dataset and not in the train_df and do it by "id"
test_df = anti_join(nonlin_df, train_df, by = "id")
```

``` r
train_df |> 
  ggplot(aes(x = x, y = y)) + 
  geom_point() + 
  geom_point(data = test_df, color = "red")
```

<img src="cross_validation_files/figure-gfm/unnamed-chunk-5-1.png" width="90%" />

``` r
#black = training data, red = testing data

# the question is can I fit some curve through the black points here and see how well that curve predicts the red points (there is going to be some error)
```

``` r
#fit a linear model 
linear_mod = lm(y ~ x, data = train_df)

# the right way to fit a smooth model using generalized additive models
#mgcv::gam() where y is a smooth term of x and you specific the data 
# s(x) says give me a smooth function of x
smooth_mod = mgcv::gam(y ~ s(x), data = train_df)

#break the smooth_mod by forcing mgcv to do something by setting k = 30 and the smoothing parameter sp = 10e-6
#k=30 means you are going to get 30 individual line segments spread across the domain
#sp = 10e-6 says it is forcing mgcv to do somethin dumb
wiggly_mod = mgcv::gam(y ~ s(x, k = 30), sp = 10e-6, data = train_df)
```

quick visualization of the linear model

``` r
#modelr::add_predictions(model you want to use to add predictions)
#plot output shows this model is not that great
train_df |> 
  modelr::add_predictions(linear_mod) |> 
  ggplot(aes(x = x, y = y)) + 
  geom_point() + 
  geom_line(aes(y = pred))
```

<img src="cross_validation_files/figure-gfm/unnamed-chunk-7-1.png" width="90%" />

``` r
# this plot shows a smooth curve going right through the middle of the data points
train_df |> 
  modelr::add_predictions(smooth_mod) |> 
  ggplot(aes(x = x, y = y)) + 
  geom_point() + 
  geom_line(aes(y = pred))
```

<img src="cross_validation_files/figure-gfm/unnamed-chunk-7-2.png" width="90%" />

``` r
#plot output shows the curve broken down
train_df |> 
  modelr::add_predictions(wiggly_mod) |> 
  ggplot(aes(x = x, y = y)) + 
  geom_point() + 
  geom_line(aes(y = pred))
```

<img src="cross_validation_files/figure-gfm/unnamed-chunk-7-3.png" width="90%" />

RMSEs on training data can be misleading …

``` r
#assessing goodness of fit can be done via RMSE
rmse(linear_mod, train_df)
```

    ## [1] 0.7178747

``` r
rmse(smooth_mod, train_df)
```

    ## [1] 0.2874834

``` r
rmse(wiggly_mod, train_df)
```

    ## [1] 0.2498309

``` r
#from the rmse, the wiggly_mod shows that it is the best fit (lowest rmse); however, this only works for the dataset used to train the model, but it does not mean it is the best for future data
```

RMSE on testing data gives a sense of out-of-sample prediction accuracy!

``` r
rmse(linear_mod, test_df)
```

    ## [1] 0.7052956

``` r
rmse(smooth_mod, test_df)
```

    ## [1] 0.2221774

``` r
rmse(wiggly_mod, test_df)
```

    ## [1] 0.289051

``` r
#using the test data set, the smooth model appears to be better (lowest rmse)
```
