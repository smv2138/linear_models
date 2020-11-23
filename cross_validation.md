Cross Validation
================

``` r
library(tidyverse)
```

    ## -- Attaching packages ------------------------------------------------------------------------------------------------------------------ tidyverse 1.3.0 --

    ## v ggplot2 3.3.2     v purrr   0.3.4
    ## v tibble  3.0.3     v dplyr   1.0.2
    ## v tidyr   1.1.2     v stringr 1.4.0
    ## v readr   1.3.1     v forcats 0.5.0

    ## -- Conflicts --------------------------------------------------------------------------------------------------------------------- tidyverse_conflicts() --
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
library(modelr)
library(mgcv)
```

    ## Loading required package: nlme

    ## 
    ## Attaching package: 'nlme'

    ## The following object is masked from 'package:dplyr':
    ## 
    ##     collapse

    ## This is mgcv 1.8-33. For overview type 'help("mgcv-package")'.

``` r
knitr::opts_chunk$set(
  fig.width = 6,
  fig.asp = 0.6,
  out.width = "90%"
)

theme_set(theme_minimal() + theme(legend.position = "bottom"))

## all plots i make will have the viridis color palette
options(
  ggplot2.continuous.colour = "viridis",
  ggplot2.continuous.fill = "viridis"
)

scale_colour_discrete = scale_colour_viridis_d
scale_fill_discrete = scale_fill_viridis_d
```

## Simulate Data

``` r
nonlin_df = 
  tibble(
    id = 1:100,
    x = runif(100, 0, 1),
    y = 1 - 10 * (x - .3) ^ 2 + rnorm(100, 0, .3)
  )
```

Look at data… it is non linear

``` r
nonlin_df %>% 
  ggplot(aes(x = x, y = y)) + 
  geom_point()
```

<img src="cross_validation_files/figure-gfm/unnamed-chunk-3-1.png" width="90%" />

## Cross validation – by hand

Get training and testing datasets

``` r
## sample 80 observations
train_df = sample_n(nonlin_df, size = 80)

## the remaining 20 goes into test dataset (observations in nonlin_df BUT not in training df)
test_df = anti_join(nonlin_df, train_df, by = "id")
```

## Build models using training dataset

``` r
linear_model = lm(y ~ x, data = train_df)
#smooth model
smooth_mod = gam(y ~ s(x), data = train_df)
#very wiggly model
wiggle_mod = gam(y ~s(x, k = 30), sp = 10e-6, data = train_df)
```

Can I see what I just did “add\_predictions” adds a column of fitted
values

``` r
## too linear
train_df %>% 
  add_predictions(linear_model) %>% 
  ggplot(aes(x = x, y = y)) +
  geom_point() +
  geom_line(aes(y = pred), color = "red")
```

<img src="cross_validation_files/figure-gfm/unnamed-chunk-6-1.png" width="90%" />

``` r
## good (looks like it will do best at predictioj accuracy)
train_df %>% 
  add_predictions(smooth_mod) %>% 
  ggplot(aes(x = x, y = y)) +
  geom_point() +
  geom_line(aes(y = pred), color = "red")
```

<img src="cross_validation_files/figure-gfm/unnamed-chunk-6-2.png" width="90%" />

``` r
## too wiggly
train_df %>% 
  add_predictions(wiggle_mod) %>% 
  ggplot(aes(x = x, y = y)) +
  geom_point() +
  geom_line(aes(y = pred), color = "red")
```

<img src="cross_validation_files/figure-gfm/unnamed-chunk-6-3.png" width="90%" />

Add predictions for multiple models at the same time

``` r
train_df %>% 
  gather_predictions(linear_model, smooth_mod, wiggle_mod) %>% 
  ggplot(aes(x = x, y = y)) +
  geom_point() +
  geom_line(aes(y = pred), color = "red") +
  facet_grid(. ~model)
```

<img src="cross_validation_files/figure-gfm/unnamed-chunk-7-1.png" width="90%" />

Look at prediction accuracy (calculate root mean squared error) (have to
look at test dataset)

``` r
## largest
rmse(linear_model, test_df)
```

    ## [1] 0.998492

``` r
## smallest (best)
rmse(smooth_mod, test_df)
```

    ## [1] 0.2724838

``` r
## middle
rmse(wiggle_mod, test_df)
```

    ## [1] 0.3542352
