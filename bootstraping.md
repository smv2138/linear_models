Bootstrapping
================

``` r
library(tidyverse)
```

    ## -- Attaching packages ------------------------------------------------------------------------------------------------------------------- tidyverse 1.3.0 --

    ## v ggplot2 3.3.2     v purrr   0.3.4
    ## v tibble  3.0.3     v dplyr   1.0.2
    ## v tidyr   1.1.2     v stringr 1.4.0
    ## v readr   1.3.1     v forcats 0.5.0

    ## -- Conflicts ---------------------------------------------------------------------------------------------------------------------- tidyverse_conflicts() --
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
library(modelr)
library(p8105.datasets)

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
n_samp = 250

sim_df_const = 
  tibble(
    x = rnorm(n_samp, 1, 1),
    error = rnorm(n_samp, 0, 1),
    y = 2 + 3 * x + error
  )

#new error structure
sim_df_nonconst = sim_df_const %>% 
  mutate(
  error = error * .75 * x,
  y = 2 + 3 * x + error
)
```

Plot the datasets

Shows linear line residuals have constant variance (good for most
assumptions of simple linear regression)

``` r
sim_df_const %>% 
  ggplot(aes(x = x, y = y)) +
  geom_point() +
  geom_smooth(method = "lm")
```

    ## `geom_smooth()` using formula 'y ~ x'

<img src="bootstraping_files/figure-gfm/unnamed-chunk-3-1.png" width="90%" />

Doesn’t meet assumptions well Residuals spread out later on
(heteroscedasticty) Can solve this with a bootstrap

``` r
sim_df_nonconst %>% 
  ggplot(aes(x = x, y = y)) +
  geom_point() +
  geom_smooth(method = "lm")
```

    ## `geom_smooth()` using formula 'y ~ x'

<img src="bootstraping_files/figure-gfm/unnamed-chunk-4-1.png" width="90%" />

Fit linear regs for both dataset

``` r
lm(y ~ x, data = sim_df_const) %>% broom::tidy()
```

    ## # A tibble: 2 x 5
    ##   term        estimate std.error statistic   p.value
    ##   <chr>          <dbl>     <dbl>     <dbl>     <dbl>
    ## 1 (Intercept)     2.07    0.0882      23.5 6.01e- 65
    ## 2 x               3.01    0.0623      48.4 2.93e-128

``` r
lm(y ~ x, data = sim_df_nonconst) %>% broom::tidy()
```

    ## # A tibble: 2 x 5
    ##   term        estimate std.error statistic   p.value
    ##   <chr>          <dbl>     <dbl>     <dbl>     <dbl>
    ## 1 (Intercept)     2.03    0.0966      21.0 4.95e- 57
    ## 2 x               3.04    0.0683      44.6 2.08e-120

## Draw 1 Bootstrap sample

Assumptions aren’t met in the nonconst data set (understand what the
distribution of intercepts are slope are in repeated sampling) - find
actual slope and intercept through repeated sample We don’t trust the CI
and the uncertainity estimates that go along with it

`sample_frac` draws a sample of a particular proportion from your
dataset (by default with grab a sample that is proportional (1:1)
proportion with the size of the dataset) We want it to be the same size
because CI are very dependent on the sample size

`arrange` because we know the df we’re passing through will have an x
and y and we want them arranged that way

``` r
boot_samp = function(df) {
  
  sample_frac(df, size = 1, replace = TRUE) %>% 
    arrange(x)
  
}
```

Check if the function works

Some observations are coming in twice Darker points signify that those
values appear multiple times in the dataset due to replacement

``` r
boot_samp(sim_df_nonconst) %>% 
  ggplot(aes(x = x, y = y)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm")
```

    ## `geom_smooth()` using formula 'y ~ x'

<img src="bootstraping_files/figure-gfm/unnamed-chunk-7-1.png" width="90%" />

Fit a regression with the bootstrap samples

``` r
boot_samp(sim_df_nonconst) %>% 
  lm(y ~ x, data = .) %>% 
  broom::tidy()
```

    ## # A tibble: 2 x 5
    ##   term        estimate std.error statistic   p.value
    ##   <chr>          <dbl>     <dbl>     <dbl>     <dbl>
    ## 1 (Intercept)     2.03    0.109       18.6 8.28e- 49
    ## 2 x               3.04    0.0741      41.0 1.89e-112

## Many samples and analysis

1000 bootstrap samples drawn with replacement from this dataframe

Ran bootstrap of sample size of 250, 1000 times

``` r
boot_straps = 
  tibble(
    strap_number = 1:1000,
    strap_sample = rerun(1000, boot_samp(sim_df_nonconst))
  )

boot_straps %>% pull(strap_sample) %>% .[[1]]
```

    ## # A tibble: 250 x 3
    ##         x   error      y
    ##     <dbl>   <dbl>  <dbl>
    ##  1 -1.38  -1.25   -3.38 
    ##  2 -1.38  -1.25   -3.38 
    ##  3 -1.22  -0.652  -2.30 
    ##  4 -1.00  -0.199  -1.21 
    ##  5 -0.998 -0.621  -1.62 
    ##  6 -0.998 -0.621  -1.62 
    ##  7 -0.922 -0.363  -1.13 
    ##  8 -0.869  0.0401 -0.568
    ##  9 -0.736 -0.412  -0.619
    ## 10 -0.599  0.218   0.422
    ## # ... with 240 more rows

We have a dataframe (can use iterative analysis methods to analyze this)

Can I run my analysis on these?? YES

We want to fit a regression to each of the 1000 dataframes For each
regression get the results

``` r
boot_results = 
  boot_straps %>%
  mutate(
    models = map(.x = strap_sample, ~lm(y ~ x, data = .x)),
    results = map(models, broom::tidy)
  ) %>% 
  select(strap_number, results) %>% 
  unnest(results)
```

What do I have now? There are 2000 rows because we had 1000 samples and
for each we have an intercept and a slope estimate Interested in the
distribution of estimates slope and intercept (we hope that the variance
are closer to the actual variance)

Took mean of estimate and ed for the intercept and slope. Using
bootstrap to say, I know linear regression assume constant variance, but
I know constant variance isn’t the right assumption, so we know whatever
the regular linear regression says about slope and intercept are not
true. Bootstrap gives you a way to get a true sd without making any
assumptions about variance, to get information about uncertainty of the
slope and interecept

``` r
boot_results %>% 
  group_by(term) %>% 
  summarize(
    mean_est = mean(estimate),
    sd_est = sd(estimate)
  )
```

    ## `summarise()` ungrouping output (override with `.groups` argument)

    ## # A tibble: 2 x 3
    ##   term        mean_est sd_est
    ##   <chr>          <dbl>  <dbl>
    ## 1 (Intercept)     2.03 0.0545
    ## 2 x               3.05 0.0996

Look at the distributions

What this show is: under repeated sampling, this is the actual
distribution of slopes (not relying on any assumptions)

``` r
boot_results %>%
  filter(term == "x") %>% 
  ggplot(aes(x = estimate)) +
  geom_density()
```

<img src="bootstraping_files/figure-gfm/unnamed-chunk-12-1.png" width="90%" />

Construct bootstrap CI CI based on repeated sampling

``` r
boot_results %>% 
  group_by(term) %>% 
  summarize(
    ci_lower = quantile(estimate, 0.025),
    ci_upper = quantile(estimate, 0.975)
  )
```

    ## `summarise()` ungrouping output (override with `.groups` argument)

    ## # A tibble: 2 x 3
    ##   term        ci_lower ci_upper
    ##   <chr>          <dbl>    <dbl>
    ## 1 (Intercept)     1.93     2.14
    ## 2 x               2.86     3.25

## Bootstrap using `modelr`

Can we simplify anything?

`bootstrap` function in `modelr` tells it you want a bootstrap, and you
can indicate how many samples you want Creates a resample object, not a
dataframe

Mean estimates are in the same ball park as when we did it above

``` r
sim_df_nonconst %>%
  bootstrap(1000, id = "strap_number") %>% 
   mutate(
    models = map(.x = strap, ~lm(y ~ x, data = .x)),
    results = map(models, broom::tidy)
  ) %>% 
  select(strap_number, results) %>% 
  unnest(results) %>% 
  group_by(term) %>% 
  summarize(
    mean_est = mean(estimate),
    sd_est = sd(estimate)
  )
```

    ## `summarise()` ungrouping output (override with `.groups` argument)

    ## # A tibble: 2 x 3
    ##   term        mean_est sd_est
    ##   <chr>          <dbl>  <dbl>
    ## 1 (Intercept)     2.03 0.0548
    ## 2 x               3.05 0.103

Bootstrap works pretty well if your assumptions aren’t met
(nonconst\_df) But what if the assumptions ARE met (const\_df)? Does it
still work? Yes\!

``` r
sim_df_const %>%
  bootstrap(1000, id = "strap_number") %>% 
   mutate(
    models = map(.x = strap, ~lm(y ~ x, data = .x)),
    results = map(models, broom::tidy)
  ) %>% 
  select(strap_number, results) %>% 
  unnest(results) %>% 
  group_by(term) %>% 
  summarize(
    mean_est = mean(estimate),
    sd_est = sd(estimate)
  )
```

    ## `summarise()` ungrouping output (override with `.groups` argument)

    ## # A tibble: 2 x 3
    ##   term        mean_est sd_est
    ##   <chr>          <dbl>  <dbl>
    ## 1 (Intercept)     2.07 0.0870
    ## 2 x               3.01 0.0656

## Revisit NYC airbnb

``` r
data("nyc_airbnb")

nyc_airbnb = 
  nyc_airbnb %>% 
  mutate(stars = review_scores_location / 2) %>% 
  rename(
    borough = neighbourhood_group,
    neighborhood = neighbourhood) %>% 
  filter(borough != "Staten Island") %>% 
  select(price, stars, borough, neighborhood, room_type)
```

There is clearly some non constant variance

``` r
nyc_airbnb %>% 
  ggplot(aes(x = stars, y = price)) +
  geom_point()
```

    ## Warning: Removed 9962 rows containing missing values (geom_point).

<img src="bootstraping_files/figure-gfm/unnamed-chunk-17-1.png" width="90%" />

Still non constant variance with a few outliers

``` r
nyc_airbnb %>% 
  filter(borough == "Manhattan") %>% 
  drop_na(stars) %>% 
  ggplot(aes(x = stars, y = price)) +
  geom_point()
```

<img src="bootstraping_files/figure-gfm/unnamed-chunk-18-1.png" width="90%" />

Want to get standard error for linear model - bootstrap This tell you
the SE would be 6.64 WITHOUT assuming constant variance (higher than the
SE under lm assumptions) Makes sense because of outliers, we know there
should be more variability in the slope

``` r
airbnb_boot_results =  
  nyc_airbnb %>% 
    filter(borough == "Manhattan") %>% 
    drop_na(stars) %>% 
    bootstrap(1000, id = "strap_number") %>% 
     mutate(
      models = map(.x = strap, ~lm(price ~ stars, data = .x)),
      results = map(models, broom::tidy)
    ) %>% 
    select(strap_number, results) %>% 
    unnest(results) 

airbnb_boot_results %>% 
    group_by(term) %>% 
    summarize(
      mean_est = mean(estimate),
      sd_est = sd(estimate)
    )
```

    ## `summarise()` ungrouping output (override with `.groups` argument)

    ## # A tibble: 2 x 3
    ##   term        mean_est sd_est
    ##   <chr>          <dbl>  <dbl>
    ## 1 (Intercept)    -32.5  33.1 
    ## 2 stars           42.9   6.72

Compare this to the `lm` estimates This SE tells you that is should be
4.78 assuming constant variance

``` r
 nyc_airbnb %>% 
  filter(borough == "Manhattan") %>% 
  drop_na(stars) %>%
  lm(price ~ stars, data = .) %>% 
  broom::tidy()
```

    ## # A tibble: 2 x 5
    ##   term        estimate std.error statistic  p.value
    ##   <chr>          <dbl>     <dbl>     <dbl>    <dbl>
    ## 1 (Intercept)    -34.3     22.9      -1.50 1.35e- 1
    ## 2 stars           43.3      4.78      9.07 1.39e-19

``` r
airbnb_boot_results %>% 
  filter(term == "stars") %>% 
  ggplot(aes(x = estimate)) +
  geom_density()
```

<img src="bootstraping_files/figure-gfm/unnamed-chunk-21-1.png" width="90%" />

Can use the same code to construct CI as above
