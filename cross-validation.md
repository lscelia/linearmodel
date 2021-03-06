Cross Validation
================

``` r
library(tidyverse)
library(modelr)
library(mgcv)
```

Stimulate a dataset

``` r
set.seed(1)

nonlin_df = 
  tibble(
    id = 1:100,
    x = runif(100, 0, 1),
    y = 1 - 10 * (x - .3) ^ 2 + rnorm(100, 0, .3)
  )

nonlin_df %>% 
  ggplot(aes(x = x, y = y)) + 
  geom_point()
```

![](cross-validation_files/figure-gfm/unnamed-chunk-1-1.png)<!-- -->

Create splits by hand; fit some models

``` r
#common training-learning sample size: 80-20
train_df = sample_n(nonlin_df, 80)
#find data points in the nonlin_df that are not in train_df
test_df = anti_join(nonlin_df, train_df, by = "id")

ggplot(train_df, aes(x = x, y = y)) + 
  geom_point() + 
  geom_point(data = test_df, color = "red")
```

![](cross-validation_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

Fit my models

``` r
linear_mod = lm(y ~ x, data = train_df)
#y is a smooth function of x
smooth_mod = mgcv::gam(y ~ s(x), data = train_df)
wiggly_mod = mgcv::gam(y ~ s(x, k = 30), sp = 10e-6, data = train_df)
```

plot the results

``` r
train_df %>% 
  gather_predictions(linear_mod, smooth_mod, wiggly_mod) %>% 
  mutate(model = fct_inorder(model)) %>% 
  ggplot(aes(x = x, y = y)) + 
  geom_point() + 
  geom_line(aes(y = pred), color = "red") + 
  facet_wrap(~model)
```

![](cross-validation_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

quantify the results

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

## CV iteratively

Use ‘modelr::cross_mc’

``` r
cv_df = 
  crossv_mc(nonlin_df, 100) %>% 
  mutate(
    train = map(train, as_tibble),
    test = map(test, as_tibble)
  )

#cv_df %>% pull(train) %>% .[[1]]
```

Let’s fit some models

``` r
cv_df = 
  cv_df %>% 
  mutate(
    linear_mod = map(.x = train, ~lm(y ~ x, data = .x)),
    smooth_mod = map(.x = train, ~gam(y ~ s(x), data = .x)),
    wiggly_mod = map(.x = train, ~gam(y ~ s(x, k = 30), sp = 10e-6, data = .x))
  ) %>% 
  mutate(
    #map 2 things, show double value
    rmse_linear = map2_dbl(.x = linear_mod, .y = test, ~rmse(model = .x, data = .y )),
    rmse_smooth = map2_dbl(.x = smooth_mod, .y = test, ~rmse(model = .x, data = .y )),
    rmse_wiggly = map2_dbl(.x = wiggly_mod, .y = test, ~rmse(model = .x, data = .y ))
  )
```

Look at output

``` r
cv_df %>% 
  select(.id, starts_with("rmse")) %>% 
  pivot_longer(
    rmse_linear:rmse_wiggly,
    names_to = "model",
    values_to = "rmse",
    names_prefix = "rmse_"
  ) %>% 
  ggplot(aes(x = model, y = rmse)) +
  geom_boxplot()
```

![](cross-validation_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

## Child growth data

import data

``` r
child_growth_df = 
  read_csv("nepalese_children.csv") %>% 
  mutate(
    #change point model
    weight_cp = (weight > 7) * (weight - 7)
  )
```

    ## Rows: 2705 Columns: 5

    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## dbl (5): age, sex, weight, height, armc

    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
child_growth_df %>% 
  ggplot(aes(x = weight, y = armc)) + 
  geom_point(alpha = .2)
```

![](cross-validation_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

Consider candidates models

``` r
linear_mod = lm(armc ~ weight, data = child_growth_df)
#piece-wise linear model
pwl_mod = lm(armc ~ weight + weight_cp, data = child_growth_df)
smooth_mod = gam(armc ~ s(weight), data = child_growth_df)
```

``` r
#plot for picec-wise
child_growth_df %>% 
  add_predictions(pwl_mod) %>% 
  ggplot(aes(x = weight, y = armc)) + 
  geom_point(alpha = 0.2) +
  geom_line(aes(y = pred), color = "red")
```

![](cross-validation_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->

Use CV to compare models

``` r
cv_df = 
  crossv_mc(child_growth_df, 100) %>% 
  mutate(
    train = map(train, as_tibble),
    test = map(test, as_tibble)
  )
```

Fit models and extract RMSE

``` r
cv_df = 
  cv_df %>% 
  mutate(
    linear_mod = map(.x = train, ~lm(armc ~ weight, data = .x)),
    pwl_mod = map(.x = train, ~lm(armc ~ weight + weight_cp, data = .x)),
    smooth_mod = map(.x = train, ~gam(armc ~ s(weight), data = .x))
  ) %>% 
  mutate(
    #map 2 things, show double value
    rmse_linear = map2_dbl(.x = linear_mod, .y = test, ~rmse(model = .x, data = .y )),
    rmse_pwl = map2_dbl(.x = pwl_mod, .y = test, ~rmse(model = .x, data = .y)),
    rmse_smooth = map2_dbl(.x = smooth_mod, .y = test, ~rmse(model = .x, data = .y ))
  )
```

Look at RMSE distributions

``` r
cv_df %>% 
  select(.id, starts_with("rmse")) %>% 
  pivot_longer(
    rmse_linear:rmse_smooth,
    names_to = "model",
    values_to = "rmse",
    names_prefix = "rmse_"
  ) %>% 
  ggplot(aes(x = model, y = rmse)) +
  geom_boxplot()
```

![](cross-validation_files/figure-gfm/unnamed-chunk-15-1.png)<!-- -->

``` r
#We can see that pwl and smooth models are doing better than linear model
#Smooth a little bit better than pwl consistently
```
