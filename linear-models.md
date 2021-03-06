Linear Models
================

``` r
library(tidyverse)
library(p8105.datasets)

set.seed(1)
```

# Model Fitting

## Import data

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

## Fit a model

``` r
nyc_airbnb %>% 
  ggplot(aes(x = stars, y = price, color = borough)) +
  geom_point()
```

    ## Warning: Removed 9962 rows containing missing values (geom_point).

![](linear-models_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

fit the model we care about

``` r
fit = lm(price ~ stars + borough, data = nyc_airbnb)
```

look at the results…

Let’s look at the results better

``` r
broom::glance(fit)
```

    ## # A tibble: 1 × 12
    ##   r.squared adj.r.squared sigma statistic   p.value    df   logLik    AIC    BIC
    ##       <dbl>         <dbl> <dbl>     <dbl>     <dbl> <dbl>    <dbl>  <dbl>  <dbl>
    ## 1    0.0342        0.0341  182.      271. 6.73e-229     4 -202113. 4.04e5 4.04e5
    ## # … with 3 more variables: deviance <dbl>, df.residual <int>, nobs <int>

``` r
broom::tidy(fit) %>% 
  select(term, estimate, p.value) %>% 
  mutate(term = str_replace(term, "borough", "Borough: ")) %>% 
  knitr::kable(digits = 3)
```

| term               | estimate | p.value |
|:-------------------|---------:|--------:|
| (Intercept)        |  -70.414 |   0.000 |
| stars              |   31.990 |   0.000 |
| Borough: Brooklyn  |   40.500 |   0.000 |
| Borough: Manhattan |   90.254 |   0.000 |
| Borough: Queens    |   13.206 |   0.145 |

## Be in control of factors

``` r
nyc_airbnb = 
  nyc_airbnb %>% 
  mutate(
    #factor in order of frequency
    borough = fct_infreq(borough),
    room_type = fct_infreq(room_type)
  )
```

look at the plot again

``` r
nyc_airbnb %>% 
  ggplot(aes(x = stars, y = price, color = borough)) +
  geom_point()
```

    ## Warning: Removed 9962 rows containing missing values (geom_point).

![](linear-models_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

``` r
fit = lm(price ~ stars + borough, data = nyc_airbnb)

broom::tidy(fit)
```

    ## # A tibble: 5 × 5
    ##   term            estimate std.error statistic   p.value
    ##   <chr>              <dbl>     <dbl>     <dbl>     <dbl>
    ## 1 (Intercept)         19.8     12.2       1.63 1.04e-  1
    ## 2 stars               32.0      2.53     12.7  1.27e- 36
    ## 3 boroughBrooklyn    -49.8      2.23    -22.3  6.32e-109
    ## 4 boroughQueens      -77.0      3.73    -20.7  2.58e- 94
    ## 5 boroughBronx       -90.3      8.57    -10.5  6.64e- 26

``` r
broom::glance(fit)
```

    ## # A tibble: 1 × 12
    ##   r.squared adj.r.squared sigma statistic   p.value    df   logLik    AIC    BIC
    ##       <dbl>         <dbl> <dbl>     <dbl>     <dbl> <dbl>    <dbl>  <dbl>  <dbl>
    ## 1    0.0342        0.0341  182.      271. 6.73e-229     4 -202113. 4.04e5 4.04e5
    ## # … with 3 more variables: deviance <dbl>, df.residual <int>, nobs <int>

## Diagnostics

``` r
#add a column related to the residuals
#value of comparing to the actual value
nyc_airbnb %>% 
  modelr::add_residuals(fit) %>% 
  ggplot(aes(x = borough, y = resid)) +
  geom_violin() +
  #exclude some outliers
  ylim(-500, 1500)
```

    ## Warning: Removed 9993 rows containing non-finite values (stat_ydensity).

![](linear-models_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

``` r
nyc_airbnb %>% 
  modelr::add_residuals(fit) %>% 
  ggplot(aes(x = stars, y = resid)) + 
  geom_point() + 
  facet_wrap(. ~ borough)
```

    ## Warning: Removed 9962 rows containing missing values (geom_point).

![](linear-models_files/figure-gfm/unnamed-chunk-9-2.png)<!-- -->

## Hypothesis tests

This does t-test by default:

``` r
fit %>% 
  broom::tidy() 
```

    ## # A tibble: 5 × 5
    ##   term            estimate std.error statistic   p.value
    ##   <chr>              <dbl>     <dbl>     <dbl>     <dbl>
    ## 1 (Intercept)         19.8     12.2       1.63 1.04e-  1
    ## 2 stars               32.0      2.53     12.7  1.27e- 36
    ## 3 boroughBrooklyn    -49.8      2.23    -22.3  6.32e-109
    ## 4 boroughQueens      -77.0      3.73    -20.7  2.58e- 94
    ## 5 boroughBronx       -90.3      8.57    -10.5  6.64e- 26

What about the significance of ‘borough’:

``` r
fit_null = lm(price ~ stars, data = nyc_airbnb)
fit_alt = lm(price ~ stars + borough, data = nyc_airbnb)

anova(fit_null, fit_alt) %>% 
  broom::tidy()
```

    ## # A tibble: 2 × 6
    ##   res.df         rss    df     sumsq statistic    p.value
    ##    <dbl>       <dbl> <dbl>     <dbl>     <dbl>      <dbl>
    ## 1  30528 1030861841.    NA       NA        NA  NA        
    ## 2  30525 1005601724.     3 25260117.      256.  7.84e-164

``` r
  #results(p-value) show there is significance difference
```

## Nest data, fit models

This is pretty formal and complex

``` r
fit = lm(price ~ stars * borough + room_type * borough, data = nyc_airbnb)

broom::tidy(fit)
```

    ## # A tibble: 16 × 5
    ##    term                                  estimate std.error statistic  p.value
    ##    <chr>                                    <dbl>     <dbl>     <dbl>    <dbl>
    ##  1 (Intercept)                              95.7      19.2     4.99   6.13e- 7
    ##  2 stars                                    27.1       3.96    6.84   8.20e-12
    ##  3 boroughBrooklyn                         -26.1      25.1    -1.04   2.99e- 1
    ##  4 boroughQueens                            -4.12     40.7    -0.101  9.19e- 1
    ##  5 boroughBronx                             -5.63     77.8    -0.0723 9.42e- 1
    ##  6 room_typePrivate room                  -124.        3.00  -41.5    0       
    ##  7 room_typeShared room                   -154.        8.69  -17.7    1.42e-69
    ##  8 stars:boroughBrooklyn                    -6.14      5.24   -1.17   2.41e- 1
    ##  9 stars:boroughQueens                     -17.5       8.54   -2.04   4.09e- 2
    ## 10 stars:boroughBronx                      -22.7      17.1    -1.33   1.85e- 1
    ## 11 boroughBrooklyn:room_typePrivate room    32.0       4.33    7.39   1.55e-13
    ## 12 boroughQueens:room_typePrivate room      54.9       7.46    7.37   1.81e-13
    ## 13 boroughBronx:room_typePrivate room       71.3      18.0     3.96   7.54e- 5
    ## 14 boroughBrooklyn:room_typeShared room     47.8      13.9     3.44   5.83e- 4
    ## 15 boroughQueens:room_typeShared room       58.7      17.9     3.28   1.05e- 3
    ## 16 boroughBronx:room_typeShared room        83.1      42.5     1.96   5.03e- 2

This is more exploratory but maybe more easier to understand

``` r
nyc_airbnb %>% 
  #nest data except borough
  nest(data = -borough) %>% 
  mutate(
    models = map(data, ~lm(price ~ stars + room_type, data = .x)),
    results = map(models, broom::tidy)
  ) %>% 
  select(-data, -models) %>% 
  unnest(results) %>% 
  #filter(term == "stars")
  #significant association between stars and price in Manhattan
  filter(term != "(Intercept)") %>% 
  select(borough, term, estimate) %>% 
  pivot_wider(
    names_from = borough,
    values_from = estimate
  )
```

    ## # A tibble: 3 × 5
    ##   term                   Bronx Queens Brooklyn Manhattan
    ##   <chr>                  <dbl>  <dbl>    <dbl>     <dbl>
    ## 1 stars                   4.45   9.65     21.0      27.1
    ## 2 room_typePrivate room -52.9  -69.3     -92.2    -124. 
    ## 3 room_typeShared room  -70.5  -95.0    -106.     -154.

Let’s nest even more…

``` r
nyc_airbnb %>% 
  filter(borough == "Manhattan") %>% 
  nest(data = -neighborhood) %>% 
  mutate(
    models = map(data, ~lm(price ~ stars + room_type, data = .x)),
    results = map(models, broom::tidy)
  ) %>% 
  select(-data, -models) %>% 
  unnest(results) %>% 
  filter(str_detect(term, "room_type")) %>% 
  ggplot(aes(x = neighborhood, y = estimate)) +
  geom_point() +
  facet_wrap(. ~ term) +
  theme(axis.text.x = element_text(angle = 80, hjust = 1))
```

![](linear-models_files/figure-gfm/unnamed-chunk-14-1.png)<!-- -->

## Logistic Regression

``` r
nyc_airbnb = 
  nyc_airbnb %>% 
  #prob of being a expensive airbnb based on stars and roomtype
  mutate(
    expensive_apt = as.numeric(price > 500)
  )
```

let’s fit a logistic regression for the binary outcome

``` r
logistic_fit = 
  glm(
    expensive_apt ~ stars + borough,
    data = nyc_airbnb,
    #default: simple linear
    family = binomial())

logistic_fit %>% 
  broom::tidy() %>% 
  #more tidy
  mutate(
    term = str_replace(term, "borough", "Borough: "),
    estimate = exp(estimate)
  ) %>% 
  select(term, OR = estimate, p.value)
```

    ## # A tibble: 5 × 3
    ##   term                      OR  p.value
    ##   <chr>                  <dbl>    <dbl>
    ## 1 (Intercept)       0.000610   1.41e-20
    ## 2 stars             2.15       2.92e- 6
    ## 3 Borough: Brooklyn 0.307      3.94e-22
    ## 4 Borough: Queens   0.142      7.85e- 9
    ## 5 Borough: Bronx    0.00000123 9.40e- 1

``` r
nyc_airbnb %>% 
  modelr::add_predictions(logistic_fit) %>% 
  #get more readable probability values
  mutate(pred = boot::inv.logit(pred))
```

    ## # A tibble: 40,492 × 7
    ##    price stars borough neighborhood room_type       expensive_apt          pred
    ##    <dbl> <dbl> <fct>   <chr>        <fct>                   <dbl>         <dbl>
    ##  1    99   5   Bronx   City Island  Private room                0  0.0000000343
    ##  2   200  NA   Bronx   City Island  Private room                0 NA           
    ##  3   300  NA   Bronx   City Island  Entire home/apt             0 NA           
    ##  4   125   5   Bronx   City Island  Entire home/apt             0  0.0000000343
    ##  5    69   5   Bronx   City Island  Private room                0  0.0000000343
    ##  6   125   5   Bronx   City Island  Entire home/apt             0  0.0000000343
    ##  7    85   5   Bronx   City Island  Entire home/apt             0  0.0000000343
    ##  8    39   4.5 Bronx   Allerton     Private room                0  0.0000000234
    ##  9    95   5   Bronx   Allerton     Entire home/apt             0  0.0000000343
    ## 10   125   4.5 Bronx   Allerton     Entire home/apt             0  0.0000000234
    ## # … with 40,482 more rows
