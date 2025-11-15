Bootstrapping
================

\##bootstrapping–通过对现有的一个样本数据进行反复抽样（with
replacement），每次抽取相同sample size的sample（bootstrap
samples），对每个sample进行分析。由于你在现实生活中无法获取多个样本，无法在population中反复抽样（repeated
sampling），你通常只有一个样本数据，而通过这种方式可以来模拟在现实中真的获得多个样本的情形，创造一个虚拟的样本分布（如获得样本均值的分布，回归系数的分布），由于这个分布完全基于你的数据本身，所以不依赖于任何理论假设（假设error服从正态分布，方差相等…），更加稳健灵活

\##你获取了一个样本，做了线性回归，现在你想得到回归系数的CI，这需要基于你对model的那三个假设（服从正态分布，方差相同，独立），而如果假设并不成立，那你的CI其实根本not
valid，并不可信。

\##而现在你可以通过bootstrap直接获得多个样本，你可以直接探寻这些样本到底服从什么分布，根本不需要基于任何假设，你得到的结论也是valid的

\##所以当你的理论假设并不成立，或者样本量太小的时候，可以使用bootstrap解决问题

Load key packages.

``` r
library(tidyverse)
library(p8105.datasets)
library(modelr)
```

Simulate two datasets.

``` r
set.seed(1)

##设定每个样本大小为250
n_samp = 250

##设定error服从normal distribution，并且方差相等
sim_df_const = 
  tibble(
    x = rnorm(n_samp, 1, 1),
    error = rnorm(n_samp, 0, 1),
    y = 2 + 3 * x + error
  )

##设定error方差不相等
sim_df_nonconst = 
  sim_df_const |> 
  mutate(
    error = .75 * error * x,  ##现在这个新的error的值取决于旧error * x，新error会收到x的值的影响
    y = 2 + 3 * x + error)  ##现在这个model的error变成上面那个新error
```

Look at these data

``` r
sim_df_const |> 
  ggplot(aes(x = x, y = y)) + 
  geom_point()
```

<img src="bootstrapping_files/figure-gfm/unnamed-chunk-4-1.png" width="90%" />
你可以相信这个model的结论，因为它符合linear model的assumptions

``` r
sim_df_nonconst |> 
  ggplot(aes(x = x, y = y)) + 
  geom_point()
```

<img src="bootstrapping_files/figure-gfm/unnamed-chunk-5-1.png" width="90%" />
而这个并不符合假设

What does `lm` do for these?

``` r
sim_df_const |> 
  lm(y ~ x, data = _) |> 
  broom::tidy() |> 
  knitr::kable(digits = 3)
```

| term        | estimate | std.error | statistic | p.value |
|:------------|---------:|----------:|----------:|--------:|
| (Intercept) |    1.977 |     0.098 |    20.157 |       0 |
| x           |    3.045 |     0.070 |    43.537 |       0 |

``` r
sim_df_nonconst |> 
  lm(y ~ x, data = _) |> 
  broom::tidy() |> 
  knitr::kable(digits = 3)
```

| term        | estimate | std.error | statistic | p.value |
|:------------|---------:|----------:|----------:|--------:|
| (Intercept) |    1.934 |     0.105 |    18.456 |       0 |
| x           |    3.112 |     0.075 |    41.661 |       0 |

可以看到这两个样本的sd都是很相近的（方差不相等的结果和方差相等的结果一样），但其实并不是这样，这里sim_df_nonconst得出的结果并不是真实的结果，`lm`是按照assumption得出的数据，但是其实sim_df_nonconst并不符合这些assumption，但我们可以通过bootstrapping模拟得出它在真实data
generating mechanism下产生的结果。

Write a function to draw a bootstrap sample.
\##`size = 1`–抽取原数据集中100%的样本

``` r
boot_sample = function(df) {
  
  sample_frac(df, size = 1, replace = TRUE)
  
}
```

Does this work?

\##点越深代表这个点被抽取的次数越多，点重叠到一起了

``` r
sim_df_nonconst |> 
  boot_sample() |> 
  ggplot(aes(x = x, y = y)) + 
  geom_point(alpha = 0.5) + 
  geom_smooth(method = "lm", se = FALSE) + 
  xlim(c(-2, 4)) + 
  ylim(c(-5, 16))
```

    ## `geom_smooth()` using formula = 'y ~ x'

<img src="bootstrapping_files/figure-gfm/unnamed-chunk-8-1.png" width="90%" />

So I want to formalize this a bit and extract results.
\##加上`\(i)`就相当于可以让bootsample这个function执行5000次，若不加，则R会把这个`i`当成function的argument导致code无法运行

``` r
boot_straps = 
  tibble(
    iter = 1:5000
  ) |> 
  mutate(
    bootstrap_sample = map(iter, \(i) boot_sample(df = sim_df_nonconst))
  )
```

(quick check) \##抽取bootstrap
sample的第二个sample作图（一共有5000个sample）

``` r
boot_straps |> 
  pull(bootstrap_sample) |> 
  nth(2) |> 
  ggplot(aes(x = x, y = y)) + 
  geom_point(alpha = 0.5) + 
  geom_smooth(method = "lm", se = FALSE) + 
  xlim(c(-2, 4)) + 
  ylim(c(-5, 16))
```

    ## `geom_smooth()` using formula = 'y ~ x'

<img src="bootstrapping_files/figure-gfm/unnamed-chunk-10-1.png" width="90%" />

Actually run my analyses!

``` r
bootstrap_results = 
  boot_straps |> 
  mutate(
    fits = map(bootstrap_sample, \(df) lm(y ~ x, data = df)),
    results = map(fits, broom::tidy)
  )
```

Look at results.

``` r
bootstrap_results |> 
  select(iter, results) |> 
  unnest(results) |> 
  group_by(term) |> 
  summarize(
    mean = mean(estimate),
    se   = sd(estimate)
  )
```

    ## # A tibble: 2 × 3
    ##   term         mean     se
    ##   <chr>       <dbl>  <dbl>
    ## 1 (Intercept)  1.93 0.0762
    ## 2 x            3.11 0.103

现在我们可以看到方差不相等的这个样本，它的intercept的sd相比于方差相等的样本更小，也就是它对intercept的estimate更有confidence，而slope的sd更大，也就是对slope的estimate更不confident。这才是我们真正应该会得到的结论

Look at these first

``` r
bootstrap_results |> 
  select(iter, results) |> 
  unnest(results) |> 
  filter(term == "x") |> 
  ggplot(aes(x = estimate)) + 
  geom_density()
```

<img src="bootstrapping_files/figure-gfm/unnamed-chunk-13-1.png" width="90%" />
可以看到是几近normal distribution，但和之前直接做lm不同，这里的sd不一样

95% CI
\##不直接使用mean+-2\*sd的公式来计算95%CI，因为这可能只是看起来像normal，实际不是（但大部分情况你可以用这个公式如果你认为这就是normal的话）

``` r
bootstrap_results |> 
  select(iter, results) |> 
  unnest(results) |> 
  group_by(term) |> 
  summarize(
    ci_lower = quantile(estimate, 0.025),
    ci_upper = quantile(estimate, 0.975)
  )
```

    ## # A tibble: 2 × 3
    ##   term        ci_lower ci_upper
    ##   <chr>          <dbl>    <dbl>
    ## 1 (Intercept)     1.78     2.09
    ## 2 x               2.91     3.32

## Do it again but faster this time

`modelr`
package中有`bootstrap`这个function可以直接用，不用像上面那样自己写公式–`bootstrap(dataset，bootstrap的次数)`

``` r
bootstrap_results = 
  sim_df_const |> 
  bootstrap(n = 5000) |> 
  mutate(
    df = map(strap, as_tibble),
    fits = map(df, \(df) lm(y ~ x, data = df)),
    results = map(fits, broom::tidy)
  ) |> 
  select(.id, results) |> 
  unnest(results)
```

Look at what this means.

``` r
bootstrap_results |> 
  group_by(term) |> 
  summarize(
    mean = mean(estimate),
    se   = sd(estimate)
  )
```

    ## # A tibble: 2 × 3
    ##   term         mean     se
    ##   <chr>       <dbl>  <dbl>
    ## 1 (Intercept)  1.98 0.0974
    ## 2 x            3.04 0.0707

## Airbnb

Remember this one?

``` r
data("nyc_airbnb")

nyc_airbnb =
  nyc_airbnb |> 
  mutate(stars = review_scores_location / 2) |> 
  rename(
    borough = neighbourhood_group
  ) |> 
  filter(
    borough != "Staten Island"
  ) |> 
  drop_na(price, stars, room_type) |> 
  select(price, stars, room_type, borough)
```

Remind me what this looks like?

``` r
nyc_airbnb |> 
  ggplot(aes(x = stars, y = price, color = room_type)) + 
  geom_point(alpha = .5)
```

<img src="bootstrapping_files/figure-gfm/unnamed-chunk-18-1.png" width="90%" />

Try to do the bootstrap.

``` r
airbnb_bootstrap_results = 
  nyc_airbnb |> 
  filter(borough == "Manhattan") |> 
  bootstrap(n = 1000) |> 
  mutate(
    df = map(strap, as_tibble),
    fits = map(df, \(df) lm(price ~ stars + room_type, data = df)),
    results = map(fits, broom::tidy)
  ) |> 
  select(.id, results) |> 
  unnest(results)
```

Look at the distribution of the slope for stars.

``` r
airbnb_bootstrap_results |> 
  filter(term == "stars") |> 
  ggplot(aes(x = estimate)) +
  geom_density()
```

<img src="bootstrapping_files/figure-gfm/unnamed-chunk-20-1.png" width="90%" />
