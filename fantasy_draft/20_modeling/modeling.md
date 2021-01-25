Modeling
================

# Prepare data

``` r
library(tidyverse)
```

    ## -- Attaching packages ---------------------------------------------------------- tidyverse 1.3.0 --

    ## v ggplot2 3.3.2     v purrr   0.3.4
    ## v tibble  3.0.3     v dplyr   1.0.2
    ## v tidyr   1.1.2     v stringr 1.4.0
    ## v readr   1.3.1     v forcats 0.5.0

    ## -- Conflicts ------------------------------------------------------------- tidyverse_conflicts() --
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
library(readr)
library(h2o)
```

    ## Warning: package 'h2o' was built under R version 4.0.3

    ## 
    ## ----------------------------------------------------------------------
    ## 
    ## Your next step is to start H2O:
    ##     > h2o.init()
    ## 
    ## For H2O package documentation, ask for help:
    ##     > ??h2o
    ## 
    ## After starting H2O, you can use the Web UI at http://localhost:54321
    ## For more information visit https://docs.h2o.ai
    ## 
    ## ----------------------------------------------------------------------

    ## 
    ## Attaching package: 'h2o'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     cor, sd, var

    ## The following objects are masked from 'package:base':
    ## 
    ##     %*%, %in%, &&, ||, apply, as.factor, as.numeric, colnames,
    ##     colnames<-, ifelse, is.character, is.factor, is.numeric, log,
    ##     log10, log1p, log2, round, signif, trunc

``` r
library(glue)
```

    ## 
    ## Attaching package: 'glue'

    ## The following object is masked from 'package:dplyr':
    ## 
    ##     collapse

``` r
library(lemon)
```

    ## 
    ## Attaching package: 'lemon'

    ## The following object is masked from 'package:purrr':
    ## 
    ##     %||%

    ## The following objects are masked from 'package:ggplot2':
    ## 
    ##     CoordCartesian, element_render

``` r
knit_print.data.frame <- lemon_print
library(knitr)
h2o.init(nthreads = -1, #Number of threads -1 means use all cores on your machine
         max_mem_size = "8G")  #max mem size is the maximum memory to allocate to H2O
```

    ##  Connection successful!
    ## 
    ## R is connected to the H2O cluster: 
    ##     H2O cluster uptime:         1 hours 8 minutes 
    ##     H2O cluster timezone:       America/Chicago 
    ##     H2O data parsing timezone:  UTC 
    ##     H2O cluster version:        3.32.0.1 
    ##     H2O cluster version age:    2 months and 10 days  
    ##     H2O cluster name:           H2O_started_from_R_Matt_Flaherty_qia640 
    ##     H2O cluster total nodes:    1 
    ##     H2O cluster total memory:   8.00 GB 
    ##     H2O cluster total cores:    4 
    ##     H2O cluster allowed cores:  4 
    ##     H2O cluster healthy:        TRUE 
    ##     H2O Connection ip:          localhost 
    ##     H2O Connection port:        54321 
    ##     H2O Connection proxy:       NA 
    ##     H2O Internal Security:      FALSE 
    ##     H2O API Extensions:         Amazon S3, Algos, AutoML, Core V3, TargetEncoder, Core V4 
    ##     R Version:                  R version 4.0.2 (2020-06-22)

``` r
fantasy <- read_csv("../data/fantasy.csv")
```

    ## Warning: Missing column names filled in: 'X1' [1]

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   player = col_character(),
    ##   pos.x = col_character(),
    ##   tm.x = col_character()
    ## )

    ## See spec(...) for full column specifications.

``` r
fantasy <- as.data.frame(fantasy)

# no non-numeric data
fantasy <- fantasy %>%
  select(-X1, -pos.x, -tm.x)
```

Call H2O and split the data.

# Split the data

``` r
test <- fantasy %>%
  filter(year == "2020") 
row.names(test) <- test$player
test <- test %>%
  select(-player, -year) %>%
  apply(2, function(r) {if (sd(r)!=0) res=(r-mean(r))/sd(r) else res=0*r; res}) %>%
  as.data.frame()

train <- fantasy %>%
  filter(year != "2020") %>%
  select(-player, - year) %>%
  apply(2, function(r) {if (sd(r)!=0) res=(r-mean(r))/sd(r) else res=0*r; res}) %>%
  as.data.frame()
```

``` r
h2o.train_data <- as.h2o(train)
```

    ## Warning in use.package("data.table"): data.table cannot be used without R
    ## package bit64 version 0.9.7 or higher. Please upgrade to take advangage of
    ## data.table speedups.

    ##   |                                                                              |                                                                      |   0%  |                                                                              |======================================================================| 100%

``` r
h2o.test_data <- as.h2o(test)
```

    ## Warning in use.package("data.table"): data.table cannot be used without R
    ## package bit64 version 0.9.7 or higher. Please upgrade to take advangage of
    ## data.table speedups.

    ##   |                                                                              |                                                                      |   0%  |                                                                              |======================================================================| 100%

``` r
y <- "fpts_100"
x <- setdiff(names(train), c(y))
```

# Run models

``` r
rf_fit2 <- h2o.glm(x = x,
                            y = y,
                            training_frame = h2o.train_data,
                            model_id = "rf_fit2",
                            #validation_frame = valid,  #only used if stopping_rounds > 0
                            nfolds = 10,
                   alpha = 0.5)
```

    ##   |                                                                              |                                                                      |   0%  |                                                                              |=================================================================     |  93%  |                                                                              |======================================================================| 100%

``` r
rf_fit2@model$cross_validation_metrics
```

    ## H2ORegressionMetrics: glm
    ## ** Reported on cross-validation data. **
    ## ** 10-fold cross-validation on training data (Metrics computed for combined holdout predictions) **
    ## 
    ## MSE:  0.001327193
    ## RMSE:  0.03643066
    ## MAE:  0.02690335
    ## RMSLE:  NaN
    ## Mean Residual Deviance :  0.001327193
    ## R^2 :  0.9986714
    ## Null Deviance :942.0853
    ## Null D.o.F. :940
    ## Residual Deviance :1.248889
    ## Residual D.o.F. :917
    ## AIC :-3513.39

# Calculate value

``` r
fpts_predict <- as.data.frame(h2o.predict(rf_fit2, h2o.test_data)) # Predict entire dataset (note that we are not using out-of-bag samples)
```

    ##   |                                                                              |                                                                      |   0%  |                                                                              |======================================================================| 100%

``` r
outcome <- test %>%
  select(fpts_100) %>%
  cbind(fpts_predict) %>%
  arrange(desc(predict))

head(outcome)
```

    ##                                  fpts_100  predict
    ## Shamorie Ponds\\pondssh01        3.770089 3.686009
    ## Giannis Antetokounmpo\\antetgi01 3.372194 3.514670
    ## Luka Don?i?\\doncilu01           2.673190 2.802980
    ## LeBron James\\jamesle01          2.597913 2.723678
    ## Johnathan Motley\\motlejo01      2.640929 2.666770
    ## Nikola Joki?\\jokicni01          2.619421 2.659320

# Top Predictors

``` r
h2o.varimp(rf_fit2) %>%
  select(variable, scaled_importance) %>%
  mutate(scaled_importance = round(scaled_importance, 2)) %>%
  head(10) %>%
  kable()
```

| variable     | scaled\_importance |
| :----------- | -----------------: |
| per          |               1.00 |
| ast          |               0.85 |
| drb          |               0.43 |
| blk          |               0.33 |
| ws\_48       |               0.29 |
| stl          |               0.19 |
| tov          |               0.18 |
| stl\_percent |               0.16 |
| blk\_percent |               0.15 |
| fta          |               0.12 |
