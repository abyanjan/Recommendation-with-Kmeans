Content Based Filtering: Movie Recommendation
================

In this tutorial contente based filtering will be applied with kmeans clustering to recommend movies to users. Data used in the analysis is the movielens dataset, which can be accessed [here](https://grouplens.org/datasets/movielens/)

### Load library and data

``` r
library(tidyverse)
```

    ## Warning: package 'tidyverse' was built under R version 3.4.4

    ## -- Attaching packages -------------------------------------------------------------- tidyverse 1.2.1 --

    ## v ggplot2 3.1.0       v purrr   0.2.4  
    ## v tibble  2.1.1       v dplyr   0.8.0.1
    ## v tidyr   0.8.0       v stringr 1.3.1  
    ## v readr   1.1.1       v forcats 0.3.0

    ## Warning: package 'ggplot2' was built under R version 3.4.4

    ## Warning: package 'tibble' was built under R version 3.4.4

    ## Warning: package 'tidyr' was built under R version 3.4.4

    ## Warning: package 'dplyr' was built under R version 3.4.4

    ## Warning: package 'stringr' was built under R version 3.4.4

    ## Warning: package 'forcats' was built under R version 3.4.4

    ## -- Conflicts ----------------------------------------------------------------- tidyverse_conflicts() --
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
# Data
```
