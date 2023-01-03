Patchmax
================
Pedro Belavenutti & Cody Evers

<!--- README.md is generated from README.Rmd. Please edit that file -->

<img src="man/figures/forsys_icon.png" align="right" style="height:90px!important; position:absolute; top:10px; right:10px" />

## Description

Patchmax is a computational module designed to explore spatially
explicit landscape treatment projects. The package can be used to
prioritize treatment locations on relatively small landscapes (project
level implementation, 1,000 ha) to large landscapes (national forests to
multi-regional planning efforts, 1 million ha). Although patchmax was
originally designed for hazardous fuel treatment planning in forested
systems, it can be applied to fiber production, habitat restoration, and
other resource management problems. Patchmax utilizes Djistra’s
algorithm to sequence stands within the projects based on both adjacency
and distance, which is modified according to the model parameters.

Dependences: R (\>= 4.0.3) and packages (igraph, data.table, sf,
doParallel, parallel, spdep, pbapply)

## Installation

The latest official version of the Patchmax package can be installed
from GitHub using the following code after we provide your personal
access token.

``` r
if (!require(remotes)) install.packages("remotes")
remotes::install_github("forsys-sp/patchmax", auth_token = 'your_token_here')
```

## Usage

Stand treatment units are represented as polygons in a spatial vector
format. Each polygon represents a different treatment unit.

``` r
library(patchmax)
pm <- patchmax_generator$new(...)
pm$area_max = 1200
pm$search()$build()$record()
```

First, let’s load some example data and map the included fields. To keep
things simple, we focus only a subset of the entire study area.

``` r
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following object is masked from 'package:patchmax':
    ## 
    ##     sample_frac

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library(sf)
```

    ## Linking to GEOS 3.11.0, GDAL 3.5.3, PROJ 9.1.0; sf_use_s2() is TRUE

``` r
geom <- patchmax::test_forest %>% 
  filter(row > 20, row <= 40, col > 20, col <= 40)

geom %>% 
  select(matches('p[0-9]|t[0-9]|b[0-9]|m[0-9]|c[0-9]')) %>%
  plot(max.plot = 20, border=NA)
```

![](man/figures/map_attributes-1.png)<!-- -->

We can combined these data to create additional fields. For example,
let’s create a new field called cost, which we’ll use later as a
secondary constraint building patches.

``` r
geom <- geom %>% mutate(cost = ((p2 + p4 - c1) * 1000) + 3000)
plot(geom[,'cost'], border=NA)
```

![](man/figures/unnamed-chunk-4-1.png)<!-- -->

Patchmax is written as R6 class, which uses a slightly different syntax
the functional programming design used in most R functions. To use
patchmax, we first create a patchmax object called `pm` using the new
method in the patchmax_generator class. `print(pm)` shows how the object
is structured into public and private components. Several of the public
elements are called active bindings, which is used to get and set
private elements under very specific conditions. The private elements
cannot be directly access or changed from the outside.

``` r
pm <- patchmax_generator$new(
  geom = geom, 
  id_field = 'id', 
  objective_field = 'p4', 
  area_field = 'ha', 
  area_max = 1000)
```

The core purpose of patchmax is build spatial contiguous patches that
maximizes some objective given some maximum size constraint. The example
below shows how this is done over a series of steps: (1) patchmax
searches for the best place, (2) builds that patch, (3) plots the patch,
(4) records the patch, (5) describe the patch statistics.

``` r
pm$search()
```

    ## Best start: 2832 (10% search)

``` r
pm$build()
```

    ## Patch stats: start 2832, area 1000, score 8.75, constraint 0, excluded 0%

    ## 

``` r
pm$plot()
```

![](man/figures/basic_example-1.png)<!-- --> Note that the same set of
sequence of steps can be chained together into a single line:
`pm$search()$build$plot()`, which shows how multiple actions can be
combined in different ways to produce different results.

To get a better sense of how the patch is selected, let’s look at the
search results in more detail.

``` r
pm$search(sample_frac = 1, plot_search = T)
```

    ## Best start: 2931 (100% search)

![](man/figures/search_example-1.png)<!-- -->

## Studies

Belavenutti, Pedro, Alan A. Ager, Michelle A. Day, and Woodam Chung.
2022. Designing forest restoration projects to optimize the application
of broadcast burning. Ecological Economics.
