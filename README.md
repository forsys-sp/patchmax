Patchmax a computational module designed to explore spatially explicit
landscape treatment projects
================
Pedro Belavenutti

<!--- README.md is generated from README.Rmd. Please edit that file -->

<img src="man/figures/forsys_icon.png" align="right" style="height:90px!important; position:absolute; top:10px; right:10px" />

## Description

Patchmax is a computational module designed to explore spatially
explicit landscape treatment projects. The package can be used to
prioritize treatment locations on relatively small landscapes (project
level implementation, 1,000 ha) to large landscapes (national forest to
multi-regional planning efforts, 1 million ha). Although patchmax was
originally designed for hazardous fuel treatment planning in forested
systems, it can be applied to fiber production, habitat restoration, and
other resource management problems. Patchmax utilizes breadth-first
search in the igraph package to sequence stands within the projects. To
improve efficiency, the application employs graph theory algorithms
combined with binary searches while running on a parallel computing
platform.

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
plot(help(calculate_adj))
```

## Studies

Belavenutti, Pedro, Alan A. Ager, Michelle A. Day, and Woodam Chung.
2022. Designing forest restoration projects to optimize the application
of broadcast burning. Ecological Economics.
