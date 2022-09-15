<!--- README.md is generated from README.Rmd. Please edit that file -->

## Belavenutti, Pedro, Alan A. Ager, Michelle A. Day, and Woodam Chung. 2022. Designing forest restoration projects to optimize the application of broadcast burning. Ecological Economics.

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
improve efficiency, the application implements binary searches with
data.table package while running on a parallel computing platform with
doParallel package. Dependences: R (&gt;= 4.0.3) and packages (igraph,
data.table, sf, doParallel, parallel)

## Installation
