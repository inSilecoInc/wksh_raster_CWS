# Rasters with R

![deploy workshop](https://github.com/inSilecoInc/workshop_R_template/workflows/deploy%20workshop/badge.svg)


## Installation 

To reproduced the presentation, once the repository is clones, one can use the following lines:


```{R}
install.packages("remotes")
remotes::install_deps()
```

Then the different `Rmd` files can be run with [`xaringan`](https://cran.r-project.org/web/packages/xaringan/index.html). 

To run the example of the workshop (without reproducing the entire presentation), 
use the following 


```{R}
install.packages(c("mapview", "stars", "shiny", "mapedit", "tmap"))
```

## Presentations

* [Part 1: introduction + vectors (`sf`)](_part1.html)

* [Part 2: raster (`stars`](_part2.html)

* [Part 2: visualisation (`tmap`)](_part2.html)