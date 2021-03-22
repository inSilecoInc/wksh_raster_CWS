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

### Day 1

* [Part 1: introduction + vectors (`sf`)](https://insilecoinc.github.io/wksh_raster_CWS/_part1.html) 
  - [presentation](https://insilecoinc.github.io/wksh_raster_CWS/_part1.html) 
  - [code](https://insilecoinc.github.io/wksh_raster_CWS/scripts/_part1.R)

* [Part 2: raster (`stars`)](https://insilecoinc.github.io/wksh_raster_CWS/_part2.html)
  - [presentation](https://insilecoinc.github.io/wksh_raster_CWS/_part2.html) 
  - [code](https://insilecoinc.github.io/wksh_raster_CWS/scripts/_part2.R)

* [Part 3: visualisation (`tmap`)](https://insilecoinc.github.io/wksh_raster_CWS/_part3.html)
  - [presentation](https://insilecoinc.github.io/wksh_raster_CWS/_part3.html) 
  - [code](https://insilecoinc.github.io/wksh_raster_CWS/scripts/_part3.R)


### Day 2

