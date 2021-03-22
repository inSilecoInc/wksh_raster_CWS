## ----setup, include = FALSE---------------------------------
source("_setup.R")
# knitr::clean_cache(TRUE)
htmltools::tagList(
  xaringanExtra::use_clipboard(
    button_text = "<i class=\"fa fa-clipboard\"></i>",
    success_text = "<i class=\"fa fa-check\" style=\"color: #37abc8\"></i>",
  ),
  rmarkdown::html_dependency_font_awesome()
)
xaringanExtra::use_scribble()


## ---- child = "_01_intro.Rmd"-------------------------------

## ----include = FALSE----------------------------------------
source("_setup.R")



## ---- child = "_02_sfmin.Rmd"-------------------------------

## ----include = FALSE----------------------------------------
source("_setup.R")
library(sf)
library(tidyverse)


## ----csv----------------------------------------------------
# Output folder
if (!file.exists('output')) dir.create('output')

# remotes::install_github("vlucet/rgovcan")
library(rgovcan)
id <- "9cd6f8a1-e660-4e78-89a8-6e3f781da556"
govcan_dl_resources(id, path = 'output')

# Import csv
cdqs <- read.csv('output/CDQS_BIOMQ_2017.csv')


## ----csvsf--------------------------------------------------
cdqs <- st_as_sf(x = cdqs,
                 coords = c("CentroideX", "CentroideY"),
                 crs = 4326)


## ----showcsvsf, echo = FALSE--------------------------------
cdqs[,1]


## ----csvplot, fig.width=4, fig.height=4---------------------
plot(cdqs[,1])


## ----shp, messages = FALSE, warnings = FALSE----------------
# Download from open canada
id <- "0c469c0e-8afd-4c62-9420-c50af2d34f97"
govcan_dl_resources(id, path = 'output')
unzip("output/Sterna_dougallii_Roseate_Tern_CH_2015.zip", exdir = 'output')


## ----showshp, messages = FALSE, warnings = FALSE------------
# Import shapefile
roseate <- st_read("output/Sterna_dougallii_Roseate_Tern_CH_2015.shp", quiet = TRUE)
roseate


## ----shpplot------------------------------------------------
plot(st_geometry(roseate[4,]))


## ----geojson, hold = TRUE-----------------------------------
stl <- st_read('data/st_laurence.geojson', quiet = TRUE)
stl


## ----geojsonplot, fig.width=5, fig.height=4, eval=TRUE------
plot(st_geometry(stl))


## ----gbd, messages = FALSE, warnings = FALSE----------------
id <- "f612e2b4-5c67-46dc-9a84-1154c649ab4e"
govcan_dl_resources(id, path = 'output')
unzip("output/AtlasGrid-GrilleAtlas.gdb.zip", exdir = 'output')


## ----gbdlayer, messages = FALSE, warnings = FALSE-----------
# Visualize layers available in the geodatabase
st_layers("output/AtlasGrid-GrilleAtlas.gdb")


## ----gbdload, messages = FALSE, warnings = FALSE------------
# Load layer
atlas <- st_read('output/AtlasGrid-GrilleAtlas.gdb',
                  layer = 'AtlasGrid_GrilleAtlas')


## ----gbdplot, messages = FALSE, warnings = FALSE, fig.width=4,fig.height=4----
plot(st_geometry(atlas))


## ----export-------------------------------------------------
st_write(roseate, "output/roseate.shp", delete_dsn = TRUE)
st_write(stl, "output/stl.geojson", delete_dsn = TRUE)


## ----echo = FALSE-------------------------------------------
oldopt <- options()$width
options(width=150)


## ----drivers------------------------------------------------

st_drivers()


## ----echo = FALSE-------------------------------------------
options(width=oldopt)


## ----sp, eval = FALSE---------------------------------------
## # Do not run
## newsf <- st_as_sf(oldsp)


## ----spsf, eval = FALSE-------------------------------------
## # Do not run
## newsp <- as(oldsf, "Spatial")


## ----crs----------------------------------------------------
cdqs <- st_transform(cdqs, st_crs(atlas))


## ----attr_hist, fig.width=6,fig.height=4--------------------
hist(cdqs$Nombre.de.nicheurs.Number.of.Breeders)


## ----attr_add, fig.width=6,fig.height=4---------------------
cdqs$var2 <- log(cdqs$Nombre.de.nicheurs.Number.of.Breeders+1)
hist(cdqs$var2)


## ----attr_add2, fig.width=6,fig.height=4--------------------
cdqs <- mutate(cdqs, var3 = log(Nombre.de.nicheurs.Number.of.Breeders+1))
hist(cdqs$var3)


## ----attr_rem, fig.width=6,fig.height=4---------------------
cdqs$var2 <- NULL
cdqs <- cdqs %>% select(-var3)
colnames(cdqs)


## ----use_pipe-----------------------------------------------
# Example with pipe operator
cdqs <- cdqs %>%
        mutate(var3 = log(Nombre.de.nicheurs.Number.of.Breeders+1)) %>%
        select(-var3)
'var3' %in% colnames(cdqs)


## ----nopipe-------------------------------------------------
cdqs <- mutate(cdqs, var3 = log(Nombre.de.nicheurs.Number.of.Breeders+1))
cdqs <- select(cdqs, -var3)
'var3' %in% colnames(cdqs)


## ----attr_sub, fig.width=4,fig.height=4---------------------
newcdqs <- cdqs[cdqs$Annee.Year > 2000, ]
plot(st_geometry(cdqs), pch = 1, col = '#2ae9ee', cex = 1)
plot(st_geometry(newcdqs), pch = 20, col = '#d76060', cex = .8, add = TRUE)


## ----attr_join, fig.width=6,fig.height=3.5------------------
atlas


## ----attr_join2, fig.width=6,fig.height=3.5-----------------
join_data <- data.frame(id = paste0('g', sample(nrow(atlas), nrow(atlas))),
                        value = runif(nrow(atlas)))
join_data[1:10, ]


## ----attr_join3, fig.width=6,fig.height=3.5-----------------
newatlas <- left_join(atlas, join_data, by='id')


## ---- echo = FALSE------------------------------------------
newatlas[1:10,c('id','value','Shape'), drop = TRUE]


## ---- echo = FALSE, fig.width=4, fig.height=3.5-------------
plot(newatlas[,'value'])


## ----area---------------------------------------------------
atlas$area <- st_area(atlas)
atlas


## ----distance-----------------------------------------------
st_distance(atlas[1:5, ], cdqs[1:5,])


## ----intersect----------------------------------------------
st_intersects(atlas[151:160, ], cdqs)


## ----buffer1, fig.width = 4, fig.height=3-------------------
newatlas <- st_buffer(atlas[1:10, ], 30000)
plot(st_geometry(atlas[1:10, ]))
plot(st_geometry(newatlas),
     border = 'red', add = TRUE)


## ----buffer2, fig.width = 4, fig.height=3-------------------
newatlas <- st_buffer(atlas[1:10, ], -30000)
plot(st_geometry(atlas[1:10, ]))
plot(st_geometry(newatlas),
     border = 'red', add = TRUE)


## ----union, fig.width = 6, fig.height=4---------------------
newatlas <- st_union(atlas)
plot(newatlas)


## ----convex, fig.width = 4, fig.height=3--------------------
cdqshull <- st_convex_hull(st_union(cdqs))
plot(st_geometry(cdqshull))
plot(st_geometry(cdqs), add = TRUE)


## ----intersection, fig.width = 4, fig.height=4--------------
cdqs_atlas <- st_intersection(newatlas, cdqshull)
plot(st_geometry(newatlas))
plot(st_geometry(cdqshull), add = TRUE)
plot(st_geometry(cdqs_atlas), col = 'red', add = TRUE)


## ----difference, fig.width = 4, fig.height=4----------------
cdqs_atlas <- st_difference(newatlas, cdqshull)
plot(st_geometry(newatlas))
plot(st_geometry(cdqshull), add = TRUE)
plot(st_geometry(cdqs_atlas), col = 'red', add = TRUE)


## ----difference2, fig.width = 4, fig.height=4---------------
cdqs_atlas <- st_difference(cdqshull, newatlas)
plot(st_geometry(newatlas))
plot(st_geometry(cdqshull), add = TRUE)
plot(st_geometry(cdqs_atlas), col = 'red', add = TRUE)


## ----spatjoin1, fig.width = 4, fig.height=3-----------------
atlasJ <- st_join(atlas, cdqs)
plot(atlasJ[,'Nombre.de.nicheurs.Number.of.Breeders'])


## ----spatjoin2, fig.width = 4, fig.height=3-----------------
cdqsJ <- st_join(cdqs, atlas)
plot(cdqsJ[,'id'])



## ---- child = "_02e_sfmin.Rmd"------------------------------

## ----include = FALSE----------------------------------------
source("_setup.R")


