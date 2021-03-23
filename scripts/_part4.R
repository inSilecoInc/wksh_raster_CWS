## ----load_leaflet, fig.width = 6, fig.height = 6, echo = FALSE----
library(leaflet)
lf <- leaflet() %>%
 addTiles(group = 'Default') %>%
 setView(lng = -63,
         lat = 48,
         zoom = 5)

lf




## ----base_maps, fig.width = 6, fig.height = 6, echo = FALSE----
leaflet() %>%
      setView(lng = -63,
              lat = 48,
              zoom = 5) %>%
      addProviderTiles('Esri.OceanBasemap',
                       group = 'Ocean')




## ----multiple_base, fig.width = 6, fig.height = 6, echo = FALSE----
lf <- lf %>%
 addProviderTiles('Esri.OceanBasemap',
     group = 'Ocean') %>%
 addProviderTiles("OpenTopoMap",
     group = "Topo") %>%

 # Add layer selection
 addLayersControl(
     baseGroups = c('Default','Ocean','Topo'),
     position = 'topleft')

lf


## ----eDrivers, eval = TRUE-------------------------------
# Install eDrivers
# devtools::install_github('eDrivers/eDrivers')

# Load data from eDrivers
library(eDrivers)
fetchDrivers(drivers = c('Hypoxia','FisheriesDD','Acidification'), output = 'data')

# Raster objects from eDrivers class objects
library(stars)
hyp <- read_stars('data/Hypoxia.tif')
fish <- read_stars('data/FisheriesDD.tif')
acid <- read_stars('data/Acidification.tif')


## ----load_leafem-----------------------------------------
library(leafem)




## ----leaflet_raster, fig.width = 6, fig.height = 6, echo = FALSE, eval = TRUE----
# Add layers to leaflet map
lf <- lf %>%
 addStarsImage(hyp, group = 'Hyp') %>%
 addStarsImage(fish ,group = 'Fish') %>%
 addStarsImage(acid, group = 'Acid') %>%

# Reset layer selection
addLayersControl(
   baseGroups = c('Default','Ocean','Topo'),
   overlayGroups = c('Hyp','Fish','Acid'),
   position = 'topleft')

lf


## ----export_leaflet, eval = FALSE------------------------
## dir.create('output')
## htmlwidgets::saveWidget(lf, file="output/lf.html")


## ----mapview, eval = FALSE, out.width = "100%"-----------
## library(mapview)
## mv <- mapview(hyp) +
##   mapview(fish) +
##   mapview(acid)
## mv@map


## ----mapview2, echo = FALSE, eval = TRUE, out.width = "100%"----
library(mapview)
mv <- mapview(hyp) + mapview(fish) + mapview(acid)
mv@map


## ----mapview_export, eval = TRUE, fig.width = 12, fig.height = 6----
dir.create('output')
mapshot(mv, url = 'output/map.html')



## ---- child = "_08e_leaflet.Rmd"-------------------------

## ----include = FALSE-------------------------------------
source("_setup.R")


