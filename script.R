## ----setup, include = FALSE---------------------------------------------------
source("_setup.R")
knitr::clean_cache(TRUE)


## ---- child = "_01_intro.Rmd"-------------------------------------------------

## ----include = FALSE----------------------------------------------------------
source("_setup.R")



## ---- child = "_02_sfmin.Rmd"-------------------------------------------------

## ----include = FALSE----------------------------------------------------------
source("_setup.R")



## ---- child = "_02e_sfmin.Rmd"------------------------------------------------

## ----include = FALSE----------------------------------------------------------
source("_setup.R")



## ---- child = "_03_stars_base.Rmd"--------------------------------------------

## ----include = FALSE----------------------------------------------------------
source("_setup.R")


## ----read_stars---------------------------------------------------------------
library(stars)
ras <- read_stars("data/bathy.tif")   # argument `driver` to specify the driver
class(ras)


## ----plot_stars, cache = TRUE-------------------------------------------------
plot(ras)


## ----stars_drivers------------------------------------------------------------
st_dr <- sf::st_drivers(what = "raster")
head(st_dr)


## ----stars_drivers2-----------------------------------------------------------
st_dr[which(st_dr$name == "GTiff"), ]


## ----create_dir---------------------------------------------------------------
dir.create("output", showWarnings = FALSE)


## ----write_stars0-------------------------------------------------------------
sort(st_dr$name[st_dr$write])


## ----write_stars1, cache = TRUE-----------------------------------------------
write_stars(ras, dsn = "output/ras.gpkg", driver = "GPKG")



## ---- child = "_04_stars_manip.Rmd"-------------------------------------------

## ----include = FALSE----------------------------------------------------------
source("_setup.R")


## ----stars_obj----------------------------------------------------------------
library(stars)
ras <- read_stars("data/bathy.tif") 
class(ras)
ras


## ----stars_val----------------------------------------------------------------
class(ras[[1]])
ras[[1]]


## ----stars_obj_fun1-----------------------------------------------------------
dim(ras)
st_bbox(ras)


## ----stars_obj_fun2-----------------------------------------------------------
ras_crs <- st_crs(ras)
class(ras_crs)
ras_crs


## ----stars_obj_fun3-----------------------------------------------------------
ras_crs$input
ras_crs$proj4string
ras_crs$epsg


## ----convert2-----------------------------------------------------------------
ras_c <- as(ras, "Raster") 
class(ras_c)


## ----bc_ras1------------------------------------------------------------------
# extract value
class(ras[[1]])
ras[[1]][1, 1]
ras[[1]][1:10, 11:20]


## ----bc_ras1_2----------------------------------------------------------------
mean(ras[[1]]) 
quantile(ras[[1]])


## ----bc_rar1_3----------------------------------------------------------------
ras2 <- ras # create a copy
ras2[[1]][ras[[1]] > units::as_units(0, "m")] <- NA # filter
plot(ras2)


## ----projrar2, cache = TRUE---------------------------------------------------
ras_t <- st_transform(ras, crs = 3857)
st_crs(ras_t)


## ----crop0--------------------------------------------------------------------
# show what it is
# plot(rar)
# rect(-65, 45, -60, 50)


## ----crop_ras, cache = TRUE---------------------------------------------------
ext <- st_bbox(c(xmin = -65, xmax = -60, ymin = 45, ymax = 50), crs = 4326)
ras_c <- st_crop(ras, ext)
plot(ras_c)


## ----stl----------------------------------------------------------------------
(stl <- sf::st_read("data/st_laurence.geojson"))


## ----plot_stl, cache = TRUE---------------------------------------------------
plot(sf::st_geometry(stl))


## ----ras_m, cache = TRUE------------------------------------------------------
ras_m <- ras[stl]
plot(ras_m)


## ----ras_m1, cache = TRUE-----------------------------------------------------
stl_i <- sf::st_difference(sf::st_as_sfc(st_bbox(ras)), stl)
plot(stl_i, col = 2)


## ----ras_m2, cache = TRUE-----------------------------------------------------
ras_mi <- ras[stl_i]
plot(ras_mi)


## ----ras_warp, cache = TRUE---------------------------------------------------
ras_template1 <- st_as_stars(st_bbox(ras), nx = 21, ny = 21, 
  values = runif(21 * 21)) # create template
ras_w1 <- st_warp(ras, ras_template1)
plot(ras_w1)


## ----ras_warp2, cache = TRUE--------------------------------------------------
ras_w2 <- st_warp(ras, cellsize = 0.25, crs = st_crs(ras)) 
plot(ras_w2)


## ----rasters1, cache = TRUE---------------------------------------------------
stl_s <- st_rasterize(stl, dy = .1, dx = .1)
plot(stl_s)


## ----ras_stc0-----------------------------------------------------------------
ras


## ----ras_stc1-----------------------------------------------------------------
ras_stc1 <- c(ras, ras * 2)
names(ras_stc1) <- c("bath_v1", "bath_v2")
ras_stc1


## ----ras_stc1b----------------------------------------------------------------
plot(ras_stc1)


## ----ras_stc2-----------------------------------------------------------------
ras_stc1[1] # or ras_stc1["bath_v1"]


## ----ras_stc2b----------------------------------------------------------------
ras_stc1[2] # or ras_stc1["bath_v2"]


## ----ras_stc2c----------------------------------------------------------------
library(dplyr)
ras_stc1 %>% select("bath_v1")


## ----ras_stc2d----------------------------------------------------------------
ras_stc1 %>% mutate(bath_v3 = bath_v2 * 2)


## ----ras_stc3, cache = TRUE---------------------------------------------------
ras_stc2 <- c(ras, ras*2, along = "z")
ras_stc2


## ----ras_stc4, cache = TRUE---------------------------------------------------
plot(ras_stc2)


## ----ras_stc5, cache = TRUE---------------------------------------------------
ext <- st_bbox(c(xmin = -65, xmax = -60, ymin = 45, ymax = 50), crs = 4326)
plot(st_crop(ras_stc2, ext))


## ----ras_stc6, cache = TRUE---------------------------------------------------
(ras_ap <- st_apply(ras_stc2, c(1, 2), sum))


## ----ras_stc6b, cache = TRUE--------------------------------------------------
plot(ras_ap)



## ---- child = "_04e_stars_manip.Rmd"------------------------------------------

## ----include = FALSE----------------------------------------------------------
source("_setup.R")



## ---- child = "_05_visualise.Rmd"---------------------------------------------

## ----include = FALSE----------------------------------------------------------
source("_setup.R")



## ---- child = "_06_tmap.Rmd"--------------------------------------------------

## ----include = FALSE----------------------------------------------------------
source("_setup.R")



## ---- child = "_06e_tmap.Rmd"-------------------------------------------------

## ----include = FALSE----------------------------------------------------------
source("_setup.R")



## ---- child = "_07_leaflet_base.Rmd"------------------------------------------

## ----include = FALSE----------------------------------------------------------
source("_setup.R")



## ---- child = "_08_leaflet_manip.Rmd"-----------------------------------------

## ----include = FALSE----------------------------------------------------------
source("_setup.R")




## ----load_leaflet, fig.width = 6, fig.height = 6, echo = FALSE----------------
library(leaflet)
lf <- leaflet() %>%
 addTiles(group = 'Default') %>%
 setView(lng = -63,
         lat = 48,
         zoom = 5)

lf


## ----use_pipe-----------------------------------------------------------------
# Example with pipe operator
lf <- leaflet() %>%
 setView(lng = -63,
         lat = 48,
         zoom = 5) %>%
 addTiles(group = 'Default')



## ----nopipe-------------------------------------------------------------------
# Example without pipe operator
lf <- setView(map = leaflet(),
              lng = -63,
              lat = 48,
              zoom = 5)
lf <- addTiles(map = lf,
               group = 'Default')




## ----base_maps, fig.width = 6, fig.height = 6, echo = FALSE-------------------
leaflet() %>%
      setView(lng = -63,
              lat = 48,
              zoom = 5) %>%
      addProviderTiles('Esri.OceanBasemap',
                       group = 'Ocean')




## ----multiple_base, fig.width = 6, fig.height = 6, echo = FALSE---------------
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


## ----eDrivers, eval = TRUE----------------------------------------------------
# Install eDrivers
# devtools::install_github('eDrivers/eDrivers')

# Load data from eDrivers
library(eDrivers)
fetchDrivers(drivers = c('Hypoxia','FisheriesDD','Acidification'),
             output = 'data')

# Raster objects from eDrivers class objects
library(raster)
hyp <- raster('data/Hypoxia.tif')
fish <- raster('data/FisheriesDD.tif')
acid <- raster('data/Acidification.tif')




## ----leaflet_raster, fig.width = 6, fig.height = 6, echo = FALSE, eval = TRUE----
# Add layers to leaflet map
lf <- lf %>%
 addRasterImage(hyp,group = 'Hyp') %>%
 addRasterImage(fish,group = 'Fish') %>%
 addRasterImage(acid,group = 'Acid') %>%

# Reset layer selection
addLayersControl(
   baseGroups = c('Default','Ocean','Topo'),
   overlayGroups = c('Hyp','Fish','Acid'),
   position = 'topleft')

lf


## ----export_leaflet, eval = FALSE---------------------------------------------
## dir.create('output')
## htmlwidgets::saveWidget(lf, file="output/lf.html")




## ----mapview, eval = TRUE, fig.width = 12, fig.height = 6---------------------
library(mapview)
mv <- mapview(hyp) + fish + acid
mv


## ----mapview_export, eval = TRUE, fig.width = 12, fig.height = 6--------------
dir.create('output')
mapshot(mv, url = 'output/map.html')



## ---- child = "_08e_leaflet_manip.Rmd"----------------------------------------

## ----include = FALSE----------------------------------------------------------
source("_setup.R")



## ---- child = "_09_shiny_intro.Rmd"-------------------------------------------

## ----include = FALSE----------------------------------------------------------
source("_setup.R")



## ---- child = "_10_shiny_mini.Rmd"--------------------------------------------

## ----include = FALSE----------------------------------------------------------
source("_setup.R")



## ---- child = "_10e_shiny_mini.Rmd"-------------------------------------------

## ----include = FALSE----------------------------------------------------------
source("_setup.R")



## ---- child = "_11_shiny_spatial.Rmd"-----------------------------------------

## ----include = FALSE----------------------------------------------------------
source("_setup.R")


## ----shiny, eval = FALSE------------------------------------------------------
## # General environment / data preparation
## library(shiny)
## library(leaflet)
## library(raster)
## Hypoxia <- raster('data/Hypoxia.tif')
## Acidification <- raster('data/Acidification.tif')
## Fisheries <- raster('data/FisheriesDD.tif')
## rstack <- stack(Hypoxia, Acidification, Fisheries)
## 
## # User interface
## ui <- fluidPage(
##  radioButtons(inputId = 'raster', label = 'Rasters to select',
##    choices = c("Hypoxia" = "Hypoxia", "Fisheries" = "FisheriesDD",
##                "Acidification" = "Acidification")),
##  leafletOutput(outputId = "map", height = 800)
## )
## 
## # Server function
## server <- function(input, output) {
##  output$map <- renderLeaflet({
##   leaflet() %>%
##   addProviderTiles("CartoDB.Positron") %>%
##   addRasterImage(rstack[[input$raster]])
##  })
## }
## 
## # Shiny app function
## shinyApp(ui, server)


## ---- eval = TRUE-------------------------------------------------------------
# General environment / data preparation
library(shiny)
library(leaflet)
library(raster)
Hypoxia <- raster('data/Hypoxia.tif')
Acidification <- raster('data/Acidification.tif')
Fisheries <- raster('data/FisheriesDD.tif')
rstack <- stack(Hypoxia, Acidification, Fisheries)


## ---- eval = FALSE------------------------------------------------------------
## radioButtons(inputId = 'raster', label = 'Rasters to select',
##   choices = c("Hypoxia" = "Hypoxia", "Fisheries" = "FisheriesDD",
##               "Acidification" = "Acidification")),


## ---- eval = FALSE------------------------------------------------------------
## input <- list(raster = 'Hypoxia')
## input$raster
## > "Hypoxia"


## ---- eval = FALSE------------------------------------------------------------
## leafletOutput(outputId = "map", height = 800)


## ---- eval = FALSE------------------------------------------------------------
## output$map <- renderLeaflet({
##  leaflet() %>%
##  addProviderTiles("CartoDB.Positron") %>%
##  addRasterImage(rstack[[input$raster]])
## })


## ---- eval = FALSE------------------------------------------------------------
## library(rmarkdown)
## library(flexdashboard)
## draft("shiny_dashboard.Rmd",
##       template = "flex_dashboard",
##       package = "flexdashboard")


## ---- eval = FALSE------------------------------------------------------------
## rmarkdown::run('shiny_dashboard.Rmd')


## ---- eval = FALSE------------------------------------------------------------
## rasterFunction <- reactive({
##   sum(rstack[[input$raster]], na.rm = T) %>%
##   calc(function(x) ifelse(x == 0, NA, x))
## })
## 


## ---- eval = FALSE------------------------------------------------------------
## observe({
##   # Clear maps if no raster is selected
##   leafletProxy(mapId = "map2") %>%
##   clearImages() %>% # Clears previous raster
##   addRasterImage(x = rasterFunction())
## })



## ---- child = "_11e_shiny_spatial.Rmd"----------------------------------------

## ----include = FALSE----------------------------------------------------------
source("_setup.R")


