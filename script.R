## ----setup, include = FALSE---------------------------------------------------
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


## ---- child = "_01_intro.Rmd"-------------------------------------------------

## ----include = FALSE----------------------------------------------------------
source("_setup.R")



## ---- child = "_02_sfmin.Rmd"-------------------------------------------------

## ----include = FALSE----------------------------------------------------------
source("_setup.R")
library(sf)
library(tidyverse)


## ----csv----------------------------------------------------------------------
# Output folder
if (!file.exists('output')) dir.create('output')

# remotes::install_github("vlucet/rgovcan")
library(rgovcan)
id <- "9cd6f8a1-e660-4e78-89a8-6e3f781da556"
govcan_dl_resources(id, path = 'output')

# Import csv
cdqs <- read.csv('output/CDQS_BIOMQ_2017.csv')


## ----csvsf--------------------------------------------------------------------
cdqs <- st_as_sf(x = cdqs,
                 coords = c("CentroideX", "CentroideY"),
                 crs = 4326)


## ----showcsvsf, echo = FALSE--------------------------------------------------
cdqs[,1]


## ----csvplot, fig.width=4, fig.height=4---------------------------------------
plot(cdqs[,1])


## ----shp, messages = FALSE, warnings = FALSE----------------------------------
# Download from open canada
id <- "0c469c0e-8afd-4c62-9420-c50af2d34f97"
govcan_dl_resources(id, path = 'output')
unzip("output/Sterna_dougallii_Roseate_Tern_CH_2015.zip", exdir = 'output')


## ----showshp, messages = FALSE, warnings = FALSE------------------------------
# Import shapefile
roseate <- st_read("output/Sterna_dougallii_Roseate_Tern_CH_2015.shp", quiet = TRUE)
roseate


## ----shpplot------------------------------------------------------------------
plot(st_geometry(roseate[4,]))


## ----geojson, hold = TRUE-----------------------------------------------------
stl <- st_read('data/st_laurence.geojson', quiet = TRUE)
stl


## ----geojsonplot, fig.width=5, fig.height=4, eval=FALSE-----------------------
## plot(stl)


## ----gbd, messages = FALSE, warnings = FALSE----------------------------------
id <- "f612e2b4-5c67-46dc-9a84-1154c649ab4e"
govcan_dl_resources(id, path = 'output')
unzip("output/AtlasGrid-GrilleAtlas.gdb.zip", exdir = 'output')


## ----gbdlayer, messages = FALSE, warnings = FALSE-----------------------------
# Visualize layers available in the geodatabase
st_layers("output/AtlasGrid-GrilleAtlas.gdb")


## ----gbdload, messages = FALSE, warnings = FALSE------------------------------
# Load layer
atlas <- st_read('output/AtlasGrid-GrilleAtlas.gdb',
                  layer = 'AtlasGrid_GrilleAtlas')


## ----gbdplot, messages = FALSE, warnings = FALSE, fig.width=4,fig.height=4----
plot(st_geometry(atlas))


## ----export-------------------------------------------------------------------
st_write(roseate, "output/roseate.shp", delete_dsn = TRUE)
st_write(stl, "output/stl.geojson", delete_dsn = TRUE)


## ----echo = FALSE-------------------------------------------------------------
oldopt <- options()$width
options(width=150)


## ----drivers------------------------------------------------------------------

st_drivers()


## ----echo = FALSE-------------------------------------------------------------
options(width=oldopt)


## ----sp, eval = FALSE---------------------------------------------------------
## # Do not run
## newsf <- st_as_sf(oldsp)


## ----spsf, eval = FALSE-------------------------------------------------------
## # Do not run
## newsp <- as(oldsf, "Spatial")


## ----crs----------------------------------------------------------------------
cdqs <- st_transform(cdqs, st_crs(atlas))


## ----attr_hist, fig.width=6,fig.height=4--------------------------------------
hist(cdqs$Nombre.de.nicheurs.Number.of.Breeders)


## ----attr_add, fig.width=6,fig.height=4---------------------------------------
cdqs$var2 <- log(cdqs$Nombre.de.nicheurs.Number.of.Breeders+1)
hist(cdqs$var2)


## ----attr_add2, fig.width=6,fig.height=4--------------------------------------
cdqs <- cdqs %>% mutate(var3 = log(Nombre.de.nicheurs.Number.of.Breeders+1))
hist(cdqs$var3)


## ----attr_rem, fig.width=6,fig.height=4---------------------------------------
cdqs$var2 <- NULL
cdqs <- cdqs %>% select(-var3)
colnames(cdqs)


## ----attr_sub, fig.width=4,fig.height=4---------------------------------------
newcdqs <- cdqs[cdqs$Annee.Year > 2000, ]
plot(st_geometry(cdqs), pch = 1, col = '#2ae9ee', cex = 1)
plot(st_geometry(newcdqs), pch = 20, col = '#d76060', cex = .8, add = TRUE)


## ----attr_join, fig.width=6,fig.height=3.5------------------------------------
atlas


## ----attr_join2, fig.width=6,fig.height=3.5-----------------------------------
join_data <- data.frame(id = paste0('g', sample(nrow(atlas), nrow(atlas))),
                        value = runif(nrow(atlas)))
join_data[1:10, ]


## ----attr_join3, fig.width=6,fig.height=3.5-----------------------------------
newatlas <- left_join(atlas, join_data, by='id')


## ---- echo = FALSE------------------------------------------------------------
newatlas[1:10,c('id','value','Shape'), drop = TRUE]


## ---- echo = FALSE, fig.width=4, fig.height=3.5-------------------------------
plot(newatlas[,'value'])


## ----area---------------------------------------------------------------------
atlas$area <- st_area(atlas)
atlas


## ----distance-----------------------------------------------------------------
st_distance(atlas[1:5, ], cdqs[1:5,])


## ----intersect----------------------------------------------------------------
st_intersects(atlas[151:160, ], cdqs)


## ----buffer1, fig.width = 4, fig.height=3-------------------------------------
newatlas <- st_buffer(atlas[1:10, ], 30000)
plot(st_geometry(atlas[1:10, ]))
plot(st_geometry(newatlas),
     border = 'red', add = TRUE)


## ----buffer2, fig.width = 4, fig.height=3-------------------------------------
newatlas <- st_buffer(atlas[1:10, ], -30000)
plot(st_geometry(atlas[1:10, ]))
plot(st_geometry(newatlas),
     border = 'red', add = TRUE)


## ----union, fig.width = 6, fig.height=4---------------------------------------
newatlas <- st_union(atlas)
plot(newatlas)


## ----convex, fig.width = 4, fig.height=3--------------------------------------
cdqshull <- st_convex_hull(st_union(cdqs))
plot(st_geometry(cdqshull))
plot(st_geometry(cdqs), add = TRUE)


## ----intersection, fig.width = 4, fig.height=4--------------------------------
cdqs_atlas <- st_intersection(newatlas, cdqshull)
plot(st_geometry(newatlas))
plot(st_geometry(cdqshull), add = TRUE)
plot(st_geometry(cdqs_atlas), col = 'red', add = TRUE)


## ----difference, fig.width = 4, fig.height=4----------------------------------
cdqs_atlas <- st_difference(newatlas, cdqshull)
plot(st_geometry(newatlas))
plot(st_geometry(cdqshull), add = TRUE)
plot(st_geometry(cdqs_atlas), col = 'red', add = TRUE)


## ----difference2, fig.width = 4, fig.height=4---------------------------------
cdqs_atlas <- st_difference(cdqshull, newatlas)
plot(st_geometry(newatlas))
plot(st_geometry(cdqshull), add = TRUE)
plot(st_geometry(cdqs_atlas), col = 'red', add = TRUE)


## ----spatjoin1, fig.width = 4, fig.height=3-----------------------------------
atlasJ <- st_join(atlas, cdqs)
plot(atlasJ[,'Nombre.de.nicheurs.Number.of.Breeders'])


## ----spatjoin2, fig.width = 4, fig.height=3-----------------------------------
cdqsJ <- st_join(cdqs, atlas)
plot(cdqsJ[,'id'])



## ---- child = "_02e_sfmin.Rmd"------------------------------------------------

## ----include = FALSE----------------------------------------------------------
source("_setup.R")


## ----exsf, include = TRUE, eval = FALSE, purl = TRUE--------------------------
## # Download and import atlas grid
## govcan_dl_resources("f612e2b4-5c67-46dc-9a84-1154c649ab4e", path = 'output')
## unzip("output/AtlasGrid-GrilleAtlas.gdb.zip", exdir = 'output')
## atlas <- st_read('output/AtlasGrid-GrilleAtlas.gdb', layer = 'AtlasGrid_GrilleAtlas')
## 
## # Import atlas data as csv
## dat <- read.csv('./data/DensityData-DonneesDeDensite.csv')
## 
## # Join data with grid
## atpu <- dat[dat$Group == 'ATPU', ] %>% group_by(Stratum) %>% summarize(ATPU = mean(Density))
## razo <- dat[dat$Group == 'RAZO', ] %>% group_by(Stratum) %>% summarize(RAZO = mean(Density))
## atlas <- left_join(atlas, atpu, by = c("id" = "Stratum")) %>%
##          left_join(razo, by = c("id" = "Stratum"))
## 
## 
## # Convex hulls
## atpu_ch <- st_convex_hull(st_union(atlas[atlas$ATPU > 6, ]))
## razo_ch <- st_convex_hull(st_union(atlas[atlas$RAZO > 1, ]))
## 
## # Intersection
## int <- st_intersection(atpu_ch,razo_ch)
## 
## # Area
## st_area(int)
## 
## # Visualize
## plot(st_geometry(atlas))
## plot(atpu_ch, border = '#b1646d', lwd = 2, add = TRUE)
## plot(razo_ch, border = '#b1646d', lwd = 2, add = TRUE)
## plot(int, col = '#074a6c', add = TRUE)
## 



## ---- child = "_03_stars_base.Rmd"--------------------------------------------

## ----include = FALSE----------------------------------------------------------
source("_setup.R")


## ----read_stars---------------------------------------------------------------
library(stars)
ras <- read_stars("data/bathy.tif")   # argument `driver` to specify the driver
ras


## ----stars_val----------------------------------------------------------------
class(ras)
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


## ----convert1-----------------------------------------------------------------
library(raster)
rar <- raster("data/bathy.tif")
ras_c <- st_as_stars(rar) 
class(ras_c)


## ----convert2-----------------------------------------------------------------
rar_c <- as(ras, "Raster") 
class(rar_c)


## ----bc_ras1------------------------------------------------------------------
# extract value
class(ras[[1]])
ras[[1]][1, 1]
ras[[1]][1:10, 11:20]


## ----bc_ras1_2----------------------------------------------------------------
range(ras[[1]])  # min/max
diff(range(ras[[1]]))
mean(ras[[1]]) 
quantile(ras[[1]])


## ----plot_stars, cache = TRUE-------------------------------------------------
plot(ras)


## ----plot_stars_ar, cache = TRUE, fig.width = 9.5, fig.height = 4-------------
par(mfrow = c(1, 2))
hist(ras[[1]])
plot(density(ras[[1]]))


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


## ----from_mat-----------------------------------------------------------------
M1 <- matrix(runif(36), 6, 6)
M1


## -----------------------------------------------------------------------------
ras_M1 <- st_as_stars(M1) 
ras_M1
st_crs(ras_M1)


## -----------------------------------------------------------------------------
plot(ras_M1)


## -----------------------------------------------------------------------------
st_crs(ras_M1) <- st_crs(4326)
st_crs(ras_M1)


## -----------------------------------------------------------------------------
D1 <- expand.grid(lon = 1:10, lat = 1:10) 
D1$val <- runif(100)
head(D1)


## -----------------------------------------------------------------------------
ras_D1 <- st_as_stars(D1, coords = c("lon", "lat"))
st_crs(ras_D1) <- st_crs(4326)
ras_D1



## ---- child = "_04_stars_manip.Rmd"-------------------------------------------

## ----include = FALSE----------------------------------------------------------
source("_setup.R")


## ----stars_obj----------------------------------------------------------------
library(stars)
ras <- read_stars("data/bathy.tif") 
ras


## -----------------------------------------------------------------------------
ras[[1]]


## ----stars_copy---------------------------------------------------------------
ras2 <- 2*ras 
ras2


## ----stars_filtr--------------------------------------------------------------
ras_f1 <- ras # create a copy
ras_f1[[1]][ras[[1]] > units::as_units(0, "m")] <- NA # filter
plot(ras_f1)


## -----------------------------------------------------------------------------
ras_f2 <- units::drop_units(ras) # create a copy without unit
ras_f2[[1]][ras_f2[[1]] < 0] <- NA # filter
plot(ras_f2)


## ----projrar2, cache = TRUE---------------------------------------------------
ras_t <- st_transform(ras, crs = 3857)
st_crs(ras_t)


## ----echo = FALSE-------------------------------------------------------------
plot(ras)


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


## ----get_poly, cache = TRUE---------------------------------------------------
ras_p <- units::drop_units(ras)
ras_p[[1]][ras_p[[1]] > 0] <- NA # filter
ras_p[[1]][ras_p[[1]] < 0] <- 1
pol <- st_as_sf(ras_p, as_points = FALSE, merge = TRUE)


## ---- cache = TRUE------------------------------------------------------------
plot(pol)  


## ---- cache = TRUE------------------------------------------------------------
plot(pol[which.max(st_area(pol)),])  


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
ras_stc1 %>% dplyr::select("bath_v1")


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


## ----plot0, cache = TRUE, echo = -1-------------------------------------------
ras <- stars::read_stars("data/bathy.tif") 
plot(ras)


## ---- pre_plot----------------------------------------------------------------
# breaks
bks <- c(seq(-5000, 0, 1000), 250, 500, 750, 1000)
# cols 
cls <- c("#c7cbce", "#687677", "#222d3d", "#25364a", "#172434", 
  "#ad6a11", "#e6a331", "#e4be29", "#f2ea8b")


## ---- plot1-------------------------------------------------------------------
par(las = 1, bg = "#f79c74", cex.axis = .7, mar = c(2, 2, 2, 4))
plot(ras,  breaks = bks, col = cls, main = "St-Lawrence map", axes = TRUE)


## ---- plot1b------------------------------------------------------------------
par(las = 1, bg = "#f79c74", cex.axis = .7, mar = c(2, 2, 2, 4), oma = c(0, 2, 0 ,2))
plot(ras,  breaks = bks, col = cls, main = "St-Lawrence map", axes = TRUE)



## ---- child = "_06_tmap.Rmd"--------------------------------------------------

## ----include = FALSE----------------------------------------------------------
source("_setup.R")


## ----tmap0, echo = -c(1:2)----------------------------------------------------
stl <- sf::st_read('data/st_laurence.geojson', quiet = TRUE)
ras <- stars::read_stars("data/bathy.tif") 
library(tmap)
map0 <- tm_shape(stl) + tm_borders(col = "red")
map0


## ----tmap1--------------------------------------------------------------------
map1 <- map0 + tm_compass(type = "8star", position = c("left", "top")) 
map1 


## ----tmap2, cache = TRUE------------------------------------------------------
names(ras)
map2 <- tm_shape(ras) + tm_raster("bathy.tif")


## ----tmap3, cache = TRUE------------------------------------------------------
map3 <- map2 + map1  # the order matters
map3


## ----tmap4, cache = TRUE------------------------------------------------------
map4 <- map2 + map1 + tm_style("bw")
map4


## ----tmap5, cache = TRUE------------------------------------------------------
map5 <- tm_shape(ras) + tm_raster("bathy.tif", breaks = c(seq(-5000,
    0, 1000), 250, 500, 750, 1000), palette = "viridis") 
map5 



## ---- child = "_06e_tmap.Rmd"-------------------------------------------------

## ----include = FALSE----------------------------------------------------------
source("_setup.R")


## ----tmap_exo, echo = FALSE, cache = TRUE, fig.width = 6, fig.height = 5, purl = TRUE----
library(tmap)
stl <- sf::st_read('data/st_laurence.geojson', quiet = TRUE) 
ras <- stars::read_stars("data/bathy.tif")                       
# manipulation 
ras_v <- ras[stl]
names(ras_v) <- "Elevation"
# colors 
pal <- colorRampPalette(c("#c7cbce", "#687677", "#222d3d", "#25364a", "#172434", "#ad6a11", "#e6a331", "#e4be29", "#f2ea8b"))
# raster 
elv <- tm_shape(ras_v) + 
  tm_raster("Elevation", breaks = seq(-800, 200, 100), palette = pal(11), midpoint = NA) + 
  tm_layout(main.title = "St-Lawrence river & Gulf", main.title.color = "#ad6a11") + 
  tm_xlab("longitude", size = 0.5) + tm_ylab("latitude", size = 0.5) +
  tm_graticules(lwd = .5, col = "#aaaaaa")
  
shp <- tm_shape(stl) + tm_borders(col = "black", lwd = 2)
oth <- tm_compass(type = "8star", position = c("left", "bottom")) +     
  tm_scale_bar(breaks = c(0, 100, 200), text.size = .9) + 
  tm_logo(c("https://www.r-project.org/logo/Rlogo.png"), position = c("right", "top"), height = 3) 
elv + shp + oth


## ----sol4a, include = TRUE, eval = FALSE, purl = TRUE-------------------------
## # manipulation
## ras_v <- ras[stl]
## names(ras_v) <- "Elevation"
## # colors
## pal <- colorRampPalette(c("#c7cbce", "#687677", "#222d3d", "#25364a",
##   "#172434", "#ad6a11", "#e6a331", "#e4be29", "#f2ea8b"))
## # raster
## elv <- tm_shape(ras_v) +
##   tm_raster("Elevation", breaks = seq(-800, 200, 100), palette = pal(11), midpoint = NA) +
##   tm_layout(main.title = "St-Lawrence (River & Gulf)", main.title.color = "#ad6a11") +
##   tm_xlab("longitude", size = 0.5) + tm_ylab("latitude", size = 0.5) +
##   tm_graticules(lwd = .5, col = "#aaaaaa")


## ----sol4b, include = TRUE, eval = FALSE, purl = TRUE-------------------------
## # borders
## shp <- tm_shape(stl) + tm_borders(col = "black", lwd = 2)
## 
## # other elements
## oth <- tm_compass(type = "8star", position = c("left", "bottom")) +
##     tm_scale_bar(breaks = c(0, 100, 200), text.size = .9) +
##     tm_logo("https://www.r-project.org/logo/Rlogo.png", position = c("right", "top"), height = 3)
## 
## mymap <- elv + shp + oth
## mymap
## tmap_save(mymap, filename = "mymap.png", dpi = 300, units = "mm", width = 160, height = 160)



## ---- child = "_07_interactive_maps.Rmd"--------------------------------------

## ----include = FALSE----------------------------------------------------------
source("_setup.R")



## ---- child = "_08_leaflet.Rmd"-----------------------------------------------

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
fetchDrivers(drivers = c('Hypoxia','FisheriesDD','Acidification'), output = 'data')

# Raster objects from eDrivers class objects
library(stars)
hyp <- read_stars('data/Hypoxia.tif')
fish <- read_stars('data/FisheriesDD.tif')
acid <- read_stars('data/Acidification.tif')


## ----load_leafem--------------------------------------------------------------
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


## ----export_leaflet, eval = FALSE---------------------------------------------
## dir.create('output')
## htmlwidgets::saveWidget(lf, file="output/lf.html")


## ----mapview, eval = FALSE, out.width = "100%"--------------------------------
## library(mapview)
## mv <- mapview(hyp) +
##   mapview(fish) +
##   mapview(acid)
## mv@map


## ----mapview2, echo = FALSE, eval = TRUE, out.width = "100%"------------------
library(mapview)
mv <- mapview(hyp) + mapview(fish) + mapview(acid)
mv@map


## ----mapview_export, eval = TRUE, fig.width = 12, fig.height = 6--------------
dir.create('output')
mapshot(mv, url = 'output/map.html')



## ---- child = "_08e_leaflet.Rmd"----------------------------------------------

## ----include = FALSE----------------------------------------------------------
source("_setup.R")



## ---- child = "_09_shiny_intro.Rmd"-------------------------------------------

## ----include = FALSE----------------------------------------------------------
source("_setup.R")
lg_shiny <- function(width) {
  glue("<img src='https://raw.githubusercontent.com/rstudio/shiny/master/man/figures/logo.png' 
  alt='Siny logo' width='{width}%' style='vertical-align:middle'>")
} 


## ---- non_react---------------------------------------------------------------
a <- 2
b <- 2*a
cat("a =", a, " | ", "b =", b)


## -----------------------------------------------------------------------------
a <- 3
cat("a =", a, " | ", "b =", b)


## ---- react1------------------------------------------------------------------
library(shiny)
reactiveConsole(TRUE)
a <- reactiveVal()
a(2)
b <- reactive({2*a()})
cat("a =", a(), " | ", "b =", b())


## -----------------------------------------------------------------------------
a(3)
cat("a =", a(), " | ", "b =", b())


## ---- include = FALSE---------------------------------------------------------
reactiveConsole(FALSE)



## ---- child = "_10_shiny_mini.Rmd"--------------------------------------------

## ----include = FALSE----------------------------------------------------------
source("_setup.R")


## ----eval = FALSE-------------------------------------------------------------
## # Global variables can go here
## n <- 200
## 
## # Define the UI
## ui <- bootstrapPage(
##   numericInput('n', 'Number of obs', n),
##   plotOutput('plot')
## )
## 
## # Define the server code
## server <- function(input, output) {
##   output$plot <- renderPlot({
##     hist(runif(input$n))
##   })
## }
## 
## # Return a Shiny app object
## shinyApp(ui = ui, server = server)
## 



## ---- child = "_10e_shiny_mini.Rmd"-------------------------------------------

## ----include = FALSE----------------------------------------------------------
source("_setup.R")



## ---- child = "_11_shiny_spatial.Rmd"-----------------------------------------

## ----include = FALSE----------------------------------------------------------
source("_setup.R")



## ---- child = "_11e_shiny_spatial.Rmd"----------------------------------------

## ----include = FALSE----------------------------------------------------------
source("_setup.R")


