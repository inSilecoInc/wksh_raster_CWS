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


## ---- child = "_05_visualise.Rmd"---------------------------

## ----include = FALSE----------------------------------------
source("_setup.R")


## ----plot0, cache = TRUE, echo = -1-------------------------
ras <- stars::read_stars("data/bathy.tif") 
plot(ras)


## ---- pre_plot----------------------------------------------
# breaks
bks <- c(seq(-5000, 0, 1000), 250, 500, 750, 1000)
# cols 
cls <- c("#c7cbce", "#687677", "#222d3d", "#25364a", "#172434", 
  "#ad6a11", "#e6a331", "#e4be29", "#f2ea8b")


## ---- plot1-------------------------------------------------
par(las = 1, bg = "#f79c74", cex.axis = .7, mar = c(2, 2, 2, 4))
plot(ras,  breaks = bks, col = cls, main = "St-Lawrence map", axes = TRUE)


## ---- plot1b------------------------------------------------
par(las = 1, bg = "#f79c74", cex.axis = .7, mar = c(2, 2, 2, 4), oma = c(0, 2, 0 ,2))
plot(ras,  breaks = bks, col = cls, main = "St-Lawrence map", axes = TRUE)



## ---- child = "_06_tmap.Rmd"--------------------------------

## ----include = FALSE----------------------------------------
source("_setup.R")


## ----tmap0, echo = -c(1:2)----------------------------------
stl <- sf::st_read('data/st_laurence.geojson', quiet = TRUE)
ras <- stars::read_stars("data/bathy.tif") 
library(tmap)
map0 <- tm_shape(stl) + tm_borders(col = "red")
map0


## ----tmap1--------------------------------------------------
map1 <- map0 + tm_compass(type = "8star", position = c("left", "top")) 
map1 


## ----tmap2, cache = TRUE------------------------------------
names(ras)
map2 <- tm_shape(ras) + tm_raster("bathy.tif")


## ----tmap3, cache = TRUE------------------------------------
map3 <- map2 + map1  # the order matters
map3


## ----tmap4, cache = TRUE------------------------------------
map4 <- map2 + map1 + tm_style("bw")
map4


## ----tmap5, cache = TRUE------------------------------------
map5 <- tm_shape(ras) + tm_raster("bathy.tif", breaks = c(seq(-5000,
    0, 1000), 250, 500, 750, 1000), palette = "viridis") 
map5 



## ---- child = "_06e_tmap.Rmd"-------------------------------

## ----include = FALSE----------------------------------------
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


