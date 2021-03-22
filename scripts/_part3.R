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


