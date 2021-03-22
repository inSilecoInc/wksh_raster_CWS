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


## ---- child = "_03_stars_base.Rmd"--------------------------

## ----read_stars---------------------------------------------
library(stars)
ras <- read_stars("data/bathy.tif")   # argument `driver` to specify the driver
class(ras)
ras


## ----stars_obj_fun1-----------------------------------------
dim(ras)
st_bbox(ras)


## ----stars_obj_fun2-----------------------------------------
ras_crs <- st_crs(ras)
class(ras_crs)
ras_crs


## ----stars_obj_fun3-----------------------------------------
ras_crs$input
ras_crs$proj4string
ras_crs$epsg


## ----convert1-----------------------------------------------
library(raster)
rar <- raster("data/bathy.tif")
ras_c <- st_as_stars(rar) 
class(ras_c)


## ----convert2-----------------------------------------------
rar_c <- as(ras, "Raster") 
class(rar_c)


## -----------------------------------------------------------
class(ras[[1]])
class(ras[["bathy.tif"]])
class(ras$bathy.tif)


## -----------------------------------------------------------
library(units)
as_units(1, "mm") + as_units(1, "cm")


## -----------------------------------------------------------
units::as_units(0, "celsius") + units::as_units(0, "fahrenheit")


## ----nounit-------------------------------------------------
ras_nounit <- drop_units(ras)
class(ras_nounit[[1]])


## ----bc_ras1------------------------------------------------
# extract value
ras[[1]][1, 1]
ras[[1]][1:10, 11:20]


## ----apply--------------------------------------------------
# mean along latitude
m_lat <- apply(ras[[1]], 2, mean)
m_lat[1:10]
# mean along longitude
m_lon <- apply(ras[[1]], 1, mean)
m_lon[1:10]


## -----------------------------------------------------------
lon <- st_get_dimension_values(ras, 1)
lat <- st_get_dimension_values(ras, 2)


## -----------------------------------------------------------
plot(lat, m_lat)


## ----bc_ras1_2----------------------------------------------
range(ras[[1]])  # min/max
diff(range(ras[[1]]))
mean(ras[[1]]) 
quantile(ras[[1]])


## ----plot_stars, cache = TRUE-------------------------------
plot(ras)


## ----plot_stars_ar, cache = TRUE, fig.width = 9.5, fig.height = 4----
par(mfrow = c(1, 2))
hist(ras[[1]])
plot(density(ras[[1]]))


## ----stars_drivers------------------------------------------
st_dr <- sf::st_drivers(what = "raster")
head(st_dr)


## ----stars_drivers2-----------------------------------------
st_dr[which(st_dr$name == "GTiff"), ]


## ----create_dir---------------------------------------------
dir.create("output", showWarnings = FALSE)


## ----write_stars0-------------------------------------------
sort(st_dr$name[st_dr$write])


## ----write_stars1, cache = TRUE-----------------------------
write_stars(ras, dsn = "output/ras.gpkg", driver = "GPKG")


## ----from_mat-----------------------------------------------
M1 <- matrix(runif(36), 6, 6)
M1


## -----------------------------------------------------------
ras_M1 <- st_as_stars(M1) 
ras_M1
st_crs(ras_M1)


## -----------------------------------------------------------
plot(ras_M1)


## -----------------------------------------------------------
st_crs(ras_M1) <- st_crs(4326)
st_crs(ras_M1)


## -----------------------------------------------------------
D1 <- expand.grid(lon = 1:10, lat = 1:10) 
D1$val <- runif(100)
head(D1)


## -----------------------------------------------------------
ras_D1 <- st_as_stars(D1, coords = c("lon", "lat"))
st_crs(ras_D1) <- st_crs(4326)
ras_D1



## ---- child = "_04_stars_manip.Rmd"-------------------------

## ----include = FALSE----------------------------------------
source("_setup.R")


## ----stars_obj, echo = FALSE--------------------------------
library(stars)
ras <- read_stars("data/bathy.tif") 


## ----stars_copy---------------------------------------------
ras2 <- 2*ras 
ras2


## ----stars_filtr--------------------------------------------
ras_f1 <- ras # create a copy
ras_f1[[1]][ras[[1]] > units::as_units(0, "m")] <- NA # filter
plot(ras_f1)


## -----------------------------------------------------------
ras_f2 <- units::drop_units(ras) # create a copy without unit
ras_f2[[1]][ras_f2[[1]] < 0] <- NA # filter
plot(ras_f2)


## ----projrar2, cache = TRUE---------------------------------
ras_t <- st_transform(ras, crs = 3857)
st_crs(ras_t)


## ----echo = FALSE-------------------------------------------
plot(ras)


## ----crop_ras, cache = TRUE---------------------------------
ext <- st_bbox(c(xmin = -65, xmax = -60, ymin = 45, ymax = 50), crs = 4326)
ras_c <- st_crop(ras, ext)
plot(ras_c)


## ----stl----------------------------------------------------
(stl <- sf::st_read("data/st_laurence.geojson"))


## ----plot_stl, cache = TRUE---------------------------------
plot(sf::st_geometry(stl))


## ----ras_m, cache = TRUE------------------------------------
ras_m <- ras[stl]
plot(ras_m)


## ----ras_m1, cache = TRUE-----------------------------------
stl_i <- sf::st_difference(sf::st_as_sfc(st_bbox(ras)), stl)
plot(stl_i, col = 2)


## ----ras_m2, cache = TRUE-----------------------------------
ras_mi <- ras[stl_i]
plot(ras_mi)


## ----ras_warp, cache = TRUE---------------------------------
ras_template1 <- st_as_stars(st_bbox(ras), nx = 21, ny = 21, 
  values = runif(21 * 21)) # create template
ras_w1 <- st_warp(ras, ras_template1) 
plot(ras_w1)


## ----ras_warp_crs, cache = TRUE-----------------------------
ras_template1 <- st_as_stars(st_bbox(ras), nx = 21, ny = 21, 
  values = runif(21 * 21)) # create template
ras_w1 <- st_warp(ras, st_transform(ras_template1, crs = 3857)) 
plot(ras_w1)


## ----ras_warp2, cache = TRUE--------------------------------
ras_w2 <- st_warp(ras, cellsize = 0.25, crs = st_crs(ras)) 
plot(ras_w2)


## ---- cache = TRUE------------------------------------------
ras_template1[[1]] <- NA
stl_r <- st_rasterize(stl, ras_template1) # use a template
plot(stl_r)


## ----rasters1, cache = TRUE---------------------------------
stl_s <- st_rasterize(stl, dy = .1, dx = .1) # use cell-size
plot(stl_s)


## ---- soldd1------------------------------------------------
# load files
gulf_region <- sf::read_sf("data/st_laurence.geojson")
strs <- stars::read_stars("data/bathy.tif")
# create raster mask
template <- strs
template[[1]][] <- NA
gulf_region$val_ras <- 1

rasterized <- stars::st_rasterize(gulf_region["val_ras"], template = template)


## ---- soldd2------------------------------------------------
plot(rasterized)


## ----get_poly, cache = TRUE---------------------------------
ras_p <- units::drop_units(ras)
ras_p[[1]][ras_p[[1]] > 0] <- NA # filter
ras_p[[1]][ras_p[[1]] < 0] <- 1
pol <- st_as_sf(ras_p, as_points = FALSE, merge = TRUE)


## ---- cache = TRUE------------------------------------------
plot(pol)  


## ---- cache = TRUE------------------------------------------
plot(pol[which.max(st_area(pol)),])  


## ----ras_stc0-----------------------------------------------
ras


## ----ras_stc1-----------------------------------------------
ras_stc1 <- c(ras, ras * 2)
names(ras_stc1) <- c("bath_v1", "bath_v2")
ras_stc1


## ----ras_stc1b----------------------------------------------
plot(ras_stc1)


## ----ras_stc2-----------------------------------------------
ras_stc1[1] # or ras_stc1["bath_v1"]


## ----ras_stc2b----------------------------------------------
ras_stc1[2] # or ras_stc1["bath_v2"]


## ----ras_stc2c----------------------------------------------
library(dplyr)
ras_stc1 %>% dplyr::select("bath_v1")


## ----ras_stc2d----------------------------------------------
ras_stc1 %>% mutate(bath_v3 = bath_v2 * 2)


## ----ras_stc3, cache = TRUE---------------------------------
ras_stc2 <- c(ras, ras*2, along = "z")
ras_stc2


## ----ras_stc4, cache = TRUE---------------------------------
plot(ras_stc2)


## ----ras_stc5, cache = TRUE---------------------------------
ext <- st_bbox(c(xmin = -65, xmax = -60, ymin = 45, ymax = 50), crs = 4326)
plot(st_crop(ras_stc2, ext))


## ----ras_stc6, cache = TRUE---------------------------------
(ras_ap <- st_apply(ras_stc2, c(1, 2), sum))


## ----ras_stc6b, cache = TRUE--------------------------------
plot(ras_ap)



## ---- child = "_04e_stars_manip.Rmd"------------------------

## ----include = FALSE----------------------------------------
source("_setup.R")


