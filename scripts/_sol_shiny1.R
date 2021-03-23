# -------------------------- GLOBAL
library(dplyr)
library(sf)
library(stars)
library(tmap)

# load data
cdqs <- st_as_sf(x = read.csv('data/cdqs.csv'),
                 coords = c("CentroideX", "CentroideY"),
                 crs = 4326)
stl <- st_read("data/st_laurence.geojson")
ras <- read_stars("data/bathy.tif")
ras[[1]][ras[[1]] < units::as_units(-1000, "m")] <- NA

## Won't change so added in the global.R
elv <- tm_shape(ras) + 
  tm_raster("bathy.tif", breaks = c(seq(-1000, 0, 250), 250, 500, 750, 1000)) 
axes <- tm_xlab("longitude", size = 1.8) + 
  tm_ylab("latitude", size = 1.8) + 
  tm_graticules(lwd = .5, col = "#aaaaaa", labels.size = 1.2)
shp <- tm_shape(stl) + 
  tm_borders(col = "#9c87e4", lwd = 1)
oth <- tm_compass(type = "8star", position = c("left", "bottom")) +
  tm_scale_bar(breaks = c(0, 100, 200), text.size = .9)
  
  
# ------------------------- UI
ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      h1("Sidebar"),
      selectInput(inputId = "species", label = "Select species", choices = "placeholder"),
      selectInput(inputId = "colony", label = "Select Colony", choices = "placeholder"),
      actionButton("save", "save map")
      # input$species
    ),
    mainPanel(
      h1("Main Panel"),
      plotOutput("map", width = "100%", height = "90vh")
    )
  )
)
  
  
# ------------------------- Server 
server <- function(input, output, session) {
  updateSelectInput(session, "species", choices = unique(cdqs$Espece.Species.EN))
  # trickier 
  observeEvent(input$species, {
    tmp <- cdqs %>% 
      filter(Espece.Species.EN == !!input$species) 
    updateSelectInput(session, "colony", choices = unique(tmp$Nom.de.colonie.Colony.name))
  })
  
  mymap <- reactive({
    pts <- cdqs %>% 
    filter(Espece.Species.EN == !!input$species, Nom.de.colonie.Colony.name == !!input$colony) 
    bub <- tm_shape(pts) + 
      tm_bubbles("Nombre.de.nicheurs.Number.of.Breeders") + 
      tm_layout(main.title = input$species, title = input$colony, 
          main.title.color = "#10afab", title.color = "#2d5352", legend.frame = TRUE) 
    elv + oth + shp + axes + bub
  })

  output$map <- renderPlot(mymap())
  
  observeEvent(input$save, {
    tmap_save(mymap(), filename = paste0("map_", input$species, "_", input$colony, ".png"), 
      width = 250, height = 250, units = "mm", dpi = 300)
  })
  
}