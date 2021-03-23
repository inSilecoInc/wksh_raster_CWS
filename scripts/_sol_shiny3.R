# -------------------------- GLOBAL
library(sf)
library(dplyr)
library(leaflet)
library(leafem)
library(mapedit)
library(stars)

cdqs <- st_as_sf(x = read.csv('data/cdqs.csv'),
                 coords = c("CentroideX", "CentroideY"),
                 crs = 4326)

ras <- read_stars("data/bathy.tif")
ras_d <- st_warp(ras, cellsize = .1, crs = 4326) 
  
  
# ------------------------- UI
ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      h1("Sidebar"),
      selectInput(inputId = "species", label = "Select species", choices = "placeholder"),
      actionButton('save', 'Draw histogram')
    ),
    mainPanel(
      tabsetPanel(
        id = "main_panel",
        tabPanel(
          "map",
          mapedit::editModUI("map", width = "100%", height = "90vh")
        ),
        tabPanel(
          "hist",
          plotOutput("hist")
        )
      )
    )
  )
)
  
  
# ------------------------- Server 
server <- function(input, output, session) {
  updateSelectInput(session, "species", choices = unique(cdqs$Espece.Species.EN))
  observeEvent(input$species, {
    tmp <- cdqs %>% 
      filter(Espece.Species.EN == !!input$species) 
    map <- leaflet(tmp) %>% 
      setView(lng = -63, lat = 48, zoom = 5) %>% 
      addProviderTiles("CartoDB.Positron") %>%
      addStarsImage(ras_d) %>%
      addCircleMarkers(
        stroke = FALSE,
        popup = ~as.character(Nom.de.colonie.Colony.name))
    edits <<- callModule(editMod, leafmap = map, id = "map")
  })
  observeEvent(input$save, {
    geom <- edits()$finished
    # not needed unless you want to keep the geojson
    if (!is.null(geom)) {
       sf::write_sf(geom, 'data/new_geom.geojson', delete_layer = TRUE, delete_dsn = TRUE) 
     }
    # with ras computation will take longer
    output$hist <- renderPlot({
      # mask if you want but st_intersects works as wll
      hist(ras_d[[1]][st_intersects(ras_d, geom, sparse = FALSE)], main = "cell extacted", xlab = "elevation (m)")
    })
   })
}