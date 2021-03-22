## ---- non_react------------------------------------------
a <- 2
b <- 2*a
cat("a =", a, " | ", "b =", b)


## --------------------------------------------------------
a <- 3
cat("a =", a, " | ", "b =", b)


## ---- react1---------------------------------------------
library(shiny)
reactiveConsole(TRUE)
a <- reactiveVal()
a(2)
b <- reactive({2*a()})
cat("a =", a(), " | ", "b =", b())


## --------------------------------------------------------
a(3)
cat("a =", a(), " | ", "b =", b())


## ---- include = FALSE------------------------------------
reactiveConsole(FALSE)



## ---- child = "_10_shiny_mini.Rmd"-----------------------

## ----include = FALSE-------------------------------------
source("_setup.R")


## ---- eval = FALSE---------------------------------------
# Global variables can go here
n <- 200

# Define the UI
ui <- bootstrapPage(
  numericInput('n', 'Number of obs', n),
  plotOutput('plot')
)

# Define the server code
server <- function(input, output) {
  output$plot <- renderPlot({
    hist(runif(input$n))
  })
}

# Return a Shiny app object
shinyApp(ui = ui, server = server)



## ---- eval = FALSE---------------------------------------
ui <- bootstrapPage(
  numericInput('n', 'Number of obs', n),
  plotOutput('plot')
)


## ---- eval = FALSE---------------------------------------
server <- function(input, output) {
  output$plot <- renderPlot({
    hist(runif(input$n))
  })
}


## ---- eval = FALSE---------------------------------------
shinyApp(ui = ui, server = server)


## ---- eval = FALSE---------------------------------------
ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      h1("Sidebar"),
      p("This is our sidebar where inputs will be added.")
    ),
    mainPanel(
      h1("Main Panel"),
      p("This is where outputs will be added.")
    )
  )
)


## ---- eval = FALSE---------------------------------------
server <- function(input, output) NULL
shinyApp(ui = ui, server = server)


## ---- eval = FALSE---------------------------------------
ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      h1("Sidebar"),
      selectInput(inputId = "species", label = "Select species", choices = "placeholder")
      # input$species
    ),
    mainPanel(
      h1("Main Panel"),
      plotOutput("map", width = "100%")
    )
  )
)


## ---- eval = FALSE---------------------------------------
library(sf)
library(dplyr)
server <- function(input, output) {
  cdqs <- st_as_sf(x = read.csv('output/CDQS_BIOMQ_2017.csv'),
                   coords = c("CentroideX", "CentroideY"),
                   crs = 4326)
  print(names(cdqs))
}


## ---- eval = FALSE---------------------------------------
library(sf)
library(dplyr)
cdqs <- st_as_sf(x = read.csv('output/CDQS_BIOMQ_2017.csv'),
                 coords = c("CentroideX", "CentroideY"),
                 crs = 4326)
sl <- st_read("data/st_laurence.geojson")

server <- function(input, output, session) {
  updateSelectInput(session, "species", choices = unique(cdqs$Espece.Species.EN))
}


## ---- eval = FALSE---------------------------------------
library(sf)
library(dplyr)
cdqs <- st_as_sf(x = read.csv('output/CDQS_BIOMQ_2017.csv'),
                 coords = c("CentroideX", "CentroideY"),
                 crs = 4326)
server <- function(input, output, session) {
  updateSelectInput(session, "species", choices = unique(cdqs$Espece.Species.EN))
  output$map <- renderPlot({
    plot(st_geometry(sl), border = "grey50")
    cdqs %>%
    filter(Espece.Species.EN == !!input$species) %>%
    st_geometry() %>%
    plot(add = TRUE, pch = 19)
  })
}


## ---- eval = FALSE---------------------------------------
ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      h1("Sidebar"),
      selectInput(inputId = "species", label = "Select species", choices = "placeholder")
    ),
    mainPanel(
      h1("Main Panel"),
      plotOutput("map", width = "100%")
    )
  )
)


## ---- eval = FALSE---------------------------------------
library(leaflet)
ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      h1("Sidebar"),
      selectInput(inputId = "species", label = "Select species", choices = "placeholder")
    ),
    mainPanel(
      h1("Main Panel"),
      leafletOutput("map", width = "100%", height = "90vh")
    )
  )
)


## ---- eval = FALSE---------------------------------------
library(sf)
library(dplyr)
server <- function(input, output, session) {
  cdqs <- st_as_sf(x = read.csv('output/CDQS_BIOMQ_2017.csv'),
                   coords = c("CentroideX", "CentroideY"),
                   crs = 4326)
  updateSelectInput(session, "species", choices = unique(cdqs$Espece.Species.EN))
  # output$map <- renderPlot(...)
}


## ---- eval = FALSE---------------------------------------
library(sf)
library(dplyr)
cdqs <- st_as_sf(x = read.csv('output/CDQS_BIOMQ_2017.csv'),
                 coords = c("CentroideX", "CentroideY"),
                 crs = 4326)
server <- function(input, output, session) {
  updateSelectInput(session, "species", choices = unique(cdqs$Espece.Species.EN))
  output$map <- renderLeaflet(
    {
      tmp <- cdqs %>%
        filter(Espece.Species.EN == !!input$species)
      leaflet(tmp) %>%
      setView(lng = -63, lat = 48, zoom = 5) %>%
      addProviderTiles("CartoDB.Positron") %>%
      addCircleMarkers(
        stroke = FALSE,
        popup = ~as.character(Nom.de.colonie.Colony.name))
    }
  )
}


## ---- eval = FALSE---------------------------------------
shinyApp(ui, server)


## ---- eval = FALSE---------------------------------------
library(mapedit)
ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      h1("Sidebar"),
      selectInput(inputId = "species", label = "Select species", choices = "placeholder")
    ),
    mainPanel(
      h1("Main Panel"),
      mapedit::editModUI("map", width = "100%", height = "90vh")
    )
  )
)


## ---- eval = FALSE---------------------------------------
library(sf)
library(dplyr)
cdqs <- st_as_sf(x = read.csv('output/CDQS_BIOMQ_2017.csv'),
                 coords = c("CentroideX", "CentroideY"),
                 crs = 4326)

server <- function(input, output, session) {
  updateSelectInput(session, "species", choices = unique(cdqs$Espece.Species.EN))
  observeEvent(input$species, {
    tmp <- cdqs %>%
      filter(Espece.Species.EN == !!input$species)
    map <- leaflet(tmp) %>%
      setView(lng = -63, lat = 48, zoom = 5) %>%
      addProviderTiles("CartoDB.Positron") %>%
      addCircleMarkers(
        stroke = FALSE,
        popup = ~as.character(Nom.de.colonie.Colony.name))
    callModule(editMod, leafmap = map, id = "map")
  }, ignoreNULL = FALSE)
}


## ---- eval = FALSE---------------------------------------
ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      h1("Sidebar"),
      selectInput(inputId = "species", label = "Select species", choices = "placeholder"),
      actionButton('save', 'Save from Map', icon = icon("download"))
    ),
    mainPanel(
      h1("Main Panel"),
      mapedit::editModUI("map", width = "100%", height = "90vh")
    )
  )
)


## ---- eval = FALSE---------------------------------------
server <- function(input, output, session) {
  updateSelectInput(session, "species", choices = unique(cdqs$Espece.Species.EN))
  observeEvent(input$species, {
    tmp <- cdqs %>%
      filter(Espece.Species.EN == !!input$species)
    map <- leaflet(tmp) %>%
      setView(lng = -63, lat = 48, zoom = 5) %>%
      addProviderTiles("CartoDB.Positron") %>%
      addCircleMarkers(
        stroke = FALSE,
        popup = ~as.character(Nom.de.colonie.Colony.name))
    edits <<- callModule(editMod, leafmap = map, id = "map")
  })
  observeEvent(input$save, {
    geom <- edits()$finished
    if (!is.null(geom)) {
       assign('new_geom', geom, envir = .GlobalEnv)
       sf::write_sf(geom, 'new_geom.geojson', delete_layer = TRUE, delete_dsn = TRUE)
     } # see https://github.com/r-spatial/mapedit/issues/95
   })
}



## ---- child = "_11e_shiny_spatial.Rmd"-------------------

## ----include = FALSE-------------------------------------
source("_setup.R")


