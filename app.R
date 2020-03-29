#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(httr)
library(jsonlite)
library(leaflet)
library(DBI)
library(pool)
library(ggplot2)
library(ggpubr)
library("RPostgres")
library(plotly)
library(data.table)

columns <- c('time','exact')
# Define UI for application that draws a histogram
ui <- fluidPage(
    # Application title
    titlePanel("Crowd sourced temperature data map."),
    tags$head(tags$script(src = "message-handler.js")),
    tags$script(
        '
      $(document).ready(function () {
        navigator.geolocation.getCurrentPosition(onSuccess, onError);
        
        $.getJSON(\'http://ip-api.com/json?callback=?\', function(data) {
          Shiny.setInputValue("ipaddress", data.query);
        });

        function onError (err) {
          Shiny.onInputChange("geolocation", false);
        }

        function onSuccess (position) {
          setTimeout(function () {
            var coords = position.coords;
            console.log(coords.latitude + ", " + coords.longitude);
            Shiny.setInputValue("geolocation", true);
            Shiny.setInputValue("lat", coords.latitude);
            Shiny.setInputValue("long", coords.longitude);
          }, 1100)
        }
      });
              '
    ),
    
    
    
    
    
    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            # Show a plot of the generated distribution
            verbatimTextOutput("lat"),
            verbatimTextOutput("long"),
            verbatimTextOutput("geolocation"),
            verbatimTextOutput("ipaddress"),
            textInput(label="temperature",  inputId = "temperature"),
            actionButton("dosubmit", "Submit temperature"),
            actionButton("domap", "Update map")
            #DT::dataTableOutput("readings")
        ),
        
       
        
        mainPanel(leafletOutput("readingsmap", height = "800"))
        
        
    )
)
updatemap <- function(input, output) {
  urltemperature <- "http://127.0.0.1:3000/temperature"
  
  r <- GET(url=urltemperature, encode = "json", verbose())
  
  rtext <-content(r,as="text")   
  
  output$readingsmap <- renderLeaflet({
    leaflet() %>% addTiles() %>% setView(input$long, input$lat, zoom = 10) %>%
      addMarkers(
        lng = ~ longditude,
        lat = ~ latitude,
        data = fromJSON(rtext),
        clusterOptions = markerClusterOptions(freezeAtZoom = 10)
      ) 
  })
}

# Define server logic required to draw a histogram
server <- function(input, output) {
    output$lat <- renderPrint({
        input$lat
    })
    
    output$long <- renderPrint({
        input$long
    })
    
    output$geolocation <- renderPrint({
        input$geolocation
    })
    
    output$ipaddress <- renderPrint({
      input$ipaddress
    })
    
    updatemap(input,output)
    
    
    observeEvent(input$domap, {
      
      updatemap(input,output)
      
    })
    observeEvent(input$dosubmit, {
      
      urltemperature <- "http://127.0.0.1:3000/temperature"
      
      temperaturebody <- list(exact=input$temperature, latitude=input$lat, longditude=input$long, time=Sys.time(), ipaddress=input$ipaddress)
      
      r <- POST(urltemperature, body = temperaturebody, encode = "json", verbose())
      
      

      
      
    })
}

# Run the application
shinyApp(ui = ui, server = server)
