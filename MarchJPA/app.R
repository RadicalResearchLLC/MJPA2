## This is an alpha version of Warehouse CITY, a community mapping tool for warehouse impacts
## Authored by Mike McCarthy, Radical Research LLC
## Thanks to Sean Raffuse at UC Davis AQRC for help with the nearby intersection code for great circles 
## First created May, 2022
## Last modified October, 2022
#

library(shiny)
library(leaflet)
library(htmltools)
library(sf)
library(tidyverse)
library(markdown)


## Define UI for application that displays warehouses
# Show app name and logos
ui <- fluidPage(title = 'RNOW March JPA Warehouse Map',
  tags$style(type="text/css", "div.info.legend.leaflet-control br {clear: both;}"),
    titlePanel(
      fluidRow(column(2, shiny::img(height = 80, src = 'RNOW.jpg')),
               column(4,
               div(style = 'height:40px; font-size: 30px;',
               'March JPA Warehouse Map')),
               column(2, shiny::img(height = 80, src = 'RNOW.jpg')))
      ),
  ##Create a tabset display to have a readme file and main warehouse page
  tabsetPanel(
    tabPanel('Dashboard',
    # Display slider bar selections, checkbox, and summary text
    fluidRow(
        column(width = 12, align = 'center', leafletOutput("map", height = 800))
        )
      ),
    tabPanel('Readme',
      div(style = 'width: 90%; margin: auto;',
      fluidRow(includeMarkdown("README.md")),
      )
    )
  )
)
  
  

server <- function(input, output) {


#Create leaflet map with legend and layers control
  
output$map <- renderLeaflet({
  leaflet() %>% 
    addTiles() %>% 
    addProviderTiles(provider = providers$Esri.WorldImagery, 
                     group = 'Imagery') %>% 
    addLayersControl(baseGroups = c('Basemap', 'Imagery'),
                     overlayGroups = c('City boundaries', 'MJPA boundary',
                                       'Existing warehouses',
                                       'Under construction warehouses',
                                       'West Campus Upper Plateau',
                                       '800 foot buffer - City policy',
                                       '1000 foot buffer - WRCOG GNG'),
                     options = layersControlOptions(collapsed = FALSE)) %>% 
    hideGroup(c('800 foot buffer - City policy',
                '1000 foot buffer - WRCOG GNG')) %>% 
    setView(lng = -117.27, lat = 33.88, zoom = 13) %>% 
    addPolygons(data = cities,
                stroke = TRUE,
                color = 'blue',
                fillOpacity = 0,
                weight = 3,
                group = 'City boundaries',
                label = ~htmlEscape(city.name)
    )  %>% 
    addPolygons(data = MJPA,
                stroke = TRUE,
                color = 'black',
                weight = 5,
                fillOpacity = 0, 
                group = 'MJPA boundary')
    })

#Existing warehouses
observe({
  leafletProxy("map", data = warehouses) %>%
    clearGroup(group = 'Existing warehouses') %>%
    addPolygons(stroke = FALSE,
                color = 'red',
                fillOpacity = 0.5,
                group = 'Existing warehouses',
                label = ~htmlEscape(paste('Parcel', apn, ';', 
                                          round(shape_area,0), 'sq.ft.', class, year_chr)))
})
#under construction warehouses
observe({
  leafletProxy("map", data = Syc_WH) %>%
    clearGroup(group = 'Under construction warehouses') %>%
    addPolygons(stroke = TRUE,
                weight = 2,
                color = 'red',
                fillOpacity = 0.5,
                label = ~htmlEscape(name),
                group = 'Under construction warehouses')
})

#West Campus Proposal
observe({
  leafletProxy("map", data = WCUP ) %>%
    clearGroup(group = 'West Campus Upper Plateau') %>%
    addPolygons(data = WCUP, 
                stroke = TRUE,
                weight = 2,
                color = 'red', 
                fillOpacity = 0.5,
                label = ~htmlEscape(name),
                group = 'West Campus Upper Plateau')
})

#Buffer 800 feet

observe({
  leafletProxy("map", data = buff_proj_800) %>%
    clearGroup(group = '800 foot buffer - City policy') %>%
    addPolygons(data = buff_proj_800,
                stroke = FALSE,
                color = 'brown',
                fillOpacity = 0.3,
                group = '800 foot buffer - City policy')
})

#Buffer 1000 feet
observe({
  leafletProxy("map", data = buff_proj_1000) %>%
    clearGroup(group = '1000 foot buffer - WRCOG GNG') %>%
    addPolygons(data = buff_proj_1000,
                stroke = FALSE,
                color = 'brown',
                fillOpacity = 0.3,
                group = '1000 foot buffer - WRCOG GNG')
})

}
# Run the application 
shinyApp(ui = ui, server = server)
