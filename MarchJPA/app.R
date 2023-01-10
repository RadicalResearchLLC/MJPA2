## This is an alpha version of March JPA warehouse map, a community mapping tool for warehouse impacts
## Authored by Mike McCarthy, Radical Research LLC
## First created September, 2022
## Last modified Novemober, 2022
#

library(shiny)
library(leaflet)
library(htmltools)
library(sf)
library(tidyverse)
library(markdown)
library(leaflet.extras)
library(DT)


## Define UI for application that displays warehouses
# Show app name and logos
ui <- fluidPage(title = 'RNOW March JPA Warehouse Map',
  tags$style(type="text/css", "div.info.legend.leaflet-control br {clear: both;}"),
    titlePanel(
      fluidRow(column(2, shiny::img(height = 80, src = 'RNOW.jpg')),
               column(4,
               div(style = 'height:40px; font-size: 30px;',
               'Warehouses on the 215/60 Corridor')),
               column(2, shiny::img(height = 80, src = 'RNOW.jpg')))
      ),
  ##Create a tabset display to have a readme file and main warehouse page
  tabsetPanel(
    tabPanel('Dashboard',
    # Display slider bar selections, checkbox, and summary text
    fluidRow(column(width = 10, dataTableOutput('sumJuris'))),
    fluidRow(
        column(width = 10, align = 'center', leafletOutput("map", height = 700))
        ),
    fluidRow(column(width = 10, align = 'center', dataTableOutput('PlannedWH')))
      ),
    tabPanel('Readme',
      div(style = 'width: 90%; margin: auto;',
      fluidRow(includeMarkdown("README.md")),
      )
    )
  )
)

server <- function(input, output) {

#Palettes
palJuris <- colorFactor(palette = c('red', 'orange', 'brown', 'blue', 'green'), 
                          domain = c('March JPA', 'Moreno Valley', 'Perris', 'Riverside', 
                                     'Unincorporated RivCo'))
palJuris2 <- colorFactor(palette = c('red', 'orange', 'brown', 'blue', 'green'), 
                        domain = c('MarchJPA', 'Moreno Valley', 'Perris', 'Riverside', 
                                   'Unincorporated RivCo'))
                                 
#Create leaflet map with legend and layers control
  
output$map <- renderLeaflet({
  leaflet() %>% 
    addTiles() %>% 
    addProviderTiles(provider = providers$CartoDB.Positron, group = 'Basemap') %>% 
    addProviderTiles(provider = providers$Esri.WorldImagery, 
                     group = 'Imagery') %>% 
    setView(lng = -117.24, lat = 33.875, zoom = 12) %>% 
    addLayersControl(baseGroups = c('Basemap', 'Imagery'),
                     overlayGroups = c('Jurisdictions', 
                                       'Existing Warehouses',
                                       'Planned Warehouses',
                                       '800 foot buffer'),
                     options = layersControlOptions(collapsed = FALSE)
    ) %>% 
    hideGroup(c('800 foot buffer', 'Jurisdictions')) %>% 
    addMapPane('Jurisdictions', zIndex = 390) %>% 
    addMapPane('800 foot buffer', zIndex = 395) %>% 
    addMapPane('Existing Warehouses', zIndex = 410) %>% 
    addMapPane('Planned Warehouses', zIndex = 420) %>% 
    addPolygons(data = jurisdictions,
                color = ~palJuris2(name),
                stroke = FALSE,
                fillOpacity = 0.4,
                label = ~htmlEscape(name),
                group = 'Jurisdictions',
                options = pathOptions(pane = 'Jurisdictions')) %>% 
    addLegend(data = warehouses_tidy,
              pal = palJuris,
              title = 'Jurisdiction',
              values = ~(jurisdiction))
})



#Existing warehouses
observe({
  leafletProxy("map", data = warehouses_tidy) %>% 
    clearGroup(group = 'Existing warehouses') %>%
    addPolygons(data = warehouses_tidy,
                color = ~palJuris(jurisdiction),
                fillColor = 'gray30',
                stroke = TRUE,
                weight = 2,
                fillOpacity = 0.6,
                group = 'Existing Warehouses',
                label = ~htmlEscape(paste(jurisdiction, name, 
                                          type, round(floorSpace.sq.ft,0), 'sq.ft.')),
                options = pathOptions(pane = 'Existing Warehouses'))
})

#Planned warehouses
observe({
  leafletProxy("map", data = planned_tidy) %>%
    clearGroup(group = 'Planned Warehouses') %>%
    addPolygons(data = planned_tidy,
                color = ~palJuris(jurisdiction),
                fillColor = 'gray70',
                stroke = TRUE,
                weight = 3,
                fillOpacity = 0.4,
                group = 'Planned Warehouses',
                label = ~htmlEscape(paste(name, 
                                          type, round(floorSpace.sq.ft,0), 'sq.ft.')),
                options = pathOptions(pane = 'Planned Warehouses')
    )
})

#Buffer 800 feet

observe({
  leafletProxy("map", data = buff_proj_800) %>%
    clearGroup(group = '800 foot buffer') %>%
    addPolygons(data = buff_proj_800,
                color = 'grey70',
                stroke = FALSE,
                fillOpacity = 0.2,
                group = '800 foot buffer',
                options = pathOptions(pane = '800 foot buffer'))
})

output$PlannedWH <- DT::renderDataTable(
  table4app,
  server = FALSE,
  caption  = 'Planned and approved warehouses',
  rownames = FALSE, 
  escape = FALSE,
  options = list(dom = 'Btp',
                 pageLength = 15,
                 buttons = c('csv','excel')),
  extensions = c('Buttons'),
  filter = list(position = 'top', clear = FALSE)
)

output$sumJuris <- DT::renderDataTable({

  DT::datatable(summaryTable,  
  caption  = 'This interactive map shows the warehouses along the 215/60 corridor jurisdictions. The summary table provides statistics on current and planned growth by jurisdiction. Note that warehouse complexes like the World Logistics Center and Stoneridge Commerce Center will have many warehouses but are currently estimated as a single warehouse. Please see Readme tab for more information on methods and data sources.',
  rownames = FALSE, 
  options = list(dom = '') 
  ) %>% 
    formatStyle(
      0,
      target = "row",
      backgroundColor = styleRow(1,'orange')
      )
}, server = FALSE)

}
# Run the application 
shinyApp(ui = ui, server = server)
