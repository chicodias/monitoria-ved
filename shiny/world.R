library(shiny)
library(leaflet)
library(spData)
library(tidyverse)

dados <- read_csv(...)

sf <- world %>% select(name_long, geom)

dados <- left_join(world, dados, by = c("name_long" = "county_name"), keep)


escolhas <- c("pop", "area_km2","lifeExp", "gdpPercap")
palhetas <- c("Reds", "Blues", "YlOrRd", "YlOrBlu")

ui <- fluidPage(
    selectInput("var", "escolha a variavel", escolhas),
    selectInput("pal", "escolha a palheta", palhetas),
    numericInput("bins", "quantidade de classes", value = 5, min = 4, max = 8),
  leafletOutput("mapa")
)

server <- function(input, output, session) {
  output$mapa <- renderLeaflet({
    
    variavel <- input$var
    palhet <- input$pal
    nbin <- input$bins
    
    colorData <- dados[[variavel]]
    pal <- colorBin(palhet, colorData, bins = nbin)
    
    
    mapa <- dados %>% leaflet() %>%
      addTiles() %>% 
      addPolygons(
        fillColor = ~pal(colorData),
        label = ~name_long,
        smoothFactor = 0.5,
        fillOpacity = 1,
        weight = 0.5,
        opacity = 0.8,
        stroke = T,
        color = "black",
        highlightOptions = highlightOptions(
          color = "black",
          weight = 2,
          bringToFront = TRUE
        ),
        popup = ~ paste0(
          sep = " ",
          "<b>", name_long, "<b><br>",
          "área km2: ", area_km2, "<br>",
          "Expectativa de vida: ", round(lifeExp,1) , "<br>",
          "Pib per capita: ", round(gdpPercap,2), "<br>"
        )) %>%
      addLegend("bottomright",
                title = variavel,
                pal = pal,
                values = ~colorData)
  
    mapa
    })
}

shinyApp(ui, server)