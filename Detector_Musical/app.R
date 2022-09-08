#library(shiny)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(ggcharts)
library(bslib)

#bbdd <- read.csv("06-09-2022.csv")
bbdd <- read.csv("./Data/06-09-2022.csv")
bbdd <- bbdd |> 
  select (-X) |> 
  rename(Pais = Country) |> 
  rename(Variables = feature_index) |> 
  rename(Frecuencia = value)
bbdd$Frecuencia <- round(bbdd$Frecuencia, digits = 2)
data <- spread(bbdd, key = Variables, value = Frecuencia)
data <- data |> 
  select(-weighted_sum_tempo) |> 
  rename(Acústica = weighted_sum_acousticness) |> 
  rename(Bailabilidad = weighted_sum_danceability) |> 
  rename(Energía = weighted_sum_energy) |> 
  rename('En vivo' = weighted_sum_liveness) |> 
  rename(Ruidosidad = weighted_sum_loudness) |> 
  rename(Modalidad = weighted_sum_mode) |> 
  rename(Habla = weighted_sum_speechiness) |> 
  rename(Positividad = weighted_sum_valence)

ui <- fluidPage(
  theme = bs_theme(version = 4, bootswatch = "lux"),
    titlePanel("América Musical"),
    sidebarLayout(
        sidebarPanel(
            p("Monitoreo de la escena musical del continente americano mediante el uso de información obtenida desde Spotify. Elija dos países distintos para compararlos:"),
            selectInput("pais1",
                        "Pais 1:",
                        data$Pais,
                        selected = "Argentina"),
            selectInput("pais2",
                        "Pais 2:",
                        data$Pais,
                        selected = "Chile")
        ),
        mainPanel(
          plotOutput("grafico")
        )
        ))


server <- function(input, output) {
  output$grafico <- renderPlot({
    data_piramide <- data |> 
      filter(Pais == input$pais1 | Pais == input$pais2) |> 
      gather(key = "Variables", value = "Frecuencia", 2:9)
    
    pyramid_chart(data = data_piramide, x = Variables, y = Frecuencia, group = Pais,
                  bar_colors = c("#C44A4B", "#67221B"))
  })
  
}

shinyApp(ui = ui, server = server)
