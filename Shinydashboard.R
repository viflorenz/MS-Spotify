#library(shinydashboard)
#library(shiny)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(ggcharts)
library(bslib)
#####
bbdd <- read.csv("./Detector_Musical/Data/06-09-2022.csv")
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


ui <- dashboardPage(
  dashboardHeader(title = "Spotify dashboard: Monitor Musical"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Mapa coroplético", tabName = "mapa", icon = icon("map")),
      menuItem("Estadísticas", tabName = "estad", icon = icon("th"))
    )
  ),
  dashboardBody(
    tabItems(
    tabItem(tabName = "mapa",
            h2("Mapa coroplético"),
            p("Esto es una prueba")
    ),
    tabItem(tabName = "estad",
            h2("Características de audio"),
    fluidRow(
      box(plotOutput("grafico", height = 250)),
      box(
        title = "Comparador de países",
        selectInput("pais1",
                    "Pais 1:",
                    data$Pais,
                    selected = "Argentina"),
        selectInput("pais2",
                    "Pais 2:",
                    data$Pais,
                    selected = "Chile"))
    )
   ))
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

shinyApp(ui, server)
