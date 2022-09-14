#library(shiny)
#library(shinythemes)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(ggcharts)
library(bslib)
library(readxl)

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

#tablaaf <- read_xlsx("Audio Features Spotify.xlsx")
tablaaf <- read_xlsx("./Data/Audio Features Spotify.xlsx")

ui <- fluidPage(
    theme = bs_theme(bootswatch = "minty"),
    navbarPage(
      "Monitor Social",
    #1----------------------
    tabPanel(
      "Panel países", 
    titlePanel("Monitor Musical"),
    p("Monitoreo de la escena musical del continente americano mediante el uso de información obtenida desde Spotify."),
    p("Datos del 6/9/2022, extraídos por",
      a("Camila Rojas.",
        href = "https://github.com/camirojasguajardo/",
        target="_blank", rel="noopener noreferrer" )
      ),
    p("De",
    a("Monitor Social.",
      href = "https://www.monitorsocial.cl/",
      target="_blank", rel="noopener noreferrer")),
    sidebarLayout(
        sidebarPanel(
            p("Elija dos países distintos para comparar las características de audio:"),
            selectInput("pais1",
                        "Pais 1:",
                        data$Pais,
                        selected = "Argentina"),
            selectInput("pais2",
                        "Pais 2:",
                        data$Pais,
                        selected = "Chile")),
        mainPanel(plotOutput("grafico"))
        )),
    #2----------------------
    tabPanel(
      "Panel variables",
             titlePanel("Monitor Musical"),
             p("Monitoreo de la escena musical del continente americano mediante el uso de información obtenida desde Spotify."),
             p("Datos del 6/9/2022, extraídos por",
               a("Camila Rojas.",
                 href = "https://github.com/camirojasguajardo/",
                 target="_blank", rel="noopener noreferrer")),
             p("De",
               a("Monitor Social.",
                 href = "https://www.monitorsocial.cl/",
                 target="_blank", rel="noopener noreferrer")),
             sidebarLayout(
               sidebarPanel(
                 p("Elija una característica de audio para comparar los países:"),
                 selectInput("variables",
                             "Características de audio:",
                             c("Acústica", "Bailabilidad", 
                               "Energía","En vivo","Ruidosidad",
                               "Modalidad","Habla","Positividad"),
                             selected = "Acústica")),
               mainPanel(plotOutput("graficobarras"))
      )),
    #3----------------------
    tabPanel(
      "Características de audio",
            tableOutput("tabla"))
        ))


server <- function(input, output) {
  output$grafico <- renderPlot({
    data_piramide <- data |> 
      filter(Pais == input$pais1 | Pais == input$pais2) |> 
      gather(key = "Variables", value = "Frecuencia", 2:9)
    pyramid_chart(data = data_piramide, x = Variables, y = Frecuencia, group = Pais,
                  bar_colors = c("#C44A4B", "#67221B"))
  })
  output$graficobarras <- renderPlot({
    bbdd2 <- data |> 
      select(Pais, input$variables) |> 
      rename("Z" = 2)
    bbdd2 %>% 
      ggplot(aes(reorder(Pais,Z),Z))+
      geom_bar(stat = "identity", width = 0.8, fill = "#67221B")+
      labs(x="Pais",
           y=input$variables)+
      coord_flip()
  })
  output$tabla <- renderTable({
    tablaaf
  })
}

shinyApp(ui = ui, server = server)
