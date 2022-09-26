library(dplyr)
library(ggplot2)
library(tidyverse)
      
###DATOS
bbdd <- read.csv("06-09-2022.csv")
bbdd <- bbdd |> 
  select (-X) |> 
  rename(Pais = Country) |> 
  rename(Variables = feature_index) |> 
  rename(Frecuencia = value)
bbdd$Frecuencia <- round(bbdd$Frecuencia, digits = 2)

#bbdd <- bbdd |> 
#  rename(Pais = X) |> 
#  select(-weighted_sum_tempo)

#data <- gather(bbdd, key = "Variables", value = "Frecuencia", 2:7)
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
#ó
#data <- pivot_wider(bbdd, names_from = Variables, values_from = Frecuencia)
###

###VARIABLES  
#bbdd$weighted_sum_energy
#bbdd$weighted_sum_danceability
#bbdd$weighted_sum_liveness
#bbdd$weighted_sum_speechiness
#bbdd$weighted_sum_liveness
#bbdd$weighted_sum_acousticness
#data$weighted_sum_loudness
#data$weighted_sum_mode
#data$weighted_sum_valence
###

###PALETA DE COLORES
#library(wesanderson)
#pal <- wes_palette(21, name = "GrandBudapest1", type = "continuous")
#print(as.character(pal))
###

##Barplot
#todos los paises
ggplot(data, aes(x=Pais, y=Energía)) + 
  geom_bar(stat = "identity", width = 0.8, fill = "#69b3a2")+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1.02))  +
  coord_flip()

#orden por f(); todos los paises
data %>% 
  ggplot(aes(reorder(Pais,Energía),Energía))+
  geom_bar(stat = "identity", width = 0.8, fill = "#69b3a2")+
  labs(x="Pais")+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1.02)) +
  coord_flip()

##Diverging Lollipop Chart
#todos los paises
ggplot(data, aes(x=Pais, y=Energía, label=Energía)) + 
  geom_point(stat='identity', colour  = "#69b3a2", size=6)  +
  geom_segment(aes(y = 0, 
                   x = Pais, 
                   yend = Energía, 
                   xend = Pais), 
               color = "#69b3a2") +
  geom_text(color="black", size=2) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1.02)) +
  coord_flip()

##Pyramid; no ggplot
#comparacion dos paises
#install.packages("ggcharts")
library(ggcharts)
data_piramide <- data |> 
  filter(Pais == "Chile" | Pais == "Argentina") |> 
  gather(key = "Variables", value = "Frecuencia", 2:9)
pyramid_chart(data = data_piramide, x = Variables, y = Frecuencia, group = Pais)


##Coropletico con leaflet
#Desde:
#https://learn.r-journalism.com/en/mapping/census_maps/census-maps/
#para cambiar el tipo de mapa https://leaflet-extras.github.io/leaflet-providers/preview/
#install.packages("tigris")
library(geojsonio)
library(tigris)
library(leaflet)

spdf <- geojson_read("https://raw.githubusercontent.com/johan/world.geo.json/master/countries.geo.json", what = "sp")
spdf@data$name[spdf@data$name == "United States of America"] <- "USA"
spdf_junto <- tigris::geo_join(spdf, data, "name", "Pais")
pal <- colorNumeric("Greens", domain=spdf_junto$Acústica)
popup <- paste0("<strong>País: </strong>",as.character(spdf_junto$name),
                "<br>",
                "<strong>Acústica: </strong>", as.character(spdf_junto$Acústica))
#spdf_junto <- subset(spdf_junto, !is.na(variable))#si se quiere sacar los nas

leaflet() %>%
  addProviderTiles(provider = "CartoDB.Positron",
                   options = providerTileOptions(minZoom = 1, maxZoom = 5.5)) |> 
  setView(lng = -63.5494, lat = -16.2837, zoom = 2) |> 
  addPolygons(data = spdf_junto , 
              fillColor = ~pal(spdf_junto$Acústica), 
              fillOpacity = 0.7, 
              weight = 0.2, 
              smoothFactor = 0.2, 
              popup = ~popup) |> 
  addLegend(pal = pal, 
            values = spdf_junto$Acústica, 
            position = "bottomright", 
            title = "Acústica")

##Radar: 1 país, todas las variables
#install.packages("fmsb")
library(fmsb)
pais1 <- data |> 
  slice(1)  #shiny
#  select(-Pais)
pais1 <- rbind(rep(1, 9), rep(0,9), pais1)

radarchart(pais1[,-1])


#desde https://r-graph-gallery.com/142-basic-radar-chart.html
radarchart(pais1[,-1], 
           title = "Características de audio de Argentina", #shiny
           axistype=1, 
           pcol=rgb(0.45,0.17,0.18,0.9), #linea
           pfcol=rgb(0.86,0.33,0.34,0.7) , plwd=2.5 , #relleno
           cglcol="grey", cglty=1, axislabcol="grey", 
           caxislabels=seq(0.00,0.99, 5), cglwd=0.8,
           vlcex=0.8 
)

##Radar: tres países, todas las variables
library(fmsb)
pais2 <- data |> 
  slice(1:3)   #shiny
 # select(-Pais)
pais2 <- rbind(rep(1, 9), rep(0,9), pais2)

#radarchart(pais2[,-1])

#https://r-graph-gallery.com/143-spider-chart-with-saveral-individuals.html
#sin rango min-max

#colores para tres categorías/países
colors_border=c( rgb(0.2,0.5,0.5,0.9), rgb(0.8,0.2,0.5,0.9) , rgb(0.7,0.5,0.1,0.9) )
colors_in=c( rgb(0.2,0.5,0.5,0.4), rgb(0.8,0.2,0.5,0.4) , rgb(0.7,0.5,0.1,0.4) )

radarchart(pais2[,-1], 
           title = "Características de audio de Argentina, Bolivia, y Brazil",
           axistype=1 , 
            pcol=colors_border, #linea
            pfcol=colors_in, plwd=2.5 , plty=1, #relleno
            cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0.00,0.99, 5), cglwd=0.8,
            #custom labels
            vlcex=0.8 
)

#leyenda
legend(x=1.2, y=1.3, legend = pais2[-c(1,2),]$Pais, 
       bty = "n",
       pch=20, 
       col=colors_in , text.col = "grey", 
       cex=1, pt.cex=3,
       y.intersp=0.58)

##Radar ggplot

bbddradar <- data |> 
  gather(key = "Variables", value = "Frecuencia", 2:9) |> 
  group_by(Pais) |> 
  filter(Pais == "Argentina")   #shiny 

ggplot(bbddradar) +
  geom_hline(
    aes(yintercept = y), 
    data.frame(y = c(0:1)),
    color = "lightgrey"
  ) + 
  geom_col(
    aes(
      x = Variables,
      y = Frecuencia,
      fill = Frecuencia
    ),
    position = "dodge2",
    show.legend = TRUE,
    alpha = .9
  ) + 
  labs(title = "Características de audio de Argentina",
       x = "Variables",
       y = "")+
  coord_polar()
