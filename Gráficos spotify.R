library(dplyr)
library(ggplot2)
library(tidyverse)
      

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
#library(wesanderson)
#pal <- wes_palette(21, name = "GrandBudapest1", type = "continuous")
#print(as.character(pal))
###

##Barplot
#todos los paises
ggplot(data, aes(x=Pais, y=Energía)) + 
  geom_bar(stat = "identity", width = 0.8, fill = "#69b3a2")+
  coord_flip()
#orden por f(); todos los paises
data %>% 
  ggplot(aes(reorder(Pais,Energía),Energía))+
  geom_bar(stat = "identity", width = 0.8, fill = "#69b3a2")+
  labs(x="Pais")+
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
  ylim(0, 1) +
  coord_flip()

##Pyramid; no ggplot
#comparacion dos paises
#install.packages("ggcharts")
library(ggcharts)
data_piramide <- data |> 
  filter(Pais == "Chile" | Pais == "Argentina") |> 
  gather(key = "Variables", value = "Frecuencia", 2:9)
pyramid_chart(data = data_piramide, x = Variables, y = Frecuencia, group = Pais)

#Bar plot 3 países; ggplot
bbdd_bp <- data |> 
  gather(key = "Variables", value = "Frecuencia", 2:9) |> 
  filter(Pais == "Chile" | Pais == "Argentina" | Pais == "Bolivia") 
my_bar <- barplot(bbdd_bp$Frecuencia , border=F , names.arg=bbdd_bp$Variable , 
                  las=2 , 
                  col=c(rgb(0.3,0.1,0.4,0.6) , rgb(0.3,0.5,0.4,0.6) , rgb(0.3,0.9,0.4,0.6) ,  rgb(0.3,0.9,0.4,0.6)) , 
                  ylim=c(0,1) , 
                  main="" )
# Add abline
abline(v=c(4.9 , 9.7) , col="grey")

# Add the text 
#Legende
legend("topleft", legend = c("Alone","with Himself","With other genotype" ) , 
       col = c(rgb(0.3,0.1,0.4,0.6) , rgb(0.3,0.5,0.4,0.6) , rgb(0.3,0.9,0.4,0.6) ,  rgb(0.3,0.9,0.4,0.6)) , 
       bty = "n", pch=20 , pt.cex = 2, cex = 0.8, horiz = FALSE, inset = c(0.05, 0.05))


#de acá abajo necesitan areglo
##Pyramid; ggplot
ggplot(data_piramide) +
  geom_bar(data=data_piramide, aes(x = Variables, y = Frecuencia, fill = Pais), stat="identity") +
 # geom_bar(data=data, aes(x = Variables, y=-Frecuencia, fill = Pais), stat="identity") +
  geom_hline(yintercept=0, colour="white", lwd=1) +
  coord_flip(ylim=c(-1,1)) + 
  scale_y_continuous(breaks=seq(-1,1,0.5), labels=c(1,0.5,0,0.5,1)) +
  ggtitle("Comparación países")

##Pyramid; otro ggplot
library(ggthemes)
options(scipen = 999) 
# X Axis Breaks and Labels 
brks <- seq(-15000000, 15000000, 5000000)
lbls = paste0(as.character(c(seq(15, 0, -5), seq(5, 15, 5))), "m")

# Plot
ggplot(data_piramide, aes(x = Variables, y = Frecuencia, fill = Pais)) +   # Fill column
  geom_bar(stat = "identity", width = .6) +   # draw the bars
  scale_y_continuous(breaks = brks,   # Breaks
                     labels = lbls) + # Labels
  geom_hline(yintercept=0, colour="white", lwd=1)+
  coord_flip() +  # Flip axes
  labs(title="Comparación países") +
  theme_tufte() +  # Tufte theme from ggfortify
  theme(plot.title = element_text(hjust = .5), 
        axis.ticks = element_blank()) +   # Centre plot title
  scale_fill_brewer(palette = "Dark2")  # Color palette

#Mezcla de grafs
brks <- seq(-1, 1, 0.5)
lbls = paste0(as.character(c(seq(1, 0, -0.5), seq(0.5, 1, 0.5))))

ggplot(data_piramide, aes(x = Variables, y = Frecuencia, fill = Pais)) + 
  geom_bar(stat = "identity", width = .6) +
  scale_y_continuous(breaks = brks,   # Breaks
                     labels = lbls) + # Labels
  geom_hline(yintercept=0, colour="white", lwd=1)+
  coord_flip(ylim=c(-1,1)) + 
  labs(title="Comparación países") +
  theme_tufte() +  # Tufte theme from ggfortify
  theme(plot.title = element_text(hjust = .5), 
        axis.ticks = element_blank()) +   # Centre plot title
  scale_fill_brewer(palette = "Dark2")  # Color palette
install.packages("ggpol")
library(ggpol)
ggplot(data, aes(x = Variables, y = Frecuencia, fill = Pais)) +
  geom_bar(stat = "identity") + 
  facet_share(~Pais, dir = "h", scales = "free", reverse_num = TRUE) +
  coord_flip() +
  theme_minimal()
