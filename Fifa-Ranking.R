file_to_download <- "fifa_ranking.csv"
url <- paste0("https://github.com/tadhgfitzgerald/fifa_ranking/raw/master/", file_to_download)
download.file(url, destfile = "./fifa_ranking.csv")


library(rio)
rk <- import("./fifa_ranking.csv")


library(devtools)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(plotly)
library(viridis)
library(hrbrthemes)
library(ggThemeAssist)
library(gganimate)

#Seleccionamos solo los datos del nombre del pais, su abreviatura, el total de puntos FIFA, la confederacion a la que corresponde y la fecha de los puntos.
aa <- rk %>% select(country_full, country_abrv, total_points, confederation, rank_date)


#Filtramos por el ultimo registro de datos, para poder sacar el grafico lo mas actualizado posible.
a2018 <- aa %>% filter(rank_date == "2018-06-07")

#Eliminamos la variable fecha, porque ya no nose hace falta.
a2018 <- a2018 %>% select(-rank_date)

#ordenamos la tabla por los puntos FIFA.
a2018 %>% arrange(total_points)

#Seleccionamos solo los 10 mejor paises.
ar2018 <- a2018 %>% slice(c(1:10))
ar2018 <- ar2018 %>% mutate(country_abrv = forcats::as_factor(country_abrv))
ar2018 <- ar2018 %>% mutate(country_abrv = forcats::fct_reorder(country_abrv, total_points))

#Realizamos el grafico indicando las variables que queremos que aparezcan en él.
g2018 <- ggplot(ar2018, aes(country_abrv, total_points))
g2018 + geom_bar(stat = "identity", fill = "red") + coord_flip() + geom_text(aes(label = total_points),hjust = -0.15, size = 3.5)


#Agrupamos las confederaciones de futbol y sacamos la media de puntos de cada una de ellas
confederation_rank <- aa %>% select(-country_abrv, -country_full, -rank_date) %>% filter(total_points > 0) %>% group_by(confederation) %>% summarise(mean(total_points))

knitr::kable(confederation_rank, format = "html")

#Vamos a ver quienes son los paises con mayor puntuación.
ab <- aa %>% filter(rank_date == "2011-12-21" | rank_date == "2012-12-19" | rank_date == "2013-12-19" | rank_date == "2014-12-18" | rank_date == "2015-12-03" | rank_date == "2016-12-22" | rank_date == "2017-12-21" | rank_date == "2018-06-07") %>% arrange(desc(total_points)) %>% slice(1:10)

ggplot(ab, aes(country_full, total_points, color = country_full, size = total_points)) + geom_point() +geom_text(aes(label = total_points),hjust = -0.15, size = 3.5)


#Evolución de España y Belica en los ultimos 8 años.
esp_bel <- aa %>% filter(country_full %in% c("Belgium","Spain" ))
esp_bel <- esp_bel %>% filter(rank_date == "2011-12-21" | rank_date == "2012-12-19" | rank_date == "2013-12-19" | rank_date == "2014-12-18" | rank_date == "2015-12-03" | rank_date == "2016-12-22" | rank_date == "2017-12-21" | rank_date == "2018-06-07")


p2 <- ggplot(esp_bel, aes(rank_date, total_points, color = country_full))
p2 + geom_point() + geom_line(aes(group = country_full)) + geom_text(aes(label = total_points),hjust = -0.15, size = 3.5) + ggtitle("Evolucion de la Seleccioon Española y la seleccion Belga", subtitle = "Evolución del año 2011 al 2018") + theme_minimal()


#Seleccionamos solo las variables que nos hacen falta, que seran la abreviatura del pais, los puntos FIFA y la fechha de ess puntos.
aa1 <- rk %>% select(country_abrv, total_points, rank_date)

#Filtramos los datos por las fechas y por los paises que nos interesa, y a su vez los ordenamos.
top10 <- aa1 %>% filter(rank_date == "2011-12-21" | rank_date == "2012-12-19" | rank_date == "2013-12-19" | rank_date == "2014-12-18" | rank_date == "2015-12-03" | rank_date == "2016-12-22" | rank_date == "2017-12-21" | rank_date == "2018-06-07") %>% filter(country_abrv %in% c("GER","BRA","BEL","POR","ARG","SUI","FRA","POL","CHI","ESP")) %>%group_by(rank_date) %>% mutate(ordering = min_rank(total_points) * 1.0) %>% ungroup()


gstop10 <- top10 %>% ggplot(aes(ordering, group = country_abrv)) + geom_tile(aes(y = total_points, height = total_points, width = 0.9, fill = country_abrv), alpha = 0.9) +

#Texto en las barras
geom_text(aes(y = total_points, label = paste(as.character(round(total_points)),'points')), vjust = 0.4, hjust = 0.55, size = 7) + geom_text(aes(y = 0, label = country_abrv), vjust = 0.4, hjust = 0.0, size = 7) +

#Ponemos un titulo al grafico
labs(title = paste("Puntos Año", '{closest_state}', sep = ""), subtitle = "Ranking FIFA", x = '', y = '') +

#Clip = 'off' para quitarel texto de los margenes
coord_flip(clip = 'off') +

#modificacion de tamaños
theme(plot.title = element_text(size = 22), plot.subtitle = element_text(size = 16), axis.ticks.x = element_blank(), axis.text.x = element_blank(), axis.ticks.y = element_blank(), axis.text.y = element_blank(), legend.position = "none")

#Creamos animacion para que cambie por fecha.
ganimtop10 <- gstop10 + transition_states(rank_date, transition_length = 50, state_length = 0, wrap = FALSE) + view_follow()

#Animacion
ganimtop10 %>% animate(fps = 15, nframes = 1000, detail = 30, start_pause = 50, end_pause = 50, rewind = FALSE, width = 400, height = 400)

#Guardar la animacion en fromato gif
anim_save('ranking top 10 2011 - 2018.gif')
