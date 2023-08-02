#instalando librerias
install.packages("nycflights13")
library(tidyverse)
library(nycflights13)

#cargando el dataframe
vuelo<-data.frame(flights)
vuelo <- vuelo[!is.na(vuelo),]

#filter
#cuantos vuelos tienen un retraso de 2 horas o mas en llegar?
vuelo %>% filter(arr_delay>120)%>%summarise(NN=n())

#cuantos volaron a Houston? (IAH o HOU)
vuelo %>% filter(dest=="IAH"|dest=="HOU")%>%summarise(NN=n())

#cuantos llegaron mas de 2 horas tarde, pero nunca salieron tarde?
vuelo %>% filter(arr_delay>120 & dep_delay <=0)%>%summarise(NN=n())

#arrange
#encontrar los 5 vuelos mas demorados en llegar
vuelo %>% arrange (desc(arr_delay)) %>% head(5)

#cual es el vuelo que estuvo mas tiempo en el aire
vuelo %>% filter(!air_time=="NA") %>% arrange(air_time) %>% tail(1)

#cual es el vuelo que estuvo menos tiempo en el aire
vuelo %>% arrange(air_time) %>% head(1)

#select 
#V o F select(year, month, day)==select(year:day)
f1<-vuelo %>% select(year, month, day)
f2<-vuelo %>% select(year:day)
vuelo %>%  mutate(Logica = f1 == f2) %>% head()
"o"
vuelo %>%  mutate(Logica = vuelo %>%select(year, month, day)== vuelo %>% select(year:day)) %>% head()

#seleccionar variables dep_time,dep_delay,arr_time y arr_delay
vuelo%>%select(dep_time,dep_delay,arr_time,arr_delay)%>% head()

#que sucede si repito la misma variable en select()
vuelo%>%select(dep_time,dep_time,dep_time)%>% head()

#mutate
#calculo de la velocidad de cada vuelo
vuelo %>% mutate(velocidad=distance/air_time)%>%arrange(velocidad)%>%head()

#rankeo por la velocidad
vuelo %>% mutate(velocidad=distance/air_time,rank=rank(velocidad))%>%arrange(rank) %>% head()

vuelo %>% mutate(min_rank(velocidad=distance/air_time)) %>% head()

#los 10 vuelos mas demorados en salir
vuelo %>% arrange(desc(dep_delay))%>%head(10)

#ggplot2
# install.packages("ggplot2")
library(ggplot2)

ggplot(vuelo, aes(x = arr_time, y = year)) +
  geom_bar(stat = "identity", fill = 4)

ggplot(vuelo, aes(x = carrier, y = dep_time, fill = carrier)) +
  geom_bar(stat = "identity")


# install.packages("ggplot2")
library(ggplot2)

ggplot(vuelo, aes(x = arr_delay, fill = carrier)) +
  geom_density(alpha = 0.5)
