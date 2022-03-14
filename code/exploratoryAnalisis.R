################ DATOS NO BALANCEADOS ################
#        DETECCION DE FRAUDE EN TRANSACCIONES        #
#             PT1. ANALISIS EXPLORATORIO             #
#                  by: VillafuerteM                  #



# librerias ----
library(tidyverse)
library(lubridate)
library(tidymodels)

# lectura de datos ----
datos <- read.csv('creditcard.csv')
summary(datos)

# ajuste de la variable Time ----
# declaramos una fecha inicial aleatoria a falta de información
datos <- datos %>% dplyr::mutate(fechaInicial=as.Date('2013-01-01'))
datos <- datos %>% dplyr::mutate(fechaTransaccion = fechaInicial+lubridate::seconds(Time))
datos <- datos %>% dplyr::select(-c(Time, fechaInicial,fechaTransaccion))
# se observa con la fechaTransaccion máxima que se tiene información de transacciones
# de dos días enteros a partir de la realización de la primera transacción del set de datos
# esta variable no se considerará ya que no nos da mucha información por ser un corto periodo
# aún si se observara una concentración de fraudes en horas más altas de la noche, solo se cuentan
# con dos días, por lo que se tendría que estudiar con información de otros días si esto se cumple

# sets de prueba, validación y entrenamiento ----
# se agrega la variable secuencial para identificar casos puntuales de ser necesario (C.U.)
datos <- datos %>% dplyr::mutate(number=seq(from=1, to=284807))
fraudes <- datos %>% filter(Class==1)
noFraudes <- datos %>% filter(Class==0)
set.seed(20010725)
datos_particion <- initial_split(fraudes, 0.8)
entrenamiento<- training(datos_particion)
prueba<- testing(datos_particion)
datos_particion <-initial_split(entrenamiento, 0.8)
entrenamiento <- training(datos_particion)
validacion <- testing(datos_particion)
# tenemos que repetir lo anterior pero con los casos de no fraude
set.seed(20210619)
datos_particion <- initial_split(noFraudes, 0.8)
entrenamiento2 <- training(datos_particion)
prueba2<- testing(datos_particion)
datos_particion <-initial_split(entrenamiento2, 0.8)
entrenamiento2 <- training(datos_particion)
validacion2 <- testing(datos_particion)
# los juntamos
entrenamiento <- rbind(entrenamiento, entrenamiento2)
prueba <- rbind(prueba, prueba2)
validacion <- rbind(validacion, validacion2)
rm(entrenamiento2, prueba2, validacion2, fraudes, noFraudes, datos_particion)

# analisis exploratorio ----
# summary para tabla de medidas descriptivas
summary(entrenamiento)
# Fig $.$
ggplot(data=entrenamiento) +
  geom_histogram(aes(x=Amount, fill=factor(Class))) + 
  facet_wrap(~Class,scales="free_y")

# analisis de correlaciones ----
