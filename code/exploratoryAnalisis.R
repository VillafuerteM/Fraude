################ DATOS NO BALANCEADOS ################
#        DETECCION DE FRAUDE EN TRANSACCIONES        #
#             PT1. ANALISIS EXPLORATORIO             #
#                  by: VillafuerteM                  #



# librerias ----
library(tidyverse)
library(lubridate)
library(tidymodels)
library(corrplot)
library(GGally)

# lectura de datos ----
datos <- read.csv('creditcard.csv')
# se hace un str para ver el tipo de variables y si identificamos faltantes
str(datos)

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

# Histogramas ----
# V1
ggplot(data=entrenamiento) + 
  geom_histogram(aes(x=V1), bins=sqrt(180000)) +
  coord_cartesian(xlim = c(-15, 3)) +
  labs(y='Frecuencia') + theme_minimal()

# V2
ggplot(data=entrenamiento) + 
  geom_histogram(aes(x=V2), bins=sqrt(180000)) +
  coord_cartesian(xlim = c(-15, 15)) +
  labs(y='Frecuencia') + theme_minimal()

# V3
ggplot(data=entrenamiento) + 
  geom_histogram(aes(x=V3), bins=sqrt(180000)) +
  coord_cartesian(xlim = c(-15,5)) +
  labs(y='Frecuencia') + theme_minimal()

# V4
ggplot(data=entrenamiento) + 
  geom_histogram(aes(x=V4), bins=sqrt(180000)) +
  coord_cartesian(xlim = c(-6,15)) +
  labs(y='Frecuencia') + theme_minimal()

# V5
ggplot(data=entrenamiento) + 
  geom_histogram(aes(x=V5), bins=sqrt(180000)) +
  coord_cartesian(xlim = c(-15,15)) +
  labs(y='Frecuencia') + theme_minimal()

# V6
ggplot(data=entrenamiento) + 
  geom_histogram(aes(x=V6), bins=sqrt(180000)) +
  coord_cartesian(xlim = c(-15,15)) +
  labs(y='Frecuencia') + theme_minimal()

# V7
ggplot(data=entrenamiento) + 
  geom_histogram(aes(x=V7), bins=sqrt(180000)) +
  coord_cartesian(xlim = c(-10,10)) +
  labs(y='Frecuencia') + theme_minimal()

# V8
ggplot(data=entrenamiento) + 
  geom_histogram(aes(x=V8), bins=sqrt(180000)) +
  coord_cartesian(xlim = c(-10,10)) +
  labs(y='Frecuencia') + theme_minimal()

# V9
ggplot(data=entrenamiento) + 
  geom_histogram(aes(x=V9), bins=sqrt(180000)) +
  #coord_cartesian(xlim = c(,)) +
  labs(y='Frecuencia') + theme_minimal()

# V10
ggplot(data=entrenamiento) + 
  geom_histogram(aes(x=V10), bins=sqrt(180000)) +
  coord_cartesian(xlim = c(-10,10)) +
  labs(y='Frecuencia') + theme_minimal()

# V11
ggplot(data=entrenamiento) + 
  geom_histogram(aes(x=V11), bins=sqrt(180000)) +
  #coord_cartesian(xlim = c(,)) +
  labs(y='Frecuencia') + theme_minimal()

# V12
ggplot(data=entrenamiento) + 
  geom_histogram(aes(x=V12), bins=sqrt(180000)) +
  #coord_cartesian(xlim = c(,)) +
  labs(y='Frecuencia') + theme_minimal()

# V13
ggplot(data=entrenamiento) + 
  geom_histogram(aes(x=V13), bins=sqrt(180000)) +
  #coord_cartesian(xlim = c(,)) +
  labs(y='Frecuencia') + theme_minimal()

# V14
ggplot(data=entrenamiento) + 
  geom_histogram(aes(x=V14), bins=sqrt(180000)) +
  coord_cartesian(xlim = c(-10,10)) +
  labs(y='Frecuencia') + theme_minimal()

# V15
ggplot(data=entrenamiento) + 
  geom_histogram(aes(x=V15), bins=sqrt(180000)) +
  #coord_cartesian(xlim = c(,)) +
  labs(y='Frecuencia') + theme_minimal()

# V16
ggplot(data=entrenamiento) + 
  geom_histogram(aes(x=V16), bins=sqrt(180000)) +
  coord_cartesian(xlim = c(-10,10)) +
  labs(y='Frecuencia') + theme_minimal()

# V17
ggplot(data=entrenamiento) + 
  geom_histogram(aes(x=V17), bins=sqrt(180000)) +
  coord_cartesian(xlim = c(-10,10)) +
  labs(y='Frecuencia') + theme_minimal()

# V18
ggplot(data=entrenamiento) + 
  geom_histogram(aes(x=V18), bins=sqrt(180000)) +
  #coord_cartesian(xlim = c(,)) +
  labs(y='Frecuencia') + theme_minimal()

# V19
ggplot(data=entrenamiento) + 
  geom_histogram(aes(x=V19), bins=sqrt(180000)) +
  #coord_cartesian(xlim = c(,)) +
  labs(y='Frecuencia') + theme_minimal()

# V20
ggplot(data=entrenamiento) + 
  geom_histogram(aes(x=V20), bins=sqrt(180000)) +
  coord_cartesian(xlim = c(-5,5)) +
  labs(y='Frecuencia') + theme_minimal()

# V21
ggplot(data=entrenamiento) + 
  geom_histogram(aes(x=V21), bins=sqrt(180000)) +
  coord_cartesian(xlim = c(-10,10)) +
  labs(y='Frecuencia') + theme_minimal()

# V22
ggplot(data=entrenamiento) + 
  geom_histogram(aes(x=V22), bins=sqrt(180000)) +
  #coord_cartesian(xlim = c(,)) +
  labs(y='Frecuencia') + theme_minimal()

# V23
ggplot(data=entrenamiento) + 
  geom_histogram(aes(x=V23), bins=sqrt(180000)) +
  coord_cartesian(xlim = c(-5,5)) +
  labs(y='Frecuencia') + theme_minimal()

# V24
ggplot(data=entrenamiento) + 
  geom_histogram(aes(x=V24), bins=sqrt(180000)) +
  #coord_cartesian(xlim = c(,)) +
  labs(y='Frecuencia') + theme_minimal()

# V25
ggplot(data=entrenamiento) + 
  geom_histogram(aes(x=V25), bins=sqrt(180000)) +
  #coord_cartesian(xlim = c(,)) +
  labs(y='Frecuencia') + theme_minimal()

# V26
ggplot(data=entrenamiento) + 
  geom_histogram(aes(x=V26), bins=sqrt(180000)) +
  #coord_cartesian(xlim = c(,)) +
  labs(y='Frecuencia') + theme_minimal()

# V27
ggplot(data=entrenamiento) + 
  geom_histogram(aes(x=V27), bins=sqrt(180000)) +
  coord_cartesian(xlim = c(-5,5)) +
  labs(y='Frecuencia') + theme_minimal()

# V28
ggplot(data=entrenamiento) + 
  geom_histogram(aes(x=V28), bins=sqrt(180000)) +
  coord_cartesian(xlim = c(-2.5,2.5)) +
  labs(y='Frecuencia') + theme_minimal()

# Amount
ggplot(data=entrenamiento) + 
  geom_histogram(aes(x=Amount), bins=sqrt(180000)) +
  coord_cartesian(xlim = c(0, 1500)) +
  labs(y='Frecuencia') + theme_minimal()

# Boxplots separados por clase ----
# V1
ggplot(data=entrenamiento,aes(y=V1, x=factor(Class))) +
  geom_boxplot() +
  stat_summary(fun=mean,na.rm=T, geom="point", shape=20, color="red", fill="red", size=2)+
  coord_cartesian(ylim=c(-20,2.5)) +
  labs(x="Fraud") + theme_minimal()

# V2 
ggplot(data=entrenamiento,aes(y=V2, x=factor(Class))) +
  geom_boxplot() +
  stat_summary(fun=mean,na.rm=T, geom="point", shape=20, color="red", fill="red", size=2)+
  coord_cartesian(ylim=c(-5,15)) +
  labs(x="Fraud") + theme_minimal()

# V3 
ggplot(data=entrenamiento,aes(y=V3, x=factor(Class))) +
  geom_boxplot() +
  stat_summary(fun=mean,na.rm=T, geom="point", shape=20, color="red", fill="red", size=2)+
  coord_cartesian(ylim=c(-20,5)) +
  labs(x="Fraud") + theme_minimal()

# V4 
ggplot(data=entrenamiento,aes(y=V4, x=factor(Class))) +
  geom_boxplot() +
  stat_summary(fun=mean,na.rm=T, geom="point", shape=20, color="red", fill="red", size=2)+
  labs(x="Fraud") + theme_minimal()

# V5 
ggplot(data=entrenamiento,aes(y=V5, x=factor(Class))) +
  geom_boxplot() +
  stat_summary(fun=mean,na.rm=T, geom="point", shape=20, color="red", fill="red", size=2)+
  coord_cartesian(ylim=c(-15,10)) +
  labs(x="Fraud") + theme_minimal()

# V6 
ggplot(data=entrenamiento,aes(y=V6, x=factor(Class))) +
  geom_boxplot() +
  stat_summary(fun=mean,na.rm=T, geom="point", shape=20, color="red", fill="red", size=2)+
  coord_cartesian(ylim=c(-7.5,5)) +
  labs(x="Fraud") + theme_minimal()

# V7 
ggplot(data=entrenamiento,aes(y=V7, x=factor(Class))) +
  geom_boxplot() +
  stat_summary(fun=mean,na.rm=T, geom="point", shape=20, color="red", fill="red", size=2)+
  coord_cartesian(ylim=c(-20,10)) +
  labs(x="Fraud") + theme_minimal()

# V8 
ggplot(data=entrenamiento,aes(y=V8, x=factor(Class))) +
  geom_boxplot() +
  stat_summary(fun=mean,na.rm=T, geom="point", shape=20, color="red", fill="red", size=2)+
  coord_cartesian(ylim=c(-5,5)) +
  labs(x="Fraud") + theme_minimal()

# V9 
ggplot(data=entrenamiento,aes(y=V9, x=factor(Class))) +
  geom_boxplot() +
  stat_summary(fun=mean,na.rm=T, geom="point", shape=20, color="red", fill="red", size=2)+
  coord_cartesian(ylim=c(-10,5)) +
  labs(x="Fraud") + theme_minimal()

# V10
ggplot(data=entrenamiento,aes(y=V10, x=factor(Class))) +
  geom_boxplot() +
  stat_summary(fun=mean,na.rm=T, geom="point", shape=20, color="red", fill="red", size=2)+
  coord_cartesian(ylim=c(-15,4)) +
  labs(x="Fraud") + theme_minimal()

# V11
ggplot(data=entrenamiento,aes(y=V11, x=factor(Class))) +
  geom_boxplot() +
  stat_summary(fun=mean,na.rm=T, geom="point", shape=20, color="red", fill="red", size=2)+
  labs(x="Fraud") + theme_minimal()

# V12
ggplot(data=entrenamiento,aes(y=V12, x=factor(Class))) +
  geom_boxplot() +
  stat_summary(fun=mean,na.rm=T, geom="point", shape=20, color="red", fill="red", size=2)+
  coord_cartesian(ylim=c(-20,5)) +
  labs(x="Fraud") + theme_minimal()

# V13
ggplot(data=entrenamiento,aes(y=V13, x=factor(Class))) +
  geom_boxplot() +
  stat_summary(fun=mean,na.rm=T, geom="point", shape=20, color="red", fill="red", size=2)+
  coord_cartesian(ylim=c(-4,4)) +
  labs(x="Fraud") + theme_minimal()

# V14
ggplot(data=entrenamiento,aes(y=V14, x=factor(Class))) +
  geom_boxplot() +
  stat_summary(fun=mean,na.rm=T, geom="point", shape=20, color="red", fill="red", size=2)+
  coord_cartesian(ylim=c(-20,5)) +
  labs(x="Fraud") + theme_minimal()

# V15
ggplot(data=entrenamiento,aes(y=V15, x=factor(Class))) +
  geom_boxplot() +
  stat_summary(fun=mean,na.rm=T, geom="point", shape=20, color="red", fill="red", size=2)+
  coord_cartesian(ylim=c(-5,5)) +
  labs(x="Fraud") + theme_minimal()

# V16
ggplot(data=entrenamiento,aes(y=V16, x=factor(Class))) +
  geom_boxplot() +
  stat_summary(fun=mean,na.rm=T, geom="point", shape=20, color="red", fill="red", size=2)+
  coord_cartesian(ylim=c(-15,5)) +
  labs(x="Fraud") + theme_minimal()

# V17
ggplot(data=entrenamiento,aes(y=V17, x=factor(Class))) +
  geom_boxplot() +
  stat_summary(fun=mean,na.rm=T, geom="point", shape=20, color="red", fill="red", size=2)+
  labs(x="Fraud") + theme_minimal()

# V18
ggplot(data=entrenamiento,aes(y=V18, x=factor(Class))) +
  geom_boxplot() +
  stat_summary(fun=mean,na.rm=T, geom="point", shape=20, color="red", fill="red", size=2)+
  labs(x="Fraud") + theme_minimal()

# V19
ggplot(data=entrenamiento,aes(y=V19, x=factor(Class))) +
  geom_boxplot() +
  stat_summary(fun=mean,na.rm=T, geom="point", shape=20, color="red", fill="red", size=2)+
  labs(x="Fraud") + theme_minimal()

# V20
ggplot(data=entrenamiento,aes(y=V20, x=factor(Class))) +
  geom_boxplot() +
  stat_summary(fun=mean,na.rm=T, geom="point", shape=20, color="red", fill="red", size=2)+
  coord_cartesian(ylim=c(-2.5,2.5)) +
  labs(x="Fraud") + theme_minimal()

# V21
ggplot(data=entrenamiento,aes(y=V21, x=factor(Class))) +
  geom_boxplot() +
  stat_summary(fun=mean,na.rm=T, geom="point", shape=20, color="red", fill="red", size=2)+
  coord_cartesian(ylim=c(-3,3)) +
  labs(x="Fraud") + theme_minimal()

# V22
ggplot(data=entrenamiento,aes(y=V22, x=factor(Class))) +
  geom_boxplot() +
  stat_summary(fun=mean,na.rm=T, geom="point", shape=20, color="red", fill="red", size=2)+
  coord_cartesian(ylim=c(-3,3)) +
  labs(x="Fraud") + theme_minimal()

# V23
ggplot(data=entrenamiento,aes(y=V23, x=factor(Class))) +
  geom_boxplot() +
  stat_summary(fun=mean,na.rm=T, geom="point", shape=20, color="red", fill="red", size=2)+
  coord_cartesian(ylim=c(-2.5,2.5)) +
  labs(x="Fraud") + theme_minimal()

# V24
ggplot(data=entrenamiento,aes(y=V24, x=factor(Class))) +
  geom_boxplot() +
  stat_summary(fun=mean,na.rm=T, geom="point", shape=20, color="red", fill="red", size=2)+
  coord_cartesian(ylim=c(-2.5,2.5)) +
  labs(x="Fraud") + theme_minimal()

# V25
ggplot(data=entrenamiento,aes(y=V25, x=factor(Class))) +
  geom_boxplot() +
  stat_summary(fun=mean,na.rm=T, geom="point", shape=20, color="red", fill="red", size=2)+
  coord_cartesian(ylim=c(-2,2)) +
  labs(x="Fraud") + theme_minimal()

# V26
ggplot(data=entrenamiento,aes(y=V26, x=factor(Class))) +
  geom_boxplot() +
  stat_summary(fun=mean,na.rm=T, geom="point", shape=20, color="red", fill="red", size=2)+
  coord_cartesian(ylim=c(-2,2)) +
  labs(x="Fraud") + theme_minimal()

# V27
ggplot(data=entrenamiento,aes(y=V27, x=factor(Class))) +
  geom_boxplot() +
  stat_summary(fun=mean,na.rm=T, geom="point", shape=20, color="red", fill="red", size=2)+
  coord_cartesian(ylim=c(-2,2)) +
  labs(x="Fraud") + theme_minimal()

# V28
ggplot(data=entrenamiento,aes(y=V28, x=factor(Class))) +
  geom_boxplot() +
  stat_summary(fun=mean,na.rm=T, geom="point", shape=20, color="red", fill="red", size=2)+
  coord_cartesian(ylim=c(-1.5,1.5)) +
  labs(x="Fraud") + theme_minimal()

# Amount
ggplot(data=entrenamiento,aes(y=Amount, x=factor(Class))) +
  geom_boxplot() +
  stat_summary(fun=mean,na.rm=T, geom="point", shape=20, color="red", fill="red", size=2)+
  coord_cartesian(ylim=c(0,350)) +
  labs(x="Fraud") + theme_minimal()

# analisis de correlaciones ----
M <-cor(entrenamiento%>%select(-number), use = "pairwise.complete.obs")
corrplot(M, type = "upper", tl.col="black")


# dispersiones todos contra todos ----
ggpairs(entrenamiento%>%select(-number))

