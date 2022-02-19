################ DATOS NO BALANCEADOS ################
#        DETECCION DE FRAUDE EN TRANSACCIONES        #
#             PT1. ANALISIS EXPLORATORIO             #
#                  by: VillafuerteM                  #

# librerias ----
library(tidyverse)

# lectura de datos ----
datos = read.csv("D:/Tesis/Fraude/data/creditcard.csv")

# summary ----
summary(datos)

# analisis exploratorio ----
# Fig $.$
ggplot(datos, aes(x=Time, y=Amount, color=as.factor(Class))) + geom_point() + facet_grid(cols=vars(Class))

# analisis de correlaciones ----
