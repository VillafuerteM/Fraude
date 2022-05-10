##################### KNN ##################### 

#librerias ----
library(tidyverse)
library(tidymodels)

# lectura de datos (mismos pasos que en el EDA) ----
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

# Primer intento con k vecinos ----
resultados <- data.frame(KNN=integer(),AUC=double())

ajusta_KNN <- function(KNN,entrenamiento,validacion,resultados){
  entrenamiento <- entrenamiento %>% mutate(Class=as.factor(Class))
  mod_knn <- nearest_neighbor(neighbors = KNN, weight_func = "rectangular") %>% 
    set_engine("kknn") %>% 
    set_mode("classification")
  receta_vmc <- recipe(Class ~ ., entrenamiento) %>% 
    step_normalize(all_predictors()) %>% 
    step_rm(number)%>% 
    prep()
  flujo <- workflow() %>% 
    add_recipe(receta_vmc) 
  ajuste_1 <- flujo %>% add_model(mod_knn) %>% fit(entrenamiento)
  
  predict_validacion <- ajuste_1 %>% 
    predict(validacion, type = "prob") %>% 
    bind_cols(validacion) %>% 
    select(number, Class, .pred_0, .pred_1)
  
  prueba_ROC <- predict_validacion%>%select(.pred_1, Class)
  colnames(prueba_ROC)<-c("predictions","labels")
  
  pred <- prediction(prueba_ROC$predictions, prueba_ROC$labels)
  
  auc_ROCR <- performance(pred, measure = "auc")
  auc_ROCR <- auc_ROCR@y.values[[1]]
  renglon <- data.frame(KNN, auc_ROCR)
  colnames(renglon)<-c("KNN", "AUC")
  
  resultados<-rbind(resultados, renglon)
}

for(i in 16:20){
  resultados<-ajusta_KNN(KNN=i,entrenamiento, validacion, resultados)
  print(i)
}


ggplot(predict_validacion, aes(x=Class, y=.pred_1))+
  geom_jitter()

prueba_ROC <- predict_validacion%>%select(.pred_1, Class)
colnames(prueba_ROC)<-c("predictions","labels")

pred <- prediction(prueba_ROC$predictions, prueba_ROC$labels)
perf <- performance(pred,"tpr","fpr")
plot(perf,colorize=TRUE)

auc_ROCR <- performance(pred, measure = "auc")
auc_ROCR <- auc_ROCR@y.values[[1]]

write.csv(resultados,"resultados_knn.csv")
