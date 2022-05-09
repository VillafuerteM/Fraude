##################### KNN ##################### 

#librerias ----
library(tidyverse)
library(tidymodels)

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
