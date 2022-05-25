##################### DECISION TREES ##################### 

# librerias ----
library(caret)
library(tidyverse)
library(rpart)
library(rpart.plot)
library(tidymodels)
library(doParallel)
library(doFuture)

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

# ajuste ----
entrenamiento <- entrenamiento %>% mutate(Class=as.factor(Class))
entrenamiento2 <- entrenamiento %>% mutate(Clase=as.factor(ifelse(Class==0,'X0','X1')))
entrenamiento2 <- entrenamiento2 %>% select(-Class)

# esto nos ayuda a correr en paralelo 
registerDoFuture()
cl <- makeCluster(10)
plan(cluster, workers = cl)


# iniciamos la validación cruzada
set.seed(20210508)
cortes_vc <- vfold_cv(entrenamiento2, v = 5)
# afinamos dos parámetros
arbol <- decision_tree(cost_complexity = tune(), 
                       min_n = tune()) %>% 
  set_engine("rpart") %>% 
  set_mode("classification") 

receta <- recipe(Clase ~ ., entrenamiento2) %>%
  step_normalize(all_predictors()) %>% 
  step_rm(number)


flujo <- workflow() %>% 
  add_recipe(receta) %>% 
  add_model(arbol) 

# validación cruzada
valores_grid <- expand_grid(cost_complexity = c(exp(seq(-8, -2, 0.25))),
                            min_n = c(1, 5, 10, 15))
evaluacion_vc <- tune_grid(flujo, 
                           resamples = cortes_vc,
                           grid = valores_grid)
metricas_vc <- collect_metrics(evaluacion_vc)
metricas_vc

# Y vemos los resultados
ggplot(metricas_vc %>% filter(.metric =="roc_auc"), 
       aes(x = cost_complexity, y = mean, 
           ymin = mean - std_err, ymax = mean + std_err, group = factor(min_n), 
           colour = factor(min_n))) +
  geom_linerange() +
  geom_line() +
  geom_point() +
  scale_x_log10() +
  ylab("AUC estimado vc-5") +
  theme_minimal() +
  labs(x='Cost complexity',colour='Min N')


#Y usamos la regla de mínimo error o a una desviación estándar del error mínimo:
mejor_arbol <- select_best(evaluacion_vc, 
                                     metric = "roc_auc", desc(cost_complexity))
mejor_arbol

consulta <- metricas_vc %>% filter(.metric=='roc_auc', min_n==10)

# Y ajustamos el modelo final y lo evaluamos:
arbol_podado_vc <- finalize_workflow(flujo, mejor_arbol) %>% 
  fit(entrenamiento2)

metricas_fraude <- metric_set(roc_auc, accuracy, sens, spec)

prueba <- prueba %>% mutate(Class=as.factor(Class))
prueba2 <- prueba %>% mutate(Clase=as.factor(ifelse(Class==0,'X0','X1')))
prueba2 <- prueba2 %>% select(-Class)

predict(arbol_podado_vc, prueba2, type = "prob") %>%
  bind_cols(predict(arbol_podado_vc, prueba2)) %>% 
  bind_cols(prueba2 %>% select(Clase)) %>% 
  metricas_fraude(Clase, .pred_X0, estimate = .pred_class) %>% filter(.metric=='roc_auc') %>% select(.estimate)
arbol_podado_vc


write.csv(metricas_vc,'vc_arboles_inicial.csv')
rpart.plot(arbol_podado_vc$trained)



# se repite el ejercicio pero variando la proporcion ----
# el nuevo set se hace de la misma manera que en KNN para que se usen los mismos datos
# nuevo set de entrenamiento ----
proporcion <- 0.01
fraudes <- entrenamiento %>% filter(Class==1)
nuevoEnt <- entrenamiento %>% filter(!number%in%fraudes$number)

set.seed(25072001)
nuevoEnt <- slice_sample(nuevoEnt, n=nrow(fraudes)/proporcion, replace=F)
nuevoEnt <- rbind(nuevoEnt, fraudes)  
nuevoEnt2 <- nuevoEnt %>% mutate(Clase=as.factor(ifelse(Class==0,'X0','X1')))
nuevoEnt2 <- nuevoEnt2 %>% select(-Class)


# se realiza el ajuste como se hizo anteriormente ----
registerDoFuture()
cl <- makeCluster(10)
plan(cluster, workers = cl)


# iniciamos la validación cruzada
set.seed(20210508)
cortes_vc_red <- vfold_cv(nuevoEnt2, v = 5)
# afinamos dos parámetros
arbol_red <- decision_tree(cost_complexity = tune(), 
                       min_n = tune()) %>% 
  set_engine("rpart") %>% 
  set_mode("classification") 

receta_red<- recipe(Clase ~ ., nuevoEnt2) %>%
  step_normalize(all_predictors()) %>% 
  step_rm(number)


flujo_red <- workflow() %>% 
  add_recipe(receta_red) %>% 
  add_model(arbol_red) 

# validación cruzada
valores_grid_red <- expand_grid(cost_complexity = c(exp(seq(-8, -2, 0.25))),
                            min_n = c(1, 5, 10, 15))
evaluacion_vc_red <- tune_grid(flujo_red, 
                           resamples = cortes_vc_red,
                           grid = valores_grid_red)
metricas_vc_red <- collect_metrics(evaluacion_vc_red)
metricas_vc_red

# Y vemos los resultados
ggplot(metricas_vc_red %>% filter(.metric =="roc_auc"), 
       aes(x = cost_complexity, y = mean, 
           ymin = mean - std_err, ymax = mean + std_err, group = factor(min_n), 
           colour = factor(min_n))) +
  geom_linerange() +
  geom_line() +
  geom_point() +
  scale_x_log10() +
  ylab("AUC estimado vc-5") +
  theme_minimal() +
  labs(x='Cost complexity',colour='Min N', title= 'Proporcion de 1%')


#Y usamos la regla de mínimo error o a una desviación estándar del error mínimo:
mejor_arbol_red <- select_best(evaluacion_vc_red, 
                           metric = "roc_auc", desc(cost_complexity))
mejor_arbol_red

consulta <- metricas_vc_red %>% filter(.metric=='roc_auc', min_n==10)

# Y ajustamos el modelo final y lo evaluamos:
arbol_podado_vc_red <- finalize_workflow(flujo_red, mejor_arbol_red) %>% 
  fit(nuevoEnt2)

metricas_fraude_red <- metric_set(roc_auc, accuracy, sens, spec)

prueba <- prueba %>% mutate(Class=as.factor(Class))
prueba2 <- prueba %>% mutate(Clase=as.factor(ifelse(Class==0,'X0','X1')))
prueba2 <- prueba2 %>% select(-Class)

a<-predict(arbol_podado_vc_red, prueba2, type = "prob") %>%
  bind_cols(predict(arbol_podado_vc_red, prueba2)) %>% 
  bind_cols(prueba2 %>% select(Clase)) %>% 
  metricas_fraude(Clase, .pred_X0, estimate = .pred_class) %>% filter(.metric=='roc_auc') %>% select(.estimate)
arbol_podado_vc_red
