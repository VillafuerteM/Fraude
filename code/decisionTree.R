##################### DECISION TREES ##################### 

# librerias ----
library(caret)
library(tidyverse)

# ajuste ----
entrenamiento <- entrenamiento %>% mutate(Class=as.factor(Class))
entrenamiento2 <- entrenamiento %>% mutate(Clase=as.factor(ifelse(Class==0,'X0','X1')))
entrenamiento2 <- entrenamiento2 %>% select(-Class)

# train control
train_control = trainControl(method = "repeatedcv", number = 10, repeats=5, search = "grid", classProbs=T, summaryFunction=twoClassSummary)

## Customsing the tuning grid (ridge regression has alpha = 0)
classification_Tree_Grid =  expand.grid(maxdepth = c(1,3,5,7,9))

set.seed(50)

# training a Regression model while tuning parameters (Method = "rpart")
model = train(Clase~., data = entrenamiento2, method = "rpart2",metric='ROC', trControl = train_control, tuneGrid = classification_Tree_Grid)

orig_fit <- train(Class ~ .,
                  data = imbal_train,
                  method = "gbm",
                  verbose = FALSE,
                  metric = "ROC",
                  trControl = ctrl)

# summarising the results
print(model)
