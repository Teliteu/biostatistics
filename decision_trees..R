#1.1
library(rpart)
library(rpart.plot)

modelo_arvore <- rpart(region ~ ., data = dados, method = "class")

rpart.plot(modelo_arvore, type = 4, extra = 106)  

printcp(modelo_arvore)

summary(modelo_arvore)

#1.2

library(randomForest)
library(caret)
library(ggplot2)

dados$region <- as.factor(dados$region)  

indices <- createDataPartition(dados$region, p = 0.8, list = FALSE)
dados_treino <- dados[indices, ]
dados_teste <- dados[-indices, ]

preproc <- preProcess(dados_treino, method = c("center", "scale"))
dados_treino_norm <- predict(preproc, dados_treino)
dados_teste_norm <- predict(preproc, dados_teste)

modelo_rf <- randomForest(region ~ ., data = dados_treino_norm, ntree = 1000, importance = TRUE)
modelo_rf

importancia <- importance(modelo_rf)
importancia

importancia_df <- data.frame(Variable = rownames(importancia), Gini = importancia[, "MeanDecreaseGini"])  # Substituir "MeanDecreaseGini" pelo nome correto, se diferente

ggplot(importancia_df, aes(x = reorder(Variable, Gini), y = Gini)) +
  geom_col(fill = "dodgerblue") +
  labs(title = "Importância dos Preditores (Índice de Gini)", x = "Preditor", y = "Importância") +
  coord_flip() +
  theme_minimal()

#2

library(caret)
library(randomForest)

train_control <- trainControl(
  method = "repeatedcv",
  number = 10,
  repeats = 100,
  savePredictions = "final",
  classProbs = TRUE,
  summaryFunction = twoClassSummary
)

modelo_rf_cv <- train(
  region ~ .,
  data = dados,
  method = "rf",
  trControl = train_control,
  metric = "ROC",
  tuneLength = 3,
  ntree = 1000,
  importance = TRUE
)

accuracy_rf <- max(modelo_rf_cv$results$ROC)
print(paste("Acurácia RF:", accuracy_rf))

importance_rf <- varImp(modelo_rf_cv, scale = FALSE)
print(importance_rf)

modelo_glm_cv <- train(
  region ~ .,
  data = dados,
  method = "glm",
  family = "binomial",
  trControl = train_control,
  metric = "ROC"
)

accuracy_glm <- max(modelo_glm_cv$results$ROC)
print(paste("Acurácia GLM:", accuracy_glm))


