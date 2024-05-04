#1.1
library(corrplot)
library(car)

dados$region <- as.numeric(factor(dados$region))

cor_matrix <- cor(dados[, -1]) 

corrplot(cor_matrix, method = "circle")

vif_results <- vif(lm(density ~ diameter + height + basal_area + biomass + region, data = dados))
vif_results

high_vif <- vif_results[vif_results > 10]
high_vif

#1.2
library(stats)

dados$region <- factor(dados$region, levels = c("AC", "TR"))
contrasts(dados$region) <- c(0, 1)
dados$region_numeric <- as.numeric(dados$region) - 1

glm_model <- glm(region_numeric ~ density + diameter + height + basal_area + biomass,
                 family = binomial(link = "logit"),
                 data = dados)
glm_model

par(mfrow = c(2, 2))
plot(glm_model)

#1.3

library(MuMIn)

full_model <- glm(region_numeric ~ density + diameter + height + basal_area + biomass,
                  family = binomial(link = "logit"),
                  data = dados)

options(na.action = "na.fail")
dados.dredge <- MuMIn::dredge(full_model)

dados.dredge

#1.4
library(visreg)
best_pred_model <- glm(region_numeric ~ height, family = binomial(link = "logit"), data = dados)
best_pred_model
visreg(best_pred_model, "height", type="conditional")


#2
library(caret)
dados$region_numeric <- factor(dados$region_numeric, levels = c(0, 1), labels = c("Class0", "Class1"))

train_control <- trainControl(
  method = "repeatedcv",
  number = 10,
  repeats = 100,
  savePredictions = "final",
  classProbs = TRUE, 
  summaryFunction = twoClassSummary)

model <- train(region_numeric ~ density + diameter + height + basal_area + biomass,
               data = dados,
               method = "glm",
               family = "binomial",
               trControl = train_control,
               metric = "Kappa")
model

var_importance <- varImp(model, scale = TRUE)
var_importance

results <- data.frame(Predictor = character(), Accuracy = numeric(), Kappa = numeric(), stringsAsFactors = FALSE)

for (i in seq_along(vars)) {
  formula <- as.formula(paste(formula_base, paste(vars[1:i], collapse = " + ")))
  model <- train(formula, data = dados, method = "glm", family = "binomial",
                 trControl = train_control, metric = "Kappa")
  
  acc <- mean(model$results$Accuracy, na.rm = TRUE)
  kappa <- mean(model$results$Kappa, na.rm = TRUE)
  
  results <- rbind(results, data.frame(Predictor = paste(vars[1:i], collapse = " + "),
                                       Accuracy = acc, Kappa = kappa))
}

results


