#1
library(MuMIn)

data <- na.omit(data)

data$rugosity <- sqrt(data$rugosity)
data$LC <- sqrt(data$LC)
data$HC <- sqrt(data$HC)
data$macro <- sqrt(data$macro)

modelo <- lm(sqrt(Herbivore.abundance) ~ complexity + rugosity + LC + HC + macro + SCORE1 + SCORE2, data = data, na.action = na.fail)

model_set <- dredge(modelo)

model_avg <- model.avg(model_set, subset = delta < 4)

tabela <- sw(model_avg)
tabela

par(mfrow = c(2, 2))
plot(modelo)

library(car)
vif(modelo) 

#2

cor.test(data$Annual_temperature, data$RN_total_effort)

interaction_model <- lm(RN_total_effort ~ Annual_temperature * Species, data = data)
anova(interaction_model)

ancova_model <- lm(RN_total_effort ~ Annual_temperature + Species, data = data)
anova(ancova_model)

par(mfrow = c(2, 2))
plot(ancova_model)

model_amazilia <- lm(RN_total_effort ~ Annual_temperature, data = data, subset = Species == "Amazilia_versicolor")
model_antilophia <- lm(RN_total_effort ~ Annual_temperature, data = data, subset = Species == "Antilophia_galeata")

plot(data$Annual_temperature, data$RN_total_effort, col = ifelse(data$Species == "Amazilia_versicolor", "blue", "red"),
     xlab = "Temperatura Anual (°C)", ylab = "Esforço Total de Amostragem (RN_total_effort)",
     main = "Esforço de Amostragem por Temperatura Anual entre Espécies")

abline(model_amazilia, col = "blue", lwd = 2)
abline(model_antilophia, col = "red", lwd = 2)


