#1.1
library(car)
library(stats)

especies <- dados$species

pca_modelo <- prcomp(resposta, center = TRUE, scale. = TRUE)

summary(pca_modelo)

pca_resposta <- pca_modelo$x[, 1:10]

manova_modelo <- manova(pca_resposta ~ especies)

summary(manova_modelo)

summary(manova_modelo, test = "Wilks")
summary(manova_modelo, test = "Pillai")
summary(manova_modelo, test = "Hotelling-Lawley")
summary(manova_modelo, test = "Roy")

#1.2
library(MVN)
library(biotools)


resposta <- dados[, grep("coords_", colnames(dados))]

especies <- dados$species

dados_completos <- cbind(especies, resposta)

resultado_mvn <- mvn(data = dados_completos[, -1], mvnTest = "mardia", multivariatePlot = "qq")

resultado_mvn

resultado_box_m <- boxM(resposta, especies)

resultado_box_m

#1.3
library(MASS)
library(caret)
library(boot)
resposta <- dados[, grep("coords_", colnames(dados))]
especies <- dados$species

bootstrap_lda <- function(data, indices) {
  bootstrap_data <- data[indices,]
  resposta_boot <- bootstrap_data[, -1]
  especies_boot <- bootstrap_data[, 1]
  
  lda_model <- lda(especies_boot ~ ., data = data.frame(especies_boot, resposta_boot))
  
  pred <- predict(lda_model, data.frame(resposta))
  
  mean(pred$class == especies)
}

dados_completos <- cbind(especies, resposta)

bootstrap_results <- boot(data = dados_completos, statistic = bootstrap_lda, R = 100)

mean_accuracy <- mean(bootstrap_results$t)
conf_interval <- boot.ci(bootstrap_results, type = "perc")

round(mean_accuracy, 3)
round(conf_interval$percent[4:5], 3)
