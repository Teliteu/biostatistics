#1.1
library(Amelia)

dados <- dados[, -ncol(dados)] 
summary(dados)

amelia_output <- amelia(dados, m = 5, ts = "Data Medicao")

imputed_data <- amelia_output$imputations[[1]]

summary(imputed_data)



#1.2
library(vegan)

dados_pca <- imputed_data[, !names(imputed_data) %in% "Data Medicao"]

pca_cov <- rda(dados_pca, scale = FALSE)  

pca_cor <- rda(dados_pca, scale = TRUE)  
autovalores_cov <- pca_cov$CA$eig
autovetores_cov <- pca_cov$CA$u
proporcao_cov <- pca_cov$CA$eig / sum(pca_cov$CA$eig)

autovalores_cor <- pca_cor$CA$eig
autovetores_cor <- pca_cor$CA$u
proporcao_cor <- pca_cor$CA$eig / sum(pca_cor$CA$eig)

resultados_cov <- data.frame(Autovalores = autovalores_cov, Proporcao = proporcao_cov)
resultados_cor <- data.frame(Autovalores = autovalores_cor, Proporcao = proporcao_cor)

print("Resultados PCA - Matriz de Covariância:")
print(resultados_cov)
print("Resultados PCA - Matriz de Correlação:")
print(resultados_cor)


#1.3

library(vegan)
pca_result <- rda(dados_pca, scale = TRUE)

library(vegan)
screeplot(pca_result, type = "lines", bstick = TRUE)
summary(pca_result)

#1.4


