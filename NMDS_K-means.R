#1.1
library(vegan)
library(rgl)

region <- as.factor(dados$region)
dados_nmds <- dados[, -1]

dist_bray <- vegdist(dados_nmds, method = "bray")

nmds <- metaMDS(dist_bray, k = 2, trymax = 100)

nmds_scores <- scores(nmds)

cores <- c("blue", "red")
names(cores) <- levels(region)

plot(nmds_scores, type = "n")
points(nmds_scores, col = cores[region], pch = 19)
text(nmds_scores, labels = 1:nrow(nmds_scores), col = cores[region], pos = 3)

ordispider(nmds, groups = region, col = cores)
ordiellipse(nmds, groups = region, col = cores, kind = "sd", conf = 0.95)

legend("topright", legend = levels(region), col = cores, pch = 19)

#1.2
library(MuMIn)
library(vegan)

nmds_scores <- scores(nmds)
NMDS1 <- nmds_scores[, 1]

dados_nmds <- cbind(NMDS1, dados[, -1])  
model1 <- lm(NMDS1 ~ density, data = dados_nmds)
model2 <- lm(NMDS1 ~ diameter, data = dados_nmds)
model3 <- lm(NMDS1 ~ height, data = dados_nmds)
model4 <- lm(NMDS1 ~ basal_area, data = dados_nmds)
model5 <- lm(NMDS1 ~ biomass, data = dados_nmds)

model_set <- list(model1, model2, model3, model4, model5)

model_selection <- model.sel(model_set)
model_selection <- model_selection[model_selection$delta < 4, ]

model_selection

#1.3

library(cluster)
library(factoextra)
library(vegan)
library(rgl)

nmds_scores <- scores(nmds)
dados_nmds <- dados[, -1]  

gap_stat <- clusGap(dados_nmds, FUN = kmeans, nstart = 25, K.max = 10, B = 50)

fviz_gap_stat(gap_stat)

best_k <- maxSE(gap_stat$Tab[, "gap"], gap_stat$Tab[, "SE.sim"])
kmeans_result <- kmeans(dados_nmds, centers = best_k, nstart = 25)
clusters <- kmeans_result$cluster

cluster_colors <- rainbow(best_k)
names(cluster_colors) <- 1:best_k

plot(nmds_scores, type = "n")
points(nmds_scores, col = cluster_colors[clusters], pch = 19)
text(nmds_scores, labels = 1:nrow(nmds_scores), col = cluster_colors[clusters], pos = 3)

ordispider(nmds_scores, groups = clusters, col = cluster_colors)
ordiellipse(nmds_scores, groups = clusters, col = cluster_colors, kind = "sd", conf = 0.95)

legend("topright", legend = paste("Cluster", 1:best_k), col = cluster_colors, pch = 19)
