#1.1
library(vegan)
library(ggplot2)
library(vegan3d)
library(rgl)

head(lidar)
head(field)

response <- lidar[, 2:8] 
predictors <- field[, 2:6] 

rda_model <- rda(response ~ ., data = predictors)

summary(rda_model)

plot(rda_model, scaling = 2)

coef(rda_model)

#1.2

library(vegan)
library(tidyverse)

cca_model <- cca(birds ~ ., data = lidar)

summary(cca_model)

plot(cca_model, scaling = 4)

