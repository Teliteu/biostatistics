#1
shapiro_rain <- shapiro.test(data$rain_mean_annual)
shapiro_temp <- shapiro.test(data$temp_mean_annual)
shapiro_rain
shapiro_temp

plot(data$temp_mean_annual, data$rain_mean_annual,
     cex = 3, 
     pch = 21, 
     lwd = 0.9, 
     col = "red", 
     bg = rgb(1, 0, 0, 0.3),
     bty = "n",
     las = 1)
abline(h = mean(data$rain_mean_annual),
       v = mean(data$temp_mean_annual),
       lty = 3)

correlacao_pearson <- cor(data$temp_mean_annual, data$rain_mean_annual, method = "pearson")
correlacao_pearson

correlacao_spearman <- cor(data$temp_mean_annual, data$rain_mean_annual, method = "spearman")
correlacao_spearman

#1.2

par(mfrow = c(1, 2))

plot(data$temp_mean_annual, data$rain_mean_annual,
     main = "Pearson",
     xlab = "Temperatura Média Anual (°C)",
     ylab = "Precipitação Média Anual (mm)",
     pch = 19, col = "blue")
abline(lm(data$rain_mean_annual ~ data$temp_mean_annual), col = "red")

plot(data$temp_mean_annual, data$rain_mean_annual,
     main = "Spearman",
     xlab = "Temperatura Média Anual (°C)",
     ylab = "Precipitação Média Anual (mm)",
     pch = 19, col = "blue")
lines(lowess(data$temp_mean_annual, data$rain_mean_annual), col = "green")

#2
species_list <- c("Vespadelus_regulus", "Vespadelus_vulturnus", 
                  "Vespadelus_pumilus", "Vespadelus_darlingtoni", "Vespertilio_murinus")

data_vespertilionidae <- bats[bats$Species %in% species_list, ]

modelo <- lm(Log_Range_area ~ Log_Relative_wing_loading, data = data_vespertilionidae)

summary(modelo)

shapiro_test <- shapiro.test(residuals(modelo))
print(shapiro_test)

library(lmtest)
bp_test <- bptest(modelo)
print(bp_test)

par(mfrow = c(2, 2))
plot(modelo)

plot(data_vespertilionidae$Log_Relative_wing_loading, data_vespertilionidae$Log_Range_area,
     main = "Log Range Area vs. Log Relative Wing Loading",
     xlab = "Log Relative Wing Loading",
     ylab = "Log Range Area",
     pch = 20)
abline(modelo, col = "red") 


