#1
library(readxl)
caminho <- ""
dados <- read_excel(caminho)
as.numeric(dados$Ano)

x <- dados$Ano

h <- hist(x, breaks=15, col="orange", xlab="Ano", main="Histograma com Curva de Densidade")

xfit <- seq(min(x), max(x), length=100)
yfit <- dnorm(xfit, mean=mean(x), sd=sd(x))
yfit <- yfit * diff(h$mids[1:2]) * length(x)

lines(xfit, yfit, col="blue", lwd=2)

#1.2
boxplot(dados$Ano, 
        ylab = "Precipitação anual (mm)", 
        las = 1,
        col = "orange",
        frame.plot = FALSE)

#1.3 
library(moments)
skewness(dados$Ano)
kurtosis(dados$Ano)
qqnorm(dados$Ano, 
       bty = "n", 
       las = 1, 
       col = "orange")
qqline(dados$Ano, 
       col = "blue")

#1.4
Median(dados$Ano)
Mode(dados$Ano)
Mean(dados$Ano)

#1.5
teste <- dados$Ano[dados$`Nome da Estação`== "Maceió"]
# Maceió é 146
1- pnorm((dados$Ano[146] - mean(dados$Ano)) / sd(dados$Ano))

#1.6
a <- (dados$Ano[146] - mean(dados$Ano)) / sd(dados$Ano)
curve(dnorm(x), 
      -4, 
      4, 
      bty = "n", 
      las = 1, 
      ylab = "Probabilidade", 
      xlab = "Z")
abline(v = a, lty = 3)
coord.x <- c(a, seq(a, 4, 0.01), 4)
coord.y <- c(0, dnorm(seq(a, 4, 0.01)), 0)
polygon(coord.x, 
        coord.y, 
        col = "orange")


#2.2


#2.3
med_N <- 0.93
dv_N <- 0.66

cv_N <- (dv_N / med_N) * 100
cv_d15N

med_C <- 1.26
dv_C <- 0.65

cv_C <- (dv_C / med_C) * 100
cv_d13C







