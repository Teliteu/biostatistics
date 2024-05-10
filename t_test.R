#1.1
boxplot(area ~ uf, 
        data = data, 
        col = rainbow(length(unique(data$uf))), 
        main = "Boxplot da área desmatada por UF", 
        xlab = "UF", 
        ylab = "Área",
        las = 1)

#1.2
shapiro.test(as.numeric(unlist(data[data$year == "2009", "area"])))
shapiro.test(as.numeric(unlist(data[data$year == "2019", "area"])))

var.test(as.numeric(unlist(data[data$year == "2009", "area"])), as.numeric(unlist(data[data$year == "2019", "area"])))

wilcox.test(as.numeric(unlist(data[data$year == "2009", "area"])), as.numeric(unlist(data[data$year == "2019", "area"])),
            paired = T,
            alternative = "greater")

#1.3
shapiro.test(as.numeric(unlist(data[data$uf == "GO", "area"])))
shapiro.test(as.numeric(unlist(data[data$uf == "MG", "area"])))

var.test(as.numeric(unlist(data[data$uf == "GO", "area"])), as.numeric(unlist(data[data$uf == "MG", "area"])))

wilcox.test(as.numeric(unlist(data[data$uf == "GO", "area"])), as.numeric(unlist(data[data$uf == "MG", "area"])), exact=FALSE)

#2
tabela <- data.frame(Tipo = c(rep("Agricola", length(data2$Agricola)), rep("Floresta", length(data2$Floresta))),
                     Valor = c(data2$Agricola, data2$Floresta))
t <- t.test(data2$Agricola, data2$Floresta)
t
tabela$Tipo <- factor(tabela$Tipo)
#mil rep
ts <- replicate(1000,
                t.test(tabela$Valor ~ sample(tabela$Tipo, 120))$statistic)
coin::oneway_test(tabela$Valor ~ tabela$Tipo,
                  distribution = coin::approximate(nresample = 1000))

pts <- seq(-4, 4, length = 100)
plot(pts,
     dt(pts, df = 18),
     col = "red",
     type = "l",
     las = 1,
     bty = "n",
     main = "",
     xlab = "t",
     ylab = "Densidade")
lines(density(ts), col = "blue")
abline(v = obs,
       lty = 3,
       col = "blue")






