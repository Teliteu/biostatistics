#1.1
shapiro <- by(dados$Concentracao, dados$Classe, shapiro.test)
shapiro

bartlett <- bartlett.test(Concentracao ~ Classe, data = dados)
bartlett

anova <- aov(Concentracao ~ Classe, data = dados)
summary(anova)

boxplot(Concentracao ~ Classe, data = dados,
        col = RColorBrewer::brewer.pal(4, "Dark2"),
        xlab = "Uso da Terra",
        ylab = "Concentração de Carbono",
        las = 1)
#1.2
kruskal.test(Concentracao ~ Classe, data = dados)
kruskal

#2.1
library(ggplot2)
data(msleep)

boxplot(awake ~ vore, data = msleep,
        main = "Horas Acordado por Guilda",
        xlab = "Guilda",
        ylab = "Horas Acordado",
        las = 1,
        col = RColorBrewer::brewer.pal(4, "Dark2"))


shapiro <- by(msleep$awake, msleep$vore, shapiro.test)
shapiro

fligner <- fligner.test(awake ~ vore, data = msleep)
fligner

kruskal <- kruskal.test(awake ~ vore, data = msleep)
kruskal
#2.2
anova <- summary(aov(awake ~ vore, data = msleep))
anova

f <- anova[[1]]["F value"]
f <- f[1,1]
f

library(permuco)
permuco::aovperm(awake ~ vore, data = msleep,
                 np = 10000)

f_cri=qf(0.95, 3, 72)
f_cri

curve(df(x, 3, 72),
      xlim = c(0, 20),
      bty = "n",
      las = 1,
      ylab = "Probabilidade",
      xlab = "F")
legend(x = "topright", 
       legend = c("Valor F", "F crítico"),
       lty = c(3,1),
       lwd = c(1,1),
       bty = "n")
abline(v = f, lty=3)
abline(v= f_cri)
coord.x <- c(f, seq(f, 20, 0.01), 20)
coord.y <- c(0, df(seq(f, 20, 0.01), 3, 72), 0)
polygon(coord.x, coord.y, col = "red")

#3.3
f_cri=qf(0.95, 2, 27)
f_cri
f=10.17

curve(df(x, 2, 27),
      xlim = c(0, 20),
      bty = "n",
      las = 1,
      ylab = "Probabilidade",
      xlab = "F")
legend(x = "topright", 
       legend = c("Valor F", "F crítico"),
       lty = c(3,1),
       lwd = c(1,1),
       bty = "n")
abline(v = f, lty=3)
abline(v= f_cri)
coord.x <- c(f_cri, seq(f_cri, 20, 0.01), 20)
coord.y <- c(0, df(seq(f_cri, 20, 0.01), 2, 27), 0)
polygon(coord.x, coord.y, col = "red")

f_cri=qf(0.95, 2, 27)
f_cri
f=1.75

curve(df(x, 2, 27),
      xlim = c(0, 20),
      bty = "n",
      las = 1,
      ylab = "Probabilidade",
      xlab = "F")
legend(x = "topright", 
       legend = c("Valor F", "F crítico"),
       lty = c(3,1),
       lwd = c(1,1),
       bty = "n")
abline(v = f, lty=3)
abline(v= f_cri)
coord.x <- c(f_cri, seq(f_cri, 20, 0.01), 20)
coord.y <- c(0, df(seq(f_cri, 20, 0.01), 2, 27), 0)
polygon(coord.x, coord.y, col = "red")
















        
      

  
  
  
