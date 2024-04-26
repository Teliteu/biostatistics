#1.1

#hipotese
(obs <- c(1, 6, 13, 5, 2, 3)) 
(spp.repr <- 0:5) 
(prob <- dbinom(spp.repr, 5, 0.75))
(esp <- 30 * prob)

(data <- matrix(c(obs, esp),
                ncol = 6,
                byrow = T))

colnames(data) <- 0:5
rownames(data) <- c("obs", "esp")
data

#grafico obsXesp
barplot(data,
        col = c("orange", "darkcyan"),
        beside = T,
        width = 2,
        xlab = "Sobrevivência de ninhegos de F. coelebs",
        ylab = "Ninhos",
        las = 1)
legend(0.5, 12,
       legend = c("Observado", "Esperado"), 
       fill = c("orange", "darkcyan"), 
       bty = "n",
       title = "Número de Ninhos", 
       title.adj = 0.5)

#quiqua

(residuos <- obs - esp)
(residuos2 <- residuos ^ 2)
(residuos.padrao <- residuos2 / esp)
(qui.quadrado <- sum(residuos.padrao))
1 - pchisq(qui.quadrado, 5)


chisq.test(obs,
           p = prob,
           simulate.p.value = T,
           B = 1000)

#curva quiqua
curve(dchisq(x, 5),
      xlim = c(0, 20),
      bty = "n",
      las = 1,
      xlab = "X2",
      ylab = "P")
crit <- qchisq(0.95, 5)
abline(v = crit,
       lty = 3,
       col = "red")
coord.x <- c(crit,
             seq(crit, 20, 0.01), 20)
coord.y <- c(0,
             dchisq(seq(crit, 20, 0.01), 5), 0)
polygon(coord.x,
        coord.y,
        col = "red")

#graf res

library(ggplot2)
data2 <- data.frame(0:5, teste$residuals)
names(data2) <- c("spp", "resid")
data2

  graf.final <- 
    ggplot(data2, aes(x = spp, y = resid)) +
    geom_bar(stat = "identity", position = "identity",
             fill = ifelse(data2$resid > 0, "blue", "red")) +
    geom_text(aes(x = spp,
                  y = resid + 0.3 * sign(resid),
                  label = format(resid, digits = 2),
                  hjust = ifelse(resid > 0, 0, 1)), 
              size = 3,
              color = rgb(100,100,100, maxColorValue = 255))  +
    scale_x_continuous(breaks = 0:5,
                       limits = c(-0.5, 5.5)) +
    scale_y_continuous(breaks = seq(-4,12,2),
                       limits = c(-5, 15)) +
    labs(x = "Sobrevivência de ninhegos de F. coelebs",
         y = "Resíduos de X2",
         title = "Diferença dos indivíduos estudados nas Ilhas Canárias") +
    theme(axis.text.x =
            element_text(size  = 10,
                         angle = 0,
                         hjust = 1,
                         vjust = 1),
          axis.text.y = element_text(hjust = 0.5),
          panel.background = element_blank(),
          panel.grid.minor = element_blank(),
          axis.ticks  = element_blank(),
          axis.line   = element_line(colour=NA),
          axis.line.x = element_line(colour="grey80")) +
    coord_flip()
  graf.final

#2.4

  # Função de densidade de probabilidade
  curve(dt(x, 4),
        xlim = c(-4, 4),
        bty = "n",
        las = 1,
        ylab = "P")
  
 