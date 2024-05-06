#1.1 
roedores$ponto <- as.factor(roedores$ponto)

blocos <- aov(captura ~ ponto + isca, data = roedores)
summary(blocos)

boxplot(captura ~ isca, data = roedores,
        main = "Distribuição de Capturas por Tipo de Isca",
        xlab = "Tipo de Isca",
        ylab = "Número de Capturas",
        col = viridis::viridis_pal()(4))

boxplot(captura ~ ponto, data = roedores,
        main = "Distribuição de Capturas por Pontos",
        xlab = "Pontos",
        ylab = "Número de Capturas",
        col = viridis::viridis_pal()(10))

#1.2
shapiro.test(residuals(blocos))
fligner.test(captura ~ isca, data = roedores)

par(mfrow=c(2,2))
plot(aov(captura ~ isca, data = roedores))

#2
modelo_anova <- aov(Seed ~ Type * Bag_treatm, data = pollination)

summary(modelo_anova)

residuos <- residuals(modelo_anova)

shapiro_test <- shapiro.test(residuos)
shapiro_test
levene_test <- leveneTest(residuos, pollination$Bag_treatm)
levene_test

pollination$Type <- as.factor(pollination$Type)
pollination$Bag_treatm <- as.factor(pollination$Bag_treatm)

resultado_anova_robusta <- t2way(Seed ~ Type * Bag_treatm, data = pollination)

resultado_anova_robusta

par(mfrow = c(2, 2))
plot(aov(Seed ~ Type * Bag_treatm, data = pollination))

#3
f_cri=qf(0.95, 4, 12)
f_cri
f=78.77
curve(df(x, 4, 12),
      xlim = c(0, 80),
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
coord.x <- c(f_cri, seq(f_cri, 80, 0.01), 80)
coord.y <- c(0, df(seq(f_cri, 80, 0.01), 4, 12), 0)
polygon(coord.x, coord.y, col = "red")













