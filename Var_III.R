#1
boxplot(ab ~ trat, data = hipo,
        main = "Abundância da Macrofauna por Tratamento",
        xlab = "Tratamento",
        ylab = "Abundância de Macrofauna",
        col = c("lightblue", "lightgreen"),
        border = "black")

boxplot(ab ~ site, data = hipo,
        main = "Abundância da Macrofauna por local",
        xlab = "Local",
        ylab = "Abundância de Macrofauna",
        col = c("lightblue", "lightgreen"),
        border = "black")

fit_hipo <- lmerTest::lmer(ab ~ trat + (1 | site),
                               data=hipo,
                               REML = T)
fit_hipo
anova(fit_hipo)

lmerTest::rand(fit_hipo)

residuos <- resid(fit_hipo)
predicoes <- fitted(fit_hipo)
plot(residuos ~ predicoes,
     las = 1,
     bty = "n",
     xlab = "Fitted",
     ylab = "Residuals")

qqnorm(residuos, 
       bty = "n", 
       las = 1, 
       col = "brown")
qqline(residuos, 
       col = "blue")

shapiro.test(residuos)

posthoc <- multcomp::glht(fit_hipo,
                          linfct = multcomp::mcp(trat = "Tukey"))
mcs <- summary(posthoc,
               test = multcomp::adjusted("single-step"))
mcs

#1.2

hipo$site <- as.factor(hipo$site)
hipo$trat <- as.factor(hipo$trat)


vca_model <- VCA::anovaVCA(ab ~ trat/site, Data = hipo)
print(vca_model)

VCA::varPlot(ab ~ trat/site, Data = hipo)

#2

model_aov <- aov(Settlement ~ Treatment * Week + Error(Replicate), data = data_clean)
summary(model_aov)

fit.data <- lmerTest::lmer(Settlement ~ Treatment * Week + (1 | Replicate),
                            data_clean,
                            REML = T)
summary(fit.data)

anova(fit.data)

lmerTest::difflsmeans(fit.data,
                      which = c("Treatment",
                                "Week",
                                "Treatment:Week"))

with(data_clean, 
     interaction.plot(Week, Treatment, Settlement,
                      ylim = c(0, 2), 
                      lwd = 2,
                      ylab = "Média da Colonização", 
                      xlab = "Semana", 
                      trace.label = "Tratamento",
                      las = 1,
                      xtick = T,
                      bty = "n"))


residuos <- resid(fit.data)
predicoes <- fitted(fit.data)
plot(residuos ~ predicoes,
     las = 1,
     bty = "n",
     xlab = "Fitted",
     ylab = "Residuals")

shapiro.test(residuos)

qqnorm(residuos, 
       bty = "n", 
       las = 1, 
       col = "brown")
qqline(residuos, 
       col = "blue")







