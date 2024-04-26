  #1.1
  pe <- 0.45 #probabilidade espécie
  ps <- 0.6 #probabilidade sexo
  pf <- pe*ps #probabilidade final
  t <- 20 # tentativa
  ts <- 0:t #tentativas
  a <- 7 #amostra
  resultado <- dbinom(ts, t, pf)
  resultado [a+1]
  barplot(resultado,
          names.arg=ts,
          col = ifelse(ts == a, "red", "orange"),
          xlab="Borboletas fêmeas da espécie Hamadryas feronia",
          ylab="Probabilidade",
          ylim = c(0, 0.20),
          las=1,
          bty="n")
#1.2
  pe <- 0.45 #probabilidade espécie
  ps <- 0.6 #probabilidade sexo
  pf <- pe*ps #probabilidade final
  t <- 20 # tentativa
  ts <- 0:t #tentativas
  a <- 7 #amostra
  resultado <- dbinom(0:a+1, t, pf)
  sum(resultado [0:a+1])
  barplot(dbinom(ts, t, pf),
          names.arg=ts,
          col = ifelse(ts <= a, "red", "orange"),
          xlab="Borboletas fêmeas da espécie Hamadryas feronia",
          ylab="Probabilidade",
          ylim = c(0, 0.20),
          las=1,
          bty="n")
  
#2
  #5
  par(mfrow = c(2, 3))
  pe <- 0.45 #probabilidade espécie
  ps <- 0.6 #probabilidade sexo
  pf <- pe*ps #probabilidade final
  t <- 20 # tentativa
  media <- t*pf
  variancia <- t*pf*(1-pf)
  desvio <- sqrt(variancia)
  media
  desvio
  
  hist(rbinom(5, t, pf),
       xlab="5 amostras - Fêmeas Hamadryas feronia",
       ylab="Frequência", 
       las=1,
       bty="n",
       main=NULL)
  abline(v=media,
         col="red",
         lwd=2)
  abline(v=c(media - desvio, media + desvio),
         col="red",
         lty=3,
         lwd=2)
  
  #10
  
  hist(rbinom(10, t, pf),
       xlab="10 amostras - Fêmeas Hamadryas feronia",
       ylab="Frequência", 
       las=1,
       bty="n",
       main=NULL)
  abline(v=media,
         col="red",
         lwd=2)
  abline(v=c(media - desvio, media + desvio),
         col="red",
         lty=3,
         lwd=2)
  
  #20
  
  hist(rbinom(20, t, pf),
       xlab="20 amostras - Fêmeas Hamadryas feronia",
       ylab="Frequência", 
       las=1,
       bty="n",
       main=NULL)
  abline(v=media,
         col="red",
         lwd=2)
  abline(v=c(media - desvio, media + desvio),
         col="red",
         lty=3,
         lwd=2)
  
  #50
  
  hist(rbinom(50, t, pf),
       xlab="50 amostras - Fêmeas Hamadryas feronia",
       ylab="Frequência", 
       las=1,
       bty="n",
       main=NULL)
  abline(v=media,
         col="red",
         lwd=2)
  abline(v=c(media - desvio, media + desvio),
         col="red",
         lty=3,
         lwd=2)
  
  #1000000
  
  hist(rbinom(1000000, t, pf),
       xlab="1000000 amostras - Fêmeas Hamadryas feronia",
       ylab="Frequência", 
       las=1,
       bty="n",
       main=NULL)
  abline(v=media,
         col="red",
         lwd=2)
  abline(v=c(media - desvio, media + desvio),
         col="red",
         lty=3,
         lwd=2)
  
#3
  
  

  
  
  
