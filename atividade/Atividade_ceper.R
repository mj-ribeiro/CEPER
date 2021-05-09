
setwd("D:/Git projects/CEPER/atividade")


dados = read.csv('dados.txt')


head(dados)
str(dados)

# A


mom = function(x){
  Máximo = max(x)
  Mínimo = min(x)
  Média = mean(x)
  Mediana = median(x)
  Variância = var(x)
  Desvio = sd(x)
  rmom = data.frame(Máximo, Mínimo, Média, Mediana,
                    Variância, Desvio )
  
}



s = mom(dados[,2])

for (i in 2:length(dados)) {
  
  s[i-1,] =mom(dados[,i])
  
}

rownames(s) = colnames(dados[,-1])

s = round(s,2)
s

stargazer::stargazer(s, summary=F, 
                     out='stats.tex', decimal.mark = ',',
                     digits.extra=0, digits=2)

# B ----

cor(dados[,-1])

stargazer::stargazer(cor(dados[,-1]), summary = F,
                     out='cor.tex', decimal.mark = ',',
                     digits.extra=0, digits=2)

# C ----

setEPS()
postscript("bar.eps", width=8, height=4)
plot(dados$mpg, dados$hp, xlab = 'Milhas por galão',
     pch=19, cex=2,
     ylab='Potência bruta do motor',
     col='darkred', main='Mpg e Hp')
dev.off()



# D ----
  
reg = lm(dados$mpg ~ dados$hp)

summary(reg)

library('stargazer')

stargazer(reg, out='res.tex', float = F)

# E ----

setEPS()
postscript("f2.eps", width=8, height=4)
plot(dados$mpg, dados$wt, xlab = 'Milhas por galão', pch=19, cex=2,
     ylab='Peso do veículo', col='darkred', main='Mpg e Wt')
dev.off()



# F ----
setEPS()
postscript("box.eps", width=8, height=4)
boxplot(dados$mpg, col='darkred', main='Mpg')
dev.off()




cov(dados$disp, dados$carb)







