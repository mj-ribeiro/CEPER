
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



# C ----


plot(dados$mpg, dados$hp, xlab = 'MPG', pch=19, cex=2,
     ylab='HP', col='blue', main='Mpg e Hp')




# D ----
  
reg = lm(dados$mpg ~ dados$hp)

summary(reg)

library('stargazer')

stargazer(reg, out='res.tex', float = F)

# E ----


plot(dados$mpg, dados$wt, xlab = 'MPG', pch=19, cex=2,
     ylab='WT', col='blue', main='MPG e WT')




# F ----

boxplot(dados$mpg)


