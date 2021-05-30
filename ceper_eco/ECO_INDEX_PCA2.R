
# Marcos Júnio Ribeiro

# CEPER


setwd("D:/Git projects/CEPER/ceper_eco")

var = readRDS('VAR_INDEX.rds')



source('call_f.R')



# correlação ----

year=2017
round(var(var[var$ano==year,4:9]), 3)


make_index = function(year){
      R = cor(var[var$ano==year,4:9])

      Rp = ppcor::pcor(var[var$ano==year,4:9])[1]$estimate
      KMO = sum(R^2)/sum(R^2 + Rp^2)
      cat('O valor do KMO é:', KMO, '\n')
      
      corrplot::corrplot(R, type="lower", order="hclust",
                         col=brewer.pal(n=5, name="RdYlBu"))
      #corrplot::corrplot.mixed(R, lower='number')
      
      ei = eigen(R)
      vet = ei$vectors[,1:4]
      vet = t(vet)
      
      
      
      val = ei$values[1:4]
      sq = sqrt(val)/sum(sqrt(val))
      w = colSums(vet*sq)
      w = w/(sum(w)) 
      w
}


make_index(2017)

v1 = c(1, 0.5, 0.8)
v2 = c(0.4, 1, 0.99)
v3 = c(0.1, 0.2, 1)
V = rbind(v1,v2,v3)

eigen(V)







