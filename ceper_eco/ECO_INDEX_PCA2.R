
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
      id = colnames(R)
      
      Rp = ppcor::pcor(var[var$ano==year,4:9])[1]$estimate
      KMO = sum(R^2)/sum(R^2 + Rp^2)
      cat('O valor do KMO é:', KMO, '\n')
      
      corrplot::corrplot(R, type="lower", method = c('number'),
                         order="hclust",
                         col=viridis::inferno(5))
      #corrplot::corrplot.mixed(R, lower='number')
      
      ei = eigen(R)
      vet = ei$vectors[,1:3]
      vet = t(vet)
      
      val = ei$values[1:3]
      sq = sqrt(val)/sum(sqrt(val))
      w = colSums(vet*sq)
      w = matrix(w^2/(sum(w^2)), 1 )
      colnames(w) = id
      w
}



year = 2017
w = make_index(year)
w

#w = matrix( rep(1/6, 6), ncol=6, nrow=1)
#w


d_17 = var[var$ano==year, ]

d_17$i17 = as.matrix(d_17[ ,4:9], 645, 1)%*%t(w) # 645 x 6     1 x 6

head(d_17)

hist(d_17$i17, col='lightgreen', breaks=20)


d_17 = d_17[order(d_17$i17, decreasing = T),]

rownames(d_17) = d_17$muni


d_17$pos = seq(1, dim(d_17)[1])




cor(d_17[,4:10] )
w



head(d_17,20)


d_17['Ribeirão Preto',]


