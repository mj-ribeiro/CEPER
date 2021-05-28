#############################################################################################
#                          Estudo dados socioêconomicos
#############################################################################################

# Marcos Júnio Ribeiro



setwd("D:/Git projects/CEPER/ceper_eco")


library(sf)
library(geobr)
library(ggplot2)
library(readxl)
library(magrittr)
library(raster)
library(tmap)
library(reshape2)
library(writexl)
library(viridis)




# geocode ----


sp = read_municipality(code_muni = 'SP')

sp = sp[order(sp$name_muni), ]



# Dados Alesp ----

df = read_excel('muni.xlsx', sheet = 'data_18')


# PCA nos dados ----

x = df[,c('riqueza', 'longevidade', 'escolaridade')]

pca = prcomp(x)

pca = princomp(x, cor = T, scores = TRUE)

summary(pca)


df$pca = pca$scores[,1]

cr = cor(df[,4:7])


pesos = (cr[4, 1:3])^2/sum((cr[4, 1:3])^2)


df$ind =  pesos[1]*df$riqueza + pesos[2]*df$longevidade + pesos[3]*df$escolaridade


pre = caret::preProcess(df[,'ind'], 'range')
df[,'ind']= predict(pre, df[,'ind'] )


df = df%>%
  select(-pca)


df = df[order(df$ind, decreasing = T), ]

df[,'pos'] = seq(1, dim(df)[1])



# dotchart 1 ----


par(family = "mono", font=2)  # mudar a fonte

options(OutDec= ".")         # colocar o separador decimal sendo vírgula


melhor = df[order(df$ind, decreasing = T), ][1:10,]
melhor = data.frame(melhor[,-1])
colnames(melhor) = c('Município', 'Grupo', 'Riqueza',
                     'Logevidade', 'Escolaridade', 'IND', 'Posição')

melhor






comp0 = max( nchar(melhor$name_muni) ) - nchar(melhor$name_muni) 



for (i in 1:length(comp0)) {
  melhor[i, 'concat'] = paste(melhor$name_muni[i], '-', sep = strrep('-',comp0[i] ) )
}


melhor['concat2'] = paste(melhor$concat, melhor$grupo, sep= '')


jpeg("dot1.jpg", width = 600,height = 400)

par(family = "mono", font=2)  # mudar a fonte

dotchart(melhor$pca, 
         melhor$concat2, 
         pch=19, 
         pt.cex = 2,
         xlab = 'PCA')
title('Municípios e Grupos do IPRS', outer = T, adj=0.05, line=-2.5)
dev.off()






# dotchart 2  ----


n = dim(df)[1] 

pior = df[order(df$ind, decreasing = T), ][(n-9):n,]

pior = data.frame(pior[,-1])
pior

colnames(pior) = c('Município', 'Grupo', 'Riqueza',
                     'Logevidade', 'Escolaridade', 'IND', 'Posição')

pior = pior[order(pior$PCA,decreasing = T),]

pior


row.names(melhor)= melhor$Município
row.names(pior) = pior$Município





BIND = rbind(melhor, pior, make.row.names=T)


# Tabela 2 -----

stargazer::stargazer(BIND,summary = F, out = 'iprs_tab.tex',
                     decimal.mark = ',',
                     digits.extra=0, digits=2,
                     rownames = F
)



comp = max( nchar(pior$name_muni) ) - nchar(pior$name_muni) 



for (i in 1:length(comp)) {
  pior[i, 'concat'] = paste(pior$name_muni[i], '-', sep = strrep('-',comp[i] ) )
}


pior['concat2'] = paste(pior$concat, pior$grupo, sep= '')



jpeg("dot2.jpg", width = 600,height = 400)
par(family = "mono", font=2)  # mudar a fonte
dotchart(pior$pca, 
         pior$concat2, 
         pch=21,
         #color = 'darkred',
         bg = 'darkred',
         pt.cex = 2,
         xlab = 'PCA')
title('Municípios e Grupos do IPRS', outer = T, adj=0.05, line=-2.5)
dev.off()



pior



# Merge data ----

df2 = merge(df, sp, by.x='code_muni', by.y='code_muni')
df2 = st_as_sf(df2)


# ver os municípios cujos nomes estão dando conflito
df2[df2$name_muni.x != df2$name_muni.y, c('name_muni.x','name_muni.y')]




# mapas ----




maps_f = function(sh, x, leg, fonte, breaks=NULL, labels=NULL, colors){
  g1 =  tm_shape(sh) +
    tm_polygons(x,  
                title=leg,  
                textNA = 'Sem dados',
                breaks = breaks,
                label = labels,
                palette=colors
               ) +
    tm_compass(type = "8star",
               position = c("right", "bottom", size = 0.0)) +
    tm_scale_bar(text.size = 0.6) +
    tm_layout(
      legend.text.size = 0.8,
      frame = T,
      legend.format = list(text.separator = "-"))
  tmap_save(g1, filename = paste(fonte,leg,".eps", sep = ""), width = 6,
            height = 4, units = 'in')
}



# Longevidade ----



mybreaks2 = c(seq(50, 90, 10), Inf)

mylabel2 = c('50 a 60',
             '60 a 70',
             '70 a 80',
             '80 a 90',
             'Mais de 90')



mycolors2 = inferno(length(mylabel2), alpha = 1, begin = 0.15, end = 1, direction = 1)



maps_f(sh=df2, x='longevidade',
       leg='Longevidade', 
       fonte='A_',
       breaks = mybreaks2,
       labels = mylabel2,
       colors = mycolors2)

head(df2)

table(df$grupo)


# grupos ----


maps_f(sh=df2, x='grupo',
       leg='Grupos', 
       fonte='A_',
       colors = mycolors2)


# Barplot ----

g =  prop.table(table(df2$grupo))

texto = paste(round(as.numeric(g)*100, 2), "%", sep='')

setEPS()
postscript("bar.eps", width=8, height=4)
barplot(prop.table(table(df2$grupo)), 
        col='darkred',
        ylab = 'Porcentagem de municípios',
        xlab = 'Grupos',
        ylim = c( 0, 0.4),
        width = 1,
        cex.lab=1.2,
        #names.arg = texto
        )
text(x=bb, y=as.numeric(g)+0.02, labels=texto, cex=1.2)
dev.off()
















# Riqueza ----

mybreaks = c(seq(10, 60, 10), Inf)

mylabel = c(
            '10 a 20',
            '20 a 30',
            '30 a 40',
            '40 a 50',
            '50 a 60',
            'Mais de 60')


mycolors = inferno(length(mylabel), alpha = 1, begin = 0.15, end = 1, direction = 1)


maps_f(sh=df2, x='riqueza',
       leg='Riqueza', 
       fonte='A_',
       breaks = mybreaks,
       labels = mylabel,
       colors = mycolors)





# Escolaridade ----




mybreaks3 = c(seq(30, 70, 10), Inf)

mylabel3 = c('30 a 40',
            '40 a 50',
            '50 a 60',
            '60 a 70',
            'Mais de 70')


mycolors3 = inferno(length(mylabel3), alpha = 1, begin = 0.15, end = 1, direction = 1)


maps_f(sh=df2, x='escolaridade',
       leg='Escolaridade', 
       fonte='A_',
       breaks = mybreaks3,
       labels = mylabel3,
       colors = mycolors3)






