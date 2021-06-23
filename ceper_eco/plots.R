# Marcos Júnio Ribeiro


setwd("D:/Git projects/CEPER/ceper_eco")

source('call_f.R')



g13 = i13[c(1:6)]
g15 = i15[c(1:6)]
g17 = i17[c(1:6)]


colnames(g13) = c('SAN', 'RIQ', 'SAU', 'LON','EDU', 'CRI')
colnames(g15) = c('SAN', 'RIQ', 'SAU', 'LON','EDU', 'CRI')
colnames(g17) = c('SAN', 'RIQ', 'SAU', 'LON','EDU', 'CRI')




options(OutDec= ",")         # colocar o separador decimal sendo vírgula



windows()

setEPS()
postscript("boxp.eps", width = 12,height = 6)
#jpeg("boxp.jpg", width = 1000,height = 400, quality=100)
par(mfrow=c(1,3))
boxplot(g13[1:6], col='darkred',    pch=19, main='2013', cex=1.1, cex.axis=1.5, cex.main=1.5, yaxt='n')
axis(side = 2, at=seq(0,1,0.1), cex.axis=1.5)
grid()
boxplot(g15[1:6], col='lightgreen', pch=19, main='2015', cex=1.1, cex.axis=1.5, cex.main=1.5, yaxt='n')
axis(side = 2, at=seq(0,1,0.1), cex.axis=1.5)
grid()
boxplot(g17[1:6], col='lightblue',  pch=19, main='2017', cex=1.1, cex.axis=1.5, cex.main=1.5, yaxt='n')
axis(side = 2, at=seq(0,1,0.1), cex.axis=1.5)
grid()
dev.off()

