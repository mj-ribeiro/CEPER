# Marcos Júnio Ribeiro


setwd("D:/Git projects/CEPER/ceper_eco")

source('call_f.R')


# maps ----

mybreaks = c(seq(0.4, 0.70, 0.05), 0.755)

mylabel = c('0,40 a 0,45',
            '0,45 a 0,50',
            '0,50 a 0,55',
            '0,55 a 0,60',
            '0,60 a 0,65',
            '0,65 a 0,70',
            '0,70 a 0,75')

mycolors = inferno(length(mylabel), alpha = 1,
                   begin = 0.15, end = 1, direction = 1)


# map 2017 ----

mp17 = tibble(i17)%>%
  dplyr::select(c(muni, code_muni, index))

mp17 = merge(mp17, sp, by='code_muni')

muni =mp17%>%
  dplyr::select(c(muni, index, geom))

mp17 = st_as_sf(mp17)



g17 = maps_f(sh=mp17, x='index',
       leg='Índice CEPER', 
       type='.eps',
       fonte='cp17_',
       breaks = mybreaks,
       colors = mycolors,
       labels = mylabel
)


# map 2015 ----

mp15 = tibble(i15)%>%
  dplyr::select(c(muni, code_muni, index))

mp15 = merge(mp15, sp, by='code_muni')

mp15 = st_as_sf(mp15)

g15 = maps_f(sh=mp15, x='index',
             leg='Índice CEPER', 
             type='.eps',
             fonte='cp15_',
             breaks = mybreaks,
             colors = mycolors,
             labels = mylabel
)



# map 2013 ----

mp13 = tibble(i13)%>%
  dplyr::select(c(muni, code_muni, index))

mp13 = merge(mp13, sp, by='code_muni')

mp13 = st_as_sf(mp13)

g13 = maps_f(sh=mp13, x='index',
             leg='Índice CEPER', 
             type='.eps',
             fonte='cp13_',
             breaks = mybreaks,
             colors = mycolors,
             labels = mylabel
)




# mapa interativo ----

view_map(sh=mp17, x='index',
         leg='Índice CEPER', 
         fonte='cp_',
         breaks = mybreaks,
         colors = mycolors,
         labels = mylabel
)




tmap_mode('view')


# Boxplot ----


g13 = i13[c(1:6)]
g15 = i15[c(1:6)]
g17 = i17[c(1:6)]


colnames(g13) = c('SAN', 'REN', 'SAU', 'LON','EDU', 'CRI')
colnames(g15) = c('SAN', 'REN', 'SAU', 'LON','EDU', 'CRI')
colnames(g17) = c('SAN', 'REN', 'SAU', 'LON','EDU', 'CRI')




options(OutDec= ".")         # colocar o separador decimal sendo vírgula



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





# ifdm x ceper ----


g1 = df2 %>%
  ggplot(aes(IFDM, index)) +
  geom_point(col='darkred', size=2) +
  geom_smooth(method='lm', se=F , aes(colour="Linha de Regressão")) +
  scale_colour_manual(name="legend", values=c("blue")) +
  scale_y_continuous(labels=function(x) format(x, big.mark = ".",
                                               decimal.mark = ",", 
                                               scientific = FALSE)) +
  scale_x_continuous(labels=function(x) format(x, big.mark = ".",
                                               decimal.mark = ",", 
                                               scientific = FALSE)) +
  theme(panel.background = element_rect(fill = "linen"),
        legend.background = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 10),
        legend.text = element_text(size = 10),
        legend.title = element_blank(),
        strip.text.x = element_blank(),
        strip.background = element_rect(colour="white", fill="white"),
        legend.position=c(0.25,0.95),
        axis.text.x = element_text(size=10), 
        axis.text.y = element_text(size=10), 
        axis.title.x = element_text(colour = 'black', size=11),
        axis.title.y = element_text(colour = 'black', size=11) ) +
  xlab('IFDM') + 
  ylab('Índice CEPER') +
  ggtitle('(A)')

g1


cor(df2[is.na(df2$IFDM)==F,'IFDM'], 
    df2[is.na(df2$IFDM)==F,'index'])




# iprs x ceper


g2 = df2 %>%
  ggplot(aes(iprs, index)) +
  geom_point(col='darkred', size=2) +
  geom_smooth(method='lm', se=F , aes(colour="Linha de Regressão")) +
  scale_colour_manual(name="legend", values=c("blue")) +
  scale_y_continuous(labels=function(x) format(x, big.mark = ".",
                                               decimal.mark = ",", 
                                               scientific = FALSE)) +
  scale_x_continuous(labels=function(x) format(x, big.mark = ".",
                                               decimal.mark = ",", 
                                               scientific = FALSE)) +
  theme(panel.background = element_rect(fill = "linen"),
        legend.background = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 10),
        legend.text = element_text(size = 10),
        legend.title = element_blank(),
        strip.text.x = element_blank(),
        strip.background = element_rect(colour="white", fill="white"),
        legend.position=c(0.25,0.95),
        axis.text.x = element_text(size=10), 
        axis.text.y = element_text(size=10), 
        axis.title.x = element_text(colour = 'black', size=12),
        axis.title.y = element_text(colour = 'black', size=12) ) +
  xlab('IPRS') + 
  ylab('Índice CEPER') +
  ggtitle('(B)') 

g2

cor(df2$iprs, df2$index)


G = ggarrange(g1, g2, nrow=1, ncol=2)
G

ggsave(G, file="comp.eps", device="eps", height=4, width=8)



# Histograma ----

par(mfrow=c(1,3))

hist(i13$index, col='darkred', probability=T, xlab='Índice CEPER - 2013',
     ylab='Frequência', main='', breaks=20, ylim=c(0,15))
curve(dnorm(x, 0.59, 0.027), col='red', add=T)


hist(i15$index, col='lightblue',probability=T, xlab='Índice CEPER - 2015',
     ylab='Frequência', main='', breaks=20, ylim=c(0,15))
curve(dnorm(x, 0.59, 0.027), col='red', add=T)


hist(i17$index, col='lightgreen', probability=T,  xlab='Índice CEPER - 2017',
     ylab='Frequência', main='', breaks=20, ylim=c(0,15))

curve(dnorm(x, 0.61, 0.027), col='red', add=T)

















# Bubble chart ----

col=viridis::inferno(7, alpha=0.5, begin=0.0, end = 0.9)

b3 = head(i17[order(i17$pop, decreasing = T), c('RIQ', 'muni', 'pop', 'CRIME', 'index') ], 30 )

b3$i_levels= cut(b3$index, breaks=seq(0.45, 0.75, 0.05))
col = c('darkred', 'red','orange', 'lightgreen' , 'yellow')

b3 %>%
  ggplot(aes(CRIME, RIQ)) +
  geom_point(aes(color=i_levels, size= log(pop) ), alpha=0.4 ) +
  geom_text(aes(CRIME, RIQ, label = muni)) +
  scale_y_continuous(labels=function(x) format(x, big.mark = ".",
                                               decimal.mark = ",", 
                                               scientific = FALSE)) +
  scale_x_continuous(labels=function(x) format(x, big.mark = ".",
                                               decimal.mark = ",", 
                                               scientific = FALSE)) +
  theme(panel.background = element_rect(fill = "linen"),
        #legend.background = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 10),
        legend.text = element_text(size = 10),
        #legend.title = element_blank(),
        strip.text.x = element_blank(),
        strip.background = element_rect(colour="white", fill="white"),
        #legend.position=c(0.25,0.95),
        axis.text.x = element_text(size=10), 
        axis.text.y = element_text(size=10), 
        axis.title.x = element_text(colour = 'black', size=12),
        axis.title.y = element_text(colour = 'black', size=12) )+
  guides(colour=guide_legend(override.aes=list(alpha=0.5, size=8))) +
  scale_size(range = c(0.1, 20), name="Log da População") +
  scale_color_manual(values = col, name='Índice CEPER') +
  ylab('Riqueza') +
  xlab("Crimes contra o patrimônio") 
  
  





