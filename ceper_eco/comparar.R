# Marcos Júnio Ribeiro


setwd("D:/Git projects/CEPER/ceper_eco")

source('call_f.R')




iprs = read_excel('muni.xlsx', sheet = 'data_18')
iprs$iprs = rowSums(iprs[,4:6])*1/300


firjan = read_rds('FIRJAN.rds')

i17 = read_rds('s_17.rds')


# geocode ----


sp = read_municipality(code_muni = 'SP')
sp = sp[order(sp$name_muni), ]


# merges ----

df2 = merge(i17, firjan, by='code_muni')

df2 = merge(df2, iprs, by='code_muni')





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
        legend.text = element_text(size = 6),
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
        legend.text = element_text(size = 6),
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

ggsave(G, file="comp.eps", device="eps", height=4, width=6)



# histograma ----


stats(df2$index)

g3 =  df2 %>%
  ggplot(aes(index)) +
  geom_histogram(fill='darkred', col='white',binwidth = 0.05,
                 boundary = 0, closed = "left") +
  scale_y_continuous(labels=function(x) format(x, big.mark = ".",
                                               decimal.mark = ",", 
                                               scientific = FALSE)) +
  scale_x_continuous(labels=function(x) format(x, big.mark = ".",
                                               decimal.mark = ",", 
                                               scientific = FALSE),
                     breaks = seq(0.45, 0.75, 0.05), 
                     #limits = c(0.47, 0.73)
                     ) +
  theme(panel.background = element_rect(fill = "linen"),
        legend.background = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 10),
        legend.text = element_text(size = 6),
        legend.title = element_blank(),
        strip.text.x = element_blank(),
        strip.background = element_rect(colour="white", fill="white"),
        legend.position=c(0.25,0.95),
        axis.text.x = element_text(size=10), 
        axis.text.y = element_text(size=10), 
        axis.title.x = element_text(colour = 'black', size=11),
        axis.title.y = element_text(colour = 'black', size=11) ) +
  xlab('Índice CEPER') +
  ylab('Frequência')


g3 





# maps ----

mp = tibble(df2)%>%
      dplyr::select(c(muni, code_muni, index))


mp = merge(mp, sp, by='code_muni')


muni =mp%>%
  select(c(muni, index, geom))

colnames(mp)


mp = st_as_sf(mp)




mybreaks = seq(0.45, 0.75, 0.05)

mylabel = c('0,45 a 0,50',
            '0,50 a 0,55',
            '0,55 a 0,60',
            '0,60 a 0,65',
            '0,65 a 0,70',
            '0,70 a 0,75')


mycolors = inferno(length(mylabel), alpha = 1,
                   begin = 0.15, end = 1, direction = 1)


summary(mp$index)




maps_f(sh=mp, x='index',
       leg='Índice CEPER', 
       type='.eps',
       fonte='cp_',
       breaks = mybreaks,
       colors = mycolors,
       labels = mylabel
)



# mapa interativo ----

view_map(sh=mp, x='index',
       leg='Índice CEPER', 
       fonte='cp_',
       breaks = mybreaks,
       colors = mycolors,
       labels = mylabel
)




tmap_mode('view')



# comparar ceper com anos anteriores ----


i13 = read_rds('s_13.rds')
i15 = read_rds('s_15.rds')


i15['Ribeirão Preto', ]


s1 =stats(i13$index)
s2 = stats(i15$index)
s3 = stats(i17$index)


BIND = cbind(s1, s2, s3)

colnames(BIND) = c('Índice CEPER 2013', 
                   'Índice CEPER 2015', 
                   'Índice CEPER 2017')

sn = c("nobs", "NAs", "Sum", "SE Mean", "LCL Mean", "UCL Mean", 'Variance')


BIND = BIND[!(rownames(BIND)%in%sn), ]

nn = c('Mínimo', 'Máximo', '1 Quartil', '3 Quartil', 'Média',
  'Mediana', 'Desvio Padrão', 'Assimetria', 'Curtose')

rownames(BIND) = nn


# Tabela 1  -----

stargazer::stargazer(BIND,summary = F, out = 'ceper.tex',
                     decimal.mark = ',',
                     digits.extra=0, digits=4,
                     rownames = T
)


BIND



# melhores e piores ----

i13 = i13%>%
  dplyr::select(c('muni', 'index'))%>%
  relocate(muni)
rownames(i13)=NULL
colnames(i13) = c('Município', 'Índice')



i15 = i15%>%
  dplyr::select(c('muni', 'index'))%>%
  relocate(muni)
rownames(i15)=NULL
colnames(i15) = c('Município', 'Índice')


i17 = i17%>%
  dplyr::select(c('muni', 'index', 'pos'))%>%
  relocate(muni)
rownames(i17)=NULL
colnames(i17) = c('Município', 'Índice', 'Ranking')


  
T13 = rbind(head(i13, 10), tail(i13, 10) )

T15 = rbind(head(i15, 10), tail(i15, 10) )

T17 = rbind(head(i17, 10), tail(i17, 10) )


rank = cbind(T13, T15, T17)


# Tabela 2  -----

stargazer::stargazer(rank,summary = F, out = 'rank.tex',
                     decimal.mark = ',',
                     digits.extra=0, digits=4,
                     rownames = F
)



















