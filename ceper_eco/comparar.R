# Marcos Júnio Ribeiro


source('call_f.R')


setwd("D:/Git projects/CEPER/ceper_eco")



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





# ifdm x ceper


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
        plot.title = element_text(hjust = 0.5, size = 10),
        legend.text = element_text(size = 8),
        legend.title = element_blank(),
        strip.text.x = element_blank(),
        strip.background = element_rect(colour="white", fill="white"),
        legend.position=c(0.12,0.95),
        axis.text.x = element_text(size=10), 
        axis.text.y = element_text(size=10), 
        axis.title.x = element_text(colour = 'black', size=11),
        axis.title.y = element_text(colour = 'black', size=11) ) +
  xlab('IFDM') + 
  ylab('Índice CEPER') +
  ggtitle('(A)')




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
        plot.title = element_text(hjust = 0.5, size = 10),
        legend.text = element_text(size = 8),
        legend.title = element_blank(),
        strip.text.x = element_blank(),
        strip.background = element_rect(colour="white", fill="white"),
        legend.position=c(0.12,0.95),
        axis.text.x = element_text(size=10), 
        axis.text.y = element_text(size=10), 
        axis.title.x = element_text(colour = 'black', size=12),
        axis.title.y = element_text(colour = 'black', size=12) ) +
  xlab('IPRS') + 
  ylab('Índice CEPER') +
  ggtitle('(B)')



cor(df2$iprs, df2$index)





library("ggpubr")

G = ggarrange(g1, g2, nrow=1, ncol=2)
G

ggsave(G, file="comp.eps", device="eps", height=4, width=6)



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


view_map(sh=mp, x='index',
       leg='Índice CEPER', 
       fonte='cp_',
       breaks = mybreaks,
       colors = mycolors,
       labels = mylabel
)




tmap_mode('view')


saveRDS(mp, 'mp.rds')

