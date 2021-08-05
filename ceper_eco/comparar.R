# Marcos Júnio Ribeiro


setwd("D:/Git projects/CEPER/ceper_eco")


source('call_f.R')

i17 = read_rds('n_17.rds')
i13 = read_rds('n_13.rds')
i15 = read_rds('n_15.rds')




# comparar ceper com anos anteriores ----

s1 =stats(i13$index)
s2 = stats(i15$index)
s3 = stats(i17$index)


BIND = cbind(s1, s2, s3)

colnames(BIND) = c('Índice CEPER 2013', 
                   'Índice CEPER 2015', 
                   'Índice CEPER 2017')

BIND


# Tabela 1: estatísticas  -----

stargazer::stargazer(BIND,summary = F, out = 'estat_desc.tex',
                     decimal.mark = ',',
                     digits.extra=0, digits=4,
                     rownames = T
)



# porcetagem de municípios nos grupos ----

prop.table(table(i17$class) )

prop.table(table(i15$class))

prop.table(table(i13$class))





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


# Tabela 2: Rank  -----

stargazer::stargazer(rank,summary = F, out = 'rank.tex',
                     decimal.mark = ',',
                     digits.extra=0, digits=4,
                     rownames = F
)





# Tabela 3: evolução -----

i17 = read_rds('n_17.rds')
i13 = read_rds('n_13.rds')
i15 = read_rds('n_15.rds')



c13 = i13%>%
  dplyr::select(c(code_muni, index, pos))

colnames(c13)[2:3] = c("index_13", "pos_13")



c17 = i17%>%
  dplyr::select(c(code_muni, muni, index, pos))
colnames(c17)[3:4] = c("index_17", "pos_17")


C = merge(c13, c17, by="code_muni")



C = C %>%
  mutate(comp = index_17 - index_13)%>%
  relocate(where(is.character)) %>%
  dplyr::select(-c(code_muni))


colnames(C) = c("Município", "Índice 2013", "Ranking 2013", "Índice 2017", "Ranking 2017", "Diferença")


C %>%
  summarise(s=mean(Diferença))


P1 = head(C[order(C$Diferença, decreasing = T), ], 10 )
P2 = tail(C[order(C$Diferença, decreasing = T), ], 10 )


CC = rbind(P1, P2)

rownames(CC) = NULL

stargazer::stargazer(CC,summary = F, out = 'comp.tex',
                     decimal.mark = ',',
                     digits.extra=0, digits=4,
                     rownames = F
)

mean(CC$Diferença)


##### Ranking  dos subíndices ----



posit = c(9, seq(14, 20))


# 2017 -----


nn = i17 %>%
  mutate(
    cri_r = dense_rank( desc(CRIME) ), 
    san_r = dense_rank( desc(SAN) ),
    riq_r = dense_rank( desc(RIQ) ),
    sau_r = dense_rank(desc(SAUDE)),
    lon_r = dense_rank( desc(LONG) ),
    educ_r = dense_rank(desc(EDUC)),
    index_r = dense_rank(desc(index))
  ) %>%
  dplyr::select(posit)


colnames(nn) = c('Município', 'CRI', 'SAN', 'RIQ', 'SAU', 'LON', 'EDU', 'Índice' )


r17 = rbind(head(nn, 10), tail(nn, 10))


# Tabela 1: Rank - 17 -----

stargazer::stargazer(r17,summary = F, out = 'r17.tex',
                     decimal.mark = ',',
                     digits.extra=0, digits=4,
                     rownames = F
)


# 2015 ----


nn2 = i15 %>%
  mutate(
    cri_r = dense_rank( desc(CRIME) ), 
    san_r = dense_rank( desc(SAN) ),
    riq_r = dense_rank( desc(RIQ) ),
    sau_r = dense_rank(desc(SAUDE)),
    lon_r = dense_rank( desc(LONG) ),
    educ_r = dense_rank(desc(EDUC)),
    index_r = dense_rank(desc(index))
  ) %>%
  dplyr::select(posit)

colnames(nn2) = c('Município', 'CRI', 'SAN', 'RIQ', 'SAU', 'LON', 'EDU', 'Índice' )

r15 = rbind(head(nn2, 10), tail(nn2, 10))



stargazer::stargazer(r15,summary = F, out = 'r15.tex',
                     decimal.mark = ',',
                     digits.extra=0, digits=4,
                     rownames = F
)




# Tabela 2: Rank - 13 -----



# 2013 ----


nn3 = i13 %>%
  mutate(
    cri_r = dense_rank( desc(CRIME) ), 
    san_r = dense_rank( desc(SAN) ),
    riq_r = dense_rank( desc(RIQ) ),
    sau_r = dense_rank(desc(SAUDE)),
    lon_r = dense_rank( desc(LONG) ),
    educ_r = dense_rank(desc(EDUC)),
    index_r = dense_rank(desc(index))
  ) %>%
  dplyr::select(posit)

colnames(nn3) = c('Município', 'CRI', 'SAN', 'RIQ', 'SAU', 'LON', 'EDU', 'Índice' )

r13 = rbind(head(nn3, 10), tail(nn3, 10))



# Tabela 3: Rank - 13 -----

stargazer::stargazer(r13,summary = F, out = 'r13.tex',
                     decimal.mark = ',',
                     digits.extra=0, digits=4,
                     rownames = F
)



# salvar ranking dos subíndices
#saveRDS(nn3, 'r13.rds')
#saveRDS(nn2, 'r15.rds')
#saveRDS(nn, 'r17.rds')


# estat desc subíndice ----


stats(i13[1:6])

basicStats(i13[1:6])

# subíndice 2013

stargazer::stargazer(stats(i13[1:6]),summary = F, out = 'subestat_13.tex',
                     decimal.mark = ',',
                     digits.extra=0, digits=4,
                     rownames = T
)


# subíndice 2015

stargazer::stargazer(stats(i15[1:6]),summary = F, out = 'subestat_15.tex',
                     decimal.mark = ',',
                     digits.extra=0, digits=4,
                     rownames = T
)



# subíndice 2017

stargazer::stargazer(stats(i17[1:6]),summary = F, out = 'subestat_17.tex',
                     decimal.mark = ',',
                     digits.extra=0, digits=4,
                     rownames = T
)





cor(i17$pop, i17$RIQ)


nn[order(nn$riq_r, decreasing = F),]





# correlação 2017

nomes =  c('Saneamento', 'Renda', 'Saúde', 'Longevidade',
                  'Educação', 'Crime', 'Índice', "class", "muni",
                  "code_muni",
                  "ano",  "pop","pos")

colnames(i17) = nomes
colnames(i15) = nomes
colnames(i13) = nomes



stargazer::stargazer(cor(i17[1:7] ),summary = F, out = 'cor17.tex',
                     decimal.mark = ',',
                     digits.extra=0, digits=4,
                     rownames = T)


# correlação 2015

stargazer::stargazer(cor(i15[1:7] ),summary = F, out = 'cor15.tex',
                     decimal.mark = ',',
                     digits.extra=0, digits=4,
                     rownames = T)


# correlação 2013

stargazer::stargazer(cor(i13[1:7] ),summary = F, out = 'cor13.tex',
                     decimal.mark = ',',
                     digits.extra=0, digits=4,
                     rownames = T)




log(0.623/0.5987)






# end



