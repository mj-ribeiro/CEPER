
setwd("D:/Git projects/CEPER/ceper_eco")


#options(OutDec= ".")         # colocar o separador decimal sendo vírgula

source('call_f.R')


seade <- readRDS('seade2.rds')

# crime -----

crime = seade %>%
  dplyr::select(c(ano, muni, furt, fv_phm,  roub_phm) )%>%
  group_by(muni)%>%
  rowwise() %>%
  mutate(fv_phm = 1-fv_phm, furt=1-furt, roub_phm=1-roub_phm)%>%
  mutate(cr = mean(c(fv_phm, roub_phm, furt), na.rm=T))


make_sub_index(2015, crime, 3, 5)




# educ ----


educ = seade %>%
  dplyr::select(c(ano, muni, dist_id_serie, ideb_ini, ideb_fin) )%>%
  group_by(muni) %>%
  mutate(dist_id_serie = 1-dist_id_serie)%>%
  rowwise() %>%
  mutate(educ = mean(c(dist_id_serie,  ideb_ini, ideb_fin), na.rm=T))



make_sub_index(2015, educ,3, 5)




# longevidade


long = seade %>%
  dplyr::select(c(ano, muni, tx_m_15, tx_m_infantil, tx_m_peri))%>%
  group_by(muni) %>%
  mutate(tx_m_15=1-tx_m_15, tx_m_infantil=1-tx_m_infantil,
         tx_m_peri=1-tx_m_peri)%>%
  rowwise() %>%
  mutate(long = mean(c( tx_m_15, tx_m_infantil, tx_m_peri), na.rm=T))



stats(long[long$ano==2015, 'tx_m_infantil'] )


make_sub_index(2015, long, 3, 5)




# saude ----

saude = seade %>%
  dplyr::select(c(ano, muni, l_sus_pmh, t_enf_phm, enf_phm, m_phm))%>%
  group_by(muni) %>%
  rowwise() %>%
  mutate(sau = mean(c(l_sus_pmh, t_enf_phm, enf_phm, m_phm), na.rm=T))


make_sub_index(2015, saude,4, 6)


# riqueza -----


riq = seade %>%
  dplyr::select(c(ano, muni, rend_emp_form, c_res_elet, pib_pc, tot_pes_cad ))%>%
  group_by(muni) %>%
  rowwise() %>%
  mutate(riq = mean(c(c_res_elet, rend_emp_form, pib_pc, tot_pes_cad), na.rm=T))%>% 
  mutate(tot_pes_cad=1-tot_pes_cad)



stats(riq[riq$ano==2015, 'pib_pc'] )


make_scatter(2017, tot_pes_cad, pib_pc, riq)


make_sub_index(2015, riq, 3, 6)



# saneamento ----


san = seade %>%
  group_by(muni) %>%
  dplyr::select(c(muni, ano, IN055_AE, IN046_AE, IN015_AE, IN016_AE))
  

make_sub_index(2015, san, 3, 5)



# Build index 

DATA_SET = function(year){
  SAN = make_var(year, san,3, 5 )
  RIQ = make_var(year, riq, 3, 6)
  SAUDE = make_var(year, saude, 4, 6)
  LONG = make_var(year, long, 3, 5)
  EDUC = make_var(year, educ, 3, 5)
  CRIME = make_var(year, crime, 3, 5)
  
  DF = data.frame(SAN, RIQ, SAUDE, LONG, EDUC, CRIME)
  
  DF[,'index'] = rowSums(DF)*1/6
  DF[,'class'] = d(DF$index)
  
  DF[,'muni'] = nomes[nomes$ano==year, 'muni']
  DF[,'code_muni'] = nomes[nomes$ano==year, 'code_muni']
  DF[,'ano'] = nomes[nomes$ano==year, 'ano']
  DF[, 'pop'] = nomes[nomes$ano==year, 'pop']
  
  DF = DF[order(DF$index, decreasing = T),]
  
  rownames(DF) = NULL
  rownames(DF) = t(DF[,'muni'])
  
  DF$pos = seq(1, dim(DF)[1])
  return(DF)
}


s_17 = DATA_SET(2017)
s_15 = DATA_SET(2015)
s_13 = DATA_SET(2013)






saveRDS(s_17, 'n_17.rds')
saveRDS(s_15, 'n_15.rds')
saveRDS(s_13, 'n_13.rds')








# end
