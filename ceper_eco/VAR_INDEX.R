
# Marcos Júnio Ribeiro

# CEPER


setwd("D:/Git projects/CEPER/ceper_eco")


source('call_f.R')


# seade ----

seade =  readxl::read_excel('DADOS_INDEX.xlsx')
cad = read_rds('cad.rds')
snis = read_rds('san.rds')



seade=seade%>%
  mutate(my_code = as.numeric(substr(code_muni, 1, 6)) )

seade = merge(seade, cad, by=c('ano', 'my_code'))


seade = merge(seade, snis, by=c('ano', 'code_muni'))




seade = seade %>%
  dplyr::select(-muni.san)%>%
  filter(ano>2008)%>%
  mutate_all( funs(ifelse(is.na(.), mean(., na.rm = TRUE),.)) ) %>%
  mutate(hom_phm =1e3*hom/pop,
         furt_phm = 1e3*furt/pop,
         fv_phm = 1e3*fv/pop,
         roub_phm = 1e3*roub/pop,
         pib_pc = log(pib_pc),
         c_res_elet = log(c_res_elet),
         tot_pes_cad = 1e3*tot_pes_cad/pop)


colnames(seade)

nomes = seade[,c('muni', "code_muni", 'ano')]

xx = caret::preProcess(seade[,5:(dim(seade)[2])] , 'range')
seade=predict(xx, as.data.frame(seade[,5:(dim(seade)[2])] ))


seade[,'ano'] = nomes$ano
seade[,'muni'] = nomes$muni

# seade[,'row'] = paste(seade$muni, seade$ano, sep='_')
# 
# seade = seade%>%
#         tibble::column_to_rownames('row')
# 



# crime -----

crime = seade %>%
  dplyr::select(c(ano, muni, hom_phm, fv_phm,  roub_phm) )%>%
  group_by(muni)%>%
  rowwise() %>%
  mutate(fv_phm = 1-fv_phm, hom_phm=1-hom_phm, roub_phm=1-roub_phm)%>%
  mutate(cr = mean(c(fv_phm, roub_phm, hom_phm), na.rm=T))



make_sub_index(2011, crime, 3, 5)



# educ ----


educ = seade %>%
  dplyr::select(c(ano, muni, dist_id_serie, ideb_ini, ideb_fin) )%>%
  group_by(muni) %>%
  mutate(dist_id_serie = 1-dist_id_serie)%>%
  rowwise() %>%
  mutate(educ = mean(c(dist_id_serie,  ideb_ini, ideb_fin), na.rm=T))

educ$ano = nomes$ano


make_sub_index(2017, educ,3, 5)




# longevidade


long = seade %>%
  dplyr::select(c(ano, muni, tx_m_15, tx_m_infantil, tx_m_peri))%>%
  group_by(muni) %>%
  mutate(tx_m_15=1-tx_m_15, tx_m_infantil=1-tx_m_infantil,
         tx_m_peri=1-tx_m_peri)%>%
  rowwise() %>%
  mutate(long = mean(c( tx_m_15, tx_m_infantil, tx_m_peri), na.rm=T))

long$ano = nomes$ano


make_sub_index(2019, long, 3, 5)




# saude ----

saude = seade %>%
  dplyr::select(c(ano, muni, l_sus_pmh, t_enf_phm, enf_phm, m_phm))%>%
  group_by(muni) %>%
  rowwise() %>%
  mutate(sau = mean(c(l_sus_pmh, t_enf_phm, enf_phm, m_phm), na.rm=T))


saude$ano = nomes$ano

make_sub_index(2019, saude,3, 6)






# riqueza -----


riq = seade %>%
  dplyr::select(c(ano, muni, rend_emp_form, c_res_elet, pib_pc, tot_pes_cad ))%>%
  group_by(muni) %>%
  rowwise() %>%
  mutate(riq = mean(c(c_res_elet, rend_emp_form, pib_pc, tot_pes_cad), na.rm=T))%>% 
  mutate(tot_pes_cad=1-tot_pes_cad)


make_scatter(2018, tot_pes_cad, pib_pc, riq)


make_sub_index(2015, riq, 3, 6)


  


# saneamento ----


san = seade %>%
  group_by(muni) %>%
  dplyr::select(c(muni, ano, IN046_AE, IN055_AE, IN015_AE))%>%
  group_by(muni) %>%
  rowwise() %>%
  mutate(san = mean(c(IN046_AE, IN055_AE, IN015_AE), na.rm=T))


san$ano = nomes$ano


make_sub_index(2015, san, 3, 5)



year=2017

DATA_SET = function(year){
  SAN = make_var(year, san,3, 5 )
  RIQ = make_var(year, riq, 3, 6)
  SAUDE = make_var(year, saude, 3, 6)
  LONG = make_var(year, long, 3, 5)
  EDUC = make_var(year, educ, 3, 5)
  CRIME = make_var(year, crime, 3, 5)
  
  DF = data.frame(SAN, RIQ, SAUDE, LONG, EDUC, CRIME)*1/6
  
  DF$index = rowSums(DF)
  
  DF$muni = nomes[nomes$ano==year, 'muni']
  DF$code_muni = nomes[nomes$ano==year, 'code_muni']
  DF$ano = nomes[nomes$ano==year, 'ano']
  
  DF = DF[order(DF$index, decreasing = T),]
  
  rownames(DF) = DF$muni

  DF$pos = seq(1, dim(DF)[1])
  return(DF)
}


s = DATA_SET(2017)
hist(s$index)
s[s$index>0.65,]

make_index()


make_scatter(2015, index, LONG, s[s$index>0.5,])



make_index = function(){
  R = cor(s[,1:6])
  id = colnames(R)
  
  corrplot::corrplot(R, type="lower", method = c('number'),
                     order="hclust",
                     col=viridis::inferno(5))
  
  ei = eigen(R)
  vet = ei$vectors[,1:3]
  vet = t(vet)
  
  val = ei$values[1:3]
  sq = sqrt(val)/sum(sqrt(val))
  w = colSums(vet*sq)
  w = matrix(w^1/(sum(w^1)), 1 )
  colnames(w) = id
  w
}



# get variables

nomes[,'crime'] =  crime$cr
nomes[,'educ']  =   educ$educ
nomes[,'long'] = long$long
nomes[,'sau'] = saude$sau
nomes[,'riq'] = riq$riq
nomes[,'san'] = san$san








saveRDS(nomes, 'VAR_INDEX.rds')










