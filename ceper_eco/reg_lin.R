# Marcos Júnio Ribeiro


setwd("D:/Git projects/CEPER/ceper_eco")


#source('call_f.R')

library(plm)
library(tidyverse)
library(glue)
library(MASS)


master <- readRDS("master.rds")
#seade <- readRDS('seade_test.rds')

# get vars to run regression ----

vars = colnames(master)
ind = c('pib_pc','tot_pes_cad', 'c_res_elet','rend_emp_form','ano', 'np', 'muni')
pos = which(vars%in%ind)
vars = vars[-pos]

# seade data ----

seade = readRDS('SEADE.rds')


# regression loop - A beautiful code ----


for (n in vars){
  for (i in seq(2013, 2017, 2)) {
    
    fit = step(lm( expr(!!rlang::sym(n) ~ pib_pc + 
                            tot_pes_cad +
                            c_res_elet +
                            rend_emp_form +
                            - 1), 
                     data = seade[seade$ano == i,]) );
    
    seade = seade %>%
      mutate( pred = case_when( 
          ano == i ~ predict(fit,.),
          ano != i ~ pred
        ) )%>%
      mutate("v.{i}":= 
               case_when(
                 ano == i ~ !!rlang::sym(n) - pred,
                 ano != i ~ pred
               )) 
  } 
  
  seade = seade%>%
    group_by(muni)%>%
    
    mutate(
      new = case_when(
        ano == 2013 ~ case_when(
          is.na(v.2013[ano == 2013])  ~  pred[ano == 2013] + ifelse( !is.na(v.2015[ano == 2015])|!is.na(v.2017[ano == 2017]),
                                                                ifelse(is.na(v.2015[ano == 2015]), v.2017[ano == 2017], v.2015[ano == 2015]), 0),
          !is.na(v.2013[ano == 2013]) ~  pred[ano == 2013] + v.2013[ano == 2013]),
        
        ano == 2015 ~ case_when(
          is.na(v.2015[ano == 2015])  ~  pred[ano == 2015] + ifelse(!is.na(v.2013[ano == 2013])|!is.na(v.2017[ano == 2017]),
                                                                ifelse(is.na(v.2017[ano == 2017]), v.2013[ano == 2013], v.2017[ano == 2017]), 0),
          !is.na(v.2015[ano == 2015]) ~  pred[ano == 2015] +  v.2015[ano == 2015]),
        
        ano == 2017 ~ case_when(
          is.na(v.2017[ano == 2017])  ~  pred[ano == 2017] +  ifelse(!is.na(v.2013[ano == 2013])|!is.na(v.2015[ano == 2015]),
                                                                ifelse(is.na(v.2013[ano == 2013]), v.2015[ano == 2015], v.2013[ano == 2013]), 0),
          !is.na(v.2017[ano == 2017]) ~  pred[ano == 2017] +  v.2017[ano == 2017])
        
      )) %>%
    mutate( !!rlang::sym(n) := ifelse(is.na( !!rlang::sym(n) ), new, !!rlang::sym(n)))%>%
    ungroup()  

}


tt= seade%>%
  dplyr::select(c('tx_m_infantil', 'new', 'ano', 'muni', 'pred',
                  'v.2013', 'v.2015', 'v.2017'))

nomes = seade[,c('muni', 'code_muni', 'ano', 'pop')]
remover = c('muni', 'code_muni', 'ano', 'pop', 'np')

normalit <- function(m){(m - min(m))/(max(m)-min(m))}



seade = seade %>%
  group_by(ano)%>%
  dplyr::select(-remover)%>%
  mutate_all(normalit)
  #filter(ano%in%seq(2013, 2017, 2))



seade[,'ano'] = nomes$ano
seade[,'muni'] = nomes$muni
seade[,'pop'] = nomes$pop

saveRDS(seade, 'seade2.rds')





#####

### exemplos



library(tidyverse)

t = rep(c('a', 'b', 'c'), rep(4, 3))
v1 = c(1, NA, 9, 4, 5, 7, NA, 8, 2, NA, 1, 5)
v2 = runif(12)*100
v3 = rnorm(12)*45
v4 = rnorm(12)
v5 = c(8, 5, 3, NA, 3, 2, 10, NA, 4, NA, 0, 11)
qual = sum((t=='a'))
pred = rep(NaN, 3*qual)

mun = rep(c('x','y','w','z'), 3)

df = tibble(t, v1, v2, v3, v4, v5, pred, mun)



vars = c('v1', 'v5')

df


for (n in vars){
  for (i in c('a', 'b', 'c')) {
    
    fit = lm(expr(!!rlang::sym(n) ~ v3 + v2), df[df$t==i, ])
     df = df %>%
      mutate(
             pred = case_when( 
               t == i ~ predict(fit,.),
               t != i ~ pred
               ) )%>%
      mutate("v.{i}":= 
               case_when(
                 t == i ~ !!rlang::sym(n) - pred,
                 t != i ~ pred
               ))  } 
      df = df%>%
      group_by(mun)%>%
      mutate(
        new = case_when(
          t =='a' ~ case_when(
            is.na(v.a[t=='a']) ~  pred[t=='a'] + ifelse( is.numeric(v.b[t=='b'])|is.numeric(v.c[t=='c']) ,
                                                ifelse(is.na(v.b[t=='b']), v.c[t=='c'], v.b[t=='b']), 0) ,
            !is.na(v.a[t=='a']) ~ pred[t=='a'] + v.a[t=='a']),
          
          t=='b' ~ case_when(
            is.na(v.b[t=='b'])  ~  pred[t=='b'] + ifelse(is.numeric(v.a[t=='a'])|is.numeric(v.c[t=='c']),
                                                  ifelse(is.na(v.c[t=='c']), v.a[t=='a'], v.c[t=='c']), 0),
            !is.na(v.b[t=='b']) ~  pred[t=='b'] +  v.b[t=='b']),
        
          t=='c' ~ case_when(
            is.na(v.c[t=='c'])  ~  pred[t=='c'] +  ifelse(is.numeric(v.a[t=='a'])|is.numeric(v.b[t=='b']),
                                                  ifelse(is.na(v.a[t=='a']), v.b[t=='b'], v.a[t=='a']), 0),
            !is.na(v.c[t=='c']) ~  pred[t=='c'] +  v.c[t=='c'])
          
        )) %>%
      mutate( !!rlang::sym(n) := ifelse(is.na( !!rlang::sym(n) ), new, !!rlang::sym(n)))%>%
      ungroup()  
      print(df)
}



















for(i in 3:length(master)){
  v_name = colnames(master)[i]
  ss = sum(is.na(master[,i]))
  cat('Número de NAs na variável', v_name, 'é', ss, '\n')
  cat('Proporcionalmente é:', ss/dim(master)[1], '\n')
}

