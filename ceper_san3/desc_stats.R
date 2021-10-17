#########################################################################
#                      PRODUTIVIDADE ESTADUAL
#########################################################################

# here I will calculate descriptive statistics on the sanitation 
# situation in the municipalities of São Paulo 


setwd("D:/Git projects/CEPER/ceper_san3")


source('functions.R', encoding = 'UTF8')


# dataset ----

df = readxl::read_excel('snis.xlsx')


# change colnames ---- 

old_n = colnames(df)

colnames(df) = substr(colnames(df), 1, 5)
colnames(df)[c(1, 2, 5)] = c('cod_ibge', 'cod_mun', 'ano')

new_n = colnames(df)


view_n = function(){
  for(o in 1:length(old_n)){
    cat(new_n[o], '---->', old_n[o], '\n')
  }
}


# create new variable ----

view_n()

tt = df %>%
  dplyr::filter(ano == 2019)%>%
  group_by(Natur)%>%
  summarise(n = mean(IN004) )

 
df = df %>%
  mutate(my_code = case_when(
    Natur %in% tt$Natur[1:5]   ~ 1,   # "Administração pública direta"
    Natur %in% tt$Natur[6:10]  ~ 2,   # "Autarquia"
    Natur %in% tt$Natur[11:12] ~ 3,   # "Empresa privada"
    Natur %in% tt$Natur[13:14] ~ 4,   # "Empresa pública"
    Natur %in% tt$Natur[15:19] ~ 5,   # "Sociedade de economia mista com administração pública"
  )
) 



stats2 = function(data, x){
  x = enquo(x)
  data %>%
    dplyr::filter(ano==2019)%>%
    group_by(my_code)%>%
    drop_na(everything())%>%
    summarise(Média = mean(!!x),
              Mediana = median(!!x),
              Máximo = max(!!x),
              Mínimo = min(!!x),
              Desvio_p = sd(!!x) )
}

stats2(df, IN055)

view_n()


zz=df %>%
  #group_by(my_code)%>%
  dplyr::filter(ano==2019 & my_code==1) %>%
  drop_na(everything())
  #summarise(table(my_code))



view_n()

pr2 = as.numeric(substr(pr$Prest, 2, 9) )

table(pr2)

# 2019 372 -> SABESP 35503000
















