

setwd("D:/Git projects/CEPER/pres")


library(PNADcIBGE)
library(survey)


# download da pnad contínua


#dadosPNADc_brutos = get_pnadc(year = 2020,
#                              quarter = 4, 
#                               design = T)



vars =  c("UF", "V2007", "V2009", 
          "V2010", "V3007", "VD3002", 
          "VD4001", "VD4002", "VD4020",
          "VD4035", 'VD4010', 'VD4036'
          )

dadosPNADc <- get_pnadc(year = 2020, 
                        quarter = 4, 
                        vars = vars)


# renda média 



rendaUF = svyby(~VD4020, ~UF, dadosPNADc, svymean, na.rm = T)

renda  = data.frame(rendaUF[2])



# tx de desocupação no Brasil

txdesocup <- svyratio(~VD4002 == "Pessoas desocupadas",
                      ~VD4001 == "Pessoas na força de trabalho", 
                      dadosPNADc, na.rm = T)

txdesocup




txdesocup_UF = svyby(~VD4002 == "Pessoas desocupadas",
      ~UF, dadosPNADc,
      svyratio, 
      denominator = ~VD4001 == "Pessoas na força de trabalho",
      na.rm = T)


tx_des = data.frame(txdesocup_UF[2])
colnames(tx_des) = 'tx_des'



# importar acrônimos


nomes = data.frame(readxl::read_xlsx('nome.xlsx'))

rownames(nomes) = nomes$nome

nomes[1] = NULL

nomes


# merge dataset com acrônimos


df = merge(tx_des,nomes, by="row.names", all=TRUE) 

rownames(df) = df$Row.names


df = merge(renda,df, by="row.names", all=TRUE) 
rownames(df) = df$Row.names



df[1] = NULL

head(df)




# import covid


covid = data.frame(readxl::read_xlsx('covid.xlsx'))



# merge df e covid


df = merge(df, covid, by.x ='UF', by.y ='UF' )




colnames(df) = c('UF', 'renda','estados','tx_des', 'covid', 'mortes')

head(df)

df

#saveRDS(df, 'df.rds')



prop =svymean(~as.factor(VD4010), subset(dadosPNADc, 
        UF =='São Paulo'),
        na.rm=T)



sum(data.frame(prop)[,1] )






# equação de salário 

library(survey)

# colocar setor, se está ou não empregado


modeloLin <- svyglm( log(VD4020) ~ V2007 + V2009 + V2010
                    + VD4035 + VD4036 + VD4010 + UF , dadosPNADc)

vars

summary(modeloLin)
















