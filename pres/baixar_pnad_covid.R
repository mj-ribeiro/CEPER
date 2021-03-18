# see https://analisemacro.com.br/data-science/dicas-de-rstats/analise-de-microdados-da-pnad-covid-com-o-r/

setwd("D:/Git projects/CEPER/pres")



library(tidyverse)

url = "ftp://ftp.ibge.gov.br/Trabalho_e_Rendimento/Pesquisa_Nacional_por_Amostra_de_Domicilios_PNAD_COVID19/Microdados/Dados/PNAD_COVID_112020.zip"

download.file(url, destfile='PNAD_COVID_122020.zip', mode='wb')


unzip('PNAD_COVID_122020.zip')


pnad_covid <- read_csv("PNAD_COVID_112020.csv",
                       col_types = cols(.default = "d"))




head(pnad_covid)




url_2 = "ftp://ftp.ibge.gov.br/Trabalho_e_Rendimento/Pesquisa_Nacional_por_Amostra_de_Domicilios_PNAD_COVID19/Microdados/Documentacao/Dicionario_PNAD_COVID_112020.xls"

download.file(url_2, destfile='Dicionario_PNAD_COVID_112020.xls', mode='wb')





