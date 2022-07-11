
#        ***********************************************************************************
#        *                                                                                 *
#        *    BREVE EXPLORAÇÃO DOS DADOS EXPORTADOS ANTERIORMENTE, FILTRAGEM E ANÁLISE     *
#        *                                                                                 *
#        ***********************************************************************************

# O objetivo desse código é explorar os dados que foram tratados na parte_1, a fim de visualizar como eles estão quando fazemos a
# importação do arquivo de dados. Note, que mesmo após a importação de um arquivo tratado é necessário fazer mais um pré tratamento.



# install.packages("dplyr")
library(dplyr)     # carregando a biblioteca dplyr


setwd("C:/gabriel/trabalho/cursos/linguagem_r_para_analise_de_dados/projeto_01")    # direcionando o caminho leitura para o diretório que contém nosso arquivo tratado.
covid_sp_tratado <- read.csv2('covid_sp_tratado.csv', sep = ",", encoding="UTF-8")  # fazendo a leitura do arquivo
View(covid_sp_tratado)      # visualizando o DataFrame covid_sp_tradado


# Observamos com o comando View() que a leitura do arquivo não se deu de forma adequada.
# Removendo o argumento enconding já é possível resolver isso.
covid_sp_tratado <- read.csv2('covid_sp_tratado.csv', sep = ",")
View(covid_sp_tratado)

# Verificando se a classe dos dados está correta
glimpse(covid_sp_tratado)

# Perce-be que a variável idoso é string, deve-se passar para numérica
# A variável data também deve ser mudada para class date 
covid_sp_tratado$idoso... <- as.numeric(covid_sp_tratado$idoso...)                          # mudando a variável idoso para o tipo numérico.
covid_sp_tratado$data <- as.Date(covid_sp_tratado$data, format ='%Y-%m-%d')                 # mudando a variável data para o tipo date.
glimpse(covid_sp_tratado)     
covid_sp_tratado <- rename(covid_sp_tratado, percentual_idoso = idoso...)                   # mudando o nome da variável ...idoso para uma mais adequada.
View(covid_sp_tratado)
 
