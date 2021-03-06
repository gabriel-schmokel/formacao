# Resumo: esse c�digo tem como fun��o fazer o tratamento, a explora��o e a an�lise 
# dos dados presente no diret�rio 'data'

# instalando os pacotes para trabalhar com a explora��o, manipula��o e an�lise dos arquivos do dataset.
install.packages("dplyr") 

# carregando o pacote
library(dplyr)

# setwd referencia o diret�rio contendo os arquivos para a leitura
setwd("C:/Gabriel/trabalho/analise_tratamento_exploracao/linguagem_r_para_analise_de_dados/dados-covid-sp-master/data")

 
covid_sp <- read.csv('dados_covid_sp.csv', sep = ";")
# visualizando o data frame
View(covid_sp)

# na coluna nome do munic�pio percebemos as nomeclaturas erradas, pois n�o foi interpretada a acentua��o. 
# podemos modificar adequadamente as nomeclaturas identificando a acentua��o atrav�s da fun��o read.csv2() com o encoding = 'UTF-8'
covid_sp <- read.csv2('dados_covid_sp.csv', sep = ";", encoding="UTF-8")
View(covid_sp)
# chamando o comando head() pra visualizar as primeiras linhas do data frame covid_sp
head(covid_sp)


# MODIFICANDO A NOMECLATURA DAS COLUNAS

# Modificando o �ndice do nome de algumas colunas, a fim de passar uma melhor interpreta��o na leitura do arquivo.
# Essa modiifica��o ser� salva em um novo data frame
covid_sp_alterado <- rename(covid_sp, municipio = nome_munic, data = datahora, rotulo=map_leg, codigo_mapa = map_leg_s)
View(covid_sp_alterado)


# EXCLUINDO COLUNAS
# Podemos excluir a coluna com c�digo da regi�o, j� que iremos trabalhar o refenciamento de uma regi�o apenas com o seu nome.
# Obs: o �ndice da coluna que possue o c�digo da regi�o � cod_ra.
covid_sp_alterado$cod_ra <- NULL

# Desejamos excluir outras colunas, como: rotulo, codigo_ibge, cod_drs, letalidade, nome_ra, cod_drs, pop, pop_60
# Na an�lise que iremos fazer n�o h� necessidade de trabalhar com as colunas que ser�o deletadas.
# Para excluir essas colunas, iremos utilizar diferentes m�todos

# excluindo a coluna pelo seu n�mero de identifica��o.
covid_sp_alterado <- select(covid_sp_alterado, -c(21))
# exluindo v�rias colunas pelo seu nome de identifica��o.
covid_sp_alterado <- subset(covid_sp_alterado, select = -c(codigo_ibge, cod_drs))
# exlcuindo v�rias colunas pelo seu n�mero de identifica��o.
covid_sp_alterado <- select(covid_sp_alterado, -c(14,15))
# exluindo um intervalos de colunas
covid_sp_alterado <- select(covid_sp_alterado, -c(17:19))


# EXCLUINDO LINHAS
# Desejamos exlucuir as linhas cuja coluna municipio apresente o resultado 'Ignorado'. Note, que n�o saber o municipio vai interir nos resultados da nossa an�lise, por isso a exclus�o.
# Abaixo, segue as diferentes formas de excluir as linhas de um dataframe

# excluir uma linhas espec�fica (deve-se passar a identifica��o da linha que se deseja excluir)
covid_sp_alterado <- slice(covid_sp_alterado, -c(239660))
# excluir um intervalo de linhas
covid_sp_alterado <- slice(covid_sp_alterado, -c(239661:239666))
# excluir um intervalo de linhas utilizando um filtro
covid_sp_alterado <- covid_sp_alterado %>% filter(municipio!="Ignorado")
View(covid_sp_alterado)


# VERIFICANDO VALORES AUSENTES (missing)
# NA = valores ausentes, os quais n�o s�o preenchidos
# NAN = not a number(valor indefinido)
# podemos visualizar atrav�s da fun��o sapply() o n�mero de valores ausentes
sapply(covid_sp_alterado, function(x) sum(is.na(x)))
sapply(covid_sp_alterado, function(x) sum(is.nan(x)))

# SUBSTITUINDO VALORES MISSING
# Deve-se antes carregar o pacote tidyr para fazer a substitui��o ds valres ausentes
install.packages("tidyr") 
library(tidyr)

# substituindo os dados ausentes por 54 usando a fun��o mutate_all
# os valores alterados ser�o armazenados no novo data frame (covid_sp_alterado2)
covid_sp_alterado2 <- covid_sp_alterado %>% mutate_all(replace_na, 54)
View(covid_sp_alterado2)
# outra forma de alterar os dados ausentes � usando a fun��o replace().
covid_sp_alterado2 <- replace(x = covid_sp_alterado,list = is.na(covid_sp_alterado),
                 values = 54)

# Como a semana_epidemia n�o vai somente at� 54, a coluna que possue a vari�vel data prova isso, � necess�rio utilizar uma condi��o associada a data 
# para atribuir a semana correta atribuida ao per�do da epidemia. 
covid_sp_alterado2$semana_epidem[covid_sp_alterado2$data >= '2021-01-01' &
                                 covid_sp_alterado2$data <= '2021-01-07'  ] <- 54

covid_sp_alterado2$semana_epidem[covid_sp_alterado2$data >= '2021-01-08' &
                                   covid_sp_alterado2$data <= '2021-01-14'  ] <- 55

covid_sp_alterado2$semana_epidem[covid_sp_alterado2$data >= '2021-01-15' &
                                   covid_sp_alterado2$data <= '2021-01-21'  ] <- 56

covid_sp_alterado2$semana_epidem[covid_sp_alterado2$data >= '2021-01-22' &
                                   covid_sp_alterado2$data <= '2021-01-28'  ] <- 57

covid_sp_alterado2$semana_epidem[covid_sp_alterado2$data >= '2021-01-29' &
                                   covid_sp_alterado2$data <= '2021-02-04'  ] <- 58

covid_sp_alterado2$semana_epidem[covid_sp_alterado2$data >= '2021-02-05' &
                                   covid_sp_alterado2$data <= '2021-02-12'  ] <- 59

covid_sp_alterado2$semana_epidem[covid_sp_alterado2$data >= '2021-02-13' &
                                   covid_sp_alterado2$data <= '2021-02-20'  ] <- 60

covid_sp_alterado2$semana_epidem[covid_sp_alterado2$data >= '2021-02-21' &
                                   covid_sp_alterado2$data <= '2021-02-27'  ] <- 61

covid_sp_alterado2$semana_epidem[covid_sp_alterado2$data >= '2021-02-28' &
                                   covid_sp_alterado2$data <= '2021-03-6'  ] <- 62


# VERIFICA��O E TRANSFORMA��O DA TIPAGEM DOS ATRIBUTOS (Vari�veis),

# H� duas formas de verificar a classe de uma vari�vel
# Pela fun��o str(...) ou pela glimpse(...)
str(covid_sp_alterado2)
glimpse(covid_sp_alterado2)

#Transformando a vari�vel semana_epidem em inteira
covid_sp_alterado2$semana_epidem <- as.integer(covid_sp_alterado2$semana_epidem)
str(covid_sp_alterado2)

#Transformando a vari�vel data que � car�cter no tipo data
covid_sp_alterado2$data <- as.Date(covid_sp_alterado2$data, format ='%Y-%m-%d')
str(covid_sp_alterado2)

# CRIA��O DE COLUNAS
# queremos criar uma nova coluna que informa o percentual de idosos presentes na popula��o
covid_sp_alterado2["idoso(%)"]<-100*covid_sp_alterado2$pop_60/covid_sp_alterado2$pop
View(covid_sp_alterado2)

#Feita a explora��o e o tratamento dos dados podemos exportar o data frame para fazer a an�lise estat�stica
#A exporta��o do data frame ser� direcionada ao local que inserimos pela fun��o setwd("")
write.table(covid_sp_alterado2, file ="covid_sp_tratado.csv", sep = ",")


