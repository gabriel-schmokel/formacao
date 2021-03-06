
#        ***********************************************************************************
#        *                                                                                 *
#        *    BREVE EXPLORA��O DOS DADOS EXPORTADOS ANTERIORMENTE, FILTRAGEM E AN�LISE     *
#        *                                                                                 *
#        ***********************************************************************************


# --------------------------------------------------------------------------------------------------------
# BREVE EXPLORA��O
# O objetivo dessa etapa � explorar os dados que foram tratados na parte_1, a fim de visualizar como eles est�o quando fazemos a
# importa��o do arquivo de dados. Note, que mesmo ap�s a importa��o de um arquivo tratado � necess�rio fazer mais um pr� tratamento.


if(!require(rstatix)) install.packages("rstatix") 
if(!require(dplyr)) install.packages("dplyr")
library(dplyr)     # carregando a biblioteca dplyr
library(rstatix)

setwd("C:/gabriel/trabalho/cursos/linguagem_r_para_analise_de_dados/projeto_01")    # direcionando o caminho leitura para o diret�rio que cont�m nosso arquivo tratado.
covid_sp_tratado <- read.csv2('covid_sp_tratado.csv', sep = ",", encoding="UTF-8")  # fazendo a leitura do arquivo
View(covid_sp_tratado)      # visualizando o DataFrame covid_sp_tradado


# Observamos com o comando View() que a leitura do arquivo n�o se deu de forma adequada.
# Removendo o argumento enconding j� � poss�vel resolver isso.
covid_sp_tratado <- read.csv2('covid_sp_tratado.csv', sep = ",")
View(covid_sp_tratado)

# Verificando se a classe dos dados est� correta
glimpse(covid_sp_tratado)

# Perce-be que a vari�vel idoso � string, deve-se passar para num�rica
# A vari�vel data tamb�m deve ser mudada para class date 
covid_sp_tratado$idoso... <- as.numeric(covid_sp_tratado$idoso...)                          # mudando a vari�vel idoso para o tipo num�rico.
covid_sp_tratado$data <- as.Date(covid_sp_tratado$data, format ='%Y-%m-%d')                 # mudando a vari�vel data para o tipo date.
glimpse(covid_sp_tratado)     
covid_sp_tratado <- rename(covid_sp_tratado, percentual_idoso = idoso...)                   # mudando o nome da vari�vel ...idoso para uma mais adequada.
View(covid_sp_tratado)
 

# --------------------------------------------------------------------------------------------------------
# FILTRAGEM
# O objetivo dessa etapa � trabalhar com a filtragem dos dados


# Podemos utilizar o m�todo filter(...), em adi��o com o operador %>% para, para buscar o que desejamos no DataFrame. 
# No exemplo abaixo estamos buscando apenas as linhas que contenham o mun�cipio de Campinas
covid_campinas <- covid_sp_tratado %>% filter(municipio=="Campinas")    # Estamos armazenando a busca feita no data frame covid_campinas
View(covid_campinas)


# Podemos criar uma nova coluna informando a densidade demogr�fica de campinas
# Para isso devemos dividir o n�mero de pessoas que vivem na cidade de campinas pela area quadrada do municipio
covid_campinas["dens_demografica"] <- covid_campinas$pop/covid_campinas$area
View(covid_campinas)


# Como o resultado da �rea demogr�fica do munic�pio de campinas parece estar errado sendo 100 vezes maior do que a informado pelos dados do google 
# iremos ent�o fazer essa altera��o. Podemos ver que isso ir� afetar o c�lculo da densidade populacional, por isso o resultado apresentado anteriormente
# parece algo invi�vel (Campinas tem mais de 15 pessoas por quilometro quadrado). 
covid_campinas["area"] <- covid_campinas$area/100
covid_campinas["dens_demografica"] <- covid_campinas$pop/covid_campinas$area
View(covid_campinas)

# Uma outra forma de fazer uma filtragem nos dados � utilizando o m�todo which(...)
# Nessa situa��o estamos pegando apenas as linhas que correspondem ao municipio de garulhos
covid_guarulhos <- covid_sp_tratado[which(covid_sp_tratado$municipio=="Guarulhos"), ]
covid_guarulhos["area"] <- covid_guarulhos$area/100                       # arrumando os dados correspondentes a �rea de guarulhos
covid_guarulhos["dens_demografica"] <- covid_guarulhos$pop/covid_guarulhos$area        # c�lculo da densidade populacional de guarulhos
View(covid_guarulhos)


# --------------------------------------------------------------------------------------------------------
# AN�LISE


# Inicialmente vamos focar a nossa an�lise nas medidas de centralidade das regi�es de garulhos e campinas. Um dos dados
# importante de se obter � m�dia dos casos novos e dos �bitos novos. 
# Para o c�lculo da m�dia utilizamos a fun��o mean(...), passamos como argumento a vari�vel do DataFrame que desejamos 
# calcular a m�dia.  
mean(covid_campinas$obitos_novos)         
mean(covid_campinas$casos_novos)


# com a fun��o summarise_at(...) podemos calcular diretamente a m�dia de mais de uma vari�vel, devemos apenas passar como argumentos
# as vari�veis que se deseja calcular a m�dia, o dataframe e opera��o que desejamos fazer (no caso � o c�lculo da m�dia)
summarise_at(covid_campinas, vars(obitos_novos, casos_novos), mean)


mean(covid_guarulhos$obitos_novos)  # c�lculo da m�dia de  novos obitos em guarulhos
mean(covid_guarulhos$casos_novos)   # c�lculo da m�dia de novos casos em guarulhos


# Uma curva importante de se ter � a m�dia m�vel em rela��o ao tempo. 
# Com ela podemos avaliar se o n�mero de casos e �bitos est� diminuindo ou aumentando.
plot(covid_campinas$data,covid_campinas$casos_mm7d, title("M�DIA M�VEL"), col = "red")
plot(covid_campinas$data,covid_campinas$obitos_mm7d, title("M�DIA M�VEL"), col = "purple")


# Mediana
# A m�dia, muitas vezes pode ser afetada por um valor outlier, por conta disso vamos tamb�m tomar a mediana dos novos obitos e casos para ambos munic�pios.
median(covid_campinas$obitos_novos)       # c�lculo da mediana de novos �bitos.
median(covid_campinas$casos_novos)        # c�lculo da mediana de novos casos.
summarise_at(covid_campinas, vars(obitos_novos, casos_novos), median)      # aplicando o c�lculo da mediana a duas vari�veis.
median(covid_guarulhos$obitos_novos)      # c�lculo da mediana de novos �bitos
median(covid_guarulhos$casos_novos)       # c�lculo da mediana de novos casos


# Moda
# Diferente da m�dia e mediana, n�o existe uma fun��o pronta para o calculo da moda, por isso a respectiva fun��o � criada
moda <- function(m) {
  valor_unico <- unique(m) # Busca todos os valores �nicos que passamos como argumento. Exemplo: recebendo obitos novos do dataframe coluna, iremos receber todos os seus valores, independentes se eles s�o iguais.
  valor_unico[which.max(tabulate(match(m, valor_unico)))] # tabulate(...) (contabilizar quantas vezes o valor �nico aparece) e buscar o maior valor depois da organiza��o por grupos
}

# Obten��o da moda
moda(covid_campinas$obitos_novos)    # c�lculo da moda no dataframe covid_campinas
moda(covid_campinas$casos_novos)     # c�lculo da moda no dataframe casos_novos
summarise_at(covid_campinas, vars(obitos_novos, casos_novos), moda)     # c�lculo da moda para duas vari�veis
moda(covid_guarulhos$obitos_novos)  # c�lculo da moda dos novos obitos para o dataframe covid_guarulhos.
moda(covid_guarulhos$casos_novos)   # c�lculo da moda dos novos casos para o dataframe covid_guarulhos.


# NOVA FILTRAGEM
# Tanto em Campinas quanto em Guarulhos o maior n�mero de �bitos foi no m�s de julho,
# portanto � importe olhar com maior profundidade para esse m�s. 
# Vamos criar um novo DataFrame chamado *covid_julho_campinas* e *covid_julho_guarulhos* e filtrar apenas 
# os dados que correspondem ao m�s de julho para os DataFrames *covid_campinas* e *covid_guarulhos*.
covid_julho_campinas <- covid_campinas %>% filter(mes==7)
covid_julho_guarulhos <- covid_guarulhos %>% filter(mes==7)


# HISTOGRAMA
# Podemos confirmar a frequ�ncia dos dados apresentados no m�s de julho plotando a curva do histograma.
hist(covid_campinas$obitos_novos, col="blue")
hist(covid_guarulhos$obitos_novos, col="red")


# MEDIDAS DE POSI��O
# Para ter uma boa compreens�o de como nossos dados est�o arranjados, � interessante obter as medidas de 
# posi��o, at� para posteriormente comparar com o gr�fico boxplot.

# M�nimo
# A medida de posi��o m�nima corresponde ao menor valor do nosso conjunto de dados da vari�vel em
# observa��o. Podemos observar que antes da epidemia come�ar nas regi�es de Campinas e Guarulhos o valor
# de �bitos e de novos casos era nulo (observe o m�s de maio para confirmar isso), por isso teremos como
# valor m�nimo para essas vari�veis o valor zero. Atrav�s do m�todo **min(...)**, passando como argumento
# a vari�vel do DataFrame, obtemos o valor m�nimo.
print("M�nimos Valores de Campinas")
min(covid_campinas$obitos_novos)
min(covid_campinas$casos_novos)
print("M�nimos Valores de Guarulhos")
min(covid_guarulhos$obitos_novos)
min(covid_guarulhos$casos_novos)
summarise_at(covid_campinas, vars(obitos_novos, casos_novos), min)
summarise_at(covid_guarulhos, vars(obitos_novos, casos_novos), min)


# M�ximo
# A medida de posi��o m�xima corresponde ao maior valor do nosso conjunto de dados da vari�vel em 
# observa��o. Podemos observar que o m�s de julho para as regi�es de Campinas e Guarulhos possuem os 
# valores de �bitos e de novos casos mais altos, para confirmar esse valor basta usarmos o
# m�todo **max(...)** onde passamos como argumento a vari�vel do DataFrame em quest�o.
print("Maximo valor Campinas:")
max(covid_campinas$obitos_novos)
max(covid_campinas$casos_novos)
print("Maximo valor Guarulhos:")
max(covid_guarulhos$obitos_novos)
max(covid_guarulhos$casos_novos)
summarise_at(covid_campinas, vars(obitos_novos, casos_novos), max)
summarise_at(covid_guarulhos, vars(obitos_novos, casos_novos), max)

# Amplitude total
# A amplitude total � uma medida de posi��o dada pela diferen�a do m�ximo valor pela do m�nimo,
# com essa medida temos ideia do espa�o em que o nosso conjunto de dados pertence.
# Para calcular a amplitude utilizamos a fun��o **range(...)** passando como argumento a vari�vel 
# do DataFrame que desejamos realizar essa opera��o.
print("Valores limites de Campinas. O m�dulo da diferen�a etre eles � a amplitude total.")
range(covid_campinas$obitos_novos)
range(covid_campinas$casos_novos)
print("Valores limites de Guarulhos. O m�dulo da diferen�a etre eles � a amplitude total.")
range(covid_guarulhos$obitos_novos)
range(covid_guarulhos$casos_novos)
summarise_at(covid_campinas, vars(obitos_novos, casos_novos), range)
summarise_at(covid_guarulhos, vars(obitos_novos, casos_novos), range)














