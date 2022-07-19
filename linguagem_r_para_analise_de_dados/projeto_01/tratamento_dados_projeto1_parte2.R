
#        ***********************************************************************************
#        *                                                                                 *
#        *    BREVE EXPLORAÇÃO DOS DADOS EXPORTADOS ANTERIORMENTE, FILTRAGEM E ANÁLISE     *
#        *                                                                                 *
#        ***********************************************************************************


# --------------------------------------------------------------------------------------------------------
# BREVE EXPLORAÇÃO
# O objetivo dessa etapa é explorar os dados que foram tratados na parte_1, a fim de visualizar como eles estão quando fazemos a
# importação do arquivo de dados. Note, que mesmo após a importação de um arquivo tratado é necessário fazer mais um pré tratamento.


if(!require(rstatix)) install.packages("rstatix") 
if(!require(dplyr)) install.packages("dplyr")
library(dplyr)     # carregando a biblioteca dplyr
library(rstatix)

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
 

# --------------------------------------------------------------------------------------------------------
# FILTRAGEM
# O objetivo dessa etapa é trabalhar com a filtragem dos dados


# Podemos utilizar o método filter(...), em adição com o operador %>% para, para buscar o que desejamos no DataFrame. 
# No exemplo abaixo estamos buscando apenas as linhas que contenham o munícipio de Campinas
covid_campinas <- covid_sp_tratado %>% filter(municipio=="Campinas")    # Estamos armazenando a busca feita no data frame covid_campinas
View(covid_campinas)


# Podemos criar uma nova coluna informando a densidade demográfica de campinas
# Para isso devemos dividir o número de pessoas que vivem na cidade de campinas pela area quadrada do municipio
covid_campinas["dens_demografica"] <- covid_campinas$pop/covid_campinas$area
View(covid_campinas)


# Como o resultado da área demográfica do município de campinas parece estar errado sendo 100 vezes maior do que a informado pelos dados do google 
# iremos então fazer essa alteração. Podemos ver que isso irá afetar o cálculo da densidade populacional, por isso o resultado apresentado anteriormente
# parece algo inviável (Campinas tem mais de 15 pessoas por quilometro quadrado). 
covid_campinas["area"] <- covid_campinas$area/100
covid_campinas["dens_demografica"] <- covid_campinas$pop/covid_campinas$area
View(covid_campinas)

# Uma outra forma de fazer uma filtragem nos dados é utilizando o método which(...)
# Nessa situação estamos pegando apenas as linhas que correspondem ao municipio de garulhos
covid_guarulhos <- covid_sp_tratado[which(covid_sp_tratado$municipio=="Guarulhos"), ]
covid_guarulhos["area"] <- covid_guarulhos$area/100                       # arrumando os dados correspondentes a área de guarulhos
covid_guarulhos["dens_demografica"] <- covid_guarulhos$pop/covid_guarulhos$area        # cálculo da densidade populacional de guarulhos
View(covid_guarulhos)


# --------------------------------------------------------------------------------------------------------
# ANÁLISE


# Inicialmente vamos focar a nossa análise nas medidas de centralidade das regiões de garulhos e campinas. Um dos dados
# importante de se obter é média dos casos novos e dos óbitos novos. 
# Para o cálculo da média utilizamos a função mean(...), passamos como argumento a variável do DataFrame que desejamos 
# calcular a média.  
mean(covid_campinas$obitos_novos)         
mean(covid_campinas$casos_novos)


# com a função summarise_at(...) podemos calcular diretamente a média de mais de uma variável, devemos apenas passar como argumentos
# as variáveis que se deseja calcular a média, o dataframe e operação que desejamos fazer (no caso é o cálculo da média)
summarise_at(covid_campinas, vars(obitos_novos, casos_novos), mean)


mean(covid_guarulhos$obitos_novos)  # cálculo da média de  novos obitos em guarulhos
mean(covid_guarulhos$casos_novos)   # cálculo da média de novos casos em guarulhos


# Uma curva importante de se ter é a média móvel em relação ao tempo. 
# Com ela podemos avaliar se o número de casos e óbitos está diminuindo ou aumentando.
plot(covid_campinas$data,covid_campinas$casos_mm7d, title("MÉDIA MÓVEL"), col = "red")
plot(covid_campinas$data,covid_campinas$obitos_mm7d, title("MÉDIA MÓVEL"), col = "purple")


# Mediana
# A média, muitas vezes pode ser afetada por um valor outlier, por conta disso vamos também tomar a mediana dos novos obitos e casos para ambos municípios.
median(covid_campinas$obitos_novos)       # cálculo da mediana de novos óbitos.
median(covid_campinas$casos_novos)        # cálculo da mediana de novos casos.
summarise_at(covid_campinas, vars(obitos_novos, casos_novos), median)      # aplicando o cálculo da mediana a duas variáveis.
median(covid_guarulhos$obitos_novos)      # cálculo da mediana de novos óbitos
median(covid_guarulhos$casos_novos)       # cálculo da mediana de novos casos


# Moda
# Diferente da média e mediana, não existe uma função pronta para o calculo da moda, por isso a respectiva função é criada
moda <- function(m) {
  valor_unico <- unique(m) # Busca todos os valores únicos que passamos como argumento. Exemplo: recebendo obitos novos do dataframe coluna, iremos receber todos os seus valores, independentes se eles são iguais.
  valor_unico[which.max(tabulate(match(m, valor_unico)))] # tabulate(...) (contabilizar quantas vezes o valor único aparece) e buscar o maior valor depois da organização por grupos
}

# Obtenção da moda
moda(covid_campinas$obitos_novos)    # cálculo da moda no dataframe covid_campinas
moda(covid_campinas$casos_novos)     # cálculo da moda no dataframe casos_novos
summarise_at(covid_campinas, vars(obitos_novos, casos_novos), moda)     # cálculo da moda para duas variáveis
moda(covid_guarulhos$obitos_novos)  # cálculo da moda dos novos obitos para o dataframe covid_guarulhos.
moda(covid_guarulhos$casos_novos)   # cálculo da moda dos novos casos para o dataframe covid_guarulhos.


# NOVA FILTRAGEM
# Tanto em Campinas quanto em Guarulhos o maior número de óbitos foi no mês de julho,
# portanto é importe olhar com maior profundidade para esse mês. 
# Vamos criar um novo DataFrame chamado *covid_julho_campinas* e *covid_julho_guarulhos* e filtrar apenas 
# os dados que correspondem ao mês de julho para os DataFrames *covid_campinas* e *covid_guarulhos*.
covid_julho_campinas <- covid_campinas %>% filter(mes==7)
covid_julho_guarulhos <- covid_guarulhos %>% filter(mes==7)


# HISTOGRAMA
# Podemos confirmar a frequência dos dados apresentados no mês de julho plotando a curva do histograma.
hist(covid_campinas$obitos_novos, col="blue")
hist(covid_guarulhos$obitos_novos, col="red")


# MEDIDAS DE POSIÇÃO
# Para ter uma boa compreensão de como nossos dados estão arranjados, é interessante obter as medidas de 
# posição, até para posteriormente comparar com o gráfico boxplot.

# Mínimo
# A medida de posição mínima corresponde ao menor valor do nosso conjunto de dados da variável em
# observação. Podemos observar que antes da epidemia começar nas regiões de Campinas e Guarulhos o valor
# de óbitos e de novos casos era nulo (observe o mês de maio para confirmar isso), por isso teremos como
# valor mínimo para essas variáveis o valor zero. Através do método **min(...)**, passando como argumento
# a variável do DataFrame, obtemos o valor mínimo.
print("Mínimos Valores de Campinas")
min(covid_campinas$obitos_novos)
min(covid_campinas$casos_novos)
print("Mínimos Valores de Guarulhos")
min(covid_guarulhos$obitos_novos)
min(covid_guarulhos$casos_novos)
summarise_at(covid_campinas, vars(obitos_novos, casos_novos), min)
summarise_at(covid_guarulhos, vars(obitos_novos, casos_novos), min)


# Máximo
# A medida de posição máxima corresponde ao maior valor do nosso conjunto de dados da variável em 
# observação. Podemos observar que o mês de julho para as regiões de Campinas e Guarulhos possuem os 
# valores de óbitos e de novos casos mais altos, para confirmar esse valor basta usarmos o
# método **max(...)** onde passamos como argumento a variável do DataFrame em questão.
print("Maximo valor Campinas:")
max(covid_campinas$obitos_novos)
max(covid_campinas$casos_novos)
print("Maximo valor Guarulhos:")
max(covid_guarulhos$obitos_novos)
max(covid_guarulhos$casos_novos)
summarise_at(covid_campinas, vars(obitos_novos, casos_novos), max)
summarise_at(covid_guarulhos, vars(obitos_novos, casos_novos), max)

# Amplitude total
# A amplitude total é uma medida de posição dada pela diferença do máximo valor pela do mínimo,
# com essa medida temos ideia do espaço em que o nosso conjunto de dados pertence.
# Para calcular a amplitude utilizamos a função **range(...)** passando como argumento a variável 
# do DataFrame que desejamos realizar essa operação.
print("Valores limites de Campinas. O módulo da diferença etre eles é a amplitude total.")
range(covid_campinas$obitos_novos)
range(covid_campinas$casos_novos)
print("Valores limites de Guarulhos. O módulo da diferença etre eles é a amplitude total.")
range(covid_guarulhos$obitos_novos)
range(covid_guarulhos$casos_novos)
summarise_at(covid_campinas, vars(obitos_novos, casos_novos), range)
summarise_at(covid_guarulhos, vars(obitos_novos, casos_novos), range)














