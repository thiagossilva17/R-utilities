#NOME: Thiago Silva   RA:2389989

#ENTENDIMENTO SOBRE A ANALISE
#O que e uma variavel instrumental e suas caracteristicas:
#Um instrumento e uma variavel exogena, com 2 propriedades: 
#1. Covariancia igual a 0 em relacao ao termo de erro.
#2. Covariancia diferente de 0 em relacao a variavel endogena. (indica relevancia, traz informacoes importantes para prevermos a renda atraves da variavel endogena (educacao).

#O que eh equacao estrutural:
#eh o modelo populacional de interesse, ou seja prever a "renda" com base na "idade" e "educacao".
#renda = Beta0 + Beta1 idade + Beta2 educ + u
#nesta equacao estrutural a variavel educacao eh endogena e temos que resolver este problema, pois a variavel endogena causa vies, ineficiencia e incosistencia no MQO.

#Quando existe uma variavel endogena nao permite usar mqo, pq causa causa vies, ineficiencia e incosistencia no MQO.
#Para isso eh necessario usar a chamada "equacao reduzida" em que relaciona a variavel endogena com exogenas e os instrumentos
#A equacao reduzida ficaria assim: educ= E0 + E1 educ mae + E2 educ pai + u

#Quando identificamos a variavel endogena e os instrumentos podemos usar a estimacao de 2 estagios afim de separarmos as variaveis endogenas e exogenas
#Ou seja, no 1estagio regredimos a endogena contra as explicativas e os instrumentos.
#Assim obtemos o predito da endogena.
#Onde a ideia aqui eh extrair apenas a parte exogena e eliminarmos a parte endogena.
#entao, no 2estagio substituimos a variavel endogena e colocamos seu valor predito no 1estagio
#Essa estrategia elimina a endogeneidade e seus problemas: eliminamos o viés, ineficiencia e a inconsistencia do MQO.

#Premissas para usar o metodo de 2 estagios: Garantir que o instrumento seja uma variavel exogena e com covariancia diferente de 0 com a variavel endogena

#Regredir endogena contra todas as exogenas e instrumentos
#Utilizar "Teste T" no instrumento para saber se ele eh relevante

#-----------------------------------------------------------------------------------------

#Exercício:
#Verifique se a variável educ é endógena, tendo como instrumentos educação do pai, educação da mãe e educação do marido.
#Com base no resultado obtido, qual método de estimação você indicaria para estimar os parâmetros da equação de interesse:
#MQO ou 2SLS? Adote 10% de significância. Para tanto, utilize o arquivo MROZ.xls.

#O exercicio adota como variaveis instrumentais "educacao do pai", "educacao da mae" e "educacao do marido". Ou seja, estamos assumindo 
# que as variaveis instrumentais possuem covariância igual a 0 quando comparadas com o termo de Erro.
#Além disso a covariância destas variaveis com a variável endogena também deve ser diferente de 0.

#Bibliotecas utilizadas
library(AER)
library(sem)
library(lmtest)
library(corrplot)
library(readxl)
library(dplyr)
library(GGally)

#importando os dados do exercicio
bd <- read_excel("C:/Users/Thiago Silva/Desktop/mroz.xls", sheet = "Sheet1")

#Identificando se a variavel "educ" eh endogena:
cov(bd$educ, bd$fathereduc)
cov(bd$educ, bd$mothereduc)
#A variavel educ eh endogena, haja visto que a covariancia com as variáveis instrumentais é diferente de 0.

#Realizando regressao simples como primeiro estagio
reg.simples <- lm(educ~fathereduc+mothereduc, data= bd)
summary(reg.simples)
#A partir do teste T, podemos ver que as variaveis "fathereduc" e "mothereduc" sao relevantes para prever a variavel "educ".

#Armazenando valores preditos no banco de dados
bd$educ_predit <- reg.simples$fitted.values

#Identificando variaveis que correlacionam com wage
#View(cor(bd))

#Correlacionando variaveis que possuem algum grau de correlacao com wage
bdcor <- bd %>% select(wage,lfp , exper , faminc , hours , age , fathereduc , mothereduc)
ggcorr(bdcor, label=T)

#Realizando segundo estagio substituindo a variavel educ pelos valores preditos na regressao de primeiro estagio
segundo.estagio <- lm(wage ~ lfp + exper + faminc + hours + age + fathereduc + mothereduc + educ_predit, data= bd)
summary(segundo.estagio)
