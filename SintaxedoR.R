#Thiago Silva
#R-utilidades

#Vou manter este codigo no github com o intuito de relembrar algumas sintaxes do R.

#------------------------------------------------------------------------------
#Carregamento de arquivos
#lendo arquivo CSV
BD <- read.csv( "C:/xxx/Thiago Silva/CAMINHO/Teste.csv", sep = ";", encoding = "UTF-8",na.strings = "")

#carregando rapidamente
BD <- fread("caminho")


#salvando em multiplas planilhas do Excel
library(openxlsx)
lista <- list("NOME_DA_SHEET" = BD, "NOME_DA_SHEET2" = BD2)
write.xlsx(lista, file = "Z:/CAMINHO/CAMINHO/excel.xlsx")

#------------------------------------------------------------------------------
#Manipulacao de textos
#Cortar string com determinacao de caractere
strsplit("eu sei #itau mais", "#")

#Converter texto para minusculas
tolower
#Converter texto para maiusculas
toupper

#Extrai as hashtags do texto, procura pelo "#" e extrai toda a sequencia de caracteres ate encontrar um espaco
BD$Hashtag <- str_extract_all(BD$tweet_text, "#[:alnum:]+")
BD$teste <- str_extract_all(BD$tweet_text, "que")
BD$teste2 <- str_extract_all(BD$tweet_text, "sim")

#substitui palavra
gsub("PALAVRA PROCURADA" ,"PALAVRA A SUBSTITUIR", BD$Hashtag)

#Unlist desfaz uma lista separando por linhas
ContagemDeHashtags <- unlist(str_extract_all(BD$tweet_text, "#[:alnum:]+"))
View(ContagemDeHashtags)

#Boolean para mencoes - Verdadeiro ou falso
BD$Hashtag <- grepl( "#", BD[,2])

#SEPARA PALAVRAS
library(tidyr)
BD$Hashtag <- separate(BD, c("tweet_text"), "#" )


#------------------------------------------------------------------------------
#Manipulacao de Dataframes
#Criando dataframes
teste1<- c(1:15, "sim")
teste2<- c(1:15, "sim")
coluna<- c(1:15, "sim")
#valores repetidos
teste3 <- rep("sim" , times= 16)

#Juntando bases atraves das colunas
df <- cbind(teste1, teste3)

#Juntando bases atraves das linhas e convertendo em data frame
bdteste <- data.frame(rbind(obd, obd2))

#Alterar nome da coluna e da linha
colnames(df) <- c("Primeira","Segunda","Terceira","Quarta")
rownames(df) <- rep("Primeira", times= 16)

#Convertendo coluna para tipo String
BD$tweet_text<- as.character(BD$tweet_text)

#Deletando coluna
BD$tweet_date <- NULL

#selecionando colunas
my_data %>% select(1:3)
my_data %>% select(Sepal.Length, Petal.Length)
my_data %>% select(-(Sepal.Length:Petal.Length))


#Exportando arquivo CSV
write.csv(bdteste, file = "exemploBI.csv")


#-----------------------------------------------------------------------------
#Exploracao
#nmr de rows and columns
dim(df)
#Qtd total de dados
length(df)

#classes das variaveis
str()
#Primeiras linhas
head(BD)
#Ultimas linhas
tail(BD)

#Filtros
#filtrando por coluna
obd <- subset(BD, BD["teste"]=="que")
BD <- subset(BD, Country == "Brazil")
BD <- subset(BD, year %in% c("2020","2021"))


#----------------------------------------------------------------------------
#Loops
#APLICANDO FUNCAO PARA COLUNAS
BD$NEWCOLUM <- apply(BD, 2, mean)
#APLICANDO FUNCAO PARA LINHAS
#BD$NEWCOL <- apply(BD, 1, function)

df$media <- apply(df[c(2,3)],1, mean)
df

#------------------------------------------------------------------------------
#Medidas de dispersao
#Variancia
var(BD$coluna)
#desvio padrao
sd(BD$coluna)

#-----------------------------------------------------------------------------
#Series temporais
#quantmode
ChartSeries(BD, subset= 'last 6 months' )
ChartSeries(BD, subset= 'last 48 hours' )
LineChart(BD)


#-----------------------------------------------------------------------------
#Datas
#lubridate / Datas
ymd_hms("20200101 131345")

BD$Data <- as.POSIXct(BD$Data, format= "%y %m %d")
BD$Mes <- month(BD$Data)
BD$Year <- year(BD$Data)

#separar coluna
BD <- separate(BD,2, c("Novacol1","Novacol2","Novacol3"), sep="-")

#------------------------------------------------------------------------------
#Correlacoes
#Correlacoes em heatmaps
library(GGally)
ggcorr(bd, label=T)

#Correlacoes em cores
corrplot(cor(df), method= "color")

#------------------------------------------------------------------------------
#Regressao
#QUI-QUADRADo - T?CNICA PARA DEFINIR VARIAVEIS MAIS RELEVANTES, AS QUE MAIS IMPACTAM A VARIAVEL TARGET(VARIAVEL QUE QUER SER PREVISTA)
#AS VARIAVEIS QUE POSSUEM OS MAIORES QUI-QUADRADOS S?O AS QUE MAIS INFLUENCIAM O MODELO(S?O AS MAIS IMPORTANTES)
#SOMANDO POR GRUPOS
BD <- BD %>% group_by(ValorDeAlgumaColuna) %>%summarise(col1= sum(col1), col2= sum(col2),col3= sum(col3))

# P-VALUE : QUANTO MAIOR O P-VALUE MAIOR A CHANCE DA VARIAVEL NAO SER RELEVANTE (ELE CALCULA A PROBABILIDADE DE UMA VARIAVEL NAO SER RELEVANTE)
REG <- lm(col2~.,BD)

#A ULTIMA COLUNA FAZ REFERENCIA AO P-VALUE E AS ESTRELAS SAO REFERENTES A PROXIMIDADE EM RELACAO AO ZERO,
# QUANTO MAIS PROXIMO DE ZERO MAIS PROXIMA DE RECEBER 3 ESTRELAS, SIGNIFICA QUE A VARIAVEL E MAIS IMPORTANTE


#------------------------------------------------------------------------------
#sites for learning
#https://www.datanovia.com/en/lessons/select-data-frame-columns-in-r/

#calculo por segmentacao
BD <- BD %>% group_by(COLUNA1, COLUNA2) %>% summarise_at(vars(COLUNA_VALOR), list(NOME_NOVA_COLUNA = sum))
#contagem de valores
BD <- BD %>% count(COLUNA1, COLUNA2, COLUNA3)

#automatizando a identificacao do nome de colunas
#Aplicando para colunas que possuem em seus titulos o valor especificado:
apply(BD[, grep("TEXTO_TITULO_DA_COLUNA", names(BD))], margin=1, function(x) sum(x %in% c("VALOR_DA_COLUNA"), na.rm=T))
