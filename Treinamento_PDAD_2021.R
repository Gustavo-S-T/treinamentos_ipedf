rm(list=ls()) 
commit()

##### Instalar e Carregar pacotes ####

#install.packages("tidyverse") # Instalar o pacote "tidyverse"
library(tidyverse) # Carregar o pacote "tidyverse"
library(data.table)
library(RODBC)

# Siga os passos abaixo para instalar os pacotes utilizados nesse curso
pacotes <- c("tidyverse","data.table","Hmisc","lubridate","ggrepel","sf","sidrar","survey",
             "srvyr","scales","ggthemr","readxl","kableExtra","psych","xtable")

if(length(setdiff(pacotes,installed.packages()))==0){}else{
  
  install.packages(setdiff(pacotes,installed.packages()))
  
}

#devtools::install_github('cttobin/ggthemr')

citation("tidyverse")

##### Operadores ####

1 + 2 # Adição
3 - 1 # Subtração
2 * 2 # Multiplicação
1 / 4 # Divisão

#Testes lógicos
2+2==4 # 2 + 2 é igual a 4?
2+2==5 # 2 + 2 é igual a 5? 
2+2&1+3==4 # 2 + 2 e 1 + 3 são iguais a 4?
2+2|1+4==5 # 2 + 2 ou 1 + 4 são iguais a 5?

c(2+2,1+4)==5 # 2 + 2 é igual a 5? 1 + 4 é igual a 5?

##### Funções ####

sum(1,2) # Adição
prod(2,2) # Multiplicação

psych::describe(c(1,2)) # Utilizando a função describe, do pacote "psych", sem carregá-lo

# Função para somar dois valores
somar <- 
  
  # Abrir argumento para criação de função e informar dentro dos parenteses os parâmetros
  function(x,y){
    
    # Construir a operação de soma
    z = x + y
    
    # Retornar o resultado
    return(z)
    
  }
#Aplicar a função
somar(2,4)

##### Tipos de Dados ####

# DOMINANTE character > complex > numeric > integer > logical RECESSIVO

x <- 1L # inteiro
class(x)
x <- 1 # numérico
class(x) 
x <- 8i # complexo
class(x)
x <- TRUE # lógico
class(x)
x <- "hello" # character
class(x)

x <- c(1,2,3) # Vetor numérico
typeof(x) # Checar o método de armazenagem
class(x) # Checar a classe
length(x) # Checar o tamanho
str(x) # Checar a estrutura

x <- 1:10
class(x)
as.numeric(x)
as.logical(x)
as.character(x)

x <- matrix(ncol = 2,nrow = 2) # Criar uma matriz  2 x 2
dim(x) # Checar as dimensões da matriz
x[,] <- c(1,7,9,6) # Atribuir valores à matriz
x <- 1:3 # Contruir um vetor x
y <- 10:12 # Construir um vetor z
z <- cbind(x,y) # Montar uma matriz a partir destes vetores (por colunas)
z <- rbind(x,y) # Montar uma matriz a partir destes vetores (por linhas)

m <- matrix(1:4,ncol = 2,nrow = 2) # Construir a matriz m
n <- matrix(5:8,ncol = 2,nrow = 2) # Construir a matriz n
t(m)      # matriz transposta de m
m %*% n   # multiplicação matricial de m por n
solve(m)  # matriz inversa de m


ista <- list(1,"galo",2i,2.61,TRUE,matrix(1:4)) # Criar uma lista
lista # Checar a lista criada
lista[[2]] # Acessar o segundo elemento da lista
lista <- list(a=1,b="galo",c=2i,d=2.61,e=TRUE,f=matrix(1:4)) # Criar uma lista, com nomes dos elementos


# Construir um fator
x <- factor(c("sim","sim","sim","não","sim","não"),levels = c("não","sim")) 
table(x) # Tabular os dados
levels(x) # Verificar os níveis dos fatores
class(x) # classe do objeto

# Construir um fator ordenado
x <- factor(c("ruim","bom","ruim","péssimo","ótimo"),
            levels = c("péssimo","ruim","bom","ótimo"),
            ordered = TRUE)
table(x) # Tabular os dados
levels(x) # Verificar os níveis dos fatores
class(x) # classe do objeto

# Construir um fator de variáveis com códigos
x <- factor(c(1,1,2,1,2,1,2),
            levels = c(1,2),
            labels = c("Sim","Não"))

table(x) # Tabular os dados
levels(x) # Verificar os níveis dos fatores
class(x) # classe do objeto


x <- factor(c(1,1,2,1,2,1,2,3,4),
            levels = c(4,3,2,1),
            labels = c("péssimo","ruim","bom","ótimo"),
            ordered = TRUE)
table(x) # Tabular os dados
levels(x) # Verificar os níveis dos fatores
class(x) # classe do objeto


1/Inf # Divisão de 1 por infinito
-1/0 # Infinito - divisão de 1 por zero (conceito de limite)
1/0-1/0 # Infinito menos infinito - Indefinido (não numérico)
x <- NA # Missing value (valor ausente)
is.na(x) # Validar se é NA
x <- NULL # Vazio
is.null(x) # Validar se é NULL

##### Controle de Fluxo #####

#IF
x <- 2
# Realizar uma soma se x é igual 2
if(x==2){
  x+2
}

# Realizar uma soma se x é igual a 3
if(x==3){
  x+2
}

#If Else
x <- 8
# Testar se x é positivo ou negativo
if(x < 0) {
  "negativo"
} else if(x == 0) {
  "zero"
} else if(x > 0) {
  "positivo"
}

# Outra maneira de escrever o código
#if(x<0){"negativo"}else if(x==0){"neutro"}else if(x>0){"positivo"}

#For
# Criar uma lista de índices
i <- 1:10
# Somar 1 aos valores de 5 a 10
for(i in c(5:10)){
  print(i+1)
}

#While
x <- 1
while (x<=10) {
  print (x)
  x=x+2
}


##### Pipe %>% ####

#Ele realiza o seguinte comando, de maneira bastante intuitiva:
#“use o resultado do lado esquerdo como argumento da função do lado direito”.

x <- c(1:10)
x %>% sum %>% sqrt %>% round

##### Ajuda ####

?psych::describe
help("describe")
help(describe)
?psych
help("psych")
help(psych)

##### Carga dados PDAD 2021 - Servidor ####

# Criar um novo diretório
#dir.create("dados")

# Listar as informações da URL
library(RCurl)

# Pegar informações da URL
info <- getURL("https://www.codeplan.df.gov.br/microdados-pdad-2021/",
               verbose=TRUE,
               ftp.use.epsv=TRUE, 
               dirlistonly = TRUE)

# Separar as informações em linhas
info <- str_split(info,"\n") %>% 
  unlist

# Extrair as informações do arquivo csv de interesse
b <- info[str_detect(info,".csv")]

# Pegar a URL, extraindo a informação entre aspas
b <- str_match(b, '["](.*?)["]')[3:4]

# Criar o caminho de destino, pegando o nome do arquivo
td <- paste0("dados/",str_extract(b,'Mor(.*?).csv$')[2])

# Fazer o download do csv
download.file(b[2], td, mode="wb")

# Fazer a mesma coisa para o arquivo de domicílios
td <- paste0("dados/",str_extract(b,'Dom(.*?).csv$')[1])

download.file(b[1], td, mode="wb")

# Fazer o download do dicionário em excel
d <- info[str_detect(info,".xlsx")]

# Pegar a url
d <- str_match(d, '["](.*?)["]')[2]

# Pegar o nome do arquivo
td <- paste0("dados/",str_extract(d,'Dic(.*?).xlsx$'))

# Fazer o download
download.file(d, td, mode="wb")

# Pegar os arquivos pdf da PDAD
pdf <- info[str_detect(info,"Man(.*?).pdf|Ques(.*?).pdf")] %>%
  # Pegar a URL
  str_match('href=(.*?).pdf') %>% 
  # Tratar a URL
  str_remove('href="') %>% 
  # Ficar com as linhas de interesse
  .[1:2]

# Baixar os dois arquivos
for(i in pdf){
  
  # Pegar o nome do arquivo
  nome <- str_extract(i,"7(.*?)$") %>% 
    str_remove("7/")
  
  # Criar o caminho do arquivo
  td <- paste0("dados/",nome)
  
  # Fazer o download
  download.file(i, td, mode="wb")
}

##### Carga dados PDAD 2021 - Ambiente #####

# Carregar a base de um link da internet.
pdad_2021_moradores <- data.table::fread("https://www.codeplan.df.gov.br/wp-content/uploads/2022/07/PDAD_2021-Moradores.csv",dec = ",",encoding = "Latin-1",
                                         data.table = FALSE,
                                         integer64 = "character")

# Carregar a base de um arquivo local
pdad_2021_moradores <- data.table::fread("dados/Moradores.csv",
                                         dec = ",",encoding = "Latin-1",
                                         data.table = FALSE,
                                         integer64 = "character")

#aa
##### Carga dados PDAD 2021 - Banco de dados ####

# Atenção! Não utilize a função Sys.getenv sem confirar o ambiente antes!
# no caso de dados sigilosos usar a função 
# usethis::edit_r_environ()
# com os valores de usario e senha fornecidos pelo admnistrador e o código que segue

# Abrir conexão com o banco de dados
db <- RODBC::odbcConnect("db_codeplan",
                         uid=Sys.getenv("usuario"),
                         pwd=Sys.getenv("senha"))

tabelas <- RODBC::sqlTables(db)

# A coluna TABLE_CAT informa o banco de origem, 
# TABLE_SCHEM informa o esquema da tabela, 
# TABLE_NAME o nome da tabela, 
# TABLE_TYPE o tipo e 
# REMARKS as observações, se houver alguma.

# Funções para observar as tabelas na base
class(tabelas) # Verificar a classe do objeto
names(tabelas) # Verificar o nome das colunas carregadas
head(tabelas) # Verificar as primeiras linhas da tabela
tail(tabelas) # Verificar as últimas linhas da tabela
str(tabelas) # Verificar as classes das colunas
dplyr::glimpse(tabelas) # Outra opção para checar as classes
nrow(tabelas) # Consultar o número de linhas
ncol(tabelas) # Consultar o número de colunas

#Funções para consultar colunas e linhas
tabelas$TABLE_SCHEM # Consultar apenas a coluna "TABLE_SCHEM"
tabelas[,2] # Outra maneira de consultar a coluna "TABLE_SCHEM"
tabelas[,"TABLE_SCHEM"] # Mais uma maneira de consultar a coluna "TABLE_SCHEM"
tabelas[1:3,1:2] # Selecionar apenas as duas primeiras linhas e colunas
tabelas[1:2,]$TABLE_SCHEM # Selecionar apenas as duas primeiras linhas da coluna "TABLE_SCHEM"
tabelas[c(1,7,10:12),c(1,3)] # Selecionar linhar e colunas distintas

#Função para sumarizar as informações da base
table(tabelas$TABLE_SCHEM) # Tabular a coluna
table(tabelas[,2]) # Tabular a coluna
table(tabelas[,"TABLE_SCHEM"]) # Tabular a coluna

#Funções para filtrar as colunas da base 
tabelas[tabelas$TABLE_SCHEM=="pdad",3]
tabelas[tabelas[,2]=='pdad',]$TABLE_NAME

#Utilizando o dplyr para fazer os filtros e seleções
library(dplyr)
# Consultar as tabelas disponíveis
RODBC::sqlTables(db) %>%
  # Filtrar apenas linhas do esquema PDAD
  dplyr::filter(TABLE_SCHEM=="pdad") %>%
  # Selecionar apenas a coluna com as tabelas
  dplyr::select(TABLE_NAME)

#Alteração dos nomes das colunas do DB

colnames(tabelas)[2] <- "Esquema"
names(tabelas)[2] <- "Esquema"
# Verificar o resultado
names(tabelas)
# Alterar o nome com o dplyr
RODBC::sqlTables(db) %>%
  # Filtrar apenas linhas do esquema PDAD
  dplyr::filter(TABLE_SCHEM=="pdad") %>%
  # Alterar os nomes das colunas
  dplyr::rename(Nome=TABLE_NAME,
                Banco=TABLE_CAT,
                Esquema=TABLE_SCHEM,
                Tipo=TABLE_TYPE,
                Observações=REMARKS)

colunas <- RODBC::sqlColumns(db,"pdad.mor2021")

# Consultar as colunas da mor2021
RODBC::sqlColumns(db,"pdad.mor2021") %>%
  # Selecionar a coluna "COLUMN_NAME"
  dplyr::select(COLUMN_NAME)

pdad_2021_moradores <- RODBC::sqlQuery(db,"select A01ra from pdad.mor2021")

##### Alternativa DBI - Carga dados PDAD 2021 - Banco de dados  ####

db <- DBI::dbConnect(odbc::odbc(), "db_codeplan",
                     uid=Sys.getenv("usuario"),
                     pwd=Sys.getenv("senha"))

DBI::dbListTables(db) # Consultar tabelas
DBI::dbListFields(db,"mor2018") # Consultar colunas da tabela mor_2018

pdad_2021_moradores <- DBI::dbGetQuery(db,"select A01ra from pdad.mor2021")

##### Tabulação e Tratamento em SQL ####

#Dados codificados é necessário o dicionário para interpretar
table(pdad_2021_moradores$A01ra)

#Carregando base completa
pdad_2021_moradores <- DBI::dbGetQuery(db,"select * from pdad.mor2021")

# Listar os arquivos do diretório dados para pegar o nome certinho do arquivo
list.files("dados")
# Carregar as informações do dicionário
dic_moradores <- readxl::read_excel("dados/Dicionario_de_variaveis_PDAD-2021.xlsx",
                                    skip = 0,
                                    sheet = 2)

# Adicionando a descrição das colunas baseado no dicionário carregado
# Carregar o pacote
library(Hmisc)
# Criar um objeto com os rótulos
var.labels <- dic_moradores$`Descrição da coluna` %>%
  # Retirar as linhas ausentes
  na.omit
# Nomear esses rótulos com o nome das variáveis do nosso banco de dados  
names(var.labels) <- names(pdad_2021_moradores)
# Adicionar os rótulos ao nosso banco de dados
pdad_2021_moradores <- Hmisc::upData(pdad_2021_moradores, labels = var.labels)
# Verificar o resultado
View(pdad_2021_moradores)

# Formas de decodificar as informação da coluna
# Manualmente - Utilizando as funções trasmute/mutate e factor 
RA <- pdad_2021_moradores %>%
  dplyr::transmute(RA=factor(A01ra,
                             levels=1:33,
                             labels=c('Plano Piloto',      
                                      'Gama',
                                      'Taguatinga',
                                      'Brazlândia',
                                      'Sobradinho',
                                      'Planaltina',
                                      'Paranoá',
                                      'Núcleo Bandeirante',
                                      'Ceilândia',
                                      'Guará',
                                      'Cruzeiro',
                                      'Samambaia',
                                      'Santa Maria',
                                      'São Sebastião',
                                      'Recanto das Emas',
                                      'Lago Sul',
                                      'Riacho Fundo',
                                      'Lago Norte',
                                      'Candangolândia',
                                      'Águas Claras',
                                      'Riacho Fundo II',
                                      'Sudoeste/Octogonal',
                                      'Varjão',
                                      'Park Way',
                                      'SCIA-Estrutural',
                                      'Sobradinho II',
                                      'Jardim Botânico',
                                      'Itapoã',
                                      'SIA',
                                      'Vicente Pires',
                                      'Fercal',
                                      'Sol Nascente/Pôr do Sol',
                                      'Arniqueira')))
# Tabular os resultados
table(RA$RA)

# Automatica via dicionário de variaveis - Utilizando as funções trasmute/mutate e factor 
# Carregar somente as informações da coluna RA
dic_ra <- readxl::read_excel("dados/Dicionario_de_variaveis_PDAD-2021.xlsx",
                             skip = 0,
                             sheet = 3)
# Recodificar os nomes
ra_codificada <- pdad_2021_moradores %>%
  dplyr::transmute(ra=factor(A01ra,
                             levels = dic_ra$Valor,
                             labels = dic_ra$`Descrição do valor`))

# Tabular os resultados
table(ra_codificada$ra)

##### Gráficos ggplot2 ####
library(ggplot2)

RA %>%
  # Criar a área de plotagem, com o eixo X
  ggplot(aes(x=RA)) +
  # Inserir a geometria do tipo "Barra", com a opção de contagem (gerada automaticamente no eixo y)
  geom_bar(stat = "count") +
  # Inverter os eixos
  coord_flip()

# Contar quantas pessoas foram amostradas em cada RA
RA %>%
  # Contar quantas observações temos em cada RA
  dplyr::count(RA) %>%
  # Plotar o gráfico, ajustando as categorias de acordo com o total amostrado
  ggplot(aes(x=forcats::fct_reorder(RA,n),y=n)) +
  # Desenhar a geometria de barras
  geom_bar(stat = "identity") +
  # Inverter os eixos
  coord_flip() +
  # Rotular os eixos
  labs(y="Amostra",
       x="Região Administrativa")

ggthemr::ggthemr()

##### Carga da base domicilios - Banco de dados ####

pdad_2021_domicilios <- RODBC::sqlQuery(db,"select * from pdad.dom2021")

# Carregar as informações do dicionário
dic_domiclios <- readxl::read_excel("dados/Dicionario_de_variaveis_PDAD-2021.xlsx",
                                    skip = 0, sheet = 1)

# Criar um objeto com os rótulos
var.labels <- dic_domiclios$`Descrição da coluna` %>%
  na.omit
# Nomear esses rótulos com o nome das variáveis do nosso banco de dados  
names(var.labels) <- names(pdad_2021_domicilios)
# Adicionar os rótulos ao nosso banco de dados
pdad_2021_domicilios <- Hmisc::upData(pdad_2021_domicilios, labels = var.labels)
# Verificar o resultado
View(pdad_2021_domicilios)

##### Trabalhando com datas ####

# Carregar pacote lubridate
library(lubridate)

# Utilizar a base de domicílios
pdad_2021_domicilios %>%
  # Selecionar a data da pesquisa
  dplyr::select(endtime) %>%
  # Transformar o campo de data (em caracter) em data (formato data)
  dplyr::mutate(endtime=lubridate::ymd(endtime),
                # Extrair o valor do mês
                MES=lubridate::month(endtime,label=T)) %>%
  # Agrupar por mÊs e contar a quantidade de casos por mÊs
  dplyr::count(MES) 

# Criar uma variável com o nome das RAs na base de domicílios
pdad_2021_domicilios <- pdad_2021_domicilios %>%
  dplyr::mutate(RA=factor(A01ra,
                          levels=1:33,
                          labels=c('Plano Piloto',      
                                   'Gama',
                                   'Taguatinga',
                                   'Brazlândia',
                                   'Sobradinho',
                                   'Planaltina',
                                   'Paranoá',
                                   'Núcleo Bandeirante',
                                   'Ceilândia',
                                   'Guará',
                                   'Cruzeiro',
                                   'Samambaia',
                                   'Santa Maria',
                                   'São Sebastião',
                                   'Recanto das Emas',
                                   'Lago Sul',
                                   'Riacho Fundo',
                                   'Lago Norte',
                                   'Candangolândia',
                                   'Águas Claras',
                                   'Riacho Fundo II',
                                   'Sudoeste/Octogonal',
                                   'Varjão',
                                   'Park Way',
                                   'SCIA-Estrutural',
                                   'Sobradinho II',
                                   'Jardim Botânico',
                                   'Itapoã',
                                   'SIA',
                                   'Vicente Pires',
                                   'Fercal',
                                   'Sol Nascente/Pôr do Sol',
                                   'Arniqueira')))

# Utilizar a base de domicílios
coleta <- pdad_2021_domicilios %>%
  # Selecionar a data da pesquisa
  dplyr::select(RA,endtime) %>%
  # Transformar o campo de data (em caracter) em data (formato data)
  dplyr::mutate(endtime=lubridate::ymd(endtime),
                # Extrair o valor do mês
                MES=lubridate::month(endtime,label=T)) %>%
  # Agrupar e contar por RA e por  Mês
  dplyr::count(RA,MES)

# Visualizar resultado
View(coleta)

##### Escrevendo tabelas em arquivos csv e xlxs ####
write.table(coleta,"dados/coleta.csv",
            row.names = F, sep = ";",
            fileEncoding = "latin1")

# Carregar o pacote
library(xlsx)

# Salvar as informações
xlsx::write.xlsx(coleta,
                 file = "dados/coleta.xlsx",
                 sheetName = "Coleta",
                 row.names = F)

##### Criando um grafico com datas ####
# Criar um objeto com a data de referência da pesquisa
ref <- lubridate::dmy("01-07-2021")

# Armazenar o resultado em um objeto
referencias <- 
  # Utilizar a base de domicílios
  pdad_2021_domicilios %>%
  # Selecionar a data da pesquisa
  dplyr::select(endtime) %>%
  # Transformar o campo de data (em caracter) em data (formato data)
  dplyr::mutate(endtime=lubridate::ymd(endtime),
                # Calcular a diferença entre as datas, em meses
                dif_data_mes=interval(ref,endtime) %/% months(1),
                # Calcular a diferença entre as datas, em dias
                dif_data_dia=interval(ref,endtime) %/% days(1))


# Utilizar o objeto de referências
referencias %>%
  # Criar um plot com a diferença de dias no eixo x
  ggplot(aes(x=dif_data_dia)) +
  # Fazer o gráfico de densidade
  geom_density() +
  # Adicionar uma linha vermelha vertical no ponto zero
  geom_vline(aes(xintercept= 0), color="red")+
  # Nomear os eixos
  labs(y="Densidade",
       x="Diferença de dias da pesquisa")


##### Atualização de valores monetarios #####

# Carregar pacote sidrar - utilizar dados do IBGE
library(sidrar)
# Carregar a inflação mensal do DF em um objeto
inflacao <- sidrar::get_sidra(api = '/t/7060/n6/5300108/v/63/p/all/c315/7169/d/v63%202')
# Guardar os inflatores em um objeto
inflacao_df <- inflacao %>%
  # Filtrar para o mês de início da PDAD
  dplyr::filter(`Mês (Código)`>=202105&`Mês (Código)`<=202112) %>%
  # Organizar dados em ordem cronológica decrescente
  dplyr::arrange(desc(`Mês (Código)`)) %>%
  # Calular o inflator, acumulando os índices mensais
  dplyr::mutate(inflator=cumprod(Valor/100+1)) %>%
  # Selecionar a referência e o inflator calculado
  dplyr::select(`Mês (Código)`,inflator) %>%
  # Criar uma variável para o mes e para o ano do inflator,
  # necessária para cruzar com as informações da PDAD,
  # mantendo a variável de inflator no objeto
  dplyr::transmute(mes=as.numeric(substr(`Mês (Código)`,5,6)),
                   ano=as.numeric(substr(`Mês (Código)`,1,4)),
                   inflator=inflator)


# Carregar informações da inflação para as localidades desejadas
inflacao <- sidrar::get_sidra(api = '/t/7060/n1/all/n7/3301,3501/n6/5300108/v/63/p/all/c315/7169/d/v63%202')

# Gravar a inflação transformada em um objeto  
inflacao_wide <- inflacao %>%
  # Selecionar as variáveis de interesse
  dplyr::select(`Mês (Código)`,`Brasil, Região Metropolitana e Município`,Valor) %>%
  # Renomear as variáveis selecionadas anteriormente
  dplyr::rename_all(list(~c("referencia","Local","Valor"))) %>%
  # Mudar dados para o formato wide.
  tidyr::spread(Local,Valor)


# Criar um objeto com os dados no formato long
inflacao_long <- inflacao_wide %>%
  # Passar as colunas de cada localidade para o formato long, criando
  # as variáveis "Local" e "Valor" para receberem os dados de inflação
  tidyr::gather("Local","Valor",-1)


inflacao_long %>%
  # Ajustar a variável de referência para o formato data,
  # utilizando as informações de mês e ano, acrescentando
  # o dia primeiro, apenas como referência
  dplyr::mutate(referencia=lubridate::dmy(paste("01",
                                                stringr::str_sub(referencia,5,6),
                                                stringr::str_sub(referencia,1,4)))) %>%
  # Filtrar para o último ano (de maio a maio)
  dplyr::filter(referencia>=lubridate::dmy("01-12-2018")) %>%
  # Plotar o gráfico, com a referência no eixo x,
  # a inflação no eixo y e a localidade colorindo as linhas
  ggplot(aes(x=referencia,y=Valor,colour=Local))+
  # Construir as linhas, variando o tipo de linha conforme o local
  geom_line(aes(linetype = Local))+
  # Adicionar os pontos
  geom_point()+
  # Ajustar os rótulos dos meses do eixo x, apresentando-os
  # a cada dois meses
  scale_x_date(date_breaks = "2 month")+
  # Ajustar a legenda das cores, atribuindo cores específicas para as linhas
  scale_colour_manual(labels=c("Brasil","Distrito Federal",
                               "Rio de Janeiro","São Paulo"),
                      values=c("cadetblue4","coral4",
                               "darkgoldenrod","chartreuse4"))+
  # Ajustar a legenda das linhas, combinando com a legenda anterior
  scale_linetype_manual(labels=c("Brasil","Distrito Federal",
                                 "Rio de Janeiro","São Paulo"),
                        values=c(1:4))+
  # Ajustar o rótulo dos eixos
  labs(y="Inflação mensal",
       x="Período")+
  # Alterar a posição da legenda
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle=90))


# Armazenar informação em um objeto
renda_domiciliar <- pdad_2021_moradores %>%
  # Vamos mudar para ausente os valores das variáveis I04_1_1,I04_2_1,I20,I21,I22_1,I22_2
  # com códigos 77777 ou 88888.
  # Vamos também mudar para 0 quando os valores que não se aplicarem
  # ou não forem observados rendimentos
  dplyr::mutate_at(vars(I04_1_1,I04_2_1,I20,I21,I22_1,I22_2), # Variáveis a serem alteradas
                   # Função a ser aplicada
                   list(M=~case_when(. %in% c(77777,88888)~NA_real_,
                                     . %in% c(66666,99999)~0,
                                     TRUE~as.numeric(.)))) %>%
  # Selecionar apenas as variáveis de interesse
  dplyr::select(A01nficha,E05,I04_1_1,I04_2_1,I20,I21,I22_1,I22_2,
                I04_1_1_M:I22_2_M) %>%
  # Somar as variáveis modificadas para construir a renda individual
  dplyr::mutate(renda_individual=rowSums(.[,c("I04_1_1_M","I04_2_1_M",
                                              "I20_M","I21_M",
                                              "I22_1_M","I22_2_M")],na.rm = F)) %>%
  # Desconsiderar os empregados domesticos moradores e seus parentes
  dplyr::filter(!E05 %in% c(17,18,19)) %>%
  # Agrupar por domicílio
  dplyr::group_by(A01nficha) %>%
  # Somar os valores por domicílios
  dplyr::summarise(renda_dom=sum(renda_individual, na.rm = F),
                   # Construir o número de pessoas no domicílio, por esse critério de rendiment0
                   pessoas=n(),
                   # Calcular a renda domiciliar per capita
                   renda_dom_pc=renda_dom/pessoas)



mean(renda_domiciliar$renda_dom, na.rm=T)


##### PDAD com expansão de DADOS ####

# Fazer o join das bases
pdad <- pdad_2021_moradores %>%
  # Entrar com a função para left join
  dplyr::left_join(
    # Informar a base que iremos unir, filtrando para colunas repetidas
    pdad_2021_domicilios %>%
      dplyr::select(-c(A01ra,A01setor)))

#Juntar os dados de renda
pdad <- pdad %>%
  dplyr::mutate(endtime=lubridate::ymd(endtime),
                # Extrair o valor do mês
                mes=lubridate::month(endtime,label=F),
                # Extratir o valor do ano
                ano=lubridate::year(endtime)) %>%
  # Trazer as informações de renda
  dplyr::left_join(renda_domiciliar) %>%
  # Trazer as infomações de inflação
  dplyr::left_join(inflacao_df) %>%
  # Criar as variáveis monetárias em termos reais
  dplyr::mutate(renda_dom_real=renda_dom*inflator,
                renda_dom_pc_real=renda_dom_pc*inflator)

# Carregar os pacotes necessários
library(survey)
library(srvyr)

# Declarar o desenho amostral
sample.pdad <- 
  survey::svydesign(id = ~A01nficha, # Identificador único da unidade amostrada
                    strata = ~A01setor, # Identificação do estrato
                    weights = ~PESO_PRE, # Inverso da fração amostral
                    nest=TRUE, # Parâmetro de tratamento para os IDs dos estratos
                    data=pdad # Declarar a base a ser utilizada
  )

# Criar um objeto para pós estrato
post.pop <- pdad %>%
  dplyr::group_by(POS_ESTRATO) %>% # Agrupar por pós-estrato
  dplyr::summarise(Freq=max(POP_AJUSTADA_PROJ)) # Capturar o total da população

# Declarar o objeto de pós-estrato
# Estamos dizendo nesse passo qual é a população alvo para cada
# pós-estrato considerado
sample.pdad <- survey::postStratify(sample.pdad,~POS_ESTRATO,post.pop)

# Criar objeto para calcular os erros por bootstrap (Rao and Wu’s(n − 1) bootstrap)
# J. N. K. Rao and C. F. J. Wu - Journal of the American Statistical Association
# Vol. 83, No. 401 (Mar., 1988), pp. 231-241
amostra <- survey::as.svrepdesign(sample.pdad, type = "subbootstrap")

# Ajustar para tratamento de estratos com apenas uma UPA (adjust=centered)
options(survey.lonely.psu = "adjust")

# Ajustar objeto de amostra, para uso com o pacote srvyr (como tibble)
amostra <- srvyr::as_survey(amostra)

# População DF com mais de 18 anos
pop18 <- 
  amostra %>%
  # Filtrar somente a população com 18 anos ou mais de idade
  srvyr::filter(idade>=18) %>%
  # Criar uma variável auxiliar para contagem
  srvyr::mutate(count=1) %>%
  # Calcular o total da população, com seu intervalo de confiança
  srvyr::summarise(n=survey_total(count, vartype = "cv"))

# Distribuição por sexo de menors de 18 anos
amostra %>%
  # Filtrar somente a população com 18 anos ou mais de idade, retirando os códigos de não informação
  srvyr::filter(idade>=18) %>%
  # Ajustar a variável de sexo
  srvyr::mutate(E04=factor(case_when(E04==1~"Masculino",
                                     TRUE~"Feminino"))) %>%
  # Informar o grupo que queremos a informação
  srvyr::group_by(E04) %>%
  # Calcular o total e o Percentual da população, com seu intervalo de confiança
  srvyr::summarise(n=survey_total(vartype = "cv"),
                   # Calcular o percentual da população
                   pct=survey_mean(vartype = "cv"))

# Fazer o desenho de domicílios
amostra_dom <- survey::svydesign(id = ~A01nficha, # Identificador do domicílio
                                 strata = ~A01setor, # Identificação do estrato
                                 weights = ~PESO_PRE, # Inverso da fração amostral
                                 nest=TRUE,  #Parâmetro de tratamento para os IDs dos estratos
                                 data=pdad %>%
                                   dplyr::filter(E05==1) # Base de dados, pegando apenas uma informação por domíclio.
)

#Criar um objeto para pós estrato
post.pop <- pdad %>%
  dplyr::filter(E05==1) %>% # Filtrar somente para uma obervação
  dplyr::group_by(A01setor) %>% # Agruprar por estrato
  dplyr::summarise(Freq=first(TOT_DOM)) %>% # Pegar uma única informação do total de domicílios no estrato
  dplyr::ungroup() # Desgrupar a base

# Declarar o objeto de pós-estrato
amostra_dom <- survey::postStratify(amostra_dom,~A01setor,post.pop)

# Criar objeto para calcular os erros por bootstrap (Rao and Wu’s(n − 1) bootstrap)
amostra_dom <- survey::as.svrepdesign(amostra_dom, type = "subbootstrap")

# Criar o objeto amostra ddo tipo tibble
amostra_dom <- srvyr::as_survey(amostra_dom)

amostra_dom %>%
  # Retirar os valores zerados
  srvyr::filter(renda_dom!=0) %>% 
  # Calcular a renda domiciliar real média do DF
  srvyr::summarise(renda_dom_real=survey_mean(renda_dom_real,na.rm=T,vartype="cv"),
                   renda_dom=survey_mean(renda_dom,na.rm=T,vartype="cv"))


##### Algumas manipulações e gráficos usuais ####

# Criar um objeto com o salário mínimo em 2021
sm <- 1100

# Criar um objeto com as variáveis de interesse
vars_relatorio <- amostra %>%
  # Criar variável de sexo
  srvyr::mutate(sexo=case_when(E04==1~"Masculino",
                               E04==2~"Feminino"),
                # Criar variável de faixas de idade
                idade_faixas=cut(idade,
                                 breaks = c(-Inf,seq(4,74,by=5),Inf),
                                 labels = c("0 a 4 anos","5 a 9 anos",
                                            "10 a 14 anos","15 a 19 anos",
                                            "20 a 24 anos","25 a 29 anos",
                                            "30 a 34 anos","35 a 39 anos",
                                            "40 a 44 anos","45 a 49 anos",
                                            "50 a 54 anos","55 a 59 anos",
                                            "60 a 64 anos","65 a 69 anos",
                                            "70 a 74 anos","Mais de 75 anos"),
                                 ordered_result = T),
                # Criar variável de faixas de salário do trabalho principal
                faixas_salario=cut(case_when(I20 %in% c(77777,88888,99999)~NA_real_,
                                             TRUE~as.numeric(I20)),
                                   breaks = c(-Inf,sm,2*sm,4*sm,10*sm,20*sm,Inf),
                                   labels = c("Até 1 salário","Mais de 1 até 2 salários",
                                              "Mais de 2 até 4 salários",
                                              "Mais de 4 até 10 salários",
                                              "Mais de 10 até 20 salários",
                                              "Mais de 20 salários")),
                # Criar variável para as RAs
                RA=factor(A01ra,
                          levels=1:33,
                          labels=c('Plano Piloto',      
                                   'Gama',
                                   'Taguatinga',
                                   'Brazlândia',
                                   'Sobradinho',
                                   'Planaltina',
                                   'Paranoá',
                                   'Núcleo Bandeirante',
                                   'Ceilândia',
                                   'Guará',
                                   'Cruzeiro',
                                   'Samambaia',
                                   'Santa Maria',
                                   'São Sebastião',
                                   'Recanto das Emas',
                                   'Lago Sul',
                                   'Riacho Fundo',
                                   'Lago Norte',
                                   'Candangolândia',
                                   'Águas Claras',
                                   'Riacho Fundo II',
                                   'Sudoeste/Octogonal',
                                   'Varjão',
                                   'Park Way',
                                   'SCIA-Estrutural',
                                   'Sobradinho II',
                                   'Jardim Botânico',
                                   'Itapoã',
                                   'SIA',
                                   'Vicente Pires',
                                   'Fercal',
                                   'Sol Nascente/Pôr do Sol',
                                   'Arniqueira'))) %>%
  # Transformar em fator variáveis do tipo character
  srvyr::mutate_if(is.character,list(~factor(.))) %>%
  # Selecionar as variáveis criadas e algumas variáveis auxiliares
  srvyr::select(RA,E05,idade,I05,sexo,idade_faixas,faixas_salario)



# Construir um objeto com as idades calculadas, por faixas de idade e sexo
# para montarmos a pirâmide etária
piramide <- vars_relatorio %>%
  # Agrupar por faixas de idade e sexo
  srvyr::group_by(idade_faixas,sexo) %>%
  # Calcular os totais
  srvyr::summarise(n=survey_total(na.rm = T, vartype = "ci"))

# Fazer o gráfico com a pirâmide
piramide_grafico <- piramide %>%
  # Construir um plot com as idades no eixo x, as quantidades no eixo y,
  #  preenchimento com a variável sexo, e os intervalos de confiança
  # inferiores e superiores
  ggplot(aes(x=idade_faixas,y=n, fill=sexo, ymin=n_low,ymax=n_upp))+
  # Fazer o gráfico de barras para o sexo Feminino
  geom_bar(data = dplyr::filter(piramide, sexo == "Feminino"),
           stat = "identity") +
  # Fazer o gráfico de barras para o sexo Masculino
  geom_bar(data = dplyr::filter(piramide, sexo == "Masculino"),
           stat = "identity",
           position = "identity",
           # Negativar os valores para espelhar no eixo
           mapping = aes(y = -n))+
  # Plotar os erros para o sexo Masculino, negativando os valores para espelhar o eixo
  geom_errorbar(data = dplyr::filter(piramide, sexo == "Masculino"),
                mapping = aes(ymin = -n_low,ymax=-n_upp),
                width=0,
                color="black")+
  # Plotar os erros para o sexo Feminino
  geom_errorbar(data = dplyr::filter(piramide, sexo == "Feminino"),
                width=0,
                color="black")+
  # Inverter os eixos, fazendo com que o gráfico de colunas verticais fique
  # horizontal
  coord_flip() + 
  # Ajustar as configurações de escala
  scale_y_continuous(labels = function(x) format(abs(x), 
                                                 big.mark = ".",
                                                 scientific = FALSE,
                                                 decimal.mark=",")) +
  # Suprimir os nomes dos eixos
  labs(x="",y="") +
  # Suprimir o nome da legenda
  scale_fill_discrete(name = "")

# Plotar gráfico
piramide_grafico


# Construir um objeto com as informações de salário
salario <- vars_relatorio %>%
  # Agrupar por faixas de salário
  srvyr::group_by(faixas_salario) %>%
  # Calcular os totais para cada grupo de salário
  srvyr::summarise(n=survey_total(na.rm=T,vartype = "ci")) %>% 
  # Retirar NA
  na.omit

# Construir um objeto com o gráfico
salario_grafico <- salario %>%
  # Plotar os eixos x e y
  ggplot(aes(x=faixas_salario, y=n))+
  # Construir o gráfico de barras
  geom_bar(stat = "identity") +
  # Construir as barras de erro
  geom_errorbar(aes(ymin=n_low,ymax=n_upp,size=4, width=0), color="darkred")+
  # Inverter os eixos
  coord_flip()+
  # Suprimir o nome dos eixos
  labs(x="",y="")+
  # Retirar o título da legenda
  theme(legend.position="none")+
  # Ajustar as formatações de escala
  scale_y_continuous(labels = function(x) format(abs(x), 
                                                 big.mark = ".",
                                                 scientific = FALSE,
                                                 decimal.mark=","))

# Plotar gráfico
salario_grafico


# Carregar o pacote Scales
library(scales)

# Construir o objeto com os valores
salario2 <- vars_relatorio %>%
  # Filtrar somente para casos válidos
  srvyr::filter(is.na(faixas_salario)==FALSE) %>% 
  # Agrupar por RA e faixas de salário
  srvyr::group_by(RA,faixas_salario) %>%
  # Calcular as proporções por faixa de salário
  srvyr::summarise(n=survey_mean(na.rm=T,vartype = "ci")) %>% 
  # Retirar NA
  na.omit

# Construir o gráfico
salario2 %>%
  # Plotar os eixos x e y
  ggplot(aes(x=faixas_salario, y=n))+
  # Construir o gráfico de barras
  geom_bar(stat = "identity") +
  # Construir o gráfico com os erros
  geom_errorbar(aes(ymin=n_low,ymax=n_upp, group=RA),size=1,width=0, color="darkred")+
  # Inverter os eixos
  coord_flip()+
  # Suprimir o nome dos eixos
  labs(x="",y="")+
  # Suprimir o nome da legenda
  theme(legend.position="none")+
  # Ajustar as formatações de escala
  scale_y_continuous(labels = scales::percent)+
  # Plotar o gráfico para cada uma das RAs, divididas em 4 colunas
  facet_wrap(.~RA, ncol=4)

# Construir o objeto com o esgotamento sanitário
esgotamento <- amostra_dom %>%
  srvyr::mutate(# Criar variável de esgotamento sanitário
    esgotamento_caesb=factor(case_when(B14_1==1~"Com Rede Geral (Caesb)",
                                       B14_1==2~"Sem Rede Geral (Caesb)"))) %>% 
  # Agrupar por situação de esgotamento sanitário
  srvyr::group_by(esgotamento_caesb) %>%
  # Calcular a proporção de cada grupo
  srvyr::summarise(n=survey_mean(na.rm = T,vartype = "ci"))

# Construir o objeto com o gráfico
esgotamento_grafico <- esgotamento %>%
  # filtrar para valores válidos
  srvyr::filter(is.na(esgotamento_caesb)==FALSE) %>% 
  # Plotar os eixos x e y, reordenando os fatores, do maior para o menor resultado
  ggplot(aes(x=fct_reorder(esgotamento_caesb,-n),y=n,ymin=n_low,ymax=n_upp))+
  # Construir o gráfico de barras
  geom_bar(stat = "identity")+
  # Construir os erros
  geom_errorbar(size=4, width=0,
                color="black")+
  # Ajustar os nomes dos eixos
  labs(x="",y="% Domicílios")+
  # Retirar o nome da legenda
  theme(legend.position="none")+
  # Ajustar a formatação dos rótulos
  scale_y_continuous(labels = scales::percent)+
  # Inserir informações dos resultados no gráfico
  geom_text(aes(label = paste0(round(100*n,0),"%")),
            size=4, fontface = "bold", 
            vjust = -0.25,hjust=1.25)

# Plotar grafico
esgotamento_grafico


# Carregar o pacote ggrepel
library(ggrepel)

# Construir o objeto com as informações de esgotamento sanitário
esgotamento2 <- amostra_dom %>%
  srvyr::mutate(# Criar variável de esgotamento sanitário
    esgotamento_caesb=factor(case_when(B14_1==1~"Com Rede Geral (Caesb)",
                                       B14_1==2~"Sem Rede Geral (Caesb)"))) %>% 
  # Filtar para o responsável
  srvyr::filter(is.na(esgotamento_caesb)==FALSE) %>%
  # Agrupar por tipo de esgotamento
  srvyr::group_by(esgotamento_caesb) %>%
  # Calcular as proporções
  srvyr::summarise(n=survey_mean(na.rm = T,vartype = "ci")) %>%
  # Deixar as informações em ordem decrescente
  dplyr::arrange(-n) %>%
  # Construir uma variável auxiliar, com a posição do label
  dplyr::mutate(pos=cumsum(n)-n/8)

# Criar o tema branco, eliminando todos os elementos gráficos padrões
tema_branco <- theme_minimal()+
  theme(
    # Retirar título do eixo x
    axis.title.x = element_blank(),
    # Retirar título do eixo y
    axis.title.y = element_blank(),
    # Retirar as bordas no painel
    panel.border = element_blank(),
    # Retirar elementos textuais do eixo y
    axis.text.y = element_blank(),
    # Retirar demais elementos textuais dos eixos
    axis.text = element_blank(),
    # Retirar as linhas de grade
    panel.grid=element_blank(),
    # Retirar os ticks
    axis.ticks = element_blank())

# Construir o objeto com as informações de esgotamento sanitário
amostra_dom %>%
  srvyr::mutate(# Criar variável de esgotamento sanitário
    esgotamento_caesb=factor(case_when(B14_1==1~"Com Rede Geral (Caesb)",
                                       B14_1==2~"Sem Rede Geral (Caesb)"))) %>% 
  # Filtar para o responsável
  srvyr::filter(is.na(esgotamento_caesb)==FALSE) %>%
  # Agrupar por tipo de esgotamento
  srvyr::group_by(esgotamento_caesb) %>%
  # Calcular as proporções
  srvyr::summarise(n=survey_mean(na.rm = T,vartype = "ci")) %>%
  # Gerar uma área de plotage,
  ggplot()+
  # Gerar a geometria da barra
  geom_bar(aes(x=1,y=n,fill=esgotamento_caesb),stat = "identity",alpha=0.5) +
  # Adicionar os percentuais ao gráfico, na forma de texto
  geom_text(aes(x=1, label = paste0(format(abs(round(n*100)), 
                                           big.mark = ".",
                                           scientific = FALSE,
                                           decimal.mark=","),"%"),
                y= n,
                group = esgotamento_caesb),
            size=4, 
            fontface = "bold",
            color = "black",
            position = position_stack(vjust = .5)) +
  # Retirar o nome da legenda
  scale_fill_discrete(name="")+
  # Ajustar o label da escala y
  scale_y_continuous(labels = scales::percent)+
  # Retirar o rótulo dos eixos
  labs(x="",y="")+
  # Retirar as informações do eixo x
  theme(axis.line.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.x=element_blank())


##### #####
