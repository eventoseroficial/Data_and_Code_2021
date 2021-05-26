#Outras bibliotecas básicas
library(tidyverse)
library(glue)


#Requisitos do pacote ----------------------------------

# 1) Instalar e abrir a biblioteca

install.packages('basedosdados')
library(basedosdados)

# 2) Ter um projeto no Google Cloud e com isso uma billing-id! 
#Link de como criar :https://basedosdados.github.io/mais/access_data_local/#criando-um-projeto-no-google-cloud



#Funções do pacote ------------------------

## 1 função set_billing_id()
#define um id de cobrança, permitindo acesso ao Google Cloud e a BigQuery

basedosdados::set_billing_id("double-voice-305816")



#2 função download()
#baixa arquivos da nuvem para a máquina, em formato csv

basedosdados::download(query = 'SELECT * FROM `basedosdados.br_sp_seduc_inse.escola` ', 
                       path = 'bases/inse3.csv', page_size = 10000)


##de um jeito mais sucinto

download("SELECT * FROM `basedosdados.br_sp_seduc_idesp.uf`", 'bases/idesp.csv')




#3 função read_sql() 
#abre bases direto da nuvem sem precisar baixá-las para a sua máquina
basedosdados::read_sql(query = 'SELECT * FROM `basedosdados.br_sp_seduc_idesp.uf` ')


read_sql("SELECT * FROM `basedosdados.br_inpe_prodes.desmatamento_municipios`", page_size = 10000)




# Aplicações das funções -------------------------------------

#1) Descobrindo a correlação entre tamanho do rebanho bovino e o desmatamento


bovinos <- read_sql("SELECT * 
                    FROM `basedosdados.br_ibge_ppm.efetivo_rebanhos` 
                    WHERE tipo_rebanho = 'Bovino' and ano = 2019",
                    page_size = 10000)


desmatamento <- read_sql("SELECT id_municipio, ano, desmatado 
                          FROM `basedosdados.br_inpe_prodes.desmatamento_municipios` 
                          WHERE ano = 2019 ", page_size = 10000)


desmatamento%>%
  left_join(bovinos, by = c("id_municipio", "ano")) %>% 
  ggplot(aes(desmatado,quantidade_animais)) + geom_point()+
  scale_y_continuous(limits=c(0,1000000))



# nao precisamos nem abrir ambas as bases no R


base_unica <- read_sql("SELECT desm.id_municipio, desm.ano, desm.desmatado, boi.quantidade_animais
                        FROM `basedosdados.br_inpe_prodes.desmatamento_municipios` as desm
                        INNER JOIN `basedosdados.br_ibge_ppm.efetivo_rebanhos` as boi 
                        on boi.id_municipio = desm.id_municipio
                        WHERE desm.ano = 2019 and boi.ano=2019 and boi.tipo_rebanho = 'Bovino'", 
                        page_size = 10000)



base_unica%>%
  ggplot(aes(desmatado,quantidade_animais)) + geom_point()+
  scale_y_continuous(limits=c(0,1000000))


cor.test(base_unica$quantidade_animais, base_unica$desmatado)

lm(base_unica$desmatado ~ base_unica$quantidade_animais)


## 2) Nossos diretorios, e possiveis listas de diretorios com map + basedosdados ------------------------


#usando diretorio da bd+ para descobrir uma CID
dir_cid<- read_sql("SELECT * FROM `basedosdados.br_bd_diretorios_brasil.cid10`")



#criando lista de diretorios
lista_de_diretorios <- list("SELECT * FROM `basedosdados.br_bd_diretorios_brasil.cbo_1994`",
                            "SELECT * FROM `basedosdados.br_bd_diretorios_brasil.cbo_2002`" )

map(lista_de_diretorios, read_sql)




# 3) Uma base de várias bases ---------------------------------------------------


minha_fun<- function(x) {
  read_sql(glue('SELECT VA_industria, id_municipio 
                 FROM `basedosdados.br_ibge_pib.municipios` 
                 WHERE ano = {x}'))
}

lista_industrial <- map(2010:2012, minha_fun)


map(2013:2014, minha_fun)


#dicas para o uso do pacote -----------------------------

##dicas: 
#evitar baixar bases enormes para sua máquina sem necessidade
#variar o page size para bases muito grandes

#possivelmente será de ajuda: 
#site do basedosdados(mecanismo de busca):https://basedosdados.org/
#site do google big query(datalake em si):https://basedosdados.github.io/mais/access_data_bq/
#nosso tutorial escrito em R:https://dev.to/basedosdados/como-usar-a-biblioteca-basedosdados-no-r-capitulo-1-46kb





