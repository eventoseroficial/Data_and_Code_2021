# V Seminário Internacional de Estatística com R (SER)
# Palestra: Bibliometria com R: Aplicação e uso do pacote Bibliometrix
# Palestrantes: Eduardo Jangutta & Morgany Leite


#Passo 1: instalar ou selecionar pacote bibliometrix na biblioteca
library(bibliometrix)

#Passo 2: importando a primeira busca 
caminho1 <- "C:/Buscas/busca1_scopus.bib"
busca1 <- convert2df(caminho1, dbsource = "scopus", format = "bibtex")

#Passo 3: importando a segunda busca
caminho2 <- "C:/Buscas/busca2_savedrecs.bib"
busca2 <- convert2df(caminho2, dbsource = "wos", format = "bibtex")

#Passo 4: importando a terceira busca
caminho3 <- "C:/Buscas/busca3_scopus.bib"
busca3 <- convert2df(caminho3, dbsource = "scopus", format = "bibtex")

#Passo 5: importando a quarta busca
caminho4 <- "C:/Buscas/busca4_savedrecs.bib"
busca4 <- convert2df(caminho4, dbsource = "wos", format = "bibtex")

#Passo 6: unindo todos as buscas em um arquivo unico e removendo duplicados
Uniao_das_Buscas <- mergeDbSources(busca1, busca2, busca3, busca4, remove.duplicated=TRUE)

#Passo 7: Exportando arquivo para analisar na função biblioshiny
write.csv2(Uniao_Buscas, file = 'C:/Buscas/Export_from_R_CSV.csv', na="NA")

#Passo 8: Abir biblioshiny
biblioshiny()
