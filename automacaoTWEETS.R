### AUTOMAÇÃO DE EXTRAÇÃO DE DADOS DO TWITTER ###

# Julia Adams & Carlos Chiarelli

#######################

puxaTWEET <- function(){
  # Essa função extrai tweets do Twitter API.
  
  # Biblioteca para extrair os dados.
  biblio <- c(
    "rtweet",
    "magrittr"
  )
  
  # Instalando e importando os pacotes.
  for(pacote in biblio){
    
    # Checar se os pacotes estão instalados.
    if( !(pacote %in% rownames(installed.packages()))  )
      install.packages(pacote)
    
    library(pacote, character.only = TRUE)
  }
  
  # Acesso ao Twitter API.
  create_token(
    app = "RPrepositionTestApp",
    consumer_key <- "WE33fxF5SDQeOaCO1wbqpIzog",
    consumer_secret <-"MZ5Gukb4ZD8fO76fQ02xv0iOrejtx4CEzK2irTj1qQE2EOymtB",
    access_token <- "1100203825527287809-QDgVkxQ9nCZL6bG0UQQqtT3AHE1007",
    access_secret <- "ihroRFd0Nm9PDD0JMOOdixmw7jBmxj83xI0s2ts9FBnIR"
  )
  
  # Delimitar que se deve extrair somente tweets publicados no Brasil.
  bz <- lookup_coords("brazil")
  
  # Busca dos tweets.
  tweets <- search_tweets(
    q = '"por"',
    n = 1000, 
    type = "recent",
    lang = "pt",
    include_rts = FALSE,
    geocode = bz, 
    max_id = NULL,
    parse = TRUE,
    token = NULL, 
    retryonratelimit = FALSE,
    verbose = TRUE
  )
  
  # Retorna a tabela de tweets com as listas.
  return(tweets)
}

# Para fazer buscas com outras palavras, indica-se que a expressão em "q" seja trocada e depois inserir no console as seguintes duas linhas.
### source("automacaoTWEETS.R")
### atualizaTWEETS_principal()

#######################

limpaTWEET <- function(tabelaTWEET){
  # Essa função pega uma tabela de tweets com listas e a limpa.
  
  # Biblioteca de pacotes.
  biblio <- c(
    "tidyverse",
    "devtools",
    "magrittr"
  )
  
  # Instalando e importando os pacotes.
  for(pacote in biblio){
    
    # Checar se os pacotes estão instalados.
    if( !(pacote %in% rownames(installed.packages()))  )
      install.packages(pacote)
    
    library(pacote, character.only = TRUE)
  }
  
  # Selecionar (nomes de) as colunas que são listas.
  nomeslistas = tabelaTWEET %>% 
    select_if(is.list) %>% 
    names()
  
  # Remover listas.
  tabelasemlistas = tabelaTWEET %>% 
    select(-nomeslistas)
  
  # Retornando a tabela limpa.
  return(tabelasemlistas)
}

#######################

criaTABELA_principal <- function(){
  # Essa função cria a tabela principal que irá guardar TODOS os tweets extraídos.
  # Ela só deve ser executada uma vez, pois cria uma tabela que receberá os outros tweets extraídos depois. 
  
  # Extrai os tweets,
  tabelaTWEET_dia <- puxaTWEET()
  
  # Limpa os tweets.
  tabelaTWEET_dia <- tabelaTWEET_dia %>% 
    limpaTWEET()
    
  # Selecionar apenas uma linha (manter tipos das colunas quando salvar tabela no computador).
  tabelaTWEET_dia <- tabelaTWEET_dia %>% 
    slice(1)
  
  # Salvar tabela no computador.
  tabelaTWEET_dia %>% 
    write_csv("tabelaTWEET_principal.csv")
}

#######################

atualizaTWEETS_principal <- function(){
  # Essa função atualiza a nossa tabela de tweets principal com as novas buscas.
  
  # Biblioteca de pacotes.
  biblio <- c(
    "tidyverse",
    "devtools",
    "magrittr"
  )
  
  # Instalando e importando os pacotes.
  for(pacote in biblio){
    
    # Checa se os pacotes estão instalados.
    if( !(pacote %in% rownames(installed.packages()))  )
      install.packages(pacote)
    
    library(pacote, character.only = TRUE)
  }
  
  # Primeiramente é preciso checar se a tabelaTWEET_principal existe. 
  arquivos_PASTA <- list.files()
  
  # Se a tabelaTWEET_principal existe, executa o código, caso contrário não executa.
  existeTABELA <- arquivos_PASTA %>% 
    str_detect("tabelaTWEET_principal.csv") %>% 
    sum()
  
  if(!existeTABELA) criaTABELA_principal()
  
  # Agora vamos extrair os novos tweets.
  tabelaTWEET_novo <- puxaTWEET()
  
  # Limpa a tabela de tweets novos (retira as listas).
  tabelaTWEET_novo <- tabelaTWEET_novo %>% 
    limpaTWEET()
  
  # Para manter a compatibilidade entre as colunas, vamos salvar o TWEET NOVO e lê-lo novamente.
  tabelaTWEET_novo %>% 
    write_csv("tabelaTEMPORARIA.csv")
  
  tabelaTWEET_novo <- read_csv("tabelaTEMPORARIA.csv")
  
  # Excluir a tabela temporária.
  file.remove("tabelaTEMPORARIA.csv")
  
  # Ler a tabela principal de tweets para depois juntar os tweets novos a ela.
  tabelaTWEET_principal <- read_csv("tabelaTWEET_principal.csv")
  
  # Fazendo a junção entre as tabelas e REMOVENDO LINHAS REPETIDAS.
  tabelaTWEET_principal <- tabelaTWEET_principal %>% 
    bind_rows(
      tabelaTWEET_novo
    ) %>% 
    distinct()
  
  # Salvando a tabela ATUALIZADA.
  tabelaTWEET_principal %>% 
    write_csv("tabelaTWEET_principal.csv")
    
  # Limpar memória.
  rm(list = ls())
}
