# executar apenas uma vez
if (!require("pacman")) install.packages("pacman") #para instalar pacotes que não foram baixados

# executar apenas uma vez
pacman::p_load(tidyverse, gsheet) 

#carregar pacotes
library(tidyverse)
library(gsheet)

url <- "https://docs.google.com/spreadsheets/d/1R5svYhxvBHNOW35NEy23oE8VXX1eWq5v/edit?gid=246705190#gid=246705190"

dados.processos.iphan <- gsheet2tbl(url)

processos2026 <- dados.processos.iphan %>% 
  filter(Ano== 2026 & Tipo=='Autorização IN') %>% 
  mutate(Processos2026= substr(Processo, 14,17)) %>% 
  filter(Processos2026==2026) %>% 
  dplyr::select(Processos2026, Processo)

set.seed(123) #semente para fixar os 5 processos
processos2026[sample(nrow(processos2026), size = 5, replace = FALSE),] #amostragem aleatória simples
