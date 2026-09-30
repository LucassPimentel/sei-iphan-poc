# scrapar.R — Uso: Rscript scripts/scrapar.R "URL_DO_PROCESSO"
# Exemplo:
#   Rscript scripts/scrapar.R "https://sei.iphan.gov.br/sei/modulos/pesquisa/md_pesq_processo_exibir.php?TOKEN"

# --- Garante que o working directory seja a RAIZ do projeto ---------------
# Este script vive em scripts/, mas os caminhos source("R/...") sao relativos
# a raiz. Detectamos a raiz (pai de scripts/) e fazemos setwd para ela, de modo
# que funcione tanto via `Rscript scripts/scrapar.R` quanto pelo RStudio.
.definir_raiz_projeto <- function() {
  args_full <- commandArgs(trailingOnly = FALSE)
  arg_file <- grep("^--file=", args_full, value = TRUE)
  caminho_script <- if (length(arg_file) > 0) {
    sub("^--file=", "", arg_file[1])
  } else if (!is.null(sys.frames()[[1]]$ofile)) {
    sys.frames()[[1]]$ofile
  } else {
    NA_character_
  }
  if (!is.na(caminho_script)) {
    raiz <- dirname(dirname(normalizePath(caminho_script)))
    setwd(raiz)
  }
}
.definir_raiz_projeto()

args <- commandArgs(trailingOnly = TRUE)

if (length(args) == 0) {
  cat("Uso: Rscript scripts/scrapar.R \"URL_DO_PROCESSO\"\n")
  quit(status = 1)
}

url <- args[1]

suppressPackageStartupMessages({
  library(httr2); library(rvest); library(xml2)
  library(tidyverse); library(lubridate); library(stringr); library(jsonlite)
})

source("R/02_sei_scraping.R")

r <- scrape_processo_sei(url)

cat("\n========== RESULTADO ==========\n")
cat("Processo         :", r$cabecalho$numero_processo, "\n")
cat("Tipo             :", r$cabecalho$tipo, "\n")
cat("Data de geracao  :", r$cabecalho$data_geracao, "\n")
cat("Processo restrito:", r$meta$processo_restrito, "\n")
cat("Documentos       :", r$meta$total_documentos,
    "| Publicos:", r$meta$documentos_publicos,
    "| Restritos:", r$meta$documentos_restritos, "\n")
cat("Andamentos       :", r$meta$total_andamentos, "\n")
cat("HTML salvo em    :", r$meta$html_salvo_em, "\n")

cat("\n--- DOCUMENTOS ---\n")
print(r$documentos[, c("numero_documento", "tipo", "data_documento", "unidade_sigla", "acesso")])

cat("\n--- ANDAMENTOS ---\n")
print(r$andamentos[, c("data_hora", "unidade_sigla", "descricao", "status")])
