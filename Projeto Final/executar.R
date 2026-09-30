# =============================================================================
# executar.R — Ponto de entrada por linha de comando para o pipeline completo
#
# Executa TODAS as etapas implementadas da POC para uma URL de processo do SEI:
#   Etapa 1 : Scraping estrutural       (02_sei_scraping.R)
#   Etapa 2 : Extracao de conteudo      (03_parser_documentos.R)
#   Etapa 3 : Text Mining               (05_text_mining.R)
#   Etapa 3b: Extracao de entidades     (04_entidades.R)
#   Etapa 4 : Classificacao (baseline)  (06_classificacao.R)
#   Etapa 5 : Event Log                 (07_event_log.R)
#   Etapa 6 : Calculo de prazos         (09_prazos.R)
#   Etapa 7 : Grafo de tramitacao       (10_grafo.R) — JSON + PNG + HTML interativo
#
# Uso (PowerShell):
#   & "C:\Program Files\R\R-4.6.1\bin\Rscript.exe" executar.R "URL_DO_PROCESSO"
#
# Observacoes:
#   - A URL do SEI Pesquisa Publica contem um token de sessao TEMPORARIO. Se ela
#     expirar, o scraping falha. Nesse caso use scripts/executar_scraping_local.R
#     contra o HTML salvo em Paginas/.
#   - Cada etapa persiste seu resultado em data/processed/ antes da proxima.
#   - O grafo e salvo como DADO (data/processed/AAAAMMDD/..._grafo_*.json), como
#     IMAGEM estatica (output/AAAAMMDD/..._grafo_*.png) e como HTML INTERATIVO
#     (output/AAAAMMDD/..._grafo_*.html; layout em arvore, zoom/pan). PNG e HTML
#     sao gerados por padrao aqui (salvar_grafo = TRUE, salvar_grafo_html = TRUE).
# =============================================================================

args <- commandArgs(trailingOnly = TRUE)

if (length(args) == 0 || is.na(args[1]) || nchar(trimws(args[1])) == 0) {
  cat("Uso: Rscript executar.R \"URL_DO_PROCESSO\"\n\n")
  cat("Exemplo:\n")
  cat("  Rscript executar.R \"https://sei.iphan.gov.br/sei/modulos/pesquisa/md_pesq_processo_exibir.php?TOKEN\"\n")
  quit(status = 1)
}

url <- trimws(args[1])

if (!grepl("^https?://", url)) {
  cat("[executar] ERRO: o argumento nao parece uma URL valida:\n  ", url, "\n")
  cat("[executar] A URL deve comecar com http:// ou https://\n")
  quit(status = 1)
}

cat("[executar] Carregando modulos (main.R)...\n")
source("main.R")

resultado <- analisar_processo(url, salvar_grafo = TRUE, salvar_grafo_html = TRUE)

cat("\n[executar] Pipeline concluido. Resultados persistidos em data/processed/.\n")
