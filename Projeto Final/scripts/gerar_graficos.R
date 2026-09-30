# =============================================================================
# gerar_graficos.R — Gera TODAS as figuras da POC de uma vez.
#
# Cobre:
#   Data Understanding
#     - Composicao do corpus por tipo de documento      (13)
#     - Cobertura de extracao por processo              (14)
#     - Distribuicao das classes do gabarito            (14)
#   Comparacao de modelos (baseline vs 3 modelos x 2 condicoes)
#     - Metricas macro                                  (14)
#     - F1 por modelo e condicao                        (14)
#     - Accuracy por processo (fold LOPO)               (14)
#     - Matriz de confusao do baseline                  (14 + 11)
#
# Todas as figuras sao salvas em output/AAAAMMDD/ (e a de composicao tambem).
#
# Uso (PowerShell):
#   & "C:\Program Files\R\R-4.6.1\bin\Rscript.exe" scripts/gerar_graficos.R
#
#   Por padrao, a comparacao de modelos REUTILIZA os JSONs ja existentes em
#   output/ (comparacao_classificacao_<modelo>_ambos_*.json). Para REGERAR esses
#   JSONs (treina os 3 modelos via LOPO — pode levar alguns minutos), passe:
#
#   & "C:\Program Files\R\R-4.6.1\bin\Rscript.exe" scripts/gerar_graficos.R --regenerar-modelos
# =============================================================================

# --- Garante que o working directory seja a RAIZ do projeto ---------------
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
regenerar_modelos <- "--regenerar-modelos" %in% args

suppressWarnings(suppressMessages({
  library(ggplot2); library(dplyr); library(tidyr); library(forcats)
  library(jsonlite); library(tibble); library(purrr); library(stringr)
  source("R/utils_saida.R")
  source("R/utils_gabarito.R")
  source("R/06_classificacao.R")
  source("R/06b_classificacao_supervisionada.R")
  source("R/11_avaliacao.R")
  source("R/13_visualizacao_textmining.R")
  source("R/14_visualizacao_avaliacao.R")
}))

# Executa um passo, reportando OK/ERRO sem abortar o restante das figuras.
.passo <- function(nome, expr) {
  r <- tryCatch({ force(expr); "OK" },
                error = function(e) paste("ERRO:", conditionMessage(e)))
  cat(sprintf("  [%-28s] %s\n", nome, r))
}

cat("=== Gerando figuras da POC ===\n")
cat("Saida: output/", format(Sys.time(), "%Y%m%d"), "/\n\n", sep = "")

# ---------------------------------------------------------------------------
# 1) Data Understanding
# ---------------------------------------------------------------------------
cat("--- Data Understanding ---\n")
.passo("composicao_corpus_tipo",  plotar_composicao_corpus_tipo(salvar = TRUE))
.passo("cobertura_extracao",      plotar_cobertura_extracao(salvar = TRUE))
.passo("distribuicao_classes",    plotar_distribuicao_classes(salvar = TRUE))

# ---------------------------------------------------------------------------
# 2) Comparacao de modelos (baseline vs 3 modelos x 2 condicoes)
# ---------------------------------------------------------------------------
cat("\n--- Comparacao de modelos ---\n")
if (regenerar_modelos) {
  cat("  (--regenerar-modelos: treinando os 3 modelos via LOPO; pode demorar)\n")
}

# Consolida uma vez (reaproveitado pelos graficos de comparacao). Se os JSONs
# nao existirem e --regenerar-modelos foi passado, sao gerados aqui.
comparacao <- tryCatch(
  consolidar_comparacao_modelos(gerar_se_faltar = regenerar_modelos),
  error = function(e) { cat("  [consolidacao] ERRO:", conditionMessage(e), "\n"); NULL })

if (!is.null(comparacao)) {
  .passo("metricas_modelos",      plotar_metricas_modelos(comparacao = comparacao, salvar = TRUE))
  .passo("f1_por_condicao",       plotar_f1_por_condicao(comparacao = comparacao, salvar = TRUE))
  .passo("accuracy_por_processo", plotar_accuracy_por_processo(
                                    gerar_se_faltar = regenerar_modelos, salvar = TRUE))
} else {
  cat("  Graficos de comparacao pulados. Gere os JSONs com:\n")
  cat("    comparar_classificadores(modelo='logistica',   balancear='ambos')\n")
  cat("    comparar_classificadores(modelo='naive_bayes', balancear='ambos')\n")
  cat("    comparar_classificadores(modelo='svm',         balancear='ambos')\n")
  cat("  ou rode este script com --regenerar-modelos.\n")
}

# ---------------------------------------------------------------------------
# 3) Matriz de confusao do baseline (avaliacao contra o gabarito)
# ---------------------------------------------------------------------------
cat("\n--- Matriz de confusao (baseline) ---\n")
.passo("matriz_confusao_baseline", {
  av <- avaliar_processos(persistir = FALSE)
  plotar_matriz_confusao(av$classificacao$baseline$matriz_confusao,
                         titulo = "Matriz de confusao - baseline (dicionario)",
                         salvar = TRUE, sufixo_saida = "matriz_confusao_baseline")
})

cat("\n=== Concluido. Figuras em output/", format(Sys.time(), "%Y%m%d"), "/ ===\n", sep = "")
