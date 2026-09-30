# =============================================================================
# regenerar_grafos.R  (utilitario de manutencao)
# Regenera entidades -> event log -> prazos -> grafo dos 5 processos,
# reaproveitando scraping/conteudo/classificacao ja persistidos, COM gabarito.
# Inclui as arestas de CITACAO por numero e os 2 bugs ja corrigidos.
#
# Uso (PowerShell):
#   & "C:\Program Files\R\R-4.6.1\bin\Rscript.exe" scripts/regenerar_grafos.R
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

suppressWarnings(suppressMessages({
  source("R/utils_saida.R")
  source("R/03_parser_documentos.R")
  source("R/04_entidades.R")
  source("R/05_text_mining.R")
  source("R/06_classificacao.R")     # ANTES do gabarito (carregar_dicionario_classes)
  source("R/07_event_log.R")
  source("R/08_normativas.R")
  source("R/09_prazos.R")
  source("R/10_grafo.R")
  source("R/utils_gabarito.R")
}))

processos <- c("01450_000399_2026_31", "01450_001620_2026_78",
               "01450_002827_2026_60", "01450_003213_2026_03",
               "01450_003836_2026_78")

# ---- Gabarito consolidado (a "verdade" para a classe dos documentos) ----
cat("=== Consolidando gabarito ===\n")
gab <- consolidar_gabarito()
ground_truth <- gab$ground_truth
cat("[gabarito] ok:", isTRUE(gab$ok),
    "| linhas:", if (!is.null(ground_truth)) nrow(ground_truth) else 0, "\n\n")

base_proc <- "data/processed"

for (proc in processos) {
  cat("\n============================================================\n")
  cat("PROCESSO:", proc, "\n")
  cat("============================================================\n")

  cam_scr <- encontrar_arquivo_processo(base_proc, proc, etapa = NULL)
  cam_con <- encontrar_arquivo_processo(base_proc, proc, etapa = "conteudo")
  cam_cla <- encontrar_arquivo_processo(base_proc, proc, etapa = "classificacao")

  if (is.na(cam_scr) || is.na(cam_con)) {
    cat("[SKIP] scraping ou conteudo ausente para", proc, "\n")
    next
  }
  cat("[fonte] scraping     :", basename(cam_scr), "\n")
  cat("[fonte] conteudo     :", basename(cam_con), "\n")
  cat("[fonte] classificacao:", if (is.na(cam_cla)) "(ausente)" else basename(cam_cla), "\n")

  # 1) Text mining (para reaproveitar os blocos de assinatura no 04)
  tm <- text_mining_processo(cam_con, persistir = FALSE)
  blocos <- tm$por_documento

  # 2) Entidades — agora com a coluna `referencias` (citacao)
  ent <- extrair_entidades_processo(cam_con, blocos_assinatura = blocos)

  # 3) Event log — COM gabarito (classe_final tem precedencia sobre baseline)
  el <- construir_event_log_processo(
    scraping      = cam_scr,
    entidades     = ent,
    classificacao = if (is.na(cam_cla)) NULL else cam_cla,
    gabarito      = ground_truth
  )

  # 4) Prazos
  pr <- calcular_prazos_processo(event_log = el)

  # 5) Grafo (HTML interativo)
  gr <- construir_grafo_processo(
    event_log   = el,
    prazos      = pr,
    salvar_html = TRUE
  )

  cat("[resumo]", proc,
      "| citacao:", gr$meta$arestas_citacao,
      "| prazo:", gr$meta$arestas_prazo,
      "| enderecamento:", gr$meta$arestas_enderecamento,
      "| cronologica:", gr$meta$arestas_cronologicas, "\n")
}

cat("\n=== Regeneracao concluida ===\n")
