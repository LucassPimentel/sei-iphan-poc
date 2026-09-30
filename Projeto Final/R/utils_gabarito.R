# =============================================================================
# utils_gabarito.R
# Validacao e consolidacao do gabarito (ground truth) de classificacao (POC IPHAN)
#
# Responsabilidade: transformar os CSVs de rotulagem (data/ground_truth/)
#   — preenchidos pelo revisor/especialista — em um ground truth consolidado,
#   validado e pronto para treinar/avaliar a classificacao supervisionada.
#
# Regras de rotulagem (definidas com o pesquisador):
#   - `classe_predita` : palpite do baseline (06_classificacao.R).
#   - `classe_manual`  : preenchida SOMENTE quando o revisor discorda.
#   - classe_final = classe_manual quando preenchida; senao classe_predita.
#
# O que este modulo faz:
#   - Le todos (ou os indicados) CSVs de rotulagem.
#   - Valida estrutura (colunas) e valores (classes permitidas).
#   - Deriva a classe_final por documento.
#   - Reporta um resumo auditavel: concordancia, correcoes, distribuicao,
#     e erros (classe invalida, linha malformada, indefinido nao resolvido).
#
# NAO depende de pacotes de ML — pode rodar antes de instalar tidymodels.
#
# Tecnologias: readr (fallback utils), dplyr, stringr, purrr, tibble
# =============================================================================

library(dplyr)
library(stringr)
library(purrr)
library(tibble)

# O dicionario de classes e a fonte da verdade para as classes validas.
# Reutiliza carregar_dicionario_classes() de 06_classificacao.R.
if (!exists("carregar_dicionario_classes")) {
  # Permite usar este modulo isoladamente (ex.: nos testes) carregando o 06.
  .this_dir <- tryCatch(dirname(sys.frame(1)$ofile), error = function(e) NA)
}

DIR_GROUND_TRUTH_PADRAO <- "data/ground_truth"

# -----------------------------------------------------------------------------
# Leitura robusta de CSV
# -----------------------------------------------------------------------------

#' Le uma planilha de rotulagem (XLSX ou CSV) de forma robusta.
#' @param caminho Caminho do arquivo (.xlsx ou .csv)
#' @return data.frame
.ler_csv_rotulagem <- function(caminho) {
  if (grepl("\\.xlsx$", caminho, ignore.case = TRUE)) {
    if (!requireNamespace("openxlsx", quietly = TRUE)) {
      stop("[gabarito] openxlsx necessario para ler XLSX: ", caminho)
    }
    return(openxlsx::read.xlsx(caminho))
  }
  if (requireNamespace("readr", quietly = TRUE)) {
    as.data.frame(readr::read_csv(caminho, show_col_types = FALSE,
                                  progress = FALSE))
  } else {
    utils::read.csv(caminho, stringsAsFactors = FALSE, encoding = "UTF-8")
  }
}

#' Obtem o vetor de classes semanticas validas a partir do dicionario.
#' @param classes_config Caminho do config de classes
#' @return character com os ids das classes
obter_classes_validas <- function(classes_config = "config/classes_semanticas.json") {
  if (!exists("carregar_dicionario_classes")) {
    stop("[gabarito] carregar_dicionario_classes() nao disponivel. ",
         "Faca source('R/06_classificacao.R') antes.")
  }
  dic <- carregar_dicionario_classes(classes_config)
  names(dic$classes)
}

# -----------------------------------------------------------------------------
# Validacao de um CSV de rotulagem
# -----------------------------------------------------------------------------

COLUNAS_OBRIGATORIAS <- c("numero_processo", "numero_documento", "tipo",
                          "classe_predita", "classe_manual")

#' Valida e consolida UM CSV de rotulagem.
#'
#' @param caminho Caminho do CSV
#' @param classes_validas Vetor de classes permitidas
#' @return list(ok, erros, dados) — `dados` e um tibble com classe_final e flags
validar_csv_rotulagem <- function(caminho, classes_validas) {
  erros <- character(0)

  if (!file.exists(caminho)) {
    return(list(ok = FALSE, erros = paste0("Arquivo nao encontrado: ", caminho),
                dados = tibble()))
  }

  tab <- tryCatch(.ler_csv_rotulagem(caminho),
                  error = function(e) NULL)
  if (is.null(tab)) {
    return(list(ok = FALSE, erros = paste0("Falha ao ler CSV (malformado?): ", caminho),
                dados = tibble()))
  }

  faltantes <- setdiff(COLUNAS_OBRIGATORIAS, names(tab))
  if (length(faltantes) > 0) {
    return(list(ok = FALSE,
                erros = paste0("Colunas ausentes em ", basename(caminho), ": ",
                               paste(faltantes, collapse = ", ")),
                dados = tibble()))
  }

  # Normaliza campos de texto
  tab$classe_predita <- str_squish(as.character(tab$classe_predita))
  tab$classe_manual  <- str_squish(as.character(tab$classe_manual))
  tab$classe_manual[is.na(tab$classe_manual)] <- ""

  # classe_final: manual quando preenchida, senao predita
  classe_final <- ifelse(tab$classe_manual != "", tab$classe_manual, tab$classe_predita)

  # --- Validacoes de valor ---
  # 1. classe_manual preenchida deve ser uma classe valida
  manual_invalida <- tab$classe_manual != "" & !(tab$classe_manual %in% classes_validas)
  if (any(manual_invalida)) {
    docs <- tab$numero_documento[manual_invalida]
    erros <- c(erros, paste0("classe_manual invalida em ", basename(caminho),
                             " (docs: ", paste(docs, collapse = ", "), ")"))
  }

  # 2. classe_final indefinida/vazia e uma pendencia de rotulagem
  final_pendente <- classe_final == "indefinido" | classe_final == "" |
    is.na(classe_final)
  # (nao e erro fatal, mas e reportado como pendencia)

  dados <- tibble(
    numero_processo  = as.character(tab$numero_processo),
    numero_documento = as.character(tab$numero_documento),
    tipo             = as.character(tab$tipo),
    classe_predita   = tab$classe_predita,
    classe_manual    = tab$classe_manual,
    classe_final     = classe_final,
    corrigido        = tab$classe_manual != "",
    pendente         = final_pendente
  )

  list(ok = length(erros) == 0, erros = erros, dados = dados)
}

# -----------------------------------------------------------------------------
# Consolidacao de todos os CSVs -> ground truth
# -----------------------------------------------------------------------------

#' Remove CSVs redundantes quando existe um XLSX de mesma base (processo+data).
#' Evita contar o mesmo processo duas vezes.
#' @param arquivos character com caminhos
#' @return character filtrado
.priorizar_xlsx <- function(arquivos) {
  if (length(arquivos) == 0) return(arquivos)
  base_sem_ext <- sub("\\.(xlsx|csv)$", "", basename(arquivos), ignore.case = TRUE)
  is_xlsx <- grepl("\\.xlsx$", arquivos, ignore.case = TRUE)
  manter <- vapply(seq_along(arquivos), function(i) {
    if (is_xlsx[i]) return(TRUE)
    # e CSV: manter apenas se NAO existir um xlsx de mesma base
    !any(is_xlsx & base_sem_ext == base_sem_ext[i])
  }, logical(1))
  arquivos[manter]
}

#' Valida e consolida TODOS os CSVs de rotulagem em um ground truth unico.
#'
#' @param dir_ground_truth Diretorio dos CSVs de rotulagem
#' @param classes_config Config de classes (fonte das classes validas)
#' @param exigir_completo Logico — se TRUE, trata pendencias (indefinido/vazio)
#'        como erro que impede o uso do gabarito.
#' @return list(ok, erros, avisos, ground_truth, resumo)
consolidar_gabarito <- function(
    dir_ground_truth = DIR_GROUND_TRUTH_PADRAO,
    classes_config   = "config/classes_semanticas.json",
    exigir_completo  = FALSE
) {
  classes_validas <- obter_classes_validas(classes_config)

  # Aceita XLSX (formato de rotulagem preferido) e CSV (fallback). Quando o
  # mesmo processo existir nos dois formatos, prioriza o XLSX para nao duplicar.
  arquivos <- list.files(dir_ground_truth, pattern = "_rotulagem_.*\\.(xlsx|csv)$",
                         full.names = TRUE)
  arquivos <- .priorizar_xlsx(arquivos)
  if (length(arquivos) == 0) {
    return(list(ok = FALSE,
                erros = paste0("Nenhuma planilha de rotulagem em ", dir_ground_truth),
                avisos = character(0), ground_truth = tibble(), resumo = list()))
  }

  erros  <- character(0)
  avisos <- character(0)
  partes <- list()

  for (a in arquivos) {
    v <- validar_csv_rotulagem(a, classes_validas)
    if (!v$ok) erros <- c(erros, v$erros)
    if (nrow(v$dados) > 0) partes[[length(partes) + 1]] <- v$dados
  }

  gt <- if (length(partes) > 0) bind_rows(partes) else tibble()

  # Pendencias (nao rotulados / indefinido nao resolvido)
  n_pendentes <- sum(gt$pendente)
  if (n_pendentes > 0) {
    msg <- paste0(n_pendentes, " documento(s) com classe_final pendente ",
                  "(indefinido/vazio) — precisam de rotulo manual.")
    if (exigir_completo) erros <- c(erros, msg) else avisos <- c(avisos, msg)
  }

  # Resumo auditavel
  resumo <- list(
    arquivos            = length(arquivos),
    processos           = if (nrow(gt) > 0) dplyr::n_distinct(gt$numero_processo) else 0L,
    documentos          = nrow(gt),
    corrigidos_manual   = sum(gt$corrigido),
    concordancia_baseline = if (nrow(gt) > 0)
      round(mean(!gt$corrigido & !gt$pendente), 4) else NA_real_,
    pendentes           = n_pendentes,
    distribuicao_classe = if (nrow(gt) > 0)
      as.list(table(gt$classe_final[!gt$pendente])) else list()
  )

  list(ok = length(erros) == 0, erros = erros, avisos = avisos,
       ground_truth = gt, resumo = resumo)
}

#' Imprime um relatorio legivel do estado do gabarito.
#' @param res Retorno de consolidar_gabarito()
relatar_gabarito <- function(res) {
  cat("========== RELATORIO DO GABARITO ==========\n")
  cat("Arquivos de rotulagem :", res$resumo$arquivos, "\n")
  cat("Processos             :", res$resumo$processos, "\n")
  cat("Documentos            :", res$resumo$documentos, "\n")
  cat("Corrigidos pelo revisor:", res$resumo$corrigidos_manual, "\n")
  cat("Concordancia c/ baseline:",
      if (is.na(res$resumo$concordancia_baseline)) "-"
      else paste0(round(res$resumo$concordancia_baseline * 100, 1), "%"), "\n")
  cat("Pendentes (sem rotulo) :", res$resumo$pendentes, "\n")

  if (length(res$resumo$distribuicao_classe) > 0) {
    cat("\nDistribuicao de classes (classe_final):\n")
    for (nm in names(res$resumo$distribuicao_classe)) {
      cat("  ", nm, ":", res$resumo$distribuicao_classe[[nm]], "\n")
    }
  }

  if (length(res$avisos) > 0) {
    cat("\n--- AVISOS ---\n")
    for (a in res$avisos) cat("  [aviso] ", a, "\n")
  }
  if (length(res$erros) > 0) {
    cat("\n--- ERROS ---\n")
    for (e in res$erros) cat("  [ERRO] ", e, "\n")
  }
  if (res$ok) {
    cat("\nStatus: OK — gabarito valido.\n")
  } else {
    cat("\nStatus: COM ERROS — corrija antes de treinar.\n")
  }
  invisible(res)
}
