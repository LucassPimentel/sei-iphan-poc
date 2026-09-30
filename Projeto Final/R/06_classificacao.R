# =============================================================================
# 06_classificacao.R
# Classificacao semantica de atos/documentos — BASELINE por dicionario (POC IPHAN)
#
# Responsabilidade: Camada de Classificacao (ver architecture.md), caminho 1:
#   baseline por regras/dicionario. O modelo supervisionado simples (caminho 2)
#   sera implementado depois e COMPARADO a este baseline.
#
# Principio metodologico (project-context / research-methodology):
#   - "Antes do ML deve existir um baseline baseado em regras/dicionario."
#   - Este baseline NAO precisa de dados rotulados para RODAR; os rotulos sao
#     necessarios apenas para AVALIAR (comparar com o gabarito) e para TREINAR
#     o modelo supervisionado.
#   - As classes e os gatilhos ficam em config/classes_semanticas.json
#     (dado editavel e auditavel), nao hardcoded no codigo.
#
# Como funciona:
#   1. Le o dicionario de classes (5 classes com termos-gatilho e pesos).
#   2. Para cada documento, normaliza o texto (minusculas, sem acento) e conta
#      os gatilhos de cada classe, ponderando pelo peso.
#   3. Soma o sinal auxiliar do TIPO do documento (ex.: Parecer -> manifestacao).
#   4. A classe de maior escore vence. Empate/zero -> "indefinido".
#   5. Registra os escores por classe e a margem (confianca), para auditoria e
#      para priorizar a revisao manual na rotulagem.
#
# Entrada : JSON "_conteudo_" (03) OU objeto em memoria; usa docs com texto.
# Saida   : data/processed/{processo}_classificacao_{data}.json
#           e uma tabela de pre-classificacao para rotulagem (Passo 3).
#
# Tecnologias: stringi, stringr, tibble, dplyr, purrr, jsonlite
# =============================================================================

library(stringi)
library(stringr)
library(tibble)
library(dplyr)
library(purrr)
library(jsonlite)

# Utilitario de pastas por data (dir_saida_dia). Carrega se ausente.
if (!exists("dir_saida_dia")) {
  .cand_utils <- c("R/utils_saida.R", "utils_saida.R",
                   file.path(tryCatch(dirname(sys.frame(1)$ofile),
                                      error = function(e) NA), "utils_saida.R"))
  .cand_utils <- .cand_utils[!is.na(.cand_utils) & file.exists(.cand_utils)]
  if (length(.cand_utils) > 0) source(.cand_utils[1])
}

# -----------------------------------------------------------------------------
# Constantes
# -----------------------------------------------------------------------------

CLASSES_CONFIG_PATH <- "config/classes_semanticas.json"
CLASSE_INDEFINIDA   <- "indefinido"

# -----------------------------------------------------------------------------
# Normalizacao (para casar gatilhos sem depender de acento/caixa)
# -----------------------------------------------------------------------------

#' Normaliza texto para comparacao com gatilhos: minusculas + remocao de acento.
#' Mantem o texto legivel (nao tokeniza). Usado tanto no texto quanto nos gatilhos.
#' @param x character
#' @return character normalizado
.normalizar_para_match <- function(x) {
  x <- stri_trans_tolower(x)
  x <- stri_trans_general(x, "Latin-ASCII")   # remove acentos
  str_squish(x)
}

#' Extrai um trecho legivel do corpo do documento para ajudar a rotulagem.
#' Remove o rodape de assinatura ("Documento assinado eletronicamente...") e
#' limita o tamanho, preservando acentos (para leitura humana no CSV).
#' @param texto Texto completo do documento
#' @param max_chars Tamanho maximo do trecho
#' @return character (trecho) ou "" se vazio
.trecho_corpo <- function(texto, max_chars = 300L) {
  if (is.na(texto) || texto == "") return("")
  corpo <- sub("Documento assinado eletronicamente.*", "", texto)
  corpo <- str_squish(corpo)
  substr(corpo, 1, max_chars)
}

# -----------------------------------------------------------------------------
# Carregamento do dicionario
# -----------------------------------------------------------------------------

#' Carrega o dicionario de classes semanticas do JSON de configuracao.
#' @param caminho Caminho do JSON
#' @return list com: classes (list), peso_tipo (tibble)
carregar_dicionario_classes <- function(caminho = CLASSES_CONFIG_PATH) {
  if (!file.exists(caminho)) {
    stop("[classificacao] Config de classes nao encontrada: ", caminho)
  }
  cfg <- jsonlite::fromJSON(caminho, simplifyVector = FALSE)

  # Classes: normaliza gatilhos uma vez
  classes <- lapply(cfg$classes, function(cl) {
    gat <- lapply(cl$gatilhos, function(g) {
      list(termo = .normalizar_para_match(g$termo), peso = as.numeric(g$peso))
    })
    list(id = cl$id, rotulo = cl$rotulo, gatilhos = gat)
  })
  names(classes) <- vapply(classes, function(c) c$id, character(1))

  # Regras de peso por tipo de documento
  regras_tipo <- cfg$peso_tipo_documento$regras
  peso_tipo <- if (is.null(regras_tipo) || length(regras_tipo) == 0) {
    tibble(prefixo_tipo = character(0), classe = character(0), peso = numeric(0))
  } else {
    bind_rows(lapply(regras_tipo, function(r) {
      tibble(prefixo_tipo = .normalizar_para_match(r$prefixo_tipo),
             classe = r$classe, peso = as.numeric(r$peso))
    }))
  }

  list(classes = classes, peso_tipo = peso_tipo)
}

# -----------------------------------------------------------------------------
# Pontuacao de um documento
# -----------------------------------------------------------------------------

#' Conta ocorrencias (nao sobrepostas) de um termo normalizado no texto.
.contar_ocorrencias <- function(texto_norm, termo_norm) {
  if (termo_norm == "") return(0L)
  # str_count com termo literal (fixed) — conta ocorrencias
  str_count(texto_norm, fixed(termo_norm))
}

#' Pontua um documento contra todas as classes do dicionario.
#'
#' @param texto Texto do documento (corpo)
#' @param tipo Tipo do documento (para o sinal auxiliar)
#' @param dicionario Retorno de carregar_dicionario_classes()
#' @return list(classe, escore, margem, escores_por_classe, gatilhos_encontrados)
classificar_documento <- function(texto, tipo, dicionario) {
  vazio <- list(classe = CLASSE_INDEFINIDA, escore = 0, margem = 0,
                escores = setNames(rep(0, length(dicionario$classes)),
                                   names(dicionario$classes)),
                gatilhos = character(0))
  if (is.na(texto) || texto == "") return(vazio)

  texto_norm <- .normalizar_para_match(texto)
  tipo_norm  <- .normalizar_para_match(if (is.na(tipo)) "" else tipo)

  gatilhos_encontrados <- character(0)

  escores <- vapply(dicionario$classes, function(cl) {
    s <- 0
    for (g in cl$gatilhos) {
      n <- .contar_ocorrencias(texto_norm, g$termo)
      if (n > 0) {
        s <- s + n * g$peso
        gatilhos_encontrados[[length(gatilhos_encontrados) + 1]] <<-
          paste0(cl$id, ":", g$termo, " (", n, "x)")
      }
    }
    s
  }, numeric(1))

  # Sinal auxiliar pelo tipo do documento (prefixo)
  if (nrow(dicionario$peso_tipo) > 0) {
    for (i in seq_len(nrow(dicionario$peso_tipo))) {
      pref <- dicionario$peso_tipo$prefixo_tipo[i]
      if (str_starts(tipo_norm, fixed(pref))) {
        cl <- dicionario$peso_tipo$classe[i]
        if (cl %in% names(escores)) {
          escores[cl] <- escores[cl] + dicionario$peso_tipo$peso[i]
        }
      }
    }
  }

  if (all(escores == 0)) {
    return(list(classe = CLASSE_INDEFINIDA, escore = 0, margem = 0,
                escores = escores, gatilhos = gatilhos_encontrados))
  }

  ordenado <- sort(escores, decreasing = TRUE)
  melhor   <- names(ordenado)[1]
  escore   <- unname(ordenado[1])
  segundo  <- if (length(ordenado) > 1) unname(ordenado[2]) else 0
  margem   <- escore - segundo  # confianca: quao a frente ficou a 1a classe

  # Empate no topo -> indefinido (marca para revisao)
  if (margem == 0) melhor <- CLASSE_INDEFINIDA

  list(classe = melhor, escore = escore, margem = margem,
       escores = escores, gatilhos = gatilhos_encontrados)
}

# -----------------------------------------------------------------------------
# Funcao principal: classificar_processo()
# -----------------------------------------------------------------------------

#' Classifica todos os documentos com texto de um processo (baseline).
#'
#' @param resultado_conteudo list de extrair_conteudo_documentos() OU caminho
#'        do JSON "_conteudo_".
#' @param dicionario list de carregar_dicionario_classes() ou NULL (carrega padrao)
#' @param dir_processed Diretorio de saida
#' @param persistir Logico — salvar JSON de classificacao
#' @param gerar_planilha_rotulagem Logico — salvar CSV de pre-classificacao
#' @return list com: classificacao (tibble), meta (list)
classificar_processo <- function(
    resultado_conteudo,
    dicionario                = NULL,
    dir_processed             = "data/processed",
    persistir                 = TRUE,
    gerar_planilha_rotulagem  = TRUE
) {
  if (is.character(resultado_conteudo)) {
    if (!file.exists(resultado_conteudo)) {
      stop("[classificacao] JSON de conteudo nao encontrado: ", resultado_conteudo)
    }
    resultado_conteudo <- jsonlite::fromJSON(resultado_conteudo, simplifyVector = TRUE)
  }
  if (is.null(dicionario)) dicionario <- carregar_dicionario_classes()

  numero_processo <- resultado_conteudo$meta$numero_processo
  numero_seguro   <- str_replace_all(numero_processo, "[^0-9A-Za-z]", "_")

  docs <- resultado_conteudo$documentos_conteudo
  docs_com_texto <- docs[!is.na(docs$conteudo_extraido) & docs$conteudo_extraido == TRUE, ]

  cat("[classificacao] Processo:", numero_processo, "\n")
  cat("[classificacao] Documentos com texto:", nrow(docs_com_texto), "\n")

  if (nrow(docs_com_texto) == 0) {
    cat("[classificacao] AVISO: nenhum documento com texto.\n")
    saida <- list(meta = list(numero_processo = numero_processo,
                              timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                              documentos_processados = 0L),
                  classificacao = tibble())
    return(invisible(saida))
  }

  linhas <- lapply(seq_len(nrow(docs_com_texto)), function(i) {
    numero <- docs_com_texto$numero_documento[i]
    tipo   <- docs_com_texto$tipo[i]
    texto  <- docs_com_texto$texto[i]

    r <- classificar_documento(texto, tipo, dicionario)
    tibble(
      numero_documento = as.character(numero),
      tipo             = as.character(tipo),
      classe_predita   = r$classe,
      escore           = r$escore,
      margem           = r$margem,
      gatilhos         = paste(r$gatilhos, collapse = "; "),
      trecho_texto     = .trecho_corpo(texto)
    )
  })
  classificacao <- bind_rows(linhas)

  # Distribuicao de classes
  distrib <- classificacao |>
    count(classe_predita, name = "n") |>
    arrange(desc(n))

  cat("[classificacao] --- Distribuicao (baseline) ---\n")
  for (i in seq_len(nrow(distrib))) {
    cat("[classificacao]  ", distrib$classe_predita[i], ":", distrib$n[i], "\n")
  }
  n_indef <- sum(classificacao$classe_predita == CLASSE_INDEFINIDA)
  if (n_indef > 0) {
    cat("[classificacao] AVISO:", n_indef, "documento(s) indefinido(s) — priorizar na revisao.\n")
  }

  saida <- list(
    meta = list(
      numero_processo        = numero_processo,
      timestamp              = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      documentos_processados = nrow(docs_com_texto),
      metodo                 = "baseline_dicionario",
      config_classes         = CLASSES_CONFIG_PATH,
      indefinidos            = n_indef
    ),
    classificacao = classificacao
  )

  if (persistir) {
    data_str <- format(Sys.time(), "%Y%m%d")
    nome     <- paste0(numero_seguro, "_classificacao_", data_str, ".json")
    caminho  <- file.path(dir_saida_dia(dir_processed), nome)
    write_json(saida, caminho, pretty = TRUE, auto_unbox = TRUE, na = "null")
    cat("[classificacao] Persistido em:", caminho, "\n")
  }

  if (gerar_planilha_rotulagem) {
    classes_validas <- names(dicionario$classes)
    .gerar_planilha_rotulagem(classificacao, numero_processo, numero_seguro,
                              classes_validas = classes_validas)
  }

  invisible(saida)
}

# -----------------------------------------------------------------------------
# Passo 3: planilha de pre-classificacao para rotulagem do gabarito
# -----------------------------------------------------------------------------

#' Gera um CSV de pre-classificacao para o especialista corrigir.
#'
#' A coluna `classe_manual` fica em branco: o revisor a preenche APENAS quando
#' discordar de `classe_predita` (rotular por correcao e mais rapido que do
#' zero). O CSV corrigido vira o ground truth em data/ground_truth/.
#'
#' @param classificacao tibble de classificar_processo()
#' @param numero_processo Numero do processo (para registro)
#' @param numero_seguro Nome seguro para arquivo
#' @param classes_validas Vetor de classes (para o dropdown do Excel)
#' @param dir_ground_truth Diretorio do gabarito
.gerar_planilha_rotulagem <- function(classificacao, numero_processo, numero_seguro,
                                      classes_validas = character(0),
                                      dir_ground_truth = "data/ground_truth") {
  dir.create(dir_ground_truth, showWarnings = FALSE, recursive = TRUE)

  planilha <- classificacao |>
    transmute(
      numero_processo = numero_processo,
      numero_documento,
      tipo,
      classe_predita,
      margem,
      gatilhos,                    # por que o baseline decidiu (auditoria)
      classe_manual = "",          # revisor preenche so quando discordar
      observacao_revisor = ""
    )

  data_str <- format(Sys.time(), "%Y%m%d")
  base_nome <- paste0(numero_seguro, "_rotulagem_", data_str)

  # Formato preferido: XLSX com dropdown (mais confortavel e sem erro de digitacao).
  # Fallback: CSV, se openxlsx nao estiver instalado.
  if (requireNamespace("openxlsx", quietly = TRUE)) {
    caminho <- file.path(dir_ground_truth, paste0(base_nome, ".xlsx"))
    if (file.exists(caminho) && .xlsx_tem_rotulos(caminho)) {
      cat("[classificacao] AVISO: planilha XLSX ja possui rotulos — NAO sobrescrita:\n  ",
          caminho, "\n")
      return(invisible(caminho))
    }
    .escrever_xlsx_rotulagem(planilha, caminho, classes_validas)
    cat("[classificacao] Planilha de rotulagem (Excel c/ dropdown) em:", caminho, "\n")
    return(invisible(caminho))
  }

  # --- Fallback CSV ---
  caminho <- file.path(dir_ground_truth, paste0(base_nome, ".csv"))
  if (file.exists(caminho) && .tem_rotulos_manuais(caminho)) {
    cat("[classificacao] AVISO: planilha ja possui rotulos manuais — NAO sobrescrita:\n  ",
        caminho, "\n")
    return(invisible(caminho))
  }
  if (requireNamespace("readr", quietly = TRUE)) {
    readr::write_excel_csv(planilha, caminho)
  } else {
    utils::write.csv(planilha, caminho, row.names = FALSE, fileEncoding = "UTF-8")
  }
  cat("[classificacao] openxlsx ausente — gerado CSV em:", caminho, "\n")
  invisible(caminho)
}

#' Escreve a planilha de rotulagem como XLSX formatado para revisao humana.
#'
#' Recursos que facilitam a rotulagem:
#'   - Coluna `classe_manual` destacada, com DROPDOWN (validacao de dados) das
#'     classes validas — impossivel digitar uma classe inexistente.
#'   - Cabecalho em negrito, primeira linha congelada, colunas com largura util,
#'     `gatilhos` com quebra de texto.
#'   - `classe_predita`, `margem` e `gatilhos` protegidos visualmente (fundo
#'     cinza) para deixar claro que sao apenas referencia, nao campos de edicao.
#'
#' @param planilha data.frame com as colunas da rotulagem
#' @param caminho Caminho do .xlsx
#' @param classes_validas Vetor de classes para o dropdown
.escrever_xlsx_rotulagem <- function(planilha, caminho, classes_validas) {
  wb <- openxlsx::createWorkbook()
  aba <- "rotulagem"
  openxlsx::addWorksheet(wb, aba)

  openxlsx::writeData(wb, aba, planilha, withFilter = TRUE)

  n_linhas <- nrow(planilha)
  col <- function(nome) match(nome, names(planilha))

  # Estilos
  est_cabecalho <- openxlsx::createStyle(textDecoration = "bold",
                                         fgFill = "#305496", fontColour = "white",
                                         halign = "center", border = "TopBottomLeftRight")
  est_referencia <- openxlsx::createStyle(fgFill = "#F2F2F2", fontColour = "#595959")
  est_rotulo    <- openxlsx::createStyle(fgFill = "#FFF2CC", textDecoration = "bold",
                                         border = "TopBottomLeftRight")
  est_wrap      <- openxlsx::createStyle(wrapText = TRUE, valign = "top")

  # Cabecalho
  openxlsx::addStyle(wb, aba, est_cabecalho, rows = 1,
                     cols = seq_along(planilha), gridExpand = TRUE)

  # Colunas de referencia (nao editar): classe_predita, margem, gatilhos
  for (nm in c("classe_predita", "margem", "gatilhos")) {
    openxlsx::addStyle(wb, aba, est_referencia, rows = 2:(n_linhas + 1),
                       cols = col(nm), gridExpand = TRUE, stack = TRUE)
  }
  # Coluna de rotulo (editar aqui): classe_manual
  openxlsx::addStyle(wb, aba, est_rotulo, rows = 2:(n_linhas + 1),
                     cols = col("classe_manual"), gridExpand = TRUE, stack = TRUE)
  # Quebra de texto na coluna de gatilhos
  openxlsx::addStyle(wb, aba, est_wrap, rows = 2:(n_linhas + 1),
                     cols = col("gatilhos"), gridExpand = TRUE, stack = TRUE)

  # Larguras uteis
  openxlsx::setColWidths(wb, aba, cols = col("numero_processo"), widths = 22)
  openxlsx::setColWidths(wb, aba, cols = col("numero_documento"), widths = 16)
  openxlsx::setColWidths(wb, aba, cols = col("tipo"), widths = 34)
  openxlsx::setColWidths(wb, aba, cols = col("classe_predita"), widths = 22)
  openxlsx::setColWidths(wb, aba, cols = col("margem"), widths = 8)
  openxlsx::setColWidths(wb, aba, cols = col("gatilhos"), widths = 55)
  openxlsx::setColWidths(wb, aba, cols = col("classe_manual"), widths = 24)
  openxlsx::setColWidths(wb, aba, cols = col("observacao_revisor"), widths = 30)

  # Congelar cabecalho
  openxlsx::freezePane(wb, aba, firstActiveRow = 2)

  # DROPDOWN na coluna classe_manual (validacao de dados)
  if (length(classes_validas) > 0) {
    openxlsx::dataValidation(
      wb, aba, col = col("classe_manual"), rows = 2:(n_linhas + 1),
      type = "list",
      value = paste0('"', paste(classes_validas, collapse = ","), '"')
    )
  }

  openxlsx::saveWorkbook(wb, caminho, overwrite = TRUE)
  invisible(caminho)
}

#' Verifica se um XLSX de rotulagem ja tem a coluna classe_manual preenchida.
.xlsx_tem_rotulos <- function(caminho) {
  if (!requireNamespace("openxlsx", quietly = TRUE)) return(FALSE)
  tab <- tryCatch(openxlsx::read.xlsx(caminho), error = function(e) NULL)
  if (is.null(tab) || !"classe_manual" %in% names(tab)) return(FALSE)
  vals <- tab$classe_manual
  any(!is.na(vals) & str_squish(as.character(vals)) != "")
}

#' Verifica se um CSV de rotulagem ja tem a coluna classe_manual preenchida.
.tem_rotulos_manuais <- function(caminho) {
  tab <- tryCatch(
    utils::read.csv(caminho, stringsAsFactors = FALSE, encoding = "UTF-8"),
    error = function(e) NULL
  )
  if (is.null(tab) || !"classe_manual" %in% names(tab)) return(FALSE)
  any(!is.na(tab$classe_manual) & str_squish(as.character(tab$classe_manual)) != "")
}
