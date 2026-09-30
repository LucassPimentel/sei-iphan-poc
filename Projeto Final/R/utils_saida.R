# =============================================================================
# utils_saida.R
# Utilitarios de organizacao das saidas em PASTAS POR DATA (POC IPHAN)
#
# Motivacao (pedido do pesquisador): em vez de despejar todos os artefatos numa
#   unica pasta (data/processed/, output/), agrupar por dia de execucao:
#     data/processed/AAAAMMDD/{proc}_etapa_...   e   output/AAAAMMDD/...
#   Isso torna a inspecao dos arquivos muito mais facil.
#
# Este modulo centraliza:
#   - dir_saida_dia(base, data)         : garante e retorna base/AAAAMMDD.
#   - nome_artefato(proc, etapa, ext)   : padroniza o nome do arquivo.
#   - encontrar_arquivo_processo(...)   : localiza o arquivo mais recente de uma
#                                         etapa para um processo, varrendo as
#                                         subpastas de data (e a raiz, p/ compat
#                                         com arquivos antigos gerados na raiz).
#
# Regra: os modulos ESCREVEM sempre na pasta do dia; a LEITURA da etapa anterior
#   (quando feita por caminho) usa encontrar_arquivo_processo, que acha o mais
#   recente independentemente de estar na raiz (legado) ou numa subpasta AAAAMMDD.
# =============================================================================

#' Retorna (criando, se preciso) o diretorio de saida do dia: base/AAAAMMDD.
#' @param base Diretorio base (ex.: "data/processed" ou "output").
#' @param data Data (character AAAAMMDD ou Date/POSIXct). Default: hoje.
#' @return caminho do diretorio base/AAAAMMDD
dir_saida_dia <- function(base, data = Sys.time()) {
  data_str <- if (inherits(data, c("Date", "POSIXct", "POSIXt"))) {
    format(data, "%Y%m%d")
  } else {
    as.character(data)
  }
  caminho <- file.path(base, data_str)
  dir.create(caminho, showWarnings = FALSE, recursive = TRUE)
  caminho
}

#' Nome padronizado de artefato: {proc}_{etapa}_{data}.{ext}
#' @param numero_seguro Numero do processo em forma segura (so [0-9A-Za-z_]).
#' @param etapa Rotulo da etapa (ex.: "entidades", "eventlog", "grafo").
#'        Pode ser "" para o scraping (que nao usa sufixo de etapa).
#' @param ext Extensao sem ponto (ex.: "json", "csv", "png").
#' @param data Data (character AAAAMMDD ou Date/POSIXct). Default: hoje.
#' @return nome do arquivo (sem diretorio)
nome_artefato <- function(numero_seguro, etapa, ext, data = Sys.time()) {
  data_str <- if (inherits(data, c("Date", "POSIXct", "POSIXt"))) {
    format(data, "%Y%m%d")
  } else {
    as.character(data)
  }
  meio <- if (is.null(etapa) || etapa == "") "" else paste0("_", etapa)
  paste0(numero_seguro, meio, "_", data_str, ".", ext)
}

#' Localiza o arquivo mais recente de uma etapa para um processo.
#'
#' Varre `base` (raiz — arquivos legados) e todas as suas subpastas de data
#' (AAAAMMDD). O "mais recente" e decidido pelo nome (que inclui a data) e, como
#' desempate, pela data de modificacao do arquivo.
#'
#' @param base Diretorio base (ex.: "data/processed").
#' @param prefixo Prefixo do processo (numero seguro, ex.: "01450_002827_2026_60").
#' @param etapa Rotulo da etapa (ex.: "eventlog", "entidades"). Para o scraping
#'        (sem sufixo de etapa) passe etapa = NULL.
#' @param ext Extensao (default "json").
#' @return caminho do arquivo mais recente, ou NA_character_ se nao houver.
encontrar_arquivo_processo <- function(base, prefixo, etapa = NULL, ext = "json") {
  # Padrao do nome: prefixo [_etapa] _AAAAMMDD[_HHMMSS] .ext
  # Para o scraping (etapa NULL) NAO deve casar arquivos de outras etapas, entao
  # exigimos que apos o prefixo venha diretamente a data (sem sufixo de etapa).
  ext_esc <- gsub("\\.", "\\\\.", ext)
  if (is.null(etapa) || etapa == "") {
    padrao <- paste0("^", prefixo, "_\\d{8}.*\\.", ext_esc, "$")
    sufixos_etapa <- "_(conteudo|textmining|entidades|classificacao|eventlog|prazos|grafo|grafo_org|local)_"
    filtro_extra <- function(nomes) !grepl(sufixos_etapa, nomes)
  } else {
    padrao <- paste0("^", prefixo, "_", etapa, "_.*\\.", ext_esc, "$")
    filtro_extra <- function(nomes) rep(TRUE, length(nomes))
  }

  candidatos <- list.files(base, pattern = padrao, full.names = TRUE,
                           recursive = TRUE)
  if (length(candidatos) == 0) return(NA_character_)

  nomes <- basename(candidatos)
  candidatos <- candidatos[filtro_extra(nomes)]
  if (length(candidatos) == 0) return(NA_character_)

  # Ordena por nome (a data no nome domina) e desempata por mtime.
  info <- file.info(candidatos)
  ordem <- order(basename(candidatos), info$mtime, decreasing = TRUE)
  candidatos[ordem][1]
}
