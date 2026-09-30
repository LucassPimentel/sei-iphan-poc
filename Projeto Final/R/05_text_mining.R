# =============================================================================
# 05_text_mining.R
# Text Mining dos documentos nativos do SEI (POC IPHAN)
#
# Responsabilidade: Camada Text Mining (ver architecture.md)
#   - normalizacao
#   - separacao de boilerplate de assinatura (reaproveitado por 04_assinaturas.R)
#   - extracao de datas
#   - tokenizacao + stopwords (pt-br + dominio)
#   - preparacao de atributos
#   - TF-IDF
#
# Decisoes de design (definidas com o pesquisador):
#   - Base: tidytext (aderente ao tidyverse).
#   - Acentos preservados (encoding UTF-8 confiavel).
#   - SEM stemming (mantem interpretabilidade para o baseline por dicionario).
#   - Boilerplate de assinatura SEPARADO e guardado (nao descartado).
#   - Extracao de datas ocorre nesta etapa (conforme arquitetura).
#
# Entrada : JSON "_conteudo_" produzido por 03_parser_documentos.R.
#           Apenas documentos com conteudo_extraido == TRUE sao processados.
# Saida   : data/processed/{processo}_textmining_{data}.json
#
# Trade-offs registrados (ver research-methodology.md):
#   - Corte de boilerplate por marcador fixo: se o SEI mudar o texto padrao,
#     o corte falha; ha fallback que mantem o texto e registra aviso.
#   - TF-IDF em corpus pequeno (dezenas de docs) tende a ser esparso; isso e
#     esperado na POC e sera medido, nao mascarado.
#   - A stoplist de dominio (config/stopwords_dominio.txt) afeta o resultado;
#     e editavel e auditavel, nao hardcoded.
#
# Tecnologias: tidytext, stopwords, stringr, stringi, lubridate, dplyr, tidyr
# =============================================================================

library(tidytext)
library(stopwords)
library(stringr)
library(stringi)
library(lubridate)
library(dplyr)
library(tidyr)
library(tibble)
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

STOPWORDS_DOMINIO_PATH <- "config/stopwords_dominio.txt"

# Marcadores que delimitam o inicio do bloco de assinatura eletronica no SEI.
# Confirmados por inspecao dos textos reais extraidos.
MARCADORES_ASSINATURA <- c(
  "Documento assinado eletronicamente por",
  "A autenticidade deste documento"
)

# Meses por extenso (para extracao de datas textuais)
MESES_PT <- c(
  "janeiro" = 1, "fevereiro" = 2, "marco" = 3, "março" = 3, "abril" = 4,
  "maio" = 5, "junho" = 6, "julho" = 7, "agosto" = 8, "setembro" = 9,
  "outubro" = 10, "novembro" = 11, "dezembro" = 12
)

# -----------------------------------------------------------------------------
# Carregamento de stopwords
# -----------------------------------------------------------------------------

#' Carrega stopwords pt-br (pacote stopwords) + stoplist de dominio (config).
#' @param caminho_dominio Caminho do arquivo de stopwords de dominio
#' @return character com o vetor unido de stopwords (sem acento normalizado a parte)
carregar_stopwords <- function(caminho_dominio = STOPWORDS_DOMINIO_PATH) {
  sw_pt <- stopwords::stopwords("pt", source = "snowball")

  sw_dom <- character(0)
  if (file.exists(caminho_dominio)) {
    linhas <- readLines(caminho_dominio, warn = FALSE, encoding = "UTF-8")
    linhas <- str_squish(linhas)
    linhas <- linhas[linhas != "" & !str_starts(linhas, "#")]
    sw_dom <- linhas
  }

  unique(c(sw_pt, sw_dom))
}

# -----------------------------------------------------------------------------
# 1. Separacao de boilerplate de assinatura
# -----------------------------------------------------------------------------

#' Separa o corpo util do bloco de assinatura eletronica.
#'
#' O SEI adiciona um rodape padrao com "Documento assinado eletronicamente
#' por ...". Cortamos no PRIMEIRO marcador encontrado. O bloco cortado e
#' guardado para reaproveitamento na etapa 04_assinaturas.R.
#'
#' @param texto Texto completo do documento (corpo + rodape)
#' @return list(corpo_util, bloco_assinaturas, cortado)
separar_boilerplate <- function(texto) {
  if (is.na(texto) || texto == "") {
    return(list(corpo_util = texto, bloco_assinaturas = NA_character_, cortado = FALSE))
  }

  # Encontra a primeira posicao de qualquer marcador
  posicoes <- vapply(MARCADORES_ASSINATURA, function(m) {
    p <- str_locate(texto, fixed(m))[1, 1]
    if (is.na(p)) Inf else p
  }, numeric(1))

  corte <- min(posicoes)

  if (is.infinite(corte)) {
    # Nenhum marcador encontrado -> mantem tudo, registra que nao cortou
    return(list(corpo_util = str_squish(texto),
                bloco_assinaturas = NA_character_,
                cortado = FALSE))
  }

  corpo <- str_squish(substr(texto, 1, corte - 1))
  bloco <- str_squish(substr(texto, corte, nchar(texto)))
  list(corpo_util = corpo, bloco_assinaturas = bloco, cortado = TRUE)
}

# -----------------------------------------------------------------------------
# 2. Normalizacao
# -----------------------------------------------------------------------------

#' Normaliza texto para tokenizacao: minusculas, colapso de espacos.
#' Mantem acentos (decisao de design). Remove caracteres de controle.
#' @param texto character
#' @return character normalizado
normalizar_texto <- function(texto) {
  if (is.na(texto)) return(NA_character_)
  t <- stri_trans_tolower(texto)
  t <- stri_replace_all_regex(t, "[\\p{Cc}\\p{Cf}]", " ")  # controle/format
  str_squish(t)
}

# -----------------------------------------------------------------------------
# 3. Extracao de datas
# -----------------------------------------------------------------------------

#' Extrai datas do texto em dois formatos:
#'   - numerico  : dd/mm/aaaa
#'   - por extenso: dd de <mes> de aaaa  (com ou sem "de" antes do ano)
#'
#' @param texto Texto (corpo util, preferencialmente)
#' @return tibble(data_iso, data_original, formato)
extrair_datas <- function(texto) {
  if (is.na(texto) || texto == "") {
    return(tibble(data_iso = as.Date(character(0)),
                  data_original = character(0),
                  formato = character(0)))
  }

  resultados <- list()

  # --- Formato numerico dd/mm/aaaa ---
  num <- str_extract_all(texto, "\\b(\\d{2})/(\\d{2})/(\\d{4})\\b")[[1]]
  if (length(num) > 0) {
    datas_num <- suppressWarnings(dmy(num))
    resultados[[length(resultados) + 1]] <- tibble(
      data_iso = as.Date(datas_num),
      data_original = num,
      formato = "numerico"
    )
  }

  # --- Formato por extenso: dd de <mes> [de] aaaa ---
  padrao_ext <- "\\b(\\d{1,2})\\s+de\\s+([A-Za-zçãéâôóí]+)\\s+(?:de\\s+)?(\\d{4})\\b"
  m <- str_match_all(texto, padrao_ext)[[1]]
  if (nrow(m) > 0) {
    dias  <- as.integer(m[, 2])
    meses_txt <- stri_trans_tolower(m[, 3])
    anos  <- as.integer(m[, 4])
    meses_num <- MESES_PT[meses_txt]
    validos <- !is.na(meses_num)
    if (any(validos)) {
      datas_ext <- as.Date(sprintf("%04d-%02d-%02d",
                                   anos[validos], meses_num[validos], dias[validos]))
      resultados[[length(resultados) + 1]] <- tibble(
        data_iso = datas_ext,
        data_original = m[validos, 1],
        formato = "extenso"
      )
    }
  }

  if (length(resultados) == 0) {
    return(tibble(data_iso = as.Date(character(0)),
                  data_original = character(0),
                  formato = character(0)))
  }

  bind_rows(resultados) |> distinct()
}

# -----------------------------------------------------------------------------
# 3b. Stopwords dinamicas: nomes de signatarios
# -----------------------------------------------------------------------------

# Padrao fixo do rodape do SEI: "... assinado eletronicamente por <NOME>, <CARGO>"
PADRAO_SIGNATARIO <- "assinado eletronicamente por\\s+([^,]+),"

#' Extrai tokens dos nomes de signatarios de um bloco de assinatura.
#'
#' Os nomes vazam para o corpo do documento (assinatura visual) e, por serem
#' raros no corpus, sobem indevidamente no TF-IDF. Como o rodape do SEI segue
#' um padrao fixo, extraimos o nome real de forma auditavel — sem inventar —
#' e o usamos como stopword dinamica especifica do processo.
#'
#' @param bloco_assinaturas character (um ou mais blocos concatenados)
#' @return character com tokens minusculos dos nomes (comprimento > 2)
extrair_nomes_signatarios <- function(bloco_assinaturas) {
  blocos <- bloco_assinaturas[!is.na(bloco_assinaturas)]
  if (length(blocos) == 0) return(character(0))

  nomes <- unlist(lapply(blocos, function(txt) {
    m <- regmatches(txt, gregexpr(PADRAO_SIGNATARIO, txt, ignore.case = TRUE))[[1]]
    if (length(m) == 0) return(character(0))
    sub(PADRAO_SIGNATARIO, "\\1", m, ignore.case = TRUE)
  }))
  if (length(nomes) == 0) return(character(0))

  tokens <- unlist(strsplit(stri_trans_tolower(nomes), "\\s+"))
  tokens <- tokens[nchar(tokens) > 2]
  unique(tokens)
}

# -----------------------------------------------------------------------------
# 4. Tokenizacao + stopwords
# -----------------------------------------------------------------------------

#' Tokeniza um texto ja normalizado em palavras, removendo stopwords,
#' numeros isolados e tokens de 1-2 caracteres.
#' @param texto_norm Texto normalizado
#' @param numero_documento Id para rastrear o token ao documento
#' @param stopwords_vec Vetor de stopwords
#' @return tibble(numero_documento, token)
tokenizar <- function(texto_norm, numero_documento, stopwords_vec) {
  if (is.na(texto_norm) || texto_norm == "") {
    return(tibble(numero_documento = character(0), token = character(0)))
  }

  df <- tibble(numero_documento = numero_documento, texto = texto_norm)
  df |>
    unnest_tokens(token, texto, token = "words") |>
    filter(!token %in% stopwords_vec) |>
    filter(!str_detect(token, "^[0-9]+$")) |>       # numeros isolados
    filter(str_length(token) > 2)                    # tokens muito curtos
}

# -----------------------------------------------------------------------------
# 5. TF-IDF
# -----------------------------------------------------------------------------

#' Calcula TF-IDF sobre o conjunto de tokens de todos os documentos do processo.
#' A estatística tf-idf tem como objetivo medir a importância de uma palavra para um documento em uma coleção 
#' (ou corpus) de documentos, por exemplo, para um romance em uma coleção de romances ou para um site em uma coleção de sites.
#' Term Frequency (TF) — usa o número de vezes, um termo ‘t’ aparece em um documento
#' Frequência inversa do documento (IDF) — idf é uma medida de quão comum ou raro um termo é em todo os documentos.
#' @param tokens_df tibble(numero_documento, token)
#' @return tibble com tf, idf, tf_idf por (documento, token)
calcular_tfidf <- function(tokens_df) {
  if (nrow(tokens_df) == 0) {
    return(tibble(numero_documento = character(0), token = character(0),
                  n = integer(0), tf = double(0), idf = double(0), tf_idf = double(0)))
  }
  contagem <- tokens_df |>
    count(numero_documento, token, name = "n")
  contagem |>
    bind_tf_idf(token, numero_documento, n)
}

# -----------------------------------------------------------------------------
# Funcao principal: text_mining_processo()
# -----------------------------------------------------------------------------

#' Executa o pipeline de text mining para um processo.
#'
#' @param resultado_conteudo list produzida por extrair_conteudo_documentos()
#'        OU caminho para o JSON "_conteudo_" (character).
#' @param dir_processed Diretorio de saida
#' @param persistir Logico — salvar JSON
#' @param top_n_termos Quantos termos de maior TF-IDF guardar por documento
#' @return list com: por_documento, tfidf, meta
text_mining_processo <- function(
    resultado_conteudo,
    dir_processed = "data/processed",
    persistir     = TRUE,
    top_n_termos  = 20
) {
  # Aceita caminho de JSON ou objeto em memoria
  if (is.character(resultado_conteudo)) {
    if (!file.exists(resultado_conteudo)) {
      stop("[textmining] JSON de conteudo nao encontrado: ", resultado_conteudo)
    }
    resultado_conteudo <- jsonlite::fromJSON(resultado_conteudo, simplifyVector = TRUE)
  }

  numero_processo <- resultado_conteudo$meta$numero_processo
  numero_seguro   <- str_replace_all(numero_processo, "[^0-9A-Za-z]", "_")

  docs <- resultado_conteudo$documentos_conteudo
  docs_com_texto <- docs[!is.na(docs$conteudo_extraido) & docs$conteudo_extraido == TRUE, ]

  cat("[textmining] Processo:", numero_processo, "\n")
  cat("[textmining] Documentos com texto:", nrow(docs_com_texto), "\n")

  stopwords_vec <- carregar_stopwords()
  cat("[textmining] Stopwords carregadas:", length(stopwords_vec), "\n")

  if (nrow(docs_com_texto) == 0) {
    cat("[textmining] AVISO: nenhum documento com texto. Nada a processar.\n")
    saida <- list(
      meta = list(numero_processo = numero_processo,
                  timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                  documentos_processados = 0L),
      por_documento = list(),
      tfidf = tibble()
    )
    return(invisible(saida))
  }

  # 1a passada: separar boilerplate de cada documento. Precisamos dos blocos de
  # assinatura ANTES de tokenizar, para derivar as stopwords de signatarios e
  # remover os nomes que vazam para o corpo (evita que subam no TF-IDF).
  seps <- lapply(seq_len(nrow(docs_com_texto)), function(i) {
    separar_boilerplate(docs_com_texto$texto[i])
  })
  blocos_assinatura <- vapply(seps, function(s) {
    if (is.null(s$bloco_assinaturas)) NA_character_ else s$bloco_assinaturas
  }, character(1))

  stopwords_signatarios <- extrair_nomes_signatarios(blocos_assinatura)
  stopwords_efetivas <- unique(c(stopwords_vec, stopwords_signatarios))
  cat("[textmining] Signatarios como stopword:", length(stopwords_signatarios), "\n")

  # 2a passada: normalizar, extrair datas e tokenizar (ja sem signatarios)
  por_documento <- list()
  tokens_todos  <- list()

  for (i in seq_len(nrow(docs_com_texto))) {
    numero <- docs_com_texto$numero_documento[i]
    tipo   <- docs_com_texto$tipo[i]

    sep    <- seps[[i]]
    norm   <- normalizar_texto(sep$corpo_util)
    datas  <- extrair_datas(sep$corpo_util)
    tokens <- tokenizar(norm, numero, stopwords_efetivas)

    tokens_todos[[length(tokens_todos) + 1]] <- tokens

    por_documento[[numero]] <- list(
      numero_documento   = numero,
      tipo               = tipo,
      boilerplate_cortado = sep$cortado,
      n_tokens           = nrow(tokens),
      datas_encontradas  = datas,
      bloco_assinaturas  = sep$bloco_assinaturas
    )
  }

  tokens_df <- bind_rows(tokens_todos)
  tfidf_df  <- calcular_tfidf(tokens_df)

  # Top termos por documento (por tf_idf)
  top_termos <- tfidf_df |>
    group_by(numero_documento) |>
    slice_max(tf_idf, n = top_n_termos, with_ties = FALSE) |>
    ungroup() |>
    arrange(numero_documento, desc(tf_idf))

  # Metricas
  n_datas_total <- sum(vapply(por_documento, function(d) nrow(d$datas_encontradas), integer(1)))
  n_sem_corte   <- sum(vapply(por_documento, function(d) !d$boilerplate_cortado, logical(1)))

  cat("[textmining] --- Resumo ---\n")
  cat("[textmining] Tokens totais    :", nrow(tokens_df), "\n")
  cat("[textmining] Vocabulario unico:", dplyr::n_distinct(tokens_df$token), "\n")
  cat("[textmining] Datas extraidas  :", n_datas_total, "\n")
  if (n_sem_corte > 0) {
    cat("[textmining] AVISO: ", n_sem_corte,
        " documento(s) sem marcador de assinatura (boilerplate nao cortado).\n")
  }

  saida <- list(
    meta = list(
      numero_processo        = numero_processo,
      timestamp              = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      documentos_processados = nrow(docs_com_texto),
      tokens_totais          = nrow(tokens_df),
      vocabulario_unico      = dplyr::n_distinct(tokens_df$token),
      datas_extraidas_total  = n_datas_total,
      docs_sem_corte_boiler  = n_sem_corte,
      signatarios_stopword   = length(stopwords_signatarios)
    ),
    por_documento = por_documento,
    top_termos    = top_termos,
    tfidf         = tfidf_df
  )

  if (persistir) {
    data_str <- format(Sys.time(), "%Y%m%d")
    nome     <- paste0(numero_seguro, "_textmining_", data_str, ".json")
    caminho  <- file.path(dir_saida_dia(dir_processed), nome)
    write_json(saida, caminho, pretty = TRUE, auto_unbox = TRUE, na = "null")
    cat("[textmining] Persistido em:", caminho, "\n")
  }

  invisible(saida)
}