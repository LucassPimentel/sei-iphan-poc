# =============================================================================
# 03_parser_documentos.R
# Extracao de conteudo dos documentos do SEI Pesquisa Publica do IPHAN
#
# Responsabilidade: Camada de Conteudo (ver architecture.md)
#   - Recebe a lista de documentos produzida por 02_sei_scraping.R
#   - Baixa a pagina de cada documento publico
#   - Distingue documentos NATIVOS (HTML gerado no SEI) de ANEXOS (binarios)
#   - Extrai o texto limpo apenas dos nativos elegiveis (Despacho, Oficio, Parecer)
#   - Registra explicitamente documentos sem texto e restritos como limitacao
#   - Persiste o resultado em data/processed/
#
# Regra de escopo (definida com o pesquisador):
#   - TODOS os documentos sao listados (feito na etapa de scraping).
#   - O conteudo textual e extraido SOMENTE dos tipos nativos configurados
#     em config/extracao_conteudo.json (Despacho, Oficio, Parecer),
#     E que respondam com Content-Type text/html.
#   - A semantica processual (pendencia/demanda/avaliacao) NAO e tratada aqui;
#     fica para a etapa de Classificacao (06_classificacao.R).
#
# Deteccao nativo vs anexo (confirmada por inspecao real do SEI):
#   - Nativo : resposta HTTP com Content-Type text/html; texto no <body>;

#              title no padrao "SEI/IPHAN - <numero> - <Tipo>".
#   - Anexo  : resposta HTTP binaria (ex.: application/octet-stream) para download.
#
# Tecnologias: httr2, rvest, xml2, stringr, jsonlite (pdftools reservado p/ futuro)
#
# Limitacoes registradas (ver research-methodology.md):
#   - Documentos restritos nao sao acessados; registrados como indisponiveis.
#   - Anexos (Word, planilha, geoespacial, PDF) nao tem texto extraido nesta etapa.
#   - PDFs podem exigir OCR futuramente; registrado como limitacao quando ocorrer.
# =============================================================================

library(httr2)
library(rvest)
library(xml2)
library(stringr)
library(jsonlite)
library(tibble)
library(dplyr)

# Utilitario de pastas por data (dir_saida_dia). Carrega se ausente.
if (!exists("dir_saida_dia")) {
  .cand_utils <- c("R/utils_saida.R", "utils_saida.R",
                   file.path(tryCatch(dirname(sys.frame(1)$ofile),
                                      error = function(e) NA), "utils_saida.R"))
  .cand_utils <- .cand_utils[!is.na(.cand_utils) & file.exists(.cand_utils)]
  if (length(.cand_utils) > 0) source(.cand_utils[1])
}
library(purrr)

# -----------------------------------------------------------------------------
# Constantes
# -----------------------------------------------------------------------------

CONTEUDO_CONFIG_PATH <- "config/extracao_conteudo.json"

# -----------------------------------------------------------------------------
# Carregamento de configuracao (tipos nativos como dado, nao hardcode)
# -----------------------------------------------------------------------------

#' Carrega a configuracao de extracao de conteudo.
#' @param caminho Caminho do JSON de configuracao
#' @return list com os campos de configuracao
carregar_config_conteudo <- function(caminho = CONTEUDO_CONFIG_PATH) {
  if (!file.exists(caminho)) {
    stop("[parser] Config nao encontrada: ", caminho)
  }
  jsonlite::fromJSON(caminho, simplifyVector = TRUE)
}

#' Determina se um tipo de documento e nativo elegivel para extracao.
#' Comparacao case-insensitive e por prefixo (ex.: "Despacho 1619" -> "Despacho").
#' @param tipo_documento Texto do tipo (coluna 'tipo' do scraping)
#' @param tipos_nativos Vetor de tipos nativos configurados
#' @return logical
.e_tipo_nativo <- function(tipo_documento, tipos_nativos) {
  if (is.na(tipo_documento) || tipo_documento == "") return(FALSE)
  tipo_norm <- .normalizar_ascii(tolower(str_squish(tipo_documento)))
  padroes   <- .normalizar_ascii(tolower(tipos_nativos))
  padroes   <- unique(padroes)
  any(vapply(padroes, function(p) str_starts(tipo_norm, fixed(p)), logical(1)))
}

#' Remove acentos para comparacao robusta (Oficio == Ofício).
.normalizar_ascii <- function(x) {
  iconv(x, to = "ASCII//TRANSLIT", sub = "")
}

# -----------------------------------------------------------------------------
# Extracao de texto de um documento nativo (HTML)
# -----------------------------------------------------------------------------

# Elementos de bloco / quebra que devem gerar separacao de texto. Sem o
# separador, html_text() concatena o texto de nos adjacentes sem espaco
# (ex.: "SP" + "IPHAN" -> "spiphan"), gerando tokens colados no text mining.
ELEMENTOS_SEPARADORES <- c("br", "p", "div", "td", "tr", "th",
                           "li", "h1", "h2", "h3", "h4", "h5", "h6")

#' Insere um separador de texto (quebra de linha) apos elementos de bloco.
#'
#' Manipula o DOM adicionando um no de texto "\n" como irmao seguinte de cada
#' elemento de bloco/quebra. Isso garante que html_text() nao cole o texto de
#' elementos adjacentes. Feito na origem para beneficiar todas as etapas
#' seguintes (text mining, classificacao, event log).
#'
#' @param doc Documento xml2 (modificado por referencia)
#' @return o proprio doc (invisivel)
.inserir_separadores_bloco <- function(doc) {
  xpath <- paste0("//", ELEMENTOS_SEPARADORES, collapse = " | ")
  nos <- xml_find_all(doc, xpath)
  for (no in nos) {
    xml_add_sibling(no, .make_text_node(), .where = "after")
  }
  invisible(doc)
}

#' Cria um no de texto contendo uma quebra de linha, para uso como separador.
#' Usa um elemento temporario cujo texto e "\n" (xml2 nao cria text node solto).
.make_text_node <- function() {
  tmp <- xml2::read_xml("<sep>\n</sep>")
  xml2::xml_root(tmp)
}

#' Extrai o texto limpo do corpo de um documento nativo do SEI.
#'
#' O SEI serve o documento nativo como HTML completo, com o conteudo no <body>.
#' Removemos scripts/estilos, inserimos separadores entre elementos de bloco
#' (para nao colar palavras de nos adjacentes) e normalizamos espacos.
#'
#' @param html_string HTML ja decodificado para UTF-8
#' @return character com o texto limpo (pode ser "" se vazio)
.extrair_texto_nativo <- function(html_string) {
  doc  <- read_html(html_string)
  # Remover nós de script e style para não poluir o texto
  xml_remove(xml_find_all(doc, "//script | //style"))
  # Inserir separadores para evitar tokens colados na extracao
  .inserir_separadores_bloco(doc)
  corpo <- html_element(doc, "body")
  if (is.na(corpo)) return("")
  str_squish(html_text(corpo, trim = TRUE))
}

#' Extrai o titulo do documento nativo (padrao "SEI/IPHAN - <n> - <Tipo>").
.extrair_titulo_nativo <- function(html_string) {
  doc <- read_html(html_string)
  t <- html_element(doc, "title")
  if (is.na(t)) return(NA_character_)
  str_squish(html_text(t, trim = TRUE))
}

# -----------------------------------------------------------------------------
# Processamento de um unico documento
# -----------------------------------------------------------------------------

#' Baixa e processa o conteudo de um unico documento.
#'
#' Fluxo:
#'   1. Se restrito ou sem URL -> registra indisponibilidade (sem requisicao).
#'   2. Se o tipo NAO e nativo elegivel -> nao extrai (lista, mas marca motivo).
#'   3. Se e nativo elegivel -> baixa a pagina:
#'        - Content-Type text/html  -> extrai texto.
#'        - Content-Type binario    -> registra como anexo (nao extraido).
#'
#' @param doc_row Uma linha (lista) da tabela de documentos do scraping
#' @param tipos_nativos Vetor de tipos nativos configurados
#' @param timeout_seg Timeout da requisicao em segundos
#' @param pausa_seg Pausa apos a requisicao (civilidade com o servidor)
#' @return tibble de uma linha com o resultado da extracao
.processar_documento <- function(doc_row, tipos_nativos,
                                  timeout_seg = 30, pausa_seg = 0.5) {
  numero  <- doc_row$numero_documento
  tipo    <- doc_row$tipo
  acesso  <- doc_row$acesso
  url_doc <- doc_row$url_documento

  base_result <- tibble(
    numero_documento = numero,
    tipo             = tipo,
    acesso           = acesso,
    e_nativo         = FALSE,
    conteudo_extraido = FALSE,
    motivo           = NA_character_,
    content_type     = NA_character_,
    titulo           = NA_character_,
    texto            = NA_character_,
    n_caracteres     = 0L
  )

  # 1. Restrito ou sem URL -> indisponivel
  if (!is.na(acesso) && acesso == "restrito") {
    base_result$motivo <- "documento_restrito"
    return(base_result)
  }
  if (is.na(url_doc) || url_doc == "") {
    base_result$motivo <- "sem_url"
    return(base_result)
  }

  # 2. Tipo nao nativo -> nao extrai (mas fica registrado na lista)
  nativo <- .e_tipo_nativo(tipo, tipos_nativos)
  base_result$e_nativo <- nativo
  if (!nativo) {
    base_result$motivo <- "tipo_nao_nativo"
    return(base_result)
  }

  # 3. Nativo elegivel -> baixar
  resp <- tryCatch(
    request(url_doc) |>
      req_headers(
        `Accept`          = "text/html,application/xhtml+xml",
        `Accept-Language` = "pt-BR,pt;q=0.9",
        `User-Agent`      = "Mozilla/5.0 (compatible; POC-IPHAN-Mining/1.0)"
      ) |>
      req_timeout(timeout_seg) |>
      req_perform(),
    error = function(e) e
  )

  Sys.sleep(pausa_seg)

  if (inherits(resp, "error")) {
    base_result$motivo <- paste0("erro_requisicao: ", conditionMessage(resp))
    return(base_result)
  }

  ct <- resp_content_type(resp)
  base_result$content_type <- ct

  # Nativo confirmado somente se responder HTML
  if (!grepl("html", ct, ignore.case = TRUE)) {
    base_result$e_nativo <- FALSE
    base_result$motivo   <- paste0("resposta_nao_html: ", ct)
    return(base_result)
  }

  bytes <- resp_body_raw(resp)
  html_string <- iconv(rawToChar(bytes), from = "iso-8859-1", to = "UTF-8", sub = "?")

  texto  <- .extrair_texto_nativo(html_string)
  titulo <- .extrair_titulo_nativo(html_string)

  if (is.na(texto) || nchar(texto) == 0) {
    base_result$titulo <- titulo
    base_result$motivo <- "documento_sem_texto"
    return(base_result)
  }

  base_result$conteudo_extraido <- TRUE
  base_result$titulo            <- titulo
  base_result$texto             <- texto
  base_result$n_caracteres      <- nchar(texto)
  base_result$motivo            <- "extraido"
  base_result
}

# -----------------------------------------------------------------------------
# Funcao principal: extrair_conteudo_documentos()
# -----------------------------------------------------------------------------

#' Extrai o conteudo textual dos documentos nativos de um processo.
#'
#' Recebe o resultado do scraping (lido do JSON de data/processed/ ou o objeto
#' em memoria) e produz, para cada documento, o registro de extracao.
#'
#' @param resultado_scraping list produzido por scrape_processo_sei()
#' @param config Configuracao de extracao (list) ou NULL para carregar padrao
#' @param dir_processed Diretorio de saida
#' @param persistir Logico — salvar JSON em data/processed/
#' @return list com: documentos_conteudo (tibble), meta (list)
extrair_conteudo_documentos <- function(
    resultado_scraping,
    config        = NULL,
    dir_processed = "data/processed",
    persistir     = TRUE
) {
  if (is.null(config)) config <- carregar_config_conteudo()
  tipos_nativos <- config$tipos_nativos_extrair

  documentos <- resultado_scraping$documentos
  numero_processo <- resultado_scraping$cabecalho$numero_processo
  numero_seguro   <- str_replace_all(numero_processo, "[^0-9A-Za-z]", "_")

  cat("[parser] Processo:", numero_processo, "\n")
  cat("[parser] Documentos a avaliar:", nrow(documentos), "\n")
  cat("[parser] Tipos nativos configurados:", paste(unique(tipos_nativos), collapse = ", "), "\n")

  # Processar cada documento (documentos e um data.frame; iteramos por linha)
  linhas <- split(documentos, seq_len(nrow(documentos)))
  resultado_docs <- purrr::map_dfr(linhas, function(l) {
    .processar_documento(as.list(l), tipos_nativos)
  })

  # Metricas de cobertura
  n_total     <- nrow(resultado_docs)
  n_nativos   <- sum(resultado_docs$e_nativo, na.rm = TRUE)
  n_extraidos <- sum(resultado_docs$conteudo_extraido, na.rm = TRUE)
  n_restritos <- sum(resultado_docs$motivo == "documento_restrito", na.rm = TRUE)
  n_sem_texto <- sum(resultado_docs$motivo == "documento_sem_texto", na.rm = TRUE)
  n_anexos    <- sum(resultado_docs$motivo == "tipo_nao_nativo", na.rm = TRUE)

  cat("[parser] --- Resumo ---\n")
  cat("[parser] Nativos elegiveis :", n_nativos, "\n")
  cat("[parser] Conteudo extraido :", n_extraidos, "\n")
  cat("[parser] Restritos         :", n_restritos, "\n")
  cat("[parser] Sem texto         :", n_sem_texto, "\n")
  cat("[parser] Anexos/nao-nativos:", n_anexos, "\n")

  saida <- list(
    meta = list(
      numero_processo   = numero_processo,
      timestamp_extracao = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      total_documentos  = n_total,
      nativos_elegiveis = n_nativos,
      conteudo_extraido = n_extraidos,
      restritos         = n_restritos,
      sem_texto         = n_sem_texto,
      anexos_nao_nativos = n_anexos,
      cobertura_conteudo = if (n_nativos > 0) round(n_extraidos / n_nativos, 4) else NA_real_
    ),
    documentos_conteudo = resultado_docs
  )

  if (persistir) {
    data_str <- format(Sys.time(), "%Y%m%d")
    nome     <- paste0(numero_seguro, "_conteudo_", data_str, ".json")
    caminho  <- file.path(dir_saida_dia(dir_processed), nome)
    write_json(saida, caminho, pretty = TRUE, auto_unbox = TRUE, na = "null")
    cat("[parser] Conteudo persistido em:", caminho, "\n")
  }

  invisible(saida)
}

#' Conveniencia: extrai conteudo a partir de um JSON de scraping salvo.
#' @param caminho_json Caminho do JSON produzido por 02_sei_scraping.R
#' @param ... Repassado para extrair_conteudo_documentos()
extrair_conteudo_de_json <- function(caminho_json, ...) {
  if (!file.exists(caminho_json)) {
    stop("[parser] JSON de scraping nao encontrado: ", caminho_json)
  }
  resultado <- jsonlite::fromJSON(caminho_json, simplifyVector = TRUE)
  extrair_conteudo_documentos(resultado, ...)
}