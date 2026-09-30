# =============================================================================
# 02_sei_scraping.R
# Scraping do SEI Pesquisa Pública do IPHAN
#
# Responsabilidade: Camada de Aquisição
#   - Recebe URL de processo do SEI Pesquisa Pública
#   - Realiza a requisição HTTP
#   - Salva o HTML bruto em data/raw/
#   - Extrai metadados do cabeçalho, lista de documentos e andamentos
#   - Identifica documentos restritos (registra como indisponível)
#   - Persiste os dados estruturados em data/processed/
#
# Tecnologias: httr2, rvest, xml2
# RSelenium: NÃO utilizado — página serve HTML estático via GET
#
# Seletores documentados a partir da inspeção real do HTML em:
#   Paginas/Pagina Processo Buscado.html (processo 01450.001620/2026-78)
#
# Limitações registradas:
#   - Documentos restritos: visíveis na lista mas sem link (sei_chave_restrito.svg)
#   - Charset da página: iso-8859-1 (tratado no parse)
#   - A URL do SEI contém token de sessão com validade desconhecida
# =============================================================================

library(httr2)
library(rvest)
library(xml2)
library(tidyverse)
library(lubridate)
library(stringr)
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
# Constantes — seletores CSS confirmados pela inspeção do HTML real
# -----------------------------------------------------------------------------

SEI_BASE_URL      <- "https://sei.iphan.gov.br"
SEI_CHARSET       <- "iso-8859-1"

# IDs das tabelas principais (confirmados no HTML inspecionado)
SEL_TABELA_CABECALHO  <- "#tblCabecalho"
SEL_TABELA_DOCUMENTOS <- "#tblDocumentos"
SEL_TABELA_HISTORICO  <- "#tblHistorico"

# Classes das linhas de documento
SEL_LINHA_DOCUMENTO   <- "tr.infraTrClara"

# Classe de link para documento público (confirmada)
SEL_LINK_DOCUMENTO_PUBLICO <- "a.ancoraPadraoAzul"

# Imagem que marca documento restrito (confirmada)
SEL_IMG_RESTRITO <- "img[src*='sei_chave_restrito']"

# Span que substitui o link em documentos restritos (confirmado)
SEL_SPAN_RESTRITO <- "span.retiraAncoraPadraoAzul"

# Classe de link para sigla de unidade (confirmada)
SEL_UNIDADE_SIGLA <- "a.ancoraSigla"

# Classes das linhas de andamento (confirmadas)
SEL_ANDAMENTO_ABERTO    <- "tr.andamentoAberto"
SEL_ANDAMENTO_CONCLUIDO <- "tr.andamentoConcluido"

# Mensagem que aparece quando o processo inteiro é restrito (confirmada)
TEXTO_PROCESSO_RESTRITO <- "Processo (ou Documento) de acesso restrito"

# -----------------------------------------------------------------------------
# Funções auxiliares internas
# -----------------------------------------------------------------------------

#' Extrai texto limpo de um nó HTML, removendo espaços redundantes.
#' @param no Nó xml_node (rvest)
#' @return character
.extrair_texto <- function(no) {
  texto <- html_text(no, trim = TRUE)
  str_squish(texto)
}

#' Constrói URL absoluta a partir de caminho relativo encontrado no onclick.
#' Os links de documentos no SEI usam window.open('md_pesq_documento_consulta_externa.php?TOKEN')
#' @param onclick_valor Conteúdo do atributo onclick
#' @return character — URL absoluta ou NA
.url_documento_onclick <- function(onclick_valor) {
  if (is.na(onclick_valor) || onclick_valor == "") return(NA_character_)
  # Extrai o caminho entre aspas simples dentro de window.open(...)
  m <- str_match(onclick_valor, "window\\.open\\('([^']+)'\\)")
  if (is.na(m[1, 2])) return(NA_character_)
  caminho <- m[1, 2]
  # Caminho relativo — base é o módulo de pesquisa
  paste0(SEI_BASE_URL, "/sei/modulos/pesquisa/", caminho)
}

# -----------------------------------------------------------------------------
# Função principal: scrape_processo_sei()
# -----------------------------------------------------------------------------

#' Realiza o scraping de uma página de processo do SEI Pesquisa Pública.
#'
#' Etapas:
#'   1. Requisição HTTP GET com encoding correto
#'   2. Salva HTML bruto em data/raw/
#'   3. Detecta se o processo completo é restrito
#'   4. Extrai cabeçalho (metadados do processo)
#'   5. Extrai lista de documentos (públicos e restritos)
#'   6. Extrai histórico de andamentos
#'   7. Persiste resultado estruturado em data/processed/ (JSON)
#'
#' @param url URL completa da página de processo no SEI
#' @param salvar_html Lógico — salvar HTML bruto em data/raw/ (padrão TRUE)
#' @param dir_raw Diretório para HTML bruto
#' @param dir_processed Diretório para dados processados
#' @return list com campos: cabecalho, documentos, andamentos, meta
scrape_processo_sei <- function(
    url,
    salvar_html   = TRUE,
    dir_raw       = "data/raw",
    dir_processed = "data/processed"
) {
  cat("[scrape] Iniciando para URL:", url, "\n")
  
  # ---- 1. Requisição HTTP ---------------------------------------------------
  resposta <- tryCatch({
    request(url) |>
      req_headers(
        `Accept`          = "text/html,application/xhtml+xml",
        `Accept-Language` = "pt-BR,pt;q=0.9",
        `User-Agent`      = "Mozilla/5.0 (compatible; POC-IPHAN-Mining/1.0)"
      ) |>
      req_timeout(30) |>
      req_perform()
  }, error = function(e) {
    stop("[scrape] Falha na requisição HTTP: ", conditionMessage(e))
  })
  
  status <- resp_status(resposta)
  cat("[scrape] HTTP status:", status, "\n")
  
  if (status != 200) {
    stop("[scrape] Resposta HTTP inesperada: ", status)
  }
  
  # ---- 2. Parse do HTML com encoding correto --------------------------------
  # O SEI usa iso-8859-1; forçamos o encoding para evitar caracteres corrompidos
  html_bytes  <- resp_body_raw(resposta)
  html_string <- iconv(rawToChar(html_bytes), from = SEI_CHARSET, to = "UTF-8", sub = "?")
  doc         <- read_html(html_string)
  
  # ---- 3. Salvar HTML bruto -------------------------------------------------
  # Usar data como sufixo: um arquivo por processo por dia. Recoleta no mesmo dia sobrescreve.
  data_str <- format(Sys.time(), "%Y%m%d")
  # Extrair número do processo do cabeçalho para nomear o arquivo
  numero_processo <- .extrair_numero_processo(doc)
  numero_seguro   <- str_replace_all(numero_processo, "[^0-9A-Za-z]", "_")
  
  if (salvar_html) {
    dir_raw_dia <- dir_saida_dia(dir_raw)
    nome_html  <- paste0(numero_seguro, "_", data_str, ".html")
    caminho_html <- file.path(dir_raw_dia, nome_html)
    writeLines(html_string, caminho_html, useBytes = FALSE)
    cat("[scrape] HTML bruto salvo em:", caminho_html, "\n")
  } else {
    caminho_html <- NA_character_
  }
  
  # ---- 4. Detectar restrição total do processo ------------------------------
  processo_restrito <- .detectar_processo_restrito(doc)
  if (processo_restrito) {
    cat("[scrape] AVISO: Processo com acesso restrito. Apenas cabeçalho disponível.\n")
  }
  
  # ---- 5. Extrair cabeçalho -------------------------------------------------
  cabecalho <- .extrair_cabecalho(doc)
  cat("[scrape] Processo:", cabecalho$numero_processo, "| Tipo:", cabecalho$tipo, "\n")
  
  # ---- 6. Extrair documentos ------------------------------------------------
  documentos <- .extrair_documentos(doc)
  n_pub  <- sum(documentos$acesso == "publico",   na.rm = TRUE)
  n_rest <- sum(documentos$acesso == "restrito",  na.rm = TRUE)
  cat("[scrape] Documentos encontrados:", nrow(documentos),
      "| Públicos:", n_pub, "| Restritos:", n_rest, "\n")
  
  # ---- 7. Extrair andamentos ------------------------------------------------
  andamentos <- .extrair_andamentos(doc)
  cat("[scrape] Andamentos encontrados:", nrow(andamentos), "\n")
  
  # ---- 8. Montar e persistir resultado --------------------------------------
  resultado <- list(
    meta = list(
      url            = url,
      timestamp_coleta = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      html_salvo_em  = caminho_html,
      processo_restrito = processo_restrito,
      total_documentos  = nrow(documentos),
      documentos_publicos  = n_pub,
      documentos_restritos = n_rest,
      total_andamentos = nrow(andamentos)
    ),
    cabecalho  = cabecalho,
    documentos = documentos,
    andamentos = andamentos
  )
  
  dir_processed_dia <- dir_saida_dia(dir_processed)
  nome_json    <- paste0(numero_seguro, "_", data_str, ".json")
  caminho_json <- file.path(dir_processed_dia, nome_json)
  write_json(resultado, caminho_json, pretty = TRUE, auto_unbox = TRUE, na = "null")
  cat("[scrape] Dados persistidos em:", caminho_json, "\n")
  
  invisible(resultado)
}

# -----------------------------------------------------------------------------
# Funções internas de extração
# -----------------------------------------------------------------------------

#' Extrai o número do processo do cabeçalho (#tblCabecalho).
#' Fallback: retorna "processo_desconhecido" se não encontrar.
.extrair_numero_processo <- function(doc) {
  tabela <- html_element(doc, SEL_TABELA_CABECALHO)
  if (is.na(tabela)) return("processo_desconhecido")
  linhas <- html_elements(tabela, "tr.infraTrClara")
  if (length(linhas) == 0) return("processo_desconhecido")
  # Primeira linha do cabeçalho: "Processo: XXXXXXXXX"
  celulas <- html_elements(linhas[[1]], "td")
  if (length(celulas) < 2) return("processo_desconhecido")
  str_squish(html_text(celulas[[2]], trim = TRUE))
}

#' Detecta se o processo inteiro tem acesso restrito (sem documentos acessíveis).
#'
#' Distinção importante baseada na inspeção do HTML real:
#'   - O parágrafo de aviso "Processo (ou Documento) de acesso restrito" aparece
#'     em qualquer processo que tenha pelo menos 1 documento restrito — mesmo
#'     quando o processo tem dezenas de documentos públicos.
#'   - Um processo verdadeiramente bloqueado não exibe a tabela #tblDocumentos
#'     com linhas de dados.
#'
#' Critério: processo restrito = tabela de documentos ausente OU sem linhas de dados.
.detectar_processo_restrito <- function(doc) {
  tabela <- html_element(doc, SEL_TABELA_DOCUMENTOS)
  if (is.na(tabela)) return(TRUE)
  linhas <- html_elements(tabela, "tr.infraTrClara")
  length(linhas) == 0
}

#' Extrai os metadados do cabeçalho do processo (#tblCabecalho).
#' Campos: numero_processo, tipo, data_geracao, interessados
.extrair_cabecalho <- function(doc) {
  tabela <- html_element(doc, SEL_TABELA_CABECALHO)
  
  resultado <- list(
    numero_processo = NA_character_,
    tipo            = NA_character_,
    data_geracao    = NA_character_,
    interessados    = NA_character_
  )
  
  if (is.na(tabela)) {
    warning("[cabecalho] Tabela #tblCabecalho não encontrada.")
    return(resultado)
  }
  
  linhas <- html_elements(tabela, "tr.infraTrClara")
  
  for (linha in linhas) {
    celulas <- html_elements(linha, "td")
    if (length(celulas) < 2) next
    
    rotulo <- str_squish(html_text(celulas[[1]], trim = TRUE))
    valor  <- str_squish(html_text(celulas[[2]], trim = TRUE))
    
    if (str_detect(rotulo, "^Processo")) {
      resultado$numero_processo <- valor
    } else if (str_detect(rotulo, "^Tipo")) {
      resultado$tipo <- valor
    } else if (str_detect(rotulo, "^Data de Gera")) {
      resultado$data_geracao <- valor
    } else if (str_detect(rotulo, "^Interessados")) {
      # Pode ser &nbsp; — normalizar para NA se vazio
      resultado$interessados <- if (valor == "" || valor == "\u00a0") NA_character_ else valor
    }
  }
  
  resultado
}

#' Extrai a lista de documentos/protocolos (#tblDocumentos).
#'
#' Para cada linha da tabela, determina:
#'   - numero_documento: ID numérico visível
#'   - tipo: tipo do documento (coluna "Tipo")
#'   - data_documento: data do documento
#'   - data_inclusao: data de inclusão no processo
#'   - unidade: sigla da unidade (atributo title = nome completo)
#'   - unidade_nome: nome completo da unidade
#'   - acesso: "publico" ou "restrito"
#'   - url_documento: URL absoluta (NA se restrito)
#'
#' Limitação documentada: documentos restritos não têm URL acessível.
.extrair_documentos <- function(doc) {
  tabela <- html_element(doc, SEL_TABELA_DOCUMENTOS)
  
  if (is.na(tabela)) {
    warning("[documentos] Tabela #tblDocumentos não encontrada.")
    return(tibble())
  }
  
  linhas <- html_elements(tabela, "tr.infraTrClara")
  
  if (length(linhas) == 0) {
    return(tibble())
  }
  
  purrr::map_dfr(linhas, .extrair_linha_documento)
}

#' Processa uma única linha <tr class="infraTrClara"> da tabela de documentos.
.extrair_linha_documento <- function(linha) {
  celulas <- html_elements(linha, "td")
  
  # A tabela de documentos tem colunas:
  # [1] checkbox (no-print)  [2] Nº documento  [3] Tipo  [4] Data  [5] Data Inclusão  [6] Unidade
  # Linhas sem checkbox (documentos filhos/sem seleção) têm apenas &nbsp; na col 1
  
  if (length(celulas) < 6) {
    return(NULL)
  }
  
  celula_numero  <- celulas[[2]]
  celula_tipo    <- celulas[[3]]
  celula_data    <- celulas[[4]]
  celula_inclusao <- celulas[[5]]
  celula_unidade <- celulas[[6]]
  
  # ---- Número do documento e acesso ----------------------------------------
  # Público: <a class="ancoraPadraoAzul"> com onclick
  # Restrito: <span class="retiraAncoraPadraoAzul"> (sem link)
  
  link_publico <- html_element(celula_numero, SEL_LINK_DOCUMENTO_PUBLICO)
  span_restrito <- html_element(celula_numero, SEL_SPAN_RESTRITO)
  img_restrito  <- html_element(celula_numero, SEL_IMG_RESTRITO)
  
  if (!is.na(link_publico)) {
    # Documento público
    numero_doc  <- str_squish(html_text(link_publico, trim = TRUE))
    onclick_val <- html_attr(link_publico, "onclick")
    url_doc     <- .url_documento_onclick(onclick_val)
    acesso      <- "publico"
  } else if (!is.na(span_restrito) || !is.na(img_restrito)) {
    # Documento restrito — registrar número e marcar como indisponível
    numero_doc <- str_squish(html_text(celula_numero, trim = TRUE))
    # Remove possível texto da imagem SVG colado ao número
    numero_doc <- str_extract(numero_doc, "^[0-9]+")
    url_doc    <- NA_character_
    acesso     <- "restrito"
  } else {
    # Estrutura não reconhecida — registrar para auditoria
    numero_doc <- str_squish(html_text(celula_numero, trim = TRUE))
    url_doc    <- NA_character_
    acesso     <- "desconhecido"
  }
  
  # ---- Tipo -----------------------------------------------------------------
  tipo <- str_squish(html_text(celula_tipo, trim = TRUE))
  
  # ---- Datas ----------------------------------------------------------------
  data_doc   <- str_squish(html_text(celula_data, trim = TRUE))
  data_incl  <- str_squish(html_text(celula_inclusao, trim = TRUE))
  
  # ---- Unidade --------------------------------------------------------------
  link_unidade <- html_element(celula_unidade, SEL_UNIDADE_SIGLA)
  if (!is.na(link_unidade)) {
    unidade_sigla <- str_squish(html_text(link_unidade, trim = TRUE))
    unidade_nome  <- html_attr(link_unidade, "title")
    if (is.na(unidade_nome)) unidade_nome <- html_attr(link_unidade, "alt")
  } else {
    unidade_sigla <- str_squish(html_text(celula_unidade, trim = TRUE))
    unidade_nome  <- NA_character_
  }
  
  tibble(
    numero_documento = numero_doc,
    tipo             = tipo,
    data_documento   = data_doc,
    data_inclusao    = data_incl,
    unidade_sigla    = unidade_sigla,
    unidade_nome     = unidade_nome,
    acesso           = acesso,
    url_documento    = url_doc
  )
}

#' Extrai o histórico de andamentos (#tblHistorico).
#'
#' Para cada andamento:
#'   - data_hora: data e hora do andamento (dd/mm/aaaa HH:MM)
#'   - unidade_sigla: sigla da unidade
#'   - unidade_nome: nome completo da unidade
#'   - descricao: texto da descrição do andamento
#'   - status: "aberto" ou "concluido"
#'   - id_atividade: atributo data-atividade da linha
.extrair_andamentos <- function(doc) {
  tabela <- html_element(doc, SEL_TABELA_HISTORICO)
  
  if (is.na(tabela)) {
    warning("[andamentos] Tabela #tblHistorico não encontrada.")
    return(tibble())
  }
  
  # Selecionar ambas as classes de andamento
  linhas_abertas    <- html_elements(tabela, SEL_ANDAMENTO_ABERTO)
  linhas_concluidas <- html_elements(tabela, SEL_ANDAMENTO_CONCLUIDO)
  
  # Processar preservando ordem original
  todas_linhas <- html_elements(tabela, "tr[class]")
  todas_linhas <- todas_linhas[
    html_attr(todas_linhas, "class") %in% c("andamentoAberto", "andamentoConcluido")
  ]
  
  if (length(todas_linhas) == 0) {
    return(tibble())
  }
  
  purrr::map_dfr(todas_linhas, .extrair_linha_andamento)
}

#' Processa uma única linha de andamento.
.extrair_linha_andamento <- function(linha) {
  classe_linha <- html_attr(linha, "class")
  id_atividade <- html_attr(linha, "data-atividade")
  status       <- if (classe_linha == "andamentoAberto") "aberto" else "concluido"
  
  celulas <- html_elements(linha, "td")
  
  if (length(celulas) < 3) {
    return(NULL)
  }
  
  data_hora <- str_squish(html_text(celulas[[1]], trim = TRUE))
  
  # Unidade (segunda célula)
  link_unidade <- html_element(celulas[[2]], SEL_UNIDADE_SIGLA)
  if (!is.na(link_unidade)) {
    unidade_sigla <- str_squish(html_text(link_unidade, trim = TRUE))
    unidade_nome  <- html_attr(link_unidade, "title")
    if (is.na(unidade_nome)) unidade_nome <- html_attr(link_unidade, "alt")
  } else {
    unidade_sigla <- str_squish(html_text(celulas[[2]], trim = TRUE))
    unidade_nome  <- NA_character_
  }
  
  # Descrição (terceira célula) — remove quebras de linha internas
  descricao <- str_squish(html_text(celulas[[3]], trim = TRUE))
  
  tibble(
    data_hora     = data_hora,
    unidade_sigla = unidade_sigla,
    unidade_nome  = unidade_nome,
    descricao     = descricao,
    status        = status,
    id_atividade  = id_atividade
  )
}


# -----------------------------------------------------------------------------
# scrape_processo_html_local() — variante para arquivo HTML já salvo
# Útil quando a URL de sessão expirou ou para rodar sem acesso à rede
# -----------------------------------------------------------------------------

#' Processa um arquivo HTML local já baixado do SEI.
#'
#' Equivalente a scrape_processo_sei(), mas lê de disco em vez de fazer
#' requisição HTTP. Útil para:
#'   - Tokens de sessão expirados
#'   - Reprocessamento sem acesso à rede
#'   - Testes e desenvolvimento
#'
#' @param caminho_html Caminho para o arquivo .html salvo
#' @param url_original URL original do processo (apenas para registro nos metadados)
#' @param dir_processed Diretório para dados processados
#' @return list com campos: cabecalho, documentos, andamentos, meta
scrape_processo_html_local <- function(
    caminho_html,
    url_original  = NA_character_,
    dir_processed = "data/processed"
) {
  if (!file.exists(caminho_html)) {
    stop("[scrape_local] Arquivo não encontrado: ", caminho_html)
  }

  cat("[scrape_local] Lendo:", caminho_html, "\n")

  # Leitura com encoding iso-8859-1 (mesmo padrão do SEI)
  raw_content <- readBin(caminho_html, what = "raw", n = file.info(caminho_html)$size)
  html_string <- iconv(rawToChar(raw_content), from = SEI_CHARSET, to = "UTF-8", sub = "?")
  doc         <- read_html(html_string)

  data_str        <- format(Sys.time(), "%Y%m%d")
  numero_processo <- .extrair_numero_processo(doc)
  numero_seguro   <- str_replace_all(numero_processo, "[^0-9A-Za-z]", "_")

  processo_restrito <- .detectar_processo_restrito(doc)
  if (processo_restrito) {
    cat("[scrape_local] AVISO: Processo com aviso de acesso restrito.\n")
  }

  cabecalho  <- .extrair_cabecalho(doc)
  documentos <- .extrair_documentos(doc)
  andamentos <- .extrair_andamentos(doc)

  n_pub  <- sum(documentos$acesso == "publico",  na.rm = TRUE)
  n_rest <- sum(documentos$acesso == "restrito", na.rm = TRUE)

  cat("[scrape_local] Processo  :", cabecalho$numero_processo, "\n")
  cat("[scrape_local] Tipo      :", cabecalho$tipo, "\n")
  cat("[scrape_local] Gerado em :", cabecalho$data_geracao, "\n")
  cat("[scrape_local] Documentos:", nrow(documentos),
      "(publicos:", n_pub, "| restritos:", n_rest, ")\n")
  cat("[scrape_local] Andamentos:", nrow(andamentos), "\n")

  resultado <- list(
    meta = list(
      url_original      = url_original,
      html_fonte        = caminho_html,
      timestamp_parse   = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      processo_restrito = processo_restrito,
      total_documentos  = nrow(documentos),
      documentos_publicos  = n_pub,
      documentos_restritos = n_rest,
      total_andamentos  = nrow(andamentos)
    ),
    cabecalho  = cabecalho,
    documentos = documentos,
    andamentos = andamentos
  )

  dir_processed_dia <- dir_saida_dia(dir_processed)
  nome_json    <- paste0(numero_seguro, "_local_", data_str, ".json")
  caminho_json <- file.path(dir_processed_dia, nome_json)
  write_json(resultado, caminho_json, pretty = TRUE, auto_unbox = TRUE, na = "null")
  cat("[scrape_local] Dados persistidos em:", caminho_json, "\n")

  invisible(resultado)
}
