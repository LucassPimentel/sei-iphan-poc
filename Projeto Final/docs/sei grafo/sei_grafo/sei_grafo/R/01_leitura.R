# =====================================================================
# 01_leitura.R — Aquisição e leitura dos autos do SEI Pesquisa Pública
#
# Duas vias de entrada:
#   (a) sei_baixar_autos(url) — raspagem a partir do link de exibição do
#       processo, que é estável e responde a GET simples;
#   (b) sei_ler_acervo(pasta) — leitura de um acervo já arquivado.
#
# O parâmetro do link "md_pesq_processo_exibir.php?<hash>" é cifrado, mas
# não depende de sessão: o servidor o decifra a qualquer momento. Por
# isso o link serve de entrada do pipeline e pode ser guardado. O código
# de confirmação é exigido apenas na PESQUISA por número de protocolo —
# a etapa de descobrir o link —, e não na leitura de um link conhecido.
# =====================================================================

suppressPackageStartupMessages({
  library(rvest); library(xml2); library(httr2)
  library(dplyr); library(stringr); library(purrr); library(tibble)
  library(lubridate); library(digest)
})

SEI_BASE <- "https://sei.iphan.gov.br/sei/modulos/pesquisa/"

# ---------------------------------------------------------------------
# Utilidades
# ---------------------------------------------------------------------

#' Lê um arquivo HTML tolerando latin-1 e utf-8
ler_html <- function(caminho) {
  bruto <- readBin(caminho, "raw", file.info(caminho)$size)
  txt <- NULL
  for (enc in c("UTF-8", "ISO-8859-1", "WINDOWS-1252")) {
    tentativa <- try(iconv(rawToChar(bruto), from = enc, to = "UTF-8"),
                     silent = TRUE)
    if (!inherits(tentativa, "try-error") && !is.na(tentativa)) {
      txt <- tentativa; break
    }
  }
  if (is.null(txt)) stop("Não foi possível decodificar: ", caminho)
  read_html(txt)
}

#' Texto de um nó, com espaços normalizados
txt1 <- function(x) {
  if (length(x) == 0) return(NA_character_)
  str_squish(html_text2(x))
}

#' Remove acentuação, para casamento de rótulos independente de codificação
sem_acento <- function(x) {
  str_squish(stringi::stri_trans_general(x, "Latin-ASCII"))
}

#' Converte "dd/mm/aaaa" (com hora opcional) em Date/POSIXct
as_data <- function(x) as.Date(str_sub(x, 1, 10), format = "%d/%m/%Y")
as_datahora <- function(x) as.POSIXct(x, format = "%d/%m/%Y %H:%M", tz = "America/Sao_Paulo")

# ---------------------------------------------------------------------
# (a) Aquisição a partir do link do processo
# ---------------------------------------------------------------------

#' Baixa a página do processo e todos os seus documentos nativos
#'
#' O link de exibição do processo é estável: uma requisição GET simples
#' o recupera, sem sessão, sem cookie e sem código de confirmação. O
#' código de confirmação só é exigido na *pesquisa* por número de
#' protocolo, que é a etapa de localizar o link — não a de lê-lo.
#'
#' Os links dos documentos vêm em atributo `onclick`, em caminho
#' relativo, e precisam ser tornados absolutos antes do download.
#'
#' @param url link de exibição do processo
#' @param destino diretório onde o acervo será arquivado
sei_baixar_autos <- function(url, destino = "acervo", pausa = 1.5) {
  pagina <- baixar_pagina(url)

  nup <- pagina |> html_element("#tblCabecalho") |> html_elements("tr") |>
    map_chr(\(tr) txt1(html_elements(tr, "td")[1])) |>
    (\(x) x[!is.na(x)])() |> paste(collapse = " ") |>
    str_extract("\\d{5}\\.\\d{6}/\\d{4}-\\d{2}")
  chave <- str_remove_all(nup %||% digest(url, algo = "crc32"), "\\D")

  pasta <- file.path(destino, chave)
  dir.create(file.path(pasta, "Autos_completo"), recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(pasta, "Documentos_html_nativo"), recursive = TRUE, showWarnings = FALSE)

  writeLines(as.character(pagina), file.path(pasta, "Autos_completo", "processo.html"),
             useBytes = TRUE)

  links <- extrair_links_documentos(pagina, url)
  for (k in seq_len(nrow(links))) {
    alvo <- file.path(pasta, "Documentos_html_nativo", links$n_doc[k])
    dir.create(alvo, showWarnings = FALSE)
    doc <- try(baixar_pagina(links$url[k]), silent = TRUE)
    if (!inherits(doc, "try-error")) {
      writeLines(as.character(doc), file.path(alvo, paste0(links$n_doc[k], ".html")),
                 useBytes = TRUE)
    }
    Sys.sleep(pausa)   # cortesia com o serviço público de consulta
  }

  registrar_proveniencia(pasta)
  pasta
}

#' Requisição GET simples, com cortesia e tolerância de codificação
baixar_pagina <- function(url) {
  resp <- request(url) |>
    req_user_agent("pesquisa-academica-ppca-unb (leitura de autos publicos)") |>
    req_timeout(60) |>
    req_retry(max_tries = 3) |>
    req_perform()
  bruto <- resp_body_raw(resp)
  txt <- NULL
  for (enc in c("UTF-8", "ISO-8859-1", "WINDOWS-1252")) {
    tentativa <- try(iconv(rawToChar(bruto), from = enc, to = "UTF-8"), silent = TRUE)
    if (!inherits(tentativa, "try-error") && !is.na(tentativa)) { txt <- tentativa; break }
  }
  read_html(txt)
}

#' Extrai os links dos documentos da lista de protocolos
#'
#' A âncora não traz `href` utilizável: o endereço está no `onclick`, em
#' caminho relativo. `url_absolute()` resolve contra o endereço do
#' processo. Documentos sem âncora são restritos e não têm link.
extrair_links_documentos <- function(pagina, url_processo) {
  linhas <- pagina |> html_element("#tblDocumentos") |> html_elements("tr")
  map_dfr(linhas, \(tr) {
    a <- tr |> html_element("a.ancoraPadraoAzul")
    if (is.na(a)) return(NULL)
    alvo <- c(html_attr(a, "href"), html_attr(a, "onclick")) |>
      paste(collapse = " ") |>
      str_extract("md_pesq_documento_consulta_externa\\.php\\?[^'\"\\s)]+")
    if (is.na(alvo)) return(NULL)
    tds <- tr |> html_elements("td") |> map_chr(txt1)
    n <- tds[str_detect(tds, "^\\d{7,8}$")][1]
    if (is.na(n)) return(NULL)
    tibble(n_doc = n, url = xml2::url_absolute(alvo, url_processo))
  })
}

#' Grava o manifesto de proveniência do acervo (integridade + data de captura)
registrar_proveniencia <- function(pasta) {
  arquivos <- list.files(pasta, recursive = TRUE, full.names = TRUE)
  arquivos <- arquivos[basename(arquivos) != "manifesto.csv"]
  manifesto <- tibble(
    arquivo      = arquivos,
    sha256       = map_chr(arquivos, ~ digest(.x, algo = "sha256", file = TRUE)),
    bytes        = file.info(arquivos)$size,
    capturado_em = Sys.time()
  )
  readr::write_csv(manifesto, file.path(pasta, "manifesto.csv"))
  manifesto
}

# ---------------------------------------------------------------------
# (b) Leitura de acervo em disco
# ---------------------------------------------------------------------

#' Localiza os arquivos de um acervo já arquivado
#'
#' Ler do acervo, e não da rede, é o modo normal de trabalho: o material
#' fica congelado, as iterações não repetem o download e a conferência
#' manual e a execução automática incidem sobre exatamente o mesmo
#' conteúdo.
sei_ler_acervo <- function(pasta) {
  autos <- list.files(file.path(pasta, "Autos_completo"),
                      pattern = "\\.html?$", full.names = TRUE)[1]
  nativos <- list.files(file.path(pasta, "Documentos_html_nativo"),
                        pattern = "\\.html?$", recursive = TRUE, full.names = TRUE)
  stopifnot(!is.na(autos), length(nativos) > 0)
  list(
    autos   = autos,
    nativos = tibble(n_doc = basename(dirname(nativos)), arquivo = nativos)
  )
}

# ---------------------------------------------------------------------
# Leitura das três tabelas da página do processo
# ---------------------------------------------------------------------

#' Cabeçalho do processo: NUP, tipo, data de geração, interessados
ler_cabecalho <- function(arquivo_autos) {
  pag <- ler_html(arquivo_autos)
  linhas <- pag |> html_element("#tblCabecalho") |> html_elements("tr")
  campos <- map_dfr(linhas, \(tr) {
    cs <- tr |> html_elements("td, th") |> map_chr(txt1)
    if (length(cs) < 2) return(NULL)
    tibble(chave = sem_acento(str_remove(cs[1], "\\s*:\\s*$")), valor = cs[2])
  })
  pega <- function(re) {
    v <- campos$valor[str_detect(campos$chave, regex(re, ignore_case = TRUE))]
    if (length(v)) v[1] else NA_character_
  }
  tibble(
    protocolo    = pega("^processo$"),
    tipo         = pega("^tipo$"),
    geracao      = as_data(pega("gerac")),
    interessados = pega("interessad")
  )
}

#' Lista de documentos
#'
#' Cuidados:
#'  - o tipo canônico vem do atributo `title` da âncora, e não do texto
#'    visível da célula, que traz o número sequencial concatenado;
#'  - documento sem âncora é restrito (sem inteiro teor na consulta);
#'  - a ordem de exibição não é confiável: a ordenação é recomposta por
#'    data e, no empate, pelo número do documento.
ler_documentos <- function(arquivo_autos) {
  pag <- ler_html(arquivo_autos)
  linhas <- pag |> html_element("#tblDocumentos") |> html_elements("tr")

  map_dfr(linhas, \(tr) {
    a   <- tr |> html_element("a.ancoraPadraoAzul")
    sig <- tr |> html_element("a.ancoraSigla")
    tds <- tr |> html_elements("td") |> map_chr(txt1)
    datas <- str_subset(tds, "^\\d{2}/\\d{2}/\\d{4}$")
    # a coluna do número é a que contém apenas 7 ou 8 dígitos; o rótulo
    # é a célula imediatamente seguinte. Localizar por conteúdo torna a
    # leitura imune a mudanças na ordem das colunas.
    i_num <- which(str_detect(tds, "^\\d{7,8}$"))[1]
    tibble(
      n_doc         = if (is.na(i_num)) NA_character_ else tds[i_num],
      rotulo        = if (!is.na(i_num) && length(tds) > i_num) tds[i_num + 1] else NA_character_,
      tipo_canonico = if (is.na(a)) NA_character_ else html_attr(a, "title"),
      data          = as_data(datas[1] %||% NA),
      data_inclusao = as_data(datas[2] %||% datas[1] %||% NA),
      unidade       = if (is.na(sig)) NA_character_ else txt1(sig),
      unidade_nome  = if (is.na(sig)) NA_character_ else html_attr(sig, "title"),
      restrito      = is.na(a)
    )
  }) |>
    filter(!is.na(n_doc)) |>
    mutate(n_doc = as.character(n_doc)) |>
    arrange(data, n_doc)
}

#' Histórico de andamentos
#'
#' O rótulo é normalizado e a unidade de origem isolada, de modo que
#' "Processo remetido pela unidade X" registrado na unidade Y signifique
#' a transferência X -> Y.
ler_andamentos <- function(arquivo_autos) {
  pag <- ler_html(arquivo_autos)
  # Nem toda instalação aplica as classes de listra às linhas do
  # histórico; percorrer todas as linhas e filtrar pelo carimbo de tempo
  # é mais seguro do que depender da classe CSS.
  linhas <- pag |> html_element("#tblHistorico") |> html_elements("tr")

  map_dfr(linhas, \(tr) {
    cs <- tr |> html_elements("td") |> map_chr(txt1)
    if (length(cs) < 3) return(NULL)
    tibble(ts = as_datahora(cs[1]), unidade = cs[2], descricao = cs[3])
  }) |>
    filter(!is.na(ts)) |>
    mutate(
      origem = str_match(descricao, "remetido pela unidade\\s+(.+)$")[, 2],
      rotulo = case_when(
        str_detect(descricao, "^Processo remetido")   ~ "remetido",
        str_detect(descricao, "^Processo recebido")   ~ "recebido",
        str_detect(descricao, "^Conclusão")           ~ "concluido",
        str_detect(descricao, "^Reabertura")          ~ "reaberto",
        str_detect(descricao, "^Processo público")    ~ "gerado",
        TRUE                                          ~ "outro"
      )
    ) |>
    arrange(ts)
}
