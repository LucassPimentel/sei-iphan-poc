# =============================================================================
# 04_entidades.R
# Extracao de entidades e relacoes dos documentos nativos (POC IPHAN)
#
# Responsabilidade: Camada de Conteudo — extracao de ENTIDADES/RELACOES.
#   Diferente de 05_text_mining.R (que produz atributos textuais para
#   modelagem), esta etapa captura informacao ESTRUTURAL que alimenta o event
#   log, o grafo organizacional e a analise de conformidade (etapas 07-10):
#     - remetente/signatario (nome, cargo)
#     - unidade emissora      (do cabecalho do documento)
#     - destinatario          (nome, cargo/unidade)
#     - prazo legal           (data-limite "encerrar-se-a em DD/MM/AAAA")
#
# Motivacao (definida com o pesquisador):
#   Os nomes de signatarios/destinatarios sao removidos do TF-IDF (nao sao
#   termos tematicos), mas NAO devem ser descartados: o grafo precisa saber
#   quem solicitou o que a quem, entre quais unidades, e ate que prazo — para
#   avaliar atraso/conformidade. Esta etapa persiste essa informacao numa
#   estrutura propria, separada do vocabulario textual.
#
# Regras de extracao — baseadas em padroes REAIS observados nos documentos:
#   - Destinatario : "Ao Senhor|A Senhora <NOME MAIUSCULO> <cargo/unidade>"
#                    (confirmado em todos os oficios do processo 01450.002827).
#   - Remetente    : "assinado eletronicamente por <NOME>, <CARGO>"
#                    (padrao fixo do rodape do SEI, reaproveitado do 05).
#   - Unidade emiss.: sigla no numero do documento "Oficio nº .../<UNID>-IPHAN".
#   - Prazo        : "prazo legal para manifestacao do IPHAN encerrar-se-a em
#                    DD/MM/AAAA" (confirmado apenas em Despachos que calculam
#                    prazo — ex.: Despacho CGM).
#
# Limitacoes registradas (ver research-methodology.md):
#   - As regras dependem do texto ja extraido COM separadores (03 corrigido).
#     Se o texto vier colado, a captura de nome/unidade degrada.
#   - Documentos fora do padrao (ex.: destinatario externo, pessoa fisica) sao
#     capturados de forma parcial e marcados; a cobertura e medida, nao
#     assumida.
#   - Nao inventamos entidade: quando o padrao nao casa, o campo fica NA e a
#     ausencia e contabilizada.
#
# Tecnologias: stringr, stringi, tibble, dplyr, purrr, lubridate, jsonlite
# =============================================================================

library(stringr)
library(stringi)
library(tibble)
library(dplyr)
library(purrr)
library(lubridate)
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
# Constantes: padroes de extracao (documentados, nao hardcode disperso)
# -----------------------------------------------------------------------------

# Destinatario: "Ao Senhor" ou "A Senhora" seguido de NOME em caixa alta.
# O nome e uma sequencia de palavras majoritariamente em maiusculas.
PADRAO_DESTINATARIO_ABERTURA <- "\\b(Ao Senhor|\u00c0 Senhora|A Senhora|Ao Senhora|Aos Senhores)\\b"

# Prazo legal — Camada 1: frase explicita (alta confianca), observada nos
# despachos de calculo de prazo.
PADRAO_PRAZO <- paste0(
  "prazo legal para manifesta\u00e7\u00e3o do IPHAN\\s+encerrar-se-\u00e1\\s+em\\s+",
  "(\\d{2}/\\d{2}/\\d{4})"
)

# Prazo legal — Camada 2: gatilhos de proximidade. Uma data proxima de uma
# destas palavras e candidata a prazo. Lista EDITAVEL (dado, nao code disperso);
# cobre variacoes sem exigir a frase exata (ex.: "prazo assinalado (DATA)",
# "observar o prazo ... DATA", "encerra em DATA", "ate DATA").
GATILHOS_PRAZO <- c("prazo", "encerr", "assinalad", "observ", "at\u00e9 o dia", "vencimento", "limite")

# Anti-gatilhos: se a data estiver proxima destes termos, e data de ASSINATURA
# ou de gera\u00e7\u00e3o do documento — NAO e prazo.
ANTIGATILHOS_PRAZO <- c("assinado eletronicamente", "\u00e0s", "autenticidade", "Decreto")

# Janela (em caracteres) ao redor da data para procurar gatilhos.
JANELA_PRAZO <- 45L

# Remetente pelo rodape de assinatura (mesmo padrao usado no 05).
PADRAO_REMETENTE <- "assinado eletronicamente por\\s+([^,]+),\\s*([^,]+?),\\s*em\\b"

# Unidade emissora: sigla no numero do documento, ex.:
#   "Of\u00edcio n\u00ba 156/2026/ETL-BA/IPHAN-BA-IPHAN" -> ETL-BA / IPHAN-BA
PADRAO_UNIDADE_EMISSORA <- "n[\u00ba\u00b0o]\\s*\\d+/\\d{4}/([A-Z0-9\\-/]+)"

# Referencias a outros documentos do processo (numero SEI de 7-8 digitos).
# Dois padroes CONSERVADORES (para minimizar falso positivo), portados do
# especialista (02_documento.R::extrair_referencias):
#   - numero entre parenteses: "(7535801)"
#   - mencao explicita "SEI n\u00ba 7535801"
# NAO usamos o padrao "_NNNNNNN.pdf" (anexo de e-mail) do especialista: e mais
# ruidoso e fica como trabalho futuro (limitacao registrada no grafo).
PADRAO_REF_PARENTESES <- "\\(\\s*(\\d{7,8})\\s*\\)"
PADRAO_REF_SEI        <- "SEI\\s*n?[\u00ba\u00b0o]?\\s*(\\d{7,8})"

# Corte de rodape: a partir de "Refer\u00eancia:" ou "A autenticidade deste
# documento" o texto e bloco de rodape do SEI (codigo verificador, links) e nao
# deve ser varrido por referencias — inflaria a estimativa com lixo.
PADRAO_CORTE_RODAPE   <- "(?i)(Refer\u00eancia\\s*:|A autenticidade deste documento)"

# -----------------------------------------------------------------------------
# 1. Destinatario
# -----------------------------------------------------------------------------

#' Extrai o destinatario de um documento a partir do corpo do texto.
#'
#' Padrao (confirmado nos oficios reais):
#'   "Ao Senhor <NOME> <cargo/unidade em texto normal> Assunto:"
#'   O nome aparece tanto em CAIXA ALTA ("ERIC LEMOS") quanto em Title Case
#'   ("Moises Julierme Stival Soares"). Ambos sao aceitos.
#'
#' Estrategia:
#'   1. Localiza a abertura ("Ao Senhor"/"A Senhora").
#'   2. Captura, logo apos, a sequencia de palavras de NOME (maiusculas ou
#'      Title Case), parando no primeiro indicador de CARGO (ver
#'      INDICADORES_CARGO) — e assim a fronteira nome/cargo se preserva mesmo
#'      quando o cargo tambem comeca com maiuscula.
#'   3. Captura o trecho seguinte (cargo/unidade) ate um delimitador
#'      ("Assunto", "Prezado", "Processo", quebra ou fim).
#'
# Token de nome: aceita CAIXA ALTA ("SOARES") e Title Case ("Soares"), com
# acentos e apostrofo/ponto. Exige inicial maiuscula e ao menos mais uma letra.
PALAVRA_NOME <- "[A-Z\u00c0-\u00dc][A-Za-z\u00c0-\u00ff'\\.]+"

# Token de nome de destinatario EM CAIXA ALTA. Nos oficios reais do IPHAN o nome
# do destinatario vem sempre em maiusculas ("ALMIR DO CARMO BEZERRA"), enquanto
# o que o segue — cargo, unidade, empresa ou endereco — vem em Title Case
# ("Energias do Brasil", "Nucleo de Patrimonio", "Rua Setenta e Nove"). Exigir
# CAIXA ALTA no token de nome impede que essas palavras sejam engolidas como
# parte do nome. Exige ao menos duas letras maiusculas (evita casar iniciais
# soltas). Acentos maiusculos, apostrofo e ponto sao aceitos.
PALAVRA_NOME_MAIUSC <- "[A-Z\u00c0-\u00dc][A-Z\u00c0-\u00dc'\\.]+"

# Conectores minusculos que podem aparecer NO MEIO de um nome composto
# ("de", "da", "do", "dos", "das", "e"). Aceitos entre tokens, nunca no fim.
CONECTOR_NOME <- "(?:de|da|do|dos|das|e)"

# Indicadores de CARGO/funcao: quando um token do bloco e uma destas palavras,
# o NOME termina e o CARGO comeca. Distingue "Moises Julierme Stival Soares"
# (nome) de "Coordenador Tecnico Substituto" (cargo), ambos em Title Case.
# Lista EDITAVEL (dado, nao code disperso); cobre os cargos observados no IPHAN.
INDICADORES_CARGO <- c(
  "Coordenador", "Coordenadora", "Coordena\u00e7\u00e3o",
  "Superintendente", "Superintend\u00eancia",
  "Chefe", "Diretor", "Diretora", "Diretoria",
  "T\u00e9cnico", "T\u00e9cnica", "Analista",
  "Presidente", "Gerente", "Secret\u00e1rio", "Secret\u00e1ria",
  "Assessor", "Assessora", "Divis\u00e3o", "Departamento",
  "Servidor", "Servidora", "Substituto", "Substituta"
)

# Delimitadores que encerram o bloco de cargo/unidade de um destinatario.
DELIM_FIM_CARGO <- "(?:Assunto|Ao Senhor|\u00c0 Senhora|A Senhora|Prezad|Senhor[ae]? Superintendente|Processo n|Ref\\.|Ref:|Cordialmente|E-mail:|$)"

#' Extrai nome e cargo/unidade de UM bloco de destinatario (texto ja iniciado
#' logo apos "Ao Senhor"/"A Senhora", ate o proximo delimitador).
#'
#' @param bloco Trecho de texto do destinatario
#' @return list(nome, cargo) ou NULL se nao houver nome
.parse_bloco_destinatario <- function(bloco) {
  # Remove zero-width spaces (U+200B/FEFF) que o SEI insere no texto e que o
  # str_squish nao trata, poluindo nome/cargo.
  bloco <- stri_replace_all_regex(bloco, "[\\u200B\\uFEFF]", "")
  bloco <- str_squish(bloco)
  if (bloco == "") return(NULL)

  # Percorre os tokens do bloco acumulando o NOME. Um token e de nome quando:
  #   - casa PALAVRA_NOME (inicial maiuscula: "ERIC", "Moises", "SOARES"); e
  #   - NAO e um indicador de cargo (ver INDICADORES_CARGO) — isso encerra o nome.
  # Conectores minusculos ("de", "da", "e") sao aceitos ENTRE tokens de nome,
  # nunca como ultimo token. Ao encontrar o primeiro token de cargo (ou um token
  # que nao seja nome/conector), paramos: dali em diante e CARGO/unidade.
  tokens <- str_split(bloco, "\\s+")[[1]]
  # Nome do destinatario em CAIXA ALTA: o cargo/unidade/empresa/endereco que o
  # segue vem em Title Case e nao deve ser engolido no nome (ver PALAVRA_NOME_MAIUSC).
  re_nome     <- paste0("^", PALAVRA_NOME_MAIUSC, "$")
  # Conector case-insensitive: aceita "de"/"da"/"e" e tambem "DA"/"E" (nomes em
  # caixa alta trazem os conectores em maiuscula, ex.: "JOAO DA SILVA E QUEIROZ").
  re_conector <- paste0("(?i)^", CONECTOR_NOME, "$")

  nome_tokens <- character(0)
  i <- 1L
  n <- length(tokens)
  while (i <= n) {
    tok <- tokens[i]
    # Indicador de cargo encerra o nome (comparacao case-insensitive).
    if (any(tolower(tok) == tolower(INDICADORES_CARGO))) break
    if (str_detect(tok, re_nome)) {
      nome_tokens <- c(nome_tokens, tok)
      i <- i + 1L
    } else if (str_detect(tok, re_conector) && length(nome_tokens) > 0 &&
               i < n && str_detect(tokens[i + 1L], re_nome) &&
               !any(tolower(tokens[i + 1L]) == tolower(INDICADORES_CARGO))) {
      # conector minusculo seguido de outro token de nome -> parte do nome
      nome_tokens <- c(nome_tokens, tok)
      i <- i + 1L
    } else {
      break
    }
  }

  if (length(nome_tokens) == 0) return(NULL)
  nome <- str_squish(paste(nome_tokens, collapse = " "))

  # Cargo/unidade: o que sobra apos o nome, ate um delimitador conhecido.
  apos_nome <- str_squish(paste(tokens[i:n], collapse = " "))
  cargo <- str_extract(apos_nome, paste0("^.*?(?=\\s+", DELIM_FIM_CARGO, ")"))
  if (is.na(cargo) || cargo == "") cargo <- str_squish(substr(apos_nome, 1, 120))
  cargo <- str_squish(cargo)
  if (cargo == "") cargo <- NA_character_

  list(nome = nome, cargo = cargo)
}

#' Extrai TODOS os destinatarios de um documento.
#'
#' Documentos podem ter mais de um destinatario, cada um introduzido por
#' "Ao Senhor"/"A Senhora". O bloco de cada destinatario vai da sua abertura
#' ate a proxima abertura (ou ate um delimitador como "Assunto:").
#'
#' Retorno mantem compatibilidade tabular: destinatario_nome/destinatario_cargo
#' referem-se ao PRIMEIRO destinatario; a lista completa fica em `destinatarios`
#' e a contagem em `n_destinatarios`.
#'
#' @param texto Texto completo do documento (corpo)
#' @return list(destinatario_nome, destinatario_cargo, destinatario_encontrado,
#'              n_destinatarios, destinatarios)
extrair_destinatario <- function(texto) {
  vazio <- list(destinatario_nome = NA_character_,
                destinatario_cargo = NA_character_,
                destinatario_encontrado = FALSE,
                n_destinatarios = 0L,
                destinatarios = list())
  if (is.na(texto) || texto == "") return(vazio)

  # Todas as aberturas de destinatario
  aberturas <- str_locate_all(texto, PADRAO_DESTINATARIO_ABERTURA)[[1]]
  if (nrow(aberturas) == 0) return(vazio)

  inicios <- aberturas[, 1]  # inicio de cada "Ao Senhor/A Senhora"
  fins_ab <- aberturas[, 2]  # fim da expressao de abertura

  destinatarios <- list()
  for (k in seq_len(nrow(aberturas))) {
    # O bloco vai do fim desta abertura ate o inicio da proxima (ou fim do texto)
    ini <- fins_ab[k] + 1
    fim <- if (k < nrow(aberturas)) inicios[k + 1] - 1 else nchar(texto)
    bloco <- substr(texto, ini, fim)

    parsed <- .parse_bloco_destinatario(bloco)
    if (!is.null(parsed)) {
      destinatarios[[length(destinatarios) + 1]] <- parsed
    }
  }

  if (length(destinatarios) == 0) return(vazio)

  list(
    destinatario_nome        = destinatarios[[1]]$nome,
    destinatario_cargo       = destinatarios[[1]]$cargo,
    destinatario_encontrado  = TRUE,
    n_destinatarios          = length(destinatarios),
    destinatarios            = destinatarios
  )
}

# -----------------------------------------------------------------------------
# 2. Remetente / signatario + unidade emissora
# -----------------------------------------------------------------------------

#' Extrai o(s) remetente(s)/signatario(s) do bloco de assinatura.
#'
#' Reutiliza o padrao fixo do rodape do SEI. Pode haver mais de um signatario
#' (ex.: chefe + coordenador). Retorna o primeiro como principal e a contagem.
#'
#' @param bloco_assinaturas Texto do bloco de assinatura (do 05/separar_boilerplate)
#' @return list(remetente_nome, remetente_cargo, n_signatarios)
extrair_remetente <- function(bloco_assinaturas) {
  vazio <- list(remetente_nome = NA_character_,
                remetente_cargo = NA_character_,
                n_signatarios = 0L,
                signatarios = character(0))
  if (is.null(bloco_assinaturas) || length(bloco_assinaturas) == 0 ||
      is.na(bloco_assinaturas) || bloco_assinaturas == "") {
    return(vazio)
  }

  m <- str_match_all(bloco_assinaturas, PADRAO_REMETENTE)[[1]]
  if (nrow(m) == 0) return(vazio)

  list(
    remetente_nome  = str_squish(m[1, 2]),
    remetente_cargo = str_squish(m[1, 3]),
    n_signatarios   = nrow(m),
    # TODOS os signatarios (nome), para o grafo casar o destinatario com quem
    # de fato assinou — o endereçado costuma ser o 2o signatario (coordenador),
    # nao o 1o. Preserva a informacao que o remetente_nome (1o) perderia.
    signatarios     = str_squish(m[, 2])
  )
}

#' Extrai a sigla da unidade emissora a partir do numero do documento.
#'
#' Ex.: "Of\u00edcio n\u00ba 156/2026/ETL-BA/IPHAN-BA-IPHAN" -> "ETL-BA/IPHAN-BA-IPHAN".
#' Retorna o bloco de siglas como aparece; a normalizacao fina (ex.: IPHAN-BA)
#' fica para a etapa de grafo, que consolida unidades.
#'
#' @param texto Texto do documento (o numero costuma estar no inicio)
#' @return character (sigla/bloco de unidade) ou NA
extrair_unidade_emissora <- function(texto) {
  if (is.na(texto) || texto == "") return(NA_character_)
  m <- str_match(texto, PADRAO_UNIDADE_EMISSORA)
  if (is.na(m[1, 2])) return(NA_character_)
  str_squish(m[1, 2])
}

# -----------------------------------------------------------------------------
# 2b. Referencias a outros documentos (citacao por numero SEI)
# -----------------------------------------------------------------------------

#' Extrai os numeros SEI (7-8 digitos) que o documento CITA no corpo.
#'
#' Alimenta as arestas de CITACAO do grafo (10_grafo.R): quando um ato cita
#' outro pelo numero, o grafo liga citado -> citante. Usa apenas os dois
#' padroes conservadores (entre parenteses e "SEI n\u00ba"), corta o rodape do SEI
#' (a partir de "Refer\u00eancia:"/"A autenticidade") e descarta o proprio numero.
#'
#' @param texto Texto (corpo) do documento
#' @param numero_proprio Numero do proprio documento (descartado das refs)
#' @return character — vetor de numeros citados (0..n), unicos
extrair_referencias <- function(texto, numero_proprio = NA_character_) {
  if (is.null(texto) || length(texto) == 0 || is.na(texto) || texto == "") {
    return(character(0))
  }

  # Corta o rodape: so o corpo antes de "Refer\u00eancia:"/"A autenticidade" e varrido.
  corte <- str_locate(texto, PADRAO_CORTE_RODAPE)
  corpo <- if (!is.na(corte[1, 1])) substr(texto, 1, corte[1, 1] - 1L) else texto

  refs <- c(
    str_match_all(corpo, PADRAO_REF_PARENTESES)[[1]][, 2],
    str_match_all(corpo, PADRAO_REF_SEI)[[1]][, 2]
  )
  refs <- unique(str_squish(refs))
  refs <- refs[!is.na(refs) & refs != ""]
  if (!is.na(numero_proprio)) refs <- setdiff(refs, as.character(numero_proprio))
  refs
}

# -----------------------------------------------------------------------------
# 3. Prazo legal
# -----------------------------------------------------------------------------

#' Extrai a data-limite do prazo, em camadas (da mais confiavel a mais generica).
#'
#' Em vez de exigir uma unica frase exata, combina:
#'   Camada 1 (explicito) : frase fixa "prazo legal ... encerrar-se-a em DATA".
#'   Camada 2 (proximidade): qualquer data proxima de um GATILHO_PRAZO (prazo,
#'                           encerra, assinalado, observar, ate...) e que NAO
#'                           esteja proxima de um anti-gatilho (assinatura).
#'
#' Registra o metodo usado (`prazo_metodo`) para que a avaliacao contra o
#' gabarito possa separar prazos de regra forte dos de heuristica.
#'
#' @param texto Texto do documento
#' @return list(prazo_data_iso, prazo_data_original, prazo_encontrado, prazo_metodo)
extrair_prazo <- function(texto) {
  vazio <- list(prazo_data_iso = NA_character_,
                prazo_data_original = NA_character_,
                prazo_encontrado = FALSE,
                prazo_metodo = NA_character_)
  if (is.na(texto) || texto == "") return(vazio)

  # --- Camada 1: padrao explicito ---
  m <- str_match(texto, PADRAO_PRAZO)
  if (!is.na(m[1, 2])) {
    return(.montar_prazo(m[1, 2], metodo = "explicito"))
  }

  # --- Camada 2: gatilho + proximidade ---
  datas <- str_locate_all(texto, "\\d{2}/\\d{2}/\\d{4}")[[1]]
  if (nrow(datas) == 0) return(vazio)

  for (i in seq_len(nrow(datas))) {
    ini <- datas[i, 1]; fim <- datas[i, 2]
    data_txt <- substr(texto, ini, fim)

    # Janela ANTERIOR a data: onde o gatilho de prazo tipicamente aparece
    # ("prazo assinalado (DATA)", "observar o prazo ... DATA").
    antes <- tolower(substr(texto, max(1, ini - JANELA_PRAZO), ini - 1))
    # Janela POSTERIOR: usada para detectar data de assinatura ("DATA, as HH").
    depois <- tolower(substr(texto, fim + 1, min(nchar(texto), fim + 15)))

    tem_gatilho <- any(vapply(GATILHOS_PRAZO,
                              function(g) grepl(g, antes, fixed = TRUE), logical(1)))
    # Anti-gatilho: assinatura aparece ANTES ("assinado ... em DATA") ou o
    # padrao "DATA, as" logo APOS (hora de assinatura).
    assinatura_antes <- any(vapply(tolower(ANTIGATILHOS_PRAZO),
                                   function(a) grepl(a, antes, fixed = TRUE), logical(1)))
    hora_apos <- grepl("^,?\\s*\u00e0s\\b", depois)

    if (tem_gatilho && !assinatura_antes && !hora_apos) {
      return(.montar_prazo(data_txt, metodo = "proximidade"))
    }
  }

  vazio
}

#' Monta o retorno de prazo a partir de uma data dd/mm/aaaa e o metodo.
.montar_prazo <- function(data_original, metodo) {
  iso <- suppressWarnings(dmy(data_original))
  list(prazo_data_iso = if (is.na(iso)) NA_character_ else as.character(iso),
       prazo_data_original = data_original,
       prazo_encontrado = TRUE,
       prazo_metodo = metodo)
}

# -----------------------------------------------------------------------------
# Extracao por documento
# -----------------------------------------------------------------------------

#' Extrai todas as entidades de um unico documento.
#'
#' @param numero_documento Id do documento
#' @param tipo Tipo do documento
#' @param texto Texto completo (corpo)
#' @param bloco_assinaturas Bloco de assinatura (opcional)
#' @return tibble de uma linha com as entidades
.extrair_entidades_documento <- function(numero_documento, tipo, texto,
                                          bloco_assinaturas = NA_character_) {
  dest  <- extrair_destinatario(texto)
  rem   <- extrair_remetente(bloco_assinaturas)
  unid  <- extrair_unidade_emissora(texto)
  prazo <- extrair_prazo(texto)
  refs  <- extrair_referencias(texto, numero_documento)

  # Lista completa de destinatarios (nome/cargo) para preservar multiplos
  # destinatarios sem perder informacao no formato tabular.
  dest_lista <- lapply(dest$destinatarios, function(d) {
    list(nome = d$nome, cargo = d$cargo)
  })

  tibble(
    numero_documento    = as.character(numero_documento),
    tipo                = as.character(tipo),
    unidade_emissora    = unid,
    remetente_nome      = rem$remetente_nome,
    remetente_cargo     = rem$remetente_cargo,
    n_signatarios       = rem$n_signatarios,
    signatarios         = if (length(rem$signatarios) > 0)
                            paste(rem$signatarios, collapse = "; ") else NA_character_,
    referencias         = if (length(refs) > 0)
                            paste(refs, collapse = "; ") else NA_character_,
    destinatario_nome   = dest$destinatario_nome,
    destinatario_cargo  = dest$destinatario_cargo,
    destinatario_ok     = dest$destinatario_encontrado,
    n_destinatarios     = dest$n_destinatarios,
    destinatarios       = list(dest_lista),
    prazo_data_iso      = prazo$prazo_data_iso,
    prazo_data_original = prazo$prazo_data_original,
    prazo_ok            = prazo$prazo_encontrado,
    prazo_metodo        = prazo$prazo_metodo
  )
}

# -----------------------------------------------------------------------------
# Funcao principal: extrair_entidades_processo()
# -----------------------------------------------------------------------------

#' Extrai entidades/relacoes de todos os documentos com texto de um processo.
#'
#' @param resultado_conteudo list de extrair_conteudo_documentos() OU caminho
#'        do JSON "_conteudo_".
#' @param blocos_assinatura tibble/data.frame opcional com colunas
#'        (numero_documento, bloco_assinaturas), tipicamente vindo do resultado
#'        de text_mining_processo()$por_documento. Se NULL, remetente fica NA.
#' @param dir_processed Diretorio de saida
#' @param persistir Logico — salvar JSON "_entidades_"
#' @return list com: entidades (tibble), meta (list)
extrair_entidades_processo <- function(
    resultado_conteudo,
    blocos_assinatura = NULL,
    dir_processed     = "data/processed",
    persistir         = TRUE
) {
  if (is.character(resultado_conteudo)) {
    if (!file.exists(resultado_conteudo)) {
      stop("[entidades] JSON de conteudo nao encontrado: ", resultado_conteudo)
    }
    resultado_conteudo <- jsonlite::fromJSON(resultado_conteudo, simplifyVector = TRUE)
  }

  numero_processo <- resultado_conteudo$meta$numero_processo
  numero_seguro   <- str_replace_all(numero_processo, "[^0-9A-Za-z]", "_")

  docs <- resultado_conteudo$documentos_conteudo
  docs_com_texto <- docs[!is.na(docs$conteudo_extraido) & docs$conteudo_extraido == TRUE, ]

  cat("[entidades] Processo:", numero_processo, "\n")
  cat("[entidades] Documentos com texto:", nrow(docs_com_texto), "\n")

  # Mapa numero_documento -> bloco_assinaturas (se fornecido)
  mapa_bloco <- .montar_mapa_bloco(blocos_assinatura)

  if (nrow(docs_com_texto) == 0) {
    cat("[entidades] AVISO: nenhum documento com texto. Nada a processar.\n")
    saida <- list(
      meta = list(numero_processo = numero_processo,
                  timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                  documentos_processados = 0L),
      entidades = tibble()
    )
    return(invisible(saida))
  }

  linhas <- lapply(seq_len(nrow(docs_com_texto)), function(i) {
    numero <- docs_com_texto$numero_documento[i]
    bloco  <- if (!is.null(mapa_bloco) && numero %in% names(mapa_bloco)) {
      mapa_bloco[[numero]]
    } else {
      NA_character_
    }
    .extrair_entidades_documento(
      numero_documento  = numero,
      tipo              = docs_com_texto$tipo[i],
      texto             = docs_com_texto$texto[i],
      bloco_assinaturas = bloco
    )
  })
  entidades <- bind_rows(linhas)

  # Metricas de cobertura
  n_dest  <- sum(entidades$destinatario_ok, na.rm = TRUE)
  n_prazo <- sum(entidades$prazo_ok, na.rm = TRUE)
  n_rem   <- sum(!is.na(entidades$remetente_nome))
  n_unid  <- sum(!is.na(entidades$unidade_emissora))

  cat("[entidades] --- Resumo ---\n")
  cat("[entidades] Destinatarios extraidos:", n_dest, "de", nrow(entidades), "\n")
  cat("[entidades] Remetentes extraidos   :", n_rem, "de", nrow(entidades), "\n")
  cat("[entidades] Unidades emissoras      :", n_unid, "de", nrow(entidades), "\n")
  cat("[entidades] Prazos extraidos        :", n_prazo, "de", nrow(entidades), "\n")

  saida <- list(
    meta = list(
      numero_processo        = numero_processo,
      timestamp              = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      documentos_processados = nrow(docs_com_texto),
      destinatarios_extraidos = n_dest,
      remetentes_extraidos    = n_rem,
      unidades_extraidas       = n_unid,
      prazos_extraidos         = n_prazo
    ),
    entidades = entidades
  )

  if (persistir) {
    data_str <- format(Sys.time(), "%Y%m%d")
    nome     <- paste0(numero_seguro, "_entidades_", data_str, ".json")
    caminho  <- file.path(dir_saida_dia(dir_processed), nome)
    write_json(saida, caminho, pretty = TRUE, auto_unbox = TRUE, na = "null")
    cat("[entidades] Persistido em:", caminho, "\n")
  }

  invisible(saida)
}

#' Monta um mapa numero_documento -> bloco_assinaturas a partir de varias
#' formas de entrada (lista por_documento do 05, data.frame, ou NULL).
.montar_mapa_bloco <- function(blocos_assinatura) {
  if (is.null(blocos_assinatura)) return(NULL)

  # Caso 1: lista nomeada por numero_documento (por_documento do 05)
  if (is.list(blocos_assinatura) && !is.data.frame(blocos_assinatura)) {
    return(lapply(blocos_assinatura, function(d) {
      b <- d$bloco_assinaturas
      if (is.null(b) || length(b) == 0) NA_character_ else as.character(b)
    }))
  }

  # Caso 2: data.frame com colunas numero_documento, bloco_assinaturas
  if (is.data.frame(blocos_assinatura) &&
      all(c("numero_documento", "bloco_assinaturas") %in% names(blocos_assinatura))) {
    mapa <- as.list(blocos_assinatura$bloco_assinaturas)
    names(mapa) <- as.character(blocos_assinatura$numero_documento)
    return(mapa)
  }

  NULL
}
