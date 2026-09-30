# =============================================================================
# 07_event_log.R
# Construcao do event log do processo (POC IPHAN) — insumo do Process Mining
#
# Responsabilidade: Camada de Process Mining (ver architecture.md).
#   Transforma os dados ja extraidos (scraping estrutural, entidades,
#   classificacao e, quando disponivel, o gabarito) em um EVENT LOG unico,
#   ordenado no tempo, no formato classico de mineracao de processos:
#     case_id | activity | timestamp | resource | ...
#   Esse log e o insumo direto da descoberta/representacao do fluxo processual
#   (grafo, etapa 10) e da analise de conformidade/prazos (etapas 08-09).
#
# Duas FAMILIAS de eventos, unificadas num so log (ambas tem timestamp real):
#   1. Eventos de DOCUMENTO (fonte = "documento"): um por peca juntada aos
#      autos (tabela #tblDocumentos do scraping). A "activity" combina a classe
#      semantica do ato (06/gabarito) com o tipo do documento. O timestamp e a
#      data do documento; o resource e a unidade emissora (entidades) com
#      fallback para a unidade da lista (scraping). Anexos/restritos entram como
#      evento de juntada SEM classe (o conteudo nao e extraido — FLUXOS sec. 10).
#   2. Eventos de TRAMITACAO (fonte = "tramitacao"): dos andamentos
#      (#tblHistorico) — processo remetido/recebido/concluido/reaberto entre
#      unidades. O registro "Processo remetido pela unidade <mae>" e pareado com
#      o "Processo recebido na unidade" seguinte para reconstruir a aresta
#      origem -> destino que explica a tramitacao entre unidades.
#
# Entradas (aceita OBJETO em memoria OU caminho de JSON, como os demais modulos):
#   - scraping     : {proc}_{data}.json (obrigatorio; autoritativo p/ docs+andamentos)
#   - entidades    : {proc}_entidades_{data}.json (opcional; enriquece docs)
#   - classificacao: {proc}_classificacao_{data}.json (opcional; classe baseline)
#   - gabarito     : tibble consolidado (utils_gabarito.R) OU NULL (opcional;
#                    classe_final tem precedencia sobre o baseline)
#
# Saida:
#   - data/processed/{proc}_eventlog_{data}.json  (log + meta + arestas de fluxo)
#   - data/processed/{proc}_eventlog_{data}.csv   (log achatado p/ inspecao/PM)
#
# Limitacoes registradas (ver research-methodology.md e FLUXOS.md sec. 10):
#   - A fase anterior a abertura no SEI ocorre no SAIP e NAO aparece nos autos.
#   - Documentos tem precisao de DIA (sem hora); andamentos tem data+hora. O
#     campo `precisao_temporal` registra essa diferenca — nao fingimos precisao.
#   - Processos em curso tem traco incompleto (atividades terminais podem faltar).
#   - Prazos NAO sao calculados aqui (isso e a etapa 09, que le o JSON de
#     normativas). Aqui apenas transportamos a data-limite ja extraida (entidades).
#   - Anexos nao-nativos e documentos restritos entram como evento de juntada,
#     porem sem classe semantica (conteudo nao extraido).
#
# Tecnologias: dplyr, tibble, purrr, stringr, lubridate, jsonlite
# =============================================================================

library(dplyr)
library(tibble)
library(purrr)
library(stringr)
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
# Constantes: rotulos de atividade de tramitacao (padroes REAIS dos andamentos)
# -----------------------------------------------------------------------------

# Padrao que identifica o repasse entre unidades: "Processo remetido pela
# unidade <MAE>". Confirmado nos andamentos reais. O grupo captura a unidade
# de ORIGEM (a peca esta registrada na unidade de DESTINO).
PADRAO_REMETIDO <- "Processo remetido pela unidade\\s+(.+)$"

# Rotulos normalizados de tramitacao, para a coluna `activity` dos eventos de
# andamento. Mapeados por deteccao de substring na descricao (dado, nao regra
# dispersa). A ordem importa: o primeiro que casar vence.
TRAMITACAO_ROTULOS <- list(
  list(padrao = "remetido pela unidade",   rotulo = "remetido"),
  list(padrao = "recebido na unidade",     rotulo = "recebido"),
  list(padrao = "Reabertura do processo",  rotulo = "reaberto"),
  list(padrao = "Conclus\u00e3o do processo",   rotulo = "concluido"),
  list(padrao = "Concluido o processo",    rotulo = "concluido")
)

# -----------------------------------------------------------------------------
# Utilitarios de leitura (aceita objeto OU caminho, como os demais modulos)
# -----------------------------------------------------------------------------

#' Carrega uma fonte que pode vir como objeto ja lido ou caminho de JSON.
#' @param x objeto (list) ou caminho (character) ou NULL
#' @param rotulo nome da fonte (para mensagens de erro)
#' @param obrigatorio se TRUE, erro quando ausente; se FALSE, retorna NULL
#' @return list (JSON lido) ou NULL
.carregar_fonte <- function(x, rotulo, obrigatorio = FALSE) {
  if (is.null(x)) {
    if (obrigatorio) stop("[eventlog] Fonte obrigatoria ausente: ", rotulo)
    return(NULL)
  }
  if (is.character(x)) {
    if (!file.exists(x)) {
      if (obrigatorio) stop("[eventlog] JSON nao encontrado (", rotulo, "): ", x)
      warning("[eventlog] JSON nao encontrado (", rotulo, "): ", x)
      return(NULL)
    }
    return(jsonlite::fromJSON(x, simplifyVector = TRUE))
  }
  x
}

#' Converte data dd/mm/aaaa (documentos) para Date. NA em caso de falha.
.parse_data_doc <- function(x) {
  suppressWarnings(dmy(x))
}

#' Converte data-hora dd/mm/aaaa HH:MM (andamentos) para POSIXct. NA se falhar.
.parse_data_hora <- function(x) {
  suppressWarnings(dmy_hm(x, tz = "America/Sao_Paulo"))
}

#' Normaliza um timestamp para string ISO-8601, preservando a granularidade:
#'   - eventos de documento: "AAAA-MM-DD" (precisao de dia)
#'   - eventos de tramitacao: "AAAA-MM-DDTHH:MM:SS" (precisao de minuto)
#' @param x Date ou POSIXct
#' @return character ISO ou NA
.iso <- function(x) {
  if (length(x) == 0 || all(is.na(x))) return(NA_character_)
  if (inherits(x, "Date")) return(format(x, "%Y-%m-%d"))
  format(x, "%Y-%m-%dT%H:%M:%S")
}

# -----------------------------------------------------------------------------
# Mapa de classe por documento (gabarito tem precedencia sobre o baseline)
# -----------------------------------------------------------------------------

#' Monta um data.frame numero_documento -> classe/fonte_classe, combinando o
#' gabarito (classe_final) e a classificacao baseline (classe_predita).
#'
#' Precedencia: classe_final (gabarito) > classe_predita (baseline) > NA.
#' Registra a origem em `fonte_classe` para auditoria (metodologia: nao
#' misturar silenciosamente gabarito e predicao).
#'
#' @param classificacao list do JSON de classificacao (ou NULL)
#' @param gabarito tibble consolidado (utils_gabarito.R) filtrado ao processo,
#'        ou NULL
#' @param numero_processo Numero do processo (para filtrar o gabarito)
#' @return tibble(numero_documento, classe, escore, margem, fonte_classe)
.montar_mapa_classe <- function(classificacao, gabarito, numero_processo) {
  base <- tibble(
    numero_documento = character(0),
    classe_predita   = character(0),
    escore           = numeric(0),
    margem           = numeric(0)
  )

  if (!is.null(classificacao) && !is.null(classificacao$classificacao) &&
      length(classificacao$classificacao) > 0) {
    cl <- as_tibble(classificacao$classificacao)
    base <- tibble(
      numero_documento = as.character(cl$numero_documento),
      classe_predita   = as.character(cl$classe_predita),
      escore           = if ("escore" %in% names(cl)) as.numeric(cl$escore) else NA_real_,
      margem           = if ("margem" %in% names(cl)) as.numeric(cl$margem) else NA_real_
    )
  }

  # Gabarito (classe_final) — precedencia sobre o baseline
  gab <- NULL
  if (!is.null(gabarito) && is.data.frame(gabarito) && nrow(gabarito) > 0 &&
      all(c("numero_documento", "classe_final") %in% names(gabarito))) {
    gab <- gabarito
    if ("numero_processo" %in% names(gab)) {
      gab <- gab[as.character(gab$numero_processo) == numero_processo, , drop = FALSE]
    }
    gab <- tibble(
      numero_documento = as.character(gab$numero_documento),
      classe_final     = as.character(gab$classe_final)
    )
  }

  todos_docs <- unique(c(base$numero_documento,
                         if (!is.null(gab)) gab$numero_documento else character(0)))
  if (length(todos_docs) == 0) {
    return(tibble(numero_documento = character(0), classe = character(0),
                  escore = numeric(0), margem = numeric(0),
                  fonte_classe = character(0)))
  }

  map_dfr(todos_docs, function(doc) {
    cf <- if (!is.null(gab)) gab$classe_final[gab$numero_documento == doc] else character(0)
    cp <- base$classe_predita[base$numero_documento == doc]
    esc <- base$escore[base$numero_documento == doc]
    mar <- base$margem[base$numero_documento == doc]

    classe <- NA_character_; fonte <- NA_character_
    if (length(cf) > 0 && !is.na(cf[1]) && str_squish(cf[1]) != "" &&
        cf[1] != "indefinido") {
      classe <- cf[1]; fonte <- "gabarito"
    } else if (length(cp) > 0 && !is.na(cp[1])) {
      classe <- cp[1]; fonte <- "baseline"
    }

    tibble(
      numero_documento = doc,
      classe           = classe,
      escore           = if (length(esc) > 0) esc[1] else NA_real_,
      margem           = if (length(mar) > 0) mar[1] else NA_real_,
      fonte_classe     = fonte
    )
  })
}

#' Monta um mapa numero_documento -> campos de entidade (para enriquecer os
#' eventos de documento). Retorna tibble vazio se entidades ausente.
.montar_mapa_entidades <- function(entidades) {
  vazio <- tibble(
    numero_documento = character(0), unidade_emissora = character(0),
    remetente_nome = character(0), n_signatarios = integer(0),
    destinatario_nome = character(0), n_destinatarios = integer(0),
    prazo_data_iso = character(0), prazo_ok = logical(0),
    prazo_metodo = character(0)
  )
  if (is.null(entidades) || is.null(entidades$entidades) ||
      length(entidades$entidades) == 0) {
    return(vazio)
  }
  e <- as_tibble(entidades$entidades)
  pega <- function(nome, default) if (nome %in% names(e)) e[[nome]] else default
  tibble(
    numero_documento   = as.character(e$numero_documento),
    unidade_emissora   = as.character(pega("unidade_emissora", NA_character_)),
    remetente_nome     = as.character(pega("remetente_nome", NA_character_)),
    remetente_cargo    = as.character(pega("remetente_cargo", NA_character_)),
    signatarios        = as.character(pega("signatarios", NA_character_)),
    referencias        = as.character(pega("referencias", NA_character_)),
    n_signatarios      = as.integer(pega("n_signatarios", NA_integer_)),
    destinatario_nome  = as.character(pega("destinatario_nome", NA_character_)),
    destinatario_cargo = as.character(pega("destinatario_cargo", NA_character_)),
    n_destinatarios    = as.integer(pega("n_destinatarios", NA_integer_)),
    prazo_data_iso     = as.character(pega("prazo_data_iso", NA_character_)),
    prazo_ok           = as.logical(pega("prazo_ok", NA)),
    prazo_metodo       = as.character(pega("prazo_metodo", NA_character_))
  )
}

# -----------------------------------------------------------------------------
# 1. Eventos de DOCUMENTO
# -----------------------------------------------------------------------------

#' Constroi os eventos de documento a partir da lista de documentos do scraping,
#' enriquecidos com classe (gabarito/baseline) e entidades.
#'
#' @param documentos data.frame de scraping$documentos
#' @param mapa_classe tibble de .montar_mapa_classe()
#' @param mapa_entidades tibble de .montar_mapa_entidades()
#' @param numero_processo Numero do processo (case_id)
#' @return tibble de eventos (fonte = "documento")
.eventos_documento <- function(documentos, mapa_classe, mapa_entidades,
                               numero_processo) {
  if (is.null(documentos) || nrow(documentos) == 0) return(tibble())

  docs <- as_tibble(documentos)

  # Junta classe e entidades por numero_documento
  docs <- docs |>
    mutate(numero_documento = as.character(numero_documento)) |>
    left_join(mapa_classe, by = "numero_documento") |>
    left_join(mapa_entidades, by = "numero_documento")

  data_doc <- .parse_data_doc(docs$data_documento)

  # activity: combina classe semantica + tipo. Quando nao ha classe (anexo/
  # restrito), usa "juntada" como acao generica, para o documento ainda existir
  # no fluxo (FLUXOS sec. 8: documento avulso e no que nao recebe nem dirige seta).
  classe_lbl <- ifelse(is.na(docs$classe), "juntada", docs$classe)
  activity   <- paste0(classe_lbl, " :: ", docs$tipo)

  # resource/unidade: a unidade da LISTA de documentos (scraping) tem
  # precedencia — e a que o SEI atribui ao ato e a mais especifica/confiavel
  # (ex.: "COIR"). A unidade_emissora, extraida do NUMERO do documento, e so
  # fallback: ela pode trazer o DEPARTAMENTO (ex.: "DAEI-IPHAN") em vez da
  # coordenacao real. unidade_emissora segue guardada a parte para auditoria.
  unidade <- ifelse(!is.na(docs$unidade_sigla) & docs$unidade_sigla != "",
                    docs$unidade_sigla, docs$unidade_emissora)

  tibble(
    case_id          = numero_processo,
    timestamp        = .iso(data_doc),
    activity         = activity,
    classe           = docs$classe,
    tipo             = docs$tipo,
    unidade          = unidade,
    fonte            = "documento",
    documento        = docs$numero_documento,
    acesso           = docs$acesso,
    precisao_temporal = "dia",
    # Enriquecimento (auditoria / etapas posteriores)
    unidade_lista    = docs$unidade_sigla,
    unidade_emissora = docs$unidade_emissora,
    fonte_classe     = docs$fonte_classe,
    escore           = docs$escore,
    margem           = docs$margem,
    remetente        = docs$remetente_nome,
    remetente_cargo  = docs$remetente_cargo,
    signatarios      = docs$signatarios,
    referencias      = docs$referencias,
    n_signatarios    = docs$n_signatarios,
    destinatario     = docs$destinatario_nome,
    destinatario_cargo = docs$destinatario_cargo,
    n_destinatarios  = docs$n_destinatarios,
    prazo_data_iso   = docs$prazo_data_iso,
    prazo_ok         = docs$prazo_ok,
    prazo_metodo     = docs$prazo_metodo,
    data_inclusao    = docs$data_inclusao,
    id_evento        = NA_character_,
    descricao        = NA_character_,
    unidade_origem   = NA_character_,
    unidade_destino  = NA_character_
  )
}

# -----------------------------------------------------------------------------
# 2. Eventos de TRAMITACAO (andamentos)
# -----------------------------------------------------------------------------

#' Classifica a descricao de um andamento num rotulo de tramitacao normalizado.
#' @param descricao character
#' @return character (rotulo) ou "outro"
.rotulo_tramitacao <- function(descricao) {
  if (is.na(descricao) || descricao == "") return("outro")
  for (r in TRAMITACAO_ROTULOS) {
    if (str_detect(descricao, fixed(r$padrao))) return(r$rotulo)
  }
  "outro"
}

#' Constroi os eventos de tramitacao a partir dos andamentos do scraping.
#'
#' Cada andamento vira um evento (fonte = "tramitacao"). Para o registro
#' "Processo remetido pela unidade <MAE>", extrai a unidade de ORIGEM (a peca
#' esta na unidade de DESTINO = unidade_sigla do andamento), preenchendo
#' unidade_origem/unidade_destino — a aresta do grafo organizacional.
#'
#' @param andamentos data.frame de scraping$andamentos
#' @param numero_processo Numero do processo (case_id)
#' @return tibble de eventos (fonte = "tramitacao")
.eventos_tramitacao <- function(andamentos, numero_processo) {
  if (is.null(andamentos) || nrow(andamentos) == 0) return(tibble())

  a <- as_tibble(andamentos)
  ts <- .parse_data_hora(a$data_hora)

  rotulo <- unname(vapply(a$descricao, .rotulo_tramitacao, character(1)))

  # Origem do repasse: "Processo remetido pela unidade <MAE>"
  origem <- str_match(a$descricao, PADRAO_REMETIDO)[, 2]
  origem <- unname(str_squish(origem))
  # Para "remetido", o destino e a unidade do proprio andamento; a origem e a
  # capturada. Para os demais rotulos, nao ha aresta (origem/destino NA).
  destino <- ifelse(rotulo == "remetido", a$unidade_sigla, NA_character_)

  tibble(
    case_id          = numero_processo,
    timestamp        = .iso(ts),
    activity         = paste0("tramitacao :: ", rotulo),
    classe           = NA_character_,
    tipo             = NA_character_,
    unidade          = a$unidade_sigla,
    fonte            = "tramitacao",
    documento        = NA_character_,
    acesso           = NA_character_,
    precisao_temporal = "minuto",
    unidade_lista    = a$unidade_sigla,
    unidade_emissora = NA_character_,
    fonte_classe     = NA_character_,
    escore           = NA_real_,
    margem           = NA_real_,
    remetente        = NA_character_,
    remetente_cargo  = NA_character_,
    signatarios      = NA_character_,
    referencias      = NA_character_,
    n_signatarios    = NA_integer_,
    destinatario     = NA_character_,
    destinatario_cargo = NA_character_,
    n_destinatarios  = NA_integer_,
    prazo_data_iso   = NA_character_,
    prazo_ok         = NA,
    prazo_metodo     = NA_character_,
    data_inclusao    = NA_character_,
    id_evento        = as.character(a$id_atividade),
    descricao        = a$descricao,
    unidade_origem   = ifelse(rotulo == "remetido", origem, NA_character_),
    unidade_destino  = destino
  )
}

# -----------------------------------------------------------------------------
# Arestas do fluxo organizacional (a partir dos eventos "remetido")
# -----------------------------------------------------------------------------

#' Deriva as arestas origem -> destino da tramitacao entre unidades.
#' @param eventos_tram tibble de eventos de tramitacao
#' @return tibble(origem, destino, timestamp, id_evento)
.arestas_tramitacao <- function(eventos_tram) {
  if (nrow(eventos_tram) == 0) return(tibble(
    origem = character(0), destino = character(0),
    timestamp = character(0), id_evento = character(0)))
  ar <- eventos_tram |>
    filter(!is.na(unidade_origem) & !is.na(unidade_destino)) |>
    transmute(origem = unidade_origem, destino = unidade_destino,
              timestamp, id_evento)
  ar
}

# -----------------------------------------------------------------------------
# Ordenacao do log
# -----------------------------------------------------------------------------

#' Ordena o event log no tempo, com desempate estavel.
#'
#' Chave de ordenacao:
#'   1. timestamp (data/data-hora ISO como string — ordenacao lexicografica
#'      coincide com a cronologica no formato ISO-8601).
#'   2. Eventos sem timestamp (NA) vao para o fim.
#'   3. Desempate: documentos antes de tramitacoes no mesmo instante (a peca e
#'      juntada e depois tramita), e id_evento crescente entre andamentos.
#'
#' @param log tibble do event log
#' @return tibble ordenado, com coluna `ordem` (1..n)
.ordenar_log <- function(log) {
  if (nrow(log) == 0) return(log)
  # NA de timestamp vai para o fim
  chave_ts <- ifelse(is.na(log$timestamp), "9999-12-31T23:59:59", log$timestamp)
  # documento (0) antes de tramitacao (1) no mesmo instante
  prioridade_fonte <- ifelse(log$fonte == "documento", 0L, 1L)
  id_num <- suppressWarnings(as.numeric(log$id_evento))
  id_num[is.na(id_num)] <- 0

  ord <- order(chave_ts, prioridade_fonte, id_num, method = "radix")
  log <- log[ord, , drop = FALSE]
  log$ordem <- seq_len(nrow(log))
  log
}

# -----------------------------------------------------------------------------
# Funcao principal: construir_event_log_processo()
# -----------------------------------------------------------------------------

#' Constroi o event log de um processo, combinando scraping (obrigatorio),
#' entidades, classificacao e gabarito (opcionais).
#'
#' @param scraping list de scrape_processo_*() OU caminho do JSON de scraping.
#'        OBRIGATORIO — e a fonte autoritativa de documentos e andamentos.
#' @param entidades list de extrair_entidades_processo() OU caminho, ou NULL.
#' @param classificacao list de classificar_processo() OU caminho, ou NULL.
#' @param gabarito tibble consolidado (consolidar_gabarito()$ground_truth) OU
#'        NULL. Quando fornecido, classe_final tem precedencia sobre o baseline.
#' @param dir_processed Diretorio de saida.
#' @param persistir Logico — salvar JSON e CSV do event log.
#' @return list com: event_log (tibble), arestas (tibble), meta (list)
construir_event_log_processo <- function(
    scraping,
    entidades     = NULL,
    classificacao = NULL,
    gabarito      = NULL,
    dir_processed = "data/processed",
    persistir     = TRUE
) {
  scr <- .carregar_fonte(scraping, "scraping", obrigatorio = TRUE)
  ent <- .carregar_fonte(entidades, "entidades", obrigatorio = FALSE)
  cla <- .carregar_fonte(classificacao, "classificacao", obrigatorio = FALSE)

  numero_processo <- scr$cabecalho$numero_processo
  if (is.null(numero_processo) || is.na(numero_processo)) {
    stop("[eventlog] numero_processo ausente no scraping.")
  }
  numero_seguro <- str_replace_all(numero_processo, "[^0-9A-Za-z]", "_")

  cat("[eventlog] Processo:", numero_processo, "\n")

  mapa_classe    <- .montar_mapa_classe(cla, gabarito, numero_processo)
  mapa_entidades <- .montar_mapa_entidades(ent)

  ev_doc  <- .eventos_documento(scr$documentos, mapa_classe, mapa_entidades,
                                numero_processo)
  ev_tram <- .eventos_tramitacao(scr$andamentos, numero_processo)

  log <- bind_rows(ev_doc, ev_tram)
  log <- .ordenar_log(log)

  arestas <- .arestas_tramitacao(ev_tram)

  # ---- Metricas e limitacoes mensuraveis ----
  n_doc  <- sum(log$fonte == "documento")
  n_tram <- sum(log$fonte == "tramitacao")
  n_com_classe <- sum(!is.na(log$classe))
  n_gabarito   <- sum(log$fonte_classe == "gabarito", na.rm = TRUE)
  n_baseline   <- sum(log$fonte_classe == "baseline", na.rm = TRUE)
  n_sem_ts     <- sum(is.na(log$timestamp))
  n_restritos  <- sum(log$fonte == "documento" & log$acesso == "restrito", na.rm = TRUE)

  cat("[eventlog] --- Resumo ---\n")
  cat("[eventlog] Eventos totais    :", nrow(log), "\n")
  cat("[eventlog]  - de documento   :", n_doc, "\n")
  cat("[eventlog]  - de tramitacao  :", n_tram, "\n")
  cat("[eventlog] Docs com classe   :", n_com_classe,
      "(gabarito:", n_gabarito, "| baseline:", n_baseline, ")\n")
  cat("[eventlog] Docs restritos    :", n_restritos, "(sem classe — conteudo nao extraido)\n")
  cat("[eventlog] Arestas de fluxo  :", nrow(arestas), "\n")
  if (n_sem_ts > 0) {
    cat("[eventlog] AVISO:", n_sem_ts, "evento(s) sem timestamp valido (ordenados ao fim).\n")
  }

  # Intervalo temporal do log (limitacao: processo em curso pode nao ter fim)
  ts_validos <- log$timestamp[!is.na(log$timestamp)]
  inicio <- if (length(ts_validos) > 0) min(ts_validos) else NA_character_
  fim    <- if (length(ts_validos) > 0) max(ts_validos) else NA_character_

  saida <- list(
    meta = list(
      numero_processo   = numero_processo,
      timestamp         = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      eventos_totais    = nrow(log),
      eventos_documento = n_doc,
      eventos_tramitacao = n_tram,
      docs_com_classe   = n_com_classe,
      classe_de_gabarito = n_gabarito,
      classe_de_baseline = n_baseline,
      docs_restritos    = n_restritos,
      eventos_sem_timestamp = n_sem_ts,
      arestas_fluxo     = nrow(arestas),
      intervalo_inicio  = inicio,
      intervalo_fim     = fim,
      fontes = list(
        scraping      = TRUE,
        entidades     = !is.null(ent),
        classificacao = !is.null(cla),
        gabarito      = !is.null(gabarito)
      ),
      limitacoes = c(
        "Fase anterior a abertura no SEI (SAIP) nao aparece nos autos.",
        "Documentos tem precisao de dia; andamentos de minuto (ver precisao_temporal).",
        "Anexos nao-nativos e documentos restritos entram como juntada, sem classe.",
        "Processos em curso tem traco incompleto (atividades terminais podem faltar).",
        "Prazos nao sao calculados aqui (etapa 09); apenas transportamos a data-limite extraida."
      )
    ),
    event_log = log,
    arestas   = arestas
  )

  if (persistir) {
    dir_processed_dia <- dir_saida_dia(dir_processed)
    data_str <- format(Sys.time(), "%Y%m%d")

    nome_json <- paste0(numero_seguro, "_eventlog_", data_str, ".json")
    caminho_json <- file.path(dir_processed_dia, nome_json)
    write_json(saida, caminho_json, pretty = TRUE, auto_unbox = TRUE, na = "null")
    cat("[eventlog] Persistido (JSON) em:", caminho_json, "\n")

    # CSV achatado: formato classico case/activity/timestamp/resource + colunas
    # uteis. Listas/objetos nao entram no CSV (o log ja e plano).
    nome_csv <- paste0(numero_seguro, "_eventlog_", data_str, ".csv")
    caminho_csv <- file.path(dir_processed_dia, nome_csv)
    .escrever_csv_log(log, caminho_csv)
    cat("[eventlog] Persistido (CSV) em :", caminho_csv, "\n")
  }

  invisible(saida)
}

#' Escreve o event log achatado em CSV (UTF-8), preservando acentos.
.escrever_csv_log <- function(log, caminho) {
  if (requireNamespace("readr", quietly = TRUE)) {
    readr::write_excel_csv(log, caminho)
  } else {
    utils::write.csv(log, caminho, row.names = FALSE, fileEncoding = "UTF-8")
  }
  invisible(caminho)
}
