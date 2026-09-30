# =============================================================================
# 09_prazos.R
# Calculo de prazos e conformidade temporal (POC IPHAN)
#
# Responsabilidade: Camada de Prazos (ver architecture.md). Consome o EVENT LOG
#   (07) e as NORMATIVAS (08) para apurar, por ATO analisavel, se a manifestacao
#   do IPHAN ocorreu dentro do prazo normativo.
#
# Criterio de apuracao (definido com o pesquisador; ver FLUXOS sec. 7 e 8):
#   - INICIO do relogio: o ato de ABERTURA da demanda que fixa/dispara o prazo.
#     Na pratica, um Despacho (tipicamente da CGM/triagem) que traz uma
#     data-limite extraida (prazo_data_iso) ou, na ausencia dela, a data do
#     proprio despacho de abertura.
#   - FIM do relogio: a data do PARECER correspondente (encerramento do ato =
#     data da manifestacao; FLUXOS sec. 8). Aqui usamos a data do documento do
#     parecer como proxy da "ultima assinatura" — LIMITACAO registrada, pois o
#     event log carrega a data do documento, nao a data de cada assinatura.
#
#   Como decidimos "dentro do prazo" (Criterio UNICO A / data_limite_cgm):
#     comparamos a data-limite fixada no despacho (prazo_data_iso) com a data do
#     parecer (FLUXOS sec. 8). So abre relogio de analise o despacho que fixou
#     essa data-limite. Um despacho que apenas pede/encaminha a PECA (ex.:
#     apresentar o PAIPA) NAO tem data-limite e NAO abre analise — "pedir a
#     peca" e diferente de "pedir o parecer sobre a peca". Sem a data-limite
#     fixada pela unidade, nao ha como afirmar que o despacho abriu um prazo de
#     analise; esses casos ficam de fora (LIMITACAO registrada na saida).
#     O prazo normativo (08) e mantido apenas como METADADO informativo
#     (peca_norma / prazo_normativo_dias), NAO como base de decisao.
#
# Emparelhamento CONSERVADOR (Opcao 1, sem motor de conformidade completo):
#   cada despacho de abertura com prazo e associado ao PROXIMO parecer no tempo.
#   Onde nao ha parecer subsequente, o relogio fica status = "em_curso" (ou
#   "indeterminado"); nao forcamos pares nem inventamos correspondencia.
#
# Complementacao (FLUXOS sec. 5): os DOIS relogios (15 dias do orgao, 30 dias do
#   interessado) NAO se somam. Aqui apuramos o relogio do ORGAO (analise da
#   complementacao) do mesmo modo que os demais atos; o relogio do interessado
#   e apenas SINALIZADO como distinto, nao somado — registrado como limitacao.
#
# Entradas (aceita objeto OU caminho, como os demais modulos):
#   - event_log  : list de construir_event_log_processo() OU caminho do JSON.
#   - normativas : list de carregar_normativas() OU NULL (carrega padrao).
#
# Saida: data/processed/{proc}_prazos_{data}.json (relogios + meta + limitacoes)
#
# Limitacoes registradas (research-methodology / FLUXOS sec. 10):
#   - "Fim" usa a data do documento do parecer, nao a data da ultima assinatura.
#   - Fase SAIP nao aparece nos autos: prazo da FCA so e mensuravel com ficha e
#     parecer juntados.
#   - Emparelhamento e conservador (proximo parecer no tempo); nao resolve casos
#     com multiplos pareceres concorrentes — marcados para revisao.
#   - Os dois relogios da complementacao nao se somam (apuramos o do orgao).
#   - Processo em curso: relogio sem parecer fica "em_curso" (nao mensuravel).
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

# 08 fornece carregar_normativas()/consultar_prazo(). Carrega se ausente.
if (!exists("consultar_prazo")) {
  .dir_09 <- tryCatch(dirname(sys.frame(1)$ofile), error = function(e) NA)
  cand <- c("R/08_normativas.R", "08_normativas.R",
            if (!is.na(.dir_09)) file.path(.dir_09, "08_normativas.R"))
  cand <- cand[file.exists(cand)]
  if (length(cand) > 0) source(cand[1])
}

# -----------------------------------------------------------------------------
# Constantes
# -----------------------------------------------------------------------------

# Classes de ato que ABREM/disparam um relogio de analise (despachos de
# triagem/encaminhamento que fixam prazo). Um despacho so vira inicio de relogio
# quando traz uma data-limite (prazo_data_iso) — senao nao ha o que apurar.
CLASSES_ABERTURA <- c("solicitacao", "encaminhamento", "exigencia_complementacao")

# Classes de ato que ENCERRAM o relogio (a manifestacao do IPHAN).
CLASSES_PARECER <- c("manifestacao_parecer", "decisao")

# Janela temporal (dias corridos) para o emparelhamento conservador abertura ->
# parecer. Um parecer alem desta janela e considerado de OUTRA fase e NAO e
# emparelhado por proximidade (evita pares cruzados que gerariam "estourado"
# espurio). Editavel; 90 dias cobre com folga os prazos da IN (max. 30 dias +
# prorrogacao por igual periodo + tramitacao).
JANELA_PAREAMENTO_DIAS <- 90L

# -----------------------------------------------------------------------------
# Utilitarios
# -----------------------------------------------------------------------------

.carregar_event_log <- function(x) {
  if (is.null(x)) stop("[prazos] event_log ausente.")
  if (is.character(x)) {
    if (!file.exists(x)) stop("[prazos] JSON de event log nao encontrado: ", x)
    return(jsonlite::fromJSON(x, simplifyVector = TRUE))
  }
  x
}

#' Extrai a parte de data (AAAA-MM-DD) de um timestamp ISO do event log.
#' Documentos tem precisao de dia; tramitacoes tem data-hora. Para prazos,
#' trabalhamos em DIAS (art. 54 conta dias corridos).
.data_de <- function(ts) {
  if (is.na(ts) || ts == "") return(as.Date(NA))
  as.Date(substr(ts, 1, 10))
}

#' Diferenca em dias corridos entre duas datas (fim - inicio).
.dias_corridos <- function(inicio, fim) {
  if (is.na(inicio) || is.na(fim)) return(NA_integer_)
  as.integer(as.numeric(difftime(fim, inicio, units = "days")))
}

# -----------------------------------------------------------------------------
# Apuracao de um relogio (um par abertura -> parecer)
# -----------------------------------------------------------------------------

#' Apura um relogio: dado o evento de abertura (que SEMPRE traz data-limite,
#' garantido por .emparelhar_relogios) e o parecer emparelhado, calcula
#' dias/excesso/status pelo Criterio A (data-limite fixada pela unidade).
#'
#' Criterio unico (A / data_limite_cgm): o unico modo de apurar prazo e comparar
#' a data-limite explicita fixada no despacho (prazo_data_iso) com a data do
#' parecer. Sem essa data-limite o despacho nao chega aqui (nao e tratado como
#' abertura de analise). O prazo normativo (08) e mantido apenas como METADADO
#' informativo (peca/dias), NAO como base de decisao — "pedir a peca" nao e
#' "pedir o parecer", e sem data-limite nao ha como afirmar que o despacho abriu
#' analise (ver FLUXOS sec. 8 e limitacao registrada na saida).
#'
#' @param abertura linha (1) do event log do despacho de abertura
#' @param parecer linha (1) do event log do parecer, ou NULL se nao houver
#' @param normativas list de carregar_normativas()
#' @return tibble de 1 linha com a apuracao do relogio
.apurar_relogio <- function(abertura, parecer, normativas) {
  data_abertura <- .data_de(abertura$timestamp)
  data_limite   <- if (!is.na(abertura$prazo_data_iso) && abertura$prazo_data_iso != "") {
    as.Date(abertura$prazo_data_iso)
  } else as.Date(NA)

  # Consulta o prazo normativo pela peca/tipo do PARECER (o ato analisado),
  # com fallback para o tipo do proprio despacho quando nao ha parecer.
  # Usado apenas como METADADO (peca_norma/prazo_normativo_dias), nao decide.
  tipo_para_consulta <- if (!is.null(parecer)) parecer$tipo else abertura$tipo
  consulta <- consultar_prazo(normativas, tipo_para_consulta)
  prazo_normativo <- if (isTRUE(consulta$encontrado)) consulta$prazo_dias else NA_integer_

  data_parecer <- if (!is.null(parecer)) .data_de(parecer$timestamp) else as.Date(NA)

  # --- Decisao de status (Criterio A: data-limite fixada) ---
  metodo <- "data_limite_cgm"
  data_limite_usada <- data_limite
  dias_decorridos <- NA_integer_
  dentro <- NA
  status <- NA_character_

  if (is.null(parecer)) {
    # Sem parecer emparelhado: relogio aberto (processo em curso). Registra a
    # data-limite fixada, mas o fim nao e mensuravel.
    status <- "em_curso"
  } else {
    dias_decorridos <- .dias_corridos(data_abertura, data_parecer)
    dentro <- !is.na(data_parecer) && data_parecer <= data_limite
    status <- if (isTRUE(dentro)) "dentro" else "estourado"
  }

  excesso <- if (!is.na(data_limite_usada) && !is.na(data_parecer)) {
    max(0L, .dias_corridos(data_limite_usada, data_parecer))
  } else NA_integer_

  tibble(
    documento_abertura = abertura$documento,
    tipo_abertura      = abertura$tipo,
    classe_abertura    = abertura$classe,
    unidade_abertura   = abertura$unidade,
    data_abertura      = as.character(data_abertura),
    documento_parecer  = if (!is.null(parecer)) parecer$documento else NA_character_,
    tipo_parecer       = if (!is.null(parecer)) parecer$tipo else NA_character_,
    data_parecer       = as.character(data_parecer),
    peca_norma         = consulta$peca_norma,
    termo_casado       = consulta$termo_casado,
    prazo_normativo_dias = prazo_normativo,
    prazo_origem       = consulta$origem,
    prazo_e_sinonimo   = isTRUE(consulta$e_sinonimo),
    prazo_origem_sinonimo = consulta$origem_sinonimo,
    data_limite        = as.character(data_limite_usada),
    metodo             = metodo,
    dias_decorridos    = dias_decorridos,
    excesso_dias       = excesso,
    dentro_do_prazo    = dentro,
    e_complementacao   = identical(abertura$classe, "exigencia_complementacao"),
    status             = status
  )
}

# -----------------------------------------------------------------------------
# Emparelhamento conservador abertura -> proximo parecer
# -----------------------------------------------------------------------------

#' Emparelha cada despacho de abertura a um parecer subsequente, de forma
#' CONSERVADORA (Opcao 1: peca compativel + janela temporal), evitando pares
#' cruzados entre fases que gerariam "estourado" espurio.
#'
#' So abrem relogio despachos de CLASSES_ABERTURA que fixaram data-limite
#' (prazo_data_iso presente). Regra de escolha do parecer para cada abertura
#' (entre os ainda nao consumidos, com data >= data da abertura):
#'   1. O parecer mais proximo DENTRO da janela (JANELA_PAREAMENTO_DIAS).
#'   2. Se o candidato mais proximo esta ALEM da janela, NAO empa­relha (relogio
#'      fica "em_curso": nao afirmamos atraso de um par incerto).
#'
#' Um parecer so serve a um relogio (conservador). "Nao inventar": sem
#' data-limite, o despacho nem entra como abertura.
#'
#' @param log tibble do event_log (ja ordenado)
#' @param normativas list de carregar_normativas()
#' @return tibble de relogios (uma linha por abertura mantida)
.emparelhar_relogios <- function(log, normativas) {
  doc <- log[log$fonte == "documento", , drop = FALSE]
  if (nrow(doc) == 0) return(tibble())

  # ABERTURA de relogio (Opcao A): so abre relogio de analise o despacho que
  # FIXOU uma data-limite explicita (prazo_data_iso presente). Isso distingue
  # "pedir a PECA" (ex.: apresentar o PAIPA — sem data-limite) de "pedir o
  # PARECER sobre a peca" (a unidade fixa a data-limite de analise). Sem
  # data-limite, o despacho NAO e tratado como abertura de analise (ver FLUXOS
  # sec. 8 e limitacao registrada na saida).
  tem_data_limite <- !is.na(doc$prazo_data_iso) & doc$prazo_data_iso != ""
  is_abertura <- !is.na(doc$classe) & doc$classe %in% CLASSES_ABERTURA &
    tem_data_limite
  is_parecer  <- !is.na(doc$classe) & doc$classe %in% CLASSES_PARECER

  idx_abertura <- which(is_abertura)
  if (length(idx_abertura) == 0) return(tibble())

  parecer_consumido <- rep(FALSE, nrow(doc))
  relogios <- list()

  for (ia in idx_abertura) {
    data_ab <- .data_de(doc$timestamp[ia])

    candidatos <- which(is_parecer & !parecer_consumido & !is.na(doc$timestamp))
    candidatos <- candidatos[vapply(candidatos, function(j)
      !is.na(.data_de(doc$timestamp[j])) && !is.na(data_ab) &&
        .data_de(doc$timestamp[j]) >= data_ab, logical(1))]

    jp <- NA_integer_
    if (length(candidatos) > 0) {
      # ordena candidatos por proximidade temporal
      candidatos <- candidatos[order(doc$timestamp[candidatos])]
      dias <- vapply(candidatos, function(j)
        .dias_corridos(data_ab, .data_de(doc$timestamp[j])), integer(1))

      # O parecer mais proximo DENTRO da janela; alem dela nao empa­relha
      # (relogio fica "em_curso": nao afirmamos atraso de um par incerto).
      na_janela <- which(dias <= JANELA_PAREAMENTO_DIAS)
      if (length(na_janela) > 0) jp <- candidatos[na_janela[1]]
    }

    parecer_linha <- if (!is.na(jp)) doc[jp, , drop = FALSE] else NULL
    rel <- .apurar_relogio(doc[ia, , drop = FALSE], parecer_linha, normativas)
    if (!is.null(rel)) {
      if (!is.na(jp)) parecer_consumido[jp] <- TRUE
      relogios[[length(relogios) + 1]] <- rel
    }
  }

  if (length(relogios) == 0) return(tibble())
  bind_rows(relogios)
}

# -----------------------------------------------------------------------------
# Funcao principal: calcular_prazos_processo()
# -----------------------------------------------------------------------------

#' Calcula os prazos/relogios de um processo a partir do event log e das
#' normativas.
#'
#' @param event_log list de construir_event_log_processo() OU caminho do JSON.
#' @param normativas list de carregar_normativas() OU NULL (carrega padrao).
#' @param dir_processed Diretorio de saida.
#' @param persistir Logico — salvar JSON de prazos.
#' @return list com: relogios (tibble), meta (list)
calcular_prazos_processo <- function(
    event_log,
    normativas    = NULL,
    dir_processed = "data/processed",
    persistir     = TRUE
) {
  el <- .carregar_event_log(event_log)
  if (is.null(normativas)) normativas <- carregar_normativas()

  numero_processo <- el$meta$numero_processo
  if (is.null(numero_processo) || is.na(numero_processo)) {
    stop("[prazos] numero_processo ausente no event log.")
  }
  numero_seguro <- str_replace_all(numero_processo, "[^0-9A-Za-z]", "_")

  log <- as_tibble(el$event_log)

  cat("[prazos] Processo:", numero_processo, "\n")

  relogios <- .emparelhar_relogios(log, normativas)

  # ---- Resumo ----
  n_rel <- nrow(relogios)
  if (n_rel == 0) {
    cat("[prazos] Nenhum relogio apuravel (sem despacho de abertura com data-limite).\n")
  } else {
    distrib <- table(relogios$status)
    cat("[prazos] Relogios apurados:", n_rel, "\n")
    for (nm in names(distrib)) cat("[prazos]   ", nm, ":", distrib[[nm]], "\n")
    n_estourado <- sum(relogios$status == "estourado", na.rm = TRUE)
    if (n_estourado > 0) {
      cat("[prazos] AVISO:", n_estourado, "relogio(s) com prazo ESTOURADO.\n")
    }
    n_interp <- sum(relogios$prazo_e_sinonimo &
                      relogios$prazo_origem_sinonimo == "interpretacao_pesquisador",
                    na.rm = TRUE)
    if (n_interp > 0) {
      cat("[prazos] Nota:", n_interp,
          "relogio(s) usam prazo por INTERPRETACAO do pesquisador (PAPIPA).\n")
    }
  }

  contar <- function(st) if (n_rel == 0) 0L else sum(relogios$status == st, na.rm = TRUE)

  saida <- list(
    meta = list(
      numero_processo   = numero_processo,
      timestamp         = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      relogios_totais   = n_rel,
      dentro            = contar("dentro"),
      estourado         = contar("estourado"),
      em_curso          = contar("em_curso"),
      indeterminado     = contar("indeterminado"),
      normativa_versao  = if (!is.null(normativas$versao)) normativas$versao else NA_character_,
      regra_contagem    = regra_contagem(normativas)$unidade,
      limitacoes = c(
        "Prazos so sao apurados quando a unidade fixou a data-limite explicitamente (Criterio A / FLUXOS sec. 8); despachos sem data-limite nao sao tratados como abertura de analise (pedir a peca != pedir o parecer).",
        "O 'fim' do relogio usa a data do documento do parecer, nao a data da ultima assinatura.",
        "Fase SAIP nao aparece nos autos: prazo da FCA so e mensuravel com ficha e parecer juntados.",
        "Emparelhamento conservador (proximo parecer no tempo); casos com pareceres concorrentes ficam para revisao.",
        "Os dois relogios da complementacao nao se somam; apuramos o do orgao (15 dias).",
        "Processo em curso: relogio sem parecer subsequente fica 'em_curso' (nao mensuravel)."
      )
    ),
    relogios = relogios
  )

  if (persistir) {
    data_str <- format(Sys.time(), "%Y%m%d")
    nome     <- paste0(numero_seguro, "_prazos_", data_str, ".json")
    caminho  <- file.path(dir_saida_dia(dir_processed), nome)
    write_json(saida, caminho, pretty = TRUE, auto_unbox = TRUE, na = "null")
    cat("[prazos] Persistido em:", caminho, "\n")
  }

  invisible(saida)
}
