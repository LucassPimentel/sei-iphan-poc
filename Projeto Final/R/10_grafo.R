# =============================================================================
# 10_grafo.R
# Grafo de TRAMITACAO do processo (POC IPHAN) — um unico grafo para validacao
#
# Responsabilidade: Camada de Grafos (ver architecture.md). Apos avaliacao com o
#   pesquisador, consolidamos UM UNICO grafo (em vez de tres) que reune as
#   informacoes necessarias para a validacao humana do fluxo: quem emitiu qual
#   documento, para quem/qual unidade foi encaminhado, e a situacao de prazo.
#
# Modelo (inspirado no 04_saida.R do especialista, porem SIMPLIFICADO):
#   - NO = documento/ato do processo. Rotulo interno: tipo + unidade + data +
#     remetente (signatario). A forma/cor comunica a classe/situacao.
#   - ARESTA = o fluxo entre documentos, na ordem cronologica. Duas fontes:
#       (a) pares abertura->parecer dos PRAZOS (09): sao ligacoes semanticas
#           reais (o despacho que abre a demanda -> o parecer que a responde);
#       (b) uma "espinha" cronologica que liga cada ato ao ato seguinte no tempo,
#           para o fluxo ficar legivel de ponta a ponta.
#     A aresta e ROTULADA com o DESTINATARIO do documento de origem (quem
#     encaminhou para quem), quando ele existe (entidades).
#
# Cores/situacao (o que os dados sustentam — sem motor de conformidade):
#   - verde  : parecer emitido DENTRO do prazo (relogio "dentro" do 09).
#   - vermelho: parecer emitido FORA do prazo (relogio "estourado").
#   - lilas  : ato de exigencia de complementacao (classe do 06).
#   - preto  : documento avulso (sem classe: anexos/restritos sem texto).
#   - branco : demais atos do processo.
#
# Saida:
#   - data/processed/AAAAMMDD/{proc}_grafo_{data}.json  (grafo COMO DADO).
#   - output/AAAAMMDD/{proc}_grafo_{data}.png           (imagem estatica, ggraph).
#   - output/AAAAMMDD/{proc}_grafo_{data}.html          (interativo, visNetwork:
#     layout hierarquico em arvore, zoom/pan — RECOMENDADO p/ grafos grandes).
#
# Limitacoes registradas (research-methodology / FLUXOS sec. 10):
#   - Nao ha ligacao documento->documento explicita nos dados; as arestas de
#     fluxo cronologico sao uma APROXIMACAO da ordem dos atos, nao o encadeamento
#     juridico exato. As arestas de prazo (abertura->parecer) sao semanticas.
#   - Marcacoes finas da secao 8 do FLUXOS (tracejado amarelo/marrom, trapezio
#     laranja, aba magenta) NAO sao geradas — exigem o motor de conformidade.
#   - Anexos/restritos entram como nos avulsos (sem texto/classe).
#
# Tecnologias: dplyr, tibble, stringr, igraph; ggraph/ggplot2 (render); jsonlite
# =============================================================================

library(dplyr)
library(tibble)
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

if (!requireNamespace("igraph", quietly = TRUE)) {
  stop("[grafo] Pacote 'igraph' necessario. Instale com install.packages('igraph').")
}

# -----------------------------------------------------------------------------
# Cores por situacao/classe (paleta simples)
# -----------------------------------------------------------------------------

COR_DENTRO      <- "#CCF2CC"   # parecer dentro do prazo
COR_FORA        <- "#FF9980"   # parecer fora do prazo
COR_COMPLEMENTO <- "#D6BCEB"   # exigencia de complementacao (lilas)
COR_AVULSO      <- "#1A1A1A"   # documento avulso (sem classe)
COR_ATO         <- "#FFFFFF"   # demais atos
COR_DEMANDA_EXT <- "#F5D58A"   # demanda externa (protocolo do interessado)

# Ordem de rotulos de situacao para a legenda/atributo
SITUACOES <- c("dentro_prazo", "fora_prazo", "complementacao",
               "demanda_externa", "avulso", "ato")

# Tipo que ABRE uma sequencia de documentacao externa: o "Recibo" de
# peticionamento (marcador real de uma nova petição do interessado no SAIP).
# As demais pecas (Requerimento, estudos, anexos) CONTINUAM o mesmo grupo.
TIPOS_ABERTURA_EXTERNA <- c("Recibo")

# Janela (em nº de ATOS a frente) para o emparelhamento por ENDERECAMENTO:
# o destinatario de A so casa com um signatario de B se B estiver dentro desta
# janela apos A. Evita saltos longos e espurios (ex.: despacho de marco casar
# com parecer de agosto so porque o nome coincide). Editavel.
JANELA_ENDERECAMENTO_ATOS <- 4L

# --- Demanda da triagem (criterio 4 do especialista; FLUXOS sec. 2 e 9) ------
# O despacho da unidade de TRIAGEM (CGM/CAIP) declara o que sera analisado; o
# parecer do componente que o atende liga-se a ele, mesmo sem citacao. Isto e
# uma aresta de FLUXO (nao apura prazo — quem faz isso e o 09). Complementa a
# aresta de prazo, que so existe quando o despacho fixou data-limite explicita.

# Classes de ato do DESPACHO que abre a demanda de analise (o config das classes
# semanticas classifica o despacho da CGM como encaminhamento/solicitacao).
CLASSES_DEMANDA_TRIAGEM <- c("encaminhamento", "solicitacao")

# Classes de ato que ATENDEM a demanda (a manifestacao do IPHAN). Espelha
# CLASSES_PARECER do 09_prazos.R (mesmo conceito; nao duplicamos a definicao de
# prazo, so reaproveitamos quais classes sao "parecer").
CLASSES_PARECER_TRIAGEM <- c("manifestacao_parecer", "decisao")

# Prefixo de tipo do despacho de triagem (o ato e sempre um "Despacho").
PREFIXO_DESPACHO_TRIAGEM <- "Despacho"

# Janela (em nº de ATOS a frente) para casar o despacho de triagem ao parecer:
# so o 1o parecer dentro desta janela apos o despacho e considerado atendimento.
# Evita casar despacho de uma fase com parecer de outra. Editavel.
JANELA_DEMANDA_TRIAGEM_ATOS <- 6L

# --- Simplificacao visual do grafo (legibilidade) ----------------------------
# (A) Limite de CITACOES desenhadas por no citante. Um parecer cita muitos
# documentos (8-13) como "objeto de analise"; desenhar todas polui o grafo.
# Mantemos as N citacoes cujos citados estao MAIS PROXIMOS na ordem de juntada
# (as mais provaveis de serem fluxo real). As demais sao omitidas do desenho —
# limitacao registrada no meta. Use Inf para nao podar. Editavel.
MAX_CITACOES_POR_NO <- Inf

# (B1) Desenhar (ou nao) a espinha CRONOLOGICA de fallback. Quando FALSE, os
# atos sem ligacao semantica (citacao/prazo/endereçamento/triagem) ficam SEM
# seta cinza — o grafo mostra so o esqueleto semantico reconstruido, o que e
# mais limpo e mais honesto (nao finge fluxo onde nao ha sinal). Editavel.
# TRUE (padrao): costura os atos orfaos pela ordem (evita nos soltos no grafo).
DESENHAR_CRONOLOGICA <- TRUE

# Config (DADO, nao hardcode) com as unidades de triagem — FLUXOS sec. 9.
UNIDADES_TRIAGEM_CONFIG <- "config/unidades_triagem.json"

#' Le a lista de unidades de triagem do config (FLUXOS sec. 9). Fallback para a
#' lista minima observada se o arquivo faltar (registrado como aviso). Retorna
#' vetor de siglas NORMALIZADAS (maiusculas, sem espacos ao redor).
.carregar_unidades_triagem <- function(caminho = UNIDADES_TRIAGEM_CONFIG) {
  padrao <- c("CGM", "CAIP-CGM", "CAIP")
  us <- tryCatch({
    if (file.exists(caminho)) {
      j <- jsonlite::fromJSON(caminho, simplifyVector = TRUE)
      if (!is.null(j$unidades_triagem)) as.character(j$unidades_triagem) else padrao
    } else {
      warning("[grafo] ", caminho, " ausente — usando lista padrao de triagem.")
      padrao
    }
  }, error = function(e) padrao)
  toupper(trimws(us))
}

# -----------------------------------------------------------------------------
# Utilitarios
# -----------------------------------------------------------------------------

.carregar_fonte_grafo <- function(x, rotulo, obrigatorio = FALSE) {
  if (is.null(x)) {
    if (obrigatorio) stop("[grafo] Fonte obrigatoria ausente: ", rotulo)
    return(NULL)
  }
  if (is.character(x)) {
    if (!file.exists(x)) {
      if (obrigatorio) stop("[grafo] JSON nao encontrado (", rotulo, "): ", x)
      return(NULL)
    }
    return(jsonlite::fromJSON(x, simplifyVector = TRUE))
  }
  x
}

.data_iso <- function(ts) if (is.na(ts) || ts == "") NA_character_ else substr(ts, 1, 10)

.vazio <- function(x) is.null(x) || is.na(x) || x == ""

#' Formata data ISO (AAAA-MM-DD) como DD/MM (curto) ou "" se ausente.
.dm <- function(data_iso) {
  if (.vazio(data_iso)) return("")
  format(as.Date(data_iso), "%d/%m")
}

#' Quebra um texto em varias linhas com no maximo `largura` caracteres por
#' linha (por palavras), para os rotulos nao estourarem a caixa do no/aresta.
.wrap <- function(txt, largura = 42L) {
  if (.vazio(txt)) return(txt)
  paste(strwrap(txt, width = largura), collapse = "\n")
}

#' Remove o "rabo" de ENDERECO que as vezes vaza para o nome do destinatario
#' (ex.: "MARIA ... Rua Setenta e Nove ... CEP:53.421-321 - Paulista/PE"). Corta
#' no primeiro marcador de endereco. E defensivo (a extracao no 04 deveria ter
#' separado); aqui evitamos que o rotulo do no fique gigante.
.limpar_nome <- function(nome, max_nome = 46L) {
  if (.vazio(nome)) return(nome)
  marcadores <- "\\b(Rua|Avenida|Av\\.|Travessa|Rodovia|Pra\u00e7a|Alameda|Estrada|Quadra|Bloco|Edif\u00edcio|CEP|SEPS|SBN|SCS)\\b"
  corte <- regexpr(marcadores, nome, perl = TRUE)
  if (corte[1] > 1) nome <- trimws(substr(nome, 1, corte[1] - 1L))
  if (nchar(nome) > max_nome) nome <- paste0(substr(nome, 1, max_nome - 1L), "\u2026")
  nome
}

#' Frase curta de nome + cargo: "Fulano (Cargo)" — ou so o nome, ou "".
#' O cargo longo e truncado para manter o rotulo legivel; o nome tem o rabo de
#' endereco removido.
.nome_cargo <- function(nome, cargo, max_cargo = 48L) {
  nome <- .limpar_nome(nome)
  if (.vazio(nome)) return("")
  if (.vazio(cargo)) return(nome)
  if (nchar(cargo) > max_cargo) cargo <- paste0(substr(cargo, 1, max_cargo - 1L), "\u2026")
  paste0(nome, " (", cargo, ")")
}

#' Frase de situacao de prazo para o rotulo (parecer): "no prazo" ou
#' "fora do prazo (+Nd)". Vazia para atos sem prazo apurado.
.frase_prazo <- function(status, excesso) {
  if (.vazio(status)) return("")
  if (status == "dentro") return("no prazo")
  if (status == "estourado") {
    ex <- suppressWarnings(as.integer(excesso))
    if (!is.na(ex) && ex > 0) return(paste0("fora do prazo (+", ex, "d)"))
    return("fora do prazo")
  }
  if (status == "em_curso") return("prazo em curso")
  ""
}

#' Rotulo interno do no. Empilha, quando existirem:
#'   Tipo / Unidade / Data / Remetente (Cargo) / "Para: Destinatario (Cargo)" /
#'   situacao de prazo / marca de restrito.
.rotulo_no <- function(tipo, unidade, data, remetente, remetente_cargo,
                       destinatario, destinatario_cargo, acesso,
                       status_prazo, excesso_prazo) {
  rem  <- .nome_cargo(remetente, remetente_cargo)
  dest <- .nome_cargo(destinatario, destinatario_cargo)
  prazo <- .frase_prazo(status_prazo, excesso_prazo)
  restrito <- if (!.vazio(acesso) && acesso == "restrito") "[restrito]" else ""

  partes <- c(
    tipo,
    unidade,
    if (!.vazio(data)) format(as.Date(data), "%d/%m/%Y") else NA,
    .wrap(rem),
    if (dest != "") .wrap(paste0("Para: ", dest)) else NA,
    if (prazo != "") prazo else NA,
    if (restrito != "") restrito else NA
  )
  partes <- partes[!is.na(partes) & partes != ""]
  paste(partes, collapse = "\n")
}

# -----------------------------------------------------------------------------
# Nos: um por documento (evento de documento do event log)
# -----------------------------------------------------------------------------

#' Monta os nos do grafo a partir dos eventos de documento e dos prazos.
#'
#' @param log tibble event_log$event_log (eventos)
#' @param prazos tibble de relogios (09) ou NULL
#' @return tibble de nos com id, rotulo, atributos e situacao
.montar_nos_grafo <- function(log, prazos) {
  docs <- log[!is.na(log$fonte) & log$fonte == "documento", , drop = FALSE]
  if (nrow(docs) == 0) return(tibble())

  pega <- function(nm, default = NA_character_)
    if (nm %in% names(docs)) docs[[nm]] else rep(default, nrow(docs))

  documento <- as.character(pega("documento"))
  data_iso  <- vapply(pega("timestamp"), .data_iso, character(1))
  unidade   <- as.character(pega("unidade"))
  classe    <- as.character(pega("classe"))
  acesso    <- as.character(pega("acesso"))

  # Situacao de prazo por documento do PARECER (do 09): dentro/estourado + o
  # excesso (para o rotulo "fora do prazo (+Nd)").
  sit_prazo    <- setNames(rep(NA_character_, nrow(docs)), documento)
  excesso_prazo <- setNames(rep(NA_integer_, nrow(docs)), documento)
  if (!is.null(prazos) && is.data.frame(prazos) && nrow(prazos) > 0 &&
      all(c("documento_parecer", "status") %in% names(prazos))) {
    tem_excesso <- "excesso_dias" %in% names(prazos)
    for (i in seq_len(nrow(prazos))) {
      dp <- as.character(prazos$documento_parecer[i])
      if (!is.na(dp) && dp %in% names(sit_prazo)) {
        sit_prazo[dp] <- prazos$status[i]
        if (tem_excesso) excesso_prazo[dp] <- suppressWarnings(as.integer(prazos$excesso_dias[i]))
      }
    }
  }

  situacao <- vapply(seq_len(nrow(docs)), function(i) {
    sp <- sit_prazo[[documento[i]]]
    if (!is.na(sp) && sp == "dentro") return("dentro_prazo")
    if (!is.na(sp) && sp == "estourado") return("fora_prazo")
    cl <- classe[i]
    if (!is.na(cl) && cl == "exigencia_complementacao") return("complementacao")
    if (is.na(cl) || cl == "") return("avulso")
    "ato"
  }, character(1))

  cor <- c(dentro_prazo = COR_DENTRO, fora_prazo = COR_FORA,
           complementacao = COR_COMPLEMENTO, avulso = COR_AVULSO,
           ato = COR_ATO)[situacao]

  remetente          <- as.character(pega("remetente"))
  remetente_cargo    <- as.character(pega("remetente_cargo"))
  signatarios        <- as.character(pega("signatarios"))
  referencias        <- as.character(pega("referencias"))
  destinatario       <- as.character(pega("destinatario"))
  destinatario_cargo <- as.character(pega("destinatario_cargo"))

  tibble(
    id        = documento,
    rotulo    = vapply(seq_len(nrow(docs)), function(i)
                   .rotulo_no(pega("tipo")[i], unidade[i], data_iso[i],
                              remetente[i], remetente_cargo[i],
                              destinatario[i], destinatario_cargo[i], acesso[i],
                              sit_prazo[[documento[i]]], excesso_prazo[[documento[i]]]),
                   character(1)),
    tipo      = as.character(pega("tipo")),
    classe    = classe,
    unidade   = unidade,
    acesso    = acesso,
    remetente = remetente,
    remetente_cargo = remetente_cargo,
    signatarios = signatarios,
    referencias = referencias,
    destinatario = destinatario,
    destinatario_cargo = destinatario_cargo,
    data      = data_iso,
    ordem     = if ("ordem" %in% names(docs)) as.integer(docs$ordem) else seq_len(nrow(docs)),
    situacao  = situacao,
    cor       = unname(cor)
  ) |> arrange(ordem)
}

# -----------------------------------------------------------------------------
# Demanda externa: agrupa a documentacao do interessado num unico no
# -----------------------------------------------------------------------------

#' Agrupa a DOCUMENTACAO EXTERNA (peticionamento do interessado) em nos de
#' "demanda externa" — a peca que o interessado protocola e que faz a unidade
#' de triagem (CGM) abrir a analise. Inspirado em agrupar_protocolos_externos()
#' do especialista, porem usando os campos que o nosso event log ja tem.
#'
#' Criterio (sem inventar): sao externos os documentos SEM classe semantica E
#' de PETICIONAMENTO — i.e., cuja unidade e a de protocolo do interessado (SAIP)
#' ou ausente. Isso separa a peca do interessado (ART, ADA, estudos, Recibo,
#' Requerimento) de atos do IPHAN que apenas nao foram classificados (E-mail,
#' Portaria com unidade da casa), que NAO viram demanda externa. Uma sequencia
#' comeca num Recibo (ou no 1o externo apos um ato) e segue enquanto os docs
#' continuarem externos e na mesma unidade. Cada sequencia vira UM no.
#'
#' @param nos_todos tibble de .montar_nos_grafo() (um por documento, ja ordenado)
#' @return list(nos = tibble de demandas externas, membros = ids agrupados)
.agrupar_demanda_externa <- function(nos_todos) {
  vazio <- list(nos = tibble(), membros = character(0))
  if (nrow(nos_todos) == 0) return(vazio)

  n <- nrow(nos_todos)
  sem_classe <- is.na(nos_todos$classe) | nos_todos$classe == ""
  # Unidade de peticionamento: SAIP (fase do interessado) ou ausente. Atos do
  # IPHAN sem classe (E-mail/Portaria em unidade da casa) NAO sao externos.
  un <- toupper(ifelse(is.na(nos_todos$unidade), "", nos_todos$unidade))
  de_peticionamento <- un == "" | startsWith(un, "SAIP")
  eh_externo <- sem_classe & de_peticionamento
  if (!any(eh_externo)) return(vazio)

  # Varre em ordem, abrindo um grupo no Recibo (ou no 1o externo apos um ato) e
  # fechando quando aparece um ato do IPHAN ou um doc nao-externo.
  grupo <- rep(NA_integer_, n)
  g <- 0L; unidade_g <- NA_character_; aberto <- FALSE
  tipo_norm <- function(t) if (.vazio(t)) "" else t
  for (i in seq_len(n)) {
    if (!eh_externo[i]) { aberto <- FALSE; next }  # ato/nao-externo encerra
    abre_recibo <- any(startsWith(tipo_norm(nos_todos$tipo[i]), TIPOS_ABERTURA_EXTERNA))
    muda_unidade <- aberto && !identical(nos_todos$unidade[i], unidade_g)
    if (!aberto || abre_recibo || muda_unidade) {
      g <- g + 1L; unidade_g <- nos_todos$unidade[i]; aberto <- TRUE
    }
    grupo[i] <- g
  }
  if (all(is.na(grupo))) return(vazio)

  ids_por_grupo <- split(nos_todos$id[!is.na(grupo)], grupo[!is.na(grupo)])
  linhas <- lapply(names(ids_por_grupo), function(gid) {
    membros <- nos_todos[nos_todos$id %in% ids_por_grupo[[gid]], , drop = FALSE]
    membros <- membros[order(membros$ordem), , drop = FALSE]
    unidade_ext <- membros$unidade[!is.na(membros$unidade)][1]
    if (is.na(unidade_ext)) unidade_ext <- "SAIP"
    # Um documento POR LINHA (nome + numero). Exibe TODOS os documentos da
    # demanda (nao resume): cada item quebrado individualmente (wrap por item),
    # preservando uma linha por documento.
    itens <- paste0(membros$tipo, " (", membros$id, ")")
    itens <- vapply(itens, function(x) .wrap(x, 40), character(1))
    detalhe <- paste(itens, collapse = "\n")
    rot <- paste(c(paste0("Demanda externa (", nrow(membros), " docs)"),
                   unidade_ext,
                   if (!.vazio(membros$data[1])) format(as.Date(membros$data[1]), "%d/%m/%Y") else NA,
                   detalhe),
                 collapse = "\n")
    rot <- paste(rot[!is.na(rot) & rot != ""], collapse = "\n")
    tibble(
      id        = paste0("EXT_", gid),
      rotulo    = rot,
      tipo      = "Demanda externa",
      classe    = "demanda_externa",
      unidade   = unidade_ext,
      acesso    = NA_character_,
      remetente = NA_character_, remetente_cargo = NA_character_,
      signatarios = NA_character_,
      referencias = NA_character_,
      destinatario = NA_character_, destinatario_cargo = NA_character_,
      data      = membros$data[1],
      ordem     = min(membros$ordem),
      situacao  = "demanda_externa",
      cor       = COR_DEMANDA_EXT
    )
  })
  list(nos = bind_rows(linhas), membros = nos_todos$id[!is.na(grupo)])
}

#' Liga cada no de DEMANDA EXTERNA ao primeiro ATO do IPHAN posterior a ela (na
#' ordem do processo) — tipicamente o despacho da triagem que abre a analise.
#'
#' @param nos tibble de nos mantidos no grafo (atos + demandas externas)
#' @param nos_dem tibble dos nos de demanda externa
#' @return tibble(de, para, tipo, rotulo)
.arestas_demanda_externa <- function(nos, nos_dem) {
  vazio <- tibble(de = character(0), para = character(0),
                  tipo = character(0), rotulo = character(0))
  if (nrow(nos_dem) == 0) return(vazio)
  atos <- nos[nos$classe != "demanda_externa" &
                !is.na(nos$classe) & nos$classe != "", , drop = FALSE]
  if (nrow(atos) == 0) return(vazio)

  linhas <- lapply(seq_len(nrow(nos_dem)), function(i) {
    d <- nos_dem[i, ]
    posteriores <- atos[atos$ordem > d$ordem, , drop = FALSE]
    alvo <- if (nrow(posteriores) > 0) posteriores[which.min(posteriores$ordem), ] else NULL
    if (is.null(alvo)) return(NULL)
    tibble(de = d$id, para = alvo$id, tipo = "demanda", rotulo = NA_character_)
  })
  linhas <- linhas[!vapply(linhas, is.null, logical(1))]
  if (length(linhas) == 0) return(vazio)
  bind_rows(linhas)
}

# -----------------------------------------------------------------------------
# Arestas de ENDERECAMENTO: destinatario de A assina o ato B (ramificacao real)
# -----------------------------------------------------------------------------

#' Normaliza um nome para casamento: minusculas, sem acento, so letras/espacos.
.norm_nome <- function(x) {
  if (.vazio(x)) return("")
  x <- iconv(x, to = "ASCII//TRANSLIT")
  if (is.na(x)) return("")
  x <- tolower(gsub("[^a-z ]", " ", tolower(x)))
  trimws(gsub("\\s+", " ", x))
}

#' Duas formas do mesmo nome? Basta o PRIMEIRO e o ULTIMO nome (>2 letras)
#' coincidirem — cobre "ERIC LEMOS" vs "Eric Lemos Pereira Faustino". Tambem
#' aceita quando um e substring do outro. (Regra portada do especialista.)
.nome_casa <- function(a, b) {
  a <- .norm_nome(a); b <- .norm_nome(b)
  if (a == "" || b == "") return(FALSE)
  if (grepl(a, b, fixed = TRUE) || grepl(b, a, fixed = TRUE)) return(TRUE)
  ta <- strsplit(a, " ")[[1]]; tb <- strsplit(b, " ")[[1]]
  ta <- ta[nchar(ta) > 2]; tb <- tb[nchar(tb) > 2]
  if (length(ta) < 2 || length(tb) < 2) return(FALSE)
  ta[1] %in% tb && ta[length(ta)] %in% tb
}

#' Arestas de ENDERECAMENTO: para cada ato A com destinatario, liga A -> B, onde
#' B e o proximo ATO (na ordem do processo) cujo remetente/signatario casa com o
#' destinatario de A. Isso reconstrui a ramificacao real (quem foi endereçado
#' respondeu). Um mesmo A pode ter varios destinatarios -> varias saidas (galho).
#'
#' Nao liga a pares JA ligados por prazo (o prazo tem precedencia semantica).
#'
#' @param nos tibble de nos (atos + demandas), com destinatario e remetente
#' @param pares_prazo character "de->para" ja ligados por prazo
#' @return tibble(de, para, tipo, rotulo)
.arestas_enderecamento <- function(nos, pares_prazo = character(0)) {
  vazio <- tibble(de = character(0), para = character(0),
                  tipo = character(0), rotulo = character(0))
  atos <- nos[!is.na(nos$classe) & nos$classe != "" &
                nos$classe != "demanda_externa", , drop = FALSE]
  if (nrow(atos) < 2) return(vazio)

  # ordena os atos por ordem e trabalha por POSICAO na lista de atos (nao pela
  # ordem bruta, que inclui avulsos): a janela de proximidade e contada em atos.
  atos <- atos[order(atos$ordem), , drop = FALSE]
  com_dest <- which(!vapply(atos$destinatario, .vazio, logical(1)))
  if (length(com_dest) == 0) return(vazio)

  # Lista de signatarios de cada ato: usa a coluna 'signatarios' (todos, sep. por
  # ";") quando existir; senao cai para o remetente (1o signatario). O endereçado
  # costuma ser o 2o signatario (coordenador), por isso comparamos com TODOS.
  tem_sig <- "signatarios" %in% names(atos)
  signatarios_de <- function(j) {
    s <- if (tem_sig) atos$signatarios[j] else NA_character_
    if (.vazio(s)) s <- atos$remetente[j]
    if (.vazio(s)) return(character(0))
    trimws(strsplit(s, ";")[[1]])
  }

  linhas <- list()
  for (i in com_dest) {
    dest <- atos$destinatario[i]
    # candidatos: atos POSTERIORES (por posicao) dentro da janela, em que ALGUM
    # signatario casa com o destinatario. A janela evita saltos longos e
    # espurios (ex.: despacho de marco casar com parecer de agosto pelo nome).
    posteriores <- (i + 1):nrow(atos)
    posteriores <- posteriores[posteriores <= i + JANELA_ENDERECAMENTO_ATOS]
    posteriores <- posteriores[posteriores <= nrow(atos)]
    casa <- posteriores[vapply(posteriores, function(j) {
      sigs <- signatarios_de(j)
      length(sigs) > 0 && any(vapply(sigs, function(s) .nome_casa(dest, s), logical(1)))
    }, logical(1))]
    if (length(casa) == 0) next
    # o mais proximo (menor posicao) responde ao endereçamento
    j <- casa[which.min(casa)]
    de <- atos$id[i]; para <- atos$id[j]
    if (paste0(de, "->", para) %in% pares_prazo) next  # prazo tem precedencia
    linhas[[length(linhas) + 1]] <- tibble(
      de = de, para = para, tipo = "enderecamento",
      rotulo = paste0("Para: ", dest))
  }
  if (length(linhas) == 0) return(vazio)
  # dedup: um mesmo par so uma vez
  ar <- bind_rows(linhas)
  ar[!duplicated(paste0(ar$de, "->", ar$para)), , drop = FALSE]
}

# -----------------------------------------------------------------------------
# Arestas de CITACAO: um documento cita outro pelo numero SEI (citado -> citante)
# -----------------------------------------------------------------------------

#' Arestas de CITACAO por numero SEI. Para cada no N que cita numeros (coluna
#' `referencias`, separada por "; "), liga o no CITADO -> N (citante), desde que
#' o citado seja um NO DE ATO presente no grafo. E a ligacao de MAIOR precedencia
#' semantica (aproxima a "arvore" do especialista, cujas setas azuis sao citacoes).
#'
#' Direcao: citado -> citante (ex.: especialista tem "D7519156 --> D7535801",
#' i.e. o doc 7519156, citado, aponta para 7535801, que o cita).
#'
#' Ciclos (A cita B e B cita A): mantem-se o elo que parte do doc de MAIOR ordem
#' (o mais novo) para o de menor (o mais antigo), descartando a inversa — o
#' criterio do especialista (o elo parte do ato mais recente).
#'
#' Limitacao: citacao a um documento agrupado na DEMANDA EXTERNA (id "EXT_n",
#' sem no individual) nao encontra alvo e NAO gera aresta (registrado no meta).
#'
#' @param nos tibble de nos (atos + demandas), com coluna `referencias`
#' @return tibble(de, para, tipo, rotulo)
.arestas_citacao <- function(nos) {
  vazio <- tibble(de = character(0), para = character(0),
                  tipo = character(0), rotulo = character(0))
  if (nrow(nos) == 0 || !("referencias" %in% names(nos))) return(vazio)

  # So atos (com classe, exceto demanda externa) podem ser citante ou citado —
  # a demanda externa nao tem no individual nem referencias.
  eh_ato <- !is.na(nos$classe) & nos$classe != "" & nos$classe != "demanda_externa"
  ids_ato <- nos$id[eh_ato]
  ordem_por_id <- setNames(nos$ordem, nos$id)
  # destinatario por id: quando a citacao coincide com um endereçamento, o PDF
  # manda PRESERVAR o "Para: <destinatario>" como rotulo da seta azul (uma unica
  # seta carrega as duas informacoes; nada e descartado). O destinatario e o do
  # documento CITADO (o despacho que enderecou alguem e depois foi citado).
  dest_por_id <- setNames(nos$destinatario, nos$id)

  linhas <- list()
  for (i in which(eh_ato)) {
    refs <- nos$referencias[i]
    if (.vazio(refs)) next
    citados <- trimws(strsplit(refs, ";")[[1]])
    citados <- unique(citados[citados != "" & citados %in% ids_ato & citados != nos$id[i]])
    if (length(citados) == 0) next
    # PODA (A): quando o no cita muitos documentos, mantem so as N citacoes cujos
    # citados estao MAIS PROXIMOS na ordem de juntada (as mais provaveis de serem
    # fluxo real; as demais sao "objeto de analise" e poluem o grafo). As
    # omitidas sao contabilizadas no meta (limitacao).
    if (is.finite(MAX_CITACOES_POR_NO) && length(citados) > MAX_CITACOES_POR_NO) {
      ord_i <- suppressWarnings(as.numeric(ordem_por_id[[nos$id[i]]]))
      dist  <- vapply(citados, function(c)
        abs(suppressWarnings(as.numeric(ordem_por_id[[c]])) - ord_i), numeric(1))
      citados <- citados[order(dist)][seq_len(MAX_CITACOES_POR_NO)]
    }
    for (cit in citados) {
      dest <- dest_por_id[[cit]]
      rot  <- if (!.vazio(dest)) paste0("Para: ", dest) else NA_character_
      linhas[[length(linhas) + 1]] <- tibble(
        de = cit, para = nos$id[i], tipo = "citacao", rotulo = rot)
    }
  }
  if (length(linhas) == 0) return(vazio)
  ar <- bind_rows(linhas)
  ar <- ar[!duplicated(paste0(ar$de, "->", ar$para)), , drop = FALSE]

  # Desfaz ciclos de 2 arestas (A<->B): mantem a que parte do no de MAIOR ordem.
  chave <- paste0(ar$de, "->", ar$para)
  inversa <- paste0(ar$para, "->", ar$de)
  manter <- rep(TRUE, nrow(ar))
  for (k in seq_len(nrow(ar))) {
    if (!manter[k]) next
    j <- which(chave == inversa[k])
    if (length(j) == 1 && manter[j]) {
      # dois elos reciprocos: fica o que sai do doc mais novo (maior ordem)
      od <- suppressWarnings(as.numeric(ordem_por_id[[ar$de[k]]]))
      op <- suppressWarnings(as.numeric(ordem_por_id[[ar$para[k]]]))
      if (is.na(od)) od <- -Inf; if (is.na(op)) op <- -Inf
      if (od >= op) manter[j] <- FALSE else manter[k] <- FALSE
    }
  }
  ar[manter, , drop = FALSE]
}

# -----------------------------------------------------------------------------
# Arestas de DEMANDA DA TRIAGEM (criterio 4): despacho da CGM -> parecer que atende
# -----------------------------------------------------------------------------

#' Verifica se um no e um DESPACHO DE TRIAGEM (a CGM/CAIP abrindo a demanda):
#' ato com classe de encaminhamento/solicitacao, tipo iniciando por "Despacho",
#' emitido por unidade de triagem (config, FLUXOS sec. 9).
.eh_despacho_triagem <- function(classe, tipo, unidade, unidades_triagem) {
  if (.vazio(classe) || !(classe %in% CLASSES_DEMANDA_TRIAGEM)) return(FALSE)
  if (.vazio(tipo) || !startsWith(tipo, PREFIXO_DESPACHO_TRIAGEM)) return(FALSE)
  if (.vazio(unidade)) return(FALSE)
  u <- toupper(trimws(unidade))
  # casa por igualdade OU por prefixo (ex.: "CAIP-CGM" cobre "CGM"/"CAIP")
  any(vapply(unidades_triagem, function(t)
    u == t || startsWith(u, t) || startsWith(t, u), logical(1)))
}

#' Arestas de DEMANDA DA TRIAGEM (criterio 4 do especialista; FLUXOS sec. 2).
#' O despacho da unidade de triagem declara o que sera analisado; liga-se ao
#' PRIMEIRO parecer posterior (na ordem da arvore de juntada, nao por data) que
#' o atende, dentro de uma janela de atos. E aresta de FLUXO, nao de prazo.
#'
#' Precedencia: entra ABAIXO de citacao/prazo/endereçamento — nao liga a pares
#' ja ligados por eles. Um parecer atende no maximo um despacho de triagem.
#'
#' @param nos tibble de nos (atos + demandas), ordenado por `ordem`
#' @param ja_ligados character "de->para" ja ligados por regras de maior preced.
#' @return tibble(de, para, tipo, rotulo)
.arestas_demanda_triagem <- function(nos, ja_ligados = character(0)) {
  vazio <- tibble(de = character(0), para = character(0),
                  tipo = character(0), rotulo = character(0))
  atos <- nos[!is.na(nos$classe) & nos$classe != "" &
                nos$classe != "demanda_externa", , drop = FALSE]
  if (nrow(atos) < 2) return(vazio)
  atos <- atos[order(atos$ordem), , drop = FALSE]

  unidades_triagem <- .carregar_unidades_triagem()

  eh_triagem <- vapply(seq_len(nrow(atos)), function(i)
    .eh_despacho_triagem(atos$classe[i], atos$tipo[i], atos$unidade[i],
                         unidades_triagem), logical(1))
  eh_parecer <- !is.na(atos$classe) & atos$classe %in% CLASSES_PARECER_TRIAGEM

  parecer_consumido <- rep(FALSE, nrow(atos))
  linhas <- list()
  for (i in which(eh_triagem)) {
    # candidatos: pareceres POSTERIORES (por posicao) dentro da janela, ainda
    # nao consumidos por outra triagem.
    janela <- (i + 1):min(nrow(atos), i + JANELA_DEMANDA_TRIAGEM_ATOS)
    janela <- janela[janela >= 1 & janela <= nrow(atos)]
    cand <- janela[eh_parecer[janela] & !parecer_consumido[janela]]
    if (length(cand) == 0) next
    j <- cand[which.min(cand)]  # o 1o parecer que atende
    de <- atos$id[i]; para <- atos$id[j]
    if (paste0(de, "->", para) %in% ja_ligados) { parecer_consumido[j] <- TRUE; next }
    parecer_consumido[j] <- TRUE
    # rotulo: destinatario do despacho (a quem a triagem enderecou), se houver
    rot <- if (!.vazio(atos$destinatario[i])) paste0("Para: ", atos$destinatario[i]) else NA_character_
    linhas[[length(linhas) + 1]] <- tibble(
      de = de, para = para, tipo = "demanda_triagem", rotulo = rot)
  }
  if (length(linhas) == 0) return(vazio)
  ar <- bind_rows(linhas)
  ar[!duplicated(paste0(ar$de, "->", ar$para)), , drop = FALSE]
}

# -----------------------------------------------------------------------------
# Arestas: prazo (semantico) + endereçamento + espinha cronologica (fallback)
# -----------------------------------------------------------------------------

#' Monta as arestas do grafo de fluxo, em ordem de PRECEDENCIA:
#'
#' (a) CITACAO: citado -> citante (numero SEI no corpo). MAIOR precedencia.
#' (b) PRAZO: documento_abertura -> documento_parecer (do 09). Semantico.
#' (c) ENDERECAMENTO: destinatario de A assina o ato B (ramificacao real).
#' (d) DEMANDA DA TRIAGEM: despacho da CGM/CAIP -> parecer que atende (FLUXOS
#'     sec. 2). Aresta de FLUXO; complementa o prazo (que so existe quando o
#'     despacho fixou data-limite). Nao duplica pares de (a)/(b)/(c).
#' (e) Espinha CRONOLOGICA (FALLBACK): liga ao ato anterior APENAS os atos que
#'     ficaram sem nenhuma entrada real (sem citacao/prazo/endereçamento/triagem/
#'     demanda). Assim o grafo ramifica em arvore em vez de virar uma linha reta;
#'     a cronologica so costura o que sobrou solto.
#'
#' @param nos tibble de .montar_nos_grafo() (ja ordenado por `ordem`)
#' @param prazos tibble de relogios (09) ou NULL
#' @return tibble(de, para, tipo, rotulo)
.montar_arestas_grafo <- function(nos, prazos) {
  if (nrow(nos) == 0) return(tibble(de = character(0), para = character(0),
                                    tipo = character(0), rotulo = character(0)))

  # Mapas por id de documento (do no de ORIGEM da aresta).
  dest_de    <- setNames(nos$destinatario, nos$id)
  destcgo_de <- setNames(nos$destinatario_cargo, nos$id)
  unid_de    <- setNames(nos$unidade, nos$id)

  # "Para: Fulano" quando o documento de origem tem destinatario. Na ARESTA
  # mostramos so o nome (o cargo ja aparece no rotulo do NO de origem), para o
  # texto da seta ficar curto e legivel.
  rotulo_destinatario <- function(id_de) {
    d <- dest_de[[as.character(id_de)]]
    if (.vazio(d)) return(NA_character_)
    paste0("Para: ", d)
  }
  # Fluxo entre unidades: "Unid.origem -> Unid.destino" (fallback da cronologica).
  rotulo_unidades <- function(id_de, id_para) {
    uo <- unid_de[[as.character(id_de)]]
    ud <- unid_de[[as.character(id_para)]]
    if (.vazio(uo) && .vazio(ud)) return(NA_character_)
    if (.vazio(ud) || identical(uo, ud)) return(if (.vazio(uo)) NA_character_ else uo)
    paste0(uo, " \u2192 ", ud)
  }

  arestas <- list()
  pares_prazo <- character(0)     # "de->para" ja ligados por prazo
  pares_citacao <- character(0)   # "de->para" ja ligados por citacao

  # (a) arestas de CITACAO (citado -> citante): MAIOR precedencia semantica.
  ar_cit <- .arestas_citacao(nos)
  if (nrow(ar_cit) > 0) {
    for (i in seq_len(nrow(ar_cit))) arestas[[length(arestas) + 1]] <- ar_cit[i, ]
    pares_citacao <- paste0(ar_cit$de, "->", ar_cit$para)
  }

  # (b) arestas de PRAZO (abertura -> parecer): rotulo = destinatario + limite +
  # situacao. Ex.: "Para: Eric Lemos (Coordenador) | limite 23/05 · no prazo".
  tem_col <- function(nm) nm %in% names(prazos)
  if (!is.null(prazos) && is.data.frame(prazos) && nrow(prazos) > 0 &&
      all(c("documento_abertura", "documento_parecer") %in% names(prazos))) {
    for (i in seq_len(nrow(prazos))) {
      de   <- as.character(prazos$documento_abertura[i])
      para <- as.character(prazos$documento_parecer[i])
      if (is.na(de) || is.na(para) || !(de %in% nos$id) || !(para %in% nos$id)) next
      if (paste0(de, "->", para) %in% pares_citacao) next  # citacao tem precedencia

      limite  <- if (tem_col("data_limite")) .dm(prazos$data_limite[i]) else ""
      status  <- if (tem_col("status")) prazos$status[i] else NA_character_
      excesso <- if (tem_col("excesso_dias")) prazos$excesso_dias[i] else NA_integer_
      compl   <- tem_col("e_complementacao") && isTRUE(prazos$e_complementacao[i])

      partes <- c(
        rotulo_destinatario(de),
        {
          base <- if (limite != "") paste0("limite ", limite) else ""
          fp <- .frase_prazo(status, excesso)
          txt <- paste(c(base, fp)[c(base, fp) != ""], collapse = " \u00b7 ")
          if (compl && txt != "") paste0(txt, " (complementacao)")
          else if (compl) "complementacao" else if (txt != "") txt else NA_character_
        }
      )
      partes <- partes[!is.na(partes)]
      rot <- if (length(partes) > 0) paste(partes, collapse = "\n") else NA_character_

      arestas[[length(arestas) + 1]] <- tibble(
        de = de, para = para, tipo = "prazo", rotulo = rot)
      pares_prazo <- c(pares_prazo, paste0(de, "->", para))
    }
  }

  # (c) ENDERECAMENTO: destinatario de A assina o ato B (ramificacao real).
  # Nao liga a pares ja ligados por citacao ou prazo (ambos tem precedencia).
  ar_end <- .arestas_enderecamento(nos, c(pares_citacao, pares_prazo))
  if (nrow(ar_end) > 0) {
    for (i in seq_len(nrow(ar_end))) arestas[[length(arestas) + 1]] <- ar_end[i, ]
  }
  pares_end <- if (nrow(ar_end) > 0) paste0(ar_end$de, "->", ar_end$para) else character(0)

  # (d) DEMANDA DA TRIAGEM (criterio 4): despacho da CGM -> parecer que atende.
  # Aresta de FLUXO (nao de prazo). Nao duplica pares de citacao/prazo/endereço.
  ar_tri <- .arestas_demanda_triagem(nos, c(pares_citacao, pares_prazo, pares_end))
  if (nrow(ar_tri) > 0) {
    for (i in seq_len(nrow(ar_tri))) arestas[[length(arestas) + 1]] <- ar_tri[i, ]
  }
  pares_tri <- if (nrow(ar_tri) > 0) paste0(ar_tri$de, "->", ar_tri$para) else character(0)

  # Conjunto de pares ja existentes (citacao + prazo + endereçamento + triagem),
  # para a espinha nao duplicar e para saber quais atos ja tem ENTRADA real.
  pares_reais <- c(pares_citacao, pares_prazo, pares_end, pares_tri)

  # (e) espinha CRONOLOGICA como FALLBACK: liga ao ato ANTERIOR apenas os atos
  # que ficaram SEM entrada real (nenhuma seta de citacao/prazo/endereçamento/
  # triagem/demanda chega neles). So costura o que sobrou solto.
  # (B1) Quando DESENHAR_CRONOLOGICA e FALSE, este bloco NAO roda: os atos sem
  # ligacao semantica ficam sem seta cinza — o grafo mostra so o esqueleto
  # semantico reconstruido (mais limpo e mais honesto).
  if (isTRUE(DESENHAR_CRONOLOGICA)) {
    eh_demanda <- !is.na(nos$classe) & nos$classe == "demanda_externa"
    ids <- nos$id[!eh_demanda]
    # destinos que ja recebem alguma seta real (prazo/endereçamento/demanda)
    destinos_reais <- character(0)
    if (length(arestas) > 0) {
      tudo <- bind_rows(arestas)
      destinos_reais <- unique(as.character(tudo$para))
    }
    if (length(ids) >= 2) {
      for (i in 2:length(ids)) {
        para <- ids[i]
        if (para %in% destinos_reais) next        # ja tem entrada real: nao costura
        de <- ids[i - 1]
        if (paste0(de, "->", para) %in% pares_reais) next
        rot <- rotulo_destinatario(de)
        if (is.na(rot)) rot <- rotulo_unidades(de, para)
        arestas[[length(arestas) + 1]] <- tibble(
          de = de, para = para, tipo = "cronologica", rotulo = rot)
      }
    }
  }

  if (length(arestas) == 0) return(tibble(de = character(0), para = character(0),
                                          tipo = character(0), rotulo = character(0)))
  bind_rows(arestas)
}

# -----------------------------------------------------------------------------
# Funcao principal: construir_grafo_processo()
# -----------------------------------------------------------------------------

#' Constroi o grafo de tramitacao (documento=no) de um processo.
#'
#' @param event_log list de construir_event_log_processo() OU caminho do JSON.
#' @param prazos list de calcular_prazos_processo() OU caminho, ou NULL.
#' @param incluir_avulsos Logico — incluir documentos avulsos (anexos/arquivos/
#'        restritos, sem classe) no grafo. Padrao FALSE: o grafo foca nos ATOS do
#'        fluxo (despachos, oficios, pareceres, decisoes), que e o que interessa
#'        para a validacao; os avulsos poluem a leitura e nao carregam fluxo.
#' @param dir_processed Diretorio base de saida do grafo-dado.
#' @param persistir Logico — salvar JSON do grafo (em subpasta de data).
#' @param salvar_plot Logico — alem do SVG, exportar tambem PNG (via rsvg).
#' @param salvar_html Logico — renderizar a figura do grafo (SVG) via Graphviz/
#'        dot (DiagrammeR): arvore estatica top-down no estilo do especialista,
#'        cores de aresta (azul=citacao, vermelho=endereçamento, cinza=resto).
#'        (Nome mantido por compat com o pipeline; nao gera mais HTML interativo.)
#' @param dir_output Diretorio base das figuras.
#' @return list com: nos, arestas, igraph, meta
construir_grafo_processo <- function(
    event_log,
    prazos          = NULL,
    incluir_avulsos = FALSE,
    dir_processed   = "data/processed",
    persistir       = TRUE,
    salvar_plot     = FALSE,
    salvar_html     = FALSE,
    dir_output      = "output"
) {
  el <- .carregar_fonte_grafo(event_log, "event_log", obrigatorio = TRUE)
  pr <- .carregar_fonte_grafo(prazos, "prazos", obrigatorio = FALSE)
  prazos_tbl <- if (!is.null(pr) && !is.null(pr$relogios)) as_tibble(pr$relogios) else NULL

  numero_processo <- el$meta$numero_processo
  if (is.null(numero_processo) || is.na(numero_processo)) {
    stop("[grafo] numero_processo ausente no event log.")
  }
  numero_seguro <- str_replace_all(numero_processo, "[^0-9A-Za-z]", "_")

  cat("[grafo] Processo:", numero_processo, "\n")

  log <- as_tibble(el$event_log)
  nos_todos <- .montar_nos_grafo(log, prazos_tbl)

  # DEMANDA EXTERNA: agrupa a documentacao do interessado (docs sem classe) em
  # nos de "demanda externa" — a peca protocolada que faz a triagem (CGM) abrir
  # a analise. Substitui a antiga ocultacao cega dos avulsos: em vez de sumir
  # com eles, resumimos cada sequencia num no e ligamos ao 1o ato do IPHAN.
  dem <- .agrupar_demanda_externa(nos_todos)
  ids_agrupados <- dem$membros
  n_demandas <- nrow(dem$nos)

  # Nos mantidos: atos (com classe) + os nos de demanda externa. Os documentos
  # externos individuais saem (viram a demanda que os resume). Com
  # incluir_avulsos = TRUE, mantem tambem os externos crus, para auditoria.
  nos_atos <- nos_todos[!(nos_todos$id %in% ids_agrupados), , drop = FALSE]
  nos <- if (incluir_avulsos) nos_todos else bind_rows(nos_atos, dem$nos)
  nos <- nos[order(nos$ordem), , drop = FALSE]
  n_avulsos <- length(ids_agrupados)

  # As arestas (inclusive a espinha cronologica) sao recalculadas sobre os nos
  # mantidos, para o fluxo permanecer conectado apos a filtragem.
  arestas <- .montar_arestas_grafo(nos, prazos_tbl)

  # Liga cada DEMANDA EXTERNA ao primeiro ATO do IPHAN que a segue no tempo
  # (tipicamente o despacho da triagem/CGM que abre a analise).
  if (n_demandas > 0 && !incluir_avulsos) {
    arestas_dem <- .arestas_demanda_externa(nos, dem$nos)
    if (nrow(arestas_dem) > 0) arestas <- bind_rows(arestas, arestas_dem)
  }

  # igraph dirigido
  g <- igraph::graph_from_data_frame(
    d = as.data.frame(arestas[, c("de", "para", "tipo", "rotulo")]),
    directed = TRUE,
    vertices = as.data.frame(nos)
  )

  n_nos    <- nrow(nos)
  n_arestas <- nrow(arestas)
  distrib  <- table(factor(nos$situacao, levels = SITUACOES))

  cat("[grafo] --- Grafo de tramitacao ---\n")
  cat("[grafo] Documentos (nos)  :", n_nos,
      if (!incluir_avulsos) paste0("(", n_demandas, " demanda(s) externa(s) resumindo ",
                                   n_avulsos, " docs)") else "", "\n")
  cat("[grafo] Ligacoes (arestas):", n_arestas,
      "(citacao:", sum(arestas$tipo == "citacao"),
      "| prazo:", sum(arestas$tipo == "prazo"),
      "| enderecamento:", sum(arestas$tipo == "enderecamento"),
      "| demanda_triagem:", sum(arestas$tipo == "demanda_triagem"),
      "| demanda:", sum(arestas$tipo == "demanda"),
      "| cronologica:", sum(arestas$tipo == "cronologica"), ")\n")
  cat("[grafo] Situacao dos nos  : dentro:", distrib[["dentro_prazo"]],
      "| fora:", distrib[["fora_prazo"]],
      "| complementacao:", distrib[["complementacao"]],
      "| demanda externa:", distrib[["demanda_externa"]],
      "| avulso:", distrib[["avulso"]],
      "| ato:", distrib[["ato"]], "\n")

  saida <- list(
    meta = list(
      numero_processo = numero_processo,
      timestamp       = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      tipo_grafo      = "tramitacao",
      documentos      = n_nos,
      avulsos_ocultados = if (incluir_avulsos) 0L else as.integer(n_avulsos),
      arestas         = n_arestas,
      arestas_citacao = sum(arestas$tipo == "citacao"),
      arestas_prazo   = sum(arestas$tipo == "prazo"),
      arestas_enderecamento = sum(arestas$tipo == "enderecamento"),
      arestas_demanda_triagem = sum(arestas$tipo == "demanda_triagem"),
      arestas_cronologicas = sum(arestas$tipo == "cronologica"),
      situacao = list(
        dentro_prazo   = as.integer(distrib[["dentro_prazo"]]),
        fora_prazo     = as.integer(distrib[["fora_prazo"]]),
        complementacao = as.integer(distrib[["complementacao"]]),
        demanda_externa = as.integer(distrib[["demanda_externa"]]),
        avulso         = as.integer(distrib[["avulso"]]),
        ato            = as.integer(distrib[["ato"]])
      ),
      demandas_externas = n_demandas,
      docs_externos_agrupados = as.integer(n_avulsos),
      arestas_demanda = sum(arestas$tipo == "demanda"),
      limitacoes = c(
        "Arestas de citacao (citado->parecer/citante) so existem em documentos com TEXTO (nativos HTML); restritos/sem texto nao citam.",
        "Citacao a documento agrupado na demanda externa (id EXT_n, sem no individual) nao gera aresta (sem alvo).",
        "Citacao usa apenas dois padroes conservadores (numero entre parenteses e 'SEI n\u00ba'); o padrao de anexo de e-mail (_NNNNNNN.pdf) nao e usado.",
        paste0("Simplificacao visual (A): as citacoes desenhadas por no sao limitadas a ",
               MAX_CITACOES_POR_NO, " (as mais proximas na ordem de juntada); citacoes adicionais (tipicamente 'objeto de analise') sao omitidas do desenho para legibilidade."),
        if (!isTRUE(DESENHAR_CRONOLOGICA))
          "Simplificacao visual (B): a espinha cronologica (sucessao por ordem) NAO e desenhada; atos sem ligacao semantica (citacao/prazo/endereçamento/triagem) ficam sem seta de entrada. Isso torna explicito o que NAO foi reconstruido, em vez de aproximar por ordem."
        else NULL,
        "Demanda da triagem (despacho CGM/CAIP -> 1o parecer que atende, por ordem de juntada) e aresta de FLUXO reconstruida da norma (FLUXOS sec. 2), nao citacao explicita; casa por janela de atos e pode errar quando ha pareceres concorrentes.",
        "Criterios do especialista NAO implementados neste grafo: enderecamento a funcao (3), comunicacao ao interessado por e-mail (5), ficha->termo de referencia (6) e complementacao atendida (7) — dependem de sinais nao extraidos com confianca (e-mails, funcao<->cargo) ou de mais regras normativas; registrado como trabalho futuro.",
        "Nao ha ligacao documento->documento explicita; a espinha cronologica e aproximacao da ordem dos atos.",
        "Arestas de prazo (abertura->parecer) sao semanticas (vindas do calculo de prazos).",
        "Documentacao externa (docs sem classe) e resumida em nos de 'demanda externa'; a ligacao ao 1o ato do IPHAN e aproximacao pela ordem, nao citacao explicita.",
        "Marcacoes finas da secao 8 do FLUXOS nao sao geradas (exigem motor de conformidade)."
      )
    ),
    nos     = nos,
    arestas = arestas
  )

  if (persistir) {
    dir_dia  <- dir_saida_dia(dir_processed)
    data_str <- format(Sys.time(), "%Y%m%d")
    caminho  <- file.path(dir_dia, paste0(numero_seguro, "_grafo_", data_str, ".json"))
    write_json(saida, caminho, pretty = TRUE, auto_unbox = TRUE, na = "null")
    cat("[grafo] Persistido (JSON) em:", caminho, "\n")
  }

  # Render principal: Graphviz/dot (DiagrammeR) — arvore estatica no estilo do
  # especialista (SVG). salvar_html mantem o nome do parametro por compat com o
  # pipeline/main.R, mas agora produz o SVG (nao mais o HTML visNetwork).
  # salvar_plot adiciona tambem o PNG (via rsvg).
  if (salvar_html || salvar_plot) {
    caminho_svg <- .render_grafo_graphviz(nos, arestas, numero_processo,
                                          numero_seguro, dir_output,
                                          gerar_png = isTRUE(salvar_plot))
    if (!is.null(caminho_svg)) cat("[grafo] Figura (SVG) salva em:", caminho_svg, "\n")
  }

  invisible(c(saida, list(igraph = g)))
}

# -----------------------------------------------------------------------------
# Render (ggraph): layout hierarquico no tempo, cor por situacao, rotulo na seta
# -----------------------------------------------------------------------------

#' Renderiza o grafo de tramitacao em PNG via ggraph. Retorna o caminho ou NULL.
.render_grafo_tramitacao <- function(g, numero_processo, numero_seguro, dir_output) {
  if (!requireNamespace("ggraph", quietly = TRUE) ||
      !requireNamespace("ggplot2", quietly = TRUE)) {
    warning("[grafo] ggraph/ggplot2 ausentes — figura nao gerada.")
    return(NULL)
  }
  if (igraph::vcount(g) == 0) return(NULL)

  # Rotulos de aresta NA (destinatario ausente) devem aparecer em branco, nao "NA".
  if ("rotulo" %in% igraph::edge_attr_names(g)) {
    rot <- igraph::edge_attr(g, "rotulo")
    rot[is.na(rot)] <- ""
    g <- igraph::set_edge_attr(g, "rotulo", value = rot)
  }

  dir_dia     <- dir_saida_dia(dir_output)
  data_str    <- format(Sys.time(), "%Y%m%d")
  caminho_png <- file.path(dir_dia, paste0(numero_seguro, "_grafo_", data_str, ".png"))

  # Mapa situacao -> cor (para escala manual, com nomes legiveis)
  cores <- c(dentro_prazo = COR_DENTRO, fora_prazo = COR_FORA,
             complementacao = COR_COMPLEMENTO, demanda_externa = COR_DEMANDA_EXT,
             avulso = COR_AVULSO, ato = COR_ATO)
  rotulos_base <- c(dentro_prazo = "parecer no prazo", fora_prazo = "parecer fora do prazo",
                    complementacao = "complementacao", demanda_externa = "demanda externa",
                    avulso = "avulso", ato = "ato")

  # Legenda com CONTAGEM por situacao: "parecer no prazo (2)". Conta os nos do
  # grafo (apos filtragem de avulsos, se aplicada), na ordem fixa de SITUACOES.
  sit_v <- igraph::vertex_attr(g, "situacao")
  cont  <- table(factor(sit_v, levels = SITUACOES))
  rotulos_sit <- vapply(SITUACOES, function(s)
    paste0(rotulos_base[[s]], " (", as.integer(cont[[s]]), ")"), character(1))
  names(rotulos_sit) <- SITUACOES

  # Layout no tempo: coloca os nos numa coluna por ordem cronologica (eixo y),
  # o que evita a sobreposicao do sugiyama e deixa o fluxo legivel de cima para
  # baixo. x levemente alternado reduz colisao de rotulos longos.
  n <- igraph::vcount(g)
  ordem_v <- igraph::vertex_attr(g, "ordem")
  if (is.null(ordem_v) || all(is.na(ordem_v))) ordem_v <- seq_len(n)
  ry <- rank(ordem_v, ties.method = "first")
  layout_manual <- data.frame(
    x = ifelse(ry %% 2 == 0, 0.5, -0.5),
    y = -ry  # cronologia de cima (mais antigo) para baixo
  )
  altura <- max(9, n * 0.95)

  p <- ggraph::ggraph(g, layout = "manual",
                      x = layout_manual$x, y = layout_manual$y) +
    ggraph::geom_edge_link(
      ggplot2::aes(label = .data$rotulo, linetype = .data$tipo),
      arrow = ggplot2::arrow(length = ggplot2::unit(2.5, "mm"), type = "closed"),
      end_cap = ggraph::circle(9, "mm"), start_cap = ggraph::circle(9, "mm"),
      edge_colour = "grey50", angle_calc = "along",
      label_size = 2.5, label_colour = "grey25",
      label_dodge = ggplot2::unit(2.5, "mm")
    ) +
    ggraph::geom_node_label(
      ggplot2::aes(label = .data$rotulo, fill = .data$situacao),
      colour = "black", size = 2.5, label.size = 0.3, lineheight = 0.95
    ) +
    ggplot2::scale_fill_manual(values = cores, labels = rotulos_sit,
                               name = "situacao", drop = FALSE) +
    ggraph::scale_edge_linetype_manual(
      values = c(citacao = "solid", prazo = "solid", enderecamento = "solid",
                 demanda_triagem = "solid", demanda = "dashed", cronologica = "dashed"),
      labels = c(citacao = "citacao (citado->citante)",
                 prazo = "prazo (abertura->parecer)",
                 enderecamento = "enderecamento",
                 demanda_triagem = "demanda da triagem (despacho->parecer)",
                 demanda = "demanda externa", cronologica = "cronologica"),
      name = "ligacao", drop = FALSE
    ) +
    ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = 0.55)) +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(add = 0.9)) +
    ggplot2::labs(
      title = "Grafo de tramitacao do processo",
      subtitle = paste("Processo", numero_processo,
                       "— no = documento; seta = fluxo (destinatario / limite / prazo)")
    ) +
    ggraph::theme_graph(base_family = "sans")

  ggplot2::ggsave(caminho_png, plot = p, width = 15, height = altura,
                  dpi = 150, limitsize = FALSE)
  caminho_png
}

# -----------------------------------------------------------------------------
# Render (visNetwork): HTML interativo, layout hierarquico (arvore), zoom/pan
# -----------------------------------------------------------------------------

#' Renderiza o grafo em HTML interativo com visNetwork (layout hierarquico em
#' arvore, cima->baixo). Resolve a sobreposicao de grafos grandes: o vis.js faz
#' o roteamento e o usuario tem zoom/pan/arraste. Retorna o caminho ou NULL.
#'
#' Todos os nos usam a MESMA forma (retangulo): so a COR comunica a situacao
#' (a forma por tipo poluia a leitura). Os rotulos das arestas tem fundo branco
#' e o espacamento entre niveis e generoso, para o texto nao ficar sob os nos.
#'
#' Calcula o NIVEL hierarquico de cada no como sua PROFUNDIDADE no fluxo: o
#' comprimento do maior caminho (em arestas) desde uma raiz ate o no. Raizes
#' (sem entrada) ficam no nivel 1. Assim, irmaos (varios pareceres que respondem
#' ao mesmo despacho) compartilham o nivel e o layout os abre lado a lado, em
#' vez de empilhar um nivel por documento (o que dava aparencia linear).
#'
#' Usa ordenacao topologica (o grafo do fluxo e majoritariamente um DAG; a
#' ordem de juntada garante que arestas apontam "para a frente"). Se restar
#' algum ciclo, os nos nao alcancados recebem nivel por fallback cronologico.
#'
#' @param nos tibble de nos (com id, ordem)
#' @param arestas tibble de arestas (de, para)
#' @return vetor inteiro de niveis, na ordem das linhas de `nos`
.niveis_por_profundidade <- function(nos, arestas) {
  n <- nrow(nos)
  if (n == 0) return(integer(0))
  idx <- setNames(seq_len(n), nos$id)
  nivel <- rep(NA_integer_, n)

  # arestas validas (ambos os extremos sao nos do grafo)
  if (nrow(arestas) > 0) {
    val <- arestas$de %in% nos$id & arestas$para %in% nos$id
    ar  <- arestas[val, , drop = FALSE]
  } else {
    ar <- arestas
  }

  # raizes = nos sem entrada -> nivel 1
  tem_entrada <- nos$id %in% ar$para
  nivel[!tem_entrada] <- 1L

  # relaxamento por ordem de juntada (topologica aproximada): visita os nos na
  # ordem de `ordem` e propaga nivel_pai + 1 para os filhos. Como as arestas
  # majoritariamente apontam para a frente, uma passada resolve o DAG; fazemos
  # ate n passadas por seguranca, parando quando estabiliza.
  ordem_visita <- order(nos$ordem)
  filhos_de <- split(ar$para, factor(ar$de, levels = nos$id))
  for (passada in seq_len(n)) {
    mudou <- FALSE
    for (i in ordem_visita) {
      niv_pai <- nivel[i]
      if (is.na(niv_pai)) next
      fs <- filhos_de[[nos$id[i]]]
      if (length(fs) == 0) next
      for (f in fs) {
        k <- idx[[f]]
        prof <- niv_pai + 1L
        if (is.na(nivel[k]) || prof > nivel[k]) { nivel[k] <- prof; mudou <- TRUE }
      }
    }
    if (!mudou) break
  }

  # Fallback: nos nunca alcancados (isolados ou em ciclo) recebem nivel pela
  # posicao cronologica relativa, para nao ficarem sem camada.
  if (any(is.na(nivel))) {
    falta <- which(is.na(nivel))
    base  <- if (all(is.na(nivel))) 1L else max(nivel, na.rm = TRUE)
    nivel[falta] <- base + rank(nos$ordem[falta], ties.method = "first")
  }
  as.integer(nivel)
}

# -----------------------------------------------------------------------------
# Render (Graphviz/dot via DiagrammeR): arvore estatica no estilo do especialista
# -----------------------------------------------------------------------------

# Cores de ARESTA no estilo do especialista: so citacao e endereçamento tem
# cor; o resto e cinza padrao. (Sem formas/tracos: todas as setas iguais.)
COR_SETA_CITACAO       <- "#1F77B4"   # azul  — citacao (documento cita outro)
COR_SETA_ENDERECAMENTO <- "#C0392B"   # vermelho — endereçamento (destinatario responde)
COR_SETA_PADRAO        <- "#7A7A7A"   # cinza — demais ligacoes (prazo/triagem/demanda/sequencia)

#' Escapa aspas e quebras para um rotulo DOT (label entre aspas).
.dot_esc <- function(x) {
  if (.vazio(x)) return("")
  x <- gsub("\\\\", "\\\\\\\\", x)
  x <- gsub("\"", "'", x)
  gsub("\n", "\\\\n", x)   # quebra de linha do DOT
}

#' Cor da seta por tipo de aresta (estilo especialista).
.cor_seta <- function(tipo) {
  if (identical(tipo, "citacao"))       return(COR_SETA_CITACAO)
  if (identical(tipo, "enderecamento")) return(COR_SETA_ENDERECAMENTO)
  COR_SETA_PADRAO
}

# Rotulo por extenso do tipo de aresta (escrito na seta, para leitura).
ROTULOS_TIPO_ARESTA <- c(
  citacao         = "citacao",
  enderecamento   = "enderecamento",
  prazo           = "prazo",
  demanda_triagem = "demanda triagem",
  demanda         = "demanda externa",
  cronologica     = "sequencia")

.rotulo_tipo_aresta <- function(tipo) {
  r <- ROTULOS_TIPO_ARESTA[[tipo]]
  if (is.null(r)) tipo else r
}

#' Monta a LEGENDA como UM UNICO no com label em TABELA HTML do Graphviz,
#' ancorado no RODAPE (rank=sink) e fora do fluxo. Compacto e previsivel — nao
#' se espalha pelo grafo. Mostra so o que esta PRESENTE: cores dos NOS
#' (situacao) e cores/tipos das ARESTAS. Retorna linhas DOT.
.legenda_dot <- function(nos, arestas) {
  rotulos_sit <- c(dentro_prazo = "parecer no prazo", fora_prazo = "parecer fora do prazo",
                   complementacao = "complementacao", demanda_externa = "demanda externa",
                   avulso = "avulso", ato = "ato")
  cor_sit <- c(dentro_prazo = COR_DENTRO, fora_prazo = COR_FORA,
               complementacao = COR_COMPLEMENTO, demanda_externa = COR_DEMANDA_EXT,
               avulso = COR_AVULSO, ato = COR_ATO)
  sit_presentes <- intersect(names(rotulos_sit), unique(nos$situacao))
  tipos_presentes <- intersect(names(ROTULOS_TIPO_ARESTA), unique(arestas$tipo))

  # Celula HTML de amostra de cor de NO (quadradinho colorido + texto).
  cel_no <- function(s) {
    fonte <- if (identical(s, "avulso")) "#FFFFFF" else "#1A1A1A"
    sprintf('<TD BGCOLOR="%s"><FONT COLOR="%s" POINT-SIZE="10"> %s </FONT></TD>',
            cor_sit[[s]], fonte, htmlEscape_local(rotulos_sit[[s]]))
  }
  # Celula HTML de amostra de ARESTA (texto colorido com a cor da seta).
  cel_ar <- function(tp) {
    sprintf('<TD><FONT COLOR="%s" POINT-SIZE="10"> &#9472;&#9472; %s </FONT></TD>',
            .cor_seta(tp), htmlEscape_local(ROTULOS_TIPO_ARESTA[[tp]]))
  }

  linha_nos <- if (length(sit_presentes) > 0)
    paste0('<TR><TD ALIGN="LEFT"><B>Nos:</B></TD>',
           paste(vapply(sit_presentes, cel_no, character(1)), collapse = ""), '</TR>')
  else ""
  linha_ars <- if (length(tipos_presentes) > 0)
    paste0('<TR><TD ALIGN="LEFT"><B>Arestas:</B></TD>',
           paste(vapply(tipos_presentes, cel_ar, character(1)), collapse = ""), '</TR>')
  else ""

  label_html <- paste0(
    '<<TABLE BORDER="0" CELLBORDER="1" CELLSPACING="2" CELLPADDING="3">',
    '<TR><TD ALIGN="LEFT" COLSPAN="', 
    max(length(sit_presentes), length(tipos_presentes)) + 1L,
    '"><B>Legenda</B></TD></TR>',
    linha_nos, linha_ars, '</TABLE>>')

  # No solto (sem rank/constraint): fica como componente separado. NAO usamos
  # rank=sink porque, combinado com ciclos de rank das citacoes que apontam
  # "para tras", dispara o assertion !TREE_EDGE do dot (merge_trees).
  sprintf('  "__legenda__" [shape=plaintext, margin=0, label=%s];', label_html)
}

#' Escape HTML minimo para os labels HTML-like do Graphviz.
htmlEscape_local <- function(x) {
  if (.vazio(x)) return("")
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  gsub(">", "&gt;", x, fixed = TRUE)
}

#' Renderiza o grafo como imagem ESTATICA (SVG, e opcionalmente PNG) via
#' Graphviz/dot (DiagrammeR), no estilo do grafo do especialista: arvore
#' top-down (rankdir=TB), nos coloridos por situacao, arestas coloridas so para
#' citacao (azul) e endereçamento (vermelho); demais cinza. SEM rotulo de aresta.
#'
#' @param nos tibble de nos; @param arestas tibble de arestas
#' @param numero_processo,numero_seguro identificacao
#' @param dir_output diretorio base das figuras
#' @param gerar_png Logico — tambem exportar PNG (exige DiagrammeRsvg + rsvg)
#' @return caminho do SVG (ou NULL)
.render_grafo_graphviz <- function(nos, arestas, numero_processo, numero_seguro,
                                   dir_output, gerar_png = FALSE) {
  if (!requireNamespace("DiagrammeR", quietly = TRUE)) {
    warning("[grafo] DiagrammeR ausente — SVG nao gerado. install.packages('DiagrammeR').")
    return(NULL)
  }
  if (nrow(nos) == 0) return(NULL)

  # INICIO: raiz principal (nivel 1 de menor ordem) recebe selo no rotulo.
  nivel <- .niveis_por_profundidade(nos, arestas)
  eh_raiz <- nivel == min(nivel)
  raiz_principal <- if (any(eh_raiz)) { c <- which(eh_raiz); c[which.min(nos$ordem[c])] } else NA_integer_

  # Rotulo do no = rotulo completo (mesmo do JSON), com quebras \n do DOT.
  rot <- nos$rotulo
  if (!is.na(raiz_principal))
    rot[raiz_principal] <- paste0("\u25B6 INICIO DO PROCESSO\n", rot[raiz_principal])

  # Cor de fonte: branca sobre o preto do avulso; escura no resto.
  cor_fonte <- ifelse(nos$situacao == "avulso", "#FFFFFF", "#1A1A1A")
  # Borda: raizes em verde (grossa na principal).
  cor_borda <- ifelse(eh_raiz, "#1B7F3B", "#555555")
  larg_borda <- ifelse(eh_raiz, "2.4", "1.2")
  if (!is.na(raiz_principal)) larg_borda[raiz_principal] <- "3.4"

  # --- Nos (box arredondado, preenchido pela cor da situacao) ---
  linhas_nos <- vapply(seq_len(nrow(nos)), function(i) {
    sprintf('  "%s" [label="%s", shape=box, style="filled,rounded", fillcolor="%s", color="%s", penwidth=%s, fontcolor="%s"];',
            .dot_esc(nos$id[i]), .dot_esc(rot[i]), nos$cor[i],
            cor_borda[i], larg_borda[i], cor_fonte[i])
  }, character(1))

  # --- Arestas (cor por tipo; SEM rotulo; setas uniformes) ---
  # O tipo da aresta e comunicado pela COR (ver legenda no rodape); nao escrevemos
  # o tipo na seta para nao poluir o grafo.
  # Arestas que apontam "para tras" na ordem de juntada (destino anterior a
  # origem) recebem constraint=false: continuam DESENHADAS, mas NAO participam
  # do calculo de ranks do dot. Isso evita o assertion !TREE_EDGE (merge_trees),
  # que o dot dispara quando essas arestas fecham ciclos na hierarquia de ranks.
  ordem_por_id <- setNames(nos$ordem, nos$id)
  linhas_ar <- if (nrow(arestas) > 0) vapply(seq_len(nrow(arestas)), function(i) {
    cor <- .cor_seta(arestas$tipo[i])
    od <- suppressWarnings(as.numeric(ordem_por_id[[as.character(arestas$de[i])]]))
    op <- suppressWarnings(as.numeric(ordem_por_id[[as.character(arestas$para[i])]]))
    para_tras <- !is.na(od) && !is.na(op) && op <= od
    extra <- if (para_tras) ", constraint=false" else ""
    sprintf('  "%s" -> "%s" [color="%s", penwidth=1.6%s];',
            .dot_esc(arestas$de[i]), .dot_esc(arestas$para[i]), cor, extra)
  }, character(1)) else character(0)

  # --- Legenda (cluster separado, canto): cores dos NOS (situacao) + cores das
  # ARESTAS (tipo de ligacao). Nos-amostra + pares de arestas-amostra. ---
  legenda_dot <- .legenda_dot(nos, arestas)

  dot <- c(
    sprintf('digraph "%s" {', .dot_esc(numero_processo)),
    '  graph [rankdir=TB, splines=true, nodesep=0.4, ranksep=0.7, fontname="Helvetica"];',
    '  node  [fontname="Helvetica", fontsize=10];',
    '  edge  [fontname="Helvetica", fontsize=9, arrowsize=0.8];',
    legenda_dot,
    linhas_nos,
    linhas_ar,
    '}'
  )
  dot_txt <- paste(dot, collapse = "\n")

  dir_dia  <- dir_saida_dia(dir_output)
  data_str <- format(Sys.time(), "%Y%m%d")
  caminho_svg <- file.path(dir_dia, paste0(numero_seguro, "_grafo_", data_str, ".svg"))

  ok <- tryCatch({
    g <- DiagrammeR::grViz(dot_txt)
    svg <- DiagrammeRsvg::export_svg(g)
    writeLines(svg, caminho_svg, useBytes = TRUE)
    TRUE
  }, error = function(e) {
    warning("[grafo] Falha ao renderizar Graphviz: ", conditionMessage(e)); FALSE
  })
  if (!ok) return(NULL)

  if (gerar_png && requireNamespace("rsvg", quietly = TRUE)) {
    caminho_png <- sub("\\.svg$", ".png", caminho_svg)
    tryCatch(rsvg::rsvg_png(caminho_svg, caminho_png, width = 2000),
             error = function(e) warning("[grafo] Falha ao exportar PNG: ",
                                         conditionMessage(e)))
  }
  caminho_svg
}

#' @param nos tibble de nos (id, rotulo, tipo, classe, situacao, cor, ordem)
#' @param arestas tibble de arestas (de, para, tipo, rotulo)
#' @param numero_processo,numero_seguro identificacao
#' @param dir_output diretorio base das figuras
.render_grafo_html <- function(nos, arestas, numero_processo, numero_seguro,
                               dir_output) {
  if (!requireNamespace("visNetwork", quietly = TRUE)) {
    warning("[grafo] visNetwork ausente — HTML nao gerado. install.packages('visNetwork').")
    return(NULL)
  }
  if (nrow(nos) == 0) return(NULL)

  # Rotulos de situacao (para a legenda e o grupo visual)
  rotulos_sit <- c(dentro_prazo = "parecer no prazo",
                   fora_prazo = "parecer fora do prazo",
                   complementacao = "complementacao",
                   demanda_externa = "demanda externa",
                   avulso = "avulso", ato = "ato")
  # Cor do texto: escuro sobre fundo claro, branco sobre o preto do avulso.
  cor_fonte <- ifelse(nos$situacao == "avulso", "#FFFFFF", "#1A1A1A")

  # O LAYOUT agora e calculado pelo igraph (layout_as_tree, Reingold-Tilford),
  # nao pelo layout hierarquico do vis.js. O igraph espalha os filhos
  # horizontalmente sob cada pai (arvore de verdade), resolvendo a aparencia
  # "so desce" do layout hierarquico. Por isso NAO fixamos mais `level` no no:
  # a posicao (x, y) vem do igraph. Mantemos .niveis_por_profundidade so para
  # escolher a(s) RAIZ(es) da arvore (nivel 1).
  nivel <- .niveis_por_profundidade(nos, arestas)

  # INICIO do grafo: as RAIZES sao os nos de nivel 1 (sem entrada) — por onde o
  # processo comeca. Destacamos para o leitor achar de imediato onde comeca:
  #   - "raiz principal" = a de MENOR ordem (o 1o ato/demanda na arvore de
  #     juntada); ganha selo "INICIO" no rotulo e borda verde grossa.
  #   - demais raizes (entradas posteriores do interessado) ganham borda verde
  #     mais fina, sem selo, para nao competirem com a principal.
  eh_raiz        <- nivel == min(nivel)
  raiz_principal <- if (any(eh_raiz)) {
    cand <- which(eh_raiz); cand[which.min(nos$ordem[cand])]
  } else NA_integer_

  # LABEL COMPLETO na caixa: todas as informacoes do no ficam VISIVEIS sem
  # precisar de hover (tipo/unidade/data/remetente/destinatario/prazo). O
  # tooltip repete o mesmo conteudo, para quem preferir ampliar.
  label_no <- nos$rotulo
  if (!is.na(raiz_principal)) {
    label_no[raiz_principal] <- paste0("\u25B6 IN\u00cdCIO DO PROCESSO\n",
                                        label_no[raiz_principal])
  }
  # Borda: raiz principal verde grossa; demais raizes verde media; resto padrao.
  cor_borda <- rep("#555555", nrow(nos))
  larg_borda <- rep(1L, nrow(nos))
  cor_borda[eh_raiz]  <- "#1B7F3B"
  larg_borda[eh_raiz] <- 3L
  if (!is.na(raiz_principal)) larg_borda[raiz_principal] <- 5L

  # Todos os nos usam a MESMA forma (retangulo "box"): so a cor comunica a
  # situacao. Label completo visivel na caixa.
  nodes <- data.frame(
    id     = nos$id,
    label  = label_no,
    group  = nos$situacao,
    shape  = "box",
    color.background = nos$cor,
    color.border     = cor_borda,
    borderWidth      = larg_borda,
    font.color       = cor_fonte,
    title  = gsub("\n", "<br>", nos$rotulo),  # tooltip (HTML) com o mesmo detalhe
    stringsAsFactors = FALSE
  )

  # Arestas: estilo por tipo (a COR distingue). citacao = azul solida (a "arvore"
  # de citacoes do especialista); prazo = azul-escuro solida; enderecamento =
  # vermelho solido; demanda_triagem = verde-petroleo solida (despacho CGM ->
  # parecer); demanda = dourada tracejada; cronologica = cinza. O rotulo ganha
  # FUNDO BRANCO para permanecer legivel.
  cor_aresta <- c(citacao = "#1F77B4", prazo = "#2E5496", enderecamento = "#C0392B",
                  demanda_triagem = "#2E8B7A", demanda = "#B8860B", cronologica = "#AAAAAA")
  dashes     <- c(citacao = FALSE, prazo = FALSE, enderecamento = FALSE,
                  demanda_triagem = FALSE, demanda = TRUE, cronologica = FALSE)
  edges <- data.frame(
    from   = arestas$de,
    to     = arestas$para,
    label  = ifelse(is.na(arestas$rotulo), "", gsub("\n", " ", arestas$rotulo)),
    color  = unname(cor_aresta[arestas$tipo]),
    dashes = unname(dashes[arestas$tipo]),
    arrows = "to",
    font.size       = 13,
    font.color      = "#222222",
    font.background = "#FFFFFFDD",  # fundo branco semi-opaco atras do texto
    font.strokeWidth = 0,
    font.align      = "horizontal",
    stringsAsFactors = FALSE
  )
  edges$color[is.na(edges$color)] <- "#AAAAAA"
  edges$dashes[is.na(edges$dashes)] <- FALSE

  # Legenda de NOS: uma entrada por situacao presente, com a cor de fundo.
  presentes <- intersect(SITUACOES, unique(nos$situacao))
  cor_sit <- c(dentro_prazo = COR_DENTRO, fora_prazo = COR_FORA,
               complementacao = COR_COMPLEMENTO, demanda_externa = COR_DEMANDA_EXT,
               avulso = COR_AVULSO, ato = COR_ATO)
  legenda_nos <- data.frame(
    label = unname(rotulos_sit[presentes]),
    color.background = unname(cor_sit[presentes]),
    color.border = "#555555",
    shape = "box",
    stringsAsFactors = FALSE
  )

  # Legenda de ARESTAS: uma entrada por tipo de ligacao PRESENTE no grafo, com a
  # cor/traco correspondentes. Rotulos legiveis (o que cada seta significa).
  rotulos_aresta <- c(citacao = "citacao (documento cita outro)",
                      prazo = "prazo (abertura -> parecer)",
                      enderecamento = "enderecamento (destinatario responde)",
                      demanda_triagem = "demanda da triagem (despacho -> parecer)",
                      demanda = "demanda externa (protocolo -> 1o ato)",
                      cronologica = "sequencia (ordem de juntada)")
  tipos_aresta_presentes <- intersect(names(rotulos_aresta), unique(arestas$tipo))
  legenda_arestas <- if (length(tipos_aresta_presentes) > 0) {
    data.frame(
      label  = unname(rotulos_aresta[tipos_aresta_presentes]),
      color  = unname(cor_aresta[tipos_aresta_presentes]),
      dashes = unname(dashes[tipos_aresta_presentes]),
      arrows = "to",
      font.align = "top",
      stringsAsFactors = FALSE
    )
  } else NULL

  titulo <- paste0("Grafo de tramitacao — Processo ", numero_processo)
  sub <- "INICIO = no com borda verde (selo no topo) | seta desce da raiz | azul = citacao | azul-escuro = prazo | vermelho = enderecamento | verde-petroleo = demanda da triagem | dourada tracejada = demanda externa | cinza = cronologica"

  # Raizes da arvore (nivel 1 = sem entrada), para o layout_as_tree crescer a
  # partir delas de cima para baixo. Se nao houver raiz clara, o igraph escolhe.
  n_niveis   <- length(unique(nivel))
  raizes_idx <- which(nivel == min(nivel))

  # Altura do canvas: cresce com o numero de niveis para a arvore nao ficar
  # espremida (o igraph posiciona; damos espaco vertical proporcional).
  max_linhas <- max(vapply(nodes$label, function(r)
    length(strsplit(r, "\n")[[1]]), integer(1)))
  altura_px  <- max(1000L, min(20000L, as.integer(n_niveis * (110L + max_linhas * 10L))))

  # LAYOUT via IGRAPH (layout_as_tree / Reingold-Tilford): espalha os filhos
  # horizontalmente sob cada pai — arvore de verdade, cima->baixo. Substitui o
  # layout hierarquico do vis.js, que empilhava tudo numa fita vertical (dava
  # aparencia "so desce" mesmo havendo galhos). Sem dependencia nova: igraph ja
  # e usado no modulo.
  rede <- visNetwork::visNetwork(nodes, edges, main = titulo, submain = sub,
                                 width = "100%", height = paste0(altura_px, "px")) |>
    visNetwork::visIgraphLayout(layout = "layout_as_tree",
                                root = raizes_idx,
                                flip.y = TRUE,        # raiz no TOPO (cima->baixo)
                                physics = FALSE,
                                smooth = FALSE,
                                type = "full") |>
    # widthConstraint menor (180) deixa as caixas mais estreitas: cabem mais
    # irmaos lado a lado sem colisao; o texto quebra em mais linhas (ok).
    visNetwork::visNodes(shape = "box", shadow = TRUE,
                         widthConstraint = list(maximum = 180),
                         heightConstraint = list(minimum = 30),
                         margin = 8,
                         font = list(size = 14, multi = FALSE, align = "left")) |>
    # Arestas RETAS (sem curva): com o layout em arvore do igraph, a ligacao
    # pai->filho ja desce direto; a curva "cubicBezier vertical" do layout
    # hierarquico brigava com as coordenadas do igraph. Retas deixam a arvore
    # limpa. O rotulo mantem fundo branco (definido em `edges`).
    visNetwork::visEdges(smooth = FALSE, arrowStrikethrough = FALSE) |>
    visNetwork::visOptions(highlightNearest = list(enabled = TRUE, degree = 1,
                                                   hover = TRUE),
                           nodesIdSelection = TRUE) |>
    visNetwork::visInteraction(dragNodes = TRUE, dragView = TRUE, zoomView = TRUE,
                               navigationButtons = TRUE, tooltipDelay = 150) |>
    # Legenda em coluna dedicada mais LARGA (0.28), fixa (zoom = FALSE) e com
    # mais espaco vertical entre itens (stepY), para o grafo nao tapa-la.
    # Reune NOS (cor = situacao) e ARESTAS (cor/traco = tipo de ligacao).
    visNetwork::visLegend(addNodes = legenda_nos, addEdges = legenda_arestas,
                          useGroups = FALSE, width = 0.28, position = "left",
                          main = list(text = "Legenda",
                                      style = "font-size:16px;font-weight:bold;"),
                          ncol = 1, stepX = 100, stepY = 70, zoom = FALSE)

  dir_dia      <- dir_saida_dia(dir_output)
  data_str     <- format(Sys.time(), "%Y%m%d")
  caminho_html <- file.path(dir_dia, paste0(numero_seguro, "_grafo_", data_str, ".html"))

  # HTML autocontido (um unico arquivo) exige pandoc. Se ele nao estiver
  # disponivel, caimos para selfcontained = FALSE: gera o .html + uma pasta
  # "..._files" ao lado com as bibliotecas JS. Ambos abrem no navegador; o
  # autocontido e mais pratico de compartilhar. A escolha e automatica.
  # O pacote 'pandoc' (se instalado) traz um binario proprio; apontamos o
  # htmlwidgets/rmarkdown para ele via RSTUDIO_PANDOC.
  if (requireNamespace("pandoc", quietly = TRUE)) {
    bin <- tryCatch(pandoc::pandoc_bin(), error = function(e) NULL)
    if (!is.null(bin) && nzchar(bin) && file.exists(bin)) {
      Sys.setenv(RSTUDIO_PANDOC = dirname(bin))
    }
  }
  tem_pandoc <- nzchar(Sys.which("pandoc")) ||
    (requireNamespace("rmarkdown", quietly = TRUE) &&
       isTRUE(tryCatch(rmarkdown::pandoc_available(), error = function(e) FALSE)))
  ok <- tryCatch({
    visNetwork::visSave(rede, caminho_html, selfcontained = tem_pandoc)
    TRUE
  }, error = function(e) {
    # ultimo recurso: sem autocontido (nao depende de pandoc)
    tryCatch({ visNetwork::visSave(rede, caminho_html, selfcontained = FALSE); TRUE },
             error = function(e2) { warning("[grafo] Falha ao salvar HTML: ",
                                            conditionMessage(e2)); FALSE })
  })
  if (!ok) return(NULL)
  if (!tem_pandoc) {
    cat("[grafo] NOTA: pandoc ausente — HTML gerado com pasta '_files' ao lado",
        "(nao autocontido). Para um unico arquivo, instale o pandoc.\n")
  }
  caminho_html
}
