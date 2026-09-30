# =============================================================================
# 08_normativas.R
# Leitura, validacao e consulta das normativas (IN IPHAN nº 06/2025) (POC IPHAN)
#
# Responsabilidade: Camada de Normativas (ver architecture.md).
#   As regras de prazo da IN 06/2025 estao armazenadas como DADO em
#   data/normativas/in_06_2025_prazos.json (transcritas da secao 7 de
#   FLUXOS.md pelo especialista). Este modulo NAO contem prazos hardcoded:
#   ele apenas LE, VALIDA e torne o JSON CONSULTAVEL para a etapa de prazos
#   (09_prazos.R) e a analise de conformidade.
#
# Principio (project-context): "As regras normativas devem ser armazenadas como
#   dados/configuracao e nao espalhadas pelo codigo." e "Nao inventar regras
#   normativas; quando um caso nao constar, registrar a limitacao."
#
# O que expoe:
#   - carregar_normativas()        : le e valida o JSON, retorna a estrutura.
#   - consultar_prazo(norm, peca)  : encontra o prazo (dias) para uma peca/tipo,
#                                     por correspondencia de nome (normalizada).
#   - regra_contagem(norm)         : dias corridos vs uteis (art. 54).
#   - criterio_encerramento(norm)  : criterio de encerramento do ato.
#
# Tecnologias: jsonlite, stringi, stringr
# =============================================================================

library(jsonlite)
library(stringi)
library(stringr)

# -----------------------------------------------------------------------------
# Constantes
# -----------------------------------------------------------------------------

NORMATIVAS_PATH_PADRAO <- "data/normativas/in_06_2025_prazos.json"

# -----------------------------------------------------------------------------
# Normalizacao para casar nomes de peca (sem acento, minusculas)
# -----------------------------------------------------------------------------

#' Normaliza um rotulo de peca/tipo para comparacao (minusculas, sem acento).
#' @param x character
#' @return character normalizado
.norm_peca <- function(x) {
  if (length(x) == 0) return(character(0))
  x <- stri_trans_tolower(x)
  x <- stri_trans_general(x, "Latin-ASCII")
  str_squish(x)
}

# -----------------------------------------------------------------------------
# Carregamento e validacao
# -----------------------------------------------------------------------------

#' Le e valida o JSON de normativas (prazos da IN 06/2025).
#'
#' Validacoes (estrutura minima esperada, sem inventar conteudo):
#'   - blocos obrigatorios presentes: regra_contagem, encerramento_do_ato,
#'     analise_de_pecas, complementacao, manifestacao_conclusiva.
#'   - analise_de_pecas$itens com pelo menos um item {peca, prazo_dias}.
#'
#' @param caminho Caminho do JSON
#' @return list com a estrutura das normativas + um indice de pecas achatado
#'         (`.indice_pecas`: data.frame peca/peca_norm/prazo_dias/prorrogavel/base/origem)
carregar_normativas <- function(caminho = NORMATIVAS_PATH_PADRAO) {
  if (!file.exists(caminho)) {
    stop("[normativas] JSON de normativas nao encontrado: ", caminho)
  }
  norm <- jsonlite::fromJSON(caminho, simplifyVector = FALSE)

  obrig <- c("regra_contagem", "encerramento_do_ato", "analise_de_pecas",
             "complementacao", "manifestacao_conclusiva")
  faltantes <- setdiff(obrig, names(norm))
  if (length(faltantes) > 0) {
    stop("[normativas] Blocos obrigatorios ausentes no JSON: ",
         paste(faltantes, collapse = ", "))
  }

  itens <- norm$analise_de_pecas$itens
  if (is.null(itens) || length(itens) == 0) {
    stop("[normativas] analise_de_pecas$itens vazio — sem prazos para consultar.")
  }

  norm$.indice_pecas <- .montar_indice_pecas(norm)
  norm
}

#' Monta um indice achatado de todas as pecas com prazo, de todos os blocos.
#' Facilita a consulta por nome. Registra a origem (bloco) de cada prazo.
#'
#' Cada peca tambem pode ter SINONIMOS (dado no JSON), que entram no indice como
#' linhas proprias com `e_sinonimo = TRUE`, apontando a `peca` canonica e a
#' `origem_sinonimo` (ex.: "IN 06/2025 sec. 7" ou "interpretacao_pesquisador").
#' Isso mantem a regra no dado e permite a avaliacao separar mapeamentos diretos
#' de interpretacoes.
#'
#' @param norm list de normativas
#' @return data.frame(peca, peca_norm, prazo_dias, prorrogavel, base, origem,
#'                     e_sinonimo, origem_sinonimo)
.montar_indice_pecas <- function(norm) {
  linhas <- list()

  add <- function(peca, prazo_dias, prorrogavel, base, origem,
                  e_sinonimo = FALSE, origem_sinonimo = NA_character_,
                  peca_canonica = NA_character_, sem_prazo = FALSE) {
    if (is.null(peca)) return(invisible(NULL))
    # prazo_dias pode ser NULL/NA (peca sem prazo normativo fixado, ex.: PAPIPA)
    prazo_int <- if (is.null(prazo_dias) || is.na(prazo_dias)) NA_integer_
                 else as.integer(prazo_dias)
    linhas[[length(linhas) + 1]] <<- data.frame(
      peca          = as.character(peca),
      peca_norm     = .norm_peca(as.character(peca)),
      peca_canonica = if (is.na(peca_canonica)) as.character(peca) else peca_canonica,
      prazo_dias    = prazo_int,
      prorrogavel   = if (is.null(prorrogavel)) NA else as.logical(prorrogavel),
      base          = if (is.null(base)) NA_character_ else paste(unlist(base), collapse = "; "),
      origem        = origem,
      e_sinonimo      = e_sinonimo,
      origem_sinonimo = origem_sinonimo,
      sem_prazo_normativo = isTRUE(sem_prazo) || is.na(prazo_int),
      stringsAsFactors = FALSE
    )
  }

  # Adiciona uma peca e, se houver, seus sinonimos (apontando a peca canonica).
  add_item <- function(peca, prazo_dias, prorrogavel, base, origem,
                       sinonimos = NULL, sem_prazo = FALSE) {
    add(peca, prazo_dias, prorrogavel, base, origem, sem_prazo = sem_prazo)
    if (!is.null(sinonimos)) {
      for (s in sinonimos) {
        # Sinonimo herda o prazo/base da peca canonica; `peca_canonica` guarda o
        # nome canonico para rastreabilidade e `origem_sinonimo` a origem propria.
        add(s$termo, prazo_dias, prorrogavel, base,
            origem = origem, e_sinonimo = TRUE,
            origem_sinonimo = if (is.null(s$origem)) NA_character_ else s$origem,
            peca_canonica = as.character(peca), sem_prazo = sem_prazo)
      }
    }
  }

  # analise_de_pecas (unico bloco com sinonimos no momento)
  for (it in norm$analise_de_pecas$itens) {
    add_item(it$peca, it$prazo_dias, it$prorrogavel, it$base,
             "analise_de_pecas", it$sinonimos,
             sem_prazo = isTRUE(it$sem_prazo_normativo))
  }
  # complementacao$relogios
  if (!is.null(norm$complementacao$relogios)) {
    for (r in norm$complementacao$relogios) {
      add_item(r$relogio, r$prazo_dias, r$prorrogavel, r$base, "complementacao",
               r$sinonimos)
    }
  }
  # manifestacao_conclusiva
  if (!is.null(norm$manifestacao_conclusiva$itens)) {
    for (it in norm$manifestacao_conclusiva$itens) {
      add_item(it$fase, it$prazo_dias, NULL, it$base, "manifestacao_conclusiva",
               it$sinonimos)
    }
  }
  # outros_prazos
  if (!is.null(norm$outros_prazos)) {
    for (it in norm$outros_prazos) {
      add_item(it$situacao, it$prazo_dias, it$prorrogavel, it$base,
               "outros_prazos", it$sinonimos)
    }
  }

  if (length(linhas) == 0) {
    return(data.frame(peca = character(0), peca_norm = character(0),
                      peca_canonica = character(0),
                      prazo_dias = integer(0), prorrogavel = logical(0),
                      base = character(0), origem = character(0),
                      e_sinonimo = logical(0), origem_sinonimo = character(0),
                      sem_prazo_normativo = logical(0),
                      stringsAsFactors = FALSE))
  }
  do.call(rbind, linhas)
}

# -----------------------------------------------------------------------------
# Consulta
# -----------------------------------------------------------------------------

#' Regra de contagem de prazos (dias corridos/uteis) da normativa.
#' @param norm list de carregar_normativas()
#' @return list(unidade, observacao, base)
regra_contagem <- function(norm) {
  norm$regra_contagem
}

#' Criterio de encerramento do ato (ex.: data da ultima assinatura).
#' @param norm list de carregar_normativas()
#' @return list(criterio, observacao)
criterio_encerramento <- function(norm) {
  norm$encerramento_do_ato
}

#' Consulta o prazo (em dias) para uma peca/tipo, por correspondencia de nome.
#'
#' A correspondencia e feita sobre nomes normalizados (sem acento, minusculas).
#' Estrategia (da mais estrita a mais frouxa), para casar tipos reais de
#' documento (ex.: "Parecer FCA Arq - IN 06/2025 434") com as pecas da norma
#' (ex.: "FCA"):
#'   1. igualdade exata do nome normalizado;
#'   2. a peca da norma aparece como palavra/sigla dentro do tipo consultado;
#'   3. o tipo consultado aparece dentro da peca da norma.
#' Quando ha multiplos candidatos, prefere o de nome mais longo (mais especifico).
#'
#' NAO inventa prazo: se nada casar, retorna encontrado = FALSE. Quando o match
#' ocorre por um SINONIMO, `e_sinonimo = TRUE` e `origem_sinonimo` registra se a
#' equivalencia vem da propria IN (ex.: "IN 06/2025 sec. 7") ou de interpretacao
#' do pesquisador — para a avaliacao separar os casos.
#'
#' @param norm list de carregar_normativas()
#' @param peca character — nome da peca ou tipo do documento a consultar
#' @return list(encontrado, peca_norma, prazo_dias, prorrogavel, base, origem,
#'              metodo_match, e_sinonimo, origem_sinonimo)
consultar_prazo <- function(norm, peca) {
  vazio <- list(encontrado = FALSE, peca_norma = NA_character_,
                termo_casado = NA_character_,
                prazo_dias = NA_integer_, prorrogavel = NA, base = NA_character_,
                origem = NA_character_, metodo_match = NA_character_,
                e_sinonimo = FALSE, origem_sinonimo = NA_character_,
                sem_prazo_normativo = FALSE)
  if (is.null(peca) || length(peca) == 0 || is.na(peca) || peca == "") return(vazio)

  idx <- norm$.indice_pecas
  if (nrow(idx) == 0) return(vazio)

  alvo <- .norm_peca(peca)

  # 1. Igualdade exata
  hit <- which(idx$peca_norm == alvo)
  metodo <- "exato"

  # 2. Peca da norma contida no tipo consultado (ex.: "fca" dentro de
  #    "parecer fca arq - in 06/2025 434"). Usa limites de palavra p/ siglas
  #    curtas evitarem casar dentro de outra palavra.
  if (length(hit) == 0) {
    contidos <- vapply(idx$peca_norm, function(pn) {
      if (pn == "") return(FALSE)
      padrao <- paste0("\\b", stringr::str_escape(pn), "\\b")
      stringr::str_detect(alvo, padrao)
    }, logical(1))
    hit <- which(contidos)
    metodo <- "peca_no_tipo"
  }

  # 3. Tipo consultado contido na peca da norma
  if (length(hit) == 0) {
    contidos <- vapply(idx$peca_norm, function(pn) {
      if (pn == "" || alvo == "") return(FALSE)
      stringr::str_detect(pn, stringr::fixed(alvo))
    }, logical(1))
    hit <- which(contidos)
    metodo <- "tipo_na_peca"
  }

  if (length(hit) == 0) return(vazio)

  # Multiplos candidatos: prefere o nome de peca mais especifico (mais longo)
  if (length(hit) > 1) {
    hit <- hit[order(nchar(idx$peca_norm[hit]), decreasing = TRUE)][1]
  }

  list(
    encontrado      = TRUE,
    peca_norma      = idx$peca_canonica[hit],
    termo_casado    = idx$peca[hit],
    prazo_dias      = idx$prazo_dias[hit],
    prorrogavel     = idx$prorrogavel[hit],
    base            = idx$base[hit],
    origem          = idx$origem[hit],
    metodo_match    = metodo,
    e_sinonimo      = isTRUE(idx$e_sinonimo[hit]),
    origem_sinonimo = idx$origem_sinonimo[hit],
    sem_prazo_normativo = isTRUE(idx$sem_prazo_normativo[hit])
  )
}
