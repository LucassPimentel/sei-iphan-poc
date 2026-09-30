# =============================================================================
# Testes para R/07_event_log.R (event log / process mining)
# Estruturas sinteticas espelham os dados REAIS (scraping/entidades/classificacao)
# e um smoke test roda contra um JSON de scraping real, se presente.
# =============================================================================

library(testthat)

.raiz <- if (file.exists("R/07_event_log.R")) "." else ".."
suppressPackageStartupMessages({
  library(dplyr); library(tibble); library(purrr); library(stringr)
  library(lubridate); library(jsonlite)
})
source(file.path(.raiz, "R/07_event_log.R"))

# -----------------------------------------------------------------------------
# Fixtures sinteticas (mesma forma dos JSONs reais)
# -----------------------------------------------------------------------------

.scraping_fake <- function() {
  list(
    cabecalho = list(numero_processo = "01450.999999/2026-99"),
    documentos = data.frame(
      numero_documento = c("100", "200", "300"),
      tipo             = c("Despacho 1", "Parecer FCA Arq", "Arquivo ADA"),
      data_documento   = c("06/03/2026", "13/03/2026", "04/03/2026"),
      data_inclusao    = c("06/03/2026", "13/03/2026", "04/03/2026"),
      unidade_sigla    = c("CGM", "COTEC IPHAN-BA", "SAIP"),
      unidade_nome     = c("Coord", "Coord Tec", "Sistema"),
      acesso           = c("publico", "publico", "restrito"),
      url_documento    = c("http://x", "http://y", NA),
      stringsAsFactors = FALSE
    ),
    andamentos = data.frame(
      data_hora     = c("12/03/2026 10:00", "12/03/2026 09:00", "10/03/2026 08:00"),
      unidade_sigla = c("CGM", "CGLic", "CGLic"),
      unidade_nome  = c("Coord", "CGLic", "CGLic"),
      descricao     = c("Processo recebido na unidade",
                        "Processo remetido pela unidade CGM",
                        "Conclus\u00e3o do processo na unidade"),
      status        = c("concluido", "concluido", "concluido"),
      id_atividade  = c("30", "20", "10"),
      stringsAsFactors = FALSE
    )
  )
}

.classificacao_fake <- function() {
  list(classificacao = data.frame(
    numero_documento = c("100", "200"),
    tipo             = c("Despacho 1", "Parecer FCA Arq"),
    classe_predita   = c("encaminhamento", "manifestacao_parecer"),
    escore           = c(7, 15),
    margem           = c(4, 13),
    stringsAsFactors = FALSE
  ))
}

.entidades_fake <- function() {
  list(entidades = data.frame(
    numero_documento  = c("100", "200"),
    tipo              = c("Despacho 1", "Parecer FCA Arq"),
    unidade_emissora  = c("CGM", "COTEC"),
    remetente_nome    = c("Fulano", "Beltrano"),
    n_signatarios     = c(1L, 1L),
    destinatario_nome = c("Sicrano", NA),
    n_destinatarios   = c(1L, 0L),
    prazo_data_iso    = c("2026-03-21", NA),
    prazo_ok          = c(TRUE, FALSE),
    prazo_metodo      = c("explicito", NA),
    stringsAsFactors  = FALSE
  ))
}

# -----------------------------------------------------------------------------
# Montagem basica do log
# -----------------------------------------------------------------------------

test_that("monta event log com eventos de documento e tramitacao", {
  r <- construir_event_log_processo(.scraping_fake(), persistir = FALSE)
  expect_s3_class(r$event_log, "data.frame")
  # 3 documentos + 3 andamentos
  expect_equal(nrow(r$event_log), 6L)
  expect_equal(sum(r$event_log$fonte == "documento"), 3L)
  expect_equal(sum(r$event_log$fonte == "tramitacao"), 3L)
  expect_true(all(c("case_id", "activity", "timestamp") %in% names(r$event_log)))
  expect_equal(unique(r$event_log$case_id), "01450.999999/2026-99")
})

test_that("timestamp de documento tem precisao de dia e de tramitacao de minuto", {
  r <- construir_event_log_processo(.scraping_fake(), persistir = FALSE)
  doc <- r$event_log[r$event_log$fonte == "documento", ]
  tram <- r$event_log[r$event_log$fonte == "tramitacao", ]
  expect_true(all(doc$precisao_temporal == "dia"))
  expect_true(all(tram$precisao_temporal == "minuto"))
  # documento: AAAA-MM-DD ; tramitacao: AAAA-MM-DDTHH:MM:SS
  expect_true(all(grepl("^\\d{4}-\\d{2}-\\d{2}$", doc$timestamp)))
  expect_true(all(grepl("^\\d{4}-\\d{2}-\\d{2}T", tram$timestamp)))
})

test_that("log e ordenado cronologicamente", {
  r <- construir_event_log_processo(.scraping_fake(), persistir = FALSE)
  ts <- r$event_log$timestamp
  # A ordenacao ISO como string coincide com a cronologica; sem NA no fixture
  expect_false(is.unsorted(ts))
  expect_equal(r$event_log$ordem, seq_len(nrow(r$event_log)))
})

# -----------------------------------------------------------------------------
# Tramitacao: aresta origem -> destino
# -----------------------------------------------------------------------------

test_that("extrai aresta origem->destino do 'Processo remetido pela unidade'", {
  r <- construir_event_log_processo(.scraping_fake(), persistir = FALSE)
  expect_equal(nrow(r$arestas), 1L)
  expect_equal(r$arestas$origem, "CGM")
  expect_equal(r$arestas$destino, "CGLic")
})

test_that("rotulos de tramitacao sao normalizados", {
  r <- construir_event_log_processo(.scraping_fake(), persistir = FALSE)
  tram <- r$event_log[r$event_log$fonte == "tramitacao", ]
  expect_true(any(tram$activity == "tramitacao :: recebido"))
  expect_true(any(tram$activity == "tramitacao :: remetido"))
  expect_true(any(tram$activity == "tramitacao :: concluido"))
})

# -----------------------------------------------------------------------------
# Classe: baseline, gabarito e ausencia
# -----------------------------------------------------------------------------

test_that("activity combina classe do baseline com o tipo", {
  r <- construir_event_log_processo(.scraping_fake(),
                                    classificacao = .classificacao_fake(),
                                    persistir = FALSE)
  ev100 <- r$event_log[r$event_log$documento == "100" &
                         r$event_log$fonte == "documento", ]
  expect_equal(ev100$classe, "encaminhamento")
  expect_equal(ev100$activity, "encaminhamento :: Despacho 1")
  expect_equal(ev100$fonte_classe, "baseline")
})

test_that("documento sem classe entra como 'juntada' (anexo/restrito)", {
  r <- construir_event_log_processo(.scraping_fake(),
                                    classificacao = .classificacao_fake(),
                                    persistir = FALSE)
  ev300 <- r$event_log[r$event_log$documento == "300" &
                         r$event_log$fonte == "documento", ]
  expect_true(is.na(ev300$classe))
  expect_equal(ev300$activity, "juntada :: Arquivo ADA")
  expect_equal(ev300$acesso, "restrito")
})

test_that("gabarito (classe_final) tem precedencia sobre o baseline", {
  gab <- tibble(
    numero_processo  = "01450.999999/2026-99",
    numero_documento = "100",
    classe_final     = "solicitacao"
  )
  r <- construir_event_log_processo(.scraping_fake(),
                                    classificacao = .classificacao_fake(),
                                    gabarito = gab, persistir = FALSE)
  ev100 <- r$event_log[r$event_log$documento == "100" &
                         r$event_log$fonte == "documento", ]
  expect_equal(ev100$classe, "solicitacao")
  expect_equal(ev100$fonte_classe, "gabarito")
})

# -----------------------------------------------------------------------------
# Entidades enriquecem o evento; unidade emissora tem precedencia
# -----------------------------------------------------------------------------

test_that("entidades enriquecem o evento de documento", {
  r <- construir_event_log_processo(.scraping_fake(),
                                    entidades = .entidades_fake(),
                                    persistir = FALSE)
  ev100 <- r$event_log[r$event_log$documento == "100" &
                         r$event_log$fonte == "documento", ]
  expect_equal(ev100$remetente, "Fulano")
  expect_equal(ev100$destinatario, "Sicrano")
  expect_equal(ev100$prazo_data_iso, "2026-03-21")
  # unidade emissora (entidades) tem precedencia sobre a unidade da lista
  expect_equal(ev100$unidade, "CGM")
})

# -----------------------------------------------------------------------------
# Robustez
# -----------------------------------------------------------------------------

test_that("funciona sem entidades/classificacao (so scraping)", {
  r <- construir_event_log_processo(.scraping_fake(), persistir = FALSE)
  expect_equal(nrow(r$event_log), 6L)
  # sem classificacao: todos os documentos sem classe
  doc <- r$event_log[r$event_log$fonte == "documento", ]
  expect_true(all(is.na(doc$classe)))
})

test_that("data invalida vira timestamp NA e vai para o fim", {
  scr <- .scraping_fake()
  scr$documentos$data_documento[1] <- "data-invalida"
  r <- construir_event_log_processo(scr, persistir = FALSE)
  # o evento com timestamp NA deve ser o ultimo
  expect_true(is.na(r$event_log$timestamp[nrow(r$event_log)]))
})

test_that("scraping como caminho de JSON tambem funciona", {
  tmp <- tempfile(fileext = ".json")
  write_json(.scraping_fake(), tmp, auto_unbox = TRUE, na = "null")
  on.exit(unlink(tmp), add = TRUE)
  r <- construir_event_log_processo(tmp, persistir = FALSE)
  expect_equal(nrow(r$event_log), 6L)
})

test_that("meta registra fontes, limitacoes e contagens", {
  r <- construir_event_log_processo(.scraping_fake(),
                                    classificacao = .classificacao_fake(),
                                    persistir = FALSE)
  expect_true(r$meta$fontes$scraping)
  expect_true(r$meta$fontes$classificacao)
  expect_false(r$meta$fontes$entidades)
  expect_equal(r$meta$eventos_totais, 6L)
  expect_equal(r$meta$docs_restritos, 1L)
  expect_true(length(r$meta$limitacoes) >= 4)
})

# -----------------------------------------------------------------------------
# Smoke test contra dados reais (se presentes)
# -----------------------------------------------------------------------------

test_that("smoke: constroi log a partir de scraping real (se existir)", {
  dir_proc <- file.path(.raiz, "data/processed")
  # scraping real = arquivos {proc}_{data}.json SEM sufixo de etapa
  arquivos <- list.files(dir_proc, pattern = "^01450_.*\\.json$", full.names = TRUE)
  arquivos <- arquivos[!grepl("_(conteudo|textmining|entidades|classificacao|eventlog|local)_",
                              basename(arquivos))]
  skip_if(length(arquivos) == 0, "Nenhum JSON de scraping real disponivel.")

  alvo <- arquivos[1]
  r <- construir_event_log_processo(alvo, persistir = FALSE)
  expect_gt(nrow(r$event_log), 0)
  expect_true(all(c("case_id", "activity", "timestamp") %in% names(r$event_log)))
  # deve haver ao menos um evento de documento e um de tramitacao
  expect_gt(sum(r$event_log$fonte == "documento"), 0)
  expect_gt(sum(r$event_log$fonte == "tramitacao"), 0)
})
