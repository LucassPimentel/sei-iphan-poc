# =============================================================================
# Testes para R/10_grafo.R (grafo de tramitacao: documento = no)
# =============================================================================

library(testthat)

.raiz <- if (file.exists("R/10_grafo.R")) "." else ".."
suppressPackageStartupMessages({
  library(dplyr); library(tibble); library(stringr); library(jsonlite)
  library(igraph)
})
source(file.path(.raiz, "R/utils_saida.R"))
source(file.path(.raiz, "R/10_grafo.R"))

# Helper: event_log minimo (eventos de documento) + prazos.
.el <- function(docs) {
  list(meta = list(numero_processo = "01450.999999/2026-99"),
       event_log = docs, arestas = data.frame())
}

.doc <- function(ordem, documento, tipo, classe = NA_character_,
                 unidade = "CGM", remetente = NA_character_,
                 destinatario = NA_character_, ts = "2026-03-01") {
  data.frame(fonte = "documento", ordem = ordem, documento = documento,
             tipo = tipo, classe = classe, unidade = unidade,
             remetente = remetente, destinatario = destinatario,
             timestamp = ts, stringsAsFactors = FALSE)
}

.prazos <- function(...) {
  rel <- bind_rows(...)
  list(relogios = rel)
}
.relogio <- function(abertura, parecer, status) {
  data.frame(documento_abertura = abertura, documento_parecer = parecer,
             status = status, stringsAsFactors = FALSE)
}

# -----------------------------------------------------------------------------
# Nos: um por documento; situacao por prazo/classe
# -----------------------------------------------------------------------------

test_that("cada documento vira um no com rotulo e ordem", {
  docs <- bind_rows(
    .doc(1, "100", "Despacho 1", classe = "solicitacao", remetente = "Fulano",
         destinatario = "SICRANO", ts = "2026-03-06"),
    .doc(2, "200", "Parecer FCA Arq", classe = "manifestacao_parecer",
         ts = "2026-03-13")
  )
  r <- construir_grafo_processo(.el(docs), persistir = FALSE)
  expect_equal(nrow(r$nos), 2L)
  expect_true(all(c("id", "rotulo", "situacao", "cor") %in% names(r$nos)))
  # rotulo do no traz tipo e unidade
  expect_match(r$nos$rotulo[r$nos$id == "100"], "Despacho 1")
  expect_match(r$nos$rotulo[r$nos$id == "100"], "Fulano")
})

test_that("situacao do parecer vem do prazo (dentro/fora)", {
  docs <- bind_rows(
    .doc(1, "100", "Despacho", classe = "solicitacao", ts = "2026-03-06"),
    .doc(2, "200", "Parecer FCA Arq", classe = "manifestacao_parecer", ts = "2026-03-13"),
    .doc(3, "300", "Parecer PAIPA", classe = "manifestacao_parecer", ts = "2026-04-30")
  )
  pz <- .prazos(.relogio("100", "200", "dentro"),
                .relogio("100", "300", "estourado"))
  r <- construir_grafo_processo(.el(docs), prazos = pz, persistir = FALSE)
  sit <- setNames(r$nos$situacao, r$nos$id)
  expect_equal(sit[["200"]], "dentro_prazo")
  expect_equal(sit[["300"]], "fora_prazo")
})

test_that("complementacao e avulso sao classificados", {
  docs <- bind_rows(
    .doc(1, "100", "Despacho 7669", classe = "exigencia_complementacao"),
    .doc(2, "200", "Anexo", classe = NA_character_)  # avulso
  )
  r <- construir_grafo_processo(.el(docs), incluir_avulsos = TRUE, persistir = FALSE)
  sit <- setNames(r$nos$situacao, r$nos$id)
  expect_equal(sit[["100"]], "complementacao")
  expect_equal(sit[["200"]], "avulso")
})

# -----------------------------------------------------------------------------
# Avulsos ocultados por padrao
# -----------------------------------------------------------------------------

test_that("documentacao externa (peticionamento) e resumida em demanda externa", {
  # A documentacao do interessado (unidade SAIP, sem classe) NAO e desenhada
  # crua: sequencias de peticionamento sao agrupadas num unico no de
  # "demanda externa" e contabilizadas em avulsos_ocultados. O ato do IPHAN
  # (Despacho) permanece.
  docs <- bind_rows(
    .doc(1, "100", "Recibo", classe = NA_character_, unidade = "SAIP", ts = "2026-03-01"),
    .doc(2, "200", "Arquivo ADA", classe = NA_character_, unidade = "SAIP", ts = "2026-03-02"),
    .doc(3, "300", "Despacho", classe = "solicitacao", unidade = "CGM", ts = "2026-03-06")
  )
  r <- construir_grafo_processo(.el(docs), persistir = FALSE)
  expect_equal(nrow(r$nos), 2L)                 # 1 demanda externa + 1 ato
  expect_equal(r$meta$avulsos_ocultados, 2L)    # os 2 docs de peticionamento
  expect_equal(r$meta$demandas_externas, 1L)
  expect_false("200" %in% r$nos$id)             # doc externo cru nao aparece
  expect_true("300" %in% r$nos$id)              # o ato do IPHAN permanece
})

test_that("avulso de unidade do IPHAN (sem classe) permanece como no avulso", {
  # Doc sem classe cuja unidade NAO e de peticionamento (ex.: CGM) nao vira
  # demanda externa nem e agrupado: fica como no avulso (preto), visivel.
  docs <- bind_rows(
    .doc(1, "100", "Despacho", classe = "solicitacao", unidade = "CGM"),
    .doc(2, "200", "Anexo", classe = NA_character_, unidade = "CGM")
  )
  r <- construir_grafo_processo(.el(docs), persistir = FALSE)
  expect_equal(nrow(r$nos), 2L)
  expect_equal(r$meta$avulsos_ocultados, 0L)
  expect_equal(setNames(r$nos$situacao, r$nos$id)[["200"]], "avulso")
})

test_that("incluir_avulsos = TRUE mantem os externos crus (sem agrupar)", {
  docs <- bind_rows(
    .doc(1, "100", "Despacho", classe = "solicitacao", unidade = "CGM"),
    .doc(2, "200", "Anexo", classe = NA_character_, unidade = "SAIP")
  )
  r <- construir_grafo_processo(.el(docs), incluir_avulsos = TRUE, persistir = FALSE)
  expect_equal(nrow(r$nos), 2L)
  expect_equal(r$meta$avulsos_ocultados, 0L)
  expect_true("200" %in% r$nos$id)
})

# -----------------------------------------------------------------------------
# Arestas: prazo (semantica) + espinha cronologica; rotulo = destinatario
# -----------------------------------------------------------------------------

test_that("aresta de prazo liga abertura->parecer", {
  docs <- bind_rows(
    .doc(1, "100", "Despacho", classe = "solicitacao",
         destinatario = "SICRANO", ts = "2026-03-06"),
    .doc(2, "200", "Parecer FCA Arq", classe = "manifestacao_parecer", ts = "2026-03-13")
  )
  pz <- .prazos(.relogio("100", "200", "dentro"))
  r <- construir_grafo_processo(.el(docs), prazos = pz, persistir = FALSE)
  ar_prazo <- r$arestas[r$arestas$tipo == "prazo", ]
  expect_equal(nrow(ar_prazo), 1L)
  expect_equal(ar_prazo$de, "100")
  expect_equal(ar_prazo$para, "200")
  # rotulo enriquecido: destinatario da origem ("Para: <dest>") + situacao do prazo
  expect_match(ar_prazo$rotulo, "SICRANO")
  expect_match(ar_prazo$rotulo, "no prazo")
})

test_that("espinha cronologica conecta atos em ordem", {
  # Atos que NAO disparam ligacao semantica de maior precedencia (nao sao
  # Despacho de triagem, nao ha prazos/citacoes): a espinha cronologica costura
  # os atos orfaos na ordem do tempo, 100->200->300.
  docs <- bind_rows(
    .doc(1, "100", "Oficio", classe = "encaminhamento", unidade = "IPHAN-BA", ts = "2026-03-06"),
    .doc(2, "200", "Oficio", classe = "encaminhamento", unidade = "IPHAN-BA", ts = "2026-03-10"),
    .doc(3, "300", "Nota Tecnica", classe = "informacao", unidade = "IPHAN-BA", ts = "2026-03-13")
  )
  r <- construir_grafo_processo(.el(docs), persistir = FALSE)
  expect_true(all(r$arestas$tipo == "cronologica"))
  expect_equal(nrow(r$arestas), 2L)
})

test_that("aresta ja coberta por prazo nao e duplicada na espinha", {
  docs <- bind_rows(
    .doc(1, "100", "Despacho", classe = "solicitacao", ts = "2026-03-06"),
    .doc(2, "200", "Parecer", classe = "manifestacao_parecer", ts = "2026-03-13")
  )
  pz <- .prazos(.relogio("100", "200", "dentro"))
  r <- construir_grafo_processo(.el(docs), prazos = pz, persistir = FALSE)
  # 100->200 e de prazo; nao deve haver uma cronologica 100->200 tambem
  expect_equal(sum(r$arestas$de == "100" & r$arestas$para == "200"), 1L)
  expect_equal(r$arestas$tipo[r$arestas$de == "100" & r$arestas$para == "200"], "prazo")
})

# -----------------------------------------------------------------------------
# igraph
# -----------------------------------------------------------------------------

test_that("retorna objeto igraph coerente", {
  docs <- bind_rows(
    .doc(1, "100", "Despacho", classe = "solicitacao"),
    .doc(2, "200", "Parecer", classe = "manifestacao_parecer")
  )
  r <- construir_grafo_processo(.el(docs), persistir = FALSE)
  expect_true(igraph::is_igraph(r$igraph))
  expect_true(igraph::is_directed(r$igraph))
  expect_equal(igraph::vcount(r$igraph), 2L)
})

# -----------------------------------------------------------------------------
# Smoke: event log + prazos reais (se existirem)
# -----------------------------------------------------------------------------

test_that("smoke: grafo de dados reais (se existirem)", {
  dir_proc <- file.path(.raiz, "data/processed")
  els <- list.files(dir_proc, pattern = "_eventlog_.*\\.json$",
                    full.names = TRUE, recursive = TRUE)
  skip_if(length(els) == 0, "Nenhum event log real disponivel.")
  r <- construir_grafo_processo(els[1], persistir = FALSE)
  expect_true(all(c("nos", "arestas", "igraph", "meta") %in% names(r)))
  expect_gt(nrow(r$nos), 0)
})
