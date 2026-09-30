# =============================================================================
# Testes para R/09_prazos.R (calculo de prazos / conformidade temporal)
# Usa event logs sinteticos (mesma forma do JSON de 07) e as normativas reais.
# =============================================================================

library(testthat)

.raiz <- if (file.exists("R/09_prazos.R")) "." else ".."
suppressPackageStartupMessages({
  library(dplyr); library(tibble); library(purrr); library(stringr)
  library(lubridate); library(jsonlite)
})
source(file.path(.raiz, "R/08_normativas.R"))
source(file.path(.raiz, "R/09_prazos.R"))

.norm <- carregar_normativas(file.path(.raiz, "data/normativas/in_06_2025_prazos.json"))

# Helper: monta um event_log minimo (apenas eventos de documento relevantes)
.el <- function(docs) {
  list(meta = list(numero_processo = "01450.999999/2026-99"),
       event_log = docs)
}

# Um evento de documento com os campos que 09 consome.
.doc <- function(timestamp, classe, tipo, prazo_iso = NA_character_,
                 documento = NA_character_, unidade = "CGM") {
  data.frame(
    case_id = "01450.999999/2026-99", timestamp = timestamp,
    classe = classe, tipo = tipo, unidade = unidade, fonte = "documento",
    documento = if (is.na(documento)) paste0("d", substr(timestamp, 9, 10)) else documento,
    prazo_data_iso = prazo_iso, stringsAsFactors = FALSE
  )
}

# -----------------------------------------------------------------------------
# Criterio A: data-limite fixada (CGM)
# -----------------------------------------------------------------------------

test_that("relogio dentro do prazo pela data-limite (criterio A)", {
  docs <- bind_rows(
    .doc("2026-03-06", "solicitacao", "Despacho 2934", prazo_iso = "2026-03-19"),
    .doc("2026-03-13", "manifestacao_parecer", "Parecer FCA Arq - IN 06/2025 434")
  )
  r <- calcular_prazos_processo(.el(docs), normativas = .norm, persistir = FALSE)
  expect_equal(nrow(r$relogios), 1L)
  rel <- r$relogios[1, ]
  expect_equal(rel$metodo, "data_limite_cgm")
  expect_equal(rel$status, "dentro")
  expect_equal(rel$dias_decorridos, 7L)
  expect_equal(rel$excesso_dias, 0L)
})

test_that("relogio estourado quando parecer passa da data-limite", {
  docs <- bind_rows(
    .doc("2026-03-06", "solicitacao", "Despacho 2934", prazo_iso = "2026-03-19"),
    .doc("2026-03-25", "manifestacao_parecer", "Parecer FCA Arq - IN 06/2025 434")
  )
  r <- calcular_prazos_processo(.el(docs), normativas = .norm, persistir = FALSE)
  rel <- r$relogios[1, ]
  expect_equal(rel$status, "estourado")
  expect_false(rel$dentro_do_prazo)
  expect_equal(rel$excesso_dias, 6L)   # 25/03 - 19/03
})

# -----------------------------------------------------------------------------
# Opcao A: despacho SEM data-limite nao abre relogio de analise
# (pedir a peca != pedir o parecer; removido o antigo Criterio B / recontagem)
# -----------------------------------------------------------------------------

test_that("despacho sem data-limite nao abre relogio (nao reconta pela norma)", {
  docs <- bind_rows(
    .doc("2026-03-06", "encaminhamento", "Despacho X"),   # sem prazo_iso
    .doc("2026-03-18", "manifestacao_parecer", "Parecer FCA Arq - IN 06/2025 434")
  )
  r <- calcular_prazos_processo(.el(docs), normativas = .norm, persistir = FALSE)
  # Sem data-limite fixada pela unidade, o despacho nao e tratado como abertura
  # de analise -> nenhum relogio (Opcao A). Nunca vira "estourado" pela norma.
  expect_equal(nrow(r$relogios), 0L)
  expect_equal(r$meta$relogios_totais, 0L)
})

test_that("todo relogio apurado usa o criterio A (data_limite_cgm)", {
  docs <- bind_rows(
    .doc("2026-03-06", "solicitacao", "Despacho 2934", prazo_iso = "2026-03-19"),
    .doc("2026-03-13", "manifestacao_parecer", "Parecer FCA Arq - IN 06/2025 434")
  )
  r <- calcular_prazos_processo(.el(docs), normativas = .norm, persistir = FALSE)
  expect_true(all(r$relogios$metodo == "data_limite_cgm"))
})

# -----------------------------------------------------------------------------
# Complementacao: sinalizada, nao somada
# -----------------------------------------------------------------------------

test_that("relogio de complementacao e marcado (e_complementacao)", {
  docs <- bind_rows(
    .doc("2026-05-27", "exigencia_complementacao", "Despacho 7669", prazo_iso = "2026-06-25"),
    .doc("2026-06-03", "manifestacao_parecer", "Parecer PAPIPA Arq - IN 06/2025 18")
  )
  r <- calcular_prazos_processo(.el(docs), normativas = .norm, persistir = FALSE)
  rel <- r$relogios[1, ]
  expect_true(rel$e_complementacao)
  expect_equal(rel$status, "dentro")
})

# -----------------------------------------------------------------------------
# Em curso: abertura sem parecer subsequente
# -----------------------------------------------------------------------------

test_that("abertura sem parecer subsequente fica em_curso", {
  docs <- .doc("2026-03-06", "solicitacao", "Despacho 2934", prazo_iso = "2026-03-19")
  r <- calcular_prazos_processo(.el(docs), normativas = .norm, persistir = FALSE)
  rel <- r$relogios[1, ]
  expect_equal(rel$status, "em_curso")
  expect_true(is.na(rel$dentro_do_prazo))
})

# -----------------------------------------------------------------------------
# Sem despacho de abertura com data-limite: nenhum relogio
# -----------------------------------------------------------------------------

test_that("sem abertura com data-limite nem classe de abertura, nenhum relogio", {
  docs <- .doc("2026-03-13", "manifestacao_parecer", "Parecer FCA Arq")
  r <- calcular_prazos_processo(.el(docs), normativas = .norm, persistir = FALSE)
  expect_equal(nrow(r$relogios), 0L)
  expect_equal(r$meta$relogios_totais, 0L)
})

# -----------------------------------------------------------------------------
# Emparelhamento conservador: um parecer serve a um relogio
# -----------------------------------------------------------------------------

test_that("emparelhamento conservador nao reutiliza o mesmo parecer", {
  docs <- bind_rows(
    .doc("2026-03-06", "solicitacao", "Despacho A", prazo_iso = "2026-03-19"),
    .doc("2026-03-07", "solicitacao", "Despacho B", prazo_iso = "2026-03-20"),
    .doc("2026-03-10", "manifestacao_parecer", "Parecer FCA Arq 1"),
    .doc("2026-03-12", "manifestacao_parecer", "Parecer FCA Arq 2")
  )
  r <- calcular_prazos_processo(.el(docs), normativas = .norm, persistir = FALSE)
  expect_equal(nrow(r$relogios), 2L)
  # pareceres distintos para relogios distintos
  expect_false(r$relogios$documento_parecer[1] == r$relogios$documento_parecer[2])
})

# -----------------------------------------------------------------------------
# Opcao 1: janela temporal evita par cruzado entre fases
# -----------------------------------------------------------------------------

test_that("parecer alem da janela nao e emparelhado (fica em_curso)", {
  # Abertura COM data-limite (abre relogio), mas o unico parecer disponivel
  # esta a >90 dias: nao deve emparelhar por proximidade -> em_curso.
  docs <- bind_rows(
    .doc("2026-03-20", "encaminhamento", "Despacho 1657", prazo_iso = "2026-04-19"),
    .doc("2026-07-01", "manifestacao_parecer", "Parecer PAA Proj/Acomp/Arq")  # +103 dias
  )
  r <- calcular_prazos_processo(.el(docs), normativas = .norm, persistir = FALSE)
  expect_equal(nrow(r$relogios), 1L)
  expect_equal(r$relogios$status[1], "em_curso")
  # Alem da janela nao emparelha: sem parecer, nunca vira "estourado".
  expect_false(any(r$relogios$status == "estourado"))
})

test_that("parecer dentro da janela e emparelhado normalmente", {
  docs <- bind_rows(
    .doc("2026-03-20", "encaminhamento", "Despacho 1657", prazo_iso = "2026-04-19"),
    .doc("2026-04-10", "manifestacao_parecer", "Parecer PAA Proj/Acomp/Arq")  # +21 dias
  )
  r <- calcular_prazos_processo(.el(docs), normativas = .norm, persistir = FALSE)
  expect_equal(nrow(r$relogios), 1L)
  expect_equal(r$relogios$documento_parecer[1], "d10")   # emparelhou o parecer
  expect_equal(r$relogios$status[1], "dentro")           # 10/04 <= 19/04
})

# -----------------------------------------------------------------------------
# PAPIPA: peca sem prazo normativo -> sem data-limite, relogio e descartado
# -----------------------------------------------------------------------------

test_that("PAPIPA sem data-limite e descartado (nao inventa prazo)", {
  docs <- bind_rows(
    .doc("2026-05-27", "encaminhamento", "Despacho X"),   # sem prazo_iso
    .doc("2026-06-03", "manifestacao_parecer", "Parecer PAPIPA Arq - IN 06/2025 18")
  )
  r <- calcular_prazos_processo(.el(docs), normativas = .norm, persistir = FALSE)
  # PAPIPA nao tem prazo normativo e nao ha data-limite -> nenhum relogio
  expect_equal(nrow(r$relogios), 0L)
})

test_that("PAPIPA COM data-limite da CGM e apurado pelo criterio A", {
  docs <- bind_rows(
    .doc("2026-05-27", "encaminhamento", "Despacho 7669", prazo_iso = "2026-06-25"),
    .doc("2026-06-03", "manifestacao_parecer", "Parecer PAPIPA Arq - IN 06/2025 18")
  )
  r <- calcular_prazos_processo(.el(docs), normativas = .norm, persistir = FALSE)
  expect_equal(nrow(r$relogios), 1L)
  rel <- r$relogios[1, ]
  expect_equal(rel$metodo, "data_limite_cgm")
  expect_equal(rel$peca_norma, "PAPIPA")
  expect_equal(rel$status, "dentro")
})

# -----------------------------------------------------------------------------
# Smoke: event log real (se existir)
# -----------------------------------------------------------------------------

test_that("smoke: apura prazos de um event log real (se existir)", {
  dir_proc <- file.path(.raiz, "data/processed")
  arquivos <- list.files(dir_proc, pattern = "_eventlog_.*\\.json$", full.names = TRUE)
  skip_if(length(arquivos) == 0, "Nenhum event log real disponivel.")
  r <- calcular_prazos_processo(arquivos[1], normativas = .norm, persistir = FALSE)
  expect_true(is.list(r))
  expect_true(all(c("relogios", "meta") %in% names(r)))
  # status validos
  if (nrow(r$relogios) > 0) {
    expect_true(all(r$relogios$status %in%
                      c("dentro", "estourado", "em_curso", "indeterminado")))
  }
})
