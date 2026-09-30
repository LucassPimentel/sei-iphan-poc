# =============================================================================
# Testes para R/utils_saida.R (pastas por data)
# =============================================================================

library(testthat)

.raiz <- if (file.exists("R/utils_saida.R")) "." else ".."
source(file.path(.raiz, "R/utils_saida.R"))

test_that("dir_saida_dia cria e retorna base/AAAAMMDD", {
  base <- file.path(tempdir(), paste0("saida_", as.integer(runif(1, 1, 1e6))))
  on.exit(unlink(base, recursive = TRUE), add = TRUE)
  d <- dir_saida_dia(base, data = "20260101")
  expect_true(dir.exists(d))
  expect_equal(basename(d), "20260101")
  # compara normalizando separadores (Windows mistura / e \\)
  expect_equal(normalizePath(dirname(d), mustWork = FALSE),
               normalizePath(base, mustWork = FALSE))
})

test_that("dir_saida_dia aceita Date/POSIXct", {
  base <- file.path(tempdir(), paste0("saida_", as.integer(runif(1, 1, 1e6))))
  on.exit(unlink(base, recursive = TRUE), add = TRUE)
  d <- dir_saida_dia(base, data = as.Date("2026-03-15"))
  expect_equal(basename(d), "20260315")
})

test_that("nome_artefato monta {proc}_{etapa}_{data}.{ext}", {
  expect_equal(nome_artefato("01450_1_2026", "entidades", "json", data = "20260101"),
               "01450_1_2026_entidades_20260101.json")
  # etapa vazia (scraping) nao insere sufixo
  expect_equal(nome_artefato("01450_1_2026", "", "json", data = "20260101"),
               "01450_1_2026_20260101.json")
})

test_that("encontrar_arquivo_processo acha o mais recente em subpastas de data", {
  base <- file.path(tempdir(), paste0("proc_", as.integer(runif(1, 1, 1e6))))
  on.exit(unlink(base, recursive = TRUE), add = TRUE)

  d1 <- dir_saida_dia(base, "20260101")
  d2 <- dir_saida_dia(base, "20260202")
  writeLines("{}", file.path(d1, "01450_1_2026_entidades_20260101.json"))
  writeLines("{}", file.path(d2, "01450_1_2026_entidades_20260202.json"))

  achado <- encontrar_arquivo_processo(base, "01450_1_2026", "entidades", "json")
  expect_equal(basename(achado), "01450_1_2026_entidades_20260202.json")
})

test_that("encontrar_arquivo_processo (scraping) nao casa arquivos de etapa", {
  base <- file.path(tempdir(), paste0("proc_", as.integer(runif(1, 1, 1e6))))
  on.exit(unlink(base, recursive = TRUE), add = TRUE)

  d <- dir_saida_dia(base, "20260101")
  writeLines("{}", file.path(d, "01450_1_2026_20260101.json"))            # scraping
  writeLines("{}", file.path(d, "01450_1_2026_entidades_20260101.json"))  # etapa

  achado <- encontrar_arquivo_processo(base, "01450_1_2026", etapa = NULL, "json")
  expect_equal(basename(achado), "01450_1_2026_20260101.json")
})

test_that("encontrar_arquivo_processo tambem acha arquivos legados na raiz", {
  base <- file.path(tempdir(), paste0("proc_", as.integer(runif(1, 1, 1e6))))
  dir.create(base, recursive = TRUE)
  on.exit(unlink(base, recursive = TRUE), add = TRUE)
  writeLines("{}", file.path(base, "01450_1_2026_eventlog_20251231.json"))
  achado <- encontrar_arquivo_processo(base, "01450_1_2026", "eventlog", "json")
  expect_equal(basename(achado), "01450_1_2026_eventlog_20251231.json")
})

test_that("encontrar_arquivo_processo retorna NA quando nao ha arquivo", {
  base <- file.path(tempdir(), paste0("proc_", as.integer(runif(1, 1, 1e6))))
  dir.create(base, recursive = TRUE)
  on.exit(unlink(base, recursive = TRUE), add = TRUE)
  expect_true(is.na(encontrar_arquivo_processo(base, "inexistente", "x", "json")))
})
