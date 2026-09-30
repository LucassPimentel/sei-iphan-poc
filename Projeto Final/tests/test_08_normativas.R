# =============================================================================
# Testes para R/08_normativas.R (leitura/consulta das normativas IN 06/2025)
# =============================================================================

library(testthat)

.raiz <- if (file.exists("R/08_normativas.R")) "." else ".."
suppressPackageStartupMessages({
  library(jsonlite); library(stringi); library(stringr)
})
source(file.path(.raiz, "R/08_normativas.R"))

.norm <- carregar_normativas(file.path(.raiz, "data/normativas/in_06_2025_prazos.json"))

test_that("carrega e valida a estrutura das normativas", {
  expect_true(is.list(.norm))
  expect_true(all(c("regra_contagem", "analise_de_pecas", "complementacao",
                    "manifestacao_conclusiva") %in% names(.norm)))
  expect_gt(nrow(.norm$.indice_pecas), 0)
})

test_that("regra de contagem e dias corridos (art. 54)", {
  expect_equal(regra_contagem(.norm)$unidade, "dias_consecutivos")
})

test_that("criterio de encerramento e a ultima assinatura", {
  expect_equal(criterio_encerramento(.norm)$criterio, "data_ultima_assinatura")
})

# -----------------------------------------------------------------------------
# Consulta direta (pecas literais da norma)
# -----------------------------------------------------------------------------

test_that("FCA casa direto com 15 dias", {
  r <- consultar_prazo(.norm, "Parecer FCA Arq - IN 06/2025 434")
  expect_true(r$encontrado)
  expect_equal(r$prazo_dias, 15L)
  expect_equal(r$peca_norma, "FCA")
  expect_false(r$e_sinonimo)
})

test_that("PAIPA casa direto com 30 dias", {
  r <- consultar_prazo(.norm, "Parecer PAIPA Arq - IN 06/2025 12")
  expect_true(r$encontrado)
  expect_equal(r$prazo_dias, 30L)
  expect_false(r$e_sinonimo)
})

# -----------------------------------------------------------------------------
# Sinonimos (dado no JSON, com origem)
# -----------------------------------------------------------------------------

test_that("PAPIPA e peca PROPRIA (Nivel IV), distinta do PAIPA, sem prazo fixado", {
  # Validado contra o texto da IN 06/2025: PAPIPA (art. 25) != PAIPA (art. 23);
  # o art. 51 NAO fixa prazo de analise de projeto para o PAPIPA.
  r <- consultar_prazo(.norm, "Parecer PAPIPA Arq - IN 06/2025 18")
  expect_true(r$encontrado)              # a peca existe no indice
  expect_equal(r$peca_norma, "PAPIPA")   # canonica propria, NAO PAIPA
  expect_false(r$e_sinonimo)
  expect_true(r$sem_prazo_normativo)     # sem prazo de analise fixado na norma
  expect_true(is.na(r$prazo_dias))
})

test_that("PAIPA e PAPIPA sao pecas distintas", {
  paipa  <- consultar_prazo(.norm, "Parecer PAIPA Arq - IN 06/2025 12")
  papipa <- consultar_prazo(.norm, "Parecer PAPIPA Arq - IN 06/2025 18")
  expect_equal(paipa$peca_norma, "PAIPA")
  expect_equal(papipa$peca_norma, "PAPIPA")
  expect_false(paipa$sem_prazo_normativo)   # PAIPA tem 30 dias (art. 51, II)
  expect_true(papipa$sem_prazo_normativo)   # PAPIPA nao tem prazo fixado
})

test_that("RAIPA casa via sinonimo com origem na propria IN", {
  r <- consultar_prazo(.norm, "Parecer RAIPA Arq - IN 06/2025 50")
  expect_true(r$encontrado)
  expect_equal(r$prazo_dias, 30L)
  expect_true(r$e_sinonimo)
  expect_equal(r$origem_sinonimo, "IN 06/2025 sec. 7")
  expect_equal(r$termo_casado, "RAIPA")
})

test_that("PAA casa via sinonimo (projeto de acompanhamento)", {
  r <- consultar_prazo(.norm, "Parecer PAA Proj/Acomp/Arq - IN 06/2025 61")
  expect_true(r$encontrado)
  expect_equal(r$prazo_dias, 30L)
  expect_true(r$e_sinonimo)
  expect_equal(r$termo_casado, "PAA")
})

# -----------------------------------------------------------------------------
# Nao inventa: tipos sem prazo proprio
# -----------------------------------------------------------------------------

test_that("Parecer Tecnico generico nao casa (sem inventar prazo)", {
  r <- consultar_prazo(.norm, "Parecer Tecnico 512")
  expect_false(r$encontrado)
  expect_true(is.na(r$prazo_dias))
})

test_that("entrada vazia/NA retorna nao encontrado", {
  expect_false(consultar_prazo(.norm, "")$encontrado)
  expect_false(consultar_prazo(.norm, NA_character_)$encontrado)
})
