# =============================================================================
# tests/test_03_parser_documentos.R
# Testes unitarios para o modulo de extracao de conteudo (03_parser_documentos.R)
#
# Os testes das funcoes puras (deteccao de tipo nativo, extracao de texto de HTML)
# nao dependem de rede. Testes que exigem requisicao HTTP sao marcados e podem
# ser pulados em ambiente offline.
# =============================================================================

library(testthat)
library(rvest)
library(xml2)
library(stringr)
library(jsonlite)
library(tibble)

# Localiza a raiz do projeto de forma robusta (testthat muda o working dir)
.raiz <- if (file.exists("R/03_parser_documentos.R")) "." else ".."
source(file.path(.raiz, "R/03_parser_documentos.R"))

# =============================================================================
# Config
# =============================================================================

test_that("config de extracao carrega e tem tipos nativos", {
  config <- carregar_config_conteudo(file.path(.raiz, "config/extracao_conteudo.json"))
  expect_true(is.list(config))
  expect_true(!is.null(config$tipos_nativos_extrair))
  expect_true("Despacho" %in% config$tipos_nativos_extrair)
  expect_true("Parecer" %in% config$tipos_nativos_extrair)
})

# =============================================================================
# Deteccao de tipo nativo
# =============================================================================

tipos_nativos_fixt <- c("Despacho", "Oficio", "Ofício", "Parecer")

test_that("Despacho com numero e reconhecido como nativo", {
  expect_true(.e_tipo_nativo("Despacho 1619", tipos_nativos_fixt))
})

test_that("Oficio com e sem acento e reconhecido", {
  expect_true(.e_tipo_nativo("Ofício 463", tipos_nativos_fixt))
  expect_true(.e_tipo_nativo("Oficio 463", tipos_nativos_fixt))
})

test_that("Parecer e reconhecido como nativo", {
  expect_true(.e_tipo_nativo("Parecer FCA Arq - IN 06/2025 265", tipos_nativos_fixt))
  expect_true(.e_tipo_nativo("Parecer Técnico 2226", tipos_nativos_fixt))
})

test_that("Anexos nao sao reconhecidos como nativos", {
  expect_false(.e_tipo_nativo("Arquivo ADA", tipos_nativos_fixt))
  expect_false(.e_tipo_nativo("Anexo", tipos_nativos_fixt))
  expect_false(.e_tipo_nativo("E-mail", tipos_nativos_fixt))
  expect_false(.e_tipo_nativo("Recibo", tipos_nativos_fixt))
  expect_false(.e_tipo_nativo("Termo de Referência Específico IN 06/2025 541", tipos_nativos_fixt))
  expect_false(.e_tipo_nativo("Portaria N° 85", tipos_nativos_fixt))
})

test_that("tipo vazio ou NA nao e nativo", {
  expect_false(.e_tipo_nativo("", tipos_nativos_fixt))
  expect_false(.e_tipo_nativo(NA_character_, tipos_nativos_fixt))
})

# =============================================================================
# Extracao de texto de HTML nativo
# =============================================================================

test_that("extrai texto do body removendo scripts e styles", {
  html <- "<html><head><title>SEI/IPHAN - 123 - Despacho</title><style>.x{color:red}</style></head><body><script>var a=1;</script><p>Conteudo do despacho.</p></body></html>"
  texto <- .extrair_texto_nativo(html)
  expect_true(grepl("Conteudo do despacho", texto))
  expect_false(grepl("var a=1", texto))
  expect_false(grepl("color:red", texto))
})

test_that("extrai titulo no padrao SEI", {
  html <- "<html><head><title>SEI/IPHAN - 7119470 - Despacho</title></head><body>x</body></html>"
  titulo <- .extrair_titulo_nativo(html)
  expect_equal(titulo, "SEI/IPHAN - 7119470 - Despacho")
})

test_that("body vazio retorna string vazia", {
  html <- "<html><head><title>t</title></head><body></body></html>"
  texto <- .extrair_texto_nativo(html)
  expect_equal(texto, "")
})

# =============================================================================
# Processamento de documento (regras sem rede)
# =============================================================================

test_that("documento restrito e registrado como indisponivel sem requisicao", {
  doc <- list(numero_documento="999", tipo="Documentação",
              acesso="restrito", url_documento=NA_character_)
  r <- .processar_documento(doc, tipos_nativos_fixt)
  expect_equal(r$motivo, "documento_restrito")
  expect_false(r$conteudo_extraido)
})

test_that("documento sem URL e registrado", {
  doc <- list(numero_documento="999", tipo="Despacho 1",
              acesso="publico", url_documento=NA_character_)
  r <- .processar_documento(doc, tipos_nativos_fixt)
  expect_equal(r$motivo, "sem_url")
  expect_false(r$conteudo_extraido)
})

test_that("anexo publico e listado mas nao extraido", {
  doc <- list(numero_documento="7117317", tipo="Arquivo ADA",
              acesso="publico",
              url_documento="https://exemplo/doc")
  r <- .processar_documento(doc, tipos_nativos_fixt)
  expect_false(r$e_nativo)
  expect_false(r$conteudo_extraido)
  expect_equal(r$motivo, "tipo_nao_nativo")
})

# =============================================================================
# Persistencia (usa JSON de scraping ja coletado, se existir)
# =============================================================================

test_that("resultado de extracao ja persistido tem estrutura esperada", {
  caminho <- file.path(.raiz, "data/processed/01450_001620_2026_78_conteudo_20260918.json")
  skip_if_not(file.exists(caminho), "JSON de conteudo ainda nao gerado")
  saida <- jsonlite::fromJSON(caminho, simplifyVector = TRUE)
  expect_true(!is.null(saida$meta$total_documentos))
  expect_true(!is.null(saida$meta$nativos_elegiveis))
  expect_true(!is.null(saida$meta$cobertura_conteudo))
  expect_true(is.data.frame(saida$documentos_conteudo))
})