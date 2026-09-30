# =============================================================================
# tests/test_05_text_mining.R
# Testes unitarios para o modulo de Text Mining (05_text_mining.R)
# Funcoes puras — nao dependem de rede.
# =============================================================================

library(testthat)

.raiz <- if (file.exists("R/05_text_mining.R")) "." else ".."
suppressPackageStartupMessages({
  library(tidytext); library(stopwords); library(stringr); library(stringi)
  library(lubridate); library(dplyr); library(tidyr); library(tibble); library(jsonlite)
})
source(file.path(.raiz, "R/05_text_mining.R"))

# =============================================================================
# Stopwords
# =============================================================================

test_that("stopwords carrega pt-br e dominio", {
  sw <- carregar_stopwords(file.path(.raiz, "config/stopwords_dominio.txt"))
  expect_true(length(sw) > 100)
  expect_true("de" %in% sw)        # stopword pt-br
  expect_true("iphan" %in% sw)     # stopword de dominio
  expect_true("processo" %in% sw)
})

# =============================================================================
# Separacao de boilerplate
# =============================================================================

test_that("separa boilerplate quando ha marcador de assinatura", {
  texto <- "Corpo do despacho aqui. Documento assinado eletronicamente por Fulano, Analista, em 05/02/2026."
  r <- separar_boilerplate(texto)
  expect_true(r$cortado)
  expect_true(grepl("Corpo do despacho", r$corpo_util))
  expect_false(grepl("assinado eletronicamente", r$corpo_util))
  expect_true(grepl("assinado eletronicamente", r$bloco_assinaturas))
})

test_that("mantem texto e sinaliza quando nao ha marcador", {
  texto <- "Texto simples sem rodape de assinatura."
  r <- separar_boilerplate(texto)
  expect_false(r$cortado)
  expect_equal(r$corpo_util, texto)
  expect_true(is.na(r$bloco_assinaturas))
})

test_that("corta no marcador de autenticidade tambem", {
  texto <- "Corpo. A autenticidade deste documento pode ser conferida no site."
  r <- separar_boilerplate(texto)
  expect_true(r$cortado)
  expect_true(grepl("Corpo", r$corpo_util))
})

test_that("texto vazio ou NA e tratado", {
  r1 <- separar_boilerplate("")
  expect_false(r1$cortado)
  r2 <- separar_boilerplate(NA_character_)
  expect_false(r2$cortado)
})

# =============================================================================
# Normalizacao
# =============================================================================

test_that("normalizacao coloca em minusculas e mantem acentos", {
  t <- normalizar_texto("Análise do PROCESSO em São Paulo")
  expect_true(grepl("análise", t))
  expect_true(grepl("são paulo", t))
  expect_false(grepl("PROCESSO", t))
})

test_that("normalizacao colapsa espacos multiplos", {
  t <- normalizar_texto("palavra1     palavra2\n\n\tpalavra3")
  expect_equal(t, "palavra1 palavra2 palavra3")
})

# =============================================================================
# Extracao de datas
# =============================================================================

test_that("extrai data numerica dd/mm/aaaa", {
  d <- extrair_datas("O prazo encerra-se em 20/02/2026 conforme norma.")
  expect_true(as.Date("2026-02-20") %in% d$data_iso)
  expect_true("numerico" %in% d$formato)
})

test_that("extrai data por extenso com 'de' antes do ano", {
  d <- extrair_datas("Brasília, 05 de fevereiro de 2026.")
  expect_true(as.Date("2026-02-05") %in% d$data_iso)
  expect_true("extenso" %in% d$formato)
})

test_that("extrai data por extenso sem 'de' antes do ano", {
  d <- extrair_datas("Brasília, 02 de março 2026.")
  expect_true(as.Date("2026-03-02") %in% d$data_iso)
})

test_that("meses com e sem acento sao reconhecidos", {
  d1 <- extrair_datas("10 de marco de 2026")
  d2 <- extrair_datas("10 de março de 2026")
  expect_true(as.Date("2026-03-10") %in% d1$data_iso)
  expect_true(as.Date("2026-03-10") %in% d2$data_iso)
})

test_that("texto sem datas retorna tibble vazio", {
  d <- extrair_datas("Nenhuma data neste texto.")
  expect_equal(nrow(d), 0)
})

test_that("datas duplicadas sao removidas", {
  d <- extrair_datas("05/02/2026 e novamente 05/02/2026")
  expect_equal(sum(d$data_original == "05/02/2026"), 1)
})

# =============================================================================
# Tokenizacao
# =============================================================================

test_that("tokenizacao remove stopwords, numeros e tokens curtos", {
  sw <- c("de", "e", "o", "a")
  tk <- tokenizar("analise de risco e 2026 no ok", "doc1", sw)
  expect_true("analise" %in% tk$token)
  expect_true("risco" %in% tk$token)
  expect_false("de" %in% tk$token)      # stopword
  expect_false("2026" %in% tk$token)    # numero isolado
  expect_false("no" %in% tk$token)      # 2 chars
  expect_false("ok" %in% tk$token)      # 2 chars
})

test_that("tokenizacao rastreia numero do documento", {
  tk <- tokenizar("parecer tecnico favoravel", "doc42", character(0))
  expect_true(all(tk$numero_documento == "doc42"))
})

test_that("texto vazio retorna tabela de tokens vazia", {
  tk <- tokenizar("", "doc1", character(0))
  expect_equal(nrow(tk), 0)
})

# =============================================================================
# TF-IDF
# =============================================================================

test_that("tfidf calcula colunas esperadas", {
  tokens <- tibble(
    numero_documento = c("d1","d1","d2","d2"),
    token = c("parecer","aprovado","oficio","pendencia")
  )
  r <- calcular_tfidf(tokens)
  expect_true(all(c("tf","idf","tf_idf") %in% names(r)))
  expect_equal(nrow(r), 4)
})

test_that("termo presente em todos os docs tem idf zero", {
  tokens <- tibble(
    numero_documento = c("d1","d2"),
    token = c("comum","comum")
  )
  r <- calcular_tfidf(tokens)
  expect_true(all(r$idf == 0))
})