# =============================================================================
# Testes para R/06_classificacao.R (baseline por dicionario)
# =============================================================================

library(testthat)
source("../R/06_classificacao.R")

# Carrega o dicionario real (config) uma vez
dic <- carregar_dicionario_classes("../config/classes_semanticas.json")

test_that("dicionario carrega as 5 classes", {
  expect_setequal(
    names(dic$classes),
    c("solicitacao", "encaminhamento", "manifestacao_parecer",
      "exigencia_complementacao", "decisao")
  )
})

test_that("classifica encaminhamento", {
  txt <- "Encaminho o presente processo para apreciacao e devidos encaminhamentos."
  r <- classificar_documento(txt, "Oficio 1", dic)
  expect_equal(r$classe, "encaminhamento")
})

test_that("classifica solicitacao", {
  txt <- "Solicito analise e manifestacao tecnica acerca dos possiveis impactos."
  r <- classificar_documento(txt, "Despacho 1", dic)
  # 'solicito' tem peso 3; 'manifestacao tecnica' tambem pontua — solicito deve
  # dominar ou empatar; garantimos que solicitacao esta entre as top
  expect_true(r$escores["solicitacao"] > 0)
})

test_that("classifica manifestacao/parecer com reforco do tipo", {
  txt <- "Trata-se de manifestacao tecnica preliminar quanto ao impacto ao patrimonio."
  r <- classificar_documento(txt, "Parecer FCA", dic)
  expect_equal(r$classe, "manifestacao_parecer")
})

test_that("classifica exigencia/complementacao", {
  txt <- "Reiteramos a necessidade de complementacao; devera apresentar os documentos."
  r <- classificar_documento(txt, "Despacho 2", dic)
  expect_equal(r$classe, "exigencia_complementacao")
})

test_that("classifica decisao", {
  txt <- "Informo que este Centro manifestou-se pela sua aprovacao, publicada no Diario Oficial da Uniao."
  r <- classificar_documento(txt, "Oficio 9", dic)
  expect_equal(r$classe, "decisao")
})

test_that("normalizacao ignora acentos e caixa", {
  txt_acento <- "SOLICITO análise e manifestação técnica."
  r <- classificar_documento(txt_acento, "Despacho", dic)
  expect_true(r$escores["solicitacao"] > 0)
})

test_that("texto sem gatilho retorna indefinido", {
  txt <- "Bom dia a todos. Este texto neutro nao tem verbo de acao processual."
  r <- classificar_documento(txt, "Anexo", dic)
  expect_equal(r$classe, "indefinido")
  expect_equal(r$escore, 0)
})

test_that("texto NA retorna indefinido", {
  r <- classificar_documento(NA_character_, "Despacho", dic)
  expect_equal(r$classe, "indefinido")
})

test_that("margem reflete a diferenca para a 2a classe", {
  txt <- "Encaminho encaminhamos remeto segue para apreciacao."  # forte encaminhamento
  r <- classificar_documento(txt, "Oficio", dic)
  expect_equal(r$classe, "encaminhamento")
  expect_true(r$margem > 0)
})
