# =============================================================================
# Testes para R/utils_gabarito.R (validador do gabarito)
# Nao dependem de pacotes de ML.
# =============================================================================

library(testthat)
source("../R/06_classificacao.R")   # para carregar_dicionario_classes
source("../R/utils_gabarito.R")

classes_validas <- obter_classes_validas("../config/classes_semanticas.json")

# Helper: cria um CSV de rotulagem temporario
.criar_csv <- function(linhas_df, dir) {
  caminho <- file.path(dir, paste0("PROC_rotulagem_20260101.csv"))
  if (requireNamespace("readr", quietly = TRUE)) {
    readr::write_excel_csv(linhas_df, caminho)
  } else {
    utils::write.csv(linhas_df, caminho, row.names = FALSE, fileEncoding = "UTF-8")
  }
  caminho
}

test_that("obter_classes_validas retorna as 5 classes", {
  expect_setequal(classes_validas,
    c("solicitacao","encaminhamento","manifestacao_parecer",
      "exigencia_complementacao","decisao"))
})

test_that("classe_final usa manual quando preenchida, senao predita", {
  df <- data.frame(
    numero_processo = c("P1","P1"),
    numero_documento = c("1","2"),
    tipo = c("Oficio","Parecer"),
    classe_predita = c("encaminhamento","indefinido"),
    margem = c(3, 0),
    gatilhos = c("",""),
    classe_manual = c("", "manifestacao_parecer"),
    observacao_revisor = c("",""),
    stringsAsFactors = FALSE
  )
  tmp <- tempfile(fileext = ".csv")
  if (requireNamespace("readr", quietly = TRUE)) readr::write_excel_csv(df, tmp)
  else utils::write.csv(df, tmp, row.names = FALSE, fileEncoding = "UTF-8")

  v <- validar_csv_rotulagem(tmp, classes_validas)
  expect_true(v$ok)
  expect_equal(v$dados$classe_final[1], "encaminhamento")       # veio da predita
  expect_equal(v$dados$classe_final[2], "manifestacao_parecer") # veio da manual
  expect_true(v$dados$corrigido[2])
  expect_false(v$dados$corrigido[1])
})

test_that("detecta classe_manual invalida", {
  df <- data.frame(
    numero_processo = "P1", numero_documento = "1", tipo = "Oficio",
    classe_predita = "encaminhamento", margem = 3, gatilhos = "",
    classe_manual = "classe_que_nao_existe", observacao_revisor = "",
    stringsAsFactors = FALSE
  )
  tmp <- tempfile(fileext = ".csv")
  if (requireNamespace("readr", quietly = TRUE)) readr::write_excel_csv(df, tmp)
  else utils::write.csv(df, tmp, row.names = FALSE, fileEncoding = "UTF-8")

  v <- validar_csv_rotulagem(tmp, classes_validas)
  expect_false(v$ok)
  expect_true(any(grepl("invalida", v$erros)))
})

test_that("detecta colunas ausentes", {
  df <- data.frame(numero_processo = "P1", numero_documento = "1")
  tmp <- tempfile(fileext = ".csv")
  utils::write.csv(df, tmp, row.names = FALSE, fileEncoding = "UTF-8")
  v <- validar_csv_rotulagem(tmp, classes_validas)
  expect_false(v$ok)
  expect_true(any(grepl("Colunas ausentes", v$erros)))
})

test_that("marca pendente quando classe_final e indefinido/vazio", {
  df <- data.frame(
    numero_processo = "P1", numero_documento = "1", tipo = "Despacho",
    classe_predita = "indefinido", margem = 0, gatilhos = "",
    classe_manual = "", observacao_revisor = "",
    stringsAsFactors = FALSE
  )
  tmp <- tempfile(fileext = ".csv")
  if (requireNamespace("readr", quietly = TRUE)) readr::write_excel_csv(df, tmp)
  else utils::write.csv(df, tmp, row.names = FALSE, fileEncoding = "UTF-8")

  v <- validar_csv_rotulagem(tmp, classes_validas)
  expect_true(v$dados$pendente[1])
})

test_that("consolidar_gabarito agrega multiplos CSVs e resume", {
  dir_tmp <- tempfile("gt_")
  dir.create(dir_tmp)
  on.exit(unlink(dir_tmp, recursive = TRUE))

  df1 <- data.frame(
    numero_processo = "P1", numero_documento = c("1","2"),
    tipo = c("Oficio","Parecer"),
    classe_predita = c("encaminhamento","manifestacao_parecer"),
    margem = c(3,5), gatilhos = c("",""),
    classe_manual = c("",""), observacao_revisor = c("",""),
    stringsAsFactors = FALSE
  )
  df2 <- data.frame(
    numero_processo = "P2", numero_documento = c("3"),
    tipo = c("Despacho"),
    classe_predita = c("indefinido"),
    margem = c(0), gatilhos = c(""),
    classe_manual = c("decisao"), observacao_revisor = c(""),
    stringsAsFactors = FALSE
  )
  w <- function(df, nome) {
    p <- file.path(dir_tmp, nome)
    if (requireNamespace("readr", quietly = TRUE)) readr::write_excel_csv(df, p)
    else utils::write.csv(df, p, row.names = FALSE, fileEncoding = "UTF-8")
  }
  w(df1, "P1_rotulagem_20260101.csv")
  w(df2, "P2_rotulagem_20260101.csv")

  res <- consolidar_gabarito(dir_tmp, "../config/classes_semanticas.json")
  expect_true(res$ok)
  expect_equal(res$resumo$processos, 2)
  expect_equal(res$resumo$documentos, 3)
  expect_equal(res$resumo$corrigidos_manual, 1)
  expect_equal(res$resumo$pendentes, 0)
})
