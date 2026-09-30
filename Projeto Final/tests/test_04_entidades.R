# =============================================================================
# Testes para R/04_entidades.R
# Baseados em padroes REAIS observados nos documentos do processo 01450.002827.
# =============================================================================

library(testthat)
source("../R/04_entidades.R")

# -----------------------------------------------------------------------------
# Destinatario
# -----------------------------------------------------------------------------

test_that("extrai destinatario simples (Ao Senhor NOME cargo Assunto)", {
  txt <- "Processo n\u00ba 01450.002827/2026-60 Ao Senhor FELLIPE DECRESCENZO ANDRADE AMARAL Coordena\u00e7\u00e3o T\u00e9cnica da Superintend\u00eancia do IPHAN na Bahia Assunto: Ficha de Caracteriza\u00e7\u00e3o."
  r <- extrair_destinatario(txt)
  expect_true(r$destinatario_encontrado)
  expect_equal(r$destinatario_nome, "FELLIPE DECRESCENZO ANDRADE AMARAL")
  expect_match(r$destinatario_cargo, "Coordena\u00e7\u00e3o T\u00e9cnica")
})

test_that("aceita conector 'E' no meio do nome", {
  txt <- "Ao Senhor HERMANO FABRICIO OLIVEIRA GUANAIS E QUEIROZ Superintendente Superintend\u00eancia do IPHAN na Bahia Assunto: PAPIPA."
  r <- extrair_destinatario(txt)
  expect_equal(r$destinatario_nome, "HERMANO FABRICIO OLIVEIRA GUANAIS E QUEIROZ")
  expect_match(r$destinatario_cargo, "Superintendente")
})

test_that("nao engole a palavra seguinte quando ha 'E' inicial (empresa)", {
  txt <- "Ao Senhor ALMIR DO CARMO BEZERRA NWG Energias do Brasil Participa\u00e7\u00f5es LTDA. E-mail: x Assunto: PAPIPA."
  r <- extrair_destinatario(txt)
  # o nome nao deve incluir "Energias"
  expect_false(grepl("Energias", r$destinatario_nome))
})

test_that("À Senhora tambem e reconhecido", {
  txt <- "\u00c0 Senhora MARIA FERNANDA DOS SANTOS BARROS CORREIA DE SOUZA Rua Setenta e Nove Assunto: PAPIPA."
  r <- extrair_destinatario(txt)
  expect_true(r$destinatario_encontrado)
  expect_equal(r$destinatario_nome, "MARIA FERNANDA DOS SANTOS BARROS CORREIA DE SOUZA")
})

test_that("texto sem destinatario retorna encontrado=FALSE", {
  txt <- "Despacho interno sem destinatario nominal. Encaminhe-se."
  r <- extrair_destinatario(txt)
  expect_false(r$destinatario_encontrado)
  expect_true(is.na(r$destinatario_nome))
})

test_that("captura multiplos destinatarios sem vazar o 2o no cargo do 1o", {
  txt <- paste(
    "Ao Senhor EDSON MIRANDA BORGES N\u00facleo de Patrim\u00f4nio Imaterial Coordena\u00e7\u00e3o T\u00e9cnica do IPHAN-BA",
    "\u00c0 Senhora LAYSE SOUZA COSTA Chefe de Escrit\u00f3rio T\u00e9cnico Escrit\u00f3rio T\u00e9cnico de Len\u00e7\u00f3is - ETL/IPHAN-BA",
    "Assunto: Encaminhamento."
  )
  r <- extrair_destinatario(txt)
  expect_equal(r$n_destinatarios, 2L)
  # 1o destinatario
  expect_equal(r$destinatarios[[1]]$nome, "EDSON MIRANDA BORGES")
  expect_false(grepl("LAYSE", r$destinatarios[[1]]$cargo))
  # 2o destinatario
  expect_equal(r$destinatarios[[2]]$nome, "LAYSE SOUZA COSTA")
  expect_match(r$destinatarios[[2]]$cargo, "Escrit\u00f3rio T\u00e9cnico de Len\u00e7\u00f3is")
})

# -----------------------------------------------------------------------------
# Prazo
# -----------------------------------------------------------------------------

test_that("extrai prazo legal com data ISO", {
  txt <- "Ressalta-se que o prazo legal para manifesta\u00e7\u00e3o do IPHAN encerrar-se-\u00e1 em 19/03/2026. Ressaltamos ainda..."
  r <- extrair_prazo(txt)
  expect_true(r$prazo_encontrado)
  expect_equal(r$prazo_data_original, "19/03/2026")
  expect_equal(r$prazo_data_iso, "2026-03-19")
})

test_that("prazo explicito registra metodo 'explicito'", {
  txt <- "Ressalta-se que o prazo legal para manifesta\u00e7\u00e3o do IPHAN encerrar-se-\u00e1 em 19/03/2026."
  r <- extrair_prazo(txt)
  expect_equal(r$prazo_metodo, "explicito")
})

test_that("prazo por proximidade: 'prazo assinalado (DATA)'", {
  txt <- "...devendo-se observar o prazo assinalado (19/03/2026). Cordialmente,"
  r <- extrair_prazo(txt)
  expect_true(r$prazo_encontrado)
  expect_equal(r$prazo_data_iso, "2026-03-19")
  expect_equal(r$prazo_metodo, "proximidade")
})

test_that("nao confunde data de assinatura com prazo", {
  # Data proxima de 'assinado eletronicamente' e 'as' NAO deve virar prazo
  txt <- "Documento assinado eletronicamente por Fulano, Analista, em 12/03/2026, \u00e0s 09:13."
  r <- extrair_prazo(txt)
  expect_false(r$prazo_encontrado)
})

test_that("prefere data com gatilho quando ha data de assinatura tambem", {
  txt <- paste("observar o prazo assinalado (19/03/2026).",
               "Documento assinado eletronicamente por X, em 12/03/2026, \u00e0s 09:13.")
  r <- extrair_prazo(txt)
  expect_equal(r$prazo_data_iso, "2026-03-19")
})

test_that("texto sem prazo (so rodape de assinatura) retorna encontrado=FALSE", {
  # Formato real do rodape do SEI: "em DATA, as HH:MM" — 'as' logo apos a data
  # sinaliza assinatura, nao prazo. Sem gatilho de prazo por perto.
  txt <- "Documento assinado eletronicamente por Fulano, Analista, em 12/03/2026, \u00e0s 09:13, conforme horario oficial."
  r <- extrair_prazo(txt)
  expect_false(r$prazo_encontrado)
  expect_true(is.na(r$prazo_data_iso))
})

# -----------------------------------------------------------------------------
# Remetente
# -----------------------------------------------------------------------------

test_that("extrai remetente do bloco de assinatura", {
  bloco <- "Documento assinado eletronicamente por Layse Souza Costa, Chefe do Escrit\u00f3rio T\u00e9cnico de Len\u00e7\u00f3is - BA, em 13/03/2026, \u00e0s 10:00."
  r <- extrair_remetente(bloco)
  expect_equal(r$remetente_nome, "Layse Souza Costa")
  expect_match(r$remetente_cargo, "Chefe do Escrit\u00f3rio")
  expect_equal(r$n_signatarios, 1L)
})

test_that("conta multiplos signatarios", {
  bloco <- paste("Documento assinado eletronicamente por A B, Chefe, em 01/01/2026, \u00e0s 10:00.",
                 "Documento assinado eletronicamente por C D, Coordenador, em 02/01/2026, \u00e0s 11:00.")
  r <- extrair_remetente(bloco)
  expect_equal(r$n_signatarios, 2L)
})

test_that("bloco NA retorna remetente vazio", {
  r <- extrair_remetente(NA_character_)
  expect_true(is.na(r$remetente_nome))
  expect_equal(r$n_signatarios, 0L)
})

# -----------------------------------------------------------------------------
# Unidade emissora
# -----------------------------------------------------------------------------

test_that("extrai unidade emissora do numero do oficio", {
  txt <- "Of\u00edcio n\u00ba 156/2026/ETL-BA/IPHAN-BA-IPHAN Processo n\u00ba 01450.002827/2026-60"
  u <- extrair_unidade_emissora(txt)
  expect_match(u, "ETL-BA")
})

test_that("texto sem numero de oficio retorna NA", {
  txt <- "Texto qualquer sem numero de oficio."
  expect_true(is.na(extrair_unidade_emissora(txt)))
})
