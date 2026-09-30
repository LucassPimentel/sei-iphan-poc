# =============================================================================
# main.R
# Ponto de entrada da POC de Mineracao de Processos de Licenciamento Ambiental
#
# Este arquivo carrega todos os modulos e expoe a funcao principal:
#   analisar_processo(url)
# & "C:\Program Files\R\R-4.6.1\bin\Rscript.exe" executar.R "https://sei.iphan.gov.br/sei/modulos/pesquisa/md_pesq_processo_exibir.php?iI3OtHvPArITY997V09rhsSkbDKbaYSycOHqqF2xsM0IaDkkEyJpus7kCPb435VNEAb16AAxmJKUdrsNWVIqQ1fbD0RZVpvDH361Oo6q4j5Pb3159ClCGO7vlH1u95S5"
# Uso (reproducao do experimento):
#   source("main.R")
#   resultado <- analisar_processo("https://sei.iphan.gov.br/...")
# =============================================================================

# Verificar dependencias minimas das etapas implementadas
pkgs_necessarios <- c("httr2", "rvest", "xml2", "tidyverse",
                       "lubridate", "stringr", "jsonlite", "purrr",
                       "tidytext", "stopwords", "stringi")

pkgs_faltantes <- pkgs_necessarios[!vapply(pkgs_necessarios, requireNamespace,
                                            quietly = TRUE, FUN.VALUE = logical(1))]
if (length(pkgs_faltantes) > 0) {
  message("Instalando pacotes necessarios: ", paste(pkgs_faltantes, collapse = ", "))
  install.packages(pkgs_faltantes)
}

# Carregar modulos na ordem de dependencia
# NB: R/01_amostragem.R NAO e carregado aqui de proposito. Ele e a etapa de
#     amostragem (executada uma unica vez para sortear os 5 processos) e roda
#     codigo no nivel do arquivo — instala pacman e baixa uma planilha do Google
#     Sheets ao ser carregado. analisar_processo() nao depende dele. Para
#     reproduzir a amostragem, rode R/01_amostragem.R separadamente.
source("R/utils_saida.R")
source("R/02_sei_scraping.R")
source("R/03_parser_documentos.R")
source("R/04_entidades.R")
source("R/05_text_mining.R")
source("R/06_classificacao.R")
source("R/07_event_log.R")
source("R/08_normativas.R")
source("R/09_prazos.R")
source("R/10_grafo.R")
# 11 (avaliacao) e 06b (supervisionado) sao de CONJUNTO: avaliam TODOS os
# processos rotulados de uma vez contra o gabarito, nao um processo isolado.
# Por isso NAO entram em analisar_processo(url); ficam expostos a parte
# (avaliar_processos()). Carregados aqui para ficarem disponiveis na sessao.
source("R/06b_classificacao_supervisionada.R")
source("R/utils_gabarito.R")
source("R/11_avaliacao.R")

# =============================================================================
# Funcao principal — sera expandida nas proximas etapas
# =============================================================================

#' Analisa um processo do SEI executando as etapas da POC ja implementadas.
#'
#' Etapas implementadas:
#'   [x] Etapa 1: Aquisicao e scraping estrutural
#'   [x] Etapa 2: Extracao de conteudo dos documentos nativos
#'   [x] Etapa 3: Text Mining (normalizacao, datas, tokens, TF-IDF)
#'   [x] Etapa 3b: Extracao de entidades (remetente, destinatario, unidade, prazo)
#'   [x] Etapa 4: Classificacao semantica — baseline por dicionario
#'   [x] Classificacao semantica — modelo supervisionado (de CONJUNTO; ver 06b)
#'   [x] Etapa 5: Process Mining / Event Log
#'   [x] Etapa 6: Calculo de prazos / conformidade temporal
#'   [x] Etapa 7: Grafo de tramitacao (documento=no, seta=fluxo/destinatario)
#'
#' Avaliacao contra o gabarito (fase EVALUATION do CRISP-DM) NAO faz parte de
#' analisar_processo() porque e de CONJUNTO (compara TODOS os processos rotulados
#' de uma vez). Rode-a a parte, apos ter o gabarito em data/ground_truth/:
#'   source("main.R")
#'   avaliar_processos()                          # baseline + cobertura
#'   avaliar_processos(incluir_supervisionado=TRUE)  # + modelo supervisionado
#' O relatorio e salvo em output/AAAAMMDD/avaliacao_{data}.json.
#'
#' @param url URL do processo no SEI Pesquisa Publica
#' @param extrair_conteudo Logico — executar a extracao de conteudo (Etapa 2)
#' @param text_mining Logico — executar o text mining (Etapa 3)
#' @param extrair_entidades Logico — executar a extracao de entidades (Etapa 3b)
#' @param classificar Logico — executar a classificacao baseline (Etapa 4)
#' @param event_log Logico — construir o event log (Etapa 5)
#' @param calcular_prazos Logico — apurar prazos/conformidade temporal (Etapa 6)
#' @param grafo Logico — construir o grafo organizacional (Etapa 7)
#' @param salvar_grafo Logico — renderizar o PNG do grafo (imagem estatica)
#' @param salvar_grafo_html Logico — renderizar o HTML interativo do grafo
#'        (visNetwork; layout em arvore com zoom/pan). Recomendado para leitura.
#' @return list com os resultados das etapas executadas
analisar_processo <- function(url, extrair_conteudo = TRUE, text_mining = TRUE,
                              extrair_entidades = TRUE, classificar = TRUE,
                              event_log = TRUE, calcular_prazos = TRUE,
                              grafo = TRUE, salvar_grafo = FALSE,
                              salvar_grafo_html = TRUE) {
  cat("=== POC IPHAN — Iniciando analise ===\n")
  cat("URL:", url, "\n\n")

  # ---- Etapa 1: Scraping estrutural ----
  cat("--- Etapa 1: Scraping estrutural ---\n")
  dados_scraping <- scrape_processo_sei(url)

  cat("\n[Etapa 1] Processo:", dados_scraping$cabecalho$numero_processo, "\n")
  cat("[Etapa 1] Documentos:", dados_scraping$meta$total_documentos,
      "| Andamentos:", dados_scraping$meta$total_andamentos, "\n")

  # ---- Etapa 2: Extracao de conteudo dos documentos nativos ----
  dados_conteudo <- NULL
  if (extrair_conteudo) {
    cat("\n--- Etapa 2: Extracao de conteudo ---\n")
    dados_conteudo <- extrair_conteudo_documentos(dados_scraping)
    cat("\n[Etapa 2] Nativos extraidos:", dados_conteudo$meta$conteudo_extraido,
        "de", dados_conteudo$meta$nativos_elegiveis,
        "(cobertura:", dados_conteudo$meta$cobertura_conteudo, ")\n")
  }

  # ---- Etapa 3: Text Mining ----
  dados_textmining <- NULL
  if (text_mining && !is.null(dados_conteudo)) {
    cat("\n--- Etapa 3: Text Mining ---\n")
    dados_textmining <- text_mining_processo(dados_conteudo)
    cat("\n[Etapa 3] Tokens:", dados_textmining$meta$tokens_totais,
        "| Vocabulario:", dados_textmining$meta$vocabulario_unico,
        "| Datas:", dados_textmining$meta$datas_extraidas_total, "\n")
  }

  # ---- Etapa 3b: Extracao de entidades (remetente/destinatario/unidade/prazo) ----
  dados_entidades <- NULL
  if (extrair_entidades && !is.null(dados_conteudo)) {
    cat("\n--- Etapa 3b: Extracao de entidades ---\n")
    # Reaproveita os blocos de assinatura do text mining (se disponiveis) para
    # extrair o remetente/signatario.
    blocos <- if (!is.null(dados_textmining)) dados_textmining$por_documento else NULL
    dados_entidades <- extrair_entidades_processo(dados_conteudo, blocos_assinatura = blocos)
    cat("\n[Etapa 3b] Destinatarios:", dados_entidades$meta$destinatarios_extraidos,
        "| Remetentes:", dados_entidades$meta$remetentes_extraidos,
        "| Prazos:", dados_entidades$meta$prazos_extraidos, "\n")
  }

  # ---- Etapa 4: Classificacao semantica (baseline por dicionario) ----
  dados_classificacao <- NULL
  if (classificar && !is.null(dados_conteudo)) {
    cat("\n--- Etapa 4: Classificacao semantica (baseline) ---\n")
    dados_classificacao <- classificar_processo(dados_conteudo)
    cat("\n[Etapa 4] Documentos classificados:",
        dados_classificacao$meta$documentos_processados,
        "| Indefinidos:", dados_classificacao$meta$indefinidos, "\n")
  }

  # ---- Etapa 5: Event Log (Process Mining) ----
  dados_eventlog <- NULL
  if (event_log) {
    cat("\n--- Etapa 5: Event Log (Process Mining) ---\n")
    dados_eventlog <- construir_event_log_processo(
      scraping      = dados_scraping,
      entidades     = dados_entidades,
      classificacao = dados_classificacao,
      gabarito      = NULL  # gabarito e opcional; consolide-o a parte se desejar
    )
    cat("\n[Etapa 5] Eventos:", dados_eventlog$meta$eventos_totais,
        "(docs:", dados_eventlog$meta$eventos_documento,
        "| tramitacao:", dados_eventlog$meta$eventos_tramitacao, ")",
        "| Arestas de fluxo:", dados_eventlog$meta$arestas_fluxo, "\n")
  }

  # ---- Etapa 6: Calculo de prazos / conformidade temporal ----
  dados_prazos <- NULL
  if (calcular_prazos && !is.null(dados_eventlog)) {
    cat("\n--- Etapa 6: Calculo de prazos ---\n")
    dados_prazos <- calcular_prazos_processo(event_log = dados_eventlog)
    cat("\n[Etapa 6] Relogios:", dados_prazos$meta$relogios_totais,
        "| Dentro:", dados_prazos$meta$dentro,
        "| Estourado:", dados_prazos$meta$estourado,
        "| Em curso:", dados_prazos$meta$em_curso,
        "| Indeterminado:", dados_prazos$meta$indeterminado, "\n")
  }

  # ---- Etapa 7: Grafo de tramitacao ----
  dados_grafo <- NULL
  if (grafo && !is.null(dados_eventlog)) {
    cat("\n--- Etapa 7: Grafo de tramitacao ---\n")
    dados_grafo <- construir_grafo_processo(
      event_log   = dados_eventlog,
      prazos      = dados_prazos,
      salvar_plot = salvar_grafo,
      salvar_html = salvar_grafo_html
    )
    cat("\n[Etapa 7] Documentos:", dados_grafo$meta$documentos,
        "| Arestas:", dados_grafo$meta$arestas,
        "| Avulsos ocultos:", dados_grafo$meta$avulsos_ocultados, "\n")
  }

  cat("\n=== Analise concluida ===\n")

  invisible(list(
    scraping      = dados_scraping,
    conteudo      = dados_conteudo,
    textmining    = dados_textmining,
    entidades     = dados_entidades,
    classificacao = dados_classificacao,
    event_log     = dados_eventlog,
    prazos        = dados_prazos,
    grafo         = dados_grafo
  ))
}