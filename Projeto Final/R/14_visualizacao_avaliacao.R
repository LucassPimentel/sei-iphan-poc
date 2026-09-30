# =============================================================================
# 14_visualizacao_avaliacao.R
# Visualizacao da avaliacao: Data Understanding (cobertura, distribuicao de
# classes) e Comparacao de modelos (baseline vs 3 modelos x 2 condicoes).
#
# Responsabilidade: camada de apoio a comunicacao dos resultados das etapas de
#   avaliacao (11_avaliacao.R) e classificacao supervisionada (06b). NAO faz
#   parte do pipeline principal (analisar_processo); serve para produzir as
#   figuras do artigo (fases Data Understanding e Evaluation do CRISP-DM).
#
# Decisoes de design (consistentes com 13_visualizacao_textmining.R):
#   - Toda funcao de plot SEMPRE retorna o objeto ggplot.
#   - Exibir na tela e salvar em arquivo sao OPCIONAIS e independentes.
#   - Saidas em output/AAAAMMDD/ (via utils_saida.R::dir_saida_dia).
#   - Nenhum dado e inventado: os graficos leem os artefatos ja persistidos
#     (JSONs "_conteudo_", gabarito, JSONs de comparacao de classificadores).
#
# Graficos:
#   Data Understanding
#     - plotar_cobertura_extracao()      : barras empilhadas por processo.
#     - plotar_distribuicao_classes()    : distribuicao de classes do gabarito.
#   Comparacao de modelos
#     - consolidar_comparacao_modelos()  : le/gera os 3 JSONs e monta o quadro.
#     - plotar_metricas_modelos()        : barras agrupadas, facet por metrica.
#     - plotar_f1_por_condicao()         : F1 por modelo x condicao (+baseline).
#     - plotar_matriz_confusao()         : heatmap de uma matriz de confusao.
#
# Tecnologias: ggplot2, dplyr, tidyr, forcats, jsonlite, tibble, purrr
# =============================================================================

library(ggplot2)
library(dplyr)
library(tidyr)
library(forcats)
library(jsonlite)
library(tibble)
library(purrr)

DIR_OUTPUT_PADRAO <- "output"

#' Operador auxiliar: retorna b se a for NULL/vazio.
`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

# Utilitario de pastas por data (dir_saida_dia). Carrega se ausente.
if (!exists("dir_saida_dia")) {
  .cand_utils <- c("R/utils_saida.R", "utils_saida.R")
  .cand_utils <- .cand_utils[file.exists(.cand_utils)]
  if (length(.cand_utils) > 0) source(.cand_utils[1])
}

# -----------------------------------------------------------------------------
# Salvamento (mesmo contrato do 13)
# -----------------------------------------------------------------------------

#' Salva um grafico ggplot em disco, criando o diretorio se necessario.
.salvar_grafico_aval <- function(grafico, caminho, largura, altura, dpi) {
  dir.create(dirname(caminho), showWarnings = FALSE, recursive = TRUE)
  ggsave(caminho, plot = grafico, width = largura, height = altura, dpi = dpi)
  cat("[viz-aval] Grafico salvo em:", caminho, "\n")
}

#' Resolve um caminho de saida em output/AAAAMMDD/ com um sufixo dado.
.caminho_saida_aval <- function(caminho_saida, sufixo) {
  if (!is.null(caminho_saida)) return(caminho_saida)
  dir_dia <- if (exists("dir_saida_dia")) dir_saida_dia(DIR_OUTPUT_PADRAO) else DIR_OUTPUT_PADRAO
  data_str <- format(Sys.time(), "%Y%m%d")
  file.path(dir_dia, paste0(sufixo, "_", data_str, ".png"))
}

# -----------------------------------------------------------------------------
# Data Understanding — Cobertura de extracao por processo
# -----------------------------------------------------------------------------

#' Monta a tabela de cobertura por processo a partir dos JSONs "_conteudo_".
#'
#' Para cada processo, quebra os documentos em: com texto extraido, restritos,
#' sem texto e anexos nao-nativos (do `meta` do JSON _conteudo_). E a base do
#' grafico de cobertura, central para a honestidade metodologica (a
#' indisponibilidade deve ser mensuravel).
#'
#' @param caminhos_conteudo character (JSONs "_conteudo_") ou NULL (auto).
#' @param dir_processed Diretorio dos processados.
#' @return tibble(numero_processo, categoria, n)
montar_tabela_cobertura <- function(caminhos_conteudo = NULL,
                                    dir_processed = "data/processed") {
  if (is.null(caminhos_conteudo)) {
    caminhos_conteudo <- list.files(dir_processed,
                                    pattern = "_conteudo_.*\\.json$",
                                    full.names = TRUE, recursive = TRUE)
  }
  if (length(caminhos_conteudo) == 0) {
    stop("[viz-aval] Nenhum JSON _conteudo_ encontrado em ", dir_processed)
  }

  # Um processo pode ter varias versoes de _conteudo_; fica com a mais recente
  # (nome ordenado desc, que inclui a data).
  linhas <- lapply(caminhos_conteudo, function(cam) {
    j <- jsonlite::fromJSON(cam, simplifyVector = TRUE)
    m <- j$meta
    tibble(
      numero_processo    = as.character(m$numero_processo %||% NA_character_),
      arquivo            = basename(cam),
      com_texto          = as.integer(m$conteudo_extraido %||% 0L),
      restritos          = as.integer(m$restritos %||% 0L),
      sem_texto          = as.integer(m$sem_texto %||% 0L),
      anexos_nao_nativos = as.integer(m$anexos_nao_nativos %||% 0L)
    )
  })
  bruto <- bind_rows(linhas) |>
    group_by(numero_processo) |>
    arrange(desc(arquivo)) |>
    slice_head(n = 1) |>
    ungroup() |>
    select(-arquivo)

  bruto |>
    pivot_longer(cols = c(com_texto, restritos, sem_texto, anexos_nao_nativos),
                 names_to = "categoria", values_to = "n") |>
    mutate(categoria = recode(categoria,
      com_texto          = "Com texto extraido",
      restritos          = "Restrito",
      sem_texto          = "Sem texto",
      anexos_nao_nativos = "Anexo nao-nativo"))
}

#' Plota a cobertura de extracao por processo (barras empilhadas).
#'
#' @param caminhos_conteudo character ou NULL (auto).
#' @param dir_processed Diretorio dos processados.
#' @param exibir,salvar Logicos (ambos opcionais).
#' @param caminho_saida Caminho explicito do PNG ou NULL (auto em output/).
#' @param largura,altura,dpi Parametros de ggsave.
#' @return objeto ggplot (invisivel)
plotar_cobertura_extracao <- function(
    caminhos_conteudo = NULL,
    dir_processed     = "data/processed",
    exibir            = FALSE,
    salvar            = FALSE,
    caminho_saida     = NULL,
    largura           = 9,
    altura            = 6,
    dpi               = 150
) {
  tab <- montar_tabela_cobertura(caminhos_conteudo, dir_processed)

  ordem_cat <- c("Com texto extraido", "Anexo nao-nativo", "Sem texto", "Restrito")
  tab <- tab |>
    mutate(categoria = factor(categoria, levels = ordem_cat),
           proc_curto = str_wrap_proc(numero_processo))

  grafico <- ggplot(tab, aes(x = proc_curto, y = n, fill = categoria)) +
    geom_col() +
    coord_flip() +
    scale_fill_manual(values = c(
      "Com texto extraido" = "#2c7fb8",
      "Anexo nao-nativo"   = "#f0a202",
      "Sem texto"          = "#bdbdbd",
      "Restrito"           = "#d7301f")) +
    labs(
      title = "Cobertura de extracao por processo",
      subtitle = "Documentos dos autos por status de extracao de texto",
      x = NULL, y = "Nro de documentos", fill = NULL
    ) +
    theme_minimal(base_size = 11) +
    theme(panel.grid.major.y = element_blank(), legend.position = "top")

  if (isTRUE(exibir)) print(grafico)
  if (isTRUE(salvar)) {
    caminho <- .caminho_saida_aval(caminho_saida, "cobertura_extracao")
    .salvar_grafico_aval(grafico, caminho, largura, altura, dpi)
  }
  invisible(grafico)
}

#' Encurta o numero do processo para o eixo (mantem os grupos finais).
str_wrap_proc <- function(x) {
  # 01450.001620/2026-78 -> 001620/2026-78 (parte que distingue os processos)
  sub("^\\d+\\.", "", x)
}

# -----------------------------------------------------------------------------
# Data Understanding — Distribuicao de classes do gabarito
# -----------------------------------------------------------------------------

#' Plota a distribuicao das classes do gabarito (classe_final), evidenciando o
#' desbalanceamento — limitacao a registrar (research-methodology).
#'
#' Aceita o objeto de consolidar_gabarito() OU o caminho do diretorio de
#' gabarito (roda a consolidacao). Usa apenas documentos nao pendentes.
#'
#' @param gabarito Objeto de consolidar_gabarito() OU NULL (consolida do dir).
#' @param dir_ground_truth Diretorio dos CSVs/XLSX de rotulagem.
#' @param exibir,salvar Logicos (ambos opcionais).
#' @param caminho_saida Caminho explicito ou NULL (auto em output/).
#' @param largura,altura,dpi Parametros de ggsave.
#' @return objeto ggplot (invisivel)
plotar_distribuicao_classes <- function(
    gabarito         = NULL,
    dir_ground_truth = "data/ground_truth",
    exibir           = FALSE,
    salvar           = FALSE,
    caminho_saida    = NULL,
    largura          = 8,
    altura           = 6,
    dpi              = 150
) {
  if (is.null(gabarito)) {
    if (!exists("consolidar_gabarito")) {
      stop("[viz-aval] consolidar_gabarito() indisponivel. source('R/utils_gabarito.R').")
    }
    gabarito <- consolidar_gabarito(dir_ground_truth)
  }
  gt <- gabarito$ground_truth
  if (is.null(gt) || nrow(gt) == 0) {
    stop("[viz-aval] Gabarito vazio: nada para plotar.")
  }

  tab <- gt |>
    filter(!pendente, classe_final != "", classe_final != "indefinido") |>
    count(classe_final, name = "n") |>
    arrange(desc(n))

  media <- mean(tab$n)

  grafico <- ggplot(tab,
                    aes(x = fct_reorder(classe_final, n), y = n)) +
    geom_col(fill = "#3690c0") +
    geom_hline(yintercept = media, linetype = "dashed", color = "#d7301f") +
    geom_text(aes(label = n), hjust = -0.2, size = 3) +
    annotate("text", x = 1, y = media, label = "media", color = "#d7301f",
             vjust = -0.5, hjust = -0.1, size = 3) +
    coord_flip() +
    expand_limits(y = max(tab$n) * 1.1) +
    labs(
      title = "Distribuicao das classes no gabarito",
      subtitle = "Contagem por classe_final (documentos rotulados); linha = media",
      x = NULL, y = "Nro de documentos"
    ) +
    theme_minimal(base_size = 11) +
    theme(panel.grid.major.y = element_blank())

  if (isTRUE(exibir)) print(grafico)
  if (isTRUE(salvar)) {
    caminho <- .caminho_saida_aval(caminho_saida, "distribuicao_classes_gabarito")
    .salvar_grafico_aval(grafico, caminho, largura, altura, dpi)
  }
  invisible(grafico)
}

# -----------------------------------------------------------------------------
# Comparacao de modelos — consolidacao dos 3 JSONs (baseline vs 3 x 2)
# -----------------------------------------------------------------------------

MODELOS_PADRAO <- c("logistica", "naive_bayes", "svm")

#' Le um tibble de metricas (.metric/.estimate) de uma sub-lista do JSON e
#' devolve um tibble largo (accuracy, precision, recall, f_meas).
.extrair_metricas <- function(m) {
  if (is.null(m)) return(NULL)
  # m pode vir como data.frame (jsonlite) com colunas .metric/.estimate
  df <- as.data.frame(m)
  if (!all(c(".metric", ".estimate") %in% names(df))) return(NULL)
  setNames(as.numeric(df$.estimate), df$.metric)
}

#' Consolida a comparacao baseline vs 3 modelos x 2 condicoes.
#'
#' Le os JSONs de comparar_classificadores() (um por modelo, gerados com
#' balancear = "ambos"). Se um JSON nao existir e `gerar_se_faltar = TRUE`,
#' roda comparar_classificadores() para aquele modelo (requer pacotes de ML).
#'
#' @param modelos Vetor de modelos (default logistica/naive_bayes/svm).
#' @param dir_output Diretorio onde estao/serao os JSONs de comparacao.
#' @param gerar_se_faltar Logico — rodar comparar_classificadores() se faltar.
#' @param dir_ground_truth,dir_processed Repassados a comparar_classificadores().
#' @return tibble(fonte, modelo, condicao, metrica, valor) em formato longo,
#'         onde `fonte` e "baseline" ou "supervisionado".
consolidar_comparacao_modelos <- function(
    modelos          = MODELOS_PADRAO,
    dir_output       = "output",
    gerar_se_faltar  = FALSE,
    dir_ground_truth = "data/ground_truth",
    dir_processed    = "data/processed"
) {
  achar_json <- function(modelo) {
    padrao <- paste0("^comparacao_classificacao_", modelo, "_ambos_.*\\.json$")
    cands <- list.files(dir_output, pattern = padrao, full.names = TRUE,
                        recursive = TRUE)
    if (length(cands) == 0) return(NA_character_)
    cands[order(basename(cands), decreasing = TRUE)][1]
  }

  linhas_sup <- list()
  baseline_vals <- NULL

  for (modelo in modelos) {
    cam <- achar_json(modelo)
    if (is.na(cam) && isTRUE(gerar_se_faltar)) {
      if (!exists("comparar_classificadores")) {
        stop("[viz-aval] comparar_classificadores() indisponivel. ",
             "source('R/06b_classificacao_supervisionada.R').")
      }
      cat("[viz-aval] Gerando comparacao para", modelo, "(balancear='ambos')...\n")
      comparar_classificadores(modelo = modelo, balancear = "ambos",
                               dir_ground_truth = dir_ground_truth,
                               dir_processed = dir_processed, persistir = TRUE)
      cam <- achar_json(modelo)
    }
    if (is.na(cam)) {
      cat("[viz-aval] AVISO: JSON de comparacao ausente para", modelo,
          "(pulado). Rode comparar_classificadores(modelo='", modelo,
          "', balancear='ambos') ou use gerar_se_faltar=TRUE.\n", sep = "")
      next
    }

    j <- jsonlite::fromJSON(cam, simplifyVector = TRUE)

    # Baseline: identico entre modelos; capturamos uma vez.
    if (is.null(baseline_vals)) baseline_vals <- .extrair_metricas(j$baseline)

    # Supervisionado: sub-listas nomeadas sem_balanceamento / balanceado.
    sup <- j$supervisionado
    for (cond in names(sup)) {
      vals <- .extrair_metricas(sup[[cond]]$metricas)
      if (is.null(vals)) next
      cond_rotulo <- if (cond == "balanceado") "Balanceado" else "Sem balanceamento"
      for (met in names(vals)) {
        linhas_sup[[length(linhas_sup) + 1]] <- tibble(
          fonte = "supervisionado", modelo = modelo,
          condicao = cond_rotulo, metrica = met, valor = vals[[met]])
      }
    }
  }

  dados <- bind_rows(linhas_sup)

  # Baseline entra como referencia unica (mesma em qualquer condicao/modelo).
  if (!is.null(baseline_vals)) {
    base_df <- tibble(fonte = "baseline", modelo = "baseline",
                      condicao = "Baseline",
                      metrica = names(baseline_vals),
                      valor = as.numeric(baseline_vals))
    dados <- bind_rows(base_df, dados)
  }

  if (nrow(dados) == 0) {
    stop("[viz-aval] Nenhuma metrica consolidada. Gere os JSONs de comparacao ",
         "com comparar_classificadores(modelo=..., balancear='ambos').")
  }

  # Rotulos amigaveis de metrica.
  dados |>
    mutate(metrica = recode(metrica,
      accuracy  = "Accuracy", precision = "Precision",
      recall    = "Recall",   f_meas    = "F1 (macro)"))
}

# -----------------------------------------------------------------------------
# Comparacao de modelos — accuracy por processo (fold LOPO)
# -----------------------------------------------------------------------------

#' Consolida a accuracy POR PROCESSO (fold do LOPO) dos 3 modelos x 2 condicoes.
#'
#' Le os JSONs de comparar_classificadores() e extrai, de cada cenario
#' (sem/com balanceamento), a tabela `por_processo` (accuracy de cada fold).
#' Esta e a base honesta para comunicar a INCERTEZA das metricas: com amostra
#' pequena (5 processos) nao ha intervalo de confianca confiavel; o que se pode
#' mostrar e o espalhamento do desempenho entre os processos (cada um foi teste
#' exatamente uma vez no LOPO).
#'
#' @param modelos Vetor de modelos (default logistica/naive_bayes/svm).
#' @param dir_output Diretorio onde estao os JSONs de comparacao.
#' @param gerar_se_faltar Logico — rodar comparar_classificadores() se faltar.
#' @param dir_ground_truth,dir_processed Repassados a comparar_classificadores().
#' @return tibble(modelo, condicao, numero_processo, n, accuracy)
consolidar_accuracy_por_processo <- function(
    modelos          = MODELOS_PADRAO,
    dir_output       = "output",
    gerar_se_faltar  = FALSE,
    dir_ground_truth = "data/ground_truth",
    dir_processed    = "data/processed"
) {
  achar_json <- function(modelo) {
    padrao <- paste0("^comparacao_classificacao_", modelo, "_ambos_.*\\.json$")
    cands <- list.files(dir_output, pattern = padrao, full.names = TRUE,
                        recursive = TRUE)
    if (length(cands) == 0) return(NA_character_)
    cands[order(basename(cands), decreasing = TRUE)][1]
  }

  linhas <- list()
  for (modelo in modelos) {
    cam <- achar_json(modelo)
    if (is.na(cam) && isTRUE(gerar_se_faltar)) {
      if (!exists("comparar_classificadores")) {
        stop("[viz-aval] comparar_classificadores() indisponivel. ",
             "source('R/06b_classificacao_supervisionada.R').")
      }
      cat("[viz-aval] Gerando comparacao para", modelo, "(balancear='ambos')...\n")
      comparar_classificadores(modelo = modelo, balancear = "ambos",
                               dir_ground_truth = dir_ground_truth,
                               dir_processed = dir_processed, persistir = TRUE)
      cam <- achar_json(modelo)
    }
    if (is.na(cam)) {
      cat("[viz-aval] AVISO: JSON de comparacao ausente para", modelo,
          "(pulado).\n", sep = "")
      next
    }

    j <- jsonlite::fromJSON(cam, simplifyVector = TRUE)
    sup <- j$supervisionado
    for (cond in names(sup)) {
      pp <- sup[[cond]]$por_processo
      if (is.null(pp) || NROW(pp) == 0) next
      pp <- as.data.frame(pp)
      cond_rotulo <- if (cond == "balanceado") "Balanceado" else "Sem balanceamento"
      linhas[[length(linhas) + 1]] <- tibble(
        modelo          = modelo,
        condicao        = cond_rotulo,
        numero_processo = as.character(pp$numero_processo),
        n               = as.integer(pp$n),
        accuracy        = as.numeric(pp$accuracy))
    }
  }

  dados <- bind_rows(linhas)
  if (nrow(dados) == 0) {
    stop("[viz-aval] Nenhuma accuracy por processo consolidada. Gere os JSONs ",
         "com comparar_classificadores(modelo=..., balancear='ambos').")
  }
  dados
}

#' Plota a accuracy por processo (fold LOPO) como um HEATMAP: uma linha por
#' processo, uma coluna por (modelo x condicao), cor e numero = accuracy.
#'
#' Esta forma e mais didatica que pontos sobrepostos: bate-se o olho e ve-se
#' onde cada modelo vai bem/mal e qual processo e "dificil" (linha inteira mais
#' clara). O valor escrito em cada celula da a leitura exata. A ultima linha
#' ("Media") resume a media entre os processos por coluna.
#'
#' Comunica o ESPALHAMENTO do desempenho entre os 5 processos — a resposta
#' honesta a "quao confiaveis sao as metricas" com amostra pequena. Nao usamos
#' boxplot (5 folds nao formam distribuicao) nem barra de erro/IC (falsa
#' precisao com n=5). Ver research-methodology (amostra pequena como limitacao).
#'
#' @param accuracy_proc tibble de consolidar_accuracy_por_processo() OU NULL.
#' @param ... Repassado a consolidar_accuracy_por_processo() quando NULL.
#' @param incluir_media Logico — acrescentar a linha "Media" (default TRUE).
#' @param exibir,salvar Logicos (ambos opcionais).
#' @param caminho_saida Caminho explicito ou NULL.
#' @param largura,altura,dpi Parametros de ggsave.
#' @return objeto ggplot (invisivel)
plotar_accuracy_por_processo <- function(
    accuracy_proc = NULL,
    ...,
    incluir_media = TRUE,
    exibir        = FALSE,
    salvar        = FALSE,
    caminho_saida = NULL,
    largura       = 10,
    altura        = 6,
    dpi           = 150
) {
  if (is.null(accuracy_proc)) accuracy_proc <- consolidar_accuracy_por_processo(...)

  # Coluna = modelo x condicao (abreviada); linha = processo (parte que distingue).
  dados <- accuracy_proc |>
    mutate(
      cond_abrev = ifelse(condicao == "Balanceado", "bal", "s/bal"),
      coluna     = paste0(modelo, "\n(", cond_abrev, ")"),
      processo   = sub("^\\d+\\.", "", numero_processo)
    )

  # Ordena as linhas pela accuracy media do processo (o processo mais dificil
  # em cima), para a leitura ficar imediata.
  ordem_proc <- dados |>
    group_by(processo) |>
    summarise(m = mean(accuracy), .groups = "drop") |>
    arrange(m) |>
    pull(processo)

  # Ordena as colunas: modelos juntos, condicao s/bal antes de bal.
  ordem_col <- dados |>
    distinct(modelo, cond_abrev, coluna) |>
    arrange(modelo, dplyr::desc(cond_abrev == "s/bal")) |>
    pull(coluna)

  # Linha de media por coluna (opcional), destacada no topo.
  linhas_grade <- dados |>
    transmute(processo, coluna, accuracy)
  niveis_proc <- ordem_proc
  if (isTRUE(incluir_media)) {
    media_col <- dados |>
      group_by(coluna) |>
      summarise(accuracy = mean(accuracy), .groups = "drop") |>
      mutate(processo = "Media")
    linhas_grade <- bind_rows(linhas_grade, media_col)
    niveis_proc <- c(ordem_proc, "Media")
  }

  linhas_grade <- linhas_grade |>
    mutate(
      processo = factor(processo, levels = niveis_proc),
      coluna   = factor(coluna, levels = ordem_col)
    )

  dados_plot <- accuracy_proc |>
    mutate(
      processo = sub("^\\d+\\.", "", numero_processo),
      processo = factor(processo, levels = unique(processo)),
      condicao = factor(condicao, levels = c("Sem balanceamento", "Balanceado"))
    )

  grafico <- ggplot(dados_plot, aes(x = processo, y = accuracy, fill = condicao)) +
    geom_col(position = position_dodge(width = 0.72), width = 0.62) +
    geom_text(aes(label = sprintf("%.2f", accuracy)),
              position = position_dodge(width = 0.72), vjust = -0.25,
              size = 3, show.legend = FALSE) +
    facet_wrap(~ modelo, nrow = 1) +
    scale_fill_manual(values = c("Sem balanceamento" = "#a6bddb",
                                 "Balanceado" = "#1c6ea4")) +
    scale_y_continuous(limits = c(0, 1.08), breaks = seq(0, 1, 0.2),
                       expand = expansion(mult = c(0, 0.04))) +
    labs(
      title = "Accuracy por processo (validacao LOPO)",
      subtitle = "Cada painel mostra um modelo; cada grupo compara as duas condicoes no mesmo processo.",
      x = NULL, y = "Accuracy", fill = NULL
    ) +
    theme_minimal(base_size = 11) +
    theme(panel.grid.major.x = element_blank(),
          legend.position = "top",
          strip.text = element_text(face = "bold"),
          axis.text.x = element_text(angle = 35, hjust = 1))

  if (isTRUE(exibir)) print(grafico)
  if (isTRUE(salvar)) {
    caminho <- .caminho_saida_aval(caminho_saida, "accuracy_por_processo")
    .salvar_grafico_aval(grafico, caminho, largura, altura, dpi)
  }
  invisible(grafico)
}

# -----------------------------------------------------------------------------
# Comparacao de modelos — grafico de metricas macro
# -----------------------------------------------------------------------------

#' Plota as metricas macro: baseline vs 3 modelos x 2 condicoes.
#'
#' Barras agrupadas por (modelo x condicao), um painel por metrica. O baseline
#' aparece como barra de referencia (condicao propria "Baseline").
#'
#' @param comparacao tibble de consolidar_comparacao_modelos() OU NULL (consolida).
#' @param metricas Vetor de metricas a exibir (default: as quatro macro).
#' @param ... Repassado a consolidar_comparacao_modelos() quando comparacao=NULL.
#' @param exibir,salvar Logicos (ambos opcionais).
#' @param caminho_saida Caminho explicito ou NULL.
#' @param largura,altura,dpi Parametros de ggsave.
#' @return objeto ggplot (invisivel)
plotar_metricas_modelos <- function(
    comparacao    = NULL,
    metricas      = c("Accuracy", "Precision", "Recall", "F1 (macro)"),
    ...,
    exibir        = FALSE,
    salvar        = FALSE,
    caminho_saida = NULL,
    largura       = 11,
    altura        = 7,
    dpi           = 150
) {
  if (is.null(comparacao)) comparacao <- consolidar_comparacao_modelos(...)

  dados <- comparacao |>
    filter(metrica %in% metricas) |>
    mutate(
      metrica = factor(metrica, levels = metricas),
      grupo   = ifelse(fonte == "baseline", "Baseline",
                       paste0(modelo, " (", condicao, ")"))
    )

  # Ordem: baseline primeiro, depois modelo x condicao.
  ordem_grupo <- c("Baseline",
                   unique(dados$grupo[dados$fonte != "baseline"]))
  dados <- dados |>
    mutate(grupo = factor(grupo, levels = ordem_grupo))

  grafico <- ggplot(dados, aes(x = grupo, y = valor, fill = fonte)) +
    geom_col() +
    geom_text(aes(label = sprintf("%.2f", valor)), vjust = -0.3, size = 2.6) +
    facet_wrap(~ metrica, ncol = 2) +
    scale_fill_manual(values = c(baseline = "#999999",
                                 supervisionado = "#2c7fb8"),
                      labels = c(baseline = "Baseline (dicionario)",
                                 supervisionado = "Supervisionado")) +
    scale_y_continuous(limits = c(0, 1.05), expand = expansion(mult = c(0, 0.02))) +
    labs(
      title = "Comparacao de desempenho: baseline vs modelos supervisionados",
      subtitle = "3 modelos x 2 condicoes (LOPO). Baseline por dicionario como referencia",
      x = NULL, y = "Valor da metrica", fill = NULL
    ) +
    theme_minimal(base_size = 11) +
    theme(axis.text.x = element_text(angle = 40, hjust = 1),
          legend.position = "top",
          strip.text = element_text(face = "bold"))

  if (isTRUE(exibir)) print(grafico)
  if (isTRUE(salvar)) {
    caminho <- .caminho_saida_aval(caminho_saida, "comparacao_metricas_modelos")
    .salvar_grafico_aval(grafico, caminho, largura, altura, dpi)
  }
  invisible(grafico)
}

# -----------------------------------------------------------------------------
# Comparacao de modelos — F1 por modelo x condicao (efeito do balanceamento)
# -----------------------------------------------------------------------------

#' Plota o F1 (macro) de cada modelo nas duas condicoes (sem/com balanceamento),
#' com o baseline como linha de referencia. Responde "o balanceamento ajudou?".
#'
#' @param comparacao tibble de consolidar_comparacao_modelos() OU NULL.
#' @param ... Repassado a consolidar_comparacao_modelos() quando comparacao=NULL.
#' @param exibir,salvar Logicos (ambos opcionais).
#' @param caminho_saida Caminho explicito ou NULL.
#' @param largura,altura,dpi Parametros de ggsave.
#' @return objeto ggplot (invisivel)
plotar_f1_por_condicao <- function(
    comparacao    = NULL,
    ...,
    exibir        = FALSE,
    salvar        = FALSE,
    caminho_saida = NULL,
    largura       = 9,
    altura        = 6,
    dpi           = 150
) {
  if (is.null(comparacao)) comparacao <- consolidar_comparacao_modelos(...)

  f1 <- comparacao |> filter(metrica == "F1 (macro)")
  base_f1 <- f1 |> filter(fonte == "baseline") |> pull(valor)
  sup <- f1 |> filter(fonte == "supervisionado")

  if (nrow(sup) == 0) {
    stop("[viz-aval] Sem F1 supervisionado para plotar.")
  }

  grafico <- ggplot(sup, aes(x = modelo, y = valor, fill = condicao)) +
    geom_col(position = position_dodge(width = 0.7), width = 0.6) +
    geom_text(aes(label = sprintf("%.2f", valor)),
              position = position_dodge(width = 0.7), vjust = -0.3, size = 2.8) +
    scale_fill_manual(values = c("Sem balanceamento" = "#a6bddb",
                                 "Balanceado" = "#1c6ea4")) +
    scale_y_continuous(limits = c(0, 1.05), expand = expansion(mult = c(0, 0.02))) +
    labs(
      title = "F1 (macro) por modelo e condicao de balanceamento",
      subtitle = "Barras = supervisionado (LOPO); linha tracejada = baseline por dicionario",
      x = NULL, y = "F1 (macro)", fill = NULL
    ) +
    theme_minimal(base_size = 11) +
    theme(legend.position = "top")

  if (length(base_f1) > 0) {
    grafico <- grafico +
      geom_hline(yintercept = base_f1[1], linetype = "dashed", color = "#d7301f") +
      annotate("text", x = 0.6, y = base_f1[1], label = "baseline",
               color = "#d7301f", vjust = -0.5, hjust = 0, size = 3)
  }

  if (isTRUE(exibir)) print(grafico)
  if (isTRUE(salvar)) {
    caminho <- .caminho_saida_aval(caminho_saida, "comparacao_f1_condicao")
    .salvar_grafico_aval(grafico, caminho, largura, altura, dpi)
  }
  invisible(grafico)
}

# -----------------------------------------------------------------------------
# Comparacao de modelos — matriz de confusao (heatmap)
# -----------------------------------------------------------------------------

#' Normaliza uma matriz de confusao para o formato longo (verdadeiro, predito, n).
#'
#' Aceita: (a) tibble/data.frame ja no formato longo (colunas verdadeiro,
#' predito, n) — como o persistido pelo 11_avaliacao; (b) objeto yardstick
#' conf_mat (usa $table -> Prediction/Truth/Freq).
#' @return tibble(verdadeiro, predito, n)
.normalizar_matriz <- function(matriz) {
  if (inherits(matriz, "conf_mat")) {
    df <- as.data.frame(matriz$table)
    return(tibble(verdadeiro = as.character(df$Truth),
                  predito    = as.character(df$Prediction),
                  n          = as.integer(df$Freq)))
  }
  df <- as.data.frame(matriz)
  if (all(c("verdadeiro", "predito", "n") %in% names(df))) {
    return(tibble(verdadeiro = as.character(df$verdadeiro),
                  predito    = as.character(df$predito),
                  n          = as.integer(df$n)))
  }
  if (all(c("Truth", "Prediction", "Freq") %in% names(df))) {
    return(tibble(verdadeiro = as.character(df$Truth),
                  predito    = as.character(df$Prediction),
                  n          = as.integer(df$Freq)))
  }
  stop("[viz-aval] Formato de matriz de confusao nao reconhecido.")
}

#' Plota uma matriz de confusao como heatmap (verdadeiro nas linhas, predito nas
#' colunas). A diagonal (acertos) fica em destaque pela intensidade da cor.
#'
#' @param matriz tibble longo (verdadeiro, predito, n) OU objeto conf_mat.
#' @param titulo Titulo do grafico.
#' @param exibir,salvar Logicos (ambos opcionais).
#' @param caminho_saida Caminho explicito ou NULL.
#' @param sufixo_saida Sufixo do arquivo quando caminho_saida = NULL.
#' @param largura,altura,dpi Parametros de ggsave.
#' @return objeto ggplot (invisivel)
plotar_matriz_confusao <- function(
    matriz,
    titulo        = "Matriz de confusao",
    exibir        = FALSE,
    salvar        = FALSE,
    caminho_saida = NULL,
    sufixo_saida  = "matriz_confusao",
    largura       = 8,
    altura        = 7,
    dpi           = 150
) {
  df <- .normalizar_matriz(matriz)

  classes <- sort(unique(c(df$verdadeiro, df$predito)))
  rotulos_classes <- c(
    decisao = "Decisao",
    encaminhamento = "Encaminhamento",
    exigencia_complementacao = "Exigencia",
    manifestacao_parecer = "Manifestacao",
    solicitacao = "Solicitacao"
  )
  df <- df |>
    mutate(verdadeiro = factor(verdadeiro, levels = rev(classes)),
           predito    = factor(predito, levels = classes),
           acerto     = as.character(verdadeiro) == as.character(predito))

  grafico <- ggplot(df, aes(x = predito, y = verdadeiro, fill = n)) +
    geom_tile(color = "white") +
    geom_text(aes(label = n, color = n > max(n) / 2), size = 3.2,
              show.legend = FALSE) +
    scale_fill_gradient(low = "#f7fbff", high = "#08519c") +
    scale_color_manual(values = c(`TRUE` = "white", `FALSE` = "#333333")) +
    labs(
      title = titulo,
      subtitle = "Diagonal = acertos | Linhas = classe verdadeira | Colunas = classe predita",
      x = "Predito", y = "Verdadeiro", fill = "n"
    ) +
    scale_x_discrete(labels = function(x) unname(rotulos_classes[x] %||% x)) +
    scale_y_discrete(labels = function(x) unname(rotulos_classes[x] %||% x)) +
    theme_minimal(base_size = 11) +
    theme(axis.text.x = element_text(angle = 40, hjust = 1),
          axis.text.y = element_text(face = "bold"),
          panel.grid = element_blank(),
          plot.title = element_text(face = "bold"))

  # Um contorno teal destaca a diagonal sem depender apenas da intensidade da cor.
  diagonais <- df |> filter(acerto)
  if (nrow(diagonais) > 0) {
    grafico <- grafico +
      geom_tile(data = diagonais, aes(x = predito, y = verdadeiro),
                fill = NA, color = "#168A8A", linewidth = 1.2, inherit.aes = FALSE)
  }

  if (isTRUE(exibir)) print(grafico)
  if (isTRUE(salvar)) {
    caminho <- .caminho_saida_aval(caminho_saida, sufixo_saida)
    .salvar_grafico_aval(grafico, caminho, largura, altura, dpi)
  }
  invisible(grafico)
}
