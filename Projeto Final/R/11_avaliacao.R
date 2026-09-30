# =============================================================================
# 11_avaliacao.R
# Avaliacao contra o gabarito (fase EVALUATION do CRISP-DM) (POC IPHAN)
#
# Responsabilidade: Camada de Avaliacao (ver architecture.md). Compara o que o
#   sistema automatico produz com o GROUND TRUTH (mapeamento manual validado
#   pelo revisor em data/ground_truth/), respondendo a pergunta da POC: "o
#   metodo reconstroi automaticamente o que se obtem no mapeamento manual?".
#
# ESCOPO (dimensionado ao gabarito REAL, sem inventar):
#   O gabarito atual e de CLASSIFICACAO (colunas: classe_predita, classe_manual
#   -> classe_final). Portanto avaliamos objetivamente:
#     1. CLASSIFICACAO (baseline por dicionario, 06) contra classe_final:
#        accuracy, precision/recall/F1 (macro e POR CLASSE), matriz de confusao,
#        e metricas POR PROCESSO. Complementa o 06b, que avalia o SUPERVISIONADO;
#        aqui o foco e o BASELINE (e, opcionalmente, o supervisionado ao lado).
#     2. COBERTURA: de todos os documentos dos autos, quantos foram tratados
#        (nativos com texto) e quantos ficaram de fora (restritos/sem texto/
#        anexos nao-nativos). Consolida por processo e no total.
#
# NAO avaliado automaticamente (LIMITACAO registrada — research-methodology):
#   Prazos, eventos, datas e fluxo processual NAO sao avaliados aqui porque o
#   gabarito atual NAO os contem (nao ha coluna de prazo/evento/data esperados).
#   Essas dimensoes ficam para a validacao QUALITATIVA com o especialista ou
#   para uma futura ampliacao do gabarito. Nao fabricamos metrica sem verdade.
#
# Reaproveitamento (nao duplicar logica):
#   - consolidar_gabarito()            (utils_gabarito.R)
#   - montar_dataset_supervisionado()  (06b) — junta ground truth + textos
#   - avaliar_baseline()               (06b) — aplica o baseline no dataset
#   - .metricas_classificacao()        (06b) — accuracy/P/R/F1 macro coerentes
#   - avaliar_lopo()                   (06b) — supervisionado (opcional)
#
# Saida: output/AAAAMMDD/avaliacao_{data}.json + resumo legivel no console.
#
# Tecnologias: dplyr, tibble, purrr, stringr, jsonlite
# =============================================================================

library(dplyr)
library(tibble)
library(purrr)
library(stringr)
library(jsonlite)

# Coalescencia nula/NA: x se "presente", senao y. Definido no topo por ser usado
# nos utilitarios abaixo.
`%||%` <- function(x, y) if (is.null(x) || length(x) == 0 || is.na(x[1])) y else x

# Utilitario de pastas por data (dir_saida_dia). Carrega se ausente.
if (!exists("dir_saida_dia")) {
  .cand_utils <- c("R/utils_saida.R", "utils_saida.R",
                   file.path(tryCatch(dirname(sys.frame(1)$ofile),
                                      error = function(e) NA), "utils_saida.R"))
  .cand_utils <- .cand_utils[!is.na(.cand_utils) & file.exists(.cand_utils)]
  if (length(.cand_utils) > 0) source(.cand_utils[1])
}

# Dependencias: utils_gabarito (consolidar_gabarito), 06 (classificar_documento/
# carregar_dicionario_classes) e 06b (avaliar_baseline/.metricas_classificacao/
# montar_dataset_supervisionado). Carrega se ausentes.
.dir_11 <- tryCatch(dirname(sys.frame(1)$ofile), error = function(e) NA)
.src_se_ausente <- function(simbolo, arquivos) {
  if (exists(simbolo)) return(invisible())
  cand <- unlist(lapply(arquivos, function(a)
    c(file.path("R", a), a, if (!is.na(.dir_11)) file.path(.dir_11, a))))
  cand <- cand[file.exists(cand)]
  if (length(cand) > 0) source(cand[1])
}
.src_se_ausente("consolidar_gabarito",         c("utils_gabarito.R"))
.src_se_ausente("carregar_dicionario_classes", c("06_classificacao.R"))
.src_se_ausente("avaliar_baseline",            c("06b_classificacao_supervisionada.R"))

# -----------------------------------------------------------------------------
# Metricas por classe e matriz de confusao (o que o 06b nao expoe)
# -----------------------------------------------------------------------------

#' Metricas POR CLASSE (precision/recall/F1 + suporte) a partir de um tibble de
#' predicoes com colunas `verdadeiro` e `predito` (fatores de mesmos niveis).
#' Convencao consistente com .metricas_classificacao() do 06b: uma classe que
#' existe no gabarito (tp+fn>0) sempre entra; quando o modelo nunca a acerta,
#' P/R/F1 valem 0 (nao NA). Predicoes NA contam como erro.
#'
#' @param pred tibble(verdadeiro, predito)
#' @return tibble(classe, suporte, tp, fp, fn, precision, recall, f1)
.metricas_por_classe <- function(pred) {
  verdadeiro <- as.character(pred$verdadeiro)
  predito    <- as.character(pred$predito)
  predito[is.na(predito)] <- "__NA__"
  niveis <- sort(unique(verdadeiro))  # so classes que EXISTEM no gabarito

  linhas <- lapply(niveis, function(cl) {
    tp <- sum(verdadeiro == cl & predito == cl)
    fp <- sum(verdadeiro != cl & predito == cl)
    fn <- sum(verdadeiro == cl & predito != cl)
    p <- if ((tp + fp) > 0) tp / (tp + fp) else 0
    r <- if ((tp + fn) > 0) tp / (tp + fn) else 0
    f <- if ((p + r) > 0) 2 * p * r / (p + r) else 0
    tibble(classe = cl, suporte = tp + fn, tp = tp, fp = fp, fn = fn,
           precision = round(p, 4), recall = round(r, 4), f1 = round(f, 4))
  })
  bind_rows(linhas)
}

#' Matriz de confusao (linhas = verdadeiro, colunas = predito) como data.frame
#' longo (verdadeiro, predito, n) — formato estavel para JSON e leitura.
#'
#' @param pred tibble(verdadeiro, predito)
#' @return tibble(verdadeiro, predito, n)
.matriz_confusao <- function(pred) {
  verdadeiro <- as.character(pred$verdadeiro)
  predito    <- as.character(pred$predito)
  predito[is.na(predito)] <- "(sem predicao)"
  tab <- as.data.frame(table(verdadeiro = verdadeiro, predito = predito),
                       stringsAsFactors = FALSE)
  names(tab) <- c("verdadeiro", "predito", "n")
  tab <- tab[tab$n > 0, , drop = FALSE]
  as_tibble(tab) |> arrange(desc(n))
}

#' Metricas agregadas POR PROCESSO (accuracy + F1 macro por fold), para expor a
#' variacao entre processos (a amostra e pequena; ver por processo importa).
#'
#' @param pred tibble com colunas numero_processo, verdadeiro, predito
#' @return tibble(numero_processo, n, accuracy, f1_macro)
.metricas_por_processo <- function(pred) {
  pred |>
    group_by(numero_processo) |>
    group_modify(function(df, key) {
      m <- .metricas_classificacao(
        tibble(verdadeiro = factor(df$verdadeiro,
                                   levels = sort(unique(as.character(df$verdadeiro)))),
               predito    = factor(df$predito,
                                   levels = sort(unique(as.character(df$verdadeiro))))))
      acc <- m$.estimate[m$.metric == "accuracy"]
      f1  <- m$.estimate[m$.metric == "f_meas"]
      tibble(n = nrow(df), accuracy = round(acc, 4), f1_macro = round(f1, 4))
    }) |>
    ungroup()
}

# -----------------------------------------------------------------------------
# Cobertura (a partir dos JSONs "_conteudo_" ja persistidos)
# -----------------------------------------------------------------------------

#' Le a cobertura de conteudo de um processo do meta do JSON "_conteudo_".
#' Retorna NULL se nao encontrar.
#' @param prefixo Numero seguro do processo (ex.: "01450_003836_2026_78")
#' @param dir_processed Diretorio base
.cobertura_processo <- function(prefixo, dir_processed = "data/processed") {
  cam <- if (exists("encontrar_arquivo_processo")) {
    encontrar_arquivo_processo(dir_processed, prefixo, etapa = "conteudo", ext = "json")
  } else NA_character_
  if (is.na(cam) || !file.exists(cam)) return(NULL)
  m <- jsonlite::fromJSON(cam, simplifyVector = TRUE)$meta
  tibble(
    numero_processo    = m$numero_processo,
    total_documentos   = as.integer(m$total_documentos %||% NA),
    nativos_elegiveis  = as.integer(m$nativos_elegiveis %||% NA),
    conteudo_extraido  = as.integer(m$conteudo_extraido %||% NA),
    restritos          = as.integer(m$restritos %||% NA),
    sem_texto          = as.integer(m$sem_texto %||% NA),
    anexos_nao_nativos = as.integer(m$anexos_nao_nativos %||% NA),
    cobertura_conteudo = as.numeric(m$cobertura_conteudo %||% NA)
  )
}

#' Consolida a cobertura de todos os processos do gabarito.
#' @param prefixos vetor de numeros seguros
#' @param dir_processed Diretorio base
#' @return list(por_processo = tibble, total = list)
.consolidar_cobertura <- function(prefixos, dir_processed = "data/processed") {
  linhas <- purrr::compact(lapply(prefixos, .cobertura_processo,
                                  dir_processed = dir_processed))
  if (length(linhas) == 0) {
    return(list(por_processo = tibble(), total = list()))
  }
  cob <- bind_rows(linhas)
  soma <- function(col) sum(cob[[col]], na.rm = TRUE)
  total_docs   <- soma("total_documentos")
  extraidos    <- soma("conteudo_extraido")
  total <- list(
    processos          = nrow(cob),
    total_documentos   = total_docs,
    nativos_elegiveis  = soma("nativos_elegiveis"),
    conteudo_extraido  = extraidos,
    restritos          = soma("restritos"),
    sem_texto          = soma("sem_texto"),
    anexos_nao_nativos = soma("anexos_nao_nativos"),
    cobertura_sobre_total   = if (total_docs > 0) round(extraidos / total_docs, 4) else NA_real_,
    cobertura_sobre_nativos = if (soma("nativos_elegiveis") > 0)
      round(extraidos / soma("nativos_elegiveis"), 4) else NA_real_
  )
  list(por_processo = cob, total = total)
}

# -----------------------------------------------------------------------------
# Funcao principal: avaliar_processos()
# -----------------------------------------------------------------------------

#' Avalia a POC contra o gabarito: classificacao (baseline) + cobertura.
#'
#' @param dir_ground_truth Diretorio dos CSVs/XLSX de rotulagem.
#' @param dir_processed Diretorio dos JSONs "_conteudo_".
#' @param incluir_supervisionado Logico — se TRUE, roda tambem o supervisionado
#'        (LOPO) do 06b para exibir baseline e modelo lado a lado. Requer os
#'        pacotes de ML instalados; se ausentes, apenas avisa e segue.
#' @param modelo Engine do supervisionado (quando incluir_supervisionado=TRUE).
#' @param dir_output Diretorio base do relatorio.
#' @param persistir Logico — salvar o relatorio JSON.
#' @return list com metricas, por_classe, matriz_confusao, por_processo,
#'         cobertura, meta e limitacoes.
avaliar_processos <- function(
    dir_ground_truth       = "data/ground_truth",
    dir_processed          = "data/processed",
    incluir_supervisionado = FALSE,
    modelo                 = "logistica",
    dir_output             = "output",
    persistir              = TRUE
) {
  # 1. Gabarito consolidado (a "verdade").
  gab <- consolidar_gabarito(dir_ground_truth)
  if (!isTRUE(gab$ok) && (is.null(gab$ground_truth) || nrow(gab$ground_truth) == 0)) {
    stop("[avaliacao] Gabarito nao consolidou: ", paste(gab$erros, collapse = " | "))
  }
  gt <- gab$ground_truth
  cat("[avaliacao] Gabarito:", nrow(gt), "documentos |",
      dplyr::n_distinct(gt$numero_processo), "processos\n")
  if (length(gab$avisos) > 0) for (a in gab$avisos) cat("[avaliacao] aviso:", a, "\n")

  # 2. Dataset (texto + classe_final), reaproveitando o 06b. remover_pendentes
  #    garante que so documentos com rotulo valido entram na avaliacao.
  dataset <- montar_dataset_supervisionado(gt, dir_processed = dir_processed,
                                           remover_pendentes = TRUE)
  cat("[avaliacao] Documentos avaliaveis (com texto e rotulo):", nrow(dataset), "\n")

  # 3. BASELINE aplicado ao mesmo conjunto (06b::avaliar_baseline).
  base_res <- avaliar_baseline(dataset)
  pred <- base_res$predicoes
  pred$numero_processo <- dataset$numero_processo

  metricas_macro <- base_res$metricas
  por_classe     <- .metricas_por_classe(pred)
  matriz         <- .matriz_confusao(pred)
  por_processo   <- .metricas_por_processo(pred)

  cat("\n[avaliacao] === CLASSIFICACAO — BASELINE (dicionario) vs gabarito ===\n")
  print(metricas_macro)
  cat("\n[avaliacao] Por classe:\n"); print(por_classe)
  cat("\n[avaliacao] Por processo:\n"); print(por_processo)

  # 4. (Opcional) supervisionado lado a lado (06b::avaliar_lopo).
  supervisionado <- NULL
  if (isTRUE(incluir_supervisionado)) {
    if (!exists("avaliar_lopo")) {
      .src_se_ausente("avaliar_lopo", c("06b_classificacao_supervisionada.R"))
    }
    falt <- if (exists(".checar_pkgs_ml")) .checar_pkgs_ml(modelo) else "tidymodels"
    if (length(falt) > 0) {
      cat("[avaliacao] Supervisionado ignorado (pacotes ausentes):",
          paste(falt, collapse = ", "), "\n")
    } else {
      sup <- avaliar_lopo(dataset, modelo = modelo, balancear = FALSE)
      supervisionado <- list(modelo = modelo, metricas = sup$metricas,
                             por_processo = sup$por_processo)
      cat("\n[avaliacao] === SUPERVISIONADO (", modelo, ", LOPO) ===\n", sep = "")
      print(sup$metricas)
    }
  }

  # 5. COBERTURA (por processo + total), a partir dos JSONs "_conteudo_".
  prefixos <- unique(str_replace_all(gt$numero_processo, "[^0-9A-Za-z]", "_"))
  cobertura <- .consolidar_cobertura(prefixos, dir_processed = dir_processed)
  cat("\n[avaliacao] === COBERTURA ===\n")
  if (length(cobertura$total) > 0) {
    ct <- cobertura$total
    cat("[avaliacao] Documentos (autos):", ct$total_documentos,
        "| nativos elegiveis:", ct$nativos_elegiveis,
        "| com texto:", ct$conteudo_extraido, "\n")
    cat("[avaliacao] Restritos:", ct$restritos, "| sem texto:", ct$sem_texto,
        "| anexos nao-nativos:", ct$anexos_nao_nativos, "\n")
    cat("[avaliacao] Cobertura sobre total:",
        if (is.na(ct$cobertura_sobre_total)) "-" else paste0(round(ct$cobertura_sobre_total*100,1), "%"),
        "| sobre nativos:",
        if (is.na(ct$cobertura_sobre_nativos)) "-" else paste0(round(ct$cobertura_sobre_nativos*100,1), "%"),
        "\n")
  }

  # 6. Concordancia baseline<->revisor (ja calculada na consolidacao): quanto o
  #    baseline acertou SEM correcao manual (proxy de qualidade da pre-classif.).
  concordancia <- gab$resumo$concordancia_baseline

  limitacoes <- c(
    "Gabarito atual e de CLASSIFICACAO; prazos, eventos, datas e fluxo NAO sao avaliados automaticamente (sem verdade anotada).",
    "Amostra pequena (POC): metricas nao generalizam; ver metricas POR PROCESSO para a variacao.",
    "Documentos restritos/sem texto ficam fora da avaliacao de classe (entram na COBERTURA como nao tratados).",
    "As metricas de classe sao calculadas sobre os documentos com texto e rotulo valido (pendentes removidos)."
  )

  saida <- list(
    meta = list(
      timestamp        = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      documentos_gabarito = nrow(gt),
      documentos_avaliados = nrow(dataset),
      processos        = dplyr::n_distinct(gt$numero_processo),
      concordancia_baseline_revisor = concordancia,
      distribuicao_classe = as.list(table(dataset$classe)),
      limitacoes       = limitacoes
    ),
    classificacao = list(
      baseline = list(
        metricas_macro = metricas_macro,
        por_classe     = por_classe,
        matriz_confusao = matriz,
        por_processo   = por_processo
      ),
      supervisionado = supervisionado
    ),
    cobertura = cobertura
  )

  if (persistir) {
    dir_dia  <- dir_saida_dia(dir_output)
    data_str <- format(Sys.time(), "%Y%m%d")
    caminho  <- file.path(dir_dia, paste0("avaliacao_", data_str, ".json"))
    write_json(saida, caminho, pretty = TRUE, auto_unbox = TRUE, na = "null")
    cat("\n[avaliacao] Relatorio salvo em:", caminho, "\n")
  }

  invisible(saida)
}
