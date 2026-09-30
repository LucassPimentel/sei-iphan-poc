# =============================================================================
# 06b_classificacao_supervisionada.R
# Classificacao semantica — MODELO SUPERVISIONADO (Passo 4) (POC IPHAN)
#
# Responsabilidade: caminho 2 da Camada de Classificacao (ver architecture.md):
#   modelo supervisionado simples, COMPARADO ao baseline por dicionario (06).
#
# Principios metodologicos (project-context / research-methodology):
#   - Modelos candidatos: Regressao Logistica (multinomial), Naive Bayes, SVM.
#     Evitar Deep Learning.
#   - Validacao POR PROCESSO para evitar vazamento: usamos
#     LEAVE-ONE-PROCESS-OUT (LOPO) — cada processo e teste exatamente uma vez.
#   - Nao ajustar o modelo sobre os dados de teste. As features (TF-IDF) sao
#     construidas DENTRO de cada fold (so no treino), evitando vazamento do
#     vocabulario/IDF do teste.
#   - Sempre reportar sobre qual conjunto a metrica foi calculada.
#   - Amostra pequena (5 processos): resultados sao de POC, sem generalizacao.
#
# Entrada:
#   - Ground truth consolidado (utils_gabarito.R::consolidar_gabarito()).
#   - Texto dos documentos (JSONs "_conteudo_") para construir as features.
#
# Dependencias de ML (tidymodels e amigos) NAO sao carregadas no source; sao
# verificadas em tempo de execucao para o modulo poder ser lido/estudado mesmo
# sem os pacotes instalados. Instale com:
#   install.packages(c("tidymodels","textrecipes","discrim","naivebayes","glmnet"))
#
# Tecnologias (execucao): tidymodels, textrecipes, discrim/naivebayes/glmnet,
#   yardstick; base: dplyr, stringr, purrr, tibble, jsonlite
# =============================================================================

library(dplyr)
library(stringr)
library(purrr)
library(tibble)
library(jsonlite)

# -----------------------------------------------------------------------------
# Pacotes necessarios para ML (verificados em runtime)
# -----------------------------------------------------------------------------

PKGS_ML <- c("recipes", "textrecipes", "parsnip", "workflows",
             "yardstick", "rsample")

# Pacotes por modelo (engine)
PKGS_ENGINE <- list(
  logistica    = c("nnet"),        # multinom (regressao logistica multinomial)
  naive_bayes  = c("discrim", "naivebayes"),
  svm          = c("kernlab")
)

#' Verifica se os pacotes de ML estao instalados; retorna os faltantes.
#' @param modelo Qual engine sera usado (para checar dependencia especifica)
#' @return character com pacotes faltantes (vazio se tudo ok)
.checar_pkgs_ml <- function(modelo = "logistica") {
  necessarios <- c(PKGS_ML, PKGS_ENGINE[[modelo]])
  necessarios[!vapply(necessarios, requireNamespace, quietly = TRUE,
                      FUN.VALUE = logical(1))]
}

# -----------------------------------------------------------------------------
# Montagem do dataset (texto + rotulo + processo)
# -----------------------------------------------------------------------------

#' Extrai o texto (corpo, sem rodape de assinatura) de um JSON "_conteudo_".
#' @param caminho_conteudo Caminho do JSON _conteudo_
#' @return tibble(numero_documento, texto_corpo)
.textos_do_conteudo <- function(caminho_conteudo) {
  j <- jsonlite::fromJSON(caminho_conteudo, simplifyVector = TRUE)
  d <- j$documentos_conteudo
  d <- d[!is.na(d$conteudo_extraido) & d$conteudo_extraido == TRUE, ]
  tibble(
    numero_documento = as.character(d$numero_documento),
    texto_corpo = vapply(d$texto, function(t) {
      if (is.na(t)) return("")
      str_squish(sub("Documento assinado eletronicamente.*", "", t))
    }, character(1))
  )
}

#' Monta o dataset supervisionado juntando ground truth + textos.
#'
#' @param ground_truth tibble de consolidar_gabarito()$ground_truth
#' @param caminhos_conteudo character — JSONs "_conteudo_" (um por processo).
#'        Se NULL, procura em dir_processed por *_conteudo_*.json.
#' @param dir_processed Diretorio dos processados
#' @param remover_pendentes Logico — descartar docs sem rotulo final
#' @return tibble(numero_processo, numero_documento, classe_final, texto_corpo)
montar_dataset_supervisionado <- function(
    ground_truth,
    caminhos_conteudo = NULL,
    dir_processed     = "data/processed",
    remover_pendentes = TRUE
) {
  if (nrow(ground_truth) == 0) {
    stop("[sup] Ground truth vazio. Rotule e consolide antes de treinar.")
  }

  if (is.null(caminhos_conteudo)) {
    caminhos_conteudo <- list.files(dir_processed,
                                    pattern = "_conteudo_.*\\.json$",
                                    full.names = TRUE)
  }
  if (length(caminhos_conteudo) == 0) {
    stop("[sup] Nenhum JSON _conteudo_ encontrado para obter os textos.")
  }

  textos <- bind_rows(lapply(caminhos_conteudo, .textos_do_conteudo)) |>
    distinct(numero_documento, .keep_all = TRUE)

  ds <- ground_truth |>
    left_join(textos, by = "numero_documento") |>
    mutate(texto_corpo = ifelse(is.na(texto_corpo), "", texto_corpo))

  if (remover_pendentes) {
    ds <- ds |> filter(!pendente, classe_final != "indefinido", classe_final != "")
  }

  sem_texto <- sum(ds$texto_corpo == "")
  if (sem_texto > 0) {
    cat("[sup] AVISO:", sem_texto, "documento(s) do gabarito sem texto correspondente.\n")
  }

  ds |>
    transmute(numero_processo, numero_documento,
              classe = factor(classe_final), texto = texto_corpo) |>
    filter(texto != "")
}

# -----------------------------------------------------------------------------
# Modelo (tidymodels) — construido DENTRO de cada fold
# -----------------------------------------------------------------------------

#' Especifica o modelo (parsnip) conforme a escolha.
#' @param modelo "logistica" | "naive_bayes" | "svm"
#' @return objeto parsnip
.spec_modelo <- function(modelo = "logistica") {
  switch(
    modelo,
    logistica = parsnip::multinom_reg(penalty = 0.01, mixture = 0) |>
      parsnip::set_engine("nnet", MaxNWts = 50000) |>
      parsnip::set_mode("classification"),
    naive_bayes = parsnip::naive_Bayes(Laplace = 1) |>
      parsnip::set_engine("naivebayes", usekernel = FALSE) |>
      parsnip::set_mode("classification"),
    svm = parsnip::svm_linear() |>
      parsnip::set_engine("kernlab") |>
      parsnip::set_mode("classification"),
    stop("[sup] Modelo desconhecido: ", modelo)
  )
}

#' Monta o recipe de texto -> TF-IDF (textrecipes). Construido no treino de
#' cada fold, entao o vocabulario/IDF vem SO do treino (sem vazamento).
#'
#' Balanceamento (opcional): quando balancear = TRUE, acrescenta um passo de
#' OVERSAMPLING (themis::step_upsample) que REPLICA exemplos das classes
#' minoritarias no TREINO ate se aproximarem da majoritaria. Aplicado apenas
#' dentro do treino de cada fold (o teste nunca e balanceado), o que evita
#' vazamento. Optamos por upsample (replicacao) em vez de SMOTE porque, com
#' classes de pouquissimos exemplos e features de texto esparsas, a
#' interpolacao sintetica do SMOTE e pouco confiavel — replicar e mais honesto
#' para a POC. Ver research-methodology (desbalanceamento como limitacao).
#'
#' @param dados_treino tibble com colunas classe, texto
#' @param max_tokens Limite de tokens no vocabulario
#' @param balancear Logico — aplicar oversampling das classes minoritarias
#' @param over_ratio Razao alvo minoria/majoria apos o upsample (0..1)
#' @return recipe
.recipe_texto <- function(dados_treino, max_tokens = 150,
                          balancear = FALSE, over_ratio = 1) {
  rec <- recipes::recipe(classe ~ texto, data = dados_treino) |>
    textrecipes::step_tokenize(texto) |>
    textrecipes::step_stopwords(texto, language = "pt") |>
    textrecipes::step_tokenfilter(texto, max_tokens = max_tokens) |>
    textrecipes::step_tfidf(texto)

  if (isTRUE(balancear)) {
    if (!requireNamespace("themis", quietly = TRUE)) {
      stop("[sup] Balanceamento requer o pacote 'themis'. ",
           "Instale com: install.packages(\"themis\")")
    }
    rec <- rec |> themis::step_upsample(classe, over_ratio = over_ratio)
  }
  rec
}

# -----------------------------------------------------------------------------
# Leave-One-Process-Out (LOPO)
# -----------------------------------------------------------------------------

#' Executa validacao leave-one-process-out para um modelo supervisionado.
#'
#' Para cada processo p: treina com os demais, prediz em p, acumula as
#' predicoes. Ao final, calcula metricas sobre TODAS as predicoes (cada doc
#' avaliado exatamente uma vez, sempre fora do treino).
#'
#' @param dataset tibble de montar_dataset_supervisionado()
#' @param modelo "logistica" | "naive_bayes" | "svm"
#' @param balancear Logico — aplicar oversampling no treino de cada fold
#' Treina e prediz um fold. Para 'svm', usa kernlab::ksvm DIRETAMENTE sobre a
#' matriz de features do recipe, contornando um bug conhecido da ponte
#' parsnip->kernlab (parsnip 1.6.0 / kernlab 0.9.33) que gera "subscript out of
#' bounds" em svm_linear multiclasse. Para os demais modelos, usa o workflow
#' parsnip normal. Em ambos, as features vem do MESMO recipe (sem vazamento).
#'
#' @param modelo Nome do modelo
#' @param spec Especificacao parsnip (usada por logistica/naive_bayes)
#' @param rec Recipe (nao preparado) do fold
#' @param treino,teste data.frames do fold
#' @return vetor character com as classes preditas para o teste
.fit_predict_fold <- function(modelo, spec, rec, treino, teste) {
  if (identical(modelo, "svm")) {
    prep    <- recipes::prep(rec, training = treino)
    mat_tr  <- recipes::bake(prep, new_data = treino)
    mat_te  <- recipes::bake(prep, new_data = teste)
    feats   <- setdiff(names(mat_tr), "classe")
    x_tr    <- as.matrix(mat_tr[, feats, drop = FALSE])
    y_tr    <- droplevels(mat_tr$classe)
    fit     <- kernlab::ksvm(x_tr, y_tr, kernel = "vanilladot")
    x_te    <- as.matrix(mat_te[, feats, drop = FALSE])
    return(as.character(kernlab::predict(fit, x_te)))
  }

  # logistica / naive_bayes: workflow parsnip padrao
  wf <- workflows::workflow() |>
    workflows::add_recipe(rec) |>
    workflows::add_model(spec)
  fit  <- parsnip::fit(wf, data = treino)
  pred <- predict(fit, teste)
  as.character(pred$.pred_class)
}

#' @return list(predicoes, metricas, matriz_confusao, por_processo)
avaliar_lopo <- function(dataset, modelo = "logistica", balancear = FALSE) {
  faltantes <- .checar_pkgs_ml(modelo)
  if (isTRUE(balancear)) faltantes <- unique(c(faltantes,
    if (!requireNamespace("themis", quietly = TRUE)) "themis" else character(0)))
  if (length(faltantes) > 0) {
    stop("[sup] Pacotes de ML ausentes para '", modelo, "': ",
         paste(faltantes, collapse = ", "),
         "\n  Instale com: install.packages(c(\"",
         paste(faltantes, collapse = "\",\""), "\"))")
  }

  processos <- unique(dataset$numero_processo)
  if (length(processos) < 2) {
    stop("[sup] LOPO requer >= 2 processos rotulados. Encontrados: ",
         length(processos), ".")
  }

  # Naive Bayes requer discrim CARREGADO (nao so instalado) para o parsnip
  # encontrar a implementacao em tempo de treino (parsnip::fit). Carregamos
  # aqui, antes do loop de folds, para garantir independente do chamador.
  if (identical(modelo, "naive_bayes")) {
    if (!requireNamespace("discrim", quietly = TRUE)) {
      stop("[sup] Pacote 'discrim' nao encontrado. ",
           "Instale com: install.packages('discrim')")
    }
    suppressPackageStartupMessages(library(discrim))
  }

  spec <- .spec_modelo(modelo)
  predicoes <- list()

  # Universo de classes do gabarito (para as metricas considerarem todas,
  # inclusive as que ficam so no teste em algum fold).
  classes_todas <- levels(dataset$classe)

  for (p in processos) {
    treino <- dataset |> filter(numero_processo != p)
    teste  <- dataset |> filter(numero_processo == p)
    if (nrow(teste) == 0 || nrow(treino) == 0) next

    # LIMITACAO do LOPO com amostra pequena: uma classe pode existir SO no
    # processo de teste (ex.: 'solicitacao' concentrada em um unico processo).
    # Nesse caso ela some do treino. Removemos os niveis nao vistos no treino
    # (droplevels) para o modelo treinar apenas com as classes disponiveis —
    # alguns engines (kernlab/SVM) dao erro fatal com classe vazia. Os
    # documentos de teste cuja classe nao foi treinada serao necessariamente
    # ERROS (o modelo nao pode prever o que nao aprendeu): isso e honesto e
    # entra na metrica como recall 0 daquela classe, em vez de ser mascarado.
    treino$classe <- droplevels(treino$classe)
    classes_treino <- levels(treino$classe)
    ausentes_no_treino <- setdiff(classes_todas, classes_treino)
    if (length(ausentes_no_treino) > 0) {
      cat("[sup] NOTA: fold", p, "- classe(s) ausente(s) no treino:",
          paste(ausentes_no_treino, collapse = ", "),
          "(docs de teste dessas classes contam como erro).\n")
    }

    # Recipe treinado SO no treino do fold (evita vazamento de IDF/vocabulario).
    # O balanceamento, quando ativo, tambem ocorre so no treino.
    rec <- .recipe_texto(treino, balancear = balancear)

    # Alguns modelos podem falhar em um fold quando uma classe fica com
    # pouquissimos exemplos no treino. Tratamos por fold: registramos e
    # seguimos, em vez de derrubar toda a avaliacao — a limitacao fica visivel.
    pred_cls <- tryCatch(
      .fit_predict_fold(modelo, spec, rec, treino, teste),
      error = function(e) e
    )
    if (inherits(pred_cls, "error")) {
      cat("[sup] AVISO: fold", p, "falhou (", modelo, "):",
          conditionMessage(pred_cls), "\n")
      next
    }

    predicoes[[length(predicoes) + 1]] <- tibble(
      numero_processo  = p,
      numero_documento = teste$numero_documento,
      verdadeiro       = as.character(teste$classe),
      predito          = as.character(pred_cls)
    )
  }

  if (length(predicoes) == 0) {
    stop("[sup] Nenhum fold concluiu para o modelo '", modelo,
         "'. Provavel incompatibilidade com a amostra (classes muito raras). ",
         "Tente 'logistica' ou balancear = TRUE.")
  }

  pred_all <- bind_rows(predicoes)
  # Niveis = universo completo de classes do gabarito, para que classes que so
  # aparecem no teste (ex.: solicitacao) tambem entrem nas metricas.
  pred_all$verdadeiro <- factor(pred_all$verdadeiro, levels = classes_todas)
  pred_all$predito    <- factor(pred_all$predito, levels = classes_todas)

  metricas <- .metricas_classificacao(pred_all)
  mc <- yardstick::conf_mat(pred_all, truth = verdadeiro, estimate = predito)

  # Metrica por processo (fold): compara em character para evitar comportamento
  # inesperado de factors com NA em niveis ausentes.
  # NaN (0 acertos / 0 observacoes) nao ocorre aqui pois n > 0 sempre,
  # mas NA em predicoes (fold que falhou) conta como 0 acertos.
  por_processo <- pred_all |>
    mutate(.v = as.character(verdadeiro), .p = as.character(predito)) |>
    group_by(numero_processo) |>
    summarise(
      n        = dplyr::n(),
      accuracy = sum(!is.na(.p) & .v == .p) / dplyr::n(),
      .groups  = "drop"
    ) |>
    select(-dplyr::any_of(c(".v", ".p")))

  list(predicoes = pred_all, metricas = metricas,
       matriz_confusao = mc, por_processo = por_processo,
       modelo = modelo, balanceado = isTRUE(balancear))
}

# -----------------------------------------------------------------------------
# Metricas
# -----------------------------------------------------------------------------

#' Calcula metricas de classificacao multiclasse (macro) + accuracy.
#'
#' O F1 macro e derivado DA MEDIA dos F1 POR CLASSE (nao chamando f_meas macro
#' isoladamente), calculado a partir da precision e recall por classe. Isso
#' garante coerencia entre precision, recall e F1: as tres metricas macro sao
#' medias sobre O MESMO conjunto de classes avaliaveis. Uma classe so entra na
#' media quando seu F1 e definido (tem ao menos um verdadeiro OU um predito que
#' permita calcular P e R). Com amostra pequena/desbalanceada, isso evita o
#' artefato de o F1 macro divergir de precision/recall.
#'
#' @param pred tibble(verdadeiro, predito) como fatores de mesmos niveis
#' @return tibble com .metric, .estimator, .estimate
.metricas_classificacao <- function(pred) {
  niveis <- levels(pred$verdadeiro)
  # Convencao (como no scikit-learn): uma classe que EXISTE no gabarito
  # (tp+fn > 0) sempre entra na media macro. Quando o modelo nunca a acerta,
  # sua precision/recall/F1 valem 0 (nao NA) — assim as classes ignoradas pelo
  # modelo PENALIZAM as metricas macro, em vez de sumirem da media. Classes que
  # nao existem no gabarito (tp+fn == 0, ex.: "indefinido" do baseline) ficam
  # fora da media (NA).
  # Predicoes NA (modelo nao conseguiu decidir) contam como erro: nunca sao TP.
  verdadeiro <- as.character(pred$verdadeiro)
  predito    <- as.character(pred$predito)
  predito[is.na(predito)] <- "__NA__"

  linhas <- lapply(niveis, function(cl) {
    tp <- sum(verdadeiro == cl & predito == cl)
    fp <- sum(verdadeiro != cl & predito == cl)
    fn <- sum(verdadeiro == cl & predito != cl)
    existe_no_gabarito <- (tp + fn) > 0
    if (!existe_no_gabarito) {
      return(tibble(classe = cl, precision = NA_real_, recall = NA_real_, f1 = NA_real_))
    }
    p <- if ((tp + fp) > 0) tp / (tp + fp) else 0
    r <- tp / (tp + fn)
    f <- if ((p + r) > 0) 2 * p * r / (p + r) else 0
    tibble(classe = cl, precision = p, recall = r, f1 = f)
  })
  por_classe <- bind_rows(linhas)

  prec_macro <- mean(por_classe$precision, na.rm = TRUE)
  rec_macro  <- mean(por_classe$recall,    na.rm = TRUE)
  f1_macro   <- mean(por_classe$f1,        na.rm = TRUE)

  # Accuracy calculada manualmente (robusta a predicoes NA, que contam erro)
  acc_val <- mean(verdadeiro == predito)

  tibble(
    .metric    = c("accuracy", "precision", "recall", "f_meas"),
    .estimator = c("multiclass", "macro", "macro", "macro"),
    .estimate  = c(acc_val, prec_macro, rec_macro, f1_macro)
  )
}

#' Avalia o BASELINE por dicionario nos mesmos documentos do gabarito, para
#' comparacao justa (mesmo conjunto, mesma metrica).
#'
#' @param dataset tibble de montar_dataset_supervisionado() (tem texto e classe)
#' @param dicionario dicionario de classes (06); se NULL, carrega
#' @return list(predicoes, metricas)
avaliar_baseline <- function(dataset, dicionario = NULL) {
  if (!exists("classificar_documento")) {
    stop("[sup] classificar_documento() indisponivel. source('R/06_classificacao.R').")
  }
  if (is.null(dicionario)) dicionario <- carregar_dicionario_classes()

  pred <- dataset |>
    mutate(predito = vapply(texto, function(t) {
      classificar_documento(t, tipo = NA_character_, dicionario)$classe
    }, character(1)))

  pred$verdadeiro <- factor(pred$classe)
  niveis <- union(levels(pred$verdadeiro), unique(pred$predito))
  pred$verdadeiro <- factor(pred$verdadeiro, levels = niveis)
  pred$predito    <- factor(pred$predito, levels = niveis)

  list(predicoes = pred, metricas = .metricas_classificacao(pred))
}

# -----------------------------------------------------------------------------
# Funcao principal: comparar baseline vs supervisionado
# -----------------------------------------------------------------------------

#' Executa a comparacao completa: baseline vs modelo supervisionado (LOPO).
#'
#' Controle simples do balanceamento pelo parametro `balancear`:
#'   - FALSE  : sem balanceamento (padrao).
#'   - TRUE   : com oversampling das classes minoritarias no treino de cada fold.
#'   - "ambos": roda os DOIS cenarios (sem e com) e imprime lado a lado — util
#'              para o experimento comparativo "o balanceamento ajudou?".
#'
#' @param modelo "logistica" | "naive_bayes" | "svm"
#' @param balancear FALSE | TRUE | "ambos"
#' @param dir_ground_truth Diretorio dos CSVs de rotulagem
#' @param dir_processed Diretorio dos JSONs _conteudo_
#' @param persistir Logico — salvar resultado em output/
#' @return list com metricas do baseline e do(s) cenario(s) supervisionado(s)
comparar_classificadores <- function(
    modelo            = "logistica",
    balancear         = FALSE,
    dir_ground_truth  = "data/ground_truth",
    dir_processed     = "data/processed",
    persistir         = TRUE
) {
  if (!exists("consolidar_gabarito")) {
    stop("[sup] consolidar_gabarito() indisponivel. source('R/utils_gabarito.R').")
  }

  # 1. Consolidar e validar gabarito
  gab <- consolidar_gabarito(dir_ground_truth)
  if (!gab$ok) {
    cat("[sup] Gabarito com erros:\n")
    for (e in gab$erros) cat("  -", e, "\n")
    stop("[sup] Corrija o gabarito antes de treinar.")
  }

  # 2. Montar dataset (texto + rotulo + processo)
  dataset <- montar_dataset_supervisionado(gab$ground_truth,
                                           dir_processed = dir_processed)
  cat("[sup] Dataset:", nrow(dataset), "documentos |",
      dplyr::n_distinct(dataset$numero_processo), "processos |",
      nlevels(dataset$classe), "classes\n")
  cat("[sup] Distribuicao de classes:\n")
  print(table(dataset$classe))

  # 3. Baseline (mesmo conjunto) — nao depende de balanceamento
  base_res <- avaliar_baseline(dataset)
  cat("\n[sup] === BASELINE (dicionario) ===\n")
  print(base_res$metricas)

  # 4. Cenarios de balanceamento a rodar
  cenarios <- if (identical(balancear, "ambos")) c(FALSE, TRUE) else isTRUE(balancear)

  sup_por_cenario <- list()
  for (bal in cenarios) {
    etiqueta <- if (bal) "com balanceamento" else "sem balanceamento"
    sup_res <- avaliar_lopo(dataset, modelo = modelo, balancear = bal)
    cat("\n[sup] === SUPERVISIONADO (", modelo, ", LOPO, ", etiqueta, ") ===\n", sep = "")
    print(sup_res$metricas)
    cat("[sup] Accuracy por processo (fold):\n")
    print(sup_res$por_processo)
    sup_por_cenario[[if (bal) "balanceado" else "sem_balanceamento"]] <- sup_res
  }

  resultado <- list(
    modelo         = modelo,
    n_documentos   = nrow(dataset),
    n_processos    = dplyr::n_distinct(dataset$numero_processo),
    distribuicao   = as.list(table(dataset$classe)),
    baseline       = base_res$metricas,
    supervisionado = sup_por_cenario
  )

  if (persistir) {
    dir.create("output", showWarnings = FALSE, recursive = TRUE)
    data_str <- format(Sys.time(), "%Y%m%d")
    sufixo <- if (identical(balancear, "ambos")) "ambos"
              else if (isTRUE(balancear)) "balanceado" else "simples"
    caminho <- file.path("output",
      paste0("comparacao_classificacao_", modelo, "_", sufixo, "_", data_str, ".json"))
    saida_json <- list(
      modelo = modelo,
      n_documentos = resultado$n_documentos,
      n_processos = resultado$n_processos,
      distribuicao = resultado$distribuicao,
      baseline = base_res$metricas,
      supervisionado = lapply(sup_por_cenario, function(s) {
        list(balanceado = s$balanceado, metricas = s$metricas,
             por_processo = s$por_processo)
      })
    )
    jsonlite::write_json(saida_json, caminho, pretty = TRUE, auto_unbox = TRUE)
    cat("\n[sup] Comparacao salva em:", caminho, "\n")
  }

  invisible(resultado)
}
