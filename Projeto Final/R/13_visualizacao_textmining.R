# =============================================================================
# 13_visualizacao_textmining.R
# Visualizacao dos resultados do Text Mining (POC IPHAN)
#
# Responsabilidade: camada de apoio a analise exploratoria (Data Understanding).
#   Nao faz parte do pipeline principal (01-12); serve para inspecionar e
#   comunicar os resultados da etapa 05_text_mining.R.
#
# Escopo atual (opcao B): apenas o grafico canonico de TF-IDF
#   - Top termos por TF-IDF, um painel (facet) por documento.
#
# Decisoes de design:
#   - A funcao SEMPRE retorna o objeto ggplot (nunca fica sem acesso ao grafico).
#   - Exibir na tela e OPCIONAL (parametro `exibir`).
#   - Salvar em arquivo e OPCIONAL (parametro `salvar`).
#   - Os dados sao plotados COMO ESTAO (sem filtrar ruido de assinatura),
#     para deixar visivel na analise exploratoria onde a limpeza pode melhorar.
#
# Entrada : objeto retornado por text_mining_processo() OU caminho do JSON
#           "_textmining_" produzido por essa funcao.
# Saida   : objeto ggplot (sempre) e, opcionalmente, PNG em output/.
#
# Tecnologias: ggplot2, dplyr, forcats, jsonlite, tibble
# =============================================================================

library(ggplot2)
library(dplyr)
library(forcats)
library(jsonlite)
library(tibble)
library(purrr)
library(lubridate)

# -----------------------------------------------------------------------------
# Constantes e utilitarios
# -----------------------------------------------------------------------------

DIR_OUTPUT_PADRAO <- "output"

#' Operador auxiliar: retorna b se a for NULL/vazio.
`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

# Utilitario de pastas por data (dir_saida_dia). Carrega se ausente, para que os
# graficos salvem em output/AAAAMMDD/ como o restante do projeto.
if (!exists("dir_saida_dia")) {
  .cand_utils_13 <- c("R/utils_saida.R", "utils_saida.R")
  .cand_utils_13 <- .cand_utils_13[file.exists(.cand_utils_13)]
  if (length(.cand_utils_13) > 0) source(.cand_utils_13[1])
}

# -----------------------------------------------------------------------------
# Carregamento / normalizacao da entrada
# -----------------------------------------------------------------------------

#' Obtem o data.frame de top termos a partir do resultado do text mining.
#'
#' Aceita tanto o objeto em memoria (list com $top_termos) quanto o caminho
#' do JSON persistido por text_mining_processo().
#'
#' @param resultado_textmining list (retorno de text_mining_processo) OU
#'        character com o caminho do JSON "_textmining_".
#' @return tibble(numero_documento, token, n, tf, idf, tf_idf)
obter_top_termos <- function(resultado_textmining) {
  if (is.character(resultado_textmining)) {
    if (!file.exists(resultado_textmining)) {
      stop("[viz] JSON de text mining nao encontrado: ", resultado_textmining)
    }
    resultado_textmining <- jsonlite::fromJSON(resultado_textmining,
                                               simplifyVector = TRUE)
  }

  top <- resultado_textmining$top_termos
  if (is.null(top) || length(top) == 0 || NROW(top) == 0) {
    stop("[viz] O resultado nao possui 'top_termos' para plotar.")
  }

  colunas_esperadas <- c("numero_documento", "token", "tf_idf")
  faltantes <- setdiff(colunas_esperadas, names(top))
  if (length(faltantes) > 0) {
    stop("[viz] Colunas ausentes em top_termos: ", paste(faltantes, collapse = ", "))
  }

  as_tibble(top)
}

#' Obtem a lista 'por_documento' do resultado do text mining.
#'
#' Aceita objeto em memoria ou caminho de JSON. Retorna sempre uma lista
#' nomeada por numero_documento (como no JSON persistido).
#'
#' @param resultado_textmining objeto ou caminho
#' @return list nomeada por numero_documento (pode ser vazia)
obter_por_documento <- function(resultado_textmining) {
  if (is.character(resultado_textmining)) {
    if (!file.exists(resultado_textmining)) {
      stop("[viz] JSON de text mining nao encontrado: ", resultado_textmining)
    }
    resultado_textmining <- jsonlite::fromJSON(resultado_textmining,
                                               simplifyVector = TRUE)
  }
  pd <- resultado_textmining$por_documento
  if (is.null(pd)) list() else pd
}

# -----------------------------------------------------------------------------
# Limpeza de ruido (nomes de signatarios + tokens colados)
# -----------------------------------------------------------------------------

#' Deriva stopwords dinamicas dos nomes de signatarios do processo.
#'
#' Os nomes vazam para o corpo do documento (assinatura visual antes do
#' marcador de boilerplate) e sobem no TF-IDF por serem raros. Como ja temos
#' o texto de assinatura em `bloco_assinaturas`, extraimos dele os nomes reais
#' — sem inventar: usamos o padrao fixo do SEI
#' "Documento assinado eletronicamente por <NOME>, <CARGO>, em <DATA>".
#'
#' @param resultado_textmining objeto ou caminho
#' @return character com tokens (palavras minusculas) dos nomes de signatarios
derivar_stopwords_signatarios <- function(resultado_textmining) {
  por_doc <- obter_por_documento(resultado_textmining)
  if (length(por_doc) == 0) return(character(0))

  blocos <- vapply(por_doc, function(d) {
    b <- d$bloco_assinaturas
    if (is.null(b) || length(b) == 0 || is.na(b)) "" else as.character(b)
  }, character(1))

  # Captura o texto entre "assinado eletronicamente por" e a primeira virgula
  # (o nome), conforme o padrao fixo do rodape do SEI.
  padrao <- "assinado eletronicamente por\\s+([^,]+),"
  nomes <- unlist(lapply(blocos, function(txt) {
    m <- regmatches(txt, gregexpr(padrao, txt, ignore.case = TRUE))[[1]]
    if (length(m) == 0) return(character(0))
    sub(padrao, "\\1", m, ignore.case = TRUE)
  }))

  if (length(nomes) == 0) return(character(0))

  # Quebrar nomes completos em tokens minusculos (ex.: "Ana Paula Moreli
  # Tauhyl" -> ana, paula, moreli, tauhyl). Mantem consistencia com a
  # tokenizacao do 05 (str_length > 2).
  tokens <- unlist(strsplit(tolower(nomes), "\\s+"))
  tokens <- tokens[nchar(tokens) > 2]
  unique(tokens)
}

#' Detecta tokens provavelmente "colados" por falha de extracao HTML.
#'
#' Heuristica conservadora (nao remove termos legitimos por engano):
#'   - contem ponto interno entre letras (ex.: "disposição.respeitosamente")
#'   - e um token muito longo (>= min_chars) sem espaco, sinal de concatenacao
#'
#' Retorna um predicado logico alinhado ao vetor de tokens de entrada.
#'
#' @param tokens character
#' @param min_chars comprimento a partir do qual um token unico e suspeito
#' @return logical (TRUE = suspeito de ser ruido colado)
detectar_tokens_colados <- function(tokens, min_chars = 18) {
  ponto_interno <- grepl("[a-zà-ú]\\.[a-zà-ú]", tokens, ignore.case = TRUE)
  muito_longo   <- nchar(tokens) >= min_chars
  ponto_interno | muito_longo
}

#' Aplica a limpeza de ruido a um data.frame de termos (top_termos ou tfidf).
#'
#' @param termos_df tibble com coluna `token`
#' @param stopwords_signatarios character (de derivar_stopwords_signatarios)
#' @param remover_colados logico — aplicar heuristica de tokens colados
#' @param min_chars_colado limite para a heuristica de token longo
#' @return tibble filtrado
filtrar_ruido_termos <- function(termos_df,
                                 stopwords_signatarios = character(0),
                                 remover_colados = TRUE,
                                 min_chars_colado = 18) {
  out <- termos_df |>
    filter(!token %in% stopwords_signatarios)

  if (isTRUE(remover_colados)) {
    out <- out |>
      filter(!detectar_tokens_colados(token, min_chars = min_chars_colado))
  }
  out
}

#' Seleciona quais documentos serao plotados.
#'
#' Por padrao escolhe os documentos com maior TF-IDF maximo (os mais
#' "caracterizados"), para evitar facetar dezenas de paineis ilegiveis.
#'
#' @param top_termos tibble de top termos
#' @param documentos NULL (auto) ou vetor de numero_documento a manter
#' @param max_documentos Quantidade maxima de documentos quando `documentos` e NULL
#' @return vetor de numero_documento selecionados (na ordem de plotagem)
selecionar_documentos <- function(top_termos, documentos = NULL,
                                   max_documentos = 6) {
  if (!is.null(documentos)) {
    return(as.character(documentos))
  }

  top_termos |>
    group_by(numero_documento) |>
    summarise(tfidf_max = max(tf_idf, na.rm = TRUE), .groups = "drop") |>
    arrange(desc(tfidf_max)) |>
    slice_head(n = max_documentos) |>
    pull(numero_documento) |>
    as.character()
}

# -----------------------------------------------------------------------------
# Grafico 1: Top termos por TF-IDF (facetado por documento)
# -----------------------------------------------------------------------------

#' Plota os termos de maior TF-IDF por documento (um painel por documento).
#'
#' A funcao SEMPRE retorna o objeto ggplot. Exibir e salvar sao opcionais e
#' independentes: pode-se so exibir, so salvar, ambos ou nenhum (apenas obter
#' o objeto para composicao posterior).
#'
#' @param resultado_textmining Retorno de text_mining_processo() OU caminho
#'        do JSON "_textmining_".
#' @param n_termos Quantidade de termos por documento (default 10).
#' @param documentos NULL (auto) ou vetor de numero_documento a plotar.
#' @param max_documentos Maximo de paineis quando `documentos` e NULL (default 6).
#' @param filtrar_ruido Logico — remover nomes de signatarios e tokens colados
#'        antes de plotar (default FALSE = versao crua).
#' @param exibir Logico — imprimir o grafico na tela (default FALSE).
#' @param salvar Logico — salvar em arquivo PNG (default FALSE).
#' @param caminho_saida Caminho do PNG quando `salvar = TRUE`. Se NULL, gera
#'        um nome em output/ com base no numero do processo.
#' @param largura,altura,dpi Parametros de ggsave (polegadas / dpi).
#' @return objeto ggplot (invisivel)
plotar_top_termos_tfidf <- function(
    resultado_textmining,
    n_termos       = 10,
    documentos     = NULL,
    max_documentos = 6,
    filtrar_ruido  = FALSE,
    exibir         = FALSE,
    salvar         = FALSE,
    caminho_saida  = NULL,
    largura        = 10,
    altura         = 7,
    dpi            = 150
) {
  top_termos <- obter_top_termos(resultado_textmining)

  sufixo_ruido <- ""
  if (isTRUE(filtrar_ruido)) {
    sw_sig <- derivar_stopwords_signatarios(resultado_textmining)
    top_termos <- filtrar_ruido_termos(top_termos, stopwords_signatarios = sw_sig)
    sufixo_ruido <- " — ruido filtrado (signatarios + tokens colados)"
  }

  docs_sel <- selecionar_documentos(top_termos, documentos, max_documentos)

  dados_plot <- top_termos |>
    filter(numero_documento %in% docs_sel) |>
    group_by(numero_documento) |>
    slice_max(tf_idf, n = n_termos, with_ties = FALSE) |>
    ungroup() |>
    # reorder_within permite ordenar as barras dentro de cada facet
    mutate(token_ord = tidytext::reorder_within(token, tf_idf, numero_documento))

  grafico <- ggplot(dados_plot,
                    aes(x = token_ord, y = tf_idf, fill = numero_documento)) +
    geom_col(show.legend = FALSE) +
    coord_flip() +
    tidytext::scale_x_reordered() +
    facet_wrap(~ numero_documento, scales = "free_y") +
    labs(
      title = "Termos de maior TF-IDF por documento",
      subtitle = paste0("Top ", n_termos, " termos por documento (",
                        length(docs_sel), " documento(s))", sufixo_ruido),
      x = "Termo",
      y = "TF-IDF"
    ) +
    theme_minimal(base_size = 11) +
    theme(
      strip.text = element_text(face = "bold"),
      panel.grid.major.y = element_blank()
    )

  if (isTRUE(exibir)) {
    print(grafico)
  }

  if (isTRUE(salvar)) {
    sufixo_arquivo <- if (isTRUE(filtrar_ruido)) "top_termos_tfidf_filtrado" else "top_termos_tfidf"
    caminho <- resolver_caminho_saida(resultado_textmining, caminho_saida, sufixo_arquivo)
    salvar_grafico(grafico, caminho, largura, altura, dpi)
  }

  invisible(grafico)
}

# -----------------------------------------------------------------------------
# Grafico 2: Tokens por documento
# -----------------------------------------------------------------------------

#' Monta um tibble (numero_documento, tipo, n_tokens) a partir de por_documento.
#' @param resultado_textmining objeto ou caminho
#' @return tibble
montar_tabela_documentos <- function(resultado_textmining) {
  por_doc <- obter_por_documento(resultado_textmining)
  if (length(por_doc) == 0) {
    stop("[viz] O resultado nao possui 'por_documento' para plotar.")
  }

  purrr_map <- lapply(por_doc, function(d) {
    tibble(
      numero_documento = as.character(d$numero_documento %||% NA_character_),
      tipo             = as.character(d$tipo %||% NA_character_),
      n_tokens         = as.integer(d$n_tokens %||% 0L)
    )
  })
  bind_rows(purrr_map)
}

#' Plota o numero de tokens por documento (barras horizontais ordenadas).
#'
#' Mostra a disparidade de tamanho entre documentos (ex.: pareceres longos vs
#' despachos curtos), util para a analise exploratoria.
#'
#' @param resultado_textmining objeto ou caminho
#' @param rotulo Qual rotulo usar no eixo: "tipo" (default) ou "numero".
#' @param exibir,salvar Logicos — visualizacao e salvamento (ambos opcionais).
#' @param caminho_saida Caminho explicito do PNG ou NULL (auto em output/).
#' @param largura,altura,dpi Parametros de ggsave.
#' @return objeto ggplot (invisivel)
plotar_tokens_por_documento <- function(
    resultado_textmining,
    rotulo        = c("tipo", "numero"),
    exibir        = FALSE,
    salvar        = FALSE,
    caminho_saida = NULL,
    largura       = 9,
    altura        = 6,
    dpi           = 150
) {
  rotulo <- match.arg(rotulo)
  tab <- montar_tabela_documentos(resultado_textmining)

  tab <- tab |>
    mutate(rotulo_eixo = if (rotulo == "tipo") {
      paste0(tipo, " (", numero_documento, ")")
    } else {
      numero_documento
    })

  grafico <- ggplot(tab,
                    aes(x = fct_reorder(rotulo_eixo, n_tokens), y = n_tokens)) +
    geom_col(fill = "#2c7fb8") +
    geom_text(aes(label = n_tokens), hjust = -0.15, size = 3) +
    coord_flip() +
    labs(
      title = "Tokens por documento",
      subtitle = "Numero de tokens apos limpeza e stopwords (etapa 05)",
      x = NULL,
      y = "Nro de tokens"
    ) +
    expand_limits(y = max(tab$n_tokens, na.rm = TRUE) * 1.08) +
    theme_minimal(base_size = 11) +
    theme(panel.grid.major.y = element_blank())

  if (isTRUE(exibir)) print(grafico)
  if (isTRUE(salvar)) {
    caminho <- resolver_caminho_saida(resultado_textmining, caminho_saida,
                                      "tokens_por_documento")
    salvar_grafico(grafico, caminho, largura, altura, dpi)
  }
  invisible(grafico)
}

# -----------------------------------------------------------------------------
# Grafico 3: Linha do tempo das datas extraidas
# -----------------------------------------------------------------------------

#' Monta um tibble com todas as datas extraidas (uma linha por data).
#' @param resultado_textmining objeto ou caminho
#' @return tibble(numero_documento, tipo, data_iso, formato)
montar_tabela_datas <- function(resultado_textmining) {
  por_doc <- obter_por_documento(resultado_textmining)
  if (length(por_doc) == 0) {
    stop("[viz] O resultado nao possui 'por_documento' para plotar.")
  }

  linhas <- lapply(por_doc, function(d) {
    datas <- d$datas_encontradas
    # datas_encontradas pode ser data.frame, list() vazia ou NULL
    if (is.null(datas) || length(datas) == 0 || NROW(datas) == 0) {
      return(NULL)
    }
    tibble(
      numero_documento = as.character(d$numero_documento %||% NA_character_),
      tipo             = as.character(d$tipo %||% NA_character_),
      data_iso         = as.Date(datas$data_iso),
      formato          = as.character(datas$formato)
    )
  })

  resultado <- bind_rows(linhas)
  if (nrow(resultado) == 0) {
    stop("[viz] Nenhuma data extraida disponivel para plotar.")
  }
  resultado
}

#' Plota a distribuicao temporal das datas extraidas dos documentos.
#'
#' Cada ponto e uma data encontrada no corpo de um documento, posicionada no
#' eixo temporal e agrupada por documento no eixo vertical. Cor indica o
#' formato (numerico vs extenso). Util para a etapa futura de prazos/event log.
#'
#' @param resultado_textmining objeto ou caminho
#' @param exibir,salvar Logicos (ambos opcionais)
#' @param caminho_saida Caminho explicito ou NULL
#' @param largura,altura,dpi Parametros de ggsave
#' @return objeto ggplot (invisivel)
plotar_linha_tempo_datas <- function(
    resultado_textmining,
    exibir        = FALSE,
    salvar        = FALSE,
    caminho_saida = NULL,
    largura       = 10,
    altura        = 6,
    dpi           = 150
) {
  tab <- montar_tabela_datas(resultado_textmining)

  tab <- tab |>
    mutate(rotulo_doc = paste0(tipo, " (", numero_documento, ")"))

  grafico <- ggplot(tab,
                    aes(x = data_iso,
                        y = fct_reorder(rotulo_doc, data_iso, .fun = min),
                        color = formato)) +
    geom_point(size = 3, alpha = 0.8) +
    scale_x_date(date_labels = "%b/%Y") +
    labs(
      title = "Datas extraidas ao longo do tempo",
      subtitle = "Cada ponto e uma data encontrada no corpo do documento",
      x = "Data",
      y = NULL,
      color = "Formato"
    ) +
    theme_minimal(base_size = 11) +
    theme(panel.grid.minor = element_blank())

  if (isTRUE(exibir)) print(grafico)
  if (isTRUE(salvar)) {
    caminho <- resolver_caminho_saida(resultado_textmining, caminho_saida,
                                      "linha_tempo_datas")
    salvar_grafico(grafico, caminho, largura, altura, dpi)
  }
  invisible(grafico)
}

# -----------------------------------------------------------------------------
# Grafico 4: Dispersao TF vs IDF
# -----------------------------------------------------------------------------

#' Plota a dispersao TF x IDF dos termos, ilustrando a logica do TF-IDF.
#'
#' Cada ponto e um par (documento, termo). O eixo x e a frequencia no
#' documento (TF), o eixo y e a raridade no corpus (IDF), e a cor/tamanho e o
#' TF-IDF resultante. Termos no canto superior-direito (frequentes no doc E
#' raros no corpus) sao os mais caracterizadores. Ajuda a explicar o metodo.
#'
#' Observacao: usa o conjunto `top_termos` (ja e o recorte de maior TF-IDF por
#' documento). Para a nuvem completa seria necessario o campo `tfidf` integral.
#'
#' @param resultado_textmining objeto ou caminho
#' @param filtrar_ruido Logico — remover signatarios e tokens colados
#' @param rotular_top Quantos termos de maior TF-IDF rotular (0 = nenhum)
#' @param exibir,salvar Logicos (ambos opcionais)
#' @param caminho_saida Caminho explicito ou NULL
#' @param largura,altura,dpi Parametros de ggsave
#' @return objeto ggplot (invisivel)
plotar_dispersao_tf_idf <- function(
    resultado_textmining,
    filtrar_ruido = FALSE,
    rotular_top   = 12,
    exibir        = FALSE,
    salvar        = FALSE,
    caminho_saida = NULL,
    largura       = 9,
    altura        = 7,
    dpi           = 150
) {
  termos <- obter_top_termos(resultado_textmining)

  colunas_esperadas <- c("tf", "idf", "tf_idf", "token")
  faltantes <- setdiff(colunas_esperadas, names(termos))
  if (length(faltantes) > 0) {
    stop("[viz] Colunas ausentes para dispersao TF x IDF: ",
         paste(faltantes, collapse = ", "))
  }

  sufixo_ruido <- ""
  if (isTRUE(filtrar_ruido)) {
    sw_sig <- derivar_stopwords_signatarios(resultado_textmining)
    termos <- filtrar_ruido_termos(termos, stopwords_signatarios = sw_sig)
    sufixo_ruido <- " — ruido filtrado"
  }

  grafico <- ggplot(termos, aes(x = tf, y = idf)) +
    geom_point(aes(color = tf_idf, size = tf_idf), alpha = 0.7) +
    scale_color_viridis_c(option = "C") +
    labs(
      title = "Dispersao TF x IDF dos termos",
      subtitle = paste0("Cor/tamanho = TF-IDF; canto sup.-dir. = mais caracterizador",
                        sufixo_ruido),
      x = "TF (frequencia no documento)",
      y = "IDF (raridade no corpus)",
      color = "TF-IDF",
      size = "TF-IDF"
    ) +
    theme_minimal(base_size = 11)

  if (rotular_top > 0) {
    top_rotulos <- termos |>
      slice_max(tf_idf, n = rotular_top, with_ties = FALSE)
    # geom_text nativo (evita dependencia extra de ggrepel). vjust desloca o
    # rotulo acima do ponto para reduzir sobreposicao.
    grafico <- grafico +
      geom_text(
        data = top_rotulos,
        aes(label = token),
        size = 3, vjust = -0.8, check_overlap = TRUE
      )
  }

  if (isTRUE(exibir)) print(grafico)
  if (isTRUE(salvar)) {
    sufixo_arquivo <- if (isTRUE(filtrar_ruido)) "dispersao_tf_idf_filtrado" else "dispersao_tf_idf"
    caminho <- resolver_caminho_saida(resultado_textmining, caminho_saida, sufixo_arquivo)
    salvar_grafico(grafico, caminho, largura, altura, dpi)
  }
  invisible(grafico)
}

# -----------------------------------------------------------------------------
# Grafico 5: Composicao do corpus por tipo de documento
# -----------------------------------------------------------------------------

#' Monta a contagem de documentos por tipo a partir dos JSONs "_conteudo_".
#'
#' Le TODOS os processos (ou os caminhos indicados) e conta os documentos por
#' `tipo`, distinguindo os que tiveram texto extraido (nativos com conteudo) dos
#' que nao (anexos/restritos/sem texto). E uma visao de corpus (Data
#' Understanding), nao de um processo isolado.
#'
#' @param caminhos_conteudo character — JSONs "_conteudo_" a ler. Se NULL,
#'        procura em dir_processed por *_conteudo_*.json (o mais recente de cada
#'        processo, quando encontrar_arquivo_processo estiver disponivel).
#' @param dir_processed Diretorio dos processados.
#' @return tibble(tipo, extraido, n)
montar_tabela_tipos_corpus <- function(caminhos_conteudo = NULL,
                                       dir_processed = "data/processed") {
  if (is.null(caminhos_conteudo)) {
    caminhos_conteudo <- list.files(dir_processed,
                                    pattern = "_conteudo_.*\\.json$",
                                    full.names = TRUE, recursive = TRUE)
  }
  if (length(caminhos_conteudo) == 0) {
    stop("[viz] Nenhum JSON _conteudo_ encontrado em ", dir_processed)
  }

  linhas <- lapply(caminhos_conteudo, function(cam) {
    j <- jsonlite::fromJSON(cam, simplifyVector = TRUE)
    d <- j$documentos_conteudo
    if (is.null(d) || NROW(d) == 0) return(NULL)
    tibble(
      numero_processo = as.character(j$meta$numero_processo %||% NA_character_),
      tipo      = as.character(d$tipo),
      extraido  = ifelse(!is.na(d$conteudo_extraido) & d$conteudo_extraido,
                         "Com texto extraido", "Sem texto (anexo/restrito)")
    )
  })
  dados <- bind_rows(linhas)
  # Um documento pode aparecer em mais de uma versao/processo; contamos por
  # (processo, tipo, extraido) para nao inflar com reprocessamentos do mesmo doc.
  dados |>
    distinct(numero_processo, tipo, extraido, .keep_all = FALSE) |>
    count(tipo, extraido, name = "n")
}

#' Plota a composicao do corpus por tipo de documento (barras horizontais
#' empilhadas por status de extracao).
#'
#' Comunica o que domina os autos (Despachos, Oficios, Pareceres, Anexos...) e,
#' pela cor, onde esta o texto util vs. o material sem conteudo extraido — um
#' apoio direto a etapa de Data Understanding.
#'
#' @param caminhos_conteudo character (JSONs "_conteudo_") ou NULL (auto).
#' @param dir_processed Diretorio dos processados.
#' @param top_n Mostrar os `top_n` tipos mais frequentes; os demais sao agrupados
#'        como "Outros". O padrao e 8 para manter a leitura do grafico.
#' @param exibir,salvar Logicos (ambos opcionais).
#' @param caminho_saida Caminho explicito do PNG ou NULL (auto em output/).
#' @param largura,altura,dpi Parametros de ggsave.
#' @return objeto ggplot (invisivel)
plotar_composicao_corpus_tipo <- function(
    caminhos_conteudo = NULL,
    dir_processed     = "data/processed",
    top_n             = 8,
    exibir            = FALSE,
    salvar            = FALSE,
    caminho_saida     = NULL,
    largura           = 9,
    altura            = 7,
    dpi               = 150
) {
  tab <- montar_tabela_tipos_corpus(caminhos_conteudo, dir_processed)

  totais_tipo <- tab |>
    group_by(tipo) |>
    summarise(total = sum(n), .groups = "drop") |>
    arrange(desc(total))

  if (top_n > 0 && nrow(totais_tipo) > top_n) {
    tipos_manter <- totais_tipo$tipo[seq_len(top_n)]
    tab <- tab |>
      mutate(tipo = if_else(tipo %in% tipos_manter, tipo, "Outros")) |>
      group_by(tipo, extraido) |>
      summarise(n = sum(n), .groups = "drop")
    totais_tipo <- tab |>
      group_by(tipo) |>
      summarise(total = sum(n), .groups = "drop") |>
      arrange(desc(total))
  }

  tab <- tab |>
    mutate(tipo = factor(tipo, levels = rev(totais_tipo$tipo)))

  grafico <- ggplot(tab, aes(x = tipo, y = n, fill = extraido)) +
    geom_col() +
    coord_flip() +
    scale_fill_manual(values = c("Com texto extraido" = "#2c7fb8",
                                 "Sem texto (anexo/restrito)" = "#bdbdbd")) +
    labs(
      title = "Composicao do corpus por tipo de documento",
      subtitle = paste0("Top ", top_n, " tipos; demais agrupados em Outros"),
      x = NULL,
      y = "Nro de documentos",
      fill = NULL
    ) +
        theme_minimal(base_size = 11) +
    theme(panel.grid.major.y = element_blank(),
          legend.position = "top",
          axis.text.y = element_text(size = 9),
          plot.title = element_text(face = "bold"),
          plot.margin = margin(10, 18, 10, 10))

  if (isTRUE(exibir)) print(grafico)
  if (isTRUE(salvar)) {
    caminho <- if (!is.null(caminho_saida)) caminho_saida else {
      data_str <- format(Sys.time(), "%Y%m%d")
      dir_dia  <- if (exists("dir_saida_dia")) dir_saida_dia(DIR_OUTPUT_PADRAO) else DIR_OUTPUT_PADRAO
      file.path(dir_dia, paste0("corpus_composicao_tipo_", data_str, ".png"))
    }
    salvar_grafico(grafico, caminho, largura, altura, dpi)
  }
  invisible(grafico)
}

# -----------------------------------------------------------------------------
# Auxiliares: caminho de saida e salvamento
# -----------------------------------------------------------------------------

#' Salva um grafico ggplot em disco, criando o diretorio se necessario.
#' @param grafico objeto ggplot
#' @param caminho caminho do arquivo
#' @param largura,altura,dpi parametros de ggsave
salvar_grafico <- function(grafico, caminho, largura, altura, dpi) {
  dir.create(dirname(caminho), showWarnings = FALSE, recursive = TRUE)
  ggsave(caminho, plot = grafico, width = largura, height = altura, dpi = dpi)
  cat("[viz] Grafico salvo em:", caminho, "\n")
}

#' Resolve o caminho do PNG de saida.
#'
#' Se `caminho_saida` for informado, usa-o. Caso contrario, gera um nome em
#' output/ a partir do numero do processo (quando disponivel), do sufixo do
#' grafico e da data.
#'
#' @param resultado_textmining Objeto ou caminho de origem (para extrair o nome)
#' @param caminho_saida Caminho explicito ou NULL
#' @param sufixo Identificador do grafico (ex.: "tokens_por_documento")
#' @return character com o caminho do arquivo
resolver_caminho_saida <- function(resultado_textmining, caminho_saida, sufixo) {
  if (!is.null(caminho_saida)) {
    return(caminho_saida)
  }

  numero_processo <- extrair_numero_processo(resultado_textmining)
  base <- if (is.null(numero_processo)) {
    "textmining"
  } else {
    gsub("[^0-9A-Za-z]", "_", numero_processo)
  }

  data_str <- format(Sys.time(), "%Y%m%d")
  file.path(DIR_OUTPUT_PADRAO, paste0(base, "_", sufixo, "_", data_str, ".png"))
}

#' Extrai o numero do processo da entrada, quando disponivel.
#' @return character (numero) ou NULL
extrair_numero_processo <- function(resultado_textmining) {
  obj <- resultado_textmining
  if (is.character(obj)) {
    if (!file.exists(obj)) return(NULL)
    obj <- jsonlite::fromJSON(obj, simplifyVector = TRUE)
  }
  if (!is.null(obj$meta) && !is.null(obj$meta$numero_processo)) {
    return(obj$meta$numero_processo)
  }
  NULL
}
