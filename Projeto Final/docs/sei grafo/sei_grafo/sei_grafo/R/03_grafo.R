# =====================================================================
# 03_grafo.R — Montagem do grafo processual
#
# Nós   : atos do processo, protocolos externos agrupados por data e
#         nós tracejados de documentos ainda não juntados.
# Setas : quatro regras, em ordem de precedência —
#         (1) referência explícita pelo número "Processo / Documento";
#         (2) destinatário de A que assina o ato seguinte B;
#         (3) protocolo externo que abre um novo ciclo de análise;
#         (4) encadeamento temporal, para o que restou sem entrada.
# Rótulo: só a regra (2) rotula a seta com o destinatário. Referência e
#         sequência não rotulam: a ligação já está no número citado.
# Prazos: abertos pela unidade que fixa a data-limite (CGM), contados da
#         documentação externa protocolada e encerrados na assinatura do
#         parecer, em dias corridos.
# =====================================================================

suppressPackageStartupMessages({
  library(dplyr); library(stringr); library(purrr); library(tibble); library(tidyr)
})

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || all(is.na(a))) b else a


# ---------------------------------------------------------------------
# Taxonomia dos pareceres da IN 06/2025
# ---------------------------------------------------------------------

# Cada parecer pertence a um componente cultural e, no componente
# arqueológico, a um nível de empreendimento.
PARECERES <- tribble(
  ~tipo,                                     ~componente,    ~nivel,
  "Parecer FCA Arq",                         "arqueologico", NA,
  "Parecer PAA Proj/Acomp/Arq",              "arqueologico", "II",
  "Parecer RAA Arq",                         "arqueologico", "II",
  "Parecer PAIPA Arq",                       "arqueologico", "III",
  "Parecer RAIPA Arq",                       "arqueologico", "III",
  "Parecer PAPIPA Arq",                      "arqueologico", "IV",
  "Parecer FCA Mat",                         "material",     NA,
  "Parecer RAIPM Mat",                       "material",     NA,
  "Parecer FCA Imat",                        "imaterial",    NA,
  "Parecer PGBIR Imat",                      "imaterial",    NA,
  "Parecer RAIBIR Imat",                     "imaterial",    NA,
  "Parecer RPGBIR Imat",                     "imaterial",    NA
)

# Pareceres de rótulo livre que pertencem ao componente arqueológico.
PARECERES_ARQ_LIVRES <- c(
  "Potencial de Impacto Arqueol", "Programa de Gest", "Projeto de Avalia",
  "Projeto de Salvamento", "Proposta de Acompanhamento",
  "Relat\u00f3rio de Pesquisa Arqueol", "An\u00e1lise Cadastro de S\u00edtios"
)

#' Componente cultural de um parecer ou nota técnica
#'
#' Primeiro pelo rótulo canônico; não havendo correspondência, pelos
#' termos do componente no próprio texto do ato.
componente_de <- function(tipo, texto = NA_character_) {
  alvo <- sem_acento(tolower(coalesce(tipo, "")))
  hit <- PARECERES$componente[map_lgl(sem_acento(tolower(PARECERES$tipo)),
                                      \(k) str_starts(alvo, fixed(k)))]
  if (length(hit)) return(hit[1])
  if (any(map_lgl(sem_acento(tolower(PARECERES_ARQ_LIVRES)),
                  \(k) str_detect(alvo, fixed(k))))) return("arqueologico")
  corpo <- sem_acento(tolower(paste(coalesce(tipo, ""), coalesce(texto, ""))))
  if (str_detect(corpo, "imaterial")) return("imaterial")
  if (str_detect(corpo, "arqueolog")) return("arqueologico")
  if (str_detect(corpo, "\\bmaterial")) return("material")
  NA_character_
}

#' Nível do empreendimento, pelo rótulo ou pelo texto do ato
nivel_de <- function(tipo, texto = NA_character_) {
  alvo <- sem_acento(tolower(coalesce(tipo, "")))
  hit <- PARECERES$nivel[map_lgl(sem_acento(tolower(PARECERES$tipo)),
                                 \(k) str_starts(alvo, fixed(k)))]
  if (length(hit) && !is.na(hit[1])) return(hit[1])
  m <- str_match(paste(coalesce(tipo, ""), coalesce(texto, "")),
                 regex("N\u00edvel\\s+(IV|III|II|I)\\b", ignore_case = TRUE))
  if (!is.na(m[1])) toupper(m[2]) else NA_character_
}

# Instrumentos que a CGM pode demandar, e o parecer correspondente.
# A ordem importa: os acrônimos mais longos vêm primeiro, para que
# "RAIPA" não seja lido como "PA".
DEMANDAS <- tribble(
  ~termo,    ~pareceres,
  "PAPIPA",  "Parecer PAPIPA Arq",
  "RAIPA",   "Parecer RAIPA Arq",
  "PAIPA",   "Parecer PAIPA Arq",
  "RAIBIR",  "Parecer RAIBIR Imat",
  "RPGBIR",  "Parecer RPGBIR Imat",
  "PGBIR",   "Parecer PGBIR Imat",
  "RAIPM",   "Parecer RAIPM Mat",
  "FCA",     "Parecer FCA Arq|Parecer FCA Mat|Parecer FCA Imat"
)

#' Pareceres demandados por um despacho da CGM
#'
#' A demanda está declarada no assunto do ato. O primeiro despacho da
#' CGM, quando informa análise manual da FCA, abre demanda para os três
#' componentes; nos demais casos, o instrumento citado define o parecer
#' esperado. Em análise automática não há parecer: a demanda é notificar
#' o interessado.
demanda_do_despacho <- function(assunto) {
  a <- sem_acento(tolower(coalesce(assunto, "")))
  if (!nzchar(a)) return(character(0))
  if (str_detect(a, "analise automatica")) return(character(0))
  achado <- DEMANDAS$pareceres[map_lgl(tolower(DEMANDAS$termo), \(t) str_detect(a, fixed(t)))]
  if (!length(achado)) return(character(0))
  str_split(achado[1], "\\|")[[1]]
}

# Hierarquia de unidades: um documento dirigido à unidade-mãe pode ser
# respondido por unidade vinculada, e o histórico confirma o repasse.
HIERARQUIA <- tribble(
  ~mae,    ~filha,
  "CGLic", "CAIP",   "CGLic", "CGM",   "CGLic", "CAIP-CGM", "CGLic", "DAP",
  "CGLic", "DIVGEO", "CGLic", "CORA",  "CGLic", "DINO",
  "CNA",   "COIR",   "CNA",   "CGINF", "CNA",   "COP"
)

# Unidades de superintendência trazem o sufixo da UF; as unidades do
# programa de gestão do licenciamento seguem a mesma regra.
RE_SUPERINTENDENCIA <- "(IPHAN|PGLic)-[A-Z]{2}$"

# Termos que caracterizam encaminhamento para cadastro em base
# georreferenciada. O nó tracejado da unidade responsável só é criado
# quando o conteúdo traz esses termos E o histórico confirma que a
# unidade recebeu o processo.
TERMOS_DBGEO    <- c("cadastr", "base de dados", "georreferenc", "dbgeo")
TERMOS_PORTARIA <- c("portaria")

eh_superintendencia <- function(u) str_detect(coalesce(u, ""), RE_SUPERINTENDENCIA)

normalizar_nome <- function(x) {
  x <- stringi::stri_trans_general(coalesce(x, ""), "Latin-ASCII")
  str_squish(tolower(str_remove_all(x, "[^A-Za-z ]")))
}

#' Contém nome de pessoa? Decide se a seta leva o destinatário no rótulo.
tem_nome_pessoa <- function(x) {
  map_lgl(x, \(v) {
    if (is.na(v) || !nzchar(v)) return(FALSE)
    primeira <- str_split(v, "\n")[[1]][1]
    str_detect(primeira, "[A-Z\u00c0-\u00dc][[:alpha:]]+\\s+([a-z\u00e0-\u00ff]{2,4}\\s+)?[A-Z\u00c0-\u00dc][[:alpha:]]+")
  })
}


#' Blocos de destinatário de um nó, como texto
blocos_dest <- function(d) {
  if (is.null(d) || !nrow(d)) character(0) else d$bloco
}

#' E-mails declarados em cada bloco de destinatário
emails_dest <- function(d) {
  if (is.null(d) || !nrow(d)) character(0) else unique(unlist(d$emails))
}

#' E-mails de destino de um documento do tipo e-mail
emails_do_email <- function(e) {
  if (is.null(e) || !length(e) || is.null(e[[1]])) return(character(0))
  campo <- e[[1]]$para %||% ""
  unique(tolower(str_extract_all(campo, "[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\\.[A-Za-z]{2,}")[[1]]))
}

#' Duas formas do mesmo nome?
#'
#' O documento cita ora o nome completo, ora a forma abreviada ("Ana
#' Tauhyl" por "Ana Paula Moreli Tauhyl"). Basta que o primeiro e o
#' último nome coincidam.
nome_casa <- function(a, b) {
  if (is.na(a) || is.na(b) || !nzchar(a) || !nzchar(b)) return(FALSE)
  if (str_detect(b, fixed(a)) || str_detect(a, fixed(b))) return(TRUE)
  ta <- str_split(a, " ")[[1]]; tb <- str_split(b, " ")[[1]]
  ta <- ta[nchar(ta) > 2]; tb <- tb[nchar(tb) > 2]
  if (length(ta) < 2 || length(tb) < 2) return(FALSE)
  ta[1] %in% tb && ta[length(ta)] %in% tb
}

#' O bloco de destinatário designa unidade interna da organização?
eh_unidade_interna <- function(x) {
  str_detect(sem_acento(tolower(coalesce(x, ""))),
             "iphan|cglic|coordenacao-geral|superintendencia|divisao|centro nacional")
}

#' O signatário do documento exerce substituição?
eh_substituto <- function(x) {
  str_detect(sem_acento(tolower(coalesce(x, ""))), "substitut")
}

#' Resolve a unidade que de fato respondeu, quando há repasse interno
resolver_unidade <- function(unidade_destino, andamentos, depois_de) {
  filhas <- HIERARQUIA$filha[HIERARQUIA$mae == unidade_destino]
  if (!length(filhas)) return(unidade_destino)
  repasse <- andamentos |>
    filter(rotulo == "remetido", origem == unidade_destino,
           unidade %in% filhas, ts >= depois_de) |>
    arrange(ts) |> slice(1)
  if (nrow(repasse)) repasse$unidade else unidade_destino
}

# ---------------------------------------------------------------------
# Nós
# ---------------------------------------------------------------------

agrupar_protocolos_externos <- function(documentos) {
  tipos_externos <- sem_acento(c(
    "Recibo", "Requerimento", "Anexo", "Termo", "Pedido", "Documenta\u00e7\u00e3o",
    "Dados pessoais ou dados pessoais sens\u00edveis",
    "Anota\u00e7\u00e3o de Responsabilidade T\u00e9cnica (ART)",
    "Arquivo ADA", "Arquivo AID", "Ficha de Caracteriza\u00e7\u00e3o de Atividade"))

  documentos |>
    filter(is.na(tipo_canonico) | sem_acento(tipo_canonico) %in% tipos_externos) |>
    group_by(data) |>
    summarise(
      id         = paste0("EXT_", format(data[1], "%Y%m%d")),
      n_docs_ids = list(n_doc),
      rotulo     = "Documenta\u00e7\u00e3o externa protocolada",
      # a unidade do protocolo e a que recebeu a peticao, nao a de
      # qualquer anexo avulso do mesmo dia
      unidade    = coalesce(unidade[sem_acento(tipo_canonico) %in% c("Recibo", "Requerimento")][1],
                            first(na.omit(unidade)), "SAIP"),
      detalhe    = paste(sprintf("%s: %s", coalesce(tipo_canonico, "Documento"), n_doc),
                         collapse = "\n"),
      .groups    = "drop"
    )
}

montar_nos <- function(cabecalho, documentos, conteudo, andamentos) {
  externos <- agrupar_protocolos_externos(documentos)
  ids_externos <- unlist(externos$n_docs_ids)

  atos <- documentos |>
    filter(!n_doc %in% ids_externos) |>
    left_join(conteudo, by = "n_doc") |>
    transmute(
      id = paste0("D", n_doc), n_doc, n_docs_ids = vector("list", n()), classe = "ato",
      rotulo, tipo_canonico, unidade, data, assinado_em,
      signatarios = map_chr(assinaturas, \(a) if (is.null(a) || !nrow(a)) NA_character_
                            else paste(a$signatario, collapse = " e ")),
      # nome e função de cada signatário, um par por linha, para exibição
      assinantes = map_chr(assinaturas, \(a) if (is.null(a) || !nrow(a)) NA_character_
                           else paste(sprintf("%s\n%s", a$signatario, a$cargo),
                                      collapse = "\n")),
      cargo_signatarios = map_chr(assinaturas, \(a) if (is.null(a) || !nrow(a)) NA_character_
                                  else paste(a$cargo, collapse = " e ")),
      assunto, destinatarios, referencias, prazo_informado,
      marcacao, recomendacao, email, complementos, mencoes_parecer,
      n_assinaturas, n_esperadas,
      completo = coalesce(n_assinaturas >= n_esperadas, TRUE)
    )

  entradas <- externos |>
    transmute(id, n_doc = NA_character_, n_docs_ids, classe = "protocolo_externo",
              rotulo, tipo_canonico = "Protocolo", unidade, data,
              assinado_em = as.POSIXct(NA), signatarios = NA_character_,
              cargo_signatarios = NA_character_, assinantes = NA_character_,
              assunto = detalhe,
              destinatarios = vector("list", n()), referencias = vector("list", n()),
              prazo_informado = as.Date(NA), marcacao = NA_character_,
              recomendacao = NA_character_, email = vector("list", n()),
              complementos = vector("list", n()),
              mencoes_parecer = vector("list", n()),
              n_assinaturas = NA_integer_, n_esperadas = NA_integer_,
              completo = TRUE)

  bind_rows(entradas, atos) |>
    arrange(data, classe != "protocolo_externo", n_doc)
}

# ---------------------------------------------------------------------
# Arestas
# ---------------------------------------------------------------------

arestas_por_referencia <- function(nos) {
  vazio <- tibble(de = character(), para = character(), tipo = character())
  # Documentos agrupados em um protocolo externo respondem pelo nó do
  # grupo: a citação de uma peça protocolada é, no grafo, uma seta que
  # parte da entrada de documentação.
  mapa <- bind_rows(
    nos |> filter(!is.na(n_doc)) |> select(n_doc, origem_id = id),
    nos |> filter(classe == "protocolo_externo") |>
      select(id, n_docs_ids) |> unnest(n_docs_ids) |>
      transmute(n_doc = n_docs_ids, origem_id = id)
  ) |> distinct(n_doc, .keep_all = TRUE)
  ref <- nos |>
    filter(map_int(referencias, length) > 0) |>
    select(destino_id = id, referencias) |>
    unnest(referencias) |>
    rename(n_doc = referencias) |>
    inner_join(mapa, by = "n_doc")
  if (!nrow(ref)) return(vazio)
  ref <- ref |> transmute(de = origem_id, para = destino_id, tipo = "referencia")

  # Um protocolo externo liga-se apenas ao primeiro ato que o cita: é o
  # que inaugura a análise daquela remessa. Os atos seguintes citam as
  # mesmas peças como objeto de exame, e não como fluxo de entrada.
  ordem <- setNames(seq_len(nrow(nos)), nos$id)
  grupos <- nos$id[nos$classe == "protocolo_externo"]
  bind_rows(
    ref |> filter(!de %in% grupos),
    ref |> filter(de %in% grupos) |>
      mutate(pos = ordem[para]) |>
      group_by(de) |> slice_min(pos, n = 1, with_ties = FALSE) |> ungroup() |>
      select(de, para, tipo)
  )
}

#' Destinatário de A que assina o ato B
#'
#' Procura primeiro adiante no tempo; não havendo resposta adiante,
#' aceita ato anterior do mesmo ciclo de análise — situação que ocorre
#' quando o parecer tem data nominal anterior à do despacho que o pede.
#' Não liga a alvo que já recebeu seta por referência: a citação
#' explícita tem precedência.
arestas_por_destinatario <- function(nos, ja_ligados = character()) {
  vazio <- tibble(de = character(), para = character(), tipo = character())
  atos <- nos |> filter(classe == "ato")
  if (!nrow(atos)) return(vazio)

  ciclos <- nos |>
    mutate(ciclo = cumsum(classe == "protocolo_externo")) |>
    select(id, ciclo)

  alvos <- atos |>
    transmute(de = id, data_de = data,
              nome = map_chr(destinatarios, \(d) {
                b <- blocos_dest(d)
                if (!length(b)) NA_character_ else normalizar_nome(b[1])
              })) |>
    filter(!is.na(nome), nchar(nome) > 6) |>
    left_join(ciclos, by = c("de" = "id")) |>
    rename(ciclo_de = ciclo)

  resp <- atos |>
    filter(!is.na(signatarios), !id %in% ja_ligados) |>
    transmute(para = id, data_para = data, assinante = normalizar_nome(signatarios)) |>
    left_join(ciclos, by = c("para" = "id")) |>
    rename(ciclo_para = ciclo)

  if (!nrow(alvos) || !nrow(resp)) return(vazio)

  ordem <- setNames(seq_len(nrow(nos)), nos$id)
  subst <- setNames(coalesce(nos$cargo_signatarios, ""), nos$id)

  pares <- expand_grid(alvos, resp) |>
    filter(para != de,
           str_detect(assinante, fixed(nome)) | str_detect(nome, fixed(assinante)) |
             # quem assina em substituição responde pelo pedido dirigido
             # à pessoa substituída, desde que seja o ato imediatamente
             # seguinte
             (eh_substituto(subst[para]) & ordem[para] == ordem[de] + 1L)) |>
    mutate(adiante = data_para >= data_de, mesmo_ciclo = ciclo_de == ciclo_para) |>
    filter(adiante | mesmo_ciclo)

  if (!nrow(pares)) return(vazio)

  # Cada pedido fica com a resposta mais proxima; respostas emitidas no
  # mesmo dia sao mantidas todas, porque um mesmo despacho costuma gerar
  # varios atos no mesmo lote. Um parecer pode, assim, receber setas de
  # mais de um despacho.
  pares |>
    mutate(dist = abs(as.numeric(data_para - data_de))) |>
    group_by(de) |>
    filter(adiante == max(adiante)) |>
    filter(dist == min(dist)) |>
    ungroup() |>
    transmute(de, para, tipo = "destinatario")
}


#' Ofício que comunica o ato ao interessado, ligado ao e-mail dos autos
#'
#' Quando o documento declara os endereços eletrônicos dos destinatários
#' e o e-mail registrado nos autos foi enviado a esses mesmos endereços,
#' a ligação está estabelecida. O rótulo traz apenas os blocos cujos
#' endereços coincidem — a unidade citada sem e-mail fica de fora.
arestas_por_email <- function(nos) {
  vazio <- tibble(de = character(), para = character(), tipo = character(),
                  rotulo_aresta = character())
  emails <- nos |>
    filter(str_detect(sem_acento(tolower(coalesce(tipo_canonico, ""))), "^e-?mail"))
  if (!nrow(emails)) return(vazio)

  fontes <- nos |> filter(classe == "ato", map_int(destinatarios, \(d) length(emails_dest(d))) > 0)
  if (!nrow(fontes)) return(vazio)

  map_dfr(seq_len(nrow(emails)), \(k) {
    e <- emails[k, ]
    alvo <- emails_do_email(e$email)
    if (!length(alvo)) return(NULL)
    cand <- fontes |> filter(data <= e$data)
    if (!nrow(cand)) return(NULL)
    achados <- map_dfr(seq_len(nrow(cand)), \(j) {
      f <- cand[j, ]
      d <- f$destinatarios[[1]]
      bate <- map_lgl(d$emails, \(x) any(tolower(x) %in% alvo))
      if (!any(bate)) return(NULL)
      # o rótulo mantém os blocos cujo endereço coincide e os que, sem
      # endereço declarado, são destinatários externos; a unidade interna
      # citada sem e-mail fica de fora
      sem_email <- map_int(d$emails, length) == 0
      manter <- bate | (sem_email & !eh_unidade_interna(d$bloco))
      tibble(de = f$id, para = e$id, tipo = "email",
             rotulo_aresta = paste(d$bloco[manter], collapse = "\n"),
             data_de = f$data)
    })
    if (!nrow(achados)) return(NULL)
    achados |> slice_max(data_de, n = 1, with_ties = FALSE) |> select(-data_de)
  })
}


#' Demanda aberta pelo despacho da unidade de triagem
#'
#' O despacho da CGM abre a demanda para emitir o parecer. Quando o
#' parecer demandado já consta dos autos, a seta o alcança ainda que não
#' haja citação por número. Quando não consta, o parecer entra como nó
#' tracejado de documentação pendente.
demandas_da_cgm <- function(nos) {
  vazio <- list(arestas = tibble(de = character(), para = character(),
                                 tipo = character(), rotulo_aresta = character()),
                nos = tibble())
  despachos <- nos |>
    filter(classe == "ato", !is.na(unidade), unidade == "CGM",
           map_int(assunto, \(a) length(demanda_do_despacho(a))) > 0)
  if (!nrow(despachos)) return(vazio)

  pareceres <- nos |>
    filter(classe == "ato",
           str_detect(sem_acento(tolower(coalesce(tipo_canonico, ""))), "^parecer"))

  arestas <- tibble(de = character(), para = character(),
                    tipo = character(), rotulo_aresta = character())
  pendentes <- tibble()

  for (k in seq_len(nrow(despachos))) {
    d <- despachos[k, ]
    esperados <- demanda_do_despacho(d$assunto)
    for (esp in esperados) {
      alvo <- pareceres |>
        filter(data >= d$data,
               str_starts(sem_acento(tolower(tipo_canonico)), sem_acento(tolower(esp)))) |>
        slice_head(n = 1)
      if (nrow(alvo)) {
        arestas <- add_row(arestas, de = d$id, para = alvo$id,
                           tipo = "demanda", rotulo_aresta = NA_character_)
      } else {
        id <- paste0("FALTA_", str_replace_all(sem_acento(esp), "\\W", ""))
        pendentes <- bind_rows(pendentes, tibble(
          id = id, classe = "faltante", tipo_canonico = "Faltante",
          rotulo = esp, unidade = NA_character_, data = as.Date(NA),
          assunto = sprintf("Documenta\u00e7\u00e3o pendente\nDemanda aberta em %s%s",
                            format(d$data, "%d/%m/%Y"),
                            if (!is.na(d$prazo_informado))
                              sprintf("\nPrazo expirado em %s",
                                      format(d$prazo_informado, "%d/%m/%Y")) else "")))
        arestas <- add_row(arestas, de = d$id, para = id,
                           tipo = "faltante", rotulo_aresta = NA_character_)
      }
    }
  }
  list(arestas = arestas, nos = distinct(pendentes, id, .keep_all = TRUE))
}

#' Fluxo abortado
#'
#' Um despacho encaminha a alguém para que emita o parecer. Se o parecer
#' da mesma demanda vem assinado por outra pessoa, e há despacho
#' posterior dirigido justamente a quem assinou, o encaminhamento
#' anterior não se concretizou: o fluxo foi abortado.
nos_fluxo_abortado <- function(nos, demandas) {
  vazio <- list(nos = tibble(), arestas = tibble())
  if (!nrow(demandas)) return(vazio)

  cgm <- nos |> filter(classe == "ato", coalesce(unidade, "") == "CGM")
  despachos <- nos |>
    filter(classe == "ato",
           str_detect(sem_acento(tolower(coalesce(tipo_canonico, ""))), "^despacho"),
           coalesce(unidade, "") != "CGM",
           map_int(destinatarios, \(d) length(blocos_dest(d))) > 0)
  if (!nrow(despachos)) return(vazio)

  achados <- map_dfr(seq_len(nrow(demandas)), \(k) {
    dem <- demandas[k, ]
    abre <- nos[nos$id == dem$de, ]
    parecer <- nos[nos$id == dem$para, ]
    if (!nrow(abre) || !nrow(parecer) || is.na(parecer$signatarios)) return(NULL)

    # a demanda vigora até o despacho seguinte da unidade de triagem
    fim <- cgm$data[cgm$data > abre$data]
    fim <- if (length(fim)) min(fim) else as.Date("9999-12-31")
    no_escopo <- despachos |> filter(data >= abre$data, data < fim)
    if (nrow(no_escopo) < 2) return(NULL)

    assina <- normalizar_nome(parecer$signatarios)
    casou <- map_lgl(no_escopo$destinatarios, \(d) {
      b <- blocos_dest(d)
      length(b) > 0 && nome_casa(normalizar_nome(str_split(b[1], "\n")[[1]][1]), assina)
    })
    # só há fluxo abortado se outro despacho, no mesmo escopo, alcançou
    # quem de fato assinou o parecer
    if (!any(casou) || all(casou)) return(NULL)
    no_escopo[!casou, ] |> transmute(id, destinatarios, alvo = parecer$id)
  })
  if (!nrow(achados)) return(vazio)
  achados <- distinct(achados, id, .keep_all = TRUE)

  novos <- achados |> transmute(
    id = paste0("ABORT_", str_remove(id, "^D")),
    classe = "abortado", tipo_canonico = "Abortado",
    rotulo = "Fluxo abortado",
    unidade = NA_character_, data = as.Date(NA),
    assunto = map_chr(destinatarios, \(d)
      sprintf("Encaminhamento a %s sem resposta\nO parecer veio de outro fluxo",
              str_replace_all(blocos_dest(d)[1], "\n", " "))))

  ar <- achados |> transmute(de = paste0("ABORT_", str_remove(id, "^D")),
                             para = id, tipo = "abortado",
                             rotulo_aresta = NA_character_)
  list(nos = novos, arestas = ar)
}

#' Data da última movimentação dos autos
#'
#' Nó informativo, sem ligação com o fluxo: informa a última data na
#' lista de protocolos e a última no histórico de andamentos.
no_ultima_movimentacao <- function(documentos, andamentos) {
  tibble(
    id = "ULT_MOV", classe = "informativo", tipo_canonico = "Informativo",
    rotulo = "\u00daltima movimenta\u00e7\u00e3o dos autos",
    unidade = NA_character_, data = as.Date(NA),
    assunto = sprintf("Lista de protocolos: %s\nLista de andamentos: %s",
                      format(max(documentos$data, na.rm = TRUE), "%d/%m/%Y"),
                      format(max(andamentos$ts, na.rm = TRUE), "%d/%m/%Y %H:%M")))
}

#' Desfaz ciclos de duas arestas
#'
#' Quando A e B apontam um para o outro, o elo verdadeiro é o que parte
#' do ato mais recente: é ele que pede, e o outro é a peça pedida, ainda
#' que tenha data nominal anterior.
remover_ciclos <- function(arestas, nos) {
  data_de <- setNames(nos$data, nos$id)
  arestas |>
    rowwise() |>
    filter({
      inverso <- any(arestas$de == para & arestas$para == de)
      !inverso || isTRUE(data_de[[de]] >= data_de[[para]])
    }) |>
    ungroup()
}

#' Protocolo externo sem entrada, ligado ao último e-mail que o antecede
#'
#' Comunicado o ato ao interessado, é ele quem protocola a peça seguinte.
arestas_email_protocolo <- function(nos, arestas) {
  vazio <- tibble(de = character(), para = character(), tipo = character())
  entradas <- nos |> filter(classe == "protocolo_externo", !id %in% arestas$para)
  emails <- nos |>
    filter(str_detect(sem_acento(tolower(coalesce(tipo_canonico, ""))), "^e-?mail"))
  if (!nrow(entradas) || !nrow(emails)) return(vazio)

  map_dfr(seq_len(nrow(entradas)), \(k) {
    e <- entradas[k, ]
    ant <- emails |> filter(data <= e$data) |> slice_max(data, n = 1, with_ties = FALSE)
    if (!nrow(ant)) return(NULL)
    tibble(de = ant$id, para = e$id, tipo = "protocolo")
  })
}

#' E-mail sem protocolo posterior, ligado à documentação ainda esperada
#'
#' Comunicado o ato ao interessado, segue-se o protocolo da peça
#' seguinte. Quando esse protocolo ainda não existe, a seta tracejada
#' alcança o nó da documentação esperada.
arestas_email_pendente <- function(nos, pendentes, arestas) {
  vazio <- tibble(de = character(), para = character(), tipo = character())
  alvo <- pendentes |> filter(classe == "pendente", str_detect(id, "RELATORIO"))
  if (!nrow(alvo)) return(vazio)
  emails <- nos |>
    filter(str_detect(sem_acento(tolower(coalesce(tipo_canonico, ""))), "^e-?mail"))
  if (!nrow(emails)) return(vazio)
  ja <- arestas |> filter(para %in% alvo$id) |> pull(de)
  soltos <- emails |>
    filter(!id %in% ja) |>
    rowwise() |>
    filter(!any(nos$classe == "protocolo_externo" & nos$data > data)) |>
    ungroup()
  if (!nrow(soltos)) return(vazio)
  tibble(de = soltos$id, para = alvo$id[1], tipo = "pendente")
}

#' Protocolo externo que abre novo ciclo de análise
#'
#' Liga-se ao primeiro despacho posterior que não o cita e que também
#' não é citado por ninguém: é o ato que inaugura a análise da remessa.
arestas_por_protocolo <- function(nos, arestas) {
  vazio <- tibble(de = character(), para = character(), tipo = character())
  entradas <- nos |> filter(classe == "protocolo_externo")
  atos <- nos |> filter(classe == "ato")
  if (!nrow(entradas) || !nrow(atos)) return(vazio)
  citados <- unique(arestas$para)

  # O protocolo que já se ligou a algum ato por citação está resolvido.
  resolvidos <- unique(arestas$de)

  map_dfr(seq_len(nrow(entradas)), \(k) {
    e <- entradas[k, ]
    if (e$id %in% resolvidos) return(NULL)
    # a busca se encerra no protocolo seguinte: cada remessa abre o seu
    # próprio ciclo de análise
    limite <- entradas$data[entradas$data > e$data]
    limite <- if (length(limite)) min(limite) else as.Date("9999-12-31")
    cand <- atos |>
      filter(data >= e$data, data < limite, !id %in% citados) |>
      arrange(data, id) |> slice_head(n = 1)
    if (!nrow(cand)) return(NULL)
    tibble(de = e$id, para = cand$id, tipo = "protocolo")
  })
}

arestas_por_sequencia <- function(nos, arestas) {
  sem_entrada <- setdiff(nos$id[-1], arestas$para)
  map_dfr(sem_entrada, \(alvo) {
    i <- which(nos$id == alvo)
    if (i <= 1) return(NULL)
    tibble(de = nos$id[i - 1], para = alvo, tipo = "sequencia")
  })
}

#' Rótulo da seta
#'
#' Só a regra do destinatário rotula. E, mesmo nela, o destinatário é
#' descartado em dois casos: quando não traz nome de pessoa — é unidade
#' ou coordenação, e o que informa a seta é o assunto — e quando coincide
#' com quem assina o próprio documento, sinal de anuência interna e não
#' de endereçamento.
aplicar_rotulos <- function(arestas, nos) {
  ordem <- setNames(seq_len(nrow(nos)), nos$id)
  dest_de <- setNames(nos$destinatarios, nos$id)
  data_de <- setNames(nos$data, nos$id)
  assina  <- setNames(nos$signatarios, nos$id)
  subst   <- setNames(coalesce(nos$cargo_signatarios, ""), nos$id)

  rotulo <- map_chr(seq_len(nrow(arestas)), \(k) {
    a <- arestas[k, ]
    if (!is.na(a$rotulo_aresta) && nzchar(a$rotulo_aresta)) return(a$rotulo_aresta)
    d <- dest_de[[a$de]]
    blocos <- blocos_dest(d)
    if (!length(blocos)) return(NA_character_)
    alvo_assina <- normalizar_nome(coalesce(assina[[a$para]], ""))
    nome <- normalizar_nome(str_split(blocos[1], "\n")[[1]][1])
    casa <- nzchar(alvo_assina) && nome_casa(nome, alvo_assina)
    # quem assina em substituicao responde pelo nome originalmente citado
    if (!casa && eh_substituto(subst[[a$para]]) &&
        ordem[[a$para]] == ordem[[a$de]] + 1L) casa <- TRUE
    if (casa) paste(blocos, collapse = "\n") else NA_character_
  })

  arestas |> mutate(rotulo_aresta = rotulo)
}

# ---------------------------------------------------------------------
# Nós tracejados: documentos ainda não juntados
# ---------------------------------------------------------------------

contem <- function(x, termos) {
  x <- sem_acento(tolower(coalesce(x, "")))
  map_lgl(x, \(v) any(map_lgl(termos, \(t) str_detect(v, fixed(t)))))
}

#' Unidade que recebeu o processo, ainda o detém e nada emitiu
nos_pendentes_unidade <- function(nos, andamentos) {
  vazio <- list(nos = tibble(), arestas = tibble())

  # Unidades que efetivamente produziram algum ato nos autos. Os
  # protocolos externos não contam: são entrada de documentação, não
  # manifestação de unidade.
  com_ato <- nos |> filter(classe == "ato") |> pull(unidade) |> na.omit() |> unique()

  # O encaminhamento é identificado pelo conteúdo do documento — cadastro
  # em base de dados georreferenciada — e confirmado pelo histórico: a
  # unidade recebeu o processo depois daquele documento e nada emitiu.
  encaminhamentos <- nos |>
    filter(classe == "ato",
           contem(assunto, TERMOS_DBGEO) |
             contem(map_chr(destinatarios, \(d) paste(d, collapse = " ")), TERMOS_DBGEO))
  if (!nrow(encaminhamentos)) return(vazio)

  achados <- map_dfr(seq_len(nrow(encaminhamentos)), \(k) {
    doc <- encaminhamentos[k, ]
    # Entre as unidades que receberam o processo depois do documento e
    # nada emitiram, interessa a que ainda o detém: aquela sem remessa
    # posterior. Unidades de trânsito remetem adiante e são descartadas.
    destino <- andamentos |>
      filter(rotulo == "recebido", as.Date(ts) >= doc$data, !unidade %in% com_ato) |>
      rowwise() |>
      filter(!any(andamentos$rotulo == "remetido" &
                    andamentos$origem == unidade & andamentos$ts > ts)) |>
      ungroup() |>
      arrange(ts) |> slice_head(n = 1)
    if (!nrow(destino)) return(NULL)
    tibble(de = doc$id,
           rotulo_aresta = paste(blocos_dest(doc$destinatarios[[1]]), collapse = "\n"),
           unidade = destino$unidade, ts = destino$ts)
  })
  if (!nrow(achados)) return(vazio)

  achados <- achados |> distinct(unidade, .keep_all = TRUE)

  novos <- achados |> transmute(
    id = paste0("PEND_", str_replace_all(unidade, "\\W", "")),
    classe = "pendente", rotulo = unidade, tipo_canonico = "Pendente",
    unidade, data = as.Date(ts),
    assunto = sprintf("Processo recebido na unidade em %s\nManifesta\u00e7\u00e3o pendente de resposta",
                      format(ts, "%d/%m/%Y %H:%M")))

  arestas <- achados |> transmute(
    de, para = paste0("PEND_", str_replace_all(unidade, "\\W", "")),
    tipo = "pendente", rotulo_aresta)

  list(nos = novos, arestas = arestas)
}

#' Peça do interessado esperada após a portaria de autorização
#'
#' Publicada a portaria, a próxima documentação a protocolar é o
#' relatório de avaliação de impacto. O nó é tracejado, e para ele
#' apontam a portaria e o ato que a comunica ao interessado.
no_pendente_relatorio <- function(nos) {
  portarias <- nos |>
    filter(classe == "ato", contem(coalesce(tipo_canonico, rotulo), TERMOS_PORTARIA))
  if (!nrow(portarias)) return(list(nos = tibble(), arestas = tibble()))

  ultima <- portarias |> slice_tail(n = 1)
  posterior <- nos |> filter(classe == "ato", data >= ultima$data) |> slice_tail(n = 1)

  novo <- tibble(
    id = "PEND_RELATORIO", classe = "pendente",
    rotulo = "Protocolo de documenta\u00e7\u00e3o - RAIPA",
    tipo_canonico = "Pendente", unidade = NA_character_,
    # sem data: trata-se de evento futuro
    data = as.Date(NA),
    assunto = "Relat\u00f3rio de avalia\u00e7\u00e3o de impacto a ser protocolado pelo interessado")

  arestas <- bind_rows(ultima, posterior) |>
    distinct(id, .keep_all = TRUE) |>
    transmute(de = id, para = "PEND_RELATORIO", tipo = "pendente",
              rotulo_aresta = NA_character_)
  list(nos = novo, arestas = arestas)
}

# ---------------------------------------------------------------------
# Prazos
# ---------------------------------------------------------------------

apurar_prazos <- function(nos) {
  aberturas <- nos |>
    filter(!is.na(prazo_informado)) |>
    transmute(id_abertura = id, unidade_abertura = unidade,
              data_abertura = data, limite = prazo_informado)
  if (!nrow(aberturas)) return(tibble())

  entradas <- nos |> filter(classe == "protocolo_externo") |>
    select(id_entrada = id, data_entrada = data)
  pareceres <- nos |>
    filter(str_detect(coalesce(tipo_canonico, ""), regex("Parecer", ignore_case = TRUE))) |>
    transmute(id_parecer = id, fim = as.Date(assinado_em))

  aberturas |>
    rowwise() |>
    mutate(
      inicio     = suppressWarnings(max(entradas$data_entrada[entradas$data_entrada <= data_abertura])),
      id_parecer = pareceres$id_parecer[which(pareceres$fim >= data_abertura)][1] %||% NA_character_,
      fim        = pareceres$fim[which(pareceres$fim >= data_abertura)][1] %||% as.Date(NA)
    ) |>
    ungroup() |>
    mutate(
      dias_corridos = as.integer(fim - inicio),
      dias_limite   = as.integer(limite - inicio),
      excedeu       = !is.na(fim) & fim > limite,
      atraso_dias   = ifelse(excedeu, as.integer(fim - limite), 0L)
    )
}


#' Exigência de complementação dirigida ao interessado
#'
#' Pareceres e ofícios que pedem complementação ou recomendam
#' providências recebem um nó próprio, de modo que a exigência fique
#' visível no fluxo em vez de escondida no corpo do documento.
nos_complemento <- function(nos) {
  vazio <- list(nos = tibble(), arestas = tibble())

  # Só interessa a exigência feita ao interessado a partir de um parecer
  # já emitido: o ato precisa citar o parecer pelo número e trazer, no
  # corpo, os termos que caracterizam o pedido.
  eh_parecer <- nos$classe == "ato" &
    str_detect(sem_acento(tolower(coalesce(nos$tipo_canonico, ""))), "^parecer")
  pareceres <- na.omit(nos$n_doc[eh_parecer])
  # numeração própria do parecer, com que os ofícios costumam citá-lo
  numeros <- na.omit(str_extract(nos$rotulo[eh_parecer], "\\d{1,5}$"))
  if (!length(pareceres) && !length(numeros)) return(vazio)

  alvo <- nos |>
    filter(classe == "ato",
           str_detect(sem_acento(tolower(coalesce(tipo_canonico, ""))), "^(parecer|oficio)"),
           map_int(complementos, length) > 0,
           map_lgl(referencias, \(r) any(r %in% pareceres)) |
             map_lgl(mencoes_parecer, \(m) any(m %in% numeros)))
  if (!nrow(alvo)) return(vazio)

  novos <- alvo |> transmute(
    id = paste0("COMP_", str_remove(id, "^D")),
    classe = "complemento", tipo_canonico = "Complemento",
    rotulo = "Complementa\u00e7\u00f5es solicitadas",
    unidade = NA_character_, data = as.Date(NA),
    # sem linha sem seta: o rótulo já diz o que é preciso saber
    assunto = NA_character_)

  ar <- alvo |> transmute(de = paste0("COMP_", str_remove(id, "^D")), para = id,
                          tipo = "complemento", rotulo_aresta = NA_character_)
  list(nos = novos, arestas = ar)
}

#' Assinaturas ainda pendentes
#'
#' Cada "De acordo" no corpo indica um anuente além de quem redigiu; a
#' forma "de acordo com" é texto corrido e não conta. O ato só se
#' aperfeiçoa quando reúne todas as assinaturas esperadas, e a diferença
#' entre o esperado e o coletado é o que falta.
nos_assinatura_pendente <- function(nos) {
  vazio <- list(nos = tibble(), arestas = tibble())
  alvo <- nos |>
    filter(classe == "ato", !is.na(n_esperadas), !is.na(n_assinaturas),
           n_esperadas > n_assinaturas, n_assinaturas > 0)
  if (!nrow(alvo)) return(vazio)

  novos <- alvo |> transmute(
    id = paste0("ASSIN_", str_remove(id, "^D")),
    classe = "assinatura", tipo_canonico = "Assinatura",
    rotulo = sprintf("%d assinatura(s) pendente(s)", n_esperadas - n_assinaturas),
    unidade = NA_character_, data = as.Date(NA),
    assunto = sprintf("Coletadas %d de %d assinaturas esperadas\nO ato ainda n\u00e3o se aperfei\u00e7oou",
                      n_assinaturas, n_esperadas))

  ar <- alvo |> transmute(de = paste0("ASSIN_", str_remove(id, "^D")), para = id,
                          tipo = "assinatura", rotulo_aresta = NA_character_)
  list(nos = novos, arestas = ar)
}

#' Nó que informa o cumprimento do prazo, apontando para o parecer
nos_de_prazo <- function(nos) {
  alvo <- nos |> filter(!is.na(excedeu))
  if (!nrow(alvo)) return(list(nos = tibble(), arestas = tibble()))

  novos <- alvo |> transmute(
    id = paste0("PRZ_", str_remove(id, "^D")),
    classe = "prazo", tipo_canonico = "Prazo",
    rotulo = if_else(excedeu,
      sprintf("Manifesta\u00e7\u00e3o FORA do prazo\n%d dias corridos de atraso\n(limite %s)",
              atraso_dias, format(limite, "%d/%m/%Y")),
      sprintf("Manifesta\u00e7\u00e3o DENTRO do prazo\n(limite %s)", format(limite, "%d/%m/%Y"))),
    unidade = NA_character_, data = as.Date(NA), excedeu)

  arestas <- alvo |> transmute(
    de = paste0("PRZ_", str_remove(id, "^D")), para = id,
    tipo = "prazo", rotulo_aresta = NA_character_)

  list(nos = novos, arestas = arestas)
}

# ---------------------------------------------------------------------
# Montagem
# ---------------------------------------------------------------------

montar_grafo <- function(acervo) {
  cabecalho  <- ler_cabecalho(acervo$autos)
  documentos <- ler_documentos(acervo$autos)
  andamentos <- ler_andamentos(acervo$autos)
  conteudo   <- ler_documentos_nativos(acervo)

  nos <- montar_nos(cabecalho, documentos, conteudo, andamentos)

  a1   <- arestas_por_referencia(nos)
  a2   <- arestas_por_destinatario(nos, ja_ligados = a1$para)
  ae   <- arestas_por_email(nos)
  a12  <- bind_rows(ae, a1, a2) |> distinct(de, para, .keep_all = TRUE)
  a12  <- remover_ciclos(a12, nos)
  aep  <- arestas_email_protocolo(nos, a12)
  a12  <- bind_rows(a12, aep) |> distinct(de, para, .keep_all = TRUE)
  a3   <- arestas_por_protocolo(nos, a12)
  a123 <- bind_rows(a12, a3) |> distinct(de, para, .keep_all = TRUE)
  a4   <- arestas_por_sequencia(nos, a123)

  arestas <- bind_rows(a123, a4) |>
    distinct(de, para, .keep_all = TRUE)
  if (!"rotulo_aresta" %in% names(arestas)) arestas$rotulo_aresta <- NA_character_
  arestas <- aplicar_rotulos(arestas, nos)

  dm <- demandas_da_cgm(nos)
  arestas <- bind_rows(arestas, dm$arestas |> filter(tipo == "demanda")) |>
    distinct(de, para, .keep_all = TRUE)

  ab <- nos_fluxo_abortado(nos, dm$arestas |> filter(tipo == "demanda"))
  pu <- nos_pendentes_unidade(nos, andamentos)
  pr <- no_pendente_relatorio(nos)
  pendentes <- bind_rows(pu$nos, pr$nos, dm$nos, ab$nos,
                         no_ultima_movimentacao(documentos, andamentos))
  arestas   <- bind_rows(arestas, pu$arestas, pr$arestas,
                         dm$arestas |> filter(tipo == "faltante"), ab$arestas)
  arestas   <- bind_rows(arestas, arestas_email_pendente(nos, pendentes, arestas))

  prazos <- apurar_prazos(nos)
  nos <- nos |>
    left_join(prazos |> select(id = id_parecer, excedeu, atraso_dias, limite),
              by = "id")

  # A situação do prazo ganha nó próprio, que aponta para o parecer: a
  # informação é sobre o cumprimento do prazo, e não um atributo visual
  # do ato.
  pz <- nos_de_prazo(nos)
  cp <- nos_complemento(nos)
  ap <- nos_assinatura_pendente(nos)
  pendentes <- bind_rows(pendentes, pz$nos, cp$nos, ap$nos)
  arestas   <- bind_rows(arestas, pz$arestas, cp$arestas, ap$arestas)

  # Ato que não recebe nem dirige seta alguma do fluxo fica isolado nos
  # autos: sinalizá-lo é o que permite encontrá-lo.
  fluxo <- arestas |> filter(!tipo %in% c("prazo", "complemento", "assinatura"))
  nos <- nos |> mutate(avulso = classe == "ato" & !id %in% c(fluxo$de, fluxo$para))

  list(cabecalho = cabecalho, nos = nos, arestas = arestas,
       pendentes = pendentes, andamentos = andamentos, prazos = prazos)
}
