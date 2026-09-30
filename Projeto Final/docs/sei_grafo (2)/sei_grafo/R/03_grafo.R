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

# Tipos que constituem ato do processo ainda que o inteiro teor não
# esteja disponível em HTML. Tudo o que estiver fora desta lista e não
# for nativo é documentação externa.
TIPOS_ATO <- c("despacho", "oficio", "parecer", "nota tecnica",
               "portaria", "termo de referencia", "e-mail")

# Ato restrito: o rótulo visível identifica o tipo e traz numeração
# própria, mas o cadeado impede a leitura do inteiro teor. O ofício
# nativo sempre vem numerado; o externo, não.
RE_ATO_RESTRITO <- "^(Despacho|Parecer|Of\u00edcio|Oficio|Nota T\u00e9cnica)\\b.*\\d"

eh_ato_restrito <- function(rotulo, restrito) {
  coalesce(restrito, FALSE) & str_detect(coalesce(rotulo, ""), RE_ATO_RESTRITO)
}

# Tipos que, quando gerados no sistema, vêm com numeração própria no
# rótulo — "Ofício 123", "Despacho 41". Sem inteiro teor e sem número, o
# documento é anexo do interessado, e não ato da administração.
TIPOS_NUMERADOS <- c("despacho", "oficio", "parecer", "nota tecnica")

eh_tipo_de_ato <- function(tipo, rotulo = NULL, nativo = FALSE) {
  alvo <- sem_acento(tolower(coalesce(tipo, "")))
  de_ato <- map_lgl(alvo, \(a) any(str_starts(a, TIPOS_ATO)))
  if (is.null(rotulo)) return(de_ato)

  numerado <- map_lgl(alvo, \(a) any(str_starts(a, TIPOS_NUMERADOS)))
  tem_numero <- str_detect(coalesce(rotulo, ""), "\\d")
  # e-mail anexado ao processo não é ato: sem inteiro teor, nada se sabe
  # do que motivou sua juntada
  email <- str_starts(alvo, "e-?mail")
  de_ato & (!numerado | nativo | tem_numero) & (!email | nativo)
}
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
  if (is.null(d)) return(character(0))
  if (is.list(d) && !is.data.frame(d)) d <- d[[1]]
  if (is.null(d) || !is.data.frame(d) || !nrow(d)) character(0) else d$bloco
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

# Palavras que não distinguem uma função de outra
# Palavras de hierarquia, comuns a funções de áreas distintas: sozinhas
# não bastam para dizer que uma substitui a outra.
CARGO_VAZIO <- c("substituto", "substituta", "iphan", "divisao", "coordenacao",
                 "coordenador", "coordenadora", "superintendente", "diretor",
                 "diretora", "analista", "nacional", "geral", "tecnico",
                 "tecnica", "chefe", "escritorio", "de", "do", "da", "dos", "das")

#' Tokens que caracterizam uma função
tokens_cargo <- function(x) {
  t <- str_split(sem_acento(tolower(coalesce(x, ""))), "[^a-z]+")[[1]]
  unique(t[nchar(t) > 3])
}

#' A função endereçada é a de quem assina?
#'
#' O documento pode dirigir-se ao gabinete ou à chefia, sem nomear a
#' pessoa. Nesse caso a correspondência é entre a função endereçada e o
#' cargo de quem assina o ato seguinte.
funcao_casa <- function(bloco, cargo) {
  if (is.na(bloco) || is.na(cargo)) return(FALSE)
  # aqui o termo de hierarquia é justamente o que identifica a função
  # endereçada: "Gabinete da Superintendência" responde-se com
  # "Superintendente". Só o nome da instituição é descartado.
  generico <- c("iphan", "brasilia", "processo", "nacional")
  raiz <- function(x) {
    t <- setdiff(tokens_cargo(x), generico)
    unique(str_sub(t, 1, 8))
  }
  comuns <- intersect(raiz(bloco), raiz(cargo))
  any(nchar(comuns) >= 6)
}

#' Quem assina substitui o destinatário?
#'
#' Não basta que o signatário exerça substituição: é preciso que
#' substitua justamente a função a que o documento se dirigiu. O chefe
#' substituto de uma divisão não responde por pedido endereçado ao
#' coordenador de outra área.
substitui_destinatario <- function(bloco, cargos) {
  if (is.na(bloco) || is.na(cargos)) return(FALSE)
  linhas <- str_split(bloco, "\n")[[1]]
  if (length(linhas) < 2) return(FALSE)
  funcao <- paste(linhas[-1], collapse = " ")

  # cada signatário responde pela própria função: só interessa aquele que
  # assina em substituição, comparado ao seu próprio cargo
  partes <- str_split(cargos, "\\s+e\\s+")[[1]]
  partes <- partes[eh_substituto(partes)]
  if (!length(partes)) return(FALSE)

  any(map_lgl(partes, \(cargo) {
    a <- setdiff(tokens_cargo(funcao), CARGO_VAZIO)
    b <- setdiff(tokens_cargo(cargo), CARGO_VAZIO)
    length(intersect(a, b)) > 0 ||
      length(intersect(tokens_cargo(funcao), tokens_cargo(cargo))) >= 2
  }))
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
# O que o Termo de Referência exige
# ---------------------------------------------------------------------

# Cada item assinalado no TRE define os pareceres esperados e, quando é
# termo de compromisso, dispensa uma família inteira de pareceres.
TRE_ITENS <- tribble(
  ~chave,                                          ~demanda,                                             ~dispensa,
  "termo de compromisso.*bens registrados",        NA_character_,
    "Parecer PGBIR Imat|Parecer RAIBIR Imat|Parecer RPGBIR Imat",
  "termo de compromisso.*bens arqueologicos",      NA_character_,
    "Parecer PAA Proj/Acomp/Arq|Parecer PAIPA Arq|Parecer PAPIPA Arq|Parecer RAA Arq|Parecer RAIPA Arq",
  "estudos.*bens imateriais registrados",          "Parecer RAIBIR Imat",                                NA_character_,
  "estudos.*patrimonio material",                  "Parecer RAIPM Mat",                                  NA_character_,
  "estudos.*bens arqueologicos.*nivel ii$",        "Parecer PAA Proj/Acomp/Arq|Parecer RAA Arq",         NA_character_,
  "estudos.*bens arqueologicos.*nivel iii$",       "Parecer PAIPA Arq|Parecer RAIPA Arq",                NA_character_,
  "estudos.*bens arqueologicos.*nivel iv$",        "Parecer PAPIPA Arq",                                 NA_character_
)

#' Pareceres demandados e dispensados pelo Termo de Referência
#'
#' Lê os itens assinalados e devolve, de um lado, os pareceres que se
#' espera ver nos autos e, de outro, as famílias dispensadas pelo termo
#' de compromisso — quando o empreendedor assume compromisso, não há
#' estudo a analisar.
leitura_do_tre <- function(itens) {
  vazio <- list(demanda = character(0), dispensa = character(0), tce = character(0))
  if (!length(itens)) return(vazio)
  alvo <- sem_acento(tolower(str_squish(itens)))

  bate <- function(padrao) any(str_detect(alvo, padrao))
  linhas <- TRE_ITENS[map_lgl(TRE_ITENS$chave, bate), ]
  if (!nrow(linhas)) return(vazio)

  parte <- function(x) unlist(str_split(na.omit(x), "\\|"))
  list(
    demanda  = unique(parte(linhas$demanda)),
    dispensa = unique(parte(linhas$dispensa)),
    tce      = itens[str_detect(alvo, "termo de compromisso")]
  )
}

#' Demandas abertas pelo Termo de Referência
#'
#' Só valem quando a triagem encaminhou a Ficha de Caracterização para
#' análise manual: na análise automática não há estudo demandado, e sim
#' notificação ao interessado.
demandas_do_tre <- function(nos) {
  vazio <- list(arestas = tibble(de = character(), para = character(),
                                 tipo = character(), rotulo_aresta = character()),
                nos = tibble(), dispensa = character(0))

  manual <- nos |>
    filter(classe == "ato", coalesce(unidade, "") == "CGM",
           str_detect(sem_acento(tolower(coalesce(assunto, ""))), "analise manual"))
  if (!nrow(manual)) return(vazio)

  tre <- nos |>
    filter(classe == "ato", map_int(itens_marcados, length) > 0,
           str_detect(sem_acento(tolower(coalesce(tipo_canonico, ""))), "termo de referencia"))
  if (!nrow(tre)) return(vazio)
  tre <- tre |> slice_tail(n = 1)

  leitura <- leitura_do_tre(tre$itens_marcados[[1]])
  if (!length(leitura$demanda) && !length(leitura$dispensa)) return(vazio)

  pareceres <- nos |>
    filter(classe == "ato",
           str_detect(sem_acento(tolower(coalesce(tipo_canonico, ""))), "^parecer|^nota tecnica"))

  arestas <- tibble(de = character(), para = character(),
                    tipo = character(), rotulo_aresta = character())
  faltantes <- tibble()

  for (esp in leitura$demanda) {
    alvo <- pareceres |>
      filter(data >= tre$data,
             str_starts(sem_acento(tolower(tipo_canonico)), sem_acento(tolower(esp)))) |>
      slice_head(n = 1)
    # pareceres de outra nomenclatura, emitidos depois do TRE, substituem
    # o de rótulo canônico; parecer e nota técnica substituem os de
    # patrimônio material e imaterial
    # O parecer de relatório não admite substituto: é peça específica e
    # posterior, que só se tem por entregue sob o próprio rótulo.
    relatorio <- str_detect(sem_acento(tolower(esp)), "^parecer r")

    if (!nrow(alvo) && !relatorio) {
      # o substituto precisa tratar do mesmo componente cultural: um
      # parecer técnico genérico não supre parecer de outra área
      comp <- componente_de(esp)
      substituto <- pareceres |>
        filter(data >= tre$data,
               str_detect(sem_acento(tolower(tipo_canonico)),
                          "^parecer tecnico|^nota tecnica|^parecer -"),
               map_lgl(seq_len(n()), \(i)
                 identical(componente_de(tipo_canonico[i], corpo_norm[i]), comp))) |>
        slice_head(n = 1)
      if (nrow(substituto)) alvo <- substituto
    }
    if (nrow(alvo)) {
      arestas <- add_row(arestas, de = tre$id, para = alvo$id,
                         tipo = "demanda", rotulo_aresta = NA_character_)
    } else {
      id <- paste0("FALTA_", str_replace_all(sem_acento(esp), "\\W", ""))
      faltantes <- bind_rows(faltantes, tibble(
        id = id, classe = "faltante", tipo_canonico = "Faltante",
        rotulo = paste(esp, "- IN 06/2025"), unidade = NA_character_,
        data = as.Date(NA),
        assunto = sprintf("Exigido pelo Termo de Refer\u00eancia de %s",
                          format(tre$data, "%d/%m/%Y"))))
      arestas <- add_row(arestas, de = tre$id, para = id,
                         tipo = "faltante", rotulo_aresta = NA_character_)
    }
  }
  if (nrow(faltantes)) faltantes <- distinct(faltantes, id, .keep_all = TRUE)
  list(arestas = arestas, nos = faltantes, dispensa = leitura$dispensa)
}

# ---------------------------------------------------------------------
# Nós
# ---------------------------------------------------------------------

agrupar_protocolos_externos <- function(documentos, nativos) {
  # Documentação externa é tudo que não é ato nativo. A sequência
  # protocolada começa no Recibo, pertence a uma única unidade e se
  # encerra quando outra unidade registra documento nativo.
  d <- documentos |>
    mutate(eh_ato = n_doc %in% nativos |
             eh_tipo_de_ato(tipo_canonico, rotulo, n_doc %in% nativos) |
             eh_ato_restrito(rotulo, restrito) |
             # e-mail anexado tem tratamento próprio, à margem do fluxo:
             # não integra a documentação protocolada
             str_detect(sem_acento(tolower(coalesce(tipo_canonico, ""))), "^e-?mail"),
           eh_recibo = sem_acento(coalesce(tipo_canonico, "")) == "Recibo") |>
    arrange(ordem_arvore)

  grupo <- integer(nrow(d)); g <- 0L; unidade_g <- NA_character_; aberto <- FALSE
  for (i in seq_len(nrow(d))) {
    if (d$eh_ato[i]) {
      # ato de outra unidade encerra a sequência
      if (aberto && (is.na(unidade_g) || is.na(d$unidade[i]) || d$unidade[i] != unidade_g))
        aberto <- FALSE
      next
    }
    abre <- !aberto || d$eh_recibo[i] ||
      (!is.na(unidade_g) && !is.na(d$unidade[i]) && d$unidade[i] != unidade_g)
    if (abre) { g <- g + 1L; unidade_g <- d$unidade[i]; aberto <- TRUE }
    grupo[i] <- g
  }
  d$grupo <- grupo

  ext <- d |> filter(grupo > 0L)
  if (!nrow(ext))
    return(tibble(data = as.Date(character()), n_docs_ids = list(),
                  rotulo = character(), unidade = character(),
                  detalhe = character(), id = character()))

  ext |>
    group_by(grupo) |>
    summarise(
      data       = min(data),
      ordem_arvore = min(ordem_arvore),
      n_docs_ids = list(n_doc),
      # o detalhe é calculado antes de `rotulo` ser redefinido: dentro do
      # summarise, a coluna nova sombreia a original. O documento
      # restrito não tem tipo canônico, e é o rótulo visível que o
      # identifica.
      detalhe    = paste(sprintf("%s: %s",
                                 coalesce(tipo_canonico, rotulo, "Documento"), n_doc),
                         collapse = "\n"),
      rotulo     = "Documenta\u00e7\u00e3o externa protocolada",
      unidade    = first(na.omit(unidade)) %||% "SAIP",
      .groups    = "drop"
    ) |>
    mutate(id = paste0("EXT_", format(data, "%Y%m%d"), "_", grupo)) |>
    select(-grupo)
}
montar_nos <- function(cabecalho, documentos, conteudo, andamentos) {
  # São atos apenas os documentos nativos do SEI efetivamente lidos.
  nativos <- conteudo$n_doc[coalesce(conteudo$nativo, FALSE)]
  externos <- agrupar_protocolos_externos(documentos, nativos)
  ids_externos <- unlist(externos$n_docs_ids)

  atos <- documentos |>
    filter(!n_doc %in% ids_externos,
           n_doc %in% nativos |
             eh_tipo_de_ato(tipo_canonico, rotulo, n_doc %in% nativos)) |>
    left_join(conteudo, by = "n_doc") |>
    transmute(
      id = paste0("D", n_doc), n_doc, ordem_arvore,
      n_docs_ids = vector("list", n()), classe = "ato",
      rotulo, tipo_canonico, unidade, data, assinado_em,
      signatarios = map_chr(assinaturas, \(a) if (is.null(a) || !nrow(a)) NA_character_
                            else paste(a$signatario, collapse = " e ")),
      # nome e função de cada signatário, um par por linha, para exibição
      assinantes = map_chr(assinaturas, \(a) if (is.null(a) || !nrow(a)) NA_character_
                           else paste(sprintf("%s\n%s", a$signatario, a$cargo),
                                      collapse = "\n")),
      cargo_signatarios = map_chr(assinaturas, \(a) if (is.null(a) || !nrow(a)) NA_character_
                                  else paste(a$cargo, collapse = " e ")),
      componente_corpo,
      assunto, destinatarios, referencias, prazo_informado,
      dbgeo, enquadramento, corpo_norm, marcacao, itens_marcados, negritos,
      recomendacao, email, complementos, mencoes_parecer,
      n_assinaturas, n_esperadas,
      completo = coalesce(n_assinaturas >= n_esperadas, TRUE)
    )

  entradas <- if (!nrow(externos)) externos[0, ] else externos |>
    transmute(id, n_doc = NA_character_, ordem_arvore, n_docs_ids,
              classe = "protocolo_externo",
              rotulo, tipo_canonico = "Protocolo", unidade, data,
              assinado_em = as.POSIXct(NA), signatarios = NA_character_,
              cargo_signatarios = NA_character_, assinantes = NA_character_,
              componente_corpo = NA_character_,
              assunto = detalhe,
              destinatarios = vector("list", n()), referencias = vector("list", n()),
              prazo_informado = as.Date(NA), marcacao = NA_character_,
              dbgeo = FALSE, enquadramento = FALSE, corpo_norm = NA_character_,
              itens_marcados = vector("list", n()), negritos = vector("list", n()),
              recomendacao = NA_character_, email = vector("list", n()),
              complementos = vector("list", n()),
              mencoes_parecer = vector("list", n()),
              n_assinaturas = NA_integer_, n_esperadas = NA_integer_,
              completo = TRUE)

  # O parecer de rótulo genérico recebe a nomenclatura canônica da IN
  # conforme o componente cultural declarado no corpo do ato.
  bind_rows(entradas, atos) |>
    mutate(tipo_canonico = dplyr::case_when(
      classe == "ato" &
        str_detect(sem_acento(tolower(coalesce(tipo_canonico, ""))),
                   "^parecer\\s*-?\\s*ficha de caracteriza") &
        componente_corpo == "arqueologico" ~ "Parecer FCA Arq - IN 06/2025",
      classe == "ato" &
        str_detect(sem_acento(tolower(coalesce(tipo_canonico, ""))),
                   "^parecer\\s*-?\\s*ficha de caracteriza") &
        componente_corpo == "material"     ~ "Parecer FCA Mat - IN 06/2025",
      classe == "ato" &
        str_detect(sem_acento(tolower(coalesce(tipo_canonico, ""))),
                   "^parecer\\s*-?\\s*ficha de caracteriza") &
        componente_corpo == "imaterial"    ~ "Parecer FCA Imat - IN 06/2025",
      TRUE ~ tipo_canonico)) |>
    arrange(ordem_arvore)
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
arestas_por_destinatario <- function(nos, ja_ligados = NULL) {
  vazio <- tibble(de = character(), para = character(), tipo = character())
  atos <- nos |> filter(classe == "ato")
  if (!nrow(atos)) return(vazio)

  ciclos <- nos |>
    mutate(ciclo = cumsum(classe == "protocolo_externo")) |>
    select(id, ciclo)

  # um documento pode dirigir-se a mais de uma pessoa: cada bloco de
  # destinatário abre a sua própria expectativa de resposta
  alvos <- atos |>
    transmute(de = id, data_de = data,
              blocos = map(destinatarios, blocos_dest)) |>
    tidyr::unnest_longer(blocos, values_to = "nome_bloco") |>
    mutate(nome = normalizar_nome(map_chr(str_split(coalesce(nome_bloco, ""), "\n"),
                                          \(x) x[1]))) |>
    filter(!is.na(nome), nchar(nome) > 6) |>
    left_join(ciclos, by = c("de" = "id")) |>
    rename(ciclo_de = ciclo)

  resp <- atos |>
    filter(!is.na(signatarios)) |>
    transmute(para = id, data_para = data, assinante = normalizar_nome(signatarios),
              cargo_para = coalesce(cargo_signatarios, "")) |>
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
             (map2_lgl(nome_bloco, subst[para], substitui_destinatario) &
                ordem[para] == ordem[de] + 1L)) |>
    # a proximidade é medida pela posição na árvore do processo, e não
    # pela data: atos do mesmo dia têm a ordem em que foram juntados
    mutate(pos_de = ordem[de], pos_para = ordem[para],
           adiante = pos_para > pos_de, mesmo_ciclo = ciclo_de == ciclo_para) |>
    filter(adiante | mesmo_ciclo)

  if (!nrow(pares)) return(vazio)

  # Cada pedido fica com a resposta mais proxima; respostas emitidas no
  # mesmo dia sao mantidas todas, porque um mesmo despacho costuma gerar
  # varios atos no mesmo lote. Um parecer pode, assim, receber setas de
  # mais de um despacho.
  pares |>
    mutate(dist = abs(pos_para - pos_de)) |>
    group_by(de, nome) |>
    filter(adiante == max(adiante)) |>
    # a resposta mais próxima na árvore; atos lavrados no mesmo dia
    # formam um só lote e são mantidos juntos
    filter(dist == min(dist) | data_para == data_para[which.min(dist)]) |>
    ungroup() |>
    transmute(de, para, tipo = "destinatario") |>
    # A resposta mais próxima já ligada por citação encerra a expectativa:
    # não se procura outra mais adiante.
    (\(x) if (is.null(ja_ligados) || !nrow(ja_ligados)) x
          else anti_join(x, ja_ligados |> select(de, para), by = c("de", "para")))()
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
  if (nrow(pendentes)) pendentes <- distinct(pendentes, id, .keep_all = TRUE)
  list(arestas = arestas, nos = pendentes)
}

#' Fluxo abortado
#'
#' Um despacho encaminha a alguém para que emita o parecer. Se o parecer
#' da mesma demanda vem assinado por outra pessoa, e há despacho
#' posterior dirigido justamente a quem assinou, o encaminhamento
#' anterior não se concretizou: o fluxo foi abortado.
nos_fluxo_abortado <- function(nos, demandas, arestas) {
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

    # o encaminhamento só é abortado quando não gerou ato algum: se o
    # destinatário assinou qualquer documento dos autos, houve resposta,
    # ainda que não o parecer desta demanda
    # quem foi endereçado respondeu se assinou qualquer ato dos autos,
    # ainda que não o parecer desta demanda
    assinantes <- normalizar_nome(na.omit(nos$signatarios))
    # o encaminhamento é abortado quando não gerou ato algum no fluxo:
    # nem seta de endereçamento, nem ato derivado do parecer
    respondeu <- arestas |>
      filter(tipo %in% c("destinatario", "enderecamento", "derivada", "referencia")) |>
      pull(de)
    recebeu_derivada <- arestas |> filter(tipo == "derivada") |> pull(para)
    no_escopo <- no_escopo |>
      filter(!id %in% respondeu, !id %in% recebeu_derivada)
    if (!nrow(no_escopo)) return(NULL)

    assina <- normalizar_nome(parecer$signatarios)
    casou <- map_lgl(no_escopo$destinatarios, \(d) {
      b <- blocos_dest(d)
      length(b) > 0 && nome_casa(normalizar_nome(str_split(b[1], "\n")[[1]][1]), assina)
    })
    # só há fluxo abortado se outro despacho, no mesmo escopo, alcançou
    # quem de fato assinou o parecer
    if (all(casou)) return(NULL)
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
  ud <- documentos |> filter(data == max(data, na.rm = TRUE)) |> slice_tail(n = 1)
  ua <- andamentos |> filter(ts == max(ts, na.rm = TRUE)) |> slice_tail(n = 1)
  tibble(
    id = "ULT_MOV", classe = "informativo", tipo_canonico = "Informativo",
    rotulo = "\u00daltima movimenta\u00e7\u00e3o dos autos",
    unidade = NA_character_, data = as.Date(NA),
    assunto = sprintf("Lista de protocolos: %s \u2014 %s\nLista de andamentos: %s \u2014 %s",
                      format(ud$data, "%d/%m/%Y"), coalesce(ud$unidade, "unidade n\u00e3o informada"),
                      format(ua$ts, "%d/%m/%Y %H:%M"), coalesce(ua$unidade, "unidade n\u00e3o informada")))
}


# Palavras sem poder de distinção no cruzamento de termos
TOKENS_VAZIOS <- c("empreendimento", "complexo", "situado", "municipio", "estado",
                   "referente", "processo", "iphan", "documento", "anexo",
                   "presente", "seguinte", "conforme", "atividade")

#' Tokens significativos de um texto
tokens_relevantes <- function(x) {
  t <- str_split(sem_acento(tolower(coalesce(x, ""))), "[^a-z0-9]+")[[1]]
  setdiff(unique(t[nchar(t) > 4]), TOKENS_VAZIOS)
}

#' Texto de referência de um e-mail: o próprio e o dos atos que ele encaminha
#'
#' O e-mail dos autos costuma apenas comunicar que segue um ofício. O
#' assunto de que ele trata está no ato encaminhado, e é lá que se
#' encontram os termos a cruzar com a documentação protocolada.
texto_do_email <- function(nos, email) {
  refs <- email$referencias[[1]]
  anexos <- nos |> filter(!is.na(n_doc), n_doc %in% refs)
  paste(c(coalesce(email$assunto, ""), coalesce(email$corpo_norm, ""),
          coalesce(anexos$assunto, ""), coalesce(anexos$corpo_norm, "")),
        collapse = " ")
}

#' E-mail que comunica o ato e a documentação que se segue
#'
#' Comunicado o ato ao interessado, é ele quem protocola a peça seguinte.
#' O vínculo é de conteúdo: os termos do ato encaminhado — termo de
#' compromisso, componente cultural, complementação — reaparecem no
#' rótulo dos documentos protocolados. Quando o ato encaminhado é
#' portaria e nenhum relatório consta dos autos, o que se espera é
#' justamente o relatório.
arestas_email_documentacao <- function(nos, pendentes) {
  vazio <- tibble(de = character(), para = character(), tipo = character(),
                  rotulo_aresta = character())
  emails <- nos |>
    filter(classe == "ato",
           str_detect(sem_acento(tolower(coalesce(tipo_canonico, ""))), "^e-?mail"))
  if (!nrow(emails)) return(vazio)

  entradas <- nos |> filter(classe == "protocolo_externo")
  tem_relatorio <- any(str_detect(sem_acento(tolower(coalesce(nos$rotulo, ""))),
                                  "relatorio"))
  alvo_relatorio <- pendentes$id[pendentes$classe == "pendente" &
                                   str_detect(pendentes$id, "RELATORIO")]

  map_dfr(seq_len(nrow(emails)), \(k) {
    e <- emails[k, ]
    # comunicação interna não provoca protocolo do interessado
    destinos <- emails_do_email(e$email)
    if (length(destinos) && all(str_detect(destinos, DOMINIO_INTERNO))) return(NULL)
    texto <- texto_do_email(nos, e)

    if (str_detect(sem_acento(tolower(texto)), "portaria") &&
        !tem_relatorio && length(alvo_relatorio)) {
      return(tibble(de = e$id, para = alvo_relatorio[1], tipo = "pendente",
                    rotulo_aresta = NA_character_, data_de = e$data))
    }

    cand <- entradas |> filter(ordem_arvore > e$ordem_arvore)
    if (!nrow(cand)) return(NULL)
    bag <- tokens_relevantes(texto)
    if (!length(bag)) return(NULL)

    # entre as entradas que compartilham termos com o ato comunicado,
    # vale a primeira: é a resposta do interessado àquela comunicação
    pontos <- map_int(cand$assunto, \(rot) length(intersect(bag, tokens_relevantes(rot))))
    cand <- cand[pontos >= 2, ]
    if (!nrow(cand)) return(NULL)
    melhor <- cand |> arrange(ordem_arvore) |> slice_head(n = 1)
    tibble(de = e$id, para = melhor$id, tipo = "protocolo",
           rotulo_aresta = NA_character_, data_de = e$data)
  }) |>
    # a documentação esperada é apontada pela comunicação mais recente:
    # a anterior já foi superada pelos atos seguintes
    group_by(para) |>
    filter(!str_detect(para, "RELATORIO") | data_de == max(data_de)) |>
    ungroup() |>
    select(-data_de)
}

#' Ato que decorre do parecer, assinado por quem o emitiu
#'
#' Emitido o parecer, seu próprio signatário costuma lavrar o ato que o
#' encaminha. Não havendo citação, a ligação é de sequência: o ato deriva
#' do parecer, e não do despacho que antes endereçou aquela pessoa.
arestas_pos_parecer <- function(nos, arestas) {
  vazio <- list(arestas = tibble(de = character(), para = character(),
                                 tipo = character(), rotulo_aresta = character()),
                remover = tibble(de = character(), para = character()))
  ordem <- setNames(seq_len(nrow(nos)), nos$id)
  pareceres <- nos |>
    filter(classe == "ato",
           str_detect(sem_acento(tolower(coalesce(tipo_canonico, ""))), "^parecer"),
           !is.na(signatarios))
  if (!nrow(pareceres)) return(vazio)

  citados <- arestas |> filter(tipo == "referencia") |> pull(para)
  atos <- nos |> filter(classe == "ato", !is.na(signatarios))

  achados <- map_dfr(seq_len(nrow(pareceres)), \(k) {
    p <- pareceres[k, ]
    # a derivação é de vizinhança: o ato que decorre do parecer vem logo
    # depois dele na árvore. Mais adiante, o vínculo é outro.
    seguinte <- atos |>
      filter(ordem[id] > ordem[p$id], ordem[id] <= ordem[p$id] + 2L,
             !id %in% citados,
             map_lgl(signatarios, \(x) nome_casa(normalizar_nome(p$signatarios),
                                                 normalizar_nome(x)))) |>
      arrange(ordem[id]) |> slice_head(n = 1)
    if (!nrow(seguinte)) return(NULL)
    tibble(de = p$id, para = seguinte$id)
  })
  if (!nrow(achados)) return(vazio)

  list(
    arestas = achados |> transmute(de, para, tipo = "derivada",
                                   rotulo_aresta = NA_character_),
    # a seta de endereçamento que apontava para o mesmo ato é desfeita
    remover = arestas |>
      filter(tipo %in% c("destinatario", "enderecamento"), para %in% achados$para) |>
      select(de, para)
  )
}

#' Desfaz setas que apontam para atos anteriores
#'
#' O fluxo acompanha a ordem dos autos: um ato não demanda outro que já
#' está juntado. Atos da mesma data seguem a ordem em que aparecem na
#' árvore do processo.
somente_adiante <- function(arestas, nos) {
  ordem <- setNames(seq_len(nrow(nos)), nos$id)
  arestas |>
    mutate(pde = ordem[de], ppara = ordem[para]) |>
    filter(is.na(pde) | is.na(ppara) | ppara > pde) |>
    select(-pde, -ppara)
}



#' Da ficha de caracterização ao termo de referência
#'
#' Encaminhada a ficha para análise manual, o parecer que a examina dá
#' subsídio à elaboração do termo de referência: é a sequência prevista
#' na norma — FCA, parecer de FCA, TRE. A ligação não se expressa por
#' citação nem por coincidência de vocabulário, e por isso precisa ser
#' declarada.
arestas_fca_tre <- function(nos) {
  vazio <- tibble(de = character(), para = character(), tipo = character(),
                  rotulo_aresta = character())
  tipo <- sem_acento(tolower(coalesce(nos$tipo_canonico, "")))

  tres <- nos[nos$classe == "ato" & str_detect(tipo, "termo de referencia"), ]
  pareceres_fca <- nos[nos$classe == "ato" & str_detect(tipo, "^parecer fca"), ]
  if (!nrow(tres) || !nrow(pareceres_fca)) return(vazio)

  atos <- nos |> filter(classe == "ato")

  map_dfr(seq_len(nrow(tres)), \(k) {
    t <- tres[k, ]
    antes <- pareceres_fca |> filter(ordem_arvore < t$ordem_arvore)
    if (!nrow(antes)) return(NULL)
    # o termo decorre do último ato da análise da ficha: ou o próprio
    # parecer, ou o expediente que o encaminha
    origem <- atos |> filter(ordem_arvore < t$ordem_arvore) |> slice_tail(n = 1)
    if (!nrow(origem)) return(NULL)
    tibble(de = origem$id, para = t$id, tipo = "fca_tre", rotulo_aresta = NA_character_)
  })
}

#' Endereçamento à função, e não à pessoa
#'
#' O documento pode dirigir-se ao gabinete ou à chefia sem nomear quem
#' os ocupa. A resposta é o ato seguinte assinado por quem exerce a
#' função endereçada.
arestas_por_funcao <- function(nos, arestas) {
  vazio <- tibble(de = character(), para = character(), tipo = character(),
                  rotulo_aresta = character())
  ordem <- setNames(seq_len(nrow(nos)), nos$id)
  atos <- nos |> filter(classe == "ato")
  if (!nrow(atos)) return(vazio)

  # O ramo que se encerrou em ciência ou em pendência não tem sucessor:
  # a unidade endereçada apenas tomou conhecimento, e o encaminhamento
  # não pede outro ato.
  encerrados <- arestas |>
    filter(tipo %in% c("ciencia", "pendente")) |> pull(de)

  blocos <- atos |>
    filter(!id %in% encerrados) |>
    transmute(de = id, b = map(destinatarios, blocos_dest)) |>
    tidyr::unnest_longer(b, values_to = "bloco") |>
    filter(!is.na(bloco))
  if (!nrow(blocos)) return(vazio)

  # só interessam os blocos que não nomeiam pessoa alguma dos autos: os
  # que nomeiam já são resolvidos pela correspondência de nomes
  assinantes <- normalizar_nome(na.omit(atos$signatarios))
  blocos <- blocos |>
    filter(!map_lgl(bloco, \(b) {
      alvo <- normalizar_nome(str_split(b, "\n")[[1]][1])
      nchar(alvo) > 6 && any(map_lgl(assinantes, \(x) nome_casa(alvo, x)))
    }))
  if (!nrow(blocos)) return(vazio)

  assinados <- atos |> filter(!is.na(cargo_signatarios)) |>
    select(para = id, cargo = cargo_signatarios)
  if (!nrow(assinados)) return(vazio)

  # o ato que responde precisa tratar do mesmo assunto: a função, por si,
  # não basta para ligar dois documentos sem relação de conteúdo
  assunto_de <- setNames(coalesce(atos$assunto, ""), atos$id)

  map_dfr(seq_len(nrow(blocos)), \(k) {
    b <- blocos[k, ]
    termos_origem <- tokens_relevantes(assunto_de[[b$de]])
    alvo <- assinados |>
      filter(ordem[para] > ordem[b$de],
             map_lgl(cargo, \(c) funcao_casa(b$bloco, c)),
             map_lgl(para, \(x)
               length(intersect(termos_origem, tokens_relevantes(assunto_de[[x]]))) >= 2)) |>
      arrange(ordem[para]) |> slice_head(n = 1)
    if (!nrow(alvo)) return(NULL)
    tibble(de = b$de, para = alvo$para, tipo = "destinatario",
           rotulo_aresta = b$bloco)
  }) |>
    (\(x) if (!nrow(x)) vazio else distinct(x, de, para, .keep_all = TRUE))()
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
    # quem assina em substituição responde pelo pedido dirigido à função
    # substituída, e apenas a ela
    if (!casa && substitui_destinatario(blocos[1], subst[[a$para]]) &&
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

  # Atos que encaminham o processo para cadastro em base
  # georreferenciada. Só ofícios e despachos encaminham; o parecer que
  # menciona georreferenciamento o faz como objeto de análise.
  encaminhamentos <- nos |>
    filter(classe == "ato", coalesce(dbgeo, FALSE),
           str_detect(sem_acento(tolower(coalesce(tipo_canonico, ""))), "^(oficio|despacho)"))
  if (!nrow(encaminhamentos)) return(vazio)

  novos <- tibble(); arestas <- tibble()

  for (k in seq_len(nrow(encaminhamentos))) {
    doc <- encaminhamentos[k, ]

    # A demanda é atendida quando outra unidade se pronuncia sobre o
    # mesmo assunto: a seta vai de um ato ao outro, sem nó intermediário.
    resposta <- encaminhamentos |>
      filter(data > doc$data, coalesce(unidade, "") != coalesce(doc$unidade, "")) |>
      slice_head(n = 1)
    if (nrow(resposta)) {
      arestas <- bind_rows(arestas, tibble(
        de = doc$id, para = resposta$id, tipo = "dbgeo",
        rotulo_aresta = paste(blocos_dest(doc$destinatarios[[1]]), collapse = "\n")))
      next
    }

    # Sem resposta nos autos, resta saber o que houve com a unidade
    # endereçada: se ela recebeu o processo, deu ciência e o ramo se
    # encerra; se não recebeu, a manifestação segue pendente.
    alvo <- unidade_do_destinatario(doc$destinatarios[[1]], andamentos, nos)
    if (is.na(alvo)) next
    # a unidade-mãe costuma repassar à unidade vinculada competente, e é
    # esta que responde pela demanda
    alvo <- resolver_unidade(alvo, andamentos, doc$data)

    recebeu <- andamentos |> filter(rotulo == "recebido", unidade == alvo, ts >= doc$data) |>
      arrange(ts) |> slice_head(n = 1)
    # recebeu e devolveu: deu ciência, e o ramo se encerra. Recebeu e
    # ainda detém: a manifestação segue pendente.
    devolveu <- nrow(recebeu) > 0 &&
      any(andamentos$rotulo == "remetido" & andamentos$origem == alvo &
            andamentos$ts > recebeu$ts)

    id <- paste0(if (devolveu) "CIEN_" else "PEND_", str_replace_all(alvo, "\\W", ""))
    novos <- bind_rows(novos, tibble(
      id = id,
      classe = if (devolveu) "ciencia" else "pendente",
      rotulo = alvo,
      tipo_canonico = if (devolveu) "Ciencia" else "Pendente",
      unidade = alvo,
      data = if (nrow(recebeu)) as.Date(recebeu$ts) else as.Date(NA),
      assunto = if (devolveu)
        sprintf("Recebimento confirmado em %s\nCi\u00eancia registrada; ramo encerrado",
                format(recebeu$ts, "%d/%m/%Y %H:%M"))
      else if (nrow(recebeu))
        sprintf("Processo recebido em %s\nManifesta\u00e7\u00e3o pendente de resposta",
                format(recebeu$ts, "%d/%m/%Y %H:%M"))
      else "Manifesta\u00e7\u00e3o pendente de resposta"))
    arestas <- bind_rows(arestas, tibble(
      de = doc$id, para = id,
      tipo = if (devolveu) "ciencia" else "pendente",
      rotulo_aresta = paste(blocos_dest(doc$destinatarios[[1]]), collapse = "\n")))
  }

  if (nrow(novos)) novos <- distinct(novos, id, .keep_all = TRUE)
  list(nos = novos, arestas = arestas)
}

#' Unidade a que o documento se dirige
#'
#' O destinatário pode vir pela sigla, pelo nome da unidade ou por
#' referência genérica à superintendência, caso em que se resolve pela
#' unidade com sufixo de UF presente nos autos.
unidade_do_destinatario <- function(dest, andamentos, nos) {
  texto <- sem_acento(tolower(paste(blocos_dest(dest), collapse = " ")))
  unidades <- unique(na.omit(c(andamentos$unidade, nos$unidade)))
  if (!length(unidades)) return(NA_character_)

  exato <- unidades[map_lgl(unidades, \(u) str_detect(texto, fixed(sem_acento(tolower(u)))))]
  if (length(exato)) return(exato[which.max(nchar(exato))])

  if (str_detect(texto, "superintend")) {
    sup <- unidades[str_detect(unidades, "^IPHAN-[A-Z]{2}$")]
    if (length(sup)) return(sup[1])
  }
  if (str_detect(texto, "coordenacao-geral de licenciamento|cglic")) {
    cg <- unidades[unidades == "CGLic"]
    if (length(cg)) return(cg[1])
  }
  NA_character_
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

  # as setas para o nó vêm do e-mail que comunica a portaria; a portaria
  # só entra como origem se nenhum e-mail o fizer
  arestas <- ultima |>
    transmute(de = id, para = "PEND_RELATORIO", tipo = "pendente",
              rotulo_aresta = NA_character_)
  list(nos = novo, arestas = arestas)
}

# ---------------------------------------------------------------------
# Prazos
# ---------------------------------------------------------------------

apurar_prazos <- function(nos, demandas = NULL, alvos = nos) {
  aberturas <- nos |>
    filter(!is.na(prazo_informado)) |>
    transmute(id_abertura = id, unidade_abertura = unidade,
              data_abertura = data, limite = prazo_informado,
              assunto_abertura = assunto)
  if (!nrow(aberturas)) return(tibble())

  entradas <- nos |> filter(classe == "protocolo_externo") |>
    select(id_entrada = id, data_entrada = data)

  # marco inicial: a documentação externa protocolada que antecede o ato
  aberturas <- aberturas |>
    rowwise() |>
    mutate(inicio = {
      antes <- entradas$data_entrada[entradas$data_entrada <= data_abertura]
      if (length(antes)) max(antes) else as.Date(NA)
    }) |>
    ungroup()

  # Um despacho pode abrir mais de uma demanda — a análise da ficha de
  # caracterização alcança os três componentes culturais —, e cada uma
  # tem seu próprio relógio. Cada demanda vira uma linha.
  dem <- if (is.null(demandas) || !nrow(demandas)) tibble() else
    demandas |> filter(tipo %in% c("demanda", "faltante")) |>
      left_join(alvos |> select(para = id, rot_para = rotulo, tipo_para = tipo_canonico,
                                n_para = n_doc, ass_para = assinado_em,
                                classe_para = classe),
                by = "para") |>
      transmute(id_abertura = de, id_parecer = para, documento = n_para,
                # o tipo canônico identifica o parecer melhor que o rótulo
                # visível, que traz a numeração interna
                # o parecer é identificado pelo tipo canônico; o que
                # ainda não existe, pelo rótulo da demanda que o espera
                demanda = if_else(coalesce(classe_para, "faltante") == "faltante",
                                  rot_para, coalesce(tipo_para, rot_para)),
                fim = as.Date(ass_para),
                pendente = coalesce(classe_para, "faltante") == "faltante") |>
      distinct(id_abertura, id_parecer, .keep_all = TRUE)

  base <- if (nrow(dem)) {
    aberturas |>
      left_join(dem, by = "id_abertura", relationship = "one-to-many")
  } else {
    aberturas |> mutate(id_parecer = NA_character_, documento = NA_character_,
                        demanda = NA_character_, fim = as.Date(NA), pendente = NA)
  }

  # Despacho que fixou prazo sem demanda identificada: a demanda vem do
  # próprio assunto, quando ele nomeia o instrumento analisado.
  base |>
    mutate(demanda = coalesce(demanda, map_chr(assunto_abertura, demanda_declarada))) |>
    select(-assunto_abertura) |>
    mutate(
      dias_corridos = as.integer(fim - inicio),
      dias_limite   = as.integer(limite - inicio),
      excedeu       = !is.na(fim) & fim > limite,
      atraso_dias   = ifelse(excedeu, as.integer(fim - limite), 0L)
    )
}


# Termos que identificam o objeto de cada demanda sem parecer
TERMOS_DEMANDA <- c(
  "Termo de Compromisso" = "termo de compromisso",
  "Complementa"          = "complementac|complemento",
  "nquadramento"         = "enquadrament"
)

#' Ato que encerra a demanda percorrendo a cadeia de encaminhamentos
#'
#' Nem toda demanda termina em parecer. Quando a triagem pede a análise
#' de termo de compromisso, de complementação ou de enquadramento, o
#' despacho segue de mão em mão até quem examina o objeto. A cadeia é
#' percorrida pelas setas de endereçamento e de citação, e o ato que
#' encerra é o mais distante que ainda trata do mesmo objeto.
encerramento_por_cadeia <- function(nos, arestas, id_abertura, termos, max_saltos = 4L) {
  abertura <- nos[nos$id == id_abertura, ]
  if (!nrow(abertura)) return(NULL)
  alvo <- blocos_dest(abertura$destinatarios[[1]])
  if (!length(alvo)) return(NULL)
  # o nome pode estar em qualquer linha do bloco: a primeira às vezes
  # traz a unidade e a segunda, a pessoa ("À COTEC / Att Senhor Fulano")
  nomes_alvo <- normalizar_nome(str_split(paste(alvo, collapse = "\n"), "\n")[[1]])
  nomes_alvo <- nomes_alvo[nchar(nomes_alvo) >= 6]
  if (!length(nomes_alvo)) return(NULL)

  atos <- nos |> filter(classe == "ato", !is.na(signatarios))
  corrente <- abertura
  visitados <- id_abertura

  for (salto in seq_len(max_saltos)) {
    dest <- blocos_dest(corrente$destinatarios[[1]])
    if (!length(dest)) return(NULL)
    procurados <- normalizar_nome(str_split(paste(dest, collapse = "\n"), "\n")[[1]])
    procurados <- procurados[nchar(procurados) >= 6]
    if (!length(procurados)) return(NULL)

    # o ato seguinte é o que a pessoa endereçada assina, tratando do
    # mesmo objeto; atos dela sobre outros assuntos são ignorados
    # A data nominal do ato nem sempre segue a do pedido: um despacho
    # pode ser datado depois da resposta que o atende. Aceita-se uma
    # janela para trás, privilegiando o que vem adiante e o mais próximo.
    seguinte <- atos |>
      filter(data >= corrente$data - 10L, !id %in% visitados,
             map_lgl(signatarios, \(x)
               any(map_lgl(procurados, \(p) nome_casa(p, normalizar_nome(x))))),
             str_detect(sem_acento(tolower(paste(coalesce(assunto, ""),
                                                 coalesce(rotulo, "")))), termos)) |>
      arrange(data < corrente$data, abs(as.numeric(data - corrente$data)), id) |>
      slice_head(n = 1)
    if (!nrow(seguinte)) return(NULL)
    visitados <- c(visitados, seguinte$id)

    # a demanda se fecha quando a cadeia retorna a quem a recebeu
    volta <- blocos_dest(seguinte$destinatarios[[1]])
    if (length(volta)) {
      linhas_volta <- normalizar_nome(str_split(paste(volta, collapse = "\n"), "\n")[[1]])
      if (any(map_lgl(linhas_volta, \(v) any(map_lgl(nomes_alvo, \(n) nome_casa(n, v))))))
        return(seguinte)
    }

    corrente <- seguinte
  }
  NULL
}
#' Completa os prazos cuja demanda não termina em parecer
completar_encerramentos <- function(prazos, nos, arestas) {
  if (!nrow(prazos)) return(prazos)
  abertos <- which(is.na(prazos$fim) & !is.na(prazos$demanda))
  for (i in abertos) {
    chave <- names(TERMOS_DEMANDA)[map_lgl(names(TERMOS_DEMANDA),
                                           \(k) str_detect(prazos$demanda[i], fixed(k)))]
    if (!length(chave)) next
    ato <- encerramento_por_cadeia(nos, arestas, prazos$id_abertura[i],
                                   TERMOS_DEMANDA[[chave[1]]])
    if (is.null(ato) || !nrow(ato)) next
    prazos$id_parecer[i] <- ato$id
    prazos$documento[i]  <- ato$n_doc
    prazos$fim[i]        <- as.Date(ato$assinado_em)
  }
  prazos |>
    mutate(dias_corridos = as.integer(fim - inicio),
           excedeu       = !is.na(fim) & fim > limite,
           atraso_dias   = ifelse(excedeu, as.integer(fim - limite), 0L))
}

#' Instrumento nomeado no assunto do despacho
#'
#' Quando a triagem analisa termo de compromisso, o assunto declara qual
#' deles; não havendo parecer a esperar, é essa a demanda do prazo.
demanda_declarada <- function(assunto) {
  a <- sem_acento(tolower(coalesce(assunto, "")))
  if (!nzchar(a)) return(NA_character_)
  if (str_detect(a, "termo de compromisso.*bens arqueologicos"))
    return("Termo de Compromisso do Empreendedor - Bens Arqueol\u00f3gicos")
  if (str_detect(a, "termo de compromisso.*bens registrados"))
    return("Termo de Compromisso do Empreendedor - Bens Registrados")
  if (str_detect(a, "termo de compromisso"))
    return("Termo de Compromisso do Empreendedor")
  if (str_detect(a, "\\breenquadrament")) return("Reenquadramento do empreendimento")
  if (str_detect(a, "\\benquadrament"))   return("Enquadramento do empreendimento")
  NA_character_
}


#' Exigência de complementação dirigida ao interessado
#'
#' Pareceres e ofícios que pedem complementação ou recomendam
#' providências recebem um nó próprio, de modo que a exigência fique
#' visível no fluxo em vez de escondida no corpo do documento.
nos_complemento <- function(nos) {
  vazio <- list(nos = tibble(), arestas = tibble())

  tipo <- sem_acento(tolower(coalesce(nos$tipo_canonico, "")))
  eh_parecer <- nos$classe == "ato" & str_detect(tipo, "^parecer")
  pareceres <- na.omit(nos$n_doc[eh_parecer])
  numeros <- na.omit(str_extract(nos$rotulo[eh_parecer], "\\d{1,5}$"))

  # O parecer que exige complementação abre a demanda por si; o ofício
  # que a comunica precisa citar o parecer, pelo número SEI ou pela
  # numeração própria do documento.
  alvo <- nos |>
    mutate(tipo_norm = sem_acento(tolower(coalesce(tipo_canonico, "")))) |>
    filter(classe == "ato", map_int(complementos, length) > 0,
           str_detect(tipo_norm, "^(parecer|oficio)"),
           str_detect(tipo_norm, "^parecer") |
             map_lgl(referencias, \(r) any(r %in% pareceres)) |
             map_lgl(mencoes_parecer, \(m) any(m %in% numeros)))
  if (!nrow(alvo)) return(vazio)

  novos <- alvo |> transmute(
    id = paste0("COMP_", str_remove(id, "^D")),
    classe = "complemento", tipo_canonico = "Complemento",
    rotulo = "Complementa\u00e7\u00f5es solicitadas",
    unidade = NA_character_, data = as.Date(NA), assunto = NA_character_)

  ar <- alvo |> transmute(de = paste0("COMP_", str_remove(id, "^D")), para = id,
                          tipo = "complemento", rotulo_aresta = NA_character_)
  list(nos = novos, arestas = ar)
}


#' Destinatário que responde por mais de um ato
#'
#' Quando o documento se dirige a alguém e essa pessoa — ou quem a
#' substitui — assina dois ou mais atos dos autos, repetir o mesmo
#' rótulo em cada seta esconde que se trata de um único encaminhamento.
#' O destinatário passa a ser nó: o ato demandante chega até ele por
#' linha sem seta, e dele partem as setas para cada ato assinado.
nos_destinatario <- function(nos, arestas) {
  vazio <- list(nos = tibble(), arestas = tibble(), remover = tibble())

  candidatos <- arestas |>
    filter(tipo == "destinatario", !is.na(rotulo_aresta), nzchar(rotulo_aresta)) |>
    group_by(de, rotulo_aresta) |>
    filter(n() >= 2) |>
    ungroup()
  if (!nrow(candidatos)) return(vazio)

  chaves <- candidatos |> distinct(de, rotulo_aresta) |>
    mutate(id = paste0("DEST_", str_remove(de, "^D")))

  novos <- chaves |> transmute(
    id, classe = "destinatario", tipo_canonico = "Destinatario",
    rotulo = rotulo_aresta, unidade = NA_character_,
    data = as.Date(NA), assunto = NA_character_)

  ar <- bind_rows(
    # ato demandante -> destinatário, sem seta
    chaves |> transmute(de, para = id, tipo = "enderecamento",
                        rotulo_aresta = NA_character_),
    # destinatário -> cada ato que ele assinou
    candidatos |> left_join(chaves, by = c("de", "rotulo_aresta")) |>
      transmute(de = id, para, tipo = "destinatario", rotulo_aresta = NA_character_)
  )
  list(nos = novos, arestas = ar, remover = candidatos |> select(de, para))
}

#' Parecer que exige complementação e a documentação que o atende
#'
#' Solicitada a complementação, é o interessado quem lê o parecer,
#' identifica o que foi requerido e protocola a documentação. A seta do
#' parecer para a entrada de documentação seguinte registra esse
#' encadeamento, que nenhuma citação por número expressa.
arestas_complemento_protocolo <- function(nos) {
  vazio <- tibble(de = character(), para = character(), tipo = character(),
                  rotulo_aresta = character())
  pareceres <- nos |>
    filter(classe == "ato", map_int(complementos, length) > 0,
           str_detect(sem_acento(tolower(coalesce(tipo_canonico, ""))), "^parecer"))
  entradas <- nos |> filter(classe == "protocolo_externo")
  if (!nrow(pareceres) || !nrow(entradas)) return(vazio)

  map_dfr(seq_len(nrow(pareceres)), \(k) {
    p <- pareceres[k, ]
    prox <- entradas |> filter(data > p$data) |> slice_head(n = 1)
    if (!nrow(prox)) return(NULL)
    tibble(de = p$id, para = prox$id, tipo = "complementacao",
           rotulo_aresta = NA_character_)
  })
}

# Domínio da própria instituição: endereço interno não caracteriza
# notificação ao interessado.
DOMINIO_INTERNO <- "@iphan\\.gov\\.br$"

#' Ofício que declara e-mail do interessado e ainda não o notificou
#'
#' O ofício traz os endereços eletrônicos dos destinatários, mas não há,
#' nos autos, e-mail enviado a eles: a comunicação ficou pendente.
nos_email_pendente_oficio <- function(nos, arestas) {
  vazio <- list(nos = tibble(), arestas = tibble())
  tipo <- sem_acento(tolower(coalesce(nos$tipo_canonico, "")))
  ids_email <- nos$id[str_detect(tipo, "^e-?mail")]

  alvo <- nos |>
    filter(classe == "ato", str_detect(sem_acento(tolower(coalesce(tipo_canonico, ""))), "^oficio")) |>
    rowwise() |>
    filter({
      enderecos <- emails_dest(destinatarios)
      externos <- enderecos[!str_detect(enderecos, DOMINIO_INTERNO)]
      length(externos) > 0 && !any(arestas$de == id & arestas$para %in% ids_email)
    }) |>
    ungroup()
  if (!nrow(alvo)) return(vazio)

  novos <- alvo |> transmute(
    id = paste0("MAIL_", str_remove(id, "^D")),
    classe = "email_pendente", tipo_canonico = "EmailPendente",
    rotulo = "Aguardando envio de e-mail",
    unidade = NA_character_, data = as.Date(NA),
    assunto = map_chr(destinatarios, \(d) {
      e <- emails_dest(d)
      paste("Endere\u00e7os declarados no of\u00edcio:",
            paste(e[!str_detect(e, DOMINIO_INTERNO)], collapse = ", "))
    }))

  ar <- alvo |> transmute(de = paste0("MAIL_", str_remove(id, "^D")), para = id,
                          tipo = "email_pendente", rotulo_aresta = NA_character_)
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




#' Termo de Referência emitido automaticamente pelo sistema de avaliação
#'
#' Quando o termo consta entre os documentos iniciais, emitido pela
#' unidade do sistema de avaliação, a análise foi automática: não houve
#' parecer, e o termo decorre diretamente da documentação protocolada.
#' Como não é documento nativo, os termos que o caracterizam — nível,
#' termo de compromisso, componente cultural — são buscados no corpo do
#' despacho da triagem, onde costumam vir destacados em negrito.
tre_automatico <- function(nos) {
  vazio <- list(arestas = tibble(de = character(), para = character(),
                                 tipo = character(), rotulo_aresta = character()),
                assunto = tibble(id = character(), texto = character()))

  tre <- nos |>
    filter(classe == "ato",
           str_detect(sem_acento(tolower(coalesce(tipo_canonico, ""))), "termo de referencia"),
           map_int(itens_marcados, length) == 0)
  if (!nrow(tre)) return(vazio)

  entradas <- nos |> filter(classe == "protocolo_externo")
  cgm <- nos |> filter(classe == "ato", coalesce(unidade, "") == "CGM")

  res <- map_dfr(seq_len(nrow(tre)), \(k) {
    t <- tre[k, ]
    ant <- entradas |> filter(data <= t$data) |> slice_tail(n = 1)
    if (!nrow(ant)) return(NULL)
    termos <- if (nrow(cgm)) {
      d <- cgm |> filter(data <= t$data) |> slice_tail(n = 1)
      if (nrow(d)) {
        n <- d$negritos[[1]]
        n[str_detect(sem_acento(tolower(n)),
                     "nivel\\s+(i{1,3}|iv)\\b|termo de compromisso|patrimonio (arqueolog|material|imaterial)")]
      } else character(0)
    } else character(0)
    tibble(de = ant$id, para = t$id, texto = paste(termos, collapse = "\n"))
  })
  if (!nrow(res)) return(vazio)

  list(
    arestas = res |> transmute(de, para, tipo = "tre", rotulo_aresta = NA_character_),
    assunto = res |> filter(nzchar(texto)) |> transmute(id = para, texto) |>
      distinct(id, .keep_all = TRUE)
  )
}

#' Termo de compromisso protocolado e a análise que ele provoca
#'
#' Protocolado o termo de compromisso do empreendedor, o despacho
#' seguinte da unidade de triagem abre a demanda de analisá-lo. O prazo
#' é de quinze dias, contados do protocolo.
demandas_do_tce <- function(nos, documentos) {
  vazio <- list(arestas = tibble(de = character(), para = character(),
                                 tipo = character(), rotulo_aresta = character()),
                prazos = tibble())

  com_tce <- nos |>
    filter(classe == "protocolo_externo") |>
    rowwise() |>
    filter(any(str_detect(
      sem_acento(tolower(coalesce(documentos$tipo_canonico[documentos$n_doc %in% n_docs_ids], ""))),
      "termo de compromisso do empreendedor"))) |>
    ungroup()
  if (!nrow(com_tce)) return(vazio)

  cgm <- nos |> filter(classe == "ato", coalesce(unidade, "") == "CGM")
  if (!nrow(cgm)) return(vazio)

  res <- map_dfr(seq_len(nrow(com_tce)), \(k) {
    e <- com_tce[k, ]
    prox <- cgm |> filter(data >= e$data) |> slice_head(n = 1)
    if (!nrow(prox)) return(NULL)
    tibble(id_entrada = e$id, id_abertura = prox$id,
           unidade_abertura = prox$unidade, inicio = e$data,
           limite = e$data + 15L)
  })
  if (!nrow(res)) return(vazio)

  list(
    arestas = res |> transmute(de = id_entrada, para = id_abertura,
                               tipo = "demanda", rotulo_aresta = NA_character_),
    prazos = res |> transmute(id_abertura, unidade_abertura, data_abertura = inicio,
                              limite, id_entrada, inicio,
                              demanda = "An\u00e1lise de Termo de Compromisso do Empreendedor")
  )
}

#' Parecer que analisa o enquadramento do empreendimento
#'
#' O despacho da triagem pede a análise do enquadramento, e o parecer que
#' responde nem sempre o cita pelo número: a ligação está no próprio
#' assunto tratado. Quando o parecer trata de enquadramento e ainda não
#' recebeu seta desse despacho, a referência é de conteúdo.
arestas_enquadramento <- function(nos, arestas) {
  vazio <- tibble(de = character(), para = character(), tipo = character(),
                  rotulo_aresta = character())
  pedidos <- nos |>
    filter(classe == "ato", coalesce(unidade, "") == "CGM",
           str_detect(sem_acento(tolower(coalesce(assunto, ""))), "enquadrament"))
  if (!nrow(pedidos)) return(vazio)

  pareceres <- nos |>
    filter(classe == "ato", coalesce(enquadramento, FALSE),
           str_detect(sem_acento(tolower(coalesce(tipo_canonico, ""))), "^parecer|^nota tecnica"))
  if (!nrow(pareceres)) return(vazio)

  map_dfr(seq_len(nrow(pedidos)), \(k) {
    p <- pedidos[k, ]
    alvo <- pareceres |> filter(data >= p$data) |> slice_head(n = 1)
    if (!nrow(alvo)) return(NULL)
    if (any(arestas$de == p$id & arestas$para == alvo$id)) return(NULL)
    tibble(de = p$id, para = alvo$id, tipo = "demanda", rotulo_aresta = NA_character_)
  })
}

#' Parecer que encaminha a publicação de portaria
#'
#' O parecer da unidade de análise de projetos é o que encaminha a
#' publicação; a portaria que o sucede é o ato dele decorrente, ainda
#' que não o cite por número.
arestas_parecer_portaria <- function(nos) {
  vazio <- tibble(de = character(), para = character(), tipo = character(),
                  rotulo_aresta = character())
  pareceres <- nos |>
    filter(classe == "ato",
           str_detect(sem_acento(tolower(coalesce(tipo_canonico, ""))), "^parecer"))
  portarias <- nos |>
    filter(classe == "ato",
           str_detect(sem_acento(tolower(coalesce(tipo_canonico, ""))), "^portaria"))
  if (!nrow(pareceres) || !nrow(portarias)) return(vazio)

  map_dfr(seq_len(nrow(portarias)), \(k) {
    pt <- portarias[k, ]
    ant <- pareceres |> filter(ordem_arvore < pt$ordem_arvore) |> slice_tail(n = 1)
    if (!nrow(ant)) return(NULL)
    tibble(de = ant$id, para = pt$id, tipo = "portaria", rotulo_aresta = NA_character_)
  })
}

#' Parecer que não foi encaminhado
#'
#' Emitido o parecer, espera-se que o destinatário produza o ato seguinte
#' ou que algum documento o cite. Não havendo nem um nem outro, o parecer
#' ficou sem encaminhamento.
nos_parecer_sem_encaminhamento <- function(nos, arestas) {
  vazio <- list(nos = tibble(), arestas = tibble())
  pareceres <- nos |>
    filter(classe == "ato",
           str_detect(sem_acento(tolower(coalesce(tipo_canonico, ""))), "^parecer"))
  if (!nrow(pareceres)) return(vazio)

  fluxo <- arestas |> filter(!tipo %in% c("prazo", "complemento", "assinatura",
                                          "abortado", "email_pendente",
                                          "sem_encaminhamento"))
  soltos <- pareceres |> filter(!id %in% fluxo$de)
  if (!nrow(soltos)) return(vazio)

  novos <- soltos |> transmute(
    id = paste0("SEMENC_", str_remove(id, "^D")),
    classe = "sem_encaminhamento", tipo_canonico = "SemEncaminhamento",
    rotulo = "Parecer aguardando encaminhamento",
    unidade = NA_character_, data = as.Date(NA),
    assunto = "Nenhum ato do destinat\u00e1rio e nenhuma cita\u00e7\u00e3o em outro documento")

  ar <- soltos |> transmute(de = paste0("SEMENC_", str_remove(id, "^D")), para = id,
                            tipo = "sem_encaminhamento", rotulo_aresta = NA_character_)
  list(nos = novos, arestas = ar)
}

#' Ato restrito, sem inteiro teor e sem citação
#'
#' Despacho, parecer ou ofício numerado cujo cadeado impede a leitura.
#' Não sendo citado em documento algum, nada se sabe de seu papel no
#' fluxo: fica à margem, ao lado da última movimentação.
nos_restritos_isolados <- function(documentos, nos) {
  alvo <- documentos |> filter(eh_ato_restrito(rotulo, restrito))
  if (!nrow(alvo)) return(tibble())

  citados <- unique(unlist(nos$referencias))
  soltos <- alvo |> filter(!n_doc %in% citados)
  if (!nrow(soltos)) return(tibble())

  soltos |> transmute(
    id = paste0("REST_", n_doc), classe = "restrito", tipo_canonico = "Restrito",
    rotulo = sprintf("%s\n%s\n%s", rotulo, coalesce(unidade, ""), n_doc),
    unidade, data = as.Date(NA),
    assunto = sprintf("Documento restrito, de %s\nInteiro teor indispon\u00edvel e sem cita\u00e7\u00e3o nos autos",
                      format(data, "%d/%m/%Y")))
}

#' E-mail anexado sem inteiro teor
#'
#' E-mail que consta dos autos como arquivo, e não como documento nativo,
#' não tem conteúdo legível: costuma ser falha de envio ou resposta do
#' interessado. Fica à margem do fluxo, sem direcionamento, ao lado da
#' última movimentação.
nos_email_externo <- function(documentos, nativos) {
  soltos <- documentos |>
    filter(!n_doc %in% nativos,
           str_detect(sem_acento(tolower(coalesce(tipo_canonico, ""))), "^e-?mail"))
  if (!nrow(soltos)) return(tibble())
  soltos |> transmute(
    id = paste0("MAILX_", n_doc), classe = "email_externo",
    tipo_canonico = "EmailExterno",
    rotulo = sprintf("E-mail anexado\n%s\n%s", unidade, n_doc),
    unidade, data = as.Date(NA),
    assunto = "Documento n\u00e3o nativo\nConte\u00fado n\u00e3o leg\u00edvel: prov\u00e1vel falha de envio ou resposta do interessado")
}

#' Documentação protocolada que ainda não gerou demanda
#'
#' Protocolada a documentação, espera-se que a unidade de triagem abra a
#' demanda de analisá-la, ou que algum ato a alcance por citação ou por
#' termo. Não havendo nem um nem outro, a entrada segue sem resposta.
nos_sem_demanda <- function(nos, arestas) {
  vazio <- list(nos = tibble(), arestas = tibble())
  entradas <- nos |> filter(classe == "protocolo_externo", !id %in% arestas$de)
  if (!nrow(entradas)) return(vazio)

  novos <- entradas |> transmute(
    id = paste0("SEMDEM_", str_remove(id, "^EXT_")),
    classe = "faltante", tipo_canonico = "Faltante",
    rotulo = "Aguardando abertura de demanda",
    unidade = NA_character_, data = as.Date(NA),
    assunto = "Documenta\u00e7\u00e3o protocolada sem demanda aberta pela unidade de triagem")

  ar <- entradas |> transmute(de = id, para = paste0("SEMDEM_", str_remove(id, "^EXT_")),
                              tipo = "faltante", rotulo_aresta = NA_character_)
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
  a2   <- arestas_por_destinatario(nos, ja_ligados = a1)
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

  dm  <- demandas_da_cgm(nos)
  dtr <- demandas_do_tre(nos)
  dtc <- demandas_do_tce(nos, documentos)
  ta  <- tre_automatico(nos)
  if (nrow(ta$arestas)) ta$arestas <- distinct(ta$arestas, de, para, .keep_all = TRUE)
  if (nrow(ta$assunto)) {
    nos <- nos |>
      left_join(ta$assunto, by = "id", relationship = "one-to-one") |>
      mutate(assunto = if_else(!is.na(texto) & nzchar(texto), texto, assunto)) |>
      select(-texto) |>
      distinct(id, .keep_all = TRUE)
  }
  pp  <- arestas_parecer_portaria(nos)
  enq <- arestas_enquadramento(nos, arestas)

  # o termo de compromisso dispensa a família de pareceres correspondente
  dispensa <- dtr$dispensa
  dispensados <- function(x) {
    if (!length(dispensa) || is.null(x) || !nrow(x) || !"rotulo" %in% names(x)) return(x)
    x |> filter(!map_lgl(rotulo, \(r)
      any(str_starts(sem_acento(tolower(coalesce(r, ""))), sem_acento(tolower(dispensa))))))
  }
  dm$nos  <- dispensados(dm$nos)
  dtr$nos <- dispensados(dtr$nos)
  ids_de <- function(x) if (is.null(x) || !nrow(x) || !"id" %in% names(x)) character(0) else x$id
  faltantes_validos <- c(ids_de(dm$nos), ids_de(dtr$nos))

  # a ligação do parecer com a portaria tem precedência sobre o
  # encadeamento cronológico que porventura já ligue os dois
  arestas <- bind_rows(pp, arestas,
                       dm$arestas  |> filter(tipo == "demanda"),
                       dtr$arestas |> filter(tipo == "demanda"),
                       dtc$arestas, enq, ta$arestas) |>
    distinct(de, para, .keep_all = TRUE)

  pu <- nos_pendentes_unidade(nos, andamentos)
  pr <- no_pendente_relatorio(nos)
  pendentes <- bind_rows(pu$nos, pr$nos, dm$nos,
                         no_ultima_movimentacao(documentos, andamentos))
  arestas   <- bind_rows(arestas, pu$arestas, pr$arestas,
                         dm$arestas  |> filter(tipo == "faltante", para %in% faltantes_validos),
                         dtr$arestas |> filter(tipo == "faltante", para %in% faltantes_validos))
  # o e-mail liga-se à documentação que se segue, por cruzamento de
  # termos com os rótulos protocolados
  ed <- arestas_email_documentacao(nos, pendentes)
  if (nrow(ed)) {
    arestas <- arestas |>
      filter(!(de %in% ed$de & tipo %in% c("sequencia", "protocolo", "pendente")))
    arestas <- bind_rows(ed, arestas) |> distinct(de, para, .keep_all = TRUE)
  }

  arestas   <- bind_rows(arestas, arestas_email_pendente(nos, pendentes, arestas))
  arestas   <- bind_rows(arestas, arestas_complemento_protocolo(nos)) |>
    distinct(de, para, .keep_all = TRUE)

  # endereçamento dirigido à função, quando a pessoa não é nomeada
  af <- arestas_por_funcao(nos, arestas)
  ft <- arestas_fca_tre(nos)
  # A ligação normativa entre ficha e termo de referência cede lugar ao
  # endereçamento, quando este já explica o mesmo par — o rótulo do
  # destinatário informa mais —, mas prevalece sobre a simples sucessão.
  if (nrow(ft)) {
    enderecados <- arestas |>
      filter(tipo %in% c("destinatario", "enderecamento")) |>
      transmute(par = paste(de, para)) |> pull(par)
    ft <- ft |> filter(!paste(de, para) %in% enderecados)
  }
  if (nrow(af) || nrow(ft))
    arestas <- bind_rows(af, ft, arestas) |> distinct(de, para, .keep_all = TRUE)

  # ato lavrado por quem assinou o parecer deriva dele, e não do despacho
  pos <- arestas_pos_parecer(nos, arestas)
  if (nrow(pos$arestas)) {
    arestas <- arestas |> anti_join(pos$remover, by = c("de", "para"))
    # a derivação tem precedência sobre o encadeamento cronológico, que
    # é apenas o fecho de última instância
    arestas <- bind_rows(pos$arestas, arestas) |> distinct(de, para, .keep_all = TRUE)
  }

  # a portaria só aponta para o relatório se nenhum e-mail o fizer
  if (nrow(pr$nos) && any(ed$para %in% pr$nos$id))
    arestas <- arestas |> filter(!(para %in% pr$nos$id & !de %in% ed$de))

  # A portaria decorre do parecer que a encaminha, e não da documentação
  # protocolada nem da simples sucessão de atos.
  portarias <- nos$id[nos$classe == "ato" &
    str_detect(sem_acento(tolower(coalesce(nos$tipo_canonico, ""))), "^portaria")]
  if (length(portarias))
    arestas <- arestas |>
      filter(!(para %in% portarias & tipo %in% c("protocolo", "sequencia")))

  # o fluxo acompanha a ordem dos autos
  arestas <- somente_adiante(arestas, nos)

  sd <- nos_sem_demanda(nos, arestas)
  pendentes <- bind_rows(pendentes, sd$nos)
  arestas   <- bind_rows(arestas, sd$arestas)

  # o fluxo abortado é apurado sobre o grafo já ajustado: ato que passou
  # a derivar do parecer não caracteriza encaminhamento sem resposta
  ab <- nos_fluxo_abortado(nos, bind_rows(dm$arestas, dtr$arestas) |>
                             filter(tipo == "demanda"), arestas)
  pendentes <- bind_rows(pendentes, ab$nos)
  arestas   <- bind_rows(arestas, ab$arestas)

  de_hub <- nos_destinatario(nos, arestas)
  if (nrow(de_hub$nos)) {
    arestas <- arestas |>
      anti_join(de_hub$remover, by = c("de", "para")) |>
      bind_rows(de_hub$arestas)
  }

  prazos <- apurar_prazos(nos, bind_rows(dm$arestas, dtr$arestas, enq),
                          alvos = bind_rows(nos, dm$nos, dtr$nos))
  prazos <- completar_encerramentos(prazos, nos, arestas)

  # o prazo do termo de compromisso não depende de parecer: o marco
  # final é a manifestação da própria triagem. Só entra quando o assunto
  # do despacho não declarou o instrumento por conta própria.
  if (nrow(dtc$prazos) && nrow(prazos)) {
    ja_declarado <- prazos$id_abertura[str_detect(coalesce(prazos$demanda, ""),
                                                  "Termo de Compromisso")]
    dtc$prazos <- dtc$prazos |> filter(!id_abertura %in% ja_declarado)
  }
  if (nrow(dtc$prazos)) {
    prazos <- bind_rows(prazos, dtc$prazos |>
      mutate(id_parecer = NA_character_, fim = as.Date(NA),
             dias_corridos = NA_integer_,
             dias_limite = as.integer(limite - inicio),
             excedeu = FALSE, atraso_dias = 0L))
  }
  nos <- nos |>
    # um mesmo parecer pode encerrar mais de um prazo; para a marcação do
    # nó interessa o mais restritivo
    left_join(prazos |> filter(!is.na(id_parecer)) |>
                arrange(desc(excedeu), limite) |>
                distinct(id_parecer, .keep_all = TRUE) |>
                select(id = id_parecer, excedeu, atraso_dias, limite),
              by = "id")

  # A situação do prazo ganha nó próprio, que aponta para o parecer: a
  # informação é sobre o cumprimento do prazo, e não um atributo visual
  # do ato.
  pz <- nos_de_prazo(nos)
  cp <- nos_complemento(nos)
  ap <- nos_assinatura_pendente(nos)
  mp <- nos_email_pendente_oficio(nos, arestas)
  se <- nos_parecer_sem_encaminhamento(nos, arestas)
  mx <- nos_email_externo(documentos, conteudo$n_doc[coalesce(conteudo$nativo, FALSE)])
  rx <- nos_restritos_isolados(documentos, nos)
  pendentes <- bind_rows(pendentes, pz$nos, cp$nos, ap$nos, de_hub$nos,
                         mp$nos, se$nos, mx, rx, dtr$nos)
  arestas   <- bind_rows(arestas, pz$arestas, cp$arestas, ap$arestas,
                         mp$arestas, se$arestas)

  # Ato que não recebe nem dirige seta alguma do fluxo fica isolado nos
  # autos: sinalizá-lo é o que permite encontrá-lo.
  fluxo <- arestas |> filter(!tipo %in% c("prazo", "complemento", "assinatura"))
  nos <- nos |> mutate(avulso = classe == "ato" & !id %in% c(fluxo$de, fluxo$para))

  list(cabecalho = cabecalho, nos = nos, arestas = arestas,
       pendentes = pendentes, andamentos = andamentos, prazos = prazos)
}
