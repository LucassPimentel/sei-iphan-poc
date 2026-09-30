# =====================================================================
# 04_saida.R — Saídas do grafo
#
#   gerar_mermaid()  — reproduz a convenção do diagrama manual
#   gerar_graphviz() — geometria própria por tipo documental
#   gerar_html()     — página autocontida, com legenda e quadro de prazos
#   gerar_eventlog() — log de eventos para as métricas de process mining
#
# Convenção visual:
#   geometria        = tipo do documento (ver GEOMETRIA)
#   caixa cinza      = destinatário, como rótulo da seta
#   linha sem seta   = assunto, marcações, destinatário não endereçado,
#                      destinatários de e-mail e situação do prazo
#   linha tracejada  = documento ainda não juntado aos autos
#   verde-claro      = parecer emitido dentro do prazo
#   vermelho-claro   = parecer emitido fora do prazo
# =====================================================================

suppressPackageStartupMessages({
  library(dplyr); library(stringr); library(purrr); library(glue)
})

# Geometria por tipo documental. A chave é comparada sem acentuação e em
# minúsculas contra o início do tipo canônico.
GEOMETRIA <- tibble::tribble(
  ~chave,                  ~graphviz,        ~mermaid_abre, ~mermaid_fecha,
  "despacho",              "box",            "[",           "]",
  "oficio",                "parallelogram",  "[/",          "/]",
  "parecer",               "ellipse",        "((",          "))",
  "termo de referencia",   "hexagon",        "{{",          "}}",
  "e-mail",                "note",           ">",           "]",
  "portaria",              "doubleoctagon",  "[[",          "]]",
  "nota tecnica",          "ellipse",        "((",          "))",
  "protocolo",             "folder",         "([",          "])",
  "pendente",              "ellipse",        "((",          "))",
  "faltante",              "ellipse",        "((",          "))",
  "abortado",              "invtrapezium",   "[\\",         "/]",
  "informativo",           "cylinder",       "[(",          ")]",
  "prazo",                 "octagon",        "{{",          "}}",
  "complemento",           "house",          "[/",          "/]",
  "assinatura",            "tab",            "[/",          "/]",
  "destinatario",          "box",            "[",           "]",
  "emailpendente",         "note",           ">",           "]",
  "ciencia",               "circle",         "((",          "))",
  "semencaminhamento",     "invhouse",       "[\\",         "\\]",
  "emailexterno",          "note",           ">",           "]",
  "restrito",              "box3d",          "[[",          "]]",
  "outro",                 "diamond",        "{",           "}"
)

# Nome das classes em português, para a legenda
CLASSES_PT <- c(despacho = "Despacho", oficio = "Of\u00edcio", parecer = "Parecer",
                "termo de referencia" = "Termo de Refer\u00eancia", "e-mail" = "E-mail",
                portaria = "Portaria", "nota tecnica" = "Nota T\u00e9cnica",
                protocolo = "Protocolo externo", faltante = "Parecer faltante",
                abortado = "Fluxo abortado", informativo = "Informativo",
                prazo = "Situa\u00e7\u00e3o do prazo", complemento = "Complementa\u00e7\u00f5es",
                assinatura = "Assinatura pendente", outro = "Outro tipo nativo",
                destinatario = "Destinat\u00e1rio", emailpendente = "E-mail pendente",
                ciencia = "Ci\u00eancia da unidade",
                semencaminhamento = "Parecer sem encaminhamento",
                emailexterno = "E-mail anexado", restrito = "Ato restrito",
                pendente = "Documento futuro")

# Nome das formas em português, para a legenda
FORMAS_PT <- c(box = "ret\u00e2ngulo", parallelogram = "paralelogramo",
               ellipse = "elipse", hexagon = "hex\u00e1gono", note = "nota",
               doubleoctagon = "oct\u00f3gono duplo", folder = "pasta",
               octagon = "oct\u00f3gono", invtrapezium = "trap\u00e9zio invertido",
               cylinder = "cilindro", diamond = "losango", tab = "aba", house = "casa",
               component = "componente")

# Tipos nativos já reconhecidos. O que não estiver aqui recebe a
# geometria de "outro", o que torna visível a chegada de um tipo
# documental ainda não tratado.
TIPOS_CONHECIDOS <- c("despacho", "oficio", "parecer", "termo de referencia",
                      "e-mail", "portaria", "nota tecnica")

COR_NO_PRAZO    <- "#CCF2CC"
COR_FORA_PRAZO  <- "#FF9980"
COR_FALTANTE    <- "#FFF0A6"
COR_FUTURO      <- "#D9B38C"   # documento esperado, ainda não juntado
COR_ABORTADO    <- "#FFA94D"
COR_COMPLEMENTO <- "#D6BCEB"   # exigência dirigida ao interessado
COR_ASSINATURA  <- "#F2A0D0"   # ato ainda não aperfeiçoado
COR_AVULSO      <- "#1A1A1A"   # documento sem ligação no fluxo
COR_INFORMATIVO <- "#DCE9F5"   # informação à margem do fluxo
COR_DESTINATARIO<- "#EDEDED"   # destinatário endereçado no documento
COR_EMAIL_PEND  <- "#8FD9DE"   # notificação ao interessado ainda não enviada
COR_CIENCIA     <- "#C9E4D8"   # unidade deu ciência e devolveu o processo
COR_SEM_ENC     <- "#B0C4DE"   # parecer sem encaminhamento
COR_PADRAO      <- "#FFFFFF"

TRACO_FUTURO   <- "#7B4B2A"
TRACO_FALTANTE <- "#8A7A00"
TRACO_PADRAO   <- "#000000"

# Cores das setas: a citação e o endereçamento precisam se distinguir à
# primeira vista, inclusive na tela.
COR_SETA_CITACAO      <- "#1F77B4"   # azul
COR_SETA_ENDERECAMENTO<- "#C0392B"   # vermelho escuro
COR_SETA_PADRAO       <- "#333333"   # fluxo derivado: demanda, protocolo, sequência

#' Classe visual de um nó: determina cor de preenchimento e de traço
classe_visual <- function(n) {
  if (isTRUE(n$avulso)) return("avulso")
  switch(coalesce(n$classe, ""),
    prazo       = if (isTRUE(n$excedeu)) "prazoFora" else "prazoDentro",
    faltante    = "faltante",
    pendente    = "futuro",
    abortado    = "abortado",
    informativo = "informativo",
    complemento  = "complemento",
    assinatura   = "assinatura",
    destinatario       = "destinatario",
    email_pendente     = "emailPendente",
    ciencia            = "ciencia",
    sem_encaminhamento = "semEncaminhamento",
    email_externo      = "avulso",
    restrito           = "restrito",
    "ato")
}

PALETA <- tibble::tribble(
  ~classe,       ~fill,            ~stroke,          ~tracejado, ~descricao,
  "ato",         COR_PADRAO,       TRACO_PADRAO,     FALSE, "ato do processo",
  "futuro",      COR_FUTURO,       TRACO_FUTURO,     TRUE,  "documento esperado, ainda n\u00e3o juntado aos autos",
  "faltante",    COR_FALTANTE,     TRACO_FALTANTE,   TRUE,  "parecer demandado que n\u00e3o consta dos autos",
  "abortado",    COR_ABORTADO,     TRACO_PADRAO,     FALSE, "encaminhamento que n\u00e3o se concretizou",
  "prazoDentro", COR_NO_PRAZO,     TRACO_PADRAO,     FALSE, "manifesta\u00e7\u00e3o dentro do prazo",
  "prazoFora",   COR_FORA_PRAZO,   TRACO_PADRAO,     FALSE, "manifesta\u00e7\u00e3o fora do prazo, com os dias de atraso",
  "informativo", COR_INFORMATIVO,  TRACO_PADRAO,     FALSE, "\u00faltima movimenta\u00e7\u00e3o dos autos",
  "destinatario",COR_DESTINATARIO, TRACO_PADRAO,     FALSE, "destinat\u00e1rios endere\u00e7ados no documento",
  "emailPendente",COR_EMAIL_PEND,  TRACO_PADRAO,     FALSE, "of\u00edcio com e-mail declarado e notifica\u00e7\u00e3o ainda n\u00e3o enviada",
  "ciencia",     COR_CIENCIA,      TRACO_PADRAO,     FALSE, "unidade deu ci\u00eancia e devolveu o processo: ramo encerrado",
  "semEncaminhamento", COR_SEM_ENC, TRACO_PADRAO,    FALSE, "parecer aguardando encaminhamento",
  "complemento", COR_COMPLEMENTO,  TRACO_PADRAO,     FALSE, "complementa\u00e7\u00f5es dirigidas ao interessado",
  "assinatura",  COR_ASSINATURA,   TRACO_PADRAO,     FALSE, "assinaturas pendentes: o ato ainda n\u00e3o se aperfei\u00e7oou",
  "avulso",      COR_AVULSO,       TRACO_PADRAO,     FALSE, "documento sem liga\u00e7\u00e3o no fluxo",
  "restrito",    "#6E6E6E",        TRACO_PADRAO,     FALSE, "ato restrito, sem inteiro teor e sem cita\u00e7\u00e3o"
)

visual_de <- function(cv) PALETA[match(cv, PALETA$classe), ]

geometria_de <- function(tipo, classe) {
  alvo <- sem_acento(tolower(coalesce(tipo, "")))
  if (identical(classe, "protocolo_externo")) alvo <- "protocolo"
  if (identical(classe, "pendente"))          alvo <- "pendente"
  if (identical(classe, "prazo"))             alvo <- "prazo"
  hit <- GEOMETRIA[str_starts(alvo, GEOMETRIA$chave), , drop = FALSE]
  if (nrow(hit)) return(hit[1, ])
  # tipo nativo ainda não tratado
  GEOMETRIA[GEOMETRIA$chave == "outro", ]
}

#' Cor de preenchimento
#'
#' Só o nó de prazo é colorido: o parecer permanece branco, porque o
#' atraso é informação do prazo, e não do ato.
cor_de <- function(n) visual_de(classe_visual(n))$fill

esc_mmd <- function(x) {
  x <- coalesce(x, "")
  x <- str_replace_all(x, "\"", "'")
  # o parser do Mermaid lê "<" e ">" como abertura de tag e recusa o
  # diagrama inteiro; endereços de e-mail entre sinais são o caso comum
  x <- str_replace_all(x, "<", "&lt;")
  x <- str_replace_all(x, ">", "&gt;")
  x <- str_replace_all(x, "\\{", "(")
  x <- str_replace_all(x, "\\}", ")")
  x
}

#' Rótulo pronto para o Mermaid: escapa, quebra em linhas e junta com <br/>
mmd_txt <- function(x, largura = 38) {
  if (is.na(x) || !nzchar(x)) return("")
  partes <- str_split(esc_mmd(x), "\n")[[1]]
  partes <- partes[nzchar(str_squish(partes))]
  paste(map_chr(partes, \(p) paste(strwrap(str_squish(p), width = largura),
                                   collapse = "<br/>")),
        collapse = "<br/>")
}

quebrar <- function(x, largura = 38, sep = "<br/>") {
  if (is.na(x) || !nzchar(x)) return("")
  partes <- str_split(x, "\n")[[1]]
  paste(map_chr(partes, \(p) paste(strwrap(p, width = largura), collapse = sep)),
        collapse = sep)
}

#' Rótulo interno do nó: Tipo, Unidade, Data, assinantes e número
rotulo_no <- function(n) {
  # nós auxiliares — pendência, prazo, fluxo abortado, informativo —
  # trazem apenas o próprio rótulo
  if (!identical(n$classe, "ato") && !identical(n$classe, "protocolo_externo"))
    return(str_replace_all(esc_mmd(n$rotulo), "\n", "<br/>"))
  partes <- c(n$rotulo, n$unidade,
              if (!is.na(n$data)) format(n$data, "%d/%m/%Y"),
              coalesce(n$assinantes, n$signatarios), n$n_doc)
  partes <- partes[!is.na(partes) & nzchar(partes)]
  # str_squish apagaria as quebras entre nome e função do signatário
  partes <- str_replace_all(partes, "[ \t]+", " ")
  paste(str_replace_all(esc_mmd(partes), "\n", "<br/>"), collapse = "<br/>")
}

#' Conteúdo da linha sem seta
#'
#' Reúne o que descreve o ato sem fazer parte do fluxo: assunto,
#' marcações de tabela, destinatários que não endereçaram seta alguma
#' (caso em que a informação se perderia), destinatários de e-mail — que
#' são endereços, não elos — e a situação do prazo, quando o ato é o
#' parecer que o encerra.
texto_no <- function(n, arestas) {
  itens <- character(0)

  # Havendo itens assinalados, são eles que descrevem o ato: o assunto
  # do termo de referência é texto de contextualização e não acrescenta.
  if (!is.na(n$marcacao) && nzchar(n$marcacao)) {
    itens <- c(itens, n$marcacao)
  } else if (!is.na(n$assunto) && nzchar(n$assunto)) {
    itens <- c(itens, n$assunto)
  }
  if (!is.na(n$recomendacao) && nzchar(n$recomendacao))
    itens <- c(itens, paste("Recomenda aprova\u00e7\u00e3o:", n$recomendacao))

  # Destinatário só aparece aqui quando não rotulou seta alguma: do
  # contrário a informação estaria duplicada.
  d <- n$destinatarios[[1]]
  dest <- blocos_dest(d)
  # unidade interna citada sem endereço eletrônico não é destinatário de
  # correspondência: não rotula seta nem aparece aqui
  if (length(dest)) {
    sem_email <- map_int(d$emails, length) == 0
    dest <- dest[!(sem_email & eh_unidade_interna(dest))]
  }
  usados <- arestas |> filter(de == n$id, !is.na(rotulo_aresta)) |> pull(rotulo_aresta)
  usados <- unlist(str_split(usados, "\n"))
  sobra <- dest[!map_lgl(dest, \(b) any(str_detect(usados, fixed(str_split(b, "\n")[[1]][1]))))]
  if (length(sobra)) itens <- c(itens, paste(sobra, collapse = "\n"))

  # A data-limite fixada pela unidade que abre o prazo pertence ao ato
  # que a fixa; a situação do prazo vai para nó próprio.
  if (!is.null(n$prazo_informado) && !is.na(n$prazo_informado))
    itens <- c(itens, sprintf("Prazo para manifesta\u00e7\u00e3o: %s",
                              format(n$prazo_informado, "%d/%m/%Y")))
  if (!length(itens)) return(NA_character_)
  paste(itens, collapse = "\n")
}

#' Linhas sem seta do diagrama
#'
#' Cada nó tem, em princípio, seu próprio texto. Os pareceres demandados
#' que não constam dos autos compartilham a mesma informação — a demanda
#' que os abriu e o prazo já expirado —, e um único texto serve a todos,
#' evitando repetição idêntica lado a lado.
mapa_textos <- function(grafo) {
  todos <- bind_rows(grafo$nos, grafo$pendentes)
  base <- tibble(
    id = todos$id, classe = todos$classe,
    texto = map_chr(seq_len(nrow(todos)), \(i) texto_no(todos[i, ], grafo$arestas))
  ) |> filter(!is.na(texto), nzchar(texto))
  if (!nrow(base)) return(tibble(id_texto = character(), texto = character(),
                                 ids = list()))
  base |>
    # nós de mesma natureza e mesmo texto compartilham uma só descrição
    mutate(chave = if_else(classe %in% c("faltante", "email_externo", "restrito"),
                           paste0(classe, "::", texto), id)) |>
    group_by(chave) |>
    summarise(texto = first(texto), ids = list(id), .groups = "drop") |>
    mutate(id_texto = paste0("T", str_replace_all(
      if_else(str_detect(chave, "::"),
              paste0("GRUPO", dplyr::row_number()), chave), "\\W", ""))) |>
    select(id_texto, texto, ids)
}

# ---------------------------------------------------------------------
# Mermaid
# ---------------------------------------------------------------------

gerar_mermaid <- function(grafo, arquivo = NULL) {
  cab <- grafo$cabecalho
  todos <- bind_rows(grafo$nos, grafo$pendentes)
  textos <- mapa_textos(grafo)

  # As classes carregam cor e traço; declarar tudo em classDef evita a
  # disputa de precedência entre "class" e "style".
  L <- c("flowchart TD",
         "    classDef noBox fill:transparent,stroke:transparent,color:#000;")
  for (i in seq_len(nrow(PALETA))) {
    p <- PALETA[i, ]
    tracejado <- if (p$tracejado) ",stroke-dasharray:5 5" else ""
    fonte <- if (p$fill %in% c(COR_AVULSO, "#6E6E6E")) ",color:#ffffff" else ""
    L <- c(L, glue("    classDef {p$classe} fill:{p$fill},stroke:{p$stroke},stroke-width:2px{tracejado}{fonte};"))
  }
  L <- c(L, "",
         glue('    Processo["Processo: {cab$protocolo}<br/>Tipo: {esc_mmd(cab$tipo)}<br/>Data de Gera\u00e7\u00e3o: {format(cab$geracao, "%d/%m/%Y")}"]:::ato'))

  for (i in seq_len(nrow(todos))) {
    n <- todos[i, ]
    g <- geometria_de(n$tipo_canonico, n$classe)
    L <- c(L, glue('    {n$id}{g$mermaid_abre}"{rotulo_no(n)}"{g$mermaid_fecha}:::{classe_visual(n)}'))
  }
  for (i in seq_len(nrow(textos)))
    L <- c(L, glue('    {textos$id_texto[i]}("{mmd_txt(textos$texto[i])}"):::noBox'))

  L <- c(L, "", glue("    Processo --> {todos$id[1]}"))
  for (i in seq_len(nrow(grafo$arestas))) {
    a <- grafo$arestas[i, ]
    # endereçamento e citação se distinguem pelo traço: a seta cheia
    # marca quem foi endereçado; a simples, a citação por número
    seta <- if (a$tipo == "enderecamento") "---"
            else if (a$tipo %in% c("pendente", "faltante", "ciencia")) "-.->"
            else if (a$tipo == "destinatario") "==>"
            else "-->"
    L <- c(L, if (is.na(a$rotulo_aresta) || !nzchar(a$rotulo_aresta))
      glue("    {a$de} {seta} {a$para}")
      else glue('    {a$de} {seta} |"{mmd_txt(a$rotulo_aresta)}"| {a$para}'))
  }

  # linkStyle usa o índice da ligação na ordem em que foi declarada; as
  # de fluxo vêm primeiro, e é sobre elas que a cor se aplica
  estilos <- map_chr(seq_len(nrow(grafo$arestas)), \(i) {
    tp <- grafo$arestas$tipo[i]
    cor <- if (tp %in% c("destinatario", "enderecamento")) COR_SETA_ENDERECAMENTO
           else if (tp == "referencia") COR_SETA_CITACAO else NA_character_
    if (is.na(cor)) NA_character_
    else glue("    linkStyle {i} stroke:{cor},stroke-width:2px;")
  })
  L <- c(L, "", estilos[!is.na(estilos)])

  L <- c(L, "")
  for (i in seq_len(nrow(textos)))
    for (id in textos$ids[[i]]) L <- c(L, glue("    {id} --- {textos$id_texto[i]}"))

  # O nó informativo não pertence ao fluxo: a ligação invisível apenas o
  # mantém ao final do diagrama.
  if ("ULT_MOV" %in% todos$id) {
    ultimo <- grafo$nos |> filter(classe == "ato") |> slice_tail(n = 1)
    if (nrow(ultimo)) L <- c(L, "", glue("    {ultimo$id} ~~~ ULT_MOV"))
    for (id in todos$id[todos$classe %in% c("email_externo", "restrito")])
      L <- c(L, glue("    ULT_MOV ~~~ {id}"))
  }

  saida <- paste(L, collapse = "\n")
  if (!is.null(arquivo)) writeLines(saida, arquivo, useBytes = TRUE)
  invisible(saida)
}

# ---------------------------------------------------------------------
# Graphviz
# ---------------------------------------------------------------------

gerar_graphviz <- function(grafo, arquivo = NULL) {
  todos <- bind_rows(grafo$nos, grafo$pendentes)
  textos <- mapa_textos(grafo)
  esc <- function(x) str_replace_all(coalesce(x, ""), '"', "'")
  nl  <- function(x) str_replace_all(quebrar(x), "<br/>", "\\\\n")

  corpo <- c(
    'digraph processo {',
    '  rankdir=TB; splines=true; concentrate=false;',
    '  nodesep=0.35; ranksep=0.65; ordering=out; newrank=true;',
    '  node [style=filled, fillcolor="#ffffff", color="#000000", penwidth=1.6,',
    '        fontname="Helvetica", fontsize=10];',
    '  edge [fontname="Helvetica", fontsize=9, color="#444444"];'
  )

  for (i in seq_len(nrow(todos))) {
    n <- todos[i, ]
    g <- geometria_de(n$tipo_canonico, n$classe)
    v <- visual_de(classe_visual(n))
    estilo <- if (v$tracejado) '"dashed,filled"' else '"filled"'
    fonte <- if (identical(v$fill, COR_AVULSO)) ', fontcolor="#ffffff"' else ''
    corpo <- c(corpo, glue(
      '  {n$id} [shape={g$graphviz}, style={estilo}, fillcolor="{v$fill}", color="{v$stroke}"{fonte}, label="{esc(nl(str_replace_all(rotulo_no(n), "<br/>", "\n")))}"];'))
  }

  for (i in seq_len(nrow(textos))) {
    corpo <- c(corpo, glue(
      '  {textos$id_texto[i]} [shape=box, style=filled, fillcolor="#f2f2f2", color="#cccccc", fontsize=8, label="{esc(nl(str_trunc(textos$texto[i], 320)))}"];'))
    for (id in textos$ids[[i]]) {
      corpo <- c(corpo,
        glue('  {id} -> {textos$id_texto[i]} [arrowhead=none, style=dotted, weight=0, constraint=false];'),
        glue('  {{ rank=same; {id}; {textos$id_texto[i]}; }}'))
    }
  }

  for (i in seq_len(nrow(grafo$arestas))) {
    a <- grafo$arestas[i, ]
    # endereçamento e citação se distinguem pela cor; a linha sem seta do
    # endereçamento dispensa a ponta
    estilo <- if (a$tipo == "enderecamento")
                glue(', arrowhead=none, color="{COR_SETA_ENDERECAMENTO}", penwidth=2')
              else if (a$tipo %in% c("pendente", "faltante", "ciencia")) ', style=dashed'
              else if (a$tipo == "destinatario")
                glue(', color="{COR_SETA_ENDERECAMENTO}", penwidth=2')
              else if (a$tipo == "referencia")
                glue(', color="{COR_SETA_CITACAO}", penwidth=1.6')
              else ''
    corpo <- c(corpo, glue(
      '  {a$de} -> {a$para} [label="{esc(nl(str_trunc(coalesce(a$rotulo_aresta, ""), 160)))}", weight=8{estilo}];'))
  }

  # A cauda do diagrama reúne o que não pertence ao fluxo: a última
  # movimentação e os documentos sem inteiro teor. Uma única aresta
  # invisível a ancora ao fim, e um só grupo de mesma altura a mantém
  # alinhada.
  cauda <- todos |> filter(classe %in% c("informativo", "email_externo", "restrito"))
  if (nrow(cauda)) {
    ultimo <- grafo$nos |> filter(classe == "ato") |> slice_tail(n = 1)
    if (nrow(ultimo))
      for (id in cauda$id)
        corpo <- c(corpo, glue('  {ultimo$id} -> {id} [style=invis, weight=20];'))
    textos_cauda <- textos$id_texto[map_lgl(textos$ids, \(x) any(x %in% cauda$id))]
    corpo <- c(corpo, glue('  {{ rank=same; {paste(c(cauda$id, textos_cauda), collapse = "; ")}; }}'))
  }

  corpo <- c(corpo, '}')
  saida <- paste(corpo, collapse = "\n")
  if (!is.null(arquivo)) writeLines(saida, arquivo, useBytes = TRUE)
  invisible(saida)
}

# ---------------------------------------------------------------------
# HTML
# ---------------------------------------------------------------------

#' Página HTML autocontida com o grafo, a legenda e o quadro de prazos
#'
#' O diagrama é embutido como SVG, produzido pelo Graphviz. Sem o
#' Graphviz instalado, a página cai para o Mermaid renderizado no
#' navegador, o que exige conexão.
gerar_html <- function(grafo, arquivo, titulo = NULL) {
  cab <- grafo$cabecalho
  titulo <- titulo %||% paste("Fluxo processual", cab$protocolo)

  gv <- tempfile(fileext = ".gv")
  gerar_graphviz(grafo, gv)
  svg <- NULL
  if (nzchar(Sys.which("dot"))) {
    svg_file <- tempfile(fileext = ".svg")
    system2("dot", c("-Tsvg", shQuote(gv), "-o", shQuote(svg_file)))
    if (file.exists(svg_file)) {
      svg <- paste(readLines(svg_file, warn = FALSE), collapse = "\n")
      svg <- str_remove(svg, "(?s)^.*?(?=<svg)")
    }
  }

  # Legenda: geometrias de um lado, cores do outro
  formas <- GEOMETRIA |>
    filter(!chave %in% c("pendente")) |>
    mutate(linha = sprintf("<li><b>%s</b> &mdash; %s</li>",
                           coalesce(CLASSES_PT[chave], str_to_sentence(chave)),
                           coalesce(FORMAS_PT[graphviz], graphviz))) |>
    pull(linha) |> paste(collapse = "\n")

  cores <- PALETA |>
    filter(classe != "ato") |>
    mutate(linha = sprintf("<li><span class='chip' style='background:%s;border-color:%s%s'></span>%s</li>",
                           fill, stroke,
                           if_else(tracejado, ";border-style:dashed", ""),
                           descricao)) |>
    pull(linha) |> paste(collapse = "\n")

  demandas <- relatar_demandas(grafo)
  linhas_dem <- if (nrow(demandas)) {
    paste(pmap_chr(demandas, \(...) {
      d <- list(...)
      sprintf("<tr><td>%s</td><td>%s</td><td>%s</td><td>%s</td><td>%s</td><td>%s</td></tr>",
              d$marcacao, d$referencia, d$documentos, d$origem, d$situacao,
              str_trunc(d$detalhe, 160))
    }), collapse = "\n")
  } else "<tr><td colspan='5'>Nenhuma demanda em aberto</td></tr>"

  prazos <- relatar_prazos(grafo)
  linhas_prazo <- if (nrow(prazos)) {
    paste(pmap_chr(prazos, \(...) {
      p <- list(...)
      sprintf("<tr class='%s'><td>%s</td><td>%s</td><td>%s</td><td>%s</td><td>%s</td><td>%s</td><td>%s</td><td>%s</td><td>%s</td></tr>",
              if (p$situacao == "EXCEDEU") "fora" else if (p$situacao == "no prazo") "dentro" else "aberto",
              p$abertura, p$unidade, p$inicio, p$demanda, p$limite,
              p$ato_encerramento, p$encerramento, p$dias_corridos, p$situacao)
    }), collapse = "\n")
  } else "<tr><td colspan='7'>Nenhum prazo apurado</td></tr>"

  corpo_grafo <- if (!is.null(svg)) svg else sprintf(
    "<div class='mermaid'>%s</div>\n<script type='module'>import mermaid from 'https://cdn.jsdelivr.net/npm/mermaid@10/dist/mermaid.esm.min.mjs'; mermaid.initialize({startOnLoad:true, maxTextSize:900000});</script>",
    gerar_mermaid(grafo))

  html <- sprintf('<!DOCTYPE html>
<html lang="pt-BR"><head><meta charset="utf-8">
<title>%s</title>
<style>
  :root { color-scheme: light; }
  body { font-family: -apple-system, Segoe UI, Roboto, Helvetica, Arial, sans-serif;
         margin: 0; padding: 20px 24px; color: #1a1a1a; background: #fff; }
  h1 { font-size: 20px; margin: 0 0 4px; color: #1F3864; }
  .meta { color: #666; font-size: 13px; margin-bottom: 12px; }
  .barra { display:flex; gap:6px; align-items:center; margin-bottom:8px; font-size:13px; }
  .barra button { font: inherit; padding: 3px 10px; border: 1px solid #c8c8c8;
                  background: #fff; border-radius: 4px; cursor: pointer; }
  .barra button:hover { background: #f0f4fa; }
  .barra .dica { color:#888; margin-left:6px; }
  #palco { overflow: auto; border: 1px solid #e0e0e0; border-radius: 6px;
           background: #fafafa; height: 78vh; position: relative; cursor: grab; }
  #palco.arrastando { cursor: grabbing; }
  #tela { transform-origin: 0 0; padding: 8px; display: inline-block; }
  #tela svg { max-width: none; height: auto; }
  h2 { font-size: 15px; margin: 22px 0 8px; color: #2E5496; }
  table { border-collapse: collapse; font-size: 13px; }
  th { background: #2E5496; color: #fff; text-align: left; padding: 6px 10px; font-weight: 600; }
  td { padding: 5px 10px; border-bottom: 1px solid #e6e6e6; }
  tr.fora td { background: %s; }
  tr.dentro td { background: %s; }
  tr.aberto td { background: #FFF6DB; }
  ul.legenda { columns: 2; font-size: 13px; list-style: none; padding: 0; margin-top: 4px; }
  ul.legenda li { margin-bottom: 4px; break-inside: avoid; }
  .chip { display:inline-block; width:14px; height:14px; border:1px solid #000;
          border-radius:2px; vertical-align:-2px; margin-right:6px; }
</style></head><body>
<h1>%s</h1>
<div class="meta">%s &middot; gerado em %s</div>
<div class="barra">
  <button data-zoom="0.8">&minus;</button>
  <button data-zoom="1.25">+</button>
  <button data-zoom="reset">100%%</button>
  <button data-zoom="fit">Ajustar</button>
  <span id="nivel">100%%</span>
  <span class="dica">roda do mouse com Ctrl para ampliar; arraste para deslocar</span>
</div>
<div id="palco"><div id="tela">%s</div></div>
<h2>Prazos apurados</h2>
<table><thead><tr><th>Ato de abertura</th><th>Unidade</th><th>In\u00edcio</th>
<th>Demanda</th><th>Limite</th><th>Ato de encerramento</th><th>Encerramento</th>
<th>Dias corridos</th><th>Situa\u00e7\u00e3o</th></tr></thead>
<tbody>%s</tbody></table>
<h2>Demandas geradas</h2>
<table><thead><tr><th>Marca\u00e7\u00e3o</th><th>Refer\u00eancia</th><th>Documentos</th>
<th>Ato de origem</th><th>Situa\u00e7\u00e3o</th><th>Detalhe</th></tr></thead>
<tbody>%s</tbody></table>
<h2>Legenda</h2>
<ul class="legenda">%s</ul>
<h2>Geometrias</h2>
<ul class="legenda">%s</ul>
<h2>Setas</h2>
<ul class="legenda">
<li><span class="chip" style="background:%s;border-color:%s"></span>azul: <b>cita\u00e7\u00e3o</b> &mdash; o documento cita o outro pelo n\u00famero</li>
<li><span class="chip" style="background:%s;border-color:%s"></span>vermelho: <b>endere\u00e7amento</b> &mdash; o destinat\u00e1rio assina o ato seguinte</li>
<li><span class="chip" style="background:%s;border-color:%s"></span>preto: <b>fluxo derivado</b> &mdash; demanda aberta pela triagem, protocolo que inaugura ciclo, encadeamento cronol\u00f3gico, parecer que encaminha portaria e cadastro em base georreferenciada</li>
</ul>
<p style="font-size:13px;color:#555">Linha tracejada: documento ainda n\u00e3o juntado aos autos.
Linha pontilhada sem seta: assunto, marca\u00e7\u00f5es e destinat\u00e1rios do ato.</p>
<script>
(function () {
  var palco = document.getElementById("palco"), tela = document.getElementById("tela");
  var k = 1, nivel = document.getElementById("nivel");
  function aplicar() { tela.style.transform = "scale(" + k + ")"; nivel.textContent = Math.round(k * 100) + "%%"; }
  function ajustar() {
    var alvo = tela.querySelector("svg") || tela.firstElementChild;
    if (!alvo) return;
    var largura = alvo.getBoundingClientRect().width / k;
    if (largura > 0) { k = Math.min(1, (palco.clientWidth - 24) / largura); aplicar(); }
  }
  document.querySelectorAll("[data-zoom]").forEach(function (b) {
    b.addEventListener("click", function () {
      var v = b.dataset.zoom;
      if (v === "reset") { k = 1; aplicar(); }
      else if (v === "fit") { ajustar(); }
      else { k = Math.min(40, Math.max(0.05, k * parseFloat(v))); aplicar(); }
    });
  });
  palco.addEventListener("wheel", function (e) {
    if (!e.ctrlKey) return;
    e.preventDefault();
    k = Math.min(40, Math.max(0.05, k * (e.deltaY < 0 ? 1.1 : 0.9)));
    aplicar();
  }, { passive: false });
  var arrastando = false, x0 = 0, y0 = 0, sx = 0, sy = 0;
  palco.addEventListener("mousedown", function (e) {
    arrastando = true; palco.classList.add("arrastando");
    x0 = e.clientX; y0 = e.clientY; sx = palco.scrollLeft; sy = palco.scrollTop;
  });
  window.addEventListener("mouseup", function () { arrastando = false; palco.classList.remove("arrastando"); });
  window.addEventListener("mousemove", function (e) {
    if (!arrastando) return;
    palco.scrollLeft = sx - (e.clientX - x0);
    palco.scrollTop = sy - (e.clientY - y0);
  });
  setTimeout(ajustar, 600);
})();
</script>
</body></html>',
    titulo, COR_FORA_PRAZO, COR_NO_PRAZO, titulo,
    paste(cab$protocolo, "\u2014", cab$tipo), format(Sys.time(), "%d/%m/%Y %H:%M"),
    corpo_grafo, linhas_prazo, linhas_dem, cores, formas,
    COR_SETA_CITACAO, COR_SETA_CITACAO,
    COR_SETA_ENDERECAMENTO, COR_SETA_ENDERECAMENTO,
    COR_SETA_PADRAO, COR_SETA_PADRAO)

  writeLines(html, arquivo, useBytes = TRUE)
  invisible(arquivo)
}
# ---------------------------------------------------------------------
# Log de eventos e relatório de prazos
# ---------------------------------------------------------------------

gerar_eventlog <- function(grafo) {
  if (!requireNamespace("bupaR", quietly = TRUE))
    stop("Instale o pacote bupaR para gerar o log de eventos.")
  grafo$nos |>
    filter(classe == "ato", !is.na(assinado_em)) |>
    transmute(
      protocolo = grafo$cabecalho$protocolo,
      atividade = coalesce(tipo_canonico, rotulo),
      instancia = id,
      estagio   = if_else(completo, "complete", "start"),
      ts        = assinado_em,
      recurso   = unidade
    ) |>
    bupaR::eventlog(case_id = "protocolo", activity_id = "atividade",
                    activity_instance_id = "instancia", lifecycle_id = "estagio",
                    timestamp = "ts", resource_id = "recurso")
}

#' Demandas geradas e sua situação
#'
#' Reúne, numa só tabela, tudo o que o diagrama sinaliza por cor: o que
#' foi demandado e ainda não veio, o que se espera do interessado, o que
#' não se concretizou e o que segue sem assinatura. É a leitura gerencial
#' do grafo.
relatar_demandas <- function(grafo) {
  todos <- bind_rows(grafo$nos, grafo$pendentes)
  extras <- todos |> filter(classe %in% c("faltante", "pendente", "complemento",
                                          "abortado", "assinatura"))
  if (!nrow(extras)) return(tibble())

  # a entrada de documentação responde pelos números que a compõem
  num <- setNames(
    map_chr(seq_len(nrow(grafo$nos)), \(i) {
      if (grafo$nos$classe[i] == "protocolo_externo")
        paste(grafo$nos$n_docs_ids[[i]], collapse = ", ")
      else grafo$nos$n_doc[i]
    }), grafo$nos$id)
  rot <- setNames(coalesce(grafo$nos$rotulo, grafo$nos$id), grafo$nos$id)

  # Documentos ligados a cada marcação. Nas marcações que apontam para o
  # ato, é o ato de destino; nas que o ato aponta — documento esperado —,
  # são todos os atos que levam até ele.
  liga <- grafo$arestas |>
    filter(tipo %in% c("faltante", "pendente", "complemento", "abortado", "assinatura")) |>
    mutate(marca = if_else(tipo %in% c("complemento", "abortado", "assinatura"), de, para),
           ato   = if_else(tipo %in% c("complemento", "abortado", "assinatura"), para, de)) |>
    group_by(marca) |>
    summarise(atos = list(unique(ato)), .groups = "drop")

  # Complementação só entra no quadro quando foi comunicada ao
  # interessado: o ofício que a veicula é o que direciona seta para o
  # e-mail registrado nos autos.
  ids_email <- grafo$nos$id[str_detect(sem_acento(tolower(coalesce(grafo$nos$tipo_canonico, ""))), "^e-?mail")]
  ids_oficio <- grafo$nos$id[str_detect(sem_acento(tolower(coalesce(grafo$nos$tipo_canonico, ""))), "^oficio")]
  comunicam <- grafo$arestas |> filter(para %in% ids_email, de %in% ids_oficio) |> pull(de)

  protocolos <- grafo$nos |> filter(classe == "protocolo_externo")
  pareceres <- grafo$nos |>
    filter(classe == "ato",
           str_detect(sem_acento(tolower(coalesce(tipo_canonico, ""))), "^parecer"))

  extras |>
    left_join(liga, by = c("id" = "marca")) |>
    rowwise() |>
    mutate(
      atos_ids = list(if (is.null(atos)) character(0) else atos),
      # entre as exigências, só uma entra no quadro; a escolha e a
      # situação dependem de até onde o fluxo avançou
      eh_comunicada = classe == "complemento" && any(atos_ids %in% comunicam),
      eh_oficio = classe == "complemento" && any(atos_ids %in% ids_oficio),
      data_ato = suppressWarnings(max(grafo$nos$data[grafo$nos$id %in% atos_ids], na.rm = TRUE)),
      documentos = paste(na.omit(num[atos_ids]), collapse = ", "),
      origem = paste(unique(na.omit(rot[atos_ids])), collapse = "; "),
      # a exigência se fecha quando o interessado protocola a
      # documentação e um parecer a analisa
      atendida = {
        d <- suppressWarnings(max(grafo$nos$data[grafo$nos$id %in% atos_ids], na.rm = TRUE))
        prot <- if (is.finite(d)) protocolos$data[protocolos$data > d] else as.Date(character(0))
        par <- if (length(prot)) pareceres$data[pareceres$data > min(prot)] else as.Date(character(0))
        length(par) > 0
      },
      situacao = dplyr::case_when(
        classe == "abortado"    ~ "encerrada sem resposta",
        classe == "assinatura"  ~ "aguarda assinatura",
        classe == "faltante"    ~ "aguarda parecer",
        classe == "complemento" ~ dplyr::case_when(
          !eh_comunicada & !eh_oficio ~ "complementa\u00e7\u00f5es aguardando convalida\u00e7\u00e3o",
          !eh_comunicada              ~ "aguardando enviar e-mail ao interessado",
          atendida                    ~ "atendida",
          TRUE                        ~ "aguarda documenta\u00e7\u00e3o complementar"),
        TRUE ~ "aguarda documento"
      ),
      detalhe = str_replace_all(coalesce(assunto, rotulo), "\n", " \u00b7 ")
    ) |>
    ungroup() |>
    # a exigência comunicada por e-mail tem precedência; não havendo,
    # vale a última registrada no fluxo
    group_by(classe == "complemento") |>
    filter(!(classe == "complemento") |
             (if (any(eh_comunicada)) eh_comunicada & data_ato == max(data_ato[eh_comunicada])
              else data_ato == max(data_ato))) |>
    ungroup() |>
    slice(if (sum(classe == "complemento") > 1)
            c(which(classe != "complemento"), which(classe == "complemento")[1])
          else seq_len(n())) |>
    transmute(
      marcacao = dplyr::recode(classe,
        faltante = "Parecer demandado n\u00e3o emitido",
        pendente = "Documento esperado",
        complemento = "Complementa\u00e7\u00f5es solicitadas",
        abortado = "Fluxo abortado",
        assinatura = "Assinatura pendente"),
      referencia = rotulo,
      documentos = if_else(nzchar(documentos), documentos, "\u2014"),
      origem = if_else(nzchar(origem), origem, "\u2014"),
      situacao, detalhe
    ) |>
    arrange(marcacao, referencia)
}
relatar_prazos <- function(grafo) {
  if (!nrow(grafo$prazos)) return(tibble::tibble())
  grafo$prazos |>
    transmute(
      # o prefixo é identificador interno do nó, não do documento
      abertura     = str_remove(id_abertura, "^D"),
      unidade      = unidade_abertura,
      inicio       = format(inicio, "%d/%m/%Y"),
      demanda      = coalesce(demanda, "\u2014"),
      limite       = format(limite, "%d/%m/%Y"),
      # ato que encerrou a contagem, pelo número do documento
      ato_encerramento = coalesce(documento, "\u2014"),
      encerramento = format(fim, "%d/%m/%Y"),
      dias_corridos, dias_limite, atraso_dias,
      # sem manifestação não há como afirmar cumprimento: o prazo segue
      # em curso ou já venceu sem resposta
      situacao = dplyr::case_when(
        excedeu                        ~ "EXCEDEU",
        !is.na(fim)                    ~ "no prazo",
        !is.na(limite) & limite < Sys.Date() ~ "VENCIDO sem manifesta\u00e7\u00e3o",
        TRUE                           ~ "em curso")
    )
}
