# =====================================================================
# 02_documento.R — Extração do conteúdo dos documentos nativos do SEI
#
# De cada documento nativo saem seis coisas, e são elas que alimentam o
# grafo:
#   destinatarios  -> rótulo da aresta (caixa cinza no diagrama)
#   assunto        -> texto ligado ao nó por linha sem seta
#   referencias    -> números "Processo / Documento" citados no corpo;
#                     é o que dá direção às setas
#   assinaturas    -> nome, cargo e instante de cada signatário
#   prazo_informado-> data-limite de manifestação, quando o documento a fixa
#   marcacoes      -> conteúdo de tabelas de marcação (X), quando houver
# =====================================================================

suppressPackageStartupMessages({
  library(rvest); library(dplyr); library(stringr); library(purrr); library(tibble)
})

# Aberturas de correspondência, em suas variações usuais no SEI:
#   "À Senhora", "Ao Senhor", "À técnica", "Ao(À) Sr(a).", "Para:"
RE_ABERTURA  <- "^(\u00c0|Ao)\\b|^Para\\s*:"
RE_FECHO     <- "^(Atenciosamente|Respeitosamente|Cordialmente|Sem mais|Sendo o que|É o parecer|Submeto)"
RE_ASSINADO  <- "Documento assinado eletronicamente por\\s+(.+?)\\s*,\\s*(.+?)\\s*,\\s*em\\s+(\\d{2}/\\d{2}/\\d{4}),\\s*às\\s+(\\d{2}:\\d{2})"
# Pedido de complementacao ou recomendacao dirigido ao interessado.
# As formas verbais em primeira pessoa e as impessoais sao o que de fato
# caracterizam a exigencia; "Recomenda aprovacao" e rotulo de tabela e
# "solicitacao em epigrafe" e mencao a pedido alheio, ambos fora.
RE_COMPLEMENTO <- paste0("complementac(ao|oes)|complemento(s)?\\b|",
                         "solicita-se|solicito\\b|solicitamos|",
                         "esclarecimento(s)?\\b")

RE_PRAZO     <- "(?:prazo legal para manifesta\\S+ do IPHAN encerrar-se-\\S+ em|Prazo de manifesta\\S+:?)\\s*(\\d{2}/\\d{2}/\\d{4})"

#' Remove caracteres invisíveis que o editor do SEI insere no corpo
#'
#' Espaços de largura zero e marcas de direção aparecem no meio de
#' números de documento e atrapalham tanto a extração de referências
#' quanto o casamento de rótulos.
limpar_invisiveis <- function(x) {
  str_remove_all(x, "[\u200b\u200c\u200d\u200e\u200f\ufeff\u00ad]")
}

#' Quebra o documento em linhas de texto, preservando a ordem visual
#'
#' Folhas de estilo e scripts embutidos são descartados: no e-mail do
#' SEI eles vêm no corpo e poluiriam qualquer busca por termos.
linhas_do_documento <- function(arquivo) {
  pag <- ler_html(arquivo)
  xml2::xml_remove(html_elements(pag, "style, script"))
  bruto <- pag |> html_element("body") |> html_text2()
  linhas <- str_split(bruto, "\n")[[1]] |> limpar_invisiveis() |> str_squish()
  linhas[nzchar(linhas)]
}

#' Destinatários: bloco entre a abertura ("À Senhora", "Para:") e o assunto
extrair_destinatarios <- function(linhas) {
  # Um documento pode dirigir-se a mais de um destinatario ("C/C"). Cada
  # bloco comeca por linha de tratamento e segue com nome e vinculo, ate
  # que apareca endereco, CEP, e-mail ou novo bloco. O endereco e
  # descartado; os e-mails sao guardados ao lado do bloco, porque sao a
  # chave que liga o oficio ao e-mail registrado nos autos.
  RE_TRATAMENTO <- paste0("^(\u00c0|Ao|Aos|\u00c0s)\\b|^Para\\s*:|",
                          "^Senhor(a|es)?\\s*,?$|^Ilustr")
  # Prefixo de tratamento: e removido da linha, e o que sobra e o nome.
  # "Ao Sr.Fulano" vira "Fulano"; "Ao Senhor," vira vazio e o nome vem na
  # linha seguinte; "Ao orgao ambiental licenciador," nao casa e e
  # mantido inteiro, por identificar o destinatario.
  RE_PREFIXO <- paste0("^(\u00c0|Ao|Aos|\u00c0s)\\s*\\(?\u00c0?\\)?\\s*",
                       "(Senhor(a|es)?|Sr\\(a\\)|Sra?|arque\u00f3log\\w*|",
                       "t\u00e9cnic\\w*|Ilustr\\w*)\\.?\\s*,?\\s*|",
                       "^(\u00c0|Ao)\\s*$")
  RE_CORPO <- paste0("^(Prezad|Cumprimentando|Ao cumpriment|Senhor\\s|Ao responder|",
                     "Encaminho\\b|Portanto|Informamos|Ressalt|[IVX]+\\.\\s)")
  RE_ENDERECO <- paste0("^(Rua|Avenida|Av\\.|Travessa|Rodovia|Pra\u00e7a|Alameda|Estrada|",
                        "SEPS|SBN|SCS|Quadra|Bloco|Edif\u00edcio)\\b|",
                        "^CEP\\b|^Tel|^Fone|^Empreendimento\\s*:|",
                        "^Refer\u00eancia\\s*:|^Processo\\s*n|^Assunto\\s*:|^Ass\\.?\\s*:|",
                        "^C/[Cc]\\s*:?$|^\\d|CEP\\s*:")
  RE_EMAIL <- "[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\\.[A-Za-z]{2,}"

  vazio <- tibble(bloco = character(), emails = list())
  corpo_ini <- which(str_detect(linhas, RE_CORPO))
  limite <- if (length(corpo_ini)) min(corpo_ini) else length(linhas) + 1L

  inicios <- which(str_detect(linhas, RE_TRATAMENTO) & !str_detect(linhas, RE_CORPO))
  inicios <- inicios[inicios < limite]
  if (!length(inicios)) return(vazio)

  out <- vazio
  for (i in inicios) {
    if (str_detect(linhas[i], "Assunto\\s*:|Ass\\.?\\s*:")) {
      alvo <- str_squish(str_remove(str_extract(linhas[i], "^.*?(?=Assunto|Ass\\.?\\s*:)"), RE_PREFIXO))
      # nome e vinculo saem colados quando o editor perde a quebra de
      # linha ("TauhylSubdivisao"); a fronteira minuscula-maiuscula a
      # restitui
      alvo <- str_replace_all(alvo, "([a-z\u00e0-\u00ff])([A-Z\u00c0-\u00dc])", "\\1\n\\2")
      if (nzchar(alvo)) out <- add_row(out, bloco = alvo, emails = list(character(0)))
      next
    }
    # A quebra colada ("Ao TécnicoEDSON MIRANDA") precisa ser desfeita
    # antes de remover o tratamento: de outro modo o "\\w*" do padrão
    # engole o começo do nome.
    partes <- str_split(str_replace_all(linhas[i],
                          "([a-z\u00e0-\u00ff])([A-Z\u00c0-\u00dc]{2,})",
                          "\\1\n\\2"), "\n")[[1]]
    cabeca <- if (str_detect(partes[1], "^Para\\s*:")) {
      str_squish(str_remove(partes[1], "^Para\\s*:"))
    } else {
      str_squish(str_remove(partes[1], regex(RE_PREFIXO, ignore_case = TRUE)))
    }
    cabeca <- c(cabeca, str_squish(partes[-1]))
    cabeca <- cabeca[nzchar(cabeca)]

    # A janela do bloco vai ate o proximo tratamento ou ate o corpo.
    # Dela saem, separadamente, as duas primeiras linhas de nome e todos
    # os enderecos eletronicos declarados.
    seguinte <- inicios[inicios > i]
    fim_bloco <- if (length(seguinte)) min(seguinte) else limite
    janela <- if (fim_bloco > i + 1) linhas[(i + 1):(fim_bloco - 1)] else character(0)

    emails <- unique(unlist(str_extract_all(janela, RE_EMAIL)))
    nomes <- character(0)
    for (ln in janela) {
      if (str_detect(ln, RE_EMAIL) || str_detect(ln, RE_ENDERECO)) break
      nomes <- c(nomes, ln)
      if (length(nomes) >= 2) break
    }
    # O editor do SEI às vezes perde a quebra e cola tratamento e nome
    # ("Ao TécnicoEDSON MIRANDA"); a fronteira minúscula-maiúscula a
    # restitui, e o tratamento volta a ser removido.
    bloco <- str_split(str_replace_all(paste(c(cabeca, nomes), collapse = "\n"),
                                       # só onde um nome em caixa alta
                                       # segue letra minúscula, para não
                                       # partir "do Patrimônio"
                                       "([a-z\u00e0-\u00ff])([A-Z\u00c0-\u00dc]{2,})",
                                       "\\1\n\\2"), "\n")[[1]]
    bloco[1] <- str_squish(str_remove(bloco[1], regex(RE_PREFIXO, ignore_case = TRUE)))
    bloco <- str_squish(bloco); bloco <- bloco[nzchar(bloco)]
    bloco <- head(bloco, 2)
    if (length(bloco))
      out <- add_row(out, bloco = paste(bloco, collapse = "\n"),
                     emails = list(unique(tolower(emails))))
  }
  out |> distinct(bloco, .keep_all = TRUE)
}

#' Assunto do documento
extrair_assunto <- function(linhas) {
  i <- which(str_detect(linhas, "Assunto\\s*:|^Ass\\.?\\s*:"))
  if (length(i)) {
    i <- i[1]
    corpo <- str_squish(str_remove(linhas[i], "^.*?(Assunto|Ass\\.?)\\s*:"))
    j <- i
    while (!nzchar(corpo) && j < length(linhas)) { j <- j + 1; corpo <- linhas[j] }
    k <- j + 1
    while (k <= length(linhas) &&
           !str_detect(linhas[k], "^(Processo|Prezad|Senhor|Ao |\u00c0 |Refer\u00eancia|[0-9]+\\.|[IVXLC]+\\.)") &&
           nchar(corpo) < 400) {
      corpo <- paste(corpo, linhas[k]); k <- k + 1
    }
    # o rodape "Referencia: Caso responda este..." nao faz parte do assunto
    corpo <- str_remove(corpo, "\\s*Refer\u00eancia\\s*:.*$")
    return(str_squish(corpo))
  }
  # Sem rotulo "Assunto:" - usa o corpo da correspondencia, que e o que os
  # despachos curtos trazem no lugar ("Encaminho ... para analise e
  # manifestacao tecnica").
  ini <- which(str_detect(linhas, "^(Prezad|Senhor|Ilustr)"))
  fim <- which(str_detect(linhas, RE_FECHO))
  if (!length(ini) || !length(fim) || !any(fim > ini[1])) return(NA_character_)
  bloco <- linhas[(ini[1] + 1):(min(fim[fim > ini[1]]) - 1)]
  bloco <- bloco[nzchar(bloco)]
  if (!length(bloco)) return(NA_character_)
  str_squish(str_trunc(paste(bloco, collapse = " "), 300))
}

#' Referências a outros documentos do processo
#'
#' Captura os números entre parênteses e as menções "SEI nº". Descarta o
#' próprio número do documento, o código verificador do rodapé e o bloco
#' final "Referência: ... SEI nº".
extrair_referencias <- function(linhas, n_doc_proprio) {
  corte <- which(str_detect(linhas, "^(Refer\\Sncia\\s*:|A autenticidade deste documento)"))
  corpo <- if (length(corte)) linhas[seq_len(min(corte) - 1)] else linhas
  texto <- limpar_invisiveis(paste(corpo, collapse = " "))
  refs <- c(
    str_match_all(texto, "\\((\\s*\\d{7,8}\\s*)\\)")[[1]][, 2],
    str_match_all(texto, "SEI\\s*n?º?\\s*(\\d{7,8})")[[1]][, 2],
    str_match_all(texto, "_(\\d{7,8})\\.pdf")[[1]][, 2]   # anexos de e-mail
  )
  refs <- unique(str_squish(refs))
  setdiff(refs, as.character(n_doc_proprio))
}

#' Assinaturas eletrônicas: nome, cargo e instante
extrair_assinaturas <- function(linhas) {
  texto <- paste(linhas, collapse = " ")
  m <- str_match_all(texto, RE_ASSINADO)[[1]]
  if (nrow(m) == 0) return(tibble(signatario = character(), cargo = character(),
                                  assinado_em = as.POSIXct(character())))
  tibble(
    signatario  = str_squish(m[, 2]),
    cargo       = str_squish(m[, 3]),
    assinado_em = as_datahora(paste(m[, 4], m[, 5]))
  ) |> distinct() |> arrange(assinado_em)
}

#' Quantas assinaturas o ato deve reunir ao final
#'
#' Cada "De acordo" no corpo indica um anuente além do redator. A forma
#' "de acordo com" é texto corrido e não conta.
assinaturas_esperadas <- function(linhas) {
  # O "De acordo" que convoca anuente fica na area de assinatura, entre o
  # fecho da correspondencia e o rodape de assinatura eletronica. No
  # corpo do texto a expressao e apenas conectivo ("de acordo com a
  # norma"), e conta-la ali inflaria a estimativa.
  rodape <- which(str_detect(linhas, "^(Documento assinado eletronicamente|A autenticidade)"))
  fim <- if (length(rodape)) min(rodape) - 1L else length(linhas)
  fecho <- which(str_detect(linhas, RE_FECHO))
  fecho <- fecho[fecho <= fim]
  ini <- if (length(fecho)) max(fecho) else max(1L, fim - 10L)
  if (ini > fim) return(1L)

  area <- paste(linhas[ini:fim], collapse = " ")
  n <- str_count(area, regex("De acordo(?!\\s+com)", ignore_case = TRUE))
  max(1L, 1L + n)
}

#' Data-limite de manifestação fixada no documento (normalmente pela CGM)
extrair_prazo <- function(linhas) {
  m <- str_match(paste(linhas, collapse = " "), RE_PRAZO)
  if (is.na(m[1])) return(as.Date(NA)) else as_data(m[2])
}

#' Termos de complementação ou recomendação encontrados no corpo
#'
#' Devolve os trechos em que o ato pede complementação ou recomenda
#' providências, para que a exigência fique visível no diagrama.
extrair_complementos <- function(linhas) {
  corte <- which(str_detect(linhas, "^(Documento assinado eletronicamente|A autenticidade)"))
  corpo <- if (length(corte)) linhas[seq_len(min(corte) - 1)] else linhas
  achados <- corpo[str_detect(sem_acento(tolower(corpo)), RE_COMPLEMENTO)]
  achados <- achados[nchar(achados) > 20]
  unique(str_squish(str_trunc(achados, 180)))
}

#' Pareceres citados no corpo pelo número interno
#'
#' Nem todo ato cita o parecer pelo número do SEI; é comum a menção pela
#' numeração própria do documento ("Parecer Técnico nº 573/2026"). A
#' referência existe, apenas em outra forma.
extrair_mencoes_parecer <- function(linhas) {
  texto <- paste(linhas, collapse = " ")
  m <- str_match_all(texto,
    regex("parecer[^.;]{0,90}?n[\u00ba\u00b0o]?\\s*(\\d{1,5})\\s*/\\s*(\\d{4})",
          ignore_case = TRUE))[[1]]
  if (!nrow(m)) return(character(0))
  unique(m[, 2])
}

#' Cabeçalho de e-mail registrado nos autos
extrair_email <- function(linhas) {
  campo <- function(rot) {
    i <- which(str_detect(linhas, paste0("^", rot, "\\s*:?$")))
    if (!length(i)) {
      i <- which(str_detect(linhas, paste0("^", rot, "\\s*:")))
      if (!length(i)) return(NA_character_)
      return(str_squish(str_remove(linhas[i[1]], paste0("^", rot, "\\s*:"))))
    }
    i <- i[1]; j <- i + 1; out <- character()
    while (j <= length(linhas) &&
           !str_detect(linhas[j], "^(Data de Envio|De|Para|Assunto|Mensagem|Anexos)\\s*:?$")) {
      out <- c(out, linhas[j]); j <- j + 1
    }
    paste(out, collapse = "; ")
  }
  tibble(
    enviado_em = campo("Data de Envio"),
    de         = campo("De"),
    para       = campo("Para"),
    assunto    = campo("Assunto"),
    anexos     = campo("Anexos")
  )
}

# ---------------------------------------------------------------------
# Tabelas de marcação
# ---------------------------------------------------------------------

#' Tabela de item único marcado com X (ex.: nível exigido no TRE)
#'
#' Percorre as tabelas do documento procurando a linha cuja primeira
#' célula contenha apenas "X"; devolve o texto da célula seguinte.
extrair_marcacao_simples <- function(arquivo) {
  itens <- extrair_itens_marcados(arquivo)
  if (!length(itens)) return(NA_character_)
  paste(itens, collapse = "\n")
}

#' Itens assinalados com X em tabela de marcação
#'
#' A célula do item traz, em negrito, o termo que o identifica e, em
#' seguida, a explicação do que será exigido. Interessa o termo: é ele
#' que define os estudos demandados e, por consequência, os pareceres
#' esperados. Havendo mais de uma linha marcada, todas são devolvidas.
extrair_itens_marcados <- function(arquivo) {
  pag <- ler_html(arquivo)
  for (t in html_elements(pag, "table")) {
    achados <- character(0)
    for (tr in html_elements(t, "tr")) {
      cs <- html_elements(tr, "td, th")
      if (length(cs) < 2) next
      if (!str_detect(txt1(cs[[1]]), "^[Xx]$")) next
      negrito <- html_elements(cs[[2]], "b, strong")
      termo <- if (length(negrito)) txt1(negrito[[1]]) else str_trunc(txt1(cs[[2]]), 120)
      if (!is.na(termo) && nzchar(termo)) achados <- c(achados, limpar_invisiveis(termo))
    }
    if (length(achados)) return(unique(str_squish(achados)))
  }
  character(0)
}

#' Termos em negrito do corpo do documento
#'
#' O despacho da unidade de triagem costuma destacar em negrito o
#' resultado da análise automática — nível do empreendimento, termo de
#' compromisso, componente cultural. Esses termos estão no corpo, e não
#' no assunto, que muitas vezes nada informa sobre o resultado.
extrair_negritos <- function(arquivo) {
  pag <- ler_html(arquivo)
  corpo <- html_elements(pag, "b, strong")
  if (!length(corpo)) return(character(0))
  termos <- map_chr(corpo, txt1)
  termos <- limpar_invisiveis(str_squish(termos))
  termos <- termos[nchar(termos) > 8 & nchar(termos) < 200]
  unique(termos)
}

#' Tabela de manifestação com marcação em coluna (ex.: "Recomenda aprovação")
#'
#' O leiaute é de duas linhas: na primeira, a célula do rótulo tem
#' rowspan=2 e as demais trazem as opções (SIM / NÃO); na segunda, as
#' células trazem a marcação, alinhadas às opções pela posição. A função
#' devolve a opção efetivamente marcada.
extrair_marcacao_opcao <- function(arquivo, rotulo) {
  pag <- ler_html(arquivo)
  for (t in html_elements(pag, "table")) {
    trs <- html_elements(t, "tr")
    for (i in seq_along(trs)) {
      cs <- trs[[i]] |> html_elements("td, th")
      if (length(cs) < 2) next
      if (!str_detect(txt1(cs[[1]]), regex(rotulo, ignore_case = TRUE))) next
      opcoes <- map_chr(cs[-1], txt1)
      opcoes <- opcoes[nzchar(opcoes)]
      if (i + 1 > length(trs)) next
      marcas <- trs[[i + 1]] |> html_elements("td, th") |> map_chr(txt1)
      pos <- which(str_detect(marcas, "^[Xx]$"))
      if (!length(pos)) next
      return(opcoes[min(pos, length(opcoes))])
    }
  }
  NA_character_
}

# ---------------------------------------------------------------------
# Orquestração por documento
# ---------------------------------------------------------------------

#' Extrai tudo de um documento nativo
ler_documento <- function(arquivo, n_doc) {
  linhas <- linhas_do_documento(arquivo)
  ass <- extrair_assinaturas(linhas)
  eh_email <- any(str_detect(linhas, "^Data de Envio\\s*:?$"))

  # Um ato do processo é documento nativo do SEI: traz o rodapé de
  # assinatura eletrônica, o selo de autenticidade ou o cabeçalho de
  # e-mail. Arquivo anexado ao processo — PDF, planilha, arquivo
  # geoespacial — não tem nenhum desses marcadores e não é ato.
  marcadores <- c("^Documento assinado eletronicamente",
                  "^A autenticidade deste documento",
                  "^Data de Envio\\s*:?$")
  nativo <- any(map_lgl(marcadores, \(m) any(str_detect(linhas, m))))

  # Componente cultural declarado no corpo, para os pareceres de rótulo
  # genérico ("Parecer - Ficha de Caracterização de Atividade FCA")
  corpo <- sem_acento(tolower(paste(linhas, collapse = " ")))
  componente_corpo <- dplyr::case_when(
    str_detect(corpo, "patrimonio imaterial")   ~ "imaterial",
    str_detect(corpo, "patrimonio arqueologic") ~ "arqueologico",
    str_detect(corpo, "patrimonio material")    ~ "material",
    TRUE ~ NA_character_)

  tibble(
    n_doc          = as.character(n_doc),
    nativo, componente_corpo,
    # corpo normalizado, para cruzamento de termos entre documentos
    corpo_norm = str_trunc(corpo, 6000),
    destinatarios  = list(extrair_destinatarios(linhas)),
    assunto        = if (eh_email) extrair_email(linhas)$assunto else extrair_assunto(linhas),
    referencias    = list(extrair_referencias(linhas, n_doc)),
    assinaturas    = list(ass),
    n_assinaturas  = nrow(ass),
    n_esperadas    = assinaturas_esperadas(linhas),
    assinado_em    = if (nrow(ass)) max(ass$assinado_em) else as.POSIXct(NA),
    prazo_informado = extrair_prazo(linhas),
    email          = list(if (eh_email) extrair_email(linhas) else NULL),
    # pedido de complementacao dirigido ao interessado
    complementos   = list(extrair_complementos(linhas)),
    # Encaminhamento para cadastro em base georreferenciada. O pedido
    # tanto pode estar no corpo quanto no assunto ou na função do
    # destinatário — "Chefe da Divisão de Geoprocessamento" já indica a
    # unidade a que a demanda se dirige.
    dbgeo          = str_detect(corpo, paste0(
                       "dbgeo|base de dados georreferenc|base de dados geografic|",
                       "base geografic|georreferenciament|georreferenciad|",
                       "cadastrament|cadastro na base|geoprocessament|",
                       "divisao de geoprocessamento")),
    # análise de enquadramento do empreendimento
    enquadramento  = str_detect(corpo, "\\benquadrament|\\breenquadrament"),
    # pareceres citados pelo número interno, e não pelo número SEI
    mencoes_parecer = list(extrair_mencoes_parecer(linhas)),
    marcacao       = extrair_marcacao_simples(arquivo),
    itens_marcados = list(extrair_itens_marcados(arquivo)),
    negritos       = list(extrair_negritos(arquivo)),
    recomendacao   = extrair_marcacao_opcao(arquivo, "Recomenda aprova")
  )
}

#' Lê todos os documentos nativos do acervo
ler_documentos_nativos <- function(acervo) {
  pmap_dfr(acervo$nativos, \(n_doc, arquivo) ler_documento(arquivo, n_doc))
}
