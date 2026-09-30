# =====================================================================
# executar.R — ÚNICO arquivo a executar
#
# Os cinco arquivos de R/ são bibliotecas de funções, carregadas
# automaticamente pela linha `source()` logo abaixo. Não é preciso
# executá-los, nem editá-los, nem rodá-los um a um.
#
# Duas formas de uso:
#
#   (1) No RStudio ou em qualquer editor de R: cole o link no bloco
#       CONFIGURAÇÃO abaixo e execute o arquivo inteiro (Ctrl+Shift+S,
#       ou o botão "Source").
#
#   (2) No terminal:
#       Rscript executar.R --url "https://sei.iphan.gov.br/.../md_pesq_processo_exibir.php?<hash>"
#       Rscript executar.R --acervo acervo/01450001620202678
#
# O diretório de trabalho precisa ser a pasta do projeto — a que contém
# R/ e este arquivo. No RStudio: Session > Set Working Directory > To
# Source File Location.
# =====================================================================

# ---------------------------------------------------------------------
# CONFIGURAÇÃO — preencha aqui se não for usar a linha de comando
# ---------------------------------------------------------------------

URL_PROCESSO <- ""   # cole entre as aspas o link de exibição do processo
PASTA_ACERVO <- ""   # ou, se os autos já estiverem salvos, o caminho da pasta

# ---------------------------------------------------------------------

for (f in list.files("R", pattern = "\\.R$", full.names = TRUE)) source(f)

args <- commandArgs(trailingOnly = TRUE)
opt  <- function(nome, padrao = NULL) {
  i <- match(paste0("--", nome), args)
  if (is.na(i) || i == length(args)) padrao else args[i + 1]
}
vazio <- function(x) is.null(x) || !nzchar(x)

# A linha de comando tem precedência; na ausência dela, vale o bloco de
# configuração acima.
url    <- opt("url",    if (vazio(URL_PROCESSO)) NULL else URL_PROCESSO)
pasta  <- opt("acervo", if (vazio(PASTA_ACERVO)) NULL else PASTA_ACERVO)
saida  <- opt("saida", "saida")
dir.create(saida, showWarnings = FALSE)

# --- 1. Acervo -------------------------------------------------------
# Com o link, o pipeline raspa a página do processo e cada documento
# nativo e arquiva tudo em disco. Com o acervo já em disco, pula esta
# etapa — e é assim que as iterações seguintes rodam, sobre material
# congelado e sem nova consulta ao serviço público.
if (is.null(pasta)) {
  if (is.null(url))
    stop("Informe o link do processo em URL_PROCESSO (bloco de ",
         "configuração no topo deste arquivo) ou por --url na linha de comando.")
  message("Baixando os autos a partir do link...")
  pasta <- sei_baixar_autos(url)
}
acervo <- sei_ler_acervo(pasta)

# --- 2. Grafo --------------------------------------------------------
grafo <- montar_grafo(acervo)

# --- 3. Saídas -------------------------------------------------------
gerar_mermaid(grafo,  file.path(saida, "grafo.mmd"))
gerar_graphviz(grafo, file.path(saida, "grafo.gv"))
gerar_html(grafo,     file.path(saida, "grafo.html"))
readr::write_csv(relatar_prazos(grafo),   file.path(saida, "prazos.csv"))
readr::write_csv(relatar_demandas(grafo), file.path(saida, "demandas.csv"))
readr::write_csv(grafo$nos |> select(-destinatarios, -referencias),
                 file.path(saida, "nos.csv"))
readr::write_csv(grafo$arestas, file.path(saida, "arestas.csv"))

# Renderização: o Graphviz de linha de comando basta, sem navegador.
if (nzchar(Sys.which("dot"))) {
  system2("dot", c("-Tpng", "-Gdpi=100",
                   shQuote(file.path(saida, "grafo.gv")),
                   "-o", shQuote(file.path(saida, "grafo.png"))))
  system2("dot", c("-Tsvg", shQuote(file.path(saida, "grafo.gv")),
                   "-o", shQuote(file.path(saida, "grafo.svg"))))
}

message("Nós: ",      nrow(grafo$nos),
        " | arestas: ", nrow(grafo$arestas),
        " | pendentes: ", nrow(grafo$pendentes),
        " | prazos apurados: ", nrow(grafo$prazos))
print(relatar_prazos(grafo))
