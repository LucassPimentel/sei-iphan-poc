# =============================================================================
# tmp_teste_referencias.R  (utilitario de diagnostico)
# Testa extrair_referencias() com textos REAIS do processo 003836.
# Esperado (arestas azuis do Mermaid do especialista):
#   D7519156 --> D7535801   (7535801 = Parecer Tecnico 1506 cita 7519156)
#   D7519156 --> D7557362   (7557362 = Despacho 9239 cita 7519156)
#   D7550397 --> D7557362   (7557362 cita 7550397)
# Ou seja, 7535801 deve conter {7519156}; 7557362 deve conter {7519156, 7550397}.
#
# Uso (PowerShell):
#   & "C:\Program Files\R\R-4.6.1\bin\Rscript.exe" scripts/tmp_teste_referencias.R
# =============================================================================

# --- Garante que o working directory seja a RAIZ do projeto ---------------
.definir_raiz_projeto <- function() {
  args_full <- commandArgs(trailingOnly = FALSE)
  arg_file <- grep("^--file=", args_full, value = TRUE)
  caminho_script <- if (length(arg_file) > 0) {
    sub("^--file=", "", arg_file[1])
  } else if (!is.null(sys.frames()[[1]]$ofile)) {
    sys.frames()[[1]]$ofile
  } else {
    NA_character_
  }
  if (!is.na(caminho_script)) {
    raiz <- dirname(dirname(normalizePath(caminho_script)))
    setwd(raiz)
  }
}
.definir_raiz_projeto()

suppressWarnings(suppressMessages({
  library(jsonlite)
  source("R/utils_saida.R")
  source("R/04_entidades.R")
}))

cam <- encontrar_arquivo_processo("data/processed", "01450_003836_2026_78",
                                  etapa = "conteudo")
cat("[conteudo]", basename(cam), "\n\n")

j <- fromJSON(cam, simplifyVector = TRUE)
docs <- j$documentos_conteudo
docs <- docs[!is.na(docs$conteudo_extraido) & docs$conteudo_extraido == TRUE, ]

alvos <- c("7535801", "7557362")
for (num in alvos) {
  linha <- docs[as.character(docs$numero_documento) == num, ]
  if (nrow(linha) == 0) {
    cat("[NAO ENCONTRADO no conteudo]:", num, "\n")
    next
  }
  refs <- extrair_referencias(linha$texto[1], num)
  cat("Doc", num, "(", linha$tipo[1], ") cita:",
      if (length(refs) > 0) paste(refs, collapse = ", ") else "(nenhuma)", "\n")
}

cat("\n--- Todas as referencias por documento (visao geral) ---\n")
for (i in seq_len(nrow(docs))) {
  num  <- as.character(docs$numero_documento[i])
  refs <- extrair_referencias(docs$texto[i], num)
  if (length(refs) > 0) {
    cat(num, "(", docs$tipo[i], ") ->", paste(refs, collapse = ", "), "\n")
  }
}
