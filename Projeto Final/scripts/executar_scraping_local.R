# executar_scraping_local.R
# Roda o scraper contra o HTML local salvo em Paginas/
# Usado quando a URL de sessão do SEI já expirou.
# Uso: Rscript scripts/executar_scraping_local.R

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

source("R/02_sei_scraping.R")

resultado <- scrape_processo_html_local(
  caminho_html  = "Paginas/Pagina Processo Buscado.html",
  url_original  = "https://sei.iphan.gov.br/sei/modulos/pesquisa/md_pesq_processo_exibir.php?iI3OtHvPArITY997V09rhsSkbDKbaYSycOHqqF2xsM0IaDkkEyJpus7kCPb435VNEAb16AAxmJKUdrsNWVIqQw02_CutLpMKF6Yxd-0vMVC_Qpr7vnJcx6Gys6LheGMm"
)

cat("\n========== DOCUMENTOS ==========\n")
print(resultado$documentos[, c("numero_documento","tipo","data_documento","unidade_sigla","acesso")])

cat("\n========== ANDAMENTOS (primeiros 10) ==========\n")
print(head(resultado$andamentos[, c("data_hora","unidade_sigla","descricao","status")], 10))
