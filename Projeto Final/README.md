# POC — Mineração de Processos de Licenciamento Ambiental (SEI/IPHAN)

## Objetivo

Prova de conceito para avaliar a viabilidade de automatizar a identificação de pendências e a
reconstrução de fluxos processuais em processos de licenciamento ambiental disponíveis no
**SEI Pesquisa Pública do IPHAN**.

O projeto compara os resultados automatizados com um gabarito construído manualmente.

**Escopo:** Termina na avaliação (CRISP-DM). Não há etapa de Deployment.

---

## Metodologia

O projeto segue o framework **CRISP-DM**:

1. Business Understanding
2. Data Understanding
3. Data Preparation
4. Modeling
5. Evaluation

---

## Estrutura do Projeto

```
R/
  01_amostragem.R                 # Amostragem aleatória simples (semente fixada)
  02_sei_scraping.R               # Scraping do SEI Pesquisa Pública
  03_parser_documentos.R          # Extração de conteúdo dos documentos nativos
  04_entidades.R                  # Entidades (remetente, destinatário, unidade, prazo, referências)
  05_text_mining.R                # Normalização, tokenização, TF-IDF
  06_classificacao.R              # Classificação semântica — baseline (regras/dicionário)
  06b_classificacao_supervisionada.R  # Classificação supervisionada (de conjunto)
  07_event_log.R                  # Construção do event log (Process Mining)
  08_normativas.R                 # Regras da IN IPHAN nº 06/2025
  09_prazos.R                     # Cálculo e verificação de prazos
  10_grafo.R                      # Grafo de tramitação (documento=nó, seta=fluxo)
  11_avaliacao.R                  # Avaliação contra gabarito manual (de conjunto)
  13_visualizacao_textmining.R    # Visualizações do text mining (TF-IDF, tokens, datas)
  utils_saida.R                   # Organização das saídas em pastas por data
  utils_gabarito.R                # Consolidação do gabarito manual

data/
  raw/                    # HTML bruto e dados não processados
  processed/              # Dados limpos e estruturados (pastas por data AAAAMMDD)
  models/                 # Modelos treinados
  normativas/             # Regras da IN nº 06/2025 (estruturado)
  ground_truth/           # Gabarito manual

output/                   # Resultados e visualizações (pastas por data AAAAMMDD)

tests/                    # Testes unitários (testthat)

config/                   # Configurações do projeto

scripts/                  # Utilitários avulsos (scraping por linha de comando, regeneração, diagnóstico)

docs/                     # Documentação de domínio (FLUXOS.md, IN 06/2025, projeto de referência)

main.R                    # Ponto de entrada (função analisar_processo)
executar.R                # Ponto de entrada por linha de comando (pipeline completo)
README.md
```

---

## Dados Experimentais

- Processos de **2026**, tipo "Licenciamento Ambiental - IN nº 006/2025"
- Amostragem: **aleatória simples**, `set.seed(123)`, n = 5 processos
- Fonte: [SEI Pesquisa Pública IPHAN](https://sei.iphan.gov.br/sei/modulos/pesquisa/md_pesq_processo_pesquisar.php)

---

## Como Executar

### Pré-requisitos

```r
install.packages(c("httr2", "rvest", "xml2", "tidyverse", "lubridate",
                   "stringr", "jsonlite", "purrr", "testthat"))
```

### Etapa 1 — Amostragem

```r
source("R/01_amostragem.R")
```

### Etapa 2 — Scraping de um processo

```r
source("R/02_sei_scraping.R")

url <- "https://sei.iphan.gov.br/sei/modulos/pesquisa/md_pesq_processo_exibir.php?TOKEN"
resultado <- scrape_processo_sei(url)
```

Ou via linha de comando (salva HTML em `data/raw/` e JSON em `data/processed/`):

```
Rscript scripts/scrapar.R "URL_DO_PROCESSO"
```

Se a URL de sessão do SEI já tiver expirado, rode o scraper contra o HTML salvo
localmente em `Paginas/`:

```
Rscript scripts/executar_scraping_local.R
```

### Etapa 3 — Extração de conteúdo dos documentos nativos

Distingue documentos **nativos** (gerados no SEI, em HTML) de **anexos** (Word, planilha,
geoespacial, PDF). O conteúdo textual é extraído apenas dos tipos nativos configurados em
`config/extracao_conteudo.json` (Despacho, Ofício, Parecer). Todos os documentos permanecem
listados; anexos e restritos são registrados como não extraídos, de forma mensurável.

```r
source("R/03_parser_documentos.R")

# A partir de um JSON de scraping já salvo
conteudo <- extrair_conteudo_de_json("data/processed/PROCESSO_DATA.json")

# Ou a partir do objeto de scraping em memória
conteudo <- extrair_conteudo_documentos(resultado)
```

A distinção nativo vs. anexo é feita pelo `Content-Type` da resposta HTTP (nativos respondem
`text/html`, anexos respondem binário), combinada com a lista de tipos configurada.

### Pipeline completo (após todas as etapas)

Na sessão do R:

```r
source("main.R")
analisar_processo(url)
```

Ou por linha de comando (executa todas as etapas e persiste os resultados):

```
Rscript executar.R "URL_DO_PROCESSO"
```

A avaliação contra o gabarito é de **conjunto** (compara todos os processos rotulados
de uma vez) e roda à parte, após ter o gabarito em `data/ground_truth/`:

```r
source("main.R")
avaliar_processos()                              # baseline + cobertura
avaliar_processos(incluir_supervisionado = TRUE) # + modelo supervisionado
```

---

## Limitações Conhecidas

- Documentos com restrição de acesso são registrados como indisponíveis (não contornados)
- Documentos sem texto ou em formato não suportado são registrados como limitação
- Anexos (Word, planilha, geoespacial, PDF) são listados mas não têm texto extraído nesta etapa
- Amostra pequena (n=5): resultados não generalizáveis
- Possível necessidade de OCR para documentos PDF digitalizados
- SEI usa charset `iso-8859-1` — requer tratamento de encoding

---

## Normativa Principal

**IN IPHAN nº 06/2025** — regras armazenadas em `data/normativas/` (estruturado, não hardcoded)

---

## Princípios

Reprodutibilidade · Modularidade · Auditabilidade · Simplicidade · Clareza científica
