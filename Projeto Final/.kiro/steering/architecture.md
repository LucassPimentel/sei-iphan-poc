---
inclusion: always
---
# Arquitetura da POC

## Organização

Estrutura preferencial:

```text
R/
  01_amostragem.R
  02_sei_scraping.R
  03_parser_documentos.R
  04_entidades.R
  05_text_mining.R
  06_classificacao.R
  06b_classificacao_supervisionada.R
  07_event_log.R
  08_normativas.R
  09_prazos.R
  10_grafo.R
  11_avaliacao.R
  12_pipeline.R
  utils_saida.R
  utils_gabarito.R

data/
  raw/
  processed/
  models/
  normativas/
  ground_truth/

output/

tests/

config/

main.R
README.md
```

## Separação das camadas

### Aquisição

Responsável exclusivamente por acessar o SEI e armazenar dados brutos.

Tecnologias preferenciais:

* httr2
* rvest
* xml2

RSelenium somente se for comprovadamente necessário devido à dependência de JavaScript.

### Dados estruturais

Responsável por produzir:

* documentos;
* andamentos;
* assinaturas.

### Conteúdo

Responsável por:

* baixar documentos públicos;
* extrair HTML/PDF;
* produzir texto;
* registrar documentos sem texto.

### Text Mining

Responsável por:

* normalização;
* tokenização;
* stopwords;
* extração de datas;
* identificação de termos;
* preparação de atributos;
* TF-IDF.

### Classificação

Possuir dois caminhos:

1. baseline por regras/dicionário;
2. modelo supervisionado simples.

Modelos candidatos:

* Logistic Regression;
* Naive Bayes;
* SVM.

Evitar Deep Learning.

### Process Mining

Construir event log com pelo menos:

* case_id;
* activity;
* timestamp.

Adicionar, quando disponível:

* resource;
* unidade;
* documento;
* resultado.

### Normativas

As regras da IN nº 06/2025 devem estar armazenadas de forma estruturada.

Evitar hardcode.

### Prazos

Entrada:

* evento inicial;
* evento final;
* regra normativa.

Saída:

* prazo normativo;
* dias decorridos;
* excesso;
* dentro do prazo;
* status.

Quando aplicável, considerar a data da última assinatura como encerramento do ato.

### Grafos

Decisão de implementação (validada com o pesquisador): em vez de três grafos
separados, o `10_grafo.R` produz **um único grafo de tramitação** voltado à
validação humana, que reúne as informações necessárias num só artefato:

* nó = documento/ato (tipo, unidade, data, remetente);
* aresta = fluxo entre documentos, rotulada com o destinatário (quem encaminhou
  o quê para quem); duas fontes: pares abertura→parecer dos prazos (semânticos)
  e uma espinha cronológica que liga os atos na ordem do tempo;
* cor/situação: verde (parecer no prazo), vermelho (fora do prazo), lilás
  (complementação), preto (avulso), branco (demais atos);
* documentos avulsos (anexos/arquivos/restritos, sem classe) são ocultados por
  padrão (`incluir_avulsos = FALSE`) para não poluir a leitura.

As marcações finas da seção 8 do FLUXOS (tracejado amarelo/marrom, trapézio
laranja, aba magenta) não são geradas — dependem do motor de conformidade e
ficam como trabalho futuro.

### Persistência

Cada etapa deve persistir seus resultados antes da próxima etapa.

A coleta de rede não deve ser necessária para executar etapas posteriores.

As saídas são organizadas em **pastas por data de execução** (via
`R/utils_saida.R`): `data/processed/AAAAMMDD/` e `output/AAAAMMDD/`. A leitura da
etapa anterior usa `encontrar_arquivo_processo()`, que localiza o artefato mais
recente varrendo as subpastas de data (e a raiz, para arquivos legados). O
gabarito (`data/ground_truth/`) permanece plano, por ser editado manualmente.

### Pipeline

A função final da POC deverá ser conceitualmente:

```r
analisar_processo(url)
```

Ela serve para reproduzir o experimento.

Não representa um produto implantado.
