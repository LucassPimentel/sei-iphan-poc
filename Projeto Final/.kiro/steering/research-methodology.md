---
inclusion: always
---
# Metodologia da Pesquisa

## Natureza

Projeto aplicado e exploratório, estruturado como prova de conceito.

O objetivo é verificar se o método consegue reconstruir automaticamente informações processuais que também podem ser obtidas por mapeamento manual.

Não buscar generalização populacional a partir dos cinco processos.

## CRISP-DM

### Business Understanding

Problema:

identificação manual de pendências, atrasos e fluxo processual em processos do SEI/IPHAN.

### Data Understanding

Realizar análise estatística descritiva dos processos coletados.

### Data Preparation

Executar:

* scraping;
* limpeza;
* normalização;
* extração de conteúdo;
* tokenização;
* construção das estruturas de dados.

### Modeling

Existem duas formas principais de modelagem:

#### Modelagem textual

Baseline:

regras linguísticas + dicionário.

Modelo:

TF-IDF + classificador supervisionado simples.

#### Modelagem processual

Construção do event log e descoberta/representação do fluxo processual.

Também devem ser aplicadas regras normativas para análise de conformidade.

### Evaluation

Comparar os resultados automáticos com o ground truth.

Avaliar separadamente:

* cobertura estrutural;
* cobertura semântica;
* classificação;
* eventos;
* datas;
* prazos;
* pendências;
* fluxo processual.

Métricas possíveis:

* precision;
* recall;
* F1;
* accuracy;
* matriz de confusão;
* cobertura;
* acerto de eventos;
* acerto de datas.

Quando apropriado, considerar validação por processo para evitar vazamento de informação entre treino e teste.

## Especialista

A validação com técnico/analista será utilizada como validação qualitativa complementar.

A opinião do especialista não deve ser misturada silenciosamente com o ground truth original.

## Limitações

Sempre registrar:

* documentos restritos;
* documentos sem texto;
* necessidade potencial de OCR;
* tamanho reduzido da amostra;
* desbalanceamento de classes;
* limitações de acesso ao SEI;
* limitações de generalização.

## Regra científica

Não ajustar o modelo sobre os dados de avaliação apenas para melhorar a métrica.

Não utilizar dados de teste como treinamento sem registrar a alteração.

Não apresentar métricas sem identificar sobre qual conjunto foram calculadas.
