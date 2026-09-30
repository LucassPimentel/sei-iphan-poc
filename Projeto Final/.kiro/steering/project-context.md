---
inclusion: always
---
# Contexto do Projeto

## Objetivo

Este é um projeto acadêmico de prova de conceito (POC) para a disciplina de Mineração de Dados.

O projeto investiga a possibilidade de automatizar a identificação de pendências e a reconstrução de fluxos processuais em processos de licenciamento ambiental disponíveis no SEI Pesquisa Pública do IPHAN.

O objetivo científico é avaliar a viabilidade da abordagem e comparar os resultados automatizados com um gabarito construído manualmente.

Este projeto NÃO é um produto de produção e NÃO terá etapa de Deployment. O escopo termina na avaliação da POC.

## CRISP-DM

O projeto deve seguir o CRISP-DM:

1. Business Understanding
2. Data Understanding
3. Data Preparation
4. Modeling
5. Evaluation

Deployment está fora do escopo.

## Componentes da solução

A POC será composta por:

* análise exploratória e estatística descritiva;
* web scraping do SEI Pesquisa Pública;
* extração estrutural dos processos;
* extração de documentos públicos;
* extração de texto;
* Text Mining;
* classificação semântica;
* Machine Learning supervisionado simples;
* construção de event log;
* Process Mining;
* aplicação de regras normativas;
* cálculo de prazos;
* identificação de pendências;
* geração de grafos;
* avaliação contra gabarito manual;
* validação com especialista.

## Princípio metodológico

Machine Learning não deve ser utilizado artificialmente apenas para "ter ML".

O ML será utilizado principalmente para a classificação semântica de atos/documentos, quando houver dados rotulados suficientes.

Antes do ML deve existir um baseline baseado em regras linguísticas/dicionário.

Os resultados do baseline e do modelo supervisionado devem ser comparáveis.

## Dados experimentais

O experimento utiliza processos de 2026.

Os processos iniciais foram obtidos por amostragem aleatória simples com semente registrada.

O script existente de amostragem deve ser preservado e incorporado ao projeto.

A amostra é pequena e deve ser tratada como prova de conceito, não como amostra para generalização populacional.

## Gabarito

O mapeamento manual dos processos é o ground truth da avaliação.

O gabarito deve registrar, quando aplicável:

* documentos;
* tipos;
* datas;
* unidades;
* andamentos;
* assinaturas;
* eventos;
* classes semânticas;
* prazos;
* início e fim dos prazos;
* pendências;
* fluxo processual.

O sistema automático deverá ser comparado ao gabarito.

## Fontes

A normativa principal do experimento é a IN IPHAN nº 06/2025.

As regras normativas devem ser armazenadas como dados/configuração e não espalhadas pelo código.

## Restrição de acesso

O sistema utiliza exclusivamente informações disponibilizadas publicamente no SEI Pesquisa Pública.

Documentos restritos não devem ser acessados ou contornados.

Quando um documento possuir restrição, ele deve ser registrado como indisponível, e essa indisponibilidade deve ser mensurável.

## Princípios

Priorizar:

* reprodutibilidade;
* modularidade;
* auditabilidade;
* simplicidade;
* separação de responsabilidades;
* testes;
* rastreabilidade dos resultados;
* clareza científica.

Não criar arquitetura enterprise desnecessária.

Não criar microsserviços, APIs distribuídas, filas ou infraestrutura complexa sem justificativa explícita.

## Linguagem

O projeto será implementado em R.

Pacotes devem ser escolhidos conforme necessidade real.

Preferências:

* tidyverse
* httr2
* rvest
* xml2
* pdftools
* stringr
* stringi
* lubridate
* tidytext
* quanteda
* tidymodels
* textrecipes
* igraph
* ggraph
* visNetwork
* testthat

Não instalar dependências desnecessárias.

## Regra fundamental

Não inventar dados, seletores HTML, classes, regras normativas ou resultados experimentais.

Quando uma informação não puder ser determinada, registrar explicitamente a limitação.
