# Contexto para a próxima sessão — POC IPHAN

## Objetivo desta próxima sessão

Corrigir um **bug de emparelhamento no cálculo de prazos** (`R/09_prazos.R`) que
faz o grafo de tramitação (`R/10_grafo.R`) atribuir o parecer ao despacho
errado. O bug foi descoberto validando o processo **01450.003836/2026-78**.

> IMPORTANTE (regras do projeto): leia primeiro os steerings em `.kiro/steering/`
> (`project-context.md`, `architecture.md`, `research-methodology.md`,
> `dominio-fluxos.md`) e o `docs/FLUXOS.md`. **Não inventar** dados, regras
> normativas nem resultados; quando algo não puder ser determinado, registrar a
> limitação. Preferência registrada: **propor o plano e aguardar a escolha do
> usuário antes de alterar código** (ver `boas-praticas`).

## Ambiente (relembrando)

- Rscript: `"C:\Program Files\R\R-4.6.1\bin\Rscript.exe"` (NÃO está no PATH;
  invoque com `& "caminho"` no PowerShell).
- Warnings do R no stderr fazem o PowerShell retornar exit code 1 mesmo quando o
  script funcionou — confirme pelo output/arquivos, não pelo exit code.
- Ao gravar arquivos `.R`/`.md` via ferramenta, o OneDrive às vezes reinsere um
  **BOM UTF-8** que quebra o parser do R ("unexpected input" na linha 1). Se um
  teste rodar 0 casos, cheque o BOM e remova-o gravando os bytes sem
  `EF BB BF` (já aconteceu com `tests/test_02_sei_scraping.R`).
- Serena: o language server de R **não inicia** (R fora do PATH). As ferramentas
  de edição do Serena que dependem do LSP falham; use as ferramentas de edição
  nativas (`str_replace`, `fs_write`). Navegação/leitura do Serena funciona.

## Estado atual do pipeline (tudo implementado e testado)

`main.R` encadeia, via `analisar_processo(url)`:
1. `02_sei_scraping.R` — scraping estrutural (documentos, andamentos).
2. `03_parser_documentos.R` — texto dos nativos HTML.
3. `04_entidades.R` — remetente, destinatário, unidade emissora, **prazo**
   (`prazo_data_iso`, `prazo_metodo` = "explicito" | "proximidade").
4. `05_text_mining.R`, `06_classificacao.R` (baseline dicionário) +
   `06b_classificacao_supervisionada.R`.
5. `07_event_log.R` — event log (nós de documento + arestas de tramitação).
6. `08_normativas.R` — lê/valida/consulta `data/normativas/in_06_2025_prazos.json`
   (v1.2). `consultar_prazo()` casa peça/tipo; retorna `peca_norma`,
   `termo_casado`, `prazo_dias`, `e_sinonimo`, `origem_sinonimo`,
   `sem_prazo_normativo`. PAPIPA é peça própria SEM prazo de análise fixado
   (art. 51 não o lista); PAIPA=30d; RAIPA/RAPIPA/RAA e PAA são sinônimos diretos.
7. `09_prazos.R` — **onde está o bug**. `calcular_prazos_processo(event_log,
   normativas, ...)`. Emparelha despacho de abertura → parecer e apura o prazo.
8. `10_grafo.R` — grafo único de tramitação (documento=nó; seta=fluxo rotulada
   com destinatário; verde/vermelho=prazo dentro/fora; lilás=complementação;
   preto=avulso, ocultado por padrão). Render PNG via ggraph.

Saídas em **pastas por data**: `data/processed/AAAAMMDD/` e `output/AAAAMMDD/`
(via `R/utils_saida.R`: `dir_saida_dia`, `encontrar_arquivo_processo`).
`data/ground_truth/` fica plano.

Suíte testthat: **303 testes, 11 arquivos, 0 falhas** (última execução).
Rodar um arquivo: `& "...Rscript.exe" -e "library(testthat); testthat::test_file('tests/test_09_prazos.R')"`.

## O BUG (com evidência real do 01450.003836/2026-78)

Atos classificados desse processo (do event log), em ordem:

| ordem | documento | tipo | classe | data | prazo_data_iso |
|---|---|---|---|---|---|
| 15 | 7259959 | Despacho 3877 | encaminhamento | 25/03 | **(nenhum)** |
| 37 | 7363767 | Despacho 5781 | encaminhamento | 28/04 | **2026-05-23** (explicito) |
| 40 | 7384861 | Parecer PAIPA Arq - IN 06/2025 186 | manifestacao_parecer | 08/05 | — |
| 59 | 7509631 | Despacho 8365 | encaminhamento | 10/06 | 2026-06-23 |
| 62 | 7519156 | Parecer Técnico 512 | manifestacao_parecer | 12/06 | — |
| ... | | | | | |

O que o `09_prazos.R` faz HOJE (errado):
- Empa­relha **Despacho 3877 → Parecer PAIPA** (recontagem_norma) e marca
  `estourado`.
- Como o parecer PAIPA foi "consumido" pelo 3877, o **Despacho 5781** (que tinha
  data-limite real 23/05) é empa­relhado ao Parecer Técnico 512 seguinte.

Por que está errado (explicação do usuário/especialista):
- **Solicitar o PAIPA (a peça) ≠ solicitar o parecer sobre o PAIPA.** São coisas
  diferentes. O Despacho 3877 pediu que o interessado apresentasse o **PAIPA**
  (a peça/projeto); ele NÃO abriu prazo de análise de parecer.
- Quem de fato pediu a **análise/parecer** foi o **Despacho 5781**, que fixou a
  data-limite **23/05**. O Parecer PAIPA (08/05) está **dentro** desse prazo.
- Não precisamos analisar o arquivo do PAIPA (não é nativo). Mas precisamos
  **distinguir** "pedir a peça" de "pedir o parecer/análise".

## Chave da solução (discriminador que JÁ existe nos dados)

O discriminador está no `prazo_data_iso` (extraído em `04_entidades.R`):
- **Despacho que abre prazo de análise** (pede o parecer) → **tem
  `prazo_data_iso`** (a CGM/unidade fixou a data-limite; `prazo_metodo` costuma
  ser "explicito"). Ex.: Despacho 5781 → 23/05.
- **Despacho que só pede/encaminha a peça** → **NÃO tem `prazo_data_iso`**.
  Ex.: Despacho 3877 (sem data-limite).

Hoje o `.emparelhar_relogios()` em `09_prazos.R` trata como abertura **qualquer**
despacho de `CLASSES_ABERTURA` (solicitacao/encaminhamento/exigencia_complementacao)
e, quando não há data-limite, tenta o **Critério B (recontagem_norma)** pela peça
do parecer. É esse fallback que casa o Despacho 3877 (sem data-limite) com o
Parecer PAIPA e gera o "estourado" falso.

### Proposta a levar ao usuário (confirmar antes de implementar)

Opção recomendada — **só abre relógio o despacho que fixou data-limite**:
- Em `.emparelhar_relogios()`, considerar abertura APENAS despachos com
  `prazo_data_iso` presente (Critério A). Sem data-limite explícita, o despacho
  NÃO abre relógio (é pedido de peça/encaminhamento, não de parecer).
- Consequência no 003836: 3877 deixa de emparelhar; 5781 (limite 23/05) passa a
  casar com o Parecer PAIPA (08/05) = **dentro**. Correto.
- Isso essencialmente **remove o Critério B (recontagem_norma)** do
  emparelhamento automático — o que é defensável, porque sem a data-limite
  fixada pela unidade não há como saber que aquele despacho pediu um parecer.
  Registrar como limitação: "prazos só são apurados quando a unidade fixou a
  data-limite explicitamente (Critério A / FLUXOS §8); despachos sem data-limite
  não são tratados como abertura de análise."
- ALTERNATIVA se o usuário quiser manter algum Critério B: só permitir
  recontagem quando a peça do parecer casar com o que o despacho **explicitamente
  cita** (exigiria detectar no texto do despacho a menção ao parecer, o que é o
  motor de conformidade — provavelmente fora de escopo agora).

Confirmar com o usuário: **remover o Critério B (só abertura com data-limite)**
ou outra regra. Depois:
1. Ajustar `.emparelhar_relogios()` / `.apurar_relogio()` em `R/09_prazos.R`.
2. Ajustar/!remover os testes do Critério B em `tests/test_09_prazos.R` (há um
   teste "sem data-limite, reconta pela norma" que passará a não valer — alinhar
   com a nova regra).
3. Regenerar prazos + grafo dos 5 processos nas pastas de data e revalidar o PNG
   do 003836 (o 3877 não deve mais aparecer como estourado; o 5781→PAIPA dentro).
4. Rodar a suíte completa (deve ficar 0 falhas).

## Detalhes de emparelhamento atual (para orientar a edição)

`R/09_prazos.R`:
- `CLASSES_ABERTURA <- c("solicitacao","encaminhamento","exigencia_complementacao")`
- `CLASSES_PARECER  <- c("manifestacao_parecer","decisao")`
- `JANELA_PAREAMENTO_DIAS <- 90` (janela para casar por proximidade).
- `.emparelhar_relogios(log, normativas)`: para cada abertura, escolhe o parecer
  (não consumido, data >= abertura): (1) por peça compatível; (2) senão o mais
  próximo dentro da janela; (3) senão não emparelha (em_curso). Um parecer serve
  a um só relógio.
- `.apurar_relogio(abertura, parecer, normativas)`: DESCARTA (NULL) se não há
  data-limite E não há prazo normativo. Critério A (data_limite_cgm) se há
  `prazo_data_iso`; senão Critério B (recontagem_norma). Status:
  dentro/estourado/em_curso/indeterminado.

A mudança central provável: no filtro de aberturas de `.emparelhar_relogios()`,
exigir `!is.na(prazo_data_iso) & prazo_data_iso != ""`; e simplificar
`.apurar_relogio()` para o Critério A (removendo o ramo B), mantendo a
compatibilidade com `exigencia_complementacao` (que também traz data-limite).

## Como o grafo consome os prazos (para revalidar)

`10_grafo.R` usa os pares `documento_abertura → documento_parecer` (arestas de
"prazo") e a situação (`status`) para colorir o nó do parecer
(verde=dentro/vermelho=estourado). Corrigido o 09, o grafo herda a correção.
Validar visualmente o PNG do 003836 em `output/AAAAMMDD/`.

## Arquivos-chave

- `R/09_prazos.R` (bug), `R/08_normativas.R`, `R/10_grafo.R`, `R/07_event_log.R`,
  `R/04_entidades.R` (origem do `prazo_data_iso`), `R/utils_saida.R`.
- `data/normativas/in_06_2025_prazos.json` (v1.2).
- Testes: `tests/test_09_prazos.R`, `tests/test_10_grafo.R`.
- Processo de teste do bug: **01450.003836/2026-78** (event log e prazos mais
  recentes em `data/processed/AAAAMMDD/`).

## Depois do bug (etapas seguintes possíveis)

- Avaliação contra o gabarito (`11_avaliacao.R`) — comparar classe/eventos/datas/
  prazos automáticos com o ground truth em `data/ground_truth/`.
- HTML interativo do grafo (Graphviz/Mermaid), como detalhe posterior.
