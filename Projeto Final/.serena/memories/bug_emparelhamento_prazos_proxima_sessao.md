# Bug de emparelhamento de prazos — RESOLVIDO (2026-09-24)

Status: **CORRIGIDO**. Contexto original em `docs/CONTEXTO_PROXIMA_SESSAO.md`.

## O bug (era)
`R/09_prazos.R` atribuía o parecer ao despacho errado (processo
01450.003836/2026-78): o Despacho 3877 (sem `prazo_data_iso`) só pedia a PEÇA
PAIPA, mas o código o emparelhava ao Parecer PAIPA via Critério B
(recontagem_norma) e marcava "estourado" falso; isso "consumia" o parecer e
empurrava o Despacho 5781 (data-limite 23/05, o verdadeiro pedido de parecer)
para o Parecer Técnico 512 seguinte.

## Correção aplicada (Opção A, escolhida pelo usuário)
`R/09_prazos.R`:
- `.emparelhar_relogios()`: só é ABERTURA o despacho de `CLASSES_ABERTURA` que
  TEM data-limite (`prazo_data_iso` não-vazio) — filtro `tem_data_limite`.
  Removido o casamento por peça (peca_abertura era sempre NA). Emparelhamento
  agora = "parecer mais próximo dentro da janela de 90 dias".
- `.apurar_relogio()`: só Critério A (`data_limite_cgm`). Removido o ramo
  Critério B (recontagem_norma) e o descarte por falta de prazo normativo (não
  mais alcançável, pois toda abertura tem data-limite). `prazo_normativo` virou
  só metadado (peca_norma/prazo_normativo_dias).
- Nova 1ª limitação na saída: "Prazos só são apurados quando a unidade fixou a
  data-limite explicitamente (Critério A / FLUXOS sec. 8)...".

`tests/test_09_prazos.R`: teste do Critério B virou "despacho sem data-limite
não abre relógio (0 relógios)" + novo "todo relógio usa critério A"; testes de
janela passaram a usar abertura COM data-limite.

## Validação
- 003836 (após correção): 3 relógios = 2 dentro + 1 estourado.
  5781(limite 23/05)→PAIPA(08/05)=dentro; 8365→Téc512=dentro;
  11794(limite 30/08)→RAIPA(31/08)=estourado legítimo (1 dia). 3877 e 9239 (sem
  data-limite) não abrem relógio. PNG revalidado visualmente.
- Regenerados prazos+grafo dos 5 em data/processed/20260924/ e output/20260924/.
- Suíte: **305 testes, 11 arquivos, 0 falhas** (era 303; +2 do novo teste).

## Ambiente (relembrando)
Rscript em "C:\Program Files\R\R-4.6.1\bin\Rscript.exe" (fora do PATH; usar & no
PowerShell). Warnings no stderr => exit code 1 falso (confirmar por output). O
PowerShell às vezes faz buffering e parece "travar" — não é travamento. OneDrive
reinsere BOM UTF-8 que quebra o parser do R. Serena LSP de R não inicia — usar
edição nativa. Saídas em pastas por data (utils_saida.R;
encontrar_arquivo_processo pega o mais recente).

## Próximas etapas possíveis
- Avaliação contra o gabarito (`11_avaliacao.R`): comparar classe/eventos/datas/
  prazos automáticos com o ground truth em `data/ground_truth/`.
- HTML interativo do grafo (Graphviz/Mermaid).
