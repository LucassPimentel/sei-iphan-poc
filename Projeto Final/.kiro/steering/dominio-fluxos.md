---
inclusion: always
---
# Domínio: fluxos do licenciamento (IN IPHAN 06/2025)

Conhecimento de domínio fornecido pelo especialista, consolidado a partir de
`FLUXOS.md` (na raiz do projeto). Este steering resume as regras que afetam a
implementação; a fonte completa e autoritativa é o próprio `FLUXOS.md`.

> Regra fundamental do projeto: **não inventar** regras normativas. Quando um
> caso não constar de `FLUXOS.md` nem da IN 06/2025, registrar a limitação em
> vez de supor.

## Fonte e forma dos dados

- Normativa principal: **IN IPHAN nº 06/2025**.
- Especificação de fluxos e marcações do grafo: **`FLUXOS.md`** (raiz).
- Prazos normativos estruturados como dado: **`data/normativas/in_06_2025_prazos.json`**
  (transcrito da seção 7 de `FLUXOS.md`). A etapa de prazos deve LER esse dado,
  não hardcodar prazos no código.

## Contagem de prazos (regra que atravessa tudo)

- Prazos em **dias consecutivos (corridos)**, não dias úteis (art. 54).
- Encerramento do ato: usar a **data da última assinatura** do parecer,
  confrontada com a data-limite fixada pela CGM.
- Os dois relógios da complementação **não se somam**: o tempo de resposta do
  interessado (30 dias, art. 52, §1º) não conta como atraso do órgão
  (15 dias, art. 51, VIII). Mantê-los separados na apuração.

## Despacho da CGM (triagem) — abre a demanda

É a regra central do fluxo. O despacho da unidade de triagem (CGM) declara no
**assunto** qual instrumento será analisado, e isso define qual parecer se
espera:

- "análise manual da FCA" → Parecer FCA Arq + FCA Mat + FCA Imat
- "análise automática" → nenhum parecer; notificar interessado por e-mail
- PAIPA/PAPIPA/RAIPA/RAIPM/PGBIR... → parecer correspondente

Na **análise automática**, os termos que caracterizam o TRE (nível, termo de
compromisso, componente) vêm no **corpo** do despacho da triagem (em negrito),
não no assunto. Situação de **passivo**: a CGM recebe demanda já vencida e
informa no assunto a data em que o prazo expirou.

## Instrumentos por nível do empreendimento (arts. 18 e Anexo I)

| Nível | Instrumento | Pareceres |
|---|---|---|
| I | TCE | — |
| II | Proj. Acompanhamento Arqueológico | PAA Proj/Acomp/Arq; RAA Arq |
| III | PAIPA | PAIPA Arq; RAIPA Arq |
| IV | PAPIPA | PAPIPA Arq |

Componentes culturais: **Material** (FCA Mat, RAIPM Mat), **Imaterial** (FCA
Imat, PGBIR/RAIBIR/RPGBIR Imat), **Arqueológico** (FCA Arq, PAIPA/PAPIPA/RAIPA).
O componente de um parecer de rótulo livre é identificado pelos termos
"Patrimônio Material/Imaterial/Arqueológico" e o nível pela expressão
"Nível I" a "Nível IV" no corpo do ato.

TCE dispensa famílias inteiras: TCE-Bens Arqueológicos dispensa PAA/PAIPA/
PAPIPA/RAA/RAIPA; TCE-Bens Registrados dispensa PGBIR/RAIBIR/RPGBIR.

## Distinção importante para a classificação

- **Portaria no DOU** (art. 51, §1º) **≠ manifestação conclusiva** (art. 58):
  eventos distintos, não confundir no modelo. Ambos são atos de "decisão".
- **Complementação única**: a solicitação deve ser feita uma vez (art. 51, §3º
  / 52) e reiterada no máximo uma vez (art. 52, §2º); não atendida a
  reiteração, pode haver arquivamento (art. 52, §3º). Uma segunda violação da
  regra da complementação única é detectável por contagem de peças do mesmo
  instrumento.

## Marcações do grafo (seção 8 de FLUXOS.md) — para a etapa de grafo

- Octógono verde/vermelho: manifestação dentro/fora do prazo.
- Tracejado amarelo: parecer demandado que não consta dos autos.
- Tracejado marrom: documento esperado ainda não juntado.
- Trapézio laranja: fluxo abortado (destinatário não assinou; outro despacho
  alcançou quem assinou).
- Casa lilás: complementações solicitadas (cita o parecer + termos de
  complementação/solicitação/esclarecimento no corpo).
- Aba magenta: assinaturas pendentes ("De acordo" entre o fecho e o rodapé).
- Preenchimento preto: documento avulso (não recebe nem dirige seta).

## Unidades (seção 9)

- **CGLic** → CAIP, CGM, CAIP-CGM, DAP, DIVGEO, CORA, DINO.
- **CNA** → COIR, CGINF, COP.
- Superintendências usam sufixo de UF: `IPHAN-UF` ou `PGLic-UF`.
- **DIVGEO** cuida do cadastro na Base de Dados Georreferenciada (DBGEO).
- Repasse mãe→filha é confirmado no histórico de andamentos pelo registro
  "Processo remetido pela unidade &lt;mãe&gt;".

## Limitações a registrar sempre

- A fase anterior à abertura no SEI ocorre no SAIP e não aparece nos autos: o
  prazo de análise da FCA só é mensurável com ficha e parecer juntados.
- Só documentos nativos em HTML são lidos; anexos (PDF/planilha/zip) não têm
  conteúdo extraído.
- Processos em curso têm traços incompletos (atividades terminais podem faltar;
  ciclo total não mensurável).

## Onde cada regra entra no código

- **Classificação (`06`)**: distinção decisão/complementação/manifestação;
  vocabulário de domínio nos gatilhos (`config/classes_semanticas.json`).
- **Normativas/prazos (futuros `08`/`09`)**: `data/normativas/in_06_2025_prazos.json`.
- **Grafo (futuro `10`)**: marcações da seção 8, unidades da seção 9.
- **Entidades (`04`)**: destinatário/remetente/unidade/prazo já extraídos
  alimentam a análise de conformidade.
