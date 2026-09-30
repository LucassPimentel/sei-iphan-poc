# Fluxos do licenciamento ambiental no IPHAN

Documenta os fluxos que o gerador de grafos reconhece, com a base
normativa de cada prazo na Instrução Normativa IPHAN nº 06/2025. Serve a
dois propósitos: registrar as regras que o código implementa e permitir
que o resultado automático seja conferido contra a norma.

Todos os prazos são contados em **dias consecutivos** (art. 54), e não em
dias úteis.

---

## 1. Visão geral

O processo administrativo no SEI começa depois da emissão do Termo de
Referência Específico. O pedido entra pelo Sistema de Avaliação de
Impacto ao Patrimônio (SAIP), com a Ficha de Caracterização da Atividade;
emitido o TRE, abre-se o processo no SEI e ele é distribuído à unidade
responsável (art. 8º, §1º). A fase anterior à abertura, portanto, não
aparece nos autos — limitação a considerar ao medir o prazo de análise da
FCA.

```
SAIP (FCA)  →  TRE  →  [abertura do processo no SEI]
                          │
                          ▼
              documentação externa protocolada
                          │
                          ▼
               despacho da CGM abre a demanda
                          │
                          ▼
                  parecer do componente
                          │
          ┌───────────────┴───────────────┐
      aprovado                     não aprovado
          │                               │
          ▼                               ▼
   portaria / anuência          complementações solicitadas
          │                               │
          ▼                               ▼
    RAIPA protocolado            nova documentação externa
          │                               │
          ▼                               └──→ (volta à análise)
  PGPA ou finalização
```

---

## 2. O despacho da unidade de triagem abre a demanda

É a regra central. O despacho da CGM declara, no assunto, qual
instrumento será analisado, e é ele que define qual parecer se espera.

| O assunto informa | Demanda aberta |
|---|---|
| análise manual da FCA | Parecer FCA Arq, Parecer FCA Mat e Parecer FCA Imat |
| análise automática | nenhum parecer: notificar o interessado por e-mail |
| PAIPA | Parecer PAIPA Arq |
| PAPIPA | Parecer PAPIPA Arq |
| RAIPA | Parecer RAIPA Arq |
| RAIPM | Parecer RAIPM Mat |
| PGBIR, RAIBIR, RPGBIR | parecer imaterial correspondente |

Quando o parecer demandado consta dos autos, a seta o alcança ainda que
não haja citação por número. Quando não consta, ele entra no diagrama
como nó tracejado amarelo de documentação pendente, com a data em que o
prazo expirou.

Há ainda a situação de **passivo**, em que a CGM recebe a demanda já
vencida vinda da superintendência e informa, no assunto do despacho, a
data em que o prazo expirou.

---

## 3. Fluxo da Ficha de Caracterização da Atividade

A manifestação do IPHAN tem por base a FCA disponibilizada no SAIP
(art. 7º). Da análise resulta o TRE, que indica os estudos exigidos
(art. 9º) e vale por **dois anos**, prorrogáveis mediante solicitação
(art. 14).

A FCA é analisada nos três componentes culturais, cada um com seu
parecer. Quando o documento nativo tem rótulo livre — "Parecer - Ficha de
Caracterização de Atividade FCA" —, o componente é identificado pelos
termos "Patrimônio Material", "Patrimônio Imaterial" ou "Patrimônio
Arqueológico" no corpo do ato.

**Prazo:** 15 dias para FCAs e TCEs (art. 51, I). O mesmo prazo vale
quando o termo de compromisso do empreendedor é protocolado: o despacho
seguinte da triagem abre a demanda de analisá-lo, contada do protocolo.

### O que o Termo de Referência exige

Os itens assinalados no TRE definem os pareceres esperados e, quando são
termo de compromisso, dispensam uma família inteira:

| Item assinalado | Consequência |
|---|---|
| TCE — Bens Registrados | dispensa os pareceres PGBIR, RAIBIR e RPGBIR Imat |
| TCE — Bens Arqueológicos | dispensa os pareceres PAA, PAIPA, PAPIPA, RAA e RAIPA Arq |
| Ambos os TCE | o processo caminha para finalização ao protocolo da documentação |
| Estudos aos Bens Imateriais Registrados | demanda RAIBIR Imat, podendo ramificar para PGBIR e RPGBIR |
| Estudos ao Patrimônio Material | demanda RAIPM Mat |
| Estudos aos Bens Arqueológicos — Nível II | demanda PAA Proj/Acomp/Arq e RAA Arq |
| Estudos aos Bens Arqueológicos — Nível III | demanda PAIPA Arq e RAIPA Arq |
| Estudos aos Bens Arqueológicos — Nível IV | demanda PAPIPA Arq |

Pareceres de outra nomenclatura presentes nos autos substituem os de
rótulo canônico; parecer técnico e nota técnica substituem os de
patrimônio material e imaterial. Essas demandas só valem quando a
triagem encaminhou a ficha para análise manual.

Na análise automática o termo de referência vem entre os documentos
iniciais, emitido pelo sistema de avaliação. Não sendo documento nativo,
os termos que o caracterizam — nível, termo de compromisso, componente
cultural — são buscados no corpo do despacho da triagem, onde costumam
vir destacados em negrito, e não no assunto, que muitas vezes nada
informa sobre o resultado.

---

## 4. Níveis do empreendimento e instrumentos arqueológicos

O nível define o instrumento exigido (arts. 18 e Anexo I):

| Nível | Interferência | Instrumento | Parecer correspondente |
|---|---|---|---|
| I | baixa, sem sítio cadastrado | TCE | — |
| II | baixa e média | Projeto de Acompanhamento Arqueológico | Parecer PAA Proj/Acomp/Arq; Parecer RAA Arq |
| III | média e alta, área definida | PAIPA | Parecer PAIPA Arq; Parecer RAIPA Arq |
| IV | média e alta, traçado indefinido até a LP | PAPIPA | Parecer PAPIPA Arq |

Constatada a existência de sítio arqueológico, terra indígena ou
território quilombola na ADA ou na AID de empreendimento classificado
como Nível I ou II, a análise pode prever a alteração de nível.

### Componente material

Parecer FCA Mat e Parecer RAIPM Mat. O RAIPM é exigido quando há bens
tombados, valorados, chancelados ou declarados tombados na ADA ou na AID
(art. 9º, §4º).

### Componente imaterial

Parecer FCA Imat, Parecer PGBIR Imat, Parecer RAIBIR Imat e Parecer
RPGBIR Imat. O RAIBIR é exigido quando a AID se sobrepõe à Área de
Ocorrência do Bem Imaterial Registrado (art. 9º, §3º).

### Pareceres de rótulo livre

Alguns atos não seguem a nomenclatura canônica — Parecer - Potencial de
Impacto Arqueológico, Parecer - Programa de Gestão do Patr Arqueológico,
Parecer - Projeto de Avaliação de Impacto Arqueol, Parecer - Projeto de
Salvamento Arqueológico, Parecer - Proposta de Acompanhamento
Arqueológico, Parecer - Relatório de Pesquisa Arqueológica e Parecer
Análise Cadastro de Sítios Arqueológicos. Todos pertencem ao componente
arqueológico, e o nível é buscado no corpo do ato, pela expressão "Nível
I" a "Nível IV".

---

## 5. Fluxo da complementação

A análise pode deferir, solicitar complementações ou indeferir (art. 51).
A solicitação de complementação **deve abordar todos os aspectos de uma
vez e ser feita uma única vez** (art. 51, §3º, e art. 52). Não atendida,
pode ser reiterada uma única vez (art. 52, §2º); não atendida a
reiteração, o processo pode ser arquivado, com informação ao órgão
licenciador (art. 52, §3º). O arquivamento não impede novo requerimento,
desde que não haja alteração projetual e não tenham transcorrido dois
anos (art. 52, §4º).

| Relógio | Prazo | Base |
|---|---|---|
| Administração analisa a complementação | 15 dias, prorrogáveis por igual período | art. 51, VIII |
| Interessado apresenta a complementação | 30 dias | art. 52, §1º |

Os dois relógios não se somam: o tempo que o interessado leva para
responder não conta como atraso do órgão, e a apuração os mantém
separados.

Uma segunda violação da regra da complementação única é detectável por
simples contagem de peças do mesmo instrumento.

---

## 6. Fluxo da portaria e do relatório

Deferido o projeto de pesquisa arqueológica, o IPHAN publica **portaria
no Diário Oficial da União** autorizando sua execução (art. 51, §1º).
Registre-se que a portaria **não equivale à manifestação conclusiva** para
fins de licença ambiental (art. 58): são eventos distintos e não devem
ser confundidos no modelo.

Publicada a portaria, a próxima peça esperada do interessado é o
relatório de avaliação de impacto — RAIPA no Nível III, RAPIPA no Nível
IV, Relatório de Acompanhamento no Nível II — ou, alternativamente, o
pedido de revalidação do termo de referência.

Aprovado o relatório, o processo segue para o Programa de Gestão do
Patrimônio Arqueológico, exigível nos Níveis III e IV (art. 33, §1º), ou
para a finalização. Não aprovado, retorna ao fluxo de complementação.

**Prazo:** 30 dias para relatórios de avaliação de impacto, de
acompanhamento arqueológico e de programas de gestão, prorrogáveis por
igual período mediante decisão motivada (art. 51, VII).

---

## 7. Prazos da IN 06/2025

### Análise de peças (art. 51)

| Peça | Prazo |
|---|---|
| FCAs e TCEs | 15 dias |
| PAIPAs, Projetos de Acompanhamento Arqueológico, PGBIR e PGPM | 30 dias, prorrogáveis |
| Inclusão de Projeto de Salvamento ou de Preservação in situ (Nível II) | 15 dias |
| Inclusão de projeto de salvamento durante preservação in situ | 15 dias |
| Inclusão de projeto de preservação in situ durante salvamento | 15 dias |
| Substituição do arqueólogo coordenador de campo | 15 dias |
| Relatórios de avaliação de impacto, de acompanhamento e de gestão | 30 dias, prorrogáveis |
| Dados complementares | 15 dias, prorrogáveis |

### Manifestação conclusiva

| Fase | Prazo | Base |
|---|---|---|
| Viabilidade locacional, com EIA/RIMA | 90 dias | art. 62 |
| Viabilidade locacional, demais casos | 30 dias | art. 62 |
| Instalação | 60 dias | art. 65 |
| Operação | 60 dias | art. 68 |

### Outros prazos

| Situação | Prazo | Base |
|---|---|---|
| Manifestação após comunicação de achado arqueológico | 15 dias | arts. 21, 35 e 38, p.ú. |
| Substituição de arqueólogo coordenador: manifestação e publicação | 15 dias | art. 47, §3º |
| Relatório do arqueólogo que se desliga | 30 dias | art. 47, §2º |
| Validade do TRE | 2 anos | art. 14 |

---

## 8. Como o gerador marca cada situação

| Marcação no diagrama | O que significa | Como é detectada |
|---|---|---|
| Octógono verde ou vermelho | manifestação dentro ou fora do prazo | data-limite fixada pela CGM confrontada com a data da última assinatura do parecer |
| Tracejado amarelo | parecer demandado que não consta dos autos | demanda declarada no assunto do despacho da CGM sem parecer correspondente |
| Tracejado marrom | documento esperado, ainda não juntado | encaminhamento a unidade que recebeu o processo e nada emitiu; peça seguinte após a portaria |
| Trapézio laranja | fluxo abortado | despacho cujo destinatário não assinou o parecer, havendo outro despacho que alcançou quem assinou |
| Casa lilás | complementações solicitadas | ato que cita o parecer e traz, no corpo, termos de complementação, solicitação ou esclarecimento |
| Aba magenta | assinaturas pendentes | "De acordo" na área de assinatura, entre o fecho e o rodapé eletrônico |
| Preenchimento preto | documento avulso | ato que não recebe nem dirige seta alguma do fluxo |

---

## 9. Unidades

A distribuição é feita pela Sede Nacional (art. 4º, p.ú.). Em
licenciamento federal, ou quando o empreendimento envolve mais de um
estado, a decisão cabe à Sede (art. 5º); em licenciamento estadual,
distrital ou municipal, à Superintendência onde se localiza o
empreendimento (art. 6º).

As unidades vinculadas conhecidas pelo gerador:

- **CGLic** — CAIP, CGM, CAIP-CGM, DAP, DIVGEO, CORA, DINO
- **CNA** — COIR, CGINF, COP

Unidades de superintendência trazem o sufixo da UF, tanto no formato
`IPHAN-UF` quanto `PGLic-UF`. A DIVGEO é a unidade responsável pelo
cadastro na Base de Dados Georreferenciada (DBGEO).

Um ato dirigido à unidade-mãe pode ser respondido por unidade vinculada;
o histórico de andamentos confirma o repasse caso a caso, pelo registro
"Processo remetido pela unidade &lt;mãe&gt;" na unidade filha.

---

## 10. Limitações conhecidas

A fase anterior à abertura do processo no SEI ocorre no SAIP e não
aparece nos autos, de modo que o prazo de análise da FCA só é mensurável
quando a ficha e o parecer correspondente estão juntados.

Somente documentos nativos em HTML são lidos. Anexos em PDF, planilhas e
arquivos compactados aparecem na lista de documentos e são referenciados
pelos atos nativos, mas seu conteúdo não é extraído.

Processos em curso têm traços incompletos: as atividades terminais podem
não estar representadas, e o tempo total de ciclo não é mensurável nesses
casos.
