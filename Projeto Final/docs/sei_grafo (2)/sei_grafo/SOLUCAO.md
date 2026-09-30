# A solução: do link do processo ao diagrama de fluxo

Documenta o funcionamento do gerador de grafos processuais tal como ele
está hoje — o que entra, o que cada estágio produz, quais regras decidem
o desenho — e, ao final, como esse conjunto se encaixa nas fases do
CRISP-DM adotado na metodologia do projeto.

Complementa dois outros documentos: o `README.md`, que é o manual de uso,
e o `FLUXOS.md`, que descreve os fluxos administrativos e os prazos da
Instrução Normativa IPHAN nº 06/2025.

---

## 1. Visão geral

O pipeline vai do endereço de exibição do processo a um conjunto de
artefatos: o diagrama do fluxo, o quadro dos prazos apurados, o quadro
das demandas em aberto e as tabelas de nós e arestas. Está organizado em
cinco estágios encadeados, cada um gravando seu resultado em disco antes
de alimentar o seguinte.

```
link do processo
      │
      ▼
[1] aquisição ──────────► acervo em disco + manifesto de proveniência
      │
      ▼
[2] leitura determinística ──► documentos, andamentos, assinaturas
      │
      ▼
[3] enriquecimento ──────► assunto, destinatários, exigências, componente
      │
      ▼
[4] grafo ───────────────► nós, arestas, marcações, prazos
      │
      ▼
[5] saídas ──────────────► HTML, PNG, SVG, Mermaid, CSV
```

Só o primeiro estágio depende de rede. Os demais rodam sobre material
congelado, o que permite iterar nas regras sem repetir o download e, mais
importante, garante que a conferência manual e a execução automática
incidam sobre exatamente o mesmo conteúdo.

---

## 2. Estágio 1 — aquisição

**Entrada:** o endereço de exibição do processo na Pesquisa Pública.

O parâmetro desse endereço é cifrado, mas não depende de sessão: uma
requisição simples recupera a página a qualquer momento. O código de
confirmação é exigido apenas na pesquisa por número de protocolo — a
etapa de descobrir o link —, e não na leitura de um link já conhecido.

Da página do processo saem os endereços de cada documento, que vêm em
atributo `onclick`, em caminho relativo, e são resolvidos contra o
endereço do processo. Cada documento é então baixado individualmente.

**Filtro de conteúdo.** A lista de documentos traz, além dos atos,
anexos em PDF, imagem e planilha. O coletor verifica o tipo de conteúdo
da resposta e só grava o que for HTML. Os demais permanecem na lista como
documentação externa, sem inteiro teor lido. Sem essa verificação, o
leitor tentaria interpretar bytes binários como texto.

**Saída:** o acervo em disco, com a página do processo, um diretório por
documento nativo e um manifesto com resumo criptográfico e carimbo de
tempo de cada arquivo.

---

## 3. Estágio 2 — leitura determinística

Não envolve mineração de texto. Produz três tabelas a partir da página do
processo.

**Lista de documentos** — número, tipo, data, unidade e indicação de
restrição. Três cuidados: o tipo canônico vem do atributo de título da
âncora, e não do texto visível da célula, que traz um número sequencial
concatenado e multiplicaria o vocabulário; documento sem âncora é
restrito, e sua ausência é o indicador de cobertura; e a ordenação é
recomposta explicitamente, sem confiar na ordem de exibição, com
desempate pelo número sequencial e pela hora da assinatura.

**Histórico de andamentos** — instante, unidade e rótulo normalizado, com
a unidade de origem isolada, de modo que "Processo remetido pela unidade
X" registrado na unidade Y signifique a transferência X → Y.

**Assinaturas** — signatário, cargo e instante de cada uma, extraídos do
rodapé de assinatura eletrônica dos documentos nativos.

### O que é ato e o que é documentação externa

A distinção governa todo o resto e tem dois caminhos:

- É ato o **documento nativo do SEI**, reconhecido pelos marcadores que
  só ele tem: rodapé de assinatura eletrônica, selo de autenticidade ou
  cabeçalho de e-mail.
- É também ato o documento de **tipo reconhecido** ainda que sem inteiro
  teor disponível: despacho, ofício, parecer, nota técnica, portaria,
  termo de referência e e-mail.

Todo o resto é documentação externa. Ela é agrupada em sequências que
começam no recibo de peticionamento, pertencem a uma só unidade e se
encerram quando outra unidade registra ato nativo. Cada sequência vira um
nó único no diagrama, e a citação de qualquer peça agrupada faz a seta
partir desse nó.

O critério anterior — uma lista fixa de tipos considerados externos —
falhava com anexos de tipo não previsto, que apareciam como atos no
fluxo. O critério atual não depende de lista: pergunta se o documento é
nativo, não que nome ele tem.

---

## 4. Estágio 3 — enriquecimento

De cada documento nativo saem os elementos que alimentam o desenho.

| Elemento | O que é | Onde aparece |
|---|---|---|
| Destinatários | blocos de nome e vínculo, com os endereços eletrônicos declarados | rótulo da seta ou nó próprio |
| Assunto | rótulo "Assunto:" ou "Ass.:"; na ausência, o corpo da correspondência | linha sem seta |
| Referências | números "Processo / Documento" citados, e menções ao parecer pela numeração própria | direção das setas |
| Assinaturas | nome, cargo e instante de cada signatário | interior do nó |
| Assinaturas esperadas | "De acordo" na área de assinatura, entre o fecho e o rodapé | marcação de pendência |
| Prazo informado | data-limite de manifestação fixada no ato | linha sem seta e apuração |
| Exigências | termos de complementação, solicitação e esclarecimento | marcação lilás |
| Componente cultural | Patrimônio Arqueológico, Material ou Imaterial declarado no corpo | nomenclatura canônica do parecer |
| Marcações de tabela | item marcado com X; opção assinalada em coluna | linha sem seta |

Dois pontos merecem registro, porque foram onde a leitura ingênua
falhou.

**O "De acordo" só conta na área de assinatura.** No corpo do texto a
expressão é conectivo — "de acordo com a norma", "com o qual estou de
acordo" — e contá-la ali produzia pendências inexistentes. A área de
assinatura vai do fecho da correspondência ao rodapé eletrônico.

**Os termos de exigência precisam ser as formas, não as raízes.** Buscar
por `complement`, `solicit` e `recomend` marcava praticamente todo
parecer, porque "Recomenda aprovação" é rótulo de tabela e "solicitação
em epígrafe" é menção a pedido alheio. As formas que de fato
caracterizam a exigência são complementação, complementações,
complementos, solicita-se, solicito, solicitamos e esclarecimentos.

---

## 5. Estágio 4 — construção do grafo

### Nós

Atos do processo, sequências de documentação externa e nós auxiliares que
não pertencem ao fluxo, mas informam sobre ele.

### Arestas, em ordem de precedência

1. **Citação** — o documento cita outro pelo número; a seta vai do citado
   para quem cita. Traço fino cinza.
2. **Endereçamento** — o destinatário de A assina o ato B. Procura
   primeiro adiante no tempo e, não havendo resposta adiante, aceita ato
   anterior do mesmo ciclo, situação que ocorre quando o parecer tem data
   nominal anterior à do despacho que o pede. Traço cheio preto.
3. **Demanda** — o despacho da unidade de triagem declara no assunto o
   instrumento a analisar, e a seta alcança o parecer correspondente
   ainda que não haja citação por número.
4. **Ligação por endereço eletrônico** — o ofício declara os e-mails dos
   destinatários e o e-mail dos autos foi enviado a eles.
5. **Protocolo externo** — a sequência protocolada liga-se ao primeiro
   ato que a cita ou, não havendo citação, ao primeiro ato do ciclo que
   ninguém cita.
6. **Complementação atendida** — o parecer que exige complementação
   liga-se à entrada de documentação seguinte: é o interessado que leu a
   exigência e protocolou o que foi requerido.
7. **Encadeamento temporal** — fecha a cadeia para o que restou sem
   entrada.

Ciclos de duas arestas são desfeitos mantendo o elo que parte do ato mais
recente: é ele que pede, e o outro é a peça pedida.

### Rótulos das setas

Rotula-se a seta cujo destinatário assina o ato de destino, qualquer que
tenha sido a regra que a criou — a citação explica a ligação, mas não
quem a endereçou. Três ajustes:

- quem assina em substituição responde pelo pedido dirigido à pessoa
  substituída, desde que seja o ato imediatamente seguinte;
- o nome abreviado casa com o completo quando o primeiro e o último nome
  coincidem;
- o destinatário é substituído pelo assunto quando não traz nome de
  pessoa, por ser unidade, ou quando coincide com quem assina o próprio
  documento, sinal de anuência interna.

**Destinatário como nó.** Quando o ato se dirige a alguém e essa pessoa
assina dois ou mais atos, repetir o mesmo rótulo em cada seta esconderia
que se trata de um único encaminhamento. O destinatário vira nó: o ato
demandante chega a ele por linha sem seta, e dele partem as setas.

### Marcações

Cada marcação é um nó que aponta para o ato, com cor própria. São as
mesmas que alimentam o quadro de demandas.

| Marcação | Quando aparece |
|---|---|
| Situação do prazo | data-limite fixada pela triagem confrontada com a assinatura do parecer |
| Parecer faltante | demanda declarada no despacho sem parecer correspondente |
| Documento esperado | unidade que recebeu o processo e nada emitiu; peça seguinte após a portaria |
| Ciência da unidade | unidade que recebeu e devolveu o processo: ramo encerrado, sem pendência |
| Fluxo abortado | despacho cujo destinatário não assinou o parecer, havendo outro que alcançou quem assinou |
| Complementações solicitadas | ato que exige complementação a partir de um parecer |
| Assinatura pendente | "De acordo" na área de assinatura sem a anuência correspondente |
| Notificação pendente | ofício com e-mail declarado e sem e-mail enviado nos autos |
| Parecer sem encaminhamento | parecer sem ato do destinatário e sem citação |
| E-mail anexado | e-mail não nativo, sem conteúdo legível, à margem do fluxo |
| Documento avulso | ato que não recebe nem dirige seta alguma |

### Prazos

O relógio é aberto pelo ato que fixa a data-limite, contado da
documentação externa protocolada que o antecede e encerrado na assinatura
do parecer — e não na data nominal do ato, porque o ato administrativo se
aperfeiçoa com a assinatura. A contagem é em dias consecutivos. O relógio
do interessado é mantido separado do relógio da administração.

---

## 6. Estágio 5 — saídas

- **`grafo.html`** — página autocontida com o diagrama embutido como SVG,
  controles de zoom e arrasto, quadro de prazos com a demanda que cada um
  cobre, quadro das demandas em aberto e legenda gerada a partir da mesma
  tabela de cores que desenha o grafo.
- **`grafo.png`**, **`grafo.svg`**, **`grafo.gv`** — o diagrama em
  formatos de imagem e a fonte Graphviz.
- **`grafo.mmd`** — o mesmo diagrama em Mermaid, para conferência lado a
  lado com o desenho manual.
- **`prazos.csv`**, **`demandas.csv`**, **`nos.csv`**, **`arestas.csv`** —
  as tabelas que sustentam as métricas.

---

## 7. Encaixe no CRISP-DM

A metodologia do projeto adota o CRISP-DM adaptado a dados textuais, com
abordagem estatística explícita. A solução descrita acima não cobre o
ciclo inteiro: ela realiza integralmente três fases, parcialmente uma
quarta e prepara o terreno para as duas restantes.

### Entendimento do negócio

Realizada fora do código, alimenta-o por dois artefatos. O primeiro é o
`FLUXOS.md`, que traduz a Instrução Normativa em fluxos, instrumentos,
níveis de empreendimento e prazos — é dele que saem as tabelas
`PARECERES`, `DEMANDAS` e os prazos da apuração. O segundo é o glossário
de eventos processuais, que define o que conta como demanda, como
exigência e como encerramento.

O que o código faz é **tornar essas definições executáveis**: cada regra
de negócio está numa tabela ou numa função nomeada, e não dispersa em
condicionais, de modo que a norma possa ser conferida contra a
implementação linha a linha.

### Entendimento dos dados

Realizada. Os estágios 1 e 2 caracterizam as fontes e medem o que o
projeto precisava saber antes de qualquer modelagem: quantos documentos
existem, quantos são nativos, quantos são restritos, que unidades
participam, qual a cobertura de inteiro teor. As tabelas `nos.csv` e
`arestas.csv` são justamente o material dessa caracterização.

Duas descobertas desta fase reorganizaram o desenho e estão incorporadas
ao código. A primeira é que o sistema **já expõe um registro de eventos**
— o histórico de andamentos —, de modo que a mineração de texto deixa de
ser condição de possibilidade do process mining e passa a ser camada de
enriquecimento. A segunda é que os prazos normativos não incidem sobre
eventos de tramitação, mas sobre atos documentais, o que define qual das
duas fontes sustenta o grafo.

### Preparação dos dados

Realizada. O estágio 3 é, na terminologia do CRISP-DM, a construção de
atributos: de texto não estruturado saem destinatário, assunto,
referências, signatários, exigências, componente cultural e prazo
informado. O estágio 4 monta o log de eventos propriamente dito, com
identificador de caso, atividade, carimbo de tempo, recurso e estágio do
ciclo de vida do ato.

É aqui que se concentra a adaptação do CRISP-DM a dados textuais: o
espaço de atributos não preexiste, é construído, e cada decisão de
construção — o que conta como exigência, onde o "De acordo" vale, qual
data encerra o prazo — precisou ser explicitada e verificada contra os
autos. Foi também onde mais erros apareceram, o que confirma a premissa
metodológica de deslocar o rigor para esta fase.

### Modelagem

Parcialmente realizada. O grafo produzido é um modelo de processo por
caso, descoberto dos dados e confrontado com o fluxo prescrito. O que
ainda não foi feito é a descoberta sobre um conjunto de casos —
comparação de algoritmos, apuração das quatro dimensões de qualidade,
análise de variantes —, que depende da ampliação amostral e do log
consolidado em formato de intercâmbio.

A saída em `bupaR::eventlog()` existe justamente para essa continuidade:
é por ela que o material atravessa da reconstrução individual para a
mineração de processos propriamente dita.

### Avaliação

Preparada, não realizada. O experimento definido na metodologia compara o
resultado automático com o desenho manual dos casos, registro a registro,
e reporta separadamente a cobertura estrutural e a semântica. Os
artefatos necessários estão prontos: as tabelas de nós, arestas, prazos e
demandas são exatamente o que se confronta com o padrão-ouro.

Cabe registrar que a verificação de conformidade — o confronto entre o
fluxo praticado e o prescrito — não é validação do método, e sim
resultado dele: os desvios encontrados representam o descompasso entre
norma e prática que constitui o objeto do projeto.

### Implantação

Iniciada. O painel de leitura para a área gestora é o `grafo.html`, com o
fluxo reconstruído, as esperas, os prazos excedidos e as demandas em
aberto. Falta o que a metodologia previu como produto final: o
processamento em lote sobre a amostra, o relatório estatístico gerencial
e a documentação de transferência.

---

## 8. Síntese

| Fase do CRISP-DM | Situação | Onde está |
|---|---|---|
| Entendimento do negócio | realizada fora do código, incorporada em tabelas | `FLUXOS.md`, `PARECERES`, `DEMANDAS` |
| Entendimento dos dados | realizada | estágios 1 e 2; `nos.csv`, `arestas.csv` |
| Preparação dos dados | realizada | estágios 3 e 4; log de eventos |
| Modelagem | parcial: um caso, não o conjunto | grafo por processo; falta descoberta agregada |
| Avaliação | preparada | tabelas prontas para o confronto com o padrão-ouro |
| Implantação | iniciada | `grafo.html`; falta o lote e o relatório gerencial |

O que a solução entrega hoje, portanto, é a espinha do experimento: a
transformação verificável de autos processuais em log de eventos e em
modelo de processo por caso. O que falta é de outra natureza — não mais
engenharia de leitura, e sim ampliação amostral e medição.
