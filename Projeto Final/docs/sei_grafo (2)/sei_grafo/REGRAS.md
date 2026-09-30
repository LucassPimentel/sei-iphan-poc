# Regras de negócio do fluxo e dificuldades da mineração

Reúne, de um lado, as regras que o gerador aplica para decidir o que é
ato, o que liga um ato a outro e o que sinaliza pendência; de outro, os
obstáculos encontrados ao extrair essa informação dos autos. As regras
foram construídas e corrigidas contra dois processos reais, e a seção
final registra o que ainda não foi verificado.

Complementa o `FLUXOS.md`, que trata dos fluxos administrativos e dos
prazos da IN 06/2025, e o `SOLUCAO.md`, que descreve o funcionamento do
pipeline.

---

# Parte I — Regras de negócio

## 1. O que é ato do processo

Duas condições, qualquer uma delas basta:

- o documento é **nativo do SEI**, reconhecido pelos marcadores que só
  ele tem — rodapé de assinatura eletrônica, selo de autenticidade ou
  cabeçalho de e-mail;
- o documento é de **tipo reconhecido como ato** ainda que sem inteiro
  teor: despacho, ofício, parecer, nota técnica, portaria, termo de
  referência.

Duas ressalvas corrigem o excesso que essas condições produziriam:

**Numeração.** Despacho, ofício, parecer e nota técnica gerados no
sistema trazem numeração própria no rótulo — "Ofício 123", "Despacho
41". Sem inteiro teor e sem número, o documento é anexo do interessado.

**E-mail.** E-mail sem inteiro teor nunca é ato: nada se sabe do que
motivou sua juntada, e ele fica à margem do fluxo.

Todo o resto é **documentação externa**, agrupada em sequências que
começam no recibo de peticionamento, pertencem a uma só unidade e se
encerram quando outra unidade registra ato nativo.

## 2. A sequência é a da árvore, não a das datas

A ordem dos autos é a ordem em que o SEI lista os documentos. Um ato
pode ser lavrado com data anterior à do que o antecede na árvore, e é a
árvore que descreve o que aconteceu antes do quê. Disso decorrem:

- setas apontam para atos subsequentes, nunca para antecedentes;
- a proximidade entre um pedido e sua resposta é medida pela posição na
  árvore, não pela diferença de datas.

## 3. Como um ato se liga a outro

Sete regras, em ordem de precedência.

**1. Citação.** O documento cita outro pelo número "Processo /
Documento"; a seta vai do citado para quem cita. A citação de peça
protocolada parte do nó da documentação externa, e não do número
listado.

**2. Endereçamento pelo nome.** O destinatário de A assina o ato B. A
busca olha primeiro adiante e, na ausência de resposta, aceita ato
anterior do mesmo ciclo. Um documento pode dirigir-se a mais de uma
pessoa, e cada bloco de destinatário abre sua própria expectativa de
resposta. Três ajustes:

- o nome abreviado casa com o completo quando o primeiro e o último nome
  coincidem;
- quem assina em substituição responde pelo pedido dirigido à pessoa
  substituída, desde que seja o ato imediatamente seguinte **e que
  substitua aquela função**;
- a resposta mais próxima já ligada por citação encerra a expectativa:
  não se procura outra mais adiante.

**3. Endereçamento à função.** O documento pode dirigir-se ao gabinete
ou à chefia sem nomear quem os ocupa; a resposta é o ato seguinte
assinado por quem exerce a função endereçada **e que trate do mesmo
assunto**. Vale apenas para blocos que não nomeiam pessoa alguma dos
autos, e não se aplica a ramo já encerrado em ciência: a unidade que
apenas tomou conhecimento não deve outro ato.

**4. Demanda da unidade de triagem.** O despacho da CGM declara no
assunto o instrumento a analisar, e a seta alcança o parecer
correspondente ainda que não haja citação.

**5. Ligação por endereço eletrônico e por termo.** O e-mail dos autos
costuma apenas comunicar que segue um ofício; o assunto de que trata
está no ato encaminhado. Os termos desse ato são cruzados com os rótulos
dos documentos protocolados depois, e a seta alcança a primeira entrada
que os compartilha. Comunicação dirigida apenas ao domínio da própria
instituição não provoca protocolo e não gera ligação.

**6. Derivação do parecer.** Emitido o parecer, seu próprio signatário
costuma lavrar o ato que o encaminha. Não havendo citação, o ato deriva
do parecer, e a seta de endereçamento que antes apontava para ele é
desfeita. A derivação é de vizinhança: vale para o ato que vem logo
depois na árvore.

**7. Da ficha ao termo de referência.** Encaminhada a ficha para análise
manual, o parecer que a examina dá subsídio à elaboração do termo de
referência: é a sequência prevista na norma — FCA, parecer de FCA, TRE. A
ligação não se expressa por citação nem por coincidência de vocabulário,
e por isso precisa ser declarada. Cede lugar ao endereçamento quando este
já explica o mesmo par, porque o rótulo do destinatário informa mais, mas
prevalece sobre a simples sucessão cronológica.

**8. Encadeamento cronológico.** Fecha a cadeia para o que restou sem
entrada.

Ciclos de duas arestas são desfeitos mantendo o elo que parte do ato mais
recente: é ele que pede, e o outro é a peça pedida.

## 4. Rótulo e cor das setas

Rotula-se a seta cujo destinatário assina o ato de destino, qualquer que
tenha sido a regra que a criou — a citação explica a ligação, mas não
quem a endereçou. O destinatário é substituído pelo assunto quando não
traz nome de pessoa, por ser unidade, e quando coincide com quem assina o
próprio documento, sinal de anuência interna.

| Cor | Significado |
|---|---|
| Azul | citação: o documento cita o outro pelo número |
| Vermelho | endereçamento: o destinatário assina o ato seguinte |
| Preto | fluxo derivado: demanda da triagem, protocolo que inaugura ciclo, encadeamento cronológico, parecer que encaminha portaria, cadastro em base georreferenciada |

## 5. Quando o destinatário vira nó

Se o ato se dirige a alguém e essa pessoa assina dois ou mais atos,
repetir o mesmo rótulo em cada seta esconderia que se trata de um único
encaminhamento. O destinatário passa a ser nó: o ato demandante chega a
ele por linha sem seta, e dele partem as setas.

## 6. Prazos

O relógio é aberto pelo ato que fixa a data-limite, contado da
documentação externa protocolada que o antecede e encerrado na
**assinatura** do ato que responde — não na data nominal, porque o ato
administrativo se aperfeiçoa com a assinatura. Contagem em dias
consecutivos, com o relógio do interessado mantido separado do relógio da
administração.

Um despacho pode abrir mais de uma demanda — a análise da ficha de
caracterização alcança os três componentes culturais —, e cada uma tem
seu próprio relógio, com uma linha no quadro.

Nem toda demanda termina em parecer. Pedida a análise de termo de
compromisso, de complementação ou de enquadramento, o despacho segue de
mão em mão até quem examina o objeto: a cadeia é percorrida pela
assinatura, e a demanda se fecha quando retorna a quem a recebeu.

Prazo cuja demanda não foi atendida não é "no prazo": fica **em curso**
enquanto o limite não vence e **vencido sem manifestação** depois disso.

## 7. O que o Termo de Referência exige

Os itens assinalados definem os pareceres esperados e, quando são termo
de compromisso, dispensam a família correspondente. Pareceres de outra
nomenclatura presentes nos autos substituem os de rótulo canônico;
parecer técnico e nota técnica substituem os de patrimônio material e
imaterial, desde que tratem do mesmo componente cultural. O parecer de
relatório não admite substituto: é peça específica e posterior, que só se
tem por entregue sob o próprio rótulo. As demandas só valem quando a triagem encaminhou a ficha para
análise manual. O detalhamento está no `FLUXOS.md`.

## 8. Sinalizações

| Marcação | Quando aparece |
|---|---|
| Situação do prazo | data-limite confrontada com a assinatura do ato que responde |
| Parecer faltante | demanda declarada sem parecer correspondente |
| Documento esperado | unidade que recebeu o processo e nada emitiu; peça seguinte após a portaria |
| Ciência da unidade | unidade que recebeu e devolveu: ramo encerrado, sem pendência |
| Documentação sem demanda | entrada protocolada que nenhuma citação, termo ou demanda alcança |
| Fluxo abortado | despacho cujo destinatário não produziu ato algum, havendo outro que alcançou quem assinou |
| Complementações solicitadas | ato que exige complementação a partir de um parecer, comunicado ao interessado |
| Assinatura pendente | "De acordo" na área de assinatura sem a anuência correspondente |
| Notificação pendente | ofício com e-mail declarado e sem e-mail enviado |
| Parecer sem encaminhamento | parecer sem ato do destinatário e sem citação |
| E-mail anexado, ato restrito, documento avulso | peças sem inteiro teor ou sem ligação, à margem do fluxo |

---

# Parte II — Dificuldades da mineração

Esta seção registra os obstáculos encontrados. Vários só apareceram no
segundo processo, o que é em si um resultado: **regra validada em um
caso não é regra validada.**

## 1. O sistema não distingue ato de anexo

Não há campo que diga se um documento é ato da administração ou peça
protocolada pelo interessado. A primeira tentativa — uma lista de tipos
considerados externos — falhou assim que apareceu um anexo de tipo não
previsto, que entrou no fluxo como se fosse ato. A solução foi inverter a
pergunta: em vez de perguntar que nome o documento tem, perguntar se ele
é nativo do sistema, o que se verifica pela presença do rodapé de
assinatura eletrônica.

Restou um caso híbrido: atos sem inteiro teor disponível, como a portaria
publicada, que precisam ser reconhecidos pelo tipo. Daí a regra da
numeração — o ato gerado no sistema traz número próprio no rótulo.

## 2. A data não é a sequência

Um despacho pode ser datado depois do ato que o responde. Encontrei isso
nos dois processos, e em dois pontos distintos: um parecer com data
nominal anterior à do despacho que o pede, e um despacho de 18/06
respondido por um ato de 17/06. Ordenar por data produz um grafo com
setas para trás e demandas que parecem fechadas antes de abertas.

A ordem correta é a da árvore do processo — a ordem em que o SEI lista os
documentos, que é a de juntada. Essa foi provavelmente a correção de
maior alcance: mudou a ordenação dos nós, a medida de proximidade entre
pedido e resposta e o filtro de setas retroativas, e resolveu de uma vez
três divergências que eu vinha tratando como casos isolados.

## 3. O editor do SEI corrompe o texto

Três defeitos recorrentes, todos silenciosos:

**Quebras de linha perdidas.** "Ao TécnicoEDSON MIRANDA BORGES" chega em
uma linha só. Pior: ao remover o tratamento com um padrão que termina em
`\w*`, o próprio nome é engolido, e o destinatário vira "MIRANDA
BORGES". A quebra precisa ser restituída **antes** da remoção do
tratamento, e apenas onde um nome em caixa alta segue letra minúscula —
do contrário "do Patrimônio" também é partido.

**Caracteres invisíveis.** Espaços de largura zero aparecem no meio dos
números de documento e quebram a captura de referências.

**Folhas de estilo no corpo.** O e-mail do SEI traz CSS embutido no
corpo. Qualquer busca por termos passa a encontrar `font-family`,
`overflow` e `helvetica`, que dominam o vocabulário e inutilizam o
cruzamento.

## 4. A referência entre documentos tem várias formas

O número "Processo / Documento" é a forma canônica, mas não a única. Um
ofício cita o parecer pela numeração própria — "Parecer Técnico nº
573/2026/COTEC..." —, sem o número do SEI. Um e-mail referencia o ofício
pelo nome do arquivo anexado. E há ligações que nenhuma citação expressa:
a demanda que a triagem abre, o protocolo que o interessado faz depois de
notificado, a portaria que decorre do parecer.

Cada uma dessas exigiu uma regra própria, e o resultado é que o grafo se
sustenta em sete critérios distintos, não em um.

## 5. Nem toda ligação está no texto

A sequência entre a ficha de caracterização e o termo de referência é o
caso exemplar: o parecer que analisa a ficha dá subsídio ao termo, mas
nenhum dos dois cita o outro nem compartilha vocabulário — o assunto do
termo é a contextualização normativa, e o da ficha é o empreendimento.

Descobri isso pelo caminho errado. A ligação aparecia no grafo por
acidente, produzida pela regra de endereçamento à função, e só ao tentar
restringir essa regra é que percebi que o par ficaria órfão. A ligação
existe porque a norma a estabelece, e precisou ser declarada como tal.

O mesmo vale para a demanda que a triagem abre, para o protocolo que o
interessado faz depois de notificado e para a portaria que decorre do
parecer: nenhuma dessas ligações está escrita nos documentos.

## 6. Nomes e funções não são identificadores

O mesmo servidor aparece como "Ana Tauhyl" e "Ana Paula Moreli Tauhyl".
Um documento se dirige ao "Gabinete da Superintendência" sem nomear
ninguém. Outro é assinado por quem substitui o titular.

Cada caso pede um critério diferente, e os critérios se contradizem. Na
substituição, termos de hierarquia — chefe, coordenador — geram falso
positivo e precisam ser descartados: o chefe substituto de uma divisão
não responde por pedido dirigido ao coordenador de outra área. No
endereçamento à função, esses mesmos termos são exatamente o que
identifica o destinatário: "Gabinete da Superintendência" responde-se com
"Superintendente". Foram necessárias duas funções de comparação com
regras opostas.

## 7. Termos frequentes não são termos relevantes

A busca por raízes — `complement`, `solicit`, `recomend` — marcou
praticamente todo parecer dos autos. "Recomenda aprovação" é rótulo de
tabela; "solicitação em epígrafe" é menção a pedido alheio; "de acordo
com a norma" é conectivo, não anuência.

A lição prática: o que caracteriza o ato não é a raiz da palavra, mas a
forma verbal e a posição no documento. "De acordo" só conta na área de
assinatura, entre o fecho e o rodapé eletrônico. As formas que
caracterizam exigência são as de primeira pessoa e as impessoais —
solicita-se, solicitamos, complementações —, não o radical solto.

## 8. Documento restrito esconde o tipo, mas não o rótulo

O cadeado impede a leitura do inteiro teor e o sistema não fornece o tipo
canônico. O rótulo visível na listagem, porém, traz a descrição completa
— "Termo de Compromisso do Empreendedor (TCE) referente aos Bens
Arqueológicos" —, e é ele que permite reconhecer a peça. Perder esse
rótulo, como aconteceu por um erro de sombreamento de coluna, apaga do
diagrama justamente a informação que liga o protocolo à demanda.

## 9. O que só se vê comparando dois processos

Cada processo tem particularidades que parecem regras gerais até o
segundo caso mostrar que não são:

- no primeiro, os três pareceres da ficha de caracterização não existem,
  e dois entram como faltantes; no segundo, existem, e um deles tem
  rótulo genérico que precisa ser resolvido pelo componente cultural
  declarado no corpo;
- no primeiro, o termo de referência é nativo; no segundo, há também uma
  minuta com a mesma tabela de marcação;
- no primeiro não há anexos em PDF ou imagem; no segundo há, e o coletor
  precisou passar a verificar o tipo de conteúdo antes de tentar ler.

## 10. O que ainda não foi verificado

Regras implementadas que nenhum dos dois processos exercita: o termo de
referência emitido automaticamente pelo sistema de avaliação, que ocorre
na análise automática da ficha; a demanda de enquadramento e
reenquadramento; o ato restrito isolado; o parecer sem encaminhamento; e
a notificação pendente por ofício com e-mail declarado. Todas foram
escritas a partir da descrição do fluxo, e não de um caso observado —
distinção que convém manter à vista.

---

## Síntese

A dificuldade central não foi ler o HTML, que é estável e bem
estruturado. Foi que **o sistema registra documentos, não eventos**. O
tipo do documento não diz qual ato ele pratica, a data não diz quando ele
entrou no fluxo, a citação não diz quem endereçou, e o nome do
destinatário não é um identificador. Cada um desses vazios foi preenchido
por uma regra de negócio derivada da norma ou da prática, e é nesse
conjunto de regras — não no código de extração — que reside o trabalho.
