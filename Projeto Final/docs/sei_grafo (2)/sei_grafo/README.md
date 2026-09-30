# Grafo processual a partir dos autos do SEI

Reconstrói o fluxo de um processo administrativo do SEI/IPHAN em forma de
grafo, apura os prazos da IN nº 06/2025 e emite o diagrama, o log de
eventos e o relatório de prazos.

## O que executar

**Apenas `executar.R`.** Os cinco arquivos em `R/` são bibliotecas de
funções, carregadas automaticamente por ele. Não precisam ser abertos,
editados nem rodados separadamente.

O link do processo vai em um destes dois lugares, nunca dentro de `R/`:

- no bloco **CONFIGURAÇÃO**, no topo de `executar.R`, se você for rodar
  pelo RStudio (basta colar o link e usar o botão *Source*);
- no argumento `--url`, se for rodar pelo terminal.

O diretório de trabalho precisa ser a pasta do projeto, a que contém
`R/` e `executar.R`. No RStudio: *Session > Set Working Directory > To
Source File Location*.

## Estrutura

```
executar.R         ÚNICO arquivo a executar; recebe o link
FLUXOS.md          fluxos do licenciamento e prazos da IN 06/2025
SOLUCAO.md         funcionamento do pipeline e encaixe no CRISP-DM
REGRAS.md          regras de negócio do fluxo e dificuldades da mineração
R/01_leitura.R     aquisição em sessão e leitura do acervo; cabeçalho,
                   lista de documentos e histórico de andamentos
R/02_documento.R   conteúdo dos documentos nativos: destinatários, assunto,
                   referências, assinaturas, prazo informado e tabelas de
                   marcação
R/03_grafo.R       nós, arestas, hierarquia de unidades e apuração de prazos
R/04_saida.R       Mermaid, Graphviz e log de eventos
executar.R         script de uso
```

## Uso

```bash
Rscript executar.R --url "https://sei.iphan.gov.br/sei/modulos/pesquisa/md_pesq_processo_exibir.php?<hash>"
Rscript executar.R --acervo acervo/01450001620202678      # material já arquivado
```

Saídas em `saida/`: `grafo.html` (página autocontida, com o diagrama
embutido como SVG, controles de zoom e arrasto, legenda, quadro de
prazos com a demanda que cada um cobre e quadro das demandas geradas com os
documentos envolvidos e a situação de cada uma), `grafo.png`, `grafo.svg`,
`grafo.gv`, `grafo.mmd` e as tabelas `nos.csv`, `arestas.csv` e
`prazos.csv` e `demandas.csv`. O `.mmd` serve à conferência lado a lado com o diagrama
manual. A renderização usa o Graphviz (`dot`); sem ele, o HTML cai para
Mermaid no navegador.

Pacotes: `rvest`, `xml2`, `httr2`, `dplyr`, `tidyr`, `stringr`, `purrr`,
`tibble`, `lubridate`, `glue`, `digest`, `readr`; opcionais `bupaR`,
`DiagrammeR`, `DiagrammeRsvg`, `rsvg`.

## Estrutura esperada do acervo

```
acervo/<nup sem pontuação>/
  Autos_completo/            página do processo salva em HTML
  Documentos_html_nativo/
    <numero>/                um diretório por documento nativo
  manifesto.csv              sha256 e carimbo de tempo de cada arquivo
```

## O link como entrada

O parâmetro do endereço `md_pesq_processo_exibir.php?<hash>` é cifrado,
mas não depende de sessão: o servidor o decifra a qualquer momento, e uma
requisição GET simples recupera a página. O link, portanto, serve de
entrada do pipeline e pode ser guardado. O mesmo vale para os endereços
dos documentos, que vêm em atributo `onclick`, em caminho relativo, e são
resolvidos com `url_absolute()` contra o endereço do processo.

O código de confirmação é exigido apenas na **pesquisa** por número de
protocolo — a etapa de descobrir o link —, e não na leitura de um link já
conhecido. Quando for preciso localizar processos pelo NUP, essa etapa é
resolvida pelo operador; o projeto não automatiza o captcha.

O acervo é arquivado com resumo criptográfico e carimbo de tempo por
outra razão: congelar o objeto de análise, de modo que a conferência
manual e a execução automática incidam sobre exatamente o mesmo material,
e evitar downloads repetidos a cada iteração nas regras.

## Convenções do diagrama

| Elemento | Significado |
|---|---|
| Geometria do nó | tipo do documento (ver abaixo) |
| Rótulo da seta (caixa cinza) | destinatário informado no documento de origem |
| Linha sem seta | assunto, marcações de tabela, destinatários não endereçados e situação do prazo |
| Linha tracejada | documento ainda não juntado aos autos |
| Tracejado amarelo | parecer demandado que não consta dos autos |
| Tracejado marrom | documento esperado, ainda não juntado aos autos |
| Trapézio laranja | encaminhamento que não se concretizou |
| Cilindro cinza | data da última movimentação, sem ligação com o fluxo |
| Casa lilás | complementações solicitadas a partir de um parecer |
| Retângulo cinza | destinatário que responde por mais de um ato |
| Nota turquesa | ofício com e-mail declarado e notificação ainda não enviada |
| Círculo verde-água | unidade deu ciência e devolveu o processo |
| Casa invertida azul | parecer aguardando encaminhamento |
| Aba magenta | assinaturas pendentes: o ato ainda não se aperfeiçoou |
| Preenchimento preto | documento sem ligação no fluxo |
| Caixa cinza-escura | ato restrito, sem inteiro teor e sem citação |

As cores e os traços vivem na tabela `PALETA`, em `R/04_saida.R`, que
alimenta ao mesmo tempo o Graphviz, o Mermaid e a legenda do HTML — um
só lugar para ajustar.
| Octógono colorido | situação do prazo, com seta apontando para o parecer: verde-claro dentro do prazo, vermelho-claro fora, com os dias de atraso |

O parecer permanece branco: o atraso é informação do prazo, e não
atributo do ato.

Geometrias: despacho é retângulo; ofício, paralelogramo; parecer e nota
técnica, elipse; termo de referência, hexágono; e-mail, nota; portaria,
octógono duplo; protocolo externo, pasta; pendente e faltante, elipse
tracejada; prazo, octógono; fluxo abortado, trapézio invertido;
informativo, cilindro. Tipo nativo ainda não tratado recebe losango — a
geometria existe justamente para tornar visível a chegada de um tipo
novo. A tabela `GEOMETRIA`, em `R/04_saida.R`, é o ponto único de
ajuste — vale tanto para o Graphviz quanto para o Mermaid.

## Regras implementadas

**Direção das setas.** Quatro regras, em ordem de precedência. A citação
explícita pelo número "Processo / Documento" liga o documento citado ao
que o cita. Na ausência de citação, vale a correspondência: A dirige-se a
Fulano e o ato seguinte assinado por Fulano é a resposta — e, quando a
resposta sai no mesmo lote, um único pedido pode gerar várias setas. O
protocolo externo liga-se ao primeiro ato que o cita ou, não havendo
citação, ao primeiro ato do ciclo que ninguém cita. O que sobra sem
entrada é encadeado pela ordem cronológica.

**Ligação por endereço eletrônico.** Quando o documento declara os
e-mails dos destinatários e o e-mail registrado nos autos foi enviado aos
mesmos endereços, a ligação está feita. O rótulo traz apenas os blocos
cujos endereços coincidem, mais os destinatários externos sem e-mail
declarado; a unidade interna citada sem endereço fica de fora, por não
ser destinatária de correspondência.

**Rótulo das setas.** Rotula-se a seta cujo destinatário assina o ato de
destino, qualquer que tenha sido a regra que a criou — inclusive a
citação, já que o número explica a ligação mas não quem a endereçou.
Dois ajustes: quem assina em substituição
responde pelo pedido dirigido à pessoa substituída, desde que seja o ato
imediatamente seguinte **e que substitua justamente aquela função** — o
chefe substituto de uma divisão não responde por pedido endereçado ao
coordenador de outra área, e termos genéricos de hierarquia, como chefe
ou coordenador, não bastam para caracterizar a substituição; e o nome
abreviado casa com o completo quando o primeiro e o último nome
coincidem.

**Destinatários e marcações.** Endereços, CEP e e-mail são descartados do
bloco de destinatário; permanecem nome e vínculo. Blocos de cópia ("C/C")
geram destinatários adicionais. Destinatários que não endereçaram seta
alguma, e os de e-mail, aparecem na linha sem seta, para que a informação
não se perca.

**Unidades vinculadas.** Um ofício endereçado a uma unidade-mãe pode ser
respondido por unidade vinculada. A hierarquia conhecida está em
`HIERARQUIA` (CGLic: CAIP, CGM, CAIP-CGM, DAP, DIVGEO, CORA, DINO; CNA:
COIR, CGINF, COP), e o histórico de andamentos confirma o repasse caso a
caso, pelo registro "Processo remetido pela unidade &lt;mãe&gt;" na
unidade filha. Superintendências são reconhecidas pelo sufixo da UF.

**Prazos.** O relógio é aberto pelo ato da CGM que fixa a data-limite de
manifestação; conta-se a partir da documentação externa protocolada que o
antecede; encerra-se na assinatura do parecer que responde à demanda, e
não em sua data nominal, porque o ato se aperfeiçoa com a assinatura. A
contagem é em dias corridos.

**Enquadramento do empreendimento.** Despacho da triagem que pede a
análise do enquadramento ou do reenquadramento entra no quadro de prazos
com esse nome. O parecer que responde nem sempre cita o despacho pelo
número; quando trata do mesmo assunto e ainda não recebeu seta dele, a
ligação é feita por referência de conteúdo.

**Demanda que não termina em parecer.** Quando a triagem pede a análise
de termo de compromisso, de complementação ou de enquadramento, o
despacho segue de mão em mão até quem examina o objeto, e nenhum parecer
encerra a contagem. A cadeia é percorrida pela assinatura: o ato seguinte
é o que a pessoa endereçada assina, tratando do mesmo objeto; a demanda
se fecha quando a cadeia retorna a quem a recebeu. Como a data nominal
nem sempre segue a do pedido — um despacho pode ser datado depois da
resposta que o atende —, a busca admite uma janela para trás,
privilegiando o que vem adiante e o mais próximo.

**Situação do prazo sem manifestação.** Prazo cuja demanda não foi
atendida não é "no prazo": fica "em curso" enquanto o limite não vence e
"vencido sem manifestação" depois disso.

**Um prazo por demanda.** Um despacho pode abrir mais de uma demanda — a
análise da ficha de caracterização alcança os três componentes culturais
—, e cada uma tem seu próprio relógio. O quadro de prazos traz uma linha
por demanda, com o ato de abertura, o ato de encerramento e a situação. Quando o despacho fixa prazo
e o assunto nomeia o instrumento analisado, como no termo de compromisso
do empreendedor, é esse o nome da demanda.

**Demanda aberta pela unidade de triagem.** O despacho da CGM abre a
demanda que o parecer responde, e o instrumento está declarado no
assunto do ato. Informando análise manual da FCA, a demanda alcança os
três componentes culturais — arqueológico, material e imaterial; citando
PAIPA, RAIPA, PAPIPA, RAIPM, PGBIR, RAIBIR ou RPGBIR, alcança o parecer
correspondente; informando análise automática, não há parecer, e a
demanda é notificar o interessado. Quando o parecer demandado consta dos
autos, a seta o alcança mesmo sem citação por número; quando não consta,
entra como nó tracejado amarelo de documentação pendente, com a data em
que o prazo expirou.

**Taxonomia dos pareceres.** A tabela `PARECERES` associa cada rótulo
canônico da IN 06/2025 ao componente cultural e, no componente
arqueológico, ao nível do empreendimento. Pareceres de rótulo livre —
potencial de impacto, programa de gestão, salvamento, acompanhamento,
relatório de pesquisa, cadastro de sítios — são reconhecidos como
arqueológicos; fora disso, o componente é buscado no próprio texto, pelos
termos arqueológico, material e imaterial, e o nível pela expressão
"Nível I" a "Nível IV".

**Complementação e a documentação que a atende.** Solicitada a
complementação, é o interessado quem lê o parecer, identifica o que foi
requerido e protocola a documentação. A seta do parecer para a entrada de
documentação seguinte registra esse encadeamento, que nenhuma citação por
número expressa.

**Ato e documentação externa.** É ato o documento nativo do SEI — o que
traz rodapé de assinatura eletrônica, selo de autenticidade ou cabeçalho
de e-mail — e também o de tipo reconhecido como ato mesmo sem inteiro
teor disponível: despacho, ofício, parecer, nota técnica, portaria, termo
de referência e e-mail. Todo o resto é documentação externa, agrupada em
sequências que começam no recibo de peticionamento, pertencem a uma só
unidade e se encerram quando outra unidade registra ato nativo. A seta de
citação parte do nó da documentação externa, e não dos números listados
na linha sem seta.

**Parecer de rótulo genérico.** "Parecer - Ficha de Caracterização de
Atividade FCA" recebe a nomenclatura canônica conforme o componente
declarado no corpo: Patrimônio Arqueológico, Material ou Imaterial.

**Parecer sem encaminhamento.** Emitido o parecer, espera-se o ato do
destinatário ou citação em outro documento. Não havendo nem um nem outro,
o parecer é marcado como aguardando encaminhamento.

**Ciência da unidade.** Unidade que recebeu o processo e o devolveu não
está em mora: apenas deu ciência, e o ramo se encerra ali. A devolução é
lida no histórico, pelo registro de remessa a partir daquela unidade.

**Ordem dos autos.** A sequência é a da árvore do processo — a ordem em
que o SEI lista os documentos —, e não a das datas. Um ato pode ser
lavrado com data anterior à do que o antecede na árvore, e é a árvore
que descreve o que aconteceu antes do quê.

**Ordem cronológica.** O fluxo acompanha a ordem dos autos: um ato não
demanda outro que já está juntado, de modo que setas para atos
anteriores são desfeitas. Atos da mesma data seguem a ordem em que
aparecem na árvore do processo, e é por essa posição — não pela data —
que se mede a proximidade entre um pedido e a resposta.

**Endereçamento à função.** O documento pode dirigir-se ao gabinete ou à
chefia sem nomear quem os ocupa. A resposta é então o ato seguinte
assinado por quem exerce a função endereçada — "Gabinete da
Superintendência" responde-se com "Superintendente". A regra só vale
para blocos que não nomeiam pessoa alguma dos autos; os que nomeiam já
são resolvidos pela correspondência de nomes.

**Ato derivado do parecer.** Emitido o parecer, seu próprio signatário
costuma lavrar o ato que o encaminha. Não havendo citação, a ligação é
de sequência: o ato deriva do parecer, e a seta de endereçamento que
antes apontava para ele é desfeita, porque quem o produziu já estava
naquele fluxo.

**Comunicação interna.** E-mail dirigido apenas a endereços do domínio
da própria instituição não provoca protocolo do interessado, e por isso
não se liga à documentação seguinte.

**Documentação sem demanda.** Documentação protocolada que não é
alcançada por citação, por termo nem por demanda da unidade de triagem
recebe nó de pendência: a entrada segue sem resposta.

**E-mail e a documentação seguinte.** O e-mail dos autos costuma apenas
comunicar que segue um ofício; o assunto de que trata está no ato
encaminhado. Os termos desse ato são cruzados com os rótulos dos
documentos protocolados depois, e a seta alcança o grupo de maior
coincidência. Quando o ato encaminhado é portaria e nenhum relatório
consta dos autos, o que se espera é o próprio relatório, e a seta
tracejada aponta para ele — apenas a partir da comunicação mais recente.

**Numeração distingue o ato do anexo.** Despacho, ofício, parecer e nota
técnica gerados no sistema vêm com numeração própria no rótulo — "Ofício
123", "Despacho 41". Sem inteiro teor e sem número, o documento é anexo
do interessado e entra na documentação externa, não como ato da
administração.

**Marcações e assunto.** Havendo itens assinalados em tabela, são eles
que descrevem o ato: o assunto do termo de referência é texto de
contextualização e não acrescenta nada à leitura do fluxo.

**Ato restrito.** Despacho, parecer ou ofício numerado cujo cadeado
impede a leitura. Não sendo citado em documento algum, nada se sabe de
seu papel: fica à margem do fluxo, ao lado da última movimentação. A
numeração é o que distingue o ofício nativo do externo.

**Cadastro em base georreferenciada.** Ofício ou despacho que encaminha o
processo para cadastro — reconhecido pelos termos em qualquer parte do
documento: no corpo, no assunto ou na função do destinatário, já que
"Chefe da Divisão de Geoprocessamento" identifica por si a unidade a que
a demanda se dirige — liga-se ao ato da unidade que se pronuncia sobre o mesmo
assunto. Não havendo pronunciamento, resolve-se a unidade endereçada,
seguindo o repasse à unidade vinculada competente: se ela recebeu e
devolveu, deu ciência e o ramo se encerra; se recebeu e ainda detém, a
manifestação segue pendente.

**E-mail anexado.** E-mail que consta dos autos como arquivo, e não como
documento nativo, não tem conteúdo legível — costuma ser falha de envio
ou resposta do interessado. Fica em preto, à margem do fluxo, ao lado da
última movimentação.

**Cor das setas.** Três famílias. A **azul** é citação: o documento cita
o outro pelo número, e o elo está no próprio texto. A **vermelha** é
endereçamento: o destinatário de um ato assina o seguinte, e o elo é a
correspondência. A **preta** é fluxo derivado — ligações que nenhum dos
dois explica e que vêm de regra de negócio: a demanda aberta pela
triagem, o protocolo que inaugura o ciclo de análise, o encadeamento
cronológico do que sobrou sem entrada, o parecer que encaminha a
publicação da portaria e o cadastro em base georreferenciada.

**O que o Termo de Referência exige.** Os itens assinalados na tabela do
TRE — capturados pelo termo em negrito da linha marcada com X — definem
os pareceres esperados e, quando são termo de compromisso, dispensam a
família correspondente. O detalhe está no `FLUXOS.md`.

**Parecer que encaminha portaria.** O parecer que precede a portaria é o
que encaminha sua publicação, ainda que a portaria não o cite; por isso
ele também não é marcado como aguardando encaminhamento.

**Notificação pendente.** Ofício que declara o endereço eletrônico do
destinatário sem que os autos registrem e-mail enviado a ele recebe nó
próprio. Endereços do domínio da própria instituição são desconsiderados:
comunicação interna não é notificação ao interessado.

**Situação da complementação.** Uma só exigência entra no quadro, e a
situação acompanha até onde o fluxo avançou. Comunicada ao interessado
por e-mail, fica em "aguarda documentação complementar" até que venha o
protocolo e um parecer que o analise, quando passa a "atendida". Parada
no ofício, sem e-mail enviado, fica em "aguardando enviar e-mail ao
interessado". Parada no parecer, antes mesmo do ofício, fica em
"complementações aguardando convalidação", já que o ato seguinte ainda
precisa apreciar o pedido.

**Destinatário como nó.** Quando o ato se dirige a alguém e essa pessoa
— ou quem a substitui — assina dois ou mais atos dos autos, repetir o
mesmo rótulo em cada seta esconderia que se trata de um único
encaminhamento. O destinatário vira nó: o ato demandante chega a ele por
linha sem seta, e dele partem as setas para cada ato assinado.

**Complementações solicitadas.** Marcadas em pareceres que abrem a
exigência e em ofícios
que citam um parecer já emitido — pelo número SEI ou pela numeração
própria do documento — e trazem, no corpo, os termos que caracterizam a
exigência: complementação, complementações, complementos, solicita-se,
solicito, solicitamos e esclarecimentos. As raízes soltas ficam de fora,
porque "Recomenda aprovação" é rótulo de tabela e "solicitação em
epígrafe" é menção a pedido alheio.

**Assinaturas pendentes.** Cada "De acordo" indica um anuente além de
quem redigiu, mas só conta o que estiver na **área de assinatura**, entre
o fecho da correspondência e o rodapé de assinatura eletrônica: no corpo
do texto a expressão é conectivo ("de acordo com a norma") e inflaria a
estimativa. O ato só se aperfeiçoa quando reúne todas as assinaturas
esperadas, e a diferença entre o esperado e o coletado aparece em nó
magenta.

**Documento avulso.** Ato que não recebe nem dirige seta alguma do fluxo
é desenhado em preto: é a forma de encontrá-lo nos autos.

**Fluxo abortado.** Um despacho encaminha a alguém para que emita o
parecer. Se o parecer da mesma demanda vem assinado por outra pessoa, e
há outro despacho no mesmo escopo dirigido justamente a quem assinou, o
encaminhamento anterior não se concretizou: o ato recebe um nó laranja
indicando o fluxo abortado.

**Documentos pendentes.** Dois casos geram nó tracejado. O primeiro é o
encaminhamento para cadastro em base georreferenciada: o documento traz
os termos de `TERMOS_DBGEO` e o histórico confirma que a unidade recebeu
o processo, nada emitiu e continua a detê-lo — unidades de trânsito, que
remeteram adiante, são descartadas. O segundo é a portaria de
autorização: publicada, a próxima peça esperada do interessado é o
relatório de avaliação de impacto, e para esse nó apontam a portaria e o
ato que a comunica.

**Tabelas de marcação.** Duas formas são tratadas. Na primeira, a linha
marcada com X tem o conteúdo na célula seguinte — é o caso do nível de
estudo exigido no Termo de Referência. Na segunda, o rótulo ocupa célula
com `rowspan=2`, as opções ficam na mesma linha e as marcações na linha
seguinte, alinhadas por posição — é o caso do campo "Recomenda aprovação"
na tabela de resultado da análise.

## Aquisição de anexos

A lista de documentos traz, além dos atos, anexos em PDF, imagem e
planilha. O coletor verifica o tipo de conteúdo da resposta e só grava o
que for HTML; os demais permanecem na lista como documentação externa,
sem inteiro teor lido. Sem essa verificação, o leitor tentaria interpretar
bytes binários como texto, produzindo avisos de codificação e conteúdo
inútil.

## Limitações

Somente documentos nativos em HTML são lidos. Anexos em PDF, planilhas e
arquivos compactados aparecem na lista de documentos e são referenciados
pelos atos nativos, mas seu conteúdo não é extraído; incorporá-los exige
reconhecimento óptico, etapa não contemplada aqui.

Documentos restritos não expõem inteiro teor na consulta pública e são
sinalizados como tais.

## Estado de verificação

O pipeline foi executado sobre o acervo do processo
01450.001620/2026-78: 33 documentos na lista, 18 nativos em HTML, 47
andamentos. Resultado: 22 nós, 25 arestas, 1 unidade pendente e 2 prazos
apurados, ambos excedidos — 11 e 86 dias corridos —, que correspondem
exatamente aos dois pareceres destacados no diagrama manual.

A etapa de download a partir do link não pôde ser exercitada no ambiente
de desenvolvimento, por restrição de rede; a extração dos endereços dos
documentos foi verificada contra a página arquivada, e devolveu os 32
links esperados (33 documentos menos 1 restrito).

Divergências conhecidas em relação ao desenho manual: o Despacho 1926 e o
Parecer PAIPA aparecem em ordem invertida, porque o parecer tem data
nominal anterior à do despacho que o solicita; e o Ofício 4259, que
encaminha à unidade-mãe, é ligado ao parecer da unidade vinculada pela
referência explícita, mas sem rótulo de destinatário, que o documento não
traz.
