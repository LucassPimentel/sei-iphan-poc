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
embutido como SVG, controles de zoom e arrasto, legenda e quadro de
prazos), `grafo.png`, `grafo.svg`,
`grafo.gv`, `grafo.mmd` e as tabelas `nos.csv`, `arestas.csv` e
`prazos.csv`. O `.mmd` serve à conferência lado a lado com o diagrama
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
| Aba magenta | assinaturas pendentes: o ato ainda não se aperfeiçoou |
| Preenchimento preto | documento sem ligação no fluxo |

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
imediatamente seguinte; e o nome abreviado casa com o completo quando o
primeiro e o último nome coincidem.

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

**Complementações solicitadas.** Marcadas apenas em pareceres e ofícios
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
