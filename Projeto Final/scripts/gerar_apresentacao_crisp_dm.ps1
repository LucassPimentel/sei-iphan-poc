$ErrorActionPreference = 'Stop'

$pp = New-Object -ComObject PowerPoint.Application
$pp.Visible = -1
$pres = $pp.Presentations.Add()
$pres.PageSetup.SlideWidth = 960
$pres.PageSetup.SlideHeight = 540

$C = @{
  ink = 0x202B3A
  navy = 0x18324B
  teal = 0x2B6777
  orange = 0xB08A4A
  yellow = 0xD7BC79
  pale = 0xF4F6F8
  white = 0xFFFFFF
  muted = 0x697586
  red = 0xA94442
  green = 0x46745B
}

function RGB([int]$hex) {
  $r = ($hex -shr 16) -band 0xFF
  $g = ($hex -shr 8) -band 0xFF
  $b = $hex -band 0xFF
  return $r + ($g -shl 8) + ($b -shl 16)
}

function Add-Box($slide, [double]$x, [double]$y, [double]$w, [double]$h, [int]$fill, [int]$line = -1, [double]$radius = 0) {
  $type = if ($radius -gt 0) { 5 } else { 1 }
  $shape = $slide.Shapes.AddShape($type, $x, $y, $w, $h)
  $shape.Fill.ForeColor.RGB = RGB $fill
  if ($line -lt 0) { $shape.Line.Visible = 0 } else { $shape.Line.ForeColor.RGB = RGB $line; $shape.Line.Weight = 1.2 }
  return $shape
}

function Add-Text($slide, [string]$text, [double]$x, [double]$y, [double]$w, [double]$h, [double]$size = 18, [int]$color = 0x17212B, [bool]$bold = $false, [string]$font = 'Aptos') {
  $box = $slide.Shapes.AddTextbox(1, $x, $y, $w, $h)
  $box.TextFrame2.TextRange.Text = $text
  $box.TextFrame2.TextRange.Font.Name = $font
  $box.TextFrame2.TextRange.Font.Size = $size
  $box.TextFrame2.TextRange.Font.Fill.ForeColor.RGB = RGB $color
  $box.TextFrame2.TextRange.Font.Bold = $bold
  $box.TextFrame.WordWrap = -1
  $box.TextFrame2.MarginLeft = 0
  $box.TextFrame2.MarginRight = 0
  $box.TextFrame2.MarginTop = 0
  $box.TextFrame2.MarginBottom = 0
  return $box
}

function Add-Line($slide, [double]$x1, [double]$y1, [double]$x2, [double]$y2, [int]$color, [double]$weight = 2) {
  $line = $slide.Shapes.AddLine($x1, $y1, $x2, $y2)
  $line.Line.ForeColor.RGB = RGB $color
  $line.Line.Weight = $weight
  return $line
}

function Add-Picture($slide, [string]$path, [double]$x, [double]$y, [double]$w, [double]$h) {
  if (-not (Test-Path $path)) { throw "Imagem nao encontrada: $path" }
  $picture = $slide.Shapes.AddPicture($path, 0, -1, $x, $y, $w, $h)
  return $picture
}

function New-Slide([string]$kicker, [string]$title, [string]$subtitle = '') {
  $slide = $pres.Slides.Add($pres.Slides.Count + 1, 12)
  $bg = Add-Box $slide 0 0 960 540 $C.pale | Out-Null
  $band = Add-Box $slide 0 0 960 12 $C.teal | Out-Null
  Add-Text $slide $kicker.ToUpper() 54 38 850 18 10 $C.teal $true 'Aptos' | Out-Null
  Add-Text $slide $title 54 60 850 48 29 $C.navy $true 'Aptos Display' | Out-Null
  if ($subtitle -ne '') { Add-Text $slide $subtitle 54 112 850 30 13 $C.muted $false 'Aptos' | Out-Null }
  Add-Text $slide ('POC IPHAN  |  ' + $pres.Slides.Count.ToString('00')) 54 510 850 14 9 $C.muted $false 'Aptos' | Out-Null
  return $slide
}

function Add-Card($slide, [double]$x, [double]$y, [double]$w, [double]$h, [string]$heading, [string]$body, [int]$accent = 0x168A8A) {
  Add-Box $slide $x $y $w $h $C.white -1 8 | Out-Null
  Add-Box $slide $x $y 7 $h $accent | Out-Null
  Add-Text $slide $heading ($x + 22) ($y + 18) ($w - 40) 25 16 $C.navy $true 'Aptos Display' | Out-Null
  Add-Text $slide $body ($x + 22) ($y + 52) ($w - 40) ($h - 68) 12 $C.ink $false 'Aptos' | Out-Null
}

# 01 - Capa
$s = New-Slide 'Mineração de processos' 'Do link do SEI ao fluxo explicável' 'Apresentação estruturada pelo CRISP-DM'
Add-Box $s 54 178 560 240 $C.navy -1 12 | Out-Null
Add-Text $s 'POC de mineração de processos de licenciamento ambiental' 82 210 480 72 27 $C.white $true 'Aptos Display'
Add-Text $s 'SEI Pesquisa Pública do IPHAN' 82 304 430 35 19 $C.yellow $true 'Aptos'
Add-Text $s 'Identificação de pendências, prazos e tramitação com rastreabilidade.' 82 358 430 45 15 $C.white $false 'Aptos'
Add-Box $s 666 174 220 244 $C.teal -1 12 | Out-Null
Add-Text $s 'CRISP-DM' 700 214 160 35 23 $C.white $true 'Aptos Display'
Add-Text $s '5 fases

1. Negócio
2. Dados
3. Preparação
4. Modelagem
5. Avaliação' 700 270 150 125 15 $C.white $false 'Aptos'

# 02 - contexto
$s = New-Slide 'Ponto de partida' 'O problema é processual, não apenas textual' 'O especialista precisa reconstruir o que aconteceu, o que falta e se houve atraso.'
Add-Card $s 54 176 260 235 'Dor operacional' 'Os autos combinam documentos nativos, anexos, restrições, andamentos, assinaturas e peças com nomenclatura variável.' $C.orange
Add-Card $s 350 176 260 235 'Pergunta da POC' 'É viável automatizar a identificação de pendências e a reconstrução do fluxo, comparando o resultado com um gabarito manual?' $C.teal
Add-Card $s 646 176 260 235 'Recorte' 'Cinco processos de 2026, disponíveis publicamente no SEI/IPHAN. O trabalho termina em Evaluation; não há Deployment.' $C.yellow

# 03 - business understanding
$s = New-Slide '01  Business Understanding' 'Transformar uma leitura manual em evidência auditável' 'O objetivo define o que deve ser extraído e também o que não pode ser inferido.'
Add-Text $s 'Objetivos' 54 172 220 25 17 $C.teal $true 'Aptos Display'
Add-Text $s '• identificar atos e pendências
• reconstruir tramitação entre unidades
• calcular conformidade temporal
• comparar automação e mapeamento manual' 54 207 330 145 17 $C.ink $false 'Aptos'
Add-Text $s 'Decisões de escopo' 510 172 300 25 17 $C.orange $true 'Aptos Display'
Add-Text $s '• somente informação pública
• restritos não são contornados
• regras normativas ficam como dados
• ML só entra se houver rótulos suficientes' 510 207 350 145 17 $C.ink $false 'Aptos'
Add-Box $s 54 391 806 66 $C.navy -1 8 | Out-Null
Add-Text $s 'Critério de sucesso: produzir artefatos reproduzíveis e explicáveis, não uma “caixa-preta” de produção.' 82 412 750 28 17 $C.white $true 'Aptos'

# 04 - data understanding
$s = New-Slide '02  Data Understanding' 'Conhecer os autos antes de modelar' 'A fonte é um sistema público, heterogêneo e com informação ausente mensurável.'
Add-Text $s 'Amostragem' 54 166 180 24 16 $C.teal $true 'Aptos Display'
Add-Text $s ("Amostragem aleatória simples" + [Environment]::NewLine + "set.seed(123)" + [Environment]::NewLine + "n = 5 processos de 2026") 54 203 230 100 20 $C.navy $true 'Aptos Display' | Out-Null
Add-Text $s 'O que existe nos autos' 366 166 250 24 16 $C.orange $true 'Aptos Display'
Add-Text $s ("documentos -> andamentos -> unidades" + [Environment]::NewLine + "assinaturas -> links -> datas" + [Environment]::NewLine + "HTML nativo + anexos + restritos") 366 203 390 100 17 $C.ink $false 'Aptos' | Out-Null
Add-Text $s 'Riscos observados' 54 345 210 24 16 $C.red $true 'Aptos Display'
Add-Text $s '• charset ISO-8859-1
• documentos sem inteiro teor
• anexos binários e PDFs
• processos em curso com fluxo incompleto' 54 380 330 100 16 $C.ink $false 'Aptos'
Add-Box $s 510 344 350 114 $C.white -1 8 | Out-Null
Add-Text $s 'Princípio de honestidade' 536 364 290 24 16 $C.navy $true 'Aptos Display'
Add-Text $s 'Ausência de conteúdo não vira “não há pendência”. Ela vira cobertura e limitação registrada.' 536 402 280 42 14 $C.ink $false 'Aptos'

# 05 - data preparation
$s = New-Slide '03  Data Preparation' 'Do HTML bruto a estruturas analisáveis' 'Cada etapa persiste seu resultado antes de alimentar a próxima.'
$xs = @(54, 225, 396, 567, 738)
$labels = @('Scraping','Conteúdo','Entidades','Text mining','Persistência')
$bodies = @('cabeçalho
documentos
andamentos','HTML nativo
texto limpo
restritos','remetente
destinatário
prazo','datas
tokens
TF-IDF','JSON
CSV
pastas por data')
for ($i=0; $i -lt 5; $i++) {
  Add-Box $s $xs[$i] 212 135 170 $(if ($i % 2 -eq 0) { $C.navy } else { $C.teal }) -1 10 | Out-Null
  Add-Text $s ('0' + ($i+1)) ($xs[$i] + 16) 229 35 22 14 $C.yellow $true 'Aptos Display' | Out-Null
  Add-Text $s $labels[$i] ($xs[$i] + 16) 260 105 25 15 $C.white $true 'Aptos Display' | Out-Null
  Add-Text $s $bodies[$i] ($xs[$i] + 16) 301 105 60 13 $C.white $false 'Aptos' | Out-Null
  if ($i -lt 4) { Add-Line $s ($xs[$i] + 137) 297 ($xs[$i+1] - 8) 297 $C.orange 2 | Out-Null }
}
Add-Box $s 54 430 819 43 $C.white -1 8 | Out-Null
Add-Text $s 'Resultado: um corpus textual e uma camada estrutural separada, preservando as entidades necessárias ao grafo.' 78 443 760 20 14 $C.navy $true 'Aptos'

# 06 - modeling overview
$s = New-Slide '04  Modeling' 'Baseline e modelos: o que comparamos e por que' 'O baseline interpretavel e a referencia; os algoritmos supervisionados aprendem padroes do corpus rotulado.'
Add-Card $s 54 164 405 135 'Baseline | dicionario ponderado' 'Conta gatilhos por classe e soma um sinal do tipo documental. E transparente, auditavel e nao precisa de treino; serve como referencia para o ML.' $C.teal
Add-Card $s 501 164 405 135 'Regressao logistica multinomial' 'Modelo linear probabilistico para cinco classes. Incluida como comparador simples: estima o peso dos termos TF-IDF associados a cada classe.' $C.navy
Add-Card $s 54 320 405 135 'Naive Bayes' 'Estima a classe pelas probabilidades dos termos, assumindo independencia entre eles. E uma referencia classica, rapida e adequada a texto.' $C.orange
Add-Card $s 501 320 405 135 'SVM linear' 'Procura uma fronteira de separacao com margem ampla no espaco TF-IDF, geralmente eficaz em textos com muitos atributos esparsos.' $C.navy
Add-Box $s 54 468 852 30 $C.white -1 8 | Out-Null
Add-Text $s 'Comparacao por processo (LOPO, 5 folds); TF-IDF e ajustado dentro de cada fold. Upsampling opcional ocorre somente no treino.' 72 475 816 16 12 $C.ink $true 'Aptos' | Out-Null

# 07 - modeling details
$s = New-Slide '04  Modeling' 'Regras de domínio dão significado ao modelo' 'A IN IPHAN nº 06/2025 é consultada como dado estruturado, não espalhada no código.'
Add-Card $s 54 176 250 246 'Classificação' 'Cinco classes semânticas: solicitação, encaminhamento, manifestação/parecer, exigência/complementação e decisão.' $C.teal
Add-Card $s 355 176 250 246 'Prazos' 'Dias corridos. O relógio usa a data-limite fixada pela unidade e encerra na manifestação correspondente.' $C.orange
Add-Card $s 656 176 250 246 'Grafo' 'Nó = ato/documento. Aresta = relação de prazo ou sequência cronológica. Cor = situação ou tipo de ato.' $C.yellow
Add-Text $s 'Regra de segurança: quando a informação não pode ser determinada, o resultado registra a limitação.' 80 452 800 22 14 $C.navy $true 'Aptos'

# 08 - evaluation
$s = New-Slide '05  Evaluation' 'Avaliar contra o gabarito, sem maquiar a incerteza' 'A avaliação atual é objetiva onde há ground truth e explícita onde ainda não há rótulo.'
Add-Box $s 54 177 245 220 $C.white -1 8 | Out-Null
Add-Text $s 'GABARITO' 82 202 190 22 13 $C.teal $true 'Aptos'
Add-Text $s 'Rotulagem manual
por documento' 82 240 170 54 22 $C.navy $true 'Aptos Display'
Add-Text $s 'classe_final = manual
quando houver correção;
senão, baseline' 82 320 170 50 14 $C.ink $false 'Aptos'
Add-Box $s 357 177 245 220 $C.white -1 8 | Out-Null
Add-Text $s 'MÉTRICAS' 385 202 190 22 13 $C.orange $true 'Aptos'
Add-Text $s 'Accuracy
Precision / Recall
F1 e matriz de confusão
Cobertura de extração' 385 240 190 110 18 $C.navy $true 'Aptos Display'
Add-Box $s 660 177 245 220 $C.white -1 8 | Out-Null
Add-Text $s 'ESCOPО ATUAL' 688 202 190 22 13 $C.red $true 'Aptos'
Add-Text $s 'Classificação e cobertura

Fluxo, eventos, datas e prazos
ainda exigem validação qualitativa
ou ampliação do gabarito.' 688 240 190 112 16 $C.ink $false 'Aptos'
Add-Box $s 54 427 851 42 $C.navy -1 8 | Out-Null
Add-Text $s 'A amostra de cinco processos sustenta uma prova de conceito, não uma conclusão de generalização populacional.' 80 440 800 18 14 $C.white $true 'Aptos'

# 09 - event log e prazos
$sProcess = New-Slide 'Process Mining e conformidade' 'Event log transforma documentos em eventos; prazos conectam demanda e resposta' 'Exemplo observado no processo 01450.003836/2026-78.'
Add-Box $sProcess 54 170 400 255 $C.white -1 8 | Out-Null
Add-Box $sProcess 54 170 400 7 $C.teal | Out-Null
Add-Text $sProcess 'EVENT LOG' 80 192 320 22 13 $C.teal $true 'Aptos' | Out-Null
Add-Text $sProcess '104 eventos no total' 80 229 330 31 22 $C.navy $true 'Aptos Display' | Out-Null
Add-Text $sProcess '47 eventos de documento\n57 eventos de tramitacao\ncase_id | atividade | timestamp | unidade\nPeriodo observado: 24/03 a 02/09/2026' 80 277 330 104 16 $C.ink $false 'Aptos' | Out-Null
Add-Box $sProcess 506 170 400 255 $C.white -1 8 | Out-Null
Add-Box $sProcess 506 170 400 7 $C.orange | Out-Null
Add-Text $sProcess 'PRAZOS' 532 192 320 22 13 $C.orange $true 'Aptos' | Out-Null
Add-Text $sProcess '3 relogios apurados' 532 229 330 31 22 $C.navy $true 'Aptos Display' | Out-Null
Add-Text $sProcess '2 dentro do prazo\n1 fora do prazo (+1 dia)\nConta dias corridos e usa limite explicito\nFim do ato: data do documento como proxy' 532 277 330 104 16 $C.ink $false 'Aptos' | Out-Null
Add-Box $sProcess 54 449 852 35 $C.navy -1 8 | Out-Null
Add-Text $sProcess 'Ressalva: o encerramento usa a data do documento; a regra normativa prioriza a data da ultima assinatura.' 76 458 810 17 12 $C.white $true 'Aptos' | Out-Null

# 11 - conclusao para apresentacao oral
$sConclusion = New-Slide 'Conclusao dos resultados' 'A POC demonstra viabilidade, ainda nao generalizacao' 'Os resultados apoiam o uso como ferramenta de apoio; a validacao precisa crescer antes de conclusoes amplas.'
Add-Box $sConclusion 54 166 258 192 $C.white -1 8 | Out-Null
Add-Box $sConclusion 54 166 258 7 $C.teal | Out-Null
Add-Text $sConclusion 'O QUE FUNCIONOU' 78 188 208 20 12 $C.teal $true 'Aptos' | Out-Null
Add-Text $sConclusion 'Pipeline reproduzivel\n82/82 nativos elegiveis com texto\nRegras e resultados auditaveis' 78 224 208 92 17 $C.navy $true 'Aptos Display' | Out-Null

Add-Box $sConclusion 350 166 258 192 $C.white -1 8 | Out-Null
Add-Box $sConclusion 350 166 258 7 $C.orange | Out-Null
Add-Text $sConclusion 'O QUE OS MODELOS MOSTRAM' 374 188 210 20 12 $C.orange $true 'Aptos' | Out-Null
Add-Text $sConclusion 'SVM: maior F1 supervisionado (0,530)\nBaseline: F1 macro 0,594\nAccuracy do baseline varia por processo' 374 224 210 106 16 $C.navy $true 'Aptos Display' | Out-Null

Add-Box $sConclusion 646 166 260 192 $C.white -1 8 | Out-Null
Add-Box $sConclusion 646 166 260 7 $C.red | Out-Null
Add-Text $sConclusion 'VARIACAO E CAUTELA' 670 188 210 20 12 $C.red $true 'Aptos' | Out-Null
Add-Text $sConclusion 'Baseline por processo: 0,364 a 0,741\nSomente 5 processos\nClasses muito desbalanceadas' 670 224 210 92 16 $C.navy $true 'Aptos Display' | Out-Null

Add-Box $sConclusion 54 382 852 82 $C.navy -1 8 | Out-Null
Add-Text $sConclusion 'Antes de afirmar desempenho geral' 78 398 790 20 13 $C.yellow $true 'Aptos' | Out-Null
Add-Text $sConclusion 'Reconciliar o gabarito: 113 linhas, 82 documentos unicos, 109 registros avaliaveis e 4 pendentes. A avaliacao quantitativa atual cobre classificacao e extracao; fluxo e prazos ainda precisam de validacao.' 78 425 790 35 14 $C.white $false 'Aptos' | Out-Null

# Evidencias visuais e aprofundamento das fases CRISP-DM
$imgDir = Join-Path (Get-Location) 'output\20260927'

$sProfile = New-Slide '02  Data Understanding' 'Perfil do corpus: tres universos de analise' 'Os numeros nao representam a mesma coisa: autos, documentos nativos e registros de avaliacao.'
Add-Box $sProfile 54 164 405 230 $C.white -1 8 | Out-Null
Add-Text $sProfile 'AUTOS PUBLICOS' 80 185 250 20 12 $C.orange $true 'Aptos' | Out-Null
Add-Text $sProfile '224 documentos' 80 214 300 32 24 $C.navy $true 'Aptos Display' | Out-Null
Add-Text $sProfile 'Autos incluem atos, anexos e restritos.' 80 255 320 34 14 $C.ink $false 'Aptos' | Out-Null
Add-Text $sProfile 'TEXTO PARA ANALISE' 80 306 250 20 12 $C.teal $true 'Aptos' | Out-Null
Add-Text $sProfile ("82 nativos elegiveis" + [Environment]::NewLine + "82 com texto extraido") 80 334 330 42 16 $C.ink $true 'Aptos Display' | Out-Null
Add-Box $sProfile 501 164 405 230 $C.white -1 8 | Out-Null
Add-Text $sProfile 'ROTULAGEM E AVALIACAO' 527 185 330 20 12 $C.orange $true 'Aptos' | Out-Null
Add-Text $sProfile '82 documentos unicos' 527 216 340 28 20 $C.navy $true 'Aptos Display' | Out-Null
Add-Text $sProfile ("113 linhas de rotulagem" + [Environment]::NewLine + "4 pendentes; 109 registros avaliaveis") 527 258 340 48 14 $C.ink $false 'Aptos' | Out-Null
Add-Text $sProfile 'As 109 linhas avaliaveis nao sao 109 documentos unicos: o gabarito tem repeticoes que precisam ser reconciliadas.' 527 324 340 48 12 $C.red $true 'Aptos' | Out-Null
Add-Box $sProfile 54 416 852 52 $C.white -1 8 | Out-Null
Add-Text $sProfile 'Cobertura textual: 36,6% dos autos | 100% dos nativos elegiveis' 78 433 800 20 15 $C.navy $true 'Aptos' | Out-Null

$sClassDistribution = New-Slide '02  Data Understanding' 'Distribuicao das classes no conjunto rotulado' 'A classe majoritaria domina o corpus; as classes raras tornam a avaliacao mais exigente.'
Add-Picture $sClassDistribution (Join-Path $imgDir 'distribuicao_classes_gabarito_20260927.png') 64 150 832 320 | Out-Null
Add-Box $sClassDistribution 64 474 832 30 $C.white -1 8 | Out-Null
Add-Text $sClassDistribution 'Encaminhamento: 63/109 (58%) | Exigencia: 4/109 (4%) | O desbalanceamento torna a accuracy isolada insuficiente.' 80 481 800 17 13 $C.navy $true 'Aptos' | Out-Null

$sCoverage = New-Slide '02  Data Understanding' 'Cobertura de extracao: o que entra na analise' 'A ausencia de texto e uma propriedade do dado, nao um erro escondido.'
Add-Picture $sCoverage (Join-Path $imgDir 'cobertura_extracao_20260927.png') 54 158 570 300 | Out-Null
Add-Box $sCoverage 660 176 246 240 $C.navy -1 12 | Out-Null
Add-Text $sCoverage 'LEITURA DO GRAFICO' 684 201 190 20 12 $C.yellow $true 'Aptos' | Out-Null
Add-Text $sCoverage ("224 documentos nos autos" + [Environment]::NewLine + "82 nativos com texto" + [Environment]::NewLine + "121 anexos" + [Environment]::NewLine + "18 restritos") 684 239 190 112 16 $C.white $true 'Aptos Display' | Out-Null
Add-Text $sCoverage 'Cobertura: 36,6% do total | 100% dos nativos elegiveis' 54 474 800 20 15 $C.red $true 'Aptos' | Out-Null

$sPrepTech = New-Slide '03  Data Preparation' 'Text Mining: preparar texto sem apagar seu significado' 'As escolhas priorizam atributos comparaveis, auditaveis e interpretaveis para um corpus pequeno.'
Add-Card $sPrepTech 54 166 260 252 'Normalizar' 'stringi + stringr

Converter para minusculas, limpar controles e uniformizar espacos. Acentos sao preservados para manter palavras legiveis e fieis ao portugues.' $C.teal
Add-Card $sPrepTech 350 166 260 252 'Reduzir ruido' 'tidytext + stopwords

Remover stopwords PT-BR e termos de dominio configuraveis. Nomes de signatarios viram stopwords dinamicas; datas e assinaturas sao preservadas em campos separados.' $C.orange
Add-Card $sPrepTech 646 166 260 252 'Criar atributos' 'TF-IDF, sem stemming

Tokenizar palavras, descartar numeros isolados e tokens curtos. TF-IDF valoriza termos distintivos; sem stemming, os termos continuam compreensiveis e comparaveis ao dicionario.' $C.navy
Add-Box $sPrepTech 54 440 852 40 $C.white -1 8 | Out-Null
Add-Text $sPrepTech 'Datas sao extraidas em formatos numerico e por extenso (lubridate/stringr); o rodape de assinatura e separado antes da analise lexical.' 76 452 810 19 13 $C.navy $true 'Aptos' | Out-Null

$sModelMetrics = New-Slide '04  Modeling' 'Comparacao de metricas dos modelos' 'A visao geral compara baseline, regressao logistica, Naive Bayes e SVM.'
Add-Picture $sModelMetrics (Join-Path $imgDir 'comparacao_metricas_modelos_20260927.png') 70 150 820 340 | Out-Null

$sModelF1 = New-Slide '04  Modeling' 'Efeito do balanceamento no F1' 'O balanceamento melhora a cobertura das classes minoritarias, mas pode reduzir a accuracy.'
Add-Picture $sModelF1 (Join-Path $imgDir 'comparacao_f1_condicao_20260927.png') 120 150 720 340 | Out-Null
Add-Text $sModelF1 'Melhor F1 macro: SVM = 0,5302 | Logistica balanceada = 0,4718 | Naive Bayes balanceado = 0,4091' 70 465 820 18 13 $C.navy $true 'Aptos' | Out-Null

$sEvalAccuracy = New-Slide '05  Evaluation' 'Variacao da accuracy entre processos' 'Cada painel representa um modelo; cada grupo compara balanceamento no mesmo processo.'
Add-Picture $sEvalAccuracy (Join-Path $imgDir 'accuracy_por_processo_20260927.png') 70 150 820 340 | Out-Null

$sEvalMatrix = New-Slide '05  Evaluation' 'Matriz de confusao do baseline' 'A diagonal representa acertos; os demais quadrantes mostram as confusoes entre classes.'
Add-Picture $sEvalMatrix (Join-Path $imgDir 'matriz_confusao_baseline_20260927.png') 150 145 660 350 | Out-Null
Add-Text $sEvalMatrix 'Baseline: accuracy 0,5960 | precision macro 0,7200 | recall macro 0,6220 | F1 macro 0,5940' 70 465 820 18 13 $C.navy $true 'Aptos' | Out-Null

$sGraph = New-Slide 'Grafo de tramitacao | 003836' 'Tres demandas, tres respostas no grafo gerado' 'Recorte das arestas semanticas de prazo do processo 01450.003836/2026-78.'
$rowY = @(164, 270, 376)
$leftLabels = @(
  "Despacho 5781  |  CGM`n28/04/2026",
  "Despacho 8365  |  CGM`n10/06/2026",
  "Despacho 11794  |  CGM`n06/08/2026"
)
$rightLabels = @(
  "Parecer PAIPA Arq  |  08/05`nSEI 7384861",
  "Parecer Tecnico  |  12/06`nSEI 7519156",
  "Parecer RAIPA Arq  |  31/08`nSEI 7790244"
)
$deadlines = @('Limite 23/05', 'Limite 23/06', 'Limite 30/08')
$statusLabels = @('NO PRAZO', 'NO PRAZO', '1 DIA ATRASO')
$statusColors = @($C.green, $C.green, $C.red)
for ($i = 0; $i -lt 3; $i++) {
  $y = $rowY[$i]
  Add-Box $sGraph 54 $y 300 68 $C.white $C.teal 6 | Out-Null
  Add-Text $sGraph $leftLabels[$i] 72 ($y + 11) 265 48 14 $C.navy $true 'Aptos' | Out-Null
  $edge = Add-Line $sGraph 354 ($y + 34) 435 ($y + 34) $C.teal 2
  $edge.Line.EndArrowheadStyle = 2
  Add-Text $sGraph $deadlines[$i] 357 ($y + 7) 78 18 10 $C.muted $true 'Aptos' | Out-Null
  Add-Box $sGraph 440 $y 286 68 $C.white $C.navy 6 | Out-Null
  Add-Text $sGraph $rightLabels[$i] 458 ($y + 11) 250 48 13 $C.navy $true 'Aptos' | Out-Null
  Add-Box $sGraph 746 ($y + 14) 160 40 $statusColors[$i] -1 8 | Out-Null
  Add-Text $sGraph $statusLabels[$i] 756 ($y + 26) 140 16 11 $C.white $true 'Aptos' | Out-Null
}
Add-Text $sGraph 'O grafo completo do caso tem 19 documentos e 22 arestas; este recorte destaca as 3 arestas de prazo. Fonte: SVG de 27/09/2026.' 58 484 840 18 11 $C.muted $false 'Aptos' | Out-Null

# Posiciona os slides novos nas fases correspondentes.
$sProfile.MoveTo(5)
$sClassDistribution.MoveTo(6)
$sCoverage.MoveTo(7)
$sPrepTech.MoveTo(8)
$sModelMetrics.MoveTo(11)
$sModelF1.MoveTo(12)
$sEvalAccuracy.MoveTo(14)
$sEvalMatrix.MoveTo(15)
$sGraph.MoveTo(18)
$sConclusion.MoveTo(19)

$out = Join-Path (Get-Location) 'output\apresentacao_crisp_dm_iphan.pptx'
New-Item -ItemType Directory -Force -Path (Split-Path $out) | Out-Null
$pres.SaveAs($out, 24)
$count = $pres.Slides.Count
$pres.Close()
$pp.Quit()
[void][Runtime.InteropServices.Marshal]::ReleaseComObject($pres)
[void][Runtime.InteropServices.Marshal]::ReleaseComObject($pp)
Write-Output "Arquivo criado: $out"
Write-Output "Slides: $count"