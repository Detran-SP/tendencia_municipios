# Análise de Tendência da Sinistralidade em Municípios

## Sobre

Esse repositório contém o código de cálculo e renderização da nota técnica de análise da tendência dos óbitos e sinistros nos municípios do Estado de SP

## Estrutura

O arquivo `index.qmd` apresenta o conteúdo do relatório. Os scripts em `R/` apresentam todas as funções utilizadas para os cálculos. Em `data/` estão parte dos dados utilizados e exportados

## Requisitos

- R >= 4.3
- quarto >= 1.7

## Execução

1. Instalação das dependências do projeto através do pacote `{renv}`:

```r
renv::restore()
```

2. Execução do pipeline com `{targets}`

```r
targets::tar_make()
```

3. Renderização do relatório com o [Quarto Markdown](https://quarto.org/):

```
quarto render index.qmd
```