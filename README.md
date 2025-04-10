# Nota Técnica 01 - Análise de Tendência da Sinistralidade em Municípios

## Sobre

Esse repositório contém o código de cálculo e renderização da nota técnica de análise da tendência dos óbitos e sinistros nos municípios do Estado de SP

## Estrutura

O arquivo `main.qmd` apresenta o conteúdo do relatório. Os scripts em `R/` apresentam todas as funções utilizadas para os cálculos. Em `data/` estão parte dos dados utilizados e exportados

## Execução

1. Instalação do `{renv}` e das dependências do projeto:

```r
install.packages("renv")
renv::init()
renv::restore()
```

2. Execução do pipeline com `{targets}`

```r
targets::tar_make()
```
