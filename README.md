# Análise de Tendência da Sinistralidade em Municípios

## Sobre

Esse repositório contém o código de cálculo e renderização da nota técnica de análise da tendência dos óbitos e sinistros nos municípios do Estado de SP

## Estrutura

O arquivo `_quarto.yml` contém a configuração do relatório e o `_brand.yml` configura o estilo. Os capítulos estão na raiz do projeto:

- `index.qmd` - Página inicial
- `01-introducao.qmd` - Introdução
- `02-metodologia.qmd` - Metodologia
- `03-resultados.qmd` - Resultados
- `04-conclusao.qmd` - Conclusão
- `05-referencias.qmd` - Referências

Os scripts em `R/` apresentam todas as funções utilizadas para os cálculos. Em `data/` estão parte dos dados utilizados e exportados

## Requisitos

- [R](https://cran.r-project.org/) >= 4.3
- [quarto](https://quarto.org) >= 1.7

## Execução

1. Faça o download dos dados abertos do [Infosiga](https://infosiga.detran.sp.gov.br/rest/painel/download/file/dados_infosiga.zip) e insira na pasta `data` (`data/dados_infosiga.zip`)

2. Instalação das dependências do projeto através do pacote `{renv}`:

```r
renv::restore()
```

3. Execução do pipeline com `{targets}`

```r
targets::tar_make()
```

O relatório renderizado é exportado para em `docs/index.html`.