# Análise de Tendência da Sinistralidade em Municípios

## Sobre

Esse repositório contém o código de cálculo e renderização da nota técnica de análise da tendência dos óbitos e sinistros nos municípios do Estado de SP

## Estrutura

O projeto está organizado como um **Quarto Book**. O arquivo `_quarto.yml` contém a configuração do book. Os capítulos estão na raiz do projeto:

- `index.qmd` - Página inicial do book (contém setup e configurações)
- `01-introducao.qmd` - Introdução
- `02-metodologia.qmd` - Metodologia
- `03-resultados.qmd` - Resultados
- `04-conclusao.qmd` - Conclusão
- `05-referencias.qmd` - Referências

Os scripts em `R/` apresentam todas as funções utilizadas para os cálculos. Em `data/` estão parte dos dados utilizados e exportados

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

3. Renderização do book com o [Quarto](https://quarto.org/):

```
quarto render
```

O comando `quarto render` irá renderizar todo o book conforme configurado no arquivo `_quarto.yml`. O resultado será gerado no diretório `_book/`.