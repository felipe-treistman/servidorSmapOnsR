<!-- README.md is generated from README.Rmd. Please edit that file -->

# servidorSmapOnsR

Rest API do smapOnsR

## Instalação

Os seguintes pacotes devem ser instalados:

```r
install.packages("devtools")
install.packages("plumber")
install.packages("httr")
install.packages("future")
devtools::install_github("ONSBR/smapOnsR")
```

Para instalar uma versão específica de algum pacote oficial, pode ser usada a função `remotes::install_version`. Já para instalar uma versão específica do SMAP, por exemplo a `v1.3.2`, pode ser feito:

```r
devtools::install_github("ONSBR/smapOnsR@v1.3.2")
```

Exemplo de como executar o servidor

```{r example}
# chamada a ser realizada no terminal
# run_main.bat
```
