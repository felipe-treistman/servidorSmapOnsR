
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
 
## Dependências
 
O servidor atualmente tem apenas uma restrição de versão de dependências, introduzida por atualizações recentes do pacote `future` que alteraram algumas funções que são chamadas. Ambientes que foram montados para versões do `servidorSmapOnsR` iguais ou anteriores à `1.0.0` devem garantir a versão correta dessa dependência. A versão necessária é `<= 1.34.0`:
 
```r
remotes::install_version("future", "<= 1.34.0")
```
 