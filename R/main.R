#' Funcao de criacao do rest api
#' 
#' @export

cria_servidor <- function() {
    library(plumber)
    
    PORT <- as.numeric(Sys.getenv("PORT", unset = "8080"))
    HOST <- Sys.getenv("HOST", unset = "0.0.0.0")

    future::plan("multisession")

    pr(system.file("R", "plumber.R", package = "servidorSmapOnsR")) %>%
        pr_run(host = HOST, port = PORT, docs = FALSE, quiet = TRUE)
}