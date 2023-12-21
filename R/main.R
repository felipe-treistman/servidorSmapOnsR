#' Funcao de criacao do rest api
#' 
#' @export

cria_servidor <- function() {
    PORT <- as.numeric(Sys.getenv("PORT", unset = "8080"))
    HOST <- Sys.getenv("HOST", unset = "0.0.0.0")

    future::plan("multisession")

    plumber::pr("R/plumber.R") %>%
        plumber::pr_run(host = HOST, port = PORT, docs = FALSE, quiet = TRUE)
}