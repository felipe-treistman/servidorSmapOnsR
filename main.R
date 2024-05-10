library(plumber)

PORT <- as.numeric(Sys.getenv("PORT", unset = "8080"))
HOST <- Sys.getenv("HOST", unset = "0.0.0.0")
LOG <- Sys.getenv("LOG", unset = "log.txt")

pr("R/plumber.R") %>%
    pr_run(host = HOST, port = PORT, docs = FALSE, quiet = TRUE)
