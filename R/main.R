library(plumber)

PORT <- as.numeric(Sys.getenv("PORT", unset = "8080"))
HOST <- Sys.getenv("HOST", unset = "0.0.0.0")

pr("plumber.R") %>%
    pr_run(host = HOST, port = PORT, docs = FALSE, quiet = TRUE)
