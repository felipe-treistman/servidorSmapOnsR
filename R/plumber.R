# plumber.R

future::plan("multisession")

log <- function(msg) {
    write.table(paste0("[", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "]  [SMAP/ONS] [RSERV] ", msg),
        file = LOG, quote = FALSE, row.names = FALSE, col.names = FALSE, append = TRUE
    )
}

# TODO - esta forma está bem ruim, podemos criar uma classe "Rodada"
# e uma classe "Rodadas" para encapsular tudo isso.
columns <- c("idSGPV", "dirBase", "urlCallback", "pid")
database <- data.frame(matrix(nrow = 0, ncol = length(columns)))
colnames(database) <- columns


log(paste0("Iniciando servico web na porta ", PORT))

#* Log some information about the incoming request
#* @filter logger
function(req) {
    cat(
        as.character(Sys.time()), "-",
        req$REQUEST_METHOD, req$PATH_INFO, "-",
        req$HTTP_USER_AGENT, "@", req$REMOTE_ADDR, "\n"
    )
    plumber::forward()
}


#* Lista as rodadas do SMAP existentes
#* @get /rodadas
#* @serializer unboxedJSON
function(req, res) {
    n <- nrow(database)
    lista_rodadas <- list()
    lista_rodadas["processos"] <- n
    lista_rodadas["tamanho_fila"] <- n
    if (n > 0) {
        for (row in 1:n) {
            id <- database[row, "idSGPV"]
            dir_base <- database[row, "dirBase"]
            lista_rodadas[dir_base] <- as.character(id)
        }
    }
    msg <- paste0(
        "Processos ", lista_rodadas$processos,
        " Tamanho da fila: ", lista_rodadas$tamanho_fila
    )
    log(msg)
    lista_rodadas
}

#* Agenda uma nova rodada do SMAP
#* @post /executar
#* @serializer unboxedJSON
function(req, res) {
    # TODO - validar o que foi recebido e retornar erro se for o caso
    # Validações principais:

    id <- req$argsBody$idSGPV
    diretorio_caso <- req$argsBody$dirBase
    url_callback <- req$argsBody$urlCallback

    sucesso <- TRUE

    if (nrow(database) > 0) {
        # 1 - Se existe já uma rodada com o mesmo idSGPV
        if (any(id == database$idSGPV)) {
            sucesso <- FALSE
            execucao <- list(
                cod = "-1",
                msg = "ID ja esta em execucao",
                idSGPV = id
            )
        }

        # 2 - Se o dirBase que foi passado já pertence a alguma rodada
        if (any(diretorio_caso == database$dirBase)) {
            sucesso <- FALSE
            execucao <- list(
                cod = "-2",
                msg = paste0("Diretorio em execucao pelo idSGPV=", database$idSGPV[database$dirBase == diretorio_caso]),
                idSGPV = id
            )
        }

        # 4 - Se o JSON está mal formatado
        if (!is.character(id) | !is.character(diretorio_caso)) {
            sucesso <- FALSE
            execucao <- list(
                cod = "-3",
                msg = paste0("Erro ao decodificar mensagem: idSGPV = ", as.character(id), " / dirBase = ", diretorio_caso),
                idSGPV = id
            )
        }
        log(paste0(
            "[REQ] [ERRO] Enviando resposta sincrona ao SGPV: ",
            "{'cod': ", execucao$cod, " 'msg': ", execucao$msg, "' idSGPV': ", execucao$idSGPV, "}"
        ))
    }

    if (sucesso) {
        # Dispara o processo de execução da rodada usando
        # uma thread separada
        fut <- future::future({
            executa_smap(id, diretorio_caso, url_callback)
        })

        # Registra o que está em execução
        database[nrow(database) + 1, ] <<- list(
            id,
            diretorio_caso,
            url_callback,
            pid = fut$workers[[fut$node]]$session_info$process$pid
        )
        execucao <- list(
            cod = "0",
            msg = "requisicao recebida com sucesso",
            idSGPV = id
        )
        log(paste0(
            "[REQ] Enviando resposta sincrona ao SGPV: ",
            "{'cod': ", execucao$cod, "'msg': ", execucao$msg, "'idSGPV': ", execucao$idSGPV, "}"
        ))
    }

    execucao
}

#* Abortar uma rodada existente do SMAP
#* @post /abortar
#* @serializer unboxedJSON
function(req, res) {
    id <- req$argsBody$idSGPV
    sucesso <- TRUE
    # Validações principais:
    if (nrow(database) > 0) {
        # 1 - Se nao existe uma rodada com o mesmo idSGPV
        if (!any(id == database$idSGPV)) {
            sucesso <- FALSE
            abortar <- list(
                cod = "-4",
                msg = paste0("idSGPV ", id, " nao encontrado"),
                idSGPV = id
            )
            msg <- paste0("[ERRO] [ABORTAR] idSGPV ", abortar$idSGPV, " nao encontrado")
        }
        # 2 - Se o JSON está mal formatado
        if (!is.character(id)) {
            sucesso <- FALSE
            execucao <- list(
                cod = "-3",
                msg = paste0("Erro ao decodificar mensagem: idSGPV = ", as.character(id)),
                idSGPV = id
            )
            msg <- paste0("[ERRO] [ABORTAR] Erro ao decodificar mensagem: idSGPV = ", as.character(id))
        }
    } else {
        # 1 - Se nao existe uma rodada com o mesmo idSGPV
        sucesso <- FALSE
        abortar <- list(
            cod = "-4",
            msg = paste0("idSGPV ", id, " nao encontrado"),
            idSGPV = id
        )
        msg <- paste0("[ERRO] [ABORTAR] idSGPV ", abortar$idSGPV, " nao encontrado")
    }

    if (sucesso) {
        # TODO - abortar a thread que está em execução paralela
        id <- req$argsBody$idSGPV
        thread_id <- database[database$idSGPV == id, "pid"]
        stop_future(database[database$idSGPV == id, "pid"])

        # Registra que não existe mais o ID
        deleta_rodada(id)
        abortar <- list(
            cod = "0",
            msg = "requisicao recebida com sucesso",
            idSGPV = id
        )
        msg <- paste0("[ABORTAR] Thread ", thread_id, " abortada com sucesso")
    }
    log(msg)
    abortar
}

#* Limpar uma rodada finalizada do SMAP
#* @post /limpar
#* @serializer unboxedJSON
function(req, res) {
    # Registra que não existe mais o ID
    id <- req$argsBody$idSGPV
    deleta_rodada(id)
    limpar <- list(
        cod = "0",
        msg = "requisicao recebida com sucesso",
        idSGPV = id
    )

    limpar
}

#* Deletar uma rodadada localmente
#* @delete /deleta_rodada
deleta_rodada <- function(id) {
    # Registra que não existe mais o ID
    database <<- subset(database, idSGPV != id)
}

executa_smap <- function(id, diretorio_caso, url_callback) {
    # Simula uma rodada de SMAP
    log(paste0("[REQ] Requisicao recebida com sucesso IdSGPV=", id))
    thread_id <- Sys.getpid()
    error_cod <- 999
    error_cb <- function(cond) {
        log(paste0("[ERRO] [idSGPV=", id, "] ", conditionMessage(cond)))
        return(error_cod)
    }
    if (dir.exists(diretorio_caso)) {
        log(paste0(
            "ID SGPV ", id,
            " iniciando validacao de arquivos de entrada ", diretorio_caso
        ))
        cod <- tryCatch(smapOnsR::executa_caso_oficial(diretorio_caso), error = error_cb)
        if (is.null(cod)) {
            msg <- "sucesso"
            cod <- 0
        } else {
            msg <- "erro na execucao"
        }
        log(paste0(
            "[REDE] Enviando mensagem assincrona ao SGPV: ",
            "{'msg': '", msg, "', 'cod': ", cod, ", 'idSGPV': '", id, "'}"
        ))

        # chamar o callback no final da execução
        log(paste0("[REDE] Enviando callback ao SGPV: ", url_callback))
        res <- tryCatch(httr::POST(url_callback, body = list(idSGPV = id, msg = msg, cod = cod), encode = "json"), error = error_cb)
        if (is.numeric(res)) {
            log(paste0("[ERRO] [REDE] Erro ao enviar callback ao SGPV"))
        } else {
            log(paste0("[REDE] Status do callback ao SGPV: ", httr::status_code(res)))
        }

        # Limpar a rodada da lista de rodadas
        log(paste0("Thread ID ", thread_id, ": removendo idSGPV=", id, " da fila de execucao"))
        res <- tryCatch(httr::POST(paste0("http://", HOST, ":", PORT, "/limpar"), body = list(idSGPV = id)), error = error_cb)
        if (is.numeric(res)) {
            log(paste0("[ERRO] [REDE] Erro ao limpar a fila"))
        } else {
            log(paste0("[REDE] Status da limpeza da fila: ", httr::status_code(res)))
        }

        # Retorna qualquer coisa por enquanto
        saida <- msg
    } else {

        # chamar o callback no final da execução
        log(paste0("[REDE] Enviando callback ao SGPV: ", url_callback))
        res <- tryCatch(httr::POST(url_callback, body = list(idSGPV = id, msg = "diretorio nao encontrado", cod = 191), encode = "json"), error = error_cb)
        if (is.numeric(res)) {
            log(paste0("[ERRO] [REDE] Erro ao enviar callback ao SGPV"))
        } else {
            log(paste0("[REDE] Status do callback ao SGPV: ", httr::status_code(res)))
        }

        # Limpar a rodada da lista de rodadas
        log(paste0("Thread ID ", thread_id, ": removendo idSGPV=", id, " da fila de execucao"))
        res <- tryCatch(httr::POST(paste0("http://", HOST, ":", PORT, "/limpar"), body = list(idSGPV = id)), error = error_cb)
        if (is.numeric(res)) {
            log(paste0("[ERRO] [REDE] Erro ao limpar a fila"))
        } else {
            log(paste0("[REDE] Status da limpeza da fila: ", httr::status_code(res)))
        }

        # Retorna qualquer coisa por enquanto
        saida <- "falha"
    }
    log(paste0("Finalizando execucao da rodada idSGPV=", id))
    saida
}

stop_future <- function(pid) {
    tools::pskill(pid, signal = tools::SIGTERM)
    tools::pskill(pid, signal = tools::SIGKILL)
}
