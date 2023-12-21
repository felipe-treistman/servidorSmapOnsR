# plumber.R

future::plan("multisession")

# TODO - esta forma está bem ruim, podemos criar uma classe "Rodada"
# e uma classe "Rodadas" para encapsular tudo isso.
columns <- c("idSGPV", "dirBase", "urlCallback", "pid")
database <- data.frame(matrix(nrow = 0, ncol = length(columns)))
colnames(database) <- columns

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
                msg = "Diretorio em execucao",
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
            fut$workers[[fut$node]]$session_info$process$pid
        )
        execucao <- list(
            cod = "0",
            msg = "requisicao recebida com sucesso",
            idSGPV = id
        )
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
        }
    # 2 - Se o JSON está mal formatado
        if (!is.character(id)) {
            sucesso <- FALSE
            execucao <- list(
                cod = "-3",
                msg = paste0("Erro ao decodificar mensagem: idSGPV = ", as.character(id)),
                idSGPV = id
            )
        }
    } else {
    # 1 - Se nao existe uma rodada com o mesmo idSGPV
        sucesso <- FALSE
        abortar <- list(
            cod = "-4",
            msg = paste0("idSGPV ", id, " nao encontrado"),
            idSGPV = id
        )
    }
    
    if (sucesso) {
        # TODO - abortar a thread que está em execução paralela
        id <- req$argsBody$idSGPV
        stop_future(database[database$idSGPV == id, "pid"])

        # Registra que não existe mais o ID
        deleta_rodada(id)
        abortar <- list(
            cod = "0",
            msg = "requisicao recebida com sucesso",
            idSGPV = id
        )
    }

    abortar
}

#* Limpar uma rodada finalizada do SMAP
#* @post /limpar
#* @serializer unboxedJSON
function(req, res) {
    id <- req$argsBody$idSGPV

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



# ==============================================================================
# Funções auxiliares, para teste e debug


#* Endpoint de teste para o callback do servidor
#* @get /callback
#* @serializer unboxedJSON
function(req, res) {
    "sucesso"
}

#* Deletar uma rodadada localmente
#* @delete /deleta_rodada
deleta_rodada <- function(id) {
    # Registra que não existe mais o ID
    database <<- subset(database, idSGPV != id)
}

executa_smap <- function(id, diretorio_caso, url_callback) {
    # Simula uma rodada de SMAP
    if (dir.exists(diretorio_caso)) {
        try(smapOnsR::executa_caso_oficial(diretorio_caso))

        # Limpar a rodada da lista de rodadas
        PORT <- as.numeric(Sys.getenv("PORT"))
        httr::POST(paste0("http://localhost:", PORT,"/limpar"), body = list(idSGPV = id))

        # chamar o callback no final da execução
        httr::GET(url_callback)

        # Retorna qualquer coisa por enquanto
        saida <- "sucesso"
    } else {
        # Limpar a rodada da lista de rodadas
        PORT <- as.numeric(Sys.getenv("PORT"))

        httr::POST(paste0("http://localhost:", PORT,"/limpar"), body = list(idSGPV = id))
        saida <- "falha"

        # chamar o callback no final da execução
        httr::GET(url_callback)
    }
    saida
}

stop_future <- function(pid) {
    tools::pskill(pid, signal = tools::SIGTERM)
    tools::pskill(pid, signal = tools::SIGKILL)
}
