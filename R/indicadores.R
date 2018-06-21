criarListaDeDados <- function() {
    listaDeDados <- list(
        indicador = c(),
        dadosBrutos = c(),
        dadosTratados = c(),
        tituloIngles = c(),
        tituloPortugues = c()
    )
    listaDeDados
}



criarlistaPadroesDeDados <- function() {
    listaPadroesDeDados <- list(
        indicadorchave = c(),
        indicadorComparacao = c(),
        anosDeDiferenca = c(),
        dadosAnalisePadrao = c()
    )
    listaPadroesDeDados
}



adicionarIndicador <-
    function(listaDeDados,
             nomeIndicador,
             sigla,
             tituloIngles,
             tituloPortugues) {
        contListaDeDados <- length(listaDeDados[[1]]) + 1

        ###########TODO
        dadosBrutos <-
            ai(
                indicator = c(nomeIndicador),
                country = c(sigla),
                startdate = 2000,
                enddate = 2017
            )

        dadosBrutos <- dadosBrutos[dim(dadosBrutos)[1]:1,]
        dadosBrutos <- dadosBrutos[, 3:5]

        listaDeDados$indicador[[contListaDeDados]] <- nomeIndicador
        listaDeDados$dadosBrutos[[contListaDeDados]] <- dadosBrutos
        listaDeDados$dadosTratados[[contListaDeDados]] <-
            processarDados(dadosBrutos)

        listaDeDados$tituloIngles[[contListaDeDados]] <-
            tituloIngles
        listaDeDados$tituloPortugues[[contListaDeDados]] <-
            tituloPortugues

        # print("listaDeDados$indicador[contListaDeDados]")
        # # print(listaDeDados$indicador)
        # # print(listaDeDados$indicador[contListaDeDados])
        # print(listaDeDados$indicador[[contListaDeDados]])
        #
        # print("listaDeDados$dadosBrutos[contListaDeDados]")
        # # print(listaDeDados$dadosBrutos)
        # print(listaDeDados$dadosBrutos[[contListaDeDados]])
        #
        # print("listaDeDados$dadosTratados[contListaDeDados]")
        # # print(listaDeDados$dadosTratados)
        # print(listaDeDados$dadosTratados[[contListaDeDados]])


        contListaDeDados <- contListaDeDados + 1
        listaDeDados
    }



processarDados <- function(dadosBrutos) {
    cont <- 1
    dadosTratados <- list(
        primeiroAno = c(),
        segundoAno = c(),
        indicadorNoPrimeiroAno = c(),
        indicadorNoSegundoAno = c(),
        diferencaIndicador = c(),
        mediaIndicador = c(),
        normalizacaoDiferenca = c()
        # mediaIndicador3Anos = c()
    )

    while (cont < nrow(dadosBrutos)) {
        # print("cont!!!!!!!!!!!!!!!!!")
        # print(cont)
        # print(nrow(dadosBrutos))
        # print("cont!!!!!!!!!!!!!!!!!")
        while (is.na(dadosBrutos[cont, 1])
               | is.na(dadosBrutos[cont + 1, 1])
               | is.na(dadosBrutos[cont, 3])
               | is.na(dadosBrutos[cont + 1, 3])) {
            cont <- cont + 1
        }

        ano <- dadosBrutos[cont, 1]
        # print(dadosBrutos[cont, 1])
        # print(dadosBrutos[cont+1, 1])
        # print(dadosBrutos[cont, 3])
        # print(dadosBrutos[cont+1, 3])

        dadosTratados$primeiroAno[cont] <- ano
        dadosTratados$segundoAno[cont] <- ano + 1
        dadosTratados$indicadorNoPrimeiroAno[cont] <-
            dadosBrutos$value[cont]
        dadosTratados$indicadorNoSegundoAno[cont] <-
            dadosBrutos$value[cont + 1]
        dadosTratados$diferencaIndicador[cont] <-
            dadosTratados$indicadorNoPrimeiroAno[cont] - dadosTratados$indicadorNoSegundoAno[cont]
        dadosTratados$mediaIndicador[cont] <-
            (dadosTratados$indicadorNoPrimeiroAno[cont] + dadosTratados$indicadorNoSegundoAno[cont])/2

        # dadosTratados$mediaIndicador3Anos[cont] <-
        #     (dadosTratados$indicadorNoPrimeiroAno[cont] + dadosTratados$indicadorNoSegundoAno[cont] + dadosTratados$indicadorNoPrimeiroAno[cont+2])/3

        cont <- cont + 1
    }

    cont <- 1
    min <- min(dadosTratados$indicadorNoPrimeiroAno)
    max <- max(dadosTratados$indicadorNoPrimeiroAno)

    intervalo <- max - min

    while (cont <= length(dadosTratados$indicadorNoPrimeiroAno)) {
        dadosTratados$normalizacaoDiferenca[cont] = ((dadosTratados$indicadorNoPrimeiroAno[cont] - min) / intervalo) * 100
        cont <- cont + 1
    }

    dadosTratados
}



verificarPadrao1x1 <- function(listaDeDados, listaIndicadoresChave, listaIndicadoresDeComparacao, listaPadroesDeDados, anosDeDiferenca) {
    cont <<- length(listaPadroesDeDados[[1]]) + 1

    contListaIndChave <- 1
    while (contListaIndChave <= length(listaIndicadoresChave)) {
        contListaIndComp <- 1
        while (contListaIndComp <= length(listaIndicadoresDeComparacao)) {
            print("COMPARACAO !!!!!")
            print("contListaIndComp")
            print(contListaIndComp)
            print("contListaIndChave")
            print(contListaIndChave)

            # aaaa <<- listaIndicadoresChave[[contListaIndChave]]
            # bbb <<- listaDeDados[[listaIndicadoresChave[[contListaIndChave]]]]
            # bbb111111 <<- listaDeDados[[1]]
            # bbb222222 <<- listaDeDados[[2]]
            # bbb3333333 <<- listaDeDados[[3]]

            # bbb9999999 <<- listaDeDados$indicador[[listaIndicadoresChave[[contListaIndChave]]]]

            # aaa <<- listaDeDados$indicador[[listaIndicadoresChave[[contListaIndChave]]]]
            # bbbb <<- listaDeDados$indicador[[listaIndicadoresDeComparacao[[contListaIndComp]]]]

            listaPadroesDeDados$indicadorchave[[cont]] <- listaDeDados$indicador[[listaIndicadoresChave[[contListaIndChave]]]]
            listaPadroesDeDados$indicadorComparacao[[cont]] <- listaDeDados$indicador[[listaIndicadoresDeComparacao[[contListaIndComp]]]]
            listaPadroesDeDados$anosDeDiferenca[[cont]] <- anosDeDiferenca
            listaPadroesDeDados$dadosAnalisePadrao[[cont]] <- list(periodo = c(), padrao = c())

            print("####################################")
            print("####################################")
            print("indicadorchave x indicadorComparacao")
            print(listaPadroesDeDados$indicadorchave[[cont]])
            print(listaPadroesDeDados$indicadorComparacao[[cont]])

            # print(length(listaDeDados$dadosTratados[[listaIndicadoresChave[[contListaIndChave]]]]))
            # print(listaDeDados$dadosTratados[[listaIndicadoresChave[[contListaIndChave]]]])
            # print(listaDeDados$dadosTratados[[listaIndicadoresChave[[contListaIndChave]]]]$primeiroAno)
            # print(listaDeDados$dadosTratados[[listaIndicadoresChave[[contListaIndChave]]]]$primeiroAno[[1]])
            # print(length(listaDeDados$dadosTratados[[listaIndicadoresChave[[contListaIndChave]]]]$primeiroAno))
            # print(length(listaDeDados$dadosTratados[[listaIndicadoresDeComparacao[[contListaIndComp]]]]$primeiroAno[[1]]))
            # print(length(listaDeDados$dadosTratados[[listaIndicadoresChave[[contListaIndChave]]]]$primeiroAno[[1]]))

            contIndChave <- 1
            contIndComp <- 1
            while (contIndChave < length(listaDeDados$dadosTratados[[listaIndicadoresChave[[contListaIndChave]]]]$primeiroAno) ||
                   contIndComp < length(listaDeDados$dadosTratados[[listaIndicadoresDeComparacao[[contListaIndComp]]]]$primeiroAno)) {

                anoIndChave <- listaDeDados$dadosTratados[[listaIndicadoresChave[[contListaIndChave]]]]$primeiroAno[[contIndChave]]
                anoIndComp <- listaDeDados$dadosTratados[[listaIndicadoresDeComparacao[[contListaIndComp]]]]$primeiroAno[[contIndComp]]

                contIndCompSTOP <- FALSE
                contIndChaveSTOP <- FALSE
                while (anoIndChave != (anoIndComp + anosDeDiferenca)) {

                    print("WHILE 11111111")
                    print("anoIndChave")
                    print(anoIndChave)
                    print("anoIndComp")
                    print(anoIndComp)

                    if (anoIndChave > (anoIndComp + anosDeDiferenca)) {
                        if ((contIndComp + 1) >= length(listaDeDados$dadosTratados[[listaIndicadoresDeComparacao[[contListaIndComp]]]]$primeiroAno)
                            || is.na(listaDeDados$dadosTratados[[listaIndicadoresDeComparacao[[contListaIndComp]]]]$primeiroAno[[contIndComp]])
                            || is.null(listaDeDados$dadosTratados[[listaIndicadoresDeComparacao[[contListaIndComp]]]]$primeiroAno[[contIndComp]])) {
                            contIndCompSTOP <- TRUE
                        } else {
                            contIndComp <- contIndComp + 1
                            anoIndComp <- listaDeDados$dadosTratados[[listaIndicadoresDeComparacao[[contListaIndComp]]]]$primeiroAno[[contIndComp]]
                        }
                    } else {
                        if ((contIndChave + 1) >= length(listaDeDados$dadosTratados[[listaIndicadoresChave[[contListaIndChave]]]]$primeiroAno)
                            || is.na(listaDeDados$dadosTratados[[listaIndicadoresChave[[contListaIndChave]]]]$primeiroAno[[contIndChave]])
                            || is.null(listaDeDados$dadosTratados[[listaIndicadoresChave[[contListaIndChave]]]]$primeiroAno[[contIndChave]])) {
                            contIndChaveSTOP <- TRUE
                        } else {
                            contIndChave <- contIndChave + 1
                            anoIndChave <- listaDeDados$dadosTratados[[listaIndicadoresChave[[contListaIndChave]]]]$primeiroAno[[contIndChave]]
                        }
                    }

                    if (contIndCompSTOP || contIndChaveSTOP) {
                        print("ERROOOOOOOOOOOOO")
                        break
                    }
                    print("WHILE 222222222")
                    print("anoIndChave")
                    print(anoIndChave)
                    print("anoIndComp")
                    print(anoIndComp)
                }
                if (contIndCompSTOP || contIndChaveSTOP) {
                    print("ERROOOOOOOOOOOOO222")
                    break
                }



                print(">>>>>>> anoIndChave")
                print(anoIndChave)
                print(">>>>>>> anoIndComp")
                print(anoIndComp)



                listaPadroesDeDados$dadosAnalisePadrao[[cont]]$periodo[[contIndComp]] <- paste(anoIndChave, anoIndComp, sep="/")

                # print('listaPadroesDeDados$dadosAnalisePadrao[[cont]]')
                # print(listaPadroesDeDados$dadosAnalisePadrao[[cont]])
                #
                # print('listaPadroesDeDados$dadosAnalisePadrao[[cont]]$periodo[[contIndComp]]')
                # print(listaPadroesDeDados$dadosAnalisePadrao[[cont]]$periodo[[contIndComp]])
                #
                # print('diferencaIndicador[[contIndChave]]')
                # print(listaDeDados$dadosTratados[[listaIndicadoresChave[[contListaIndChave]]]]$diferencaIndicador[[contIndChave]])
                # print('diferencaIndicador[[contIndComp]]')
                # print(listaDeDados$dadosTratados[[listaIndicadoresDeComparacao[[contListaIndComp]]]]$diferencaIndicador[[contIndComp]])


                if (listaDeDados$dadosTratados[[listaIndicadoresChave[[contListaIndChave]]]]$diferencaIndicador[[contIndChave]] > 0
                    & listaDeDados$dadosTratados[[listaIndicadoresDeComparacao[[contListaIndComp]]]]$diferencaIndicador[[contIndComp]] > 0) {
                    listaPadroesDeDados$dadosAnalisePadrao[[cont]]$padrao[[contIndComp]] <- TRUE

                } else if (listaDeDados$dadosTratados[[listaIndicadoresChave[[contListaIndChave]]]]$diferencaIndicador[[contIndChave]] < 0
                           & listaDeDados$dadosTratados[[listaIndicadoresDeComparacao[[contListaIndComp]]]]$diferencaIndicador[[contIndComp]] < 0) {
                    listaPadroesDeDados$dadosAnalisePadrao[[cont]]$padrao[[contIndComp]] <- TRUE
                } else {
                    listaPadroesDeDados$dadosAnalisePadrao[[cont]]$padrao[[contIndComp]] <- FALSE
                }

                print("----------------------")
                if (contIndChave < length(listaDeDados$dadosTratados[[listaIndicadoresChave[[contListaIndChave]]]]$primeiroAno)) {
                    contIndChave <- contIndChave + 1
                    print("contIndChave")
                    print(contIndChave)
                }
                if (contIndComp < length(listaDeDados$dadosTratados[[listaIndicadoresDeComparacao[[contListaIndComp]]]]$primeiroAno)) {
                    contIndComp <- contIndComp + 1
                    print("contIndComp")
                    print(contIndComp)
                }
                print("----------------------")
            }
            contListaIndComp <- contListaIndComp + 1
            cont <- cont + 1
        }
        contListaIndChave <- contListaIndChave + 1
    }

    print("SAIU!!!!!!!!!!")
    listaPadroesDeDados
}





verificarPadrao1xN <- function(listaDeDados, listaIndicadoresChave, listaIndicadoresDeComparacao, listaPadroesDeDados, anosDeDiferenca) {
    cont <<- length(listaPadroesDeDados[[1]]) + 1

    contListaIndChave <- 1
    while (contListaIndChave <= length(listaIndicadoresChave)) {
        contListaIndComp <- 1

        listaPadroesDeDados$indicadorchave[[cont]] <- listaDeDados$indicador[[listaIndicadoresChave[[contListaIndChave]]]]
        listaPadroesDeDados$indicadorComparacao[[cont]] <- ""
        listaPadroesDeDados$anosDeDiferenca[[cont]] <- anosDeDiferenca
        listaPadroesDeDados$dadosAnalisePadrao[[cont]] <- list(periodo = c(), indicadorNormalizado = c(), padrao = c())

        while (contListaIndComp <= length(listaIndicadoresDeComparacao)) {
            # if (!is.na(listaDeDados$dadosTratados[[listaIndicadoresDeComparacao[[contListaIndComp]]]]$normalizacaoDiferenca[[contIndComp]])) {
            #     listaPadroesDeDados$indicadorComparacao[[cont]] <- paste(listaPadroesDeDados$indicadorComparacao[[cont]], listaDeDados$indicador[[listaIndicadoresDeComparacao[[contListaIndComp]]]], sep=", ")
            #     print('listaPadroesDeDados$indicadorComparacao[[cont]]')
            #     print(listaPadroesDeDados$indicadorComparacao[[cont]])
            #
            #     listaPadroesDeDados$dadosAnalisePadrao[[cont]]$indicadorNormalizado[[contListaIndComp]] <- listaDeDados$dadosTratados[[listaIndicadoresDeComparacao[[contListaIndComp]]]]$normalizacaoDiferenca[[contIndComp]]
            # }

            contListaIndComp <- contListaIndComp + 1
            cont <- cont + 1
        }
        contListaIndChave <- contListaIndChave + 1
    }


    print("SAIU!!!!!!!!!!")
    listaPadroesDeDados
}
