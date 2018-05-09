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



adicionarIndicador <-
    function(listaDeDados,
             nomeIndicador,
             sigla,
             tituloIngles,
             tituloPortugues) {
        contListaDeDados <- length(listaDeDados[[1]]) + 1

        dadosBrutos <-
            ai(
                indicator = c(nomeIndicador),
                country = c(sigla),
                startdate = 1970,
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
        mediaIndicador = c()
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

        cont <- cont + 1
    }

    dadosTratados
}
