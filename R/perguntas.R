warnings()

# install.packages('devtools')
# install.packages('plotly')
# install_github('EL-BID/Libreria-R-Numeros-para-el-Desarrollo')
# install_github("arcuellar88/govdata360R")
# install_github('EL-BID/Agregador-de-indicadores')
library(devtools)
# library(agregadorindicadores)
library(plotly)


# PARA VER COMO A BIBLIOTECA FUNCIONA, ACESSAR:
# https://rdrr.io/github/EL-BID/Agregador-de-indicadores/f/README.md

# SE.XPD.TOTL.GD.ZS: Government expenditure on education, total (% of GDP)
# SE.XPD.TOTL.GB.ZS: Expenditure on education as % of total government expenditure (%)

# SE.XPD.TERT.ZS, Expenditure on tertiary as % of government expenditure on education (%)
# SE.XPD.TERT.PC.ZS, Government expenditure per tertiary student as % of GDP per capita (%)
# SE.XPD.SECO.ZS, Expenditure on secondary as % of government expenditure on education (%)
# SE.XPD.SECO.PC.ZS, Government expenditure per student, secondary (% of GDP per capita)
# SE.XPD.PRIM.ZS, Expenditure on primary as % of government expenditure on education (%)
# SE.XPD.PRIM.PC.ZS, Government expenditure per student, primary (% of GDP per capita)

# SE.XPD.CTOT.ZS, Current education expenditure, total (% of total expenditure in public institutions)
# SE.XPD.CTER.ZS, Current education expenditure, tertiary (% of total expenditure in tertiary public institutions)
# SE.XPD.CSEC.ZS, Current education expenditure, secondary (% of total expenditure in secondary public institutions)
# SE.XPD.CPRM.ZS, Current education expenditure, primary (% of total expenditure in primary public institutions)

# GB.XPD.RSDV.GD.ZS,Research and development expenditure (% of GDP)

# NY.GDP.PCAP.KD.ZG,GDP per capita growth (annual %)
# NY.GDP.MKTP.KD.ZG,GDP growth (annual %)



#########################################
#########################################
#########################################
primeiraPergunta <- function() {
    cat("NOME PERGUNTA: primeiraPergunta\n")
    cat("DADOS ANALISADOS: \n")
    cat("RESULTADO ESPERADO: \n")

    #dt_inicio <- grep(2000, names(dados))
    #dt_final <- grep(2017, names(dados))
    #colunas <- c(1:4,dt_inicio:dt_final)

    campos <- grep("x", indicadores$ANALYSIS)
    campos_nomes <- indicadores[campos, 1:2]

    codigo_indicador <- menu(campos_nomes)

    dt_analise <- menuData()

    n_cluster <- leNumero("Digite a quantidade de clusteres: ")

    ## Criar funções de clusterização
}



#########################################
#########################################
#########################################
segundaPergunta <- function() {
    cat(
        "\nNOME PERGUNTA: \nHa um padrao que relaciona os investimentos em P&D e o aumento do PIB?"
    )
    cat("\nINDICADORES ANALISADOS: GB.XPD.RSDV.GD.ZS e NY.GDP.MKTP.KD.ZG")
    cat(
        "\nRESULTADO ESPERADO: Responder se o crescimento do PIB esta diretamente relacionado ao aumento dos investimentos em P&D, ou seja, confirmar se eh de fato um padrao.\n\n"
    )
    print("######################################")
    cat("NY.GDP.MKTP.KD.ZG: GDP growth (annual %)\n")
    cat("GB.XPD.RSDV.GD.ZS: Research and development expenditure (% of GDP)\n")


    investimentosArray <-
        ai(
            indicator = c("GB.XPD.RSDV.GD.ZS"),
            country = c("BR"),
            startdate = 1970,
            enddate = 2017
        )
    investimentosArray <-
        investimentosArray[dim(investimentosArray)[1]:1, ]

    pibArray <-
        ai(
            indicator = c("NY.GDP.MKTP.KD.ZG"),
            country = c("BR"),
            startdate = 1970,
            enddate = 2017
        )
    pibArray <- pibArray[dim(pibArray)[1]:1, ]

    cont <- 1
    contPibArray <- 1
    contInvArray <- 1
    resultados <- list(
        anoInvestimento = c(),
        investimentoNoPrimeiroAno = c(),
        investimentoNoSegundoAno = c(),
        diferencaInvestimento = c(),
        anoPIB = c(),
        PIBNoPrimeiroAno = c(),
        PIBNoSegundoAno = c(),
        diferencaPIB = c(),
        padrao = c()
    )


    while (contInvArray <= nrow(investimentosArray) ||
           contPibArray <= nrow(pibArray)) {
        while (is.na(investimentosArray[contInvArray, 3])
               | is.na(investimentosArray[contInvArray + 1, 3])
               | is.na(investimentosArray[contInvArray, 5])
               | is.na(investimentosArray[contInvArray + 1, 5])) {
            contInvArray <- contInvArray + 1
        }

        while (is.na(pibArray[contPibArray, 3])
               | is.na(pibArray[contPibArray + 1, 3])
               | is.na(pibArray[contPibArray, 5])
               | is.na(pibArray[contPibArray + 1, 5])) {
            contPibArray <- contPibArray + 1
        }

        anoInvestimento <- investimentosArray[contInvArray, 3]
        anoPIB <- pibArray[contPibArray, 3]

        while (anoInvestimento != anoPIB - 1) {
            if (anoInvestimento < anoPIB) {
                contInvArray <- contInvArray + 1
                while (is.na(investimentosArray[contInvArray, 3])
                       |
                       is.na(investimentosArray[contInvArray + 1, 3])
                       | is.na(investimentosArray[contInvArray, 5])
                       |
                       is.na(investimentosArray[contInvArray + 1, 5])) {
                    contInvArray <- contInvArray + 1
                }
            } else if (anoInvestimento >= anoPIB) {
                contPibArray <- contPibArray + 1
                while (is.na(pibArray[contPibArray, 3])
                       | is.na(pibArray[contPibArray + 1, 3])
                       | is.na(pibArray[contPibArray, 5])
                       | is.na(pibArray[contPibArray + 1, 5])) {
                    contPibArray <- contPibArray + 1
                }
            }
            anoInvestimento <- investimentosArray[contInvArray, 3]
            anoPIB <- pibArray[contPibArray, 3]
        }

        resultados$anoInvestimento[cont] <- anoInvestimento
        resultados$investimentoNoPrimeiroAno[cont] <-
            investimentosArray$value[contInvArray]
        resultados$investimentoNoSegundoAno[cont] <-
            investimentosArray$value[contInvArray + 1]
        resultados$diferencaInvestimento[cont] <-
            resultados$investimentoNoSegundoAno[cont] - resultados$investimentoNoPrimeiroAno[cont]

        resultados$anoPIB[cont] <- anoPIB
        resultados$PIBNoPrimeiroAno[cont] <-
            pibArray$value[contPibArray]
        resultados$PIBNoSegundoAno[cont] <-
            pibArray$value[contPibArray + 1]
        resultados$diferencaPIB[cont] <-
            resultados$PIBNoSegundoAno[cont] - resultados$PIBNoPrimeiroAno[cont]

        if (resultados$diferencaInvestimento[cont] > 0 &
            resultados$diferencaPIB[cont] > 0) {
            resultados$padrao[cont] <- TRUE
        } else if (resultados$diferencaInvestimento[cont] < 0 &
                   resultados$diferencaPIB[cont] < 0) {
            resultados$padrao[cont] <- TRUE
        } else {
            resultados$padrao[cont] <- FALSE
        }

        cont <- cont + 1
        contInvArray <- contInvArray + 1
        contPibArray <- contPibArray + 1

        if (contInvArray == nrow(investimentosArray) |
            contPibArray == nrow(pibArray)) {
            break
        }
    }
    print(resultados)


    print("######################################")
    ay <- list(
        tickfont = list(color = "red"),
        overlaying = "y",
        side = "right",
        title = "% do PIB"
    )
    p <- plot_ly() %>%
        add_lines(
            x = resultados$anoInvestimento,
            y = resultados$investimentoNoPrimeiroAno,
            name = "Research and development expenditure (% of GDP)"
        ) %>%
        add_lines(
            x = resultados$anoPIB,
            y = resultados$PIBNoPrimeiroAno,
            name = "GDP growth (annual %)"
        ) %>%
        layout(title = "Comparacao dos indicadores",
               xaxis = list(title = "Ano"))
    print(p)


    print("######################################")
    ay <- list(
        tickfont = list(color = "red"),
        overlaying = "y",
        side = "right",
        title = "% do PIB"
    )
    p <- plot_ly() %>%
        add_lines(
            x = resultados$anoInvestimento,
            y = resultados$investimentoNoPrimeiroAno,
            name = "Research and development expenditure (% of GDP)"
        ) %>%
        add_lines(
            x = resultados$anoPIB,
            y = resultados$PIBNoPrimeiroAno,
            name = "GDP growth (annual %)",
            yaxis = "y2"
        ) %>%
        layout(title = "Comparacao dos indicadores",
               yaxis2 = ay,
               xaxis = list(title = "Ano"))
    print(p)


    print("######################################")
    ay <- list(
        tickfont = list(color = "red"),
        overlaying = "y",
        side = "right",
        title = "% do PIB"
    )
    p <- plot_ly() %>%
        add_lines(
            x = resultados$anoInvestimento,
            y = resultados$diferencaInvestimento,
            name = "Diferenca investimento"
        ) %>%
        add_lines(
            x = resultados$anoPIB,
            y = resultados$diferencaPIB,
            name = "Diferenca PIB",
            yaxis = "y2"
        ) %>%
        layout(title = "Comparacao da variacao dos indicadores",
               yaxis2 = ay,
               xaxis = list(title = "Ano"))
    print(p)


    print("######################################")
    p <- plot_ly() %>%
        add_lines(
            x = resultados$anoInvestimento,
            y = resultados$diferencaInvestimento,
            name = "Diferenca investimento"
        ) %>%
        add_lines(x = resultados$anoPIB,
                  y = resultados$diferencaPIB,
                  name = "Diferenca PIB") %>%
        layout(title = "Comparacao da variacao dos indicadores",
               xaxis = list(title = "Ano"))
    print(p)



    print("######################################")
    # df<-ai(indicator = c("NY.GDP.MKTP.KD.ZG","GB.XPD.RSDV.GD.ZS"), country = c("BR"), startdate = 2000)
    #
    # ay <- list(
    #     tickfont = list(color = "red"),
    #     overlaying = "y",
    #     side = "right",
    #     title = "% do PIB"
    # )
    # p <- plot_ly() %>%
    #     add_lines(x = df[df$src_id_ind=="NY.GDP.MKTP.KD.ZG",]$year, y = df[df$src_id_ind=="NY.GDP.MKTP.KD.ZG",]$value, name = "GDP growth (annual %)") %>%
    #     add_lines(x = df[df$src_id_ind=="GB.XPD.RSDV.GD.ZS",]$year, y = df[df$src_id_ind=="GB.XPD.RSDV.GD.ZS",]$value, name = "Research and development expenditure (% of GDP)", yaxis = "y2") %>%
    #     layout(
    #         title = "Comparacao dos indicadores", yaxis2 = ay,
    #         xaxis = list(title="Ano")
    #     )
    # print(p)
}



#########################################
#########################################
#########################################
terceiraPergunta <- function() {
    # cat("\nNOME PERGUNTA: ???\n")
    # cat("\nINDICADORES ANALISADOS: ???")
    # cat("\nRESULTADO ESPERADO: ???\n\n")
    # print("######################################")


    listaDeDadosBR <- criarListaDeDados()
    listaDeDadosBR <-
        adicionarIndicador(
            listaDeDadosBR,
            "NY.GDP.MKTP.KD.ZG",
            "BR",
            "GDP growth (annual %)",
            "Crescrimento do PIB (% anual)"
        )
    listaDeDadosBR <-
        adicionarIndicador(
            listaDeDadosBR,
            "GB.XPD.RSDV.GD.ZS",
            "BR",
            "Research and development expenditure (% of GDP)",
            "Gastos com pesquisa e desenvolvimento (% do PIB)"
        )
    listaDeDadosBR <-
        adicionarIndicador(
            listaDeDadosBR,
            "SE.XPD.TOTL.GD.ZS",
            "BR",
            "Government expenditure on education, total (% of GDP)",
            "Gasto governamental em educacao, Total (% do PIB)"
        )
    listaDeDadosBR <-
        adicionarIndicador(
            listaDeDadosBR,
            "SE.XPD.TERT.ZS",
            "BR",
            "Expenditure on tertiary as % of government expenditure on education (%)",
            "Gasto em educacao terciaria (superior) como porcentagem do gasto governamental em educacao"
        )
    listaDeDadosBR <-
        adicionarIndicador(
            listaDeDadosBR,
            "SE.XPD.SECO.ZS",
            "BR",
            "Expenditure on secondary as % of government expenditure on education (%)",
            "Gasto em educacao secundaria como porcentagem do gasto governamental em educacao"
        )
    listaDeDadosBR <-
        adicionarIndicador(
            listaDeDadosBR,
            "SE.XPD.CTOT.ZS",
            "BR",
            "Current education expenditure, total (% of total expenditure in public institutions)",
            "Porcentagem do total de gastos em instituicoes publicas (TOTAL)"
        )
    listaDeDadosBR <-
        adicionarIndicador(
            listaDeDadosBR,
            "SE.XPD.CTER.ZS",
            "BR",
            "Current education expenditure, tertiary (% of total expenditure in tertiary public institutions)",
            "Porcentagem do total de gastos em instituicoes publicas (TERCIARIO/SUPERIOR)"
        )
    listaDeDadosBR <-
        adicionarIndicador(
            listaDeDadosBR,
            "SE.XPD.CSEC.ZS",
            "BR",
            "Current education expenditure, secondary (% of total expenditure in secondary public institutions)",
            "Porcentagem do total de gastos em instituicoes publicas (SECUNDARIO/MEDIO)"
        )

    # print("!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!")
    # print(listaDeDadosBR$dadosTratados)
    # print("!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!")
    # print(listaDeDadosBR$dadosTratados[[1]])
    # print("!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!")
    # print("!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!")
    # print("!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!")
    # print("!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!")
    # print("!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!")
    # print(listaDeDadosBR$dadosTratados[[1]]$primeiroAno)
    # print("!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!")
    # print(listaDeDadosBR$dadosTratados[[1]]$indicadorNoPrimeiroAno)
    # print("!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!")
    # print(listaDeDadosBR$dadosTratados[[1]]$primeiroAno[[1]])
    # print("!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!")
    # print(listaDeDadosBR$dadosTratados[[1]]$indicadorNoPrimeiroAno[[1]])


    p <- plot_ly() %>%
        add_lines(
            x = listaDeDadosBR$dadosTratados[[1]]$primeiroAno,
            y = listaDeDadosBR$dadosTratados[[1]]$indicadorNoPrimeiroAno,
            name = listaDeDadosBR$tituloPortugues[[1]]
        ) %>%
        add_lines(
            x = listaDeDadosBR$dadosTratados[[2]]$primeiroAno,
            y = listaDeDadosBR$dadosTratados[[2]]$indicadorNoPrimeiroAno,
            name = listaDeDadosBR$tituloPortugues[[2]]
        ) %>%
        add_lines(
            x = listaDeDadosBR$dadosTratados[[3]]$primeiroAno,
            y = listaDeDadosBR$dadosTratados[[3]]$indicadorNoPrimeiroAno,
            name = listaDeDadosBR$tituloPortugues[[3]]
        ) %>%
        add_lines(
            x = listaDeDadosBR$dadosTratados[[4]]$primeiroAno,
            y = listaDeDadosBR$dadosTratados[[4]]$indicadorNoPrimeiroAno,
            name = listaDeDadosBR$tituloPortugues[[4]]
        ) %>%
        add_lines(
            x = listaDeDadosBR$dadosTratados[[5]]$primeiroAno,
            y = listaDeDadosBR$dadosTratados[[5]]$indicadorNoPrimeiroAno,
            name = listaDeDadosBR$tituloPortugues[[5]]
        ) %>%
        add_lines(
            x = listaDeDadosBR$dadosTratados[[6]]$primeiroAno,
            y = listaDeDadosBR$dadosTratados[[6]]$indicadorNoPrimeiroAno,
            name = listaDeDadosBR$tituloPortugues[[6]]
        ) %>%
        add_lines(
            x = listaDeDadosBR$dadosTratados[[7]]$primeiroAno,
            y = listaDeDadosBR$dadosTratados[[7]]$indicadorNoPrimeiroAno,
            name = listaDeDadosBR$tituloPortugues[[7]]
        ) %>%
        add_lines(
            x = listaDeDadosBR$dadosTratados[[8]]$primeiroAno,
            y = listaDeDadosBR$dadosTratados[[8]]$indicadorNoPrimeiroAno,
            name = listaDeDadosBR$tituloPortugues[[8]]
        ) %>%
        layout(title = "Comparacao dos indicadores",
               xaxis = list(title = "Ano"))
    print(p)
}



#########################################
#########################################
#########################################
quartaPergunta <- function() {
    cat("NOME PERGUNTA: quartaPergunta")
    cat("DADOS ANALISADOS: ")
    cat("RESULTADO ESPERADO: ")
}



#########################################
#########################################
#########################################
quintaPergunta <- function() {
    cat("NOME PERGUNTA: quintaPergunta")
    cat("DADOS ANALISADOS: ")
    cat("RESULTADO ESPERADO: ")
}
