source("R/file_manager.R")

GDP <- function() {
    dados <- leDados()
    brasil <- subset(dados, Country.Code == "BRA", Country.Code:X2017)

    result <- grep("(% of GDP)", brasil$Indicator.Name)
    brasil <- brasil[result,]

    # GDP = GDP (current US$)

    # C = Household final consumption expenditure (current US$)
    # General government final consumption expenditure (% of GDP)

    # I1 = Net investment in nonfinancial assets (% of GDP)
    # I2 = Foreign direct investment, net inflows (% of GDP)
    # I3 = Foreign direct investment, net outflows (% of GDP)

    # G = General government final consumption expenditure (% of GDP)
    # X = Exports of goods and services (% of GDP)
    # M = Imports of goods and services (% of GDP)
    brasil
}




#########################################
#########################################
#########################################
acharMediaInvestimento <- function(){
    # I1 = Net investment in nonfinancial assets (% of GDP)
    #       GC.NFN.TOTL.GD.ZS
    # I2 = Foreign direct investment, net inflows (% of GDP)
    #       BX.KLT.DINV.WD.GD.ZS
    # I3 = Foreign direct investment, net outflows (% of GDP)
    #       BM.KLT.DINV.WD.GD.ZS

    cat("Analise de sensibilidade")

    listaDeDadosBR <- criarlistaPadroesDeDados()

    listaDeDadosBR <<- adicionarIndicador(
        listaDeDadosBR,
        "GC.NFN.TOTL.GD.ZS",
        "BR",
        "Net investment in nonfinancial assets (% of GDP)",
        "Investimentos liquido em ativo nao -financiaveis (% do PIB)"
    )
    listaDeDadosBR <<- adicionarIndicador(
        listaDeDadosBR,
        "BX.KLT.DINV.WD.GD.ZS",
        "BR",
        "Foreign direct investment, net inflows (% of GDP)",
        "Investimento estrangeiro direto, entrada liquida (% do PIB)"
    )
    listaDeDadosBR <<- adicionarIndicador(
        listaDeDadosBR,
        "BM.KLT.DINV.WD.GD.ZS",
        "BR",
        "Foreign direct investment, net outflows (% of GDP)",
        "Investimento estrangeiro direto, saida liquida (% do PIB)"
    )
}
