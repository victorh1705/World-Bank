cleaningData <- function(base_dados){

    # excluindo base_dados especificos de Masculino e Feminino
    linhas <- grep(".MA.|.MA|.FE.|.FE", base_dados$Indicator.Code)
    base_dados <- base_dados[-linhas, ]

    # excluindo base_dados especificos de moedas especificas
    linhas <- grep(".CN", base_dados$Indicator.Code)
    base_dados <- base_dados[-linhas, ]

    # excluindo base_dados especificos de idades especificas
    linhas <- grep("[[:digit:]]{4}", base_dados$Indicator.Code )
    base_dados <- base_dados[-linhas, ]

    # excluindo base_dados irrelevantes
    linhas <- grep("IS.|LP.|IT.|ST.|SE.|IP.|DC.", base_dados$Indicator.Code )
    base_dados <- base_dados[-linhas, ]

    base_dados
}
