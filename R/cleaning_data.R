cleaningData <- function(dados){
    
    # excluindo dados especificos de Masculino e Feminino
    linhas <- grep(".MA.|.FE.", dados$Indicator.Code)
    dados <- dados[-linhas, ]
    
    # excluindo dados especificos de moedas especificas
    linhas <- grep(".CN", dados$Indicator.Code)
    dados <- dados[-linhas, ]
    
    # excluindo dados especificos de idades especificas
    linhas <- grep("[[:digit:]]{4}", dados$Indicator.Code )
    dados <- dados[-linhas, ]
    
    # excluindo dados irrelevantes
    linhas <- grep("IS.|LP.|IT.|ST.|SE.|IP.|DC.", dados$Indicator.Code )
    dados <- dados[-linhas, ]
    
    dados
}