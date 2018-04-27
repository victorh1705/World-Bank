source("R/io.R")

#opcoes = lista a ser printada
menu <- function(opcoes){
    print(opcoes)
    valor <- leNumero("Escolha uma opção : ")
    cat("valor: ")
    opcoes[row.names(opcoes)==valor, 1]
}

menuData <- function(){
    cat("Escolha uma Data - ")
    textoMenu <- "Escolha um [u]nico ano ou [v]ários anos\n'u' ou 'v' "
    
    opcao <- leCaracter(textoMenu, opcaoSim = "u", opcaoNao = "v")
    
    if(opcao=="u"){
        data <- leDataAno("Digite o ano: ")
    }else if(opcao =="v"){
        data <- leDataAno("Digite o ano Inicial: ")
        data1 <- leDataAno("Digite o ano Final: ")
        
        data <-  cbind(data,data1)
    }
    data
}