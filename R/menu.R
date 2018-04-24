source("R/io.R")

menu <- function(opcoes){
    print("Escolha uma opção : ")
    valor <- leNumero(opcoes)
    cat("valor: ")
    opcoes[row.names(opcoes)==valor, 1]
}