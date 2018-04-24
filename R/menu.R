source("R/io.R")

menu <- function(opcoes){
    print("Escolha uma opção : ")
    valor <- leNumero(opcoes)
    print("valor: ")
    print(valor )
}