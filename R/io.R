read <- function(texto){
    cat(texto)
    lido <- readline()
    lido
}

leNumero <- function(texto) {
    
    num <- as.numeric(read(texto))
    while( is.na(num) | !is.numeric(num) | num < 0 ) {
        num <- as.numeric(read(texto))
    }
    num
}

leDataAno <- function(texto) {
    
    num <- leNumero(texto)
    while( num < 1959 | num > 2017  ){
        num <- leNumero(texto)    
    }
    num
}

leTexto <- function(texto) {
    lido <-read(texto)
    lido
}

leCaracter <- function(texto, opcaoSim = "y", opcaoNao= "n") {
    
    opcao <- read(texto)
    while (  !is.character(opcao) | 
             (nchar(opcao) != 1) | 
             (opcao != opcaoSim & opcao != opcaoNao ) ) {
        opcao <- read(texto)
    }
    opcao
}