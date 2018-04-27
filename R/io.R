leNumero <- function(texto) {
    
    cat(texto)
    num <- as.numeric(readline())
    while( is.na(num) | !is.numeric(num) | num < 0 ) {
        cat(texto)
        num <- as.numeric(readline())
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
    
    cat(texto)
    lido <- readline()
    while (  !is.character(lido)) {
        cat(texto)
        lido <- readline()
    }
    lido
}

leCaracter <- function(texto, opcaoSim = "y", opcaoNao= "n") {
    
    cat(texto)
    opcao <- readline()
    while (  !is.character(opcao) | 
             (nchar(opcao) != 1) | 
             (opcao != opcaoSim & opcao != opcaoNao ) ) {
        cat(texto)
        opcao <- readline()
    }
    opcao
}