leNumero <- function(texto) {
    
    print(texto)
    num <- as.numeric(readline())
    while( is.na(num) | !is.numeric(num) | num < 0 ) {
        print(texto)
        num <- as.numeric(readline())
    }
    num
}

leDataAno <- function(texto) {
    
    print(texto)
    num <- as.numeric(readline())
    while( is.na(num) | (!is.numeric(num) & (num < 1959 | num > 2017 )) )  {
        print(texto)
        num <- as.numeric(readline())
    }
    num
}

leTexto <- function(texto) {
    
    print(texto)
    lido <- readline()
    while (  !is.character(texto)) {
        print(texto)
        texto <- readline()
        break
    }
    texto
}