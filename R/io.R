leNumero <- function(texto) {

    print(texto)
    num <- readline("")
    while( !is.na(num) | !is.numeric(num)) {
        num <- readline(texto)
    }
    num
}

leTexto <- function(texto) {

    lido <- readline(texto)
    while ( !is.na(texto) | !is.character(texto)) {
        texto <- readline(texto)
        break
    }
    texto
}