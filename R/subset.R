warnings()

subsetMean <- function(dados, cols, nameColNum = 1) {
    mean <- rowMeans(dados[cols], na.rm = TRUE)
    new_data <- cbind(dados[nameColNum], mean)
    new_data
}

subsetSum <- function(dados, cols, nameColNum = 1) {
    mean <- rowSums(dados[cols], na.rm = TRUE)
    new_data <- cbind(dados[nameColNum], mean)
    new_data
}

criarDF <- function(dados, campo_nome, dt_analise) {
    if (is.matrix(dt_analise)) {
        ini_dt <- grep(dt_analise[1], names(dados))
        fin_dt <- grep(dt_analise[2], names(dados))
        colunas <- c(1:4, ini_dt:fin_dt)

    } else{
        new_dt <- grep(dt_analise, names(dados))
        colunas <- c(1:4, new_dt)
    }

    new_data <-
        subset(dados,
               Indicator.Code == as.character(campos_nomes),
               colunas)
    new_data
}
