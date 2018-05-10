warnings()

subset_Mean <- function(dados, cols, nameColNum = 1) {
    mean <- rowMeans(dados[cols], na.rm = TRUE)
    new_data <- cbind(dados[nameColNum], mean)
    new_data
}

subset_Sum <- function(dados, cols, nameColNum = 1) {
    mean <- rowSums(dados[cols], na.rm = TRUE)
    new_data <- cbind(dados[nameColNum], mean)
    new_data
}

data_frame_per_code <- function(dados, campo_nome, dt_analise) {
    if (is.matrix(dt_analise)) {
        ini_dt <- grep(dt_analise[1], names(dados))
        fin_dt <- grep(dt_analise[length(dt_analise)], names(dados))
        colunas <- c(1:4, ini_dt:fin_dt)

    } else{
        new_dt <- grep(dt_analise, names(dados))
        colunas <- c(1:4, new_dt)
    }

    new_data <-subset(dados,
               Indicator.Code==as.character(campo_nome),
               colunas)
    new_data
}

data_frame_per_country <- function(dados, cols, pais, nameColNum = 1) {
    new_data <- data_frame_per_code(dados,cols,nameColNum)

    new_data <- subset(new_data,
                       Country.Code==as.character(pais))

    new_data
}

data_frame_date_to_column <- function(dados) {
    dados <-  melt(dados,c("Country.Name","Country.Code","Indicator.Name","Indicator.Code"))

    dados
}
