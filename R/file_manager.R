lePaises <- function(){
    paises <- read.csv("~/Documentos/Programs/R Language/World Bank/data/Metadata_Country_API_Download_DS2_en_csv_v2_9444809.csv", comment.char="#")
    paises
}

leDados <- function(){
    dados <- read.csv("~/Documentos/Programs/R Language/World Bank/data/API_Download_DS2_en_csv_v2_9444809.csv", comment.char="#", skip = 4)
    dados
}

leIndicadores <- function(){
    indicadores <- read.csv("~/Documentos/Programs/R Language/World Bank/data/Metadata_Indicator_API_Download_DS2_en_csv_v2_9444809.csv")
}