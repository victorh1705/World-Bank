warnings()

lePaises <- function() {
    pathArquivo <-
        paste(
            getwd(),
            "data/Metadata_Country_API_Download_DS2_en_csv_v2_9444809.csv",
            sep = "/"
        )
    paises <- read.csv(pathArquivo, comment.char = "#")
    paises
}

leDados <- function() {
    pathArquivo <-
        paste(getwd(),
              "data/API_Download_DS2_en_csv_v2_9444809.csv",
              sep = "/")
    dados <- read.csv(pathArquivo, comment.char = "#", skip = 4)
    dados
}

leIndicadores <- function() {
    pathArquivo <-
        paste(
            getwd(),
            "data/Metadata_Indicator_API_Download_DS2_en_csv_v2_9444809.csv",
            sep = "/"
        )
    indicadores <- read.csv(pathArquivo)

    # Rename columns
    indicadores <-  rename(indicadores, Indicator.Code = INDICATOR_CODE)
    indicadores <-  rename(indicadores, Indicator.Name = INDICATOR_NAME)
    indicadores <-  rename(indicadores, Source.Note = SOURCE_NOTE)
    indicadores <-  rename(indicadores, Source.Organization = SOURCE_ORGANIZATION)

    indicadores
}

descricaoArquivos <- function() {
    cat("\n######################################################\n")
    cat("ARQUIVO: data/Metadata_Country_API_Download_DS2_en_csv_v2_9444809.csv")

    paises <- lePaises()

    cat("\nTIPO: ")
    cat(typeof(paises))

    cat("\nNUMERO DE COLUNAS: ")
    cat(ncol(paises))

    cat("\nNUMERO DE LINHAS: ")
    cat(nrow(paises))

    cat("\nESTRUTURA: \n")
    cat(str(paises))

    cat("\n######################################################\n")


    cat("\n######################################################\n")
    cat("ARQUIVO: data/API_Download_DS2_en_csv_v2_9444809.csv")

    dados <- leDados()

    cat("\nTIPO: ")
    cat(typeof(dados))

    cat("\nNUMERO DE COLUNAS: ")
    cat(ncol(dados))

    cat("\nNUMERO DE LINHAS: ")
    cat(nrow(dados))

    cat("\nESTRUTURA: \n")
    cat(str(dados))

    cat("\n######################################################\n")


    cat("\n######################################################\n")
    cat("ARQUIVO: data/Metadata_Indicator_API_Download_DS2_en_csv_v2_9444809.csv")

    indicadores <- leIndicadores()

    cat("\nTIPO: ")
    cat(typeof(indicadores))

    cat("\nNUMERO DE COLUNAS: ")
    cat(ncol(indicadores))

    cat("\nNUMERO DE LINHAS: ")
    cat(nrow(indicadores))

    cat("\nESTRUTURA: \n")
    cat(str(indicadores))

    cat("\n######################################################\n")
}

# EXEMPLO: Extract Specific columns.
#result <- data.frame(paises$"ï..Country.Code")
#result

# Analisando os investimentos em educação (SE.XPD.TOTL.GD.ZS)
#investPorPais <- subset(dados, Indicator.Code == "SE.XPD.TOTL.GD.ZS", select = c(Country.Code), DROP=FALSE)
#investPorPais

# max(investPorPais$x2011,na.rm=TRUE)
# retval <- subset(investPorPais, x2011 == max(double(x2011)))
# retval
