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


# Para saber a estrutura de PAISES
#str(paises)

# PAISES é um frame?
#print(is.data.frame(paises))

# número de colunas de PAISES
#print(ncol(paises))

# número de linhas de PAISES
#print(nrow(paises))

# EXEMPLO: Extract Specific columns.
#result <- data.frame(paises$"ï..Country.Code")
#result

# Analisando os investimentos em educação (SE.XPD.TOTL.GD.ZS)
#investPorPais <- subset(dados, Indicator.Code == "SE.XPD.TOTL.GD.ZS", select = c(Country.Code), DROP=FALSE)
#investPorPais

# max(investPorPais$x2011,na.rm=TRUE)
# retval <- subset(investPorPais, x2011 == max(double(x2011)))
# retval