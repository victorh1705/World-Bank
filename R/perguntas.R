# install.packages('devtools')
# install.packages('plotly')
# install_github('EL-BID/Libreria-R-Numeros-para-el-Desarrollo')
# install_github("arcuellar88/govdata360R")
# install_github('EL-BID/Agregador-de-indicadores')
# devtools::install_github('hadley/ggplot2')
library(devtools)
library(agregadorindicadores)
library(plotly)

warnings()

#########################################
#########################################
#########################################
primeiraPergunta <- function() {
    cat("NOME PERGUNTA: primeiraPergunta\n")
    cat("DADOS ANALISADOS: \n")
    cat("RESULTADO ESPERADO: \n")

    #dt_inicio <- grep(2000, names(dados))
    #dt_final <- grep(2017, names(dados))
    #colunas <- c(1:4,dt_inicio:dt_final)

    campos <- grep("x", indicadores$ANALYSIS)
    campos_nomes <- indicadores[campos,1:2]

    codigo_indicador <- menu(campos_nomes)

    dt_analise <- menuData()

    n_cluster <- leNumero("Digite a quantidade de clusteres: ")

    ## Criar funções de clusterização
}



#########################################
#########################################
#########################################
segundaPergunta <- function() {
    cat("\nNOME PERGUNTA: \nRelação entre o crescimento anual do PIB e o investimento anual em Pesquisa e Desenvolvimento")
    cat("\nDADOS ANALISADOS: ")
    cat("\nRESULTADO ESPERADO: \n\n")

    # PARA VER COMO A BIBLIOTECA FUNCIONA, ACESSAR:
    # https://rdrr.io/github/EL-BID/Agregador-de-indicadores/f/README.md



    # SE.XPD.TOTL.GD.ZS: Government expenditure on education, total (% of GDP)
    # SE.XPD.TOTL.GB.ZS: Expenditure on education as % of total government expenditure (%)

    # SE.XPD.TERT.ZS, Expenditure on tertiary as % of government expenditure on education (%)
    # SE.XPD.TERT.PC.ZS, Government expenditure per tertiary student as % of GDP per capita (%)
    # SE.XPD.SECO.ZS, Expenditure on secondary as % of government expenditure on education (%)
    # SE.XPD.SECO.PC.ZS, Government expenditure per student, secondary (% of GDP per capita)
    # SE.XPD.PRIM.ZS, Expenditure on primary as % of government expenditure on education (%)
    # SE.XPD.PRIM.PC.ZS, Government expenditure per student, primary (% of GDP per capita)

    # SE.XPD.CTOT.ZS, Current education expenditure, total (% of total expenditure in public institutions)
    # SE.XPD.CTER.ZS, Current education expenditure, tertiary (% of total expenditure in tertiary public institutions)
    # SE.XPD.CSEC.ZS, Current education expenditure, secondary (% of total expenditure in secondary public institutions)
    # SE.XPD.CPRM.ZS, Current education expenditure, primary (% of total expenditure in primary public institutions)

    # GB.XPD.RSDV.GD.ZS,Research and development expenditure (% of GDP)

    # NY.GDP.PCAP.KD.ZG,GDP per capita growth (annual %)
    # NY.GDP.MKTP.KD.ZG,GDP growth (annual %)



    print("######################################")



    data<-ai(indicator = c("SE.XPD.TOTL.GD.ZS"), country = c("BR"), startdate = 2000, enddate=2015)
    print(data[,1:6])



    print("######################################")



    # EXEMPLO: Extract Specific columns.
    #result <- data.frame(paises$"ï..Country.Code")
    #result

    # Analisando os investimentos em educação (SE.XPD.TOTL.GD.ZS)
    investPorPais <- subset(dados, Indicator.Code == "SE.XPD.TOTL.GD.ZS" & Country.Code=="BRA", DROP=FALSE)
    print(investPorPais[,56:61])

    crescimentoPIBAnual <- subset(dados, Indicator.Code == "NY.GDP.MKTP.KD.ZG" & Country.Code=="BRA", DROP=FALSE)
    print(crescimentoPIBAnual[,56:61])



    print("######################################")



    # max(investPorPais$x2011,na.rm=TRUE)
    # retval <- subset(investPorPais, investPorPais$x2011 == max(investPorPais$x2011,na.rm = TRUE ))
    # print(retval, "\n\n")



    print("######################################")



    cat("INDICADORES:\n")
    cat("\nNY.GDP.MKTP.KD.ZG: GDP growth (annual %)\n\n")
    cat("\nGB.XPD.RSDV.GD.ZS: Research and development expenditure (% of GDP)")

    df<-ai(indicator = c("NY.GDP.MKTP.KD.ZG","GB.XPD.RSDV.GD.ZS"), country = c("BR"), startdate = 2000)

    ay <- list(
        tickfont = list(color = "red"),
        overlaying = "y",
        side = "right",
        title = "% do PIB"
    )
    p <- plot_ly() %>%
        add_lines(x = df[df$src_id_ind=="NY.GDP.MKTP.KD.ZG",]$year, y = df[df$src_id_ind=="NY.GDP.MKTP.KD.ZG",]$value, name = "GDP growth (annual %)") %>%
        add_lines(x = df[df$src_id_ind=="GB.XPD.RSDV.GD.ZS",]$year, y = df[df$src_id_ind=="GB.XPD.RSDV.GD.ZS",]$value, name = "Research and development expenditure (% of GDP)", yaxis = "y2") %>%
        layout(
            title = "Comparacao dos indicadores", yaxis2 = ay,
            xaxis = list(title="Ano")
        )
    print(p)
}



#########################################
#########################################
#########################################
terceiraPergunta <- function() {
    cat("NOME PERGUNTA: terceiraPergunta")
    cat("DADOS ANALISADOS: ")
    cat("RESULTADO ESPERADO: ")
}



#########################################
#########################################
#########################################
quartaPergunta <- function() {
    cat("NOME PERGUNTA: quartaPergunta")
    cat("DADOS ANALISADOS: ")
    cat("RESULTADO ESPERADO: ")
}



#########################################
#########################################
#########################################
quintaPergunta <- function() {
    cat("NOME PERGUNTA: quintaPergunta")
    cat("DADOS ANALISADOS: ")
    cat("RESULTADO ESPERADO: ")
}
