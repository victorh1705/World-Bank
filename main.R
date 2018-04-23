dados <- read.csv("~/Documentos/Programs/R Language/World Bank/data/API_Download_DS2_en_csv_v2_9444809.csv", comment.char="#", skip = 4)
# Lista de paises  = paises[1] 
# Codigo de paises = paises[2]
paises <- read.csv("~/Documentos/Programs/R Language/World Bank/data/Metadata_Country_API_Download_DS2_en_csv_v2_9444809.csv", comment.char="#")
indicadores <- read.csv("~/Documentos/Programs/R Language/World Bank/data/Metadata_Indicator_API_Download_DS2_en_csv_v2_9444809.csv")

dt_inicio <- grep(2000, names(dados))
dt_final <- grep(2017, names(dados))
colunas <- c(1:4,dt_inicio:dt_final)

campos <- grep("x", indicadores$ANALYSIS)
aux_campos_nomes <- indicadores[campos,1]

n_linha <- nrow(dados)
first = TRUE

# Criar data frame resultado
# variaveis do loop
i <- 1
aux_linha <- 0
repeat{
 if(length(campos)<i){
   break
 }
  linhas <- grep(aux_campos_nomes[i], dados$Indicator.Code)
  
  # Add toda linha em aux_campos em resultados
  # criar for para isso
  #for (linha in aux_campos) {
    # add linha em resultados usando bind()
  if( isTRUE(first)){
    first <- FALSE
    dados_analise <- dados[linhas,colunas]
  }else{
    aux_linha <- dados[linhas,colunas]
  }
  dados_analise <- rbind(dados_analise,aux_linha)
  #}
  
  i <- i + 1
}

print("Numero de linhas de dados analise")
print(nrow(dados_analise))

View(dados_analise)
