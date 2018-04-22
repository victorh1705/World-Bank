dados <- read.csv("~/Documentos/Programs/R Language/World Bank/data/API_Download_DS2_en_csv_v2_9444809.csv", comment.char="#", skip = 4)
# Lista de paises  = paises[1] 
# Codigo de paises = paises[2]
paises <- read.csv("~/Documentos/Programs/R Language/World Bank/data/Metadata_Country_API_Download_DS2_en_csv_v2_9444809.csv", comment.char="#")
indicadores <- read.csv("~/Documentos/Programs/R Language/World Bank/data/Metadata_Indicator_API_Download_DS2_en_csv_v2_9444809.csv")

dt_inicio <- grep(2000, names(dados))
dt_final <- grep(2017, names(dados))

campos <- grep("x", indicadores$ANALYSIS)
aux_campos <- campos

n_linha <- nrow(dados)

# Criar data frame resultado

i <- 1
repeat{
 if(campos[1] > n_linha){
   break
 }
  aux_campos <- aux_campos * i
  
  # Add toda linha em aux_campos em resultados
  # criar for para isso
  for (linha in aux_campos) {
    
    # add linha em resultados usando bind()
  }
  
  i <- i + 1
}