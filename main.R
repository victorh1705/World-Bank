source("R/file_manager.R")
source("R/io.R")
source("R/menu.R")

dados <- leDados()
paises <- lePaises()
indicadores <- leIndicadores()

dt_inicio <- grep(2000, names(dados))
dt_final <- grep(2017, names(dados))
colunas <- c(1:4,dt_inicio:dt_final)

campos <- grep("x", indicadores$ANALYSIS)
campos_nomes <- indicadores[campos,1]

menu(indicadores[campos,1:2])