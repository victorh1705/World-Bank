
warnings()
# source("custom-repos.R")
# packrat::init()
# packrat::snapshot()
# packrat::restore()
# library("dplyr", lib.loc="~/R/x86_64-pc-linux-gnu-library/3.5")

source("R/file_manager.R")
source("R/io.R")
source("R/menu.R")
source("R/subset.R")
source("R/perguntas.R")
source("R/indicadores.R")
# dados <- leDados()
# paises <- lePaises()
# indicadores <- leIndicadores()

listaDeDadosBR <<- criarListaDeDados()
listaPadroesDeDadosBR1x1 <<- criarlistaPadroesDeDados()
listaPadroesDeDadosBR1xN <<- criarlistaPadroesDeDados()


cat("[1] Pergunta 1\n")
cat("[2] Ha um padrao que relaciona os investimentos em P&D e o aumento do PIB?\n")
cat("[3] Pergunta 3\n")
cat("[4] Pergunta 4\n")
cat("[5] Pergunta 5\n")
cat("[6] Informacoes dos arquivos de dados\n\n")

cat("Escolha uma [opcao]: ")
opcao <- readline()

if (opcao == 1) {
    primeiraPergunta()
} else if (opcao == 2) {
    segundaPergunta()
} else if (opcao == 3) {
    terceiraPergunta()
} else if (opcao == 4) {
    quartaPergunta()
} else if (opcao == 5) {
    quintaPergunta()
} else if (opcao == 6) {
    descricaoArquivos()
} else {
    cat("Pergunta inexistente.")
}
