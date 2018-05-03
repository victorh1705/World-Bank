warnings()

source("R/file_manager.R")
source("R/io.R")
source("R/menu.R")
source("R/subset.R")
source("R/perguntas.R")

dados <- leDados()
paises <- lePaises()
indicadores <- leIndicadores()

cat("[1] Pergunta 1\n")
cat("[2] Relação entre o crescimento anual do PIB e o investimento em Pesquisa e Desenvolvimento\n")
cat("[3] Pergunta 3\n")
cat("[4] Pergunta 4\n")
cat("[5] Pergunta 5\n")
cat("[6] Informações dos arquivos de dados\n\n")

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