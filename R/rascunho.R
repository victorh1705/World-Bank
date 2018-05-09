warnings()

n_linha <- nrow(dados)
first = TRUE

# Criar data frame resultado
# variaveis do loop
i <- 1
aux_linha <- 0
repeat {
    if (length(campos) < i) {
        break
    }
    linhas <- grep(campos_nomes[i], dados$Indicator.Code)

    # Add toda linha em aux_campos em resultados
    # criar for para isso
    #for (linha in aux_campos) {
    # add linha em resultados usando bind()
    if (isTRUE(first)) {
        first <- FALSE
        dados_analise <- dados[linhas, colunas]
    } else{
        aux_linha <- dados[linhas, colunas]
    }
    dados_analise <- rbind(dados_analise, aux_linha)
    #}

    i <- i + 1
}

print("Numero de linhas de dados analise")
print(nrow(dados_analise))

View(dados_analise)
