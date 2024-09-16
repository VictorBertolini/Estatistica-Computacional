# Bibliotecas:
library(ggplot2)
library(rpart)
library(rpart.plot)
library(patchwork)


# == Questão 1 == #

a <- c(10:30)
b <- c(30:10)
vetorFinal <- c(a, b[2:length(b)])

print(vetorFinal)


# == Questão 2 == #

# a)
vetorPar <- rep(seq(2, 8, by = 2), times = 10)

# b)
vetorFinalPar <- rep(seq(2, 8, by = 2), times = 10)
vetorFinalPar <- c(vetorFinalPar, c(2))


# == Questão 3 == #
# a)
vet_a <- c(20:30)
somatorio_a <- sum(vet_a * vet_a + 4 * vet_a)

# b)
vet_b <- c(10:20)
somatorio_b <- sum((3 ^ vet_b)/ vet_b + (2 ^ vet_b) / (vet_b ^ 2))

# == Questão 4 == #
bolas_tiradas <- sample(1:100, 10, replace = TRUE)

# a)
pares <- sum(bolas_tiradas %% 2 == 0)


# b)
maior_70 <- sum(bolas_tiradas > 70)


# c)
impares <- which(bolas_tiradas %% 2 == 1)


# == Questão 5 == #

doisQuatro <- function() 
{
  
    vez <- 0
    conjuntoTirado <- c()
  
    while (vez != 2)
    {
        numero <- sample(1:6, size = 1)
        
        if (numero == 4)
        {
          vez <- vez + 1
        }
        
        conjuntoTirado <- c(conjuntoTirado, numero)
    }
    
    
    return (length(conjuntoTirado))
}


quantidade <- doisQuatro()



# == Questão 6 == #
doisQuatro <- function() 
{
    
    vez <- 0
    conjuntoTirado <- c()
    
    while (vez != 2)
    {
        numero <- sample(1:6, size = 1)
        
        if (numero == 4)
        {
            vez <- vez + 1
        }
        
        conjuntoTirado <- c(conjuntoTirado, numero)
    }
    
    
    return (length(conjuntoTirado))
}

quantidades <- c()
for (i in 1:10000)
{
    quantidades <- c(quantidades, doisQuatro())
}

media <- mean(quantidades)

# Análise:
# Cada número do dado possui 1/6 chances de sair
# Então pensa-se que para sair o primeiro 4 há uma chance de 1/6 
# Essa porcentagem gira em torno de 16.67%
# Todavia para sair outro 4 a porcentagem diminui um pouco, gerando o resultado apresentado 


# == Questão 7 == #

fibonacci <- function(n)
{
    if (n < 3)
        return (0)
    

    sequencia <- c(1, 1)
    
    while (n > 2)
    {
        tam <- length(sequencia)
        sequencia <- c(sequencia, sequencia[tam] + sequencia[tam - 1])
        
        n <- n - 1
    }
    
    return(sequencia)
}


fib <- fibonacci(10)
fib

# == Questão 8 == #
amigoSecreto <- function(nomes)
{
    nomesSorteados <- sample(nomes)
    
    if (identical(nomes, nomesSorteados))
        return(0)
    else 
        return(1)
}


nomes <- c("Michael Scott", "Dwight Schrute", "Jim Halpert", "Kevin Malone" ,"Creed Bratton")
sorteios <- c()

jogos <- 100000
for (i in 1:jogos)
{
    sorteios <- c(sorteios, amigoSecreto(nomes))
}

jogosValidos <- sum(sorteios)

jogosInvalidos <- (jogos - jogosValidos) / jogos


# == Questão 9 == #

craps_game <- function()
{
    num_sorteado <- sample(1:6, 2, replace = TRUE)
    soma <- sum(num_sorteado)
    
    if (soma == 7 || soma == 11)
        return (1)
    else if (soma == 2 || soma == 3 || soma == 12)
        return(0)
    else 
    {
        while(TRUE)
        {
            novo_num <- sample(1:6, 2, replace = TRUE)
            nova_soma <- sum(novo_num)
            
            if (nova_soma == 7)
                return(0)
            else if (nova_soma == soma)
                return(1)
            
        }
    }
}

for (i in 1:100000)
    vitorias_craps <- c(vitorias_craps, craps_game())

taxa_vitoria <- mean(vitorias_craps)


# == Questão 10 == #

# a)
passeio <- function(L, N)
{
    while(TRUE)
    {
        
        if (L == N)
            return (1)
        
        else if (L == 0)
            return (0)
        
        moeda <- sample(1:2, 1)
        
        if (moeda == 1)
            L <- L - 1
        else 
            L <- L + 1
    }
}


# b)

diversos_passeios <- function(L, N, Q)
{
    destino <- c()
    for (i in 1:Q)
    {
        destino <- c(destino, passeio(L, N))
    }
    
    return(mean(destino))
}

# c)
resultado_cada_valor <- c()
media_cada_valor <- c()
for (j in 1:19)
    media_cada_valor <- c(media_cada_valor, diversos_passeios(j, 20, 100))

df = data.frame(x = 1:19, y = media_cada_valor)

ggplot(data = df, aes(x = 1:19, y = media_cada_valor)) + labs(title = "Proporção de chegadas pela posição inicial",x = "Posição Inicial", y = "Porcentagem de chegadas em Casa") + geom_line()


# == Questão 11 == #

# a)
passeioLink <- function(Passos)
{
    coordenada <- c('U', 'D', 'L', 'R')
    posicao <- c(0, 0)
    while (Passos > 0) 
    {
        passo <- sample(coordenada, 1)
        
        if (passo == 'U')
            posicao[2] <- posicao[2] + 1
        
        else if (passo == 'D')
            posicao[2] <- posicao[2] - 1
        
        else if (passo == 'L')
            posicao[1] <- posicao[1] - 1
        
        else if (passo == 'R')
            posicao[1] <- posicao[1] + 1
        
        Passos <- Passos - 1
    }
    
    return (posicao)
}

resultado <- passeioLink(4)


# b)
voltaOrigem <- function(resultadoPasseio)
{
    resultadoPasseio
    matriz_coord_destino <- matrix(resultadoPasseio, ncol = 2, byrow = TRUE)
    
    origem <- matriz_coord_destino[matriz_coord_destino[,1] == 0 & matriz_coord_destino[,2] == 0]
    
    return ((length(origem ) / 2 * 100) / (length(matriz_coord_destino) / 2))
}


resultadoPasseio <- c()
for (i in 1:10000)
{
    resultadoPasseio <- c(resultadoPasseio, passeioLink(8))
}


porc <- voltaOrigem(resultadoPasseio)


# c)
proporcaoPasseio <- function(N)
{
    if (N %% 2 == 1)
        return("Impossível retornar a origem depois de um número ímpar de passos")
    
    resultadoPasseio <- c()
    for (i in 1:10000)
    {
        resultadoPasseio <- c(resultadoPasseio, passeioLink(N))
    }
    
    porc <- voltaOrigem(resultadoPasseio)
    
    frase <- cat("Após o passeio, Link chegou à origem na proporção: ", porc, " do total de vezes\n")
    
    return(frase)
}

proporcaoPasseio(10)




# == Questão 12 == #

jogo_moeda <- function(steven, garnit)
{
    sequencia <- sample(0:1, 3, replace = TRUE)
    
    while (TRUE)
    {
        if (identical(steven, sequencia))
            return ("Steven")
        else if (identical(garnit, sequencia))
            return ("Garnit")
        
        sequencia <- c(sequencia, sample(0:1, 1))
        sequencia <- sequencia[-1]
    }
}


total <- 10000
vitorias_moeda <- c()

for (i in 1:total)
{
    steven <- sample(0:1, 3, replace = TRUE)
    garnit <- sample(0:1, 3, replace = TRUE)
    while (identical(steven, garnit))
        garnit <- sample(0:1, 3, replace = TRUE)
    
    vitorias_moeda <- c(vitorias_moeda, jogo_moeda(steven, garnit))
}

tabela_jogo <- table(vitorias_moeda)
vitoria_garnit <- na.omit(tabela_jogo["Garnit"])
vitoria_garnit <- vitoria_garnit / total

# Análise:

# Observa-se que a taxa de vitórias de Garnit fica muito próxima de 0.5 (50%)
# E isso deve-se ao caráter aleatório da moeda, 
# visto que, em razão da dependência do resultado de uma moeda 
# (a qual é um dado de 2 lados (50% de sair cada lado)) 
# tem-se a chance de vitória tanto de Garnit quanto de Steven muito próxima de 50%
# observado na conclusão desse problema 


# == Questão 13 == #
setwd("\\Users\\Pc Gamer\\OneDrive - Universidade Federal de Uberlândia\\Faculdade\\5° Período\\Estatística Computacional\\Primeiro Trabalho")
dados_shipman <- read.table(file = "dados.txt", sep =";", header = TRUE)

dados_shipman$LocalDaMorte <- as.factor(dados_shipman$LocalDaMorte)
dados_shipman$Genero <- as.factor(dados_shipman$Genero)


summary(dados_shipman)

# a)
ggplot(data = dados_shipman, aes(x = Genero)) + 
    labs(y = 'Quantidade', x = "Sexo") + 
    geom_bar(fill = 'darkblue') + 
    theme_minimal()

# Análise:
# É notório a partir do gráfico que Shipman possuía uma preferência maior por assassinar mulheres

# b)
ggplot(data = dados_shipman, aes(x = dados_shipman$Idade)) + 
    geom_histogram(color = 'white', fill = '#20B2AA', bins = 8) +
    labs(x = "Idades", y = "Quantidade de Pessoas") +
    theme_minimal()
 
# Análise:
#     Entende-se que Shipman possuía uma preferência por assassinar pessoas com idade mais avançada
#     Dado que no gráfico, há um disparo na quantiade de pessoas entre 60 e 100 anos

#Homens
men <- data.frame(dados_shipman[dados_shipman$Genero == "Men", ])
ggplot(data = men, aes(x = Idade)) +
    geom_histogram(bins = 8, fill = "darkblue", color = "white") +
    labs(x = "Idade", y = "Frequência", 
         title = "Histograma das Idades dos Homens mortos por Shipman") +
    theme_minimal()

# Mulheres
women <- data.frame(dados_shipman[dados_shipman$Genero == "Women", ])
ggplot(data = women, aes(x = Idade)) +
    geom_histogram(bins = 8, fill = "#DC143C", color = 'white') +
    labs(x = "Idade", y = "Frequência", 
         title = "Histograma das Idades das Mulheres mortas por Shipman") +
    theme_minimal()

# Análise de cada gênero:
    # Homens:
    #     Percebe-se além da baixa quantidade de mortos homens por shipman 
    #     Que foram poucas pessoas mais novas (em torno de 40 anos)
    #     E uma quantidade mais elevada de homens mais velhos 
    #     E nitidamente percebe-se uma lacuna de mortos com idade próxima aos 60 anos 
    # 
    # Mulheres:
    #     Além das quantidades exorbitantes de mortas
    #     Nota-se uma clara preferência por matar mulheres com idade mais avaçada,
    #     Principalmente aquelas com idade próxima aos 80 anos 

# c)
ggplot(data = dados_shipman, aes(y = Idade)) + 
    geom_boxplot(fill = '#87CEEB', color = 'black') +
    labs(title = "BoxPlot da idade de mortos por Shipman") +
    theme_minimal()

# Análise Resultado Obtido:

# Observa-se com nitidez a grande massa de mortos por shipman concetrada em iaddes mais avançadas 
# Ainda que haja casos particulares de mortos abaixo dos 60 anos, são minoria 
# O boxPlot deixa ainda mais evidente a concentração em massa de assassinatos de idosos 

# d)
ggplot(data = dados_shipman, aes(x = LocalDaMorte)) +
    labs(x = "Local da Morte", y = "Quantidade de Mortes",
         title = "Gráfico de Barras dos locais de morte das vítimas de Shipman") +
    geom_bar(fill = '#FF4500', color = 'white') +
    theme_minimal()

# Análise Resultados Obtidos:

# Com base na análise do gráfico, percebe-se que a maioria dos assassinatos de Shipman 
# Foi feito na casa das próprias vítimas, havendo apenas casos particulares no hosppital e na casa de repouso
# 


# e)
ggplot(data = dados_shipman, aes(x = AnoDaMorte)) +
    geom_bar(fill = "darkblue", color = 'white') +
    labs(
        x = "Ano",
        y = "Quantidade de Mortos",
        title = "Gráfico de Barras de mortos por ano de Shipman"
    ) +
    theme_minimal()

# Percebe-se que exsite um momento entre 1985 e 1990 com uma grande quantidade de mortos 
# Mas o foco e estopim de assassinatos se deu por volta de 1995 até 2000 
# Onde está os maiores picos de mortos


# f)
# Shipman tem preferência por mulheres com idade mais avançada 
# E possuía padrão de assassina-las em sua própria casa 
# E seu índice de assassinatos aumentou com o decorrer do tempo, 
# terminando seus trabalhos matando enormes quantidades de pessoas



# == Questão 14 == #
# a)
setwd("C:\\Users\\Pc Gamer\\OneDrive - Universidade Federal de Uberlândia\\Faculdade\\5° Período\\Estatística Computacional\\Primeiro Trabalho")
dados_primatas <- read.table("primatas.txt", header = TRUE, sep = ':')

summary(dados_primatas)

dados_primatas$especie <- as.factor(dados_primatas$especie)
dados_primatas$genero <- as.factor(dados_primatas$genero)

summary(dados_primatas)



# b)
ggplot(data = dados_primatas, aes(x = especie)) +
    geom_bar(fill = 'purple', color = 'black') +
    theme_minimal()
    

# Machos e fêmeas
ggplot(data = dados_primatas, aes(x = especie, fill = genero)) +
    geom_bar()


# c)
dados_primatas$altura <- as.numeric(dados_primatas$altura)

dados_bonobo <- dados_primatas[dados_primatas$especie == "bonobo",]

bonobo <- ggplot(data = dados_bonobo, aes(x = peso, y = altura)) +
    geom_point(aes(color = genero), size = 2) + 
    theme_minimal() +
    scale_color_manual(values = c("#FF1493", "#00BFFF")) + 
    labs(
        x = "Peso dos Bonobos",
        y = "Altura dos Bonobos",
        title = "Gráfico de pontos da altura pelo peso dos Bonobos"
        )

dados_chimpanze <- dados_primatas[dados_primatas$especie == "chimpanze",]
chimpanze <- ggplot(data = dados_chimpanze, aes(x = peso, y = altura)) +
    geom_point(aes(color = genero), size = 2) +
    theme_minimal() +
    scale_color_manual(values = c("#FFD700", "#00FF00")) +
    labs(
        x = "Peso dos Chimpanzes",
        y = "Altura dos Chimpanzes",
        title = "Gráfico de pontos da altura pelo peso dos Chimpanzes"
    )

# Gráfico lado a lado
bonobo + chimpanze



# d)
femeas <- dados_primatas[dados_primatas$genero == "femea",]
ggplot(data = femeas, aes(x = peso, y = altura)) +
    geom_point(aes(color = especie), size = 2) +
    theme_minimal() +
    scale_color_manual(values = c("#FF4500", "#008000")) +
    labs(
        x = "Peso",
        y = "Altura",
        title = "Gráfico de comparação de Fêmeas primatas"
    )

machos <- dados_primatas[dados_primatas$genero == "macho",]
ggplot(data = machos, aes(x = peso, y = altura)) +
    geom_point(aes(color = especie), size = 2) +
    theme_minimal() +
    scale_color_manual(values = c("#D2691E", "#9400D3")) +
    labs(
        x = "Peso",
        y = "Altura",
        title = "Gráfico de comparação de Machos primatas"
    )
    

# e)
# Gráfico em conjunto
ggplot(data = dados_primatas, aes(x = peso, y = altura)) +
    geom_point(aes(color = interaction(especie, genero)), size = 2) +
    theme_minimal() +
    labs(color = "Especie e Genero", title = "Gráfico de pontos de primatas (bonobos e chimpanzés)")


#Análise
# Percebe-se que entre os primatas machos, os chimpanzés são muito mais pesados que os bonobos e atingem alturas maiores
# Já entre as fêmeas, as bonobos fêmeas atingem alturas maiores, mas são menos pesadas
# Dessa forma, nota-se que em relação as espécies, os bonobos em geral são mais baixos e mais leves que os chimpanzés machos 
# Mas possuem peso parecido com as fêmeas, todavia são mais altos que elas


# f)
n <- round(0.8 * nrow(dados_primatas))

indices_treino <- sample(1:nrow(dados_primatas), size = n, replace = FALSE)

treino <- dados_primatas[indices_treino,]
teste <- dados_primatas[-indices_treino,]

modelo.arvore <- rpart(formula = especie ~ . , data = treino, method = "class")
rpart.plot(modelo.arvore, extra = 101)

previsao <- predict(modelo.arvore, newdata = teste, method = "class")

previsao[previsao[,1] > previsao[,2]] <- "bonobo"
previsao[previsao[,1] < previsao[,2]] <- "chimpanze"

mean(previsao == teste$especie)











