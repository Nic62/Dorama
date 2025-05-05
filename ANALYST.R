# Importando bibliotecas
library(readr)
library(dplyr)
library(ggplot2)

# importando o Data frame
df <- read_csv("C:\\Users\\nickg\\OneDrive\\dorama -analyst\\kdrama_DATASET.csv")
#Pré-Vizualização

##Lendo as primeias dez linhas
head(df,10)

## nome das colunas
colnames(df)


## Extraindo as imnformações
glimpse(df)

## Extraindo a descrição
summary(df)

## Contando valores não nulos para cada coluna
sapply(df, function(x) sum(!is.na(x)))
## Contando valores nulos por coluna
sapply(df, function(x) sum(is.na(x)))

## Contando valores únicos para cada coluna
sapply(df, function(x) n_distinct(x))

# Contando valores duplicados por coluna
sapply(df, function(x) sum(duplicated(x)))

#Visualização dos dados
## Quantidade de Filmes Lançados por Ano
ggplot(df, aes(x = `Year of release`)) +
  geom_bar(fill = "#E6E6FA", color = "black") +
  labs(title = "Quantidade de Filmes Lançados por Ano", 
       x = "Ano de Lançamento", 
       y = "Quantidade de Filmes") +
  theme_minimal()

## Quantidade de Episódios por Filmes
ggplot(df, aes(x = `Number of Episodes`)) +
  geom_bar(fill = "#E6E6FA", color = "black") +
  labs(title = "Quantidade de Episódios por Filmes", 
       x = "Number of Episodes", 
       y = "Quantidade de Filmes") +
  theme_minimal()

## Distribuição dos 10 Principais Gêneros

### Dividir e inumerar os gêneros por vírgula
generos <- strsplit(df$Genre, ", ")
generos_flat <- unlist(generos)
genero_freq <- sort(table(generos_flat), decreasing = TRUE)
top_generos <- head(genero_freq, 10)

### Criar labels com porcentagens
labels <- paste(names(top_generos),
                round(100 * top_generos / sum(top_generos), 1), "%")
### cores
roxo_cores <- c("#D8BFD8", "#DDA0DD", "#DA70D6", "#BA55D3", "#9932CC", 
                "#9400D3", "#8A2BE2", "#9370DB", "#A020F0", "#6A0DAD")
### Gráfico de pizza top 10
pie(top_generos,
    labels = labels,
    main = "Distribuição dos 10 Principais Gêneros",
    col = roxo_cores)



### Gráfico distribuição de Nota
ggplot(df, aes(x = Rating)) +
  geom_histogram(binwidth = 0.1, fill = "#D8BFD8", color = "black") +
  labs(title = "Distribuição das Avaliações (Rating)", 
       x = "Nota", 
       y = "Frequência") +
  theme_minimal()

### Gráfico Número de Episódios e Rating
ggplot(df, aes(x = `Number of Episodes`, y = Rating)) +
  geom_point(color = "#6A0DAD") +
  geom_smooth(method = "lm", color = "purple") +
  labs(title = "Relação entre Número de Episódios e Rating", 
       x = "Número de Episódios", 
       y = "Rating") +
  theme_minimal()
## Calcular nota ao longo dos anos
df_avg_rating_by_year <- df %>%
  group_by(`Year of release`) %>%
  summarise(avg_rating = mean(Rating, na.rm = TRUE))
### Gráfico de notas pelos anos
ggplot(df_avg_rating_by_year, aes(x = `Year of release`, y = avg_rating)) +
  geom_line() +
  geom_point() +
  labs(title = "Média de Rating ao Longo dos Anos", 
       x = "Ano de Lançamento", 
       y = "Média de Rating") +
  theme_minimal()


## Calcular as tags
tags <- strsplit(df$Tags, ", ")
tags_flat <- unlist(tags)
tag_freq <- sort(table(tags_flat), decreasing = TRUE)

### Gráfico das 10 tags +frequentes
top_tags <- head(tag_freq, 10)
barplot(top_tags, main = "Top 10 Tags Mais Frequentes", col = "lightblue", 
        ylab = "Número de Filmes", las = 2)
# Contar a frequência de atores
atores <- strsplit(df$Actors, ", ")
atores_flat <- unlist(atores)
ator_freq <- sort(table(atores_flat), decreasing = TRUE)

# Plotar os 10 atores mais frequentes
top_atores <- head(ator_freq, 10)
barplot(top_atores, main = "Top 10 Atores Mais Frequentes", col = "lightgreen", 
        ylab = "Número de Filmes", las = 2)

