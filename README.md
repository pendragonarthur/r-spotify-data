# Projeto de Análise de Dados e Machine Learning com R

Este é meu primeiro projeto de dados e machine learning, onde utilizo R para analisar um conjunto de dados do Spotify, criar visualizações e desenvolver um modelo preditivo. O projeto inclui etapas de limpeza de dados, visualização e construção de um modelo de regressão linear para prever a movimentação semanal das músicas.

## Pré-requisitos

Certifique-se de ter o R e os pacotes necessários instalados:

```R
install.packages("dplyr")
install.packages("ggplot2")
install.packages("tidyr")
install.packages("caret")
```

## Análise dos Dados

### 1. Leitura e Seleção de Dados

O dataset é lido a partir de um arquivo CSV e as colunas relevantes são selecionadas:

```R
spotify_dataset <- read.csv("data.csv")
dataset <- spotify_dataset %>%
  select(name, artists, weekly_movement)
```

### 2. Verificação e Substituição de Valores N/A

Verificamos a existência de dados faltantes (N/A) e substituímos esses valores por zero:

```R
dados_na <- any(is.na(dataset))
contagem_na <- colSums(is.na(dataset))
resumo_na <- data.frame(Column = names(contagem_na), NA_count = contagem_na)
dataset_sem_na <- replace(dataset, is.na(dataset), 0)
dataset <- dataset_sem_na
```

### 3. Limpeza e Organização dos Dados

Removemos colunas com apenas um nível e organizamos as músicas pela movimentação semanal:

```R
dataset <- dataset %>% select(-snapshot_date)

dados_move <- dataset %>%
  group_by(name) %>%
  summarise(weekly_movement = mean(weekly_movement, na.rm = TRUE)) %>%
  arrange(desc(weekly_movement)) %>%
  top_n(10, weekly_movement)
```

### 4. Visualização dos Dados

Criamos um gráfico de barras para visualizar a movimentação semanal das músicas:

```R
grafico_move <- ggplot(dados_move, aes(x = reorder(name, weekly_movement), y = weekly_movement)) +
  geom_bar(stat = "identity", fill = "lightblue") +
  labs(title = "Movimentação semanal das músicas",
       x = "Música",
       y = "Movimentação",
       caption = "Dados: Spotify") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
print(grafico_move)
```

## Modelo de Machine Learning

### 1. Criação de Partições de Treinamento e Teste

Dividimos o dataset em conjuntos de treinamento e teste:

```R
set.seed(123)
trainIndex <- createDataPartition(dataset$weekly_movement, p = .8, list = FALSE)
dataset_train <- dataset[trainIndex, ]
dataset_test <- dataset[-trainIndex, ]

unique_names_train <- unique(dataset_train$name)
dataset_test$name <- factor(dataset_test$name, levels = unique_names_train)
```

### 2. Treinamento do Modelo

Treinamos um modelo de regressão linear:

```R
model <- train(weekly_movement ~ ., data = dataset_train, method = "lm")
```

### 3. Avaliação do Modelo

Fazemos previsões e avaliamos o desempenho do modelo:

```R
predictions <- predict(model, newdata = dataset_test)

MAE <- mean(abs(predictions - dataset_test$weekly_movement), na.rm = TRUE)
RMSE <- sqrt(mean((predictions - dataset_test$weekly_movement)^2, na.rm = TRUE))
R2 <- cor(predictions, dataset_test$weekly_movement)

cat("MAE:", MAE, "\n")
cat("RMSE:", RMSE, "\n")
cat("R2:", R2, "\n")
```

## Utilização do Modelo

Utilizamos o modelo para prever a movimentação de uma música específica:

```R
nome_artista <- "Sabrina Carpenter"
nome_musica <- "Espresso"

dados_artista <- dataset %>%
  filter(artists == nome_artista & name == nome_musica) %>%
  select(name, artists, weekly_movement)

if (nrow(dados_artista) == 0) {
  cat("Não foram encontrados dados para o artista e música especificados.\n")
} else {
  artista <- data.frame(
    name = dados_artista$name,
    artists = dados_artista$artists,
    weekly_movement = dados_artista$weekly_movement,
    stringsAsFactors = FALSE
  )
}

prediction_artista <- predict(model, newdata = artista)

if(mean(prediction_artista > 0)){
    cat("O artista tende a subir")
} else if (mean(prediction_artista < 0)) {
    cat("O artista tende a descer")
} else {
    cat("O modelo não conseguiu determinar")
}
```

### 4. Visualização dos Resultados

Criamos um gráfico para comparar os valores observados e previstos:

```R
resultados_grafico <- data.frame(Observado = dataset_test$weekly_movement, Previsto = predictions)
ggplot(resultados_grafico, aes(x = 1:nrow(resultados_grafico))) +
  geom_col(aes(y = Observado, fill = "Observado"), position = "dodge") +
  geom_col(aes(y = Previsto, fill = "Previsto"), position = "dodge") +
  scale_fill_manual(values = c("Observado" = "blue", "Previsto" = "red")) +
  labs(title = "Comparação entre Observado e Previsto",
       x = "Amostras",
       y = "Movimentação Semanal",
       fill = "Tipo") +
  theme_minimal()
```

## Conclusão

Este projeto demonstra um fluxo básico de análise de dados e machine learning usando R, desde a limpeza dos dados até a construção e avaliação de um modelo preditivo. É um ótimo ponto de partida para explorar mais técnicas e ferramentas na ciência de dados.
