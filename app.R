library("dplyr")
library("ggplot2")
library("tidyr")

# ---------------------------------Análise dos Dados------------------------------------ #

# Lendo dataset
spotify_dataset <- read.csv("data.csv")
dataset <- spotify_dataset %>%
  select(name, artists, weekly_movement)
head(dataset)


# Verificando existencia de dados N/A
dados_na <- any(is.na(dataset))
contagem_na <- colSums(is.na(dataset))
resumo_na <- data.frame(Column = names(contagem_na), NA_count = contagem_na)
print(resumo_na)

# Substituindo N/A por 0
dataset_sem_na <- replace(dataset, is.na(dataset), 0)
dataset <- dataset_sem_na
print(dataset)

# Removendo colunas com apenas um nível
dataset <- dataset %>% select(-snapshot_date)

#Organizando musicas por weekly_movement
dados_move <- dataset %>%
    group_by(name) %>%
    summarise(weekly_movement = mean(weekly_movement, na.rm = TRUE)) %>%
    arrange(desc(weekly_movement)) %>%
    top_n(10, weekly_movement) %>%
    select(name, weekly_movement)

print(dados_move)

# Imprimindo gráfico por weekly_movement

grafico_move <- ggplot(dados_move, aes(x = reorder(name, weekly_movement), y = weekly_movement)) +
    geom_bar(stat = "identity", fill = "lightblue") +
    labs(title = "Movimentação semanal das músicas",
    x = "Música",
    y = "Movimentação",
    caption = "Dados: Spotify") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))


print(grafico_move)

# ---------------------------------Modelo ML------------------------------------ #

library("caret")


set.seed(123)
trainIndex <- createDataPartition(dataset$weekly_movement, p = .8, list = FALSE)
dataset_train <- dataset[trainIndex, ]
dataset_test <- dataset[-trainIndex, ]

# Verificação e ajuste dos níveis da variável 'name' no conjunto de teste
unique_names_train <- unique(dataset_train$name)
dataset_test$name <- factor(dataset_test$name, levels = unique_names_train)

# Removendo NAs da variável weekly_movement
dataset$weekly_movement <- ifelse(is.na(dataset$weekly_movement), 0, dataset$weekly_movement)

model <- train(weekly_movement ~ ., data = dataset_train, method = "lm")

predictions <- predict(model, newdata = dataset_test)

MAE <- mean(abs(predictions - dataset_test$weekly_movement), na.rm = TRUE)
RMSE <- sqrt(mean((predictions - dataset_test$weekly_movement)^2, na.rm = TRUE))
R2 <- cor(predictions, dataset_test$weekly_movement)

cat("MAE:", MAE, "\n")
cat("RMSE:", RMSE, "\n")
cat("R2:", R2, "\n")

# ---------------------------------Utilização do Modelo------------------------------------ #


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
} else if (mean(prediction_artista > 0)) {
    cat("O artista tende a descer")
} else {
    cat("O modelo não conseguiu determinar")
}

resultados_grafico <- data.frame(Observado = testData$weekly_movement, Previsto = predictions)
ggplot(resultados_grafico, aes(x = 1:nrow(resultados_grafico))) +
  geom_col(aes(y = Observado, fill = "Observado"), position = "dodge") +
  geom_col(aes(y = Previsto, fill = "Previsto"), position = "dodge") +
  scale_fill_manual(values = c("Observado" = "blue", "Previsto" = "red")) +
  labs(title = "Comparação entre Observado e Previsto",
       x = "Amostras",
       y = "Movimentação Semanal",
       fill = "Tipo") +
  theme_minimal()
