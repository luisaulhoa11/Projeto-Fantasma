#Projeto Fantasma
#Nome:Luísa Ulhoa Chaves Padula
#Cliente: Warner Bross Entertainment
#Título:"Explorando a Interseção Entre Qualidade Audiovisual e Engajamento: Uma Análise Estatística Abrangente"
#Analises:
    #1) Número de lançamentos a cada década por formato de lançamento;
    #2) Variação da nota IMDB por temporada dos episódios;
    #3) Top 3 terrenos mais frequentes pela ativação da armadilha;
    #4) Relação entre as notas IMDB e engajamento;
    #5) Variação da nota de engajamento pelo personagem que conseguiu capturar o monstro.
#Pacotes----
if (!require(tidyverse)) install.packages(c("tidyverse"))

library(tidyverse)
library(lubridate)
library(readxl)
library(janitor)

setwd("C:/Users/Admin/Documents/ESTAT/template/banco")
banco_f<- read.csv2("banco_final.csv", sep = ",", header = TRUE) ##banco de dados

banco_f
#Análise 1 : Número de lançamentos a cada década por formato de lançamento----
# Instale e carregue as bibliotecas necessárias
# Converta a coluna de datas para o formato Date
banco_f$date_aired <- as.Date(banco_f$date_aired)

# Crie a nova coluna 'decada'
decada <- banco_f %>%
  mutate(decada = cut(date_aired, breaks = "10 years", labels = seq(min(date_aired), max(date_aired), by = "10 years")))

# Visualize as primeiras linhas do dataframe para verificar
head(banco_f)

contagem_decadas <- table(cut(banco_f$date_aired, "10 years"))
print(contagem_decadas)


# Agrupe os dados por década e formato, e então conte o número de observações em cada grupo
contagem_décadas_formato <- banco_f %>%
  mutate(decada = cut(date_aired, "10 years")) %>%
  group_by(decada, format) %>%
  summarize(Numero_de_lancamentos = n()) %>%
  mutate(format=case_when(
    format == "Movie" ~ "Filme",
    format == "Serie" ~ "Série",
    TRUE ~ format))
  


# Visualize os resultados
print(contagem_décadas_formato)

#cores padronizadas
minhas_cores <- c("#A11D21", "#003366", "#CC9900",
                  "#663333", "#FF6600", "#CC9966",
                  "#999966", "#006606", "#008091", 
                  "#041835", "#666666")


# Extrair os quatro primeiros caracteres das décadas
contagem_décadas_formato$decada <- substr(contagem_décadas_formato$decada, start = 1, stop = 4)

# Imprimir a tabela formatada
knitr::kable(contagem_décadas_formato)

# Crie o gráfico de linhas com cores personalizadas e título
ggplot(contagem_décadas_formato, aes(x = contagem_décadas_formato, y = Numero_de_lancamentos)) +
  geom_line(stat = "identity") +
  labs(x = "Década", y = "Número de Lançamentos", fill = "Formato") +
  ggtitle("Número de Lançamentos por Década e Formato")   # Adiciona o título
  
decada
contagem_décadas_formato









