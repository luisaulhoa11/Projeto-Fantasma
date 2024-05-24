#Projeto Fantasma----
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
if (!require(xtable)) install.packages("xtable")

library(tidyverse)
library(lubridate)
library(readxl)
library(janitor)
library(gridExtra)
library(xtable)

setwd("C:/Users/Admin/Documents/ESTAT/template/banco")
banco_final<- read.csv2("banco_final.csv", sep = ",", header = TRUE) ##banco de dados

#Análise 1 : Número de lançamentos a cada década por formato de lançamento----
banco_final$date_aired <- as.Date(banco_final$date_aired)

contagem_decadas <- table(cut(banco_final$date_aired, "10 years"))
print(contagem_decadas)

# Agrupei os dados por década e formato
  mutate(decada = cut(date_aired, "10 years")) %>%
  group_by(decada, format) %>%
  summarize(Numero_de_lancamentos = n()) %>%
  mutate(format=case_when(
    format == "Movie" ~ "Filme",
    format == "Serie" ~ "Série",
    TRUE ~ format))

print(contagem_décadas_formato)
#cores padronizadas
minhas_cores <- c("#A11D21", "#003366", "#CC9900",
                  "#663333", "#FF6600", "#CC9966",
                  "#999966", "#006606", "#008091", 
                  "#041835", "#666666")


contagem_décadas_formato$decada <- substr(contagem_décadas_formato$decada, start = 1, stop = 4)

# tabela formatada
knitr::kable(contagem_décadas_formato)

decada
contagem_décadas_formato

# linhas
linhas_formato <- contagem_décadas_formato %>%
  group_by(format) %>%
  arrange(format, decada) %>%
  summarize(linhas_decada = list(decada),
            linhas_lancamentos = list(Numero_de_lancamentos)) %>%
  unnest(c(linhas_decada, linhas_lancamentos)) %>%
  rename(decada_linha = linhas_decada, Numero_de_lancamentos = linhas_lancamentos)

#gráfico de linhas com cores personalizadas
ggplot(contagem_décadas_formato, aes(x = decada, y = Numero_de_lancamentos, colour = format)) +
  geom_line(data = linhas_formato, aes(x = decada_linha, y = Numero_de_lancamentos, group = format), 
            size = 1, # Espessura da linha
            alpha = 0.7, # Transparência das linhas
            linetype = "solid") + # Tipo de linha
  geom_point() +
  labs(x = "Década", y = "Número de Lançamentos", colour = "Formato") +
  scale_colour_manual(values = minhas_cores) + # Definir cores personalizadas
  theme_minimal()

#Análise 2: Variação da nota IMDB por temporada dos episódios----

# Filtrar as temporadas que não são do tipo série
banco_filtrado <- subset(banco_final, !(season %in% c("Movie", "Crossover", "Special")))

# média da nota IMDb por temporada
media_imdb_por_temporada <- aggregate(imdb ~ season, data = banco_filtrado, FUN = function(x) round(mean(x), 2))
print(media_imdb_por_temporada)

#boxplot
ggplot(banco_filtrado, aes(x = season, y = imdb, fill = season)) +
  geom_boxplot() +
  scale_fill_manual(values = minhas_cores) +  # Aplica suas cores personalizadas
  labs(x = "Temporada", y = "Nota IMDB", fill="temporada") +
  ggtitle("Variação das notas IMDB por temporada")


#desvio padrão 

desvio_padrao_por_temporada <- aggregate(imdb ~ season, data = banco_filtrado, FUN = function(x) round(sd(x), 2))
print(desvio_padrao_por_temporada)

estatisticas <- banco_final %>%
  filter(season %in% c("1", "2", "3", "4")) %>%
  group_by(season) %>%
  summarise(Media = mean(imdb),
            Mediana = median(imdb),
            Desvio_Padrao = sd(imdb),
            Maximo = max(imdb)) %>%
  pivot_longer(!season) %>%
  pivot_wider(names_from = season, values_from = value)


# print(estatisticas)

#Análise 3: Top 3 terrenos mais frequentes pela ativação da armadilha
table(banco_final$setting_terrain)
#Urban,Rural,Forest
terrenos <- c("Urbano", "Rural", "Floresta")
frequencias <- c(267, 109, 48)
densidade_frequencia <- frequencias / sum(frequencias)
dados <- data.frame(terrenos, densidade_frequencia)
#Gráfico de pizza
proporcoes <- round(frequencias / sum(frequencias) * 100, 1)
 ggplot(dados, aes(x = "", y = frequencias, fill = terrenos)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  labs(title = "Terrenos Mais Frequentes (Proporções)", fill = "Terreno") +
  theme_void() +
  scale_fill_manual(values = minhas_cores) +
  geom_text(aes(label = paste0(terrenos, "\n", proporcoes, "%")), position = position_stack(vjust = 0.5))
 
frequência_Urban <- 267/603
frequência_Rural <- 109/603
frequência_Forest <- 48/603

frequência_Urban
frequência_Rural
frequência_Forest

frequência_total <- c(frequência_Urban, frequência_Rural, frequência_Forest)

dados <- data.frame(terrenos = c("Urbano", "Rural", "Floresta"),
                    Frequencia = c(frequência_Urban, frequência_Rural, frequência_Forest))

# Gráfico de barras frequência dos 3 em relação ao total
densidade_Urban <- frequencia_Urban / total_frequencias
densidade_Rural <- frequencia_Rural / total_frequencias
densidade_Forest <- frequencia_Forest / total_frequencias
ggplot(dados, aes(x = terrenos, y = Frequencia, fill = terrenos)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = minhas_cores) +
  labs(title = "Frequência dos Top 3 Terrenos em Relação ao Total", x = "Terreno", y = "Frequência") +
  theme_minimal()

terrenosfreq <- c("Urban", "Rural", "Forest")
banco_filtrado <- banco_final %>%
  filter(!is.na(trap_work_first), setting_terrain %in% terrenosfreq)
rename(banco_filtrado, "julia" = trap_work_first)

#tabela de contingência
tabela <- table(banco_filtrado$setting_terrain, banco_filtrado$trap_work_first)

print(tabela)
View(tabela)


#  gráfico de colunas
ggplot(banco_filtrado, aes(x = setting_terrain, fill=trap_work_first)) +
  geom_bar(position = "dodge") +
  labs(x = "Tipo de Terreno", y = "Ativação da armadilha", fill="Armadilha") +
  scale_fill_manual(values = minhas_cores, labels = c("Funcionou", "Não Funcionou")) +
  ggtitle("Ativação da Armadilha por Tipo de Terreno") +
  theme_minimal()


#Análise 4 : Relação entre as notas IMDB e engajamento
#Gráfico de dispersão
ggplot(banco_final, aes(x = imdb, y = engagement)) +
  geom_point(color= "#A11D21") +
  scale_color_manual(values = minhas_cores) +
  labs(x = "Notas IMDB", y = "Engajamento") +
  ggtitle("Relação entre as notas IMDB e o Engajamento")

#Análise 5:Variação da nota de engajamento pelo personagem que conseguiu capturar o monstro
banco_final$caught_fred
linhasfred<-which(banco_final$caught_fred == TRUE)
View(linhasfred)
engajamentofred <- banco_final$engagement[linhasfred]
engajamentofred
mean(engajamentofred)
sd(engajamentofred)

banco_final$caught_daphnie
linhasdaphnie <- which(banco_final$caught_daphnie==TRUE)
engajamentodaphnie<- banco_final$engagement[linhasdaphnie]
engajamentodaphnie
mean(engajamentodaphnie)
sd(engajamentodaphnie)

banco_final$caught_velma
linhasvelma <- which(banco_final$caught_velma==TRUE)
engajamentovelma<- banco_final$engagement[linhasvelma]
engajamentovelma
mean(engajamentovelma)
sd(engajamentovelma)

banco_final$caught_shaggy
linhasshaggy <- which(banco_final$caught_shaggy==TRUE)
engajamentoshaggy<- banco_final$engagement[linhasshaggy]
engajamentoshaggy
mean(engajamentoshaggy)
sd(engajamentoshaggy)

banco_final$caught_scooby
linhasscooby <- which(banco_final$caught_scooby==TRUE)
engajamentoscooby<- banco_final$engagement[linhasscooby]
engajamentoscooby
mean(engajamentoscooby)
sd(engajamentoscooby)

banco_final$caught_other
linhasoutro <- which(banco_final$caught_other==TRUE)
engajamentooutro<- banco_final$engagement[linhasoutro]
engajamentooutro
mean(engajamentooutro)
sd(engajamentooutro)

#gráfico
engajamentos <- list(
  "Fred" = engajamentofred,
  "Daphnie" = engajamentodaphnie,
  "Velma" = engajamentovelma,
  "Shaggy" = engajamentoshaggy,
  "Scooby" = engajamentoscooby,
  "Outro" = engajamentooutro)


#  gráfico boxplot

boxplot(engajamentos,
        main = "Engajamento por Personagem",
        xlab = "Personagem",
        ylab = "Engajamento",
        col= minhas_cores,
        border = "black")

media<- round(c(mean(engajamentofred), mean(engajamentodaphnie), mean(engajamentovelma), mean(engajamentoshaggy), mean(engajamentoscooby), mean(engajamentooutro)), 2)
desvio_padrao<- round(c(sd(engajamentofred), sd(engajamentodaphnie), sd(engajamentovelma), sd(engajamentoshaggy), sd(engajamentoscooby), sd(engajamentooutro)), 2)
mediana<- round(c(median(engajamentofred), median(engajamentodaphnie), median(engajamentovelma), median(engajamentoshaggy), median(engajamentoscooby), median(engajamentooutro)), 2)

# fiz um data.frame das medidas resumo
estatisticas <- data.frame(
  Personagem = c("Fred", "Daphnie", "Velma", "Shaggy", "Scooby", "Other"),
  Media = media,
  Desvio_Padrao = desvio_padrao,
  Mediana = mediana)

# Imprimir o data frame
print(estatisticas)
view(estatisticas)





#

