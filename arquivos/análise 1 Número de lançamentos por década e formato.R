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

#TEMA DA ESTAT 
estat_colors <- c(
  "#A11D21", "#003366", "#CC9900",
  "#663333", "#FF6600", "#CC9966",
  "#999966", "#006606", "#008091",
  "#041835", "#666666" )

theme_estat <- function(...) {
  theme <- ggplot2::theme_bw() +
    ggplot2::theme(
      axis.title.y = ggplot2::element_text(colour = "black", size = 12),
      axis.title.x = ggplot2::element_text(colour = "black", size = 12),
      axis.text = ggplot2::element_text(colour = "black", size = 9.5),
      panel.border = ggplot2::element_blank(),
      axis.line = ggplot2::element_line(colour = "black"),
      legend.position = "top",
      ...
    )
  return(
    list(
      theme,
      scale_fill_manual(values = estat_colors),
      scale_colour_manual(values = estat_colors)
    )
  )
}


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

#gráfico de linhas com cores da estat
ggplot(contagem_décadas_formato) +
  aes(x = decada, y = Numero_de_lancamentos, group = format, colour = format) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(x = "Década", y = "Número de lançamentos", colour= "Formato") +
  theme_estat()
ggsave("series_grupo.pdf", width = 158, height = 93, units = "mm")

#Análise 2: Variação da nota IMDB por temporada dos episódios----
season <- banco_filtrado$season


# Filtrar as temporadas que não são do tipo série
banco_filtrado <- subset(banco_final, !(season %in% c("Movie", "Crossover", "Special")))

# média da nota IMDb por temporada
media_imdb_por_temporada <- aggregate(imdb ~ season, data = banco_filtrado, FUN = function(x) round(mean(x), 2))
print(media_imdb_por_temporada)

str(banco_filtrado$season)

class(banco_filtrado$season)

banco_filtrado$season <- factor(banco_filtrado$season)

#BOXPLOT 
ggplot(banco_filtrado) +
  aes(x = season, y = imdb) +
  geom_boxplot(fill = "#A11D21", width = 0.5) +
  stat_summary(
    fun = "mean", geom = "point", shape = 23, size = 3, fill = "white"
  ) +
  labs(x = "Temporada", y = "Nota IMDB") +
  theme_estat()

#BOXPLOT COM MEDIANAS CRESCENTES
medianas_por_season <- aggregate(imdb ~ season, data = banco_filtrado, FUN = median)

banco_filtrado$season <- factor(banco_filtrado$season, levels = medianas_por_season[order(medianas_por_season$imdb), "season"])

ggplot(banco_filtrado) +
  aes(x = season, y = imdb) +
  geom_boxplot(fill = "#A11D21", width = 0.5) +
  stat_summary(
    fun = "mean", geom = "point", shape = 23, size = 3, fill = "white"
  ) +
  labs(x = "Temporada", y = "Nota IMDB") +
  theme_estat()

#desvio padrão 

desvio_padrao_por_temporada <- aggregate(imdb ~ season, data = banco_filtrado, FUN = function(x) round(sd(x), 2))
print(desvio_padrao_por_temporada)


#TABELA

estatisticas <- banco_final %>%
  filter(season %in% c("1", "2", "3", "4")) %>%
  group_by(season) %>%
  summarise("Média" = mean(imdb),
            "Desvio_Padrão" = sd(imdb),
            "Variância" = var(imdb),
            "Mínimo" = min(imdb),
            "1ª Quartil" = quantile(imdb, probs = 0.25),
            "Mediana" = median(imdb),
            "3ª Quartil" = quantile(imdb, probs = 0.75),
            "Máximo" = max(imdb)) %>%
  pivot_longer(!season) %>%
  pivot_wider(names_from = season, values_from = value)

print(estatisticas)

print( xtable(estatisticas, caption="Estatísticas das notas IMDB por temporada", label="tab:estatisticas_temporada"),
       include.rownames=FALSE)

#Análise 3: Top 3 terrenos mais frequentes pela ativação da armadilha----
table(banco_final$setting_terrain)
#Urban,Rural,Forest
terrenos <- c("Urbano", "Rural", "Floresta")
frequencias <- c(267, 109, 48)
densidade_frequencia <- frequencias / sum(frequencias)
dados <- data.frame(terrenos, densidade_frequencia)
frequência_Urban <- 267/603
frequência_Rural <- 109/603
frequência_Forest <- 48/603

frequência_Urban
frequência_Rural
frequência_Forest

frequência_total <- c(frequência_Urban, frequência_Rural, frequência_Forest)

dados <- data.frame(terrenos = c("Urbano", "Rural", "Floresta"),
                    Frequencia = c(frequência_Urban, frequência_Rural, frequência_Forest))


terrenosfreq <- c("Urban", "Rural", "Forest")
banco_filtrado <- banco_final %>%
  filter(!is.na(trap_work_first), setting_terrain %in% terrenosfreq)
rename(banco_filtrado, "julia" = trap_work_first)

#tabela de contingência
tabela <- table(banco_filtrado$setting_terrain, banco_filtrado$trap_work_first)

print(tabela)

View(tabela)

ajustes<- banco_filtrado%>%
  mutate(trap_work_first = case_when(
    trap_work_first%>% str_detect("FALSE") ~ "Não funcionou",
    trap_work_first%>% str_detect("TRUE") ~ "Funcionou"
  )) %>%
  group_by( setting_terrain, trap_work_first) %>%
  summarise(freq = n()) %>%
  mutate(
    freq_relativa = round(freq / sum(freq) * 100,1))

porcentagens <- str_c(ajustes$freq_relativa, "%") %>% str_replace("
\\.", ",")
legendas <- str_squish(str_c(ajustes$freq, " (", porcentagens, ")")
)
ggplot(ajustes) +
  aes(
    x = fct_reorder(setting_terrain, freq, .desc = T), y = freq,
    fill = trap_work_first, label = legendas
  ) +
  geom_col(position = position_dodge2(preserve = "single", padding =
                                        0)) +
  geom_text(
    position = position_dodge(width = .9),
    vjust = -0.5, hjust = 0.5,
    size = 3
  ) +
  labs(x = "Transmissão", y = "Frequência", fill="Armadilha") +
  theme_estat()
ggsave("colunas-bi-freq.pdf", width = 158, height = 93, units = "mm")16

#Análise 4 : Relação entre as notas IMDB e engajamento----
#Gráfico de dispersão
ggplot(banco_final, aes(x = imdb, y = engagement)) +
  geom_point(color= "#A11D21",size = 2) +
  scale_color_manual(values = minhas_cores) +
  labs(x = "Notas IMDB", y = "Engajamento") +
  theme_estat()
ggsave("disp_uni.pdf", width = 158, height = 93, units = "mm")
24
#quadro resumo da variável engajamento
medidasengajamento <- banco_filtrado%>%
  group_by(engagement) %>%
  summarise("Média" = mean(engagement),
            "Desvio_Padrão" = sd(engagement),
            "Variância" = var(engagement),
            "Mínimo" = min(engagement),
            "1ª Quartil" = quantile(engagement, probs = 0.25),
            "Mediana" = median(engagement),
            "3ª Quartil" = quantile(engagement, probs = 0.75),
            "Máximo" = max(engagement)) %>%
  

mean(banco_filtrado$engagement)
sd(banco_filtrado$engagement)
var(banco_filtrado$engagement)
min(banco_filtrado$engagement)
quantile(banco_filtrado$engagement, probs=0.25)
median(banco_filtrado$engagement)
quantile(banco_filtrado$engagement,probs=0.75)
max(banco_filtrado$engagement)

#quadro resumo da variável notas do IMDB
mean(banco_filtrado$imdb)
sd(banco_filtrado$imdb)
var(banco_filtrado$imdb)
min(banco_filtrado$imdb)
quantile(banco_filtrado$imdb, probs=0.25)
median(banco_filtrado$imdb)
quantile(banco_filtrado$imdb,probs=0.75)
max(banco_filtrado$imdb)

#Análise 5:Variação da nota de engajamento pelo personagem que conseguiu capturar o monstro
banco_final$caught_fred
linhasfred<-which(banco_final$caught_fred == TRUE)
engajamentofred <- banco_final$engagement[linhasfred]
engajamentofred
mean(engajamentofred)
sd(engajamentofred)
var(engajamentofred)
min(engajamentofred)
quantile(engajamentofred, probs = 0.25)
median(engajamentofred)
quantile(engajamentofred, probs=0.75)
max(engajamentofred)

banco_final$caught_daphnie
linhasdaphnie <- which(banco_final$caught_daphnie==TRUE)
engajamentodaphnie<- banco_final$engagement[linhasdaphnie]
engajamentodaphnie
mean(engajamentodaphnie)
sd(engajamentodaphnie)
var(engajamentodaphnie)
min(engajamentodaphnie)
quantile(engajamentodaphnie,probs=0.25)
median(engajamentodaphnie)
quantile(engajamentodaphnie,probs=0,75)
max(engajamentodaphnie)

banco_final$caught_velma
linhasvelma <- which(banco_final$caught_velma==TRUE)
engajamentovelma<- banco_final$engagement[linhasvelma]
engajamentovelma
mean(engajamentovelma)
sd(engajamentovelma)
var(engajamentovelma)
min(engajamentovelma)
quantile(engajamentovelma,probs=0.25)
median(engajamentovelma)
quantile(engajamentovelma,probs=0.75)
max(engajamentovelma)

banco_final$caught_shaggy
linhasshaggy <- which(banco_final$caught_shaggy==TRUE)
engajamentoshaggy<- banco_final$engagement[linhasshaggy]
engajamentoshaggy
mean(engajamentoshaggy)
sd(engajamentoshaggy)
var(engajamentoshaggy)
min(engajamentoshaggy)
quantile(engajamentoshaggy,probs=0.25)
median(engajamentoshaggy)
quantile(engajamentoshaggy,probs=0.75)
max(engajamentoshaggy)


banco_final$caught_scooby
linhasscooby <- which(banco_final$caught_scooby==TRUE)
engajamentoscooby<- banco_final$engagement[linhasscooby]
engajamentoscooby
mean(engajamentoscooby)
sd(engajamentoscooby)
var(engajamentoscooby)
min(engajamentoscooby)
quantile(engajamentoscooby,probs=0.25)
median(engajamentoscooby)
quantile(engajamentoscooby,probs=0,75)
max(engajamentoscooby)


banco_final$caught_other
linhasoutro <- which(banco_final$caught_other==TRUE)
engajamentooutro<- banco_final$engagement[linhasoutro]
engajamentooutro
mean(engajamentooutro)
sd(engajamentooutro)
var(engajamentooutro)
min(engajamentooutro)
quantile(engajamentooutro,probs=0.25)
median(engajamentooutro)
quantile(engajamentooutro,probs=0.75)
max(engajamentooutro)

#gráfico
engajamento <- list(
  "Fred" = engajamentofred,
  "Daphnie" = engajamentodaphnie,
  "Velma" = engajamentovelma,
  "Shaggy" = engajamentoshaggy,
  "Scooby" = engajamentoscooby,
  "Outro" = engajamentooutro)

engajamentos 


# Criar um dataframe com os valores de engajamento de cada personagem
dados_engajamento <- data.frame(
  Personagem = rep(c("Fred", "Daphnie", "Velma", "Shaggy", "Scooby", "Outro"), 
                   c(length(engajamentofred), 
                     length(engajamentodaphnie), 
                     length(engajamentovelma), 
                     length(engajamentoshaggy), 
                     length(engajamentoscooby), 
                     length(engajamentooutro))),
  Engajamento = c(engajamentofred, engajamentodaphnie, engajamentovelma, 
                  engajamentoshaggy, engajamentoscooby, engajamentooutro)
)

# Criar boxplots
ggplot(dados_engajamento, aes(x = Personagem, y = Engajamento)) +
  geom_boxplot(fill = "#A11D21", width = 0.5) +
  stat_summary(fun = "mean", geom = "point", shape = 23, size = 3, fill = "white") +
  labs(x = "Personagem", y = "Engajamento") +
  theme_estat()


# Calcular as medianas de cada personagem
medianas <- c(
  median(engajamentofred),
  median(engajamentodaphnie),
  median(engajamentovelma),
  median(engajamentoshaggy),
  median(engajamentoscooby),
  median(engajamentooutro))

# Criar um dataframe com as medianas e os nomes dos personagens
dados_medianas <- data.frame(
  Personagem = c("Fred", "Daphnie", "Velma", "Shaggy", "Scooby", "Outro"),
  Mediana = medianas)

# Ordenar o dataframe das medianas
dados_medianas <- dados_medianas[order(dados_medianas$Mediana), ]

# Reordenar os valores de engajamento de acordo com a ordem das medianas
dados_engajamento$Personagem <- factor(dados_engajamento$Personagem, levels = dados_medianas$Personagem)

# Definir a ordem desejada das personagens com base nas medianas
ordem_personagens <- dados_medianas$Personagem[order(dados_medianas$Mediana)]

# Criar o boxplot
ggplot(dados_engajamento, aes(x = factor(Personagem, levels = ordem_personagens), y = Engajamento)) +
  geom_boxplot(fill = "#A11D21", width = 0.5) +
  stat_summary(fun = "mean", geom = "point", shape = 23, size = 3, fill = "white") +
  labs(x = "Personagem", y = "Engajamento") +
  theme_estat()

