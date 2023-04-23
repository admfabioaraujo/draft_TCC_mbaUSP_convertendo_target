#........................................................................................
#                                                                                        #
#      Regressão Múltipla - Previsão GDP - Gross Domestic Product (PIB) - Brasil         #
#  fonte do dataset: https://databank.worldbank.org/source/world-development-indicators  #
#                                                                                        #
#.........................................................................................

# parte 2

## Configurando o diretório de trabalho:
getwd()

## Listar os arquivos do nosso project
list.files()

## Versão utilizada - https://cran.r-project.org/bin/windows/base/old/4.2.1/
R.version # version.string R version 4.2.1 (2022-06-23 ucrt)

## Carregando os pacotes inaugurais:
if(!require(pacman)) install.packages("pacman")
library(pacman)
pacman::p_load(dplyr, car, rstatix, lmtest, ggpubr, 
               QuantPsyc, psych, scatterplot3d)

## Carregando os demais pacotes:
pacotes <- c("readxl","Amelia","visdat", "e1071", "tidyverse","tidyr","car","rgl","scatterplot3", 
             "caret", "rpart", "janitor", "faraway","knitr","kableExtra","plotly","nortest","olsrr",
             "jtools", "huxtable","stargazer", "ggplotly", "ggside",
             "correlation","ggplot2","clipr","see","PerformanceAnalytics","ggraph","lmtest","MASS")
if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T)
} else { 
  sapply (pacotes, require, character = T)
}
(.packages())

## Resgatando o dataset_higienizado:
load(file = "dataset_higienizado.RData")
options(scipen = 999)
View(dataset_higienizado) # 62 X 8

## Convertendo RData em XLSX:
library(clipr)
write_clip(dataset_higienizado)

## Carregando o dataset convertido de RData para XLSX:
library("readxl")
higienizado_convertido <- read_excel("dataset_higienizado_convertido_de_RData_para_MSExcel.xlsx", sheet = "enter+ctrlV")
View(higienizado_convertido) #_ shape original: 62 X 8

## Convertendo o objeto, de tibble para data-frame:
higienizado_convertido <- as.data.frame(higienizado_convertido)
class(higienizado_convertido)

## Convertendo todas as variáveis para num:
higienizado_convertido_2 <- higienizado_convertido
for(i in 1:ncol(higienizado_convertido)) {
  higienizado_convertido_2[,i] <- as.numeric(higienizado_convertido_2[,i])
}
View(higienizado_convertido_2) #_ 62 X 8

## Alterando a grandeza da variável de interesse:
library(dplyr)
higienizado_convertido_3 <- higienizado_convertido_2
higienizado_convertido_3[52,2] #_ 2616156606667
higienizado_convertido_3[1,2] #_ 170304655393906
higienizado_convertido_3$gdp_current_usd <- NULL
higienizado_convertido_3 <- higienizado_convertido_3 %>% mutate(gdp_current_usd = higienizado_convertido_2$gdp_current_usd/1000000000 )
View(higienizado_convertido_3)
higienizado_convertido_3 <- higienizado_convertido_3 %>%
  relocate(gdp_current_usd, .after = adolescent_fertility)
higienizado_convertido_3[52,2] #_ 2616.157
higienizado_convertido_3[1,2] #_ 170304.7
options(scipen = 999)
View(higienizado_convertido_3)

## Salvando o dataset sem valores ausentes:
save(higienizado_convertido_3, file = "higienizado_convertido_3.RData") #_ 62 X 8 
file.info("higienizado_convertido_3")
load(file = "higienizado_convertido_3.RData")
View(higienizado_convertido_3)

## Gerando um bloxplot da variável dependente:
options(scipen = 999)
dev.off()
library(ggplot2)
library(plotly)
library(tibble)
ggplotly(
  ggplot(higienizado_convertido_3,aes(x = "", y = gdp_current_usd)) +
    geom_boxplot(fill = "deepskyblue",    # cor da caixa
                 alpha = 0.7,             # transparência
                 color = "black",         # cor da borda
                 outlier.colour = "red",  # cor dos outliers
                 outlier.shape = 15,      # formato dos marcadores dos outliers
                 outlier.size = 1.5) +    # tamanho dos marcadores dos outliers
    geom_jitter(width = 0.1, alpha = 0.3, size = 2.0, color = "darkorchid") +
    labs(y = "Real_Values_USD") +
    theme(panel.background = element_rect("white"),
          panel.grid = element_line("grey95"),
          panel.border = element_rect(NA),
          legend.position="none",
          plot.title = element_text(size=15)) +
    ggtitle("Boxplot da variável de interesse - Gross Domestic Product") +
    xlab("")
)

# Criando um gráfico kernel density estimation (KDE) - função densidade de probabilidade da
# variável dependente com histograma
dev.off()
ggplotly(
  ggplot(higienizado_convertido_3, aes(x = gdp_current_usd)) +
    geom_density(aes(x = gdp_current_usd), 
                 position = "identity", color = "black", size = 1) +
    geom_histogram(aes(y = ..density..), color = "white", fill = "deepskyblue",
                   bins = 30) +
    theme_classic()
)

## Confirmando a ausência de correlações acima de 0.8:
higienizado_convertido_3_no_target <- higienizado_convertido_3
higienizado_convertido_3_no_target$gdp_current_usd <- NULL
View(higienizado_convertido_3_no_target) #_ shape 62 X 7
correlation <- cor(higienizado_convertido_3_no_target)
library(caret)
findCorrelation(correlation, cutoff = 0.80, verbose = T, names = T) 

## Confirmando a ausência de variáveis inflacionadas:
View(higienizado_convertido_3)
lm_convertido <- lm(higienizado_convertido_3$gdp_current_usd ~., data = higienizado_convertido_3) 
summary(lm_convertido) #_ R Squared = 0.3787   R Adjusted = 0.2982 
library(car)
vif(lm_convertido)  #_ variáveis entre 1.02 e 1.27 

## Seleção de variáveis com a função stepwise:
library(dplyr)
library(MASS)
step_convertido <- step(lm_convertido, direction = "both")
summary(step_convertido) #_ R Squared = 0.335   R Adjusted = 0.3125
#... códigos de significância:    6 ***

## Criando um dataset com a variável target e as varíáveis explicativas estatisticamente significantes, abaixo de 0.05:
higienizado_convertido_step <- higienizado_convertido_3
higienizado_convertido_step <- higienizado_convertido_3 %>% dplyr::select(c(2,6))
View(higienizado_convertido_step) #_ shape 62 X 2

## Salvando o dataset com as variáveis escolhidas pelo procedimento stepwise:
save(higienizado_convertido_step, file = "higienizado_convertido_step.RData")
file.info("higienizado_convertido_step")
load(file = "higienizado_convertido_step.RData")

## Salvando o modelo gerado com dataset sem correlações acima de 0.8 e sem variáveis inflacionadas acimda de 10:
save(lm_convertido, file = "lm_convertido.RData")
load(file = "lm_convertido.RData")

## Calculando o sumário, agrupamento, somatória, skewness, kurtosis, moda, desvio padrão, quartis e range da variável target: 
GDP_summarise <- summarise(higienizado_convertido_step,
                           observações=n(),
                           média=mean(`gdp_current_usd`),
                           mediana=median(`gdp_current_usd`),
                           desv_pad=sd(`gdp_current_usd`),
                           mínimo=min(`gdp_current_usd`),
                           máximo=max(`gdp_current_usd`),
                           quartil_3=quantile(`gdp_current_usd`, type=5, 0.75))
options(scipen = 999)
View(GDP_summarise) 
print(GDP_summarise)
library(e1071)
skewness(higienizado_convertido_step$gdp_current_usd) #_ 0.9221445 - o coeficiente indica assimetria positiva, à direita, sendo a moda > média
kurtosis(higienizado_convertido_step$gdp_current_usd) #_ -0.1895701 - o coeficiente indica grau de achatamento, a distribuição é platicúrtica
var(higienizado_convertido_step$gdp_current_usd) #_ 72622876429
sum(higienizado_convertido_step$gdp_current_usd) #_ 16253914
library(tidyverse)
library(tidyr)
options(scipen = 999)
quantile((higienizado_convertido_step$gdp_current_usd), probs = c(0.01, 0.99))
quantile((higienizado_convertido_step$gdp_current_usd), seq(from = 0, to = 1, by = 0.20))
IQR(higienizado_convertido_step$gdp_current_usd) #_ diferença entre q3 e q1
range(higienizado_convertido_step$gdp_current_usd)
diff(range(higienizado_convertido_step$gdp_current_usd))
summary(higienizado_convertido_step)

# Criando e salvando o gráfico Box Plot de todas as variáveis: 
higienizado_convertido_step_scaled <- as.data.frame(scale(higienizado_convertido_step[,1:2]))
View(higienizado_convertido_step_scaled)
outliers_geral <- higienizado_convertido_step_scaled
outliers_geral <- lapply(outliers_geral, function(x) boxplot.stats(x)$out)
options(scipen = 999)
print(outliers_geral)
library(dplyr)
glimpse(outliers_geral) #_ list
class(outliers_geral)
statisc_list <- list() # lista vazia
dev.off()
for (i in seq_along(higienizado_convertido_step_scaled)) {
  statisc_list[[i]] <- boxplot.stats(higienizado_convertido_step_scaled[[i]])$stats[2]
}
boxplot(higienizado_convertido_step_scaled, col="deepskyblue") -> boxplot_higienizado
points(1:length(statisc_list), statisc_list, col="red") -> boxplot_higienizado
axis(side = 1, at = 1:ncol(dataset_higienizado), labels = colnames(dataset_higienizado)) -> boxplot_higienizado
title(main = "Gráfico Boxplot do dataset convertido - shape 62 X 2") -> boxplot_higienizado

## Visualização do dataset_convertido:
library(dplyr)
library(knitr)
library(kableExtra)
dev.off()
options(scipen = 999)
higienizado_convertido_step %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = F, 
                font_size = 14)

## Visualizando as observações e as especificações referentes às variáveis do dataset
glimpse(higienizado_convertido_step) 

## Estatísticas univariadas:
summary(higienizado_convertido_step)

## Gráfico de dispersão
ggplotly(
  ggplot(higienizado_convertido_step, aes(x = gdp_current_usd, y = savings_mineral_depletion_percent_of_gni)) +
    geom_point(color = "#39568CFF", size = 2.5) +
    geom_smooth(aes(color = "Fitted Values"),
                method = "lm", se = F, size = 2) +
    xlab("gdp_current_usd") +
    ylab("savings_mineral_depletion_percent_of_gni") +
    scale_color_manual("Legenda:",
                       values = "grey50") +
    theme_classic()
)

## Estimando o modelo para regressão linear simples
modelo_convertido <- lm(formula = gdp_current_usd ~ savings_mineral_depletion_percent_of_gni,
                       data = higienizado_convertido_step)

#Observando os parâmetros do modelo_tempodist
summary(modelo_convertido) #_ RSquared: 0.3055     R Adjusted: 0.2939

#Outras maneiras de apresentar os outputs do modelo, com afunção summ do pacote jtools
summ(modelo_convertido, confint = T, digits = 4, ci.width = .95)
export_summs(modelo_convertido, scale = F, digits = 4)

#Salvando fitted values (variável yhat) e residuals (variável erro) no dataset
higienizado_convertido_step$yhat <- modelo_convertido$fitted.values
higienizado_convertido_step$erro <- modelo_convertido$residuals

## Visualizando a base de dados com as variáveis yhat e erro
higienizado_convertido_step %>%
  select(gdp_current_usd, yhat, yhat, erro) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = F, 
                font_size = 22)

## Plotando o Intervalo de Confiança de 90%
ggplotly(
  ggplot(higienizado_convertido_step, aes(x = gdp_current_usd, y = savings_mineral_depletion_percent_of_gni)) +
    geom_point(color = "#39568CFF") +
    geom_smooth(aes(color = "Fitted Values"),
                method = "lm", 
                level = 0.95,) +
    labs(x = "gdp_current_usd",
         y = "savings_mineral_depletion_percent_of_gni") +
    scale_color_manual("Legenda:",
                       values = "grey50") +
    theme_bw()
)

## Calculando os intervalos de confiança
confint(modelo_convertido, level = 0.95) # siginificância 5%

## Fazendo predições em modelos OLS - e.g.: qual seria o PIB, em média, 
## para um savings_mineral_depletion_percent_of_gni de: 0.14421674 [12,2]
predict(object = modelo_convertido,
data.frame(savings_mineral_depletion_percent_of_gni = 0.1442167),
interval = "confidence", level = 0.95)

higienizado_convertido_step[12,1] #_ 488698.3

sum(higienizado_convertido_step$gdp_current_usd) #_ 16253914
sum(higienizado_convertido_step$yhat) #_ 16253914

# .........

