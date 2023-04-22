# Análise exploratíria e transformação de dados brutos

## Como chegamos no arquivo base.csv 

## Pacotes utilizados

library(psych)
library(dplyr)
library (tidyverse) 
library(readr)
library(readr)
library(tidyr)
library(dplyr)
library(purrr)
library(stringr)
library(data.table)
library(ggplot2)

# Configurar o diretório de trabalho
setwd("C:/Users/Marcos/Desktop/Isolde/Londrina")

# Listar todos os arquivos .csv no diretório de trabalho (EPA)
arquivos_csv <- list.files(pattern = "^(199[6-9]|200[0-9]|2010)\\.csv$")

# Ler todos os arquivos e armazená-los em uma lista
lista_dataframes <- lapply(arquivos_csv, function(x) read.csv(x))

# Combinar todos os dataframes em um único dataframe
dataframe_final <- do.call("rbind", lista_dataframes)

# Verificar o resultado
headTail(dataframe_final)


# Selecionar as colunas 'Date' e 'Daily Mean PM10 Concentration' e renomear a última para 'PM10'
pm10 <- dataframe_final %>%
    select(Date, `Daily.Mean.PM10.Concentration`) %>%
    rename(PM10 = `Daily.Mean.PM10.Concentration`)

# Verificar o resultado
headTail(pm10)

# write.csv(pm10, "pm10.csv", row.names = FALSE)

files <- list.files(pattern = "Table_III_.*_New_York_City_NY\\.txt")


# Define a função para extrair dados numéricos de uma linha da tabela
extract_numbers <- function(line) {
    # Separa os valores da linha pelo caractere de tabulação
    values <- strsplit(line, "\t")[[1]]
    
    # Extrai apenas os valores numéricos
    numbers <- as.integer(gsub(",", "", values[2:8]))
    
    return(numbers)
}

# Lista os arquivos de texto
files <- list.files(pattern = "Table_III_.*_New_York_City_NY\\.txt")

# Inicializa uma lista vazia para armazenar os dados de todas as tabelas
data_list <- list()

# Loop pelos arquivos de texto
for (file in files) {
    # Extrai o ano a partir do nome do arquivo
    year <- as.integer(substr(file, 11, 14))
    
    # Lê as linhas do arquivo de texto
    lines <- readLines(file)
    
    # Extrai os dados numéricos de cada linha da tabela
    numbers_list <- lapply(lines[which(grepl("^\\d\\d", lines)):length(lines)], extract_numbers)
    
    # Cria um data frame com os dados numéricos e as informações de ID
    data <- data.frame(ID = paste0(year, "-", sprintf("%02d", 1:length(numbers_list))),
                       matrix(unlist(numbers_list), nrow = length(numbers_list), byrow = TRUE))
    
    # Adiciona os dados à lista
    data_list[[length(data_list) + 1]] <- data
}

# Combina todos os data frames em um único data frame
final_data <- do.call(rbind, data_list)


# Remove as linhas com valores ausentes
final_data <- final_data[complete.cases(final_data), ]

# Exporta o data.frame para um arquivo CSV
write.csv(final_data, "final_data.csv", row.names = FALSE)



############# Explorando 

# 1. Importar os dados de pm10.csv e converter a coluna Date para o formato Date.
pm10 <- read.csv("pm10.csv", stringsAsFactors = FALSE)
pm10$Date <- as.Date(pm10$Date, format = "%m/%d/%Y")

# 2. Importar os dados do final_data.csv
final_data <- read.csv("final_data.csv")

# 3. Verificar quais datas teve medição de PM10 e buscar a semana correspondente no dataframe final_data.
pm10_weeks <- format(pm10$Date, "%Y-%V")
final_data_weeks <- final_data$ID

# 4. Criar um novo dataframe agregando mais 8 colunas no dataframe pm10 com os valores das mortes nas variáveis de X1 a X7 dividido por 7 na semana correspondente à data da medição.
new_pm10 <- data.frame(Date = pm10$Date, PM10 = pm10$PM10)
for (i in 1:7) {
    col_name <- paste0("Deaths_daily_X", i-1)
    new_pm10[[col_name]] <- NA
}

for (i in 1:nrow(pm10)) {
    week <- pm10_weeks[i]
    idx <- match(week, final_data_weeks)
    if (!is.na(idx)) {
        for (j in 1:7) {
            col_name <- paste0("Deaths_daily_X", j-1)
            new_pm10[i, col_name] <- final_data[idx, j+1] / 7
        }
    }
}

# 5. Remover as linhas que não tiveram correspondência entre as datas de medição de PM10 e as semanas do dataframe final_data.
new_pm10 <- new_pm10[complete.cases(new_pm10), ]

write.csv(new_pm10, "new_pm10.csv")

# Renomeando as colunas do dataframe 
colnames(new_pm10) <- c("Date", "PM10", "total", "I65", "I45.64", "I25.44", "I1.24", "I0.1", "IN")

new_pm10

write.csv(new_pm10, "base.csv")


###########################################################################
################# A PARTIR DO ARQUIVO basse.csv ###########################
###########################################################################
dataraw<- read.csv("base.csv")


dataraw$Date <- as.Date(dataraw$Date)


headTail(dataraw)


###### Modelos Matemáticos 


# Ajustando o modelo de regressão linear (morte por todas as causas e faixas etárias/dia)
modelo_g <- lm(total ~ PM10, data = dataraw)
summary(modelo_g)

### Mortes por causas respiratórias 
modelo_i <- lm(IN ~ PM10, data = dataraw)
summary(modelo_i)


# Estratificação dos dados por estação do ano 
outono <- subset(dataraw, format(Date, "%m") %in% c("09", "10", "11"))
inverno <- subset(dataraw, format(Date, "%m") %in% c("12", "01", "02"))
primavera <- subset(dataraw, format(Date, "%m") %in% c("03", "04", "05"))
verao <- subset(dataraw, format(Date, "%m") %in% c("06", "07", "08"))


# Regressões por estação do ano todas as causas
reg_primavera <- lm(total ~ PM10, data = primavera)
summary(reg_primavera)
reg_verao <- lm(total ~ PM10, data = verao)
summary(reg_verao)
reg_outono <- lm(total ~ PM10, data = outono)
summary(reg_outono)
reg_inverno <- lm(total ~ PM10, data = inverno)
summary(reg_inverno)


# Regressões por estação do ano e mortes por Pneumonia e Influenza
reg_primavera_i <- lm(IN ~ PM10, data = primavera)
summary(reg_primavera_i)
reg_verao_i <- lm(IN ~ PM10, data = verao)
summary(reg_verao_i)
reg_outono_i <- lm(IN ~ PM10, data = outono)
summary(reg_outono_i)
reg_inverno_i <- lm(IN ~ PM10, data = inverno)
summary(reg_inverno_i)



# Regressões por estação do ano e mortes em idosos >65 anos
reg_primavera_65 <- lm(I65 ~ PM10, data = primavera)
summary(reg_primavera_65)
reg_verao_65 <- lm(I65 ~ PM10, data = verao)
summary(reg_verao_65)
reg_outono_65 <- lm(I65 ~ PM10, data = outono)
summary(reg_outono_65)
reg_inverno_65 <- lm(I65 ~ PM10, data = inverno)
summary(reg_inverno_65)


#### Subset dos anos de 2009 e 2010

### Dispersão 
library(ggpmisc)

# Filtrando dados para 2009 e 2010
dados_2009_2010 <- subset(dataraw, format(Date, "%Y") %in% c("2009", "2010"))

# Criando gráfico de dispersão com reta de regressão linear e sombreamento do SE
p <- ggplot(dados_2009_2010, aes(x = PM10, y = total)) +
    geom_point() +
    geom_smooth(method = "lm", se = TRUE, linetype = "solid", color = "blue") +
    theme_bw() +
    labs(x = "PM10 (μg/m³)", y = "Mortes Diárias", title = "Dispersão de PM10 x Mortes Diárias (2009-2010)")

print(p)



# Filtrando dados por estação do ano
primavera_2009_2010 <- subset(dados_2009_2010, format(Date, "%m") %in% c("09", "10", "11"))
verao_2009_2010 <- subset(dados_2009_2010, format(Date, "%m") %in% c("12", "01", "02"))
outono_2009_2010 <- subset(dados_2009_2010, format(Date, "%m") %in% c("03", "04", "05"))
inverno_2009_2010 <- subset(dados_2009_2010, format(Date, "%m") %in% c("06", "07", "08"))

# Adicionando a coluna 'Estacao' aos dataframes
primavera_2009_2010$Estacao <- "Primavera"
verao_2009_2010$Estacao <- "Verão"
outono_2009_2010$Estacao <- "Outono"
inverno_2009_2010$Estacao <- "Inverno"

# Combinando todos os dataframes
dados_estacoes <- rbind(primavera_2009_2010, verao_2009_2010, outono_2009_2010, inverno_2009_2010)

# Criando gráficos por estação do ano
p <- ggplot(dados_estacoes, aes(x = PM10, y = total)) +
    geom_point() +
    geom_smooth(method = "lm", se = TRUE, linetype = "solid", color = "blue") +
    theme_bw() +
    labs(x = "PM10 (μg/m³)", y = "Mortes Diárias", title = "Dispersão de PM10 x Mortes Diárias (2009-2010)") +
    facet_wrap(~ Estacao, ncol = 2)

print(p)


# Regressões por estação do ano todas as causas (2009 - 2010) 
reg_primavera <- lm(total ~ PM10, data = primavera_2009_2010)
summary(reg_primavera)
reg_verao <- lm(total ~ PM10, data = verao_2009_2010)
summary(reg_verao)
reg_outono <- lm(total ~ PM10, data = outono_2009_2010)
summary(reg_outono)
reg_inverno <- lm(total ~ PM10, data = inverno_2009_2010)
summary(reg_inverno)


# Regressões por estação do ano e mortes por Pneumonia e Influenza (2009 - 2010)
reg_primavera_i <- lm(IN ~ PM10, data = primavera_2009_2010)
summary(reg_primavera_i)
reg_verao_i <- lm(IN ~ PM10, data = verao_2009_2010)
summary(reg_verao_i)
reg_outono_i <- lm(IN ~ PM10, data = outono_2009_2010)
summary(reg_outono_i)
reg_inverno_i <- lm(IN ~ PM10, data = inverno_2009_2010)
summary(reg_inverno_i)



# Regressões por estação do ano e mortes em idosos >65 anos (2009 - 2010)
reg_primavera_65 <- lm(I65 ~ PM10, data = primavera_2009_2010)
summary(reg_primavera_65)
reg_verao_65 <- lm(I65 ~ PM10, data = verao_2009_2010)
summary(reg_verao_65)
reg_outono_65 <- lm(I65 ~ PM10, data = outono_2009_2010)
summary(reg_outono_65)
reg_inverno_65 <- lm(I65 ~ PM10, data = inverno_2009_2010)
summary(reg_inverno_65)



