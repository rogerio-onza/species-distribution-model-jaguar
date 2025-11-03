# Limpeza e validação de dados - 03/11/2025 - Oliveira, 2025

# 1ª - Limpar e validar as coordenadas

# Carrega os pacotes necessários
library(CoordinateCleaner)
library(countrycode)
library(dplyr)
library(ggplot2)
library(sf)
library(readr)
library(rnaturalearthdata)

# Importa os dados brutos do GBIF
data <- readr::read_csv("data_raw/dataset_long_lat_countryCode.csv")

# Seleciona apenas as colunas relevantes para análise - GBIF
data <- data %>%
  dplyr::select(species, decimalLongitude, decimalLatitude, countryCode, 
                individualCount, gbifID, family, taxonRank, 
                coordinateUncertaintyInMeters, year, basisOfRecord, 
                institutionCode)

# Remove registros sem coordenadas geográficas
data <- data %>%
  dplyr::filter(!is.na(decimalLongitude), !is.na(decimalLatitude))

# Dados fora do GBIF
data <- data %>%
  dplyr::filter(!is.na(Longitude), !is.na(Latitude))

# Converte códigos de país de ISO2 para ISO3
data$countryCode <- countrycode::countrycode(data$countryCode, 
                                             origin = "iso2c", 
                                             destination = "iso3c")

# Converte para data.frame para compatibilidade com CoordinateCleaner
data <- data.frame(data)

# Identifica coordenadas problemáticas usando múltiplos testes
flags <- CoordinateCleaner::clean_coordinates(
  x = data, 
  lon = "Longitude", # trocar para decimalLongitude (GBIF)
  lat = "Latitude", # trocar para decimalLongitude (GBIF)
  countries = "countryCode",
  species = "species",
  tests = c("capitals", "centroids", "equal", "zeros", "countries",
            "institutions", "gbif", "outliers", "seas"),
  seas_scale = 110
)

# Separa dados limpos e problemáticos
data_cl <- data[flags$.summary, ]
data_fl <- data[!flags$.summary, ]

# Visualiza a distribuição da incerteza de coordenadas
data_cl %>% 
  dplyr::mutate(Uncertainty = coordinateUncertaintyInMeters / 1000) %>% 
  ggplot2::ggplot(aes(x = Uncertainty)) + 
  ggplot2::geom_histogram() +
  ggplot2::xlab("Incerteza de coordenadas (km)") +
  ggplot2::theme_bw()

# Remove registros com incerteza de coordenadas maior que 50 km
data_cl <- data_cl %>%
  dplyr::filter(coordinateUncertaintyInMeters / 1000 <= 50 | 
                  is.na(coordinateUncertaintyInMeters))

# Verifica a distribuição de contagem individual
table(data_cl$individualCount)

# Filtra contagens individuais inválidas ou extremas
data_cl <- data_cl %>%
  dplyr::filter(individualCount > 0 | is.na(individualCount)) %>%
  dplyr::filter(individualCount < 99 | is.na(individualCount))

# Visualiza a distribuição temporal dos dados
data_cl %>%
  ggplot2::ggplot(aes(x = year)) +
  ggplot2::geom_bar() +
  ggplot2::theme_bw()

# Verifica a distribuição por família taxonômica
table(data_cl$family)

# Identifica possíveis erros de conversão ddmm para decimal
out_ddmm <- CoordinateCleaner::cd_ddmm(
  data_cl, 
  lon = "Longitude", # trocar para decimalLongitude (GBIF)
  lat = "Latitude", # trocar para decimalLatitude (GBIF)
  ds = "species", 
  diagnostic = TRUE, 
  diff = 1,
  value = "dataset"
)

# Identifica e remove coordenadas arredondadas ou em grade
data_clean <- CoordinateCleaner::cd_round(
  data_cl, 
  lon = "Longitude", # trocar para decimalLongitude (GBIF)
  lat = "Latitude", # trocar para decimalLongitude (GBIF)
  ds = "species",
  value = "clean",
  T1 = 7,
  graphs = FALSE,
  verbose = TRUE
)

# Prepara dados finais para exportação
data_export <- data_clean %>%
  dplyr::select(Longitude, Latitude) # trocar para decimalLongitude/Latitude (GBIF)

# Exporta os dados limpos
readr::write_csv(data_export, "data/dataset_final.csv")

# 2ª - Excluir coordenadas repetidas entre os conjuntos de dados

# Importar dados limpos
gbif <- readr::read_csv("data/gbif_final.csv")
dataset <- readr::read_csv("data/dataset_final.csv")

# Remover coordenadas duplicadas
gbif_uni  <- gbif %>% 
  distinct(Longitude, Latitude)
dataset_uni  <- dataset %>% 
  distinct(Longitude, Latitude)

# Remover coordenadas duplicadas entre os conjuntos de dados
gbif_nodupl <- gbif_uni %>%
  dplyr::anti_join(dataset_uni,
                   by = c("Longitude","Latitude"))

# Exportar dados finais
readr::write_csv(dataset_uni, "data/dataset.csv")
readr::write_csv(gbif_nodupl,"data/gbif.csv")

