# Gerar os backgrounds para cada conjunto de dados - 06/11/2025 - Oliveira, 2025

# Carregar pacotes
library(terra)
library(dplyr)
library(flexsdm)
library(readr)

# Importar dados brutos para TGB
gbif <- readr::read_csv("data/ecoregions/Pantanal/gbif_pantanal.csv")
data <- readr::read_csv("data/ecoregions/Pantanal/data_pantanal.csv")

# Importar raster de viés
bias <- terra::rast("rasters/bias/pantanal/onca_bias_processed_pantanal.tif")

# Importar rasters de variáveis ambientais
variables_files <- list.files("rasters/pantanal/", pattern = "\\.tif$", full.names = TRUE)
variables <- terra::rast(variables_files)

# Importar vetor para calibragem da área
pantanal <- terra::vect("shps/Pantanal/pantanal_border.shp")

# Gerar background para dados GBIF
gbif_bg <- flexsdm::sample_background(
  data = gbif,
  x = "Longitude",
  y = "Latitude",
  n = 10000,
  method = "biased",
  rlayer = variables$`CHELSA_bio01_1981-2010_V.2.1`,
  rbias = bias,
  calibarea = pantanal
)

# Gerar background para dados complementares
data_bg <- flexsdm::sample_background(
  data = data,
  x = "Longitude",
  y = "Latitude",
  n = 10000,
  method = "biased",
  rlayer = variables$`CHELSA_bio01_1981-2010_V.2.1`,
  rbias = bias,
  calibarea = pantanal
)

# Exportar backgrounds gerados
readr::write_csv(gbif_bg, "data/ecoregions/Pantanal/gbif_pantanal_bg.csv")
readr::write_csv(data_bg, "data/ecoregions/Pantanal/data_pantanal_bg.csv")
