# Recortar presenças baseada no shapefile da ecorregião - 04/11/2025 - Oliveira, 2025

# Carregar pacotes necessários
library(terra)
library(readr)
library(dplyr)

# Importar dados de ocorrências
gbif <- readr::read_csv("data/gbif_final.csv")
data <- readr::read_csv("data/dataset_final.csv")

# Importar shapefiles das ecorregiões
pantanal <- terra::vect("shps/Pantanal/pantanal_border.shp")
veracruz <- terra::vect("shps/Peten-Veracruz/peten_veracruz.shp")

# Garantir que os shapefiles têm o mesmo sistema de coordenadas
pantanal <- terra::project(pantanal, "EPSG:4326")
veracruz <- terra::project(veracruz, "EPSG:4326")

# Converter dados de ocorrências em objetos espaciais
gbif_vect <- terra::vect(gbif, 
                         geom = c("Longitude", "Latitude"),
                         crs = "EPSG:4326")

data_vect <- terra::vect(data, 
                         geom = c("Longitude", "Latitude"),
                         crs = "EPSG:4326")

# Selecionar presenças que caem dentro do Pantanal
gbif_pantanal_idx <- terra::relate(gbif_vect, pantanal, relation = "intersects")
data_pantanal_idx <- terra::relate(data_vect, pantanal, relation = "intersects")

gbif_pantanal <- gbif[gbif_pantanal_idx, ]
data_pantanal <- data[data_pantanal_idx, ]

# Selecionar presenças que caem dentro de Peten-Veracruz
gbif_veracruz_idx <- terra::relate(gbif_vect, veracruz, relation = "intersects")
data_veracruz_idx <- terra::relate(data_vect, veracruz, relation = "intersects")

gbif_veracruz <- gbif[gbif_veracruz_idx, ]
data_veracruz <- data[data_veracruz_idx, ]

# Verificar quantos registros foram selecionados
cat("GBIF - Total de registros:", nrow(gbif), "\n")
cat("GBIF - Registros no Pantanal:", nrow(gbif_pantanal), "\n")
cat("GBIF - Registros em Peten-Veracruz:", nrow(gbif_veracruz), "\n\n")

cat("Dataset - Total de registros:", nrow(data), "\n")
cat("Dataset - Registros no Pantanal:", nrow(data_pantanal), "\n")
cat("Dataset - Registros em Peten-Veracruz:", nrow(data_veracruz), "\n")

# Exportar os dados filtrados
readr::write_csv(gbif_pantanal, "data/Ecorregions/Pantanal/gbif_pantanal.csv")
readr::write_csv(data_pantanal, "data/Ecorregions/Pantanal/data_pantanal.csv")
readr::write_csv(gbif_veracruz, "data/Ecorregions/Peten-veracruz/gbif_veracruz.csv")
readr::write_csv(data_veracruz, "data/Ecorregions/Peten-veracruz/data_veracruz.csv")
