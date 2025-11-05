# Filtragem espacial das presenças - 04/11/2025 - Oliveira, 2025

# Carregar pacotes
library(GeoThinneR)
library(terra)
library(ggplot2)
library(readr)

# Importar presenças
gbif_pan <- readr::read_csv("data/ecoregions/Pantanal/gbif_pantanal.csv")
data_pan <- readr::read_csv("data/ecoregions/Pantanal/data_pantanal.csv")
gbif_vera <- readr::read_csv("data/ecoregions/Peten-veracruz/gbif_veracruz.csv")
data_vera <- readr::read_csv("data/ecoregions/Peten-veracruz/data_veracruz.csv")

# Definir raios base das áreas de vida (home range)
hr_pantanal <- 5.2
hr_veracruz <- 12.2

# Definir multiplicadores para thinning
multiplicadores <- c(0.5, 0.75, 1, 1.25, 1.5)

# Aplicar thinning para GBIF Pantanal
gbif_pan_thinned <- lapply(multiplicadores, function(mult) {
  thin_dist <- hr_pantanal * mult
  GeoThinneR::thin_points(
    data = gbif_pan,
    lon_col = "Longitude",
    lat_col = "Latitude",
    method = "distance",
    thin_dist = thin_dist,
    search_type = "kd_tree",
    trials = 10,
    all_trials = FALSE,
    seed = 18,
    verbose = TRUE
  )
})
names(gbif_pan_thinned) <- paste0("mult_", multiplicadores)

# Aplicar thinning para Data Pantanal
data_pan_thinned <- lapply(multiplicadores, function(mult) {
  thin_dist <- hr_pantanal * mult
  GeoThinneR::thin_points(
    data = data_pan,
    lon_col = "Longitude",
    lat_col = "Latitude",
    method = "distance",
    thin_dist = thin_dist,
    search_type = "kd_tree",
    trials = 10,
    all_trials = FALSE,
    seed = 18,
    verbose = TRUE
  )
})
names(data_pan_thinned) <- paste0("mult_", multiplicadores)

# Aplicar thinning para GBIF Veracruz
gbif_vera_thinned <- lapply(multiplicadores, function(mult) {
  thin_dist <- hr_veracruz * mult
  GeoThinneR::thin_points(
    data = gbif_vera,
    lon_col = "Longitude",
    lat_col = "Latitude",
    method = "distance",
    thin_dist = thin_dist,
    search_type = "kd_tree",
    trials = 10,
    all_trials = FALSE,
    seed = 18,
    verbose = TRUE
  )
})
names(gbif_vera_thinned) <- paste0("mult_", multiplicadores)

# Aplicar thinning para Data Veracruz
data_vera_thinned <- lapply(multiplicadores, function(mult) {
  thin_dist <- hr_veracruz * mult
  GeoThinneR::thin_points(
    data = data_vera,
    lon_col = "Longitude",
    lat_col = "Latitude",
    method = "distance",
    thin_dist = thin_dist,
    search_type = "kd_tree",
    trials = 10,
    all_trials = FALSE,
    seed = 18,
    verbose = TRUE
  )
})
names(data_vera_thinned) <- paste0("mult_", multiplicadores)

# Função auxiliar para extrair registros mantidos
extrair_mantidos <- function(dados_originais, resultado_thinned) {
  indices_mantidos <- resultado_thinned$retained[[1]]
  dados_originais[indices_mantidos, ]
}

# Extrair dados filtrados para Pantanal GBIF
gbif_pan_filtrados <- lapply(seq_along(gbif_pan_thinned), function(i) {
  extrair_mantidos(gbif_pan, gbif_pan_thinned[[i]])
})
names(gbif_pan_filtrados) <- names(gbif_pan_thinned)

# Extrair dados filtrados para Pantanal Data
data_pan_filtrados <- lapply(seq_along(data_pan_thinned), function(i) {
  extrair_mantidos(data_pan, data_pan_thinned[[i]])
})
names(data_pan_filtrados) <- names(data_pan_thinned)

# Extrair dados filtrados para Veracruz GBIF
gbif_vera_filtrados <- lapply(seq_along(gbif_vera_thinned), function(i) {
  extrair_mantidos(gbif_vera, gbif_vera_thinned[[i]])
})
names(gbif_vera_filtrados) <- names(gbif_vera_thinned)

# Extrair dados filtrados para Veracruz Data
data_vera_filtrados <- lapply(seq_along(data_vera_thinned), function(i) {
  extrair_mantidos(data_vera, data_vera_thinned[[i]])
})
names(data_vera_filtrados) <- names(data_vera_thinned)

# Criar diretórios se não existirem
dir.create("data/ecoregions/Pantanal/thinned", showWarnings = FALSE, recursive = TRUE)
dir.create("data/ecoregions/Peten-veracruz/thinned", showWarnings = FALSE, recursive = TRUE)

# Exportar GBIF Pantanal
for(i in seq_along(gbif_pan_filtrados)) {
  nome_arquivo <- paste0("data/ecoregions/Pantanal/thinned/gbif_pantanal_", names(gbif_pan_filtrados)[i], ".csv")
  readr::write_csv(gbif_pan_filtrados[[i]], nome_arquivo)
}

# Exportar Data Pantanal
for(i in seq_along(data_pan_filtrados)) {
  nome_arquivo <- paste0("data/ecoregions/Pantanal/thinned/data_pantanal_", names(data_pan_filtrados)[i], ".csv")
  readr::write_csv(data_pan_filtrados[[i]], nome_arquivo)
}

# Exportar GBIF Veracruz
for(i in seq_along(gbif_vera_filtrados)) {
  nome_arquivo <- paste0("data/ecoregions/Peten-veracruz/thinned/gbif_veracruz_", names(gbif_vera_filtrados)[i], ".csv")
  readr::write_csv(gbif_vera_filtrados[[i]], nome_arquivo)
}

# Exportar Data Veracruz
for(i in seq_along(data_vera_filtrados)) {
  nome_arquivo <- paste0("data/ecoregions/Peten-veracruz/thinned/data_veracruz_", names(data_vera_filtrados)[i], ".csv")
  readr::write_csv(data_vera_filtrados[[i]], nome_arquivo)
}
