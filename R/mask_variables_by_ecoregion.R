# Recortar variáveis dentro da ecorregião - 04/11/2025 - Oliveira, 2025

# Carregar pacotes
library(terra)
library(dplyr)

# Carregar vetores das ecorregiões
pantanal <- terra::vect("shps/Pantanal/pantanal_border.shp")
veracruz <- terra::vect("shps/Peten-Veracruz/peten_veracruz.shp")

# Carregar rasters das variáveis ambientais
predictors <- list.files("rasters/", pattern = "\\.tif$", full.names = TRUE)
variables <- terra::rast(predictors)

# Reprojetar os shapefiles para o mesmo CRS dos rasters (se necessário)
pantanal <- terra::project(pantanal, terra::crs(variables))
veracruz <- terra::project(veracruz, terra::crs(variables))

# Recortar e mascarar variáveis para o Pantanal
variables_pantanal <- terra::crop(variables, pantanal)
variables_pantanal <- terra::mask(variables_pantanal, pantanal)

# Recortar e mascarar variáveis para Peten-Veracruz
variables_veracruz <- terra::crop(variables, veracruz)
variables_veracruz <- terra::mask(variables_veracruz, veracruz)

# Criar diretórios para salvar os rasters recortados
dir.create("rasters/pantanal", showWarnings = FALSE, recursive = TRUE)
dir.create("rasters/veracruz", showWarnings = FALSE, recursive = TRUE)

# Salvar os rasters recortados do Pantanal
for(i in 1:nlyr(variables_pantanal)) {
  layer_name <- names(variables_pantanal)[i]
  terra::writeRaster(variables_pantanal[[i]], 
                     filename = paste0("rasters/pantanal/", layer_name, ".tif"),
                     overwrite = TRUE)
}

# Salvar os rasters recortados de Veracruz
for(i in 1:nlyr(variables_veracruz)) {
  layer_name <- names(variables_veracruz)[i]
  terra::writeRaster(variables_veracruz[[i]], 
                     filename = paste0("rasters/veracruz/", layer_name, ".tif"),
                     overwrite = TRUE)
}
