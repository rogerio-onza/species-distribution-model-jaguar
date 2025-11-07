# Script automatizado para treino e avaliação de modelos de distribuição de espécies
# Panthera onca - Ecorregiões - 07/11 - Oliveira, 2025

# Carregar pacotes necessários
library(terra)
library(readr)
library(SDMtune)
library(ENMeval)
library(enmSdmX)

# Definir parâmetros globais
SPECIES_NAME <- "Panthera onca"
ECOREGION <- "Pantanal"
BASE_PATH <- "data/ecoregions/Pantanal"
RASTER_PATH <- "rasters/pantanal/"
OUTPUT_DIR <- "results/pantanal"

# Criar diretório de saída
if (!dir.exists(OUTPUT_DIR)) dir.create(OUTPUT_DIR, recursive = TRUE)

# Função auxiliar para renomear camadas do raster
rename_layers <- function(r, strip_suffix = TRUE) {
  nm <- names(r)
  if (strip_suffix) nm <- sub("_1981.*$", "", nm)
  nm <- gsub("[-.]", "_", nm)
  nm <- gsub("_+", "_", nm)
  nm <- sub("^_", "", nm)
  nm <- sub("_$", "", nm)
  nm <- make.names(nm, unique = TRUE)
  names(r) <- nm
  r
}

# Função para calcular métricas de avaliação
calculate_metrics <- function(model, test_swd, threshold_val, distance, region) {
  
  # Predições no conjunto de teste
  preds_test <- SDMtune::predict(model, data = test_swd, type = "cloglog")
  pres_vals <- as.numeric(preds_test[test_swd@pa == 1])
  bg_vals <- as.numeric(preds_test[test_swd@pa == 0])
  
  # Calcular AUC treino e teste
  auc_train <- SDMtune::auc(model)
  auc_test <- SDMtune::auc(model, test = test_swd)
  auc_diff <- auc_train - auc_test
  
  # Calcular TSS treino e teste
  tss_train <- SDMtune::tss(model)
  tss_test <- SDMtune::tss(model, test = test_swd)
  tss_diff <- tss_train - tss_test
  
  # Calcular CBI
  cbi_value <- tryCatch(
    enmSdmX::evalContBoyce(pres_vals, bg_vals, na.rm = TRUE),
    error = function(e) NA_real_
  )
  
  # Obter matriz de confusão
  cm <- tryCatch(
    SDMtune::confMatrix(model, test = test_swd, th = threshold_val, type = "cloglog"),
    error = function(e) NULL
  )
  
  # Extrair valores da matriz de confusão
  TP <- if (!is.null(cm)) cm$tp else NA_real_
  TN <- if (!is.null(cm)) cm$tn else NA_real_
  FP <- if (!is.null(cm)) cm$fp else NA_real_
  FN <- if (!is.null(cm)) cm$fn else NA_real_
  
  # Calcular métricas derivadas
  commission_rate <- if (!is.null(cm) && (FP + TN) > 0) FP/(FP + TN) else NA_real_
  omission_rate <- if (!is.null(cm) && (TP + FN) > 0) FN/(TP + FN) else NA_real_
  opr <- if (!is.null(cm) && (TP + FP) > 0) FP/(TP + FP) else NA_real_
  upr <- if (!is.null(cm) && (FN + TN) > 0) FN/(FN + TN) else NA_real_
  
  # Criar data frame com resultados
  data.frame(
    Region = region,
    Distance = distance,
    AUC_train = round(auc_train, 3),
    AUC_test = round(auc_test, 3),
    AUC_diff = round(auc_diff, 3),
    TSS_train = round(tss_train, 3),
    TSS_test = round(tss_test, 3),
    TSS_diff = round(tss_diff, 3),
    CBI_test = round(cbi_value, 3),
    Commission = round(commission_rate, 3),
    Omission = round(omission_rate, 3),
    OPR = round(opr, 3),
    UPR = round(upr, 3),
    ThresholdValue = round(threshold_val, 5),
    stringsAsFactors = FALSE
  )
}

# Carregar variáveis ambientais
variables <- list.files(RASTER_PATH, pattern = "\\.tif$", full.names = TRUE)
variables <- terra::rast(variables)
variables <- rename_layers(variables)

# Carregar background
gbif_bg <- readr::read_csv(file.path(BASE_PATH, "gbif_pantanal_bg.csv"))
data_bg <- readr::read_csv(file.path(BASE_PATH, "data_pantanal_bg.csv"))

# Carregar dados de teste
data <- readr::read_csv(file.path(BASE_PATH, "data_pantanal.csv"))
data_swd <- prepareSWD(species = SPECIES_NAME, p = data, a = data_bg, env = variables)

# Definir distâncias de thinning
distances <- c("raw", "0.5", "0.75", "1", "1.25", "1.5")

# Lista para armazenar resultados
all_results <- list()

# Loop para processar cada distância
for (dist in distances) {
  
  # Carregar dados de treino
  if (dist == "raw") {
    gbif_file <- file.path(BASE_PATH, "gbif_pantanal.csv")
  } else {
    gbif_file <- file.path(BASE_PATH, "thinned", 
                           sprintf("gbif_pantanal_mult_%s.csv", dist))
  }
  
  gbif_data <- readr::read_csv(gbif_file)
  
  # Criar objeto SWD de treino
  gbif_swd <- prepareSWD(species = SPECIES_NAME, p = gbif_data, 
                         a = gbif_bg, env = variables)
  
  # Criar partições espaciais com checkerboard
  folds <- ENMeval::get.checkerboard(
    occs = gbif_swd@coords[gbif_swd@pa == 1, ], 
    bg = gbif_swd@coords[gbif_swd@pa == 0, ],
    aggregation.factor = 4, 
    envs = variables
  )
  
  # Treinar modelo
  model_cv <- SDMtune::train(
    method = "Maxent", 
    data = gbif_swd, 
    fc = "lqph", 
    reg = 1, 
    iter = 500, 
    folds = folds
  )
  
  # Combinar resultados de cross-validation
  model <- SDMtune::combineCV(model_cv)
  
  # Calcular threshold
  thr_tbl <- SDMtune::thresholds(model, type = "cloglog", test = data_swd)
  threshold_val <- thr_tbl[
    thr_tbl$Threshold == "Maximum test sensitivity plus specificity", 
    "Cloglog value"
  ]
  threshold_val <- as.numeric(threshold_val[1])
  
  # Calcular métricas
  results <- calculate_metrics(
    model = model,
    test_swd = data_swd,
    threshold_val = threshold_val,
    distance = dist,
    region = ECOREGION
  )
  
  # Adicionar à lista de resultados
  all_results[[length(all_results) + 1]] <- results
  
  # Limpar memória
  rm(gbif_data, gbif_swd, folds, model_cv, model)
  gc()
}

# Combinar todos os resultados
final_results <- do.call(rbind, all_results)

# Salvar resultados
output_file <- file.path(OUTPUT_DIR, paste0(ECOREGION, "_metrics_results.csv"))
readr::write_csv(final_results, output_file)

# Exibir resultados
print(final_results)
