
library(randomForest)
library(caret)



ForetAlea <- function(var_rep, df_app, df_valid, mtry, ntree, maxnodes) {
  
  set.seed(1863)
  col_posi <- which(names(df_app) == var_rep)
  ForeVDT <- randomForest::randomForest(df_app[-col_posi], df_app[[col_posi]], mtry = mtry, ntree = ntree, maxnodes = maxnodes)
  
  # Prediction on the validation dataset
  col_posi <- which(names(df_valid) == var_rep)
  pred.RF <- predict(ForeVDT, newdata = df_valid[, -col_posi])
  
  # Calculate RMSE to evaluate model quality
  rmse <- sqrt(mean((df_valid[, col_posi] - pred.RF)^2))
  
  
  # Calcul du R² ajusté pour train
  R_adj_train <- calcule_R2(df_app[,var_rep],  predict(ForeVDT, newdata=df_app))
  n_train <- nrow(df_app)
  p_train <- ncol(df_app) - 1
  r_adj_train <- 1 - ((1 - R_adj_train) * (n_train - 1) / (n_train - p_train - 1))
  
  # Calcul du R² pour test
  
  res <- rms::lrm(df_valid[,var_rep]  ~ pred.RF, x= TRUE, y = TRUE)
  res = res$stats
  r_adj_test = round (res[["R2"]],2)
  
  # Calculate MAE
  MAE <- mean(abs(pred.RF - df_valid[, col_posi]))
  
  # Round results
  rmse <- round(rmse, 2)
  r_adj_train <- round(r_adj_train, 2)
  r_adj_test <- round(r_adj_test, 2)
  MAE <- round(MAE, 2)
  
  # output
  results_df <- data.frame(Algorithms = "RF",
                           Response_variables = var_rep,
                           R2_adjusted_train = r_adj_train,
                           R2_adjusted_test = r_adj_test,
                           RMSE = rmse,
                           MAE = MAE)
  
  
  results <- list(RMSE = rmse, R_adj_train = r_adj_train, R_adj_test = r_adj_test, MAE = MAE, model = ForeVDT, predit = pred.RF, df = results_df)
  
  return(results)
}



# Grille des hyperparamètres
RF_df_grid <- expand.grid(ntree = c(100,300,500,700,900,1000,1300,1500,1700,2000),
                          mtry = c(1:9),
                          nodesize = c(5, 10 , 20,  30,  50,  70,  100))

# RF_df_grid <- expand.grid(ntree = c(100, 300,500,700,900),
#                           mtry = c(1:2),
#                           nodesize = c(5, 10))

# Fonction de validation croisée pour Random Forest
cross_validation_RF <- function(data, target, folds = 5) {
  
  # Création des folds
  set.seed(123)
  k = folds
  folds <- createFolds(data[[target]], k = k, list = TRUE, returnTrain = TRUE)
  
  results <- data.frame(RMSE = numeric(), R2_adjusted_train = numeric(), R2_adjusted_test = numeric(), MAE = numeric(),
                        ntree = numeric(), mtry = numeric(), nodesize = numeric())
  
  cat("****************************\n")
  cat(paste0("Début de l'hyperparamétrisation du RF avec cross validation de ", k, ":\n"))
  
  for (i in 1:nrow(RF_df_grid)) {
    cat(paste0(i, "/", nrow(RF_df_grid), "\n"))
    params <- RF_df_grid[i, ]
    fold_results <- data.frame(RMSE = numeric(), R2_adjusted_train = numeric(), R2_adjusted_test = numeric(), MAE = numeric())
    
    for (j in 1:length(folds)) {
      train_indices <- folds[[j]]
      test_indices <- setdiff(1:nrow(data), train_indices)
      
      df_app <- data[train_indices, ]
      df_valid <- data[test_indices, ]
      
      # Entraînement du modèle
      result <- ForetAlea(var_rep = target, df_app = df_app, df_valid = df_valid, 
                          mtry = params$mtry, ntree = params$ntree, maxnodes = params$nodesize)
      
      # Stocker les résultats
      fold_results <- rbind(fold_results, data.frame(RMSE = result$RMSE, R2_adjusted_train = result$R_adj_train, 
                                                     R2_adjusted_test = result$R_adj_test, MAE = result$MAE))
    }
    
    # Moyenne des résultats pour les folds
    avg_results <- colMeans(fold_results)
    avg_results <- data.frame(t(avg_results))
    avg_results <- cbind(avg_results, RF_df_grid[i, ])
    
    results <- rbind(results, avg_results)
  }
  
  return(results)
}




# AB_tot ------------------------------------------------------------------
var_rep = "AB_tot"
df_train = read.csv2(paste0("datas/proDij/",var_rep, "_train.csv"))

RF_tune_AB_tot <- cross_validation_RF(data = df_train, target = var_rep)

# meilleurs paramètres
best_result_index <- which.min(RF_tune_AB_tot$RMSE)
best_params <- RF_tune_AB_tot[best_result_index, ]

write.csv2(x =RF_tune_AB_tot,file = paste0("results_tuning/ProDij/", var_rep, "_RF_tuning.csv"), row.names = FALSE)
cat(paste0("******************* Fin tunning RF ",var_rep, " *********************"))


# BM_tot ------------------------------------------------------------------

var_rep = "BM_tot"
df_train = read.csv2(paste0("datas/proDij/",var_rep, "_train.csv"))
RF_tune_BM_tot <- cross_validation_RF(data = df_train, target = var_rep)

# meilleurs paramètres
best_result_index <- which.min(RF_tune_BM_tot$RMSE)
best_params <- RF_tune_BM_tot[best_result_index, ]

write.csv2(x =RF_tune_BM_tot,file = paste0("results_tuning/ProDij/", var_rep, "_RF_tuning.csv"), row.names = FALSE)
cat(paste0("******************* Fin tunning RF ",var_rep, " *********************"))


# Richesse ------------------------------------------------------------------

var_rep = "Richesse"
df_train = read.csv2(paste0("datas/proDij/",var_rep, "_train.csv"))
RF_tune_Richesse <- cross_validation_RF(data = df_train, target = var_rep)

# meilleurs paramètres
best_result_index <- which.min(RF_tune_Richesse$RMSE)
best_params <- RF_tune_Richesse[best_result_index, ]

write.csv2(x =RF_tune_Richesse,file = paste0("results_tuning/ProDij/", var_rep, "_RF_tuning.csv"), row.names = FALSE)
cat(paste0("******************* Fin tunning RF ",var_rep, " *********************"))





