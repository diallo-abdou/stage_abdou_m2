
library(gbm)
library(caret)

GBM <- function(var_rep, df_app, df_valid,distribution = 'gaussian',n.trees ,shrinkage,interaction.depth,n.minobsinnode){
  set.seed(1863)
  
  formula <- substitute(var_rep ~ ., list(var_rep = as.name(var_rep)))
  
  Gradboost<-gbm(formula, data = df_app,
                 distribution = distribution, 
                 n.trees = n.trees,
                 shrinkage = shrinkage,
                 interaction.depth = interaction.depth,
                 n.minobsinnode = n.minobsinnode) 
  
  # Prediction sur le jeu de validation :
  col_posi <- which(names(df_valid) == var_rep)
  prev.GBM<-predict(Gradboost,newdata=as.data.frame(df_valid[,-col_posi]))
  
  # Calcul du RMSE pour évaluer la qualité du modele
  rmse <- sqrt(mean((df_valid[,var_rep] - prev.GBM)^2))
  
  
  # Calcul du R² ajusté pour train
  R_adj_train <- calcule_R2(df_app[,var_rep],  predict(Gradboost, newdata=df_app))
  n_train <- nrow(df_app)
  p_train <- ncol(df_app) - 1
  r_adj_train <- 1 - ((1 - R_adj_train) * (n_train - 1) / (n_train - p_train - 1))
  
  # Calcul du R² ajusté pour test
  res <- rms::lrm(df_valid[,var_rep]  ~ prev.GBM, x= TRUE, y = TRUE)
  res = res$stats
  r_adj_test = round (res[["R2"]],2)
  
  
  # calcule MAE
  MAE <- mean(abs(prev.GBM - df_valid[,col_posi])) 
  
  
  # Round results
  rmse <- round(rmse, 2)
  r_adj_train <- round(r_adj_train, 2)
  r_adj_test <- round(r_adj_test, 2)
  MAE <- round(MAE, 2)
  
  
  # output
  results_df <- data.frame(Algorithms = "GBM",
                           Response_variables = var_rep,
                           R2_adjusted_train = r_adj_train,
                           R2_adjusted_test = r_adj_test,
                           RMSE = rmse,
                           MAE = MAE)
  
  
  results <- list(RMSE = rmse, R_adj_train = r_adj_train, R_adj_test = r_adj_test, MAE = MAE, model = Gradboost, predit = prev.GBM, df = results_df)
  
  
  return(results)
}



# Grille de hyperparametisation
GBM_df_grid <- expand.grid(n.trees = c(500, 1000,1500,1700,2000,3000),
                           shrinkage = c(0.01, 0.02, 0.05, 0.001, 0.002, 0.005),
                           interaction.depth = c(3,  5,  6,  8),
                           n.minobsinnode = c(2 , 5,  10,  30,  50,  70))

# GBM_df_grid <- expand.grid(n.trees = c(1000),
#                            shrinkage = c(0.01, 0.02),
#                            interaction.depth = c( 5),
#                            n.minobsinnode = c(2 ,  10))


cross_validation_GBM <- function(data, target, folds = 5) {
  
  # Création des folds
  set.seed(123)
  k <- folds
  folds <- createFolds(data[[target]], k = k, list = TRUE, returnTrain = TRUE)
  
  results <- data.frame(RMSE = numeric(), R2_adjusted_train = numeric(), R2_adjusted_test = numeric(), MAE = numeric(),
                        n.trees = numeric(), shrinkage = numeric(), interaction.depth = numeric(), n.minobsinnode = numeric())
  
  cat("****************************\n")
  cat(paste0("Début de l'hyperparamétrisation du GBM avec cross validation de ", k, ":\n"))
  
  for (i in 1:nrow(GBM_df_grid)) {
    cat(paste0(i, "/", nrow(GBM_df_grid), "\n"))
    params <- GBM_df_grid[i, ]
    fold_results <- data.frame(RMSE = numeric(), R2_adjusted_train = numeric(), R2_adjusted_test = numeric(), MAE = numeric())
    
    for (j in 1:length(folds)) {
      train_indices <- folds[[j]]
      test_indices <- setdiff(1:nrow(data), train_indices)
      
      df_app <- data[train_indices, ]
      df_valid <- data[test_indices, ]
      
      # Entraînement du modèle
      result <- GBM(var_rep = target, df_app = df_app, df_valid = df_valid, 
                    distribution = 'gaussian', n.trees = params$n.trees, shrinkage = params$shrinkage, 
                    interaction.depth = params$interaction.depth, n.minobsinnode = params$n.minobsinnode)
      

      fold_results <- rbind(fold_results, data.frame(RMSE = result$RMSE, R2_adjusted_train = result$R_adj_train, 
                                                     R2_adjusted_test = result$R_adj_test, MAE = result$MAE))
    }
    
    # Moyenne des résultats pour les folds
    avg_results <- colMeans(fold_results)
    avg_results <- data.frame(t(avg_results))
    avg_results <- cbind(avg_results, GBM_df_grid[i, ])
    
    results <- rbind(results, avg_results)
  }
  
  return(results)
}






# AB_tot ------------------------------------------------------------------
var_rep = "AB_tot"
df_train = read.csv2(paste0("datas/proDij/",var_rep, "_train.csv"))
GBM_tune_AB_tot <- cross_validation_GBM(data = df_train, target = var_rep)

#les meilleurs paramètres
best_result_index_gbm <- which.min(GBM_tune_AB_tot$RMSE)
best_params_gbm <- GBM_tune_AB_tot[best_result_index_gbm, ]

write.csv2(x =GBM_tune_AB_tot,file = paste0("results_tuning/ProDij/", var_rep, "_GBM_tuning.csv"), row.names = FALSE)
cat(paste0("******************* Fin tunning GBM ",var_rep, " *********************"))

# BM_tot ------------------------------------------------------------------
var_rep = "BM_tot"
df_train = read.csv2(paste0("datas/proDij/",var_rep, "_train.csv"))
GBM_tune_BM_tot <- cross_validation_GBM(data = df_train, target = var_rep)

#les meilleurs paramètres
best_result_index_gbm <- which.min(GBM_tune_BM_tot$RMSE)
best_params_gbm <- GBM_tune_BM_tot[best_result_index_gbm, ]

write.csv2(x =GBM_tune_BM_tot,file = paste0("results_tuning/ProDij/", var_rep, "_GBM_tuning.csv"), row.names = FALSE)
cat(paste0("******************* Fin tunning GBM ",var_rep, " *********************"))

# Richesse_tot ------------------------------------------------------------------
var_rep = "Richesse"
df_train = read.csv2(paste0("datas/proDij/",var_rep, "_train.csv"))
GBM_tune_Richesse <- cross_validation_GBM(data = df_train, target = var_rep)

#les meilleurs paramètres
best_result_index_gbm <- which.min(GBM_tune_Richesse$RMSE)
best_params_gbm <- GBM_tune_Richesse[best_result_index_gbm, ]

write.csv2(x =GBM_tune_Richesse,file = paste0("results_tuning/ProDij/", var_rep, "_GBM_tuning.csv"), row.names = FALSE)
cat(paste0("******************* Fin tunning GBM ",var_rep, " *********************"))





