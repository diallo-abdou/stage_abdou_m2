## FUNCTION
GLM_2 <- function(var_rep, df_app, df_valid){
  
  var_predicteurs = names(df_app[,-1])
  
  df_app = df_app[,c(var_rep,var_predicteurs)]
  df_valid = df_valid[,c(var_rep,var_predicteurs)]
  
  formula <- substitute(var_rep ~ ., list(var_rep = as.name(var_rep)))
  
  
  # entrainement du modele sur le jeu d'entrainement
  meta_mode_AB_tot<-glm(formula,data = df_app)
  
  # Prediction sur le jeu de validation
  final_preds_AB_tot<-predict(meta_mode_AB_tot,newdata=as.data.frame(df_valid[,var_predicteurs]))
  
  # Calcul du RMSE pour évaluer la qualite du modele
  rmse <- round (sqrt(mean((df_valid[,var_rep] - final_preds_AB_tot)^2,na.rm=TRUE)),2)
  
  
  # Calcul du R² ajusté pour train
  R_adj_train <- calcule_R2(df_app[,var_rep],  predict(meta_mode_AB_tot, newdata=df_app))
  n_train <- nrow(df_app)
  p_train <- ncol(df_app) - 1
  r_adj_train <- 1 - ((1 - R_adj_train) * (n_train - 1) / (n_train - p_train - 1))
  
  # Calcul du R² 
  # R_adj_test <-calcule_R2(df_valid[,var_rep],final_preds_AB_tot)
  # n_test <- nrow(df_valid)
  # p_test <- ncol(df_valid) - 1
  # r_adj_test <- 1 - ((1 - R_adj_test) * (n_test - 1) / (n_test - p_test - 1))
  
  res <- rms::lrm(df_valid[,var_rep]  ~ final_preds_AB_tot, x= TRUE, y = TRUE)
  res = res$stats
  r_adj_test = round (res[["R2"]],2)
  
  MAE <- mean(abs(final_preds_AB_tot - df_valid[,var_rep]),na.rm=TRUE)
  
  # Round results
  rmse <- round(rmse, 2)
  r_adj_train <- round(r_adj_train, 2)
  r_adj_test <- round(r_adj_test, 2)
  MAE <- round(MAE, 2)
  
  # output
  results_df <- data.frame(Algorithms = "GLM",
                           Response_variables = var_rep,
                           R2_adjusted_train = r_adj_train,
                           R2_adjusted_test = r_adj_test,
                           RMSE = rmse,
                           MAE = MAE)
  
  
  results <- list(RMSE = rmse, R_adj_train = r_adj_train, R_adj_test = r_adj_test, MAE = MAE, model = meta_mode_AB_tot,predit = final_preds_AB_tot, df = results_df)
  return(results)
}


GAM_2 <- function(var_rep, df_app, df_valid){
  
  var_predicteurs = names(df_app[,-1])
  
  
  if (var_rep == "AB_tot"){ 
    # for (i in best_vars_AB_tot_GAM){ cat(paste0 ("s(",i,")", sep = " + "))}
    modelgam<-gam(AB_tot ~ s(tas_sd_6) + s(tas_sd_3) + s(Sand) + s(tas_sd_120) + s(K) + s(pet_sd_6) + 
                    s(pH) + s(CN) + s(P) + s(tas_mean_6) + s(pr_mean_3) + s(tas_mean_120) + LC_,
                  data = df_app)
    
  }
  
  
  
  
  if (var_rep == "BM_tot"){ 
    # for (i in best_vars_BM_tot_GAM){ cat(paste0 ("s(",i,")", sep = " + "))}
    modelgam<-gam(BM_tot ~ s(P) + s(CN) + s(K) + s(C_org) + s(Sand) + s(tas_sd_3) + s(pet_sd_6) + s(pr_sd_3) + 
                    s(tas_sd_6) + s(tas_mean_120) + s(pr_mean_3) + s(pH) + s(pr_mean_120) + LC_ + s(tas_mean_6),
                  data = df_app)
    
    
  }
  
  
  
  if(var_rep == "Richesse_tot"){ 
    # for (i in best_vars_Richesse_tot_GAM){ cat(paste0 ("s(",i,")", sep = " + "))}
    modelgam<-gam(Richesse_tot ~ s(Sand) + s(K) + s(pr_sd_3) + s(tas_sd_3) + s(CN) + s(tas_sd_120) + s(P) + 
                    s(tas_sd_6) + s(tas_mean_120) + s(pet_sd_6) + s(pet_mean_6) + s(N) + s(tas_mean_6) + 
                    s(pr_mean_120) + s(pH) + LC_ ,data = df_app)
    
  }
  
  
  # Prediction sur le jeu de validation
  pred.GAM <- predict(modelgam,newdata=as.data.frame(df_valid[,var_predicteurs]))
  
  # Calcul du RMSE pour évaluer la qualite du modele
  rmse <- sqrt(mean((df_valid[,var_rep] - pred.GAM)^2,na.rm=TRUE))
  
  
  # Calcul du R² ajusté pour train
  R_adj_train <- calcule_R2(df_app[,var_rep],  predict(modelgam, newdata=df_app))
  n_train <- nrow(df_app)
  p_train <- ncol(df_app) - 1
  r_adj_train <- 1 - ((1 - R_adj_train) * (n_train - 1) / (n_train - p_train - 1))
  
  # Calcul du R² ajusté pour test
  # R_adj_test <-calcule_R2(df_valid[,var_rep],pred.GAM)
  # n_test <- nrow(df_valid)
  # p_test <- ncol(df_valid) - 1
  # r_adj_test <- 1 - ((1 - R_adj_test) * (n_test - 1) / (n_test - p_test - 1))
  res <- rms::lrm(df_valid[,var_rep]  ~ pred.GAM, x= TRUE, y = TRUE)
  res = res$stats
  r_adj_test = round (res[["R2"]],2)
  
  
  # Calcule le MAE
  MAE <- mean(abs(pred.GAM - df_valid[,var_rep]))
  
  # Round results
  rmse <- round(rmse, 2)
  r_adj_train <- round(r_adj_train, 2)
  r_adj_test <- round(r_adj_test, 2)
  MAE <- round(MAE, 2)
  
  
  # output
  results_df <- data.frame(Algorithms = "GAM",
                           Response_variables = var_rep,
                           R2_adjusted_train = r_adj_train,
                           R2_adjusted_test = r_adj_test,
                           RMSE = rmse,
                           MAE = MAE)
  
  
  results <- list(RMSE = rmse, R_adj_train = r_adj_train, R_adj_test = r_adj_test, MAE = MAE, model = modelgam, predit = pred.GAM, df = results_df)
  
  return(results)
}


ForetAlea_2 <- function(var_rep, df_app, df_valid) {
  
  set.seed(1863)
  col_posi <- which(names(df_app) == var_rep)
  ForeVDT <- randomForest::randomForest(df_app[-col_posi], df_app[[col_posi]])
  
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
  
  # Calcul du R² ajusté pour test
  # R_adj_test <-calcule_R2(df_valid[,col_posi],pred.RF)
  # n_test <- nrow(df_valid)
  # p_test <- ncol(df_valid) - 1
  # r_adj_test <- 1 - ((1 - R_adj_test) * (n_test - 1) / (n_test - p_test - 1))
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


GBM_2 <- function(var_rep, df_app, df_valid){
  
  formula <- substitute(var_rep ~ ., list(var_rep = as.name(var_rep)))
  
  Gradboost<-gbm(formula, data = df_app) 
  
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
  # R_adj_test <-calcule_R2(df_valid[,col_posi],prev.GBM)
  # n_test <- nrow(df_valid)
  # p_test <- ncol(df_valid) - 1
  # r_adj_test <- 1 - ((1 - R_adj_test) * (n_test - 1) / (n_test - p_test - 1))
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





#1Préparer les données

predictor_non_r = c("LC_","Sand", "Clay", "pH", "N", "P", "K", "CN", "C_org", "tas_mean_6", "tas_mean_120", 
                    "tas_sd_3", "tas_sd_6", "tas_sd_120", "pr_mean_3", 
                    "pr_mean_120", "pr_sd_3", "pet_mean_6", "pet_sd_6")


LC_levels = c("LC_For", "LC_Gua", "LC_Nag", "LC_Nial", "LC_Pmo", "LC_Viny")

predictor_deep = c("LC_", "Sand", "Silt", "Clay", "pH", "N", "P", 
                   "K", "CN", "CaCO3", "C_org", "tas_mean_3", "tas_mean_6", "tas_mean_120", 
                   "tas_sd_3", "tas_sd_6", "tas_sd_120", "tasmax_mean_3", "tasmax_mean_6", 
                   "tasmax_mean_120", "tasmax_sd_3", "tasmax_sd_6", "tasmax_sd_120", 
                   "tasmin_mean_3", "tasmin_mean_6", "tasmin_mean_120", "tasmin_sd_3", 
                   "tasmin_sd_6", "tasmin_sd_120", "pr_mean_3", "pr_mean_6", "pr_mean_120", 
                   "pr_sd_3", "pr_sd_6", "pr_sd_120", "pet_mean_3", "pet_mean_6", 
                   "pet_mean_120", "pet_sd_3", "pet_sd_6", "pet_sd_120")


vdt_col=c("AB_tot", "BM_tot", "Richesse_tot")

AB_tot_train = read.csv2("datas/landW/AB_tot_train.csv")
AB_tot_train$LC_ = as.factor(AB_tot_train$LC_)
AB_tot_test = read.csv2("datas/landW/AB_tot_test.csv")
AB_tot_test$LC_ = as.factor(AB_tot_test$LC_)

BM_tot_train = read.csv2("datas/landW/BM_tot_train.csv")
BM_tot_train$LC_ = as.factor(BM_tot_train$LC_)
BM_tot_test = read.csv2("datas/landW/BM_tot_test.csv")
BM_tot_test$LC_ = as.factor(BM_tot_test$LC_)

Richesse_tot_train = read.csv2("datas/landW/Richesse_tot_train.csv")
Richesse_tot_train$LC_ = as.factor(Richesse_tot_train$LC_)
Richesse_tot_test = read.csv2("datas/landW/Richesse_tot_test.csv")
Richesse_tot_test$LC_ = as.factor(Richesse_tot_test$LC_)






# Abundance ---------------------------------------------------------------

var_rep = "AB_tot" 

#2 Ajuster les modèles

   # GLM
   glm_mode_AB_tot <- GLM_2(var_rep = var_rep, 
                            df_app =AB_tot_train[,c(var_rep,best_vars_AB_tot_GLM)], 
                            df_valid =AB_tot_test [,c(var_rep,best_vars_AB_tot_GLM)])
   glm_mode_AB_tot$df

   # GAM
   gam_mode_AB_tot <- GAM_2(var_rep = var_rep, 
                            df_app =AB_tot_train[,c(var_rep,best_vars_AB_tot_GAM)], 
                            df_valid =AB_tot_test [,c(var_rep,best_vars_AB_tot_GAM)])
   gam_mode_AB_tot$df
   
   # Random Forest
   rf_mode_AB_tot <- ForetAlea_2(var_rep = var_rep, 
                                 df_app =AB_tot_train[,c(var_rep,best_vars_AB_tot_RF)], 
                                 df_valid =AB_tot_test [,c(var_rep,best_vars_AB_tot_RF)])
   rf_mode_AB_tot$df
   # GBM
   gbm_mode_AB_tot <- GBM_2(var_rep = var_rep, 
                             df_app =AB_tot_train[,c(var_rep,best_vars_AB_tot_GBM)], 
                             df_valid =AB_tot_test [,c(var_rep,best_vars_AB_tot_GBM)])
   gbm_mode_AB_tot$df

   
   # ANN
   
   df_train = AB_tot_train[, c(var_rep, best_vars_AB_tot_ANN, LC_levels)]
   df_test = AB_tot_test[, c(var_rep, best_vars_AB_tot_ANN,LC_levels)]
   df_train$LC_ = NULL
   df_test$LC_ =NULL
  
     df_train = drop_na(df_train)
     df_train = droplevels(df_train)
     
     # les feature  a fusionnn?e en faisant la somme
     LC_levels
   
   
   variables = names(df_train)[!names(df_train) %in% var_rep]
   X_train <- df_train[, variables]
   y_train <- df_train[, var_rep]
   
   X_test <- df_test[, variables]
   y_test <- df_test[, var_rep]
   
   
   X_train <- as.data.frame(lapply(X_train, as.numeric))
   X_test <- as.data.frame(lapply(X_test, as.numeric))

     
     # Convert data to matrix and scale
     X_train_mat <- as.matrix(scale(X_train))
     X_test_mat <- as.matrix(scale(X_test))
     
     # Define the model
     model <- keras_model_sequential() %>%
       layer_dense(units = 32, activation = 'relu', input_shape = c(ncol(X_train_mat))) %>%
       layer_dropout(rate = 0.2)  %>%
       layer_dense(units = 32, activation = 'relu') %>%
       layer_dropout(rate = 0.2)  %>%
       layer_dense(units = 16, activation = 'relu') %>%
       layer_dropout(rate = 0.2)  %>%
       layer_dense(units = 16, activation = 'relu') %>%
       layer_dropout(rate = 0.2)  %>%
       layer_dense(units = 1)
     
     model %>% keras::compile(loss = 'mse',
                              optimizer = 'rmsprop',
                              metrics = 'mae')
     
     # Fit the model
     model %>%
       fit(X_train_mat,
           y_train,
           epochs = 100,
           #batch_size = 1,
           validation_split = 0.2)
     
     # Prediction on test data
     pred. <- model %>% predict(X_test_mat)
     ANN_pred_AB_tot = pred.
     
     # Calculate RMSE
     rmse <- round(sqrt(mean((y_test - pred.)^2, na.rm = TRUE)), 2)
     
     # Calculate adjusted R? for training data
     pred.train =  model %>% predict(X_train_mat)
     R_adj_train <- calcule_R2(y_train,pred.train)
     n_train <- nrow(df_train)
     p_train <- ncol(df_train) - 6
     r_adj_train <- 1 - ((1 - R_adj_train) * (n_train - 1) / (n_train - p_train - 1))
     
     # Calculate R? for test data
     res <- lrm(y_test ~ pred., x = TRUE, y = TRUE)
     res = res$stats
     R2_test = round(res[["R2"]], 2)
     
     MAE <- mean(abs(pred. - y_test), na.rm = TRUE)
     
     # Round results
     rmse <- round(rmse, 2)
     r_adj_train <- round(r_adj_train, 2)
     R2_test <- round(R2_test, 2)
     MAE <- round(MAE, 2)
     
     # Output
     results_df <- data.frame(
       Response_variables = var_rep,
       R2_adjusted_train = r_adj_train,
       R2_test = R2_test,
       RMSE = rmse,
       MAE = MAE
     )
     
     
   
#3 Faire des prédictions sur l'ensemble de test
  
glm_preds_AB_tot <- as.numeric(glm_mode_AB_tot$predit)
gam_preds_AB_tot <- as.numeric(gam_mode_AB_tot$predit)
rf_preds_AB_tot <- as.numeric(rf_mode_AB_tot$predit)
gbm_preds_AB_tot <- as.numeric(gbm_mode_AB_tot$predit)
ann_preds_AB_tot <- as.numeric(ANN_pred_AB_tot)


#4 modèle d'ensemble


   #4.1 Moyenne des prédictions
    ensemble_preds_AB_tot <- (glm_preds_AB_tot + 
                                gam_preds_AB_tot + 
                                rf_preds_AB_tot + 
                                gbm_preds_AB_tot + 
                                ann_preds_AB_tot) / 5
     

   #4.2 Stacking (méta-apprentissage) 
     
     # prédictions de chaque modèle
     ensemble_datas <- data.frame(glm = glm_preds_AB_tot, 
                                  gam = gam_preds_AB_tot, 
                                  rf = rf_preds_AB_tot, 
                                  gbm = gbm_preds_AB_tot, 
                                  ann = ann_preds_AB_tot, 
                                  actual = AB_tot_test[[var_rep]])
     
     # Ajustement d'un LM
     cor_df <- round(cor(ensemble_datas), 2)
     meta_mode_AB_tot <- lm(actual ~ ., data = ensemble_datas)
    summary(meta_mode_AB_tot)
     # Faire des prédictions finales
     final_preds_AB_tot <- predict(meta_mode_AB_tot, newdata = ensemble_datas)
     
    

#5 Performances du modèle d'ensemble :

### 5.1 Moyenne des prédictions   ***
# postResample(pred = ensemble_preds_AB_tot, obs = AB_tot_test[[var_rep]])

# Calcul du RMSE pour évaluer la qualite du modele
rmse <- round (sqrt(mean((AB_tot_test[[var_rep]] - ensemble_preds_AB_tot)^2,na.rm=TRUE)),2)

# Calcul du R² ajusté pour train
R_adj_train <- calcule_R2(AB_tot_test[[var_rep]],  ensemble_preds_AB_tot)
n_train <- nrow(ensemble_datas)
p_train <- ncol(ensemble_datas) - 1
r_adj_train <- 1 - ((1 - R_adj_train) * (n_train - 1) / (n_train - p_train - 1))

# Calcul du R² 
res <- rms::lrm(AB_tot_test[[var_rep]]  ~ ensemble_preds_AB_tot, x= TRUE, y = TRUE)
res = res$stats
R2_test = round (res[["R2"]],2)
MAE <- mean(abs(ensemble_preds_AB_tot - AB_tot_test[[var_rep]]),na.rm=TRUE)

# Round results
rmse <- round(rmse, 2)
r_adj_train <- round(r_adj_train, 2)
R2_test <- round(R2_test, 2)
MAE <- round(MAE, 2)

# output
Averaging_AB_tot <- data.frame(
  Models = "Averaging",
  Rep.var = "Abundance",
  R2_adj_train  = r_adj_train,
  R2_test = R2_test,
  RMSE = rmse,
  MAE = MAE
)
Averaging_AB_tot
write.xlsx(x = Averaging_AB_tot,file = paste0("Results/", var_rep, "_averaging.xlsx"),
           rowNames = FALSE, overwrite = TRUE)




### 5.2 Stacking (méta-apprentissage) ***
# postResample(pred = final_preds_AB_tot, obs = AB_tot_test[[var_rep]])

# Calcul du RMSE pour évaluer la qualite du modele
rmse <- round (sqrt(mean((AB_tot_test[[var_rep]] - final_preds_AB_tot)^2,na.rm=TRUE)),2)

# Calcul du R² ajusté pour train
R_adj_train <- calcule_R2(AB_tot_test[[var_rep]],  final_preds_AB_tot)
n_train <- nrow(ensemble_datas)
p_train <- ncol(ensemble_datas) - 1
r_adj_train <- 1 - ((1 - R_adj_train) * (n_train - 1) / (n_train - p_train - 1))

# Calcul du R² 
res <- rms::lrm(AB_tot_test[[var_rep]]  ~ final_preds_AB_tot, x= TRUE, y = TRUE)
res = res$stats
R2_test = round (res[["R2"]],2)
MAE <- mean(abs(final_preds_AB_tot - AB_tot_test[[var_rep]]),na.rm=TRUE)

# Round results
rmse <- round(rmse, 2)
r_adj_train <- round(r_adj_train, 2)
R2_test <- round(R2_test, 2)
MAE <- round(MAE, 2)

# output
Stking_AB_tot <- data.frame(
  Models = "Stacking",
  Rep.var = "Abundance",
  R2_adj_train  = r_adj_train,
  R2_test = R2_test,
  RMSE = rmse,
  MAE = MAE
)
Stking_AB_tot
write.xlsx(x = Stking_AB_tot,file = paste0("Results/", var_rep, "_stacking.xlsx"),
           rowNames = FALSE, overwrite = TRUE)







# Biomass ---------------------------------------------------------------

var_rep = "BM_tot" 

#2 Ajuster les modèles

# GLM
glm_mode_BM_tot <- GLM_2(var_rep = var_rep, 
                         df_app =BM_tot_train[,c(var_rep,best_vars_BM_tot_GLM)], 
                         df_valid =BM_tot_test [,c(var_rep,best_vars_BM_tot_GLM)])
glm_mode_BM_tot$df

# GAM
gam_mode_BM_tot <- GAM_2(var_rep = var_rep, 
                         df_app =BM_tot_train[,c(var_rep,best_vars_BM_tot_GAM)], 
                         df_valid =BM_tot_test [,c(var_rep,best_vars_BM_tot_GAM)])
gam_mode_BM_tot$df

# Random Forest
rf_mode_BM_tot <- ForetAlea_2(var_rep = var_rep, 
                              df_app =BM_tot_train[,c(var_rep,best_vars_BM_tot_RF)], 
                              df_valid =BM_tot_test [,c(var_rep,best_vars_BM_tot_RF)])
rf_mode_BM_tot$df
# GBM
gbm_mode_BM_tot <- GBM_2(var_rep = var_rep, 
                         df_app =BM_tot_train[,c(var_rep,best_vars_BM_tot_GBM)], 
                         df_valid =BM_tot_test [,c(var_rep,best_vars_BM_tot_GBM)])
gbm_mode_BM_tot$df


# ANN

df_train = BM_tot_train[, c(var_rep, best_vars_BM_tot_ANN, LC_levels)]
df_test = BM_tot_test[, c(var_rep, best_vars_BM_tot_ANN,LC_levels)]
df_train$LC_ = NULL
df_test$LC_ =NULL

df_train = drop_na(df_train)
df_train = droplevels(df_train)

# les feature  a fusionnn?e en faisant la somme
LC_levels


variables = names(df_train)[!names(df_train) %in% var_rep]
X_train <- df_train[, variables]
y_train <- df_train[, var_rep]

X_test <- df_test[, variables]
y_test <- df_test[, var_rep]


X_train <- as.data.frame(lapply(X_train, as.numeric))
X_test <- as.data.frame(lapply(X_test, as.numeric))


# Convert data to matrix and scale
X_train_mat <- as.matrix(scale(X_train))
X_test_mat <- as.matrix(scale(X_test))

# Define the model
model <- keras_model_sequential() %>%
  layer_dense(units = 32, activation = 'relu', input_shape = c(ncol(X_train_mat))) %>%
  layer_dropout(rate = 0.2)  %>%
  layer_dense(units = 32, activation = 'relu') %>%
  layer_dropout(rate = 0.2)  %>%
  layer_dense(units = 16, activation = 'relu') %>%
  layer_dropout(rate = 0.2)  %>%
  layer_dense(units = 16, activation = 'relu') %>%
  layer_dropout(rate = 0.2)  %>%
  layer_dense(units = 1)

model %>% keras::compile(loss = 'mse',
                         optimizer = 'rmsprop',
                         metrics = 'mae')

# Fit the model
model %>%
  fit(X_train_mat,
      y_train,
      epochs = 100,
      #batch_size = 1,
      validation_split = 0.2)

# Prediction on test data
pred. <- model %>% predict(X_test_mat)
ANN_pred_BM_tot = pred.

# Calculate RMSE
rmse <- round(sqrt(mean((y_test - pred.)^2, na.rm = TRUE)), 2)

# Calculate adjusted R? for training data
pred.train =  model %>% predict(X_train_mat)
R_adj_train <- calcule_R2(y_train,pred.train)
n_train <- nrow(df_train)
p_train <- ncol(df_train) - 6
r_adj_train <- 1 - ((1 - R_adj_train) * (n_train - 1) / (n_train - p_train - 1))

# Calculate R? for test data
res <- lrm(y_test ~ pred., x = TRUE, y = TRUE)
res = res$stats
R2_test = round(res[["R2"]], 2)

MAE <- mean(abs(pred. - y_test), na.rm = TRUE)

# Round results
rmse <- round(rmse, 2)
r_adj_train <- round(r_adj_train, 2)
R2_test <- round(R2_test, 2)
MAE <- round(MAE, 2)

# Output
results_df <- data.frame(
  Response_variables = var_rep,
  R2_adjusted_train = r_adj_train,
  R2_test = R2_test,
  RMSE = rmse,
  MAE = MAE
)



#3 Faire des prédictions sur l'ensemble de test

glm_preds_BM_tot <- as.numeric(glm_mode_BM_tot$predit)
gam_preds_BM_tot <- as.numeric(gam_mode_BM_tot$predit)
rf_preds_BM_tot <- as.numeric(rf_mode_BM_tot$predit)
gbm_preds_BM_tot <- as.numeric(gbm_mode_BM_tot$predit)
ann_preds_BM_tot <- as.numeric(ANN_pred_BM_tot)


#4 modèle d'ensemble


#4.1 Moyenne des prédictions
ensemble_preds_BM_tot <- (glm_preds_BM_tot + 
                            gam_preds_BM_tot + 
                            rf_preds_BM_tot + 
                            gbm_preds_BM_tot + 
                            ann_preds_BM_tot) / 5


#4.2 Stacking (méta-apprentissage) 

# prédictions de chaque modèle
ensemble_datas <- data.frame(glm = glm_preds_BM_tot, 
                             gam = gam_preds_BM_tot, 
                             rf = rf_preds_BM_tot, 
                             gbm = gbm_preds_BM_tot, 
                             ann = ann_preds_BM_tot, 
                             actual = BM_tot_test[[var_rep]])

# Ajustement d'un LM
meta_mode_BM_tot <- lm(actual ~ ., data = ensemble_datas)
summary(meta_mode_BM_tot)
# Faire des prédictions finales
final_preds_BM_tot <- predict(meta_mode_BM_tot, newdata = ensemble_datas)



#5 Performances du modèle d'ensemble :

### 5.1 Moyenne des prédictions   ***
# postResample(pred = ensemble_preds_BM_tot, obs = BM_tot_test[[var_rep]])

# Calcul du RMSE pour évaluer la qualite du modele
rmse <- round (sqrt(mean((BM_tot_test[[var_rep]] - ensemble_preds_BM_tot)^2,na.rm=TRUE)),2)

# Calcul du R² ajusté pour train
R_adj_train <- calcule_R2(BM_tot_test[[var_rep]],  ensemble_preds_BM_tot)
n_train <- nrow(ensemble_datas)
p_train <- ncol(ensemble_datas) - 1
r_adj_train <- 1 - ((1 - R_adj_train) * (n_train - 1) / (n_train - p_train - 1))

# Calcul du R² 
res <- rms::lrm(BM_tot_test[[var_rep]]  ~ ensemble_preds_BM_tot, x= TRUE, y = TRUE)
res = res$stats
R2_test = round (res[["R2"]],2)
MAE <- mean(abs(ensemble_preds_BM_tot - BM_tot_test[[var_rep]]),na.rm=TRUE)

# Round results
rmse <- round(rmse, 2)
r_adj_train <- round(r_adj_train, 2)
R2_test <- round(R2_test, 2)
MAE <- round(MAE, 2)

# output
Averaging_BM_tot <- data.frame(
  Models = "Averaging",
  Rep.var = "Biomass",
  R2_adj_train  = r_adj_train,
  R2_test = R2_test,
  RMSE = rmse,
  MAE = MAE
)
Averaging_BM_tot
write.xlsx(x = Averaging_BM_tot,file = paste0("Results/", var_rep, "_averaging.xlsx"),
           rowNames = FALSE, overwrite = TRUE)




### 5.2 Stacking (méta-apprentissage) ***
# postResample(pred = final_preds_BM_tot, obs = BM_tot_test[[var_rep]])

# Calcul du RMSE pour évaluer la qualite du modele
rmse <- round (sqrt(mean((BM_tot_test[[var_rep]] - final_preds_BM_tot)^2,na.rm=TRUE)),2)

# Calcul du R² ajusté pour train
R_adj_train <- calcule_R2(BM_tot_test[[var_rep]],  final_preds_BM_tot)
n_train <- nrow(ensemble_datas)
p_train <- ncol(ensemble_datas) - 1
r_adj_train <- 1 - ((1 - R_adj_train) * (n_train - 1) / (n_train - p_train - 1))

# Calcul du R² 
res <- rms::lrm(BM_tot_test[[var_rep]]  ~ final_preds_BM_tot, x= TRUE, y = TRUE)
res = res$stats
R2_test = round (res[["R2"]],2)
MAE <- mean(abs(final_preds_BM_tot - BM_tot_test[[var_rep]]),na.rm=TRUE)

# Round results
rmse <- round(rmse, 2)
r_adj_train <- round(r_adj_train, 2)
R2_test <- round(R2_test, 2)
MAE <- round(MAE, 2)

# output
Stking_BM_tot <- data.frame(
  Models = "Stacking",
  Rep.var = "Biomass",
  R2_adj_train  = r_adj_train,
  R2_test = R2_test,
  RMSE = rmse,
  MAE = MAE
)
Stking_BM_tot
write.xlsx(x = Stking_BM_tot,file = paste0("Results/", var_rep, "_stacking.xlsx"),
           rowNames = FALSE, overwrite = TRUE)








# Richness ---------------------------------------------------------------

var_rep = "Richesse_tot" 

#2 Ajuster les modèles

# GLM
glm_mode_Richesse_tot <- GLM_2(var_rep = var_rep, 
                         df_app =Richesse_tot_train[,c(var_rep,best_vars_Richesse_tot_GLM)], 
                         df_valid =Richesse_tot_test [,c(var_rep,best_vars_Richesse_tot_GLM)])
glm_mode_Richesse_tot$df

# GAM
gam_mode_Richesse_tot <- GAM_2(var_rep = var_rep, 
                         df_app =Richesse_tot_train[,c(var_rep,best_vars_Richesse_tot_GAM)], 
                         df_valid =Richesse_tot_test [,c(var_rep,best_vars_Richesse_tot_GAM)])
gam_mode_Richesse_tot$df

# Random Forest
rf_mode_Richesse_tot <- ForetAlea_2(var_rep = var_rep, 
                              df_app =Richesse_tot_train[,c(var_rep,best_vars_Richesse_tot_RF)], 
                              df_valid =Richesse_tot_test [,c(var_rep,best_vars_Richesse_tot_RF)])
rf_mode_Richesse_tot$df
# GBM
gbm_mode_Richesse_tot <- GBM_2(var_rep = var_rep, 
                         df_app =Richesse_tot_train[,c(var_rep,best_vars_Richesse_tot_GBM)], 
                         df_valid =Richesse_tot_test [,c(var_rep,best_vars_Richesse_tot_GBM)])
gbm_mode_Richesse_tot$df


# ANN

df_train = Richesse_tot_train[, c(var_rep, best_vars_Richesse_tot_ANN, LC_levels)]
df_test = Richesse_tot_test[, c(var_rep, best_vars_Richesse_tot_ANN,LC_levels)]
df_train$LC_ = NULL
df_test$LC_ =NULL

df_train = drop_na(df_train)
df_train = droplevels(df_train)

# les feature  a fusionnn?e en faisant la somme
LC_levels


variables = names(df_train)[!names(df_train) %in% var_rep]
X_train <- df_train[, variables]
y_train <- df_train[, var_rep]

X_test <- df_test[, variables]
y_test <- df_test[, var_rep]


X_train <- as.data.frame(lapply(X_train, as.numeric))
X_test <- as.data.frame(lapply(X_test, as.numeric))


# Convert data to matrix and scale
X_train_mat <- as.matrix(scale(X_train))
X_test_mat <- as.matrix(scale(X_test))

# Define the model
model <- keras_model_sequential() %>%
  layer_dense(units = 32, activation = 'relu', input_shape = c(ncol(X_train_mat))) %>%
  layer_dropout(rate = 0.2)  %>%
  layer_dense(units = 32, activation = 'relu') %>%
  layer_dropout(rate = 0.2)  %>%
  layer_dense(units = 16, activation = 'relu') %>%
  layer_dropout(rate = 0.2)  %>%
  layer_dense(units = 16, activation = 'relu') %>%
  layer_dropout(rate = 0.2)  %>%
  layer_dense(units = 1)

model %>% keras::compile(loss = 'mse',
                         optimizer = 'rmsprop',
                         metrics = 'mae')

# Fit the model
model %>%
  fit(X_train_mat,
      y_train,
      epochs = 100,
      #batch_size = 1,
      validation_split = 0.2)

# Prediction on test data
pred. <- model %>% predict(X_test_mat)
ANN_pred_Richesse_tot = pred.

# Calculate RMSE
rmse <- round(sqrt(mean((y_test - pred.)^2, na.rm = TRUE)), 2)

# Calculate adjusted R? for training data
pred.train =  model %>% predict(X_train_mat)
R_adj_train <- calcule_R2(y_train,pred.train)
n_train <- nrow(df_train)
p_train <- ncol(df_train) - 6
r_adj_train <- 1 - ((1 - R_adj_train) * (n_train - 1) / (n_train - p_train - 1))

# Calculate R? for test data
res <- lrm(y_test ~ pred., x = TRUE, y = TRUE)
res = res$stats
R2_test = round(res[["R2"]], 2)

MAE <- mean(abs(pred. - y_test), na.rm = TRUE)

# Round results
rmse <- round(rmse, 2)
r_adj_train <- round(r_adj_train, 2)
R2_test <- round(R2_test, 2)
MAE <- round(MAE, 2)

# Output
results_df <- data.frame(
  Response_variables = var_rep,
  R2_adjusted_train = r_adj_train,
  R2_test = R2_test,
  RMSE = rmse,
  MAE = MAE
)



#3 Faire des prédictions sur l'ensemble de test

glm_preds_Richesse_tot <- as.numeric(glm_mode_Richesse_tot$predit)
gam_preds_Richesse_tot <- as.numeric(gam_mode_Richesse_tot$predit)
rf_preds_Richesse_tot <- as.numeric(rf_mode_Richesse_tot$predit)
gbm_preds_Richesse_tot <- as.numeric(gbm_mode_Richesse_tot$predit)
ann_preds_Richesse_tot <- as.numeric(ANN_pred_Richesse_tot)


#4 modèle d'ensemble


#4.1 Moyenne des prédictions
ensemble_preds_Richesse_tot <- (glm_preds_Richesse_tot + 
                            gam_preds_Richesse_tot + 
                            rf_preds_Richesse_tot + 
                            gbm_preds_Richesse_tot + 
                            ann_preds_Richesse_tot) / 5


#4.2 Stacking (méta-apprentissage) 

# prédictions de chaque modèle
ensemble_datas <- data.frame(glm = glm_preds_Richesse_tot, 
                             gam = gam_preds_Richesse_tot, 
                             rf = rf_preds_Richesse_tot, 
                             gbm = gbm_preds_Richesse_tot, 
                             ann = ann_preds_Richesse_tot, 
                             actual = Richesse_tot_test[[var_rep]])

# Ajustement d'un LM
meta_mode_Richesse_tot <- lm(actual ~ ., data = ensemble_datas)
summary(meta_mode_Richesse_tot)
# Faire des prédictions finales
final_preds_Richesse_tot <- predict(meta_mode_Richesse_tot, newdata = ensemble_datas)



#5 Performances du modèle d'ensemble :

### 5.1 Moyenne des prédictions   ***
# postResample(pred = ensemble_preds_Richesse_tot, obs = Richesse_tot_test[[var_rep]])

# Calcul du RMSE pour évaluer la qualite du modele
rmse <- round (sqrt(mean((Richesse_tot_test[[var_rep]] - ensemble_preds_Richesse_tot)^2,na.rm=TRUE)),2)

# Calcul du R² ajusté pour train
R_adj_train <- calcule_R2(Richesse_tot_test[[var_rep]],  ensemble_preds_Richesse_tot)
n_train <- nrow(ensemble_datas)
p_train <- ncol(ensemble_datas) - 1
r_adj_train <- 1 - ((1 - R_adj_train) * (n_train - 1) / (n_train - p_train - 1))

# Calcul du R² 
res <- rms::lrm(Richesse_tot_test[[var_rep]]  ~ ensemble_preds_Richesse_tot, x= TRUE, y = TRUE)
res = res$stats
R2_test = round (res[["R2"]],2)
MAE <- mean(abs(ensemble_preds_Richesse_tot - Richesse_tot_test[[var_rep]]),na.rm=TRUE)

# Round results
rmse <- round(rmse, 2)
r_adj_train <- round(r_adj_train, 2)
R2_test <- round(R2_test, 2)
MAE <- round(MAE, 2)

# output
Averaging_Richesse_tot <- data.frame(
  Models = "Averaging",
  Rep.var = "Richness",
  R2_adj_train  = r_adj_train,
  R2_test = R2_test,
  RMSE = rmse,
  MAE = MAE
)
Averaging_Richesse_tot
write.xlsx(x = Averaging_Richesse_tot,file = paste0("Results/", var_rep, "_averaging.xlsx"),
           rowNames = FALSE, overwrite = TRUE)




### 5.2 Stacking (méta-apprentissage) ***
# postResample(pred = final_preds_Richesse_tot, obs = Richesse_tot_test[[var_rep]])

# Calcul du RMSE pour évaluer la qualite du modele
rmse <- round (sqrt(mean((Richesse_tot_test[[var_rep]] - final_preds_Richesse_tot)^2,na.rm=TRUE)),2)

# Calcul du R² ajusté pour train
R_adj_train <- calcule_R2(Richesse_tot_test[[var_rep]],  final_preds_Richesse_tot)
n_train <- nrow(ensemble_datas)
p_train <- ncol(ensemble_datas) - 1
r_adj_train <- 1 - ((1 - R_adj_train) * (n_train - 1) / (n_train - p_train - 1))

# Calcul du R² 
res <- rms::lrm(Richesse_tot_test[[var_rep]]  ~ final_preds_Richesse_tot, x= TRUE, y = TRUE)
res = res$stats
R2_test = round (res[["R2"]],2)
MAE <- mean(abs(final_preds_Richesse_tot - Richesse_tot_test[[var_rep]]),na.rm=TRUE)

# Round results
rmse <- round(rmse, 2)
r_adj_train <- round(r_adj_train, 2)
R2_test <- round(R2_test, 2)
MAE <- round(MAE, 2)

# output
Stking_Richesse_tot <- data.frame(
  Models = "Stacking",
  Rep.var = "Richness",
  R2_adj_train  = r_adj_train,
  R2_test = R2_test,
  RMSE = rmse,
  MAE = MAE
)
Stking_Richesse_tot
write.xlsx(x = Stking_Richesse_tot,file = paste0("Results/", var_rep, "_stacking.xlsx"),
           rowNames = FALSE, overwrite = TRUE)
