


prodij_clean = read.csv2("datas/proDij/prodij_clean.csv")


predictor_non_r = c("Long", "Lat" ,"SableF" , "LimonF" ,"LimonG" ,"Argile" ,"C_org" ,"C.N", "pH_eau","OS")
predictor_deep = c(predictor_non_r,c("Sables","SableG","Limons","MO","C_tot","N_tot"))
vdt_col=c("AB_tot", "BM_tot", "Richesse")

prodij_clean_short = prodij_clean[, c(vdt_col,predictor_deep)]

# prodij_clean_short$AB_tot = sqrt(prodij_clean_short$AB_tot)
# prodij_clean_short$BM_tot = sqrt(prodij_clean_short$BM_tot)

variables_factor = c("OS")
vars_scale = predictor_deep[!predictor_deep %in% variables_factor]
# prodij_clean_short[,vars_scale] = scale(prodij_clean_short[,vars_scale])

prodij_clean_short$OS = as.factor(prodij_clean_short$OS)
# prodij_clean_short$Ferti_ = as.factor(prodij_clean_short$Ferti_)
# prodij_clean_short$w_sol_ = as.factor(prodij_clean_short$w_sol_)

colSums(is.na(prodij_clean_short))


set.seed(42)
split <- rsample::initial_split(prodij_clean_short, prop = 0.8, strata = "AB_tot")
AB_tot_train <- rsample::training(split)
AB_tot_test <- rsample::testing(split)



set.seed(42)
split <- rsample::initial_split(prodij_clean_short, prop = 0.8, strata = "BM_tot")
BM_tot_train <- rsample::training(split)
BM_tot_test <- rsample::testing(split)



set.seed(42)
split <- rsample::initial_split(prodij_clean_short, prop = 0.8, strata = "Richesse")
Richesse_train <- rsample::training(split)
Richesse_test <- rsample::testing(split)




seuil_nbr_vars = 7 
loss = "rmse"


# AB_tot ---------------------------------------------------
## glm -------------------------------

df_train = AB_tot_train[,c("AB_tot",predictor_non_r )]
df_test = AB_tot_test[,c("AB_tot",predictor_non_r )]

var_rep = "AB_tot"  
algo = "glm"


variables = names(df_train)[! names(df_train) %in% var_rep]
X <- df_train[, variables]
y <- df_train[, var_rep]


AB_tot_select_glm <- data.frame(Algorithms = character(),
           Response_variables = character(),
           R2_adjusted_train = numeric(),
           R2_test = numeric(),
           RMSE = numeric(),
           MAE = numeric(),
           delet = character(),
           models = numeric())

del_vars =c()
for (i in 1: (length(variables)-1) ) {
  
  cat(paste0("***** model: ",i,"/",length(variables),"\n" ))
  
  model = glm(AB_tot ~ ., data = df_train)
  
  # Utilisation du conteneur iml Predictor()
  set.seed(1111)
  predictor <- Predictor$new(model, data = X, y = y) 
  imp <- FeatureImp$new(predictor, loss = loss) 
  var_importance <- as.data.frame(imp$results)
  
  
  #  la variable à supprimer
  to_remove <- var_importance[which.min(var_importance$importance), "feature"]
  del_vars = c(del_vars, paste0(i,"_", to_remove))

  # Prediction sur le jeu de donnée test
  pred.<- predict(model,newdata=df_test)
  
  # Calcul du RMSE pour évaluer la qualite du modele
  rmse <- round (sqrt(mean((df_test[[var_rep]] - pred.)^2,na.rm=TRUE)),2)
  
  # Calcul du R² ajusté pour df_train
  R_adj_train <- calcule_R2(df_train[,var_rep],  predict(model, newdata=df_train))
  n_train <- nrow(df_train)
  p_train <- ncol(df_train) - 1
  r_adj_train <- 1 - ((1 - R_adj_train) * (n_train - 1) / (n_train - p_train - 1))
  
  # Calcul du R² 
  res <- rms::lrm(df_test[[var_rep]]  ~ pred., x= TRUE, y = TRUE)
  res = res$stats
  R2_test = round (res[["R2"]],2)
  
  MAE <- mean(abs(pred. - df_test[[var_rep]]),na.rm=TRUE)
  
  # Round results
  rmse <- round(rmse, 2)
  r_adj_train <- round(r_adj_train, 2)
  R2_test <- round(R2_test, 2)
  MAE <- round(MAE, 2)
  
  
  # output
  results_df <- data.frame(Algorithms = algo,
                           Response_variables = var_rep,
                           R2_adjusted_train = r_adj_train,
                           R2_test = R2_test,
                           RMSE = rmse,
                           MAE = MAE,
                           delet = paste0(i,"_",to_remove),
                           models = paste0("mod_",i))

  
  AB_tot_select_glm = rbind(AB_tot_select_glm,results_df)
  
  
  # mis a jours de df_train, et de X
  df_train[[to_remove]] = NULL # suppression de la variables la moins importante
  df_test[[to_remove]] = NULL # suppression de la variables la moins importante
  variables_tp = names(df_train)[! names(df_train) %in% var_rep]
  X <- as.data.frame(df_train[, variables_tp])
  names(X) = names(df_test)[-1]
  
  rm("results_df")
}

AB_tot_select_glm 

write.xlsx(x =AB_tot_select_glm,
           file = paste0("variables_selections/ProDij/",var_rep,"_select_",algo,".xlsx"),
           rowNames = FALSE,overwrite = TRUE)


rm("predictor","imp", "var_importance","df_info","var_rep","selected_vars")

## end variables selection for AB_tot with glm ##





## rf -------------------------------

df_train = AB_tot_train[, c("AB_tot", predictor_deep)]
df_test = AB_tot_test[, c("AB_tot", predictor_deep)]

var_rep = "AB_tot"  
algo = "RF"

variables = names(df_train)[!names(df_train) %in% var_rep]
X <- df_train[, variables]
y <- df_train[, var_rep]

AB_tot_select_rf <- data.frame(
  Algorithms = character(),
  Response_variables = character(),
  R2_adjusted_train = numeric(),
  R2_test = numeric(),
  RMSE = numeric(),
  MAE = numeric(),
  delet = character(),
  models = numeric()
)

del_vars = c()
for (i in 1:(length(variables) - 1)) {
  cat(paste0("***** model: ", i, "/", length(variables), "\n"))
  
  model = randomForest(AB_tot ~ ., data = df_train)
  
  # Utilisation du conteneur iml Predictor()
  set.seed(1111)
  predictor <- Predictor$new(model, data = X, y = y)
  imp <- FeatureImp$new(predictor, loss = loss)
  var_importance <- as.data.frame(imp$results)
  
  # La variable à supprimer
  to_remove <- var_importance[which.min(var_importance$importance), "feature"]
  del_vars = c(del_vars, paste0(i, "_", to_remove))
  
  # Prédiction sur le jeu de données test
  pred. <- predict(model, newdata = df_test)
  
  # Calcul du RMSE pour évaluer la qualité du modèle
  rmse <- round(sqrt(mean((df_test[[var_rep]] - pred.)^2, na.rm = TRUE)), 2)
  
  # Calcul du R² ajusté pour df_train
  R_adj_train <- calcule_R2(df_train[, var_rep], predict(model, newdata = df_train))
  n_train <- nrow(df_train)
  p_train <- ncol(df_train) - 1
  r_adj_train <- 1 - ((1 - R_adj_train) * (n_train - 1) / (n_train - p_train - 1))
  
  # Calcul du R²
  res <- lrm(df_test[[var_rep]] ~ pred., x = TRUE, y = TRUE)
  res = res$stats
  R2_test = round(res[["R2"]], 2)
  
  MAE <- mean(abs(pred. - df_test[[var_rep]]), na.rm = TRUE)
  
  # Round results
  rmse <- round(rmse, 2)
  r_adj_train <- round(r_adj_train, 2)
  R2_test <- round(R2_test, 2)
  MAE <- round(MAE, 2)
  
  # Output
  results_df <- data.frame(
    Algorithms = algo,
    Response_variables = var_rep,
    R2_adjusted_train = r_adj_train,
    R2_test = R2_test,
    RMSE = rmse,
    MAE = MAE,
    delet = paste0(i,"_",to_remove),
    models = paste0("mod_", i)
  )
  
  AB_tot_select_rf = rbind(AB_tot_select_rf, results_df)
  
  # Mise à jour de df_train et de X
  df_train[[to_remove]] = NULL  # suppression de la variable la moins importante
  df_test[[to_remove]] = NULL   # suppression de la variable la moins importante
  variables_tp = names(df_train)[!names(df_train) %in% var_rep]
  X <- df_train[, variables_tp]
  names(X) = names(df_test)[-1]
  
  rm("results_df")
}

AB_tot_select_rf 

write.xlsx(x = AB_tot_select_rf,
           file = paste0("variables_selections/ProDij/",var_rep,"_select_",algo,".xlsx"),
           rowNames = FALSE, overwrite = TRUE)


## end variables selection for AB_tot with rf ##



## gbm -------------------------------

df_train = AB_tot_train[, c("AB_tot", predictor_deep)]
df_test = AB_tot_test[, c("AB_tot", predictor_deep)]

var_rep = "AB_tot"  
algo = "gbm"

variables = names(df_train)[!names(df_train) %in% var_rep]
X <- df_train[, variables]
y <- df_train[, var_rep]

AB_tot_select_gbm <- data.frame(
  Algorithms = character(),
  Response_variables = character(),
  R2_adjusted_train = numeric(),
  R2_test = numeric(),
  RMSE = numeric(),
  MAE = numeric(),
  delet = character(),
  models = numeric()
)

del_vars = c()
for (i in 1:(length(variables) - 1)) {
  cat(paste0("***** model: ", i, "/", length(variables), "\n"))
  
  model = gbm(AB_tot ~ ., data = df_train)
  
  # Utilisation du conteneur iml Predictor()
  set.seed(1111)
  predictor <- Predictor$new(model, data = X, y = y)
  imp <- FeatureImp$new(predictor, loss = loss)
  var_importance <- as.data.frame(imp$results)
  
  # La variable à supprimer
  to_remove <- var_importance[which.min(var_importance$importance), "feature"]
  del_vars = c(del_vars, paste0(i, "_", to_remove))
  
  # Prédiction sur le jeu de données test
  pred. <- predict(model, newdata = df_test, n.trees = model$n.trees)
  
  # Calcul du RMSE pour évaluer la qualité du modèle
  rmse <- round(sqrt(mean((df_test[[var_rep]] - pred.)^2, na.rm = TRUE)), 2)
  
  # Calcul du R² ajusté pour df_train
  R_adj_train <- calcule_R2(df_train[, var_rep], predict(model, newdata = df_train, n.trees = model$n.trees))
  n_train <- nrow(df_train)
  p_train <- ncol(df_train) - 1
  r_adj_train <- 1 - ((1 - R_adj_train) * (n_train - 1) / (n_train - p_train - 1))
  
  # Calcul du R²
  res <- lrm(df_test[[var_rep]] ~ pred., x = TRUE, y = TRUE)
  res = res$stats
  R2_test = round(res[["R2"]], 2)
  
  MAE <- mean(abs(pred. - df_test[[var_rep]]), na.rm = TRUE)
  
  # Round results
  rmse <- round(rmse, 2)
  r_adj_train <- round(r_adj_train, 2)
  R2_test <- round(R2_test, 2)
  MAE <- round(MAE, 2)
  
  # Output
  results_df <- data.frame(
    Algorithms = algo,
    Response_variables = var_rep,
    R2_adjusted_train = r_adj_train,
    R2_test = R2_test,
    RMSE = rmse,
    MAE = MAE,
    delet = paste0(i,"_",to_remove),
    models = paste0("mod_", i)
  )
  
  AB_tot_select_gbm = rbind(AB_tot_select_gbm, results_df)
  
  # Mise à jour de df_train et de X
  df_train[[to_remove]] = NULL  # suppression de la variable la moins importante
  df_test[[to_remove]] = NULL   # suppression de la variable la moins importante
  variables_tp = names(df_train)[!names(df_train) %in% var_rep]
  X <- df_train[, variables_tp]
  names(X) = names(df_test)[-1]
  
  rm("results_df")
}

AB_tot_select_gbm 

write.xlsx(x = AB_tot_select_gbm,
           file = paste0("variables_selections/ProDij/",var_rep,"_select_",algo,".xlsx"),
           rowNames = FALSE, overwrite = TRUE)

## end variables selection for AB_tot with gbm ##








## ann -------------------------------


df_train = AB_tot_train[, c("AB_tot", predictor_deep)]
df_test = AB_tot_test[, c("AB_tot", predictor_deep)]

# converssion des factors et function de fusion
{ 
dummy_vars <- model.matrix(~ OS - 1, data = df_train)
df_train <- cbind(df_train, dummy_vars)
df_train <- df_train[, -which(names(df_train) == "OS")]
# dummy_vars <- model.matrix(~ w_sol_ - 1, data = df_train)
# df_train <- cbind(df_train, dummy_vars)
# df_train <- df_train[, -which(names(df_train) == "w_sol_")]
# dummy_vars <- model.matrix(~ Ferti_ - 1, data = df_train)
# df_train <- cbind(df_train, dummy_vars)
# df_train <- df_train[, -which(names(df_train) == "Ferti_")]


dummy_vars <- model.matrix(~ OS - 1, data = df_test)
df_test <- cbind(df_test, dummy_vars)
df_test <- df_test[, -which(names(df_test) == "OS")]
# dummy_vars <- model.matrix(~ w_sol_ - 1, data = df_test)
# df_test <- cbind(df_test, dummy_vars)
# df_test <- df_test[, -which(names(df_test) == "w_sol_")]
# dummy_vars <- model.matrix(~ Ferti_ - 1, data = df_test)
# df_test <- cbind(df_test, dummy_vars)
# df_test <- df_test[, -which(names(df_test) == "Ferti_")]


df_train = drop_na(df_train)
df_train = droplevels(df_train)

# les feature  a fusionnnée en faisant la somme
OS_l = c("OSOS_111", "OSOS_210", "OSOS_214", "OSOS_218")
# w_sol_l = c("w_sol_non", "w_sol_oui")
# Ferti_l = c("Ferti_non", "Ferti_oui")


# Function to merge features
merge_features <- function(df, feature_list, new_feature_name) {
  merged_row <- data.frame(
    feature = new_feature_name,
    importance.05 = sum(df[df$feature %in% feature_list, "importance.05"]),
    importance = sum(df[df$feature %in% feature_list, "importance"]),
    importance.95 = sum(df[df$feature %in% feature_list, "importance.95"]),
    permutation.error = sum(df[df$feature %in% feature_list, "permutation.error"])
  )
  df <- df[!df$feature %in% feature_list, ]
  df <- rbind(df, merged_row)
  return(df)
}

}

var_rep = "AB_tot"  
algo = "ann"

variables = names(df_train)[!names(df_train) %in% var_rep]
X_train <- df_train[, variables]
y_train <- df_train[, var_rep]

X_test <- df_test[, variables]
y_test <- df_test[, var_rep]

AB_tot_select_ann <- data.frame(
  Algorithms = character(),
  Response_variables = character(),
  R2_adjusted_train = numeric(),
  R2_test = numeric(),
  RMSE = numeric(),
  MAE = numeric(),
  delet = character(),
  models = numeric()
)

del_vars = c()

nbr_iter = length(predictor_deep) - 1

X_train <- as.data.frame(lapply(X_train, as.numeric))
X_test <- as.data.frame(lapply(X_test, as.numeric))

for (i in 1:nbr_iter) {
  
  cat(paste0("***** model: ", i, "/", nbr_iter, "\n"))
  
  # Convert data to matrix and scale
  X_train_mat <- as.matrix(scale(X_train))
  X_test_mat <- as.matrix(scale(X_test))
  
  # Define the model
  model <- keras_model_sequential() %>%
    layer_dense(units = 8, activation = 'relu', input_shape = c(ncol(X_train_mat))) %>%
    # layer_dropout(rate = 0.2)  %>%
    # layer_dense(units = 8, activation = 'relu') %>%
    # layer_dropout(rate = 0.2)  %>%
    # layer_dense(units = 16, activation = 'relu') %>%
    # layer_dropout(rate = 0.2)  %>%
    # layer_dense(units = 16, activation = 'relu') %>%
    # layer_dropout(rate = 0.2)  %>%
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
  
  # Predictor and feature importance
  set.seed(1111)
  predictor <- Predictor$new(model, data = as.data.frame(X_train), y = y_train)
  imp <- FeatureImp$new(predictor, loss = loss)
  var_importance <- as.data.frame(imp$results)
  # Merge the features
  var_importance <- merge_features(var_importance, OS_l, "OS")
  # var_importance <- merge_features(var_importance, w_sol_l, "w_sol")
  # var_importance <- merge_features(var_importance, Ferti_l, "Ferti")
  var_importance <- var_importance[order(-var_importance$importance), ]
  
  # Variable to remove
  to_remove <- var_importance[which.min(var_importance$importance), "feature"]
  del_vars = c(del_vars, to_remove)
  
  # Prediction on test data
  pred. <- model %>% predict(X_test_mat)
  
  # Calculate RMSE
  rmse <- round(sqrt(mean((y_test - pred.)^2, na.rm = TRUE)), 2)
  
  # Calculate adjusted R² for training data
  pred.train =  model %>% predict(X_train_mat)
  R_adj_train <- calcule_R2(y_train,pred.train)
  n_train <- nrow(df_train)
  p_train <- ncol(df_train) - 1
  r_adj_train <- 1 - ((1 - R_adj_train) * (n_train - 1) / (n_train - p_train - 1))
  
  # Calculate R² for test data
  res <- lm(y_test ~ pred.)
  R2_test <- summary(res)$r.squared
  
  MAE <- mean(abs(pred. - y_test), na.rm = TRUE)
  
  # Round results
  rmse <- round(rmse, 2)
  r_adj_train <- round(r_adj_train, 2)
  R2_test <- round(R2_test, 2)
  MAE <- round(MAE, 2)
  
  # Output
  results_df <- data.frame(
    Algorithms = algo,
    Response_variables = var_rep,
    R2_adjusted_train = r_adj_train,
    R2_test = R2_test,
    RMSE = rmse,
    MAE = MAE,
    delet = paste0(i,"_",to_remove),
    models = paste0("mod_", i)
  )
  
  AB_tot_select_ann = rbind(AB_tot_select_ann, results_df)
  
  # Update df_train and X_train
  df_train[[to_remove]] <- NULL  # Remove the least important variable
  df_test[[to_remove]] <- NULL   # Remove the least important variable
  variables <- names(df_train)[!names(df_train) %in% var_rep]
  X_train <- df_train[, variables]
  X_test <- df_test[, variables]
  
  X_train <- as.data.frame(lapply(X_train, as.numeric))
  X_test <- as.data.frame(lapply(X_test, as.numeric))
  
  rm("results_df")
}

AB_tot_select_ann
# Write the results to an Excel file
write.xlsx(x = AB_tot_select_ann,
           file = paste0("variables_selections/ProDij/", var_rep, "_select_", algo, ".xlsx"),
           rowNames = FALSE, overwrite = TRUE)


## end variables selection for AB_tot with ann ##









## gam -------------------------------

df_train <- AB_tot_train[, c("AB_tot", predictor_non_r)]
df_test <- AB_tot_test[, c("AB_tot", predictor_non_r)]

var_rep <- "AB_tot"  
algo <- "gam"

variables <- names(df_train)[!names(df_train) %in% var_rep]
X <- df_train[, variables]
y <- df_train[, var_rep]

AB_tot_select_gam <- data.frame(
  Algorithms = character(),
  Response_variables = character(),
  R2_adjusted_train = numeric(),
  R2_test = numeric(),
  RMSE = numeric(),
  MAE = numeric(),
  delet = character(),
  models = numeric()
)

del_vars <- c()

model = gam(AB_tot ~ s(Long) + s(Lat) + s(SableF) + s(LimonF) +
              s(LimonG) + s(Argile) + s(C_org) + s(C.N) + s(pH_eau) + 
              OS, data = df_train)



### A FAIRE MANUELLEMENT
{ 
  # model = gam(AB_tot ~ s(LimonF) + s(Argile), data = df_train)
  
  # Utilisation du conteneur iml Predictor
  set.seed(1111)
  predictor <- Predictor$new(model, data = X, y = y)
  imp <- FeatureImp$new(predictor, loss = loss)
  var_importance <- as.data.frame(imp$results)
  
  # La variable à supprimer
  to_remove <- var_importance[which.min(var_importance$importance), "feature"]
  del_vars <- c(del_vars, to_remove)
  
  
  # Prédiction sur le jeu de données test
  pred <- predict(model, newdata = df_test)
  
  # Calcul du RMSE pour évaluer la qualité du modèle
  rmse <- round(sqrt(mean((df_test[[var_rep]] - pred)^2, na.rm = TRUE)), 2)
  
  # Calcul du R² ajusté pour df_train
  R_adj_train <- calcule_R2(df_train[, var_rep], predict(model, newdata = df_train))
  n_train <- nrow(df_train)
  p_train <- ncol(df_train) - 1
  r_adj_train <- 1 - ((1 - R_adj_train) * (n_train - 1) / (n_train - p_train - 1))
  
  # Calcul du R²
  res <- lrm(df_test[[var_rep]] ~ pred, x = TRUE, y = TRUE)
  res <- res$stats
  R2_test <- round(res[["R2"]], 2)
  
  MAE <- mean(abs(pred - df_test[[var_rep]]), na.rm = TRUE)
  
  # Arrondir les résultats
  rmse <- round(rmse, 2)
  r_adj_train <- round(r_adj_train, 2)
  R2_test <- round(R2_test, 2)
  MAE <- round(MAE, 2)
  
  # Output
  results_df <- data.frame(
    Algorithms = algo,
    Response_variables = var_rep,
    R2_adjusted_train = r_adj_train,
    R2_test = R2_test,
    RMSE = rmse,
    MAE = MAE,
    delet = to_remove,
    models = paste0("mod_", i)
  )
  
  AB_tot_select_gam <- rbind(AB_tot_select_gam, results_df)
  
  # Mise à jour de df_train et de X
  df_train[[to_remove]] <- NULL  # suppression de la variable la moins importante
  df_test[[to_remove]] <- NULL   # suppression de la variable la moins importante
  variables_tp <- names(df_train)[!names(df_train) %in% var_rep]
  X <- df_train[, variables_tp]
  names(X) <- names(df_test)[-1]
  
  rm("results_df")
  
  AB_tot_select_gam 
  
}

AB_tot_select_gam
write.xlsx(x = AB_tot_select_gam,
           file = paste0("variables_selections/ProDij/",var_rep,"_select_",algo,".xlsx"),
           rowNames = FALSE, overwrite = TRUE)


## end variables selection for AB_tot with gam #















# ********************* END AB_tot ************************************** #


# BM_tot ---------------------------------------------------
## glm -------------------------------

df_train = BM_tot_train[,c("BM_tot",predictor_non_r )]
df_test = BM_tot_test[,c("BM_tot",predictor_non_r )]

var_rep = "BM_tot"  
algo = "glm"


variables = names(df_train)[! names(df_train) %in% var_rep]
X <- df_train[, variables]
y <- df_train[, var_rep]


BM_tot_select_glm <- data.frame(Algorithms = character(),
                                Response_variables = character(),
                                R2_adjusted_train = numeric(),
                                R2_test = numeric(),
                                RMSE = numeric(),
                                MAE = numeric(),
                                delet = character(),
                                models = numeric())

del_vars =c()
for (i in 1: (length(variables)-1) ) {
  
  cat(paste0("***** model: ",i,"/",length(variables),"\n" ))
  
  model = glm(BM_tot ~ ., data = df_train)
  
  # Utilisation du conteneur iml Predictor()
  set.seed(1111)
  predictor <- Predictor$new(model, data = X, y = y) 
  imp <- FeatureImp$new(predictor, loss = loss) 
  var_importance <- as.data.frame(imp$results)
  
  
  #  la variable à supprimer
  to_remove <- var_importance[which.min(var_importance$importance), "feature"]
  del_vars = c(del_vars, paste0(i,"_", to_remove))
  
  # Prediction sur le jeu de donnée test
  pred.<- predict(model,newdata=df_test)
  
  # Calcul du RMSE pour évaluer la qualite du modele
  rmse <- round (sqrt(mean((df_test[[var_rep]] - pred.)^2,na.rm=TRUE)),2)
  
  # Calcul du R² ajusté pour df_train
  R_adj_train <- calcule_R2(df_train[,var_rep],  predict(model, newdata=df_train))
  n_train <- nrow(df_train)
  p_train <- ncol(df_train) - 1
  r_adj_train <- 1 - ((1 - R_adj_train) * (n_train - 1) / (n_train - p_train - 1))
  
  # Calcul du R² 
  res <- rms::lrm(df_test[[var_rep]]  ~ pred., x= TRUE, y = TRUE)
  res = res$stats
  R2_test = round (res[["R2"]],2)
  
  MAE <- mean(abs(pred. - df_test[[var_rep]]),na.rm=TRUE)
  
  # Round results
  rmse <- round(rmse, 2)
  r_adj_train <- round(r_adj_train, 2)
  R2_test <- round(R2_test, 2)
  MAE <- round(MAE, 2)
  
  
  # output
  results_df <- data.frame(Algorithms = algo,
                           Response_variables = var_rep,
                           R2_adjusted_train = r_adj_train,
                           R2_test = R2_test,
                           RMSE = rmse,
                           MAE = MAE,
                           delet = paste0(i,"_",to_remove),
                           models = paste0("mod_",i))
  
  
  BM_tot_select_glm = rbind(BM_tot_select_glm,results_df)
  
  
  # mis a jours de df_train, et de X
  df_train[[to_remove]] = NULL # suppression de la variables la moins importante
  df_test[[to_remove]] = NULL # suppression de la variables la moins importante
  variables_tp = names(df_train)[! names(df_train) %in% var_rep]
  X <- as.data.frame(df_train[, variables_tp])
  names(X) = names(df_test)[-1]
  
  rm("results_df")
}

BM_tot_select_glm 

write.xlsx(x =BM_tot_select_glm,
           file = paste0("variables_selections/ProDij/",var_rep,"_select_",algo,".xlsx"),
           rowNames = FALSE,overwrite = TRUE)


rm("predictor","imp", "var_importance","df_info","var_rep","selected_vars")

## end variables selection for BM_tot with glm ##





## rf -------------------------------

df_train = BM_tot_train[, c("BM_tot", predictor_deep)]
df_test = BM_tot_test[, c("BM_tot", predictor_deep)]

var_rep = "BM_tot"  
algo = "RF"

variables = names(df_train)[!names(df_train) %in% var_rep]
X <- df_train[, variables]
y <- df_train[, var_rep]

BM_tot_select_rf <- data.frame(
  Algorithms = character(),
  Response_variables = character(),
  R2_adjusted_train = numeric(),
  R2_test = numeric(),
  RMSE = numeric(),
  MAE = numeric(),
  delet = character(),
  models = numeric()
)

del_vars = c()
for (i in 1:(length(variables) - 1)) {
  cat(paste0("***** model: ", i, "/", length(variables), "\n"))
  
  model = randomForest(BM_tot ~ ., data = df_train)
  
  # Utilisation du conteneur iml Predictor()
  set.seed(1111)
  predictor <- Predictor$new(model, data = X, y = y)
  imp <- FeatureImp$new(predictor, loss = loss)
  var_importance <- as.data.frame(imp$results)
  
  # La variable à supprimer
  to_remove <- var_importance[which.min(var_importance$importance), "feature"]
  del_vars = c(del_vars, paste0(i, "_", to_remove))
  
  # Prédiction sur le jeu de données test
  pred. <- predict(model, newdata = df_test)
  
  # Calcul du RMSE pour évaluer la qualité du modèle
  rmse <- round(sqrt(mean((df_test[[var_rep]] - pred.)^2, na.rm = TRUE)), 2)
  
  # Calcul du R² ajusté pour df_train
  R_adj_train <- calcule_R2(df_train[, var_rep], predict(model, newdata = df_train))
  n_train <- nrow(df_train)
  p_train <- ncol(df_train) - 1
  r_adj_train <- 1 - ((1 - R_adj_train) * (n_train - 1) / (n_train - p_train - 1))
  
  # Calcul du R²
  res <- lrm(df_test[[var_rep]] ~ pred., x = TRUE, y = TRUE)
  res = res$stats
  R2_test = round(res[["R2"]], 2)
  
  MAE <- mean(abs(pred. - df_test[[var_rep]]), na.rm = TRUE)
  
  # Round results
  rmse <- round(rmse, 2)
  r_adj_train <- round(r_adj_train, 2)
  R2_test <- round(R2_test, 2)
  MAE <- round(MAE, 2)
  
  # Output
  results_df <- data.frame(
    Algorithms = algo,
    Response_variables = var_rep,
    R2_adjusted_train = r_adj_train,
    R2_test = R2_test,
    RMSE = rmse,
    MAE = MAE,
    delet = paste0(i,"_",to_remove),
    models = paste0("mod_", i)
  )
  
  BM_tot_select_rf = rbind(BM_tot_select_rf, results_df)
  
  # Mise à jour de df_train et de X
  df_train[[to_remove]] = NULL  # suppression de la variable la moins importante
  df_test[[to_remove]] = NULL   # suppression de la variable la moins importante
  variables_tp = names(df_train)[!names(df_train) %in% var_rep]
  X <- df_train[, variables_tp]
  names(X) = names(df_test)[-1]
  
  rm("results_df")
}

BM_tot_select_rf 

write.xlsx(x = BM_tot_select_rf,
           file = paste0("variables_selections/ProDij/",var_rep,"_select_",algo,".xlsx"),
           rowNames = FALSE, overwrite = TRUE)


## end variables selection for BM_tot with rf ##



## gbm -------------------------------

df_train = BM_tot_train[, c("BM_tot", predictor_deep)]
df_test = BM_tot_test[, c("BM_tot", predictor_deep)]

var_rep = "BM_tot"  
algo = "gbm"

variables = names(df_train)[!names(df_train) %in% var_rep]
X <- df_train[, variables]
y <- df_train[, var_rep]

BM_tot_select_gbm <- data.frame(
  Algorithms = character(),
  Response_variables = character(),
  R2_adjusted_train = numeric(),
  R2_test = numeric(),
  RMSE = numeric(),
  MAE = numeric(),
  delet = character(),
  models = numeric()
)

del_vars = c()
for (i in 1:(length(variables) - 1)) {
  cat(paste0("***** model: ", i, "/", length(variables), "\n"))
  
  model = gbm(BM_tot ~ ., data = df_train)
  
  # Utilisation du conteneur iml Predictor()
  set.seed(1111)
  predictor <- Predictor$new(model, data = X, y = y)
  imp <- FeatureImp$new(predictor, loss = loss)
  var_importance <- as.data.frame(imp$results)
  
  # La variable à supprimer
  to_remove <- var_importance[which.min(var_importance$importance), "feature"]
  del_vars = c(del_vars, paste0(i, "_", to_remove))
  
  # Prédiction sur le jeu de données test
  pred. <- predict(model, newdata = df_test, n.trees = model$n.trees)
  
  # Calcul du RMSE pour évaluer la qualité du modèle
  rmse <- round(sqrt(mean((df_test[[var_rep]] - pred.)^2, na.rm = TRUE)), 2)
  
  # Calcul du R² ajusté pour df_train
  R_adj_train <- calcule_R2(df_train[, var_rep], predict(model, newdata = df_train, n.trees = model$n.trees))
  n_train <- nrow(df_train)
  p_train <- ncol(df_train) - 1
  r_adj_train <- 1 - ((1 - R_adj_train) * (n_train - 1) / (n_train - p_train - 1))
  
  # Calcul du R²
  res <- lrm(df_test[[var_rep]] ~ pred., x = TRUE, y = TRUE)
  res = res$stats
  R2_test = round(res[["R2"]], 2)
  
  MAE <- mean(abs(pred. - df_test[[var_rep]]), na.rm = TRUE)
  
  # Round results
  rmse <- round(rmse, 2)
  r_adj_train <- round(r_adj_train, 2)
  R2_test <- round(R2_test, 2)
  MAE <- round(MAE, 2)
  
  # Output
  results_df <- data.frame(
    Algorithms = algo,
    Response_variables = var_rep,
    R2_adjusted_train = r_adj_train,
    R2_test = R2_test,
    RMSE = rmse,
    MAE = MAE,
    delet = paste0(i,"_",to_remove),
    models = paste0("mod_", i)
  )
  
  BM_tot_select_gbm = rbind(BM_tot_select_gbm, results_df)
  
  # Mise à jour de df_train et de X
  df_train[[to_remove]] = NULL  # suppression de la variable la moins importante
  df_test[[to_remove]] = NULL   # suppression de la variable la moins importante
  variables_tp = names(df_train)[!names(df_train) %in% var_rep]
  X <- df_train[, variables_tp]
  names(X) = names(df_test)[-1]
  
  rm("results_df")
}

BM_tot_select_gbm 

write.xlsx(x = BM_tot_select_gbm,
           file = paste0("variables_selections/ProDij/",var_rep,"_select_",algo,".xlsx"),
           rowNames = FALSE, overwrite = TRUE)

## end variables selection for BM_tot with gbm ##








## ann -------------------------------


df_train = BM_tot_train[, c("BM_tot", predictor_deep)]
df_test = BM_tot_test[, c("BM_tot", predictor_deep)]

# converssion des factors et function de fusion
{ 
  dummy_vars <- model.matrix(~ OS - 1, data = df_train)
  df_train <- cbind(df_train, dummy_vars)
  df_train <- df_train[, -which(names(df_train) == "OS")]
  # dummy_vars <- model.matrix(~ w_sol_ - 1, data = df_train)
  # df_train <- cbind(df_train, dummy_vars)
  # df_train <- df_train[, -which(names(df_train) == "w_sol_")]
  # dummy_vars <- model.matrix(~ Ferti_ - 1, data = df_train)
  # df_train <- cbind(df_train, dummy_vars)
  # df_train <- df_train[, -which(names(df_train) == "Ferti_")]
  
  
  dummy_vars <- model.matrix(~ OS - 1, data = df_test)
  df_test <- cbind(df_test, dummy_vars)
  df_test <- df_test[, -which(names(df_test) == "OS")]
  # dummy_vars <- model.matrix(~ w_sol_ - 1, data = df_test)
  # df_test <- cbind(df_test, dummy_vars)
  # df_test <- df_test[, -which(names(df_test) == "w_sol_")]
  # dummy_vars <- model.matrix(~ Ferti_ - 1, data = df_test)
  # df_test <- cbind(df_test, dummy_vars)
  # df_test <- df_test[, -which(names(df_test) == "Ferti_")]
  
  
  df_train = drop_na(df_train)
  df_train = droplevels(df_train)
  
  # les feature  a fusionnnée en faisant la somme
  OS_l = c("OSOS_111", "OSOS_210", "OSOS_214", "OSOS_218")
  # w_sol_l = c("w_sol_non", "w_sol_oui")
  # Ferti_l = c("Ferti_non", "Ferti_oui")
  
  
  # Function to merge features
  merge_features <- function(df, feature_list, new_feature_name) {
    merged_row <- data.frame(
      feature = new_feature_name,
      importance.05 = sum(df[df$feature %in% feature_list, "importance.05"]),
      importance = sum(df[df$feature %in% feature_list, "importance"]),
      importance.95 = sum(df[df$feature %in% feature_list, "importance.95"]),
      permutation.error = sum(df[df$feature %in% feature_list, "permutation.error"])
    )
    df <- df[!df$feature %in% feature_list, ]
    df <- rbind(df, merged_row)
    return(df)
  }
  
}

var_rep = "BM_tot"  
algo = "ann"

variables = names(df_train)[!names(df_train) %in% var_rep]
X_train <- df_train[, variables]
y_train <- df_train[, var_rep]

X_test <- df_test[, variables]
y_test <- df_test[, var_rep]

BM_tot_select_ann <- data.frame(
  Algorithms = character(),
  Response_variables = character(),
  R2_adjusted_train = numeric(),
  R2_test = numeric(),
  RMSE = numeric(),
  MAE = numeric(),
  delet = character(),
  models = numeric()
)

del_vars = c()

nbr_iter = length(predictor_deep) - 1

X_train <- as.data.frame(lapply(X_train, as.numeric))
X_test <- as.data.frame(lapply(X_test, as.numeric))

for (i in 1:nbr_iter) {
  
  cat(paste0("***** model: ", i, "/", nbr_iter, "\n"))
  
  # Convert data to matrix and scale
  X_train_mat <- as.matrix(scale(X_train))
  X_test_mat <- as.matrix(scale(X_test))
  
  # Define the model
  model <- keras_model_sequential() %>%
    layer_dense(units = 8, activation = 'relu', input_shape = c(ncol(X_train_mat))) %>%
    # layer_dropout(rate = 0.2)  %>%
    # layer_dense(units = 8, activation = 'relu') %>%
    # layer_dropout(rate = 0.2)  %>%
    # layer_dense(units = 16, activation = 'relu') %>%
    # layer_dropout(rate = 0.2)  %>%
    # layer_dense(units = 16, activation = 'relu') %>%
    # layer_dropout(rate = 0.2)  %>%
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
  
  # Predictor and feature importance
  set.seed(1111)
  predictor <- Predictor$new(model, data = as.data.frame(X_train), y = y_train)
  imp <- FeatureImp$new(predictor, loss = loss)
  var_importance <- as.data.frame(imp$results)
  # Merge the features
  var_importance <- merge_features(var_importance, OS_l, "OS")
  # var_importance <- merge_features(var_importance, w_sol_l, "w_sol")
  # var_importance <- merge_features(var_importance, Ferti_l, "Ferti")
  var_importance <- var_importance[order(-var_importance$importance), ]
  
  # Variable to remove
  to_remove <- var_importance[which.min(var_importance$importance), "feature"]
  del_vars = c(del_vars, to_remove)
  
  # Prediction on test data
  pred. <- model %>% predict(X_test_mat)
  
  # Calculate RMSE
  rmse <- round(sqrt(mean((y_test - pred.)^2, na.rm = TRUE)), 2)
  
  # Calculate adjusted R² for training data
  pred.train =  model %>% predict(X_train_mat)
  R_adj_train <- calcule_R2(y_train,pred.train)
  n_train <- nrow(df_train)
  p_train <- ncol(df_train) - 1
  r_adj_train <- 1 - ((1 - R_adj_train) * (n_train - 1) / (n_train - p_train - 1))
  
  # Calculate R² for test data
  res <- lm(y_test ~ pred.)
  R2_test <- summary(res)$r.squared
  
  MAE <- mean(abs(pred. - y_test), na.rm = TRUE)
  
  # Round results
  rmse <- round(rmse, 2)
  r_adj_train <- round(r_adj_train, 2)
  R2_test <- round(R2_test, 2)
  MAE <- round(MAE, 2)
  
  # Output
  results_df <- data.frame(
    Algorithms = algo,
    Response_variables = var_rep,
    R2_adjusted_train = r_adj_train,
    R2_test = R2_test,
    RMSE = rmse,
    MAE = MAE,
    delet = paste0(i,"_",to_remove),
    models = paste0("mod_", i)
  )
  
  BM_tot_select_ann = rbind(BM_tot_select_ann, results_df)
  
  # Update df_train and X_train
  df_train[[to_remove]] <- NULL  # Remove the least important variable
  df_test[[to_remove]] <- NULL   # Remove the least important variable
  variables <- names(df_train)[!names(df_train) %in% var_rep]
  X_train <- df_train[, variables]
  X_test <- df_test[, variables]
  
  X_train <- as.data.frame(lapply(X_train, as.numeric))
  X_test <- as.data.frame(lapply(X_test, as.numeric))
  
  rm("results_df")
}

BM_tot_select_ann
# Write the results to an Excel file
write.xlsx(x = BM_tot_select_ann,
           file = paste0("variables_selections/ProDij/", var_rep, "_select_", algo, ".xlsx"),
           rowNames = FALSE, overwrite = TRUE)


## end variables selection for BM_tot with ann ##









## gam -------------------------------
# model, params,

df_train <- BM_tot_train[, c("BM_tot", predictor_non_r)]
df_test <- BM_tot_test[, c("BM_tot", predictor_non_r)]

var_rep <- "BM_tot"  
algo <- "gam"

variables <- names(df_train)[!names(df_train) %in% var_rep]
X <- df_train[, variables]
y <- df_train[, var_rep]

BM_tot_select_gam <- data.frame(
  Algorithms = character(),
  Response_variables = character(),
  R2_adjusted_train = numeric(),
  R2_test = numeric(),
  RMSE = numeric(),
  MAE = numeric(),
  delet = character(),
  models = numeric()
)


del_vars <- c()

model = gam(BM_tot ~ s(Long) + s(Lat) + s(SableF) + s(LimonF) +
              s(LimonG) + s(Argile) + s(C_org) + s(C.N) + s(pH_eau) + 
              OS, data = df_train)



### A FAIRE MANUELLEMENT
{ 
  # model = gam(BM_tot ~ s(Long) + s(Lat), data = df_train)
  
  # Utilisation du conteneur iml Predictor
  set.seed(1111)
  predictor <- Predictor$new(model, data = X, y = y)
  imp <- FeatureImp$new(predictor, loss = loss)
  var_importance <- as.data.frame(imp$results)
  
  # La variable à supprimer
  to_remove <- var_importance[which.min(var_importance$importance), "feature"]
  del_vars <- c(del_vars, to_remove)
  
  
  # Prédiction sur le jeu de données test
  pred <- predict(model, newdata = df_test)
  
  # Calcul du RMSE pour évaluer la qualité du modèle
  rmse <- round(sqrt(mean((df_test[[var_rep]] - pred)^2, na.rm = TRUE)), 2)
  
  # Calcul du R² ajusté pour df_train
  R_adj_train <- calcule_R2(df_train[, var_rep], predict(model, newdata = df_train))
  n_train <- nrow(df_train)
  p_train <- ncol(df_train) - 1
  r_adj_train <- 1 - ((1 - R_adj_train) * (n_train - 1) / (n_train - p_train - 1))
  
  # Calcul du R²
  res <- lrm(df_test[[var_rep]] ~ pred, x = TRUE, y = TRUE)
  res <- res$stats
  R2_test <- round(res[["R2"]], 2)
  
  MAE <- mean(abs(pred - df_test[[var_rep]]), na.rm = TRUE)
  
  # Arrondir les résultats
  rmse <- round(rmse, 2)
  r_adj_train <- round(r_adj_train, 2)
  R2_test <- round(R2_test, 2)
  MAE <- round(MAE, 2)
  
  # Output
  results_df <- data.frame(
    Algorithms = algo,
    Response_variables = var_rep,
    R2_adjusted_train = r_adj_train,
    R2_test = R2_test,
    RMSE = rmse,
    MAE = MAE,
    delet = to_remove,
    models = paste0("mod_", i)
  )
  
  BM_tot_select_gam <- rbind(BM_tot_select_gam, results_df)
  
  # Mise à jour de df_train et de X
  df_train[[to_remove]] <- NULL  # suppression de la variable la moins importante
  df_test[[to_remove]] <- NULL   # suppression de la variable la moins importante
  variables_tp <- names(df_train)[!names(df_train) %in% var_rep]
  X <- df_train[, variables_tp]
  names(X) <- names(df_test)[-1]
  
  rm("results_df")
  
  BM_tot_select_gam 
  
}

BM_tot_select_gam
write.xlsx(x = BM_tot_select_gam,
           file = paste0("variables_selections/ProDij/",var_rep,"_select_",algo,".xlsx"),
           rowNames = FALSE, overwrite = TRUE)


## end variables selection for BM_tot with gam #








# ********************* END BM_tot ************************************** #















# Richesse ---------------------------------------------------
## glm -------------------------------

# set.seed(425)
# split <- rsample::initial_split(prodij_clean_short, prop = 0.8, strata = "Richesse")
# Richesse_train <- rsample::training(split)
# Richesse_test <- rsample::testing(split)

df_train = Richesse_train[,c("Richesse",predictor_non_r )]
df_test = Richesse_test[,c("Richesse",predictor_non_r )]

var_rep = "Richesse"  
algo = "glm"


variables = names(df_train)[! names(df_train) %in% var_rep]
X <- df_train[, variables]
y <- df_train[, var_rep]


Richesse_select_glm <- data.frame(Algorithms = character(),
                                Response_variables = character(),
                                R2_adjusted_train = numeric(),
                                R2_test = numeric(),
                                RMSE = numeric(),
                                MAE = numeric(),
                                delet = character(),
                                models = numeric())

del_vars =c()
for (i in 1: (length(variables)-1) ) {
  
  cat(paste0("***** model: ",i,"/",length(variables),"\n" ))
  
  model = glm(Richesse ~ ., data = df_train)
  
  # Utilisation du conteneur iml Predictor()
  set.seed(1111)
  predictor <- Predictor$new(model, data = X, y = y) 
  imp <- FeatureImp$new(predictor, loss = loss) 
  var_importance <- as.data.frame(imp$results)
  
  
  #  la variable à supprimer
  to_remove <- var_importance[which.min(var_importance$importance), "feature"]
  del_vars = c(del_vars, paste0(i,"_", to_remove))
  
  # Prediction sur le jeu de donnée test
  pred.<- predict(model,newdata=df_test)
  
  # Calcul du RMSE pour évaluer la qualite du modele
  rmse <- round (sqrt(mean((df_test[[var_rep]] - pred.)^2,na.rm=TRUE)),2)
  
  # Calcul du R² ajusté pour df_train
  R_adj_train <- calcule_R2(df_train[,var_rep],  predict(model, newdata=df_train))
  n_train <- nrow(df_train)
  p_train <- ncol(df_train) - 1
  r_adj_train <- 1 - ((1 - R_adj_train) * (n_train - 1) / (n_train - p_train - 1))
  
  # Calcul du R² 
  res <- rms::lrm(df_test[[var_rep]]  ~ pred., x= TRUE, y = TRUE)
  res = res$stats
  R2_test = round (res[["R2"]],2)
  
  MAE <- mean(abs(pred. - df_test[[var_rep]]),na.rm=TRUE)
  
  # Round results
  rmse <- round(rmse, 2)
  r_adj_train <- round(r_adj_train, 2)
  R2_test <- round(R2_test, 2)
  MAE <- round(MAE, 2)
  
  
  # output
  results_df <- data.frame(Algorithms = algo,
                           Response_variables = var_rep,
                           R2_adjusted_train = r_adj_train,
                           R2_test = R2_test,
                           RMSE = rmse,
                           MAE = MAE,
                           delet = paste0(i,"_",to_remove),
                           models = paste0("mod_",i))
  
  
  Richesse_select_glm = rbind(Richesse_select_glm,results_df)
  
  
  # mis a jours de df_train, et de X
  df_train[[to_remove]] = NULL # suppression de la variables la moins importante
  df_test[[to_remove]] = NULL # suppression de la variables la moins importante
  variables_tp = names(df_train)[! names(df_train) %in% var_rep]
  X <- as.data.frame(df_train[, variables_tp])
  names(X) = names(df_test)[-1]
  
  rm("results_df")
}

Richesse_select_glm 

write.xlsx(x =Richesse_select_glm,
           file = paste0("variables_selections/ProDij/",var_rep,"_select_",algo,".xlsx"),
           rowNames = FALSE,overwrite = TRUE)


rm("predictor","imp", "var_importance","df_info","var_rep","selected_vars")

## end variables selection for Richesse with glm ##





## rf -------------------------------

df_train = Richesse_train[, c("Richesse", predictor_deep)]
df_test = Richesse_test[, c("Richesse", predictor_deep)]

var_rep = "Richesse"  
algo = "RF"

variables = names(df_train)[!names(df_train) %in% var_rep]
X <- df_train[, variables]
y <- df_train[, var_rep]

Richesse_select_rf <- data.frame(
  Algorithms = character(),
  Response_variables = character(),
  R2_adjusted_train = numeric(),
  R2_test = numeric(),
  RMSE = numeric(),
  MAE = numeric(),
  delet = character(),
  models = numeric()
)

del_vars = c()
for (i in 1:(length(variables) - 1)) {
  cat(paste0("***** model: ", i, "/", length(variables), "\n"))
  
  model = randomForest(Richesse ~ ., data = df_train)
  
  # Utilisation du conteneur iml Predictor()
  set.seed(1111)
  predictor <- Predictor$new(model, data = X, y = y)
  imp <- FeatureImp$new(predictor, loss = loss)
  var_importance <- as.data.frame(imp$results)
  
  # La variable à supprimer
  to_remove <- var_importance[which.min(var_importance$importance), "feature"]
  del_vars = c(del_vars, paste0(i, "_", to_remove))
  
  # Prédiction sur le jeu de données test
  pred. <- predict(model, newdata = df_test)
  
  # Calcul du RMSE pour évaluer la qualité du modèle
  rmse <- round(sqrt(mean((df_test[[var_rep]] - pred.)^2, na.rm = TRUE)), 2)
  
  # Calcul du R² ajusté pour df_train
  R_adj_train <- calcule_R2(df_train[, var_rep], predict(model, newdata = df_train))
  n_train <- nrow(df_train)
  p_train <- ncol(df_train) - 1
  r_adj_train <- 1 - ((1 - R_adj_train) * (n_train - 1) / (n_train - p_train - 1))
  
  # Calcul du R²
  res <- lrm(df_test[[var_rep]] ~ pred., x = TRUE, y = TRUE)
  res = res$stats
  R2_test = round(res[["R2"]], 2)
  
  MAE <- mean(abs(pred. - df_test[[var_rep]]), na.rm = TRUE)
  
  # Round results
  rmse <- round(rmse, 2)
  r_adj_train <- round(r_adj_train, 2)
  R2_test <- round(R2_test, 2)
  MAE <- round(MAE, 2)
  
  # Output
  results_df <- data.frame(
    Algorithms = algo,
    Response_variables = var_rep,
    R2_adjusted_train = r_adj_train,
    R2_test = R2_test,
    RMSE = rmse,
    MAE = MAE,
    delet = paste0(i,"_",to_remove),
    models = paste0("mod_", i)
  )
  
  Richesse_select_rf = rbind(Richesse_select_rf, results_df)
  
  # Mise à jour de df_train et de X
  df_train[[to_remove]] = NULL  # suppression de la variable la moins importante
  df_test[[to_remove]] = NULL   # suppression de la variable la moins importante
  variables_tp = names(df_train)[!names(df_train) %in% var_rep]
  X <- df_train[, variables_tp]
  names(X) = names(df_test)[-1]
  
  rm("results_df")
}

Richesse_select_rf 

write.xlsx(x = Richesse_select_rf,
           file = paste0("variables_selections/ProDij/",var_rep,"_select_",algo,".xlsx"),
           rowNames = FALSE, overwrite = TRUE)


## end variables selection for Richesse with rf ##



## gbm -------------------------------

df_train = Richesse_train[, c("Richesse", predictor_deep)]
df_test = Richesse_test[, c("Richesse", predictor_deep)]

var_rep = "Richesse"  
algo = "gbm"

variables = names(df_train)[!names(df_train) %in% var_rep]
X <- df_train[, variables]
y <- df_train[, var_rep]

Richesse_select_gbm <- data.frame(
  Algorithms = character(),
  Response_variables = character(),
  R2_adjusted_train = numeric(),
  R2_test = numeric(),
  RMSE = numeric(),
  MAE = numeric(),
  delet = character(),
  models = numeric()
)

del_vars = c()
for (i in 1:(length(variables) - 1)) {
  cat(paste0("***** model: ", i, "/", length(variables), "\n"))
  
  model = gbm(Richesse ~ ., data = df_train)
  
  # Utilisation du conteneur iml Predictor()
  set.seed(1111)
  predictor <- Predictor$new(model, data = X, y = y)
  imp <- FeatureImp$new(predictor, loss = loss)
  var_importance <- as.data.frame(imp$results)
  
  # La variable à supprimer
  to_remove <- var_importance[which.min(var_importance$importance), "feature"]
  del_vars = c(del_vars, paste0(i, "_", to_remove))
  
  # Prédiction sur le jeu de données test
  pred. <- predict(model, newdata = df_test, n.trees = model$n.trees)
  
  # Calcul du RMSE pour évaluer la qualité du modèle
  rmse <- round(sqrt(mean((df_test[[var_rep]] - pred.)^2, na.rm = TRUE)), 2)
  
  # Calcul du R² ajusté pour df_train
  R_adj_train <- calcule_R2(df_train[, var_rep], predict(model, newdata = df_train, n.trees = model$n.trees))
  n_train <- nrow(df_train)
  p_train <- ncol(df_train) - 1
  r_adj_train <- 1 - ((1 - R_adj_train) * (n_train - 1) / (n_train - p_train - 1))
  
  # Calcul du R²
  res <- lrm(df_test[[var_rep]] ~ pred., x = TRUE, y = TRUE)
  res = res$stats
  R2_test = round(res[["R2"]], 2)
  
  MAE <- mean(abs(pred. - df_test[[var_rep]]), na.rm = TRUE)
  
  # Round results
  rmse <- round(rmse, 2)
  r_adj_train <- round(r_adj_train, 2)
  R2_test <- round(R2_test, 2)
  MAE <- round(MAE, 2)
  
  # Output
  results_df <- data.frame(
    Algorithms = algo,
    Response_variables = var_rep,
    R2_adjusted_train = r_adj_train,
    R2_test = R2_test,
    RMSE = rmse,
    MAE = MAE,
    delet = paste0(i,"_",to_remove),
    models = paste0("mod_", i)
  )
  
  Richesse_select_gbm = rbind(Richesse_select_gbm, results_df)
  
  # Mise à jour de df_train et de X
  df_train[[to_remove]] = NULL  # suppression de la variable la moins importante
  df_test[[to_remove]] = NULL   # suppression de la variable la moins importante
  variables_tp = names(df_train)[!names(df_train) %in% var_rep]
  X <- df_train[, variables_tp]
  names(X) = names(df_test)[-1]
  
  rm("results_df")
}

Richesse_select_gbm 

write.xlsx(x = Richesse_select_gbm,
           file = paste0("variables_selections/ProDij/",var_rep,"_select_",algo,".xlsx"),
           rowNames = FALSE, overwrite = TRUE)

## end variables selection for Richesse with gbm ##








## ann -------------------------------


df_train = Richesse_train[, c("Richesse", predictor_deep)]
df_test = Richesse_test[, c("Richesse", predictor_deep)]

# converssion des factors et function de fusion
{ 
  dummy_vars <- model.matrix(~ OS - 1, data = df_train)
  df_train <- cbind(df_train, dummy_vars)
  # df_train <- df_train[, -which(names(df_train) == "OS")]
  # dummy_vars <- model.matrix(~ w_sol_ - 1, data = df_train)
  # df_train <- cbind(df_train, dummy_vars)
  # df_train <- df_train[, -which(names(df_train) == "w_sol_")]
  # dummy_vars <- model.matrix(~ Ferti_ - 1, data = df_train)
  # df_train <- cbind(df_train, dummy_vars)
  # df_train <- df_train[, -which(names(df_train) == "Ferti_")]
  
  
  dummy_vars <- model.matrix(~ OS - 1, data = df_test)
  df_test <- cbind(df_test, dummy_vars)
  # df_test <- df_test[, -which(names(df_test) == "OS")]
  # dummy_vars <- model.matrix(~ w_sol_ - 1, data = df_test)
  # df_test <- cbind(df_test, dummy_vars)
  # df_test <- df_test[, -which(names(df_test) == "w_sol_")]
  # dummy_vars <- model.matrix(~ Ferti_ - 1, data = df_test)
  # df_test <- cbind(df_test, dummy_vars)
  # df_test <- df_test[, -which(names(df_test) == "Ferti_")]
  
  
  df_train = drop_na(df_train)
  df_train = droplevels(df_train)
  
  # les feature  a fusionnnée en faisant la somme
  OS_l = c("OSOS_111", "OSOS_210", "OSOS_214", "OSOS_218")
  # w_sol_l = c("w_sol_non", "w_sol_oui")
  # Ferti_l = c("Ferti_non", "Ferti_oui")
  
  
  # Function to merge features
  merge_features <- function(df, feature_list, new_feature_name) {
    merged_row <- data.frame(
      feature = new_feature_name,
      importance.05 = sum(df[df$feature %in% feature_list, "importance.05"]),
      importance = sum(df[df$feature %in% feature_list, "importance"]),
      importance.95 = sum(df[df$feature %in% feature_list, "importance.95"]),
      permutation.error = sum(df[df$feature %in% feature_list, "permutation.error"])
    )
    df <- df[!df$feature %in% feature_list, ]
    df <- rbind(df, merged_row)
    return(df)
  }
  
}

var_rep = "Richesse"  
algo = "ann"

variables = names(df_train)[!names(df_train) %in% var_rep]
X_train <- df_train[, variables]
y_train <- df_train[, var_rep]

X_test <- df_test[, variables]
y_test <- df_test[, var_rep]

Richesse_select_ann <- data.frame(
  Algorithms = character(),
  Response_variables = character(),
  R2_adjusted_train = numeric(),
  R2_test = numeric(),
  RMSE = numeric(),
  MAE = numeric(),
  delet = character(),
  models = numeric()
)

del_vars = c()

nbr_iter = length(predictor_deep) - 1

X_train <- as.data.frame(lapply(X_train, as.numeric))
X_test <- as.data.frame(lapply(X_test, as.numeric))

for (i in 1:nbr_iter) {
  
  cat(paste0("***** model: ", i, "/", nbr_iter, "\n"))
  
  # Convert data to matrix and scale
  X_train_mat <- as.matrix(scale(X_train))
  X_test_mat <- as.matrix(scale(X_test))
  
  # Define the model
  model <- keras_model_sequential() %>%
    layer_dense(units = 8, activation = 'relu', input_shape = c(ncol(X_train_mat))) %>%
    # layer_dropout(rate = 0.2)  %>%
    # layer_dense(units = 8, activation = 'relu') %>%
    # layer_dropout(rate = 0.2)  %>%
    # layer_dense(units = 16, activation = 'relu') %>%
    # layer_dropout(rate = 0.2)  %>%
    # layer_dense(units = 16, activation = 'relu') %>%
    # layer_dropout(rate = 0.2)  %>%
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
  
  # Predictor and feature importance
  set.seed(1111)
  predictor <- Predictor$new(model, data = as.data.frame(X_train), y = y_train)
  imp <- FeatureImp$new(predictor, loss = loss)
  var_importance <- as.data.frame(imp$results)
  # Merge the features
  var_importance <- merge_features(var_importance, OS_l, "OS")
  # var_importance <- merge_features(var_importance, w_sol_l, "w_sol")
  # var_importance <- merge_features(var_importance, Ferti_l, "Ferti")
  var_importance <- var_importance[order(-var_importance$importance), ]
  
  # Variable to remove
  to_remove <- var_importance[which.min(var_importance$importance), "feature"]
  del_vars = c(del_vars, to_remove)
  
  # Prediction on test data
  pred. <- model %>% predict(X_test_mat)
  
  # Calculate RMSE
  rmse <- round(sqrt(mean((y_test - pred.)^2, na.rm = TRUE)), 2)
  
  # Calculate adjusted R² for training data
  pred.train =  model %>% predict(X_train_mat)
  R_adj_train <- calcule_R2(y_train,pred.train)
  n_train <- nrow(df_train)
  p_train <- ncol(df_train) - 1
  r_adj_train <- 1 - ((1 - R_adj_train) * (n_train - 1) / (n_train - p_train - 1))
  
  # Calculate R² for test data
  res <- lm(y_test ~ pred.)
  R2_test <- summary(res)$r.squared
  
  MAE <- mean(abs(pred. - y_test), na.rm = TRUE)
  
  # Round results
  rmse <- round(rmse, 2)
  r_adj_train <- round(r_adj_train, 2)
  R2_test <- round(R2_test, 2)
  MAE <- round(MAE, 2)
  
  # Output
  results_df <- data.frame(
    Algorithms = algo,
    Response_variables = var_rep,
    R2_adjusted_train = r_adj_train,
    R2_test = R2_test,
    RMSE = rmse,
    MAE = MAE,
    delet = paste0(i,"_",to_remove),
    models = paste0("mod_", i)
  )
  
  Richesse_select_ann = rbind(Richesse_select_ann, results_df)
  
  # Update df_train and X_train
  df_train[[to_remove]] <- NULL  # Remove the least important variable
  df_test[[to_remove]] <- NULL   # Remove the least important variable
  variables <- names(df_train)[!names(df_train) %in% var_rep]
  X_train <- df_train[, variables]
  X_test <- df_test[, variables]
  
  X_train <- as.data.frame(lapply(X_train, as.numeric))
  X_test <- as.data.frame(lapply(X_test, as.numeric))
  
  rm("results_df")
}

Richesse_select_ann
# Write the results to an Excel file
write.xlsx(x = Richesse_select_ann,
           file = paste0("variables_selections/ProDij/", var_rep, "_select_", algo, ".xlsx"),
           rowNames = FALSE, overwrite = TRUE)


## end variables selection for Richesse with ann ##









## gam -------------------------------
# model, params,

df_train <- Richesse_train[, c("Richesse", predictor_non_r)]
df_test <- Richesse_test[, c("Richesse", predictor_non_r)]

var_rep <- "Richesse"  
algo <- "gam"

variables <- names(df_train)[!names(df_train) %in% var_rep]
X <- df_train[, variables]
y <- df_train[, var_rep]

Richesse_select_gam <- data.frame(
  Algorithms = character(),
  Response_variables = character(),
  R2_adjusted_train = numeric(),
  R2_test = numeric(),
  RMSE = numeric(),
  MAE = numeric(),
  delet = character(),
  models = numeric()
)

del_vars <- c()

model = gam(Richesse ~ s(Long) + s(Lat) + s(SableF) + s(LimonF) +
              s(LimonG) + s(Argile) + s(C_org) + s(C.N) + s(pH_eau) + 
              OS , data = df_train)



### A FAIRE MANUELLEMENT
{ 
  # model = gam(Richesse ~ s(Long)      +   OS , data = df_train)
  
  # Utilisation du conteneur iml Predictor
  set.seed(1111)
  predictor <- Predictor$new(model, data = X, y = y)
  imp <- FeatureImp$new(predictor, loss = loss)
  var_importance <- as.data.frame(imp$results)
  
  # La variable à supprimer
  to_remove <- var_importance[which.min(var_importance$importance), "feature"]
  del_vars <- c(del_vars, to_remove)
  
  
  # Prédiction sur le jeu de données test
  pred <- predict(model, newdata = df_test)
  
  # Calcul du RMSE pour évaluer la qualité du modèle
  rmse <- round(sqrt(mean((df_test[[var_rep]] - pred)^2, na.rm = TRUE)), 2)
  
  # Calcul du R² ajusté pour df_train
  R_adj_train <- calcule_R2(df_train[, var_rep], predict(model, newdata = df_train))
  n_train <- nrow(df_train)
  p_train <- ncol(df_train) - 1
  r_adj_train <- 1 - ((1 - R_adj_train) * (n_train - 1) / (n_train - p_train - 1))
  
  # Calcul du R²
  res <- lrm(df_test[[var_rep]] ~ pred, x = TRUE, y = TRUE)
  res <- res$stats
  R2_test <- round(res[["R2"]], 2)
  
  MAE <- mean(abs(pred - df_test[[var_rep]]), na.rm = TRUE)
  
  # Arrondir les résultats
  rmse <- round(rmse, 2)
  r_adj_train <- round(r_adj_train, 2)
  R2_test <- round(R2_test, 2)
  MAE <- round(MAE, 2)
  
  # Output
  results_df <- data.frame(
    Algorithms = algo,
    Response_variables = var_rep,
    R2_adjusted_train = r_adj_train,
    R2_test = R2_test,
    RMSE = rmse,
    MAE = MAE,
    delet = to_remove,
    models = paste0("mod_", i)
  )
  
  Richesse_select_gam <- rbind(Richesse_select_gam, results_df)
  
  # Mise à jour de df_train et de X
  df_train[[to_remove]] <- NULL  # suppression de la variable la moins importante
  df_test[[to_remove]] <- NULL   # suppression de la variable la moins importante
  variables_tp <- names(df_train)[!names(df_train) %in% var_rep]
  X <- df_train[, variables_tp]
  names(X) <- names(df_test)[-1]
  
  rm("results_df")
  
  Richesse_select_gam 
  
}

Richesse_select_gam
write.xlsx(x = Richesse_select_gam,
           file = paste0("variables_selections/ProDij/",var_rep,"_select_",algo,".xlsx"),
           rowNames = FALSE, overwrite = TRUE)


## end variables selection for Richesse with gam #















# ********************* END Richesse ************************************** #








