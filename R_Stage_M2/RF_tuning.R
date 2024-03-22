

# Liste des packages nécessaire
packages_to_check <- c("keras", "mlbench", "dplyr", "magrittr", "neuralnet", "ggplot2", "reshape2", "tfruns", "randomForest", "tidyverse")

for(i in packages_to_check){ cat("library(",i,") \n")}

install_missing_packages <- function(packages) {
  for (pkg in packages) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      message(paste("Le package", pkg, "n'est pas installé. Installation en cours..."))
      install.packages(pkg, dependencies = TRUE)
    } else {
      message(paste("Le package", pkg, "est déjà installé."))
    }
  }
}

install_missing_packages(packages_to_check)



# Grille de hyperparametisation
RF_df_grid <- expand.grid(ntree = c(100,300,500,700,900,1000,1300,1500,1700,2000),
                       mtry = c(2, 4, 6, 8, 10, 12, 14, 16, 18, 20, 22, 24),
                       nodesize = c(10 , 20,  30,  40,  50,  60,  70,  80))


# nrow(RF_df_grid)

# RF_df_grid <- expand.grid(ntree = c(100,300),
#                           mtry = c(2,4),
#                           nodesize = c(10))
# 
# 
# nrow(RF_df_grid)


ForetAlea <- function(var_rep, df_app, df_valid,mtry,ntree,maxnodes){
  
  
  col_posi <- which(names(df_app) == var_rep)
  ForeVDT <- randomForest::randomForest(df_app[-col_posi], df_app[[col_posi]],  mtry=mtry,ntree=ntree,maxnodes=maxnodes)
  
  # Prediction sur le jeu de validation
  col_posi <- which(names(df_valid) == var_rep)
  pred.RF<-predict(ForeVDT,newdata=df_valid[,-col_posi])
  
  # Calcul du RMSE pour évaluer la qualité du modele
  rmse <- sqrt(mean((df_valid[,col_posi] - pred.RF)^2))
  
  # calcule du R-squared
  r_adj = cor(df_valid[,col_posi],pred.RF)^2
  
  # calcule MAE
  MAE <- mean(abs(pred.RF - df_valid[,col_posi]))
  
  results <- list(RMSE = rmse, R_squared= r_adj,MAE = MAE, model=ForeVDT, predit= pred.RF)
  
  
  return(results)
}

# AB_tot ------------------------------------------------------------------

var_rep = "AB_tot"
df_app = read.csv2("datas/AB_tot_train.csv")
df_valid = read.csv2("datas/AB_tot_test.csv")

# str(df_app)
# str(df_valid)

ind_var_rep <- which(names(df_app) == var_rep)
trainingtarget <- df_app[, ind_var_rep]
trainingtarget = sqrt(trainingtarget)
training = df_app[,-ind_var_rep]
training[,1:19] = scale(training [,1:19])
df_app = cbind(trainingtarget,training)
colnames(df_app)[ind_var_rep] <- var_rep
df_app = as.data.frame(df_app)

ind_var_rep <- which(names(df_valid) == var_rep)
testtarget <- df_valid[, ind_var_rep]
testtarget <- sqrt(testtarget)
test = df_valid[,-ind_var_rep]
test[,1:19] = scale(test[,1:19])
df_valid = cbind(testtarget,test)
colnames(df_valid)[ind_var_rep] <- var_rep
df_valid = as.data.frame(df_valid)

# str(df_app)
# str(df_valid)



df_app = as.data.frame(df_app)
df_valid = as.data.frame(df_valid)



res_test = ForetAlea(var_rep, df_app, df_valid, mtry = 2,ntree = 100,maxnodes = 10)

res_test$RMSE
res_test$R_squared





# temps1 = Sys.time()
# # le df result tot
# RF_tune_AB_tot <- data.frame(mtry = numeric(),
#                              ntree = numeric(),
#                              maxnodes = numeric(),
#                              rmse = numeric(),
#                              r_squared = numeric(),
#                              mae = numeric())
# 
# # Utilisation lapply pour appliquer ForetAlea à chaque combinaison
# results_list <- lapply(1:nrow(RF_df_grid), function(i) {
#   mtry <- RF_df_grid[i, "mtry"]
#   ntree <- RF_df_grid[i, "ntree"]
#   maxnodes <- RF_df_grid[i, "nodesize"]
#   
#   res <- ForetAlea(var_rep, df_app, df_valid, 
#                    mtry = mtry, ntree = ntree, maxnodes = maxnodes)
#   
#   res_df = data.frame(mtry = mtry,
#              ntree = ntree,
#              maxnodes = maxnodes,
#              rmse = res$RMSE,
#              r_squared = res$R_squared,
#              mae = res$MAE)
#   cat(i,"/",nrow(RF_df_grid))
#   
#   res_df
# })
# 
# # les résultats en un seul data frame
# RF_tune_AB_tot <- do.call(rbind, results_list)
# 
# temps2 = Sys.time()  
# duree = difftime(temps2,temps1)
# duree
# 
# write.csv2(x =RF_tune_AB_tot,file = paste0("results_tuning/", var_rep, "_RF_tuning.csv"), row.names = FALSE)
# 
# RF_tune_AB_tot %>% arrange(mae)






# BM_tot ------------------------------------------------------------------

var_rep = "BM_tot"
df_app = read.csv2("datas/BM_tot_train.csv")
df_valid = read.csv2("datas/BM_tot_test.csv")

# str(df_app)
# str(df_valid)

ind_var_rep <- which(names(df_app) == var_rep)
trainingtarget <- df_app[, ind_var_rep]
trainingtarget = sqrt(trainingtarget)
training = df_app[,-ind_var_rep]
training[,1:19] = scale(training [,1:19])
df_app = cbind(trainingtarget,training)
colnames(df_app)[ind_var_rep] <- var_rep
df_app = as.data.frame(df_app)

ind_var_rep <- which(names(df_valid) == var_rep)
testtarget <- df_valid[, ind_var_rep]
testtarget <- sqrt(testtarget)
test = df_valid[,-ind_var_rep]
test[,1:19] = scale(test[,1:19])
df_valid = cbind(testtarget,test)
colnames(df_valid)[ind_var_rep] <- var_rep
df_valid = as.data.frame(df_valid)

# str(df_app)
# str(df_valid)



df_app = as.data.frame(df_app)
df_valid = as.data.frame(df_valid)



res_test = ForetAlea(var_rep, df_app, df_valid, mtry = 2,ntree = 100,maxnodes = 10)

res_test$RMSE
res_test$R_squared





# temps1 = Sys.time()
# # le df result tot
# RF_tune_BM_tot <- data.frame(mtry = numeric(),
#                              ntree = numeric(),
#                              maxnodes = numeric(),
#                              rmse = numeric(),
#                              r_squared = numeric(),
#                              mae = numeric())
# 
# # Utilisation lapply pour appliquer ForetAlea à chaque combinaison
# results_list <- lapply(1:nrow(RF_df_grid), function(i) {
#   mtry <- RF_df_grid[i, "mtry"]
#   ntree <- RF_df_grid[i, "ntree"]
#   maxnodes <- RF_df_grid[i, "nodesize"]
#   
#   res <- ForetAlea(var_rep, df_app, df_valid, 
#                    mtry = mtry, ntree = ntree, maxnodes = maxnodes)
#   
#   res_df = data.frame(mtry = mtry,
#                       ntree = ntree,
#                       maxnodes = maxnodes,
#                       rmse = res$RMSE,
#                       r_squared = res$R_squared,
#                       mae = res$MAE)
#   cat(i,"/",nrow(RF_df_grid))
#   
#   res_df
# })
# 
# # les résultats en un seul data frame
# RF_tune_BM_tot <- do.call(rbind, results_list)
# 
# temps2 = Sys.time()  
# duree = difftime(temps2,temps1)
# duree
# 
# write.csv2(x =RF_tune_BM_tot,file = paste0("results_tuning/", var_rep, "_RF_tuning.csv"), row.names = FALSE)
# 
# RF_tune_BM_tot %>% arrange(mae)




# Richesse_tot ------------------------------------------------------------------

var_rep = "Richesse_tot"
df_app = read.csv2("datas/Richesse_tot_train.csv")
df_valid = read.csv2("datas/Richesse_tot_test.csv")

# str(df_app)
# str(df_valid)

ind_var_rep <- which(names(df_app) == var_rep)
trainingtarget <- df_app[, ind_var_rep]
#trainingtarget = sqrt(trainingtarget)
training = df_app[,-ind_var_rep]
training[,1:19] = scale(training [,1:19])
df_app = cbind(trainingtarget,training)
colnames(df_app)[ind_var_rep] <- var_rep
df_app = as.data.frame(df_app)

ind_var_rep <- which(names(df_valid) == var_rep)
testtarget <- df_valid[, ind_var_rep]
#testtarget <- sqrt(testtarget)
test = df_valid[,-ind_var_rep]
test[,1:19] = scale(test[,1:19])
df_valid = cbind(testtarget,test)
colnames(df_valid)[ind_var_rep] <- var_rep
df_valid = as.data.frame(df_valid)

# str(df_app)
# str(df_valid)



df_app = as.data.frame(df_app)
df_valid = as.data.frame(df_valid)



res_test = ForetAlea(var_rep, df_app, df_valid, mtry = 2,ntree = 100,maxnodes = 10)

res_test$RMSE
res_test$R_squared





# temps1 = Sys.time()
# # le df result tot
# RF_tune_Richesse_tot <- data.frame(mtry = numeric(),
#                              ntree = numeric(),
#                              maxnodes = numeric(),
#                              rmse = numeric(),
#                              r_squared = numeric(),
#                              mae = numeric())
# 
# # Utilisation lapply pour appliquer ForetAlea à chaque combinaison
# results_list <- lapply(1:nrow(RF_df_grid), function(i) {
#   mtry <- RF_df_grid[i, "mtry"]
#   ntree <- RF_df_grid[i, "ntree"]
#   maxnodes <- RF_df_grid[i, "nodesize"]
#   
#   res <- ForetAlea(var_rep, df_app, df_valid, 
#                    mtry = mtry, ntree = ntree, maxnodes = maxnodes)
#   
#   res_df = data.frame(mtry = mtry,
#                       ntree = ntree,
#                       maxnodes = maxnodes,
#                       rmse = res$RMSE,
#                       r_squared = res$R_squared,
#                       mae = res$MAE)
#   cat(i,"/",nrow(RF_df_grid))
#   
#   res_df
# })
# 
# # les résultats en un seul data frame
# RF_tune_Richesse_tot <- do.call(rbind, results_list)
# 
# temps2 = Sys.time()  
# duree = difftime(temps2,temps1)
# duree
# 
# write.csv2(x =RF_tune_Richesse_tot,file = paste0("results_tuning/", var_rep, "_RF_tuning.csv"), row.names = FALSE)
# 
# RF_tune_Richesse_tot %>% arrange(mae)


# Anciens tuning RF -------------------------------------------------------

# # # Define the control
# trControl <- trainControl(method = "cv",
#                           number = 5,
#                           search = "grid")
# 
# # Étape 1) default model
# set.seed(1234)
# # Run the model
# rf_default <- train(AB_tot~.,
#                     data = AB_tot_train,
#                     method = "rf",
#                     metric = "RMSE",
#                     trControl = trControl)
# # Print the results
# print(rf_default)
# 
# 
# 
# # Étape 2) Recherchez le meilleur essai
# set.seed(1234)
# tuneGrid <- expand.grid(.mtry = c(2: 10))
# rf_mtry <- train(AB_tot~.,
#                  data = AB_tot_train,
#                  method = "rf",
#                  metric = "RMSE",
#                  tuneGrid = tuneGrid,
#                  trControl = trControl,
#                  importance = TRUE,
#                  nodesize = 14,
#                  ntree = 300)
# print(rf_mtry)
# 
# ## RMSE was used to select the optimal model using  the largest value.
# ## The final value used for the model was mtry = XX
# rf_mtry$bestTune$mtry
# min(rf_mtry$results$RMSE)
# best_mtry_AB_tot <- rf_mtry$bestTune$mtry
# 
# 
# # Étape 3) Recherchez les meilleurs maxnodes
# 
# store_maxnode <- list()
# tuneGrid <- expand.grid(.mtry = best_mtry_AB_tot)
# for (maxnodes in c(50: 60)) {
#   set.seed(1234)
#   rf_maxnode <- train(AB_tot~.,
#                       data = AB_tot_train,
#                       method = "rf",
#                       metric = "RMSE",
#                       tuneGrid = tuneGrid,
#                       trControl = trControl,
#                       importance = TRUE,
#                       nodesize = 14,
#                       maxnodes = maxnodes,
#                       ntree = 300)
#   current_iteration <- toString(maxnodes)
#   store_maxnode[[current_iteration]] <- rf_maxnode
# }
# results_mtry <- resamples(store_maxnode)
# summary(results_mtry)
# best_maxnodes_AB_tot = 60
# 
# # Étape 4) Recherchez les meilleurs ntrees
# store_maxtrees <- list()
# for (ntree in c(250, 300, 350, 400, 450, 500, 550, 600, 800, 1000, 2000)) {
#   set.seed(5678)
#   rf_maxtrees <- train(AB_tot~.,
#                        data = AB_tot_train,
#                        method = "rf",
#                        metric = "RMSE",
#                        tuneGrid = tuneGrid,
#                        trControl = trControl,
#                        importance = TRUE,
#                        nodesize = 14,
#                        maxnodes = 60,
#                        ntree = ntree)
#   key <- toString(ntree)
#   store_maxtrees[[key]] <- rf_maxtrees
# }
# results_tree <- resamples(store_maxtrees)
# summary(results_tree)
# best_ntree_AB_tot = 2000
# 
# 
# AB_tot_fit_rf <- randomForest(AB_tot~.,
#                               data=AB_tot_train,
#                               method = "rf",
#                               metric = "RMSE",
#                               tuneGrid = tuneGrid,
#                               trControl = trControl,
#                               importance = TRUE,
#                               nodesize = 14,
#                               ntree = best_ntree_AB_tot,
#                               maxnodes = best_maxnodes_AB_tot)
# 
# 
# # tune model
# df_AB_tot=df_mod
# names(df_AB_tot)
# df_AB_tot = df_AB_tot[,c(6,10:32)]
# model_tuned <- tuneRF(
#   x=df_AB_tot[,-1],
#   y=df_AB_tot$AB_tot,
#   ntreeTry=500,
#   mtryStart=10,
#   stepFactor=1.5,
#   improve=0.01, doBest =TRUE,
#   trace=FALSE #don't show real-time progress
# )
# plot(model_tuned)
  