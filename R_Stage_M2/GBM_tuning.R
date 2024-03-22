

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
GBM_df_grid <- expand.grid(n.trees = c(1000,1500,1700,2000,3000),
                       shrinkage = c(0.01, 0.02, 0.05, 0.001, 0.002, 0.005),
                       interaction.depth = c(3,  5,  6,  8, 10),
                       n.minobsinnode = c(2 , 5,  10,  30,  50,  70))
nrow(GBM_df_grid)


# GBM_df_grid <- expand.grid(n.trees = c(1000),
#                            shrinkage = c(0.01, 0.02),
#                            interaction.depth = c(  5),
#                            n.minobsinnode = c(2 ,  10))
# nrow(GBM_df_grid)



GBM <- function(var_rep, df_app, df_valid,distribution = 'gaussian',n.trees ,shrinkage,
                interaction.depth,n.minobsinnode){
  
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
  rmse <- round (sqrt(mean((df_valid[,var_rep] - prev.GBM)^2)),2)
  
  
  # calcule du R-squared
  r_adj = cor(df_valid[,col_posi],prev.GBM)^2
  
  # calcule MAE
  MAE <- mean(abs(prev.GBM - df_valid[,col_posi])) 
  
  results <- list(RMSE = rmse, R_squared= r_adj,MAE = MAE, model = Gradboost, predit = prev.GBM)
  
  
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



res_test = GBM(var_rep, df_app, df_valid,distribution = 'gaussian',n.trees=100 ,
               shrinkage=0.001,interaction.depth = 6,n.minobsinnode = 4)

res_test$RMSE
res_test$R_squared
res_test$MAE
res_test$model





# temps1 = Sys.time()
# 
# # Initialisation du data frame en dehors de la boucle
# GBM_tune_AB_tot <- data.frame(
#   shrinkage = numeric(),
#   n.trees = numeric(),
#   interaction.depth = numeric(),
#   n.minobsinnode = numeric(),
#   rmse = numeric(),
#   r_squared = numeric(),
#   mae = numeric()
# )
# 
# # Utilisation de lapply pour appliquer GBM à chaque combinaison
# results_list <- lapply(1:nrow(GBM_df_grid), function(i) {
#   shrinkage <- GBM_df_grid[i, "shrinkage"]
#   n.trees <- GBM_df_grid[i, "n.trees"]
#   interaction.depth <- GBM_df_grid[i, "interaction.depth"]
#   n.minobsinnode <- GBM_df_grid[i, "n.minobsinnode"]
#   
#   
#   res <- GBM(var_rep, df_app, df_valid,n.trees=n.trees,
#              shrinkage = shrinkage,interaction.depth = interaction.depth,
#              n.minobsinnode=n.minobsinnode)
#   
#   res_df <- data.frame(
#     shrinkage = shrinkage,
#     n.trees = n.trees,
#     interaction.depth = interaction.depth,
#     n.minobsinnode = n.minobsinnode,
#     rmse = res$RMSE,
#     r_squared = res$R_squared,
#     mae = res$MAE
#   )
#   cat(i,"/",nrow(GBM_df_grid))
#   
#   res_df
# })
# 
# # Fusion des résultats en un seul data frame
# GBM_tune_AB_tot <- do.call(rbind, results_list)
# 
# temps2 = Sys.time()  
# duree = difftime(temps2,temps1)
# duree
# 
# write.csv2(x =GBM_tune_AB_tot,file = paste0("results_tuning/", var_rep, "_GBM_tuning.csv"), row.names = FALSE)
# 
# GBM_tune_AB_tot %>% arrange(mae)







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



res_test = GBM(var_rep, df_app, df_valid,distribution = 'gaussian',n.trees=100 ,
               shrinkage=0.001,interaction.depth = 6,n.minobsinnode = 4)

res_test$RMSE
res_test$R_squared
res_test$MAE
res_test$model





# temps1 = Sys.time()
# 
# # Initialisation du data frame en dehors de la boucle
# GBM_tune_BM_tot <- data.frame(
#   shrinkage = numeric(),
#   n.trees = numeric(),
#   interaction.depth = numeric(),
#   n.minobsinnode = numeric(),
#   rmse = numeric(),
#   r_squared = numeric(),
#   mae = numeric()
# )
# 
# # Utilisation de lapply pour appliquer GBM à chaque combinaison
# results_list <- lapply(1:nrow(GBM_df_grid), function(i) {
#   shrinkage <- GBM_df_grid[i, "shrinkage"]
#   n.trees <- GBM_df_grid[i, "n.trees"]
#   interaction.depth <- GBM_df_grid[i, "interaction.depth"]
#   n.minobsinnode <- GBM_df_grid[i, "n.minobsinnode"]
#   
#   
#   res <- GBM(var_rep, df_app, df_valid,n.trees=n.trees,
#              shrinkage = shrinkage,interaction.depth = interaction.depth,
#              n.minobsinnode=n.minobsinnode)
#   
#   res_df <- data.frame(
#     shrinkage = shrinkage,
#     n.trees = n.trees,
#     interaction.depth = interaction.depth,
#     n.minobsinnode = n.minobsinnode,
#     rmse = res$RMSE,
#     r_squared = res$R_squared,
#     mae = res$MAE
#   )
#   cat(i,"/",nrow(GBM_df_grid))
#   
#   res_df
# })
# 
# # Fusion des résultats en un seul data frame
# GBM_tune_BM_tot <- do.call(rbind, results_list)
# 
# temps2 = Sys.time()  
# duree = difftime(temps2,temps1)
# duree
# 
# write.csv2(x =GBM_tune_BM_tot,file = paste0("results_tuning/", var_rep, "_GBM_tuning.csv"), row.names = FALSE)
# 
# GBM_tune_BM_tot %>% arrange(mae)







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



res_test = GBM(var_rep, df_app, df_valid,distribution = 'gaussian',n.trees=100 ,
               shrinkage=0.001,interaction.depth = 6,n.minobsinnode = 4)

res_test$RMSE
res_test$R_squared
res_test$MAE
res_test$model





# temps1 = Sys.time()
# 
# # Initialisation du data frame en dehors de la boucle
# GBM_tune_Richesse_tot <- data.frame(
#   shrinkage = numeric(),
#   n.trees = numeric(),
#   interaction.depth = numeric(),
#   n.minobsinnode = numeric(),
#   rmse = numeric(),
#   r_squared = numeric(),
#   mae = numeric()
# )
# 
# # Utilisation de lapply pour appliquer GBM à chaque combinaison
# results_list <- lapply(1:nrow(GBM_df_grid), function(i) {
#   shrinkage <- GBM_df_grid[i, "shrinkage"]
#   n.trees <- GBM_df_grid[i, "n.trees"]
#   interaction.depth <- GBM_df_grid[i, "interaction.depth"]
#   n.minobsinnode <- GBM_df_grid[i, "n.minobsinnode"]
#   
#   
#   res <- GBM(var_rep, df_app, df_valid,n.trees=n.trees,
#              shrinkage = shrinkage,interaction.depth = interaction.depth,
#              n.minobsinnode=n.minobsinnode)
#   
#   res_df <- data.frame(
#     shrinkage = shrinkage,
#     n.trees = n.trees,
#     interaction.depth = interaction.depth,
#     n.minobsinnode = n.minobsinnode,
#     rmse = res$RMSE,
#     r_squared = res$R_squared,
#     mae = res$MAE
#   )
#   cat(i,"/",nrow(GBM_df_grid))
#   
#   res_df
# })
# 
# # Fusion des résultats en un seul data frame
# GBM_tune_Richesse_tot <- do.call(rbind, results_list)
# 
# temps2 = Sys.time()  
# duree = difftime(temps2,temps1)
# duree
# 
# write.csv2(x =GBM_tune_Richesse_tot,file = paste0("results_tuning/", var_rep, "_GBM_tuning.csv"), row.names = FALSE)
# 
# GBM_tune_Richesse_tot %>% arrange(mae)


# Anciens tuning GBM -------------------------------------------------------

# # see : https://bradleyboehmke.github.io/HOML/gbm.html
# # AB_tot_train
# # AB_tot_test
# 
# # run a basic GBM model
# set.seed(123)  # for reproducibility
# ames_gbm1 <- gbm(
#   formula = AB_tot ~ .,
#   data = AB_tot_train,
#   distribution = "gaussian",  # SSE loss function
#   n.trees = 5000,
#   shrinkage = 0.1,
#   interaction.depth = 3,
#   n.minobsinnode = 10,
#   cv.folds = 10
# )
# 
# # find index for number trees with minimum CV error
# best <- which.min(ames_gbm1$cv.error)
# 
# # get MSE and compute RMSE
# sqrt(ames_gbm1$cv.error[best])
# 
# # plot error curve
# gbm.perf(ames_gbm1, method = "cv")
# 
# 
# 
# # a partir de l'etape 3
# # create grid search
# hyper_grid <- expand.grid(
#   learning_rate = c(0.3, 0.1, 0.05, 0.01, 0.005),
#   RMSE = NA,
#   trees = NA,
#   time = NA
# )
# 
# # execute grid search
# for(i in seq_len(nrow(hyper_grid))) {
# 
#   # fit gbm
#   set.seed(123)  # for reproducibility
#   train_time <- system.time({
#     m <- gbm(
#       formula = AB_tot ~ .,
#       data = AB_tot_train,
#       distribution = "gaussian",
#       n.trees = 5000, 
#       shrinkage = hyper_grid$learning_rate[i], 
#       interaction.depth = 3, 
#       n.minobsinnode = 10,
#       cv.folds = 10 
#    )
#   })
#   
#   # add SSE, trees, and training time to results
#   hyper_grid$RMSE[i]  <- sqrt(min(m$cv.error))
#   hyper_grid$trees[i] <- which.min(m$cv.error)
#   hyper_grid$Time[i]  <- train_time[["elapsed"]]
# cat(i)
# }
# 
# # results
# arrange(hyper_grid, RMSE)
# #   learning_rate     RMSE trees time   Time
# # 1         0.010 5.635777  4174   NA  81.22
# # 2         0.050 5.655101   819   NA 107.34
# # 3         0.005 5.658337  4997   NA 103.94
# # 4         0.100 5.703994   274   NA 108.89
# # 5         0.300 5.788693   141   NA 109.50
# 
# 
# 
# 
# 
# # etape 4
# # search grid
# hyper_grid <- expand.grid(
#   n.trees = 6000,
#   shrinkage = 0.01,
#   interaction.depth = c(3, 5, 7),
#   n.minobsinnode = c(5, 10, 15)
# )
# 
# # create model fit function
# model_fit <- function(n.trees, shrinkage, interaction.depth, n.minobsinnode) {
#   set.seed(123)
#   m <- gbm(
#     formula = AB_tot ~ .,
#     data = AB_tot_train,
#     distribution = "gaussian",
#     n.trees = n.trees,
#     shrinkage = shrinkage,
#     interaction.depth = interaction.depth,
#     n.minobsinnode = n.minobsinnode,
#     cv.folds = 10
#   )
#   # compute RMSE
#   sqrt(min(m$cv.error))
# }
# 
# # perform search grid with functional programming
# hyper_grid$rmse <- purrr::pmap_dbl(
#   hyper_grid,
#   ~ model_fit(
#     n.trees = ..1,
#     shrinkage = ..2,
#     interaction.depth = ..3,
#     n.minobsinnode = ..4
#     )
# )
# 
# # results
# arrange(hyper_grid, rmse)
# #   n.trees shrinkage interaction.depth n.minobsinnode     rmse
# # 1    6000      0.01                 7             15 5.600324
# # 2    6000      0.01                 7             10 5.607520
# # 3    6000      0.01                 5             15 5.612125
# # 4    6000      0.01                 7              5 5.612956
# # 5    6000      0.01                 5              5 5.625295
# # 6    6000      0.01                 5             10 5.627353
# # 7    6000      0.01                 3             15 5.632210
# # 8    6000      0.01                 3             10 5.635777
# # 9    6000      0.01                 3              5 5.653090
# 
# # nouvelle tantative
# # search grid
# hyper_grid <- expand.grid(
#   n.trees = 8000,
#   shrinkage = 0.001,
#   interaction.depth = c(7),
#   n.minobsinnode = c(15)
# )
# # perform search grid with functional programming
# hyper_grid$rmse <- purrr::pmap_dbl(
#   hyper_grid,
#   ~ model_fit(
#     n.trees = ..1,
#     shrinkage = ..2,
#     interaction.depth = ..3,
#     n.minobsinnode = ..4
#     )
# )
# # results
# arrange(hyper_grid, rmse)