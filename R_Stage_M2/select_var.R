





# POLY + RF ---------------------------------------------------------------


RF_result_AB_tot = ForetAlea(var_rep ="AB_tot", 
                             df_app=AB_tot_train, 
                             df_valid = AB_tot_test,
                             mtry = AB_tot_best_mtry,
                             ntree= AB_tot_best_ntree,
                             maxnodes = AB_tot_best_maxnodes)

# RF_result_AB_tot$RMSE
# RF_result_AB_tot$MAE
# RF_result_AB_tot$R_adj_train
# RF_result_AB_tot$R_adj_test
# RF_result_AB_tot$predit
# RF_result_AB_tot$model




degree <- 5
train_poly <- AB_tot_train
poly_df <- data.frame()
for (col in names(train_poly[,2:20])) {  
  for (i in 1:degree) {
    col_name <- paste0(col, "_poly", i)  # Nom de la nouvelle colonne
    df <- data.frame(poly(train_poly[[col]], i, raw = TRUE))
    colnames(df) <- col_name
    train_poly[[col_name]] = df[[col_name]]
  }
}

head(train_poly)
names(train_poly)
train_poly = as.data.frame(train_poly)





degree <- 5
test_poly <- AB_tot_test
poly_df <- data.frame()
for (col in names(test_poly[,2:20])) {  
  for (i in 1:degree) {
    col_name <- paste0(col, "_poly", i)  # Nom de la nouvelle colonne
    df <- data.frame(poly(test_poly[[col]], i, raw = TRUE))
    colnames(df) <- col_name
    test_poly[[col_name]] = df[[col_name]]
  }
}
head(test_poly)
names(test_poly)
test_poly = as.data.frame(test_poly)



vv = poly(test_poly$CaCO3,5)
vv = as.data.frame(vv)
all(test_poly$CaCO3_poly1 == test_poly$CaCO3)
all(vv$X1 == test_poly$CaCO3)
all(test_poly$CaCO3_poly1 == vv$X1)
all(test_poly$CaCO3_poly2 == vv$X2)



poly_RF_result_AB_tot = ForetAlea(var_rep ="AB_tot", 
                                  df_app=train_poly [,c(1, 21:121)], 
                                  df_valid = test_poly [,c(1, 21:121)],
                                  mtry = AB_tot_best_mtry,
                                  ntree= AB_tot_best_ntree,
                                  maxnodes = NULL)

poly_RF_result_AB_tot$RMSE
poly_RF_result_AB_tot$MAE
poly_RF_result_AB_tot$R_adj_train
poly_RF_result_AB_tot$R_adj_test
poly_RF_result_AB_tot$predit
poly_RF_result_AB_tot$model


varImpPlot(poly_RF_result_AB_tot$model)



# Sans transformation vdt --------------------------------------------------

### AB_tot
AB_tot_train = read.csv2("datas/AB_tot_train.csv")
AB_tot_test = read.csv2("datas/AB_tot_test.csv")

AB_tot_train$AB_tot = AB_tot_train$AB_tot^2
AB_tot_test$AB_tot = AB_tot_test$AB_tot^2

RF_result_AB_tot = ForetAlea(var_rep ="AB_tot", 
                             df_app=AB_tot_train, 
                             df_valid = AB_tot_test,
                             mtry = AB_tot_best_mtry,
                             ntree= AB_tot_best_ntree,
                             maxnodes = NULL)
RF_result_AB_tot$RMSE
RF_result_AB_tot$MAE
RF_result_AB_tot$R_adj_train
RF_result_AB_tot$R_adj_test
# RF_result_AB_tot$predit
RF_result_AB_tot$model
summary(AB_tot_test$AB_tot); cat( "prediction: \n");summary(RF_result_AB_tot$predit)


RF_AB_tot_pred <- RF_result_AB_tot$predit
RF_df_AB_tot = data.frame(Observed=AB_tot_test[,1],Predicted = RF_AB_tot_pred)

# graphique avec ggplot
RF_AB_tot <- ggplot(RF_df_AB_tot, aes(x = Observed, y = Predicted)) +
  geom_point() + # Ajout des points
  geom_smooth(method = "lm", se = TRUE, color = "red") + 
  labs(subtitle =paste0(" RF: R² adj (train) = ", round(RF_result_AB_tot$R_adj_train,2), 
                        "; \n R² adj (test) = ", round(RF_result_AB_tot$R_adj_test,2),
                        "; \n RMSE (test) = ",  round(RF_result_AB_tot$RMSE,2)),
       x = "Abundance: real values", 
       y = "Abundance: predicted values") + 
  theme_classic() 
chemin = paste0("Results/non_transformation/RF_AB_tot_fig.png")
ggsave(chemin, plot = RF_AB_tot,dpi = 300,width = 2.333333,height = 3)




### BM_tot
BM_tot_train = read.csv2("datas/BM_tot_train.csv")
BM_tot_test = read.csv2("datas/BM_tot_test.csv")

BM_tot_train$BM_tot = BM_tot_train$BM_tot^2
BM_tot_test$BM_tot = BM_tot_test$BM_tot^2

RF_result_BM_tot = ForetAlea(var_rep ="BM_tot", 
                             df_app=BM_tot_train, 
                             df_valid = BM_tot_test,
                             mtry = BM_tot_best_mtry,
                             ntree= BM_tot_best_ntree,
                             maxnodes = NULL)
RF_result_BM_tot$RMSE
RF_result_BM_tot$MAE
RF_result_BM_tot$R_adj_train
RF_result_BM_tot$R_adj_test
# RF_result_BM_tot$predit
RF_result_BM_tot$model
summary(BM_tot_test$BM_tot); cat( "prediction: \n");summary(RF_result_BM_tot$predit)


RF_BM_tot_pred <- RF_result_BM_tot$predit
RF_df_BM_tot = data.frame(Observed=BM_tot_test[,1],Predicted = RF_BM_tot_pred)

# graphique avec ggplot
RF_BM_tot <- ggplot(RF_df_BM_tot, aes(x = Observed, y = Predicted)) +
  geom_point() + # Ajout des points
  geom_smooth(method = "lm", se = TRUE, color = "red") + 
  labs(subtitle =paste0(" RF: R² adj (train) = ", round(RF_result_BM_tot$R_adj_train,2), 
                        "; \n R² adj (test) = ", round(RF_result_BM_tot$R_adj_test,2),
                        "; \n RMSE (test) = ",  round(RF_result_BM_tot$RMSE,2)),
       x = "Biomass: real values", 
       y = "Biomass: predicted values") + 
  theme_classic() 
chemin = paste0("Results/non_transformation/RF_BM_tot_fig.png")
ggsave(chemin, plot = RF_BM_tot,dpi = 300,width = 2.333333,height = 3)








### Richesse_tot
Richesse_tot_train = read.csv2("datas/Richesse_tot_train.csv")
Richesse_tot_test = read.csv2("datas/Richesse_tot_test.csv")

Richesse_tot_train$Richesse_tot = Richesse_tot_train$Richesse_tot
Richesse_tot_test$Richesse_tot = Richesse_tot_test$Richesse_tot

RF_result_Richesse_tot = ForetAlea(var_rep ="Richesse_tot", 
                             df_app=Richesse_tot_train, 
                             df_valid = Richesse_tot_test,
                             mtry = Richesse_tot_best_mtry,
                             ntree= Richesse_tot_best_ntree,
                             maxnodes = Richesse_tot_best_maxnodes)
RF_result_Richesse_tot$RMSE
RF_result_Richesse_tot$MAE
RF_result_Richesse_tot$R_adj_train
RF_result_Richesse_tot$R_adj_test
# RF_result_Richesse_tot$predit
RF_result_Richesse_tot$model
summary(Richesse_tot_test$Richesse_tot); cat( "prediction: \n");summary(RF_result_Richesse_tot$predit)


RF_Richesse_tot_pred <- RF_result_Richesse_tot$predit
RF_df_Richesse_tot = data.frame(Observed=Richesse_tot_test[,1],Predicted = RF_Richesse_tot_pred)

# graphique avec ggplot
RF_Richesse_tot <- ggplot(RF_df_Richesse_tot, aes(x = Observed, y = Predicted)) +
  geom_point() + # Ajout des points
  geom_smooth(method = "lm", se = TRUE, color = "red") + 
  labs(subtitle =paste0(" RF: R² adj (train) = ", round(RF_result_Richesse_tot$R_adj_train,2), 
                        "; \n R² adj (test) = ", round(RF_result_Richesse_tot$R_adj_test,2),
                        "; \n RMSE (test) = ",  round(RF_result_Richesse_tot$RMSE,2)),
       x = "Richness: real values", 
       y = "Richness: predicted values") + 
  theme_classic() 
chemin = paste0("Results/non_transformation/RF_Richesse_tot_fig.png")
ggsave(chemin, plot = RF_Richesse_tot,dpi = 300,width = 2.333333,height = 3)





### All 
all_fig_RF = ggarrange(RF_AB_tot, RF_BM_tot, RF_Richesse_tot,
                            labels = c('(a)', '(b)','(c)'),ncol = 3,
                            common.legend = TRUE,
                            legend = 'right'
)
chemin = paste0("Results/non_transformation/all_fig_RF.png")
ggsave(chemin, plot = all_fig_RF,dpi = 300,height = 3,width = 7)


# comparaison avec RF



# Data partion avec kenston -----------------------------------------------

### AB_tot
AB_tot_train = read.csv2("datas/AB_tot_train.csv")
AB_tot_test = read.csv2("datas/AB_tot_test.csv")

df <- data.frame(y =AB_tot_train$AB_tot)
g_AB_tot_train = ggplot(df, aes(x=y)) +
  geom_histogram(aes(y=..density..), fill="#69b3a2", color="#e9ecef", bins=30, alpha=2) +
  geom_density(fill="black", alpha=0.2) +
  theme_gray() +
  labs(title="Train: abundance", x="Value", y="Density") +
  theme(plot.title = element_text(hjust = 0.5))

df <- data.frame(y =AB_tot_test$AB_tot)
g_AB_tot_test = ggplot(df, aes(x=y)) +
  geom_histogram(aes(y=..density..), fill="#69b3a2", color="#e9ecef", bins=30, alpha=2) +
  geom_density(fill="black", alpha=0.2) +
  theme_gray() +
  labs(title="Test: abundance", x="Value", y="Density") +
  theme(plot.title = element_text(hjust = 0.5))

all_fig_RF = ggarrange(g_AB_tot_train, g_AB_tot_test,
                       labels = c('(a)', '(b)'),ncol = 2,
                       common.legend = TRUE,
                       legend = 'right'
)
chemin = paste0("Results/kenSton_partition/AB_tot_sample_fig.png")
ggsave(chemin, plot = all_fig_RF,dpi = 300,height = 2.5,width = 5.333334)


df_AB_tot = rbind(AB_tot_train,AB_tot_test)
partition = kenStone(df_AB_tot,k= round (nrow(AB_tot_train)))
AB_tot_train = df_AB_tot[partition$model,]
AB_tot_test = df_AB_tot[partition$test,]
df <- data.frame(y =AB_tot_train$AB_tot)
g_AB_tot_train = ggplot(df, aes(x=y)) +
  geom_histogram(aes(y=..density..), fill="#69b3a2", color="#e9ecef", bins=30, alpha=2) +
  geom_density(fill="black", alpha=0.2) +
  theme_gray() +
  labs(title="Train: abundance", x="Value", y="Density") +
  theme(plot.title = element_text(hjust = 0.5))

df <- data.frame(y =AB_tot_test$AB_tot)
g_AB_tot_test = ggplot(df, aes(x=y)) +
  geom_histogram(aes(y=..density..), fill="#69b3a2", color="#e9ecef", bins=30, alpha=2) +
  geom_density(fill="black", alpha=0.2) +
  theme_gray() +
  labs(title="Test: abundance", x="Value", y="Density") +
  theme(plot.title = element_text(hjust = 0.5))

all_fig_RF = ggarrange(g_AB_tot_train, g_AB_tot_test,
                       labels = c('(a)', '(b)'),ncol = 2,
                       common.legend = TRUE,
                       legend = 'right'
)
chemin = paste0("Results/kenSton_partition/AB_tot_kenStone_fig.png")
ggsave(chemin, plot = all_fig_RF,dpi = 300,height = 2.5,width = 5.333334)



### CaCO3
AB_tot_train = read.csv2("datas/AB_tot_train.csv")
AB_tot_test = read.csv2("datas/AB_tot_test.csv")

df <- data.frame(y =AB_tot_train$CaCO3)
g_AB_tot_train = ggplot(df, aes(x=y)) +
  geom_histogram(aes(y=..density..), fill="#69b3a2", color="#e9ecef", bins=30, alpha=2) +
  geom_density(fill="black", alpha=0.2) +
  theme_gray() +
  labs(title="Train: CaCO3", x="Value", y="Density") +
  theme(plot.title = element_text(hjust = 0.5))

df <- data.frame(y =AB_tot_test$CaCO3)
g_AB_tot_test = ggplot(df, aes(x=y)) +
  geom_histogram(aes(y=..density..), fill="#69b3a2", color="#e9ecef", bins=30, alpha=2) +
  geom_density(fill="black", alpha=0.2) +
  theme_gray() +
  labs(title="Test: CaCO3", x="Value", y="Density") +
  theme(plot.title = element_text(hjust = 0.5))

all_fig_RF = ggarrange(g_AB_tot_train, g_AB_tot_test,
                       labels = c('(a)', '(b)'),ncol = 2,
                       common.legend = TRUE,
                       legend = 'right'
)
chemin = paste0("Results/kenSton_partition/CaCO3_sample_fig.png")
ggsave(chemin, plot = all_fig_RF,dpi = 300,height = 2.5,width = 5.333334)


df_AB_tot = rbind(AB_tot_train,AB_tot_test)
partition = kenStone(df_AB_tot,k= round (nrow(AB_tot_train)))
AB_tot_train = df_AB_tot[partition$model,]
AB_tot_test = df_AB_tot[partition$test,]
df <- data.frame(y =AB_tot_train$CaCO3)
g_AB_tot_train = ggplot(df, aes(x=y)) +
  geom_histogram(aes(y=..density..), fill="#69b3a2", color="#e9ecef", bins=30, alpha=2) +
  geom_density(fill="black", alpha=0.2) +
  theme_gray() +
  labs(title="Train: CaCO3", x="Value", y="Density") +
  theme(plot.title = element_text(hjust = 0.5))

df <- data.frame(y =AB_tot_test$CaCO3)
g_AB_tot_test = ggplot(df, aes(x=y)) +
  geom_histogram(aes(y=..density..), fill="#69b3a2", color="#e9ecef", bins=30, alpha=2) +
  geom_density(fill="black", alpha=0.2) +
  theme_gray() +
  labs(title="Test: CaCO3", x="Value", y="Density") +
  theme(plot.title = element_text(hjust = 0.5))

all_fig_RF = ggarrange(g_AB_tot_train, g_AB_tot_test,
                       labels = c('(a)', '(b)'),ncol = 2,
                       common.legend = TRUE,
                       legend = 'right'
)
chemin = paste0("Results/kenSton_partition/CaCO3_kenStone_fig.png")
ggsave(chemin, plot = all_fig_RF,dpi = 300,height = 2.5,width = 5.333334)



### model
AB_tot_train = read.csv2("datas/AB_tot_train.csv")
AB_tot_test = read.csv2("datas/AB_tot_test.csv")
RF_result_AB_tot = ForetAlea(var_rep ="AB_tot", 
                             df_app=AB_tot_train, 
                             df_valid = AB_tot_test,
                             mtry = AB_tot_best_mtry,
                             ntree= AB_tot_best_ntree,
                             maxnodes = NULL)
RF_result_AB_tot$R_adj_train
RF_result_AB_tot$R_adj_test





AB_tot_train = read.csv2("datas/AB_tot_train.csv")
AB_tot_test = read.csv2("datas/AB_tot_test.csv")
df_AB_tot = rbind(AB_tot_train,AB_tot_test)
partition = kenStone(df_AB_tot,k= round (nrow(AB_tot_train)))
AB_tot_train = df_AB_tot[partition$model,]
AB_tot_test = df_AB_tot[partition$test,]
RF_result_AB_tot = ForetAlea(var_rep ="AB_tot", 
                             df_app=AB_tot_train, 
                             df_valid = AB_tot_test,
                             mtry = AB_tot_best_mtry,
                             ntree= AB_tot_best_ntree,
                             maxnodes = NULL)
RF_result_AB_tot$R_adj_train
RF_result_AB_tot$R_adj_test






RF_result_AB_tot$RMSE
RF_result_AB_tot$MAE
# RF_result_AB_tot$predit
RF_result_AB_tot$model
summary(AB_tot_test$AB_tot); cat( "prediction: \n");summary(RF_result_AB_tot$predit)


RF_AB_tot_pred <- RF_result_AB_tot$predit^2
RF_df_AB_tot = data.frame(Observed=AB_tot_test[,1]^2,Predicted = RF_AB_tot_pred)

# graphique avec ggplot
RF_AB_tot <- ggplot(RF_df_AB_tot, aes(x = Observed, y = Predicted)) +
  geom_point() + # Ajout des points
  geom_smooth(method = "lm", se = TRUE, color = "red") + 
  labs(subtitle =paste0(" RF: R² adj (train) = ", round(RF_result_AB_tot$R_adj_train,2), 
                        "; \n R² adj (test) = ", round(RF_result_AB_tot$R_adj_test,2),
                        "; \n RMSE (test) = ",  round(RF_result_AB_tot$RMSE^2,2)),
       x = "Real values", 
       y = "Predicted values") + 
  theme_classic()  
chemin = paste0("Results/kenSton_partition/RF_AB_tot_fig.png")
ggsave(chemin, plot = RF_AB_tot,dpi = 300,width = 2.333333,height = 3)


# selection ---------------------------------------------------------------


### AB_tot
VAR_REP = "AB_tot"
TRAIN = df_train_AB_tot
TEST = df_test_AB_tot

variables = names(df_explo_AB_tot[,-c(1:2)])
interation=length(variables)
AB_tot_select_rf <- data.frame(var_rep=character(),
                               R_adj_train = numeric(),
                               R_adj_test = numeric(),
                               RMSE = numeric(),
                               delet = character(),
                               interation = numeric())


# Boucle pour la sélection des variables
for (i in 1:interation) {
  AB_tot_RF_model <- ForetAlea(var_rep = VAR_REP, 
                               df_app = TRAIN[, c(VAR_REP,"clcm_lvl3", variables)], 
                               df_valid = TEST[, c(VAR_REP,"clcm_lvl3", variables)], 
                               mtry = round(length(variables)/3), ntree = 1000, maxnodes = NULL)
  cat(i, "/",interation,"\n")
  
  #  la variable à supprimer
  var_importance <- data.frame(AB_tot_RF_model$model$importance)
  to_remove <- row.names(var_importance)[which.min(var_importance$IncNodePurity)]
  
  variables <- variables[!variables %in% to_remove]
  
  
  df <- data.frame(var_rep=VAR_REP,
                   R_adj_train = AB_tot_RF_model$R_adj_train,
                   R_adj_test = AB_tot_RF_model$R_adj_test,
                   RMSE = AB_tot_RF_model$RMSE,
                   delet = to_remove,
                   interation = i)
  AB_tot_select_rf = rbind(AB_tot_select_rf,df)
  rm("df")
}
AB_tot_select_rf 
write.csv2(x =AB_tot_select_rf,file = "models/AB_tot_select_rf.csv", row.names = FALSE)
best_var_AB_tot = c("clcm_lvl3",variables,"bio4","N","gdd0","clay","P","CN","silt")





### BM_tot
VAR_REP = "BM_tot"
TRAIN = df_train_BM_tot 
TEST = df_test_BM_tot

variables = names(df_explo_BM_tot[,-c(1:2)])
interation=length(variables)
BM_tot_select_rf <- data.frame(var_rep=character(),
                               R_adj_train = numeric(),
                               R_adj_test = numeric(),
                               RMSE = numeric(),
                               delet = character(),
                               interation = numeric())


# Boucle pour la sélection des variables
for (i in 1:interation) {
  BM_tot_RF_model <- ForetAlea(var_rep = VAR_REP, 
                               df_app = TRAIN[, c(VAR_REP,"clcm_lvl3", variables)], 
                               df_valid = TEST[, c(VAR_REP,"clcm_lvl3", variables)], 
                               mtry = round(length(variables)/3), ntree = 1000, maxnodes = NULL)
  cat(i, "/",interation,"\n")
  
  #  la variable à supprimer
  var_importance <- data.frame(BM_tot_RF_model$model$importance)
  to_remove <- row.names(var_importance)[which.min(var_importance$IncNodePurity)]
  
  variables <- variables[!variables %in% to_remove]
  
  
  df <- data.frame(var_rep=VAR_REP,
                   R_adj_train = BM_tot_RF_model$R_adj_train,
                   R_adj_test = BM_tot_RF_model$R_adj_test,
                   RMSE = BM_tot_RF_model$RMSE,
                   delet = to_remove,
                   interation = i)
  BM_tot_select_rf = rbind(BM_tot_select_rf,df)
  rm("df")
}
BM_tot_select_rf 
write.csv2(x =BM_tot_select_rf,file = "models/BM_tot_select_rf.csv", row.names = FALSE)
best_var_BM_tot = c("clcm_lvl3",variables,"bio4","N","gdd0","clay","P","CN","silt")

rest_BM_tot = variables[!variables %in% BM_tot_select_rf$delet]

best_var_BM_tot = c("clcm_lvl3",rest_BM_tot,"CaCO3","bio18","clay")




### Richesse_tot
VAR_REP = "Richesse_tot"
TRAIN = df_train_Richesse_tot
TEST = df_test_Richesse_tot

variables = names(df_explo_Richesse_tot[,-c(1:2)])
interation=length(variables)
Richesse_tot_select_rf <- data.frame(var_rep=character(),
                               R_adj_train = numeric(),
                               R_adj_test = numeric(),
                               RMSE = numeric(),
                               delet = character(),
                               interation = numeric())

# Boucle pour la sélection des variables
for (i in 1:interation) {
  Richesse_tot_RF_model <- ForetAlea(var_rep = VAR_REP, 
                               df_app = TRAIN[, c(VAR_REP,"clcm_lvl3", variables)], 
                               df_valid = TEST[, c(VAR_REP,"clcm_lvl3", variables)], 
                               mtry = round(length(variables)/3), ntree = 1000, maxnodes = NULL)
  cat(i, "/",interation,"\n")
  
  #  la variable à supprimer
  var_importance <- data.frame(Richesse_tot_RF_model$model$importance)
  to_remove <- row.names(var_importance)[which.min(var_importance$IncNodePurity)]
  
  variables <- variables[!variables %in% to_remove]
  
  
  df <- data.frame(var_rep=VAR_REP,
                   R_adj_train = Richesse_tot_RF_model$R_adj_train,
                   R_adj_test = Richesse_tot_RF_model$R_adj_test,
                   RMSE = Richesse_tot_RF_model$RMSE,
                   delet = to_remove,
                   interation = i)
  Richesse_tot_select_rf = rbind(Richesse_tot_select_rf,df)
  rm("df")
}
Richesse_tot_select_rf 
write.csv2(x =Richesse_tot_select_rf,file = "models/Richesse_tot_select_rf.csv", row.names = FALSE)

rest_Richesse_tot = variables[!variables %in% Richesse_tot_select_rf$delet]
  
best_var_Richesse_tot = c("clcm_lvl3",rest_Richesse_tot,"CaCO3","bio18","clay")

Richesse_tot_RF_model <- ForetAlea(var_rep = VAR_REP, 
                                   df_app = TRAIN[, c(VAR_REP,best_var_Richesse_tot)], 
                                   df_valid = TEST[, c(VAR_REP,best_var_Richesse_tot)], 
                                   mtry = round(length(best_var_Richesse_tot)/3), ntree = 1000, maxnodes = NULL)


















