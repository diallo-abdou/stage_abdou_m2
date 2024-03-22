
# LIMITE : qualité des données inputs, desequilibre des os



# Libraries

library(keras)
library(mlbench) 
library(dplyr)
library(magrittr)
library(neuralnet)
library(ggplot2)
library(reshape2)
library(tfruns)

VAR_REP = "BM_tot"
# Data
training = read.csv2("datas/BM_tot_train.csv")
test = read.csv2("datas/BM_tot_test.csv")

data = rbind(training,test)




ind_var_rep <- which(names(training) == VAR_REP)
trainingtarget <- training[, ind_var_rep]
trainingtarget = sqrt(trainingtarget)
# trainingtarget = round(trainingtarget/25)
training = training[,-ind_var_rep]



ind_var_rep <- which(names(test) == VAR_REP)
testtarget <- test[, ind_var_rep]
testtarget = sqrt(testtarget)
# testtarget = round(testtarget/25)
test = test[,-ind_var_rep]





# Matrix
training %<>% mutate_if(is.factor, as.numeric)
training <- as.matrix(training)
dimnames(training) <- NULL

test %<>% mutate_if(is.factor, as.numeric)
test <- as.matrix(test)
dimnames(test) <- NULL


# Distribution de toute les variables 
data_long <- melt(data)

ggplot(data_long, aes(x=value)) +
  geom_histogram(bins=20) +
  facet_wrap(~variable, scales="free") +
  theme_gray() +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(title="Histogramme", x="Valeur", y="Fréquence")



# Distribution de var rep
#par(mfrow=c(1,2))
df <- data.frame(y =trainingtarget)
biomass_dist_train = ggplot(df, aes(x=y)) +
  geom_histogram(aes(y=..density..), fill="#69b3a2", color="#e9ecef", bins=30, alpha=2) +
  geom_density(fill="black", alpha=0.2) +
  theme_gray() +
  labs(title="Biomass: Train", x="Value", y="Density") +
  theme(plot.title = element_text(hjust = 0.5))
ggsave("Results/biomass_dist_train.png", plot = biomass_dist_train, dpi = 300,width = 3,height = 2)

df <- data.frame(y =testtarget)
biomass_dist_test = ggplot(df, aes(x=y)) +
  geom_histogram(aes(y=..density..), fill="#69b3a2", color="#e9ecef", bins=30, alpha=2) +
  geom_density(fill="black", alpha=0.2) +
  theme_gray() +
  labs(title="Biomass: Test", x="Value", y="Density") +
  theme(plot.title = element_text(hjust = 0.5))
ggsave("Results/biomass_dist_test.png", plot = biomass_dist_test, dpi = 300,width = 3,height = 2)

biomass_dist_train
biomass_dist_test


# Distrvitbution de var rep dans train et de test: est ce homogene ?

biomass_dist_train_and_test = ggarrange(biomass_dist_train, biomass_dist_test,
                                          labels = c('(a)', '(b)'),
                                          common.legend = TRUE,
                                          legend = 'right'
)


ggsave("Results/biomass_dist_train_and_test.png", plot = biomass_dist_train_and_test, dpi = 300 ,height = 2,width = 4)


df_train <- data.frame(y = trainingtarget, set = "Training set")
df_test <- data.frame(y = testtarget, set = "Test set")

df <- rbind(df_train, df_test)

biomass_dist_train_and_test= ggplot(df, aes(x=y, fill=set)) +
  geom_histogram(aes(y=..density..), bins=200, alpha=0.75, position="identity") +
  scale_fill_manual(values=c("Training set"="#F8766D", "Test set"="#00BFC4")) +
  labs(title="Distribution of training and test data",
       x="biomass",
       y="Density",
       fill="Set") +
  theme_gray() +
  theme(plot.title = element_text(hjust = 0.5))
ggsave("Results/biomass_dist_train_and_test.png", plot = biomass_dist_train_and_test, dpi = 300 ,height = 2,width = 4)
biomass_dist_train_and_test







# Normalize
m <- colMeans(training)
s <- apply(training, 2, sd)
training <- scale(training, center = m, scale = s)
test <- scale(test, center = m, scale = s)






# Create Model : ANN_1 ----------------------------------------------------
ANN_1 <- keras_model_sequential()
ANN_1 %>% 
  layer_dense(units = 1, activation = 'relu', input_shape = c(25)) %>%
  layer_dense(units = 1)

# Compile
ANN_1 %>% compile(loss = 'mse',
                  optimizer = 'rmsprop',
                  metrics = 'mae')
summary(ANN_1)

# Fit ANN_1
myANN_1 <- ANN_1 %>%
  fit(training,
      trainingtarget,
      epochs = 100,
      #batch_size = 1,
      validation_split = 0.2)
plot(myANN_1)

# Evaluate
ANN_1 %>% evaluate(test, testtarget)
ANN_1_pred <- ANN_1 %>% predict(test)
ANN_1_mse = mean((testtarget-ANN_1_pred)^2) # loss -> mse
ANN_1_mae =mean(abs(ANN_1_pred - testtarget),na.rm=TRUE) # MAE 
ANN_1_rmse =sqrt(mean((testtarget - ANN_1_pred)^2,na.rm=TRUE)) # rmse
SSR <- sum((ANN_1_pred - testtarget) ^ 2,na.rm=TRUE)
SST <- sum((testtarget - mean(testtarget)) ^ 2)
ANN_1_rsquare = 1 - (SSR / SST) # r_adj <- 
ANN_1_plot_cor = plot(testtarget, ANN_1_pred,main="ANN_1")
ANN_1_cor = cor(testtarget,ANN_1_pred)^2
ANN_1_results = data.frame(model = "ANN_1",mse = ANN_1_mse, mae = ANN_1_mae,
                           rmse = ANN_1_rmse, r_square =ANN_1_rsquare, cor_pred_obs= ANN_1_cor)
ANN_1_results

cbind(testtarget,ANN_1_pred)[1:20,]



# Hyperparameter tuning ---------------------------------------------------


# runs <- tuning_run("Experiment.R",
#                    flags = list(dense_units1 = c(32, 64),
#                                 dense_units2 = c(16, 32),
#                                 dense_units3 = c(8, 16),
#                                 dense_units4 = c(4, 8),
#                                 dropout1 = c(0.4, 0.5),
#                                 dropout1 = c(0.3, 0.4),
#                                 dropout1 = c(0.2, 0.3),
#                                 dropout1 = c(0.1, 0.2),
#                                 batch_size = c(32, 64)))
# 
# write.csv2(x =runs,file = "results_tuning/BM_tot_ANN_tuning.csv", row.names = FALSE)

runs = read.csv2("results_tuning/BM_tot_ANN_tuning.csv")

# Best hyperparameter values
save_runs = runs
results <- runs

results = as.data.frame(results)

results = results[,2:16]
results = results %>% arrange(metric_val_mae)
head(results)

best_param = results[1,]

dense_units1 = as.numeric(best_param$flag_dense_units1)
dense_units2 = as.numeric(best_param$flag_dense_units2)
dense_units3 = as.numeric(best_param$flag_dense_units3)
dense_units4 = as.numeric(best_param$flag_dense_units4)

dropout1 =as.numeric(best_param$flag_dropout1)
dropout2 =as.numeric(best_param$flag_dropout2)
dropout3 =as.numeric(best_param$flag_dropout3)
dropout4 =as.numeric(best_param$flag_dropout4)

batch_size =as.numeric(best_param$flag_batch_size)


# TUNE MODEL
ANN_tune <- keras_model_sequential()
ANN_tune %>% 
  layer_dense(units = dense_units1, activation = 'relu', input_shape = c(25)) %>%
  layer_dropout(rate = dropout1)  %>%
  layer_dense(units = dense_units2, activation = 'relu') %>%
  layer_dropout(rate = dropout2)  %>%
  layer_dense(units = dense_units3, activation = 'relu') %>%
  layer_dropout(rate = dropout3)  %>%
  layer_dense(units = dense_units4, activation = 'relu') %>%
  layer_dropout(rate = dropout4)  %>%
  layer_dense(units = 1)


# Compile
ANN_tune %>% compile(loss = 'mse',
                     optimizer = 'rmsprop',
                     metrics = 'mae')

#  callback EarlyStopping
mon_callback <- callback_early_stopping(
  monitor = "val_mae",  # Surveille la perte sur l'ensemble de validation
  patience = 10,         # Nombre d'époques sans amélioration avant l'arrêt
  restore_best_weights = TRUE  # Restaure les poids du meilleur modèle
)



# Fit ANN_tune
myANN_tune <- ANN_tune %>%
  fit(training,
      trainingtarget,
      epochs = 100,
      batch_size = batch_size,
      validation_split = 0.2,
      callbacks = list(mon_callback)
  )

plot(myANN_tune)

# Evaluate
ANN_tune %>% evaluate(test, testtarget)
ANN_tune_pred <- ANN_tune %>% predict(test)
ANN_tune_mse = mean((testtarget-ANN_tune_pred)^2) # loss -> mse
ANN_tune_mae =mean(abs(ANN_tune_pred - testtarget),na.rm=TRUE) # MAE 
ANN_tune_rmse =sqrt(mean((testtarget - ANN_tune_pred)^2,na.rm=TRUE)) # rmse
SSR <- sum((ANN_tune_pred - testtarget) ^ 2,na.rm=TRUE)
SST <- sum((testtarget - mean(testtarget)) ^ 2)
ANN_tune_rsquare = 1 - (SSR / SST) # r_adj <- 
ANN_tune_plot_cor = plot(testtarget, ANN_tune_pred, main = "ANN_tune")
ANN_tune_cor = cor(testtarget,ANN_tune_pred)^2
ANN_tune_results = data.frame(model = "ANN_tune",mse = ANN_tune_mse, mae = ANN_tune_mae,
                              rmse = ANN_tune_rmse, r_square =ANN_tune_rsquare, cor_pred_obs= ANN_tune_cor)
rbind(ANN_1_results,ANN_tune_results)





# Create Model : ANN_2 ----------------------------------------------------
ANN_2 <- keras_model_sequential()
ANN_2 %>% 
  layer_dense(units = 10, activation = 'relu', input_shape = c(25)) %>%
  layer_dense(units = 5, activation = 'relu') %>%
  layer_dense(units = 1)


# Compile
ANN_2 %>% compile(loss = 'mse',
                  optimizer = 'rmsprop',
                  metrics = 'mae')
summary(ANN_2)


#  callback EarlyStopping
mon_callback <- callback_early_stopping(
  monitor = "val_mae",  # Surveille la perte sur l'ensemble de validation
  patience = 10,         # Nombre d'époques sans amélioration avant l'arrêt
  restore_best_weights = TRUE  # Restaure les poids du meilleur modèle
)


# Fit ANN_2
myANN_2 <- ANN_2 %>%
  fit(training,
      trainingtarget,
      epochs = 100,
      batch_size = 32,
      validation_split = 0.2,
      callbacks = list(mon_callback)
  )

plot(myANN_2)

# Evaluate
ANN_2 %>% evaluate(test, testtarget)
ANN_2_pred <- ANN_2 %>% predict(test)
ANN_2_mse = mean((testtarget-ANN_2_pred)^2) # loss -> mse
ANN_2_mae =mean(abs(ANN_2_pred - testtarget),na.rm=TRUE) # MAE 
ANN_2_rmse =sqrt(mean((testtarget - ANN_2_pred)^2,na.rm=TRUE)) # rmse
SSR <- sum((ANN_2_pred - testtarget) ^ 2,na.rm=TRUE)
SST <- sum((testtarget - mean(testtarget)) ^ 2)
ANN_2_rsquare = 1 - (SSR / SST) # r_adj <- 
ANN_2_plot_cor = plot(testtarget, ANN_2_pred, main = "ANN_2")
ANN_2_cor = cor(testtarget,ANN_2_pred)^2
ANN_2_results = data.frame(model = "ANN_2",mse = ANN_2_mse, mae = ANN_2_mae,
                           rmse = ANN_2_rmse, r_square =ANN_2_rsquare, cor_pred_obs= ANN_2_cor)
rbind(ANN_1_results,ANN_tune_results,ANN_2_results)






# Create Model : ANN_3 ----------------------------------------------------

ANN_3 <- keras_model_sequential()
ANN_3 %>% 
  layer_dense(units = 100, activation = 'relu', input_shape = c(25)) %>%
  layer_dropout(rate = 0.4)  %>%
  layer_dense(units = 50, activation = 'relu') %>%
  layer_dropout(rate = 0.3)  %>%
  layer_dense(units = 25, activation = 'relu') %>%
  layer_dropout(rate = 0.2)  %>%
  layer_dense(units = 10, activation = 'relu') %>%
  layer_dropout(rate = 0.1)  %>%
  layer_dense(units = 1)

summary(ANN_3)
# Compile
ANN_3 %>% compile(loss = 'mse',
                  optimizer = 'rmsprop',
                  metrics = 'mae')
summary(ANN_3)


#  callback EarlyStopping
mon_callback <- callback_early_stopping(
  monitor = "val_mae",  # Surveille la perte sur l'ensemble de validation
  patience = 10,         # Nombre d'époques sans amélioration avant l'arrêt
  restore_best_weights = TRUE  # Restaure les poids du meilleur modèle
)


# Fit ANN_3
myANN_3 <- ANN_3 %>%
  fit(training,
      trainingtarget,
      epochs = 100,
      batch_size = 16,
      validation_split = 0.2,
      callbacks = list(mon_callback))
plot(myANN_3)

# Evaluate
ANN_3 %>% evaluate(test, testtarget)
ANN_3_pred <- ANN_3 %>% predict(test)
ANN_3_mse = mean((testtarget-ANN_3_pred)^2) # loss -> mse
ANN_3_mae =mean(abs(ANN_3_pred - testtarget),na.rm=TRUE) # MAE 
ANN_3_rmse =sqrt(mean((testtarget - ANN_3_pred)^2,na.rm=TRUE)) # rmse
SSR <- sum((ANN_3_pred - testtarget) ^ 2,na.rm=TRUE)
SST <- sum((testtarget - mean(testtarget)) ^ 2)
ANN_3_rsquare = 1 - (SSR / SST) # r_adj <- 
ANN_3_plot_cor = plot(testtarget, ANN_3_pred, main = "ANN_3")
ANN_3_cor = cor(testtarget,ANN_3_pred)^2
ANN_3_results = data.frame(model = "ANN_3",mse = ANN_3_mse, mae = ANN_3_mae,
                           rmse = ANN_3_rmse, r_square =ANN_3_rsquare, cor_pred_obs= ANN_3_cor)

rbind(ANN_1_results,ANN_tune_results,ANN_2_results,ANN_3_results)




# ANN_4 -------------------------------------------------------------------

ANN_4 <- keras_model_sequential()
ANN_4 %>% 
  layer_dense(units = 100, activation = 'relu', input_shape = c(25)) %>%
  layer_dropout(rate = 0.4)  %>%
  layer_dense(units = 50, activation = 'relu') %>%
  layer_dropout(rate = 0.3)  %>%
  layer_dense(units = 25, activation = 'relu') %>%
  layer_dropout(rate = 0.2)  %>%
  layer_dense(units = 10, activation = 'relu') %>%
  layer_dropout(rate = 0.1)  %>%
  layer_dense(units = 1)
# 
# ANN_4 %>%
#   layer_dense(100, activation = 'elu',
#               kernel_regularizer = regularizer_l2(0.002),
#               input_shape <- shape(25)) %>%
#   layer_dropout(rate = 0.4)  %>%
#   layer_dense(50, activation = 'elu',
#               kernel_regularizer = regularizer_l2(0.002)) %>%
#   layer_dropout(rate = 0.3)  %>%
#   layer_dense(25, activation = 'elu',
#               kernel_regularizer = regularizer_l2(0.002)) %>%
#   layer_dropout(rate = 0.2)  %>%
#   layer_dense(5, activation = 'elu',
#               kernel_regularizer = regularizer_l2(0.002)) %>%
#   layer_dropout(rate = 0.1)  %>%
#   layer_dense(1)


# Compile
ANN_4 %>% compile(loss = 'mse',
                  optimizer = optimizer_rmsprop(learning_rate = 0.001),
                  metrics = 'mae')
summary(ANN_4)


#  callback EarlyStopping
mon_callback <- callback_early_stopping(
  monitor = "val_mae",  # Surveille la perte sur l'ensemble de validation
  patience = 10,         # Nombre d'époques sans amélioration avant l'arrêt
  restore_best_weights = TRUE  # Restaure les poids du meilleur modèle
)


# Fit ANN_4
myANN_4 <- ANN_4 %>%
  fit(training,
      trainingtarget,
      epochs = 100,
      batch_size = 16,
      validation_split = 0.2,
      callbacks = list(mon_callback))
plot(myANN_4)

# Evaluate
ANN_4 %>% evaluate(test, testtarget)
ANN_4_pred <- ANN_4 %>% predict(test)
ANN_4_mse = mean((testtarget-ANN_4_pred)^2) # loss -> mse
ANN_4_mae =mean(abs(ANN_4_pred - testtarget),na.rm=TRUE) # MAE 
ANN_4_rmse =sqrt(mean((testtarget - ANN_4_pred)^2,na.rm=TRUE)) # rmse
SSR <- sum((ANN_4_pred - testtarget) ^ 2,na.rm=TRUE)
SST <- sum((testtarget - mean(testtarget)) ^ 2)
ANN_4_rsquare = 1 - (SSR / SST) # r_adj <- 
ANN_4_plot_cor = plot(testtarget, ANN_4_pred, main = "ANN_4")
ANN_4_cor = cor(testtarget,ANN_4_pred)^2
ANN_4_results = data.frame(model = "ANN_4",mse = ANN_4_mse, mae = ANN_4_mae,
                           rmse = ANN_4_rmse, r_square =ANN_4_rsquare, cor_pred_obs= ANN_4_cor)

rbind(ANN_1_results,ANN_tune_results,ANN_2_results,ANN_3_results,ANN_4_results)




# Visualisation du best ANN BM_tot ----------------------------------------

df_ANN_BM_tot = rbind(ANN_1_results,ANN_tune_results,ANN_2_results,ANN_3_results,ANN_4_results)


df_ANN_BM_tot = df_ANN_BM_tot %>% arrange(mae)
df_ANN_BM_tot[1,1]

#best_ANN_BM_tot = as.name(df_ANN_BM_tot[1,1])

# Save the entire model as a SavedModel.
save_model_tf(as.name(df_ANN_BM_tot[1,1]), "models/best_ANN_BM_tot")
# my_model directory
# Contains an assets folder, models.pb, and variables folder.
#fs::dir_tree("models/")

best_ANN_BM_tot <- load_model_tf('models/best_ANN_BM_tot')

# Check its architecture
summary(best_ANN_BM_tot)


  
data %<>% mutate_if(is.factor, as.numeric)

# for (i in names(data)){
#   cat( paste0(i, '+'))
# }
# Neural Network Visualization
n <- neuralnet(BM_tot ~ gps_x+CaCO3+P+CEC+sand+CN+hurs_mean+gps_y+pH+gdd10+bio4+N+silt+
                 bio3+K+clay+bio16+bio13+bio12+clcm_lvl3mf+clcm_lvl3gua+clcm_lvl3ng+
                 clcm_lvl3nial+clcm_lvl3p+clcm_lvl3v,
               data = data,
               hidden = c(32,16),
               linear.output = F,
               lifesign = 'full',
               rep=1)

plot(n,
     col.hidden = 'darkgreen',
     col.hidden.synapse = 'darkgreen',
     show.weights = F,
     information = F,
     fill = 'lightblue')
#ggsave("Results/biomass_ANN.png", plot = biomass_ANN, dpi = 300)

plot(n, rep="best")


