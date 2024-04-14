
poly_models <- function(df, response_var, predictor_vars, degree = 2) {
  model_results <- list()
  
  # Boucle à travers chaque prédicteur
  for (predictor_var in predictor_vars) {
    
    predictor_results <- c()
    for (deg in 1:degree) {
      
      formula <- as.formula(paste(response_var, "~ poly(", predictor_var, ",", deg, ")"))
      model <- lm(formula, data = df)
      
      adj_r_squared <- summary(model)$adj.r.squared
      
      predictor_results <- c(predictor_results, adj_r_squared)
    }
    
    model_results[[predictor_var]] <- predictor_results
  }
  
  model_df <- data.frame(predictor = predictor_vars,matrix(unlist(model_results), nrow = length(predictor_vars), byrow = TRUE))
  colnames(model_df) <- c("predictor", paste0("degree_", 1:degree))
  
  return(model_df)
}


# For Abundance -----------------------------------------------------------



VAR_REP = "AB_tot"
TITRE = "Abundance"

training = read.csv2(paste0("datas/",VAR_REP,"_train.csv"))
test = read.csv2(paste0("datas/",VAR_REP,"_test.csv"))

# varImpPlot(best20_var_AB_tot$model)
# sup_var_AB_tot = c( "bio6", "CEC","sand","elevation","bio4","hurs_mean","bio7","silt","P", "bio15")
# training = training %>% select(- sup_var_AB_tot)
# test = test %>% select(- sup_var_AB_tot)



variables = names(training[,2:20])
AB_tot_poly_models <- poly_models(df = training, response_var = VAR_REP, predictor_vars = variables, degree = 5)
AB_tot_poly_models$best_degree <- apply(AB_tot_poly_models[, -1], 1, which.max)
AB_tot_poly_models$formula <- paste("poly(", AB_tot_poly_models$predictor, ",", AB_tot_poly_models$best_degree, ")", 
                                    sep = "")

formula_str <- paste(VAR_REP, "~ clcm_lvl3mf + clcm_lvl3gua + clcm_lvl3ng + clcm_lvl3nial + 
                     clcm_lvl3p + clcm_lvl3v")

for (var in AB_tot_poly_models$formula ) {
  formula_str <-
    paste(formula_str, var, sep = " + ")
}

# formula_str = "AB_tot ~ clcm_lvl3mf + clcm_lvl3gua + clcm_lvl3ng + clcm_lvl3nial +
# clcm_lvl3p + clcm_lvl3v + CaCO3 + poly(gps_x,2) + bio7 + poly(bio4,5) + 
# poly(N,4) + poly(bio3,4) + poly(gps_y,5) + poly(bio18,5) + poly(bio15,4) + P + 
# poly(gdd0,2) + poly(clay,3) + poly(elevation,3) + poly(CEC,3) + poly(silt,3) + 
# poly(hurs_mean,3) + poly(sand,5) + poly(CN,3) + bio6"

# Create the polynomial model using the provided training data
mod <- lm(formula_str, data = training)
mod = stepAIC(mod)
# plot(mod)
summary(mod)
# 
# for(i in names(training[,2:20])) cat(i,"*")
# mod <- lm(AB_tot ~ clcm_lvl3mf + clcm_lvl3gua + clcm_lvl3ng + clcm_lvl3nial +
#             clcm_lvl3p + clcm_lvl3v ,data = training)
# 
# mod <- lm(AB_tot ~ clcm_lvl3mf + clcm_lvl3gua + clcm_lvl3ng + clcm_lvl3nial +
#             clcm_lvl3p + clcm_lvl3v + poly(CaCO3,degree=5, raw = TRUE),data = training)
# 
# mod <- lm(AB_tot ~ clcm_lvl3mf + clcm_lvl3gua + clcm_lvl3ng + clcm_lvl3nial +
#             clcm_lvl3p + clcm_lvl3v + poly(CaCO3, gps_x, degree=5, raw = TRUE),data = training)
# 
# mod <- lm(AB_tot ~ clcm_lvl3mf + clcm_lvl3gua + clcm_lvl3ng + clcm_lvl3nial +
#             clcm_lvl3p + clcm_lvl3v + poly(CaCO3, gps_x +gps_y, degree=5, raw = TRUE),data = training)
# 
# mod <- lm(AB_tot ~ clcm_lvl3mf + clcm_lvl3gua + clcm_lvl3ng + clcm_lvl3nial +
#             clcm_lvl3p + clcm_lvl3v + poly(CaCO3, gps_x +gps_y + bio4, degree=5, raw = TRUE),data = training)
# 
# 
# mod <- lm(AB_tot ~ clcm_lvl3mf + clcm_lvl3gua + clcm_lvl3ng + clcm_lvl3nial +
#             clcm_lvl3p + clcm_lvl3v + poly(CaCO3,gps_x,bio7,gps_y,bio18,bio4,bio3,N,gdd0,bio15,degree=3, raw = TRUE),data = training)
# 
# summary(mod)



# # interaction 2 -----------------------------------------------------------
# degree <- 5
# training_poly <- training
# # Boucle à travers chaque colonne du dataframe
# for (col in names(training[,2:10])) {  # Exclure la première colonne si nécessaire
#   # Créer les colonnes polynomiales pour chaque degré
#   for (i in 1:degree) {
#     col_name <- paste0(col, "_poly", i)  # Nom de la nouvelle colonne
#     training_poly <- mutate(training_poly, !!col_name := poly(training[[col]], i, raw = TRUE))
#   }
# }
# # Afficher les premières lignes du nouveau dataframe
# head(training_poly)
# mod= lm(AB_tot~. ,data = training_poly)
# summary(mod)
# 
# 
# training_inter2= training
# 
# inter_AB_tot <- select_inter("AB_tot", training, names(training)[-1])
# inter_AB_tot <- arrange(inter_AB_tot, desc(r_squared))
# best_inte_AB_tot = inter_AB_tot[1:53, "variables"] 
# 
# 
# AB_tot_inter_train = training
# for (interaction in best_inte_AB_tot) {
#   vars <- strsplit(interaction, ".inter.")[[1]]
#   # cat(vars[1], ":", vars[2], "\n")
#   # la colonne d'interaction
#   AB_tot_inter_train[[interaction]] <- AB_tot_inter_train[[vars[1]]] * AB_tot_inter_train[[vars[2]]]
# }
# # names(AB_tot_inter_train)
# 
# mod= lm(AB_tot~. ,data = AB_tot_inter_train)
# summary(mod)
# 
# best_inter = c("bio4.inter.clcm_lvl3v","N.inter.clcm_lvl3v","CaCO3.inter.bio4","gps_x.inter.clcm_lvl3p",
#                "bio15.inter.clcm_lvl3v","CaCO3.inter.silt","CaCO3.inter.sand","CaCO3.inter.P",
#                "CaCO3.inter.gps_y")
# df_inter = AB_tot_inter_train[,c(names(AB_tot_inter_train)[1:26],best_inter)]
# mod= lm(AB_tot~. ,data = df_inter)
# summary(mod)
# 
# variables = names(df_inter[,c(2:20,27:35)])
# AB_tot_poly_models <- poly_models(df = df_inter, response_var = VAR_REP, predictor_vars = variables, degree = 5)
# AB_tot_poly_models$best_degree <- apply(AB_tot_poly_models[, -1], 1, which.max)
# AB_tot_poly_models$formula <- paste("poly(", AB_tot_poly_models$predictor, ",", AB_tot_poly_models$best_degree, ")", 
#                                     sep = "")
# 
# formula_str <- paste(VAR_REP, "~ clcm_lvl3mf + clcm_lvl3gua + clcm_lvl3ng + clcm_lvl3nial + 
#                      clcm_lvl3p + clcm_lvl3v")
# 
# for (var in AB_tot_poly_models$formula ) {
#   formula_str <-
#     paste(formula_str, var, sep = " + ")
# }
# 
# 
# # Create the polynomial model using the provided training data
# mod <- lm(formula_str, data = df_inter)
# mod = stepAIC(mod)
# # plot(mod)
# summary(mod)
# 
# 
# 
# 
# # metric ------------------------------------------------------------------


pred.train <- predict(mod, newdata = training)
pred.train = as.numeric(pred.train)

pred.test <- predict(mod, newdata = test)
pred.test = as.numeric(pred.test)

# Calculate RMSE to evaluate model quality
rmse <- sqrt(mean((test[, VAR_REP] - pred.test)^2))


# Calcul du R² ajusté pour train
R_adj_train <- calcule_R2(training[,VAR_REP],  pred.train)
n_train <- nrow(training)
p_train <- ncol(training) - 1
r_adj_train <- 1 - ((1 - R_adj_train) * (n_train - 1) / (n_train - p_train - 1))

# Calcul du R² ajusté pour test
R_adj_test <-calcule_R2(test[,VAR_REP],pred.test)
n_test <- nrow(test)
p_test <- ncol(test) - 1
r_adj_test <- 1 - ((1 - R_adj_test) * (n_test - 1) / (n_test - p_test - 1))


# Calculate MAE
MAE <- mean(abs(pred.test - test[, VAR_REP]))

# Round results
rmse <- round(rmse, 2);rmse^2
r_adj_train <- round(r_adj_train, 2);r_adj_train
r_adj_test <- round(r_adj_test, 2);r_adj_test
MAE <- round(MAE, 2);MAE

results <- list(RMSE = rmse, R_adj_train = r_adj_train, R_adj_test = r_adj_test, MAE = MAE, model = mod, predit = pred.test)

POLY_pred <- results$predit^2

POLY_df = data.frame(Observed=test[,1]^2,predicted = POLY_pred)

cor_POLY <- cor(POLY_df$Observed, POLY_df$predicted)

# graphique avec ggplot
POLY_AB_tot <- ggplot(POLY_df, aes(x = Observed, y = predicted)) +
  geom_point() + # Ajout des points
  geom_smooth(method = "lm", se = TRUE, color = "red") + 
  labs(subtitle =paste0(" POLY: R² adj (train) = ", round(results$R_adj_train,2),
                        "; \n R² adj (test) = ", round(results$R_adj_test,2),
                        "; \n RMSE (test) = ",  round(results$RMSE^2,2)),
       x = paste0(TITRE,": real values"), 
       y = paste0(TITRE,":predicted values") ) +
  theme_classic() 

POLY_AB_tot

chemin = paste0("Results/POLY/POLY_",VAR_REP,"_fig.png")
ggsave(chemin, plot = POLY_AB_tot,dpi = 300,width = 2.333333,height = 3)



# For Biomass -----------------------------------------------------------
VAR_REP = "BM_tot"
TITRE = "Biomass"

training = read.csv2(paste0("datas/",VAR_REP,"_train.csv"))
test = read.csv2(paste0("datas/",VAR_REP,"_test.csv"))

variables = names(training[,2:20])
BM_tot_poly_models <- poly_models(df = training, response_var = VAR_REP, predictor_vars = variables, degree = 6)
BM_tot_poly_models$best_degree <- apply(BM_tot_poly_models[, -1], 1, which.max)
BM_tot_poly_models$formula <- paste("poly(", BM_tot_poly_models$predictor, ",", BM_tot_poly_models$best_degree, ")", 
                                    sep = "")

formula_str <- paste(VAR_REP, "~ clcm_lvl3mf + clcm_lvl3gua + clcm_lvl3ng + clcm_lvl3nial + 
                     clcm_lvl3p + clcm_lvl3v")

for (var in BM_tot_poly_models$formula ) {
  formula_str <-
    paste(formula_str, var, sep = " + ")
}

# Create the polynomial model using the provided training data
mod <- lm(formula_str, data = training)
mod = stepAIC(mod)
# plot(mod)


pred.train <- predict(mod, newdata = training)
pred.train = as.numeric(pred.train)

pred.test <- predict(mod, newdata = test)
pred.test = as.numeric(pred.test)

# Calculate RMSE to evaluate model quality
rmse <- sqrt(mean((test[, VAR_REP] - pred.test)^2))


# Calcul du R² ajusté pour train
R_adj_train <- calcule_R2(training[,VAR_REP],  pred.train)
n_train <- nrow(training)
p_train <- ncol(training) - 1
r_adj_train <- 1 - ((1 - R_adj_train) * (n_train - 1) / (n_train - p_train - 1))

# Calcul du R² ajusté pour test
R_adj_test <-calcule_R2(test[,VAR_REP],pred.test)
n_test <- nrow(test)
p_test <- ncol(test) - 1
r_adj_test <- 1 - ((1 - R_adj_test) * (n_test - 1) / (n_test - p_test - 1))


# Calculate MAE
MAE <- mean(abs(pred.test - test[, VAR_REP]))

# Round results
rmse <- round(rmse, 2)
r_adj_train <- round(r_adj_train, 2)
r_adj_test <- round(r_adj_test, 2)
MAE <- round(MAE, 2)

results <- list(RMSE = rmse, R_adj_train = r_adj_train, R_adj_test = r_adj_test, MAE = MAE, model = mod, predit = pred.test)

POLY_pred <- results$predit^2

POLY_df = data.frame(Observed=test[,1]^2,predicted = POLY_pred)

cor_POLY <- cor(POLY_df$Observed, POLY_df$predicted)

# graphique avec ggplot
POLY_BM_tot <- ggplot(POLY_df, aes(x = Observed, y = predicted)) +
  geom_point() + # Ajout des points
  geom_smooth(method = "lm", se = TRUE, color = "red") + 
  labs(subtitle =paste0(" POLY: R² adj (train) = ", round(results$R_adj_train,2),
                        "; \n R² adj (test) = ", round(results$R_adj_test,2),
                        "; \n RMSE (test) = ",  round(results$RMSE^2,2)),
       x = paste0(TITRE,": real values"), 
       y = paste0(TITRE,":predicted values") ) +
  theme_classic() 

POLY_BM_tot

chemin = paste0("Results/POLY/POLY_",VAR_REP,"_fig.png")
ggsave(chemin, plot = POLY_BM_tot,dpi = 300,width = 2.333333,height = 3)





# For Richness -----------------------------------------------------------
VAR_REP = "Richesse_tot"
TITRE = "Richness"

training = read.csv2(paste0("datas/",VAR_REP,"_train.csv"))
test = read.csv2(paste0("datas/",VAR_REP,"_test.csv"))

variables = names(training[,2:20])
Richesse_tot_poly_models <- poly_models(df = training, response_var = VAR_REP, predictor_vars = variables, degree = 6)
Richesse_tot_poly_models$best_degree <- apply(Richesse_tot_poly_models[, -1], 1, which.max)
Richesse_tot_poly_models$formula <- paste("poly(", Richesse_tot_poly_models$predictor, ",", Richesse_tot_poly_models$best_degree, ")", 
                                    sep = "")

formula_str <- paste(VAR_REP, "~ clcm_lvl3mf + clcm_lvl3gua + clcm_lvl3ng + clcm_lvl3nial + 
                     clcm_lvl3p + clcm_lvl3v")

for (var in Richesse_tot_poly_models$formula ) {
  formula_str <-
    paste(formula_str, var, sep = " + ")
}

# Create the polynomial model using the provided training data
mod <- lm(formula_str, data = training)
mod = stepAIC(mod)
# plot(mod)


pred.train <- predict(mod, newdata = training)
pred.train = as.numeric(pred.train)

pred.test <- predict(mod, newdata = test)
pred.test = as.numeric(pred.test)

# Calculate RMSE to evaluate model quality
rmse <- sqrt(mean((test[, VAR_REP] - pred.test)^2))


# Calcul du R² ajusté pour train
R_adj_train <- calcule_R2(training[,VAR_REP],  pred.train)
n_train <- nrow(training)
p_train <- ncol(training) - 1
r_adj_train <- 1 - ((1 - R_adj_train) * (n_train - 1) / (n_train - p_train - 1))

# Calcul du R² ajusté pour test
R_adj_test <-calcule_R2(test[,VAR_REP],pred.test)
n_test <- nrow(test)
p_test <- ncol(test) - 1
r_adj_test <- 1 - ((1 - R_adj_test) * (n_test - 1) / (n_test - p_test - 1))


# Calculate MAE
MAE <- mean(abs(pred.test - test[, VAR_REP]))

# Round results
rmse <- round(rmse, 2)
r_adj_train <- round(r_adj_train, 2)
r_adj_test <- round(r_adj_test, 2)
MAE <- round(MAE, 2)

results <- list(RMSE = rmse, R_adj_train = r_adj_train, R_adj_test = r_adj_test, MAE = MAE, model = mod, predit = pred.test)

POLY_pred <- results$predit^2

POLY_df = data.frame(Observed=test[,1]^2,predicted = POLY_pred)

cor_POLY <- cor(POLY_df$Observed, POLY_df$predicted)

# graphique avec ggplot
POLY_Richesse_tot <- ggplot(POLY_df, aes(x = Observed, y = predicted)) +
  geom_point() + # Ajout des points
  geom_smooth(method = "lm", se = TRUE, color = "red") + 
  labs(subtitle =paste0(" POLY: R² adj (train) = ", round(results$R_adj_train,2),
                        "; \n R² adj (test) = ", round(results$R_adj_test,2),
                        "; \n RMSE (test) = ",  round(results$RMSE^2,2)),
       x = paste0(TITRE,": real values"), 
       y = paste0(TITRE,":predicted values") ) +
  theme_classic() 

POLY_Richesse_tot

chemin = paste0("Results/POLY/POLY_",VAR_REP,"_fig.png")
ggsave(chemin, plot = POLY_Richesse_tot,dpi = 300,width = 2.333333,height = 3)




# All ---------------------------------------------------------------------
all_graphe_poly = ggarrange(POLY_AB_tot, POLY_BM_tot, POLY_Richesse_tot,
                          labels = c('(a)', '(b)','(c)'),ncol = 3,
                          common.legend = TRUE,
                          legend = 'right'
)
ggsave("Results/POLY/all_graphe_poly.png", plot = all_graphe_poly, dpi = 300,height = 3,width = 7.1)
all_graphe_poly
# comparaison avec RF









