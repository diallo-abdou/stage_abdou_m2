

# occu --------------------------------------------------------------------



value1 = c(20,20,25,25,40,35,30,20,35)
hist(value1,col = "red")


value1 = c(20,20,25,25,40,35,30,20,35)
value2 = c(15,25,30,25,25,20,40,40,40)

hist(value1,col = "red")
hist(value2, add = T, col = "blue")


ggplot(data=iris, aes(x=Sepal.Width,fill = Species)) + geom_histogram()


ggplot(data=iris, aes(x=Species, y=Sepal.Width,fill = Species)) +
  theme(panel.background=element_rect(fill="grey90"), # #7B7B7B15
        axis.title=element_text(face="bold"), # Mettre en gras axis
        axis.text.x=element_text(size=14, face="bold", color="black"),
        axis.text.y=element_text(size=14, face="bold", color="black"),
        axis.title.y=element_text(face="bold", vjust=1.5 ,size=15),
        panel.grid.major=element_line(size=1.2, color="white"), # Augmenter taille ligne y
        legend.title=element_blank(),
        legend.text=element_text(size=14),
        plot.margin=unit(c(1, 1, 1.5, 1), "cm"), # (top, right, bottom, left)
        legend.position="none" ) +
  geom_bar( stat="identity", width=0.75, bg="grey50")

+ # Barplot
  scale_y_continuous( limits=c(0, 100), breaks=seq(0,100,10) )  +
  scale_colour_manual(values="black") + # couleur texte
  xlab(NULL) + ylab(NULL)




# Exemple de données (vous pouvez remplacer ces valeurs par vos propres données)
occupation_du_sol <- c("Prairie", "Culture", "Forêt", "Zone humide")
espece_A <- c(20, 20, 20, 50)  # % d'occurrence de l'espèce A
espece_B <- c(50, 25, 75, 20)   # % d'occurrence de l'espèce B
espece_C <- c(30, 55, 15, 30)  # % d'occurrence de l'espèce C

# Création d'un DataFrame
donnees <- data.frame(Occupation = occupation_du_sol,
                      Espece_A = espece_A,
                      Espece_B = espece_B,
                      Espece_C = espece_C)

# Installation et chargement du package ggplot2 (si ce n'est pas déjà fait)
# install.packages("ggplot2")
library(ggplot2)

# Création de l'histogramme empilé
ggplot(donnees, aes(x = Occupation)) +
  geom_bar(aes(y = Espece_A, fill = "Espèce A"), stat = "identity") +
  geom_bar(aes(y = Espece_B, fill = "Espèce B"), stat = "identity") +
  geom_bar(aes(y = Espece_C, fill = "Espèce C"), stat = "identity") +
  labs(x = "Occupation du sol", y = "% d'occurrence des espèces",
       title = "Répartition des espèces par occupation du sol") +
  scale_fill_manual(values = c("Espèce A" = "blue", "Espèce B" = "green", "Espèce C" = "red")) +
  theme_minimal()


# CV ----------------------------------------------------------------------



# Validation croissée manuelle
{
  # Validation croisée avec la méthode de K fold
  tiga_f= tiga_c_sansva_noncor_minmax
  # determiner le nombre et la taille des k fold
  resultat <- determineMaxBlocs(nrow(tiga_f))
  # Nombre de blocs
  num_blocks <- resultat$n_blocs
  # Taille des blocs
  block_size <- resultat$taille_bloc
  
  
  
  # Nombre de lignes à supprimer si num_blocks*block_size est different de nrow (data)
  num_lignes_a_supprimer <- nrow(tiga_f) - num_blocks*block_size
  # Index des lignes à supprimer
  index_lignes_a_supprimer <- sample(nrow(tiga_f), num_lignes_a_supprimer)
  # Supprimer les lignes du dataframe
  tiga_f_modifier <- tiga_f[-index_lignes_a_supprimer, ]
  nrow(tiga_f_modifier)
  
  
  # Mélanger les numéros des lignes
  shuffled_indices <- sample(nrow(tiga_f))
  
  # Répartir les données dans les blocs
  blocs <- data.frame(matrix(NA, nrow = block_size, ncol = num_blocks))
  for (i in 1:num_blocks) {
    start_index <- (i - 1) * block_size + 1
    end_index <- i * block_size
    blocs[, i] <- shuffled_indices[start_index:end_index]
  }
  blocs
  
  MSE <- NULL
  Pred=NULL
  RMSE=NULL
  for (i in 1:num_blocks) {
    data1.app <- tiga_f[-blocs[,i], ] 
    data1.test <- tiga_f[blocs[,i],]
    mod <-gam(
      AB_tot ~ Land_use + X_L93 + Y_L93 +
        Terre_fine_..2mm._g.kg +
        Argiles_.0_a_0.002_mm._g.kg +
        Limons_fins_.0.002_a_0.02_mm._g.kg +
        Limons_grossiers_.0.02_a_0.05_mm._g.kg +
        Sables_fins_.0.05_a_0.2_mm._g.kg +
        Sables_grossiers_.0.2_a_2.0_mm._g.kg +
        Carbone_.C._total_g.kg + pH +
        Matiere_organique_g.kg,
      data = data1.app
    )
    Pred <- predict(mod, newdata = data1.test)
    MSE[i] <- sum((Pred - data1.test$AB_tot)^2)
    RMSE[i]= sqrt(mean((data1.test$AB_tot - Pred)^2))
  }
  MSE
  mean(MSE)
  
  mean(RMSE)
}


set.seed(0)
dat <- gamSim(1,n=200)

library(mgcv)
# Fit the model
mod <- gam(y ~ s(x0, k = 5) + s(x1, k = 5) + s(x2, k = 5) + s(x3, k = 5),
           data = dat, method = "REML")

# Run the check function
gam.check(mod)



dat <- mgcv::gamSim(1,n=600, scale=0.6, verbose=FALSE)

library(mgcv)
# Fit the model
mod <- gam(y ~ s(x0, k = 3) + s(x1, k = 3) + s(x2, k = 3) + s(x3, k = 3),
           data = dat, method = "REML")

# Check the diagnostics
gam.check(mod)

# Refit to fix issues
mod2 <- gam(y ~ s(x0, k = 3) + s(x1, k = 3) + s(x2, k = 10) + s(x3, k = 3),
            data = dat, method = "REML")

# Check the new model
gam.check(mod2)




set.seed(0)
data("mpg", package="gamair", verbose=FALSE)

library(mgcv)
# Fit the model
mod <- gam(hw.mpg ~ s(length) + s(width) + s(height) + s(weight),
           data = mpg, method = "REML")

# Check overall concurvity
concurvity(mod, full = TRUE)



set.seed(0)
data("mpg", package="gamair", verbose=FALSE)

library(mgcv)
# Fit the model
mod <- gam(hw.mpg ~ s(length) + s(width) + s(height) + s(weight),
           data = mpg, method = "REML")

# Check overall concurvity
concurvity(mod, full = FALSE)













library(tidyverse)
library(caret)
#install.packages("datarium")
#library(datarium)
# setting seed to generate a 
# reproducible random sampling
set.seed(125) 

# defining training control
# as cross-validation and 
# value of K equal to 10
train_control <- trainControl(method = "cv",
                              number = 10)

# training the model by assigning sales column
# as target variable and rest other column
# as independent variable

model <- train(AB_tot ~., data = tiga_c_AB_tot, 
               method = "lm",
               trControl = train_control)


# printing model performance metrics
# along with other details
print(model)
summary(model)


# Créer le boxplot
ggplot(data_df, aes(x = models, y = Valeurs)) +
  geom_boxplot() +geom_jitter()+
  labs(x = "models", y = "Abondance des vers de terre") +
  ggtitle("Boxplot de l'AB_tot et des valeurs prédictes (models sans interaction)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Charger le package ggplot2 s'il n'est pas déjà chargé
# install.packages("ggplot2")
library(ggplot2)
train <- na.omit(train)
# Convertir les variables continues en polynômes de degré 2
train$Terre_fine_poly <- poly(tiga_c_sansva$Terre_fine_..2mm._g.kg, degree = 2)
train$Limons_grossiers_poly <- poly(train$Limons_grossiers_.0.02_a_0.05_mm._g.kg, degree = 2)
train$Sables_fins_poly <- poly(train$Sables_fins_.0.05_a_0.2_mm._g.kg, degree = 2)
train$Sables_grossiers_poly <- poly(train$Sables_grossiers_.0.2_a_2.0_mm._g.kg, degree = 2)
train$Carbone_poly <- poly(train$Carbone_.C._total_g.kg, degree = 2)
train$Matiere_organique_poly <- poly(train$Matiere_organique_g.kg, degree = 2)
train$X_L93_poly <- poly(train$X_L93, degree = 2)

# Ajuster le modèle polynomial de degré 2
mod_AB2 <- lm(
  AB_tot ~ Land_use + Terre_fine_poly + Limons_grossiers_poly +
    Sables_fins_poly + Sables_grossiers_poly + Carbone_poly +
    Matiere_organique_poly + X_L93_poly,
  data = train
)

# Afficher un résumé du modèle
summary(mod_AB2)











hist(mod_full1$res)
ggdensity(tiga_c_sansva_noncor_minmax, x="AB_tot",fill = "lightgray", title = "Résidus") +
  stat_overlay_normal_density(color = "red", linetype = "dashed")

bestmod1 = lm(formula = AB_tot ~ Land_use + Sables_fins_.0.05_a_0.2_mm._g.kg + 
                Sables_grossiers_.0.2_a_2.0_mm._g.kg + X_L93, data = tiga_c_sansva_noncor_minmax)
summary(bestmod1)






mod_full_bm <- lm(
  BM_tot ~ Land_use +
    Terre_fine_..2mm._g.kg *
    Argiles_.0_a_0.002_mm._g.kg *
    Limons_fins_.0.002_a_0.02_mm._g.kg *
    Limons_grossiers_.0.02_a_0.05_mm._g.kg *
    Sables_fins_.0.05_a_0.2_mm._g.kg *
    Sables_grossiers_.0.2_a_2.0_mm._g.kg *
    Carbone_.C._total_g.kg *
    Matiere_organique_g.kg +X_L93+Y_L93,
  data = tiga_c_sansva_noncor_minmax
)
plot(mod_full_bm)
plotresid(mod_full_bm)
stepAIC(mod_full_bm)
summary(mod_full_bm) # un R2 de 12%
bestmod_bm = lm(
  formula = BM_tot ~ Land_use + Terre_fine_..2mm._g.kg +
    Sables_fins_.0.05_a_0.2_mm._g.kg,
  data = tiga_acp
)
summary(bestmod_bm)




mod_full_richesse <- lm(
  Richesse ~ Land_use +
    Terre_fine_..2mm._g.kg *
    Argiles_.0_a_0.002_mm._g.kg *
    Limons_fins_.0.002_a_0.02_mm._g.kg *
    Limons_grossiers_.0.02_a_0.05_mm._g.kg *
    Sables_fins_.0.05_a_0.2_mm._g.kg *
    Sables_grossiers_.0.2_a_2.0_mm._g.kg *
    Carbone_.C._total_g.kg *
    Matiere_organique_g.kg +X_L93+Y_L93,
  data = tiga_c_sansva_noncor_minmax
)

plot(mod_full_richesse)
plotresid(mod_full_richesse)
stepAIC(mod_full_richesse)
summary(mod_full_richesse) # un R2 de 18%

bestmod_richesse = lm(
  formula = Richesse ~ Land_use + 
    Terre_fine_..2mm._g.kg + 
    Limons_fins_.0.002_a_0.02_mm._g.kg +
    Sables_fins_.0.05_a_0.2_mm._g.kg,
  data = tiga_acp
)
summary(bestmod_richesse)





# Obtenir le R² ajusté pour chaque variable en utilisant un modèle sans la variable
variables_of_interest <- c("Land_use", "Terre_fine_..2mm._g.kg", "Argiles_.0_a_0.002_mm._g.kg",
                           "Limons_fins_.0.002_a_0.02_mm._g.kg", "Limons_grossiers_.0.02_a_0.05_mm._g.kg",
                           "Sables_fins_.0.05_a_0.2_mm._g.kg", "Sables_grossiers_.0.2_a_2.0_mm._g.kg",
                           "Carbone_.C._total_g.kg", "Matiere_organique_g.kg", "X_L93", "Y_L93")

# Liste pour stocker les R² ajustés, AICc et BIC de chaque variable
R_squared_adj_variables <- vector("numeric", length = length(variables_of_interest))
AICc_variables <- vector("numeric", length = length(variables_of_interest))
BIC_variables <- vector("numeric", length = length(variables_of_interest))

# Nombre total d'observations
n <- nrow(tiga_c_sansva_noncor_minmax)

# Boucle pour ajuster un modèle pour chaque variable
for (i in seq_along(variables_of_interest)) {
  formula <- as.formula(paste("AB_tot ~", variables_of_interest[i]))
  model <- lm(formula, data = tiga_c_sansva_noncor_minmax)
  R_squared_adj_variables[i] <- summary(model)$adj.r.squared
  
  # Calculer l'AICc
  residuals <- model$residuals
  k <- length(coefficients(model)) - 1  # Nombre de coefficients (paramètres estimés) dans le modèle
  AICc_variables[i] <- AIC(model) + (2 * k * (k + 1)) / (n - k - 1)
  
  # Calculer le BIC
  BIC_variables[i] <- log(n) * k - 2 * logLik(model)
}

# Afficher les R² ajustés, AICc et BIC pour chaque variable
result <- data.frame(Variable = variables_of_interest, 
                     R_squared_adj = R_squared_adj_variables, 
                     AICc = AICc_variables,
                     BIC = BIC_variables)
print(result)



