
chemin_repertoire <- "C:/Users/diall/OneDrive/Bureau/M2_MODE/stage_abdou_m2/R_Stage_M2/cartographie"
fichiers <- list.files(chemin_repertoire, full.names = TRUE, recursive = TRUE)
taille_max_Mo <- 100
fichiers_grands <- fichiers[file.info(fichiers)$size > taille_max_Mo * 1024^2]
cat("Fichiers ayant une taille supérieure à", fichiers_grands, "Mo :/n")




















df_AB_tot_all <- data.frame(
  Latitude = c(45.6, 45.8, 47.6),
  Longitude = c(6.35, 6.37, 6.30)
)

AB_tot_mesuree <- data.frame(
  Latitude = c(45.7, 47.6),
  Longitude = c(6.32, 6.30),
  Abundance = c(100, 200)  # Exemple de colonne Abundance
)

# Fusionnez les dataframes en utilisant les colonnes Latitude et Longitude
df_merged <- merge(df_AB_tot_all, AB_tot_mesuree, by = c("Latitude", "Longitude"))






#grille régulière couvrant la zone géographique d'intérêt
res <- 0.008333333  # Résolution de la grille en degrés
grid <- expand.grid(x = seq(from = -5.141277, to = 8.232497, by = res),
                    y = seq(from = 42.33292, to = 51.08899, by = res))
coordinates(grid) <- ~x + y
gridded(grid) <- TRUE

# df = grid
# df = as.data.frame(df)
# leaflet(df) %>%
#   addTiles() %>%
#   addCircleMarkers(lng = ~x, lat = ~y, radius = 0.8, fillOpacity = 0.8, fillColor = "blue")
# 
# df = grid
# df = as.data.frame(df)
# df$n = 1:nrow(df)
# mapview::mapview(df, main = "Echantillonnage",xcol = "x",ycol ="y", zcol = "n") 
# mapview::mapview(Richness_raster, main = "Richness", maxpixels  = 2001670)
# mapView(df)



predicted <-  rnorm(50)
observed <- rnorm(10)
apply(predicted, 2, postResample, obs = observed)

caret::postResample(observed,predicted)

df = data.frame(observed = observed, predicted = predicted)
res <- rms::lrm(observed  ~ predicted  , data = df, x= TRUE, y = TRUE)

cor(df$observed, df$predicted)^2


# Sélectionnez les variables catégorielles (de type booléen)
df = landworm_explo_non_t [,c(9:18)]
df = drop_na(df)
categorical_vars <- c("clcm_lvl3")  
categorical_sum <- summary(df[, categorical_vars])


continuous_vars <- names(df[,-8])
continuous_percentiles <- apply(df[, continuous_vars], 2, function(x) quantile(x, probs = c(0.05, 0.5, 0.95)))


tableau_x <- data.frame(
  Variable = c(continuous_vars),
  # Description = c("Description de var1", "Description de var2", "Description de var3", "Description de var4", "Description de var5", "Description de var6"),
  # Sum = c(categorical_sum, NA, NA, NA),
  p_0.05 = continuous_percentiles[1, ],
  p_0.5 = continuous_percentiles[2, ],
  p_0.95 = continuous_percentiles[3, ]
)











set.seed(1)
x <- data.frame(x1=gl(53, 10), x2=runif(530), y=rnorm(530))

(rf1 <- randomForest(x[-3], x[[3]], ntree=10))
rf1$importance




df = bdd_occu
dim(bdd_sp)
dim(df)
all (df$ID == bdd_sp$ID)

# 
# df$AB_tot = bdd_sp$AB_tot
# rowSums(df[,3:23],na.rm = TRUE)

# df_re = df

# for ( i in names(df_re[,3:23])) {
#   df_re[[i]] = df_re[[i]]/df_re$AB_tot *100
#   df_re[[i]] = round (df_re[[i]],3)
# }
# df_re$AB_tot_c = NULL
# 
# names(df_re)
# df_re[df_re$ID == "AF_BE_2014_ATE_NA",]


# 1
df_long <- df %>%
  pivot_longer(cols = -c(ID, clcm_lvl3),  
               names_to = "Species",  
               values_to = "Abundance")  
# df_long %>% datatable(options = list(pageLength = 10))


# 2
df_long_mean <- df_long %>%
  group_by(clcm_lvl3, Species) %>%
  summarise(abundance_mean = mean(Abundance, na.rm = TRUE))
# df_long_mean %>% datatable(options = list(pageLength = 10))

df_tot <- df_long_mean %>%
  group_by(Species) %>%
  summarise(abundance_mean_tot = mean(abundance_mean, na.rm = TRUE))

df_long_mean_pour_tot <- bind_rows(df_long_mean_pour, 
                               df_tot %>% 
                                 mutate(clcm_lvl3 = "Tot") %>%
                                 select(clcm_lvl3, Species, abundance_mean_pourc))


# 3
df_long_mean_pour <- df_long_mean %>%
  group_by(clcm_lvl3) %>%
  mutate(abundance_mean_pourc = abundance_mean / sum(abundance_mean, na.rm=TRUE))
df_long_mean_pour$abundance_mean_pourc = df_long_mean_pour$abundance_mean_pourc*100
df_long_mean_pour$abundance_mean_pourc = round(df_long_mean_pour$abundance_mean_pourc,3)
# df_long_mean_pour %>% datatable(options = list(pageLength = 10))




f = df_long[df_long$clcm_lvl3 =="f",]
f = droplevels(f)
summary(f)

sum(f$Abundance, na.rm = TRUE)
levels(df_long$clcm_lvl3)

df_long_mean_pour <- df_long_mean_pour %>%
  mutate(clcm_lvl3 = recode(clcm_lvl3,
                            "f" = "Forest",
                            "ng" = "Natural /n grasslands",
                            "p" = "Pastures",
                            "nial" = "Non irrigated /n arable land",
                            "v" = "Vineyards",
                            "gua" = "Green urban/n areas",
                            # "Tot" = "Total",
  ))

df_long_mean_pour = as.data.frame(df_long_mean_pour)
df_long_mean_pour$abundance_mean_pourc = round(df_long_mean_pour$abundance_mean_pourc,3)
g = ggplot(df_long_mean_pour, aes(x = clcm_lvl3 , y = abundance_mean_pourc, fill = Species)) +
  geom_bar(stat = "identity", position = "fill") +
  labs(x = "Occupation du sol", y = "Abondance relative", 
       title = "Occurrence taxonomique par occupation du sol") +
  theme_minimal() +
  scale_fill_viridis_d() +
  theme(panel.background=element_rect(fill="grey90"), # #7B7B7B15
        axis.title=element_text(face="bold"), # Mettre en gras axis
        axis.text.x=element_text(size=14, face="bold", color="black"),
        axis.text.y=element_text(size=14, face="bold", color="black"),
        axis.title.y=element_text(face="bold", vjust=1.5 ,size=15),
        axis.title.x=element_text(face="bold", hjust=0.5 ,size=15,vjust=-20),
        panel.grid.major=element_line(size=1.2, color="white"), # Augmenter taille ligne y
        legend.title=element_blank(),
        legend.text=element_text(size=12),
        plot.margin=unit(c(1, 1, 1.5, 1), "cm"), # (top, right, bottom, left)
        legend.position="right" ,
        plot.title = element_text(face = "bold", size = 15)) 

ggplotly(g)



# PPD ---------------------------------------------------------------------


install.packages("randomForest")
library(randomForest)

set.seed(123)
df <- data.frame(
  var_cat = sample(c("A", "B", "C"), 100, replace = TRUE),
  var_cont1 = runif(100, 0, 10),
  var_cont2 = runif(100, 0, 10),
  target = sample(c(0, 1), 100, replace = TRUE)
)
plot(df$target)
df$target = as.factor(df$target)
model <- randomForest(target ~ var_cat + var_cont1 + var_cont2, data = df,
                      ntree = 1000, importance = T)

print(model)
imp = importance(model)
plot(model)
plot(importance(model))

varImpPlot(model)

library(pdp)
partial_plot <- partial(model, pred.var = "var_cont2")
plot(partial_plot)
partialPlot(model, df, "var_cont1", plot = TRUE)

library(vip)
vip(model)



# iml ---------------------------------------------------------------------
# https://christophm.github.io/interpretable-ml-book/pdp.html # 8.1
# https://cran.r-project.org/web/packages/iml/vignettes/intro.html

#Ajustement du modèle d'apprentissage automatique
set.seed(42)
library("iml")
library("randomForest")
data("Boston", package = "MASS")
rf <- randomForest(medv ~ ., data = Boston, ntree = 50)

# Utilisation du conteneur iml Predictor()
X <- Boston[which(names(Boston) != "medv")]
predictor <- Predictor$new(rf, data = X, y = Boston$medv)

# Importance des fonctionnalités
imp <- FeatureImp$new(predictor, loss = "mae") # mean absolute error
# imp <- FeatureImp$new(predictor, loss = "mse") # mean squared error  
library("ggplot2")
plot(imp)
imp$results

# Effets de fonctionnalités
ale <- FeatureEffect$new(predictor, feature = "lstat") # uniquement lstat
ale$plot()
ale$set.feature("rm")
ale$plot()
effs <- FeatureEffects$new(predictor) # toutes les variables
plot(effs)

# Mesurer les interactions
interact <- Interaction$new(predictor)
plot(interact)
interact <- Interaction$new(predictor, feature = "crim")
plot(interact)



#_______________
AB_tot_train = read.csv2("datas/AB_tot_train.csv")
AB_tot_var = names(AB_tot_train)
AB_tot_var_sup=c("bio7","bio6", "bio4", "elevation","sand","hurs_mean", "bio15", "CEC","P", "gdd0")

AB_tot_var[!AB_tot_var %in% AB_tot_var_sup]

AB_tot_predictors = c("AB_tot","CaCO3","gps_x","N","bio3","gps_y","bio18","clay",
                      "silt","CN","clcm_lvl3")

AB_tot_predictors = c("AB_tot","CaCO3","gps_x","N","bio3","gps_y","bio18","argile.0_30",
                      "limon.0_30","CN","clcm_lvl3")

AB_tot_df = bdd[,AB_tot_predictors]
AB_tot_df <- AB_tot_df %>% 
  rename(clay = argile.0_30)
AB_tot_df <- AB_tot_df %>% 
  rename(silt =limon.0_30 )
colSums(is.na(AB_tot_df))
AB_tot_df = drop_na(AB_tot_df)
AB_tot_df$clcm_lvl3 = as.factor(AB_tot_df$clcm_lvl3)
summary(AB_tot_df$clcm_lvl3)
levels(AB_tot_df$clcm_lvl3)[levels(AB_tot_df$clcm_lvl3) == "Broad-leaved forest"] <- "Forest"
levels(AB_tot_df$clcm_lvl3)[levels(AB_tot_df$clcm_lvl3) == "Coniferous forest"] <- "Forest"
levels(AB_tot_df$clcm_lvl3)[levels(AB_tot_df$clcm_lvl3) == "Mixed forest"] <- "Forest"
AB_tot_df$clcm_lvl3= as.factor(AB_tot_df$clcm_lvl3)
cl_original <- levels(AB_tot_df$clcm_lvl3)
new_cl <- c("f","gua", "ng", "nial", "p", "v")
AB_tot_df$clcm_lvl3 <- factor(AB_tot_df$clcm_lvl3, levels = cl_original, labels = new_cl)



rf <- randomForest(AB_tot ~ ., data = AB_tot_df, ntree = 500)

# Utilisation du conteneur iml Predictor()
X <- AB_tot_df[which(names(AB_tot_df) != "AB_tot")]
predictor <- Predictor$new(rf, data = X, y = AB_tot_df$AB_tot)

# Importance des fonctionnalités
imp <- FeatureImp$new(predictor, loss = "mae") # mean absolute error
imp <- FeatureImp$new(predictor, loss = "mse") # mean squared error  
library("ggplot2")
plot(imp)
imp$results
importance_rf <- as.data.frame(importance(rf))
importance_rf$nom=rownames(importance_rf)
importance_rf <- importance_rf[order(importance_rf$IncNodePurity,decreasing = TRUE), ]
row.names(importance_rf)=NULL
importance_rf$percent <- importance_rf$IncNodePurity / sum(importance_rf$IncNodePurity)*100
barplot(importance_rf$percent, main = "Importance of variables for total abundance", names.arg = importance_rf$nom, ylab = "Importance (%)", las = 2)

# Effets de fonctionnalités
# Les effets locaux accumulés décrivent comment les caractéristiques influencent en moyenne la prédiction d'un modèle d'apprentissage automatique.
ale <- FeatureEffect$new(predictor, feature = "gps_x") # uniquement lstat
ale$plot()
# ale$set.feature("rm")
# ale$plot()
effs <- FeatureEffects$new(predictor) # toutes les variables
plot(effs)

# Mesurer les interactions
interact <- Interaction$new(predictor)
plot(interact)
# interact <- Interaction$new(predictor, feature = "crim")
# plot(interact)



# Matrice decorrelation entre 20 sp et les 20 predictors
# Ajout des regions sur les ACP



# Ajout regions sur les ACP -----------------------------------------------



get_region <- function(lat, lon) {
  # Seuils de latitude et de longitude pour chaque région
  bretagne_lat_min <- 47.0
  bretagne_lat_max <- 48.5
  bretagne_lon_min <- -5.0
  bretagne_lon_max <- -2.0
  
  ile_de_france_lat_min <- 48.5
  ile_de_france_lat_max <- 49.5
  ile_de_france_lon_min <- 1.5
  ile_de_france_lon_max <- 3.5
  
  bourgogne_lat_min <- 46.5
  bourgogne_lat_max <- 48.0
  bourgogne_lon_min <- 3.0
  bourgogne_lon_max <- 5.0
  
  occitanie_lat_min <- 42.0
  occitanie_lat_max <- 44.5
  occitanie_lon_min <- 1.0
  occitanie_lon_max <- 4.0
  
  # Vérifiez les coordonnées et attribuez la région
  if (lat >= bretagne_lat_min && lat <= bretagne_lat_max &&
      lon >= bretagne_lon_min && lon <= bretagne_lon_max) {
    return("Bretagne")
  } else if (lat >= ile_de_france_lat_min && lat <= ile_de_france_lat_max &&
             lon >= ile_de_france_lon_min && lon <= ile_de_france_lon_max) {
    return("Île-de-France")
  } else if (lat >= bourgogne_lat_min && lat <= bourgogne_lat_max &&
             lon >= bourgogne_lon_min && lon <= bourgogne_lon_max) {
    return("Bourgogne")
  } else if (lat >= occitanie_lat_min && lat <= occitanie_lat_max &&
             lon >= occitanie_lon_min && lon <= occitanie_lon_max) {
    return("Occitanie")
  } else {
    return("Autres")
  }
}


latitude <- 48.6
longitude <- 2.5
region_attribuee <- get_region(latitude, longitude)
cat("La région attribuée aux coordonnées (", latitude, ", ", longitude, ") est :", region_attribuee, "/n")



df_region = bdd
df_region$gps_x = as.numeric(df_region$gps_x)
df_region$gps_y = as.numeric(df_region$gps_y)
df_region <- df_region %>%
  mutate(regions = sapply(1:nrow(df_region), function(i) get_region(df_region$gps_x[i], df_region$gps_y[i])))
names(df_region)
df_region$regions = as.factor(df_region$regions)

levels(df_region$regions)



# occu --------------------------------------------------------------------



value1 = c(20,20,25,25,40,35,30,20,35)
hist(value1,col = "red")


value1 = c(20,20,25,25,40,35,30,20,35)
value2 = c(15,25,30,25,25,20,40,40,40)

hist(value1,col = "red")
hist(value2, add = T, col = "blue")


ggplot(data=iris, aes(x=Sepal.Width,fill = Species)) + geom_histogram()


ggplot(data=df_long, aes(x=clcm_lvl3, y=Pourcentage,fill = Species)) +
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
  geom_bar( stat="identity", width=0.75, bg="grey50")+ # Barplot
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



