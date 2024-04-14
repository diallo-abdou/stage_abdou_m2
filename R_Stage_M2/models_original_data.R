test_grub <- function(data, variable, direction = "maxi") {
  
  if (direction == "maxi") { 
    repeat {
      # Effectuer le test de Grubbs
      test_aberrant <- grubbs.test(data[[variable]], opposite = FALSE)
      
      # Obtenir la p-valeur du test
      p.value <- test_aberrant$p.value
      # Si la p-valeur est inférieure au seuil de 0.05, on supprime la valeur aberrante
      if (p.value < 0.05) {
        max_value <- max(data[[variable]],na.rm=TRUE)
        data <- subset(data, data[[variable]] != max_value | is.na(data[[variable]]))
      } else {
        # S'il n'y a plus de valeurs aberrantes, sortir de la boucle
        break
      }
    }
  }
  
  
  if (direction == "mini") { 
    repeat {
      test_aberrant <- grubbs.test(data[[variable]], opposite = TRUE)
      # Obtenir la p-valeur du test
      p.value <- test_aberrant$p.value
      # Si la p-valeur est inférieure au seuil de 0.05, on supprime la valeur aberrante
      if (p.value < 0.05) {
        min_value <- min(data[[variable]],na.rm=TRUE)
        data <- subset(data, data[[variable]] != min_value | is.na(data[[variable]]))
      } else {
        # S'il n'y a plus de valeurs aberrantes, sortir de la boucle
        break
      }
    }
  }
  
  
  return(data)
}


chemin_fichier_excel = "C:/Users/diall/Downloads/datas/LandWorm_dataset_site_V1.9.xlsx"
bdd1.9 <- read.xlsx(chemin_fichier_excel, sheet = "Sheet1")

col = c("ID", "Programme", "Annee", "ID_Site", "Protocole", "AB_tot", "BM_tot",
        "clcm_lvl2", "clcm_lvl3", "gps_x", "gps_y", "clay", "fine_sand", "coarse_sand",
        "fine_silt", "coarse_silt", "ph_eau", "om", "sand", "silt", "n_tot", "c_org",
        "Altitude", "c_tot")

bdd_select <- bdd1.9 %>% select(all_of(col))

dim(bdd_select)
colSums(is.na(bdd_select))

bdd_select$Protocole = as.factor(bdd_select$Protocole)
select_protocole =c("F_HS", "HS","hand sorting")
bdd_select <- bdd_select[bdd$Protocole %in% select_protocole, ]
bdd_select=droplevels(bdd_select)

bdd_select <- bdd_select %>%filter(!is.na(gps_x) & !is.na(gps_y))
bdd_select <- bdd_select %>%filter(!is.na(ph_eau))
bdd_select <- bdd_select %>%filter(!is.na(om))
bdd_select <- bdd_select %>%filter(!is.na(clay))
bdd_select <- bdd_select %>%filter(!is.na(Annee))
bdd_select <- bdd_select %>%filter(!is.na(fine_sand))
bdd_select <- bdd_select %>%filter(!is.na(n_tot))


# bdd_select <- bdd_select %>%filter(!is.na(clcm_lvl2))
bdd_select$clcm_lvl2 = as.factor(bdd_select$clcm_lvl2)
summary(bdd_select$clcm_lvl2)


bdd_select$clcm_lvl3 = as.factor(bdd_select$clcm_lvl3)
summary(bdd_select$clcm_lvl3)


# bdd_select %>%
#   group_by(clcm_lvl2, clcm_lvl3) %>%
#   summarise(count = n())



select_os= c("Arable land", "Artificial, non-agricultural vegetated areas","Forests","Pastures","Permanent crops" )
bdd_select <- bdd_select[bdd_select$clcm_lvl2 %in% select_os, ]
bdd_select=droplevels(bdd_select)




bdd_select <- bdd_select %>%
  select(-sand, -silt, -c_org, -Altitude, -c_tot,-clcm_lvl3)
dim(bdd_select)
colSums(is.na(bdd_select))



# # calcule de la richesse ------------------------------------------------

colonnes_na <- colnames(bdd1.9)[colSums(is.na(bdd1.9)) == nrow(bdd1.9)]
bdd1.9 <- bdd1.9[, !colnames(bdd1.9) %in% colonnes_na]
colonnes_numeriques <- sapply(bdd1.9, is.numeric)
somme_colonnes_numeriques <- colSums(bdd1.9[, colonnes_numeriques],na.rm=TRUE)
colonnes_zeros <- names(somme_colonnes_numeriques[somme_colonnes_numeriques == 0])
bdd1.9 <- bdd1.9[, !colnames(bdd1.9) %in% colonnes_zeros]
colonnes_AB <- grep("^AB_", names(bdd1.9), value = TRUE)
ab_supprimee =  c("AB_AD","AB_JV","AB_SA","AB_STAD_X","AB_indéterminable","AB_Indéterminable","AB_indéterminable_endogeic","AB_tot","AB_Indéterminable_epigeic","AB_indéterminable_endogeic","AB_Ep.X","AB_vide", "AB_Ep.X1","AB_Ep.X2","AB_A.X","AB_Adult","AB_cocon","AB_indéterminé","AB_Juvenile","AB_Sub.adult","AB_Indéterminé","AB_Lumbricidae")
colonnes_AB <- colonnes_AB[!colonnes_AB %in% ab_supprimee]
bdd1.9$Richesse_tot <- 0
bdd1.9$Richesse_tot <- rowSums(!is.na(bdd1.9[colonnes_AB]) & bdd1.9[colonnes_AB] != 0)
#sum (is.na(bdd1.9$Richesse_tot) )
#summary(bdd1.9$Richesse_tot)

rich = bdd1.9[, c("ID","Richesse_tot")] 


rows_not_in_df_fusion <- anti_join(bdd_select, rich, by = "ID")
merged_df <- merge(bdd_select, rich, by = "ID")
ids_not_matching <- anti_join( merged_df,bdd_select, by = "ID")
bdd_select = merged_df


bdd_select <- bdd_select %>%
  select(-ID, -Programme, -Annee, -ID_Site, -Protocole)





# nettoyage ---------------------------------------------------------------
# 
# Liste des colonnes à nettoyer
columns_to_clean <- c(names(bdd_select))
# Produire le code pour chaque colonne
for (col in columns_to_clean) {
  cat(paste0("  ## ", col, "\n"))
  cat(paste0("df_cleaned = bdd_select\n"))
  cat(paste0("df_cleaned$", col, " = as.numeric(df_cleaned$", col, ")\n"))
  cat(paste0("explo_num(nom_col = '", col, "', titre = '", col, "', df = df_cleaned)\n")) # pas ok
  cat(paste0("df_cleaned <- test_grub(df_cleaned, '", col, "', direction = 'maxi')\n"))
  cat(paste0("df_cleaned <- test_grub(df_cleaned, '", col, "', direction = 'mini')\n"))
  cat(paste0("explo_num(nom_col = '", col, "', titre = '", col, "', df = df_cleaned)\n")) # ok
  cat(paste0("bdd_select = df_cleaned\n\n"))
}

## AB_tot
df_cleaned = bdd_select
df_cleaned$AB_tot = as.numeric(df_cleaned$AB_tot)
explo_num(nom_col = 'AB_tot', titre = 'AB_tot', df = df_cleaned)
df_cleaned <- test_grub(df_cleaned, 'AB_tot', direction = 'maxi')
df_cleaned <- test_grub(df_cleaned, 'AB_tot', direction = 'mini')
explo_num(nom_col = 'AB_tot', titre = 'AB_tot', df = df_cleaned)
bdd_select = df_cleaned

## BM_tot
df_cleaned = bdd_select
df_cleaned$BM_tot = as.numeric(df_cleaned$BM_tot)
explo_num(nom_col = 'BM_tot', titre = 'BM_tot', df = df_cleaned)
df_cleaned <- test_grub(df_cleaned, 'BM_tot', direction = 'maxi')
df_cleaned <- test_grub(df_cleaned, 'BM_tot', direction = 'mini')
explo_num(nom_col = 'BM_tot', titre = 'BM_tot', df = df_cleaned)
bdd_select = df_cleaned


## gps_x
df_cleaned = bdd_select
df_cleaned$gps_x = as.numeric(df_cleaned$gps_x)
explo_num(nom_col = 'gps_x', titre = 'gps_x', df = df_cleaned)
df_cleaned <- test_grub(df_cleaned, 'gps_x', direction = 'maxi')
df_cleaned <- test_grub(df_cleaned, 'gps_x', direction = 'mini')
explo_num(nom_col = 'gps_x', titre = 'gps_x', df = df_cleaned)
bdd_select = df_cleaned

## gps_y
df_cleaned = bdd_select
df_cleaned$gps_y = as.numeric(df_cleaned$gps_y)
explo_num(nom_col = 'gps_y', titre = 'gps_y', df = df_cleaned)
df_cleaned <- test_grub(df_cleaned, 'gps_y', direction = 'maxi')
df_cleaned <- test_grub(df_cleaned, 'gps_y', direction = 'mini')
explo_num(nom_col = 'gps_y', titre = 'gps_y', df = df_cleaned)
bdd_select = df_cleaned

## clay
df_cleaned = bdd_select
df_cleaned$clay = as.numeric(df_cleaned$clay)
explo_num(nom_col = 'clay', titre = 'clay', df = df_cleaned)
df_cleaned <- test_grub(df_cleaned, 'clay', direction = 'maxi')
df_cleaned <- test_grub(df_cleaned, 'clay', direction = 'mini')
explo_num(nom_col = 'clay', titre = 'clay', df = df_cleaned)
bdd_select = df_cleaned

## fine_sand
df_cleaned = bdd_select
df_cleaned$fine_sand = as.numeric(df_cleaned$fine_sand)
explo_num(nom_col = 'fine_sand', titre = 'fine_sand', df = df_cleaned)
df_cleaned <- test_grub(df_cleaned, 'fine_sand', direction = 'maxi')
df_cleaned <- test_grub(df_cleaned, 'fine_sand', direction = 'mini')
explo_num(nom_col = 'fine_sand', titre = 'fine_sand', df = df_cleaned)
bdd_select = df_cleaned

## coarse_sand
df_cleaned = bdd_select
df_cleaned$coarse_sand = as.numeric(df_cleaned$coarse_sand)
explo_num(nom_col = 'coarse_sand', titre = 'coarse_sand', df = df_cleaned)
df_cleaned <- test_grub(df_cleaned, 'coarse_sand', direction = 'maxi')
df_cleaned <- test_grub(df_cleaned, 'coarse_sand', direction = 'mini')
explo_num(nom_col = 'coarse_sand', titre = 'coarse_sand', df = df_cleaned)
bdd_select = df_cleaned

## fine_silt
df_cleaned = bdd_select
df_cleaned$fine_silt = as.numeric(df_cleaned$fine_silt)
explo_num(nom_col = 'fine_silt', titre = 'fine_silt', df = df_cleaned)
df_cleaned <- test_grub(df_cleaned, 'fine_silt', direction = 'maxi')
df_cleaned <- test_grub(df_cleaned, 'fine_silt', direction = 'mini')
explo_num(nom_col = 'fine_silt', titre = 'fine_silt', df = df_cleaned)
bdd_select = df_cleaned

## coarse_silt
df_cleaned = bdd_select
df_cleaned$coarse_silt = as.numeric(df_cleaned$coarse_silt)
explo_num(nom_col = 'coarse_silt', titre = 'coarse_silt', df = df_cleaned)
df_cleaned <- test_grub(df_cleaned, 'coarse_silt', direction = 'maxi')
df_cleaned <- test_grub(df_cleaned, 'coarse_silt', direction = 'mini')
explo_num(nom_col = 'coarse_silt', titre = 'coarse_silt', df = df_cleaned)
bdd_select = df_cleaned

## ph_eau
df_cleaned = bdd_select
df_cleaned$ph_eau = as.numeric(df_cleaned$ph_eau)
explo_num(nom_col = 'ph_eau', titre = 'ph_eau', df = df_cleaned)
# df_cleaned <- test_grub(df_cleaned, 'ph_eau', direction = 'maxi')
# df_cleaned = df_cleaned[!df_cleaned$ph_eau == 2.09,]
# df_cleaned = subset(df_cleaned, df_cleaned$ph_eau != 2.09 | is.na(df_cleaned$ph_eau))
df_cleaned <- test_grub(df_cleaned, 'ph_eau', direction = 'mini')
explo_num(nom_col = 'ph_eau', titre = 'ph_eau', df = df_cleaned)
bdd_select = df_cleaned

## om
df_cleaned = bdd_select
df_cleaned$om = as.numeric(df_cleaned$om)
explo_num(nom_col = 'om', titre = 'om', df = df_cleaned)
df_cleaned <- test_grub(df_cleaned, 'om', direction = 'maxi')
df_cleaned <- test_grub(df_cleaned, 'om', direction = 'mini')
explo_num(nom_col = 'om', titre = 'om', df = df_cleaned)
bdd_select = df_cleaned

## n_tot
df_cleaned = bdd_select
df_cleaned$n_tot = as.numeric(df_cleaned$n_tot)
explo_num(nom_col = 'n_tot', titre = 'n_tot', df = df_cleaned)
df_cleaned <- test_grub(df_cleaned, 'n_tot', direction = 'maxi')
df_cleaned <- test_grub(df_cleaned, 'n_tot', direction = 'mini')
explo_num(nom_col = 'n_tot', titre = 'n_tot', df = df_cleaned)
bdd_select = df_cleaned

## Richesse_tot
df_cleaned = bdd_select
df_cleaned$Richesse_tot = as.numeric(df_cleaned$Richesse_tot)
explo_num(nom_col = 'Richesse_tot', titre = 'Richesse_tot', df = df_cleaned)
df_cleaned <- test_grub(df_cleaned, 'Richesse_tot', direction = 'maxi')
df_cleaned <- test_grub(df_cleaned, 'Richesse_tot', direction = 'mini')
explo_num(nom_col = 'Richesse_tot', titre = 'Richesse_tot', df = df_cleaned)
bdd_select = df_cleaned


str(bdd_select)
summary(bdd_select)
colSums(is.na(bdd_select))

# # Standarization --------------------------------------------------------
shapiro.test(bdd_select$AB_tot)
x = as.numeric (bdd_select$AB_tot)+1
b <- boxcox(lm(x ~ 1),plotit = TRUE)
lambda1 <- b$x[which.max(b$y)]
# bdd_select$AB_tot = bdd_select$AB_tot/25
bdd_select$AB_tot = sqrt(bdd_select$AB_tot)

shapiro.test(bdd_select$BM_tot)
x = as.numeric (bdd_select$BM_tot)+1
b <- boxcox(lm(x ~ 1),plotit = TRUE)
lambda1 <- b$x[which.max(b$y)]
# bdd_select$BM_tot = bdd_select$BM_tot/25
bdd_select$BM_tot = sqrt(bdd_select$BM_tot)

shapiro.test(bdd_select$Richesse_tot)
x = as.numeric (bdd_select$Richesse_tot)+1
b <- boxcox(lm(x ~ 1),plotit = TRUE)
lambda1 <- b$x[which.max(b$y)]
bdd_select$Richesse_tot = sqrt(bdd_select$Richesse_tot)


col_num <- c("gps_x", "gps_y", "clay", "fine_sand", "coarse_sand", "fine_silt", "coarse_silt", "ph_eau", "om", "n_tot")
bdd_select[, col_num] <- scale(bdd_select[, col_num])

bdd_select = as.data.frame(bdd_select)
colSums(is.na(bdd_select))
summary(bdd_select)
str(bdd_select)


# interaction
# Sélectionner les colonnes numériques
col_num <- c("gps_x", "gps_y", "clay", "fine_sand", "coarse_sand", 
             "fine_silt", "coarse_silt", "ph_eau", "om", "n_tot")

interactions <- expand.grid(col_num, col_num)
interactions <- interactions[interactions$Var1 != interactions$Var2, ]
bdd_select_inter <- cbind(bdd_select, inter = apply(interactions, 1, 
                                                       function(x) bdd_select[[x[1]]] * bdd_select[[x[2]]]))



# AB_tot ------------------------------------------------------------------

AB_tot_bdd_select = bdd_select[, c("AB_tot", "clcm_lvl2","gps_x","gps_y","clay","fine_sand","coarse_sand",
                                   "fine_silt","coarse_silt","ph_eau","om","n_tot")]

summary(AB_tot_bdd_select)
# Partission
set.seed(1234)  # Pour rendre les résultats reproductibles
index <- createDataPartition(AB_tot_bdd_select$clcm_lvl2, p = 0.8, list = FALSE)

# Séparer les données en ensembles d'entraînement et de test
df_train <- AB_tot_bdd_select[index, ]  # Données d'entraînement
df_test <- AB_tot_bdd_select[-index, ]  # Données de test
df_train = droplevels(df_train)
df_test = droplevels(df_test)

stststs = ForetAlea(var_rep = "AB_tot", 
                    df_app =df_train, 
                    df_valid =df_test, 
                    mtry = 3,
                    ntree = 2000 , 
                    maxnodes = 80)
stststs$RMSE
stststs$R_adj_train
stststs$R_adj_test
stststs$MAE
stststs$model
stststs$predit

pred <- stststs$predit^2

RF_df_AB_tot = data.frame(Observed=df_test[,1]^2,Predicted = pred)

RF_AB_tot_fig = ggplot(RF_df_AB_tot, aes(x = Observed, y = Predicted)) +
  geom_point() + # Ajout des points
  geom_smooth(method = "lm", se = TRUE, color = "red") + 
  labs(subtitle =paste0(" RF: R² adj (train) = ", round(stststs$R_adj_train,2), 
                       "; \n R² adj (test) = ", round(stststs$R_adj_test,2),
                       "; \n RMSE = ",  round(stststs$RMSE^2,2)),
       x = "Abundance: real values", 
       y = "Predicted values") + 
  # theme(plot.title = element_text(size = 10)) +
  theme_classic()
ggsave("Results/original_data/RF_AB_tot_fig.png", plot = RF_AB_tot_fig,dpi = 300 ,width = 2.333333,height = 3)


RF_df_AB_tot$observation = seq(1,nrow(df_test))


df_tot = RF_df_AB_tot
# Calcul des quartiles
q1 <- quantile(df_tot$Observed, 0.25)
median <- quantile(df_tot$Observed, 0.50)
q3 <- quantile(df_tot$Observed, 0.75)
max_value <- max(df_tot$Observed)

# Création des DataFrames en fonction des quartiles
df1 <- df_tot[df_tot$Observed <= q1,]
df2 <- df_tot[df_tot$Observed > q1 & df_tot$Observed <= median,]
df3 <- df_tot[df_tot$Observed > median & df_tot$Observed <= q3,]
df4 <- df_tot[df_tot$Observed > q3,]

AB_tot_p1 = plot_comp(df = df1,ylabel = "",title_class = "  min to Q1",legende = TRUE,xlabel = "",title = "RF: Abundance predicted and observed values \n for different quartiles")

AB_tot_p2 = plot_comp(df = df2,ylabel = "" ,title_class = "Q1 to median",legende = FALSE,xlabel = "")
AB_tot_p3 = plot_comp(df = df3,ylabel = "" ,title_class = "median to Q3" ,legende = FALSE,xlabel = "")
AB_tot_p4 = plot_comp(df = df4,ylabel = "" ,title_class = " Q3 to max" ,legende = FALSE)


RF_AB_tot_p_o = ggarrange(AB_tot_p1, AB_tot_p2, AB_tot_p3, AB_tot_p4,
                          # labels = c('(a)', '(b)','(c)', '(d)'),
                          ncol = 1,vjust = 0.5,
                          common.legend = TRUE,
                          legend = 'right'
)

RF_AB_tot_fig
ggsave("Results/RF_AB_tot_p_o.png", plot = RF_AB_tot_p_o, dpi = 300,height = 8)

## fin 



# BM_tot ------------------------------------------------------------------

BM_tot_bdd_select = bdd_select[, c("BM_tot", "clcm_lvl2","gps_x","gps_y","clay","fine_sand","coarse_sand",
                                   "fine_silt","coarse_silt","ph_eau","om","n_tot")]
BM_tot_bdd_select = drop_na(BM_tot_bdd_select)
summary(BM_tot_bdd_select)
# BM_tot_bdd_select = BM_tot_bdd_select[!BM_tot_bdd_select$clcm_lvl2=="Forests",]
BM_tot_bdd_select = droplevels(BM_tot_bdd_select)
# Partission
set.seed(1234)  # Pour rendre les résultats reproductibles
index <- createDataPartition(BM_tot_bdd_select$clcm_lvl2, p = 0.8, list = FALSE)

# Séparer les données en ensembles d'entraînement et de test
df_train <- BM_tot_bdd_select[index, ]  # Données d'entraînement
df_test <- BM_tot_bdd_select[-index, ]  # Données de test
df_train = droplevels(df_train)
df_test = droplevels(df_test)

stststs = ForetAlea(var_rep = "BM_tot", 
                    df_app =df_train, 
                    df_valid =df_test, 
                    mtry = 3,
                    ntree = 2000 , 
                    maxnodes = 80)
stststs$RMSE
stststs$R_adj_train
stststs$R_adj_test
stststs$MAE
stststs$model
stststs$predit

pred <- stststs$predit^2

RF_df_BM_tot = data.frame(Observed=df_test[,1]^2,Predicted = pred)

RF_BM_tot_fig = ggplot(RF_df_BM_tot, aes(x = Observed, y = Predicted)) +
  geom_point() + # Ajout des points
  geom_smooth(method = "lm", se = TRUE, color = "red") + 
  labs(subtitle =paste0(" RF: R² adj (train) = ", round(stststs$R_adj_train,2), 
                       "; \n R² adj (test) = ", round(stststs$R_adj_test,2),
                       "; \n RMSE = ",  round(stststs$RMSE^2,2)),
       x = "Biomass: real values", 
       y = "Biomass: predicted values") + 
  theme_classic() 

ggsave("Results/original_data/RF_BM_tot_fig.png", plot = RF_BM_tot_fig, dpi = 300,width = 2.333333,height = 3)

RF_df_BM_tot$observation = seq(1,nrow(df_test))


df_tot = RF_df_BM_tot
# Calcul des quartiles
q1 <- quantile(df_tot$Observed, 0.25)
median <- quantile(df_tot$Observed, 0.50)
q3 <- quantile(df_tot$Observed, 0.75)
max_value <- max(df_tot$Observed)

# Création des DataFrames en fonction des quartiles
df1 <- df_tot[df_tot$Observed <= q1,]
df2 <- df_tot[df_tot$Observed > q1 & df_tot$Observed <= median,]
df3 <- df_tot[df_tot$Observed > median & df_tot$Observed <= q3,]
df4 <- df_tot[df_tot$Observed > q3,]

BM_tot_p1 = plot_comp(df = df1,ylabel = "",title_class = "  min to Q1",legende = TRUE,xlabel = "",title = "RF: Biomass predicted and observed values \n for different quartiles")

BM_tot_p2 = plot_comp(df = df2,ylabel = "" ,title_class = "Q1 to median",legende = FALSE,xlabel = "")
BM_tot_p3 = plot_comp(df = df3,ylabel = "" ,title_class = "median to Q3" ,legende = FALSE,xlabel = "")
BM_tot_p4 = plot_comp(df = df4,ylabel = "" ,title_class = " Q3 to max" ,legende = FALSE)


RF_BM_tot_p_o = ggarrange(BM_tot_p1, BM_tot_p2, BM_tot_p3, BM_tot_p4,
                          # labels = c('(a)', '(b)','(c)', '(d)'),
                          ncol = 1,vjust = 0.5,
                          common.legend = TRUE,
                          legend = 'right'
)

RF_BM_tot_p_o
ggsave("Results/RF_BM_tot_p_o.png", plot = RF_BM_tot_p_o, dpi = 300,height = 8)

## fin 




# Richesse_tot ------------------------------------------------------------------

Richesse_tot_bdd_select = bdd_select[, c("Richesse_tot", "clcm_lvl2","gps_x","gps_y","clay","fine_sand","coarse_sand",
                                         "fine_silt","coarse_silt","ph_eau","om","n_tot")]

summary(Richesse_tot_bdd_select)
# Partission
set.seed(1234)  # Pour rendre les résultats reproductibles
index <- createDataPartition(Richesse_tot_bdd_select$clcm_lvl2, p = 0.8, list = FALSE)

# Séparer les données en ensembles d'entraînement et de test
df_train <- Richesse_tot_bdd_select[index, ]  # Données d'entraînement
df_test <- Richesse_tot_bdd_select[-index, ]  # Données de test
df_train = droplevels(df_train)
df_test = droplevels(df_test)

stststs = ForetAlea(var_rep = "Richesse_tot", 
                    df_app =df_train, 
                    df_valid =df_test, 
                    mtry = 20,
                    ntree = 2000 , 
                    maxnodes = 80)
stststs$RMSE
stststs$R_adj_train
stststs$R_adj_test
stststs$MAE
stststs$model
stststs$predit

pred <- stststs$predit^2

RF_df_Richesse_tot = data.frame(Observed=df_test[,1]^2,Predicted = pred)

RF_Richesse_tot_fig = ggplot(RF_df_Richesse_tot, aes(x = Observed, y = Predicted)) +
  geom_point() + # Ajout des points
  geom_smooth(method = "lm", se = TRUE, color = "red") + 
  labs(subtitle =paste0(" RF: R² adj (train) = ", round(stststs$R_adj_train,2), 
                       "; \n R² adj (test) = ", round(stststs$R_adj_test,2),
                       "; \n RMSE = ",  round(stststs$RMSE^2,2)),
       x = "Richness: real values", 
       y = "Richness: predicted values") + 
  theme_classic() 

ggsave("Results/original_data/RF_Richesse_tot_fig.png", plot = RF_Richesse_tot_fig, dpi = 300,width = 2.333333,height = 3)

RF_df_Richesse_tot$observation = seq(1,nrow(df_test))


df_tot = RF_df_Richesse_tot
# Calcul des quartiles
q1 <- quantile(df_tot$Observed, 0.25)
median <- quantile(df_tot$Observed, 0.50)
q3 <- quantile(df_tot$Observed, 0.75)
max_value <- max(df_tot$Observed)

# Création des DataFrames en fonction des quartiles
df1 <- df_tot[df_tot$Observed <= q1,]
df2 <- df_tot[df_tot$Observed > q1 & df_tot$Observed <= median,]
df3 <- df_tot[df_tot$Observed > median & df_tot$Observed <= q3,]
df4 <- df_tot[df_tot$Observed > q3,]

Richesse_tot_p1 = plot_comp(df = df1,ylabel = "",title_class = "  min to Q1",legende = TRUE,xlabel = "",title = "RF: Richness predicted and observed values \n for different quartiles")

Richesse_tot_p2 = plot_comp(df = df2,ylabel = "" ,title_class = "Q1 to median",legende = FALSE,xlabel = "")
Richesse_tot_p3 = plot_comp(df = df3,ylabel = "" ,title_class = "median to Q3" ,legende = FALSE,xlabel = "")
Richesse_tot_p4 = plot_comp(df = df4,ylabel = "" ,title_class = " Q3 to max" ,legende = FALSE)


RF_Richesse_tot_p_o = ggarrange(Richesse_tot_p1, Richesse_tot_p2, Richesse_tot_p3, Richesse_tot_p4,
                                # labels = c('(a)', '(b)','(c)', '(d)'),
                                ncol = 1,vjust = 0.5,
                                common.legend = TRUE,
                                legend = 'right'
)

RF_Richesse_tot_p_o
ggsave("Results/RF_Richesse_tot_p_o.png", plot = RF_Richesse_tot_p_o, dpi = 300,height = 8)

## fin 



# All ---------------------------------------------------------------------
all_graphe_poly = ggarrange(RF_AB_tot_fig, RF_BM_tot_fig, RF_Richesse_tot_fig,
                            labels = c('(a)', '(b)','(c)'),ncol = 3,
                            common.legend = TRUE,
                            legend = 'right'
)
ggsave("Results/original_data/all_graphe_ori.png", plot = all_graphe_poly, dpi = 300,height = 3,width = 7)


