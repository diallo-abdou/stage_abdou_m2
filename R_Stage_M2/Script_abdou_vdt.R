
################################################################################
###                                                                          ###
###           Script stage Abdou model predictive                            ###
###                                                                          ###
################################################################################


# Chargment des packages ----------------------------------------------------------------








# Importation de la BDD KEVIN ---------------------------------------------------------------


#bdd = read.table("clipboard",header = TRUE,dec = ",",stringsAsFactors = TRUE)
chemin_fichier_excel = "C:/Users/diall/OneDrive/Bureau/M2_MODE/stage_abdou_m2/data/fusionPUBLI_v2022.11.01.xlsx"
# a cause de Onedrive
chemin_fichier_excel = "C:/Users/diall/Downloads/fusionPUBLI_v2022.11.01.xlsx"

bdd <- read.xlsx(chemin_fichier_excel, sheet = "fusionPUBLI_v2022.11.01_V2")
# avec read_xlsx
#bdd <- read_xlsx(chemin_fichier_excel, sheet = "fusionPUBLI_v2022.11.01_V2")


str(bdd)
summary(bdd)
names(bdd)
nrow(bdd)

# conversion des colonnes au bon format
col_en_factor = c("cle","Programme","Programme","ID_Site","Code_Parcelle","Annee",                    
              "Modalite","Bloc","Protocole","Code_Methode","Date_Prelevement",           
              "Code_Postal","Ville","Departement","Region","Nouv_Region",               
              "Categorie_Milieu_Niv1","SousCategorie_Milieu_Niv2", 
              "Details_Milieu_Niv3","Specificites_Parcelle_Niv4",
              "OS","OS_MS","Climat","Commentaire_META" )
bdd = conv_col(bdd, col_en_factor, "factor")
levels (bdd$OS)
levels (bdd$Climat)
calculer_pourcentage_NA(bdd,TRUE)
calculer_pourcentage_NA(bdd,FALSE)


bdd_simpl = bdd[, c(1:4, 9:11, 13, 17:23, 25:26, 45, 130, 131)]
names(bdd_simpl)
calculer_pourcentage_NA(bdd_simpl)
nrow(bdd_simpl)



bdd_simpl_sans_NA <- na.omit(bdd_simpl)
nrow(bdd_simpl_sans_NA)
calculer_pourcentage_NA(bdd_simpl_sans_NA)



complet = calculer_pourcentage_NA(bdd_simpl,
                                  afficher_zero_percent = TRUE)
bdd_simpl_complt = bdd[, complet$Variable[-13]]
names(bdd_simpl_complt)
nrow(bdd_simpl_complt)


gg_miss_var(bdd_simpl)
visdat::vis_miss(bdd_simpl)


# TIGA DIJON ----------------------------------------------------------------

tiga = read.table(
  "clipboard",
  header = TRUE,
  dec = ",",
  stringsAsFactors = TRUE
)
str(tiga)
nrow(tiga)
summary(tiga$Land_use)



calculer_pourcentage_NA(tiga)
names(tiga)
tiga_simpl = tiga[, c(1:114, 228:249)] # On enleve les variables des ETM
calculer_pourcentage_NA(tiga_simpl)
nrow(tiga_simpl)


tiga_simpl_sans_NA <- na.omit(tiga_simpl)
gg_miss_var(tiga_simpl)
nrow(tiga_simpl_sans_NA)
calculer_pourcentage_NA(tiga_simpl_sans_NA)




var_complt <- calculer_pourcentage_NA(tiga_simpl,
                                      afficher_zero_percent = TRUE)

var_complt$Variable
tiga_simpl_complt = tiga[, var_complt$Variable[-87]] # On selectionne uniquement les variables complets
nrow(tiga_simpl_complt)
colnames(tiga_simpl_complt)
str(tiga_simpl_complt)
calculer_pourcentage_NA(tiga_simpl_complt,
                        afficher_zero_percent = TRUE)






















# Nettoyage et préparation des données --------------------------------------------------

tiga_c = tiga_simpl_complt[, -c(85, 86)]


str(tiga_c)
names(tiga_c)


tiga_c <-
  conv_col(tiga_c, names(tiga_c [, c(8:81, 83:84)]), "numeric")
str(tiga_c)






# # Gestion des variables quantitatives -----------------------------------


# Connaitre le pourcentage de zero
zero_percentage <-
  (sum(tiga_c == 0, na.rm = TRUE) / sum(!is.na(tiga_c))) * 100
zero_percentage
# Les valeurs Zeo represente 47% des valeurs du jdd: donc pas de transformation log
# mais plutôt la transformation la racine quadratique tableau<-tableau^(1/4).


# Transformation des var expli quanti avec la methode de la racine quadratique (MH P144)
var_quanti <- names(tiga_c[, c(67:81)]) # var expli quanti
for (col in var_quanti) {
  tiga_c[[col]] <- tiga_c[[col]] ^ (1 / 4)
}

summary(tiga_c[, c(67:81)])


# Centree et reduire
var_quanti <- names(tiga_c[, c(67:81)]) # var expli quanti

for (col in var_quanti) {
  tiga_c[[col]] <- scale(tiga_c[[col]])
}
summary(tiga_c[, c(67:81)])




# # Gestion des variables qualitatives ------------------------------------

levels(tiga_c$Land_use)
str(tiga_c)
names(tiga_c)
var_quali <- tiga_c %>%
  select(c("Land_use"))

tiga_c <- dummy_cols(tiga_c, select_columns = c("Land_use"))

head(tiga_c[, 85:90])
names(tiga_c)




# # Exploration visuel ----------------------------------------------------

data_var = tiga_c[, c(67:81)]
str(data_var)

pairs(data_var)

col_names <- names(data_var)

# Boucle pour tracer tous les graphiques de combinaisons possibles de deux variables
combi <- c() # stocke les combinaisons pour les modeles apres
# Creer une liste vide pour stocker les graphiques
plots <- list()

# Boucle sur toutes les combinaisons de colonnes
for (i in 1:(length(col_names) - 1)) {
  for (j in (i + 1):length(col_names)) {
    # Creer le graphique et l'ajouter a la liste avec un nom descriptif
    p <- plot(
      data_var[, i],
      data_var[, j],
      main = paste(col_names[j], "vs", col_names[i]),
      xlab = col_names[i],
      ylab = col_names[j],
      col = ifelse(data_var[, i] == 0 |
                     data_var[, j] == 0, "red", "black")
    )
    plots[[paste(col_names[j], "vs", col_names[i])]] <- p
    combi <- c(combi, paste(col_names[i], ":", col_names[j]))
  }
}

combi





# # Vérifications des valeurs abérantes -----------------------------------


# Pour les vdt
verifier_valeurs_aberrantes_vdt(tiga_c, "AB_tot", opposite = FALSE)
verifier_valeurs_aberrantes_vdt(tiga_c, "AB_tot", opposite = TRUE)


verifier_valeurs_aberrantes_vdt(tiga_c, "BM_tot", opposite = FALSE)
verifier_valeurs_aberrantes_vdt(tiga_c, "BM_tot", opposite = TRUE)

verifier_valeurs_aberrantes_vdt(tiga_c, "Richesse", opposite = FALSE)
verifier_valeurs_aberrantes_vdt(tiga_c, "Richesse", opposite = TRUE)


# Pour les catégories écologies
for (i in colnames(tiga_c[, c(10:13, 15:18)])) {
  cat(paste0(
    "verifier_valeurs_aberrantes_vdt(tiga_c, '",
    i,
    "', opposite = FALSE)\n"
  ))
  cat(paste0(
    "verifier_valeurs_aberrantes_vdt(tiga_c, '",
    i,
    "', opposite = TRUE)\n\n"
  ))
}
verifier_valeurs_aberrantes_vdt(tiga_c, 'AB_EPI', opposite = FALSE)
verifier_valeurs_aberrantes_vdt(tiga_c, 'AB_EPI', opposite = TRUE)

verifier_valeurs_aberrantes_vdt(tiga_c, 'AB_EPA', opposite = FALSE)
verifier_valeurs_aberrantes_vdt(tiga_c, 'AB_EPA', opposite = TRUE)

verifier_valeurs_aberrantes_vdt(tiga_c, 'AB_ANS', opposite = FALSE)
verifier_valeurs_aberrantes_vdt(tiga_c, 'AB_ANS', opposite = TRUE)

verifier_valeurs_aberrantes_vdt(tiga_c, 'AB_END', opposite = FALSE)
verifier_valeurs_aberrantes_vdt(tiga_c, 'AB_END', opposite = TRUE)

verifier_valeurs_aberrantes_vdt(tiga_c, 'BM_EPI', opposite = FALSE)
verifier_valeurs_aberrantes_vdt(tiga_c, 'BM_EPI', opposite = TRUE)

verifier_valeurs_aberrantes_vdt(tiga_c, 'BM_EPA', opposite = FALSE)
verifier_valeurs_aberrantes_vdt(tiga_c, 'BM_EPA', opposite = TRUE)

verifier_valeurs_aberrantes_vdt(tiga_c, 'BM_ANS', opposite = FALSE)
verifier_valeurs_aberrantes_vdt(tiga_c, 'BM_ANS', opposite = TRUE)

verifier_valeurs_aberrantes_vdt(tiga_c, 'BM_END', opposite = FALSE)
verifier_valeurs_aberrantes_vdt(tiga_c, 'BM_END', opposite = TRUE)

# Pour les variables explicatives
for (i in colnames(tiga_c[, c(67:77)])) {
  cat(paste0(
    "verifier_valeurs_aberrantes(tiga_c, '",
    i,
    "', opposite = FALSE)\n"
  ))
  cat(paste0(
    "verifier_valeurs_aberrantes(tiga_c, '",
    i,
    "', opposite = TRUE)\n\n"
  ))
}


verifier_valeurs_aberrantes(tiga_c, 'Terre_fine_..2mm._g.kg', opposite = FALSE)
verifier_valeurs_aberrantes(tiga_c, 'Terre_fine_..2mm._g.kg', opposite = TRUE)

verifier_valeurs_aberrantes(tiga_c, 'Argiles_.0_a_0.002_mm._g.kg', opposite = FALSE)
verifier_valeurs_aberrantes(tiga_c, 'Argiles_.0_a_0.002_mm._g.kg', opposite = TRUE)

verifier_valeurs_aberrantes(tiga_c,
                            'Limons_fins_.0.002_a_0.02_mm._g.kg',
                            opposite = FALSE)
verifier_valeurs_aberrantes(tiga_c,
                            'Limons_fins_.0.002_a_0.02_mm._g.kg',
                            opposite = TRUE)

verifier_valeurs_aberrantes(tiga_c,
                            'Limons_grossiers_.0.02_a_0.05_mm._g.kg',
                            opposite = FALSE)
verifier_valeurs_aberrantes(tiga_c,
                            'Limons_grossiers_.0.02_a_0.05_mm._g.kg',
                            opposite = TRUE)

verifier_valeurs_aberrantes(tiga_c, 'Sables_fins_.0.05_a_0.2_mm._g.kg', opposite = FALSE)
verifier_valeurs_aberrantes(tiga_c, 'Sables_fins_.0.05_a_0.2_mm._g.kg', opposite = TRUE)

verifier_valeurs_aberrantes(tiga_c,
                            'Sables_grossiers_.0.2_a_2.0_mm._g.kg',
                            opposite = FALSE)
verifier_valeurs_aberrantes(tiga_c,
                            'Sables_grossiers_.0.2_a_2.0_mm._g.kg',
                            opposite = TRUE)

verifier_valeurs_aberrantes(tiga_c, 'Carbone_.C._total_g.kg', opposite = FALSE)
verifier_valeurs_aberrantes(tiga_c, 'Carbone_.C._total_g.kg', opposite = TRUE)

verifier_valeurs_aberrantes(tiga_c, 'C_N', opposite = FALSE)
verifier_valeurs_aberrantes(tiga_c, 'C_N', opposite = TRUE)

verifier_valeurs_aberrantes(tiga_c, 'Matiere_organique_g.kg', opposite = FALSE)
verifier_valeurs_aberrantes(tiga_c, 'Matiere_organique_g.kg', opposite = TRUE)

verifier_valeurs_aberrantes(tiga_c, 'pH', opposite = FALSE)
verifier_valeurs_aberrantes(tiga_c, 'pH', opposite = TRUE)

verifier_valeurs_aberrantes(tiga_c, 'Phosphore_.P._g.kg', opposite = FALSE)
verifier_valeurs_aberrantes(tiga_c, 'Phosphore_.P._g.kg', opposite = TRUE)





linges_va = c(328, 276, 159, 269, 218, 27, 230, 59)
#          -->  Data sans les valeurs abérant
tiga_c_sansva = tiga_c[-c(328, 276, 159, 269, 218, 27, 230, 59), ]
tiga_c_sansva = droplevels(tiga_c_sansva)
#view(tiga_c_sansva)


# # Verification des corrélations ------------------------------------------

names(tiga_c_sansva)
data_var = tiga_c_sansva[, c(67:81, 83:90)]
str(data_var)
summary(data_var)

var_corelee <- cor_function_seuil(data = data_var, seuil = 0.7)
var_corelee <- unique(var_corelee)
gardee <- c("Terre_fine_..2mm._g.kg",
            "Matiere_organique_g.kg",
            "Carbone_.C._total_g.kg")
#supprimer <- setdiff(var_corelee, gardee)
supprimer <- c(
  "Cailloux_..5mm._g.kg",
  "Gravier_.2_._5_mm._g.kg",
  "Azote_.N._total_g.kg",
  "Carbone_organique_total_.COT._g.kg"
)

# Enlever les colonnes correspondant aux éléments dans le vecteur supprimer
column_positions <- which(colnames(tiga_c_sansva) %in% supprimer)





# -->  Data sans les valeurs abérant et non corrélée
tiga_c_sansva_noncor <- tiga_c_sansva[,-column_positions]
names(tiga_c_sansva_noncor)
cor_function_seuil(data = tiga_c_sansva_noncor[, 67:77], seuil = 0.7)




# Selection des variables  via un ACP -------------------------------------------------


# Mise à l'échelle des données
names(tiga_c_sansva_noncor)

# -->  Data sans les valeurs abérant, non corrélée standard min max
tiga_c_sansva_noncor_minmax = tiga_c_sansva_noncor


preproc <-
  preProcess(tiga_c_sansva_noncor_minmax[, c(67:77, 79, 80)], method = c("range"))
scaled <-
  predict(preproc, tiga_c_sansva_noncor_minmax[, c(67:77, 79, 80)])

for (i in names(scaled)) {
  tiga_c_sansva_noncor_minmax[[i]] = scaled[[i]]
}
summary(tiga_c_sansva_noncor_minmax)


# Réalisation d'un ACP sur les colonne quantitatives

names(tiga_c_sansva_noncor_minmax)
data_acp = tiga_c_sansva_noncor_minmax[, c(67:77)]
names(data_acp)
colnames(data_acp)=c("Terre_f","Argiles","Limons_f","Limons_g","Sables_f",
                     "Sables_g","Carbone_tot","C_N","MO","pH","Phosphore")
acp0 <- PCA(data_acp, graph = FALSE)


## Choix du nombre d'axes
acp0$eig
fviz_eig(acp0, addlabels = TRUE) # on prend les trois premiers axes
contrib_axes <- acp0$var$contrib[, 1:2]  # 3 premiers axes
contrib_axes <- round(contrib_axes, 3)   # Plus facile a lire
fviz_contrib(acp0, choice = "var", axes = 1)
fviz_contrib(acp0, choice = "var", axes = 2)
fviz_contrib(acp0, choice = "var", axes = 3)

seuil <- 1 / ncol(data_acp) * 100
lignes_superieures <- rownames(contrib_axes)[apply(contrib_axes, 1,
                                                   function(x)
                                                     any(x >= seuil))]
lignes_superieures

#  **** En consderant les deux premiers axes:
#le Phosphore_.P._g.kg et le C_N et le pH ne sont pas trés influant





# coloree les variables selon leurs contributions aux axes
fviz_pca_var(
  axes = c(1, 2),
  acp0,
  col.var = "contrib",
  gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
  repel = TRUE
) # evite le chevauchement de texte


# Suppression de Limon fin

#Suppression de Argiles


coul = c("yellow", "green", "violet", "blue", "black", "red")
fviz_pca_ind(
  axes = c(1, 2),
  acp0,
  geom.ind = "text",
  pointshape = 21,
  pointsize = 2,
  palette = coul,
  addEllipses = TRUE,
  legend.title = "Land_use",
  fill.ind = tiga_c_sansva_noncor_minmax$Land_use
)
fviz_pca_ind(
  axes = c(1, 2),
  acp0,
  geom.ind = "point",
  pointshape = 21,
  pointsize = 2,
  palette = "viridis",
  addEllipses = TRUE,
  legend.title = "Land_use",
  fill.ind = tiga_c_sansva_noncor_minmax$Land_use
)


# Classification
class1 = HCPC(acp0)
plot(class1$call$t$inert.gain, type = "s")
#Je choisis finalement 4 groupes
class1 = HCPC(acp0, nb.clust = 4)
fviz_dend(class1)
fviz_cluster(class1)
fviz_cluster(class1, ellipse.type = "norm", ellipse.level = 0.8)    #Groupes



################################################################################
###                                                                          ###
###           Regression polynomiale                                         ###
###                                                                          ###
################################################################################


names(tiga_c_sansva_noncor_minmax)
view(tiga_c_sansva_noncor_minmax)

#tiga_acp=tiga_acp[,-c(77)] # On enleve

#Phosphore_.P._g.kg
#le pH
#C_N
#Limon fin
#Argiles_.0_a_0.002_mm._g.kg


# Séparation des données
set.seed(42)

sample <-
  sample.split(tiga_c_sansva_noncor_minmax, SplitRatio = 0.8)
train <- subset(tiga_c_sansva_noncor_minmax, sample == TRUE)
test <- subset(tiga_c_sansva_noncor_minmax, sample == FALSE)
print(dim(train))
print(dim(test))

train$Land_use=droplevels(train$Land_use)
train=na.omit(train)



## Pour AB_tot -------------------------------------------------------------

calculate_metrics(
  data = train,
  response_var = "AB_tot",
  predictor_vars = c(
    "Land_use",
    "Terre_fine_..2mm._g.kg",
    "Argiles_.0_a_0.002_mm._g.kg",
    "Limons_fins_.0.002_a_0.02_mm._g.kg",
    "Limons_grossiers_.0.02_a_0.05_mm._g.kg",
    "Sables_fins_.0.05_a_0.2_mm._g.kg",
    "Sables_grossiers_.0.2_a_2.0_mm._g.kg",
    "Carbone_.C._total_g.kg",
    "Matiere_organique_g.kg",
    "X_L93",
    "Y_L93"
  )
)

###  SANS INTERACTION
for (i in 1:6) {
  model_name <- paste0("poly_", i)
  
  # Fit the polynomial model for the current degree (i)
  result <-
    compare_mod_poly(
      var_reponse = "AB_tot",
      degre = i,
      data_train = train,
      data_test = test
    )
  assign(model_name, result$predict_values)  # Store the predicted values in a variable
  
  # Get the summary of the model and print it
  cat(
    paste0(
      "************************** Summary for degree ",
      i,
      " **************************",
      "\n"
    )
  )
  print(summary(result$model))
  cat("\n\n")
}


# Créer un data.frame à partir des trois vecteurs et du vecteur noms_objets
AB_tot_reel = test$AB_tot
data_df <-
  data.frame(
    models = rep(
      c(
        "AB_tot_reel",
        "poly_1",
        "poly_2",
        "poly_3",
        "poly_4",
        "poly_5",
        "poly_6",
        "RPRT"
      ),
      each = length(AB_tot_reel)
    ),
    Valeurs = c(AB_tot_reel, poly_1, poly_2, poly_3, poly_4, poly_5, poly_6, RPRT)
  )


analyse_stats(donnee = data_df,
              nom_var = "Valeurs",
              genotype = "models")
coul3 = c("#E69F00",
          "#1F77B4",
          "#7F7F7F",
          "#2CA02C",
          "#D62728",
          "#9467BD",
          "#F0E442",
          "black")
G1 = fsg(
  donnee = data_df,
  nom_var = "Valeurs",
  genotype = "models",
  x_label = "models",
  y_label = "Abondance des vers de terre",
  title = "Boxplot de AB_Tot (models sans interaction)",
  legend_title = "Models",
  couleurs = coul3,
  affiche_point = FALSE,
  lettre_postion = 0
)

G2 = fsg(
  donnee = data_df,
  nom_var = "Valeurs",
  genotype = "models",
  x_label = "models",
  y_label = "",
  title = "Boxplot de AB_Tot (models sans interaction)",
  legend_title = "Models",
  couleurs = coul3,
  affiche_point = TRUE,
  lettre_postion = 0
)
ggarrange(
  G1,
  G2,
  labels = c('(a)', '(b)'),
  common.legend = TRUE,
  legend = 'right'
)


# model avec les interactions
mod_AB2 <- lm(
  AB_tot ~ Land_use + X_L93 + Y_L93 +
    Terre_fine_..2mm._g.kg *
    Argiles_.0_a_0.002_mm._g.kg *
    Limons_fins_.0.002_a_0.02_mm._g.kg *
    Limons_grossiers_.0.02_a_0.05_mm._g.kg *
    Sables_fins_.0.05_a_0.2_mm._g.kg *
    Sables_grossiers_.0.2_a_2.0_mm._g.kg *
    Carbone_.C._total_g.kg *
    Matiere_organique_g.kg,
  data = train
)
summary(mod_AB2)

vif(mod_AB2)
plot(mod_AB2)
plotresid(mod_AB2)
stepAIC(mod_AB2)
summary(mod_AB2) # Adjusted R-squared:  0.2322
shapiro.test(mod_AB2$res)
Pred2 = predict(mod_AB2, newdata = test)

data_df <- data.frame(models = rep(c("AB_tot_reel", "Lm_2_I"),
                                   each = length(AB_tot_reel)),
                      Valeurs = c(AB_tot_reel, Pred2))

analyse_stats(donnee = data_df,
              nom_var = "Valeurs",
              genotype = "models")
coul3 = c("#E69F00",
          "#1F77B4",
          "#7F7F7F",
          "#2CA02C",
          "#D62728",
          "#9467BD",
          "#F0E442",
          "black")
fsg(
  donnee = data_df,
  nom_var = "Valeurs",
  genotype = "models",
  x_label = "models",
  y_label = "Abondance des vers de terre",
  title = "Boxplot de l'AB_Tot (models sans interaction)",
  legend_title = "geno",
  couleurs = coul3,
  affiche_point = TRUE,
  lettre_postion = 0
)






## Pour BM_tot -------------------------------------------------------------

calculate_metrics(
  data = train,
  response_var = "BM_tot",
  predictor_vars = c(
    "Land_use",
    "Terre_fine_..2mm._g.kg",
    "Argiles_.0_a_0.002_mm._g.kg",
    "Limons_fins_.0.002_a_0.02_mm._g.kg",
    "Limons_grossiers_.0.02_a_0.05_mm._g.kg",
    "Sables_fins_.0.05_a_0.2_mm._g.kg",
    "Sables_grossiers_.0.2_a_2.0_mm._g.kg",
    "Carbone_.C._total_g.kg",
    "Matiere_organique_g.kg",
    "X_L93",
    "Y_L93"
  )
)

###  SANS INTERACTION
for (i in 1:12) {
  model_name <- paste0("poly_bm", i)
  
  # Fit the polynomial model for the current degree (i)
  result <-
    compare_mod_poly(
      var_reponse = "BM_tot",
      degre = i,
      data_train = train,
      data_test = test
    )
  assign(model_name, result$predict_values)  # Store the predicted values in a variable
  
  # Get the summary of the model and print it
  cat(
    paste0(
      "************************** Summary for degree ",
      i,
      " **************************",
      "\n"
    )
  )
  print(summary(result$model))
  cat("\n\n")
}




## Pour Richesse -------------------------------------------------------------

calculate_metrics(
  data = train,
  response_var = "Richesse",
  predictor_vars = c(
    "Land_use",
    "Terre_fine_..2mm._g.kg",
    "Argiles_.0_a_0.002_mm._g.kg",
    "Limons_fins_.0.002_a_0.02_mm._g.kg",
    "Limons_grossiers_.0.02_a_0.05_mm._g.kg",
    "Sables_fins_.0.05_a_0.2_mm._g.kg",
    "Sables_grossiers_.0.2_a_2.0_mm._g.kg",
    "Carbone_.C._total_g.kg",
    "Matiere_organique_g.kg",
    "X_L93",
    "Y_L93"
  )
)

###  SANS INTERACTION
for (i in 1:6) {
  model_name <- paste0("poly_rich", i)
  
  # Fit the polynomial model for the current degree (i)
  result <-
    compare_mod_poly(
      var_reponse = "Richesse",
      degre = i,
      data_train = train,
      data_test = test
    )
  assign(model_name, result$predict_values)  # Store the predicted values in a variable
  
  # Get the summary of the model and print it
  cat(
    paste0(
      "************************** Summary for degree ",
      i,
      " **************************",
      "\n"
    )
  )
  print(summary(result$model))
  cat("\n\n")
}





# model avec les interactions
mod_BM2 <- lm(
  Richesse ~ Land_use + X_L93 + Y_L93 +
    Terre_fine_..2mm._g.kg *
    Argiles_.0_a_0.002_mm._g.kg *
    Limons_fins_.0.002_a_0.02_mm._g.kg *
    Limons_grossiers_.0.02_a_0.05_mm._g.kg *
    Sables_fins_.0.05_a_0.2_mm._g.kg *
    Sables_grossiers_.0.2_a_2.0_mm._g.kg *
    Carbone_.C._total_g.kg *
    Matiere_organique_g.kg,
  data = train
)
summary(mod_BM2)

vif(mod_BM2)
plot(mod_BM2)
plotresid(mod_BM2)
stepAIC(mod_BM2)
summary(mod_BM2) # Adjusted R-squared:  0.2322
shapiro.test(mod_BM2$res)
Pred2 = predict(mod_BM2, newdata = test)

data_df <- data.frame(models = rep(c("Richesse_reel", "Lm_2_I"),
                                   each = length(Richesse_reel)),
                      Valeurs = c(Richesse_reel, Pred2))

analyse_stats(donnee = data_df,
              nom_var = "Valeurs",
              genotype = "models")
coul3 = c("#E69F00",
          "#1F77B4",
          "#7F7F7F",
          "#2CA02C",
          "#D62728",
          "#9467BD",
          "#F0E442",
          "black")
fsg(
  donnee = data_df,
  nom_var = "Valeurs",
  genotype = "models",
  x_label = "models",
  y_label = "Abondance des vers de terre",
  title = "Boxplot de l'Richesse (models sans interaction)",
  legend_title = "geno",
  couleurs = coul3,
  affiche_point = TRUE,
  lettre_postion = 0
)




################################################################################
###                                                                          ###
###                 GAM                                                      ###
###                                                                          ###
################################################################################

nom = names(tiga_c_sansva_noncor_minmax[, c(6, 8, 9, 66:77, 79, 80)])




#Phosphore_.P._g.kg
#le pH
#C_N
#Limon fin
#Argiles_.0_a_0.002_mm._g.kg

# Sélectionner uniquement les variables pour la matrice de dispersion
selected_vars <- c(
  "AB_tot",
  "Terre_fine_..2mm._g.kg",
  "Limons_grossiers_.0.02_a_0.05_mm._g.kg",
  "Sables_fins_.0.05_a_0.2_mm._g.kg",
  "Sables_grossiers_.0.2_a_2.0_mm._g.kg",
  "Carbone_.C._total_g.kg",
  "Matiere_organique_g.kg",
  "X_L93",
  "Y_L93"
)

# Sélectionner les données correspondant aux variables sélectionnées
data_selected <- tiga_c_sansva_noncor_minmax[, selected_vars]
colnames(data_selected) = c(
  "AB_tot",
  "Terre_f",
  "Limons_g",
  "Sables_f",
  "Sables_g",
  "Carbone_tot",
  "M_O",
  "X_L93",
  "Y_L93"
)

# Créer la matrice de diagrammes de dispersion
ggpairs(
  data_selected,
  lower = list(continuous = "smooth"),
  diag = list(continuous = "density")
)



# Pour AB_tot -------------------------------------------------------------


calculate_metrics_gam(
  data = tiga_f,
  response_var = "AB_tot",
  predictor_vars = c(
    "Terre_fine_..2mm._g.kg",
    "Argiles_.0_a_0.002_mm._g.kg",
    "Limons_fins_.0.002_a_0.02_mm._g.kg",
    "Limons_grossiers_.0.02_a_0.05_mm._g.kg",
    "Sables_fins_.0.05_a_0.2_mm._g.kg",
    "Sables_grossiers_.0.2_a_2.0_mm._g.kg",
    "Carbone_.C._total_g.kg",
    "Matiere_organique_g.kg",
    "X_L93",
    "Y_L93"
  )
)


# GAM LM 1 POUR AB_tot
mod_lm_ab = gam(
  AB_tot ~ Land_use + X_L93 + Y_L93 +
    Terre_fine_..2mm._g.kg +
    Limons_grossiers_.0.02_a_0.05_mm._g.kg +
    Sables_fins_.0.05_a_0.2_mm._g.kg +
    Sables_grossiers_.0.2_a_2.0_mm._g.kg +
    Carbone_.C._total_g.kg +
    Matiere_organique_g.kg,method = "REML",
  data = train
)

summary(mod_lm_ab)

# Verification
par(mfrow = c(2, 2))
gam.check(mod_lm_ab)
shapiro.test(mod_lm_ab$res)
concurvity(mod_lm_ab,full = TRUE)
concurvity(mod_lm_ab,full = FALSE)

# Validation non croisée
lm_ab= predict(mod_lm_ab,
                  newdata = test)
sum((lm_ab-test$AB_tot)^2)
sqrt(mean((lm_ab-test$AB_tot)^2)) # 186.2722


# Validation croissée
# Définir le modèle GAM en dehors de la fonction
my_model <- function(data) {
  return(gam(
    AB_tot ~ Land_use + X_L93 + Y_L93 +
      Terre_fine_..2mm._g.kg +
      Limons_grossiers_.0.02_a_0.05_mm._g.kg +
      Sables_fins_.0.05_a_0.2_mm._g.kg +
      Sables_grossiers_.0.2_a_2.0_mm._g.kg +
      Carbone_.C._total_g.kg +
      Matiere_organique_g.kg,method = "REML",
    data = tiga_f
  ))
}

# Utiliser la fonction perform_block_cross_validation avec le modèle défini
perform_block_cross_validation(data = tiga_f, num_blocks = 10, block_size = 39, model = my_model)







# GAM 1 POUR AB_tot
mod_gam1_ab = gam(
  AB_tot ~ Land_use + s(X_L93) + s(Y_L93) +
    s(Terre_fine_..2mm._g.kg) +
    s(Limons_grossiers_.0.02_a_0.05_mm._g.kg) +
    s(Sables_fins_.0.05_a_0.2_mm._g.kg) +
    s(Sables_grossiers_.0.2_a_2.0_mm._g.kg) +
    s(Carbone_.C._total_g.kg) +
    s(Matiere_organique_g.kg),method = "REML",
  data = train
)

summary(mod_gam1_ab)

# Visualisation
plot(mod_gam1_ab)
plot(
  mod_gam1_ab,
  pages = 1,
  residuals = TRUE,
  all.terms = TRUE,
  shade = TRUE,
  shade.col = 2
)

plot(mod_gam1_ab, pages = 1, seWithMean = TRUE)
plot(mod_gam1_ab, residuals = TRUE, pch = 1)
plot(ggeffects::ggpredict(mod_gam1_ab), facets = TRUE)
gratia::draw(mod_gam1_ab, residuals = TRUE)


# Verification
par(mfrow = c(2, 2))
gam.check(mod_gam1_ab)
shapiro.test(mod_gam1_ab$res)
concurvity(mod_gam1_ab,full = TRUE)
concurvity(mod_gam1_ab,full = FALSE)


#validation non croisée
gam_1_ab = predict(mod_gam1_ab,
                      newdata = test)
sum((gam_1_ab-test$AB_tot)^2)
sqrt(mean((gam_1_ab-test$AB_tot)^2)) # 175.4125


#  Validation croissé
my_gam_1_ab <- function(data) {
  return(gam(
    AB_tot ~ Land_use + s(X_L93) + s(Y_L93) +
      s(Terre_fine_..2mm._g.kg) +
      s(Limons_grossiers_.0.02_a_0.05_mm._g.kg) +
      s(Sables_fins_.0.05_a_0.2_mm._g.kg) +
      s(Sables_grossiers_.0.2_a_2.0_mm._g.kg) +
      s(Carbone_.C._total_g.kg) +
      s(Matiere_organique_g.kg),method = "REML",
    data = tiga_f
  ))
}

# Utiliser la fonction perform_block_cross_validation avec le modèle défini
perform_block_cross_validation(data = tiga_f, num_blocks = 10, block_size = 39, model = my_gam_1_ab)
# $mean_RMSE [1] 161.768






# MOD GAM 2 POUR AB_tot
mod_gam2_ab = gam(AB_tot ~ Land_use + 
                 s(X_L93)+ s(Y_L93) +
                 s(Terre_fine_..2mm._g.kg)+
                 s(Limons_grossiers_.0.02_a_0.05_mm._g.kg) +
                 s(Sables_fins_.0.05_a_0.2_mm._g.kg, by = Land_use) +
                 s(Sables_grossiers_.0.2_a_2.0_mm._g.kg) +
                 s(Carbone_.C._total_g.kg) +
                 s(Matiere_organique_g.kg, by = Land_use),method = "REML",
               data = train)

summary(mod_gam2_ab)

plot(mod_gam2_ab)
plot(
  mod_gam2_ab,
  pages = 1,
  residuals = TRUE,
  all.terms = TRUE,
  shade = TRUE,
  shade.col = 2
)
plot(mod_gam2_ab, pages = 1, seWithMean = TRUE)
plot(o3_t, residuals = TRUE, pch = 1)
gratia::draw(mod_gam2_ab, residuals = TRUE)
par(mfrow = c(2, 2))
gam.check(mod_gam2_ab)
vis.gam(mod_gam2_ab, theta = 120, 
        n.grid = 50, lwd = .4)
vis.gam(mod_gam2_ab, theta = 60, 
        n.grid = 50, lwd = .4)


# Validation non croisée
gam_2_ab=predict(mod_gam2_ab,
        newdata = test)
sum((gam_2_ab-test$AB_tot)^2)
sqrt(mean((gam_2_ab-test$AB_tot)^2)) #  162.462



#  Validation croissé
my_gam_2_ab <- function(data) {
  return(gam(
    AB_tot ~ Land_use + 
      s(X_L93)+ s(Y_L93) +
      s(Terre_fine_..2mm._g.kg)+
      s(Limons_grossiers_.0.02_a_0.05_mm._g.kg) +
      s(Sables_fins_.0.05_a_0.2_mm._g.kg, by = Land_use) +
      s(Sables_grossiers_.0.2_a_2.0_mm._g.kg) +
      s(Carbone_.C._total_g.kg) +
      s(Matiere_organique_g.kg, by = Land_use),method = "REML",
    data = tiga_f
  ))
}

# Utiliser la fonction perform_block_cross_validation avec le modèle défini
perform_block_cross_validation(data = tiga_f, num_blocks = 10, block_size = 39, model = my_gam_2_ab)
# $mean_RMSE [1] 152.3821



# MOD GAM 3 POUR AB_tot
mod_gam3_ab = gam(AB_tot ~ Land_use + 
                    s(X_L93)+ s(Y_L93) +
                    s(Terre_fine_..2mm._g.kg)+
                    s(Limons_grossiers_.0.02_a_0.05_mm._g.kg) +
                    s(Sables_fins_.0.05_a_0.2_mm._g.kg, by = Land_use) +
                    s(Sables_grossiers_.0.2_a_2.0_mm._g.kg) +
                    s(Carbone_.C._total_g.kg) +
                    s(Matiere_organique_g.kg, by = Land_use)+
                    s(X_L93,Matiere_organique_g.kg)+ 
                  s(X_L93,Carbone_.C._total_g.kg)+ 
                  s(X_L93,Limons_grossiers_.0.02_a_0.05_mm._g.kg)+ 
                  s(X_L93,Sables_grossiers_.0.2_a_2.0_mm._g.kg),
                  method = "REML",
                  data = train)

summary(mod_gam3_ab)
plot(mod_gam3_ab)
plot(
  mod_gam3_ab,
  pages = 1,
  residuals = TRUE,
  all.terms = TRUE,
  shade = TRUE,
  shade.col = 2
)
plot(mod_gam3_ab, pages = 1, seWithMean = TRUE)
plot(o3_t, residuals = TRUE, pch = 1)
gratia::draw(mod_gam3_ab, residuals = TRUE)
par(mfrow = c(2, 2))
gam.check(mod_gam3_ab)
vis.gam(mod_gam3_ab, theta = 120, 
        n.grid = 50, lwd = .4)
vis.gam(mod_gam3_ab, theta = 60, 
        n.grid = 50, lwd = .4)


# Validation non croisée
gam_3_ab=predict(mod_gam3_ab,
                 newdata = test)
sum((gam_3_ab-test$AB_tot)^2)
sqrt(mean((gam_3_ab-test$AB_tot)^2)) #  158.2527



#  Validation croissé
my_gam_3_ab <- function(data) {
  return(gam(AB_tot ~ Land_use + 
               s(X_L93)+ s(Y_L93) +
               s(Terre_fine_..2mm._g.kg)+
               s(Limons_grossiers_.0.02_a_0.05_mm._g.kg) +
               s(Sables_fins_.0.05_a_0.2_mm._g.kg, by = Land_use) +
               s(Sables_grossiers_.0.2_a_2.0_mm._g.kg) +
               s(Carbone_.C._total_g.kg) +
               s(Matiere_organique_g.kg, by = Land_use)+
               s(X_L93,Matiere_organique_g.kg)+ 
               s(X_L93,Carbone_.C._total_g.kg)+ 
               s(X_L93,Limons_grossiers_.0.02_a_0.05_mm._g.kg)+ 
               s(X_L93,Sables_grossiers_.0.2_a_2.0_mm._g.kg),
             method = "REML",
    data = tiga_f
  ))
}

# Utiliser la fonction perform_block_cross_validation avec le modèle défini
perform_block_cross_validation(data = tiga_f, num_blocks = 10, block_size = 39, model = my_gam_3_ab)
# $mean_RMSE [1] 150.4869



# Comparaison des modèles de l' AB_tot
anova(mod_lm_ab, mod_gam1_ab, mod_gam2_ab, mod_gam3_ab,test = "Chisq")
AIC(mod_lm_ab, mod_gam1_ab, mod_gam2_ab, mod_gam3_ab)





# Pour BM_tot -------------------------------------------------------------


calculate_metrics_gam(
  data = tiga_f,
  response_var = "BM_tot",
  predictor_vars = c(
    "Terre_fine_..2mm._g.kg",
    "Argiles_.0_a_0.002_mm._g.kg",
    "Limons_fins_.0.002_a_0.02_mm._g.kg",
    "Limons_grossiers_.0.02_a_0.05_mm._g.kg",
    "Sables_fins_.0.05_a_0.2_mm._g.kg",
    "Sables_grossiers_.0.2_a_2.0_mm._g.kg",
    "Carbone_.C._total_g.kg",
    "Matiere_organique_g.kg",
    "X_L93",
    "Y_L93"
  )
)


# GAM  LM 1 POUR BM_tot
mod_lm_bm = gam(
  BM_tot ~ Land_use + X_L93 + Y_L93 +
    Terre_fine_..2mm._g.kg +
    Limons_grossiers_.0.02_a_0.05_mm._g.kg +
    Sables_fins_.0.05_a_0.2_mm._g.kg +
    Sables_grossiers_.0.2_a_2.0_mm._g.kg +
    Carbone_.C._total_g.kg +
    Matiere_organique_g.kg,method = "REML",
  data = train
)

summary(mod_lm_bm)

# Verification
par(mfrow = c(2, 2))
gam.check(mod_lm_bm)
shapiro.test(mod_lm_bm$res)
concurvity(mod_lm_bm,full = TRUE)
concurvity(mod_lm_bm,full = FALSE)

# Validation non croisée
lm_bm= predict(mod_lm_bm,
               newdata = test)
sum((lm_bm-test$BM_tot)^2)
sqrt(mean((lm_bm-test$BM_tot)^2)) # 36.75424


# Validation croissée
# Définir le modèle GAM en dehors de la fonction
my_model <- function(data) {
  return(gam(
    BM_tot ~ Land_use + X_L93 + Y_L93 +
      Terre_fine_..2mm._g.kg +
      Limons_grossiers_.0.02_a_0.05_mm._g.kg +
      Sables_fins_.0.05_a_0.2_mm._g.kg +
      Sables_grossiers_.0.2_a_2.0_mm._g.kg +
      Carbone_.C._total_g.kg +
      Matiere_organique_g.kg,method = "REML",
    data = tiga_f
  ))
}

# Utiliser la fonction perform_block_cross_validation avec le modèle défini
perform_block_cross_validation(data = tiga_f, num_blocks = 10, block_size = 39, 
                               model = my_model,var_rep = "BM_tot")
# $mean_RMSE [1] 35.41938





# GAM 1 POUR BM_tot
mod_gam1_bm = gam(
  BM_tot ~ Land_use + s(X_L93) + s(Y_L93) +
    s(Terre_fine_..2mm._g.kg) +
    s(Limons_grossiers_.0.02_a_0.05_mm._g.kg,k = 25) +
    s(Sables_fins_.0.05_a_0.2_mm._g.kg) +
    s(Sables_grossiers_.0.2_a_2.0_mm._g.kg) +
    s(Carbone_.C._total_g.kg) +
    s(Matiere_organique_g.kg),method = "REML",
  data = train
)

summary(mod_gam1_bm)

# Visualisation
plot(mod_gam1_bm)
plot(
  mod_gam1_bm,
  pages = 1,
  residuals = TRUE,
  all.terms = TRUE,
  shade = TRUE,
  shade.col = 2
)

plot(mod_gam1_bm, pages = 1, seWithMean = TRUE)
plot(mod_gam1_bm, residuals = TRUE, pch = 1)
plot(ggeffects::ggpredict(mod_gam1_bm), facets = TRUE)
gratia::draw(mod_gam1_bm, residuals = TRUE)


# Verification
par(mfrow = c(2, 2))
gam.check(mod_gam1_bm)
shapiro.test(mod_gam1_bm$res)
concurvity(mod_gam1_bm,full = TRUE)
concurvity(mod_gam1_bm,full = FALSE)


#validation non croisée
gam_1_bm = predict(mod_gam1_bm,
                   newdata = test)
sum((gam_1_bm-test$BM_tot)^2)
sqrt(mean((gam_1_bm-test$BM_tot)^2)) # 36.1138


#  Validation croissé
my_gam_1_bm <- function(data) {
  return(gam(
    BM_tot ~ Land_use + s(X_L93) + s(Y_L93) +
      s(Terre_fine_..2mm._g.kg) +
      s(Limons_grossiers_.0.02_a_0.05_mm._g.kg) +
      s(Sables_fins_.0.05_a_0.2_mm._g.kg) +
      s(Sables_grossiers_.0.2_a_2.0_mm._g.kg) +
      s(Carbone_.C._total_g.kg) +
      s(Matiere_organique_g.kg),method = "REML",
    data = tiga_f
  ))
}

# Utiliser la fonction perform_block_cross_validation avec le modèle défini
perform_block_cross_validation(data = tiga_f, num_blocks = 10, block_size = 39,
                               model = my_gam_1_bm,var_rep = "BM_tot")
# $mean_RMSE [1] 34.25215






# MOD GAM 2 POUR BM_tot
mod_gam2_bm = gam(BM_tot ~ Land_use + 
                    s(X_L93)+ s(Y_L93) +
                    s(Terre_fine_..2mm._g.kg)+
                    s(Limons_grossiers_.0.02_a_0.05_mm._g.kg) +
                    s(Sables_fins_.0.05_a_0.2_mm._g.kg, by = Land_use) +
                    s(Sables_grossiers_.0.2_a_2.0_mm._g.kg) +
                    s(Carbone_.C._total_g.kg) +
                    s(Matiere_organique_g.kg, by = Land_use),method = "REML",
                  data = train)

summary(mod_gam2_bm)
plot(mod_gam2_bm)
plot(
  mod_gam2_bm,
  pages = 1,
  residuals = TRUE,
  all.terms = TRUE,
  shade = TRUE,
  shade.col = 2
)
plot(mod_gam2_bm, pages = 1, seWithMean = TRUE)
plot(o3_t, residuals = TRUE, pch = 1)
gratia::draw(mod_gam2_bm, residuals = TRUE)
par(mfrow = c(2, 2))
gam.check(mod_gam2_bm)
vis.gam(mod_gam2_bm, theta = 120, 
        n.grid = 50, lwd = .4)
vis.gam(mod_gam2_bm, theta = 60, 
        n.grid = 50, lwd = .4)


# Validation non croisée
gam_2_bm=predict(mod_gam2_bm,
                 newdata = test)
sum((gam_2_bm-test$BM_tot)^2)
sqrt(mean((gam_2_bm-test$BM_tot)^2)) #  33.4188



#  Validation croissé
my_gam_2_bm <- function(data) {
  return(gam(
    BM_tot ~ Land_use + 
      s(X_L93)+ s(Y_L93) +
      s(Terre_fine_..2mm._g.kg)+
      s(Limons_grossiers_.0.02_a_0.05_mm._g.kg) +
      s(Sables_fins_.0.05_a_0.2_mm._g.kg, by = Land_use) +
      s(Sables_grossiers_.0.2_a_2.0_mm._g.kg) +
      s(Carbone_.C._total_g.kg) +
      s(Matiere_organique_g.kg, by = Land_use),method = "REML",
    data = tiga_f
  ))
}

# Utiliser la fonction perform_block_cross_validation avec le modèle défini
perform_block_cross_validation(data = tiga_f, num_blocks = 10, block_size = 39, 
                               model = my_gam_2_bm,var_rep = "BM_tot")
# $mean_RMSE [1] 33.18669

X_L93:Matiere_organique_g.kg
X_L93:Sables_fins_.0.05_a_0.2_mm._g.kg
X_L93:Sables_grossiers_.0.2_a_2.0_mm._g.kg 
X_L93:Y_L93
X_L93:Terre_fine_..2mm._g.kg
X_L93:Carbone_.C._total_g.kg 

# MOD GAM 3 POUR BM_tot
mod_gam3_bm = gam(BM_tot ~ Land_use + 
                    s(X_L93)+ s(Y_L93) +
                    s(Terre_fine_..2mm._g.kg)+
                    s(Limons_grossiers_.0.02_a_0.05_mm._g.kg) +
                    s(Sables_fins_.0.05_a_0.2_mm._g.kg, by = Land_use) +
                    s(Sables_grossiers_.0.2_a_2.0_mm._g.kg) +
                    s(Carbone_.C._total_g.kg) +
                    s(Matiere_organique_g.kg, by = Land_use)+
                    s(X_L93,Matiere_organique_g.kg,k=15)+ 
                    s(X_L93,Sables_grossiers_.0.2_a_2.0_mm._g.kg)+
                    s(X_L93,Terre_fine_..2mm._g.kg),
                  method = "REML",
                  data = train)

summary(mod_gam3_bm)
plot(mod_gam3_bm)
plot(
  mod_gam3_bm,
  pages = 1,
  residuals = TRUE,
  all.terms = TRUE,
  shade = TRUE,
  shade.col = 2
)
plot(mod_gam3_bm, pages = 1, seWithMean = TRUE)
plot(o3_t, residuals = TRUE, pch = 1)
gratia::draw(mod_gam3_bm, residuals = TRUE)
par(mfrow = c(2, 2))
gam.check(mod_gam3_bm)
vis.gam(mod_gam3_bm, theta = 120, 
        n.grid = 50, lwd = .4)
vis.gam(mod_gam3_bm, theta = 60, 
        n.grid = 50, lwd = .4)


# Validation non croisée
gam_3_bm=predict(mod_gam3_bm,
                 newdata = test)
sum((gam_3_bm-test$BM_tot)^2)
sqrt(mean((gam_3_bm-test$BM_tot)^2)) #  33.31988



#  Validation croissé
my_gam_3_bm <- function(data) {
  return(gam(BM_tot ~ Land_use + 
               s(X_L93)+ s(Y_L93) +
               s(Terre_fine_..2mm._g.kg)+
               s(Limons_grossiers_.0.02_a_0.05_mm._g.kg) +
               s(Sables_fins_.0.05_a_0.2_mm._g.kg, by = Land_use) +
               s(Sables_grossiers_.0.2_a_2.0_mm._g.kg) +
               s(Carbone_.C._total_g.kg) +
               s(Matiere_organique_g.kg, by = Land_use)+
               s(X_L93,Matiere_organique_g.kg,k=15)+ 
               s(X_L93,Sables_grossiers_.0.2_a_2.0_mm._g.kg)+
               s(X_L93,Terre_fine_..2mm._g.kg),
             method = "REML",
             data = tiga_f
  ))
}

# Utiliser la fonction perform_block_cross_validation avec le modèle défini
perform_block_cross_validation(data = tiga_f, num_blocks = 10, block_size = 39, 
                               model = my_gam_3_bm,var_rep = "BM_tot")
# $mean_RMSE [1] 33.04929



# Comparaison des modèles de l' BM_tot
anova(mod_lm_bm, mod_gam1_bm, mod_gam2_bm, mod_gam3_bm,test = "Chisq")
AIC(mod_lm_bm, mod_gam1_bm, mod_gam2_bm, mod_gam3_bm)





# Pour Richesse -------------------------------------------------------------


calculate_metrics_gam(
  data = tiga_f,
  response_var = "Richesse",
  predictor_vars = c(
    "Terre_fine_..2mm._g.kg",
    "Argiles_.0_a_0.002_mm._g.kg",
    "Limons_fins_.0.002_a_0.02_mm._g.kg",
    "Limons_grossiers_.0.02_a_0.05_mm._g.kg",
    "Sables_fins_.0.05_a_0.2_mm._g.kg",
    "Sables_grossiers_.0.2_a_2.0_mm._g.kg",
    "Carbone_.C._total_g.kg",
    "Matiere_organique_g.kg",
    "X_L93",
    "Y_L93"
  )
)


# GAM  LM 1 POUR Richesse
mod_lm_rich = gam(
  Richesse ~ Land_use + X_L93 + Y_L93 +
    Terre_fine_..2mm._g.kg +
    Limons_grossiers_.0.02_a_0.05_mm._g.kg +
    Sables_fins_.0.05_a_0.2_mm._g.kg +
    Sables_grossiers_.0.2_a_2.0_mm._g.kg +
    Carbone_.C._total_g.kg +
    Matiere_organique_g.kg,method = "REML",
  data = train
)

summary(mod_lm_rich)

# Verification
par(mfrow = c(2, 2))
gam.check(mod_lm_rich)
shapiro.test(mod_lm_rich$res)
concurvity(mod_lm_rich,full = TRUE)
concurvity(mod_lm_rich,full = FALSE)

# Validation non croisée
lm_rich= predict(mod_lm_rich,
               newdata = test)
sum((lm_rich-test$Richesse)^2)
sqrt(mean((lm_rich-test$Richesse)^2)) # 1.731407


# Validation croissée
# Définir le modèle GAM en dehors de la fonction
my_model <- function(data) {
  return(gam(
    Richesse ~ Land_use + X_L93 + Y_L93 +
      Terre_fine_..2mm._g.kg +
      Limons_grossiers_.0.02_a_0.05_mm._g.kg +
      Sables_fins_.0.05_a_0.2_mm._g.kg +
      Sables_grossiers_.0.2_a_2.0_mm._g.kg +
      Carbone_.C._total_g.kg +
      Matiere_organique_g.kg,method = "REML",
    data = tiga_f
  ))
}

# Utiliser la fonction perform_block_cross_validation avec le modèle défini
perform_block_cross_validation(data = tiga_f, num_blocks = 10, block_size = 39, 
                               model = my_model,var_rep = "Richesse")
# $mean_RMSE [1] 1.704534





# GAM 1 POUR Richesse
mod_gam1_rich = gam(
  Richesse ~ Land_use + s(X_L93) + s(Y_L93) +
    s(Terre_fine_..2mm._g.kg) +
    s(Limons_grossiers_.0.02_a_0.05_mm._g.kg,k = 25) +
    s(Sables_fins_.0.05_a_0.2_mm._g.kg) +
    s(Sables_grossiers_.0.2_a_2.0_mm._g.kg) +
    s(Carbone_.C._total_g.kg) +
    s(Matiere_organique_g.kg),method = "REML",
  data = train
)

summary(mod_gam1_rich)

# Visualisation
plot(mod_gam1_rich)
plot(
  mod_gam1_rich,
  pages = 1,
  residuals = TRUE,
  all.terms = TRUE,
  shade = TRUE,
  shade.col = 2
)

plot(mod_gam1_rich, pages = 1, seWithMean = TRUE)
plot(mod_gam1_rich, residuals = TRUE, pch = 1)
plot(ggeffects::ggpredict(mod_gam1_rich), facets = TRUE)
gratia::draw(mod_gam1_rich, residuals = TRUE)


# Verification
par(mfrow = c(2, 2))
gam.check(mod_gam1_rich)
shapiro.test(mod_gam1_rich$res)
concurvity(mod_gam1_rich,full = TRUE)
concurvity(mod_gam1_rich,full = FALSE)


#validation non croisée
gam_1_rich = predict(mod_gam1_rich,
                   newdata = test)
sum((gam_1_rich-test$Richesse)^2)
sqrt(mean((gam_1_rich-test$Richesse)^2)) # 1.629317


#  Validation croissé
my_gam_1_rich <- function(data) {
  return(gam(
    Richesse ~ Land_use + s(X_L93) + s(Y_L93) +
      s(Terre_fine_..2mm._g.kg) +
      s(Limons_grossiers_.0.02_a_0.05_mm._g.kg) +
      s(Sables_fins_.0.05_a_0.2_mm._g.kg) +
      s(Sables_grossiers_.0.2_a_2.0_mm._g.kg) +
      s(Carbone_.C._total_g.kg) +
      s(Matiere_organique_g.kg),method = "REML",
    data = tiga_f
  ))
}

# Utiliser la fonction perform_block_cross_validation avec le modèle défini
perform_block_cross_validation(data = tiga_f, num_blocks = 10, block_size = 39,
                               model = my_gam_1_rich,var_rep = "Richesse")
# $mean_RMSE [1] 1.642813






# MOD GAM 2 POUR Richesse
mod_gam2_rich = gam(Richesse ~ Land_use + 
                    s(X_L93)+ s(Y_L93) +
                    s(Terre_fine_..2mm._g.kg)+
                    s(Limons_grossiers_.0.02_a_0.05_mm._g.kg) +
                    s(Sables_fins_.0.05_a_0.2_mm._g.kg, by = Land_use) +
                    s(Sables_grossiers_.0.2_a_2.0_mm._g.kg) +
                    s(Carbone_.C._total_g.kg) +
                    s(Matiere_organique_g.kg, by = Land_use),method = "REML",
                  data = train)

summary(mod_gam2_rich)
plot(mod_gam2_rich)
plot(
  mod_gam2_rich,
  pages = 1,
  residuals = TRUE,
  all.terms = TRUE,
  shade = TRUE,
  shade.col = 2
)
plot(mod_gam2_rich, pages = 1, seWithMean = TRUE)
plot(o3_t, residuals = TRUE, pch = 1)
gratia::draw(mod_gam2_rich, residuals = TRUE)
par(mfrow = c(2, 2))
gam.check(mod_gam2_rich)
vis.gam(mod_gam2_rich, theta = 120, 
        n.grid = 50, lwd = .4)
vis.gam(mod_gam2_rich, theta = 60, 
        n.grid = 50, lwd = .4)


# Validation non croisée
gam_2_rich=predict(mod_gam2_rich,
                 newdata = test)
sum((gam_2_rich-test$Richesse)^2)
sqrt(mean((gam_2_rich-test$Richesse)^2)) #  1.377052



#  Validation croissé
my_gam_2_rich <- function(data) {
  return(gam(
    Richesse ~ Land_use + 
      s(X_L93)+ s(Y_L93) +
      s(Terre_fine_..2mm._g.kg)+
      s(Limons_grossiers_.0.02_a_0.05_mm._g.kg) +
      s(Sables_fins_.0.05_a_0.2_mm._g.kg, by = Land_use) +
      s(Sables_grossiers_.0.2_a_2.0_mm._g.kg) +
      s(Carbone_.C._total_g.kg) +
      s(Matiere_organique_g.kg, by = Land_use),method = "REML",
    data = tiga_f
  ))
}

# Utiliser la fonction perform_block_cross_validation avec le modèle défini
perform_block_cross_validation(data = tiga_f, num_blocks = 10, block_size = 39, 
                               model = my_gam_2_rich,var_rep = "Richesse")
# $mean_RMSE [1] 1.559918



Sables_fins_.0.05_a_0.2_mm._g.kg:Matiere_organique_g.kg
Sables_fins_.0.05_a_0.2_mm._g.kg:Sables_grossiers_.0.2_a_2.0_mm._g.kg 
Limons_grossiers_.0.02_a_0.05_mm._g.kg:Matiere_organique_g.kg
Sables_fins_.0.05_a_0.2_mm._g.kg:Carbone_.C._total_g.kg
Limons_grossiers_.0.02_a_0.05_mm._g.kg:Sables_fins_.0.05_a_0.2_mm._g.kg 



# MOD GAM 3 POUR Richesse
mod_gam3_rich = gam(Richesse ~ Land_use + 
                    s(X_L93)+ s(Y_L93) +
                    s(Terre_fine_..2mm._g.kg)+
                    s(Limons_grossiers_.0.02_a_0.05_mm._g.kg) +
                    s(Sables_fins_.0.05_a_0.2_mm._g.kg, by = Land_use) +
                    s(Sables_grossiers_.0.2_a_2.0_mm._g.kg) +
                    s(Carbone_.C._total_g.kg) +
                    s(Matiere_organique_g.kg, by = Land_use)+
                    s(Sables_fins_.0.05_a_0.2_mm._g.kg,Matiere_organique_g.kg,k=15)+ 
                    s(Sables_fins_.0.05_a_0.2_mm._g.kg,Sables_grossiers_.0.2_a_2.0_mm._g.kg)+
                    s(Limons_grossiers_.0.02_a_0.05_mm._g.kg,Matiere_organique_g.kg)+
                    s(Sables_fins_.0.05_a_0.2_mm._g.kg,Carbone_.C._total_g.kg)+
                    s(Limons_grossiers_.0.02_a_0.05_mm._g.kg,Sables_fins_.0.05_a_0.2_mm._g.kg),
                  method = "REML",
                  data = train)

summary(mod_gam3_rich)
plot(mod_gam3_rich)
plot(
  mod_gam3_rich,
  pages = 1,
  residuals = TRUE,
  all.terms = TRUE,
  shade = TRUE,
  shade.col = 2
)
plot(mod_gam3_rich, pages = 1, seWithMean = TRUE)
plot(o3_t, residuals = TRUE, pch = 1)
gratia::draw(mod_gam3_rich, residuals = TRUE)
par(mfrow = c(2, 2))
gam.check(mod_gam3_rich)
vis.gam(mod_gam3_rich, theta = 120, 
        n.grid = 50, lwd = .4)
vis.gam(mod_gam3_rich, theta = 60, 
        n.grid = 50, lwd = .4)


# Validation non croisée
gam_3_rich=predict(mod_gam3_rich,
                 newdata = test)
sum((gam_3_rich-test$Richesse)^2)
sqrt(mean((gam_3_rich-test$Richesse)^2)) #  1.354158



#  Validation croissé
my_gam_3_rich <- function(data) {
  return(gam(Richesse ~ Land_use + 
               s(X_L93)+ s(Y_L93) +
               s(Terre_fine_..2mm._g.kg)+
               s(Limons_grossiers_.0.02_a_0.05_mm._g.kg) +
               s(Sables_fins_.0.05_a_0.2_mm._g.kg, by = Land_use) +
               s(Sables_grossiers_.0.2_a_2.0_mm._g.kg) +
               s(Carbone_.C._total_g.kg) +
               s(Matiere_organique_g.kg, by = Land_use)+
               s(Sables_fins_.0.05_a_0.2_mm._g.kg,Matiere_organique_g.kg,k=15)+ 
               s(Sables_fins_.0.05_a_0.2_mm._g.kg,Sables_grossiers_.0.2_a_2.0_mm._g.kg)+
               s(Limons_grossiers_.0.02_a_0.05_mm._g.kg,Matiere_organique_g.kg)+
               s(Sables_fins_.0.05_a_0.2_mm._g.kg,Carbone_.C._total_g.kg)+
               s(Limons_grossiers_.0.02_a_0.05_mm._g.kg,Sables_fins_.0.05_a_0.2_mm._g.kg),
             method = "REML",
             data = tiga_f
  ))
}

# Utiliser la fonction perform_block_cross_validation avec le modèle défini
perform_block_cross_validation(data = tiga_f, num_blocks = 10, block_size = 39, 
                               model = my_gam_3_rich,var_rep = "Richesse")
# $mean_RMSE [1] 1.510081



# Comparaison des modèles de l' Richesse
anova(mod_lm_rich, mod_gam1_rich, mod_gam2_rich, mod_gam3_rich,test = "Chisq")
AIC(mod_lm_rich, mod_gam1_rich, mod_gam2_rich, mod_gam3_rich)





# Selection de varibmle ---------------------------------------------------


Gam.object <- gam(Richesse ~ 
                    s(X_L93) + s(Y_L93) + 
                    s(Terre_fine_..2mm._g.kg) + 
                    s(Argiles_.0_a_0.002_mm._g.kg) + 
                    s(Limons_fins_.0.002_a_0.02_mm._g.kg) + 
                    s(Limons_grossiers_.0.02_a_0.05_mm._g.kg) + 
                    s(Sables_fins_.0.05_a_0.2_mm._g.kg) + 
                    s(Sables_grossiers_.0.2_a_2.0_mm._g.kg) + 
                    s(Carbone_.C._total_g.kg) + s(pH) + 
                    s(Matiere_organique_g.kg),
                  data = tiga_f)


library(gam)
gam::step.Gam(Gam.object, 
              scope = list("X_L93" = ~1 + X_L93 + s(X_L93, 4) + s(X_L93, 6) + s(X_L93, 12),
                           "Y_L93" = ~1 + Y_L93 + s(Y_L93, 4) + s(Y_L93, 6) + s(Y_L93, 12)),
)  # Mettez trace = TRUE pour afficher les étapes du processus de sélection

detach("package:gam", unload = TRUE)

predictor_vars=c("X_L93","Y_L93",
                    "Terre_fine_..2mm._g.kg",
                    "Limons_grossiers_.0.02_a_0.05_mm._g.kg",
                    "Sables_fins_.0.05_a_0.2_mm._g.kg", 
                    "Sables_grossiers_.0.2_a_2.0_mm._g.kg",
                    "Carbone_.C._total_g.kg" ,
                    "Matiere_organique_g.kg")
# Liste pour stocker les résultats
aic_results <- list()

# Boucle pour ajouter toutes les interactions possibles deux à deux
for (i in 1:(length(predictor_vars) - 1)) {
  for (j in (i+1):length(predictor_vars)) {
    # Ajouter l'interaction entre predictor_vars[i] et predictor_vars[j] au modèle de base
    formula <- as.formula(paste("Richesse ~ s(", predictor_vars[i], ",", predictor_vars[j], ")"))
    mod_gam_with_interaction <- gam(formula, method = "REML", data = train)
    
    # Calculer l'AIC du modèle avec l'interaction nouvellement ajoutée
    aic <- AIC(mod_gam_with_interaction)
    
    # Stocker les résultats dans la liste
    var_interaction <- paste(predictor_vars[i], predictor_vars[j], sep = ":")
    aic_results[[var_interaction]] <- aic
  }
}


# Convertir les résultats en data frame
aic_df <- data.frame(
  Interaction = names(aic_results),
  AIC = unlist(aic_results)
)
aic_df
print(sort(aic_df$AIC))

X_L93:Matiere_organique_g.kg 
X_L93:Carbone_.C._total_g.kg 
X_L93:Limons_grossiers_.0.02_a_0.05_mm._g.kg 
X_L93:Sables_grossiers_.0.2_a_2.0_mm._g.kg 
                                   

X_L93:Matiere_organique_g.kg
X_L93:Sables_fins_.0.05_a_0.2_mm._g.kg
X_L93:Sables_grossiers_.0.2_a_2.0_mm._g.kg 
X_L93:Y_L93
X_L93:Terre_fine_..2mm._g.kg
X_L93:Carbone_.C._total_g.kg 


Sables_fins_.0.05_a_0.2_mm._g.kg:Matiere_organique_g.kg
Sables_fins_.0.05_a_0.2_mm._g.kg:Sables_grossiers_.0.2_a_2.0_mm._g.kg 
Limons_grossiers_.0.02_a_0.05_mm._g.kg:Matiere_organique_g.kg
Sables_fins_.0.05_a_0.2_mm._g.kg:Carbone_.C._total_g.kg
Limons_grossiers_.0.02_a_0.05_mm._g.kg:Sables_fins_.0.05_a_0.2_mm._g.kg 





################################################################################
###                                                                          ###
###                 Machine Learning                                         ###
###                                                                          ###
################################################################################


#partitionnement récursif et arbres de régression

# manuelle
{
  var_complt <- calculer_pourcentage_NA(tiga_simpl,
                                        afficher_zero_percent = TRUE)
  
  tiga_simpl_complt = tiga[, var_complt$Variable[-87]] # On selectionne uniquement les variables complets
  colnames(tiga_simpl_complt)
  str(tiga_simpl_complt)
  
  
  tiga_prediction = tiga_simpl_complt
  str(tiga_prediction)
  names(tiga_prediction)
  tiga_prediction <-
    conv_col(tiga_prediction, names(tiga_prediction [, c(8:81, 83:86)]), "numeric")
  str(tiga_prediction)
  
  
  #Créer des variables factices
  var_quali <- tiga_prediction %>%
    select(c("Land_use"))
  
  tiga_prediction <-
    dummy_cols(tiga_prediction, select_columns = c("Land_use"))
  
  
  names(tiga_prediction)
  head(tiga_prediction[, 87:92])
  str(tiga_prediction)
  
  
  
  
  #Finalisation des données pour la modélisation prédictive ...
  
  
  
  
  # Mise à l'échelle des données
  nom_quanti <-
    names(tiga_prediction[, c(67:81)]) # var expli quanti
  
  positions <- which(colnames(tiga_prediction) %in% nom_quanti)
  
  
  library(caret)
  
  preproc <-
    preProcess(tiga_prediction[, c(67:81)], method = c("range"))
  scaled <- predict(preproc, tiga_prediction[, c(67:81)])
  
  for (i in names(scaled)) {
    tiga_prediction[[i]] = scaled[[i]]
  }
  summary(tiga_prediction)
  
  
  
  #Répartition entraînement/test (80% formation et 20% tests)
  
  names(tiga_prediction)
  tiga_prediction_AB_tot = tiga_prediction[, c(8, 67:81, 87:92)]
  str(tiga_prediction_AB_tot)
  #tiga_prediction <- conv_col(tiga_prediction, names(tiga_prediction[,c(67:81)]), "numeric")
  
  
  
  library(caTools)
  library(rpart)
  library(rpart.plot)
  
  set.seed(42)
  
  sample <-
    sample.split(tiga_prediction_AB_tot$AB_tot, SplitRatio = 0.8)
  train2 <- subset(tiga_prediction_AB_tot, sample == TRUE)
  test <- subset(tiga_prediction_AB_tot, sample == FALSE)
  print(dim(train))
  print(dim(test))
  
  
  
  
  # Entraînement d'un modèle d'apprentissage automatique
  names(train)
  data = train[, c(6, 66, 67:77, 79:80)]
  
  model_dt <- rpart(Richesse ~ ., data = data, method = "anova")
  rpart.plot(model_dt)
  
  summary(model_dt)
  
  
  #Évaluation d'un modèle d'apprentissage automatique
  
  importances <- varImp(model_dt)
  importances %>%
    arrange(desc(Overall))
  
  RPRT <- predict(model_dt, test)
  
  eval_df <- data.frame(actual = test$Richesse,
                        predicted_dt = RPRT)
  head(eval_df)
  
  sqrt(mean((eval_df$actual - eval_df$predicted_dt) ^ 2))
  
}

# Avec la fonction





## Pour AB_tot -------------------------------------------------------------

names(tiga_f)
tiga_prediction_AB_tot = tiga_f[, c(8, 67,70,71,72,73,75,79,80,6)]
result_ab=train_test_model(data = tiga_prediction_AB_tot,
                           var_reponse = "AB_tot",
                           split_ratio = 0.8) # $rmse [1] 178.2767


train_ab=train[, c(8, 67,70,71,72,73,75,79,80,6)]
result_ab=train_test_model_1(data_train = train_ab,data_test = test,var_reponse = "AB_tot")
data_ab=result_ab$evaluation

mod_ml1_ab <- function(data) {
  return(rpart(
    AB_tot ~ Land_use + X_L93 + Y_L93 +
      Terre_fine_..2mm._g.kg +
      Argiles_.0_a_0.002_mm._g.kg +
      Limons_fins_.0.002_a_0.02_mm._g.kg +
      Limons_grossiers_.0.02_a_0.05_mm._g.kg +
      Sables_fins_.0.05_a_0.2_mm._g.kg +
      Sables_grossiers_.0.2_a_2.0_mm._g.kg +
      Carbone_.C._total_g.kg + pH +
      Matiere_organique_g.kg,
    data = data
  ))
}

# Utiliser la fonction perform_block_cross_validation avec le modèle défini
perform_block_cross_validation(data = tiga_f, num_blocks = 10, block_size = 39, 
                               model = mod_ml1_ab,var_rep = "AB_tot")
# $mean_RMSE [1] 199.542






## Pour BM_tot -------------------------------------------------------------

names(tiga_f)
tiga_prediction_BM_tot = tiga_f[, c(9,67,70,71,72,73,75,79,80,6)]
result_bm=train_test_model(data = tiga_prediction_BM_tot,
                           var_reponse = "BM_tot",
                           split_ratio = 0.8) # $rmse [1] 42.08092
data_bm=result_bm$evaluation

mod_ml1_bm <- function(data) {
  return(rpart(
    BM_tot ~ Land_use + X_L93 + Y_L93 +
      Terre_fine_..2mm._g.kg +
      Argiles_.0_a_0.002_mm._g.kg +
      Limons_fins_.0.002_a_0.02_mm._g.kg +
      Limons_grossiers_.0.02_a_0.05_mm._g.kg +
      Sables_fins_.0.05_a_0.2_mm._g.kg +
      Sables_grossiers_.0.2_a_2.0_mm._g.kg +
      Carbone_.C._total_g.kg + pH +
      Matiere_organique_g.kg,
    data = data
  ))
}

# Utiliser la fonction perform_block_cross_validation avec le modèle défini
perform_block_cross_validation(data = tiga_f, num_blocks = 10, block_size = 39, 
                               model = mod_ml1_bm,var_rep = "BM_tot")
#$mean_RMSE [1] 40.6869






## Pour Richesse -------------------------------------------------------------

names(tiga_f)
tiga_prediction_Richesse = tiga_f[, c(66, 67,70,71,72,73,75,79,80,6)]
result_rich=train_test_model(data = tiga_prediction_Richesse,
                             var_reponse = "Richesse",
                             split_ratio = 0.8) # $rmse [1] 1.995261
data_rich=result_rich$evaluation

mod_ml1_rich <- function(data) {
  return(rpart(
    Richesse ~ Land_use + X_L93 + Y_L93 +
      Terre_fine_..2mm._g.kg +
      Argiles_.0_a_0.002_mm._g.kg +
      Limons_fins_.0.002_a_0.02_mm._g.kg +
      Limons_grossiers_.0.02_a_0.05_mm._g.kg +
      Sables_fins_.0.05_a_0.2_mm._g.kg +
      Sables_grossiers_.0.2_a_2.0_mm._g.kg +
      Carbone_.C._total_g.kg + pH +
      Matiere_organique_g.kg,
    data = data
  ))
}

# Utiliser la fonction perform_block_cross_validation avec le modèle défini
perform_block_cross_validation(data = tiga_f, num_blocks = 10, block_size = 39, 
                               model = mod_ml1_rich,var_rep = "Richesse")

# $mean_RMSE [1] 2.029376








