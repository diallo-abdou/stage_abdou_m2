

# AB_tot------------------------------------------------------------------


AB_tot_reel = test$AB_tot


## Model POLY  sans interaction
data_df <-
  data.frame(
    models = rep(
      c(
        "AB_tot",
        "poly_1",
        "poly_2",
        "poly_3",
        "poly_4",
        "poly_5",
        "poly_6"
      ),
      each = length(AB_tot_reel)
    ),
    Valeurs = c(AB_tot_reel, poly_1, poly_2, poly_3, poly_4, poly_5, poly_6)
  )
RPRT

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
  x_label = "models poly",
  y_label = "Abondance des vers de terre",
  title = "",
  legend_title = "Models",
  couleurs = coul3,
  affiche_point = FALSE,
  lettre_postion = 0
)

G2 = fsg(
  donnee = data_df,
  nom_var = "Valeurs",
  genotype = "models",
  x_label = "models poly",
  y_label = "",
  title = "",
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




# Model GAM ###

data_gam_ab <-
  data.frame(
    models = rep(
      c(
        "AB_tot",
        "gam_lm",
        "gam_1",
        "gam_2",
        "gam_3"
      ),
      each = length(AB_tot_reel)
    ),
    Valeurs = c(AB_tot_reel, lm_ab, gam_1_ab, gam_2_ab, gam_3_ab)
  )


data_gam_ab$models <- fct_relevel(data_gam_ab$models, c(
  "AB_tot",
  "gam_lm",
  "gam_1",
  "gam_2",
  "gam_3"
))

analyse_stats(donnee = data_gam_ab,
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
  donnee = data_gam_ab,
  nom_var = "Valeurs",
  genotype = "models",
  x_label = "models GAM",
  y_label = "Abondance des vers de terre",
  title = "",
  legend_title = "Models",
  couleurs = coul3,
  affiche_point = FALSE,
  lettre_postion = 0
)

G2 = fsg(
  donnee = data_gam_ab,
  nom_var = "Valeurs",
  genotype = "models",
  x_label = "models GAM",
  y_label = "",
  title = "",
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






# Model Machine Learning ###
result_ab
data_ab=result_ab$evaluation
data_ab <-
  data.frame(
    models = rep(
      c(
        "AB_tot",
        "Predicted"
      ),
      each = length(data_ab$actual)
    ),
    Valeurs = c(data_ab$actual, data_ab$predicted_dt)
  )


analyse_stats(donnee = data_ab,
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
  donnee = data_ab,
  nom_var = "Valeurs",
  genotype = "models",
  x_label = "Modèle machine learning",
  y_label = "Abondance des vers de terre",
  title = "",
  legend_title = "Models",
  couleurs = coul3,
  affiche_point = FALSE,
  lettre_postion = 600
)

G2 = fsg(
  donnee = data_ab,
  nom_var = "Valeurs",
  genotype = "models",
  x_label = "Modèle machine learning",
  y_label = "",
  title = "",
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


# BM_tot ------------------------------------------------------------------

BM_tot_reel=test$BM_tot


# Model poly

df_bm <-
  data.frame(
    models = rep(
      c(
        "BM_tot",
        "poly_1",
        "poly_2",
        "poly_3",
        "poly_4",
        "poly_5",
        "poly_6"
      ),
      each = length(BM_tot_reel)
    ),
    Valeurs = c(BM_tot_reel, poly_bm1, poly_bm2, poly_bm3, 
                poly_bm4, poly_bm5, poly_bm6)
  )


analyse_stats(donnee = df_bm,
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
  donnee = df_bm,
  nom_var = "Valeurs",
  genotype = "models",
  x_label = "models poly",
  y_label = "Biomasse des vers de terre",
  title = "",
  legend_title = "Models",
  couleurs = coul3,
  affiche_point = FALSE,
  lettre_postion = 0
)

G2 = fsg(
  donnee = df_bm,
  nom_var = "Valeurs",
  genotype = "models",
  x_label = "models poly",
  y_label = "",
  title = "",
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
mod_BM2 <- lm(
  BM_tot ~ Land_use + X_L93 + Y_L93 +
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

data_df <- data.frame(models = rep(c("BM_tot_reel", "Lm_2_I"),
                                   each = length(BM_tot_reel)),
                      Valeurs = c(BM_tot_reel, Pred2))

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
  title = "Boxplot de l'BM_Tot (models sans interaction)",
  legend_title = "geno",
  couleurs = coul3,
  affiche_point = TRUE,
  lettre_postion = 0
)





# Model GAM ###

data_gam_bm <-
  data.frame(
    models = rep(
      c(
        "BM_tot",
        "gam_lm",
        "gam_1",
        "gam_2",
        "gam_3"
      ),
      each = length(AB_tot_reel)
    ),
    Valeurs = c(BM_tot_reel, lm_bm, gam_1_bm, gam_2_bm, gam_3_bm)
  )


data_gam_bm$models <- fct_relevel(data_gam_bm$models, c(
  "BM_tot",
  "gam_lm",
  "gam_1",
  "gam_2",
  "gam_3"
))

analyse_stats(donnee = data_gam_bm,
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
  donnee = data_gam_bm,
  nom_var = "Valeurs",
  genotype = "models",
  x_label = "models GAM",
  y_label = "Biomasse des vers de terre",
  title = "",
  legend_title = "Models",
  couleurs = coul3,
  affiche_point = FALSE,
  lettre_postion = 0
)

G2 = fsg(
  donnee = data_gam_bm,
  nom_var = "Valeurs",
  genotype = "models",
  x_label = "models GAM",
  y_label = "",
  title = "",
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





# Model Machine Learning ###
data_bm=result_bm$evaluation

data_bm <-
  data.frame(
    models = rep(
      c(
        "BM_tot",
        "Predicted"
      ),
      each = length(data_bm$actual)
    ),
    Valeurs = c(data_bm$actual, data_bm$predicted_dt)
  )


analyse_stats(donnee = data_bm,
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
  donnee = data_bm,
  nom_var = "Valeurs",
  genotype = "models",
  x_label = "Modèle machine learning",
  y_label = "Biomasse des vers de terre",
  title = "",
  legend_title = "Models",
  couleurs = coul3,
  affiche_point = FALSE,
  lettre_postion = 600
)

G2 = fsg(
  donnee = data_bm,
  nom_var = "Valeurs",
  genotype = "models",
  x_label = "Modèle machine learning",
  y_label = "",
  title = "",
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


# Richesse ------------------------------------------------------------------

Richesse_reel=test$Richesse


# Mod poly richesse
df_rich <-
  data.frame(
    models = rep(
      c(
        "Richesse",
        "poly_1",
        "poly_2",
        "poly_3",
        "poly_4",
        "poly_5",
        "poly_6"
      ),
      each = length(Richesse_reel)
    ),
    Valeurs = c(
      Richesse_reel,
      poly_rich1,
      poly_rich2,
      poly_rich3,
      poly_rich4,
      poly_rich5,
      poly_rich6
    )
  )
df_rich[, "models"] <-
  fct_relevel(
    df_rich[, "models"],
    c(
      "Richesse",
      "poly_1",
      "poly_2",
      "poly_3",
      "poly_4",
      "poly_5",
      "poly_6"
    )
  )

analyse_stats(donnee = df_rich,
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
  donnee = df_rich,
  nom_var = "Valeurs",
  genotype = "models",
  x_label = "models",
  y_label = "Richesse totale des vers de terre",
  title = "",
  legend_title = "Models",
  couleurs = coul3,
  affiche_point = FALSE,
  lettre_postion = 0
)

G2 = fsg(
  donnee = df_rich,
  nom_var = "Valeurs",
  genotype = "models",
  x_label = "models",
  y_label = "",
  title = "",
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





# Model GAM ###

data_gam_rich <-
  data.frame(
    models = rep(
      c(
        "Richesse",
        "gam_lm",
        "gam_1",
        "gam_2",
        "gam_3"
      ),
      each = length(AB_tot_reel)
    ),
    Valeurs = c(Richesse_reel, lm_rich, gam_1_rich, gam_2_rich, gam_3_rich)
  )


data_gam_rich$models <- fct_relevel(data_gam_rich$models, c(
  "Richesse",
  "gam_lm",
  "gam_1",
  "gam_2",
  "gam_3"
))

analyse_stats(donnee = data_gam_rich,
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
  donnee = data_gam_rich,
  nom_var = "Valeurs",
  genotype = "models",
  x_label = "models GAM",
  y_label = "Richesse totale des vers de terre",
  title = "",
  legend_title = "Models",
  couleurs = coul3,
  affiche_point = FALSE,
  lettre_postion = 0
)

G2 = fsg(
  donnee = data_gam_rich,
  nom_var = "Valeurs",
  genotype = "models",
  x_label = "models GAM",
  y_label = "",
  title = "",
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




# Model Machine Learning ###
data_rich=result_rich$evaluation

data_rich <-
  data.frame(
    models = rep(
      c(
        "BM_tot",
        "Predicted"
      ),
      each = length(data_rich$actual)
    ),
    Valeurs = c(data_rich$actual, data_rich$predicted_dt)
  )


analyse_stats(donnee = data_rich,
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
  donnee = data_rich,
  nom_var = "Valeurs",
  genotype = "models",
  x_label = "Modèle machine learning",
  y_label = "Richesse totale des vers de terre",
  title = "",
  legend_title = "Models",
  couleurs = coul3,
  affiche_point = FALSE,
  lettre_postion = 600
)

G2 = fsg(
  donnee = data_rich,
  nom_var = "Valeurs",
  genotype = "models",
  x_label = "Modèle machine learning",
  y_label = "",
  title = "",
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








