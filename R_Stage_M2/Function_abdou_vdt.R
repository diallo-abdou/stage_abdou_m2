

#                   STAGE Abdou modèle prédictif vdt                           #

################################################################################
###                                                                          ###
###           Creation des fonctions pour simplifier les analyses            ###
###                                                                          ###
################################################################################
# Chargment des packages ----------------------------------------------------------------

library(tidyverse)
library(glme)
library(lsmeans)
library(agricolae)
library(RVAideMemoire)
library(corrplot)
library(emmeans)
library(ggplot2)
library(lme4)
library(multcomp)
library(MASS)
library(R2WinBUGS)
library(arm)
library(performance)
library(AER)
library(dplyr)
library(AICcmodavg)
library(MuMIn)
library(ade4)
library(Hmisc)
library(labdsv)
library(vegan)
library(cowplot)
library(ggpubr)
library(rstatix)
library(patchwork)
library(multcompView)
library(ggsignif)
library(grid)
library(FactoMineR)
library(factoextra)
library(explore)
library(ggrepel)
library(mgcv)
library(ISLR)
library(leaps)



# Exploration -------------------------------------------------------------

## Identification des NA dans un df
calculer_pourcentage_NA <-
  function(df, afficher_zero_percent = FALSE) {
    # Calculer le pourcentage de NA dans le dataframe
    pourcentage_total <-
      round(sum(is.na(df)) / (nrow(df) * ncol(df)) * 100, 2)
    
    # Calculer le pourcentage de NA par colonne
    pourcentage_colonnes <- round(colMeans(is.na(df)) * 100, 2)
    
    # Créer un dataframe résultat avec deux colonnes
    result <-
      data.frame(
        Variable = names(df),
        Pourcentage_NA = pourcentage_colonnes,
        row.names = NULL
      )
    
    if (afficher_zero_percent) {
      result <- result[result$Pourcentage_NA == 0, ]
    } else {
      result <- result[result$Pourcentage_NA > 0, ]
    }
    
    result <- rbind(result, c("Total", pourcentage_total))
    
    # Sélectionner uniquement les colonnes "Variable" et "Pourcentage_NA"
    result <- result[, c("Variable", "Pourcentage_NA")]
    
    return(result)
  }



# Converssion des colonne en num ou factor
conv_col <- function (data, columns_to_convert, to_types) {
  if (to_types == "numeric") {
    # Conversion des colonnes en numeric
    for (col in columns_to_convert) {
      data[, col] <- as.numeric(data[, col])
    }
  } else {
    # Conversion des colonnes en facteurs
    for (col in columns_to_convert) {
      data[, col] <- as.factor(data[, col])
    }
  }
  return(data)
}
data_converted <- conv_col(data, names(data [, c(1, 3)]), "factor")


# Detection des vleurs aberantes pour les variables vdt
verifier_valeurs_aberrantes_vdt <-
  function(data, variable, opposite = FALSE) {
    test_aberrant <- grubbs.test(data[[variable]], opposite = opposite)
    numero_colonne <- which(names(data) == variable)
    info <- summary(data[, variable])
    
    if (test_aberrant$p.value < 0.05) {
      texte <- test_aberrant$alternative
      chiffres <- gsub("[^0-9.]", "", texte)
      valeurs_aberrantes <-
        data[which(data[[variable]] == chiffres), c(1, 5:7, numero_colonne)]
      messages <-
        "Le tableau suivant indique la parcelle ayant la valeur aberrante"
      
      plot(data[, variable], pch = 16, ylab = variable)
      points(which(data[[variable]] == chiffres),
             chiffres,
             col = "red",
             pch = 16)
      return(list(test_aberrant, messages, valeurs_aberrantes, info))
      
    } else {
      messages2 <- "Pas de valeurs aberrantes"
      return(list(test_aberrant, messages2, info))
    }
  }



# Detection des vleurs aberantes pour les variables expli
verifier_valeurs_aberrantes <-
  function(data, variable, opposite = FALSE) {
    test_aberrant <- grubbs.test(data[[variable]], opposite = opposite)
    numero_colonne <- which(names(data) == variable)
    info <- summary(data[, variable])
    
    if (test_aberrant$p.value < 0.05) {
      texte <- test_aberrant$alternative
      # Utilisation d'une expression régulière pour extraire la partie correspondante
      chiffres <- regmatches(texte, regexpr("-?\\d+\\.\\d+", texte))
      valeurs_aberrantes <-
        data[which(data[[variable]] == chiffres), c(1, 5:7, numero_colonne)]
      messages <-
        "Le tableau suivant indique la parcelle ayant la valeur aberrante"
      plot(data[, variable], pch = 16, ylab = variable)
      points(which(data[[variable]] == chiffres),
             chiffres,
             col = "red",
             pch = 16)
      
      return(list(test_aberrant, messages, valeurs_aberrantes, info))
      
    } else {
      messages2 <- "Pas de valeurs aberrantes"
      
      return(list(test_aberrant, messages2, info))
    }
  }
verifier_valeurs_aberrantes(tiga_c_select, "AB_tot", opposite = FALSE)







# Fonction pour faires des testes de corrélation avec un seuil
cor_function_seuil <- function(data, seuil) {
  # Création d'un vecteur pour stocker les paires de variables corrélées
  variables_corr <- c()
  
  # Boucle pour tester la corrélation entre chaque paire de variables
  for (i in 1:(ncol(data) - 1)) {
    for (j in (i + 1):ncol(data)) {
      # Calcul de la corrélation entre les variables i et j
      cor_value <- cor(data[, i], data[, j], method = "spearman")
      
      # Stockage du résultat dans le vecteur si supérieur au seuil
      if (cor_value >= seuil | cor_value <= -seuil) {
        cat(
          "***",
          colnames(data)[i],
          "  __est corrélée à__  ",
          colnames(data)[j],
          "avec un R =",
          cor_value,
          "\n \n \n"
        )
        
        variables_corr <-
          c(variables_corr, colnames(data)[i], colnames(data)[j])
      }
    }
  }
  
  return(variables_corr)
}
cor_function_seuil(data = data_var, seuil = 0.8)


# Selection de var
calculate_metrics <- function(data, response_var, predictor_vars) {
  # Liste pour stocker les R² ajustés, AICc et BIC de chaque variable
  R_squared_adj_variables <-
    vector("numeric", length = length(predictor_vars))
  AICc_variables <-
    vector("numeric", length = length(predictor_vars))
  BIC_variables <-
    vector("numeric", length = length(predictor_vars))
  
  # Nombre total d'observations
  n <- nrow(data)
  
  # Boucle pour ajuster un modèle pour chaque variable
  for (i in seq_along(predictor_vars)) {
    formula <- as.formula(paste(response_var, "~", predictor_vars[i]))
    model <- lm(formula, data = data)
    R_squared_adj_variables[i] <- summary(model)$adj.r.squared
    
    # Calculer l'AICc
    residuals <- model$residuals
    k <-
      length(coefficients(model)) - 1  # Nombre de coefficients (paramètres estimés) dans le modèle
    AICc_variables[i] <-
      AIC(model) + (2 * k * (k + 1)) / (n - k - 1)
    
    # Calculer le BIC
    BIC_variables[i] <- log(n) * k - 2 * logLik(model)
  }
  
  # Créer le data.frame résultat
  result <- data.frame(
    Variable = predictor_vars,
    R_squared_adj = R_squared_adj_variables,
    AICc = AICc_variables,
    BIC = BIC_variables
  )
  
  return(result)
}

# Utilisation de la fonction
result <- calculate_metrics(
  data = tiga_c_sansva_noncor_minmax,
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

print(result)




calculate_metrics_gam <- function(data, response_var, predictor_vars) {
  # Liste pour stocker les R² ajustés, AICc et BIC de chaque variable
  R_squared_adj_variables <- vector("numeric", length = length(predictor_vars))
  AICc_variables <- vector("numeric", length = length(predictor_vars))
  BIC_variables <- vector("numeric", length = length(predictor_vars))
  
  # Nombre total d'observations
  n <- nrow(data)
  
  # Boucle pour ajuster un modèle pour chaque variable
  for (i in seq_along(predictor_vars)) {
    formula <- reformulate(paste("s(", predictor_vars[i], ")", sep = ""), response_var)
    model <- gam(formula, data = data, method = "REML")
    R_squared_adj_variables[i] <- summary(model)$r.sq
    
    # Calculer l'AICc
    residuals <- model$residuals
    k <- length(coefficients(model)) - 1  # Nombre de coefficients (paramètres estimés) dans le modèle
    AICc_variables[i] <- AIC(model) + (2 * k * (k + 1)) / (n - k - 1)
    
    # Calculer le BIC
    BIC_variables[i] <- log(n) * k - 2 * logLik(model)
  }
  
  # Créer le data.frame résultat
  result <- data.frame(
    Variable = predictor_vars,
    R_squared_adj = R_squared_adj_variables,
    AICc = AICc_variables,
    BIC = BIC_variables
  )
  
  return(result)
}

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



# Prédictions -------------------------------------------------------------

train_test_model<-function(data, var_reponse, split_ratio = 0.8,method = "anova") {
  
  library(caTools)
  library(rpart)
  library(rpart.plot)
  set.seed(42)
  
  #Répartition entraînement/test (80% formation et 20% tests)
  sample <- sample.split(data[[var_reponse]], SplitRatio = split_ratio)
  train <- subset(data, sample == TRUE)
  test <- subset(data, sample == FALSE)
  
  # Entraînement d'un modèle d'apprentissage automatique
  model_dt <- rpart(formula = as.formula(paste(var_reponse, "~ .")), data = train, method = method)
  rpart.plot(model_dt)
  
  #Évaluation d'un modèle d'apprentissage automatique
  importances <- varImp(model_dt)
  importances <- importances %>%
    arrange(desc(Overall))
  
  # Compapré les valeurs prédicte et valeur réel
  predictions_dt <- predict(model_dt, test)
  eval_df <- data.frame(
    actual = test[[var_reponse]],
    predicted_dt = predictions_dt
  )
  
  # Quantifier l'écart moyen du modèle (RMSE)
  rmse <- sqrt(mean((eval_df$actual - eval_df$predicted_dt)^2))
  
  return(list(model = model_dt, importances = importances, evaluation = eval_df, rmse = rmse))
}


train_test_model_1 <-
  function(data_train,
           data_test,
           var_reponse,
           method = "anova") {
    library(caTools)
    library(rpart)
    library(rpart.plot)
    set.seed(42)
    
    
    # Entraînement d'un modèle d'apprentissage automatique
    model_dt <-
      rpart(formula = as.formula(paste(var_reponse, "~ .")),
            data = data_train,
            method = method)
    rpart.plot(model_dt)
    
    #Évaluation d'un modèle d'apprentissage automatique
    importances <- varImp(model_dt)
    importances <- importances %>%
      arrange(desc(Overall))
    
    # Compapré les valeurs prédicte et valeur réel
    predictions_dt <- predict(model_dt, data_test)
    
    
    eval_df <- data.frame(models= rep(c(var_reponse,"Predicted"),
                          each=length(data_test)),
                          valeurs=c(data_test[[var_reponse]],predictions_dt))

    
    # Quantifier l'écart moyen du modèle (RMSE)
    rmse <- sqrt(mean((data_test[[var_reponse]] - predictions_dt) ^ 2))
    
    return(list(
      model = model_dt,
      importances = importances,
      evaluation = eval_df,
      rmse = rmse
    ))
  }


train_test_model(data = tiga_c_AB_tot,
                 var_reponse = "AB_tot",
                 split_ratio = 0.8)








determineMaxBlocs <- function(n_total) {
  max_blocs <- floor(n_total / 10)  # Nombre maximal de blocs
  target_size <-
    n_total %/% max_blocs  # Taille cible pour chaque bloc
  
  n_blocs <- max_blocs
  taille_bloc <- target_size
  
  while (n_total %% n_blocs != 0) {
    n_blocs <- n_blocs - 1
    taille_bloc <- n_total %/% n_blocs
  }
  
  list(n_blocs = taille_bloc, taille_bloc = n_blocs)
}



# début fonction Validation croissé
perform_block_cross_validation <- function(data, num_blocks, block_size, 
                                           model,var_rep) {
  # Mélanger les numéros des lignes
  shuffled_indices <- sample(nrow(data))
  
  # Répartir les données dans les blocs
  blocs <- data.frame(matrix(NA, nrow = block_size, ncol = num_blocks))
  for (i in 1:num_blocks) {
    start_index <- (i - 1) * block_size + 1
    end_index <- i * block_size
    blocs[, i] <- shuffled_indices[start_index:end_index]
  }
  
  MSE <- NULL
  Pred <- NULL
  RMSE <- NULL
  
  for (i in 1:num_blocks) {
    data1.app <- data[-blocs[, i], ]
    data1.test <- data[blocs[, i], ]
    mod <- model(data = data1.app)
    Pred <- predict(mod, newdata = data1.test)
    MSE[i] <- sum((Pred - data1.test[[var_rep]])^2)
    RMSE[i] = sqrt(mean((data1.test[[var_rep]] - Pred)^2))
  }
  
  mean_MSE <- mean(MSE)
  mean_RMSE <- mean(RMSE)
  
  return(list(MSE = MSE, mean_MSE = mean_MSE, RMSE = RMSE, mean_RMSE = mean_RMSE))
}

# Définir le modèle GAM en dehors de la fonction
my_model <- function(data) {
  return(gam(
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
perform_block_cross_validation(data = tiga_f, num_blocks = 10, block_size = 39, model = my_model)

# fin fonction Validation croissé


# Create the polynomial model (degree 2) using the I() function
# Sans interaction

compare_mod_poly <-
  function(var_reponse, degre, data_train, data_test) {
    # Create the polynomial formula dynamically
    formula_str <- paste(var_reponse, "~ Land_use")
    predictor_vars <-
      c(
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
    
    for (var in predictor_vars) {
      formula_str <-
        paste(formula_str, "+ I(", var, "^", degre, ")", sep = "")
    }
    
    # Create the polynomial model using the provided training data
    mod <- lm(formula_str, data = data_train)
    
    # Predict using the model and the provided test data
    predict_values <- predict(mod, newdata = data_test)
    
    return(list(model = mod, predict_values = predict_values))
  }
result <-
  compare_mod_poly(
    var_reponse = "AB_tot",
    degre = 2,
    data_train = train,
    data_test = test
  )

# To obtain the summary of the model:
summary(result$model)






compare_mod_poly_1 <- function(degre) {
  mod <- lm(
    AB_tot ~ Land_use +
      I(Terre_fine_..2mm._g.kg ^ degre) +
      I(Argiles_.0_a_0.002_mm._g.kg ^ degre) +
      I(Limons_fins_.0.002_a_0.02_mm._g.kg ^ degre) +
      I(Limons_grossiers_.0.02_a_0.05_mm._g.kg ^ degre) +
      I(Sables_fins_.0.05_a_0.2_mm._g.kg ^ degre) +
      I(Sables_grossiers_.0.2_a_2.0_mm._g.kg ^ degre) +
      I(Carbone_.C._total_g.kg ^ degre) +
      I(Matiere_organique_g.kg ^ degre) +
      I(X_L93 ^ degre) +
      I(Y_L93 ^ degre),
    data = train
  )
  
  predict_values = predict(mod_AB1_poly_2, newdata = test)
  
  return(list(model = mod, predict_values = predict_values))
}
summary(compare_mod_poly_1(degre = 2)$model)


# avec interaction

compare_mod_poly_I <-
  function(var_reponse, degre, data_train, data_test) {
    # Create the polynomial formula dynamically with interactions
    formula_str <- paste(var_reponse, "~ Land_use + X_L93 + Y_L93")
    predictor_vars <-
      c(
        "Terre_fine_..2mm._g.kg",
        "Argiles_.0_a_0.002_mm._g.kg",
        "Limons_fins_.0.002_a_0.02_mm._g.kg",
        "Limons_grossiers_.0.02_a_0.05_mm._g.kg",
        "Sables_fins_.0.05_a_0.2_mm._g.kg",
        "Sables_grossiers_.0.2_a_2.0_mm._g.kg",
        "Carbone_.C._total_g.kg",
        "Matiere_organique_g.kg"
      )
    
    # Add polynomial terms for each predictor
    for (var1 in predictor_vars) {
      formula_str <-
        paste(formula_str, "+ I(", var1, "^", degre, ")", sep = "")
    }
    
    # Generate all possible interactions between predictors
    for (i in seq_along(predictor_vars)) {
      for (j in (i + 1):length(predictor_vars)) {
        formula_str <-
          paste(formula_str,
                "+ I(",
                predictor_vars[i],
                "*",
                predictor_vars[j],
                "^",
                degre,
                ")",
                sep = "")
      }
    }
    
    # Create the polynomial model using the provided training data
    mod <- lm(formula_str, data = data_train)
    
    # Identify categorical variables with a single level
    categorical_vars <-
      names(Filter(function(x)
        is.factor(x) && nlevels(x) > 1, data_test))
    
    # Convert categorical variables to numeric using model.matrix()
    if (length(categorical_vars) > 0) {
      data_test_matrix <-
        model.matrix(formula(mod), data = data_test[, setdiff(names(data_test), categorical_vars)])
    } else {
      data_test_matrix <- model.matrix(formula(mod), data = data_test)
    }
    
    # Predict using the model and the provided test data matrix
    predict_values <- predict(mod, newdata = data_test_matrix)
    
    return(list(model = mod, predict_values = predict_values))
  }
result <-
  compare_mod_poly_I(
    var_reponse = "AB_tot",
    degre = 1,
    data_train = train,
    data_test = test
  )
# To obtain the summary of the model:
summary(result$model)


# Methode de selection forward


forward_stepwise_gam_selection <-
  function(response_var, predictor_vars, data) {
    # Initialize an empty list to store the selected variables
    selected_vars <- c()
    
    # Initialize an empty data frame to store the models and their R² values
    model_rsq <-
      data.frame(
        Model = character(),
        R_squared = numeric(),
        stringsAsFactors = FALSE
      )
    
    # Step 1: Model with only the intercept
    model_formula <- as.formula(paste(response_var, "~ 1"))
    model <- gam(formula = model_formula, data = data)
    model_rsq <-
      rbind(model_rsq, c("Intercept Only", summary(model)$r.sq))
    
    # Loop through the predictor variables
    for (k in 1:length(predictor_vars)) {
      best_rsq <- 0
      best_var <- NULL
      
      # Step 2: Find the best model with the current selected variables and one additional variable
      for (var in setdiff(predictor_vars, selected_vars)) {
        if (length(selected_vars) == 0) {
          formula_str <- paste(response_var, "~ s(", var, ")", sep = "")
        } else {
          formula_str <-
            paste(response_var,
                  "~ ",
                  paste(selected_vars, collapse = " + "),
                  " + s(",
                  var,
                  ")",
                  sep = "")
        }
        model <- gam(formula = as.formula(formula_str), data = data)
        
        # Check if the model has a variable
        if (length(gam.check(model)$model$sp) > 0) {
          current_rsq <- summary(model)$r.sq
          
          # Update the best variable and R² value
          if (current_rsq > best_rsq) {
            best_rsq <- current_rsq
            best_var <- var
          }
        }
      }
      
      # Step 3: Add the best variable to the selected variables
      if (!is.null(best_var)) {
        selected_vars <- c(selected_vars, best_var)
        model_rsq <-
          rbind(model_rsq, c(paste(selected_vars, collapse = " + "), best_rsq))
      } else {
        break
      }
    }
    
    # Return the data frame containing the models and their R² values
    return(model_rsq)
  }





result_gam <-
  forward_stepwise_gam_selection(
    response_var = "AB_tot",
    predictor_vars = c(
      "X_L93",
      "Y_L93",
      "Terre_fine_..2mm._g.kg",
      "Argiles_.0_a_0.002_mm._g.kg",
      "Limons_fins_.0.002_a_0.02_mm._g.kg",
      "Limons_grossiers_.0.02_a_0.05_mm._g.kg",
      "Sables_fins_.0.05_a_0.2_mm._g.kg",
      "Sables_grossiers_.0.2_a_2.0_mm._g.kg",
      "Carbone_.C._total_g.kg",
      "pH",
      "Matiere_organique_g.kg"
    ),
    data = train
  )

print(result_gam)




# 3.a) On tente de modéliser le salaire des joueurs par un pack de 19 variables
#    Pour chaque complexité allant de 1 à 19, on sélectionne le meilleur modèle
#    grâce à une forward selection (sur le training set)
#    On va donc obtenir 19 modèles comprenant entre une et 19 variables
best_models19_forward = regsubsets(AB_tot ~ .,
                                   data = train[, c(6, 8, 67:77, 79, 80)],
                                   nvmax = 14,
                                   method = 'forward')

# 3.b) Pour accéder aux cpefficient du modèle 5, on appelle la fonction coeff
#      Pour accéder aux RSS des modèles, on lance la fonction summary
coef(best_models19_forward, 3)
summary(best_models19_forward)$rss

mse = rep(NA, 14)
tt = model.matrix(AB_tot ~ ., data = test)
for (i in 1:14) {
  coefi = coef(best_models19_forward, id = i)
  pred = tt[, names(coefi)] %*% coefi
  mse[i] = mean((test$AB_tot - pred) ^ 2)
}
