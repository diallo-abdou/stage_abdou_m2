


#                            STAGE ECOLEG                                      #

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
# Explorations -------------------------------------------------------------



# Pour comparer facilement les genotypes: moyenne sur les données brutes
data_summary <- function(data, varname, groupnames) {
  require(plyr)
  summary_func <- function(x, col) {
    c(mean_val = mean(x[[col]], na.rm = TRUE),
      sd_val = sd(x[[col]], na.rm = TRUE))
  }
  data_sum <- ddply(data, groupnames, .fun = summary_func,
                    varname)
  #data_sum <- rename(data_sum, c("mean" = varname))
  return(data_sum)
}
data_summary(data = data,
             varname = "bm_totale",
             groupnames = "geno")
comp_geno <-
  function(donnee,
           nom_var,
           genotype,
           couleurs,
           affiche_point) {
    p <-
      plot_boxplot(
        donnee = donnee,
        x_col = genotype,
        y_col = nom_var,
        x_label = genotype,
        y_label = nom_var,
        title = "",
        legend_title = "geno",
        couleurs = couleurs,
        affiche_point = affiche_point
      )
    #comp <- donnee %>% group_by({{ genotype }}) %>% summarise(moy = mean({{ nom_var }}, na.rm = TRUE))
    comp <-
      data_summary(data = donnee,
                   varname = nom_var,
                   groupnames = genotype)
    return(list(p, comp))
  }
comp_geno(
  donnee = data,
  nom_var = "bm_totale",
  genotype = "geno",
  couleurs = coul,
  affiche_point = TRUE
)



# Exprimé le pourcentage d'augmentation ou de diminution entre WW et WS
pourc <- function(donnee, var) {
  donnee_ws <- donnee[which(donnee$condition == "WS"), ]
  donnee_ww <- donnee[which(donnee$condition == "WW"), ]
  
  moy_ws <- round (mean(donnee_ws[[var]]),2)
  moy_ww <- round (mean(donnee_ww[[var]]),2)
  
  pourcentage <- round ((moy_ws - moy_ww) / moy_ww * 100, 2)
  
  if (sign(pourcentage) > 0){
    pourcentage_text <- paste0("Le pourcentage d'augmentation est: ", pourcentage)
  } else {
    pourcentage_text <- paste0("Le pourcentage de diminution est: ", pourcentage)
  }
  
  ws <- paste0("La moyenne en WS est: ", moy_ws)
  ww <- paste0("La moyenne en WW est: ", moy_ww)
  
  
  return(list(ws, ww, pourcentage_text))
}
pourc(donnee = aub, var = "bm_totale")


# Statistiques ------------------------------------------------------------


# Pour comparer facilement les genotypes: Post hoc mnuel
combi_posthoc <-
  function(donnee,
           nom_var,
           genotype = "geno",
           nom_test = wilcox.test) {
    genotypes <- levels(donnee[[genotype]])
    num_genotypes <- length(genotypes)
    result <- vector("list", num_genotypes)
    
    for (i in 1:(num_genotypes - 1)) {
      for (j in (i + 1):num_genotypes) {
        geno1 <- genotypes[i]
        geno2 <- genotypes[j]
        geno1_data <- donnee[donnee[[genotype]] == geno1, ]
        geno2_data <- donnee[donnee[[genotype]] == geno2, ]
        
        if (nrow(geno1_data) < 2 || nrow(geno2_data) < 2) {
          result[[i]][[j]] <- NA
        } else {
          result[[i]][[j]] <- list(
            genotype1 = geno1,
            genotype2 = geno2,
            test_result = nom_test(geno1_data[[nom_var]], geno2_data[[nom_var]])
          )
        }
      }
    }
    
    names(result) <- genotypes
    result
  }
combi_posthoc(
  donnee = aub,
  nom_var = "shoot_root",
  genotype = "geno",
  nom_test = wilcox.test
)

#pour le  pairwise.t.test()
tri.to.squ <- function(x) {
  rn <- row.names(x)
  cn <- colnames(x)
  an <- unique(c(cn, rn))
  myval <- x[!is.na(x)]
  mymat <-
    matrix(
      1,
      nrow = length(an),
      ncol = length(an),
      dimnames = list(an, an)
    )
  for (ext in 1:length(cn))
  {
    for (int in 1:length(rn))
    {
      if (is.na(x[row.names(x) == rn[int], colnames(x) == cn[ext]]))
        next
      mymat[row.names(mymat) == rn[int], colnames(mymat) == cn[ext]] <-
        x[row.names(x) == rn[int], colnames(x) == cn[ext]]
      mymat[row.names(mymat) == cn[ext], colnames(mymat) == rn[int]] <-
        x[row.names(x) == rn[int], colnames(x) == cn[ext]]
    }
  }
  return(mymat)
}

# Pour les analyses statistique
analyse_stats <- function(donnee, nom_var, genotype) {
  bar <- bartlett.test(donnee[[nom_var]], donnee[[genotype]])$p.value
  Kteste <-
    kruskal.test(donnee[[nom_var]] ~ donnee[[genotype]]) # YES !!!!
  p_value = Kteste$p.value
  if (Kteste$p.value < 0.001) {
    p_value = paste0("< ", 0.001)
  } else{
    p_value = paste0("= ", round(p_value, 3))
  }
  
  
  mod <- aov(donnee[[nom_var]] ~ donnee[[genotype]])
  residu <- shapiro.test(resid(mod))$p.value
  res_anova = anova(mod)
  pvalue = res_anova$`Pr(>F)`[1]
  if (res_anova$`Pr(>F)`[1] < 0.001) {
    pvalue = paste0("< ", 0.001)
  } else{
    pvalue = paste0("= ", round(pvalue, 3))
  }
  
  if (bar < 0.05) {
    cat(
      "***L'égalité des variances n'est pas respectée: le p-value est:",
      bar,
      "\n Application des tests non paramétriques: WILCOXON.\n"
    )
    pp <-
      pairwise.wilcox.test(
        donnee[[nom_var]],
        donnee[[genotype]],
        p.adjust.method = "bonferroni",
        paired = FALSE,
        pool.sd = FALSE
      )
    mymat <- tri.to.squ(pp$p.value)
    nom_test = paste0("Kruskal-Wallis, chi-squared = ",
                      round(Kteste$statistic, 2),
                      ", p-value ",
                      p_value)
    # Création des lettres correspondant à chaque moyenne
    myletters <-
      multcompLetters(mymat,
                      compare = "<=",
                      threshold = 0.05,
                      Letters = letters)
    # Conserver les lettres dans un data.frame
    myletters_df <-
      data.frame(group = names(myletters$Letters),
                 letter = myletters$Letters)
    #}
    return(list(myletters_df, nom_test))
  } else {
    if (bar > 0.05) {
      cat(
        "***L'égalité des variances est respectée: le p-value est:",
        bar,
        "\n Création d'un modèle ANOVA à un facteur.\n"
      )
      if (residu > 0.05) {
        cat(
          "*** Les résidus du modèle sont normaux:",
          residu,
          "\n Application des tests paramétriques: ANOVA\n"
        )
        tuktuk <-
          TukeyHSD(
            mod,
            ordered = FALSE,
            conf.level = 0.95,
            na.rm = TRUE
          )
        myletters <- multcompLetters4(mod, tuktuk)
        myletters <- myletters$`donnee[[genotype]]`
        myletters_df <-
          data.frame(group = names(myletters$Letters),
                     letter = myletters$Letters)
        colnames(myletters_df) <- c("group", "letter")
        rownames(myletters_df) <- myletters_df$group
        
        nom_test = paste0("ANOVA, F value = ",
                          round(res_anova$`F value`[1], 2),
                          ", p-value ",
                          pvalue)
        return(list(myletters_df, nom_test))
      } else {
        cat(
          "Les résidus du modèle ne sont pas normaux:",
          residu,
          "\n Application des tests non paramétriques: WILCOXON.\n"
        )
        pp <-
          pairwise.wilcox.test(
            donnee[[nom_var]],
            donnee[[genotype]],
            p.adjust.method = "bonferroni",
            paired = FALSE,
            pool.sd = FALSE
          )
        mymat <- tri.to.squ(pp$p.value)
        nom_test = paste0(
          "Kruskal-Wallis, chi-squared = ",
          round(Kteste$statistic, 2),
          ", p-value ",
          p_value
        )
        # Création des lettres correspondant à chaque moyenne
        myletters <-
          multcompLetters(
            mymat,
            compare = "<=",
            threshold = 0.05,
            Letters = letters
          )
        # Conserver les lettres dans un data.frame
        myletters_df <-
          data.frame(group = names(myletters$Letters),
                     letter = myletters$Letters)
        #}
        return(list(myletters_df, nom_test))
      }
    }
  }
}

analyse_stats(donnee = data_df,
              nom_var = "Valeurs",
              genotype = "models")


# Calcule des effets du stress hydriques ----------------------------------


calcul_y <- function(donnee, genotype, nom_var, modalite) {
  result <-
    matrix(0, nrow = 50, ncol = length(levels(donnee[[genotype]])))
  colnames(result) <- levels(donnee[[genotype]])
  
  for (k in levels(donnee[[genotype]])) {
    gen <- donnee[donnee[[genotype]] == k,]
    boots <- numeric(50)
    
    for (l in 1:50) {
      y_ws <-
        sample(gen[[nom_var]][gen[[modalite]] == "WS"], size = 1, replace = TRUE)
      y_ww <-
        sample(gen[[nom_var]][gen[[modalite]] == "WW"], size = 1, replace = TRUE)
      boots[l] <- (y_ws - y_ww) / y_ww
    }
    
    result[, k] <- boots
  }
  
  result_df <- data.frame(geno = rep(colnames(result), each = 50))
  col_name <- paste0("y_", nom_var)
  result_df[col_name] <- as.vector(result)
  
  result_df$geno <- as.factor(result_df$geno)
  return(list(result, result_df))
  
}
calcul_y(
  donnee = data,
  genotype = "geno",
  nom_var = "bm_racinaire",
  modalite = "condition"
)


# Calculer y et faire les analyses stats
calcul_y_stats <- function(donnee, genotype, nom_var, modalite) {
  result <-
    matrix(0, nrow = 50, ncol = length(levels(donnee[[genotype]])))
  colnames(result) <- levels(donnee[[genotype]])
  
  for (k in levels(donnee[[genotype]])) {
    gen <- donnee[donnee[[genotype]] == k,]
    boots <- numeric(50)
    
    for (l in 1:50) {
      y_ws <-
        sample(gen[[nom_var]][gen[[modalite]] == "WS"], size = 1, replace = TRUE)
      y_ww <-
        sample(gen[[nom_var]][gen[[modalite]] == "WW"], size = 1, replace = TRUE)
      boots[l] <- (y_ws - y_ww) / y_ww
    }
    
    result[, k] <- boots
  }
  
  result_df <- data.frame(geno = rep(colnames(result), each = 50))
  col_name <- paste0("y_", nom_var)
  result_df[col_name] <- as.vector(result)
  
  result_df$geno <- as.factor(result_df$geno)
  
  # Appel de la fonction analyse_stats avec les paramètres appropriés
  ana = analyse_stats(donnee = result_df,
                      nom_var = col_name,
                      genotype = "geno")
  ana
}
calcul_y_stats(
  donnee = data,
  genotype = "geno",
  nom_var = "bm_racinaire",
  modalite = "condition"
)



# Pour les graphiques  -------------------------------------------------------

## Pour les boxplot
# la couleur des génotypes
coul = c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2")
coul2 = c("#E69F00", "#1F77B4", "#009E73", "#F0E442", "#9467BD")
coul3=c("#1F77B4", "#7F7F7F", "#2CA02C", "#D62728", "#9467BD") # utilisé pour mon rapport de stage
get_legend <- function(plot) {
  g <- ggplotGrob(plot)
  leg <- g$grobs[[which(g$layout$name == "guide-box")]]
  return(leg)
}
is_outlier <- function(x) {
  return(x < quantile(x, 0.25) - 1.5 * IQR(x) |
           x > quantile(x, 0.75) + 1.5 * IQR(x))
}
### boxplote simple sans odre
plot_boxplot <-
  function(donnee,
           x_col,
           y_col,
           x_label,
           y_label,
           title,
           legend_title,
           couleurs,
           affiche_point = TRUE,
           ymin = min(donnee[[y_col]]),
           ymax = 1.2 * max(donnee[[y_col]])) {
    graphe <-
      ggplot(donnee,
             aes_string(
               x = x_col,
               y = y_col,
               colour = x_col
             )) +
      geom_boxplot(
        outlier.shape = NA,
        outlier.colour = "black",
        alpha = 0.25,
        size = 0.5
      ) +
      labs(
        title = title,
        x = x_label,
        y = y_label
      ) +
      scale_color_manual(values = couleurs, name = legend_title) +
      theme_classic(base_size = 12, base_family = "Arial") +
      theme(
        axis.text = element_text(size = 10),
        axis.title.y = element_text(
          vjust = 5,
          size = 12,
          face = "bold"
        ),
        axis.title.x = element_text(face = "bold"),
        axis.ticks.length = unit(0.2, "cm"),
        legend.position = "none",  # Cette ligne supprime la légende
        #legend.position = "right",
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 12, face = "bold"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(size = 14, face = "bold"),
        plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm")
      )
    if (affiche_point) {
      graphe <-
        graphe + geom_jitter(position = position_jitter(seed = 1), size = 2)
    }
    
    if (y_col %in% names(donnee)) {
      graphe <- graphe +
        coord_cartesian(ylim = c(ymin, ymax))
    }
    graphe = graphe + stat_summary(
      fun.y = mean,
      geom = "point",
      shape = 15,
      size = 2,
      col = "black",
      fill = "black"
    )
    return(graphe)
  }
plot_boxplot(
  donnee = data_df,
  x_col = "models",
  y_col = "Valeurs",
  x_label = "models",
  y_label = "bm_racinaire",
  title = "Well-watered plant",
  legend_title = "genotypes",
  couleurs = coul3,
  affiche_point = T
)

### boxplote simple avec ordre
plot_boxplot2 <-
  function(donnee,
           x_col,
           y_col,
           x_label,
           y_label,
           title,
           legend_title,
           couleurs,
           affiche_point = TRUE,
           ymax = 1.1) {
    # Calculer la moyenne de y_col pour chaque niveau de x_col
    moyennes <-
      aggregate(donnee[[y_col]], by = list(donnee[[x_col]]), FUN = mean)
    
    # Trier les niveaux de x_col en fonction des moyennes dans l'ordre décroissant
    niveaux_tries <-
      moyennes[order(moyennes$x, decreasing = TRUE), "Group.1"]
    
    # Réorganiser les niveaux de x_col dans le jeu de données
    donnee[[x_col]] <- factor(donnee[[x_col]], levels = niveaux_tries)
    
    
    graphe <-
      ggplot(donnee,
             aes_string(
               x = x_col,
               y = y_col,
               colour = x_col,
               fill = x_col
             )) +
      geom_boxplot(
        outlier.shape = NA,
        outlier.colour = "black",
        alpha = 0.25,
        size = 0.5
      ) +
      labs(
        title = title,
        x = x_label,
        y = y_label,
        fill = legend_title
      ) +
      scale_fill_manual(values = couleurs) +
      scale_color_manual(values = couleurs) +
      theme_classic(base_size = 12, base_family = "Arial") +
      theme(
        axis.text = element_text(size = 10),
        axis.title.y = element_text(
          vjust = 5,
          size = 12,
          face = "bold"
        ),
        axis.title.x = element_text(face = "bold"),
        axis.ticks.length = unit(0.2, "cm"),
        legend.position = "right",
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 12, face = "bold"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(size = 14, face = "bold"),
        plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm")
      )
    if (affiche_point) {
      graphe <-
        graphe + geom_jitter(position = position_jitter(seed = 1), size = 2)
    }
    
    if (y_col %in% names(donnee)) {
      graphe <- graphe +
        coord_cartesian(ylim = c(min(donnee[[y_col]]), ymax * max(donnee[[y_col]])))
    }
    return(graphe)
    
    return(graphe)
  }
plot_boxplot2(
  donnee = test,
  x_col = "geno",
  y_col = "bm_totale",
  x_label = "geno",
  y_label = "bm_totale",
  title = "Well-watered plant",
  legend_title = "geno",
  couleurs = coul,
  affiche_point = FALSE
)


#Pour comparer les geno dans ww et dans ws mais aussi chaque geno
plot_ggboxplot_comp <-
  function(donnee,
           genotypes,
           nom_var,
           traitements,
           nom_pwc,
           title,
           x_label,
           y_label,
           couleurs_geno=coul3,
           ymin = min(donnee[[nom_var]]),
           ymax = 1.2 * max(donnee[[nom_var]])) {
    donnee[[traitements]] <- fct_relevel(donnee[[traitements]], c("WW", "WS"))
    graphe <-
      ggboxplot(
        donnee,
        x = genotypes,
        y = nom_var,
        fill = traitements,
        scales = "free",
        color=genotypes
      ) +
      labs(
        title = title,
        x = x_label,
        y = y_label,
        fill = "Traitements"
      ) +
      stat_pvalue_manual(nom_pwc, label = "p.adj.signif") +
      scale_color_manual(values = couleurs_geno, 
                         name = "Génotypes",
                         labels = c("CC23","CC51","CC91","CC96","CC100"))+
      theme(
        axis.text = element_text(size = 10),
        axis.title.y = element_text(
          vjust = 5,
          size = 12,
          face = "bold"
        ),
        axis.title.x = element_text(face = "bold"),
        axis.ticks.length = unit(0.2, "cm"),
        legend.position = "none",
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 12, face = "bold"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(size = 14, face = "bold"),
        plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm")
      )
    
    if (nom_var %in% names(donnee)) {
      graphe <- graphe + coord_cartesian(ylim = c(ymin, ymax))
    }
    
    return(graphe)
  }
plot_ggboxplot_comp(
  donnee = cf,
  genotypes = "geno",
  nom_var = "bm_tige",
  traitements = "condition",
  nom_pwc = pwc,
  title = "",
  x_label = "Génotypes",
  y_label = "SLA",couleurs_geno = coul
)



# pour les traitements
plot_ggboxplot_comp_cond <- function(donnee,
                                     genotypes,
                                     nom_var,
                                     traitements,
                                     nom_pwc,
                                     title,
                                     x_label,
                                     y_label,
                                     ymin = min(donnee[[nom_var]]),
                                     ymax = 1.2 * max(donnee[[nom_var]])) {
  donnee[[traitements]] <- fct_relevel(donnee[[traitements]], c("WW", "WS"))
  
  myColors <- ifelse(levels(donnee[[traitements]]) == "WW", "blue", 
                     ifelse(levels(donnee[[traitements]]) == "WS", "red", "grey90"))
  
  graphe <- ggboxplot(
    donnee,
    x = genotypes,
    y = nom_var,
    fill = traitements,  # Correction de la variable traitements
    scales = "free"
  )  +
    scale_fill_manual(values = myColors, name = "Traitements",
                      labels = c("WW","WS"))+ 
    labs(
      title = title,
      x = x_label,
      y = y_label,
      fill = "Traitements"
    ) +
    stat_pvalue_manual(nom_pwc, label = "p.adj.signif") +
    theme(
      axis.text = element_text(size = 10),
      axis.title.y = element_text(
        vjust = 5,
        size = 12,
        face = "bold"
      ),
      axis.title.x = element_text(face = "bold"),
      axis.ticks.length = unit(0.2, "cm"),
      legend.position = "right",
      legend.text = element_text(size = 10),
      legend.title = element_text(size = 12, face = "bold"),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      plot.title = element_text(size = 14, face = "bold"),
      plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm")
    )
  
  if (nom_var %in% names(donnee)) {
    graphe <- graphe + coord_cartesian(ylim = c(ymin, ymax))
  }
  
  return(graphe)
}

plot_ggboxplot_comp_cond(
  donnee = cf,
  genotypes = "geno",
  nom_var = "sla",
  traitements = "condition",
  nom_pwc = pwc,
  title = "",
  x_label = "Génotypes",
  y_label = "SLA"
)
## diagramme simple
plot_diag_barre_simpl <-
  function(donnee,
           genotype,
           nom_var,
           title,
           y_label,
           x_label) {
    comp <-
      data_summary(data = donnee,
                   varname = nom_var,
                   groupnames = genotype)
    comp[[genotype]] = as.factor(comp[[genotype]])
    
    g <- ggplot(comp, aes(x = geno, y = mean_val)) +
      geom_bar(stat = "identity",
               color = "black",
               position = position_dodge()) +
      geom_errorbar(
        aes(ymin = mean_val, ymax = mean_val + sd_val),
        width = .2,
        position = position_dodge(.9)
      )  +
      labs(title = title, x = x_label, y = y_label) +
      theme_classic(base_size = 12) +
      theme(
        axis.text = element_text(face = "bold", size = 12),
        axis.ticks.margin = unit(c(0.5), 'cm'),
        axis.title.y = element_text(
          vjust = 3,
          size = 12,
          face = "bold"
        ),
        axis.title.x = element_text(size = 12, face = "bold"),
        axis.ticks = element_line(size = 1),
        axis.ticks.length = unit(0.2, "cm"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title = element_text(vjust = 5, size = 12),
        plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm")
      )
    
    g
  }
plot_diag_barre_simpl(
  donnee = y_bm_racinaire[[2]],
  genotype = "geno",
  nom_var = "y_bm_racinaire",
  title = "",
  y_label = "Biomasse racinaire",
  x_label = "genotype"
)

## idag simple mais avec des donnée negatif et classé selon les effets
plot_diag_barre_simpl_neg <-
  function(donnee,
           genotype,
           nom_var,
           title,
           y_label,
           x_label,
           ymax = 1.2) {
    comp <-
      data_summary(data = donnee,
                   varname = nom_var,
                   groupnames = genotype)
    comp[[genotype]] <- as.factor(comp[[genotype]])
    comp <-
      comp %>% arrange(desc(mean_val))  # Tri par ordre décroissant de la moyenne (mean_val)
    comp[[genotype]] <-
      reorder(comp[[genotype]], desc(comp$mean_val))  # Réorganiser les niveaux du facteur geno en fonction de la moyenne
    
    g <- ggplot(comp, aes(x = geno, y = mean_val)) +
      geom_bar(stat = "identity",
               color = "black",
               position = position_dodge()) +
      geom_errorbar(
        aes(ymin = mean_val, ymax = mean_val - sd_val),
        width = 0.2,
        position = position_dodge(0.9)
      ) +
      labs(title = title, x = x_label, y = y_label) +
      theme_classic(base_size = 12) +
      theme(
        axis.text = element_text(face = "bold", size = 12),
        axis.ticks.margin = unit(c(0.5), 'cm'),
        axis.title.y = element_text(
          vjust = 3,
          size = 12,
          face = "bold"
        ),
        axis.title.x = element_text(size = 12, face = "bold"),
        axis.ticks = element_line(size = 1),
        axis.ticks.length = unit(0.2, "cm"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title = element_text(vjust = 5, size = 12),
        plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm")
      ) +
      coord_cartesian(ylim = c(0, ymax * mean(comp$mean_val)))  # Déplacer l'axe des x à y = 0
    
    g
  }
plot_diag_barre_simpl_neg(
  donnee = y_bm_racinaire[[2]],
  genotype = "geno",
  nom_var = "y_bm_racinaire",
  title = "",
  y_label = "Biomasse racinaire",
  x_label = "genotype"
)


## pour les diag_barre compare
plot_diag_barre_comp <-
  function(donnee, genotype, nom_var, nom_complet) {
    donnee_ww <- donnee[donnee$condition == "WW",]
    donnee_ws <- donnee[donnee$condition == "WS",]
    
    g_ww <-
      ggplot(donnee_ww, aes(
        x = !!rlang::sym(genotype),
        y = !!rlang::sym(nom_var)
      )) +
      geom_bar(stat = "identity", alpha = 0.7) +
      labs(title = "Well-watered plant", x = "", y = nom_complet) +
      theme_classic(base_size = 12) +
      theme(
        axis.text = element_text(face = "bold", size = 12),
        axis.ticks.margin = unit(c(0.5), 'cm'),
        axis.title.y = element_text(vjust = 3, size = 12),
        axis.ticks = element_line(size = 1),
        axis.ticks.length = unit(0.2, "cm"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title = element_text(vjust = 5, size = 12),
        plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm")
      )
    
    g_ws <-
      ggplot(donnee_ws, aes(
        x = !!rlang::sym(genotype),
        y = !!rlang::sym(nom_var)
      )) +
      geom_bar(stat = "identity", alpha = 0.7) +
      labs(title = "Water-stressed plant", x = "", y = "") +
      theme_classic(base_size = 12) +
      theme(
        axis.text = element_text(face = "bold", size = 12),
        axis.ticks.margin = unit(c(0.5), 'cm'),
        axis.title.y = element_text(vjust = 3, size = 12),
        axis.ticks = element_line(size = 1),
        axis.ticks.length = unit(0.2, "cm"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title = element_text(vjust = 5, size = 12),
        plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm")
      )
    
    
    # Combinaison des deux graphiques et ajout de la légende commune
    graph_combi <-
      plot_grid(
        g_ww + theme(),
        g_ws + theme(),
        ncol = 2,
        align = "h",
        axis = "tb",
        rel_widths = c(1, 1),
        labels = c("(a)", "(b)"),
        label_size = 12,
        label_fontface = "bold"
      )
    graph_combi <- ggdraw() +
      draw_plot(graph_combi) +
      draw_label(
        expression("Genotypes"),
        fontface = "bold",
        size = 12,
        x = 0.5,
        y = 0.03
      )
    graph_combi
  }
plot_diag_barre_comp(
  donnee = data,
  genotype = "geno",
  nom_var = "bm_racinaire",
  nom_complet = "Biomasse racinaire"
)


## diagramme en barre avec couleurs
plot_diag_barre_comp_coul <-
  function(donnee,
           x_col,
           y_col,
           x_label,
           y_label,
           title,
           legend_title,
           couleurs,
           affiche_point = TRUE) {
    donnee_ww <- donnee[donnee$condition == "WW",]
    donnee_ws <- donnee[donnee$condition == "WS",]
    
    g_ww <-
      ggplot(donnee_ww,
             aes_string(
               x = x_col,
               y = y_col,
               colour = x_col,
               fill = x_col
             )) +
      geom_bar(stat = "identity", alpha = 0.7) +
      labs(
        title = "Well-watered plant",
        x = "",
        y = y_label,
        fill = legend_title
      ) +
      scale_fill_manual(values = couleurs) +
      scale_color_manual(values = couleurs) +
      theme_classic(base_size = 12, base_family = "Arial") +
      theme(
        axis.text = element_text(size = 10),
        axis.title.y = element_text(
          vjust = 3,
          size = 12,
          face = "bold"
        ),
        axis.ticks.length = unit(0.2, "cm"),
        legend.position = "right",
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 12, face = "bold"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(size = 14, face = "bold"),
        plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm")
      ) + guides(fill = FALSE, color = FALSE)
    g_ws <-
      ggplot(donnee_ws,
             aes_string(
               x = x_col,
               y = y_col,
               colour = x_col,
               fill = x_col
             )) +
      geom_bar(stat = "identity", alpha = 0.7) +
      labs(
        title = "Water-stressed plant",
        x = "",
        y = "",
        fill = legend_title
      ) +
      scale_fill_manual(values = couleurs) +
      scale_color_manual(values = couleurs) +
      theme_classic(base_size = 12, base_family = "Arial") +
      theme(
        axis.text = element_text(size = 10),
        axis.title.y = element_text(
          vjust = 3,
          size = 12,
          face = "bold"
        ),
        axis.ticks.length = unit(0.2, "cm"),
        legend.position = "right",
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 12, face = "bold"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(size = 14, face = "bold"),
        plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm")
      )
    
    # Test de différence de génotypes
    
    # Combinaison des deux graphiques et ajout de la légende commune
    graph_combi <-
      plot_grid(
        g_ww + theme(),
        g_ws + theme(),
        ncol = 2,
        align = "h",
        axis = "tb",
        rel_widths = c(1, 1.69201931299294),
        labels = c("(a)", "(b)"),
        label_size = 12,
        label_fontface = "bold"
      )
    graph_combi <- ggdraw() +
      draw_plot(graph_combi) +
      draw_label(
        label = x_label,
        fontface = "bold",
        size = 12,
        x = 0.5,
        y = 0.03
      )
    graph_combi
    
    # Obtention des largeurs
    #gtable_combi <- ggplot_gtable(ggplot_build(graph_combi))
    #g1_width <- gtable_combi$widths[2]
    #g2_width <- gtable_combi$widths[4]
    
    #legend_ws <- get_legend(g_ws)
    #legend_width <- sum(legend_ws$widths)
    #return(list(g1_width,g2_width,legend_width))
    
  }
plot_diag_barre_comp_coul(
  data,
  x_col = "geno",
  y_col = "bm_racinaire",
  x_label = "geno",
  y_label = "bm_racinaire",
  title = "Well-watered plant",
  legend_title = "geno",
  couleurs = coul3,
  affiche_point = FALSE
)





# Fonction global (tests, calcule y, graphe et affiche les lettes) --------------------------------------------------------


## Simple avec boxplote
fsg <-
  function(donnee,
           nom_var,
           genotype,
           x_label,
           y_label,
           title,
           legend_title,
           couleurs,
           affiche_point = TRUE,
           lettre_postion = max(donnee[[nom_var]]),
           ymin = min(donnee[[nom_var]]),
           ymax = 1.2 * max(donnee[[nom_var]])){
    library(dplyr)
    library(ggplot2)
    
    rest <-
      analyse_stats(donnee = donnee,
                    nom_var = nom_var,
                    genotype = genotype)
    myletters_df <- rest[[1]]
    nom_stat <- rest [[2]]
    
    donnee$group <- NA
    for (i in 1:length(donnee[[nom_var]])) {
      for (j in 1:length(myletters_df$group)) {
        if (donnee[[nom_var]][i] == myletters_df$group[j]) {
          donnee$group[i] <- as.character(myletters_df$letter[j])
        } else {
        }
      }
    }
    
    p <- plot_boxplot(
      donnee = donnee,
      x_col = genotype,
      y_col = nom_var,
      x_label = x_label,
      y_label = y_label,
      title = title,
      legend_title = legend_title,
      couleurs = couleurs,
      affiche_point = affiche_point,
      ymin = ymin,
      ymax = ymax)
    
    p + labs(subtitle = paste0(nom_stat)) +
      geom_text(data = donnee,
                aes(label = group, y = lettre_postion))
  
  }
fsg(
  donnee = data_df,
  nom_var = "Valeurs",
  genotype = "models",
  x_label = "geno",
  y_label = "bm_racinaire",
  title = "Well-watered plant",
  legend_title = "geno",
  couleurs = coul3,
  affiche_point = TRUE,lettre_postion = 9000)
## Pour les donnée neagtif avec diagrame en barre selon les effets du stress
fsg_neg <-
  function(donnee,
           nom_var,
           genotype,
           x_label,
           y_label,
           title,
           legend_title,
           couleurs,
           lettre_postion = 1.08,
           ymax = 1.1)
  {
    library(dplyr)
    library(ggplot2)
    
    rest <-
      analyse_stats(donnee = donnee,
                    nom_var = nom_var,
                    genotype = genotype)
    myletters_df <- rest[[1]]
    nom_stat <- rest [[2]]
    
    donnee$group <- NA
    for (i in 1:length(donnee[[nom_var]])) {
      for (j in 1:length(myletters_df$group)) {
        if (donnee$geno[i] == myletters_df$group[j]) {
          donnee$group[i] <- as.character(myletters_df$letter[j])
        } else {
        }
      }
    }
    
    p <- plot_diag_barre_simpl_neg(
      donnee = donnee,
      genotype = genotype,
      nom_var = nom_var,
      title = title,
      y_label = y_label,
      x_label = x_label,
      ymax = ymax
    )
    
    
    p + labs(subtitle = paste0(nom_stat)) +
      geom_text(data = donnee,
                aes(label = group, y = lettre_postion * max(donnee[[nom_var]][!is.na(donnee[[nom_var]])])))
  }

fsg_neg(
  donnee = y_bm_racinaire[[2]],
  nom_var = "y_bm_racinaire",
  genotype = "geno",
  x_label = "geno",
  y_label = "bm_racinaire",
  title = "",
  legend_title = "geno",
  lettre_postion = -5,
  ymax = 1.36
)

## Simple avec boxplote selon l'ordre des effets du stress hydrique
fsg2 <-
  function(donnee,
           nom_var,
           genotype,
           x_label,
           y_label,
           title,
           legend_title,
           couleurs,
           affiche_point = TRUE,
           lettre_postion = 1.08,
           ymax = 1.1)
  {
    library(dplyr)
    library(ggplot2)
    
    rest <-
      analyse_stats(donnee = donnee,
                    nom_var = nom_var,
                    genotype = genotype)
    myletters_df <- rest[[1]]
    nom_stat <- rest [[2]]
    
    donnee$group <- NA
    for (i in 1:length(donnee[[nom_var]])) {
      for (j in 1:length(myletters_df$group)) {
        if (donnee$geno[i] == myletters_df$group[j]) {
          donnee$group[i] <- as.character(myletters_df$letter[j])
        } else {
        }
      }
    }
    
    p <- plot_boxplot2(
      donnee = donnee,
      x_col = genotype,
      y_col = nom_var,
      x_label = x_label,
      y_label = y_label,
      title = title,
      legend_title = legend_title,
      couleurs = couleurs,
      affiche_point = affiche_point,
      ymax = ymax
    )
    
    p + labs(subtitle = paste0(nom_stat)) +
      geom_text(data = donnee,
                aes(label = group, y = lettre_postion * max(donnee[[nom_var]][!is.na(donnee[[nom_var]])])))
  }
fsg2(
  donnee = test,
  nom_var = "bm_totale",
  genotype = "geno",
  x_label = "geno",
  y_label = "bm_racinaire",
  title = "Well-watered plant",
  legend_title = "geno",
  couleurs = coul,
  affiche_point = TRUE
)



# Code pour facilité la rédaction du script -----------------------------------------

variables <-
  c(
    "surf_foliaire",
    "surf_tige",
    "surf_aerienne",
    "nbr_feuilles",
    "bm_feuille",
    "bm_tige",
    "bm_aerienne",
    "bm_totale",
    "shoot_root",
    "sla"
  )
code_texte <- ""
for (var in variables) {
  code <- paste0(
    "## Pour la ",
    var,
    " -----------------------------------------------\n",
    "y_",
    var,
    "=calcul_y(donnee = data, genotype = \"geno\", nom_var = \"",
    var,
    "\", modalite = \"condition\");y_",
    var,
    "\n",
    "analyse_stats(donnee = y_",
    var,
    "[[2]], nom_var = \"y_",
    var,
    "\", genotype = \"geno\")\n",
    "verif=y_",
    var,
    "[[2]]\n",
    "kruskal.test(verif$y_",
    var,
    " ~ verif$geno)\n",
    "pairwise.wilcox.test(verif$y_",
    var,
    " , verif$geno, p.adjust.method = \"bonferroni\", paired = FALSE, pool.sd = FALSE)\n\n",
    "fsg(donnee = y_",
    var,
    "[[2]], nom_var = \"y_",
    var,
    "\", genotype = \"geno\", x_label = \"geno\", ",
    "y_label = \"",
    var,
    "\", title = \"\", legend_title = \"geno\", couleurs = coul, ",
    "affiche_point = FALSE, lettre_postion = -0.5, ymax = 0)\n\n"
  )
  
  code_texte <- paste0(code_texte, code)
}
# Coller le code dans le script
cat(code_texte, file = "brou_function.R", append = TRUE)


variables = c("perimeter_cm", "area_cm", "profondeur_cm", "largeur_cm")
code_texte <- ""
for (var in variables) {
  code <- paste0(
    "# Pour la ",
    var,
    " -----------------------------------------------\n",
    "g1 <- fsg(donnee = R807_ww, nom_var = \"",
    var,
    "\", genotype = \"geno\", x_label = \"Génotypes\", y_label = \"",
    var,
    "\",
              title = \"ww\", legend_title = \"Génotypes\", couleurs = coul, affiche_point = )\n",
    "g2 <- fsg(donnee = R807_ws, nom_var = \"",
    var,
    "\", genotype = \"geno\", x_label = \"Génotypes\", y_label = \"",
    var,
    "\",
              title = \"ws\", legend_title = \"Génotypes\", couleurs = coul, affiche_point = T)\n",
    "g_",
    var,
    "1 <- ggarrange(g1, g2, labels = c(\"(a)\", \"(b)\"), common.legend = TRUE, legend = \"right\")\n\n",
    "# Caculer les effets du stress sur le ",
    var,
    "\n",
    "y_",
    var,
    " <- calcul_y(donnee = R807, genotype = \"geno\", nom_var = \"",
    var,
    "\", modalite = \"condition\")\n\n",
    "# Verification\n",
    "analyse_stats(donnee = y_",
    var,
    "[[2]], nom_var = \"y_",
    var,
    "\", genotype = \"geno\")\n",
    "verif = y_",
    var,
    "[[2]]\n",
    "kruskal.test(verif$y_",
    var,
    " ~ verif$geno)\n",
    "pairwise.wilcox.test(verif$y_",
    var,
    ", verif$geno, p.adjust.method = \"bonferroni\", paired = FALSE, pool.sd = FALSE)\n\n",
    "# Graphique complet\n",
    "g_",
    var,
    "2 <- fsg(donnee = y_",
    var,
    "[[2]], nom_var = \"y_",
    var,
    "\", genotype = \"geno\", x_label = \"Génotypes\",
                      y_label = \"ISH sur le ",
    var,
    "\", title = \"\", legend_title = \"Génotypes\", couleurs = coul, affiche_point = T)\n\n",
    "g_perimetre_cm2 <- ggarrange(g1, g2, g_",
    var,
    "2, ncol = 3, labels = c(\"(a)\", \"(b)\", \"(c)\"), common.legend = TRUE, legend = \"right\")\n\n",
    "# Pour la rédaction\n",
    "data_summary(data = R807_ww, varname = \"",
    var,
    "\", groupnames = \"geno\")\n",
    "data_summary(data = R807_ws, varname = \"",
    var,
    "\", groupnames = \"geno\")\n\n"
  )
  
  code_texte <- paste0(code_texte, code)
}
# Coller le code dans le script
cat(code_texte, file = "brouillons.R", append = TRUE)



# Dupliquer le code pour chaque élément dans nom0

nom0 = c(
  "rt_surf_foliaire",
  "rt_surf_tige",
  "rt_surf_aerienne",
  "rt_nbr_feuilles",
  "rt_bm_racinaire",
  "rt_bm_feuille",
  "rt_bm_tige",
  "rt_bm_aerienne",
  "rt_bm_totale",
  "rt_shoot_root",
  "rt_sla"
)

nom_j1 = c(
  "rt_surf_foliaire_j1",
  "rt_surf_tige_j1",
  "rt_surf_aerienne_j1",
  "rt_nbr_feuilles_j1",
  "rt_bm_racinaire_j1",
  "rt_bm_feuille_j1",
  "rt_bm_tige_j1",
  "rt_bm_aerienne_j1",
  "rt_bm_totale_j1",
  "rt_shoot_root_j1",
  "rt_sla_j1"
)

nom_j2 = c(
  "rt_surf_foliaire_j2",
  "rt_surf_tige_j2",
  "rt_surf_aerienne_j2",
  "rt_nbr_feuilles_j2",
  "rt_bm_racinaire_j2",
  "rt_bm_feuille_j2",
  "rt_bm_tige_j2",
  "rt_bm_aerienne_j2",
  "rt_bm_totale_j2",
  "rt_shoot_root_j2",
  "rt_sla_j2"
)
for (i in 1:length(nom0)) {
  var <- nom0[i]
  var_j1 <- nom_j1[i]
  var_j2 <- nom_j2[i]
  
  cat(paste0(
    "## Pour la ",
    var,
    " -----------------------------------------------\n"
  ))
  cat(paste0("### Par quantité d'eau\n"))
  
  # Par quantité d'eau j1
  cat(
    paste0(
      "g1 <- fsg(donnee = evapo_ww, nom_var = '",
      var_j1,
      "', genotype = 'geno', x_label = 'Génotypes', y_label = '",
      var_j1,
      "',\n"
    )
  )
  cat(
    paste0(
      "          title = 'ww j30', legend_title = 'Génotypes', couleurs = coul, affiche_point = T)\n"
    )
  )
  
  # Par quantité d'eau j2
  cat(
    paste0(
      "g2 <- fsg(donnee = evapo_ww, nom_var = '",
      var_j2,
      "', genotype = 'geno', x_label = 'Génotypes', y_label = '",
      var_j2,
      "',\n"
    )
  )
  cat(
    paste0(
      "          title = 'ww j29', legend_title = 'Génotypes', couleurs = coul, affiche_point = T)\n"
    )
  )
  
  cat(
    paste0(
      "g_",
      var,
      " <- ggarrange(g1, g2, labels = c('(a)', '(b)'), common.legend = TRUE, legend = 'right');g_",
      var,
      "\n"
    )
  )
  
  # Pour la rédaction
  cat(
    paste0(
      "data_summary(data = evapo_ww, varname = '",
      var_j1,
      "', groupnames = 'geno')\n"
    )
  )
  cat(
    paste0(
      "data_summary(data = evapo_ww, varname = '",
      var_j2,
      "', groupnames = 'geno')\n\n"
    )
  )
}




# Créer la nouvelle colonne nom_geno
cf$nom_geno <- ifelse(cf$geno == "G1", "CC23",
                      ifelse(cf$geno == "G2", "CC51",
                             ifelse(
                               cf$geno == "G3", "CC91",
                               ifelse(cf$geno == "G4", "CC96",
                                      ifelse(cf$geno == "G5", "CC100", NA))
                             )))

# Les racines -------------------------------------------------------------

#création de nouvelle colonnes (rt, id, ge,....) et enttoyage des donnée dont les doublons
creer_nouvelles_colonnes <- function(donnee, nom_col) {
  # Diviser la première colonne en utilisant le caractère "_"
  racines_decoupees <- strsplit(donnee[[nom_col]], "_")
  # Extraire les éléments individuels et les assigner à de nouvelles colonnes
  donnee$id <- sapply(racines_decoupees, function(x)
    x[4])
  donnee$RT <- sapply(racines_decoupees, function(x)
    x[9])
  donnee$geno <- sapply(racines_decoupees, function(x)
    x[12])
  donnee$condition <- sapply(racines_decoupees, function(x)
    x[13])
  donnee$rep <- sapply(racines_decoupees, function(x)
    x[15])
  donnee$plante <- sapply(racines_decoupees, function(x)
    x[23])
  # Liste des colonnes à convertir en facteurs
  columns_to_convert <- c("id", "geno", "condition", "rep", "plante")
  # Conversion des colonnes en facteurs
  for (col in columns_to_convert) {
    donnee[[col]] <- factor(donnee[[col]])
  }
  # Liste des colonnes à convertir en num
  columns_to_convert2 <-
    c("perimeter", "area", "profondeur", "largeur")
  # Conversion des colonnes en facteurs
  for (b in columns_to_convert2) {
    donnee[[b]] <- as.numeric(donnee[[b]])
  }
  donnee$RT = as.integer(donnee$RT)
  donnee$cle = paste0(
    "ID_",
    donnee$id,
    "_RT_",
    donnee$RT,
    "_",
    donnee$geno,
    "_",
    donnee$condition,
    "_",
    donnee$rep,
    "_",
    donnee$plante
  )
  donnee$col_sum <- rowSums(donnee[2:5])
  # Vérification des doublons dans la colonne 'cle'
  doublons <- duplicated(donnee$cle)
  # Affichage des lignes avec des doublons
  lignes_doublons <- donnee[doublons,]
  # Affichage des doublons
  doublons_uniques <- unique(donnee$cle[doublons])
  # Affichage des résultats
  if (any(doublons)) {
    cat("Les doublons ont été trouvés dans la colonne 'cle'.\n")
    cat("Lignes avec des doublons :\n")
    print(lignes_doublons)
    cat("Valeurs de doublons uniques :\n")
    print(doublons_uniques)
  } else {
    cat("Aucun doublon n'a été trouvé dans la colonne 'cle'.\n")
  }
  
  # Tri du dataframe par ordre décroissant de la colonne 'col_sum'
  donnee <- donnee[order(-donnee$col_sum),]
  
  # Suppression des doublons en conservant uniquement la première occurrence de chaque valeur dans la colonne 'cle'
  data_unique <- donnee[!duplicated(donnee$cle),]
  
  # Retourner le dataframe modifié avec les nouvelles colonnes
  return(data_unique)
}
creer_nouvelles_colonnes(rac2, nom_col = "Label")

# Detection des doublons et nétoyyage
conv_col_doublons <- function(donnee) {
  # Liste des colonnes à convertir en facteurs
  columns_to_convert <- c("id", "geno", "condition", "rep", "plante")
  # Conversion des colonnes en facteurs
  for (col in columns_to_convert) {
    donnee[[col]] <- factor(donnee[[col]])
  }
  # Liste des colonnes à convertir en num
  columns_to_convert2 <-
    c("perimeter", "area", "profondeur", "largeur")
  # Conversion des colonnes en facteurs
  for (b in columns_to_convert2) {
    donnee[[b]] <- as.numeric(donnee[[b]])
  }
  donnee$RT = as.integer(donnee$RT)
  donnee$cle = paste0(
    "ID_",
    donnee$id,
    "_RT_",
    donnee$RT,
    "_",
    donnee$geno,
    "_",
    donnee$condition,
    "_",
    donnee$rep,
    "_",
    donnee$plante
  )
  donnee$col_sum <- rowSums(donnee[columns_to_convert2])
  # Vérification des doublons dans la colonne 'cle'
  doublons <- duplicated(donnee$cle)
  # Affichage des lignes avec des doublons
  lignes_doublons <- donnee[doublons,]
  # Affichage des doublons
  doublons_uniques <- unique(donnee$cle[doublons])
  # Affichage des résultats
  if (any(doublons)) {
    cat("Les doublons ont été trouvés dans la colonne 'cle'.\n")
    cat("Lignes avec des doublons :\n")
    print(lignes_doublons)
    cat("Valeurs de doublons uniques :\n")
    print(doublons_uniques)
  } else {
    cat("Aucun doublon n'a été trouvé dans la colonne 'cle'.\n")
  }
  
  # Tri du dataframe par ordre décroissant de la colonne 'col_sum'
  donnee <- donnee[order(-donnee$col_sum),]
  
  # Suppression des doublons en conservant uniquement la première occurrence de chaque valeur dans la colonne 'cle'
  data_unique <- donnee[!duplicated(donnee$cle),]
  
  # Retourner le dataframe modifié avec les nouvelles colonnes
  return(data_unique)
}
conv_col_doublons(donnee = j25)


# Detection des doublons et nétoyyage
conv_col_doublons_2 <- function(donnee) {
  
  # Liste des colonnes à convertir en num
  columns_to_convert2 <-
    c("perimeter", "area", "profondeur", "largeur")
  # Conversion des colonnes en facteurs
  for (b in columns_to_convert2) {
    donnee[[b]] <- as.numeric(donnee[[b]])
  }
  
  donnee$cle = paste0(
    "_RT_",
    donnee$RT,
    "_",
    donnee$plante,
    "_",
    donnee$geno,
    "_",
    donnee$nom,
    "_",
    donnee$condition
  )
  donnee$col_sum <- rowSums(donnee[columns_to_convert2])
  # Vérification des doublons dans la colonne 'cle'
  doublons <- duplicated(donnee$cle)
  # Affichage des lignes avec des doublons
  lignes_doublons <- donnee[doublons,]
  # Affichage des doublons
  doublons_uniques <- unique(donnee$cle[doublons])
  # Affichage des résultats
  if (any(doublons)) {
    cat("Les doublons ont été trouvés dans la colonne 'cle'.\n")
    cat("Lignes avec des doublons :\n")
    print(lignes_doublons)
    cat("Valeurs de doublons uniques :\n")
    print(doublons_uniques)
  } else {
    cat("Aucun doublon n'a été trouvé dans la colonne 'cle'.\n")
  }
  
  # Tri du dataframe par ordre décroissant de la colonne 'col_sum'
  donnee <- donnee[order(-donnee$col_sum),]
  
  # Suppression des doublons en conservant uniquement la première occurrence de chaque valeur dans la colonne 'cle'
  data_unique <- donnee[!duplicated(donnee$cle),]
  
  # Retourner le dataframe modifié avec les nouvelles colonnes
  return(data_unique)
}
conv_col_doublons_2(donnee = j25)

#Fonction pour convertir les pixel en cm
conv_px_cm <- function(donnee) {
  size_conversion = 4 # car j'ai modifié la taille pour l'analyse d'image
  
  pixel_conv = 0.0040 * 0.0044 # 40 est le y (vertical) car oui ce n'est pas du 1 sur 1, 0.0044 pour la verticale
  
  donnee$area_cm2 = donnee$area * size_conversion * pixel_conv
  
  donnee$perimeter_cm2 = sqrt(donnee$perimeter * pixel_conv * size_conversion)
  
  donnee$profondeur_cm = donnee$profondeur * size_conversion * 0.0040 # attention ça ce sont les bonnes valeurs avant inversion
  
  donnee$largeur_cm = donnee$largeur * size_conversion * 0.0045 # attention ça ce sont les bonnes valeurs avant inversion
  
  # attention pour skull (mesure longueur racine conversion automatique)
  
  return(donnee)
}
j99 <- conv_px_cm(j9)




























































# Evapotranspiration -----------------------------------------------------
