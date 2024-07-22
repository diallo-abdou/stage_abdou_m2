
library(shiny)
library(ggplot2)

# Fonction d'exploration graphique des relations entre les vd et les predictors -----------------------------------
nuage_point <- function(df, var_rep, predicteur, titre_x, titre_y) {
  # Convertir les variables en numeriques
  df[[predicteur]] <- as.numeric(df[[predicteur]])
  df[[var_rep]] <- as.numeric(df[[var_rep]])
  
  df= df[,c(predicteur,var_rep)]
  df = drop_na(df)
  # correlation entre les deux variables
  correlation <- round(cor(df[[predicteur]], df[[var_rep]]), 3)
  
  #  le graphique
  g <- ggplot(df, aes(x = !!rlang::sym(predicteur), y = !!rlang::sym(var_rep))) +
    geom_point() + # Ajout des points
    geom_smooth(method = "lm", se = TRUE, color = "red") + 
    labs(
      # title = paste("Relationship between",var_rep, "and", predicteur),
      subtitle = paste0(" r = ", correlation),
      x = titre_x, 
      y = titre_y) +
    theme_classic()   
  
  return(g)
}

box_plot <- function(df, var_rep, predicteur, titre_x, titre_y) {
  g <- ggplot(df, aes(x = !!rlang::sym(predicteur), y = !!rlang::sym(var_rep))) +
    geom_boxplot() + # Ajout des boxplots
    labs(
      # title = paste("Boxplot of", var_rep, "by", predicteur),
      x = titre_x, 
      y = titre_y) +
    theme_classic()   
  
  print(g)
}



landworm_explo = read.csv2 ("App_shiny/landworm_explo.csv",sep = ",")

# all_predictors_num = all_predictors[!all_predictors %in% c(variables_factor,'Long', 'Lat')]
# dput(all_predictors_num)
# 
# # Liste des predicteurs avec l'option "Toutes les predicteurs"
# all_predictors_num_with_all <- c(all_predictors_num,"Toutes les predicteurs")
# dput(all_predictors_num_with_all)

all_predictors_num_with_all = c("Sand", "Silt", "Clay", "pH", "N", "P", "K", "CN", "CaCO3", 
                                "C_org", "tas_mean_3", "tas_mean_6", "tas_mean_120", "tas_sd_3", 
                                "tas_sd_6", "tas_sd_120", "tasmax_mean_3", "tasmax_mean_6", "tasmax_mean_120", 
                                "tasmax_sd_3", "tasmax_sd_6", "tasmax_sd_120", "tasmin_mean_3", 
                                "tasmin_mean_6", "tasmin_mean_120", "tasmin_sd_3", "tasmin_sd_6", 
                                "tasmin_sd_120", "pr_mean_3", "pr_mean_6", "pr_mean_120", "pr_sd_3", 
                                "pr_sd_6", "pr_sd_120", "pet_mean_3", "pet_mean_6", "pet_mean_120", 
                                "pet_sd_3", "pet_sd_6", "pet_sd_120", "Toutes les predicteurs"
)

# Definition de l'interface utilisateur
ui <- fluidPage(
  titlePanel("Exploration des relations"),
  
  # Sidebar layout with input and output definitions
  sidebarLayout(
    sidebarPanel(
      # Selection de la variable de reponse
      selectInput("response_var", "Variable de reponse", choices = c("AB_tot", "BM_tot", "Richesse_tot")), 
      
      # Selection de la variable explicative
      selectInput("predictor_var", "Variable explicative", choices = all_predictors_num_with_all)
    ),
    
    #  graphiques generes
    mainPanel(
      uiOutput("plots_ui")
    )
  )
)

# Definition du serveur
server <- function(input, output) {
  
  # Fonction pour generer les graphiques
  output$plots_ui <- renderUI({
    # Si l'utilisateur choisit "Toutes les predicteurs", generer un graphique pour chaque predicteur
    if (input$predictor_var == "Toutes les predicteurs") {
      plot_output_list <- lapply(all_predictors_num, function(predictor) {
        plotname <- paste("plot", predictor, sep = "_")
        plotOutput(plotname)
      })
      do.call(tagList, plot_output_list)
    } else {
      plotOutput("scatter_plot")
    }
  })
  
  observe({
    if (input$predictor_var == "Toutes les predicteurs") {
      lapply(all_predictors_num, function(predictor) {
        plotname <- paste("plot", predictor, sep = "_")
        output[[plotname]] <- renderPlot({
          #  fonction pour le nuage de points
          g <- nuage_point(df = landworm_explo,
                           var_rep = input$response_var,
                           predicteur = predictor,
                           titre_x = predictor,
                           titre_y = input$response_var)
          print(g)
        })
      })
    } else {
      output$scatter_plot <- renderPlot({
        #  fonction pour le nuage de points
        g <- nuage_point(df = landworm_explo,
                         var_rep = input$response_var,
                         predicteur = input$predictor_var,
                         titre_x = input$predictor_var,
                         titre_y = input$response_var)
        print(g)
      })
    }
  })
}

# Shiny
shinyApp(ui = ui, server = server)

