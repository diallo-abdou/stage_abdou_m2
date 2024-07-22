



Predictors_i = c("bio12","bio3","CaCO3","clay","clcm_lvl3","gps_x","gps_y","N","P","silt")
response_vars= c('abundance', 'BM_tot', 'Richesse_tot')
df_all = landworm_explo_non_t[,c(response_vars,Predictors_i)]
colnames(df_all)
Predictors_i = c("Prec","Isot","CaCO3","Clay","CLC","Long","Lat","N","P","Silt")
response_vars= c('abundance', 'biomass', 'richess')
colnames(df_all) = c(response_vars,Predictors_i)
dummy_vars <- model.matrix(~ CLC - 1, data = df_all)
names_CLC = colnames(dummy_vars)
df_all <- cbind(df_all, dummy_vars)
df_all$CLC = NULL




abundance_df = df_all
abundance_df$biomass = NULL
abundance_df$richess =NULL
abundance_df = drop_na(abundance_df)
# abundance_df$abundance = sqrt(abundance_df$abundance)
# abundance_df[, c("Prec","Isot","CaCO3","Clay","Long","Lat","N","P","Silt")] = 
#   scale(abundance_df[, c("Prec","Isot","CaCO3","Clay","Long","Lat","N","P","Silt")])

biomass_df = df_all
biomass_df$abundance = NULL
biomass_df$richess = NULL
biomass_df = drop_na(biomass_df)
# biomass_df$biomass = sqrt(biomass_df$biomass)

richness_df = df_all
richness_df$abundance = NULL
richness_df$biomass = NULL
richness_df = drop_na(richness_df)


seuil_nbr_vars = 7 
loss = "rmse"


# abundance ---------------------------------------------------
## glm -------------------------------
# model, params,

df = abundance_df
var_rep = "abundance" 
algo = "glm"



X <- df[, which(names(df) != var_rep)]
y <- df[, var_rep]


  model = glm(abundance ~ ., data = df)
  
  # Utilisation du conteneur iml Predictor()
  predictor <- Predictor$new(model, data = X, y = y) 
  imp <- FeatureImp$new(predictor, loss = loss) 
  var_importance <- as.data.frame(imp$results)
  
  df_imps_sans_CLC = var_importance[!var_importance$feature %in% names_CLC,]
  df_imp_CLC = var_importance[var_importance$feature %in% names_CLC,]
  df_imp_CLC= data.frame(feature="CLC",
                           importance.05 = sum(df_imp_CLC$importance.05), 
                           importance = sum(df_imp_CLC$importance),
                           importance.95 = sum(df_imp_CLC$importance.95),
                           permutation.error = sum(df_imp_CLC$permutation.error))
  var_importance = rbind(df_imp_CLC,df_imps_sans_CLC)
  rownames(var_importance) = NULL
  var_importance = var_importance %>% arrange(desc(importance))

df_info = data.frame(
  rep.var = rep (var_rep, seuil_nbr_vars),
  model = rep(algo,seuil_nbr_vars),
  rank.vars = 1:seuil_nbr_vars)

variable_name <- paste0("abundance_top_", seuil_nbr_vars, "_vars_glm")
selected_vars <- var_importance[1:seuil_nbr_vars, ]
selected_vars = cbind(df_info, selected_vars)
assign(variable_name, selected_vars)

write.xlsx(x =selected_vars,
           file = paste0("variables_selections/",variable_name,".xlsx"),
           rowNames = FALSE,overwrite = TRUE)

cat("***** Les variables séléctionnées par odre d'importance sont: \n",
    selected_vars$feature,sep = "; ")
cat("***** Les variables non séléctionnées par odre de non importance sont: \n",
    var_importance[(seuil_nbr_vars+1):nrow(var_importance),"feature" ] ,sep = "; ")

rm("predictor","imp", "var_importance","df_info","variable_name","selected_vars")

## end variables selection for abundance with glm ##





## gam -------------------------------
# model, params,

df = abundance_df
var_rep = "abundance" 
algo = "gam"




X <- df[, which(names(df) != var_rep)]
y <- df[, var_rep]
  
  
  model = gam(abundance ~ s(Prec)+
                s(Isot)+s(CaCO3)+s(Clay)+
                s(Long)+ s(Lat)+
                s(N)+s(P)+ s(Silt)+
              CLCf+ CLCgua+   CLCng+   CLCnial+ CLCp+CLCv, 
              data = df)
  
  # Utilisation du conteneur iml Predictor()
  predictor <- Predictor$new(model, data = X, y = y) 
  imp <- FeatureImp$new(predictor, loss = loss) 
  var_importance <- as.data.frame(imp$results)
  
  df_imps_sans_CLC = var_importance[!var_importance$feature %in% names_CLC,]
  df_imp_CLC = var_importance[var_importance$feature %in% names_CLC,]
  df_imp_CLC= data.frame(feature="CLC",
                         importance.05 = sum(df_imp_CLC$importance.05), 
                         importance = sum(df_imp_CLC$importance),
                         importance.95 = sum(df_imp_CLC$importance.95),
                         permutation.error = sum(df_imp_CLC$permutation.error))
  var_importance = rbind(df_imp_CLC,df_imps_sans_CLC)
  rownames(var_importance) = NULL
  var_importance = var_importance %>% arrange(desc(importance))
  
  df_info = data.frame(
    rep.var = rep (var_rep, seuil_nbr_vars),
    model = rep(algo,seuil_nbr_vars),
    rank.vars = 1:seuil_nbr_vars)
  
  variable_name <- paste0("abundance_top_", seuil_nbr_vars, "_vars_gam")
  selected_vars <- var_importance[1:seuil_nbr_vars, ]
  selected_vars = cbind(df_info, selected_vars)
  assign(variable_name, selected_vars)
  
  write.xlsx(x =selected_vars,
             file = paste0("variables_selections/",variable_name,".xlsx"),
             rowNames = FALSE,overwrite = TRUE)
  
  cat("***** Les variables séléctionnées par odre d'importance sont: \n",
      selected_vars$feature,sep = "; ")
  cat("***** Les variables non séléctionnées par odre de non importance sont: \n",
      var_importance[(seuil_nbr_vars+1):nrow(var_importance),"feature" ] ,sep = "; ")

  rm("predictor","imp", "var_importance","df_info","variable_name","selected_vars")
## end variables selection for abundance with gam ##





## rf -------------------------------
# model, params,

df = abundance_df
var_rep = "abundance" 
algo = "rf"



X <- df[, which(names(df) != var_rep)]
y <- df[, var_rep]

  
  model = randomForest(abundance ~ ., data = df)
  
  # Utilisation du conteneur iml Predictor()
  predictor <- Predictor$new(model, data = X, y = y) 
  imp <- FeatureImp$new(predictor, loss = loss) 
  var_importance <- as.data.frame(imp$results)
  
  df_imps_sans_CLC = var_importance[!var_importance$feature %in% names_CLC,]
  df_imp_CLC = var_importance[var_importance$feature %in% names_CLC,]
  df_imp_CLC= data.frame(feature="CLC",
                         importance.05 = sum(df_imp_CLC$importance.05), 
                         importance = sum(df_imp_CLC$importance),
                         importance.95 = sum(df_imp_CLC$importance.95),
                         permutation.error = sum(df_imp_CLC$permutation.error))
  var_importance = rbind(df_imp_CLC,df_imps_sans_CLC)
  rownames(var_importance) = NULL
  var_importance = var_importance %>% arrange(desc(importance))
  
  df_info = data.frame(
    rep.var = rep (var_rep, seuil_nbr_vars),
    model = rep(algo,seuil_nbr_vars),
    rank.vars = 1:seuil_nbr_vars)
  
  variable_name <- paste0("abundance_top_", seuil_nbr_vars, "_vars_rf")
  selected_vars <- var_importance[1:seuil_nbr_vars, ]
  selected_vars = cbind(df_info, selected_vars)
  assign(variable_name, selected_vars)
  
  write.xlsx(x =selected_vars,
             file = paste0("variables_selections/",variable_name,".xlsx"),
             rowNames = FALSE,overwrite = TRUE)
  
  cat("***** Les variables séléctionnées par odre d'importance sont: \n",
      selected_vars$feature,sep = "; ")
  cat("***** Les variables non séléctionnées par odre de non importance sont: \n",
      var_importance[(seuil_nbr_vars+1):nrow(var_importance),"feature" ] ,sep = "; ")
  
  rm("predictor","imp", "var_importance","df_info","variable_name","selected_vars")
  

## end variables selection for abundance with rf ##
  
  

## gbm -------------------------------
# model, params,

df = abundance_df
var_rep = "abundance" 
algo = "gbm"


X <- df[, which(names(df) != var_rep)]
y <- df[, var_rep]


  
  model = gbm(abundance ~ ., data = df)

  
  # Utilisation du conteneur iml Predictor()
  predictor <- Predictor$new(model, data = X, y = y) 
  imp <- FeatureImp$new(predictor, loss = loss) 
  var_importance <- as.data.frame(imp$results)
  
  df_imps_sans_CLC = var_importance[!var_importance$feature %in% names_CLC,]
  df_imp_CLC = var_importance[var_importance$feature %in% names_CLC,]
  df_imp_CLC= data.frame(feature="CLC",
                         importance.05 = sum(df_imp_CLC$importance.05), 
                         importance = sum(df_imp_CLC$importance),
                         importance.95 = sum(df_imp_CLC$importance.95),
                         permutation.error = sum(df_imp_CLC$permutation.error))
  var_importance = rbind(df_imp_CLC,df_imps_sans_CLC)
  rownames(var_importance) = NULL
  var_importance = var_importance %>% arrange(desc(importance))
  
  df_info = data.frame(
    rep.var = rep (var_rep, seuil_nbr_vars),
    model = rep(algo,seuil_nbr_vars),
    rank.vars = 1:seuil_nbr_vars)
  
  variable_name <- paste0("abundance_top_", seuil_nbr_vars, "_vars_gbm")
  selected_vars <- var_importance[1:seuil_nbr_vars, ]
  selected_vars = cbind(df_info, selected_vars)
  assign(variable_name, selected_vars)
  
  write.xlsx(x =selected_vars,
             file = paste0("variables_selections/",variable_name,".xlsx"),
             rowNames = FALSE,overwrite = TRUE)
  
  cat("***** Les variables séléctionnées par odre d'importance sont: \n",
      selected_vars$feature,sep = "; ")
  cat("***** Les variables non séléctionnées par odre de non importance sont: \n",
      var_importance[(seuil_nbr_vars+1):nrow(var_importance),"feature" ] ,sep = "; ")
  
  rm("predictor","imp", "var_importance","df_info","variable_name","selected_vars")
## end variables selection for abundance with gbm ##





  
  
  
## ann -------------------------------
  # model, params,
  
  df = abundance_df
  var_rep = "abundance" 
  algo = "ann"

  
  
  X <- df[, which(names(df) != var_rep)]
  X %<>% mutate_if(is.factor, as.numeric)
  X <- as.matrix(X)
  # dimnames(X) <- NULL
  
  y <- df[, var_rep]
  
  
  model <- keras_model_sequential()
  model %>% 
    layer_dense(units = 8,activation = 'relu',input_shape = c(ncol(df)-1)) %>%
    layer_dense(units = 1)
  
  model %>% keras::compile(loss = 'mse',
                    optimizer = 'rmsprop',
                    metrics = 'mae')
  
  # my_model <- model %>%
  #   fit(X,
  #       y,
  #       epochs = 100,
  #       #batch_size = 1,
  #       validation_split = 0.2)
  
  
  # Utilisation du conteneur iml Predictor()
  predictor <- Predictor$new(model, data = as.data.frame(X), y = y) 
  imp <- FeatureImp$new(predictor, loss = loss) 
  var_importance <- as.data.frame(imp$results)
  
  df_imps_sans_CLC = var_importance[!var_importance$feature %in% names_CLC,]
  df_imp_CLC = var_importance[var_importance$feature %in% names_CLC,]
  df_imp_CLC= data.frame(feature="CLC",
                         importance.05 = sum(df_imp_CLC$importance.05), 
                         importance = sum(df_imp_CLC$importance),
                         importance.95 = sum(df_imp_CLC$importance.95),
                         permutation.error = sum(df_imp_CLC$permutation.error))
  var_importance = rbind(df_imp_CLC,df_imps_sans_CLC)
  rownames(var_importance) = NULL
  var_importance = var_importance %>% arrange(desc(importance))
  
  df_info = data.frame(
    rep.var = rep (var_rep, seuil_nbr_vars),
    model = rep(algo,seuil_nbr_vars),
    rank.vars = 1:seuil_nbr_vars)
  
  variable_name <- paste0("abundance_top_", seuil_nbr_vars, "_vars_ann")
  selected_vars <- var_importance[1:seuil_nbr_vars, ]
  selected_vars = cbind(df_info, selected_vars)
  assign(variable_name, selected_vars)
  
  write.xlsx(x =selected_vars,
             file = paste0("variables_selections/",variable_name,".xlsx"),
             rowNames = FALSE,overwrite = TRUE)
  
  cat("***** Les variables séléctionnées par odre d'importance sont: \n",
      selected_vars$feature,sep = "; ")
  cat("***** Les variables non séléctionnées par odre de non importance sont: \n",
      var_importance[(seuil_nbr_vars+1):nrow(var_importance),"feature" ] ,sep = "; ")
  
  rm("predictor","imp", "var_importance","df_info","variable_name","selected_vars")
  
  ## end variables selection for abundance with ann ##
  
  

# Identification ----------------------------------------------------------

  abundance_top_7_vars_ann
  
  all_algo =c("glm","gam","rf","gbm","ann")
  for ( i in all_algo){
    cat (paste0("abundance_top_", seuil_nbr_vars, "_vars_",i),"\n")
  }

  abundance_top_vars = rbind(  abundance_top_7_vars_glm,
                                 abundance_top_7_vars_gam,
                                 abundance_top_7_vars_rf, 
                                 abundance_top_7_vars_gbm, 
                                 abundance_top_7_vars_ann )
    
  str(abundance_top_vars)
  abundance_top_vars$rep.var = as.factor(abundance_top_vars$rep.var)
  abundance_top_vars$model = as.factor(abundance_top_vars$model)
  abundance_top_vars$feature = as.factor(abundance_top_vars$feature)
  abundance_top_vars =   data.frame(nbr = summary(abundance_top_vars$feature))
  abundance_top_vars$feature = rownames(abundance_top_vars)
  abundance_top_vars = abundance_top_vars %>% arrange(desc(nbr))
  abundance_top_vars$re.var= rep("abundance")
    
  write.xlsx(x =abundance_top_vars,
             file = paste0("variables_selections/abundance_top_vars.xlsx"),
             rowNames = FALSE,overwrite = TRUE)
    
  
  
  
  
  
  
  
  
  
  