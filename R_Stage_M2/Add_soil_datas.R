# Extraction des predictors + moyennes 

extraction <- function(nom_col, tif_file_path, df = landworm, conv = 1) {
  #df <- df %>%filter(!is.na(gps_x) & !is.na(gps_y))
  raster_data <- raster(tif_file_path)
  
  # Cr?ation d'un dataframe pour stocker les valeurs extraites
  df_interne <- data.frame(gps_x = df$gps_x, gps_y = df$gps_y)
  proj4Str <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
  # Transformer les coordonn?es GPS en syst?me de coordonn?es du raster
  gps_coords_sp <- SpatialPoints(df_interne, proj4string = CRS(proj4Str))
  gps_coords_proj <- spTransform(gps_coords_sp, crs(raster_data))
  
  # Extraction des valeurs du raster 
  values <- raster::extract(raster_data, gps_coords_proj)
  
  # Ajout des valeurs extraites comme nouvelles colonnes a df
  #df_save = data.frame()
  #df_save[[nom_col]] <- values / conv
  
  df[[nom_col]] <- values / conv
  
  return(df)
  
  library(beepr)
  beep()
}

# la moyenne des predictores 
moyenne_val_extrct <- function(nom_col, vec_col, df=landworm) {
  df[[nom_col]] <- rowMeans(as.matrix(df[, vec_col, drop = FALSE]), na.rm = TRUE)
  df[[nom_col]] = round(df[[nom_col]],1)
  return(as.data.frame(df))
  
}



## LOAD DATA ------

chemin_fichier_excel = "datas/landworm_bif_1.xlsx"
soil_datas <- read.xlsx(chemin_fichier_excel, sheet = "Sheet 1")

soil_datas = soil_datas[,c( "Programme" ,                                    
              "Annee"  ,                                       
              "Date_Prelevement",                              
              "ID_Site",                                       
              "gps_x" ,                                        
              "gps_y",
              "ID_2")]

colSums(is.na(soil_datas)) 


soil_datas <- extraction(nom_col = "sable.0_5",df = soil_datas,conv = 1, 
                       tif_file_path = "C:/Users/diall/Downloads/datas/raster_modif/sable.0_5.tif")
soil_datas <- extraction(nom_col = "sable.5_15",df = soil_datas,conv = 1, 
                       tif_file_path = "C:/Users/diall/Downloads/datas/raster_modif/sable.5_15.tif")
soil_datas <- extraction(nom_col = "sable.15_30",df = soil_datas,conv = 1, 
                       tif_file_path = "C:/Users/diall/Downloads/datas/raster_modif/sable.15_30.tif")
soil_datas = moyenne_val_extrct(nom_col = "sable.0_30", vec_col = c("sable.0_5","sable.5_15","sable.15_30"),
                              df=soil_datas)



soil_datas <- extraction(nom_col = "limon.0_5",df = soil_datas,conv = 1, 
                       tif_file_path = "C:/Users/diall/Downloads/datas/raster_modif/limon.0_5.tif")
soil_datas <- extraction(nom_col = "limon.5_15",df = soil_datas,conv = 1, 
                       tif_file_path = "C:/Users/diall/Downloads/datas/raster_modif/limon.5_15.tif")
soil_datas <- extraction(nom_col = "limon.15_30",df = soil_datas,conv = 1, 
                       tif_file_path = "C:/Users/diall/Downloads/datas/raster_modif/limon.15_30.tif")
soil_datas = moyenne_val_extrct(nom_col = "limon.0_30", vec_col = c("limon.0_5","limon.5_15","limon.15_30"),
                              df=soil_datas)


soil_datas <- extraction(nom_col = "argile.0_5",df = soil_datas,conv = 1, 
                       tif_file_path = "C:/Users/diall/Downloads/datas/raster_modif/argile.0_5.tif")
soil_datas <- extraction(nom_col = "argile.5_15",df = soil_datas,conv = 1, 
                       tif_file_path = "C:/Users/diall/Downloads/datas/raster_modif/argile.5_15.tif")
soil_datas <- extraction(nom_col = "argile.15_30",df = soil_datas,conv = 1, 
                       tif_file_path = "C:/Users/diall/Downloads/datas/raster_modif/argile.15_30.tif")
soil_datas = moyenne_val_extrct(nom_col = "argile.0_30", vec_col = c("argile.0_5","argile.5_15","argile.15_30"),
                              df=soil_datas)


soil_datas <- extraction(nom_col = 'jrc_pH_H2O', df = soil_datas, conv = 1, 
                  tif_file_path = 'C:/Users/diall/Downloads/datas/raster_modif/pH_H2O.tif')


# soil_datas <- extraction(nom_col = 'jrc_pH_CaCl', df = soil_datas, conv = 1, 
#                   tif_file_path = 'C:/Users/diall/Downloads/datas/raster_modif/pH_CaCl.tif')


soil_datas <- extraction(nom_col = 'P', df = soil_datas, conv = 1, 
                  tif_file_path = 'C:/Users/diall/Downloads/datas/raster_modif/P.tif')


soil_datas <- extraction(nom_col = 'N', df = soil_datas, conv = 1, 
                  tif_file_path = 'C:/Users/diall/Downloads/datas/raster_modif/N.tif')


soil_datas <- extraction(nom_col = 'K', df = soil_datas, conv = 1, 
                  tif_file_path = 'C:/Users/diall/Downloads/datas/raster_modif/K.tif')


soil_datas <- extraction(nom_col = 'CN', df = soil_datas, conv = 1, 
                  tif_file_path = 'C:/Users/diall/Downloads/datas/raster_modif/CN.tif')


# soil_datas <- extraction(nom_col = 'CEC', df = soil_datas, conv = 1, 
#                   tif_file_path = 'C:/Users/diall/Downloads/datas/raster_modif/CEC.tif')


soil_datas <- extraction(nom_col = 'CaCO3', df = soil_datas, conv = 1, 
                  tif_file_path = 'C:/Users/diall/Downloads/datas/raster_modif/CaCO3.tif')


soil_datas <- extraction(nom_col = "c_orga_0",df = soil_datas,conv = 5, 
                       tif_file_path = "C:/Users/diall/Downloads/datas/raster_modif/sol_organic.carbon_usda.6a1c_m_250m_b0..0cm_1950..2017_v0.2.tif")
soil_datas <- extraction(nom_col = "c_orga_10",df = soil_datas,conv = 5, 
                       tif_file_path = "C:/Users/diall/Downloads/datas/raster_modif/sol_organic.carbon_usda.6a1c_m_250m_b10..10cm_1950..2017_v0.2.tif")
soil_datas <- extraction(nom_col = "c_orga_30",df = soil_datas,conv = 5, 
                       tif_file_path = "C:/Users/diall/Downloads/datas/raster_modif/sol_organic.carbon_usda.6a1c_m_250m_b30..30cm_1950..2017_v0.2.tif")
soil_datas = moyenne_val_extrct(nom_col = "c_orga_0_a_30", vec_col = c("c_orga_0","c_orga_10","c_orga_30"),df=soil_datas)



columns_to_remove <- c("sable.0_5", "sable.5_15", "sable.15_30",
                       "limon.0_5","limon.5_15","limon.15_30",
                       "argile.0_5","argile.5_15","argile.15_30",
                       "c_orga_0","c_orga_10","c_orga_30")
soil_datas <- soil_datas %>%
  select(-all_of(columns_to_remove))


colSums(is.na(soil_datas)) 
 
write.xlsx(x =soil_datas,file = "datas/soil_datas.xlsx", overwrite = TRUE)


## END ad soil datas








## LUCAS 2018
# 
# library(dplyr)
# library(sp)
# library(raster)
# 
# df_source <- read.csv("C:/Users/diall/Downloads/datas/LUCAS-SOIL-2018-v2/LUCAS-SOIL-2018.csv")
# str(df_source)
# df_source = df_source[,c(1:10, 15,19:22)]
# df_source = df_source [df_source$NUTS_0 =="FR",]
# 
# 
# 
# 
# 
# 
# col = names(df_source[,c(3,)])
# df_source[, col] <- as.numeric(df_source[, col])
# 
# # CARTE
# 
# df_source <- df_source %>%
#   dplyr::rename(
#     gps_x = TH_LONG,
#     gps_y = TH_LAT,
#   )
# 
# df_coord <- df_source[, c("gps_x", "gps_y")] %>% mutate(gps_x = as.numeric(gps_x),gps_y = as.numeric(gps_y))
# 
# df_coord$num_ligne <- seq(nrow(df_coord))
# carte <- leaflet(df_coord) %>%
#   addTiles() %>%
#   addCircleMarkers(lng = ~gps_x, lat = ~gps_y, radius = 0.8, fillOpacity = 0.8, fillColor = "blue")
# carte
# 
# 
# 
# tif_file_path = 'C:/Users/diall/Downloads/datas/raster_origine/N.tif'
# N = raster(tif_file_path)
# plot(N)
# 
# 
# # Fonction pour trouver les valeurs les plus proches dans un CSV et les ajouter au dataframe cible
# extraction_csv <- function(nom_col, df_source, df, conv = 1, coords = c("gps_x", "gps_y")) {
#   
#   # V?rifier si les colonnes de coordonn?es existent dans les deux dataframes
#   if (!all(coords %in% names(df_source))) {
#     stop("Les colonnes de coordonn?es GPS sp?cifi?es n'existent pas dans le fichier source.")
#   }
#   if (!all(coords %in% names(df))) {
#     stop("Les colonnes de coordonn?es GPS sp?cifi?es n'existent pas dans le dataframe cible.")
#   }
#   
#   # Convertir les coordonn?es GPS en objets spatiaux
#   proj4Str <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
#   gps_coords_sp_source <- SpatialPoints(df_source[, coords], proj4string = CRS(proj4Str))
#   gps_coords_sp_target <- SpatialPoints(df[, coords], proj4string = CRS(proj4Str))
#   
#   # Trouver les indices des points les plus proches
#   indices <- sp::over(gps_coords_sp_target, gps_coords_sp_source, returnList = TRUE)
#   
#   # Extraire les valeurs correspondantes
#   values <- sapply(indices, function(i) {
#     if (is.null(i)) {
#       return(NA)
#     } else {
#       return(mean(df_source[i, nom_col], na.rm = TRUE))
#     }
#     
#     
#   })
#   
#   #  les valeurs extraites comme nouvelle colonne dans le dataframe cible
#   df[[nom_col]] <- values / conv
#   
#   return(df)
# }




# For prediction ----------------------------------------------------------

# landworm_explo <- landworm_explo %>%
#   dplyr::rename(
#     Sand = sable.0_30,
#     Silt = limon.0_30,
#     Clay = argile.0_30,
#     pH = jrc_pH_H2O,
#     C_org = c_orga_0_a_30,
#     LC_ = clcm_lvl3,
#     Long = gps_x ,
#     Lat =  gps_y
#   )


predictors <- readRDS("cartographie/predictors.rds")
summary(predictors)
str(predictors)
prdtors_LC_soil = predictors[,c("gps_x", "gps_y","clcm_lvl3")]

Predictors_f_sol = c("CN", "K", "Clay", "CaCO3", "pH", "Sand", "Silt", "P")


prdtors_LC_soil <- extraction(nom_col = 'CN', df = prdtors_LC_soil, conv = 1, 
                         tif_file_path = 'C:/Users/diall/Downloads/datas/raster_modif/CN.tif')

prdtors_LC_soil <- extraction(nom_col = 'K', df = prdtors_LC_soil, conv = 1, 
                         tif_file_path = 'C:/Users/diall/Downloads/datas/raster_modif/K.tif')


prdtors_LC_soil <- extraction(nom_col = "argile.0_5",df = prdtors_LC_soil,conv = 1, 
                         tif_file_path = "C:/Users/diall/Downloads/datas/raster_modif/argile.0_5.tif")
prdtors_LC_soil <- extraction(nom_col = "argile.5_15",df = prdtors_LC_soil,conv = 1, 
                         tif_file_path = "C:/Users/diall/Downloads/datas/raster_modif/argile.5_15.tif")
prdtors_LC_soil <- extraction(nom_col = "argile.15_30",df = prdtors_LC_soil,conv = 1, 
                         tif_file_path = "C:/Users/diall/Downloads/datas/raster_modif/argile.15_30.tif")
prdtors_LC_soil = moyenne_val_extrct(nom_col = "argile.0_30", vec_col = c("argile.0_5","argile.5_15","argile.15_30"),
                                df=prdtors_LC_soil)

prdtors_LC_soil <- extraction(nom_col = 'CaCO3', df = prdtors_LC_soil, conv = 1, 
                         tif_file_path = 'C:/Users/diall/Downloads/datas/raster_modif/CaCO3.tif')


prdtors_LC_soil <- extraction(nom_col = 'jrc_pH_H2O', df = prdtors_LC_soil, conv = 1, 
                         tif_file_path = 'C:/Users/diall/Downloads/datas/raster_modif/pH_H2O.tif')

prdtors_LC_soil <- extraction(nom_col = "sable.0_5",df = prdtors_LC_soil,conv = 1, 
                         tif_file_path = "C:/Users/diall/Downloads/datas/raster_modif/sable.0_5.tif")
prdtors_LC_soil <- extraction(nom_col = "sable.5_15",df = prdtors_LC_soil,conv = 1, 
                         tif_file_path = "C:/Users/diall/Downloads/datas/raster_modif/sable.5_15.tif")
prdtors_LC_soil <- extraction(nom_col = "sable.15_30",df = prdtors_LC_soil,conv = 1, 
                         tif_file_path = "C:/Users/diall/Downloads/datas/raster_modif/sable.15_30.tif")
prdtors_LC_soil = moyenne_val_extrct(nom_col = "sable.0_30", vec_col = c("sable.0_5","sable.5_15","sable.15_30"),
                                df=prdtors_LC_soil)


prdtors_LC_soil <- extraction(nom_col = "limon.0_5",df = prdtors_LC_soil,conv = 1, 
                         tif_file_path = "C:/Users/diall/Downloads/datas/raster_modif/limon.0_5.tif")
prdtors_LC_soil <- extraction(nom_col = "limon.5_15",df = prdtors_LC_soil,conv = 1, 
                         tif_file_path = "C:/Users/diall/Downloads/datas/raster_modif/limon.5_15.tif")
prdtors_LC_soil <- extraction(nom_col = "limon.15_30",df = prdtors_LC_soil,conv = 1, 
                         tif_file_path = "C:/Users/diall/Downloads/datas/raster_modif/limon.15_30.tif")
prdtors_LC_soil = moyenne_val_extrct(nom_col = "limon.0_30", vec_col = c("limon.0_5","limon.5_15","limon.15_30"),
                                df=prdtors_LC_soil)



prdtors_LC_soil <- extraction(nom_col = 'P', df = prdtors_LC_soil, conv = 1, 
                         tif_file_path = 'C:/Users/diall/Downloads/datas/raster_modif/P.tif')





columns_to_remove <- c("sable.0_5", "sable.5_15", "sable.15_30",
                       "limon.0_5","limon.5_15","limon.15_30",
                       "argile.0_5","argile.5_15","argile.15_30"
                      )
prdtors_LC_soil <- prdtors_LC_soil %>%
  select(-all_of(columns_to_remove))


colSums(is.na(prdtors_LC_soil)) 

# write.xlsx(x =prdtors_LC_soil,file = "cartographie/prdtors_LC_soil.xlsx", overwrite = TRUE)

saveRDS(prdtors_LC_soil, "cartographie/prdtors_LC_soil.rds")


## END ad soil datas

