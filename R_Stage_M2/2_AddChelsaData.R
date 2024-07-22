


library(raster)
library(sp)



t1 = Sys.time()

# Fonction pour calculer les p?riodes pr?c?dentes
calculate_previous_periods <- function(date_str, num_month) {
  # Conversion de la date au format Date
  date_prelevement <- as.Date(paste0("01/", date_str), format = "%d/%m/%Y")
  
  # Calcul des p?riodes pr?c?dentes
  periods <- sapply(1:num_month, function(i) format(date_prelevement %m-% months(i), "%m_%Y"))
  
  return(periods)
}


## LOAD DATA ------

chemin_fichier_excel = "datas/landworm_bif_1.xlsx"
climate_datas <- read.xlsx(chemin_fichier_excel, sheet = "Sheet 1")

climate_datas = climate_datas[,c( "Programme" ,                                    
            "Annee"  ,                                       
           "Date_Prelevement",                              
           "ID_Site",                                       
           "gps_x" ,                                        
           "gps_y",
           "ID_2")]

colSums(is.na(climate_datas)) # O NA




climate_datas$mois_Annee = climate_datas$Date_Prelevement
incorrect_entries <- climate_datas[!grepl("^\\d{2}/\\d{4}$", climate_datas$mois_Annee), ]
incorrect_entries # 0 lignes

climate_datas$mois_Annee <- gsub("03/0015", "03/2015", climate_datas$mois_Annee)

levels(as.factor(climate_datas$Annee))


# Obtenir un vecteur des mois_Annee
yrs <- unique(climate_datas$mois_Annee)
date_not_120_files = NULL

# yrs = yrs[1]
# Initialisation des colonnes n?cessaires
periods <- c("3", "6", "12", "120")
allSubFolders <- c("pet", "pr", "tas" ,"tasmax", "tasmin")
# allSubFolders <- c("tas" ,"tasmax", "tasmin")

for (var in allSubFolders) {
  for (period in periods) {
    climate_datas[[paste0(var, "_mean_", period)]] <- NA
    climate_datas[[paste0(var, "_sd_", period)]] <- NA
  }
}


chelsea_dirs <- list(
  pr = "D:/CHELSEA",
  tas = "D:/CHELSEA",
  tasmax = "E:/CHELSEA",
  tasmin = "F:/CHELSEA",
  pet = "F:/CHELSEA"
)


###### Boucle principale ? travers les p?riodes
# chel="pet"
for (chel in allSubFolders) {
  subFolder <- chel
  print(chel)
  
  mainDir <- ifelse(chel %in% names(chelsea_dirs), chelsea_dirs[[chel]], "default_directory")

  
  period_means <- lapply(periods, function(period) which(names(climate_datas) == paste0(chel, "_mean_", period)))
  period_sds <- lapply(periods, function(period) which(names(climate_datas) == paste0(chel, "_sd_", period)))
  
  for (i in yrs) {
    print(i)
    # year <- strsplit(i, "/")[[1]][2]

    filePath <- file.path(mainDir, subFolder)
    rastlist_short <- list.files(path = filePath, pattern = '.tif$', all.files = TRUE, full.names = FALSE)
    rastlist_full <- list.files(path = filePath, pattern = '.tif$', all.files = TRUE, full.names = TRUE)
    
    # P?riode 120 mois (10 ans)
    num_month <- 120
    periods_list_120 <- calculate_previous_periods(i, num_month)
    indx_120 <- unlist(sapply(periods_list_120, function(pattern) grep(pattern, rastlist_short))) ## index of files
    names(indx_120) <- NULL
    if (length(indx_120) != 120) {
      cat("**************** Not 120 files for 120 mois \n")
      date_not_120_files = c(date_not_120_files,i)
      }
    
    
    rastlist_120 <- rastlist_full[indx_120]
    allrasters_120 <- stack(lapply(rastlist_120, raster))
    
    # Extraction et calculs
    tempDat <- climate_datas[climate_datas$mois_Annee == i,]
    pts <- SpatialPoints(cbind(tempDat$gps_x , tempDat$gps_y), proj4string = CRS(proj4string(allrasters_120)))
    
    res_120 <- extract(allrasters_120, pts) 
    
    # Conversion des unit?s
    if (chel == "pet" | chel == "pr") {
      res_120 <- res_120 / 100
    }
    if (chel %in% c("tas", "tasmin", "tasmax")) {
      res_120 <- res_120 / 10 - 273.15
    }
    
    res_3 <- res_120[, 1:3]
    res_6 <- res_120[, 1:6]
    res_12 <- res_120[, 1:12]
    
    #  si les r?sultats sont des vecteurs et les transformer en matrices si n?cessaire
    if (is.vector(res_3)) res_3 <- matrix(res_3, nrow = 1)
    if (is.vector(res_6)) res_6 <- matrix(res_6, nrow = 1)
    if (is.vector(res_12)) res_12 <- matrix(res_12, nrow = 1)
    if (is.vector(res_120)) res_120 <- matrix(res_120, nrow = 1)
    
    # Calcul des moyennes et ?carts-types
    climate_datas[climate_datas$mois_Annee == i, period_means[[1]]] <- rowMeans(res_3, na.rm = TRUE)
    climate_datas[climate_datas$mois_Annee == i, period_sds[[1]]] <- apply(res_3, 1, sd, na.rm = TRUE)
    climate_datas[climate_datas$mois_Annee == i, period_means[[2]]] <- rowMeans(res_6, na.rm = TRUE)
    climate_datas[climate_datas$mois_Annee == i, period_sds[[2]]] <- apply(res_6, 1, sd, na.rm = TRUE)
    climate_datas[climate_datas$mois_Annee == i, period_means[[3]]] <- rowMeans(res_12, na.rm = TRUE)
    climate_datas[climate_datas$mois_Annee == i, period_sds[[3]]] <- apply(res_12, 1, sd, na.rm = TRUE)
    climate_datas[climate_datas$mois_Annee == i, period_means[[4]]] <- rowMeans(res_120, na.rm = TRUE)
    climate_datas[climate_datas$mois_Annee == i, period_sds[[4]]] <- apply(res_120, 1, sd, na.rm = TRUE)
  }
  
  library(beepr)
  beep()
  cat("*************************", chel, "Heheeeeee, C'est fini!**************")
  
}

# Enregistrement des donn?es
write.xlsx(x =climate_datas,file = "datas/climate_datas.xlsx" , overwrite = TRUE)


t2 = Sys.time()

dureeee = difftime(t2,t1)


dureeee

library(beepr)
beep()








## Pour la prediction   ------
t1 = Sys.time()
Predictors_f_climat = c("pr_mean_6", "pet_mean_3", "tasmin_sd_6", "tasmin_mean_120", 
                        "tasmin_mean_6", "pr_sd_120", "tas_mean_120", "tas_sd_120",
                        "pr_mean_120", "pet_sd_120", "tasmax_sd_3", "tasmin_sd_120", 
                        "pr_sd_3", "tas_sd_6", "tasmax_sd_120")


prdtors_LC_soil = readRDS("cartographie/prdtors_LC_soil.rds")
prdtors_LC_soil_climat=prdtors_LC_soil
colSums(is.na(prdtors_LC_soil_climat)) # O NA
str(prdtors_LC_soil_climat)



prdtors_LC_soil_climat$mois_Annee = "03/2018"

incorrect_entries <- prdtors_LC_soil_climat[!grepl("^\\d{2}/\\d{4}$", prdtors_LC_soil_climat$mois_Annee), ]
incorrect_entries # 0 lignes

prdtors_LC_soil_climat$mois_Annee = as.factor(prdtors_LC_soil_climat$mois_Annee)
levels(prdtors_LC_soil_climat$mois_Annee)


# Obtenir un vecteur des mois_Annee ----
yrs <- unique(prdtors_LC_soil_climat$mois_Annee)
date_not_120_files = NULL

# yrs = yrs[1]
# Initialisation des colonnes n?cessaires
periods <- c("3", "6", "12", "120")
allSubFolders <- c("pet", "tasmin","tasmax", "tas", "pr")
# allSubFolders <- c("tas" ,"tasmax", "tasmin")


for (var in allSubFolders) {
  for (period in periods) {
    prdtors_LC_soil_climat[[paste0(var, "_mean_", period)]] <- NA
    prdtors_LC_soil_climat[[paste0(var, "_sd_", period)]] <- NA
  }
}


chelsea_dirs <- list(
  pr = "D:/CHELSEA",
  tas = "D:/CHELSEA",
  tasmax = "E:/CHELSEA",
  tasmin = "E:/CHELSEA",
  pet = "D:/CHELSEA"
)


###### Boucle principale ? travers les p?riodes
chel = "pr"
for (chel in allSubFolders) {
  subFolder <- chel
  print(chel)
  
  mainDir <- ifelse(chel %in% names(chelsea_dirs), chelsea_dirs[[chel]], "default_directory")
  
  
  period_means <- lapply(periods, function(period) which(names(prdtors_LC_soil_climat) == paste0(chel, "_mean_", period)))
  period_sds <- lapply(periods, function(period) which(names(prdtors_LC_soil_climat) == paste0(chel, "_sd_", period)))
  
  i="03/2018"
  for (i in yrs) {
    print(i)
    # year <- strsplit(i, "/")[[1]][2]
    
    filePath <- file.path(mainDir, subFolder)
    rastlist_short <- list.files(path = filePath, pattern = '.tif$', all.files = TRUE, full.names = FALSE)
    rastlist_full <- list.files(path = filePath, pattern = '.tif$', all.files = TRUE, full.names = TRUE)
    
    # P?riode 120 mois (10 ans)
    num_month <- 120
    periods_list_120 <- calculate_previous_periods(i, num_month)
    indx_120 <- unlist(sapply(periods_list_120, function(pattern) grep(pattern, rastlist_short))) ## index of files
    names(indx_120) <- NULL
    if (length(indx_120) != 120) {
      cat("**************** Not 120 files for 120 mois \n")
      date_not_120_files = c(date_not_120_files,i)
    }
    
    
    rastlist_120 <- rastlist_full[indx_120]
    allrasters_120 <- stack(lapply(rastlist_120, raster))
    
    # Extraction et calculs
    tempDat <- prdtors_LC_soil_climat[prdtors_LC_soil_climat$mois_Annee == i,]
    pts <- SpatialPoints(cbind(tempDat$gps_x , tempDat$gps_y), proj4string = CRS(proj4string(allrasters_120)))
    
    res_120 <- extract(allrasters_120, pts) 
    
    # Conversion des unit?s
    if (chel == "pet" | chel == "pr") {
      res_120 <- res_120 / 100
    }
    if (chel %in% c("tas", "tasmin", "tasmax")) {
      res_120 <- res_120 / 10 - 273.15
    }
    
    res_3 <- res_120[, 1:3]
    res_6 <- res_120[, 1:6]
    res_12 <- res_120[, 1:12]
    
    #  si les r?sultats sont des vecteurs et les transformer en matrices si n?cessaire
    if (is.vector(res_3)) res_3 <- matrix(res_3, nrow = 1)
    if (is.vector(res_6)) res_6 <- matrix(res_6, nrow = 1)
    if (is.vector(res_12)) res_12 <- matrix(res_12, nrow = 1)
    if (is.vector(res_120)) res_120 <- matrix(res_120, nrow = 1)
    
    # Calcul des moyennes et ?carts-types
    prdtors_LC_soil_climat[prdtors_LC_soil_climat$mois_Annee == i, period_means[[1]]] <- rowMeans(res_3, na.rm = TRUE)
    prdtors_LC_soil_climat[prdtors_LC_soil_climat$mois_Annee == i, period_sds[[1]]] <- apply(res_3, 1, sd, na.rm = TRUE)
    prdtors_LC_soil_climat[prdtors_LC_soil_climat$mois_Annee == i, period_means[[2]]] <- rowMeans(res_6, na.rm = TRUE)
    prdtors_LC_soil_climat[prdtors_LC_soil_climat$mois_Annee == i, period_sds[[2]]] <- apply(res_6, 1, sd, na.rm = TRUE)
    prdtors_LC_soil_climat[prdtors_LC_soil_climat$mois_Annee == i, period_means[[3]]] <- rowMeans(res_12, na.rm = TRUE)
    prdtors_LC_soil_climat[prdtors_LC_soil_climat$mois_Annee == i, period_sds[[3]]] <- apply(res_12, 1, sd, na.rm = TRUE)
    prdtors_LC_soil_climat[prdtors_LC_soil_climat$mois_Annee == i, period_means[[4]]] <- rowMeans(res_120, na.rm = TRUE)
    prdtors_LC_soil_climat[prdtors_LC_soil_climat$mois_Annee == i, period_sds[[4]]] <- apply(res_120, 1, sd, na.rm = TRUE)
  }
  
  library(beepr)
  beep()
  cat("*************************", chel, "Heheeeeee, C'est fini!**************")
  
}

# Enregistrement des donn?es
# write.xlsx(x =prdtors_LC_soil_climat,file = "datas/prdtors_LC_soil_climat.xlsx" , overwrite = TRUE)

saveRDS(prdtors_LC_soil_climat, "cartographie/prdtors_LC_soil_climat.rds")

t2 = Sys.time()

dureeee = difftime(t2,t1)


dureeee

library(beepr)
beep()













# library(raster)
# library(sp)
# library(dplyr)
# library(lubridate)
# 
# ## LOAD DATA ------
# # climate_datas <- read.csv("C:/Users/helenphi/WORK/Landworm_UrbanAnalysis/outputData/basicData_190424.csv")
# climate_datas <- landworm[523:527, c("ID", "Annee", "Date_Prelevement", "gps_x", "gps_y")] %>%
#   mutate(mois_Annee = sapply(strsplit(Date_Prelevement, "/"), function(x) paste(x[2], x[3], sep="/"))) %>%
#   droplevels()
# 
# 
# 
# 
# calculate_previous_periods <- function(date_str, num_month) {
#   # Conversion de la date au format Date
#   date_prelevement <- as.Date(paste0("01/", date_str), format = "%d/%m/%Y")
#   
#   # Calcul des p?riodes pr?c?dentes
#   periods <- sapply(1:num_month, function(i) format(date_prelevement %m-% months(i), "%m_%Y"))
#   
#   return(periods)
# }
# 
# # date_str <- "03/2010"
# # num_month <- 3
# # periods_list <- calculate_previous_periods(date_str, num_month)
# # periods_list
# 
# 
# 
# ## Get a vector of the mois_Annee ----
# yrs <- unique(climate_datas$mois_Annee)
# 
# ## Add in the needed columns
# periods <- c("3_mois", "6_mois", "12_mois", "120_mois")
# allSubFolders <- c("pet", "pr", "tas","tasmax")
# 
# for (var in allSubFolders) {
#   for (period in periods) {
#     climate_datas[[paste0(var, "_mean_", period)]] <- NA
#     climate_datas[[paste0(var, "_sd_", period)]] <- NA
#   }
# }
# 
# ###### Loop through the mois_Annee
# # mainDir <- "D:/CHELSA_MONTHLY-1980-2019" # my passport drive
# mainDir= "D:/CHELSEA"
# 
# 
# # chel = "pet"
# for (chel in allSubFolders) {
#   subFolder <- chel
#   print(chel)
# 
#   
#   period_means <- lapply(periods, function(period) which(names(climate_datas) == paste0(chel, "_mean_", period)))
#   period_sds <- lapply(periods, function(period) which(names(climate_datas) == paste0(chel, "_sd_", period)))
#   
#   # i = "03/2018"
#   for (i in yrs) {
#     print(i)
#     filePath <- file.path(mainDir, subFolder)
#     rastlist_short <- list.files(path = filePath, pattern = '.tif$', all.files = TRUE, full.names = FALSE)
#     rastlist_full <- list.files(path = filePath, pattern = '.tif$', all.files = TRUE, full.names = TRUE)
#     
#     
#     
#     # # P?riode 3 mois
#     # num_month <- 3
#     # periods_list_3 <- calculate_previous_periods(i, num_month)
#     # indx_3 <- unlist(sapply(periods_list_3, function(pattern) grep(pattern, rastlist_short))) ## index of files
#     # names(indx_3) = NULL
#     # if (length(indx_3) != 3) stop("Not 3 files for 3 mois")
#     # rastlist_3 <- rastlist_full[indx_3]
#     # allrasters_3 <- stack(lapply(rastlist_3, raster))
#     # 
#     # # P?riode 6 mois
#     # num_month <- 6
#     # periods_list_6 <- calculate_previous_periods(i, num_month)
#     # indx_6 <- unlist(sapply(periods_list_6, function(pattern) grep(pattern, rastlist_short))) ## index of files
#     # names(indx_6) = NULL
#     # if (length(indx_6) != 6) stop("Not 6 files for 6 mois")
#     # rastlist_6 <- rastlist_full[indx_6]
#     # allrasters_6 <- stack(lapply(rastlist_6, raster))
#     # 
#     # # P?riode 12 mois
#     # num_month <- 12
#     # periods_list_12 <- calculate_previous_periods(i, num_month)
#     # indx_12 <- unlist(sapply(periods_list_12, function(pattern) grep(pattern, rastlist_short))) ## index of files
#     # names(indx_12) = NULL
#     # if (length(indx_12) != 12) stop("Not 12 files for 12 mois")
#     # rastlist_12 <- rastlist_full[indx_12]
#     # allrasters_12 <- stack(lapply(rastlist_12, raster))
#     
#     # P?riode 120 mois (10 ans)
#     num_month <- 120
#     periods_list_120 <- calculate_previous_periods(i, num_month)
#     indx_120 <- unlist(sapply(periods_list_120, function(pattern) grep(pattern, rastlist_short))) ## index of files
#     names(indx_120) = NULL
#     if (length(indx_120) != 120) stop("Not 120 files for 120 mois")
#     rastlist_120 <- rastlist_full[indx_120]
#     allrasters_120 <- stack(lapply(rastlist_120, raster))
#     
#     # Extraction et calculs
#     tempDat <- climate_datas[climate_datas$mois_Annee == i,]
#     pts <- SpatialPoints(cbind(tempDat$gps_x , tempDat$gps_y), proj4string = CRS(proj4string(allrasters_3)))
#     
#     # res_3 <- extract(allrasters_3, pts)
#     # res_6 <- extract(allrasters_6, pts)
#     # res_12 <- extract(allrasters_12, pts)
#     res_120 <- extract(allrasters_120, pts) 
#     
#     # Conversion des unit?s
#     if (chel == "pet" | chel == "pr") {
#       # res_3 <- res_3 / 100
#       # res_6 <- res_6 / 100
#       # res_12 <- res_12 / 100
#       res_120 <- res_120 / 100
#     }
#     if (chel == "tas" | "tasmin" | "tasmax") {
#       # res_3 <- res_3 / 10 - 273.15
#       # res_6 <- res_6 / 10 - 273.15
#       # res_12 <- res_12 / 10 - 273.15
#      res_120 <- res_120 / 10 - 273.15
#     }
#     res_3 = res_120[,1:3]
#     res_6 = res_120[,1:6]
#     res_12 = res_120[,1:12]
#     # Calcul des moyennes et ?carts-types
#     climate_datas[climate_datas$mois_Annee == i, period_means[[1]]] <- rowMeans(res_3)
#     climate_datas[climate_datas$mois_Annee == i, period_sds[[1]]] <- apply(res_3, 1, sd)
#     climate_datas[climate_datas$mois_Annee == i, period_means[[2]]] <- rowMeans(res_6)
#     climate_datas[climate_datas$mois_Annee == i, period_sds[[2]]] <- apply(res_6, 1, sd)
#     climate_datas[climate_datas$mois_Annee == i, period_means[[3]]] <- rowMeans(res_12)
#     climate_datas[climate_datas$mois_Annee == i, period_sds[[3]]] <- apply(res_12, 1, sd)
#     climate_datas[climate_datas$mois_Annee == i, period_means[[4]]] <- rowMeans(res_120)
#     climate_datas[climate_datas$mois_Annee == i, period_sds[[4]]] <- apply(res_120, 1, sd)
#   
#   }
# }
# 
# # Enregistrement des donn?es mises ? jour
# # write.csv(climate_datas, file = "C:/Users/helenphi/WORK/Landworm_UrbanAnalysis/outputData/basicData+Chelsa_19042024.csv", row.names = FALSE)
# 
# 
# s_dat = climate_datas
# 
# s_dat$pet_mean_120_mois
# climate_datas$pet_mean_120_mois
# 
# ## End Abdou
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# library(raster)
# library(sp)
# 
# 
# ## LOAD DATA ------
# 
# climate_datas <- read.csv("C:/Users/helenphi/WORK/Landworm_UrbanAnalysis/outputData/basicData_190424.csv")
# 
# 
# ## Get a vector of the Annees ----
# 
# 
# # check all are later than 1980
# all(climate_datas$Annee > 1980)
# 
# 
# yrs <- unique(climate_datas$Annee)
# 
# 
# ## Add in the needed columns
# 
# climate_datas$pet_mean <- NA
# climate_datas$pet_sd <- NA
# 
# 
# climate_datas$PR_mean <- NA
# climate_datas$PR_sd <- NA
# 
# 
# climate_datas$TA_mean <- NA
# climate_datas$TA_sd <- NA
# 
# ###### Loop through the Annees
# 
# mainDir <- "D:/CHELSA_MONTHLY-1980-2019" # my passport drive
# allSubFolders <- c("pet", "pr", "tas")
# 
# for(chel in allSubFolders){
#   
#   subFolder <- chel
#   
#   print(chel)
#   
#   if(chel == "pet"){
#     j <- which(names(climate_datas) == "pet_mean")
#     k <- which(names(climate_datas) == "pet_sd")
#   }
# 
#   if(chel == "pr"){
#     j <- which(names(climate_datas) == "PR_mean")
#     k <- which(names(climate_datas) == "PR_sd")
#   }
#   
#   if(chel == "tas"){
#     j <- which(names(climate_datas) == "TA_mean")
#     k <- which(names(climate_datas) == "TA_sd")
#   }
#   for(i in yrs){
#     
#     print(i)
#     
#     # grab the 12 rasters for the Annee
#     
#     filePath <- file.path(mainDir, subFolder)
#     
#     #first list all files in a single folder as a list with path
#     rastlist_short <- list.files(path = filePath, pattern='.tif$', 
#                                 all.files=TRUE, full.names=FALSE)
#     
#     
#     #first list all files in a single folder as a list with path
#     rastlist_full <- list.files(path = filePath, pattern='.tif$', 
#                                 all.files=TRUE, full.names=TRUE)
#     
#     
#   # select the Annee
# 
#     if(i > 2018){
#       indx <- grep("2018", rastlist_short)
#     }else{
#     indx <- grep(i, rastlist_short)} ## index of files
#     # check length of 12
#     if(length(indx) != 12){stop("Not 12 files")}
# 
#     # subset to just the Annee
# 
#     rastlist <- rastlist_full[indx]
#     
#     #import all raster files in folder using lapply
#     allrasters <- lapply(rastlist, raster)
#     allrasters <- stack(allrasters)
#     
#     # subset data to the Annee
#     tempDat <- climate_datas[which(climate_datas$Annee == i),]
#     
#     # extract all 12 pixels for each site
#     pts <- SpatialPoints(cbind(tempDat$Long, tempDat$Lat),  # x,y
#                          proj4string = CRS(proj4string(allrasters))) 
#     
#     res <- extract(allrasters, pts)
#     
#     if(chel == "pet"){
#       res <- res/100 ## mm/month
#     }
#     
#     if(chel == "pr"){
#       res <- res/100 ## mm/month
#       # https://www.researchgate.net/post/Hello_all_I_have_monthly_data_for_rainfall_in_the_unit_kg_m-2_month-1_but_I_need_the_data_to_be_in_mm_month_Could_someone_please_help_me_out
#     }
#     
#     if(chel == "tas"){
#       res <- res/10 - 273.15 # Kelvin to celsius
#     }
#     
#     
#     
#     climate_datas[which(climate_datas$Annee == i),j] <- rowMeans(res)
#     climate_datas[which(climate_datas$Annee == i),k] <- apply(res, 1, sd)    
#   }
#   
# }
# 
# write.csv(climate_datas, 
#           file = "C:/Users/helenphi/WORK/Landworm_UrbanAnalysis/outputData/basicData+Chelsa_19042024.csv",
#           row.names = FALSE)
# 


