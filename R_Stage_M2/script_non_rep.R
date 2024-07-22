


bdd_all = read.csv2("datas/Non_repeated_data/bdd_all.csv")
bdd_all <- bdd_all %>%
  select(where(~!all(is.na(.))))

bdd_all$ID_2 = paste(bdd_all$Programme,bdd_all$ID_Site,bdd_all$Annee,bdd_all$Modalite,sep = "_")
id_1=c("Programme","ID_Site","Annee","Modalite","Bloc")
names(bdd_all)
id_2 = c("Programme","ID_Site","Annee","Modalite","Bloc","Code_Parcelle","AB_tot", "BM_tot", "Richesse_tot")
id_3 = c("Programme","ID_Site","Annee","Modalite","Bloc","Code_Parcelle","AB_tot", "BM_tot", "Richesse_tot","gps_x","gps_y","clcm_lvl2")
id_4 = c("Programme","ID_Site","Annee","Modalite","Bloc","gps_x","gps_y","clcm_lvl2")

bdd_all = conv_col(bdd_all, names(bdd_all[,c(1:3,5:7)]), "factor")
bdd_all = conv_col(bdd_all, c("AB_tot", "BM_tot", "Richesse_tot"), "numeric")
dim(bdd_all) #  3311  337


# Identification des doublons ID
dupld_ID <- subset(bdd_all, duplicated(bdd_all[, c("ID")]))
dupld_ID$ID
bdd_all[which(bdd_all$ID==dupld_ID$ID),id_1]
bdd_all[which(bdd_all$ID==dupld_ID$ID),id_2]
bdd_all[which(bdd_all$ID==dupld_ID$ID),id_3]
diff_lignes <- bdd_all[ which(bdd_all$ID==dupld_ID$ID)[1], ] == bdd_all[which(bdd_all$ID==dupld_ID$ID)[2] , ]
sum(diff_lignes,na.rm=TRUE)
ncol(bdd_all)
bdd_all = bdd_all [- which(bdd_all$ID==dupld_ID$ID)[2],] # les deux lignes sont les mêmes, suppression
dim(bdd_all) # 3310  337




# bdd_double: Doublons des ID_Site dans le meme Programme
bdd_double <- duplicated(bdd_all[c("Programme", "ID_Site")]) | 
                     duplicated(bdd_all[c("Programme", "ID_Site")], fromLast = TRUE)
bdd_double <- bdd_all[bdd_double, ]
bdd_double = droplevels(bdd_double)
dim(bdd_double) # 1485 337
unique(bdd_double$Programme) # 19 programmes
bdd_double[bdd_double$Programme=="ZAA",id_4]
bdd_all$Programme = as.factor(bdd_all$Programme)
levels(bdd_all$Programme)






# bdd_clean: les lignes de bdd_all où l'ID n'est pas présent dans bdd_double
bdd_all$ID <- as.character(bdd_all$ID)
bdd_double$ID <- as.character(bdd_double$ID)
bdd_clean <- bdd_all[!(bdd_all$ID %in% bdd_double$ID), ]
dim(bdd_clean) # 1825  348
doub_id_site = subset(bdd_clean, duplicated(bdd_clean[, c("ID_Site")]))
nrow(doub_id_site)
bdd_clean = bdd_clean [- which(bdd_clean$ID %in% doub_id_site$ID),]
subset(bdd_clean, duplicated(bdd_clean[, c("Programme", "ID_Site")]))
double_gps = subset(bdd_clean, duplicated(bdd_clean[, c("gps_x", "gps_y")]))
nrow(double_gps)
bdd_clean = bdd_clean [- which(bdd_clean$ID %in% double_gps$ID),]
dim(bdd_clean) # 1683  337



# 1: Pour AF : garde -------------------------------------------------------------
# Regle de selection: meme ID_Site, Modalite different et pas de bloc: on garde les parcelles
AF = bdd_double[bdd_double$Programme=="AF",]
AF = droplevels(AF)
dim(AF) # 25
AF[,id_1]
AF [,id_2]
AF[,id_4] # gps repetee par ID_Site, OS different
unique(AF$ID_Site) # 9
AF$Annee <- as.numeric(as.character(AF$Annee))
AF[AF$Modalite %in% c("FTE_IR","FTE_R"),id_4]
AF = AF[!AF$Modalite %in% c("FTE_IR","FTE_R"),] # VZ 2011 ATE, 2013 FTE
AF_select = data.frame(Programme = rep("AF"), ID =AF$ID )
write.csv2(x =AF_select,file = "datas/Non_repeated_data/select_id/AF_select.csv", row.names = FALSE)

# VALIDE ***
sum (names(bdd_double)==names(AF))
bdd_double_clean = AF


# 8: Pour Kerguéhennec  : garde -------------------------------------------------------------
# Regle de selection: meme ID_Site, Modalite different, bloc ????: on garde les parcelles
Kerguéhennec = bdd_double[bdd_double$Programme=="Kerguéhennec",]
nrow(Kerguéhennec) # 30
Kerguéhennec [,id_1] # meme annee, est ce 3 bloc?
Kerguéhennec [,id_2]
Kerguéhennec [,id_4] # GPS ???, meme OS
bdd_all[bdd_all$Programme=="Kerguéhennec",id_3]
Kerguéhennec_select = data.frame(Programme = rep("Kerguéhennec"), ID =Kerguéhennec$ID )
write.csv2(x =Kerguéhennec_select,file = "datas/Non_repeated_data/select_id/Kerguéhennec_select.csv", row.names = FALSE)

# mean( A101, A201, A301): DC parcelles differente ? est ce meme chose que EFFEL
# KH  = pas touché
# voir mail 05/04/2024
bdd_double_clean = rbind(bdd_double_clean,Kerguéhennec)


# 3: Pour Bio2_ANDRA : garde -------------------------------------------------------------
# Regle de selection: meme ID_Site, Modalite different et pas de bloc: on garde les parcelles
Bio2_ANDRA = bdd_double[bdd_double$Programme=="Bio2_ANDRA",]
nrow(Bio2_ANDRA) # 3
Bio2_ANDRA [,id_1]
Bio2_ANDRA [,id_2]
Bio2_ANDRA [,id_3]# gps répété
bdd_all[bdd_all$Programme=="Bio2_ANDRA",id_3]
Bio2_ANDRA_select = data.frame(Programme = rep("Bio2_ANDRA"), ID =Bio2_ANDRA$ID )
write.csv2(x =Bio2_ANDRA_select,file = "datas/Non_repeated_data/select_id/Bio2_ANDRA_select.csv", row.names = FALSE)
# VALIDE ***
bdd_double_clean = rbind(bdd_double_clean,Bio2_ANDRA)


# 4: Pour BIO2 : garde -------------------------------------------------------------
# Regle de selection: meme ID_Site, Modalite different et pas de bloc: on garde les parcelles
BIO2 = bdd_double[bdd_double$Programme=="BIO2",]
nrow(BIO2) #19
BIO2 [,id_1]
BIO2 [,id_2]
BIO2 [,id_3]
bdd_all[bdd_all$Programme=="BIO2",id_3]
BIO2_select = data.frame(Programme = rep("BIO2"), ID =BIO2$ID )
write.csv2(x =BIO2_select,file = "datas/Non_repeated_data/select_id/BIO2_select.csv", row.names = FALSE)
# VALIDE ***
bdd_double_clean = rbind(bdd_double_clean,BIO2)

# 5: Pour CN : garde -------------------------------------------------------------
# Regle de selection: meme ID_Site, Modalite different et pas de bloc: on garde les parcelles
CN = bdd_double[bdd_double$Programme=="CN",]
nrow(CN) # 2
CN [,id_1]
CN [,id_2]
CN [,id_3] # gps répété
bdd_all[bdd_all$Programme=="CN",id_3]
CN_select = data.frame(Programme = rep("CN"), ID =CN$ID )
write.csv2(x =CN_select,file = "datas/Non_repeated_data/select_id/CN_select.csv", row.names = FALSE)
# VALIDE ***
bdd_double_clean = rbind(bdd_double_clean,CN)


# 12: Pour OBL  : garde -------------------------------------------------------------
# Regle de selection: meme ID_Site, Modalite different et pas de bloc: on garde les parcelles
OBL = bdd_double[bdd_double$Programme=="OBL",]
nrow(OBL) # 9
OBL [,id_1] # pas de blocs
OBL [,id_2] # meme annee
OBL [,id_3] # gps different, idem pour les OS
bdd_all[bdd_all$Programme=="OBL",id_3]
OBL_select = data.frame(Programme = rep("OBL"), ID =OBL$ID )
write.csv2(x =OBL_select,file = "datas/Non_repeated_data/select_id/OBL_select.csv", row.names = FALSE)

# VALIDE ***
bdd_double_clean = rbind(bdd_double_clean,OBL)

# 13: Pour OPVT_BZH  : garde -------------------------------------------------------------
# Regle de selection: meme ID_Site, Modalite different et pas de bloc: on garde les parcelles
OPVT_BZH = bdd_double[bdd_double$Programme=="OPVT_BZH",]
nrow(OPVT_BZH) # 2
OPVT_BZH [,id_1] # pas de blocs
OPVT_BZH [,id_2] 
OPVT_BZH [,id_4] # gps different
bdd_all[bdd_all$Programme=="OPVT_BZH",id_3]
OPVT_BZH_select = data.frame(Programme = rep("OPVT_BZH"), ID =OPVT_BZH$ID )
write.csv2(x =OPVT_BZH_select,file = "datas/Non_repeated_data/select_id/OPVT_BZH_select.csv", row.names = FALSE)
# impossible de savoir: prendre 2021 ?
bdd_double_clean = rbind(bdd_double_clean,OPVT_BZH)

# 14: Pour OPVT_IDF  : garde -------------------------------------------------------------
# Regle de selection: meme ID_Site, Modalite different et pas de bloc: on garde les parcelles
OPVT_IDF = bdd_double[bdd_double$Programme=="OPVT_IDF",]
OPVT_IDF = droplevels(OPVT_IDF)
nrow(OPVT_IDF)# 19
unique(OPVT_IDF$ID_Site) #9
OPVT_IDF [,id_1]
OPVT_IDF [,id_2]
OPVT_IDF [,id_4] # GPS different

OPVT_IDF$Annee <- as.numeric(as.character(OPVT_IDF$Annee))
derniere_annee <- OPVT_IDF %>%
  group_by(ID_Site) %>%
  filter(Annee == max(Annee)) %>%
  select(ID)
derniere_annee = as.data.frame(derniere_annee)
derniere_annee = droplevels(derniere_annee)
derniere_annee$ID <- as.character(derniere_annee$ID)
OPVT_IDF %>% filter(ID %in% derniere_annee$ID)
OPVT_IDF_select = data.frame(Programme = rep("OPVT_IDF"), ID =derniere_annee$ID)
write.csv2(x =OPVT_IDF_select,file = "datas/Non_repeated_data/select_id/OPVT_IDF_select.csv", row.names = FALSE)
# prendre dernier annee si annee different, mean(parcelle meme annee): meme ide_site et meme anne = mean

# VALIDE ***
bdd_double_clean = rbind(bdd_double_clean,OPVT_IDF)



# 17: Pour VineDivers  : garde -------------------------------------------------------------
# Regle de selection: meme ID_Site, Modalite different et pas de bloc: on garde les parcelles
VineDivers = bdd_double[bdd_double$Programme=="VineDivers",]
nrow(VineDivers) # 4
VineDivers [,id_1] # pas de blocs
VineDivers [,id_2] # meme annee
VineDivers [,id_4] # gps répété, meme OS
bdd_all[bdd_all$Programme=="VineDivers",id_3]
VineDivers_select = data.frame(Programme = rep("VineDivers"), ID =VineDivers$ID )
write.csv2(x =VineDivers_select,file = "datas/Non_repeated_data/select_id/VineDivers_select.csv", row.names = FALSE)

# VALIDE ***
bdd_double_clean = rbind(bdd_double_clean,VineDivers)





# 6: Pour Dephy : dernier annee -------------------------------------------------------------
# Regle de selection: pour chaque ID_Site, on prend la derniere annee
Dephy = bdd_double[bdd_double$Programme=="Dephy",]
Dephy = droplevels(Dephy)
nrow(Dephy)
unique(Dephy$ID_Site) #38
Dephy [,id_1] # pas de bloc et ni de moda
Dephy [,id_2]
Dephy [,id_3] # GPS répété par ID_Site
Dephy$Annee <- as.numeric(as.character(Dephy$Annee))
derniere_annee <- Dephy %>%
  group_by(ID_Site) %>%
  filter(Annee == max(Annee)) %>%
  select(ID)
derniere_annee = as.data.frame(derniere_annee)
derniere_annee = droplevels(derniere_annee)
derniere_annee$ID <- as.character(derniere_annee$ID)
Dephy %>% filter(ID %in% derniere_annee$ID)
Dephy_select = data.frame(Programme = rep("Dephy"), ID =derniere_annee$ID )
write.csv2(x =Dephy_select,file = "datas/Non_repeated_data/select_id/Dephy_select.csv", row.names = FALSE)
# VALIDE ***

Dephy_last = Dephy[Dephy$ID %in% Dephy_select$ID,]
dim(Dephy_last)
bdd_double_clean = rbind(bdd_double_clean,Dephy_last)


# 7: Pour Dephy_bio  : dernier annee -------------------------------------------------------------
# Regle de selection: pour chaque ID_Site, on prend la derniere annee
Dephy_bio = bdd_double[bdd_double$Programme=="Dephy Bio",]
Dephy_bio = droplevels(Dephy_bio)
nrow(Dephy_bio) # 16
unique(Dephy_bio$ID_Site) # 08
Dephy_bio [,id_1]
Dephy_bio [,id_2]
Dephy_bio [,id_3]
Dephy_bio$Annee <- as.numeric(as.character(Dephy_bio$Annee))
derniere_annee <- Dephy_bio %>%
  group_by(ID_Site) %>%
  filter(Annee == max(Annee)) %>%
  select(ID)
derniere_annee = as.data.frame(derniere_annee)
derniere_annee = droplevels(derniere_annee)
derniere_annee$ID <- as.character(derniere_annee$ID)
Dephy_bio %>% filter(ID %in% derniere_annee$ID)
Dephy_bio_select = data.frame(Programme = rep("Dephy Bio"), ID =derniere_annee$ID )
write.csv2(x =Dephy_bio_select,file = "datas/Non_repeated_data/select_id/Dephy_bio_select.csv", row.names = FALSE)

# VALIDE ***
Dephy_bio_last = Dephy_bio[Dephy_bio$ID %in% Dephy_bio_select$ID,]
dim(Dephy_bio_last)
bdd_double_clean = rbind(bdd_double_clean,Dephy_bio_last)



# 9: Pour Life-PTD : dernier annee  -------------------------------------------------------------
# Regle de selection: pour chaque ID_Site, on prend la derniere annee
Life_PTD = bdd_double[bdd_double$Programme=="Life-PTD",]
Life_PTD  = droplevels(Life_PTD )
nrow(Life_PTD) # 64
Life_PTD [,id_1]
Life_PTD [,id_2]
Life_PTD [,id_4] # GPS répété par ID_Site, meme OS
bdd_all[bdd_all$Programme=="Life-PTD",id_3]
unique(Life_PTD $ID_Site) # 32 parcelles uniques
Life_PTD $Annee <- as.numeric(as.character(Life_PTD $Annee))
derniere_annee <- Life_PTD  %>%
  group_by(ID_Site) %>%
  filter(Annee == max(Annee)) %>%
  select(ID)
derniere_annee = as.data.frame(derniere_annee)
derniere_annee = droplevels(derniere_annee)
derniere_annee$ID <- as.character(derniere_annee$ID)
Life_PTD  %>% filter(ID %in% derniere_annee$ID)
Life_PTD_select = data.frame(Programme = rep("Life-PTD"), ID =derniere_annee$ID )
write.csv2(x =Life_PTD_select,file = "datas/Non_repeated_data/select_id/Life_PTD_select.csv", row.names = FALSE)

# VALIDE ***
Life_PTD_last = Life_PTD[Life_PTD$ID %in% Life_PTD_select$ID,]
dim(Life_PTD_last)
bdd_double_clean = rbind(bdd_double_clean,Life_PTD_last)





# 15: Pour SBT_ENI_TB  : dernier annee -------------------------------------------------------------
# Regle de selection: pour chaque ID_Site, on prend la derniere annee
SBT_ENI_TB = bdd_double[bdd_double$Programme=="SBT-ENI-TB",]
SBT_ENI_TB = droplevels(SBT_ENI_TB)
nrow(SBT_ENI_TB) # 571
unique(SBT_ENI_TB$ID_Site) #190
SBT_ENI_TB [,id_1] # pas de moda et de bloc
SBT_ENI_TB [,id_2] 
SBT_ENI_TB [,id_4] # GPS identique par ID_Site
SBT_ENI_TB$Annee <- as.numeric(as.character(SBT_ENI_TB$Annee))
derniere_annee <- SBT_ENI_TB %>%
  group_by(ID_Site) %>%
  filter(Annee == max(Annee)) %>%
  select(ID)
derniere_annee = as.data.frame(derniere_annee)
derniere_annee = droplevels(derniere_annee)
derniere_annee$ID <- as.character(derniere_annee$ID)
SBT_ENI_TB %>% filter(ID %in% derniere_annee$ID)
SBT_ENI_TB_select = data.frame(Programme = rep("SBT-ENI-TB"), ID =derniere_annee$ID )
write.csv2(x =SBT_ENI_TB_select,file = "datas/Non_repeated_data/select_id/SBT_ENI_TB_select.csv", row.names = FALSE)

# VALIDE ***
SBT_ENI_TB_last = SBT_ENI_TB[SBT_ENI_TB$ID %in% SBT_ENI_TB_select$ID,]
dim(SBT_ENI_TB_last)
bdd_double_clean = rbind(bdd_double_clean,SBT_ENI_TB_last)
dim(bdd_double_clean)



# 16: Pour Vadebio  -------------------------------------------------------------
# Regle de selection, meme ID_Site, Modalite different + blocs : moyenne des blocs
Vadebio = bdd_double[bdd_double$Programme=="Vadebio",]
Vadebio = droplevels(Vadebio)
nrow(Vadebio) # 44
Vadebio [,id_1] # meme Annee
Vadebio [,id_2]
Vadebio [,id_4] # GPS répété, meme OS
bdd_all[bdd_all$Programme=="Vadebio",id_3]
unique(Vadebio$ID_Site) # 1
unique(Vadebio$ID_2) # 11 
Vadebio$Annee <- as.numeric(as.character(Vadebio$Annee))
uni = unique(Vadebio$ID_2)
Vadebio_select =  data.frame(ID_2 = character(), AB_tot = numeric(), 
                          BM_tot = numeric(), Richesse_tot =numeric())
for( i in  uni){
  df = Vadebio[Vadebio$ID_2==i, c("AB_tot", "BM_tot", "Richesse_tot")]
  df = droplevels(df)
  df_2 = data.frame(ID_2 = i, AB_tot = mean(df$AB_tot, na.rm = TRUE), 
                    BM_tot = mean(df$BM_tot, na.rm = TRUE), 
                    Richesse_tot = mean(df$Richesse_tot, na.rm = TRUE))
  Vadebio_select = rbind(Vadebio_select,df_2)
}
Vadebio_select$BM_tot[is.nan(Vadebio_select$BM_tot)] <- NA
Vadebio_select
# verification
Vadebio [Vadebio$ID_2 == "Vadebio_La Bouzule_2011_CA",id_2]
write.csv2(x =Vadebio_select,file = "datas/Non_repeated_data/select_id/Vadebio_select.csv", row.names = FALSE)
# DC ?
# PAS TOUCHE 
Vadebio[Vadebio$Bloc == "B1", id_1]
Vadebio_bloc = Vadebio[Vadebio$Bloc == "B1",]
Vadebio_bloc = droplevels(Vadebio_bloc)
Vadebio_bloc$ID_2
Vadebio_select$ID_2
Vadebio_bloc[,id_1]
Vadebio_bloc$AB_tot = Vadebio_select$AB_tot
Vadebio_bloc$BM_tot = Vadebio_select$BM_tot
Vadebio_bloc$Richesse_tot = round (Vadebio_select$Richesse_tot)

dim(Vadebio_bloc)
bdd_double_clean = rbind(bdd_double_clean,Vadebio_bloc)
dim(bdd_double_clean)




# 18: Pour VitiEcobioSol_CIBC  -------------------------------------------------------------
# Regle de selection, meme ID_Site, Modalite different + blocs : moyenne des blocs
VitiEcobioSol_CIBC = bdd_double[bdd_double$Programme=="VitiEcobioSol-CIBC",]
VitiEcobioSol_CIBC = droplevels(VitiEcobioSol_CIBC)
nrow(VitiEcobioSol_CIBC) # 278
VitiEcobioSol_CIBC [,id_1]
VitiEcobioSol_CIBC [,id_2]
VitiEcobioSol_CIBC [,id_4] # GPS répété, meme OS
bdd_all[bdd_all$Programme=="VitiEcobioSol-CIBC",id_3]
unique(VitiEcobioSol_CIBC$ID_Site) # 17
unique(VitiEcobioSol_CIBC$ID_2) # 195
VitiEcobioSol_CIBC$Annee <- as.numeric(as.character(VitiEcobioSol_CIBC$Annee))
uni = unique(VitiEcobioSol_CIBC$ID_2)
VitiEcobioSol_CIBC_select =  data.frame(ID_2 = character(), AB_tot = numeric(), 
                             BM_tot = numeric(), Richesse_tot =numeric())
for( i in  uni){
  df = VitiEcobioSol_CIBC[VitiEcobioSol_CIBC$ID_2==i, c("AB_tot", "BM_tot", "Richesse_tot")]
  df = droplevels(df)
  df_2 = data.frame(ID_2 = i, AB_tot = mean(df$AB_tot, na.rm = TRUE), 
                    BM_tot = mean(df$BM_tot, na.rm = TRUE), 
                    Richesse_tot = mean(df$Richesse_tot, na.rm = TRUE))
  VitiEcobioSol_CIBC_select = rbind(VitiEcobioSol_CIBC_select,df_2)
}
VitiEcobioSol_CIBC_select$BM_tot[is.nan(VitiEcobioSol_CIBC_select$BM_tot)] <- NA
VitiEcobioSol_CIBC_select
# verification
VitiEcobioSol_CIBC [VitiEcobioSol_CIBC$ID_2 == "VitiEcobioSol-CIBC_MB_Ferti_2000_MB_Ferti_M",id_2]
write.csv2(x =VitiEcobioSol_CIBC_select,file = "datas/Non_repeated_data/select_id/VitiEcobioSol_CIBC_select.csv", row.names = FALSE)
# prendre uniquement 2000

VitiEcobioSol_CIBC[VitiEcobioSol_CIBC$Annee =="2000", id_1]
VitiEcobioSol_CIBC_2000 = VitiEcobioSol_CIBC[VitiEcobioSol_CIBC$Annee =="2000",]
VitiEcobioSol_CIBC_2000 = droplevels(VitiEcobioSol_CIBC_2000)


dim(VitiEcobioSol_CIBC_2000)
bdd_double_clean = rbind(bdd_double_clean,VitiEcobioSol_CIBC_2000)
dim(bdd_double_clean)





# 19: Pour ZAA_HR  -------------------------------------------------------------
# Regle de selection: meme ID_Site, Modalite different et pas de bloc: mean par id site

ZAA_HR = bdd_double[bdd_double$Programme=="ZAA_HR",]
ZAA_HR = droplevels(ZAA_HR)
nrow(ZAA_HR) # 19
ZAA_HR [,id_1] # pas de blocs
ZAA_HR [,id_2] # meme annee
ZAA_HR [,id_4] # gps répété par ID_Site, meme OS
bdd_all[bdd_all$Programme=="ZAA_HR",id_3]
unique(ZAA_HR$ID_Site) # 3
unique(ZAA_HR$ID_2) # 19 
ZAA_HR$Annee <- as.numeric(as.character(ZAA_HR$Annee))
# ZAA_HR <- ZAA_HR %>% filter (Annee == max(ZAA_HR$Annee))
uni = unique(ZAA_HR$ID_Site)
ZAA_HR_select =  data.frame(ID_2 = character(), AB_tot = numeric(), 
                          BM_tot = numeric(), Richesse_tot =numeric())
for( i in  uni){
  df = ZAA_HR[ZAA_HR$ID_Site==i, c("AB_tot", "BM_tot", "Richesse_tot")]
  df = droplevels(df)
  df_2 = data.frame(ID_Site = i, AB_tot = mean(df$AB_tot, na.rm = TRUE), 
                    BM_tot = mean(df$BM_tot, na.rm = TRUE), 
                    Richesse_tot = mean(df$Richesse_tot, na.rm = TRUE))
  ZAA_HR_select = rbind(ZAA_HR_select,df_2)
}
ZAA_HR_select$BM_tot[is.nan(ZAA_HR_select$BM_tot)] <- NA
ZAA_HR_select
# verification
ZAA_HR [ZAA_HR$ID_Site == "ZAA_HR_GP_2014_P25",id_2]
write.csv2(x =ZAA_HR_select,file = "datas/Non_repeated_data/select_id/ZAA_HR_select.csv", row.names = FALSE)

# supprimer ZAA_HR si idsite s v et x dans l'autre ZAA: mean par ID_Site 
# bdd_all$Programme = as.factor(bdd_all$Programme)
# levels(bdd_all$Programme)
# bdd_all[bdd_all$Programme=="ZAA",id_4]

ZAA_HR[ZAA_HR$Modalite == "H01", id_1]
ZAA_HR_ID_Site = ZAA_HR[ZAA_HR$Modalite == "H01",]
ZAA_HR_ID_Site = droplevels(ZAA_HR_ID_Site)
ZAA_HR_ID_Site$ID_Site
ZAA_HR_select$ID_Site
ZAA_HR_ID_Site[,id_1]
ZAA_HR_ID_Site$AB_tot = ZAA_HR_select$AB_tot
ZAA_HR_ID_Site$BM_tot = ZAA_HR_select$BM_tot
ZAA_HR_ID_Site$Richesse_tot = round (ZAA_HR_select$Richesse_tot)

dim(ZAA_HR_ID_Site)
bdd_double_clean = rbind(bdd_double_clean,ZAA_HR_ID_Site)
dim(bdd_double_clean)




# 2: Pour Beaujolais-SICAREX  -------------------------------------------------------------
# Regle de selection, meme ID_Site, Modalite different + blocs : moyenne des blocs
Beaujolais_S = bdd_double[bdd_double$Programme=="Beaujolais-SICAREX",]
Beaujolais_S = droplevels(Beaujolais_S)
nrow(Beaujolais_S) # 54
Beaujolais_S [,id_1]
Beaujolais_S [,id_2]
Beaujolais_S [,id_4] # GPS répété, meme OS
bdd_all[bdd_all$Programme=="Beaujolais-SICAREX",id_3] # gps répété et meme OS
Beaujolais_S <- Beaujolais_S %>% filter(! Annee =="2008") # on supprime Annee==2008
unique(Beaujolais_S$ID_Site) # 3
unique(Beaujolais_S$ID_2) # 12
Beaujolais_S$Annee <- as.numeric(as.character(Beaujolais_S$Annee))
uni =  unique(Beaujolais_S$ID_2)
Beaujolais_S_select =  data.frame(ID_2 = character(), AB_tot = numeric(), 
                                  BM_tot = numeric(), Richesse_tot =numeric())
for( i in  uni){
  df = Beaujolais_S[Beaujolais_S$ID_2==i, c("AB_tot", "BM_tot", "Richesse_tot")]
  df = droplevels(df)
  df_2 = data.frame(ID_2 = i, AB_tot = mean(df$AB_tot, na.rm = TRUE), 
                    BM_tot = mean(df$BM_tot, na.rm = TRUE), 
                    Richesse_tot = mean(df$Richesse_tot, na.rm = TRUE))
  Beaujolais_S_select = rbind(Beaujolais_S_select,df_2)
}
Beaujolais_S_select$BM_tot[is.nan(Beaujolais_S_select$BM_tot)] <- NA
Beaujolais_S_select
# verification
Beaujolais_S [Beaujolais_S$ID_2 == "Beaujolais-SICAREX_EntretienSol-C_2014_C - EC",id_2]
write.csv2(x =Beaujolais_S_select,file = "datas/Non_repeated_data/select_id/Beaujolais_S_select.csv", row.names = FALSE)

# VALIDE ***
Beaujolais_S [,id_1]
Beaujolais_S[1:6,id_1]$Bloc = "B1"
Beaujolais_S[Beaujolais_S$Bloc == "B1", id_1]
Beaujolais_S_bloc = Beaujolais_S[Beaujolais_S$Bloc == "B1",]
Beaujolais_S_bloc = droplevels(Beaujolais_S_bloc)
Beaujolais_S_bloc$ID_2
Beaujolais_S_select$ID_2
Beaujolais_S_bloc[,id_1]
Beaujolais_S_bloc$AB_tot = Beaujolais_S_select$AB_tot
Beaujolais_S_bloc$BM_tot = Beaujolais_S_select$BM_tot
Beaujolais_S_bloc$Richesse_tot = round (Beaujolais_S_select$Richesse_tot)

dim(Beaujolais_S_bloc)
bdd_double_clean = rbind(bdd_double_clean,Beaujolais_S_bloc)
dim(bdd_double_clean)




# 10: Pour Macon-Davayé  -------------------------------------------------------------
# Regle de selection, meme ID_Site, Modalite different + blocs : moyenne des blocs
Macon_Davayé = bdd_double[bdd_double$Programme=="Macon-Davayé",]
Macon_Davayé = droplevels(Macon_Davayé)
nrow(Macon_Davayé) # 36
Macon_Davayé [,id_1]
Macon_Davayé [,id_2]
Macon_Davayé [,id_4] # GPS répété, meme OS
bdd_all[bdd_all$Programme=="Macon-Davayé",id_3]
unique(Macon_Davayé$ID_Site) # 1
unique(Macon_Davayé$ID_2) # 12 
Macon_Davayé$Annee <- as.numeric(as.character(Macon_Davayé$Annee))
Macon_Davayé = Macon_Davayé %>% filter(Annee=="2018")
uni = unique(Macon_Davayé$ID_2)
Macon_Davayé_select =  data.frame(ID_2 = character(), AB_tot = numeric(), 
                                  BM_tot = numeric(), Richesse_tot =numeric())
for( i in  uni){
  df = Macon_Davayé[Macon_Davayé$ID_2==i, c("AB_tot", "BM_tot", "Richesse_tot")]
  df = droplevels(df)
  df_2 = data.frame(ID_2 = i, AB_tot = mean(df$AB_tot, na.rm = TRUE), 
                    BM_tot = mean(df$BM_tot, na.rm = TRUE), 
                    Richesse_tot = mean(df$Richesse_tot, na.rm = TRUE))
  Macon_Davayé_select = rbind(Macon_Davayé_select,df_2)
}
Macon_Davayé_select$BM_tot[is.nan(Macon_Davayé_select$BM_tot)] <- NA
Macon_Davayé_select
# verification
Macon_Davayé [Macon_Davayé$ID_2 == "Macon-Davayé_Davayé_2018_BL",id_2]
write.csv2(x =Macon_Davayé_select,file = "datas/Non_repeated_data/select_id/Macon_Davayé_select.csv", row.names = FALSE)
# uniquement 2018 et mean bloc

# VALIDE ***: voir mail 05/04/2024
Macon_Davayé [,id_1]
Macon_Davayé[Macon_Davayé$Bloc == "1", id_1]
Macon_Davayé_bloc = Macon_Davayé[Macon_Davayé$Bloc == "1",]
Macon_Davayé_bloc = droplevels(Macon_Davayé_bloc)
Macon_Davayé_bloc$ID_2
Macon_Davayé_select$ID_2
Macon_Davayé_bloc[,id_1]
Macon_Davayé_bloc$AB_tot = Macon_Davayé_select$AB_tot
Macon_Davayé_bloc$BM_tot = Macon_Davayé_select$BM_tot
Macon_Davayé_bloc$Richesse_tot = round (Macon_Davayé_select$Richesse_tot)

dim(Macon_Davayé_bloc)
bdd_double_clean = rbind(bdd_double_clean,Macon_Davayé_bloc)
dim(bdd_double_clean)



# 11: Pour MONS : derniers annee -------------------------------------------------------------
# Regle de selection, meme ID_Site, Modalite different + blocs : derniers annee
MONS = bdd_double[bdd_double$Programme=="MONS",]
MONS = droplevels(MONS)
nrow(MONS) # 108
MONS [,id_1]
MONS [,id_2]
MONS [,id_4] # GPS répété, meme OS
bdd_all[bdd_all$Programme=="MONS",id_3]
unique(MONS$ID_Site) # 2
unique(MONS$ID_2) # 30 
MONS$Annee <- as.numeric(as.character(MONS$Annee))
MONS <- MONS %>% filter (Annee == max(MONS$Annee))
uni = unique(MONS$ID_2)
MONS_select =  data.frame(ID_2 = character(), AB_tot = numeric(), 
                          BM_tot = numeric(), Richesse_tot =numeric())
for( i in  uni){
  df = MONS[MONS$ID_2==i, c("AB_tot", "BM_tot", "Richesse_tot")]
  df = droplevels(df)
  df_2 = data.frame(ID_2 = i, AB_tot = mean(df$AB_tot, na.rm = TRUE), 
                    BM_tot = mean(df$BM_tot, na.rm = TRUE), 
                    Richesse_tot = mean(df$Richesse_tot, na.rm = TRUE))
  MONS_select = rbind(MONS_select,df_2)
}
MONS_select$BM_tot[is.nan(MONS_select$BM_tot)] <- NA
MONS_select
# verification
MONS [MONS$ID_2 == "MONS_GP_2014_P25",id_2]
write.csv2(x =MONS_select,file = "datas/Non_repeated_data/select_id/MONS_select.csv", row.names = FALSE)
# uniquement dernier annee et pas touché: idem EFFFEL

# VALIDE ***
MONS = droplevels(MONS)
dim(MONS)
MONS$Annee
bdd_double_clean = rbind(bdd_double_clean,MONS)
dim(bdd_double_clean)





# bdd 100% clean ----------------------------------------------------------
# bdd_clean: les lignes de bdd_all où l'ID n'est pas présent dans bdd_double, 0 doulons dans gps
bdd_all$ID <- as.character(bdd_all$ID)
bdd_double$ID <- as.character(bdd_double$ID)
bdd_clean <- bdd_all[!(bdd_all$ID %in% bdd_double$ID), ]
dim(bdd_clean) # 1825  348
doub_id_site = subset(bdd_clean, duplicated(bdd_clean[, c("ID_Site")]))
nrow(doub_id_site)
bdd_clean = bdd_clean [- which(bdd_clean$ID %in% doub_id_site$ID),]
subset(bdd_clean, duplicated(bdd_clean[, c("Programme", "ID_Site")]))
double_gps = subset(bdd_clean, duplicated(bdd_clean[, c("gps_x", "gps_y")]))
nrow(double_gps)
bdd_clean = bdd_clean [- which(bdd_clean$ID %in% double_gps$ID),]
dim(bdd_clean)# 1683  348

n_ligne= nrow(bdd_clean)
df_coord <- bdd_clean[, c("gps_x", "gps_y")] %>% mutate(gps_x = as.numeric(gps_x),gps_y = as.numeric(gps_y))

df_coord$num_ligne <- seq(nrow(df_coord))
carte <- leaflet(df_coord) %>%
  addTiles() %>%
  addCircleMarkers(lng = ~gps_x, lat = ~gps_y, radius = 0.8, fillOpacity = 0.8, fillColor = "blue")
carte


id_2=c("ID","Programme","Annee","ID_Site","Protocole")
vdt_col=c("AB_tot", "BM_tot", "Richesse_tot")
land_cover_col=c("clcm_lvl3")
topo_col=c("elevation","gps_x","gps_y")
soil_col=c("ph_0_a_30","sable.0_30","limon.0_30","argile.0_30","c_orga_0_a_30","P","N","K","CN","CEC","CaCO3")


climate_col=c()
for (i in 1:19){
  climate_col=c(climate_col, paste0("bio",i) )
}
climate_col=c(climate_col,"cmi_mean","gdd0","gdd10","hurs_mean","pet_penman_mean")

bdd_clean_mini= bdd_clean[,c(id_2,vdt_col,land_cover_col,topo_col,soil_col,climate_col)]
# str(bdd_clean_mini)
bdd_clean_mini$ID = as.factor(bdd_clean_mini$ID)
bdd_clean_mini$clcm_lvl3 = as.factor(bdd_clean_mini$clcm_lvl3)


# Renome
new_soil_col=c("pH","sand","silt","clay","C","P","N","K","CN","CEC","CaCO3")
bdd_clean_mini <- rename(bdd_clean_mini, !!setNames(soil_col, new_soil_col))

bdd_clean_mini <- bdd_clean_mini %>% 
  rename(PET = pet_penman_mean)
climate_col=c()
for (i in 1:19){
  climate_col=c(climate_col, paste0("bio",i) )
}
climate_col=c(climate_col,"cmi_mean","gdd0","gdd10","hurs_mean","PET")
col_graph=c(vdt_col,land_cover_col,topo_col,new_soil_col,climate_col)


summary(bdd_clean_mini$clcm_lvl3)
levels(bdd_clean_mini$clcm_lvl3)[levels(bdd_clean_mini$clcm_lvl3) == "Broad-leaved forest"] <- "forest"
cl_original <- levels(bdd_clean_mini$clcm_lvl3)
new_cl <- c("f","gua", "ng", "nial", "p", "v")
bdd_clean_mini$clcm_lvl3 <- factor(bdd_clean_mini$clcm_lvl3, levels = cl_original, labels = new_cl)



# distribution
## AB_tot
shapiro.test(bdd_clean_mini$AB_tot)
ggplot(bdd_clean_mini, aes(x=AB_tot)) +
  geom_histogram(aes(y=..density..), fill="#69b3a2", color="#e9ecef", bins=30, alpha=2) +
  geom_density(fill="black", alpha=0.2) +
  theme_gray() +
  labs(title="Abundance", x="Value", y="Density") +
  theme(plot.title = element_text(hjust = 0.5))

x = as.numeric ( bdd_clean_mini$AB_tot)
bestNormalize(x)
orderNorm_obj <- orderNorm(x)
orderNorm_obj
p <- predict(orderNorm_obj)
x2 <- predict(orderNorm_obj, newdata = p, inverse = TRUE)
all.equal(x2, x)

bdd_clean_mini$AB_tot_2=p
shapiro.test(bdd_clean_mini$AB_tot_2)
ggplot(bdd_clean_mini, aes(x=AB_tot_2)) +
  geom_histogram(aes(y=..density..), fill="#69b3a2", color="#e9ecef", bins=30, alpha=2) +
  geom_density(fill="black", alpha=0.2) +
  theme_gray() +
  labs(title="Abundance", x="Value", y="Density") +
  theme(plot.title = element_text(hjust = 0.5))


## BM_tot
shapiro.test(bdd_clean_mini$BM_tot)
ggplot(bdd_clean_mini, aes(x=BM_tot)) +
  geom_histogram(aes(y=..density..), fill="#69b3a2", color="#e9ecef", bins=30, alpha=2) +
  geom_density(fill="black", alpha=0.2) +
  theme_gray() +
  labs(title="Abundance", x="Value", y="Density") +
  theme(plot.title = element_text(hjust = 0.5))

x = as.numeric ( bdd_clean_mini$BM_tot)
bestNormalize(x)
orderNorm_obj <- orderNorm(x)
orderNorm_obj
p <- predict(orderNorm_obj)
x2 <- predict(orderNorm_obj, newdata = p, inverse = TRUE)
all.equal(x2, x)

bdd_clean_mini$BM_tot_2=p
shapiro.test(bdd_clean_mini$BM_tot_2)
ggplot(bdd_clean_mini, aes(x=BM_tot_2)) +
  geom_histogram(aes(y=..density..), fill="#69b3a2", color="#e9ecef", bins=30, alpha=2) +
  geom_density(fill="black", alpha=0.2) +
  theme_gray() +
  labs(title="Abundance", x="Value", y="Density") +
  theme(plot.title = element_text(hjust = 0.5))




## Richesse_tot
shapiro.test(bdd_clean_mini$Richesse_tot)
ggplot(bdd_clean_mini, aes(x=Richesse_tot)) +
  geom_histogram(aes(y=..density..), fill="#69b3a2", color="#e9ecef", bins=30, alpha=2) +
  geom_density(fill="black", alpha=0.2) +
  theme_gray() +
  labs(title="Abundance", x="Value", y="Density") +
  theme(plot.title = element_text(hjust = 0.5))

x = as.numeric ( bdd_clean_mini$Richesse_tot)
bestNormalize(x)
orderNorm_obj <- orderNorm(x)
orderNorm_obj
p <- predict(orderNorm_obj)
x2 <- predict(orderNorm_obj, newdata = p, inverse = TRUE)
all.equal(x2, x)

bdd_clean_mini$Richesse_tot_2=p
shapiro.test(bdd_clean_mini$Richesse_tot_2)
ggplot(bdd_clean_mini, aes(x=Richesse_tot_2)) +
  geom_histogram(aes(y=..density..), fill="#69b3a2", color="#e9ecef", bins=30, alpha=2) +
  geom_density(fill="black", alpha=0.2) +
  theme_gray() +
  labs(title="Abundance", x="Value", y="Density") +
  theme(plot.title = element_text(hjust = 0.5))





bdd_clean_mini$AB_tot_sqrt = sqrt(bdd_clean_mini$AB_tot)
bdd_clean_mini$BM_tot_sqrt = sqrt(bdd_clean_mini$BM_tot)
# bdd_clean_mini$Richesse_tot = sqrt(bdd_clean_mini$Richesse_tot)
# for(i in names(bdd_clean_mini)){ cat (i,"," )}
predicteurs = c('elevation' ,'gps_x' ,'gps_y' ,'pH' ,'sand' ,'silt' ,'clay' ,'C' ,'P' ,'N' ,'K' ,
                'CN' ,'CEC' ,'CaCO3' ,'bio1' ,'bio2' ,'bio3' ,'bio4' ,'bio5' ,'bio6' ,'bio7' ,'bio8'
                ,'bio9' ,'bio10' ,'bio11' ,'bio12' ,'bio13' ,'bio14' ,'bio15' ,'bio16' ,'bio17' ,
                'bio18' ,'bio19' ,'cmi_mean' ,'gdd0' ,'gdd10' ,'hurs_mean' ,'PET')
bdd_clean_mini[,predicteurs] = scale(bdd_clean_mini[,predicteurs])
summary(bdd_clean_mini)
# names(bdd_clean_mini)



# AB_tot _______________________________________________________________________
df_explo_AB_tot = bdd_clean_mini[,c("AB_tot_sqrt","clcm_lvl3",predicteurs )]
df_explo_AB_tot = drop_na(df_explo_AB_tot)
# summary(df_explo_AB_tot)

set.seed(1234)  # Pour rendre les résultats reproductibles
index <- createDataPartition(df_explo_AB_tot$clcm_lvl3, p = 0.8, list = FALSE)

# Séparer les données en ensembles d'entraînement et de test
df_train_AB_tot <- df_explo_AB_tot[index, ]  # Données d'entraînement
df_test_AB_tot <- df_explo_AB_tot[-index, ]  # Données de test
df_train_AB_tot = droplevels(df_train_AB_tot)
df_test_AB_tot = droplevels(df_test_AB_tot)


all_var_AB_tot=ForetAlea(var_rep ="AB_tot_sqrt", df_app=df_train_AB_tot, df_valid = df_test_AB_tot,
                         mtry =9,ntree= 2000,maxnodes = NULL)

all_var_AB_tot$RMSE
all_var_AB_tot$R_adj_train
all_var_AB_tot$R_adj_test
AB_tot_fit_rf = all_var_AB_tot$model
AB_tot_fit_rf #display fitted model
# which.min(AB_tot_fit_rf$mse) #find number of trees that produce lowest test MSE
# sqrt(AB_tot_fit_rf$mse[which.min(AB_tot_fit_rf$mse)]) #find RMSE of best model
# varImpPlot(AB_tot_fit_rf) #produce variable importance plot

# Obtention de l'importance des variables
importance_rf <- as.data.frame(importance(AB_tot_fit_rf))
importance_rf$nom=rownames(importance_rf)
importance_rf <- importance_rf[order(importance_rf$IncNodePurity,decreasing = TRUE), ]
best_20_AB_tot = c(importance_rf$nom[1:20])
var_a_sup_AB_tot= setdiff(y = best_20_AB_tot,x = importance_rf$nom)




best20_var_AB_tot=ForetAlea(var_rep ="AB_tot_sqrt", df_app=df_train_AB_tot[,c("AB_tot_sqrt",best_20_AB_tot)], 
                            df_valid = df_test_AB_tot [,c("AB_tot_sqrt",best_20_AB_tot)],mtry =9,ntree= 2000,maxnodes = NULL)

best20_var_AB_tot$RMSE # 5.4
best20_var_AB_tot$R_adj_train # 0.37
best20_var_AB_tot$R_adj_test # 0.36
best20_var_AB_tot$MAE
rf.model = best20_var_AB_tot$model
rf.model # 36.74, 36.73

AB_tot_RF_tuning = read.csv2("results_tuning/AB_tot_RF_tuning.csv")
AB_tot_RF_tuning = as.data.frame(AB_tot_RF_tuning)
AB_tot_RF_tuning = AB_tot_RF_tuning %>% arrange(mae)
AB_tot_best_param = AB_tot_RF_tuning[1,]
AB_tot_best_mtry = AB_tot_best_param$mtry
AB_tot_best_ntree = AB_tot_best_param$ntree
AB_tot_best_maxnodes = AB_tot_best_param$maxnode
RF_result_AB_tot = ForetAlea(var_rep ="AB_tot_sqrt", 
                             df_app=df_train_AB_tot[,c("AB_tot_sqrt",best_20_AB_tot)], 
                             df_valid = df_test_AB_tot[,c("AB_tot_sqrt",best_20_AB_tot)],
                             mtry = AB_tot_best_mtry,
                             ntree= AB_tot_best_ntree,
                             maxnodes = NULL)
RF_AB_tot_pred <- RF_result_AB_tot$predit^2
RF_df_AB_tot = data.frame(Observed=df_test_AB_tot[,1]^2,Predicted = RF_AB_tot_pred)
cor_RF_AB_tot <- cor(RF_df_AB_tot$Observed, RF_df_AB_tot$Predicted)
# graphique avec ggplot
RF_AB_tot <- ggplot(RF_df_AB_tot, aes(x = Observed, y = Predicted)) +
  geom_point() + # Ajout des points
  geom_smooth(method = "lm", se = TRUE, color = "red") + 
  labs(subtitle =paste0(" RF: R² adj (train) = ", round(RF_result_AB_tot$R_adj_train,2), 
                        "; \n R² adj (test) = ", round(RF_result_AB_tot$R_adj_test,2),
                        "; \n RMSE (test) = ",  round(RF_result_AB_tot$RMSE^2,2)),
       x = "Real values", 
       y = "Predicted values") + 
  theme_classic() 




# BM_tot _______________________________________________________________________
df_explo_BM_tot = bdd_clean_mini[,c("BM_tot_sqrt","clcm_lvl3",predicteurs )]
df_explo_BM_tot = drop_na(df_explo_BM_tot)
# summary(df_explo_BM_tot)

set.seed(1234)  # Pour rendre les résultats reproductibles
index <- createDataPartition(df_explo_BM_tot$clcm_lvl3, p = 0.8, list = FALSE)

# Séparer les données en ensembles d'entraînement et de test
df_train_BM_tot <- df_explo_BM_tot[index, ]  # Données d'entraînement
df_test_BM_tot <- df_explo_BM_tot[-index, ]  # Données de test
df_train_BM_tot = droplevels(df_train_BM_tot)
df_test_BM_tot = droplevels(df_test_BM_tot)


all_var_BM_tot=ForetAlea(var_rep ="BM_tot_sqrt", df_app=df_train_BM_tot, df_valid = df_test_BM_tot,
                         mtry =9,ntree= 2000,maxnodes = NULL)

all_var_BM_tot$RMSE
all_var_BM_tot$R_adj_train
all_var_BM_tot$R_adj_test
BM_tot_fit_rf = all_var_BM_tot$model
BM_tot_fit_rf #display fitted model
# which.min(BM_tot_fit_rf$mse) #find number of trees that produce lowest test MSE
# sqrt(BM_tot_fit_rf$mse[which.min(BM_tot_fit_rf$mse)]) #find RMSE of best model
# varImpPlot(BM_tot_fit_rf) #produce variable importance plot

# Obtention de l'importance des variables
importance_rf <- as.data.frame(importance(BM_tot_fit_rf))
importance_rf$nom=rownames(importance_rf)
importance_rf <- importance_rf[order(importance_rf$IncNodePurity,decreasing = TRUE), ]
best_20_BM_tot = c(importance_rf$nom[1:20])
var_a_sup_BM_tot= setdiff(y = best_20_BM_tot,x = importance_rf$nom)




best20_var_BM_tot=ForetAlea(var_rep ="BM_tot_sqrt", df_app=df_train_BM_tot[,c("BM_tot_sqrt",best_20_BM_tot)], 
                            df_valid = df_test_BM_tot [,c("BM_tot_sqrt",best_20_BM_tot)],mtry =9,ntree= 2000,maxnodes = NULL)

best20_var_BM_tot$RMSE # 5.4
best20_var_BM_tot$R_adj_train # 0.37
best20_var_BM_tot$R_adj_test # 0.36
best20_var_BM_tot$MAE
rf.model = best20_var_BM_tot$model
rf.model # 36.74, 36.73

BM_tot_RF_tuning = read.csv2("results_tuning/BM_tot_RF_tuning.csv")
BM_tot_RF_tuning = as.data.frame(BM_tot_RF_tuning)
BM_tot_RF_tuning = BM_tot_RF_tuning %>% arrange(mae)
BM_tot_best_param = BM_tot_RF_tuning[1,]
BM_tot_best_mtry = BM_tot_best_param$mtry
BM_tot_best_ntree = BM_tot_best_param$ntree
BM_tot_best_maxnodes = BM_tot_best_param$maxnode
RF_result_BM_tot = ForetAlea(var_rep ="BM_tot_sqrt", 
                             df_app=df_train_BM_tot[,c("BM_tot_sqrt",best_20_BM_tot)], 
                             df_valid = df_test_BM_tot[,c("BM_tot_sqrt",best_20_BM_tot)],
                             mtry = BM_tot_best_mtry,
                             ntree= BM_tot_best_ntree,
                             maxnodes = NULL)
RF_BM_tot_pred <- RF_result_BM_tot$predit^2
RF_df_BM_tot = data.frame(Observed=df_test_BM_tot[,1]^2,Predicted = RF_BM_tot_pred)
cor_RF_BM_tot <- cor(RF_df_BM_tot$Observed, RF_df_BM_tot$Predicted)
# graphique avec ggplot
RF_BM_tot <- ggplot(RF_df_BM_tot, aes(x = Observed, y = Predicted)) +
  geom_point() + # Ajout des points
  geom_smooth(method = "lm", se = TRUE, color = "red") + 
  labs(subtitle =paste0(" RF: R² adj (train) = ", round(RF_result_BM_tot$R_adj_train,2), 
                        "; \n R² adj (test) = ", round(RF_result_BM_tot$R_adj_test,2),
                        "; \n RMSE (test) = ",  round(RF_result_BM_tot$RMSE^2,2)),
       x = "Real values", 
       y = "Predicted values") + 
  theme_classic() 



# Richesse_tot _______________________________________________________________________
df_explo_Richesse_tot = bdd_clean_mini[,c("Richesse_tot","clcm_lvl3",predicteurs )]
df_explo_Richesse_tot = drop_na(df_explo_Richesse_tot)
# summary(df_explo_Richesse_tot)

set.seed(1234)  # Pour rendre les résultats reproductibles
index <- createDataPartition(df_explo_Richesse_tot$clcm_lvl3, p = 0.8, list = FALSE)

# Séparer les données en ensembles d'entraînement et de test
df_train_Richesse_tot <- df_explo_Richesse_tot[index, ]  # Données d'entraînement
df_test_Richesse_tot <- df_explo_Richesse_tot[-index, ]  # Données de test
df_train_Richesse_tot = droplevels(df_train_Richesse_tot)
df_test_Richesse_tot = droplevels(df_test_Richesse_tot)


all_var_Richesse_tot=ForetAlea(var_rep ="Richesse_tot", df_app=df_train_Richesse_tot, df_valid = df_test_Richesse_tot,
                         mtry =9,ntree= 2000,maxnodes = NULL)

all_var_Richesse_tot$RMSE
all_var_Richesse_tot$R_adj_train
all_var_Richesse_tot$R_adj_test
Richesse_tot_fit_rf = all_var_Richesse_tot$model
Richesse_tot_fit_rf #display fitted model
# which.min(Richesse_tot_fit_rf$mse) #find number of trees that produce lowest test MSE
# sqrt(Richesse_tot_fit_rf$mse[which.min(Richesse_tot_fit_rf$mse)]) #find RMSE of best model
# varImpPlot(Richesse_tot_fit_rf) #produce variable importance plot

# Obtention de l'importance des variables
importance_rf <- as.data.frame(importance(Richesse_tot_fit_rf))
importance_rf$nom=rownames(importance_rf)
importance_rf <- importance_rf[order(importance_rf$IncNodePurity,decreasing = TRUE), ]
best_20_Richesse_tot = c(importance_rf$nom[1:20])
var_a_sup_Richesse_tot= setdiff(y = best_20_Richesse_tot,x = importance_rf$nom)




best20_var_Richesse_tot=ForetAlea(var_rep ="Richesse_tot", df_app=df_train_Richesse_tot[,c("Richesse_tot",best_20_Richesse_tot)], 
                            df_valid = df_test_Richesse_tot [,c("Richesse_tot",best_20_Richesse_tot)],mtry =9,ntree= 2000,maxnodes = NULL)

best20_var_Richesse_tot$RMSE # 5.4
best20_var_Richesse_tot$R_adj_train # 0.37
best20_var_Richesse_tot$R_adj_test # 0.36
best20_var_Richesse_tot$MAE
rf.model = best20_var_Richesse_tot$model
rf.model # 36.74, 36.73

Richesse_tot_RF_tuning = read.csv2("results_tuning/Richesse_tot_RF_tuning.csv")
Richesse_tot_RF_tuning = as.data.frame(Richesse_tot_RF_tuning)
Richesse_tot_RF_tuning = Richesse_tot_RF_tuning %>% arrange(mae)
Richesse_tot_best_param = Richesse_tot_RF_tuning[1,]
Richesse_tot_best_mtry = Richesse_tot_best_param$mtry
Richesse_tot_best_ntree = Richesse_tot_best_param$ntree
Richesse_tot_best_maxnodes = Richesse_tot_best_param$maxnode
RF_result_Richesse_tot = ForetAlea(var_rep ="Richesse_tot", 
                             df_app=df_train_Richesse_tot[,c("Richesse_tot",best_20_Richesse_tot)], 
                             df_valid = df_test_Richesse_tot[,c("Richesse_tot",best_20_Richesse_tot)],
                             mtry = Richesse_tot_best_mtry,
                             ntree= Richesse_tot_best_ntree,
                             maxnodes = NULL)
RF_Richesse_tot_pred <- RF_result_Richesse_tot$predit
RF_df_Richesse_tot = data.frame(Observed=df_test_Richesse_tot[,1],Predicted = RF_Richesse_tot_pred)
cor_RF_Richesse_tot <- cor(RF_df_Richesse_tot$Observed, RF_df_Richesse_tot$Predicted)
# graphique avec ggplot
RF_Richesse_tot <- ggplot(RF_df_Richesse_tot, aes(x = Observed, y = Predicted)) +
  geom_point() + # Ajout des points
  geom_smooth(method = "lm", se = TRUE, color = "red") + 
  labs(subtitle =paste0(" RF: R² adj (train) = ", round(RF_result_Richesse_tot$R_adj_train,2), 
                        "; \n R² adj (test) = ", round(RF_result_Richesse_tot$R_adj_test,2),
                        "; \n RMSE (test) = ",  round(RF_result_Richesse_tot$RMSE,2)),
       x = "Real values", 
       y = "Predicted values") + 
  theme_classic() 

#all ________________________________________________________________________
all_graphe_clean100 = ggarrange(RF_AB_tot, RF_BM_tot, RF_Richesse_tot,
                            labels = c('(a)', '(b)','(c)'),ncol = 3,
                            common.legend = TRUE,
                            legend = 'right'
)
ggsave("Results/100_clean/all_graphe_clean100.png", plot = all_graphe_clean100, dpi = 300,height = 2.5,width = 8)

# bdd 100% double clean ----------------------------------------------------------
# bdd_clean: les lignes de bdd_all où l'ID n'est pas présent dans bdd_double, 0 doulons dans gps
bdd_all$ID <- as.character(bdd_all$ID)
bdd_double$ID <- as.character(bdd_double$ID)
bdd_clean <- bdd_all[!(bdd_all$ID %in% bdd_double$ID), ]
dim(bdd_clean) # 1825  348
dim (bdd_double_clean) #  430 348

bdd_all_clean = rbind(bdd_clean,bdd_double_clean) # la bdd clean y compris le nettoyage des doublons
dim(bdd_all_clean) # 2255  348


n_ligne= nrow(bdd_all_clean)
df_coord <- bdd_all_clean[, c("gps_x", "gps_y")] %>% mutate(gps_x = as.numeric(gps_x),gps_y = as.numeric(gps_y))

df_coord$num_ligne <- seq(nrow(df_coord))
carte <- leaflet(df_coord) %>%
  addTiles() %>%
  addCircleMarkers(lng = ~gps_x, lat = ~gps_y, radius = 0.8, fillOpacity = 0.8, fillColor = "blue")
carte


id_2=c("ID","Programme","Annee","ID_Site","Protocole")
vdt_col=c("AB_tot", "BM_tot", "Richesse_tot")
land_cover_col=c("clcm_lvl3")
topo_col=c("elevation","gps_x","gps_y")
soil_col=c("ph_0_a_30","sable.0_30","limon.0_30","argile.0_30","c_orga_0_a_30","P","N","K","CN","CEC","CaCO3")


climate_col=c()
for (i in 1:19){
  climate_col=c(climate_col, paste0("bio",i) )
}
climate_col=c(climate_col,"cmi_mean","gdd0","gdd10","hurs_mean","pet_penman_mean")

bdd_clean_mini= bdd_all_clean[,c(id_2,vdt_col,land_cover_col,topo_col,soil_col,climate_col)]
bdd_clean_mini = droplevels(bdd_clean_mini)
# str(bdd_clean_mini)
bdd_clean_mini$ID = as.factor(bdd_clean_mini$ID)
bdd_clean_mini$clcm_lvl3 = as.factor(bdd_clean_mini$clcm_lvl3)


# Renome
new_soil_col=c("pH","sand","silt","clay","C","P","N","K","CN","CEC","CaCO3")
bdd_clean_mini <- rename(bdd_clean_mini, !!setNames(soil_col, new_soil_col))

bdd_clean_mini <- bdd_clean_mini %>% 
  rename(PET = pet_penman_mean)
climate_col=c()
for (i in 1:19){
  climate_col=c(climate_col, paste0("bio",i) )
}
climate_col=c(climate_col,"cmi_mean","gdd0","gdd10","hurs_mean","PET")
col_graph=c(vdt_col,land_cover_col,topo_col,new_soil_col,climate_col)


summary(bdd_clean_mini$clcm_lvl3)
levels(bdd_clean_mini$clcm_lvl3)[levels(bdd_clean_mini$clcm_lvl3) == "Broad-leaved forest"] <- "forest"
levels(bdd_clean_mini$clcm_lvl3)[levels(bdd_clean_mini$clcm_lvl3) == "Coniferous forest"] <- "forest"
levels(bdd_clean_mini$clcm_lvl3)[levels(bdd_clean_mini$clcm_lvl3) == "Mixed forest"] <- "forest"

cl_original <- levels(bdd_clean_mini$clcm_lvl3)
new_cl <- c("f","gua", "ng", "nial", "p", "v")
bdd_clean_mini$clcm_lvl3 <- factor(bdd_clean_mini$clcm_lvl3, levels = cl_original, labels = new_cl)



# distribution
## AB_tot
shapiro.test(bdd_clean_mini$AB_tot)
ggplot(bdd_clean_mini, aes(x=AB_tot)) +
  geom_histogram(aes(y=..density..), fill="#69b3a2", color="#e9ecef", bins=30, alpha=2) +
  geom_density(fill="black", alpha=0.2) +
  theme_gray() +
  labs(title="Abundance", x="Value", y="Density") +
  theme(plot.title = element_text(hjust = 0.5))

x = as.numeric ( bdd_clean_mini$AB_tot)
bestNormalize(x)
orderNorm_obj <- orderNorm(x)
orderNorm_obj
p <- predict(orderNorm_obj)
x2 <- predict(orderNorm_obj, newdata = p, inverse = TRUE)
all.equal(x2, x)

bdd_clean_mini$AB_tot_2=p
shapiro.test(bdd_clean_mini$AB_tot_2)
ggplot(bdd_clean_mini, aes(x=AB_tot_2)) +
  geom_histogram(aes(y=..density..), fill="#69b3a2", color="#e9ecef", bins=30, alpha=2) +
  geom_density(fill="black", alpha=0.2) +
  theme_gray() +
  labs(title="Abundance", x="Value", y="Density") +
  theme(plot.title = element_text(hjust = 0.5))


## BM_tot
shapiro.test(bdd_clean_mini$BM_tot)
ggplot(bdd_clean_mini, aes(x=BM_tot)) +
  geom_histogram(aes(y=..density..), fill="#69b3a2", color="#e9ecef", bins=30, alpha=2) +
  geom_density(fill="black", alpha=0.2) +
  theme_gray() +
  labs(title="Abundance", x="Value", y="Density") +
  theme(plot.title = element_text(hjust = 0.5))

x = as.numeric ( bdd_clean_mini$BM_tot)
bestNormalize(x)
orderNorm_obj <- orderNorm(x)
orderNorm_obj
p <- predict(orderNorm_obj)
x2 <- predict(orderNorm_obj, newdata = p, inverse = TRUE)
all.equal(x2, x)

bdd_clean_mini$BM_tot_2=p
shapiro.test(bdd_clean_mini$BM_tot_2)
ggplot(bdd_clean_mini, aes(x=BM_tot_2)) +
  geom_histogram(aes(y=..density..), fill="#69b3a2", color="#e9ecef", bins=30, alpha=2) +
  geom_density(fill="black", alpha=0.2) +
  theme_gray() +
  labs(title="Abundance", x="Value", y="Density") +
  theme(plot.title = element_text(hjust = 0.5))




## Richesse_tot
shapiro.test(bdd_clean_mini$Richesse_tot)
ggplot(bdd_clean_mini, aes(x=Richesse_tot)) +
  geom_histogram(aes(y=..density..), fill="#69b3a2", color="#e9ecef", bins=30, alpha=2) +
  geom_density(fill="black", alpha=0.2) +
  theme_gray() +
  labs(title="Abundance", x="Value", y="Density") +
  theme(plot.title = element_text(hjust = 0.5))

x = as.numeric ( bdd_clean_mini$Richesse_tot)
bestNormalize(x)
orderNorm_obj <- orderNorm(x)
orderNorm_obj
p <- predict(orderNorm_obj)
x2 <- predict(orderNorm_obj, newdata = p, inverse = TRUE)
all.equal(x2, x)

bdd_clean_mini$Richesse_tot_2=p
shapiro.test(bdd_clean_mini$Richesse_tot_2)
ggplot(bdd_clean_mini, aes(x=Richesse_tot_2)) +
  geom_histogram(aes(y=..density..), fill="#69b3a2", color="#e9ecef", bins=30, alpha=2) +
  geom_density(fill="black", alpha=0.2) +
  theme_gray() +
  labs(title="Abundance", x="Value", y="Density") +
  theme(plot.title = element_text(hjust = 0.5))





bdd_clean_mini$AB_tot_sqrt = sqrt(bdd_clean_mini$AB_tot)
bdd_clean_mini$BM_tot_sqrt = sqrt(bdd_clean_mini$BM_tot)
# bdd_clean_mini$Richesse_tot = sqrt(bdd_clean_mini$Richesse_tot)
# for(i in names(bdd_clean_mini)){ cat (i,"," )}
predicteurs = c('elevation' ,'gps_x' ,'gps_y' ,'pH' ,'sand' ,'silt' ,'clay' ,'C' ,'P' ,'N' ,'K' ,
                'CN' ,'CEC' ,'CaCO3' ,'bio1' ,'bio2' ,'bio3' ,'bio4' ,'bio5' ,'bio6' ,'bio7' ,'bio8'
                ,'bio9' ,'bio10' ,'bio11' ,'bio12' ,'bio13' ,'bio14' ,'bio15' ,'bio16' ,'bio17' ,
                'bio18' ,'bio19' ,'cmi_mean' ,'gdd0' ,'gdd10' ,'hurs_mean' ,'PET')
bdd_clean_mini[,predicteurs] = scale(bdd_clean_mini[,predicteurs])
summary(bdd_clean_mini)
# names(bdd_clean_mini)



# AB_tot _______________________________________________________________________
df_explo_AB_tot = bdd_clean_mini[,c("AB_tot_sqrt","clcm_lvl3",predicteurs )]
df_explo_AB_tot = drop_na(df_explo_AB_tot)
# summary(df_explo_AB_tot)

set.seed(1234)  # Pour rendre les résultats reproductibles
index <- createDataPartition(df_explo_AB_tot$clcm_lvl3, p = 0.8, list = FALSE)

# Séparer les données en ensembles d'entraînement et de test
df_train_AB_tot <- df_explo_AB_tot[index, ]  # Données d'entraînement
df_test_AB_tot <- df_explo_AB_tot[-index, ]  # Données de test
df_train_AB_tot = droplevels(df_train_AB_tot)
df_test_AB_tot = droplevels(df_test_AB_tot)


all_var_AB_tot=ForetAlea(var_rep ="AB_tot_sqrt", df_app=df_train_AB_tot, df_valid = df_test_AB_tot,
                         mtry =9,ntree= 2000,maxnodes = NULL)

all_var_AB_tot$RMSE
all_var_AB_tot$R_adj_train
all_var_AB_tot$R_adj_test
AB_tot_fit_rf = all_var_AB_tot$model
AB_tot_fit_rf #display fitted model
# which.min(AB_tot_fit_rf$mse) #find number of trees that produce lowest test MSE
# sqrt(AB_tot_fit_rf$mse[which.min(AB_tot_fit_rf$mse)]) #find RMSE of best model
# varImpPlot(AB_tot_fit_rf) #produce variable importance plot

# Obtention de l'importance des variables
importance_rf <- as.data.frame(importance(AB_tot_fit_rf))
importance_rf$nom=rownames(importance_rf)
importance_rf <- importance_rf[order(importance_rf$IncNodePurity,decreasing = TRUE), ]
best_20_AB_tot = c(importance_rf$nom[1:20])
var_a_sup_AB_tot= setdiff(y = best_20_AB_tot,x = importance_rf$nom)




best20_var_AB_tot=ForetAlea(var_rep ="AB_tot_sqrt", df_app=df_train_AB_tot[,c("AB_tot_sqrt",best_20_AB_tot)], 
                            df_valid = df_test_AB_tot [,c("AB_tot_sqrt",best_20_AB_tot)],mtry =9,ntree= 2000,maxnodes = NULL)

best20_var_AB_tot$RMSE # 5.4
best20_var_AB_tot$R_adj_train # 0.37
best20_var_AB_tot$R_adj_test # 0.36
best20_var_AB_tot$MAE
rf.model = best20_var_AB_tot$model
rf.model # 36.74, 36.73

AB_tot_RF_tuning = read.csv2("results_tuning/AB_tot_RF_tuning.csv")
AB_tot_RF_tuning = as.data.frame(AB_tot_RF_tuning)
AB_tot_RF_tuning = AB_tot_RF_tuning %>% arrange(mae)
AB_tot_best_param = AB_tot_RF_tuning[1,]
AB_tot_best_mtry = AB_tot_best_param$mtry
AB_tot_best_ntree = AB_tot_best_param$ntree
AB_tot_best_maxnodes = AB_tot_best_param$maxnode
RF_result_AB_tot = ForetAlea(var_rep ="AB_tot_sqrt", 
                             df_app=df_train_AB_tot[,c("AB_tot_sqrt",best_20_AB_tot)], 
                             df_valid = df_test_AB_tot[,c("AB_tot_sqrt",best_20_AB_tot)],
                             mtry = AB_tot_best_mtry,
                             ntree= AB_tot_best_ntree,
                             maxnodes = NULL)
RF_AB_tot_pred <- RF_result_AB_tot$predit^2
RF_df_AB_tot = data.frame(Observed=df_test_AB_tot[,1]^2,Predicted = RF_AB_tot_pred)
cor_RF_AB_tot <- cor(RF_df_AB_tot$Observed, RF_df_AB_tot$Predicted)
# graphique avec ggplot
RF_AB_tot <- ggplot(RF_df_AB_tot, aes(x = Observed, y = Predicted)) +
  geom_point() + # Ajout des points
  geom_smooth(method = "lm", se = TRUE, color = "red") + 
  labs(subtitle =paste0(" RF: R² adj (train) = ", round(RF_result_AB_tot$R_adj_train,2), 
                        "; \n R² adj (test) = ", round(RF_result_AB_tot$R_adj_test,2),
                        "; \n RMSE (test) = ",  round(RF_result_AB_tot$RMSE^2,2)),
       x = "Real values", 
       y = "Predicted values") + 
  theme_classic() 
RF_AB_tot



# BM_tot _______________________________________________________________________
df_explo_BM_tot = bdd_clean_mini[,c("BM_tot_sqrt","clcm_lvl3",predicteurs )]
df_explo_BM_tot = drop_na(df_explo_BM_tot)
# summary(df_explo_BM_tot)
dim(df_explo_BM_tot)


dummy_vars <- model.matrix(~ clcm_lvl3 - 1, data = df_explo_BM_tot)
df_explo_BM_tot <- cbind(df_explo_BM_tot, dummy_vars)
df_explo_BM_tot <- df_explo_BM_tot[, -which(names(df_explo_BM_tot) == "clcm_lvl3")]


set.seed(1234)  # Pour rendre les résultats reproductibles
index <- createDataPartition(df_explo_BM_tot$BM_tot_sqrt, p = 0.8, list = FALSE)

# Séparer les données en ensembles d'entraînement et de test
df_train_BM_tot <- df_explo_BM_tot[index, ]  # Données d'entraînement
df_test_BM_tot <- df_explo_BM_tot[-index, ]  # Données de test
df_train_BM_tot = droplevels(df_train_BM_tot)
df_test_BM_tot = droplevels(df_test_BM_tot)


all_var_BM_tot=ForetAlea(var_rep ="BM_tot_sqrt", df_app=df_train_BM_tot, df_valid = df_test_BM_tot,
                         mtry =9,ntree= 2000,maxnodes = NULL)

all_var_BM_tot$RMSE
all_var_BM_tot$R_adj_train
all_var_BM_tot$R_adj_test
BM_tot_fit_rf = all_var_BM_tot$model
BM_tot_fit_rf #display fitted model
# which.min(BM_tot_fit_rf$mse) #find number of trees that produce lowest test MSE
# sqrt(BM_tot_fit_rf$mse[which.min(BM_tot_fit_rf$mse)]) #find RMSE of best model
# varImpPlot(BM_tot_fit_rf) #produce variable importance plot

# Obtention de l'importance des variables
importance_rf <- as.data.frame(importance(BM_tot_fit_rf))
importance_rf$nom=rownames(importance_rf)
importance_rf <- importance_rf[order(importance_rf$IncNodePurity,decreasing = TRUE), ]
best_20_BM_tot = c(importance_rf$nom[1:20])
var_a_sup_BM_tot= setdiff(y = best_20_BM_tot,x = importance_rf$nom)




best20_var_BM_tot=ForetAlea(var_rep ="BM_tot_sqrt", df_app=df_train_BM_tot[,c("BM_tot_sqrt",best_20_BM_tot)], 
                            df_valid = df_test_BM_tot [,c("BM_tot_sqrt",best_20_BM_tot)],mtry =9,ntree= 2000,maxnodes = NULL)

best20_var_BM_tot$RMSE # 5.4
best20_var_BM_tot$R_adj_train # 0.37
best20_var_BM_tot$R_adj_test # 0.36
best20_var_BM_tot$MAE
rf.model = best20_var_BM_tot$model
rf.model # 36.74, 36.73

BM_tot_RF_tuning = read.csv2("results_tuning/BM_tot_RF_tuning.csv")
BM_tot_RF_tuning = as.data.frame(BM_tot_RF_tuning)
BM_tot_RF_tuning = BM_tot_RF_tuning %>% arrange(mae)
BM_tot_best_param = BM_tot_RF_tuning[1,]
BM_tot_best_mtry = BM_tot_best_param$mtry
BM_tot_best_ntree = BM_tot_best_param$ntree
BM_tot_best_maxnodes = BM_tot_best_param$maxnode
RF_result_BM_tot = ForetAlea(var_rep ="BM_tot_sqrt", 
                             df_app=df_train_BM_tot[,c("BM_tot_sqrt",best_20_BM_tot)], 
                             df_valid = df_test_BM_tot[,c("BM_tot_sqrt",best_20_BM_tot)],
                             mtry = BM_tot_best_mtry,
                             ntree= BM_tot_best_ntree,
                             maxnodes = NULL)
RF_BM_tot_pred <- RF_result_BM_tot$predit^2
RF_df_BM_tot = data.frame(Observed=df_test_BM_tot[,1]^2,Predicted = RF_BM_tot_pred)
cor_RF_BM_tot <- cor(RF_df_BM_tot$Observed, RF_df_BM_tot$Predicted)
# graphique avec ggplot
RF_BM_tot <- ggplot(RF_df_BM_tot, aes(x = Observed, y = Predicted)) +
  geom_point() + # Ajout des points
  geom_smooth(method = "lm", se = TRUE, color = "red") + 
  labs(subtitle =paste0(" RF: R² adj (train) = ", round(RF_result_BM_tot$R_adj_train,2), 
                        "; \n R² adj (test) = ", round(RF_result_BM_tot$R_adj_test,2),
                        "; \n RMSE (test) = ",  round(RF_result_BM_tot$RMSE^2,2)),
       x = "Real values", 
       y = "Predicted values") + 
  theme_classic() 

RF_BM_tot

# Richesse_tot _______________________________________________________________________
df_explo_Richesse_tot = bdd_clean_mini[,c("Richesse_tot","clcm_lvl3",predicteurs )]
df_explo_Richesse_tot = drop_na(df_explo_Richesse_tot)
# summary(df_explo_Richesse_tot)

set.seed(1234)  # Pour rendre les résultats reproductibles
index <- createDataPartition(df_explo_Richesse_tot$clcm_lvl3, p = 0.8, list = FALSE)

# Séparer les données en ensembles d'entraînement et de test
df_train_Richesse_tot <- df_explo_Richesse_tot[index, ]  # Données d'entraînement
df_test_Richesse_tot <- df_explo_Richesse_tot[-index, ]  # Données de test
df_train_Richesse_tot = droplevels(df_train_Richesse_tot)
df_test_Richesse_tot = droplevels(df_test_Richesse_tot)


all_var_Richesse_tot=ForetAlea(var_rep ="Richesse_tot", df_app=df_train_Richesse_tot, df_valid = df_test_Richesse_tot,
                               mtry =9,ntree= 2000,maxnodes = NULL)

all_var_Richesse_tot$RMSE
all_var_Richesse_tot$R_adj_train
all_var_Richesse_tot$R_adj_test
Richesse_tot_fit_rf = all_var_Richesse_tot$model
Richesse_tot_fit_rf #display fitted model
# which.min(Richesse_tot_fit_rf$mse) #find number of trees that produce lowest test MSE
# sqrt(Richesse_tot_fit_rf$mse[which.min(Richesse_tot_fit_rf$mse)]) #find RMSE of best model
# varImpPlot(Richesse_tot_fit_rf) #produce variable importance plot

# Obtention de l'importance des variables
importance_rf <- as.data.frame(importance(Richesse_tot_fit_rf))
importance_rf$nom=rownames(importance_rf)
importance_rf <- importance_rf[order(importance_rf$IncNodePurity,decreasing = TRUE), ]
best_20_Richesse_tot = c(importance_rf$nom[1:20])
var_a_sup_Richesse_tot= setdiff(y = best_20_Richesse_tot,x = importance_rf$nom)




best20_var_Richesse_tot=ForetAlea(var_rep ="Richesse_tot", df_app=df_train_Richesse_tot[,c("Richesse_tot",best_20_Richesse_tot)], 
                                  df_valid = df_test_Richesse_tot [,c("Richesse_tot",best_20_Richesse_tot)],mtry =9,ntree= 2000,maxnodes = NULL)

best20_var_Richesse_tot$RMSE # 5.4
best20_var_Richesse_tot$R_adj_train # 0.37
best20_var_Richesse_tot$R_adj_test # 0.36
best20_var_Richesse_tot$MAE
rf.model = best20_var_Richesse_tot$model
rf.model # 36.74, 36.73

Richesse_tot_RF_tuning = read.csv2("results_tuning/Richesse_tot_RF_tuning.csv")
Richesse_tot_RF_tuning = as.data.frame(Richesse_tot_RF_tuning)
Richesse_tot_RF_tuning = Richesse_tot_RF_tuning %>% arrange(mae)
Richesse_tot_best_param = Richesse_tot_RF_tuning[1,]
Richesse_tot_best_mtry = Richesse_tot_best_param$mtry
Richesse_tot_best_ntree = Richesse_tot_best_param$ntree
Richesse_tot_best_maxnodes = Richesse_tot_best_param$maxnode
RF_result_Richesse_tot = ForetAlea(var_rep ="Richesse_tot", 
                                   df_app=df_train_Richesse_tot[,c("Richesse_tot",best_20_Richesse_tot)], 
                                   df_valid = df_test_Richesse_tot[,c("Richesse_tot",best_20_Richesse_tot)],
                                   mtry = Richesse_tot_best_mtry,
                                   ntree= Richesse_tot_best_ntree,
                                   maxnodes = NULL)
RF_Richesse_tot_pred <- RF_result_Richesse_tot$predit
RF_df_Richesse_tot = data.frame(Observed=df_test_Richesse_tot[,1],Predicted = RF_Richesse_tot_pred)
cor_RF_Richesse_tot <- cor(RF_df_Richesse_tot$Observed, RF_df_Richesse_tot$Predicted)
# graphique avec ggplot
RF_Richesse_tot <- ggplot(RF_df_Richesse_tot, aes(x = Observed, y = Predicted)) +
  geom_point() + # Ajout des points
  geom_smooth(method = "lm", se = TRUE, color = "red") + 
  labs(subtitle =paste0(" RF: R² adj (train) = ", round(RF_result_Richesse_tot$R_adj_train,2), 
                        "; \n R² adj (test) = ", round(RF_result_Richesse_tot$R_adj_test,2),
                        "; \n RMSE (test) = ",  round(RF_result_Richesse_tot$RMSE,2)),
       x = "Real values", 
       y = "Predicted values") + 
  theme_classic() 
RF_Richesse_tot

#all ________________________________________________________________________
all_graphe_clean = ggarrange(RF_AB_tot, RF_BM_tot, RF_Richesse_tot,
                                labels = c('(a)', '(b)','(c)'),ncol = 3,
                                common.legend = TRUE,
                                legend = 'right'
)
all_graphe_clean
ggsave("Results/100_clean/all_graphe_clean.png", plot = all_graphe_clean, dpi = 300,height = 2.5,width = 8)
