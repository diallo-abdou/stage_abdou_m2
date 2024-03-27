
library(dplyr)


bdd_n_rep = read.csv2("datas/Non_repeated_data/bdd_n_rep.csv")
bdd_n_rep$ID_2 = paste0(bdd_n_rep$Programme,bdd_n_rep$ID_Site,bdd_n_rep$Annee,bdd_n_rep$Modalite,collapse = "_")
names(bdd_n_rep)
id_col = c("ID_2","Bloc","Code_Parcelle","ID")
id_col2 = c("ID_2","Bloc","Code_Parcelle","gps_x","gps_y","clcm_lvl2","clcm_lvl3","AB_tot","BM_tot","Richesse_tot")
bdd_n_rep = conv_col(bdd_n_rep, id_col, "factor")
nrow(bdd_n_rep) # 3310


# Identification des doublons ID
dupld_ID <- subset(bdd_n_rep, duplicated(bdd_n_rep[, c("ID")]))
dupld_ID$ID
bdd_n_rep[which(bdd_n_rep$ID==dupld_ID$ID),]
which(bdd_n_rep$ID==dupld_ID$ID)[1]
diff_lignes <- bdd_n_rep[ which(bdd_n_rep$ID==dupld_ID$ID)[1], ] == bdd_n_rep[which(bdd_n_rep$ID==dupld_ID$ID)[2] , ]
sum(diff_lignes,na.rm=TRUE)
ncol(bdd_n_rep)
bdd_n_rep = bdd_n_rep [- which(bdd_n_rep$ID==dupld_ID$ID)[2],] # les deux lignes sont les mêmes, suppression





# Doublons des ID_Site dans le meme Programme
lignes_dupliquees <- duplicated(bdd_n_rep[c("Programme", "ID_Site")]) | 
                     duplicated(bdd_n_rep[c("Programme", "ID_Site")], fromLast = TRUE)
lignes_dupliquees <- bdd_n_rep[lignes_dupliquees, id_col]
lignes_dupliquees = droplevels(lignes_dupliquees)
nrow(lignes_dupliquees) # 1485
unique(lignes_dupliquees$Programme) # 19 programmes





# Pour AF  -------------------------------------------------------------
# Regle de selection, meme ID_Site mais Modalite different ......
AF = lignes_dupliquees[lignes_dupliquees$Programme=="AF",]
bdd_n_rep[bdd_n_rep$Programme=="AF",]
AF = droplevels(AF)
unique(AF$ID_Site)
AF$Annee <- as.numeric(as.character(AF$Annee))
# bla bla bla
AF_select = data.frame(Programme = rep("AF"), ID =derniere_annee$ID )
write.csv2(x =AF_select,file = "datas/Non_repeated_data/select_id/AF_select.csv", row.names = FALSE)



# Pour Beaujolais-SICAREX  -------------------------------------------------------------
# Regle de selection, meme ID_Site mais Modalite different ......
Beaujolais_S = lignes_dupliquees[lignes_dupliquees$Programme=="Beaujolais-SICAREX",]
bdd_n_rep[bdd_n_rep$Programme=="Beaujolais-SICAREX",id_col2]
Beaujolais_S = droplevels(Beaujolais_S)
unique(Beaujolais_S$ID_Site)
Beaujolais_S$Annee <- as.numeric(as.character(Beaujolais_S$Annee))
# bla bla bla
Beaujolais_S_select = data.frame(Programme = rep("Beaujolais-SICAREX"), ID =derniere_annee$ID )
write.csv2(x =Beaujolais_S_select,file = "datas/Non_repeated_data/select_id/Beaujolais_S_select.csv", row.names = FALSE)



# Identifier les lignes avec des ID_2 dupliqués
dupld_ID_2 <- bdd_n_rep[duplicated(bdd_n_rep$ID_2), ]

# Filtrer les lignes avec la colonne Blocs différente de NA
dupld_ID_2 <- dupld_ID_2[!is.na(dupld_ID_2$Blocs), ]

# Calculer la moyenne pour chaque ID_2
moyennes_ID_2 <- dupld_ID_2 %>%
  group_by(ID_2) %>%
  summarize(across(where(is.numeric), mean, na.rm = TRUE))

# Garder les levels pour les colonnes factor
facteurs <- select(dupld_ID_2, where(is.factor)) %>% distinct()

# Fusionner les moyennes avec les levels pour les colonnes factor
resultat <- merge(moyennes_ID_2, facteurs, by = "ID_2")

# Afficher le résultat
print(resultat)



# Pour Dephy  -------------------------------------------------------------
#pour chaque ID_Site, on prend la derniere annee
Dephy = lignes_dupliquees[lignes_dupliquees$Programme=="Dephy",]
Dephy = droplevels(Dephy)
unique(Dephy$ID_Site)
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


# Pour Dephy_bio  -------------------------------------------------------------
#pour chaque ID_Site, on prend la derniere annee
Dephy_bio = lignes_dupliquees[lignes_dupliquees$Programme=="Dephy Bio",]
Dephy_bio = droplevels(Dephy_bio)
unique(Dephy_bio$ID_Site)
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























 
 