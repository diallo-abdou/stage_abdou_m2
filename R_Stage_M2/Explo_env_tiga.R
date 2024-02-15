

str(donnees_physico_long)
colnames(donnees_physico_long)
ncol(donnees_physico_long)
unique(donnees_physico_long$analyse_desc)
unique(donnees_physico_long$parametre)
unique(donnees_physico_long$code_ech_client)
calculer_pourcentage_NA(df = donnees_physico_long,afficher_zero_percent = FALSE)
summary(donnees_physico_long)  


library(naniar)
gg_miss_var(donnees_physico_long) 




# Récupération des noms des dataframes dans l'environnement actuel
noms_dataframes <- ls(pattern = "^[^\\.]")

# Affichage des noms des dataframes
print(noms_dataframes)

noms_dataframes[-1]




###
str(liste_fichiers_LAS)
print(liste_fichiers_LAS)


###
str(coord_GPS_date_prelevement_commune_usage)
summary(coord_GPS_date_prelevement_commune_usage)

coord_GPS_date_prelevement_commune_usage$usage1=as.factor(coord_GPS_date_prelevement_commune_usage$usage1)
table(coord_GPS_date_prelevement_commune_usage[,c("usage1","annee")])


length(coord_GPS_date_prelevement_commune_usage$X_L93)
length(coord_GPS_date_prelevement_commune_usage$Y_L93)
length(unique(coord_GPS_date_prelevement_commune_usage$X_L93))
length(unique(coord_GPS_date_prelevement_commune_usage$Y_L93))

length(coord_GPS_date_prelevement_commune_usage$code_ech)
length(unique(coord_GPS_date_prelevement_commune_usage$code_ech))


coord_GPS_date_prelevement_commune_usage$date_du_prelevement
coord_GPS_date_prelevement_commune_usage$annee=format(date, "%Y")

head(coord_GPS_date_prelevement_commune_usage)
date=as.Date(coord_GPS_date_prelevement_commune_usage$date_du_prelevement, format = "%d/%m/%Y")
summary(as.factor(format(date, "%Y")))



###
str(donnees_ETM_pesticices_Aurea)
names(donnees_ETM_pesticices_Aurea)
summary(donnees_ETM_pesticices_Aurea)
calculer_pourcentage_NA(donnees_ETM_pesticices_Aurea,afficher_zero_percent = TRUE)

donnees_ETM_pesticices_Aurea$usage1=as.factor(donnees_ETM_pesticices_Aurea$usage1)
summary(donnees_ETM_pesticices_Aurea$usage1)


length(donnees_ETM_pesticices_Aurea$X_L93)
length(donnees_ETM_pesticices_Aurea$Y_L93)
length(unique(donnees_ETM_pesticices_Aurea$X_L93))
length(unique(donnees_ETM_pesticices_Aurea$Y_L93))

length(donnees_ETM_pesticices_Aurea$code_ech)
length(unique(donnees_ETM_pesticices_Aurea$code_ech))


donnees_ETM_pesticices_Aurea$date_du_prelevement
head(donnees_ETM_pesticices_Aurea)
date=as.Date(donnees_ETM_pesticices_Aurea$date_du_prelevement, format = "%d/%m/%Y")
summary(as.factor(format(date, "%Y")))




###
str(donnees_physico_LAS)
names(donnees_physico_LAS)
summary(donnees_physico_LAS)
calculer_pourcentage_NA(donnees_physico_LAS)
calculer_pourcentage_NA(donnees_physico_LAS,afficher_zero_percent = TRUE)

donnees_physico_LAS$usage1=as.factor(donnees_physico_LAS$usage1)
summary(donnees_physico_LAS$usage1)


length(donnees_physico_LAS$X_L93)
length(donnees_physico_LAS$Y_L93)
length(unique(donnees_physico_LAS$X_L93))
length(unique(donnees_physico_LAS$Y_L93))

length(donnees_physico_LAS$code_ech)
length(unique(donnees_physico_LAS$code_ech))


donnees_physico_LAS$date_du_prelevement
head(donnees_physico_LAS)
date=as.Date(donnees_physico_LAS$date_du_prelevement, format = "%d/%m/%Y")
summary(as.factor(format(date, "%Y")))




###
str(enquetes_GC_typologie_Jean)
names(enquetes_GC_typologie_Jean)
summary(enquetes_GC_typologie_Jean_conv)
calculer_pourcentage_NA(enquetes_GC_typologie_Jean)
calculer_pourcentage_NA(enquetes_GC_typologie_Jean,afficher_zero_percent = TRUE)

enquetes_GC_typologie_Jean_conv <- conv_col(enquetes_GC_typologie_Jean, 
                  names(enquetes_GC_typologie_Jean [,c(2:12)]),
                  "factor")


enquetes_GC_typologie_Jean$usage1=as.factor(enquetes_GC_typologie_Jean$usage1)
summary(enquetes_GC_typologie_Jean$usage1)


length(enquetes_GC_typologie_Jean$rotation_complexe_fam)
length(enquetes_GC_typologie_Jean$Y_L93)
length(unique(enquetes_GC_typologie_Jean$X_L93))
length(unique(enquetes_GC_typologie_Jean$Y_L93))

length(enquetes_GC_typologie_Jean$code_ech)
length(unique(enquetes_GC_typologie_Jean$code_ech))


enquetes_GC_typologie_Jean$date_du_prelevement
head(enquetes_GC_typologie_Jean)
date=as.Date(enquetes_GC_typologie_Jean$date_du_prelevement, format = "%d/%m/%Y")
summary(as.factor(format(date, "%Y")))

















