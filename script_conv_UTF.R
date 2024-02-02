


chemin_fichier <- "sdms.Rmd"

contenu_fichier <- readLines(chemin_fichier, encoding = "UTF-8")

contenu_correct <- iconv(contenu_fichier, from = "ISO-8859-1", to = "latin1", sub = "byte")

cat(contenu_correct, sep = "\n")

writeLines(contenu_correct, chemin_fichier, useBytes = TRUE)
