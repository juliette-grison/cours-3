# Question 2
data_exercice <- read.table("C:/Users/julie/Documents/semestre 2/R avancé et GitHub/cours_r_semaine_3/data/elus-conseillers-municipaux-cm.csv", header = TRUE, sep = ";", quote = "")

df_Nantes <- subset(data_exercice, Libellé.de.la.commune == "Nantes")
df_Faverelles <- subset(data_exercice, Libellé.de.la.commune == "Faverelles")
df_Loire_Atlantique <- subset(data_exercice, Libellé.du.département == "Loire-Atlantique")
df_Gers <- subset(data_exercice, Libellé.du.département == "Gers")

# Question 3
library(dplyr)
compter_nombre_d_elus <- function(df) {
  schema <- c("Code.du.département", "Libellé.du.département", "Code.de.la.collectivité.à.statut.particulier", 
              "Libellé.de.la.collectivité.à.statut.particulier", "Code.de.la.commune", 
              "Libellé.de.la.commune", "Nom.de.l.élu", "Prénom.de.l.élu", 
              "Code.sexe", "Date.de.naissance", "Code.de.la.catégorie.socio.professionnelle", 
              "Libellé.de.la.catégorie.socio.professionnelle", "Date.de.début.du.mandat", 
              "Libellé.de.la.fonction", "Date.de.début.de.la.fonction", "Code.nationalité" # trouvé avec dput(colnames(data_exercice))
  )
  stopifnot(identical(colnames(df), schema))
  
  df |>
    select(Nom.de.l.élu, Prénom.de.l.élu, Date.de.naissance) |>
    distinct() |>
    nrow()
}

sapply(list(df_Nantes, df_Faverelles, df_Loire_Atlantique, df_Gers), compter_nombre_d_elus)

compter_nombre_d_elus(df_Nantes |>
                        select(-Code.du.département)
                      )
