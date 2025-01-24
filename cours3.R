# Question 2
data_exercice <- read.table("C:/Users/julie/Documents/semestre 2/R avancé et GitHub/cours_r_semaine_3/data/elus-conseillers-municipaux-cm.csv", header = TRUE, sep = ";", quote = "")

data <- as_tibble(data_exercice)

df_Nantes <- subset(data_exercice, Libellé.de.la.commune == "Nantes")
df_Faverelles <- subset(data_exercice, Libellé.de.la.commune == "Faverelles")
df_Loire_Atlantique <- subset(data_exercice, Libellé.du.département == "Loire-Atlantique")
df_Gers <- subset(data_exercice, Libellé.du.département == "Gers")

# Question 3

validate_schema <- function(df) {
  schema <- c("Code.du.département", "Libellé.du.département", "Code.de.la.collectivité.à.statut.particulier", 
              "Libellé.de.la.collectivité.à.statut.particulier", "Code.de.la.commune", 
              "Libellé.de.la.commune", "Nom.de.l.élu", "Prénom.de.l.élu", 
              "Code.sexe", "Date.de.naissance", "Code.de.la.catégorie.socio.professionnelle", 
              "Libellé.de.la.catégorie.socio.professionnelle", "Date.de.début.du.mandat", 
              "Libellé.de.la.fonction", "Date.de.début.de.la.fonction", "Code.nationalité"
  )
  stopifnot(identical(colnames(df), schema))
}

library(dplyr)
compter_nombre_d_elus <- function(df) {
  validate_schema(df)
  
  df |>
    select(Nom.de.l.élu, Prénom.de.l.élu, Date.de.naissance) |>
    distinct() |>
    nrow()
}

sapply(list(df_Nantes, df_Faverelles, df_Loire_Atlantique, df_Gers), compter_nombre_d_elus)

# Question 4
library(stringr)
compter_nombre_d_adjoints <- function(df) {
  validate_schema(df)
  
  sum(str_detect(df$Libellé.de.la.fonction, "adjoint"))
}

sapply(list(df_Nantes, df_Faverelles, df_Loire_Atlantique, df_Gers), compter_nombre_d_adjoints)

# Question 5 (avec dplyr)
library(lubridate)
trouver_l_elu_le_plus_age <- function(df) {
  validate_schema(df)
  df |>
    mutate(DAte.de.naissance = dmy(Date.de.naissance)) |>
    slice(which.min(Date.de.naissance)) |>
    select(Nom.de.l.élu, Prénom.de.l.élu, Date.de.naissance)
}

sapply(list(df_Nantes, df_Faverelles, df_Loire_Atlantique, df_Gers), trouver_l_elu_le_plus_age)

purr::map_df(list(df_Nantes, df_Faverelles, df_Loire_Atlantique, df_Gers),
             .f = trouver_l_elu_le_plus_age |>