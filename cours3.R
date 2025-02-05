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
    mutate(Date.de.naissance = dmy(Date.de.naissance)) |>
    slice(which.min(Date.de.naissance)) |>
    select(Nom.de.l.élu, Prénom.de.l.élu, Date.de.naissance)
}

sapply(list(df_Nantes, df_Faverelles, df_Loire_Atlantique, df_Gers), trouver_l_elu_le_plus_age)

# purr::map_df(list(df_Nantes, df_Faverelles, df_Loire_Atlantique, df_Gers),
  #           .f = trouver_l_elu_le_plus_age |>

# Question 6

calcul_distribution_age <- function(df) {
  validate_schema(df)  
  
  df |>
    mutate(Date.de.naissance = dmy(Date.de.naissance),
           age = as.integer(difftime(Sys.Date(), Date.de.naissance, units = "days") / 365.25)) |>
    summarise(
      quantile_0 = min(age, na.rm = TRUE),
      quantile_25 = quantile(age, 0.25, na.rm = TRUE),
      quantile_50 = quantile(age, 0.50, na.rm = TRUE),
      quantile_75 = quantile(age, 0.75, na.rm = TRUE),
      quantile_100 = max(age, na.rm = TRUE)
    )
}

sapply(list(df_Nantes, df_Faverelles, df_Loire_Atlantique, df_Gers), calcul_distribution_age)

# Question 7
library(ggplot2)

plot_code_professions <- function(df) {
  validate_schema(df)
  
  count_data <- df |>
    group_by(Code.de.la.catégorie.socio.professionnelle) |>
    summarise(nombre = n()) |>
    arrange(nombre)
  
  ggplot(count_data, aes(
    x = nombre,
    y = reorder(
      Code.de.la.catégorie.socio.professionnelle, nombre
    )
  )
  ) +
    geom_bar(stat = "identity", fill = "steelblue") +
    labs(title = "Nombre d'élus par code de catégorie socio-professionelle",
         x = "Nombre d'élus",
         y = "Code de la catégorie socio-professionnelle") +
    theme_minimal()
  
  return(count_data)
}

lapply(list(df_Nantes, df_Faverelles, df_Loire_Atlantique, df_Gers), plot_code_professions)

