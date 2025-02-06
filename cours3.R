# Question 2
data_exercice <- read.table("C:/Users/julie/Documents/semestre 2/R avancé et GitHub/cours_r_semaine_3/data/elus-conseillers-municipaux-cm.csv", header = TRUE, sep = ";", quote = "")

library(dplyr)

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
    mutate(
      Date.de.naissance = dmy(Date.de.naissance),
      Âge = as.integer(interval(Date.de.naissance, today()) / years(1))
      ) |>
    slice(which.max(Âge)) |>
    select(Nom.de.l.élu, Prénom.de.l.élu, Âge)
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
  
}

lapply(list(df_Nantes, df_Faverelles, df_Loire_Atlantique, df_Gers), plot_code_professions)

# Question 8
## Attribution de la classe "commune" aux 2 df concernés
class(df_Nantes) <- c("commune", class(df_Nantes))
class(df_Faverelles) <- c("commune", class(df_Faverelles))

## Création de la fonxtion générique
summary.commune <- function(x, ...) {

    # Vérifier que x est bien un data.frame
  if (!is.data.frame(x)) {
    stop("L'objet fourni n'est pas un data.frame.")
  }
  
  # Vérifier que la colonne 'Commune' existe
  if (!"Libellé.de.la.commune" %in% names(x)) {
    stop("La colonne 'Libellé.de.la.commune' est absente du dataframe.")
  }
  
  # Vérifier qu'il s'agit d'une seule commune
  commune_unique <- unique(x$Libellé.de.la.commune)
  if (length(commune_unique) != 1) {
    stop("Le dataframe contient plusieurs communes. Fournissez un dataframe ne contenant qu'une seule commune.")
  }
  
  # Extraire le nom de la commune
  nom_commune <- commune_unique
  
  # Utiliser les fonctions existantes
  nombre_elus <- compter_nombre_d_elus(x)
  distribution_ages <- calcul_distribution_age(x)
  elu_plus_age <- trouver_l_elu_le_plus_age(x)
  
  # Affichage des résultats
  cat("\nNom de la commune :", nom_commune, "\n")
  cat("Nombre d'élu.e.s :", nombre_elus, "\n")
  cat("Distribution des âges des élu.e.s :\n")
  print(distribution_ages)
  cat("\nL'élu.e le/la plus âgé.e :\n")
  print(elu_plus_age)
}

## Test sur les 2 df concernés
summary(df_Nantes)
summary(df_Faverelles)

# Question 9
class(df_Loire_Atlantique) <- c("departement", class(df_Loire_Atlantique))
class(df_Gers) <- c("departement", class(df_Gers))

summary.departement <- function(x) {
  validate_schema(x)  # Vérifie que le schéma du dataframe est correct
  
  # Vérifier que x est bien un data.frame
  if (!is.data.frame(x)) {
    stop("L'objet fourni n'est pas un data.frame.")
  }
  
  # Vérifier que la colonne 'Département' existe
  if (!"Libellé.du.département" %in% names(x)) {
    stop("La colonne 'Libellé.du.département' est absente du dataframe.")
  }
  
  # Extraire le nom du département
  nom_departement <- unique(x$Libellé.du.département)
  if (length(nom_departement) != 1) {
    stop("Le dataframe contient plusieurs départements. Fournissez un dataframe ne contenant qu'un seul département.")
  }
  
  # Nombre de communes dans le département
  nombre_communes <- length(unique(x$Libellé.de.la.commune))
  
  # Nombre total d'élus dans le département
  nombre_elus <- compter_nombre_d_elus(x)
  
  # Distribution des âges des élus du département
  distribution_ages <- calcul_distribution_age(x)
  
  # Trouver l'élu.e le/la plus âgé.e et sa commune
  elu_plus_age <- x |> 
    group_by(Libellé.de.la.commune) |> 
    slice(which.min(dmy(Date.de.naissance))) |> 
    ungroup() |> 
    slice(which.min(dmy(Date.de.naissance))) |> 
    select(Nom.de.l.élu, Prénom.de.l.élu, Libellé.de.la.commune, Date.de.naissance) |> 
    mutate(Âge = as.integer(difftime(Sys.Date(), dmy(Date.de.naissance), units = "days") / 365.25)) |> 
    select(-Date.de.naissance)
  
  # Trouver l'élu.e le/la plus jeune et sa commune
  elu_plus_jeune <- x |> 
    group_by(Libellé.de.la.commune) |> 
    slice(which.max(dmy(Date.de.naissance))) |> 
    ungroup() |> 
    slice(which.max(dmy(Date.de.naissance))) |> 
    select(Nom.de.l.élu, Prénom.de.l.élu, Libellé.de.la.commune, Date.de.naissance) |> 
    mutate(Âge = as.integer(difftime(Sys.Date(), dmy(Date.de.naissance), units = "days") / 365.25)) |> 
    select(-Date.de.naissance)
  
  # Calculer la moyenne d'âge par commune
  moyenne_age_par_commune <- x |> 
    group_by(Libellé.de.la.commune) |> 
    summarise(Age_Moyen = mean(as.integer(difftime(Sys.Date(), dmy(Date.de.naissance), units = "days") / 365.25))) |> 
    ungroup()
  
  # Trouver la commune avec la moyenne d'âge la plus faible
  commune_age_min <- moyenne_age_par_commune |> slice(which.min(Age_Moyen)) |> pull(Libellé.de.la.commune)
  distribution_age_min <- calcul_distribution_age(x |> filter(Libellé.de.la.commune == commune_age_min))
  
  # Trouver la commune avec la moyenne d'âge la plus élevée
  commune_age_max <- moyenne_age_par_commune |> slice(which.max(Age_Moyen)) |> pull(Libellé.de.la.commune)
  distribution_age_max <- calcul_distribution_age(x |> filter(Libellé.de.la.commune == commune_age_max))
  
  # Affichage des résultats
  cat("\nNom du département :", nom_departement, "\n")
  cat("Nombre de communes :", nombre_communes, "\n")
  cat("Nombre d'élu.e.s :", nombre_elus, "\n")
  cat("Distribution des âges des élu.e.s du département :\n")
  print(distribution_ages)
  
  cat("\nL'élu.e le/la plus âgé.e :\n")
  print(elu_plus_age)
  
  cat("\nL'élu.e le/la plus jeune :\n")
  print(elu_plus_jeune)
  
  cat("\nCommune à la moyenne d'âge la plus faible :", commune_age_min, "\n")
  cat("Distribution des âges des élu.e.s dans cette commune :\n")
  print(distribution_age_min)
  
  cat("\nCommune à la moyenne d'âge la plus élevée :", commune_age_max, "\n")
  cat("Distribution des âges des élu.e.s dans cette commune :\n")
  print(distribution_age_max)
}


summary(df_Gers)
summary(df_Loire_Atlantique)
