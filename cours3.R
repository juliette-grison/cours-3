# Question 2
data_exercice <- read_delim("data/elus-conseillers-municipaux-cm.csv", delim = ";", escape_double = FALSE, trim_ws = TRUE)
data_exercice <- read.table("C:/Users/julie/Documents/semestre 2/R avancé et GitHub/cours_r_semaine_3/data/elus-conseillers-municipaux-cm.csv", header = TRUE, sep = ";", quote = "")

df_Nantes <- subset(data_exercice, Libellé.de.la.commune == "Nantes")
df_Faverelles <- subset(data_exercice, Libellé.de.la.commune == "Faverelles")
df_Loire_Atlantique <- subset(data_exercice, Libellé.du.département == "Loire-Atlantique")
df_Gers <- subset(data_exercice, Libellé.du.département == "Gers")
# 