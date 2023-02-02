## Le code ci-dessous sert dans le cadre du cours LCHM1371.

# Le package tidyverse facilite le traitement des données dans R. Si il n'est pas installé, il
# peut être installé en tapant 'install.packages('tidyverse')' dans la console.
library(tidyverse)

# Défini le dossier de travail
setwd("Z:/Encadrement/Assistanat/Q2/LCHM1371/LCHM1371")

# On a besoin d'un df avec les pourcentage d'inhibition et la concentration en SOD ici je
# l'importe d'un fichier csv avec des données d'exemples

sod.df <- read.csv("sample_data.csv")

# On peut vérifier que les données ont du sens, on s'attend à tendre vers 100 quand [SOD] augmente

plot(sod.df$csod, sod.df$inhib)

# Pour la suite je vais transformer les pourcentage d'inhibition en fraction d'inhbition

sod.df <- sod.df %>%
  mutate(inhib = inhib/100)

# On suppose que les données sont expliquée par une équation de type inhb = 1 - v0/(V0+[SOD]) et
# on utilise nls() pour trouver VO NLS a besoin d'une valeur initiale. On peut estimer ça car
# quand V0 = [SOD] inhib = 0.5. Donc ici V0 a l'air d'etre autour de 1
sod.nls <- nls(formula = inhib ~ 1 - V0/(V0 + csod), data = sod.df, start = list(V0 = 1))

# Summary() permet d'accéder à la valeur de V0.
summary(sod.nls)

# On peut rajouter une ligne sur le graphe pour voir si le modèle colle
V0 <- coef(sod.nls)
x <- c(0:20)
y <- 1 - V0/(V0 + x)
lines(x, y, lty = "dotted", col = "red")

# Une fois V0 trouvé, on peux l'utiliser pour calculer la concentration en unité de nos
# échantillons en connaissant le %age d'inhibition