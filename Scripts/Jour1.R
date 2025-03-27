# Mise en mémoire des packages utiles
library(tidyverse)
library(palmerpenguins)

# Les vecteurs : collection d'éléments du même type ----

# Un vecteur numérique
c(3, 5, 2, 5, 6, 2, 1)

# Un vecteur de chaînes de caractères
c("rouge", "vert", "bleu", "rouge")

# Un vecteur "logique" (vrais/faux)
c(TRUE, TRUE, FALSE, TRUE, F, F, T, T)

# Si on mélange les types...
c(2, 4, 5, "rouge")
c(TRUE, FALSE, 1, 0, 0, 1)

# Création d'un vecteur de tailles
taille <- c(165, 162, 184, 191, 174, 174, 188, 157)

# Pour afficher le contenu d'un objet, on tape son nom :
taille

# Opérations sur les vecteurs
mean(taille)  # Calcul de la moyenne
var(taille)   # Calcul de la variance

# Recyclage
taille * 2

a <- c(3, 5, 2, 4)
b <- c(10, 100)
d <- c(25, 50, 75)

a + a  # Ajout terme à terme
a + b  # Recyclage des valeurs de b
a + d  # Recyclage partiel des valeurs de d : un message d'avertissement est affiché

# Autres fonctions pour créer des vecteurs ----

# L'opérateur ":" pour créer des suites régulières
1:10
100:88
-4:3
0.3:10.3

# La fonction "seq()" pour créer des séquences régulières
seq(from = 8, to = 54, by = 5)
seq(from = 54, to = 8, by = -5)  # Le pas doit être négatif pour les séquences décroissantes

seq(from = 8, to = 54, length.out = 16) # Pour imposer un nombre de valeurs dans la séquence

# La fonction "rep()" pour répéter des séquences
rep(b, 3)
rep(a, each = 4)
rep(c("bleu", "rouge", "gris"), each = 4)

vec_repete <- rep(a, 4)
vec_repete

rep(c("bleu", "rouge", "gris"), each = 2, times = 2)

# Les facteurs ----
# Pour stocker des variables catégorielles

couleurs <- c("rouge", "vert", "bleu", "rouge")
fac_couleur <- factor(couleurs, levels = c("vert", "rouge", "bleu"))
fac_couleur

summary(fac_couleur)

# Les tableaux de données ----
# Création d'un data.frame
df_ex <- data.frame(esp = c("Adélie", "Adélie", "Gentoo", "Adélie"),
                    sex = c("M", "M", "F", "F"),
                    mass = c(65, 43, 38, 42))

df_ex

# Création d'un tibble
tbl_ex <- tibble(esp = factor(c("Adélie", "Adélie", "Gentoo", "Adélie")),
                 sex = factor(c("M", "M", "F", "F")),
                 mass = c(65, 43, 38, 42))

tbl_ex

summary(tbl_ex)

# Quelques fonctions utiles ----
ls() # pour lister les objets de l'environnement de travail
rm(a) # pour supprimer des objets de l'environnement de travail
head(taille, 4) # pour afficher les premiers éléments d'un objet
tail(taille, 4) # pour afficher les derniers éléments d'un objet
names(tbl_ex)   # Pour afficher les noms de colonnes
ncol(tbl_ex)    # Pour afficher le nombre de colonnes d'un tableau
nrow(tbl_ex)    # Pour afficher le nombre de lignes d'un tableau
dim(tbl_ex)     # Pour afficher le nombre de lignes et de colonnes d'un tableau

# Indexation par position ----
taille
taille[4]
taille[c(4,5,3)]
taille[1:3]
taille[-1]
taille[-(3:5)]

tbl_ex[c(1,3) , ]
tbl_ex[, -2]
tbl_ex[, c(1,3)]
tbl_ex[1:2, c(1,3)]

# Modification d'une valeur spécifique dans une table
tbl_ex[2,1] <- "Gentoo"
tbl_ex

# Indexation par condition ----
tbl_ex[tbl_ex$esp == "Gentoo" & tbl_ex$sex == "M",]

# Commande équivalente avec les syntaxes du tidyverse
tbl_ex |> 
  filter(esp == "Gentoo", 
         sex == "M")

# Quelques opérateurs de comparaison
3 == 4
3 != 4
3 <= 4

# Visualisation du tibble "penguins" et résumé des variables
penguins
View(penguins)
summary(penguins)

# 4 façons de faire la même chose
penguins[penguins$species == "Chinstrap" | penguins$species == "Gentoo", ]
penguins[penguins$species %in% c("Chinstrap", "Gentoo"), ]

penguins |> 
  filter(species %in% c("Chinstrap", "Gentoo"))

peng2 <- penguins |> 
  filter(species != "Adelie")

# Importation de fichiers ----
# Avec la fonction read.table()
dauph <- read.table("Data/dauphin.csv", header = TRUE, 
                    sep = ";", dec = ",")

dauph

# Avec l'assistant d'importation de RStudio
library(readr)  # Commande inutile si le tidyverse est chargé par ailleurs
dauphin <- read_delim("Data/dauphin.csv", 
                      delim = ";", escape_double = FALSE, 
                      locale = locale(decimal_mark = ","), 
                      trim_ws = TRUE)
dauphin