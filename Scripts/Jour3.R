# Mise en mémoire des packages utiles
library(tidyverse)        # Pour la manipulation des données et les graphiques
library(palmerpenguins)   # Pour le tableau penguins
library(scales)           # Pour l'aspect du texte sur les axes des graphiques
library(nycflights23)     # Pour le tableau flights
library(skimr)            # Pour faire des résumés de données avec skim()
library(rstatix)          # Pour les tests statistiques à la mode tidyverse

# Importation des données du fichier dauphin.csv
dauphin <- read_delim("Data/dauphin.csv", 
                      delim = ";", escape_double = FALSE, 
                      locale = locale(decimal_mark = ","), 
                      trim_ws = TRUE)
dauphin
penguins
flights

# filter() : pour sélectionner des lignes ----
# Ne retenir que les lignes des manchots femelles
filter(penguins, sex == "female")

# Équivalent à la notation "pipe"
penguins |> 
  filter(sex == "female")

# Attention aux nombres à virgules et aux arrondis
sqrt(2)^2 == 2
(1/49) * 49 == 1

penguins |> 
  filter(bill_depth_mm >= 19.99, bill_depth_mm <= 20.01)

penguins |> 
  filter(bill_depth_mm == 20)

# Le OU logique : "|"
penguins |> 
  filter(bill_length_mm > 40 | bill_depth_mm > 25)

# Suppression des toutes les lignes qui contiennent au moins un NA
penguins |> 
  na.omit()

# Suppression des lignes qui contiennent un NA *pour une variable donnée*
penguins |> 
  filter(!is.na(bill_length_mm))

penguins |> 
  filter(is.na(bill_length_mm))


# arrange() : pour trier des lignes ----
# Par défaut, tri par ordre croissant
penguins |> 
  arrange(bill_depth_mm)

# On peut trier par plusieurs variables pour résoudre les égalités éventuelles
penguins |> 
  arrange(bill_depth_mm, bill_length_mm)

# On peut aussi faire des tris pour des sous-groupes d'un jeu de données
# Ici, on trie par longueur de bec croissante, pour chaque espèce et sexe
penguins |> 
  group_by(species, sex) |> 
  arrange(bill_depth_mm, .by_group = TRUE) |> 
  View()

# Pour trier par ordre décroissant
penguins |> 
  arrange(desc(bill_depth_mm))

# select() : pour sélectionner des variables ----
penguins |> 
  select(bill_length_mm)

# On peut renommer des variables en même temps qu'on les sélectionne
penguins |> 
  select(bill_length = bill_length_mm,
         bill_depth = bill_depth_mm)

# La fonction rename() permet de renommer une ou des variables en conservant toutes
# les variables du tableau d'origine
penguins |> 
  rename(bill_length = bill_length_mm)

# La fonction relocate() permet de déplacer des colonnes
penguins |> 
  relocate(year, .after = island)

# L'opérateur ":" permet de sélectionner des colonnes contigües
penguins |> 
  select(bill_length_mm:body_mass_g)

# Le "-" permet de retirer une ou des colonnes
penguins |> 
  select(-sex)

penguins |> 
  select(-(bill_length_mm:body_mass_g))

# Plusieurs "helper functions" permettent de sélectionner les colonnes dont le
# nom contient, commence ou se termine par des chaînes de caractères spécifiques
penguins |> 
  select(contains("mm"))

penguins |> 
  select(ends_with("mm"))

penguins |> 
  select(starts_with("bill"), species, sex)

# Enfin, everything() permet de sélectionner toutes les colonnes d'un tableau
# qui ne l'ont pas encore été par une autre méthode
penguins |> 
  select(year, body_mass_g, everything())


# mutate() : pour créer de nouvelles variables ou modifier des variables existantes ----
# Pour transformer une variable en facteur
dauphin <- dauphin |> 
  mutate(Sexe = factor(Sexe, levels = c("m", "f")))

dauphin

# Ici, je transforme la variable "sexe" en facteur et je spécifie explicitement
# l'ordre des catégories souhaitées avant de faire un graphique
dauphin |> 
  mutate(Sexe = factor(Sexe, levels = c("m", "f"))) |> 
  ggplot(aes(x = Sexe, y = Cu)) +
  geom_boxplot()

# Les longueurs en cm sont transformées en mètres et la variable Organe est 
# transformée en facteur. Au préalable, on a supprimé la variable Id
dauphin |> 
  select(-Id) |> 
  mutate(Taille = Taille / 100,
         Organe = factor(Organe))

# Création d'une nouvelle variable composite "forme du bec" et visualisation
penguins |> 
  mutate(bill_shape = bill_length_mm / bill_depth_mm) |> 
  relocate(bill_shape) |> 
  ggplot(aes(x = species, y = bill_shape)) +
  geom_violin()

# Les opérateur division entière :
1987 %/% 100   # quotient
1987 %% 100    # reste

# Exemple d'utilisation : transformer des années en décennies
(1987 %/% 10) * 10
(1981 %/% 10) * 10
(1990 %/% 10) * 10

# Les fonction lag() et lead() permettent de décaler des séries de données,
# vers l'avant ou l'arrière, d'une ou plusieurs unités.
1:10
lag(1:10,n = 2)
lead(1:10)

# Cela permet par exemple de calculer des intervalles entre observations ou
# lignes successives d'un tableau
flights |> 
  relocate(time_hour) |> 
  mutate(diff = time_hour - lag(time_hour),
         jour = year(time_hour)) |> 
  relocate(diff, jour)

# Ici, on utilise cummean() pour calculer des moyennes cumulées pour les
# individus dont la hauteur du bec est connue
penguins |> 
  filter(!is.na(bill_depth_mm)) |> 
  mutate(moy_cumul = cummean(bill_depth_mm)) |> 
  relocate(moy_cumul)

# Création d'un graphique sur lequel les individus dont la longueur du bec est
# supérieure au 3e quartile de chaque espèce et chaque sexe apparaît en rouge
penguins |> 
  group_by(species, sex) |> 
  mutate(
    big_flipper = flipper_length_mm > quantile(flipper_length_mm, 0.75, 
                                               na.rm = TRUE),
    sex = fct_recode(sex, Femelle = "female", Mâle = "male")) |> 
  relocate(big_flipper) |> 
  filter(!is.na(sex)) |> 
  ggplot(aes(x = sex, y = bill_length_mm, color = big_flipper)) +
  geom_jitter(width = 0.2, alpha = 0.5) +
  facet_wrap(~species) +
  theme_bw() +
  labs(color = "Nageoires", x = "", y = "Longueur du bec (mm)") +
  scale_color_manual(values = c("grey60", "firebrick3"),
                     labels = c("petites", "grandes")) +
  theme(legend.position = "top")

# summarise() : pour résumer des données ----

# Calcul de la moyenne, de la variance et de l'effectif pour chaque espèce et
# chaque sexe
penguins |> 
  summarise(moy = mean(bill_length_mm, na.rm = TRUE),
            variance = var(bill_length_mm, na.rm = TRUE),
            effectif = n(),
            .by = c(species, sex))

# Utilisation de la fonction pivot_wider() pour passer d'un tableau "long"
# à un tableau "large"...
resum <- penguins |> 
  count(species, sex) |> 
  filter(!is.na(sex)) |> 
  pivot_wider(names_from = sex,
              values_from = n)

resum

# Et de la fonction pivot_longer() pour passer d'un tableau "large" à un tableau
# "long"
resum |> 
  pivot_longer(cols = c(female, male),
               names_to = "sex",
               values_to = "effectif")

# la fonction skim() du package skimr pour produire des résumés de données ----

# Pour un tableau entier
penguins |> 
  skim()

# Pour des variables spécifiques et plusieurs sous-groupes (ici, pour chaque espèce)
penguins |> 
  group_by(species) |> 
  skim(bill_length_mm, bill_depth_mm)

# unite() : pour fusionner des colonnes ----
penguins |> 
  unite(new_col, species, sex, sep = "_")

penguins |> 
  unite(new_col, species, sex, sep = "_", remove = FALSE)

# separate() : pour séparer des colonnes ----
penguins |> 
  separate(year, into = c("siecle", "annee"),
           sep = 2, remove = FALSE)


# ------------------------------------------------------------------------------

# Tests d'hypothèses ----

# Question scientifique : existe-t-il un dimorphisme sexuel secondaire chez les 
# manchots Adélie ? Si oui lequel ?

# On souhaite comparer la moyenne de 2 groupes avec les hypothèses suivantes :
# H0 : la longueur des nageoires est la même chez les mâles et les femelles de 
# l'espèce Adélie. µ_males = µ_femelles
# H1 : la longueur des nageoires n'est pas la même chez les mâles et les femelles de 
# l'espèce Adélie. µ_males ≠ µ_femelles

# Le test paramétrique est le test de Student
# Le test non paramétrique équivalent est le test de Wilcoxon (Mann_Whitney)

# 3 conditions d'application pour le test de Student
# 1. les données sont issues d'un échantillonnage aléatoire
# 2. les données de chaque groupe doivent suivre une distribution normale dans la population générale
# 3. les variances des 2 groupes est homogène dans les populations générales

# Test de normalité (condition nº2) : test de Shapiro-Wilk
# H0 : dans la population, les données suivent la loi Normale
# H1 : dans la population, les données ne suivent pas la loi Normale

# On commence par extraire les données dont on a besoin : 
# espèce Adélie, sexe connu, et uniquement les variables d'intérêt pour l'étude
adelie <- penguins |> 
  filter(species == "Adelie",
         !is.na(sex)) |> 
  select(species, sex, flipper_length_mm)

adelie

# Réalisation des 2 tests de normalité
adelie |> 
  group_by(sex) |> 
  shapiro_test(flipper_length_mm)

# p >> alpha pour les mâles et les femelles.
# On ne peut pas rejeter l'hypothèse nulle de normalité. On est donc dans la 
# situation idéale et on va donc vérifier la dernière condition d'application.

# Test d'homogénéité des variances : test de Levene
# H0 : la variance de tous les groupes est identique (sigma2_mâles = sigma2_femelles)
# H1 : au moins un groupe a une variance différente des autres
adelie |> 
  levene_test(flipper_length_mm ~ sex)
# p > 0.05, donc on ne peut pas rejeter H0 : les variances sont homogènes


# On a donc le droit de faire le test de Student. Rappel des hypothèses :
# H0 : la longueur des nageoires est la même chez les mâles et les femelles de 
# l'espèce Adélie. µ_males = µ_femelles
# H1 : la longueur des nageoires n'est pas la même chez les mâles et les femelles de 
# l'espèce Adélie. µ_males ≠ µ_femelles
adelie |> 
  t_test(flipper_length_mm ~ sex, var.equal = TRUE, detailed = TRUE)

# ici, on rejette largement H0. Les résultats du test indiquent quel groupe a les
# plus grandes nageoires, ainsi que la différence de taille entre les groupes
# et l'intervalle de confiance à 95% de cette différence. 

# On a donc la réponse à la question posée et on doit donc s'arrêter là.
# Si toutefois les données suivent la loi Normale, mais que les variances ne sont 
# pas homogènes, et si la différence des variance n'excède pas un facteur 10, on 
# a le droit de réaliser un test de Student dit "de Welch" :
adelie |> 
  t_test(flipper_length_mm ~ sex, var.equal = FALSE, detailed = TRUE)

# Enfin, si les données ne suivent pas la loi Normale, on n'a pas le droit de 
# faire un test de Student : on fait alors un test de Wilcoxon :
adelie |> 
  wilcox_test(flipper_length_mm ~ sex, detailed = TRUE)

# Pour finir, attention aux tests unilatéraux : se tromper dans le choix de 
# l'hypothèse alternative peut conduire à des erreurs d'interprétation :

# H0 : µ_female = µ_male -> µ_female - µ_male = 0
# H1 : µ_female > µ_male -> µ_female - µ_male > 0
adelie |> 
  t_test(flipper_length_mm ~ sex, var.equal = TRUE, detailed = TRUE,
         alternative = "greater") 

# H0 : µ_female = µ_male -> µ_female - µ_male = 0
# H1 : µ_female < µ_male -> µ_female - µ_male < 0
adelie |> 
  t_test(flipper_length_mm ~ sex, var.equal = TRUE, detailed = TRUE,
         alternative = "less") 

