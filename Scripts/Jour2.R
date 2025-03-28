# Mise en mémoire des packages utiles
library(tidyverse)
library(palmerpenguins)
library(scales)

# Importation des données du fichier dauphin.csv
dauphin <- read_delim("Data/dauphin.csv", 
                      delim = ";", escape_double = FALSE, 
                      locale = locale(decimal_mark = ","), 
                      trim_ws = TRUE)
dauphin
penguins


# Nuages de points ----
ggplot(data = penguins, 
       mapping = aes(x = bill_length_mm, 
                     y = bill_depth_mm)) +
  geom_point()

# Le même graphique avec un code plus concis.
penguins |> 
  ggplot(aes(x = bill_length_mm, y = bill_depth_mm)) +
  geom_point()

# Comment gérer l'overplotting ?
# En jouant sur la transparence/opacité
penguins |> 
  ggplot(aes(x = bill_length_mm, y = bill_depth_mm)) +
  geom_point(alpha = 0.4)

# En jouant sur la taille des symboles
penguins |> 
  ggplot(aes(x = bill_length_mm, y = bill_depth_mm)) +
  geom_point(size = 0.8)

# En jouant sur le type de symbole
penguins |> 
  ggplot(aes(x = bill_length_mm, y = bill_depth_mm)) +
  geom_point(shape = 20)

# Pour sauvegarder/exporter les figures créées
asp <- 1/1.618
ggsave("Figures/Nuage_1.png", dpi = 300, width = 7, height = 7 * asp)

# Pour modifier la couleur des symboles
penguins |> 
  ggplot(aes(x = bill_length_mm, y = bill_depth_mm)) +
  geom_point(color = "purple", shape = 17, 
             alpha = 0.4)

# Association d'une variable du tableau de données à une caractéristique des
# objets géométriques : ici, on associe l'espèce à la couleur des points
penguins |> 
  ggplot(aes(x = bill_length_mm, y = bill_depth_mm,
             color = species)) +
  geom_point(shape = 17, alpha = 0.8)

# On peut associer l'espèce au type de symbole (shape), et associer une variable
# numérique à la couleur des symboles. L'échelle est alors un gradient de couleurs
penguins |> 
  ggplot(aes(x = bill_length_mm, y = bill_depth_mm,
             color = body_mass_g, shape = species)) +
  geom_point(alpha = 0.8)

# Une façon d'afficher uniquement les données de l'espèce Adélie
penguins |> 
  filter(species == "Adelie") |> 
  ggplot(aes(x = bill_length_mm, y = bill_depth_mm)) +
  geom_point(alpha = 0.8)

# Et une autre façon de faire ressortir uniquement les données de l'espèce Adélie
penguins |> 
  ggplot(aes(x = bill_length_mm, y = bill_depth_mm,
             color = species != "Adelie")) +
  geom_point(alpha = 0.7) +
  scale_color_manual(values = c("firebrick3", "grey40"), 
                     labels = c("Adélie", "autres")) +
  labs(color = "Espèce", x = "Longueur du bec (mm)", y = "Épaisseur du bec (mm)") +
  theme_bw()

# Possibilité de spécifier une couleur de contour (color) et une couleur de 
# remplissage (fill) pour certains symboles (shape = 21, 22, 23 ou 24)
penguins |> 
  ggplot(aes(x = bill_length_mm, y = bill_depth_mm)) +
  geom_point(color = "grey20", shape = 21, 
             fill = "orange", alpha = 0.7)

penguins |> 
  ggplot(aes(x = bill_length_mm, y = bill_depth_mm,
             fill = species)) +
  geom_point(shape = 22)

# Illustration de l'importance du positionnement de aes() :
# - dans ggplot() : s'applique à toutes les couches géométriques
# - dans un geom_ : ne s'applique qu'à cette couche géométrique
# Ici, illustration du paradoxe de Simpson : la relation décroissante entre les
# deux variables n'est pas réelle :
penguins |> 
  ggplot(aes(x = bill_length_mm, y = bill_depth_mm)) +
  geom_point(aes(color = species)) +
  geom_smooth(method = "lm")

# ... pour chaque espèce, la relation est en fait positive. Attention donc au 
# placement de color = species : dans le aes() de ggplot() ou dans celui de geom_point()
penguins |> 
  ggplot(aes(x = bill_length_mm, y = bill_depth_mm, color = species)) +
  geom_point() +
  geom_smooth(method = "lm")

# Pour créer une couleur qu'on pourra réutiliser ensuite
ma_couleur <- rgb(123, 196, 163, maxColorValue = 255)
penguins |> 
  ggplot(aes(x = bill_length_mm, y = bill_depth_mm)) +
  geom_point(color = ma_couleur)


# Visualiser les distributions ----
## Avec un stripchart ----
# C'est une façon de visualiser des distributions en affichant toutes les données
# C'est aussi une façon de limiter l'overplotting quand on souhaite visualiser la
# relation entre une variable numérique et un facteur
penguins |> 
  ggplot(aes(x = species, y = body_mass_g)) +
  geom_jitter(width = 0.2, height = 0, alpha = 0.5)

## Avec un histogramme ----
# On peut choisir les classes de l'histogramme avec :
# - bins : le nombre de classes souhaitées
# - binwidth : la largeur des classes souhaitées
penguins |> 
  ggplot(aes(x = body_mass_g, fill = sex)) +
  geom_histogram(binwidth = 200, alpha = 0.5,
                 color = "black") +
  facet_wrap(~ species, ncol = 1)

# - breaks : en spécifiant manuellement les limites des classes souhaitées
lmt <- seq(from = 2500, to = 6500, by = 150)
lmt

penguins |> 
  ggplot(aes(x = body_mass_g, fill = sex)) +
  geom_histogram(breaks = lmt, alpha = 0.5,
                 color = "black") +
  facet_wrap(~ species, ncol = 1, scales = "free_y")

# facet_wrap() (ci-dessus) et facet_grid() (ci-dessous) permettent respectivement
# de créer des sous-figures pour chaque modalité d'un facteur ou chaque  modalité
# de combinaison de 2 facteurs.
penguins |> 
  ggplot(aes(x = body_mass_g, fill = species)) +
  geom_histogram(breaks = lmt, alpha = 0.5,
                 color = "black", show.legend = FALSE) +
  facet_grid(species ~ sex)


## Avec des graphiques de densités ----
penguins |> 
  ggplot(aes(x = body_mass_g)) +
  geom_density(alpha = 0.5, fill = "steelblue") +
  facet_wrap(~ species, ncol = 1, scales = "free_y")

## Avec des boîtes à moustaches ----
# notch = TRUE permet d'afficher les encoches d'incertitude. Il s'agit des 
# intervalles de confiance à 95% des médianes
penguins |> 
  ggplot(aes(x = species, y = body_mass_g)) +
  geom_boxplot(notch = TRUE, outlier.color = NA) +
  geom_jitter(width = 0.2, alpha = 0.3)

# Comme pour les autres objets géométriques, on peut associer d'autres variables
# (ici, le sexe des individus) à la couleur de remplissage des boîtes à moustaches
penguins |> 
  filter(!is.na(sex)) |> 
  ggplot(aes(x = species, y = body_mass_g)) +
  geom_boxplot(aes(fill = sex))

# Pour retirer les lignes du tableau penguins pour lesquelles le sexe des individus
# est inconnu
penguins |> 
  filter(!is.na(sex))

# Quand on ne dispose pas de suffisamment de données au regard de la variabilité
# de la grandeur étudiée, les bornes des IC95% dépassent les quartiles
# (effet nœud papillon)
dauphin |> 
  ggplot(aes(x = Repro, y = Taille)) +
  geom_boxplot(notch = TRUE) +
  geom_jitter(width = 0.2)

## Avec des violin plots ----
penguins |> 
  ggplot(aes(x = species, y = body_mass_g)) +
  geom_violin(draw_quantiles = c(0.25, 0.5, 0.75))


# Correction de l'exercice sur les dauphins----
# Graphique 1
dauphin |> 
  ggplot(aes(x = Age, y = Taille)) +
  geom_point(aes(color = Sexe)) +
  geom_smooth(se = FALSE)

# Graphique 2
dauphin |> 
  ggplot(aes(x = Age, y = Taille, color = Sexe)) +
  geom_point() +
  geom_smooth(se = FALSE)

# Graphique 3
dauphin |> 
  ggplot(aes(x = Age, y = Taille, shape = Sexe)) +
  geom_point() +
  geom_smooth(se = FALSE)

# Autre possibilité pour le graphique 3
dauphin |> 
  ggplot(aes(x = Age, y = Taille)) +
  geom_point(aes(shape = Sexe)) +
  geom_smooth(aes(group = Sexe), se = FALSE)

# Graphique 4
dauphin |> 
  ggplot(aes(x = Age, y = Taille)) +
  geom_point(aes(shape = Sexe)) +
  geom_smooth(aes(linetype = Sexe), se = FALSE)


# Les diagrammes bâtons ----
penguins |> 
  ggplot(aes(x = species)) +
  geom_bar(color = "orange", fill = "pink")

# Barres empilées
penguins |> 
  filter(!is.na(sex)) |> 
  ggplot(aes(x = species)) +
  geom_bar(aes(fill = sex), color = "grey30",
           position = "stack")

# Barres juxtaposées
penguins |> 
  filter(!is.na(sex)) |> 
  ggplot(aes(x = species)) +
  geom_bar(aes(fill = sex), color = "grey30",
           position = "dodge")

# Comparaison de proportions
penguins |> 
  filter(!is.na(sex)) |> 
  ggplot(aes(x = species)) +
  geom_bar(aes(fill = sex), color = "grey30",
           position = "fill")

# Changer l'ordre des catégories pour trier par ordre de fréquence décroissante
penguins |> 
  filter(!is.na(sex)) |> 
  ggplot(aes(x = fct_infreq(species))) +
  geom_bar(color = "grey30")

# Changer l'ordre des catégories pour trier par ordre de fréquence croissante
penguins |> 
  filter(!is.na(sex)) |> 
  ggplot(aes(x = fct_rev(fct_infreq(species)))) +
  geom_bar(color = "grey30")

# Un exemple de carte ----
fr <- map_data("france")
fr |> 
  ggplot(aes(x = long, y = lat, group = group)) +
  geom_polygon() +
  coord_quickmap()

# Améliorer l'aspect des graphiques ----
graph <- penguins |> 
  filter(!is.na(sex)) |> 
  ggplot(aes(x = species, y = body_mass_g)) +
  geom_boxplot(aes(fill = sex))

## La fonction labs() pour modifier les titres et labels ----
graph +
  labs(x = "Espèce", 
       y = "Masse (g)",
       fill = "Sexe",
       title = "Masses corporelles pour 3 espèces de manchots",
       caption = "Source des données : palmerpenguins")

## Les fonctions scale_XXX_YYY pour modifier les échelles ----
# échelles de couleur, des valeurs sur les axes, de types de symboles...
graph +
  scale_y_continuous(breaks = seq(from = 2000, to = 8000, by = 500),
                     labels = number_format(), limits = c(2000, 8000)) +
  scale_fill_manual(values = c("steelblue", "orange"), 
                    labels = c("Femelles", "Mâles"))

## Les fonctions theme_XX() et theme() poru modifier tout le reste ----
# c'est à dire tout ce qui ne concerne pas directement les données
penguins |> 
  ggplot(aes(x = bill_length_mm, y = bill_depth_mm,
             color = body_mass_g, shape = species)) +
  geom_point(alpha = 0.8) +
  scale_color_viridis_c(direction = -1, option = "C",) +
  scale_shape_manual(values = c(1, 2, 3)) +
  labs(title = "Titre") +
  theme_bw(base_size = 12, base_family = "Josefin Slab") +
  theme()

graph +
  labs(title = "Titre", x = "Espèce") +
  theme_bw(base_size = 12, base_family = "Josefin Slab") +
  theme(axis.title.x = element_text(colour = "blue"),
        panel.background = element_rect(fill = "green"))


graph +
  labs(title = "Titre", x = "Espèce") +
  theme_bw(base_size = 12, base_family = "Josefin Slab") +
  theme(
    axis.title.x = element_text(colour = "blue"),
    panel.background = element_rect(fill = "green")
  ) +
  scale_y_continuous(position = "right")
