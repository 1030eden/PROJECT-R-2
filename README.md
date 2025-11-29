# projet-R
title: "Le rapport des Françaises et Français à l'information"
author: "Julie Scholler et vous"
date: ' '
output:
  html_document:
    toc_float: yes
    toc: yes
    theme: flatly
    df_print: paged
    number_section: false
editor_options: 
  chunk_output_type: console
---

```{r setup, echo = FALSE}
knitr::opts_chunk$set(echo = FALSE)
```

```{r packages, message = FALSE, warning = FALSE}
library(dplyr)
library(knitr)
library(kableExtra)# à charger après dplyr pour group_rows
library(ggplot2) # pour les graphiques
library(ggridges)# pour les densités décalées
library(forcats)# pour travailler sur les levels des facteurs
set_theme(theme_minimal())
```

```{r import}
data <- read.table("data/les-francais-et-l-information-arcom-2024-base-anonymisee.txt", quote ="",
                   header = TRUE, 
                   sep = "\t")
```

<!-- Fonctions de recodage -->

```{r fonction recode_possession}
recode_possession <- function(x){
  if(sum(!(x[!is.na(x)] %in% 1:3))==0){
    ordered(x,
           levels = c(3,1,2),
           labels = c("Aucun","Un","Plusieurs"
                              ))
  }else{
    stop("Toutes les valeurs ne sont pas des entiers entre 1 et 3.")
  }
}
```

```{r fonction recode_interet}
recode_interet <- function(x){
  if(sum(!(x[!is.na(x)] %in% 1:4))==0){
    ordered(x,
           levels = c(4,3,2,1),
                 labels = c("Pas du tout interessé.e",
                            "Peu interessé.e",
                            "Assez interessé.e",
                            "Très interessé.é"))
  }else{
    stop("Toutes les valeurs ne sont pas des entiers entre 1 et 4.")
  }
}
```

<!-- Recodage de variables-->

```{r recodage genre}
# recodage de la variable de genre
data$RS1_R <- ifelse(data$RS1_R == 1, # TEST
                     "homme", # valeur si TEST == TRUE
                     "femme") # valeur si TEST == FALSE
```

```{r recodage freq internet}
data$FREQ_INTERNET_R <- ordered(data$FREQ_INTERNET_R,
                 levels = c(3,2,1),
                 labels = c("Moins souvent",
                            "1 fois par jour ou presque",
                            "Plusieurs fois par jour"
                            ))
```

```{r recodage possession et interet info}
data <-  data |> 
  mutate(across(starts_with("RS16_R_"), # variable de possession de matériel
                recode_possession))|> 
  mutate(across(starts_with("INT2_R_"), # variable d'interet d'information thématique
                recode_interet))
```

```{r recodage de MOTIV_R1 en factor} 
data$MOTIV_R1 <- factor(data$MOTIV_R1,
       levels = 1:13,
       labels = c("Comprendre le monde qui m'entoure", 
                  "Rester informé des grands événements",
  "Me faire ma propre opinion", 
  "Prendre des décisions éclairées en tant que citoyen",
  "Pouvoir en discuter et débattre avec mon entourage", 
  "Me divertir",
  "Découvrir des nouvelles tendances ou cultures", "Satisfaire ma curiosité",
  "Passer le temps", 
  "M'instruire, me cultiver", 
  "Progresser dans mon travail, mes études",
  "Connaître d'autres avis que le mien", 
  "Par habitude")
)
```

<!-- Création de variables -->

```{r création pessimisme brut et Niveau-optimisme}
data$Niveau_pessimisme_brut <- data |> 
  select(OPTI_R_1:OPTI_R_6) |> 
  rowSums()
data$Niveau_optimisme <- round((24-data$Niveau_pessimisme_brut)/18*10, 2)
```

```{r création AGE avec recodage des tranches d age}
data$AGE <- ordered(data$RS2C_RECODE_AG_R,
                 levels = 1:7,
                 labels = c("15-17 ans", "18-24 ans",
                            "25-34 ans", "35-44 ans",
                            "45-59 ans", "60-69 ans", 
                            "70 ans et plus"
                            ))
```

# Présentation des données

Nous allons travailler sur les résultats d'une étude de l'[Arcom](https://www.arcom.fr/) sur le rapport des français à l'information dont les informations et les bases de données se trouvent sur le site [data.gouv](https://www.data.gouv.fr/datasets/les-francais-et-linformation-2024/).

L'enquête aborde différentes thématiques : 

- le profil sociodémographique et la consommation média ; 
- l'intérêt et le mode d'accès à l'information :
  * intêrét et motivations, 
  * modes d'accès et préférences, 
  * propension à payer pour s'informer, 
  * focus information en ligne, sources détaillées ; 
- les attitudes et le rapport à l'information :
  * attitudes générales, 
  * dynamique de la consommation ; 
- la confiance et la qualité : 
  * qualité et confiance générale par marque média et par type d'émetteurs, 
  * attitudes vis à vis des sources, 
  * *information fiable et défiance*, 
  * exposition aux théories complotistes ; 
- les médias éditorialisés et les journalistes ; 
- les réseaux sociaux et algorithmes ; 
- le rôle de la régulation ; 
- les personnalités, les valeurs et le rapport aux institutions ; 
- la proximité partisane ; 
- la **connaissance de l'information via un quizz**.

La base de données contient `r nrow(data)` lignes correspondant au nombre de personnes interrogées.

# Genre et information sportive

Il y a `r round(sum(data$RS1_R == "femme")/nrow(data)*100,1)`% de femmes parmi les répondants.

```{r graph sport, fig.height = 7, fig.width = 6, fig.align = "center"}
par(mfrow = c(2,1),
    cex = 0.8) # factor de taille d'écriture
barplot(proportions(table(data$INT2_R_10[data$RS1_R == "homme"])),
        main = "Intérêt des hommes pour l'information sportive",
        col = "lightblue2", border = "lightblue2",
        las = 1, space = 1)
barplot(proportions(table(data$INT2_R_10[data$RS1_R == "femme"])),
        main = "Intérêt des femmes pour l'information sportive", col = "lightblue2", border = "lightblue2",
        las = 1, space = 1)
```

# Possession de matériel numérique

```{r tab_possession}
tab_possession <- data |> 
  reframe(across(RS16_R_1:RS16_R_12, table)) |> 
  rename_with(~ c("TV", "Radio ou Hifi", "Autoradio", "Smartphone", "Ordinateur Fixe", "Ordinateur Portable", "Tablette", "Appareil de streaming", "Enceinte connectée", "Casque de VR", "Montre connectée", "Console"),  
              RS16_R_1:RS16_R_12) |> 
  as.data.frame(row.names = levels(data$RS16_R_1)) |> 
  t() |> as.data.frame() |> 
  arrange(Aucun)
```

```{r kable_plus}
kable_plus <- function(tab, transp = FALSE, ...){
  if(transp == TRUE){tab <- t(tab)}
  tab |> 
  kable(...) |> 
  kable_styling(full_width = FALSE, 
                position = "center", 
                bootstrap_options = c("striped", "hover"))
}
```

```{r pstation tab_possession}
tab_possession |> 
  kable_plus(caption = "Possession de materiel numérique") |> 
  column_spec(1, bold = TRUE) |> 
  group_rows(start_row = 1, end_row = 3, "Possédés par plus des trois quarts", color = "grey40") |> 
  group_rows(start_row = 9, end_row = 12, "Possédés par moins de la moitié", color = "grey40") |> 
  group_rows(start_row = 4, end_row = 8, "Possédés par moins des trois quarts", color = "grey40")
```

# Motivation à s'informer

```{r création vecteur effectif motivation}
# création et enregistrement du tableau d'effectif
motivation_top1_effectif <-  data$MOTIV_R1 |> table() |> sort() 
```

```{r fonction test_top3}
test_top3 <- function(x){
  x %in% 1:3
}
```

```{r creation vecteur motivation_top3_effectif}
motivation_top3_effectif <- data |> 
  mutate(across(starts_with("MOTIV_R_"), 
                test_top3)) |> 
  select(starts_with("MOTIV_R_"), -MOTIV_R_14) |> 
  rename_with(~c("Comprendre le monde qui m'entoure", "Rester informé des grands événements",
  "Me faire ma propre opinion", "Prendre des décisions éclairées en tant que citoyen",
  "Pouvoir en discuter et débattre avec mon entourage", "Me divertir",
  "Découvrir des nouvelles tendances ou cultures", "Satisfaire ma curiosité",
  "Passer le temps", "M'instruire, me cultiver", "Progresser dans mon travail, mes études",
  "Connaître d'autres avis que le mien", "Par habitude"), everything()) |> 
  colSums() |> 
  sort()
```

```{r barplot des motivations, fig.width = 10}
par(mfrow = c(1,2),
    mar = c(5,14,5,2), cex.axis = 0.7)
# top 1
barplot_motivation_top1 <- barplot(motivation_top1_effectif,
                                   col = "steelblue4", border = "steelblue4",
        main = "Motivation principale \npour s'informer",
        axes = FALSE ,
    horiz = TRUE, las = 2, space = 0.5
  )
text(y = barplot_motivation_top1,
     x = motivation_top1_effectif + 20,
     labels = motivation_top1_effectif, cex = 0.7,
     pos = 2, col = "white"
)
# top 3
barplot_motivation_top3 <- barplot(motivation_top3_effectif,
                                   col = "steelblue4", border = "steelblue4",
        main = "Top 3 des motivations \npour s'informer",
        axes = FALSE ,
    horiz = TRUE, las = 2, space = 0.5
  )
text(y = barplot_motivation_top3,
     x = motivation_top3_effectif+70,
     labels = motivation_top3_effectif, cex = 0.7,
     pos = 2, col = "white"
)
```


```{r}
data |> 
  ggplot() +
  aes(x = AGE,
      fill = MOTIV_R1 |> 
        fct_infreq() |> 
        fct_lump_n(5, other_level = "Autre")) +
  labs(title = "Motivation à s'informer selon l'âge",
       subtitle = "parmi les 5 raisons principales",
       y = "Proportion", x = NULL, fill = NULL)+
  labs(fill = NULL) +
  geom_bar(position = "fill") +
  coord_flip() +
  theme(legend.position = "bottom") +
  scale_fill_brewer(palette ="Set1")
```

# Optimisme

```{r}
data |> 
  ggplot() +
  aes(x = Niveau_optimisme, fill = RS1_R) +
  geom_histogram(color = "white",
                 breaks = seq(0, 10, length.out = 20)) +
  labs(title = "Niveau d'optimisme face à l'avenir",
       subtitle = "construit à partir des plusieurs thématiques",
       caption = "Sources : sondage pour l'Arcom, data.gouv",
       x = "Niveau d'optimisme sur 10", 
       y = "Effectif",
       fill = "Genre") +
  theme_minimal() +
  theme(legend.position = "none",
        legend.title = element_text(face = "italic"),
        plot.caption = element_text(face= "italic")
  ) +
  facet_wrap(~ RS1_R) +
  scale_fill_manual(values = c("darkorange", "chartreuse3"))
```


```{r, message = FALSE}
data |> 
  ggplot()+
  aes(x = Niveau_optimisme, fill = AGE,
      y = AGE) +
  geom_density_ridges(alpha = 0.7, col = NA,
                      scale = 8) +
  labs(title = "Niveau d'optimisme face à l'avenir par tranche d'age",
       subtitle = "construit à partir des plusieurs thématiques",
       x = "Niveau d'optimisme sur 10", y = " ") +
  theme_minimal() +
  theme(legend.position = "none") +
  annotate(geom = "text", 
           x = 9, y = 12,
           label = "Les 18-24 ans \nsont plus \noptimistes que les \nautres tranches d'âge.") +
  annotate(geom = "curve", 
           angle = 110, curvature = -0.25,
           x = 9, y = 10.5, 
           arrow = arrow(length = unit(2, "mm")),
            xend = 6.5, yend = 8.5 )
```


```{r}
data |> 
  summarise(Moyenne_optimisme = mean(Niveau_optimisme),
            .by = c(AGE, RS1_R))
```

```{r}
data |> 
  summarise(Moyenne_optimisme = mean(Niveau_optimisme),
            .by = c(AGE, RS1_R)) |> 
  ggplot() +
  aes( y = AGE, x = Moyenne_optimisme, color = RS1_R) +
  geom_point()
```

```{r}
library(tidyr)# à installer la première fois
```

```{r}
data |> 
  summarise(Moyenne_optimisme = mean(Niveau_optimisme),
            .by = c(AGE, RS1_R)) |> 
  pivot_wider(names_from = RS1_R,
              values_from = Moyenne_optimisme)
```

```{r}
data |> 
  summarise(Moyenne_optimisme = mean(Niveau_optimisme),
            .by = c(AGE, RS1_R)) |> 
  pivot_wider(names_from = RS1_R,
              values_from = Moyenne_optimisme) |> 
  pivot_longer(cols = c(femme, homme),
               names_to = "Genre",
               values_to = "Valeurs")
  
```

```{r}
data |> 
  summarise(Moyenne_optimisme = mean(Niveau_optimisme) |> round(2),
            ET_optimisme = sd(Niveau_optimisme) |> round(2),
            Effectif = n(),
            .by = AGE) |> 
  kable_plus()
```

# Tableaux avec modelsummary

```{r}
library(modelsummary) # à installer d'abord
```

```{r}
data_extract <- data |> 
  select(Genre = RS1_R, AGE, Niveau_optimisme,
         TV = RS16_R_1, Console = RS16_R_12)
```

```{r}
datasummary_skim(data_extract, type = "categorical")
datasummary_skim(data_extract, type = "numeric")
```

```{r}
datasummary(Niveau_optimisme ~ Min + (Médiane =Median) + (Moyenne = mean) + Max + Histogram, 
            data = data)
```

```{r}
datasummary(Niveau_optimisme ~ P0 + P25 + P50 + P75 + P100, data = data)
```

```{r}
datasummary(Niveau_optimisme ~ AGE * (Median + Mean ), data = data)
```

```{r}
datasummary(AGE * (` `=Niveau_optimisme) ~ Mean * RS1_R, data = data,
    title = "Niveau d'optimisme",
    notes = "calculé à partir de 6 thématiques",
    align = "rccc",
    output = "kableExtra") |> 
  row_spec(2, color = "deeppink") |> 
  column_spec(1, bold = TRUE)
```

# tests de comparaison

```{r}
t.test(Niveau_optimisme~RS1_R, 
       data = data |> filter(AGE=="15-17 ans"))
t.test(Niveau_optimisme~RS1_R, 
       data = data, subset = (AGE=="15-17 ans"))
```

```{r}
moy_test_auto <- function(...){
  var_egales <- var.test(...)$p.value > 0.05
  #
   test <- t.test(..., var.equal = var_egales)
   output <- c(test$estimate, test$conf.int[1], test$conf.int[2], test$p.value)
   names(output) <- c(names(test$estimate), "Borne inf. de l'IC à 95% de la diff.", 
                      "Borne sup. de l'IC à 95% de la diff.", "p-value")
   return(output)
 }
```

```{r}
moy_test_auto(Niveau_optimisme~RS1_R, 
       data = data |> filter(AGE=="15-17 ans"))
```

```{r}
test_opti_genre_souspartie <- function(sous_df) {
  c(n_h = sum(sous_df$RS1_R == "homme" & !is.na(sous_df$Niveau_optimisme)),
    n_f = sum(sous_df$RS1_R == "femme" & !is.na(sous_df$Niveau_optimisme)),
    moy_test_auto(Niveau_optimisme~RS1_R, data = sous_df)
  )
}
```

```{r}
sapply(split(data, data$AGE), 
       FUN = test_opti_genre_souspartie) |> 
  kable_plus(transp = TRUE, digits = c(0,0,2,2,2,2,6))
```

```{r}
color_pvalue <- function(vec){
  ifelse(vec < 0.05, "red",
         ifelse(vec < 0.1,
                "darkorange",
                "grey"))
}
```

```{r}
color_pvalue(c(0.01, 0.02, 0.055, 0.12, 0.90, 0.02))
```

```{r}
res_test <- sapply(split(data, data$AGE), 
       FUN = test_opti_genre_souspartie) |> t()
res_test |> 
  kable_plus( digits = c(0,0,2,2,2,2,6)) |> 
  column_spec(8, color = color_pvalue(res_test[,7]))
```
# PROJECT-R-2
