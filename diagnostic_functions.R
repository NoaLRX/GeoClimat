# Fonctions pour la génération des graphiques de diagnostic climatique
library(ggplot2)
library(dplyr)
library(tidyr)
library(gridExtra)
library(cowplot)  # Pour une meilleure mise en page des graphiques multiples

# Fonction pour générer des données climatiques simulées pour une commune
generate_mock_climate_data <- function(code_commune) {
  # Utiliser le code INSEE pour générer des valeurs pseudo-aléatoires mais cohérentes
  # Convertir le code commune en nombre pour initialiser le générateur aléatoire
  code_numeric <- as.numeric(gsub("[^0-9]", "", as.character(code_commune)))
  if (is.na(code_numeric)) code_numeric <- 12345
  set.seed(code_numeric)
  
  # Générer des données fictives pour les visualisations
  ref_data <- list()
  s26_data <- list()
  s45_data <- list()
  s85_data <- list()
  
  # Température moyenne
  ref_temp <- runif(1, 10, 15)
  ref_data$NORTAV_H1 <- ref_temp
  ref_data$NORTAV_H2 <- ref_temp
  ref_data$NORTAV_H3 <- ref_temp
  
  s26_data$NORTAV_H1 <- ref_temp + runif(1, 0.2, 1.0)
  s26_data$NORTAV_H2 <- ref_temp + runif(1, 0.5, 1.5)
  s26_data$NORTAV_H3 <- ref_temp + runif(1, 0.8, 1.8)
  
  s45_data$NORTAV_H1 <- ref_temp + runif(1, 0.5, 1.5)
  s45_data$NORTAV_H2 <- ref_temp + runif(1, 1.0, 2.0)
  s45_data$NORTAV_H3 <- ref_temp + runif(1, 1.5, 2.5)
  
  s85_data$NORTAV_H1 <- ref_temp + runif(1, 0.8, 1.8)
  s85_data$NORTAV_H2 <- ref_temp + runif(1, 1.5, 2.5)
  s85_data$NORTAV_H3 <- ref_temp + runif(1, 2.5, 4.0)
  
  # Température maximale
  ref_tx <- runif(1, 15, 20)
  ref_data$NORTXAV_H1 <- ref_tx
  ref_data$NORTXAV_H2 <- ref_tx
  ref_data$NORTXAV_H3 <- ref_tx
  
  s26_data$NORTXAV_H1 <- ref_tx + runif(1, 0.2, 1.0)
  s26_data$NORTXAV_H2 <- ref_tx + runif(1, 0.5, 1.5)
  s26_data$NORTXAV_H3 <- ref_tx + runif(1, 0.8, 1.8)
  
  s45_data$NORTXAV_H1 <- ref_tx + runif(1, 0.5, 1.5)
  s45_data$NORTXAV_H2 <- ref_tx + runif(1, 1.0, 2.0)
  s45_data$NORTXAV_H3 <- ref_tx + runif(1, 1.5, 2.5)
  
  s85_data$NORTXAV_H1 <- ref_tx + runif(1, 0.8, 1.8)
  s85_data$NORTXAV_H2 <- ref_tx + runif(1, 1.5, 2.5)
  s85_data$NORTXAV_H3 <- ref_tx + runif(1, 2.5, 4.0)
  
  # Précipitations
  ref_pr <- runif(1, 600, 1000)
  ref_data$ATAV_H1 <- ref_pr
  ref_data$ATAV_H2 <- ref_pr
  ref_data$ATAV_H3 <- ref_pr
  
  s26_data$ATAV_H1 <- ref_pr * runif(1, 0.95, 1.05)
  s26_data$ATAV_H2 <- ref_pr * runif(1, 0.9, 1.0)
  s26_data$ATAV_H3 <- ref_pr * runif(1, 0.85, 0.95)
  
  s45_data$ATAV_H1 <- ref_pr * runif(1, 0.9, 1.0)
  s45_data$ATAV_H2 <- ref_pr * runif(1, 0.85, 0.95)
  s45_data$ATAV_H3 <- ref_pr * runif(1, 0.8, 0.9)
  
  s85_data$ATAV_H1 <- ref_pr * runif(1, 0.85, 0.95)
  s85_data$ATAV_H2 <- ref_pr * runif(1, 0.75, 0.85)
  s85_data$ATAV_H3 <- ref_pr * runif(1, 0.65, 0.75)
  
  # Retourner les listes de données
  return(list(
    ref_data = ref_data,
    s26_data = s26_data,
    s45_data = s45_data, 
    s85_data = s85_data
  ))
}

# Fonction pour créer un graphique pour une variable spécifique (comme dans wrapper.R)
create_scenario_plot <- function(ref_data, s26_data, s45_data, s85_data, var_prefix, var_name, y_label, subtitle = NULL) {
  # Extraire la valeur de référence pour la commune spécifique
  ref_value <- ref_data[[paste0(var_prefix, "_H1")]]
  
  # Extraire les valeurs pour chaque scénario et horizon
  s26_h1 <- s26_data[[paste0(var_prefix, "_H1")]]
  s26_h2 <- s26_data[[paste0(var_prefix, "_H2")]]
  s26_h3 <- s26_data[[paste0(var_prefix, "_H3")]]
  
  s45_h1 <- s45_data[[paste0(var_prefix, "_H1")]]
  s45_h2 <- s45_data[[paste0(var_prefix, "_H2")]]
  s45_h3 <- s45_data[[paste0(var_prefix, "_H3")]]
  
  s85_h1 <- s85_data[[paste0(var_prefix, "_H1")]]
  s85_h2 <- s85_data[[paste0(var_prefix, "_H2")]]
  s85_h3 <- s85_data[[paste0(var_prefix, "_H3")]]
  
  # Créer un dataframe pour le graphique
  plot_data <- data.frame(
    scenario = c(
      "REF", "2.6", "4.5", "8.5",  # H1
      "REF", "2.6", "4.5", "8.5",  # H2
      "REF", "2.6", "4.5", "8.5"   # H3
    ),
    horizon = c(
      rep("H1", 4),
      rep("H2", 4),
      rep("H3", 4)
    ),
    value = c(
      ref_value, s26_h1, s45_h1, s85_h1,
      ref_value, s26_h2, s45_h2, s85_h2,
      ref_value, s26_h3, s45_h3, s85_h3
    )
  )
  
  # Convertir scenario en facteur ordonné
  plot_data$scenario <- factor(plot_data$scenario, levels = c("REF", "2.6", "4.5", "8.5"))
  
  # Créer un dataframe filtré pour les connexions entre les points (sans REF)
  plot_data_filtered <- plot_data %>%
    dplyr::filter(scenario != "REF")
  
  # Créer le graphique avec des éléments plus grands pour mieux utiliser l'espace
  p <- ggplot(plot_data_filtered, aes(x = scenario, y = value, color = horizon, group = horizon)) +
    # Ajouter les lignes reliant les points
    geom_line(size = 2.5) +
    # Ajouter les points
    geom_point(size = 14, alpha = 0.7) +
    # Ajouter les étiquettes de valeurs
    geom_text(aes(label = round(value, 1)), color = "white", size = 5, fontface = "bold") +
    # Ajouter la valeur de référence comme ligne horizontale en pointillés
    geom_hline(yintercept = ref_value, linetype = "dashed", color = "gray50", size = 1.2) +
    # Ajouter l'étiquette de la valeur de référence
    annotate("text", x = 0.75, y = ref_value, label = paste("Réf:", round(ref_value, 1)), 
             hjust = 0, color = "gray50", fontface = "italic", size = 5) +
    # Définir les couleurs
    scale_color_manual(values = c("H1" = "#69b3d6", "H2" = "#f89c39", "H3" = "#d93b48")) +
    # Personnaliser les axes et le titre
    labs(
      title = paste("Évolution de", var_name),
      subtitle = subtitle,
      x = "Scénario RCP",
      y = y_label,
      color = "Horizon"
    ) +
    # Personnaliser le thème
    theme_minimal() +
    theme(
      panel.grid.minor = element_blank(),
      legend.position = "bottom",
      plot.title = element_text(hjust = 0.5, face = "bold", size = 18),
      plot.subtitle = element_text(hjust = 0.5, size = 16),
      axis.title = element_text(face = "bold", size = 16),
      axis.text = element_text(size = 14),
      legend.title = element_text(face = "bold", size = 14),
      legend.text = element_text(size = 12),
      legend.key.size = unit(1.2, "cm")
    )
  
  return(p)
}

# Fonction principale pour générer tous les graphiques de diagnostic
generate_diagnostic_plots <- function(commune_name, code_commune) {
  if (is.null(code_commune) || is.null(commune_name)) {
    # Retourner un graphique de message si pas de commune sélectionnée
    p <- ggplot() + 
         annotate("text", x = 0.5, y = 0.5, 
                 label = "Sélectionnez d'abord une adresse dans l'onglet 'Carte interactive'",
                 size = 4) +
         theme_void()
    return(p)
  }
  
  # Générer des données fictives pour le diagnostic
  data <- generate_mock_climate_data(code_commune)
  
  # Créer les graphiques pour chaque variable - Données locales
  p1_local <- create_scenario_plot(data$ref_data, data$s26_data, data$s45_data, data$s85_data, 
                                  "NORTAV", "température moyenne", "Température (°C)", 
                                  paste("Données locales -", commune_name))
  
  p2_local <- create_scenario_plot(data$ref_data, data$s26_data, data$s45_data, data$s85_data, 
                                  "NORTXAV", "journées d'été (>25°C)", "Nombre de jours", 
                                  paste("Données locales -", commune_name))
  
  p3_local <- create_scenario_plot(data$ref_data, data$s26_data, data$s45_data, data$s85_data, 
                                  "ATAV", "précipitations", "Cumul (mm)", 
                                  paste("Données locales -", commune_name))
  
  # Simuler des données pour la moyenne nationale
  # En pratique, on utiliserait des données réelles
  national_data <- generate_mock_climate_data("00000")  # Utiliser un code commun pour tous
  
  # Créer les graphiques pour la moyenne nationale
  p1_national <- create_scenario_plot(national_data$ref_data, national_data$s26_data, 
                                     national_data$s45_data, national_data$s85_data,
                                     "NORTAV", "température moyenne", "Température (°C)", 
                                     "Moyenne nationale")
  
  p2_national <- create_scenario_plot(national_data$ref_data, national_data$s26_data, 
                                     national_data$s45_data, national_data$s85_data,
                                     "NORTXAV", "journées d'été (>25°C)", "Nombre de jours", 
                                     "Moyenne nationale")
  
  p3_national <- create_scenario_plot(national_data$ref_data, national_data$s26_data, 
                                     national_data$s45_data, national_data$s85_data,
                                     "ATAV", "précipitations", "Cumul (mm)", 
                                     "Moyenne nationale")
  
  # Extraire les légendes une seule fois pour éviter les répétitions
  legend <- get_legend(p1_local + theme(legend.position = "bottom", legend.box = "horizontal"))
  
  # Supprimer les légendes de tous les graphiques individuels
  p1_local <- p1_local + theme(legend.position = "none")
  p2_local <- p2_local + theme(legend.position = "none")
  p3_local <- p3_local + theme(legend.position = "none")
  p1_national <- p1_national + theme(legend.position = "none")
  p2_national <- p2_national + theme(legend.position = "none")
  p3_national <- p3_national + theme(legend.position = "none")
  
  # Organiser les graphiques en grille
  grid_plot <- plot_grid(
    p1_local, p2_local, p3_local,
    p1_national, p2_national, p3_national,
    ncol = 3, nrow = 2,
    align = "hv",
    labels = NULL
  )
  
  # Ajouter la légende commune en bas
  final_plot <- plot_grid(
    grid_plot, 
    legend, 
    ncol = 1, 
    rel_heights = c(1, 0.1)
  )
  
  return(final_plot)
}

# Fonctions spécifiques pour chaque type de graphique (températures, précipitations, vagues de chaleur)
# Ces fonctions sont utilisées par server.R

# Fonction pour le graphique des températures
generate_temperature_plot <- function(code_commune, commune_name) {
  if (is.null(code_commune) || is.null(commune_name)) {
    # Message si aucune commune n'est sélectionnée
    p <- ggplot() + 
         annotate("text", x = 0.5, y = 0.5, 
                 label = "Sélectionnez d'abord une adresse dans l'onglet 'Carte interactive'",
                 size = 4) +
         theme_void()
    return(p)
  }
  
  # Générer des données fictives pour le diagnostic
  data <- generate_mock_climate_data(code_commune)
  
  # Générer des données nationales (légèrement différentes)
  national_data <- list(
    ref_data = list(
      NORTAV_H1 = data$ref_data$NORTAV_H1 * 0.95,
      NORTAV_H2 = data$ref_data$NORTAV_H2 * 0.95,
      NORTAV_H3 = data$ref_data$NORTAV_H3 * 0.95
    ),
    s26_data = list(
      NORTAV_H1 = data$s26_data$NORTAV_H1 * 0.96,
      NORTAV_H2 = data$s26_data$NORTAV_H2 * 0.96,
      NORTAV_H3 = data$s26_data$NORTAV_H3 * 0.96
    ),
    s45_data = list(
      NORTAV_H1 = data$s45_data$NORTAV_H1 * 0.97,
      NORTAV_H2 = data$s45_data$NORTAV_H2 * 0.97,
      NORTAV_H3 = data$s45_data$NORTAV_H3 * 0.97
    ),
    s85_data = list(
      NORTAV_H1 = data$s85_data$NORTAV_H1 * 0.98,
      NORTAV_H2 = data$s85_data$NORTAV_H2 * 0.98,
      NORTAV_H3 = data$s85_data$NORTAV_H3 * 0.98
    )
  )
  
  # Préparer les données pour le graphique - Données locales
  local_data <- data.frame(
    scenario = c(
      "REF", "2.6", "4.5", "8.5",  # H1
      "REF", "2.6", "4.5", "8.5",  # H2
      "REF", "2.6", "4.5", "8.5"   # H3
    ),
    horizon = c(
      rep("H1", 4),
      rep("H2", 4),
      rep("H3", 4)
    ),
    type = "Locale",
    value = c(
      data$ref_data$NORTAV_H1, data$s26_data$NORTAV_H1, data$s45_data$NORTAV_H1, data$s85_data$NORTAV_H1,
      data$ref_data$NORTAV_H2, data$s26_data$NORTAV_H2, data$s45_data$NORTAV_H2, data$s85_data$NORTAV_H2,
      data$ref_data$NORTAV_H3, data$s26_data$NORTAV_H3, data$s45_data$NORTAV_H3, data$s85_data$NORTAV_H3
    )
  )
  
  # Préparer les données pour le graphique - Moyenne nationale
  national_df <- data.frame(
    scenario = c(
      "REF", "2.6", "4.5", "8.5",  # H1
      "REF", "2.6", "4.5", "8.5",  # H2
      "REF", "2.6", "4.5", "8.5"   # H3
    ),
    horizon = c(
      rep("H1", 4),
      rep("H2", 4),
      rep("H3", 4)
    ),
    type = "Nationale",
    value = c(
      national_data$ref_data$NORTAV_H1, national_data$s26_data$NORTAV_H1, national_data$s45_data$NORTAV_H1, national_data$s85_data$NORTAV_H1,
      national_data$ref_data$NORTAV_H2, national_data$s26_data$NORTAV_H2, national_data$s45_data$NORTAV_H2, national_data$s85_data$NORTAV_H2,
      national_data$ref_data$NORTAV_H3, national_data$s26_data$NORTAV_H3, national_data$s45_data$NORTAV_H3, national_data$s85_data$NORTAV_H3
    )
  )
  
  # Combiner les données
  combined_data <- rbind(local_data, national_df)
  
  # Filtrer les données en gardant seulement les points non-REF
  plot_data_filtered <- combined_data %>% dplyr::filter(scenario != "REF")
  
  # Valeur de référence pour la ligne pointillée
  ref_value <- data$ref_data$NORTAV_H1
  
  # Convertir scenario en facteur ordonné
  plot_data_filtered$scenario <- factor(plot_data_filtered$scenario, levels = c("2.6", "4.5", "8.5"))
  
  # Créer le graphique
  p <- ggplot(plot_data_filtered, aes(x = scenario, y = value, color = horizon, group = interaction(horizon, type), linetype = type)) +
    # Ajouter les lignes reliant les points
    geom_line(size = 1.5) +
    # Ajouter les points
    geom_point(size = 8, alpha = 0.7) +
    # Ajouter la valeur de référence comme ligne horizontale en pointillés
    geom_hline(yintercept = ref_value, linetype = "dashed", color = "gray50", size = 1.0) +
    # Ajouter l'étiquette de la valeur de référence
    annotate("text", x = 0.75, y = ref_value, label = paste("Réf:", round(ref_value, 1)), 
             hjust = 0, color = "gray50", fontface = "italic", size = 5) +
    # Définir les couleurs et les types de lignes
    scale_color_manual(values = c("H1" = "#69b3d6", "H2" = "#f89c39", "H3" = "#d93b48")) +
    scale_linetype_manual(values = c("Locale" = "solid", "Nationale" = "dotted")) +
    # Personnaliser les axes et le titre
    labs(
      title = "Évolution de la température moyenne (°C)",
      subtitle = paste("Données pour", commune_name, "et moyenne nationale"),
      x = "Scénario RCP",
      y = "Température (°C)",
      color = "Horizon",
      linetype = "Type"
    ) +
    # Personnaliser le thème
    theme_minimal() +
    theme(
      panel.grid.minor = element_blank(),
      legend.position = "bottom",
      plot.title = element_text(hjust = 0.5, face = "bold", size = 18),
      plot.subtitle = element_text(hjust = 0.5, size = 16),
      axis.title = element_text(face = "bold", size = 16),
      axis.text = element_text(size = 14),
      legend.title = element_text(face = "bold", size = 14),
      legend.text = element_text(size = 12),
      legend.key.size = unit(1.2, "cm")
    )
  
  return(p)
}

# Fonction pour le graphique des précipitations
generate_precipitation_plot <- function(code_commune, commune_name) {
  if (is.null(code_commune) || is.null(commune_name)) {
    # Message si aucune commune n'est sélectionnée
    p <- ggplot() + 
         annotate("text", x = 0.5, y = 0.5, 
                 label = "Sélectionnez d'abord une adresse dans l'onglet 'Carte interactive'",
                 size = 4) +
         theme_void()
    return(p)
  }
  
  # Générer des données fictives pour le diagnostic
  data <- generate_mock_climate_data(code_commune)
  
  # Générer des données nationales (légèrement différentes)
  national_data <- list(
    ref_data = list(
      ATAV_H1 = data$ref_data$ATAV_H1 * 1.05,
      ATAV_H2 = data$ref_data$ATAV_H2 * 1.05,
      ATAV_H3 = data$ref_data$ATAV_H3 * 1.05
    ),
    s26_data = list(
      ATAV_H1 = data$s26_data$ATAV_H1 * 1.03,
      ATAV_H2 = data$s26_data$ATAV_H2 * 1.03,
      ATAV_H3 = data$s26_data$ATAV_H3 * 1.03
    ),
    s45_data = list(
      ATAV_H1 = data$s45_data$ATAV_H1 * 1.02,
      ATAV_H2 = data$s45_data$ATAV_H2 * 1.01,
      ATAV_H3 = data$s45_data$ATAV_H3 * 1.00
    ),
    s85_data = list(
      ATAV_H1 = data$s85_data$ATAV_H1 * 0.99,
      ATAV_H2 = data$s85_data$ATAV_H2 * 0.98,
      ATAV_H3 = data$s85_data$ATAV_H3 * 0.97
    )
  )
  
  # Préparer les données pour le graphique - Données locales
  local_data <- data.frame(
    scenario = c(
      "REF", "2.6", "4.5", "8.5",  # H1
      "REF", "2.6", "4.5", "8.5",  # H2
      "REF", "2.6", "4.5", "8.5"   # H3
    ),
    horizon = c(
      rep("H1", 4),
      rep("H2", 4),
      rep("H3", 4)
    ),
    type = "Locale",
    value = c(
      data$ref_data$ATAV_H1, data$s26_data$ATAV_H1, data$s45_data$ATAV_H1, data$s85_data$ATAV_H1,
      data$ref_data$ATAV_H2, data$s26_data$ATAV_H2, data$s45_data$ATAV_H2, data$s85_data$ATAV_H2,
      data$ref_data$ATAV_H3, data$s26_data$ATAV_H3, data$s45_data$ATAV_H3, data$s85_data$ATAV_H3
    )
  )
  
  # Préparer les données pour le graphique - Moyenne nationale
  national_df <- data.frame(
    scenario = c(
      "REF", "2.6", "4.5", "8.5",  # H1
      "REF", "2.6", "4.5", "8.5",  # H2
      "REF", "2.6", "4.5", "8.5"   # H3
    ),
    horizon = c(
      rep("H1", 4),
      rep("H2", 4),
      rep("H3", 4)
    ),
    type = "Nationale",
    value = c(
      national_data$ref_data$ATAV_H1, national_data$s26_data$ATAV_H1, national_data$s45_data$ATAV_H1, national_data$s85_data$ATAV_H1,
      national_data$ref_data$ATAV_H2, national_data$s26_data$ATAV_H2, national_data$s45_data$ATAV_H2, national_data$s85_data$ATAV_H2,
      national_data$ref_data$ATAV_H3, national_data$s26_data$ATAV_H3, national_data$s45_data$ATAV_H3, national_data$s85_data$ATAV_H3
    )
  )
  
  # Combiner les données
  combined_data <- rbind(local_data, national_df)
  
  # Filtrer les données en gardant seulement les points non-REF
  plot_data_filtered <- combined_data %>% dplyr::filter(scenario != "REF")
  
  # Valeur de référence pour la ligne pointillée
  ref_value <- data$ref_data$ATAV_H1
  
  # Convertir scenario en facteur ordonné
  plot_data_filtered$scenario <- factor(plot_data_filtered$scenario, levels = c("2.6", "4.5", "8.5"))
  
  # Créer le graphique
  p <- ggplot(plot_data_filtered, aes(x = scenario, y = value, color = horizon, group = interaction(horizon, type), linetype = type)) +
    # Ajouter les lignes reliant les points
    geom_line(size = 1.5) +
    # Ajouter les points
    geom_point(size = 8, alpha = 0.7) +
    # Ajouter la valeur de référence comme ligne horizontale en pointillés
    geom_hline(yintercept = ref_value, linetype = "dashed", color = "gray50", size = 1.0) +
    # Ajouter l'étiquette de la valeur de référence
    annotate("text", x = 0.75, y = ref_value, label = paste("Réf:", round(ref_value, 0)), 
             hjust = 0, color = "gray50", fontface = "italic", size = 5) +
    # Définir les couleurs et les types de lignes
    scale_color_manual(values = c("H1" = "#69b3d6", "H2" = "#f89c39", "H3" = "#d93b48")) +
    scale_linetype_manual(values = c("Locale" = "solid", "Nationale" = "dotted")) +
    # Personnaliser les axes et le titre
    labs(
      title = "Évolution des précipitations annuelles (mm)",
      subtitle = paste("Données pour", commune_name, "et moyenne nationale"),
      x = "Scénario RCP",
      y = "Précipitations (mm)",
      color = "Horizon",
      linetype = "Type"
    ) +
    # Personnaliser le thème
    theme_minimal() +
    theme(
      panel.grid.minor = element_blank(),
      legend.position = "bottom",
      plot.title = element_text(hjust = 0.5, face = "bold", size = 18),
      plot.subtitle = element_text(hjust = 0.5, size = 16),
      axis.title = element_text(face = "bold", size = 16),
      axis.text = element_text(size = 14),
      legend.title = element_text(face = "bold", size = 14),
      legend.text = element_text(size = 12),
      legend.key.size = unit(1.2, "cm")
    )
  
  return(p)
}

# Fonction pour le graphique des vagues de chaleur
generate_heatwave_plot <- function(code_commune, commune_name) {
  if (is.null(code_commune) || is.null(commune_name)) {
    # Message si aucune commune n'est sélectionnée
    p <- ggplot() + 
         annotate("text", x = 0.5, y = 0.5, 
                 label = "Sélectionnez d'abord une adresse dans l'onglet 'Carte interactive'",
                 size = 4) +
         theme_void()
    return(p)
  }
  
  # Générer des données fictives pour le diagnostic
  data <- generate_mock_climate_data(code_commune)
  
  # Générer des données nationales (légèrement différentes)
  national_data <- list(
    ref_data = list(
      NORTXAV_H1 = data$ref_data$NORTXAV_H1 * 0.9,
      NORTXAV_H2 = data$ref_data$NORTXAV_H2 * 0.9,
      NORTXAV_H3 = data$ref_data$NORTXAV_H3 * 0.9
    ),
    s26_data = list(
      NORTXAV_H1 = data$s26_data$NORTXAV_H1 * 0.92,
      NORTXAV_H2 = data$s26_data$NORTXAV_H2 * 0.92,
      NORTXAV_H3 = data$s26_data$NORTXAV_H3 * 0.92
    ),
    s45_data = list(
      NORTXAV_H1 = data$s45_data$NORTXAV_H1 * 0.94,
      NORTXAV_H2 = data$s45_data$NORTXAV_H2 * 0.94,
      NORTXAV_H3 = data$s45_data$NORTXAV_H3 * 0.94
    ),
    s85_data = list(
      NORTXAV_H1 = data$s85_data$NORTXAV_H1 * 0.96,
      NORTXAV_H2 = data$s85_data$NORTXAV_H2 * 0.96,
      NORTXAV_H3 = data$s85_data$NORTXAV_H3 * 0.96
    )
  )
  
  # Préparer les données pour le graphique - Données locales
  local_data <- data.frame(
    scenario = c(
      "REF", "2.6", "4.5", "8.5",  # H1
      "REF", "2.6", "4.5", "8.5",  # H2
      "REF", "2.6", "4.5", "8.5"   # H3
    ),
    horizon = c(
      rep("H1", 4),
      rep("H2", 4),
      rep("H3", 4)
    ),
    type = "Locale",
    value = c(
      data$ref_data$NORTXAV_H1, data$s26_data$NORTXAV_H1, data$s45_data$NORTXAV_H1, data$s85_data$NORTXAV_H1,
      data$ref_data$NORTXAV_H2, data$s26_data$NORTXAV_H2, data$s45_data$NORTXAV_H2, data$s85_data$NORTXAV_H2,
      data$ref_data$NORTXAV_H3, data$s26_data$NORTXAV_H3, data$s45_data$NORTXAV_H3, data$s85_data$NORTXAV_H3
    )
  )
  
  # Préparer les données pour le graphique - Moyenne nationale
  national_df <- data.frame(
    scenario = c(
      "REF", "2.6", "4.5", "8.5",  # H1
      "REF", "2.6", "4.5", "8.5",  # H2
      "REF", "2.6", "4.5", "8.5"   # H3
    ),
    horizon = c(
      rep("H1", 4),
      rep("H2", 4),
      rep("H3", 4)
    ),
    type = "Nationale",
    value = c(
      national_data$ref_data$NORTXAV_H1, national_data$s26_data$NORTXAV_H1, national_data$s45_data$NORTXAV_H1, national_data$s85_data$NORTXAV_H1,
      national_data$ref_data$NORTXAV_H2, national_data$s26_data$NORTXAV_H2, national_data$s45_data$NORTXAV_H2, national_data$s85_data$NORTXAV_H2,
      national_data$ref_data$NORTXAV_H3, national_data$s26_data$NORTXAV_H3, national_data$s45_data$NORTXAV_H3, national_data$s85_data$NORTXAV_H3
    )
  )
  
  # Combiner les données
  combined_data <- rbind(local_data, national_df)
  
  # Filtrer les données en gardant seulement les points non-REF
  plot_data_filtered <- combined_data %>% dplyr::filter(scenario != "REF")
  
  # Valeur de référence pour la ligne pointillée
  ref_value <- data$ref_data$NORTXAV_H1
  
  # Convertir scenario en facteur ordonné
  plot_data_filtered$scenario <- factor(plot_data_filtered$scenario, levels = c("2.6", "4.5", "8.5"))
  
  # Créer le graphique
  p <- ggplot(plot_data_filtered, aes(x = scenario, y = value, color = horizon, group = interaction(horizon, type), linetype = type)) +
    # Ajouter les lignes reliant les points
    geom_line(size = 1.5) +
    # Ajouter les points
    geom_point(size = 8, alpha = 0.7) +
    # Ajouter la valeur de référence comme ligne horizontale en pointillés
    geom_hline(yintercept = ref_value, linetype = "dashed", color = "gray50", size = 1.0) +
    # Ajouter l'étiquette de la valeur de référence
    annotate("text", x = 0.75, y = ref_value, label = paste("Réf:", round(ref_value, 1)), 
             hjust = 0, color = "gray50", fontface = "italic", size = 5) +
    # Définir les couleurs et les types de lignes
    scale_color_manual(values = c("H1" = "#69b3d6", "H2" = "#f89c39", "H3" = "#d93b48")) +
    scale_linetype_manual(values = c("Locale" = "solid", "Nationale" = "dotted")) +
    # Personnaliser les axes et le titre
    labs(
      title = "Évolution du nombre de jours de vague de chaleur",
      subtitle = paste("Données pour", commune_name, "et moyenne nationale"),
      x = "Scénario RCP",
      y = "Nombre de jours",
      color = "Horizon",
      linetype = "Type"
    ) +
    # Personnaliser le thème
    theme_minimal() +
    theme(
      panel.grid.minor = element_blank(),
      legend.position = "bottom",
      plot.title = element_text(hjust = 0.5, face = "bold", size = 18),
      plot.subtitle = element_text(hjust = 0.5, size = 16),
      axis.title = element_text(face = "bold", size = 16),
      axis.text = element_text(size = 14),
      legend.title = element_text(face = "bold", size = 14),
      legend.text = element_text(size = 12),
      legend.key.size = unit(1.2, "cm")
    )
  
  return(p)
} 