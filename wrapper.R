# Wrapper pour la génération de diagnostic PDF

# Fonction pour générer le diagnostic PDF
generate_diagnostic_pdf <- function(file_path, code_commune, commune_name) {
  # Charger les bibliothèques nécessaires
  library(ggplot2)
  library(dplyr)
  library(gridExtra)
  library(cowplot)
  library(tidyr)  # Ajout pour supporter les fonctions de diagnostic_functions.R
  
  # Vérifier que diagnostic_functions.R est bien chargé
  if (!exists("generate_temperature_plot", mode="function")) {
    source("diagnostic_functions.R")
  }
  
  # Générer des données simulées pour la commune
  print(paste("Génération du diagnostic pour la commune:", commune_name, "Code:", code_commune))
  
  # Vérifier si le dossier existe et le créer si nécessaire
  dir_path <- dirname(file_path)
  if (!dir.exists(dir_path) && dir_path != ".") {
    dir.create(dir_path, recursive = TRUE, showWarnings = FALSE)
  }
  
  # S'assurer que le périphérique PDF est fermé avant de commencer
  if(names(dev.cur()) != "null device") dev.off()
  
  # Créer un PDF avec des graphiques - s'assurer que le périphérique est fermé à la fin
  tryCatch({
    # Vérifier que le code commune est valide
    if (is.null(code_commune) || is.na(code_commune) || code_commune == "") {
      code_commune <- "29000" # Code par défaut (Finistère)
      print("Code commune invalide, utilisation d'un code par défaut")
    }
    
    # Vérifier que le nom de commune est valide
    if (is.null(commune_name) || is.na(commune_name) || commune_name == "") {
      commune_name <- "Commune inconnue"
      print("Nom de commune invalide, utilisation d'un nom par défaut")
    }
    
    # Générer des graphiques en utilisant les fonctions de diagnostic_functions.R
    print("Génération des graphiques...")
    temp_plot <- generate_temperature_plot(code_commune, commune_name)
    precip_plot <- generate_precipitation_plot(code_commune, commune_name)
    heatwave_plot <- generate_heatwave_plot(code_commune, commune_name)
    
    # Créer une page de titre
    title_page <- create_title_page(commune_name, code_commune)
    
    print("Création du fichier PDF...")
    
    # Enregistrer dans un fichier PDF - Format PAYSAGE
    pdf(file_path, width = 11, height = 8.5, paper = "a4r", title = paste("Diagnostic climatique -", commune_name))
    
    # Afficher le titre en pleine page
    print(title_page)
    
    # Afficher les graphiques
    print(temp_plot)
    print(precip_plot)
    print(heatwave_plot)
    
    # Fermer explicitement le PDF
    dev.off()
    print("PDF généré avec succès")
    
  }, error = function(e) {
    # S'assurer que le périphérique est fermé en cas d'erreur
    if(names(dev.cur()) != "null device") dev.off()
    print(paste("Erreur lors de la génération du PDF:", e$message))
    
    # Créer un PDF d'erreur simple
    tryCatch({
      pdf(file_path, width = 11, height = 8.5)
      plot(c(1:10), type="n", main = paste("Diagnostique climatique pour", commune_name),
           xlab = "Horizons temporels", ylab = "Données", axes = FALSE)
      text(5, 5, paste("Commune:", commune_name, "\nCode:", code_commune, 
                       "\nDate:", format(Sys.Date(), "%d/%m/%Y"),
                       "\n\nErreur lors de la génération du diagnostic complet:",
                       e$message), cex = 1.2)
      dev.off()
    }, error = function(e2) {
      print(paste("Erreur lors de la création du PDF d'erreur:", e2$message))
    }, finally = {
      # S'assurer que le périphérique est fermé
      if(names(dev.cur()) != "null device") dev.off()
    })
    
    return(FALSE)
  }, finally = {
    # Fermer le PDF si ce n'est pas déjà fait
    if(names(dev.cur()) != "null device") dev.off()
  })
  
  # Vérifier que le fichier a bien été créé
  if (!file.exists(file_path)) {
    warning("Le fichier PDF n'a pas été créé correctement")
    return(FALSE)
  }
  
  # Vérifier la taille du fichier
  file_size <- file.info(file_path)$size
  if (is.na(file_size) || file_size < 1000) { # Moins de 1 Ko, probablement vide ou corrompu
    warning("Le fichier PDF semble vide ou corrompu")
    return(FALSE)
  }
  
  print(paste("PDF généré avec succès:", file_path))
  return(TRUE)
}

# Fonction pour créer la page de titre
create_title_page <- function(commune_name, code_commune) {
  # Créer un graphique vide pour le titre
  title_plot <- ggplot() + 
    # Titre principal
    annotate("text", x = 0.5, y = 0.9, label = paste("Diagnostic climatique pour", commune_name), 
             size = 8, fontface = "bold") +
    annotate("text", x = 0.5, y = 0.85, label = paste("Code INSEE:", code_commune), 
             size = 6) +
    
    # Sous-titre
    annotate("text", x = 0.5, y = 0.75, 
             label = "Ce diagnostic présente les projections climatiques pour votre commune", 
             size = 5) +
    annotate("text", x = 0.5, y = 0.72, 
             label = "et les compare avec les moyennes nationales.", 
             size = 5) +
    
    # Explication des horizons
    annotate("text", x = 0.5, y = 0.6, 
             label = "Les projections sont basées sur trois horizons temporels:", 
             size = 4.5) +
    annotate("text", x = 0.5, y = 0.56, 
             label = "H1 (2021-2050), H2 (2041-2070) et H3 (2071-2100)", 
             size = 4.5) +
    annotate("text", x = 0.5, y = 0.52, 
             label = "comparés à la période de référence (1976-2005).", 
             size = 4.5) +
    
    # Explication des scénarios
    annotate("text", x = 0.5, y = 0.4, 
             label = "Les scénarios RCP correspondent à différentes évolutions d'émissions de gaz à effet de serre :", 
             size = 4) +
    annotate("text", x = 0.5, y = 0.36, 
             label = "RCP 2.6 (émissions maîtrisées), RCP 4.5 (modérées), RCP 8.5 (non réduites)", 
             size = 4) +
    
    # Pied de page
    annotate("text", x = 0.5, y = 0.1, 
             label = paste("Généré le", format(Sys.time(), "%d/%m/%Y %H:%M")), 
             size = 3.5) +
    annotate("text", x = 0.5, y = 0.07, 
             label = "GéoClimat - Visualisation des données DRIAS", 
             size = 3.5, fontface = "italic") +
    
    # Suppression des éléments du graphique
    theme_void() +
    
    # Défini les limites du graphique pour s'assurer que toutes les annotations sont visibles
    xlim(0, 1) + 
    ylim(0, 1)
  
  return(title_plot)
}

# Note: Nous n'utilisons plus cette fonction car nous utilisons désormais celle de diagnostic_functions.R
# Elle est laissée ici pour référence, mais peut être supprimée si nécessaire
# generate_mock_climate_data <- function(code_commune) {
#   # Utiliser le code INSEE pour générer des valeurs pseudo-aléatoires mais cohérentes
#   code_numeric <- as.numeric(gsub("[^0-9]", "", code_commune))
#   if (is.na(code_numeric)) code_numeric <- 12345
#   set.seed(code_numeric)
#   
#   # Générer des données fictives pour les visualisations
#   data <- list(
#     temperature = list(
#       ref = runif(1, 10, 15),
#       h1 = runif(1, 11, 16),
#       h2 = runif(1, 12, 17),
#       h3 = runif(1, 13, 18)
#     ),
#     precipitation = list(
#       ref = runif(1, 600, 1000),
#       h1 = runif(1, 550, 950),
#       h2 = runif(1, 500, 900),
#       h3 = runif(1, 450, 850)
#     ),
#     heatwaves = list(
#       ref = round(runif(1, 2, 5)),
#       h1 = round(runif(1, 3, 8)),
#       h2 = round(runif(1, 5, 12)),
#       h3 = round(runif(1, 8, 20))
#     )
#   )
#   
#   return(data)
# }