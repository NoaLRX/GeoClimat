# Fonctions pour le téléchargement des données et cartes

# Fonction pour le téléchargement PDF
download_map_as_pdf <- function(file, current_map, input) {
  # Vérifier que les données sont disponibles
  if (is.null(current_map)) {
    # Créer un PDF simple avec un message d'erreur
    pdf(file, width = 11, height = 8.5)
    plot.new()
    text(0.5, 0.5, "Aucune carte disponible à exporter", cex = 1.5)
    dev.off()
    return()
  }
  
  # Extraire les données de la carte
  map_data <- current_map
  
  # Approche simplifiée: créer un PDF
  tryCatch({
    # Créer un PDF avec des dimensions standards (paysage)
    pdf(file, width = 11, height = 8.5)
    
    # Configurer la mise en page
    par(mar = c(2, 2, 4, 2))
    
    # Titre du PDF
    title_text <- paste0(
      map_data$variable_code, " - ", map_data$var_desc, "\n",
      input$scenario, " - ", input$horizon
    )
    
    # Extraire les valeurs pour la légende
    values <- map_data$data[[map_data$col_name]]
    values <- values[!is.na(values)]
    
    # Obtenir les couleurs pour chaque polygone
    if(length(values) > 0) {
      colors <- map_data$pal(sort(values))
      
      # Créer une carte simplifiée
      plot(st_geometry(map_data$data), col = map_data$pal(map_data$data[[map_data$col_name]]), 
           border = "#444444", lwd = 0.5, main = title_text)
      
      # Ajouter une légende simplifiée
      min_val <- min(values, na.rm = TRUE)
      max_val <- max(values, na.rm = TRUE)
      legend_breaks <- seq(min_val, max_val, length.out = 5)
      legend_colors <- map_data$pal(legend_breaks)
      legend_labels <- round(legend_breaks, 2)
      
      legend("bottomleft", legend = legend_labels, fill = legend_colors, 
             title = paste("Indicateur:", map_data$variable_code), cex = 0.8, bty = "n")
      
      # Ajouter des informations supplémentaires
      mtext(paste0("Format: ", if(map_data$use_departments) "Départements" else "Communes"), 
            side = 1, line = 0, adj = 0.02, cex = 0.8)
      
      # Ajouter la date de génération
      mtext(paste0("Généré le: ", format(Sys.time(), "%d/%m/%Y %H:%M")), 
            side = 1, line = 0, adj = 0.98, cex = 0.8)
    } else {
      # Si pas de données, afficher un message
      plot.new()
      text(0.5, 0.5, "Données insuffisantes pour générer la carte", cex = 1.5)
    }
    
    dev.off()
  }, error = function(e) {
    # En cas d'erreur, créer un PDF simple avec un message d'erreur
    message("Erreur lors de l'export PDF: ", e$message)
    pdf(file, width = 11, height = 8.5)
    plot.new()
    text(0.5, 0.5, paste0("Erreur: ", e$message), cex = 1.2)
    text(0.5, 0.45, "Veuillez réessayer ou contacter l'administrateur", cex = 1)
    dev.off()
  })
}

# Fonction pour le téléchargement Excel
download_data_as_excel <- function(file, session, input, scenario_files) {
  # Obtenir le fichier gpkg actuellement sélectionné
  req(input$scenario)
  
  selected_scenario <- input$scenario
  scenario_files_selected <- scenario_files[[selected_scenario]]
  
  if(length(scenario_files_selected) == 0) {
    # Créer un fichier Excel vide avec un message d'erreur
    wb <- openxlsx::createWorkbook()
    openxlsx::addWorksheet(wb, "Erreur")
    openxlsx::writeData(wb, "Erreur", "Aucune donnée disponible", startRow = 1, startCol = 1)
    openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
    return()
  }
  
  # Obtenir le chemin du fichier Excel correspondant
  gpkg_file <- scenario_files_selected[1]
  excel_file <- gsub("\\.gpkg$", ".xlsx", gpkg_file)
  
  # Vérifier si le fichier Excel existe
  if(file.exists(excel_file)) {
    # Copier le fichier Excel existant vers la destination
    file.copy(excel_file, file)
  } else {
    # Si le fichier Excel n'existe pas, créer un à partir des données GPKG
    tryCatch({
      # Lire les données du fichier GPKG
      data <- sf::st_read(gpkg_file, quiet = TRUE)
      
      # Convertir SF object en dataframe
      data_non_spatial <- sf::st_drop_geometry(data)
      
      # Créer un workbook Excel
      wb <- openxlsx::createWorkbook()
      
      # Ajouter une feuille pour les données
      openxlsx::addWorksheet(wb, "Données climatiques")
      
      # Écrire les données dans la feuille de calcul
      openxlsx::writeData(wb, "Données climatiques", data_non_spatial)
      
      # Ajouter une feuille avec des métadonnées
      openxlsx::addWorksheet(wb, "Metadata")
      
      # Ajouter des informations sur les données
      metadata <- data.frame(
        Propriété = c("Source", "Date d'extraction", "Scénario", "Format spatial"),
        Valeur = c(
          basename(gpkg_file),
          format(Sys.Date(), "%d/%m/%Y"),
          input$scenario,
          ifelse(grepl("DEPARTEMENTS", basename(gpkg_file)), "Départements", "Communes")
        )
      )
      
      openxlsx::writeData(wb, "Metadata", metadata)
      
      # Sauvegarder le workbook
      openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
      
    }, error = function(e) {
      # En cas d'erreur, créer un fichier Excel avec un message d'erreur
      wb <- openxlsx::createWorkbook()
      openxlsx::addWorksheet(wb, "Erreur")
      openxlsx::writeData(wb, "Erreur", paste("Erreur lors de la conversion des données:", e$message), 
                         startRow = 1, startCol = 1)
      openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
    })
  }
} 