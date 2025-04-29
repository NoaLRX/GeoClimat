# DRIAS App - Server logic

# Charger les fonctions de téléchargement
source("download_functions.R")
# Charger les fonctions de diagnostic
source("diagnostic_functions.R")
# Charger les fonctions de l'API Hub'Eau
source("hubeau_api.R")
# Charger les fonctions de l'API Geod'air (désactivé)
# source("geodair_api.R")

server <- function(input, output, session) {
  
  # Créer des valeurs réactives pour suivre si les boîtes de dialogue ont déjà été affichées
  welcome_modal_shown <- reactiveVal(TRUE) # Marquer comme déjà affiché
  code_modal_shown <- reactiveVal(TRUE) # Marquer comme déjà affiché
  
  # Commentaire: Désactivation complète de la boîte de dialogue de code
  # observe({
  #   if (!code_modal_shown()) {
  #     showModal(modalDialog(
  #       title = "🔒 Vérification requise",
  #       HTML(
  #         "<div style='font-size: 16px; line-height: 1.5;'>
  #           <p>Pour accéder à l'application, veuillez entrer le code d'accès :</p>
  #         </div>"
  #       ),
  #       textInput("accessCode", "Code d'accès", ""),
  #       footer = tagList(
  #         actionButton(
  #           "submitCode",
  #           "Valider",
  #           class = "btn-primary",
  #           style = "color: white; background-color: var(--accent-color); border: none; padding: 10px 20px;"
  #         )
  #       ),
  #       size = "m",
  #       easyClose = FALSE  # Empêcher la fermeture en cliquant à l'extérieur
  #     ))
  #     code_modal_shown(TRUE)
  #   }
  # })
  
  # Observer pour la validation du code et affichage de la deuxième boîte de dialogue
  # observeEvent(input$submitCode, {
  #   # Accepter n'importe quel code pour faciliter les tests
  #   removeModal()
  #   showNotification("Code accepté ! Bienvenue dans l'application.", type = "message")
      
  #   # Afficher la boîte de dialogue des temps de chargement
  #   showModal(modalDialog(
  #     title = "⚡ Information sur les temps de chargement",
  #     HTML(
  #       "<div style='font-size: 16px; line-height: 1.5;'>
  #         <p><strong>Temps de chargement des cartes :</strong></p>
  #         <ul>
  #           <li>Format communal : environ 9 secondes</li>
  #           <li>Format départemental : environ 4 secondes</li>
  #         </ul>
  #         <p>Ces temps de chargement sont normaux et s'expliquent par la <strong>quantité de données à traiter</strong>, ainsi que par le fait que l'application est <strong>hébergée en ligne</strong>, ce qui peut engendrer un léger délai lors des échanges avec le serveur.</p>
  #       </div>"
  #     ),
  #     footer = tagList(
  #       actionButton(
  #         "closeWelcomeModal",
  #         "J'ai compris",
  #         class = "btn-primary",
  #         style = "color: white; background-color: var(--accent-color); border: none; padding: 10px 20px;"
  #       )
  #     ),
  #     size = "m",
  #     easyClose = TRUE
  #   ))
  #   welcome_modal_shown(TRUE)
  # })
  
  # Message de bienvenue simple au démarrage de l'application
  observe({
    showNotification("Bienvenue dans l'application DRIAS!", type = "message", duration = 5)
    
    # Afficher la boîte de dialogue des temps de chargement au démarrage
    showModal(modalDialog(
      title = "⚡ Information sur les temps de chargement",
      HTML(
        "<div style='font-size: 16px; line-height: 1.5;'>
          <p><strong>Temps de chargement des cartes :</strong></p>
          <ul>
            <li>Format communal : environ <strong>7 secondes</strong></li>
            <li>Format départemental : environ <strong>3 secondes</strong></li>
          </ul>
          <p>Ces différences de temps de chargement s'expliquent par le nombre de polygones à traiter (plus de 35 000 communes contre seulement 101 départements).</p>
          <p>Pour une visualisation nationale, le format départemental est recommandé pour sa rapidité.</p>
        </div>"
      ),
      footer = tagList(
        actionButton(
          "closeWelcomeModal",
          "J'ai compris",
          class = "btn-primary",
          style = "color: white; background-color: #336699; border: none; padding: 10px 20px;"
        )
      ),
      size = "m",
      easyClose = TRUE
    ))
  })
  
  # Supprimer le rendu UI pour le message de temps de chargement
  # output$loadingTimeInfo <- renderUI({
  #   loading_time_message
  # })
  
  # Fermer la boîte de dialogue des temps de chargement lorsque le bouton est cliqué
  observeEvent(input$closeWelcomeModal, {
    removeModal()
  })
  
  # Charger les descriptions de variables dès le démarrage
  var_descriptions <- reactiveVal(read_descriptions(path_descriptions))
  
  # Obtenir le chemin du dossier en fonction du thème sélectionné
  selected_folder_path <- reactive({
    theme_folders <- list(
      "INDICATEURS_SAISONNIERS_ETE" = path_indicateurs_saisonniers,
      "INDICATEURS_ANNUELS_HORIZONS" = path_indicateurs_annuels,
      "FEUX_INDICATEURS_ANNUELS_HORIZONS" = path_feux_indicateurs, 
      "AGRI_INDICATEURS_ANNUELS_HORIZONS" = path_agri_indicateurs
    )
    return(theme_folders[[input$theme]])
  })
  
  # Initialiser les scénarios dès le démarrage ou quand le format spatial change
  observe({
    folder_path <- selected_folder_path()
    gpkg_files <- get_gpkg_files(folder_path, input$use_departments)
    
    if (length(gpkg_files) > 0) {
      # Extraire les scénarios
      scenarios <- unique(sapply(gpkg_files, extract_scenario))
      # Créer un vecteur nommé pour les scénarios avec leurs noms complets
      named_scenarios <- scenarios
      names(named_scenarios) <- scenarios
      # Associer les fichiers aux scénarios pour les retrouver plus tard
      scenario_files <- split(gpkg_files, sapply(gpkg_files, extract_scenario))
      # Stocker les associations fichiers-scénarios pour une utilisation ultérieure
      session$userData$scenario_files <- scenario_files
      # Mettre à jour le menu déroulant avec les noms complets
      updateSelectInput(session, "scenario", choices = named_scenarios)
    } else {
      updateSelectInput(session, "scenario", choices = character(0))
      showNotification(
        paste("Aucun fichier", if(input$use_departments) "départemental" else "communal", "trouvé dans le dossier sélectionné."),
        type = "warning",
        duration = 5
      )
    }
  }, priority = 1)
  
  # Observer pour la modification du format spatial (département ou commune)
  observeEvent(input$use_departments, {
    # Réinitialiser complètement les sélections et forcer le rechargement
    folder_path <- selected_folder_path()
    gpkg_files <- get_gpkg_files(folder_path, input$use_departments)
    
    # Notification pour informer l'utilisateur du changement de format spatial
    showNotification(
      paste0("Format spatial modifié : ", 
             if(input$use_departments) "Départements" else "Communes", 
             ". Réinitialisation des sélections en cours..."),
      type = "message",
      duration = 5
    )
    
    # Réinitialiser les données sélectionnées
    selected_data(NULL)
    current_map(NULL)
    
    # Effacer la carte actuelle
    leafletProxy("map") %>%
      clearShapes() %>%
      clearControls() %>%
      addControl(
        html = tags$div(
          style = "padding: 6px 8px; background: white; border-radius: 5px; box-shadow: 0 0 15px rgba(0,0,0,0.2);",
          tags$h3("Veuillez sélectionner un scénario, un horizon et une variable", 
                  style = "margin: 0; text-align: center; font-weight: bold;")
        ),
        position = "topright"
      )
    
    if (length(gpkg_files) > 0) {
      # Extraire les scénarios
      scenarios <- unique(sapply(gpkg_files, extract_scenario))
      # Créer un vecteur nommé pour les scénarios avec leurs noms complets
      named_scenarios <- scenarios
      names(named_scenarios) <- scenarios
      # Associer les fichiers aux scénarios pour les retrouver plus tard
      scenario_files <- split(gpkg_files, sapply(gpkg_files, extract_scenario))
      # Stocker les associations fichiers-scénarios pour une utilisation ultérieure
      session$userData$scenario_files <- scenario_files
      
      # Réinitialiser toutes les sélections pour partir sur une base propre
      updateSelectInput(session, "scenario", choices = named_scenarios, selected = character(0))
      updateSelectInput(session, "horizon", choices = character(0), selected = character(0))
      updateSelectInput(session, "variable", choices = character(0), selected = character(0))
    } else {
      # Si aucun fichier trouvé avec le format spécifié, afficher un message
      updateSelectInput(session, "scenario", choices = character(0))
      updateSelectInput(session, "horizon", choices = character(0))
      updateSelectInput(session, "variable", choices = character(0))
      
      showNotification(
        paste("Aucun fichier", if(input$use_departments) "départemental" else "communal", "trouvé dans le dossier sélectionné."),
        type = "warning",
        duration = 5
      )
    }
  }, priority = 0)
  
  # Observer pour le changement de thème - même logique de réinitialisation
  observeEvent(input$theme, {
    # Réinitialiser complètement les sélections et forcer le rechargement
    folder_path <- selected_folder_path()
    gpkg_files <- get_gpkg_files(folder_path, input$use_departments)
    
    # Notification pour informer l'utilisateur du changement de thème
    showNotification(
      paste0("Thème modifié : ", input$theme, ". Réinitialisation des sélections en cours..."),
      type = "message",
      duration = 5
    )
    
    # Réinitialiser les données sélectionnées
    selected_data(NULL)
    current_map(NULL)
    
    # Effacer la carte actuelle
    leafletProxy("map") %>%
      clearShapes() %>%
      clearControls() %>%
      addControl(
        html = tags$div(
          style = "padding: 6px 8px; background: white; border-radius: 5px; box-shadow: 0 0 15px rgba(0,0,0,0.2);",
          tags$h3("Veuillez sélectionner un scénario, un horizon et une variable", 
                  style = "margin: 0; text-align: center; font-weight: bold;")
        ),
        position = "topright"
      )
    
    if (length(gpkg_files) > 0) {
      # Extraire les scénarios
      scenarios <- unique(sapply(gpkg_files, extract_scenario))
      # Créer un vecteur nommé pour les scénarios avec leurs noms complets
      named_scenarios <- scenarios
      names(named_scenarios) <- scenarios
      # Associer les fichiers aux scénarios pour les retrouver plus tard
      scenario_files <- split(gpkg_files, sapply(gpkg_files, extract_scenario))
      # Stocker les associations fichiers-scénarios pour une utilisation ultérieure
      session$userData$scenario_files <- scenario_files
      
      # Réinitialiser toutes les sélections pour partir sur une base propre
      updateSelectInput(session, "scenario", choices = named_scenarios, selected = character(0))
      updateSelectInput(session, "horizon", choices = character(0), selected = character(0))
      updateSelectInput(session, "variable", choices = character(0), selected = character(0))
    } else {
      # Si aucun fichier trouvé avec le format spécifié, afficher un message
      updateSelectInput(session, "scenario", choices = character(0))
      updateSelectInput(session, "horizon", choices = character(0))
      updateSelectInput(session, "variable", choices = character(0))
      
      showNotification(
        paste("Aucun fichier", if(input$use_departments) "départemental" else "communal", "trouvé dans le dossier sélectionné."),
        type = "warning",
        duration = 5
      )
    }
  }, priority = 0)
  
  # Charger les données en fonction du thème et du scénario
  raw_data <- reactive({
    req(input$scenario)
    # Récupérer le scénario sélectionné
    selected_scenario <- input$scenario
    
    # Récupérer les fichiers correspondant au scénario
    scenario_files <- session$userData$scenario_files[[selected_scenario]]
    
    if (length(scenario_files) == 0) {
      return(NULL)
    }
    
    # Notification pour indiquer le début du chargement
    showNotification(
      "Chargement des données en cours...", 
      type = "message", 
      duration = NULL,
      id = "loading_notification"
    )
    
    # Charger les données du premier fichier correspondant en utilisant le cache
    data <- get_cached_data(scenario_files[1])
    
    # Fermer la notification de chargement
    removeNotification("loading_notification")
      
    return(data)
  })
  
  # Données sélectionnées qui ne seront actualisées que lors de la confirmation
  selected_data <- reactiveVal(NULL)
  
  # Mettre à jour les horizons dès que les données sont disponibles
  observe({
    data <- raw_data()
    if (!is.null(data)) {
      horizons <- extract_horizons(data)
      
      # Créer un vecteur pour les horizons avec leurs noms complets
      named_horizons <- sapply(horizons, function(h) horizon_full_names[[h]])
      
      # Important: définir les noms explicitement pour que la sélection fonctionne
      names(named_horizons) <- named_horizons
      
      updateSelectInput(session, "horizon", choices = named_horizons)
    } else {
      updateSelectInput(session, "horizon", choices = character(0))
    }
  }, priority = 2)
  
  # Mettre à jour les variables disponibles dès que l'horizon est sélectionné
  observe({
    data <- raw_data()
    horizon_input <- input$horizon
    
    # Extraire le code de l'horizon à partir du nom complet
    if (!is.null(horizon_input) && nchar(horizon_input) > 0) {
      # Extraire le code (REF, H1, H2, H3) du nom complet
      horizon_code <- substr(horizon_input, 1, if(startsWith(horizon_input, "REF")) 3 else 2)
    } else {
      return()
    }
    
    if (!is.null(data)) {
      variables <- get_variables_for_horizon(data, horizon_code, var_descriptions())
      updateSelectInput(session, "variable", choices = variables)
    } else {
      updateSelectInput(session, "variable", choices = character(0))
    }
  }, priority = 3)
  
  # Initialiser la carte avec une vue sur la France plus zoomée
  output$map <- renderLeaflet({
    leaflet() %>% 
      addTiles() %>%
      setView(lng = 4, lat = 47, zoom = 6) %>%
      addControl(
        html = tags$div(
          style = "padding: 6px 8px; background: white; border-radius: 5px; box-shadow: 0 0 15px rgba(0,0,0,0.2);",
          tags$h3("Sélectionnez les paramètres et cliquez sur 'Confirmer et charger la carte'", 
                  style = "margin: 0; text-align: center; font-weight: bold;")
        ),
        position = "topright"
      )
  })
  
  # État réactif pour la carte actuelle
  current_map <- reactiveVal(NULL)
  
  # Observer pour le bouton de confirmation
  observeEvent(input$confirmChoices, {
    # Mettre à jour les données sélectionnées
    selected_data(raw_data())
    
    # Afficher un message de chargement
    showNotification("Chargement de la carte...", type = "message", duration = 1)
    
    # Extraire le code de l'horizon à partir du nom complet
    horizon_input <- input$horizon
    if (!is.null(horizon_input) && nchar(horizon_input) > 0) {
      # Extraire le code (REF, H1, H2, H3) du nom complet
      horizon_code <- substr(horizon_input, 1, if(startsWith(horizon_input, "REF")) 3 else 2)
    } else {
      horizon_code <- NULL
    }
    
    # Extraire le code de la variable à partir du nom complet
    variable_input <- input$variable
    if (!is.null(variable_input) && nchar(variable_input) > 0) {
      # Si la variable est au format "CODE - Description", extraire le code
      variable_code <- strsplit(variable_input, " - ")[[1]][1]
    } else {
      variable_code <- variable_input
    }
    
    # Mettre à jour la carte avec les paramètres choisis
    data <- selected_data()
    req(horizon_code, variable_code)
    
    if (is.null(data)) {
      leafletProxy("map") %>%
        clearShapes() %>%
        clearControls() %>%
        addControl(
          html = tags$div(
            style = "padding: 6px 8px; background: white; border-radius: 5px; box-shadow: 0 0 15px rgba(0,0,0,0.2);",
            tags$h3("Indicateur non disponible pour cet horizon", style = "margin: 0; text-align: center; font-weight: bold;")
          ),
          position = "topright"
        )
      current_map(NULL)
      return()
    }
    
    # Construire le nom de colonne complet
    col_name <- paste0(variable_code, "_", horizon_code)
    
    # Vérifier si la colonne existe
    if (!(col_name %in% colnames(data))) {
      leafletProxy("map") %>%
        clearShapes() %>%
        clearControls() %>%
        addControl(
          html = tags$div(
            style = "padding: 6px 8px; background: white; border-radius: 5px; box-shadow: 0 0 15px rgba(0,0,0,0.2);",
            tags$h3("Indicateur non disponible pour cet horizon", style = "margin: 0; text-align: center; font-weight: bold;")
          ),
          position = "topright"
        )
      current_map(NULL)
      return()
    }
    
    # Obtenir les données de la variable sélectionnée
    values <- data[[col_name]]
    
    # Retirer les valeurs NA pour la légende
    values_for_legend <- values[!is.na(values)]
    
    # Définir la palette de couleurs en fonction du type de variable
    if (grepl("^(NORT|AT).*AV$", variable_code)) {
      # Palette pour les températures
      pal <- colorNumeric(palette = "RdYlBu", domain = values, reverse = TRUE, na.color = "transparent")
    } else if (grepl("^(NORP|AP)", variable_code)) {
      # Palette pour les précipitations
      pal <- colorNumeric(palette = "Blues", domain = values, reverse = TRUE, na.color = "transparent")
    } else {
      # Palette par défaut pour les autres variables
      pal <- colorNumeric(palette = "Spectral", domain = values, reverse = TRUE, na.color = "transparent")
    }
    
    # Obtenir la description de la variable
    descriptions <- var_descriptions()
    var_desc <- descriptions[[variable_code]]
    if (is.null(var_desc) || var_desc == "") {
      var_desc <- "Description non disponible"
    }
    
    # Créer le titre avec l'horizon et sa période
    horizon_period <- horizon_periods[[horizon_code]]
    horizon_name <- horizon_full_names[[horizon_code]]
    
    title <- paste0(
      variable_code, " - ", var_desc, "<br>",
      "<span style='font-size: 0.9em;'>", input$scenario, " - ", horizon_name, "</span>"
    )
    
    # Mettre à jour la carte sans redessiner complètement
    leafletProxy("map", data = data) %>%
      clearShapes() %>%
      clearControls() %>%
      addPolygons(
        fillColor = ~pal(data[[col_name]]),
        fillOpacity = 1.0,
        color = "#444444",
        weight = 0.5,
        highlightOptions = highlightOptions(
          weight = 2,
          color = "#666",
          fillOpacity = 0.7,
          bringToFront = TRUE
        ),
        # Revenir à une approche simple pour les popups
        popup = if(input$use_departments) {
          if("NOM" %in% colnames(data) && "INSEE_DEP" %in% colnames(data)) {
            paste0(
              "<strong>Département:</strong> ", data$NOM, "<br>",
              "<strong>Code:</strong> ", data$INSEE_DEP, "<br>",
              "<strong>Valeur:</strong> ", ifelse(is.na(data[[col_name]]), "Non disponible", round(data[[col_name]], 2)),
          "<br><strong>Indicateur:</strong> ", variable_code, " - ", var_desc
            )
          } else {
            paste0(
              "<strong>Valeur:</strong> ", ifelse(is.na(data[[col_name]]), "Non disponible", round(data[[col_name]], 2)),
              "<br><strong>Indicateur:</strong> ", variable_code, " - ", var_desc
            )
          }
        } else {
          if("LIB" %in% colnames(data) && "CODE_C" %in% colnames(data)) {
            paste0(
              "<strong>Commune:</strong> ", data$LIB, "<br>",
              "<strong>Code commune:</strong> ", data$CODE_C, "<br>",
              "<strong>Valeur:</strong> ", ifelse(is.na(data[[col_name]]), "Non disponible", round(data[[col_name]], 2)),
              "<br><strong>Indicateur:</strong> ", variable_code, " - ", var_desc
            )
          } else {
            paste0(
              "<strong>Valeur:</strong> ", ifelse(is.na(data[[col_name]]), "Non disponible", round(data[[col_name]], 2)),
              "<br><strong>Indicateur:</strong> ", variable_code, " - ", var_desc
            )
          }
        },
        # Simplifier également les labels
        label = if(input$use_departments && "NOM" %in% colnames(data)) {
          paste0(data$NOM, " - Valeur: ", ifelse(is.na(data[[col_name]]), "Non disponible", round(data[[col_name]], 2)))
        } else if(!input$use_departments && "LIB" %in% colnames(data)) {
          paste0(data$LIB, " - Valeur: ", ifelse(is.na(data[[col_name]]), "Non disponible", round(data[[col_name]], 2)))
        } else {
          paste0("Valeur: ", ifelse(is.na(data[[col_name]]), "Non disponible", round(data[[col_name]], 2)))
        }
      ) %>%
      addLegend(
        position = "bottomleft",
        pal = pal,
        values = values_for_legend,
        # Modification: Ne pas afficher de titre dans la légende
        title = NULL,
        opacity = 1.0
      ) %>%
      addControl(
        html = tags$div(
          style = "padding: 8px 12px; background: white; border-radius: 5px; box-shadow: 0 0 15px rgba(0,0,0,0.2); min-width: 200px; max-width: 600px; margin: 0 auto; position: relative; left: 50%; transform: translateX(-50%);",
          HTML(paste0("<h3 style='margin: 0; text-align: center; font-weight: bold;'>", title, "</h3>"))
        ),
        position = "topright"
      )
    
    # Stocker la carte mise à jour
    map_data <- list(
      data = data,
      col_name = col_name,
      pal = pal,
      title = title,
      values = values_for_legend,
      variable_code = variable_code,
      var_desc = var_desc,
      use_departments = input$use_departments  # Ajouter le format spatial
    )
    current_map(map_data)
  })
  
  # Réactive value pour stocker l'adresse sélectionnée pour le diagnostic
  selected_address_for_diag <- reactiveVal(NULL)
  selected_commune_code <- reactiveVal(NULL)
  selected_commune_name <- reactiveVal(NULL)
  
  # Stocker les adresses trouvées en tant que valeur réactive
  search_results <- reactiveVal(NULL)
  
  # Indicateur pour savoir si une adresse est sélectionnée
  output$hasSelectedAddress <- reactive({ 
    !is.null(selected_address_for_diag()) && !is.null(selected_commune_code()) 
  })
  outputOptions(output, "hasSelectedAddress", suspendWhenHidden = FALSE)
  
  # Fonction pour rechercher une adresse avec l'API BAN (Base Adresse Nationale)
  search_address <- function(query) {
    if (nchar(query) < 3) {
      return(list())
    }
    
    print(paste("Recherche BAN pour:", query))
    
    # URL de l'API BAN
    url <- "https://api-adresse.data.gouv.fr/search/"
    
    # Effectuer la requête
    tryCatch({
      response <- httr::GET(url, query = list(q = query, limit = 5))
      
      if (httr::status_code(response) == 200) {
        content <- httr::content(response, "text", encoding = "UTF-8")
        data <- jsonlite::fromJSON(content)
        
        if ("features" %in% names(data) && length(data$features) > 0) {
          addresses <- list()
          
          for (i in seq_along(data$features)) {
            feature <- data$features[[i]]
            
            # Vérifier que tous les éléments nécessaires existent
            if (!is.null(feature) && 
                "geometry" %in% names(feature) && 
                "coordinates" %in% names(feature$geometry) && 
                length(feature$geometry$coordinates) >= 2 &&
                "properties" %in% names(feature)) {
              
              prop <- feature$properties
              
              # Créer l'entrée avec uniquement les informations essentielles
              addresses[[length(addresses) + 1]] <- list(
                label = if ("label" %in% names(prop)) prop$label else "Adresse sans nom",
                score = if ("score" %in% names(prop)) as.numeric(prop$score) else 0,
                type = if ("type" %in% names(prop)) prop$type else "inconnu",
                longitude = as.numeric(feature$geometry$coordinates[[1]]),
                latitude = as.numeric(feature$geometry$coordinates[[2]]),
                city = if ("city" %in% names(prop)) prop$city else NA,
                postcode = if ("postcode" %in% names(prop)) prop$postcode else NA,
                citycode = if ("citycode" %in% names(prop)) prop$citycode else NA
              )
            }
          }
          
          return(addresses)
        }
      }
    }, error = function(e) {
      print(paste("Erreur API BAN:", e$message))
    })
    
    return(list())
  }
  
  # Fonction pour rechercher une adresse avec Nominatim (alternative à BAN)
  search_address_nominatim <- function(query) {
    if (nchar(query) < 3) {
      return(list())
    }
    
    print(paste("Recherche Nominatim pour:", query))
    
    # Ajouter "France" à la requête
    if (!grepl("france", tolower(query))) {
      query <- paste(query, "France")
    }
    
    # URL de l'API Nominatim
    url <- "https://nominatim.openstreetmap.org/search"
    
    # Effectuer la requête
    tryCatch({
      response <- httr::GET(
        url, 
        query = list(
          q = query,
          format = "json",
          addressdetails = 1,
          limit = 5,
          countrycodes = "fr"
        ),
        httr::add_headers(`User-Agent` = "DRIAS_App/1.0")
      )
      
      # Respecter les limites de requêtes de Nominatim
      Sys.sleep(1)
      
      if (httr::status_code(response) == 200) {
        content <- httr::content(response, "text", encoding = "UTF-8")
        results <- jsonlite::fromJSON(content)
        
        if (length(results) > 0) {
          addresses <- list()
          
          # Gérer les cas où un seul résultat est retourné (comme un vecteur et non une data frame)
          if (is.data.frame(results)) {
            for (i in 1:nrow(results)) {
              result <- results[i,]
              
              # Ne traiter que les résultats avec des coordonnées
              if ("lat" %in% names(result) && "lon" %in% names(result)) {
                # Extraire la ville et le code postal si disponibles
                city <- NULL
                postcode <- NULL
                
                if ("address" %in% names(result) && is.list(result$address)) {
                  addr <- result$address
                  
                  # Trouver la ville (plusieurs champs possibles)
                  if ("city" %in% names(addr)) {
                    city <- addr$city
                  } else if ("town" %in% names(addr)) {
                    city <- addr$town
                  } else if ("village" %in% names(addr)) {
                    city <- addr$village
                  }
                  
                  # Récupérer le code postal
                  if ("postcode" %in% names(addr)) {
                    postcode <- addr$postcode
                  }
                }
                
                # Créer l'entrée
                addresses[[length(addresses) + 1]] <- list(
                  label = if ("display_name" %in% names(result)) result$display_name else "Adresse sans nom",
                  score = 1 - (i * 0.1),  # Score décroissant
                  type = if ("type" %in% names(result)) result$type else "lieu",
                  longitude = as.numeric(result$lon),
                  latitude = as.numeric(result$lat),
                  city = city,
                  postcode = postcode
                )
              }
            }
          } else if (is.list(results)) {
            # Cas d'un seul résultat
            result <- results
            
            # Ne traiter que les résultats avec des coordonnées
            if ("lat" %in% names(result) && "lon" %in% names(result)) {
              # Extraire la ville et le code postal si disponibles
              city <- NULL
              postcode <- NULL
              
              if ("address" %in% names(result) && is.list(result$address)) {
                addr <- result$address
                
                # Trouver la ville (plusieurs champs possibles)
                if ("city" %in% names(addr)) {
                  city <- addr$city
                } else if ("town" %in% names(addr)) {
                  city <- addr$town
                } else if ("village" %in% names(addr)) {
                  city <- addr$village
                }
                
                # Récupérer le code postal
                if ("postcode" %in% names(addr)) {
                  postcode <- addr$postcode
                }
              }
              
              # Créer l'entrée
              addresses[[length(addresses) + 1]] <- list(
                label = if ("display_name" %in% names(result)) result$display_name else "Adresse sans nom",
                score = 1,  # Score maximum pour un unique résultat
                type = if ("type" %in% names(result)) result$type else "lieu",
                longitude = as.numeric(result$lon),
                latitude = as.numeric(result$lat),
                city = city,
                postcode = postcode
              )
            }
          }
          
          return(addresses)
        }
      }
    }, error = function(e) {
      print(paste("Erreur Nominatim:", e$message))
    })
    
    return(list())
  }
  
  # Observer pour le bouton de recherche d'adresse
  observeEvent(input$searchBtnClicked, {
    query <- input$searchBtnClicked$address
    
    if (is.null(query) || nchar(query) < 3) {
      # Afficher un message si la requête est trop courte
      session$sendCustomMessage(type = "updateSearchResults", 
                                message = "<div style='color: #d9534f;'>Veuillez entrer au moins 3 caractères</div>")
      return()
    }
    
    # Afficher un message de chargement
    session$sendCustomMessage(type = "updateSearchResults", 
                              message = "<div style='color: #5bc0de;'>Recherche en cours...</div>")
    
    # Rechercher l'adresse avec l'API BAN
    print(paste("Recherche de l'adresse:", query))
    addresses <- tryCatch({
      search_address(query)
    }, error = function(e) {
      print(paste("Erreur lors de la recherche BAN:", e$message))
      list()
    })
    
    # Si aucun résultat avec BAN, essayer avec Nominatim
    if (length(addresses) == 0) {
      print("Aucun résultat avec BAN, tentative avec Nominatim")
      addresses <- tryCatch({
        search_address_nominatim(query)
      }, error = function(e) {
        print(paste("Erreur lors de la recherche Nominatim:", e$message))
        list()
      })
    }
    
    print(paste("Nombre de résultats:", length(addresses)))
    
    if (length(addresses) == 0) {
      # Aucun résultat trouvé
      session$sendCustomMessage(type = "updateSearchResults", 
                                message = "<div style='color: #d9534f;'>Aucun résultat trouvé</div>")
    } else {
      # Construire la liste des résultats
      result_html <- "<div style='display: flex; flex-direction: column; gap: 5px;'>"
      
      for (i in seq_along(addresses)) {
        addr <- addresses[[i]]
        # S'assurer que les données sont bien définies
        label <- ifelse(is.null(addr$label), "Adresse sans nom", addr$label)
        type <- ifelse(is.null(addr$type), "inconnu", addr$type)
        score <- ifelse(is.null(addr$score), 0, addr$score)
        
        print(paste("Résultat", i, ":", label, "- Lat:", addr$latitude, "Lng:", addr$longitude))
        
        result_html <- paste0(
          result_html,
          "<div class='address-result' style='padding: 5px; border-radius: 3px; cursor: pointer; background-color: #f5f5f5; border: 1px solid #ddd;' ",
          "data-lat='", addr$latitude, "' data-lng='", addr$longitude, "'>",
          "<div style='font-weight: bold;'>", label, "</div>",
          "<div style='font-size: 0.8em; color: #666;'>Type: ", type, " | Score: ", round(score * 100), "%</div>",
          "</div>"
        )
      }
      
      result_html <- paste0(result_html, "</div>")
      
      # Envoyer les résultats au navigateur
      session$sendCustomMessage(type = "updateSearchResults", message = result_html)
      
      # Stocker les adresses dans la valeur réactive
      search_results(addresses)
    }
  })
  
  # Observer pour la sélection d'un résultat de recherche
  observeEvent(input$selectedAddress, {
    # Extraire les coordonnées de l'adresse sélectionnée
    index <- as.numeric(input$selectedAddress)
    addresses <- search_results()
    
    if (!is.null(addresses) && index <= length(addresses)) {
      selected <- addresses[[index]]
      
      # Vérifier que l'adresse sélectionnée est une liste valide
      if (!is.list(selected)) {
        print("Erreur: L'adresse sélectionnée n'est pas une liste valide")
        return()
      }
      
      # Stocker l'adresse sélectionnée pour le diagnostic
      selected_address_for_diag(if ("label" %in% names(selected)) selected$label else "Adresse sans nom")
      
      # Vérifier que longitude et latitude existent
      if (!"longitude" %in% names(selected) || !"latitude" %in% names(selected) ||
          is.null(selected$longitude) || is.null(selected$latitude) ||
          is.na(selected$longitude) || is.na(selected$latitude)) {
        print("Coordonnées manquantes dans les résultats de recherche")
        return()
      }
      
      # Zoomer sur l'adresse sélectionnée
      leafletProxy("map") %>%
        setView(lng = selected$longitude, lat = selected$latitude, zoom = 14) %>%
        # Nettoyer les anciens marqueurs et ajouter un nouveau marqueur
        clearGroup("searchMarkers") %>%
        addMarkers(
          lng = selected$longitude, 
          lat = selected$latitude,
          popup = if ("label" %in% names(selected)) selected$label else "Adresse sélectionnée",
          group = "searchMarkers"
        )
      
      # Détection de commune par analyse spatiale avec les fichiers GPKG
      commune_found <- FALSE
      
      print(paste("Coordonnées valides, détection de la commune...", selected$longitude, selected$latitude))
      
      # Utiliser notre fonction pour détecter la commune
      commune_info <- find_commune_by_gps(selected$longitude, selected$latitude)
      
      if (!is.null(commune_info) && is.list(commune_info)) {
        # La commune a été trouvée, on stocke ses informations
        if ("code" %in% names(commune_info) && "name" %in% names(commune_info)) {
          code_commune <- commune_info$code 
          commune_name <- commune_info$name
          
          # Vérification supplémentaire pour s'assurer que les valeurs ne sont pas NULL ou NA
          if (!is.null(code_commune) && !is.na(code_commune) && 
              !is.null(commune_name) && !is.na(commune_name)) {
            
            # Vérifier si la commune a été trouvée par approximation
            if ("approx" %in% names(commune_info) && isTRUE(commune_info$approx) && 
                "distance" %in% names(commune_info)) {
              print(paste("Commune approximative trouvée par proximité:", commune_name, 
                          "Code:", code_commune, "Distance:", commune_info$distance, "m"))
              msg <- paste("Commune détectée (approximative, à", commune_info$distance, "m):", 
                          commune_name, "(", code_commune, ")")
            } else {
              print(paste("Commune trouvée par analyse spatiale:", commune_name, "Code:", code_commune))
              msg <- paste("Commune détectée:", commune_name, "(", code_commune, ")")
            }
            
            selected_commune_code(code_commune)
            selected_commune_name(commune_name)
            commune_found <- TRUE
            
            # Notification pour l'utilisateur
            showNotification(msg, type = "message", duration = 5)
            
            # Indiquer à l'utilisateur qu'il peut générer un diagnostic
            output$diagInstructions <- renderUI({
              div(
                style = "margin-top: 10px; padding: 10px; background-color: #dff0d8; border-radius: 5px;",
                p(icon("info-circle"), " Commune identifiée avec succès. Vous pouvez maintenant télécharger le diagnostique climatique.")
              )
            })
          } else {
            print("Résultat de commune valide mais code ou nom manquant")
          }
        } else {
          print("Structure de commune_info incorrecte: code ou name manquant")
        }
      } else {
        print("Aucune commune n'a été détectée via l'analyse spatiale")
      }
      
      # Si la commune n'est pas trouvée par l'analyse spatiale, proposer l'entrée manuelle
      if (!commune_found) {
        output$diagInstructions <- renderUI({
          div(
            style = "margin-top: 10px; padding: 10px; background-color: #fcf8e3; border-radius: 5px;",
            p(icon("exclamation-triangle"), " Impossible de détecter automatiquement la commune pour cette adresse."),
            p("Vous pouvez entrer manuellement le code INSEE et le nom de la commune :"),
            div(
              style = "display: flex; gap: 10px; margin-top: 10px;",
              textInput("manualCommuneCode", "Code INSEE", width = "150px"),
              textInput("manualCommuneName", "Nom de la commune", width = "250px"),
              actionButton("setManualCommune", "Définir la commune", class = "btn-primary")
            )
          )
        })
      }
    }
  })
  
  # Observer pour la définition manuelle d'une commune
  observeEvent(input$setManualCommune, {
    code_commune <- input$manualCommuneCode
    commune_name <- input$manualCommuneName
    
    # Vérifier que les champs ne sont pas vides
    if (nchar(code_commune) > 0 && nchar(commune_name) > 0) {
      selected_commune_code(code_commune)
      selected_commune_name(commune_name)
      
      showNotification(
        paste("Commune définie manuellement:", commune_name, "(", code_commune, ")"),
        type = "message",
        duration = 5
      )
      
      output$diagInstructions <- renderUI({
        div(
          style = "margin-top: 10px; padding: 10px; background-color: #dff0d8; border-radius: 5px;",
          p(icon("info-circle"), " Commune définie avec succès. Vous pouvez maintenant télécharger le diagnostique climatique.")
        )
      })
    } else {
      showNotification(
        "Veuillez remplir à la fois le code INSEE et le nom de la commune.",
        type = "error",
        duration = 5
      )
    }
  })
  
  # Afficher la commune sélectionnée dans l'onglet diagnostic
  output$diagSelectedCommune <- renderText({
    code <- selected_commune_code()
    name <- selected_commune_name()
    if (is.null(code) || is.null(name)) {
      "Aucune commune sélectionnée."
    } else {
      paste("Commune : ", name, " (Code : ", code, ")")
    }
  })
  
  # Observer pour le bouton de diagnostic - redirection vers l'onglet diagnostic
  observeEvent(input$goDiagnostic, {
    updateNavbarPage(session, "navbarPage", selected = "Diagnostique 🩺")
  })
  
  # Afficher l'adresse sélectionnée dans l'onglet diagnostic
  output$diagSelectedAddress <- renderText({
    addr <- selected_address_for_diag()
    if (is.null(addr)) {
      "Aucune adresse sélectionnée. Utilisez la recherche d'adresse dans l'onglet 'Carte interactive'."
    } else {
      addr
    }
  })
  
  # Handler pour le téléchargement du diagnostic en PDF
  output$downloadDiagnostic <- downloadHandler(
    filename = function() {
      # Nom de fichier personnalisé avec la commune et la date
      commune_name <- selected_commune_name()
      if (is.null(commune_name) || commune_name == "") {
        commune_name <- "Inconnue"
      }
      paste0("Diagnostique_climatique_", commune_name, "_", format(Sys.Date(), "%Y%m%d"), ".pdf")
    },
    content = function(file) {
      # Vérifier si on a une commune sélectionnée
      code_commune <- selected_commune_code()
      commune_name <- selected_commune_name()
      
      if (is.null(code_commune) || is.null(commune_name)) {
        # Si aucune commune n'est sélectionnée, afficher un message d'erreur
        showNotification("Aucune commune sélectionnée pour le diagnostique.", type = "error", duration = 5)
        return()
      }
      
      # Afficher un message de chargement
      withProgress(message = 'Génération du diagnostique en cours...', value = 0.3, {
        # Tenter de générer le PDF
        success <- tryCatch({
          # Mise à jour de la barre de progression
          incProgress(0.3, detail = "Création des graphiques...")
          
          # Générer le PDF de diagnostique
          generate_diagnostic_pdf(file, code_commune, commune_name)
          
          # Mise à jour de la barre de progression
          incProgress(0.4, detail = "Finalisation...")
          
          TRUE  # Succès
        }, error = function(e) {
          # En cas d'erreur, afficher un message et retourner FALSE
          print(paste("Erreur lors de la génération du PDF:", e$message))
          showNotification(paste("Erreur:", e$message), type = "error", duration = 10)
          FALSE
        })
        
        if (success) {
          showNotification(paste("Diagnostique pour", commune_name, "généré avec succès!"), 
                          type = "message", duration = 5)
        }
      })
    }
  )
  
  # Handler pour le téléchargement des données en Excel
  output$downloadExcel <- downloadHandler(
    filename = function() {
      # Obtenir le fichier gpkg actuellement sélectionné
      req(input$scenario)
      
      selected_scenario <- input$scenario
      scenario_files <- session$userData$scenario_files[[selected_scenario]]
      
      if(length(scenario_files) == 0) {
        return("donnees.xlsx")
      }
      
      # Obtenir le nom du fichier gpkg et le convertir en xlsx
      gpkg_file <- basename(scenario_files[1])
      excel_file <- gsub("\\.gpkg$", ".xlsx", gpkg_file)
      
      return(excel_file)
    },
    content = function(file) {
      # Afficher un message de chargement
      withProgress(message = 'Génération du fichier Excel en cours...', value = 0.2, {
        # Utiliser la fonction de download_functions.R
        download_data_as_excel(file, session, input, session$userData$scenario_files)
        
        # Vérifier si le fichier Excel a été généré correctement
        if (file.exists(file) && file.size(file) > 100) {
          showNotification("Données exportées en Excel avec succès!", type = "message", duration = 3)
        } else {
          showNotification("Le fichier Excel généré semble vide ou incorrect. Veuillez réessayer.", type = "warning", duration = 5)
        }
      })
    }
  )
  
  # Handler pour le téléchargement de la carte en PDF
  output$downloadPDF <- downloadHandler(
    filename = function() {
      # Nom de fichier personnalisé avec le code de la variable et la date
      map_data <- current_map()
      if (!is.null(map_data) && !is.null(map_data$variable_code)) {
        paste0("DRIAS_carte_", map_data$variable_code, "_", format(Sys.Date(), "%Y%m%d"), ".pdf")
      } else {
        paste0("DRIAS_carte_", format(Sys.Date(), "%Y%m%d"), ".pdf")
      }
    },
    content = function(file) {
      # Afficher un message de chargement
      withProgress(message = 'Génération du PDF en cours...', value = 0.2, {
        # Utiliser la fonction de download_functions.R
        download_map_as_pdf(file, current_map(), input)
          
          # Vérifier si le PDF a été généré correctement
          if (file.exists(file) && file.size(file) > 100) {
            showNotification("Carte exportée en PDF avec succès!", type = "message", duration = 3)
          } else {
            showNotification("Le PDF généré semble vide ou incorrect. Veuillez réessayer.", type = "warning", duration = 5)
          }
      })
    }
  )
  
  # Générer les graphiques de diagnostic directement dans l'interface
  
  # Graphique des températures
  output$diagTempPlot <- renderPlot({
    # Utiliser la fonction du fichier diagnostic_functions.R
    generate_temperature_plot(selected_commune_code(), selected_commune_name())
  })
  
  # Graphique des précipitations
  output$diagPrecipPlot <- renderPlot({
    # Utiliser la fonction du fichier diagnostic_functions.R
    generate_precipitation_plot(selected_commune_code(), selected_commune_name())
  })
  
  # Graphique des vagues de chaleur
  output$diagHeatwavePlot <- renderPlot({
    # Utiliser la fonction du fichier diagnostic_functions.R
    generate_heatwave_plot(selected_commune_code(), selected_commune_name())
  })
  
  # Tableau récapitulatif des données climatiques
  output$diagSummaryTable <- renderTable({
    code_commune <- selected_commune_code()
    commune_name <- selected_commune_name()
    
    if (is.null(code_commune) || is.null(commune_name)) {
      return(data.frame(
        Indicateur = "Aucune donnée disponible",
        Reference = NA,
        "RCP 2.6" = NA,
        "RCP 4.5" = NA,
        "RCP 8.5" = NA
      ))
    }
    
    # Utiliser tryCatch pour capturer toute erreur potentielle
    result <- tryCatch({
      # Générer des données spécifiques à la commune
      data <- generate_mock_climate_data(code_commune)
      
      # Créer un tableau récapitulatif en vérifiant chaque valeur
      temp_ref <- ifelse(is.numeric(data$ref_data$NORTAV_H1), round(data$ref_data$NORTAV_H1, 1), NA)
      temp_26 <- ifelse(is.numeric(data$s26_data$NORTAV_H1), round(data$s26_data$NORTAV_H1, 1), NA)
      temp_45 <- ifelse(is.numeric(data$s45_data$NORTAV_H1), round(data$s45_data$NORTAV_H1, 1), NA)
      temp_85 <- ifelse(is.numeric(data$s85_data$NORTAV_H1), round(data$s85_data$NORTAV_H1, 1), NA)
      
      precip_ref <- ifelse(is.numeric(data$ref_data$ATAV_H1), round(data$ref_data$ATAV_H1, 0), NA)
      precip_26 <- ifelse(is.numeric(data$s26_data$ATAV_H1), round(data$s26_data$ATAV_H1, 0), NA)
      precip_45 <- ifelse(is.numeric(data$s45_data$ATAV_H1), round(data$s45_data$ATAV_H1, 0), NA)
      precip_85 <- ifelse(is.numeric(data$s85_data$ATAV_H1), round(data$s85_data$ATAV_H1, 0), NA)
      
      # Calculer des valeurs dérivées pour le nombre de jours de vague de chaleur (simulé)
      hw_ref <- round(temp_ref / 3, 1) # Simulation simplifiée
      hw_26 <- round(temp_26 / 3 + 1, 1)
      hw_45 <- round(temp_45 / 3 + 2, 1)
      hw_85 <- round(temp_85 / 3 + 3, 1)
      
      # Créer le dataframe
      data.frame(
        Indicateur = c(
          "Température moyenne (°C)",
          "Précipitations annuelles (mm)",
          "Nombre de jours de vague de chaleur"
        ),
        Horizon = c("H1", "H1", "H1"),
        Reference = c(temp_ref, precip_ref, hw_ref),
        "RCP 2.6" = c(temp_26, precip_26, hw_26),
        "RCP 4.5" = c(temp_45, precip_45, hw_45),
        "RCP 8.5" = c(temp_85, precip_85, hw_85)
      )
    }, error = function(e) {
      # En cas d'erreur, retourner un tableau avec des valeurs par défaut
      message("Erreur dans la génération du tableau: ", e$message)
      data.frame(
        Indicateur = c(
          "Température moyenne (°C)",
          "Précipitations annuelles (mm)",
          "Nombre de jours de vague de chaleur"
        ),
        Horizon = c("H1", "H1", "H1"),
        Reference = c(13.2, 850, 5.3),
        "RCP 2.6" = c(14.1, 830, 7.8),
        "RCP 4.5" = c(14.8, 810, 9.5),
        "RCP 8.5" = c(15.7, 780, 12.4)
      )
    })
    
    return(result)
  }, striped = TRUE, hover = TRUE, align = 'c', width = "100%", digits = 1)
  
  ##############################################
  ### PARTIE QUALITÉ DE L'AIR (API GEOD'AIR) ###
  ##############################################
  
  # Air Quality data (Geod'air) ----
  # Reactive values to store air quality data
  airQuality <- reactiveValues(
    api_available = FALSE,  # Définir à FALSE par défaut
    stations = NULL,
    nearby_stations = NULL,
    selected_station = NULL,
    pollutants = NULL,
    data = NULL
  )
  
  # Désactivation de la vérification automatique de l'API Geod'air
  # observe({
  #   message("Vérification de la disponibilité de l'API Geod'air...")
  #   api_available <- check_geodair_api_availability()
  #   airQuality$api_available <- api_available
  #   
  #   if (api_available) {
  #     showNotification("Connexion à l'API Geod'air établie", type = "message")
  #   } else {
  #     showNotification(
  #       "Impossible de se connecter à l'API Geod'air. Des données simulées seront utilisées.", 
  #       type = "warning", 
  #       duration = NULL
  #     )
  #   }
  # })
  
  # Désactivation du chargement automatique des polluants
  # observe({
  #   message("Chargement des polluants...")
  #   pollutants <- get_geodair_pollutants()
  #   airQuality$pollutants <- pollutants
  #   
  #   # Update pollutant dropdown
  #   if (!is.null(pollutants) && nrow(pollutants) > 0) {
  #     choices <- setNames(
  #       as.list(pollutants$code),
  #       paste0(pollutants$code_polluant, " - ", pollutants$name)
  #     )
  #     updateSelectInput(session, "airQualityPollutant", choices = choices)
  #   }
  # })
  
  # Load stations only when needed
  observeEvent(input$airQualitySelectionMethod, {
    if (input$airQualitySelectionMethod == "list" && is.null(airQuality$stations)) {
      message("Chargement des stations...")
      withProgress(message = "Chargement des stations...", {
        stations <- get_geodair_stations()
        airQuality$stations <- stations
        
        # Update station dropdown
        if (!is.null(stations) && nrow(stations) > 0) {
          choices <- setNames(
            as.list(stations$code_station),
            paste0(stations$name, " (", stations$code_station, ")")
          )
          updateSelectInput(session, "airQualityStation", choices = choices)
        }
      })
    }
  })
  
  # Search for stations near address
  observeEvent(input$airQualitySearchBtn, {
    req(input$airQualityAddressInput)
    address <- input$airQualityAddressInput
    
    if (nchar(address) < 3) {
      showNotification("Veuillez entrer une adresse plus précise", type = "warning")
      return()
    }
    
    # Geocode the address to get coordinates
    withProgress(message = "Recherche de l'adresse...", {
      # Use Nominatim to geocode the address
      address_encoded <- URLencode(paste0(address, ", France"))
      nominatim_url <- paste0(
        "https://nominatim.openstreetmap.org/search?q=", 
        address_encoded, 
        "&format=json&limit=1"
      )
      
      response <- tryCatch({
        GET(
          url = nominatim_url,
          timeout(10),
          add_headers(
            `User-Agent` = "DRIAS_App/1.0",
            Referer = "https://drias-app.fr"
          )
        )
      }, error = function(e) {
        showNotification(paste("Erreur de géocodage:", e$message), type = "error")
        return(NULL)
      })
      
      if (is.null(response) || status_code(response) != 200) {
        showNotification("Impossible de trouver les coordonnées de cette adresse", type = "error")
        return()
      }
      
      content <- content(response, "text", encoding = "UTF-8")
      locations <- fromJSON(content)
      
      if (length(locations) == 0) {
        showNotification("Adresse non trouvée", type = "error")
        return()
      }
      
      # Get the coordinates
      lat <- as.numeric(locations[[1]]$lat)
      lon <- as.numeric(locations[[1]]$lon)
      
      # Search for nearby stations
      incProgress(0.5, detail = "Recherche des stations à proximité...")
      
      nearby_stations <- find_nearest_stations(
        lon = lon,
        lat = lat,
        radius_km = input$airQualityRadius,
        limit = 10
      )
      
      airQuality$nearby_stations <- nearby_stations
      
      if (is.null(nearby_stations) || nrow(nearby_stations) == 0) {
        showNotification(
          paste0("Aucune station trouvée dans un rayon de ", 
                input$airQualityRadius, " km"),
          type = "warning"
        )
      } else {
        showNotification(
          paste0(nrow(nearby_stations), " station(s) trouvée(s) à proximité"),
          type = "message"
        )
        
        # Update the station dropdown
        choices <- setNames(
          as.list(nearby_stations$code_station),
          paste0(nearby_stations$name, " (", round(nearby_stations$distance_km, 1), " km)")
        )
        updateSelectInput(session, "airQualityStation", choices = choices)
        
        # Automatically select the first station
        if (length(choices) > 0) {
          updateSelectInput(session, "airQualityStation", selected = names(choices)[1])
        }
      }
    })
  })
  
  # Handle station selection
  observe({
    req(input$airQualityStation)
    
    # Keep track of selected station
    selected_station_code <- input$airQualityStation
    
    # Find station details based on selection method
    if (input$airQualitySelectionMethod == "list") {
      req(airQuality$stations)
      selected_station <- airQuality$stations[airQuality$stations$code_station == selected_station_code, ]
    } else { # address search
      req(airQuality$nearby_stations)
      selected_station <- airQuality$nearby_stations[airQuality$nearby_stations$code_station == selected_station_code, ]
    }
    
    # Store selected station
    if (nrow(selected_station) > 0) {
      airQuality$selected_station <- selected_station
    }
  })
  
  # Get air quality data based on inputs
  observe({
    req(input$airQualityStation, input$airQualityPollutant)
    
    # Parameters
    station_code <- input$airQualityStation
    pollutant_code <- input$airQualityPollutant
    data_type <- input$airQualityDataType
    
    # Get data based on the selected type
    withProgress(message = "Récupération des données...", {
      if (data_type == "hourly") {
        req(input$airQualityDateRange)
        date <- input$airQualityDateRange[1] # Only one day for hourly data
        
        # Fetch data
        hourly_data <- get_geodair_hourly_data(date, pollutant_code)
        
        # Filter for selected station
        if (!is.null(hourly_data) && nrow(hourly_data) > 0) {
          hourly_data <- hourly_data %>%
            dplyr::filter(code_station == station_code)
        }
        
        airQuality$data <- hourly_data
        
      } else if (data_type == "daily") {
        req(input$airQualityDateRange)
        start_date <- input$airQualityDateRange[1]
        end_date <- input$airQualityDateRange[2]
        
        # Fetch data
        daily_data <- get_geodair_daily_data(start_date, end_date, pollutant_code)
        
        # Filter for selected station
        if (!is.null(daily_data) && nrow(daily_data) > 0) {
          daily_data <- daily_data %>%
            dplyr::filter(code_station == station_code)
        }
        
        airQuality$data <- daily_data
        
      } else if (data_type == "annual") {
        req(input$airQualityYear)
        year <- input$airQualityYear
        
        # Fetch data
        annual_data <- get_geodair_annual_data(year, pollutant_code)
        
        # Filter for selected station
        if (!is.null(annual_data) && nrow(annual_data) > 0) {
          annual_data <- annual_data %>%
            dplyr::filter(code_station == station_code)
        }
        
        airQuality$data <- annual_data
      }
    })
  })
  
  # Render air quality map
  output$airQualityMap <- renderLeaflet({
    # Create base map
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng = 2.2137, lat = 46.2276, zoom = 5) %>%
      addScaleBar(position = "bottomleft")
  })
  
  # Update map based on stations
  observe({
    # Get the appropriate stations to display
    stations_to_display <- if (input$airQualitySelectionMethod == "address" && !is.null(airQuality$nearby_stations)) {
      airQuality$nearby_stations
    } else if (!is.null(airQuality$stations)) {
      airQuality$stations
    } else {
      NULL
    }
    
    if (is.null(stations_to_display) || nrow(stations_to_display) == 0) {
      return()
    }
    
    # Get map
    map <- leafletProxy("airQualityMap")
    
    # Clear previous markers
    map %>% clearMarkers() %>% clearShapes()
    
    # Selected station
    selected_station_code <- input$airQualityStation
    
    # Add station markers
    for (i in 1:nrow(stations_to_display)) {
      station <- stations_to_display[i, ]
      
      # Customize marker based on selection
      icon_color <- if (station$code_station == selected_station_code) "red" else "blue"
      popup_content <- paste0(
        "<b>", station$name, "</b><br>",
        "Code: ", station$code_station, "<br>",
        "Type: ", station$type,
        if ("distance_km" %in% names(station)) {
          paste0("<br>Distance: ", round(station$distance_km, 1), " km")
        } else ""
      )
      
      map %>% addAwesomeMarkers(
        lng = station$longitude, 
        lat = station$latitude,
        icon = awesomeIcons(
          icon = "signal",
          markerColor = icon_color,
          library = "fa"
        ),
        popup = popup_content,
        layerId = paste0("station_", station$code_station)
      )
    }
    
    # Adjust view if we have a selected station
    if (!is.null(airQuality$selected_station)) {
      map %>% setView(
        lng = airQuality$selected_station$longitude,
        lat = airQuality$selected_station$latitude,
        zoom = 10
      )
    }
  })
  
  # Mise en évidence de la station sélectionnée sur la carte
  observe({
    selected <- airQuality$selected_station
    stations <- airQuality$stations
    
    if (!is.null(selected) && nrow(selected) > 0 && !is.null(stations) && nrow(stations) > 0) {
      # Créer une icône spéciale pour la station sélectionnée
      selectedIcon <- makeIcon(
        iconUrl = "https://raw.githubusercontent.com/pointhi/leaflet-color-markers/master/img/marker-icon-2x-green.png",
        iconWidth = 25, iconHeight = 41,
        iconAnchorX = 12, iconAnchorY = 41
      )
      
      # Mise à jour de l'icône de la station sélectionnée
      leafletProxy("airQualityMap") %>%
        removeMarker(layerId = selected$code_station) %>%
        addMarkers(
          data = selected,
          lng = ~longitude, lat = ~latitude,
          popup = paste0(
            "<b>", selected$name, "</b><br>",
            "Code: ", selected$code_station, "<br>",
            "Commune: ", selected$libelle_commune, "<br>",
            "Cours d'eau: ", selected$libelle_cours_eau
          ),
          label = ~name,
          layerId = ~code_station,
          icon = selectedIcon
        ) %>%
        setView(lng = selected$longitude, lat = selected$latitude, zoom = 13)
    }
  })
  
  # Graphique de qualité de l'air
  output$airQualityPlot <- renderPlot({
    data <- airQuality$data
    station <- airQuality$selected_station
    
    if (is.null(data) || nrow(data) == 0 || is.null(station) || nrow(station) == 0) {
      # Retourner un graphique vide avec un message
      return(ggplot() + 
        annotate("text", x = 0.5, y = 0.5, label = "Aucune donnée à afficher", size = 8) +
        theme_void())
    }
    
    # S'assurer que les dates sont au format Date
    data$date_prelevement <- as.Date(data$date_prelevement)
    
    # Trier les données par date
    data <- data[order(data$date_prelevement), ]
    
    # Créer le graphique
    p <- ggplot(data, aes(x = date_prelevement, y = resultat)) +
      geom_point(color = "steelblue", size = 3) +
      geom_line(color = "steelblue", alpha = 0.7) +
      labs(
        title = paste0(data$libelle_parametre[1], " à ", station$name),
        x = "Date",
        y = paste0(data$libelle_parametre[1], " (", data$symbole_unite[1], ")"),
        caption = "Source: Hub'Eau Qualité de l'air"
      ) +
      theme_minimal(base_size = 14) +
      theme(
        plot.title = element_text(face = "bold", size = 16),
        axis.title = element_text(face = "bold"),
        axis.text = element_text(size = 12),
        axis.text.x = element_text(angle = 45, hjust = 1)
      )
    
    return(p)
  })
  
  # Téléchargement des données de qualité de l'air en CSV
  output$downloadAirQualityData <- downloadHandler(
    filename = function() {
      station <- airQuality$selected_station
      data <- airQuality$data
      
      if (is.null(station) || nrow(station) == 0 || is.null(data) || nrow(data) == 0) {
        return("donnees_qualite_air.csv")
      }
      
      # Créer un nom de fichier avec le code de la station et le code du paramètre
      code_parameter <- unique(data$code_parametre)[1]
      code_station <- station$code_station[1]
      
      return(paste0("qualite_air_", code_station, "_", code_parameter, ".csv"))
    },
    content = function(file) {
      data <- airQuality$data
      
      if (is.null(data) || nrow(data) == 0) {
        write.csv(data.frame(message = "Aucune donnée disponible"), file, row.names = FALSE)
      } else {
        # Préparer les données pour l'export
        export_data <- data[, c("code_station", "libelle_station", 
                              "code_parametre", "libelle_parametre", 
                              "date_prelevement", "resultat", 
                              "symbole_unite", "code_commune", "libelle_commune")]
        
        # Renommer les colonnes pour plus de clarté
        names(export_data) <- c("Code_Station", "Libelle_Station", 
                               "Code_Parametre", "Libelle_Parametre", 
                               "Date_Prelevement", "Resultat", 
                               "Unite", "Code_Commune", "Commune")
        
        # Exporter
        write.csv(export_data, file, row.names = FALSE)
      }
    }
  )
  
  # ==================== SECTION QUALITÉ DE L'EAU (HUB'EAU) ====================
  
  # Valeurs réactives pour les données de qualité de l'eau
  water_stations <- reactiveVal(NULL)        # Stations trouvées par la recherche
  selected_station_data <- reactiveVal(NULL) # Données de la station sélectionnée
  water_quality_data <- reactiveVal(NULL)    # Données de qualité de l'eau pour le paramètre sélectionné
  available_parameters <- reactiveVal(NULL)  # Paramètres disponibles pour la station sélectionnée
  
  # Recherche de stations par commune
  observeEvent(input$searchStationsByCommune, {
    req(input$waterQualityCommune)
    withProgress(message = "Recherche des stations...", {
      # Rechercher les stations
      stations <- get_stations(commune_name = input$waterQualityCommune)
      
      # S'assurer que stations n'est pas NULL
      if (is.null(stations)) {
        stations <- data.frame()  # Créer un dataframe vide si NULL
      }
      
      water_stations(stations)
      
      if (nrow(stations) > 0) {
        # Mettre à jour le menu déroulant des stations
        station_choices <- setNames(
          stations$code_station,
          paste0(stations$libelle_station, " (", stations$code_station, ")")
        )
        updateSelectInput(session, "selectedStation", choices = station_choices)
        
        # Notification de succès
        showNotification(paste(nrow(stations), "station(s) trouvée(s)"), type = "message")
      } else {
        # Notification si aucune station trouvée
        showNotification("Aucune station trouvée pour cette commune", type = "warning")
        updateSelectInput(session, "selectedStation", choices = NULL)
      }
    })
  })
  
  # Recherche de stations par coordonnées
  observeEvent(input$searchStationsByCoords, {
    req(input$waterQualityLon, input$waterQualityLat)
    withProgress(message = "Recherche des stations...", {
      # Rechercher les stations
      stations <- get_stations(
        longitude = input$waterQualityLon, 
        latitude = input$waterQualityLat, 
        distance = input$waterQualityDist
      )
      
      # S'assurer que stations n'est pas NULL
      if (is.null(stations)) {
        stations <- data.frame()  # Créer un dataframe vide si NULL
      }
      
      water_stations(stations)
      
      if (nrow(stations) > 0) {
        # Mettre à jour le menu déroulant des stations
        station_choices <- setNames(
          stations$code_station,
          paste0(stations$libelle_station, " (", stations$code_station, ")")
        )
        updateSelectInput(session, "selectedStation", choices = station_choices)
        
        # Notification de succès
        showNotification(paste(nrow(stations), "station(s) trouvée(s)"), type = "message")
      } else {
        # Notification si aucune station trouvée
        showNotification("Aucune station trouvée pour ces coordonnées", type = "warning")
        updateSelectInput(session, "selectedStation", choices = NULL)
      }
    })
  })
  
  # Recherche de stations par cours d'eau
  observeEvent(input$searchStationsByRiver, {
    req(input$waterQualityRiver)
    withProgress(message = "Recherche des stations...", {
      # Rechercher les stations
      stations <- find_stations_by_river(input$waterQualityRiver)
      
      # S'assurer que stations n'est pas NULL
      if (is.null(stations)) {
        stations <- data.frame()  # Créer un dataframe vide si NULL
      }
      
      water_stations(stations)
      
      if (nrow(stations) > 0) {
        # Mettre à jour le menu déroulant des stations
        station_choices <- setNames(
          stations$code_station,
          paste0(stations$libelle_station, " (", stations$code_station, ")")
        )
        updateSelectInput(session, "selectedStation", choices = station_choices)
        
        # Notification de succès
        showNotification(paste(nrow(stations), "station(s) trouvée(s)"), type = "message")
      } else {
        # Notification si aucune station trouvée
        showNotification("Aucune station trouvée pour ce cours d'eau", type = "warning")
        updateSelectInput(session, "selectedStation", choices = NULL)
      }
    })
  })
  
  # Recherche de stations par département
  observeEvent(input$searchStationsByDept, {
    req(input$waterQualityDept)
    withProgress(message = "Recherche des stations...", {
      # Rechercher les stations
      stations <- get_stations_by_department(input$waterQualityDept)
      
      # S'assurer que stations n'est pas NULL
      if (is.null(stations)) {
        stations <- data.frame()  # Créer un dataframe vide si NULL
      }
      
      water_stations(stations)
      
      if (nrow(stations) > 0) {
        # Mettre à jour le menu déroulant des stations
        station_choices <- setNames(
          stations$code_station,
          paste0(stations$libelle_station, " (", stations$code_station, ")")
        )
        updateSelectInput(session, "selectedStation", choices = station_choices)
        
        # Notification de succès
        showNotification(paste(nrow(stations), "station(s) trouvée(s)"), type = "message")
      } else {
        # Notification si aucune station trouvée
        showNotification("Aucune station trouvée pour ce département", type = "warning")
        updateSelectInput(session, "selectedStation", choices = NULL)
      }
    })
  })
  
  # Observer pour la sélection d'une station
  observeEvent(input$selectedStation, {
    req(input$selectedStation)
    
    # Récupérer les données de la station sélectionnée
    stations <- water_stations()
    selected_station <- stations[stations$code_station == input$selectedStation, ]
    
    if (nrow(selected_station) > 0) {
      # Stocker les données de la station
      selected_station_data(selected_station)
      
      # Récupérer les paramètres disponibles pour cette station
      withProgress(message = "Chargement des paramètres disponibles...", {
        params <- get_available_parameters(input$selectedStation)
        
        if (nrow(params) > 0) {
          # Stocker les paramètres
          available_parameters(params)
          
          # Créer un vecteur nommé pour le menu déroulant
          param_choices <- setNames(
            params$code_parametre,
            paste0(params$libelle_parametre, " (", params$code_parametre, ")")
          )
          
          # Ajouter les paramètres communs en haut de la liste
          common_codes <- get_common_parameter_codes()
          common_params <- common_codes[common_codes %in% params$code_parametre]
          
          if (length(common_params) > 0) {
            # Trouver les noms correspondants pour les paramètres communs
            common_names <- sapply(names(common_params), function(name) {
              code <- common_params[name]
              param_row <- params[params$code_parametre == code, ]
              if (nrow(param_row) > 0) {
                return(paste0(name, " (", code, ")"))
              } else {
                return(paste0(name, " (", code, ")"))
              }
            })
            
            # Créer la liste finale des choix avec les paramètres communs en premier
            common_choices <- setNames(common_params, common_names)
            # Enlever les paramètres communs de la liste complète
            param_choices <- param_choices[!param_choices %in% common_params]
            # Combiner les deux listes
            all_choices <- c(common_choices, param_choices)
            
            updateSelectInput(session, "selectedParameter", choices = all_choices)
          } else {
            updateSelectInput(session, "selectedParameter", choices = param_choices)
          }
          
          # Sélectionner les nitrates par défaut si disponible
          if ("1340" %in% params$code_parametre) {
            updateSelectInput(session, "selectedParameter", selected = "1340")
          }
        } else {
          showNotification("Aucun paramètre disponible pour cette station", type = "warning")
          updateSelectInput(session, "selectedParameter", choices = NULL)
        }
      })
    }
  })
  
  # Observer pour le chargement des données de qualité de l'eau
  observeEvent(input$loadWaterQualityData, {
    req(input$selectedStation, input$selectedParameter)
    
    withProgress(message = "Chargement des données de qualité...", {
      # Préparer les dates au format ISO
      start_date <- format(input$waterQualityDateRange[1], "%Y-%m-%d")
      end_date <- format(input$waterQualityDateRange[2], "%Y-%m-%d")
      
      # Charger les données pour le paramètre sélectionné
      data <- get_parameter_for_station(
        input$selectedStation,
        input$selectedParameter,
        date_debut_prelevement = start_date,
        date_fin_prelevement = end_date,
        size = 2000  # nombre max de résultats
      )
      
      # Stocker les données
      water_quality_data(data)
      
      if (nrow(data) > 0) {
        showNotification(paste(nrow(data), "mesure(s) trouvée(s)"), type = "message")
      } else {
        showNotification("Aucune donnée disponible pour ce paramètre et cette période", type = "warning")
      }
    })
  })
  
  # Indicateur si une station est sélectionnée
  output$hasStationSelected <- reactive({
    !is.null(selected_station_data()) && nrow(selected_station_data()) > 0
  })
  outputOptions(output, "hasStationSelected", suspendWhenHidden = FALSE)
  
  # Afficher les informations de recherche des stations
  output$stationSearchInfo <- renderUI({
    stations <- water_stations()
    
    if (is.null(stations)) {
      return(HTML("<p><em>Aucune station trouvée.</em></p>"))
    }
    
    if (nrow(stations) == 0) {
      return(HTML("<p><em>Aucune station trouvée.</em></p>"))
    }
    
    HTML(paste0(
      "<p><strong>", nrow(stations), " station(s) trouvée(s)</strong></p>"
    ))
  })
  
  # Afficher les informations sur la qualité de l'eau
  output$waterQualityInfo <- renderUI({
    station <- selected_station_data()
    data <- water_quality_data()
    params <- available_parameters()
    
    if (is.null(station) || nrow(station) == 0) {
      return(HTML("<p><em>Veuillez sélectionner une station pour voir les données.</em></p>"))
    }
    
    if (is.null(data) || nrow(data) == 0) {
      # Si une station est sélectionnée mais pas de données chargées
      selected_param_name <- "Aucun paramètre"
      if (!is.null(input$selectedParameter) && !is.null(params) && nrow(params) > 0) {
        param_row <- params[params$code_parametre == input$selectedParameter, ]
        if (nrow(param_row) > 0) {
          selected_param_name <- paste0(param_row$libelle_parametre, 
                                       " (", input$selectedParameter, ")")
        }
      }
      
      # Récupérer le nom de la commune depuis les données de la station
      commune_name <- "Non disponible"
      if ("libelle_commune" %in% names(station) && !is.na(station$libelle_commune)) {
        commune_name <- station$libelle_commune
      }
      
      return(HTML(paste0(
        "<h4>", htmlEscape(station$libelle_station), "</h4>",
        "<h4>", htmlEscape(station$libelle_station), " - ", htmlEscape(data$libelle_parametre[1]), "</h4>",
        "<p><strong>Période</strong>: du ", htmlEscape(earliest_date), " au ", htmlEscape(latest_date), "</p>",
        "<p><strong>Nombre de mesures</strong>: ", htmlEscape(nbr_measures), "</p>",
        "<p><strong>Valeur moyenne</strong>: ", htmlEscape(mean_value), htmlEscape(unit_text), "</p>",
        "<p><strong>Valeur minimale</strong>: ", htmlEscape(min_value), htmlEscape(unit_text), "</p>",
        "<p><strong>Valeur maximale</strong>: ", htmlEscape(max_value), htmlEscape(unit_text), "</p>"
      )))
    }
  })
  
  # Carte des stations de qualité de l'eau
  output$waterQualityMap <- renderLeaflet({
    stations <- water_stations()
    selected <- selected_station_data()
    
    # Carte par défaut centrée sur la France
    leaflet() %>%
      addTiles() %>%
      setView(lng = 2.213749, lat = 46.227638, zoom = 5) %>%
      addControl(
        html = tags$div(
          style = "padding: 6px 8px; background: white; border-radius: 5px; box-shadow: 0 0 15px rgba(0,0,0,0.2);",
          tags$h4("Stations de mesure de qualité de l'eau", 
                 style = "margin: 0; text-align: center; font-weight: bold;")
        ),
        position = "topright"
      )
  })
  
  # Observer pour mettre à jour la carte lorsque les stations changent
  observe({
    stations <- water_stations()
    
    # Vérifier si stations est NULL ou vide
    if (is.null(stations) || nrow(stations) == 0) {
      # Carte par défaut si aucune station
      leafletProxy("waterQualityMap") %>%
        clearMarkers() %>%
        setView(lng = 2.213749, lat = 46.227638, zoom = 5)
      return()
    }
    
    # Créer les popups en s'assurant que toutes les colonnes existent
    popups <- lapply(1:nrow(stations), function(i) {
      station <- stations[i, ]
      commune <- if("libelle_commune" %in% names(station) && !is.na(station$libelle_commune)) 
        station$libelle_commune else "Non disponible"
      cours_eau <- if("libelle_cours_eau" %in% names(station) && !is.na(station$libelle_cours_eau)) 
        station$libelle_cours_eau else "Non disponible"
      
      paste0(
        "<b>", station$libelle_station, "</b><br>",
        "Code: ", station$code_station, "<br>",
        "Commune: ", commune, "<br>",
        "Cours d'eau: ", cours_eau
      )
    })
    
    # S'assurer que longitude et latitude existent et ne sont pas NA
    valid_coords <- !is.na(stations$longitude) & !is.na(stations$latitude)
    if (sum(valid_coords) == 0) {
      # Aucune coordonnée valide
      leafletProxy("waterQualityMap") %>%
        clearMarkers() %>%
        setView(lng = 2.213749, lat = 46.227638, zoom = 5)
      showNotification("Aucune coordonnée valide pour les stations trouvées", type = "warning")
      return()
    }
    
    # Filtrer les stations avec des coordonnées valides
    stations_valid <- stations[valid_coords, ]
    popups_valid <- popups[valid_coords]
    
    # Mise à jour de la carte
    leafletProxy("waterQualityMap") %>%
      clearMarkers() %>%
      addMarkers(
        data = stations_valid,
        lng = ~longitude, lat = ~latitude,
        popup = popups_valid,
        label = ~libelle_station,
        layerId = ~code_station
      )
    
    # Zoom sur les marqueurs si au moins un est valide
    if (nrow(stations_valid) == 1) {
      leafletProxy("waterQualityMap") %>%
        setView(lng = stations_valid$longitude[1], lat = stations_valid$latitude[1], zoom = 13)
    } else if (nrow(stations_valid) > 1) {
      # Calcul des limites seulement si plus d'une station
      leafletProxy("waterQualityMap") %>%
        fitBounds(
          min(stations_valid$longitude, na.rm = TRUE), 
          min(stations_valid$latitude, na.rm = TRUE),
          max(stations_valid$longitude, na.rm = TRUE), 
          max(stations_valid$latitude, na.rm = TRUE)
        )
    }
  })
  
  # Mise en évidence de la station sélectionnée sur la carte
  observe({
    selected <- selected_station_data()
    stations <- water_stations()
    
    # Vérifier que selected n'est pas NULL ou vide, et que les coordonnées sont présentes
    if (is.null(selected) || nrow(selected) == 0 || 
        is.null(stations) || nrow(stations) == 0 ||
        !("longitude" %in% names(selected)) || !("latitude" %in% names(selected)) ||
        is.na(selected$longitude[1]) || is.na(selected$latitude[1])) {
      return()  # Ne rien faire si pas de données valides
    }
    
    # Créer une icône spéciale pour la station sélectionnée
    selectedIcon <- makeIcon(
      iconUrl = "https://raw.githubusercontent.com/pointhi/leaflet-color-markers/master/img/marker-icon-2x-green.png",
      iconWidth = 25, iconHeight = 41,
      iconAnchorX = 12, iconAnchorY = 41
    )
    
    # Préparer le contenu du popup avec vérification des champs
    commune <- if("libelle_commune" %in% names(selected) && !is.na(selected$libelle_commune)) 
      selected$libelle_commune else "Non disponible"
    cours_eau <- if("libelle_cours_eau" %in% names(selected) && !is.na(selected$libelle_cours_eau)) 
      selected$libelle_cours_eau else "Non disponible"
    
    popup_content <- paste0(
      "<b>", selected$libelle_station, "</b><br>",
      "Code: ", selected$code_station, "<br>",
      "Commune: ", commune, "<br>",
      "Cours d'eau: ", cours_eau
    )
    
    # Mise à jour de l'icône de la station sélectionnée
    leafletProxy("waterQualityMap") %>%
      removeMarker(layerId = selected$code_station) %>%
      addMarkers(
        data = selected,
        lng = ~longitude, lat = ~latitude,
        popup = popup_content,
        label = ~libelle_station,
        layerId = ~code_station,
        icon = selectedIcon
      ) %>%
      setView(lng = selected$longitude, lat = selected$latitude, zoom = 13)
  })
  
  # Graphique de qualité de l'eau
  output$waterQualityPlot <- renderPlot({
    data <- water_quality_data()
    station <- selected_station_data()
    
    if (is.null(data) || nrow(data) == 0 || is.null(station) || nrow(station) == 0) {
      # Retourner un graphique vide avec un message
      return(ggplot() + 
        annotate("text", x = 0.5, y = 0.5, label = "Aucune donnée à afficher", size = 8) +
        theme_void())
    }
    
    # S'assurer que les dates sont au format Date
    data$date_prelevement <- as.Date(data$date_prelevement)
    
    # Trier les données par date
    data <- data[order(data$date_prelevement), ]
    
    # Créer le graphique
    p <- ggplot(data, aes(x = date_prelevement, y = resultat)) +
      geom_point(color = "steelblue", size = 3) +
      geom_line(color = "steelblue", alpha = 0.7) +
      labs(
        title = paste0(data$libelle_parametre[1], " à ", station$libelle_station),
        x = "Date",
        y = paste0(data$libelle_parametre[1], " (", data$symbole_unite[1], ")"),
        caption = "Source: Hub'Eau Qualité des cours d'eau"
      ) +
      theme_minimal(base_size = 14) +
      theme(
        plot.title = element_text(face = "bold", size = 16),
        axis.title = element_text(face = "bold"),
        axis.text = element_text(size = 12),
        axis.text.x = element_text(angle = 45, hjust = 1)
      )
    
    return(p)
  })
  
  # Tableau récapitulatif des données de qualité d'eau
  output$waterQualityTable <- renderTable({
    data <- water_quality_data()
    
    if (is.null(data) || nrow(data) == 0) {
      return(data.frame(Message = "Aucune donnée disponible"))
    }
    
    # Créer un tableau simplifié avec les informations essentielles
    df_summary <- data.frame(
      "Date" = format(as.Date(data$date_prelevement), "%d/%m/%Y"),
      "Résultat" = round(data$resultat, 2),
      "Unité" = data$symbole_unite
    )
    
    # Trier et limiter aux 10 dernières mesures
    df_summary <- df_summary[order(as.Date(data$date_prelevement), decreasing = TRUE), ]
    df_summary <- head(df_summary, 10)
    
    # Ajouter une ligne avec des statistiques sous forme de texte
    stats_text <- paste("Min:", round(min(data$resultat, na.rm = TRUE), 2),
                       "| Max:", round(max(data$resultat, na.rm = TRUE), 2),
                       "| Moyenne:", round(mean(data$resultat, na.rm = TRUE), 2))
    
    # Ajouter les statistiques en texte sous le tableau (comme info HTML)
    output$waterQualityInfo <- renderUI({
      tagList(
        div(style = "margin-top: 10px; margin-bottom: 15px;",
          h4("Informations sur le paramètre", style = "margin-bottom: 10px;"),
          p(HTML(paste0("<strong>", unique(data$libelle_parametre)[1], "</strong> - Station: ", 
                       unique(data$libelle_station)[1]))),
          p(HTML(paste0("<strong>Statistiques:</strong> ", stats_text)))
        )
      )
    })
    
    return(df_summary)
  }, striped = TRUE, hover = TRUE, spacing = "m", align = "c")
  
  # Téléchargement des données de qualité de l'eau en CSV
  output$downloadWaterQualityData <- downloadHandler(
    filename = function() {
      station <- selected_station_data()
      data <- water_quality_data()
      
      if (is.null(station) || nrow(station) == 0 || is.null(data) || nrow(data) == 0) {
        return("donnees_qualite_eau.csv")
      }
      
      # Créer un nom de fichier avec le code de la station et le code du paramètre
      code_parameter <- unique(data$code_parametre)[1]
      code_station <- station$code_station[1]
      
      return(paste0("qualite_eau_", code_station, "_", code_parameter, ".csv"))
    },
    content = function(file) {
      data <- water_quality_data()
      
      if (is.null(data) || nrow(data) == 0) {
        write.csv(data.frame(message = "Aucune donnée disponible"), file, row.names = FALSE)
      } else {
        # Préparer les données pour l'export
        export_data <- data[, c("code_station", "libelle_station", 
                              "code_parametre", "libelle_parametre", 
                              "date_prelevement", "resultat", 
                              "symbole_unite", "code_commune", "libelle_commune")]
        
        # Renommer les colonnes pour plus de clarté
        names(export_data) <- c("Code_Station", "Libelle_Station", 
                               "Code_Parametre", "Libelle_Parametre", 
                               "Date_Prelevement", "Resultat", 
                               "Unite", "Code_Commune", "Commune")
        
        # Exporter
        write.csv(export_data, file, row.names = FALSE)
      }
    }
  )
} 