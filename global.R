library(shiny)
library(sf)
library(leaflet)
library(dplyr)
library(stringr)
library(htmltools)
library(RColorBrewer)
library(mapview) # Pour exporter en PDF
library(webshot2) # Pour l'export PDF
library(rsconnect)
library(raster)
library(shinydashboard)  # Pour les onglets et l'interface plus élaborée
library(openxlsx)  # Pour la gestion des fichiers Excel
library(htmlwidgets)  # Pour l'export des cartes au format HTML
library(httr)  # Pour les requêtes API
library(jsonlite)  # Pour parser les réponses JSON
library(ggplot2)  # Pour les graphiques de diagnostic
library(tidyr)  # Pour la manipulation des données
library(cowplot)  # Pour la mise en page des graphiques
library(readxl)  # Pour lire les fichiers Excel
library(base64enc)  # Pour encoder les images en base64
library(gridExtra)  # Pour combiner les graphiques
library(shinyjs)  # Pour les fonctionnalités JavaScript avancées
library(shinythemes)  # Pour le thème de l'interface

# Charger la fonction pour générer le diagnostic PDF
source("wrapper.R")

# Message d'information sur les temps de chargement 
loading_time_message <- HTML(
  "<div style='background-color: rgba(255, 255, 255, 0.9); padding: 15px; border-radius: 10px; margin-bottom: 20px;'>
    <h4 style='margin-top: 0; color: #336699;'><i class='fas fa-info-circle'></i> Information sur les temps de chargement</h4>
    <p>Les cartes au format <strong>communal</strong> nécessitent environ <strong>7 secondes</strong> pour se charger en raison du grand nombre de polygones à traiter.</p>
    <p>Les cartes au format <strong>départemental</strong> se chargent plus rapidement (environ <strong>3 secondes</strong>) et sont recommandées pour une visualisation nationale.</p>
  </div>"
)

# Chemins des dossiers et fichiers
path_indicateurs_saisonniers <- "Data/INDICATEURS_SAISONNIERS_ETE/Resultats/"
path_indicateurs_annuels <- "Data/INDICATEURS_ANNUELS_HORIZONS/Resultats/"
path_feux_indicateurs <- "Data/FEUX_INDICATEURS_ANNUELS_HORIZONS/Resultats/"
path_agri_indicateurs <- "Data/AGRI_INDICATEURS_ANNUELS_HORIZONS/Resultats/"
path_descriptions <- "Data/noms_variables.txt"
path_communes <- "Data/Communes/codes_postaux_region.shp"
# Dossier pour stocker les fichiers de cache
path_cache <- "Data/cache"

# Créer le dossier de cache s'il n'existe pas
if (!dir.exists(path_cache)) {
  dir.create(path_cache, recursive = TRUE)
}

# Définition des périodes des horizons avec noms complets
horizon_periods <- list(
  "REF" = "Référence",
  "H1" = "2021-2050",
  "H2" = "2041-2070",
  "H3" = "2071-2100"
)

# Définition des noms complets des horizons
horizon_full_names <- list(
  "REF" = "REF : Période de Référence",
  "H1" = "H1 : Horizon proche [2021-2050]",
  "H2" = "H2 : Horizon Moyen [2041-2070]",
  "H3" = "H3 : Horizon Lointain [2071-2100]"
)

# Définition des noms complets des scénarios
scenario_full_names <- list(
  "REFERENCE" = "REFERENCE",
  "Scénario RCP 2.6: Émissions maitrisées " = "RCP 2.6",
  "Scénario RCP 4.5: Émissions modérées" = "RCP 4.5",
  "Scénario RCP 8.5: Émissions non réduites" = "RCP 8.5",
  "Inconnu" = "Inconnu"
)

# Lecture des descriptions de variables
read_descriptions <- function(file_path) {
  # Vérification si le fichier existe
  if (!file.exists(file_path)) {
    warning("Fichier de descriptions non trouvé: ", file_path)
    return(list())
  }
  
  lines <- readLines(file_path, warn = FALSE, encoding = "UTF-8")
  desc_list <- list()
  
  for (line in lines) {
    # Recherche de patterns comme [CODE]: Description
    pattern <- "\\[(.*?)\\]:\\s*(.*)"
    if (grepl(pattern, line)) {
      var_code <- gsub(pattern, "\\1", line)
      var_desc <- gsub(pattern, "\\2", line)
      desc_list[[var_code]] <- var_desc
    }
  }
  
  return(desc_list)
}

# Fonction pour extraire le scénario du nom de fichier
extract_scenario <- function(file_path) {
  file_name <- basename(file_path)
  
  # Correspondance de motifs pour différents formats de noms de fichiers
  if (grepl("REFERENCE", file_name, ignore.case = TRUE)) {
    return("REFERENCE")
  } else if (grepl("_2_6_", file_name)) {
    return("Scénario RCP 2.6: Émissions maitrisées ")
  } else if (grepl("_4_5_", file_name)) {
    return("Scénario RCP 4.5: Émissions modérées")
  } else if (grepl("_8_5_", file_name)) {
    return("Scénario RCP 8.5: Émissions non réduites")
  } else {
    return("Inconnu")
  }
}

# Fonction pour obtenir les fichiers gpkg d'un dossier avec filtre sur le format spatial
get_gpkg_files <- function(folder_path, use_departments = FALSE) {
  if (!dir.exists(folder_path)) {
    warning("Dossier non trouvé: ", folder_path)
    return(character(0))
  }
  
  # Filtre pour les fichiers selon le format spatial choisi
  spatial_pattern <- if(use_departments) "_DEPARTEMENTS\\.gpkg$" else "_COMMUNES\\.gpkg$"
  files <- list.files(folder_path, pattern = spatial_pattern, full.names = TRUE)
  
  # Si aucun fichier trouvé avec le pattern spécifique, retourner une liste vide
  # au lieu d'essayer sans le pattern
  return(files)
}

# Fonction pour gérer le cache des données géospatiales
get_cached_data <- function(file_path, transform_to_4326 = TRUE) {
  # Créer un nom de fichier unique pour le cache basé sur le chemin du fichier original
  cache_file_name <- gsub("[^a-zA-Z0-9]", "_", basename(file_path))
  cache_file_path <- file.path(path_cache, paste0(cache_file_name, ".rds"))
  
  # Vérifier si le fichier cache existe
  if (file.exists(cache_file_path)) {
    # Vérifier si le fichier cache est plus récent que le fichier original
    if (file.info(cache_file_path)$mtime > file.info(file_path)$mtime) {
      message("Chargement depuis le cache: ", basename(cache_file_path))
      return(readRDS(cache_file_path))
    }
  }
  
  # Si pas de cache valide, charger et traiter les données
  message("Chargement et traitement du fichier: ", basename(file_path))
  tryCatch({
    # Lecture avec transformation EPSG:4326 (WGS84) pour Leaflet
    data <- st_read(file_path, quiet = TRUE)
    
    # Ajouter un index pour la jointure
    data$index_original <- seq_len(nrow(data))
    
    # Vérifier et transformer la projection si nécessaire et demandé
    if (transform_to_4326) {
      if (!is.na(st_crs(data)$wkt) && st_crs(data)$epsg != 4326) {
        data <- st_transform(data, 4326)
      } else if (is.na(st_crs(data)$wkt)) {
        # Si la projection n'est pas définie, assigner une projection (souvent Lambert-93 pour la France)
        data <- st_set_crs(data, 2154)
        data <- st_transform(data, 4326)
      }
    }
    
    # Sauvegarder dans le cache
    saveRDS(data, cache_file_path)
    message("Données sauvegardées dans le cache: ", basename(cache_file_path))
    
    return(data)
  }, error = function(e) {
    warning("Erreur lors de la lecture du fichier: ", e$message)
    return(NULL)
  })
}

# Fonction pour extraire les horizons disponibles à partir des colonnes
extract_horizons <- function(data) {
  col_names <- colnames(data)
  # Recherche les colonnes se terminant par _REF, _H1, _H2, _H3
  horizons <- unique(c(
    if(any(grepl("_REF$", col_names))) "REF",
    if(any(grepl("_H1$", col_names))) "H1", 
    if(any(grepl("_H2$", col_names))) "H2",
    if(any(grepl("_H3$", col_names))) "H3"
  ))
  return(horizons)
}

# Fonction pour obtenir les variables disponibles pour un horizon donné
get_variables_for_horizon <- function(data, horizon, var_descriptions) {
  col_names <- colnames(data)
  # Recherche les colonnes se terminant par l'horizon spécifié
  vars <- col_names[grepl(paste0("_", horizon, "$"), col_names)]
  # Extraction des noms de variables sans le suffixe _Hn
  vars <- gsub(paste0("_", horizon, "$"), "", vars)
  # Exclure les colonnes non-variables (geom, index_original, etc.)
  vars <- vars[!vars %in% c("geom", "index_original")]
  
  # Créer un vecteur nommé pour le menu déroulant avec codes et descriptions
  vars_named <- vars
  names(vars_named) <- sapply(vars, function(var) {
    desc <- var_descriptions[[var]]
    if (!is.null(desc) && desc != "") {
      paste0(var, " - ", desc)
    } else {
      var
    }
  })
  
  return(vars_named)
}

# Charger le shapefile des communes et préparer le spatial join
load_communes <- function(path_communes) {
  if (!file.exists(path_communes)) {
    warning("Fichier de communes non trouvé: ", path_communes)
    return(NULL)
  }
  
  print(paste("Chargement du shapefile des communes:", path_communes))
  
  tryCatch({
    # Lire le shapefile avec st_read en supprimant les NA
    communes <- st_read(path_communes, quiet = TRUE, stringsAsFactors = FALSE, options = "ENCODING=UTF-8")
    
    # Informations sur les communes chargées
    print(paste("Nombre de communes chargées:", nrow(communes)))
    print(paste("Colonnes disponibles:", paste(colnames(communes), collapse = ", ")))
    print(paste("CRS original:", st_crs(communes)$epsg))
    
    # Vérifier si les données sont vides
    if (nrow(communes) == 0) {
      warning("Le fichier des communes est vide")
      return(NULL)
    }
    
    # S'assurer que toutes les géométries sont valides, avec gestion d'erreur
    print("Validation des géométries...")
    communes <- suppressWarnings(st_make_valid(communes))
    
    # Ajouter un index corrigé pour la jointure
    communes$index_corrected <- seq_len(nrow(communes))
    
    # Transformer en WGS84 pour Leaflet avec gestion d'erreur
    print("Transformation en WGS84 (EPSG:4326)...")
    if (!is.na(st_crs(communes)$wkt) && st_crs(communes)$epsg != 4326) {
      communes <- suppressWarnings(st_transform(communes, 4326))
    } else if (is.na(st_crs(communes)$wkt)) {
      print("CRS non défini, assignation de EPSG:2154 (Lambert-93)...")
      communes <- suppressWarnings(st_set_crs(communes, 2154))
      communes <- suppressWarnings(st_transform(communes, 4326))
    }
    
    print(paste("CRS final:", st_crs(communes)$epsg))
    
    # Vérifier si le shapefile contient des informations essentielles
    has_code <- any(c("CODE_INSEE", "INSEE_COM", "CODE_C") %in% colnames(communes))
    has_name <- any(c("NOM_COMMUNE", "NOM_COM", "LIB") %in% colnames(communes))
    
    if (!has_code || !has_name) {
      warning("Le shapefile ne contient pas les colonnes nécessaires pour les codes ou noms de communes")
      print(paste("Colonnes manquantes - Code:", !has_code, "Nom:", !has_name))
    }
    
    return(communes)
  }, error = function(e) {
    warning("Erreur lors de la lecture du fichier des communes: ", e$message)
    return(NULL)
  })
}

# Fonction pour détecter une commune à partir de coordonnées GPS en cherchant dans tous les fichiers GPKG disponibles
find_commune_by_gps <- function(lon, lat) {
  print(paste("Recherche de commune pour les coordonnées:", lon, lat))
  
  # Chercher tous les fichiers GPKG de communes
  all_gpkg_files <- c()
  
  # Chercher dans le dossier des indicateurs saisonniers
  saisonniers_files <- list.files(path_indicateurs_saisonniers, 
                                 pattern = ".*COMMUNES.*\\.gpkg$", 
                                 recursive = TRUE, 
                                 full.names = TRUE)
  all_gpkg_files <- c(all_gpkg_files, saisonniers_files)
  
  # Chercher dans le dossier des indicateurs annuels
  annuels_files <- list.files(path_indicateurs_annuels, 
                            pattern = ".*COMMUNES.*\\.gpkg$", 
                            recursive = TRUE, 
                            full.names = TRUE)
  all_gpkg_files <- c(all_gpkg_files, annuels_files)
  
  # Chercher dans le dossier des feux
  feux_files <- list.files(path_feux_indicateurs, 
                         pattern = ".*COMMUNES.*\\.gpkg$", 
                         recursive = TRUE, 
                         full.names = TRUE)
  all_gpkg_files <- c(all_gpkg_files, feux_files)
  
  # Chercher dans le dossier agricole
  agri_files <- list.files(path_agri_indicateurs, 
                         pattern = ".*COMMUNES.*\\.gpkg$", 
                         recursive = TRUE, 
                         full.names = TRUE)
  all_gpkg_files <- c(all_gpkg_files, agri_files)
  
  # Supprimer les doublons
  all_gpkg_files <- unique(all_gpkg_files)
  
  print(paste("Nombre total de fichiers GPKG trouvés:", length(all_gpkg_files)))
  
  if (length(all_gpkg_files) == 0) {
    print("Aucun fichier GPKG de communes trouvé!")
    return(NULL)
  }
  
  # Essayer chaque fichier GPKG jusqu'à ce qu'on trouve une commune
  for (gpkg_file in all_gpkg_files) {
    print(paste("Essai avec le fichier:", gpkg_file))
    commune_info <- detect_commune_from_gpkg(lon, lat, gpkg_file)
    
    if (!is.null(commune_info)) {
      print("Commune trouvée!")
      return(commune_info)
    }
  }
  
  print("Aucune commune trouvée dans tous les fichiers GPKG testés")
  return(NULL)
}

# Fonction pour détecter la commune à partir des coordonnées GPS avec un fichier GPKG
detect_commune_from_gpkg <- function(lon, lat, gpkg_file) {
  print(paste("Détection de commune dans", gpkg_file, "pour:", lon, lat))
  
  # Vérifier que les coordonnées sont dans des limites raisonnables pour la France
  if (is.na(lon) || is.na(lat) || lon < -5.5 || lon > 10 || lat < 41 || lat > 52) {
    print("Coordonnées hors des limites de la France métropolitaine")
    return(NULL)
  }
  
  # Nom du fichier de cache
  gpkg_basename <- basename(gpkg_file)
  cache_filename <- paste0("communes_", gsub("[^a-zA-Z0-9]", "_", gpkg_basename), ".rds")
  cache_filepath <- file.path(path_cache, cache_filename)
  
  # Essayer de charger depuis le cache
  commune_sf <- NULL
  if (file.exists(cache_filepath)) {
    print("Chargement des communes depuis le cache...")
    tryCatch({
      commune_sf <- readRDS(cache_filepath)
      print(paste("Chargé", nrow(commune_sf), "communes depuis le cache"))
    }, error = function(e) {
      print(paste("Erreur lors du chargement du cache:", e$message))
      commune_sf <- NULL
    })
  }
  
  # Si pas de cache, charger depuis le fichier GPKG
  if (is.null(commune_sf)) {
    print("Chargement des communes depuis le fichier GPKG...")
    tryCatch({
      commune_sf <- sf::st_read(gpkg_file, quiet = TRUE)
      print(paste("Chargé", nrow(commune_sf), "communes depuis GPKG"))
      
      # Trouver les colonnes de code et nom commune
      code_column <- NULL
      name_column <- NULL
      
      # Rechercher des colonnes possibles pour le code
      for (col_name in c("CODE_C", "INSEE_COM", "CODE_INSEE", "ID", "CODE")) {
        if (col_name %in% colnames(commune_sf)) {
          code_column <- col_name
          print(paste("Colonne de code commune trouvée:", code_column))
          break
        }
      }
      
      # Si aucune colonne de code trouvée, créer une colonne CODE_C vide
      if (is.null(code_column)) {
        print("Aucune colonne de code commune trouvée, création d'une colonne CODE_C")
        commune_sf$CODE_C <- NA
        code_column <- "CODE_C"
      }
      
      # Rechercher des colonnes possibles pour le nom
      for (col_name in c("LIB", "NOM_COM", "NOM", "COMMUNE", "LIBELLE")) {
        if (col_name %in% colnames(commune_sf)) {
          name_column <- col_name
          print(paste("Colonne de nom commune trouvée:", name_column))
          break
        }
      }
      
      # Si aucune colonne de nom trouvée, créer une colonne LIB vide
      if (is.null(name_column)) {
        print("Aucune colonne de nom commune trouvée, création d'une colonne LIB")
        commune_sf$LIB <- NA
        name_column <- "LIB"
      }
      
      # Si la colonne s'appelle différemment de CODE_C ou LIB, créer des alias
      if (code_column != "CODE_C") {
        commune_sf$CODE_C <- commune_sf[[code_column]]
      }
      
      if (name_column != "LIB") {
        commune_sf$LIB <- commune_sf[[name_column]]
      }
      
      # S'assurer que la géométrie est valide
      print("Validation des géométries...")
      commune_sf <- sf::st_make_valid(commune_sf)
      
      # Vérifier et transformer en WGS84 si nécessaire
      print(paste("CRS original:", sf::st_crs(commune_sf)$epsg))
      if (sf::st_crs(commune_sf)$epsg != 4326) {
        print("Transformation en WGS84 (EPSG:4326)...")
        commune_sf <- sf::st_transform(commune_sf, 4326)
      }
      
      # Sauvegarder dans le cache pour utilisation future
      print("Sauvegarde des communes dans le cache...")
      dir.create(path_cache, showWarnings = FALSE, recursive = TRUE)
      saveRDS(commune_sf, cache_filepath)
      print("Communes sauvegardées dans le cache")
      
    }, error = function(e) {
      print(paste("Erreur lors du chargement du fichier GPKG:", e$message))
      return(NULL)
    })
  }
  
  if (is.null(commune_sf) || nrow(commune_sf) == 0) {
    print("Aucune donnée de commune disponible")
    return(NULL)
  }
  
  # Créer un point à partir des coordonnées (en WGS84)
  point <- sf::st_sfc(sf::st_point(c(lon, lat)), crs = 4326)
  
  # Trouver la commune qui contient le point
  print("Recherche de la commune contenant le point...")
  commune_found <- NULL
  
  tryCatch({
    # Utiliser st_intersects pour trouver quelle commune contient le point
    intersects <- sf::st_intersects(point, commune_sf)
    
    if (length(intersects[[1]]) > 0) {
      # Récupérer la première commune qui contient le point
      commune_idx <- intersects[[1]][1]
      commune_found <- commune_sf[commune_idx, ]
      
      # Extraire les informations de la commune
      code_commune <- as.character(commune_found$CODE_C)
      commune_name <- as.character(commune_found$LIB)
      
      print(paste("Commune trouvée par intersection spatiale:", commune_name, "Code:", code_commune))
      
      return(list(
        code = code_commune,
        name = commune_name
      ))
    } else {
      print("Aucune commune ne contient ce point. Recherche de la commune la plus proche...")
      
      # Comme alternative, trouver la commune la plus proche
      dists <- sf::st_distance(point, commune_sf)
      nearest_idx <- which.min(dists)
      
      nearest_commune <- commune_sf[nearest_idx, ]
      nearest_code <- as.character(nearest_commune$CODE_C)
      nearest_name <- as.character(nearest_commune$LIB)
      
      # Calculer la distance en mètres
      min_dist <- min(dists)
      print(paste("Commune la plus proche:", nearest_name, "Code:", nearest_code, 
                 "Distance:", round(min_dist), "mètres"))
      
      # Ne retourner la commune la plus proche que si elle est à moins de 5km
      if (min_dist < 5000) {
        return(list(
          code = nearest_code,
          name = nearest_name,
          approx = TRUE,
          distance = round(min_dist)
        ))
      } else {
        print("La commune la plus proche est trop éloignée (>5km)")
        return(NULL)
      }
    }
  }, error = function(e) {
    print(paste("Erreur lors de la recherche spatiale:", e$message))
    return(NULL)
  })
  
  return(NULL)
} 