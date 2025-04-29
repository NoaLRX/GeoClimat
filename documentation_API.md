# Documentation des API GeoClimat

## API Hub'Eau - Qualité des cours d'eau

### Informations générales

-   **Version de l'API**: v2
-   **URL de base**: `https://hubeau.eaufrance.fr/api/v2/qualite_rivieres`
-   **Documentation officielle**: <https://hubeau.eaufrance.fr/page/api-qualite-cours-deau>

### Fonction principale

``` r
hubeau_request(endpoint, params = list(), format_json = TRUE)
```

Cette fonction gère toutes les requêtes HTTP vers l'API Hub'Eau avec: - Formatage des paramètres de requête - Gestion des erreurs HTTP - Conversion automatique des réponses JSON

### Endpoints disponibles

| Endpoint     | Description                             |
|--------------|-----------------------------------------|
| `station_pc` | Informations sur les stations de mesure |
| `analyse_pc` | Résultats d'analyses physico-chimiques  |

### Fonctions de recherche de stations

#### Recherche par commune ou coordonnées GPS

``` r
get_stations(commune_name = NULL, code_commune = NULL, 
             longitude = NULL, latitude = NULL, 
             distance = 5, size = 20)
```

-   Permet de trouver des stations dans un rayon autour d'une commune ou de coordonnées
-   Le paramètre `distance` définit le rayon en kilomètres
-   `size` limite le nombre de résultats retournés

#### Recherche par rivière

``` r
find_stations_by_river(river_name, size = 20)
```

-   Trouve toutes les stations sur une rivière spécifique

#### Recherche par département

``` r
get_stations_by_department(code_departement, size = 100)
```

-   Liste toutes les stations d'un département

### Fonctions d'accès aux données

#### Mesures de nitrates

``` r
get_nitrates_for_station(code_station, 
                         date_debut_prelevement = NULL,
                         date_fin_prelevement = NULL,
                         size = 1000)
```

-   Récupère l'historique des mesures de nitrates pour une station
-   Utilise automatiquement le code Sandre 1340 pour les nitrates

#### Mesures d'un paramètre spécifique

``` r
get_parameter_for_station(code_station, 
                          code_parametre,
                          date_debut_prelevement = NULL,
                          date_fin_prelevement = NULL,
                          size = 1000)
```

-   Récupère l'historique des mesures pour n'importe quel paramètre
-   Nécessite le code Sandre du paramètre

#### Liste des paramètres disponibles

``` r
get_available_parameters(code_station, size = 100)
```

-   Renvoie tous les paramètres mesurés à une station donnée

### Fonctions utilitaires

#### Codes des paramètres courants

``` r
get_common_parameter_codes()
```

Renvoie un vecteur nommé avec les codes Sandre des paramètres courants:

| Nom             | Code Sandre |
|-----------------|-------------|
| Nitrates        | 1340        |
| Phosphore total | 1350        |
| Ammonium        | 1335        |
| pH              | 1302        |
| Conductivité    | 1303        |
| Oxygène dissous | 1311        |
| Température     | 1301        |
| MES             | 1305        |
| DBO5            | 1313        |
| DCO             | 1314        |

#### Visualisation des nitrates

``` r
plot_nitrates(nitrates_data, station_name = "Station")
```

-   Génère un graphique d'évolution des nitrates dans le temps
-   Utilise ggplot2 pour un rendu de qualité

### Exemple d'utilisation

``` r
# Rechercher des stations près de Lyon
stations_lyon <- get_stations(commune_name = "Lyon")

# Récupérer la première station
if (nrow(stations_lyon) > 0) {
  station_code <- stations_lyon$code_station[1]
  
  # Récupérer les données de nitrates
  nitrates_data <- get_nitrates_for_station(station_code)
  
  # Créer un graphique
  plot_nitrates(nitrates_data, station_name = stations_lyon$libelle_station[1])
}
```

## API BAN - Base Adresse Nationale

### Informations générales

-   **URL de l'API**: `https://api-adresse.data.gouv.fr/search/`
-   **Documentation officielle**: <https://adresse.data.gouv.fr/api-doc/adresse>

### Implémentation dans GeoClimat

L'API BAN est utilisée pour la recherche d'adresses et la géolocalisation dans l'application. Voici comment elle est implémentée:

``` r
# Fonction pour rechercher une adresse avec l'API BAN
search_address_BAN <- function(query) {
  print(paste("Recherche BAN pour:", query))
  
  # URL de l'API BAN
  url <- "https://api-adresse.data.gouv.fr/search/"
  
  # Paramètres de la requête
  params <- list(q = query, limit = 10)
  
  # Effectuer la requête GET
  tryCatch({
    response <- GET(url, query = params)
    
    # Vérifier le code de statut HTTP
    if (http_error(response)) {
      status <- status_code(response)
      message <- content(response, "text", encoding = "UTF-8")
      warning(paste("Erreur API (", status, "):", message))
      return(list())
    }
    
    # Convertir la réponse JSON en liste R
    data <- content(response, "text", encoding = "UTF-8") %>% fromJSON()
    
    # Si aucun résultat, retourner une liste vide
    if (length(data$features) == 0) {
      return(list())
    }
    
    # Traiter les résultats
    results <- lapply(data$features, function(feature) {
      prop <- feature$properties
      geom <- feature$geometry
      
      # Créer un objet résultat
      list(
        label = if ("label" %in% names(prop)) prop$label else "Adresse sans nom",
        city = if ("city" %in% names(prop)) prop$city else NA,
        postcode = if ("postcode" %in% names(prop)) prop$postcode else NA,
        lat = if (length(geom$coordinates) >= 2) geom$coordinates[2] else NA,
        lon = if (length(geom$coordinates) >= 1) geom$coordinates[1] else NA,
        score = if ("score" %in% names(prop)) prop$score else 0
      )
    })
    
    return(results)
  }, error = function(e) {
    print(paste("Erreur API BAN:", e$message))
    return(list())
  })
}
```

### Fonctionnalités

L'API BAN est utilisée pour:

1.  **Recherche d'adresses** - Permet aux utilisateurs de rechercher une adresse en France
2.  **Géolocalisation** - Convertit une adresse textuelle en coordonnées (latitude/longitude)
3.  **Auto-complétion** - Suggestions d'adresses lors de la saisie

### Paramètres de recherche

| Paramètre | Description                                         |
|-----------|-----------------------------------------------------|
| `q`       | Texte de recherche (adresse, ville, code postal...) |
| `limit`   | Nombre maximum de résultats (défaut: 10)            |

### Format de réponse

L'API BAN renvoie des données au format GeoJSON. Dans GeoClimat, ces données sont traitées pour extraire:

-   **Label**: Adresse complète formatée
-   **Ville**: Nom de la commune
-   **Code postal**: Code postal à 5 chiffres
-   **Coordonnées**: Latitude et longitude (WGS84)
-   **Score**: Indice de confiance du résultat (0-1)

### Gestion des erreurs

L'application implémente une stratégie de repli:

1.  Tentative de recherche avec l'API BAN
2.  En cas d'échec ou de résultats vides, utilisation de l'API Nominatim (OpenStreetMap)

``` r
# Observer pour le bouton de recherche d'adresse
observeEvent(input$searchAddress, {
  query <- input$addressInput
  
  if (is.null(query) || nchar(query) < 3) {
    showNotification("Veuillez entrer au moins 3 caractères", type = "warning")
    return()
  }
  
  # Rechercher l'adresse avec l'API BAN
  print(paste("Recherche de l'adresse:", query))
  addresses <- tryCatch({
    search_address_BAN(query)
  }, error = function(e) {
    print(paste("Erreur lors de la recherche BAN:", e$message))
    return(list())
  })
  
  # Si aucun résultat avec BAN, essayer avec Nominatim
  if (length(addresses) == 0) {
    print("Aucun résultat avec BAN, tentative avec Nominatim")
    addresses <- tryCatch({
      search_address_nominatim(query)
    }, error = function(e) {
      print(paste("Erreur lors de la recherche Nominatim:", e$message))
      return(list())
    })
  }
  
  # Mettre à jour la liste des adresses trouvées
  found_addresses(addresses)
})
```

### Exemple d'utilisation

Dans GeoClimat, l'API BAN est principalement utilisée pour:

1.  Permettre à l'utilisateur de localiser un lieu précis sur la carte
2.  Générer un diagnostic climatique pour une adresse spécifique
3.  Charger automatiquement les données climatiques de la commune correspondante
