# DRIAS App - User Interface
library(shiny)
library(leaflet)
library(shinydashboard)
library(htmltools)
library(shinyjs)
library(shinythemes)

# Charger la source des explications
source("explications.R")

# Définir l'interface utilisateur
ui <- navbarPage(
  # Utilisation d'une div avec une classe spécifique pour le logo
  title = div(
    tags$img(src = "GeoClimat.png", height = "30px", class = "brand-logo", alt = "Logo GéoClimat"),
    "Visualisation des Données DRIAS"
  ),
  id = "navbarPage",
  
  # Utiliser le thème Quartz avec les styles personnalisés
  header = tags$head(
    # Inclure le CSS personnalisé
    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
  ),
  
  useShinyjs(),  # Activer shinyjs
  
  # Premier onglet - Carte interactive
  tabPanel(
    title = "Carte interactive 🗺️",
    
    # Ajouter un JavaScript personnalisé pour gérer la recherche d'adresse
    tags$head(
      tags$script("
        $(document).ready(function() {
          console.log('Document ready, initializing address search handlers');
          
          // Gestionnaire pour le bouton de recherche
          $(document).on('click', '#searchBtn', function(e) {
            e.preventDefault(); // Empêcher le comportement par défaut
            console.log('Search button clicked');
            var address = $('#addressInput').val() || '';
            console.log('Search address: ' + address);
            Shiny.setInputValue('searchBtnClicked', {
              address: address,
              timestamp: new Date().getTime()
            });
          });
          
          // Gestionnaire pour la touche Entrée dans le champ de recherche
          $(document).on('keyup', '#addressInput', function(e) {
            if (e.key === 'Enter') {
              e.preventDefault(); // Empêcher le comportement par défaut
              console.log('Enter key pressed in address input');
              var address = $(this).val() || '';
              console.log('Search address: ' + address);
              Shiny.setInputValue('searchBtnClicked', {
                address: address,
                timestamp: new Date().getTime()
              });
            }
          });
          
          // Gestionnaire pour les résultats de recherche
          $(document).on('click', '.address-result', function(e) {
            e.preventDefault(); // Empêcher le comportement par défaut
            console.log('Address result clicked');
            var index = $(this).index() + 1;
            console.log('Selected index: ' + index);
            Shiny.setInputValue('selectedAddress', index, {priority: 'event'});
          });
          
          // Récepteur de message personnalisé pour mettre à jour les résultats
          Shiny.addCustomMessageHandler('updateSearchResults', function(message) {
            console.log('Updating search results');
            if ($('#searchResults').length) {
              $('#searchResults').html(message);
            } else {
              console.error('searchResults element not found');
            }
          });
        });
      ")
    ),
  
    sidebarLayout(
      sidebarPanel(
        div(class = "glass-panel",
          selectInput("theme", "Thème:", 
                  choices = c("DRIAS - Indicateurs Saisonniers" = "INDICATEURS_SAISONNIERS_ETE",
                              "DRIAS - Indicateurs Annuels" = "INDICATEURS_ANNUELS_HORIZONS",
                              "🔥 DRIAS FEUX - Indicateurs Annuels" = "FEUX_INDICATEURS_ANNUELS_HORIZONS",
                              "🌱 DRIAS AGRI - Indicateurs Annuels" = "AGRI_INDICATEURS_ANNUELS_HORIZONS")),
      
          checkboxInput("use_departments", "Passer la carte au format départements", value = FALSE),
      
          selectInput("scenario", "Scénario:", choices = NULL),
      
          selectInput("horizon", "Horizon:", choices = NULL),
      
          selectInput("variable", "Indicateur:", choices = NULL),
      
          # Bouton pour confirmer les sélections et charger la carte
          actionButton("confirmChoices", "Confirmer et charger la carte ✅", 
                    style = "margin-top: 15px; margin-bottom: 15px; width: 100%;"),
      
          # Boutons de téléchargement dans un conteneur div avec style
          tags$div(
            style = "margin-top: 15px; display: flex; flex-direction: column; gap: 10px;",
            # Bouton pour télécharger la carte en PDF
            downloadButton("downloadPDF", "Télécharger la carte (PDF) 📄", 
                          class = "btn-download"),
            
            # Bouton pour télécharger les données en Excel
            downloadButton("downloadExcel", "Télécharger les données (Excel) 📊", 
                          class = "btn-download")
          )
        ),
        width = 3
      ),
      
      mainPanel(
        # Barre de recherche d'adresse au-dessus de la carte
        div(class = "glass-panel search-container",
          style = "margin-bottom: 20px; padding: 15px;",
          tags$div(
            style = "display: flex; flex-direction: column; gap: 10px;",
            tags$h4("Rechercher une adresse", style = "margin-top: 0; margin-bottom: 5px;"),
            tags$div(
              style = "display: flex; gap: 10px;",
              tags$input(id = "addressInput", type = "text", placeholder = "Entrez une adresse...", 
                        style = "flex-grow: 1; padding: 12px;"),
              tags$button(id = "searchBtn", type = "button", "🔍 Rechercher", 
                        style = "padding: 12px 20px;")
            ),
            tags$div(id = "searchResults", style = "margin-top: 10px; max-height: 200px; overflow-y: auto;"),
            # Bouton de diagnostic conditionnel
            conditionalPanel(
              condition = "output.hasSelectedAddress == true",
              div(
                style = "margin-top: 10px; text-align: right;",
                actionButton("goDiagnostic", "📊 Voir le diagnostique climatique", 
                            icon = icon("chart-line"))
              )
            )
          )
        ),
        
        # Carte
        div(class = "glass-panel map-container",
          leafletOutput("map", height = "700px")
        ),
        width = 9
      )
    )
  ),
  
  # Deuxième onglet - Explications des indicateurs
  tabPanel(
    title = "Explications des indicateurs 🧭",
    fluidRow(
      column(width = 12,
             h2("Guide des indicateurs DRIAS", style = "text-align: center; margin-bottom: 30px;"),
             p("Cette section fournit des explications sur les différents indicateurs disponibles dans l'application DRIAS. 
               Ces indicateurs permettent de comprendre l'évolution du climat et ses impacts potentiels sur différents secteurs.",
               style = "font-size: 16px; margin-bottom: 20px;")
      )
    ),
    
    # Utiliser la fonction generate_explanations_tab du fichier explications.R
    generate_explanations_tab()
  ),
  
  # Troisième onglet - Diagnostic climatique
  tabPanel(
    title = "Diagnostique 🩺",
    fluidRow(
      column(width = 12,
             div(class = "glass-panel text-center",
               h2("Diagnostique climatique personnalisé", style = "margin-bottom: 20px;"),
               p("Cette page vous permet d'obtenir un diagnostique personnalisé des projections climatiques pour votre commune.", 
                 style = "font-size: 16px; margin-bottom: 20px;")
             )
      )
    ),
    
    # Contenu principal
    fluidRow(
      # Panneau latéral gauche
      column(width = 4,
        div(class = "glass-panel",
          h3("Adresse sélectionnée"),
          textOutput("diagSelectedAddress"),
          textOutput("diagSelectedCommune"),
          downloadButton("downloadDiagnostic", "Télécharger le diagnostique complet (PDF)", 
                      class = "btn-download"),
          uiOutput("diagInstructions")
        )
      ),
      
      # Panneau principal droit avec graphiques
      column(width = 8,
        div(class = "glass-panel", style = "padding: 20px;",
          h3("Projections climatiques pour votre commune", style = "margin-bottom: 20px;"),
          
          # Onglets pour les différents graphiques
          tabsetPanel(
            tabPanel("Températures", plotOutput("diagTempPlot", height = "500px")),
            tabPanel("Précipitations", plotOutput("diagPrecipPlot", height = "500px")),
            tabPanel("Vagues de chaleur", plotOutput("diagHeatwavePlot", height = "500px"))
          )
        )
      )
    ),
    
    # Ajout d'une nouvelle ligne avec le tableau récapitulatif
    fluidRow(
      column(width = 12,
        div(class = "glass-panel", style = "padding: 20px; margin-top: 20px;",
          h3("Résumé des indicateurs climatiques", style = "margin-bottom: 20px;"),
          p("Tableau récapitulatif des principaux indicateurs climatiques pour votre commune.", 
            style = "margin-bottom: 15px;"),
          div(style = "overflow-x: auto;",
            tableOutput("diagSummaryTable") %>% 
              tagAppendAttributes(class = "table table-striped table-hover", 
                                  style = "width: 100%; font-size: 16px; margin-top: 10px;")
          )
        )
      )
    )
  ),
  
  # Nouvel onglet - Qualité de l'eau (Hub'Eau)
  tabPanel(
    title = "Qualité de l'eau 💧",
    fluidRow(
      column(width = 12,
             div(class = "glass-panel text-center",
               h2("Données de qualité des cours d'eau", style = "margin-bottom: 20px;"),
               p("Accédez aux données sur la qualité physico-chimique des fleuves, rivières et plans d'eau. 
                 Source: Hub'Eau (API Qualité des cours d'eau)",
                 style = "font-size: 16px; margin-bottom: 20px;")
             )
      )
    ),
    
    # Contenu principal
    fluidRow(
      # Panneau latéral gauche (recherche)
      column(width = 3,
        div(class = "glass-panel",
          h3("Recherche de stations"),
          
          # Onglets pour les différentes méthodes de recherche
          tabsetPanel(
            id = "stationSearchTabs",
            tabPanel("Par commune",
              textInput("waterQualityCommune", "Nom de la commune", ""),
              actionButton("searchStationsByCommune", "Rechercher", 
                           class = "btn-primary", style = "width: 100%; margin-top: 10px")
            ),
            tabPanel("Par coordonnées",
              numericInput("waterQualityLon", "Longitude", value = NULL, step = 0.01),
              numericInput("waterQualityLat", "Latitude", value = NULL, step = 0.01),
              numericInput("waterQualityDist", "Rayon de recherche (km)", value = 5, min = 1, max = 50),
              actionButton("searchStationsByCoords", "Rechercher", 
                           class = "btn-primary", style = "width: 100%; margin-top: 10px")
            ),
            tabPanel("Par cours d'eau",
              textInput("waterQualityRiver", "Nom du cours d'eau", ""),
              actionButton("searchStationsByRiver", "Rechercher", 
                           class = "btn-primary", style = "width: 100%; margin-top: 10px")
            ),
            tabPanel("Par département",
              textInput("waterQualityDept", "Code département", ""),
              actionButton("searchStationsByDept", "Rechercher", 
                           class = "btn-primary", style = "width: 100%; margin-top: 10px")
            )
          ),
          
          # Résultats de la recherche
          h4("Stations trouvées", style = "margin-top: 20px"),
          htmlOutput("stationSearchInfo"),
          selectInput("selectedStation", "Sélectionner une station", choices = NULL),
          
          # Sélection du paramètre (visible seulement une fois la station sélectionnée)
          conditionalPanel(
            condition = "output.hasStationSelected == true",
            h4("Paramètres disponibles"),
            selectInput("selectedParameter", "Paramètre", choices = NULL),
            dateRangeInput("waterQualityDateRange", "Période d'analyse",
                          start = Sys.Date() - 365*5,  # 5 ans en arrière par défaut
                          end = Sys.Date(),
                          separator = " au "),
            actionButton("loadWaterQualityData", "Charger les données", 
                       class = "btn-primary", style = "width: 100%; margin-top: 10px"),
            downloadButton("downloadWaterQualityData", "Télécharger les données (CSV)", 
                          class = "btn-download", style = "width: 100%; margin-top: 10px")
          )
        )
      ),
      
      # Panneau principal (visualisation)
      column(width = 9,
        # Carte des stations
        div(class = "glass-panel map-container",
          h3("Carte des stations"),
          leafletOutput("waterQualityMap", height = "400px")
        ),
        
        # Visualisation des données
        div(class = "glass-panel",
          h3("Analyse de la qualité de l'eau"),
          htmlOutput("waterQualityInfo"),
          plotOutput("waterQualityPlot", height = "400px"),
          # Ajout du tableau sous le graphique
          h4("Tableau récapitulatif des données", style = "margin-top: 20px; margin-bottom: 10px;"),
          tableOutput("waterQualityTable")
        )
      )
    )
  ),
  
  # Quatrième onglet - Contact
  tabPanel(
    title = "Contact 📧",
    fluidRow(
      column(width = 12,
             div(class = "glass-panel text-center",
               h2("Signaler un bug ou demander des fonctionnalités", style = "margin-bottom: 20px;"),
               p("Vous avez repéré un bug ou vous avez une idée d'amélioration pour cette application ? N'hésitez pas à me contacter !", 
                 style = "font-size: 16px; margin-bottom: 20px;")
             )
      )
    ),
    
    # Contenu avec informations de contact amélioré
    fluidRow(
      column(width = 6, offset = 3,
             div(class = "glass-panel contact-card",
               style = "padding: 30px; text-align: center; border-radius: 15px; background: linear-gradient(145deg, rgba(255,255,255,0.8), rgba(240,240,255,0.9));",
               h3("Contact", style = "margin-bottom: 30px; color: #336699; font-weight: bold;"),
               
               # Photo de profil (optionnelle - vous pouvez ajouter une image dans le dossier www/)
               # tags$div(
               #   style = "margin-bottom: 20px;",
               #   tags$img(src = "profile.jpg", style = "width: 120px; height: 120px; border-radius: 50%; object-fit: cover; border: 3px solid #fff; box-shadow: 0 4px 8px rgba(0,0,0,0.1);")
               # ),
               
               # Informations de contact stylisées
               tags$div(
                 class = "contact-info",
                 style = "font-size: 18px; line-height: 1.8; margin-bottom: 30px;",
                 tags$p(HTML("<strong style='font-size: 22px; color: #333;'>Noa Leroux</strong>")),
                 tags$p(HTML("<i class='fas fa-university' style='color: #336699; width: 20px; margin-right: 8px;'></i> Université de Nantes")),
                 tags$p(HTML("<i class='fas fa-graduation-cap' style='color: #336699; width: 20px; margin-right: 8px;'></i> Master Statistiques Économétrie"))
               ),
               
               # Bouton email stylisé
               tags$div(
                 style = "margin-top: 20px;",
                 tags$a(
                   href = "mailto:noalerouxx@gmail.com",
                   class = "btn-contact",
                   HTML("<i class='fas fa-envelope' style='margin-right: 10px;'></i> Me contacter par email")
                 )
               )
             )
      )
    )
  ),
  
  # Footer
  footer = tags$div(
    class = "footer",
    paste("© GéoClimat", format(Sys.Date(), "%Y"), "- Application de visualisation des données DRIAS")
  )
) 