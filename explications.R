# Fonction pour générer l'onglet d'explications des indicateurs
generate_explanations_tab <- function() {
  tabsetPanel(
    # Onglet Tous les indicateurs (liste complète)
    tabPanel(
      title = "Liste complète des indicateurs",
      fluidRow(
        column(width = 12,
               h3("Indicateurs de température", style = "color: #d9534f; border-bottom: 1px solid #d9534f; padding-bottom: 5px;"),
               tags$div(
                 tags$b("NORTAV"), " - Température moyenne de l'air sur une période donnée, exprimée en degrés Celsius.", tags$br(),
                 tags$b("NORSTM0"), " - Somme de température 'en base 0°C' : Accumulation des températures journalières au-dessus de 0°C d'octobre à juillet, utilisée pour suivre le développement des cultures.", tags$br(),
                 tags$b("NORTXAV"), " - Température maximale : Valeur moyenne des températures les plus élevées enregistrées quotidiennement.", tags$br(),
                 tags$b("ATAV"), " - Écart de température moyenne : Différence entre la température moyenne observée et une valeur de référence.", tags$br(),
                 tags$b("ATXAV"), " - Écart de température maximale : Différence entre la température maximale observée et une valeur de référence.", tags$br(),
                 tags$b("NORSD"), " - Nombre de journées d'été : Nombre de jours où la température dépasse un seuil estival (souvent 25°C).", tags$br(),
                 tags$b("NORTX35"), " - Nombre de jours de forte chaleur : Nombre de jours où la température maximale atteint ou dépasse 35°C.", tags$br(),
                 tags$b("NORTXHWD"), " - Nombre de jours de vague de chaleur : Nombre de jours consécutifs où la température reste élevée, caractérisant une canicule.", tags$br(),
                 tags$b("NORTR"), " - Nombre de nuits tropicales : Nombre de nuits où la température ne descend pas en dessous de 20°C.", tags$br(),
                 tags$b("NORSDA"), " - Nombre de jours d'été d'avril à juin : Nombre de jours où la température dépasse un seuil estival sur cette période spécifique.", tags$br(),
                 tags$b("NORTNFD"), " - Nombre de jours de gel : Nombre de jours où la température descend sous 0°C.", tags$br(),
                 tags$b("NORTNCWD"), " - Nombre de jours de vague de froid : Nombre de jours consécutifs avec des températures très basses, caractérisant une période de froid intense.", tags$br(),
                 tags$b("ASDA"), " - Écart du nombre de jours d'été d'avril à juin : Différence entre le nombre de jours d'été sur cette période et une valeur de référence.", tags$br(),
                 tags$b("ASD"), " - Écart du nombre de journées d'été : Différence dans le nombre total de journées d'été par rapport à une période historique.", tags$br(),
                 tags$b("ATX35"), " - Écart du nombre de jours de forte chaleur : Différence entre le nombre de jours de forte chaleur observé et une moyenne historique.", tags$br(),
                 tags$b("ATXHWD"), " - Écart du nombre de jours de vague de chaleur : Variation du nombre de jours de canicule par rapport à une période de référence.", tags$br(),
                 tags$b("ATR"), " - Écart du nombre de nuits tropicales : Différence dans le nombre de nuits où la température reste élevée par rapport à une période donnée.", tags$br(),
                 tags$b("ATNFD"), " - Écart du nombre de jours de gel : Différence dans le nombre de jours de gel comparé à une période historique.", tags$br(),
                 tags$b("ATNCWD"), " - Écart du nombre de jours de vague de froid : Variation du nombre de jours de froid extrême par rapport à une moyenne de référence."
               ),
               
               h3("Indicateurs de précipitations", style = "color: #5bc0de; border-bottom: 1px solid #5bc0de; padding-bottom: 5px; margin-top: 20px;"),
               tags$div(
                 tags$b("NORRRA"), " - Cumul de précipitations d'avril à octobre : Total des précipitations enregistrées sur cette période.", tags$br(),
                 tags$b("NORRR"), " - Cumul de précipitations : Total des précipitations tombées sur une période donnée.", tags$br(),
                 tags$b("NORPQ90"), " - Précipitations quotidiennes intenses : Quantité de pluie tombée lors des jours où les précipitations sont dans les 10% les plus fortes.", tags$br(),
                 tags$b("NORPQ99"), " - Précipitations quotidiennes extrêmes : Quantité de pluie tombée lors des jours où les précipitations sont dans le 1% le plus extrême.", tags$br(),
                 tags$b("NORPFL90"), " - Pourcentage des précipitations intenses : Part des précipitations tombées lors des jours les plus pluvieux.", tags$br(),
                 tags$b("NORTPSPI"), " - Temps passé en sécheresse météorologique : Durée des périodes où il y a un déficit important de précipitations.", tags$br(),
                 tags$b("ARR"), " - Écart du cumul de précipitations : Différence entre la quantité de précipitations observée et une moyenne historique.", tags$br(),
                 tags$b("APQ90"), " - Écart de précipitations quotidiennes intenses : Différence dans les précipitations des jours les plus pluvieux par rapport à une période de référence.", tags$br(),
                 tags$b("APQ99"), " - Écart de précipitations quotidiennes extrêmes : Différence dans les précipitations des jours les plus pluvieux extrêmes par rapport à une période donnée.", tags$br(),
                 tags$b("APFL90"), " - Écart du pourcentage des précipitations intenses : Variation de la part des précipitations tombées lors des jours les plus pluvieux.", tags$br(),
                 tags$b("NORRR1MM"), " - Nombre de jours de pluie : Nombre de jours où il est tombé au moins 1 mm de pluie.", tags$br(),
                 tags$b("ARR1MM"), " - Écart du nombre de jours de pluie : Différence dans le nombre de jours de pluie par rapport à une période historique."
               ),
               
               h3("Indicateurs de risques d'incendie", style = "color: #f0ad4e; border-bottom: 1px solid #f0ad4e; padding-bottom: 5px; margin-top: 20px;"),
               tags$div(
                 tags$b("NORIFM40"), " - Sensibilité Feu Météo Élevée : Nombre de jours où l'indice de risque d'incendie (IFM12) dépasse 40, indiquant un risque important de départ de feu.", tags$br(),
                 tags$b("NORIFMxAV"), " - IFMx moyen : Valeur moyenne d'un indicateur météorologique de risque d'incendie.", tags$br(),
                 tags$b("NORIFMx50"), " - Danger Feu Météo Végétation Vivante Élevé : Nombre de jours où l'indice de risque d'incendie dépasse 50, signalant un danger critique.", tags$br(),
                 tags$b("AIFM40"), " - Écart de Sensibilité Feu Météo Élevée : Différence dans le nombre de jours où l'indice IFM12 dépasse 40 par rapport à une période de référence.", tags$br(),
                 tags$b("AIFMxAV"), " - Écart de IFMx moyen : Différence entre l'IFMx moyen observé et une valeur de référence.", tags$br(),
                 tags$b("AIFMx50"), " - Écart de Danger Feu Météo Végétation Vivante Élevé : Différence dans le nombre de jours où l'IFMx dépasse 50 par rapport à une période de référence."
               ),
               
               h3("Indicateurs agricoles et de végétation", style = "color: #5cb85c; border-bottom: 1px solid #5cb85c; padding-bottom: 5px; margin-top: 20px;"),
               tags$div(
                 tags$b("NORDATEVEG"), " - Date de la reprise de la végétation : Jour de l'année où la prairie commence à repousser après l'hiver.", tags$br(),
                 tags$b("NORDATEPG"), " - Date de la première gelée : Premier jour après le 1er juillet où la température descend sous 0°C.", tags$br(),
                 tags$b("NORDATEDG"), " - Date de la dernière gelée : Dernier jour après le 1er juillet où la température passe sous 0°C.", tags$br(),
                 tags$b("ADATEVEG"), " - Écart de la date de la reprise de la végétation : Différence entre la date effective de reprise de la végétation et une date moyenne de référence.", tags$br(),
                 tags$b("ADATEDG"), " - Écart de la date de la dernière gelée : Décalage entre la date réelle de la dernière gelée et une date moyenne historique."
               ),
               
               h3("Indicateurs de vent", style = "color: #337ab7; border-bottom: 1px solid #337ab7; padding-bottom: 5px; margin-top: 20px;"),
               tags$div(
                 tags$b("NORFFQ98"), " - Vent fort : Vitesse du vent correspondant aux 2% des jours les plus venteux.", tags$br(),
                 tags$b("AFFQ98"), " - Écart de vent fort : Différence dans l'intensité des vents forts par rapport à une valeur historique.", tags$br(),
                 tags$b("AFFAV"), " - Écart de la vitesse de vent quotidienne moyenne : Différence dans la vitesse moyenne du vent par rapport à une période donnée.", tags$br(),
                 tags$b("NORFF98"), " - Nombre de jours de vent > Q98 : Nombre de jours où le vent dépasse une valeur correspondant aux 2% des jours les plus venteux.", tags$br(),
                 tags$b("AFF98"), " - Écart du nombre de jours de vent > Q98 : Différence dans le nombre de jours avec des vents très forts par rapport à une moyenne historique.", tags$br(),
                 tags$b("AFF3"), " - Écart du nombre de jours sans vent : Variation dans le nombre de jours avec une absence significative de vent."
               )
            )
        )
      ),
    
    # Onglet Horizons et Scénarios
    tabPanel(
      title = "Horizons et Scénarios",
      fluidRow(
        column(width = 12,
               h3("Horizons temporels", style = "color: #5bc0de; border-bottom: 1px solid #5bc0de; padding-bottom: 5px;"),
               tags$div(
                 tags$b("REF"), " - Période de référence", tags$br(),
                 "Période historique utilisée comme base de comparaison.", tags$br(), tags$br(),
                 tags$b("H1 (2021-2050)"), " - Horizon proche", tags$br(),
                 "Projections climatiques pour le futur proche.", tags$br(), tags$br(),
                 tags$b("H2 (2041-2070)"), " - Horizon moyen", tags$br(),
                 "Projections climatiques pour le milieu du siècle.", tags$br(), tags$br(),
                 tags$b("H3 (2071-2100)"), " - Horizon lointain", tags$br(),
                 "Projections climatiques pour la fin du siècle."
               ),
               h3("Scénarios d'émissions", style = "color: #f0ad4e; border-bottom: 1px solid #f0ad4e; padding-bottom: 5px; margin-top: 20px;"),
               tags$div(
                 tags$b("RCP 2.6"), " - Émissions maîtrisées", tags$br(),
                 "Scénario optimiste impliquant une forte réduction des émissions de gaz à effet de serre et une neutralité carbone 
                 atteinte dans la seconde moitié du siècle. L'augmentation de température moyenne globale serait limitée à environ 2°C 
                 par rapport à l'ère préindustrielle.", tags$br(), tags$br(),
                 tags$b("RCP 4.5"), " - Émissions modérées", tags$br(),
                 "Scénario intermédiaire avec stabilisation des émissions à un niveau moyen, impliquant certaines mesures d'atténuation. 
                 L'augmentation de température moyenne serait d'environ 2,5 à 3°C d'ici 2100.", tags$br(), tags$br(),
                 tags$b("RCP 8.5"), " - Émissions non réduites", tags$br(),
                 "Scénario pessimiste avec des émissions continuant à augmenter tout au long du siècle. L'augmentation de température 
                 pourrait atteindre 4 à 5°C d'ici 2100, entraînant des impacts climatiques majeurs."
               )
            )
        )
    ),
    
    # Onglet Comment utiliser cette application
    tabPanel(
      title = "Utilisation de l'application",
      fluidRow(
        column(width = 12,
               h3("Guide d'utilisation", style = "color: #5cb85c; border-bottom: 1px solid #5cb85c; padding-bottom: 5px;"),
               tags$ol(
                 tags$li(tags$b("Sélectionnez un thème"), " : Choisissez parmi les indicateurs saisonniers, annuels, feux ou agricoles selon votre intérêt."),
                 tags$li(tags$b("Choisissez le format spatial"), " : Communes pour une vision détaillée, départements pour une vue plus globale."),
                 tags$li(tags$b("Sélectionnez un scénario climatique"), " : Du plus optimiste (RCP 2.6) au plus pessimiste (RCP 8.5)."),
                 tags$li(tags$b("Choisissez un horizon temporel"), " : De la période de référence (REF) au futur lointain (H3)."),
                 tags$li(tags$b("Sélectionnez un indicateur"), " : Choisissez l'indicateur spécifique que vous souhaitez visualiser."),
                 tags$li(tags$b("Confirmez vos choix"), " : Cliquez sur le bouton vert pour charger la carte."),
                 tags$li(tags$b("Explorez la carte"), " : Survolez ou cliquez sur les zones pour voir les valeurs détaillées."),
                 tags$li(tags$b("Exportez si nécessaire"), " : Utilisez le bouton de téléchargement pour obtenir une version PDF.")
               ),
               h3("Interprétation des résultats", style = "color: #5bc0de; border-bottom: 1px solid #5bc0de; padding-bottom: 5px; margin-top: 20px;"),
               tags$div(
                 tags$p("Les couleurs sur la carte indiquent l'intensité de l'indicateur sélectionné :"),
                 tags$ul(
                   tags$li(tags$b("Températures"), " : Du bleu (plus froid) au rouge (plus chaud)"),
                   tags$li(tags$b("Précipitations"), " : Du blanc/jaune clair (plus sec) au bleu foncé (plus humide)"),
                   tags$li(tags$b("Autres indicateurs"), " : L'échelle de couleur est adaptée à chaque variable")
                 ),
                 tags$p("Pour une analyse complète, il est recommandé de comparer :"),
                 tags$ul(
                   tags$li("Différents horizons temporels pour voir l'évolution dans le temps"),
                   tags$li("Différents scénarios pour comprendre la gamme des futurs possibles"),
                   tags$li("Différentes variables pour saisir les multiples aspects du changement climatique")
                 )
               )
            )
        )
    )
  )
} 