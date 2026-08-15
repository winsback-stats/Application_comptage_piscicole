# ==============================================================================
# Application de visualisation et d'analyse des données de vidéocomptage
# ==============================================================================
#
# Script : fonctions.R
# Auteur : Romain Winsback (adresse permanente : romain.winsback04@gmail.com)
# Structure : SHEM
# Site : Pont de la Reine
#
# Projet :
#   Développement d'une application sous R Shiny, destinée à faciliter le
#   traitement des données de vidécomptage du site de Pont de la Reine.
#
# Objet du script :
# Ce script constitue le point d'entrée de l'application Shiny.
#
# Il contient :
#   - la définition de l'interface utilisateur (UI)
#   - la définition de la logique serveur
#   - la gestion des interactions avec l'utilisateur
#   - le déclenchement des fonctions de traitement définies dans fonctions.R
#   - la gestion de l'importation et de la sauvegarde des données 
#   - la gestion des périodes de dysfonctionnement du dispositif
#   - l'affichage et l'export des résultats
#
# Dépendances principales :
#   dplyr, tidyr, lubridate, ggplot2, httr, jsonlite, openxlsx, purrr, tibble
#
# Fichiers associés :
#   fonctions.R : fonctions de traitement et de visualisation
#   data/ : dossier contenant les jeux de données annuels sauvegardés au format RDS
#   pannes/ : dossier contenant les périodes de dysfonctionnement sauvegardées en RDS
#
#
# Date de création : 25-03-2026
# 
# Date de dernière mise à jour : 13-08-2026
#
# Historique des versions : 
#   1.0 | 13-08-2026
#   Version destinée à la mise à disposition et à la reprise par un développeur.
#
# Remarques :
#   Les principales fonctions de traitement sont regroupées dans fonctions.R.
#   Pensez à enregistrer la version précédente avant d'effectuer des modifications.
#
# ==============================================================================


# ==============================================================================
# CHARGEMENT DES DEPENDANCES
# ==============================================================================
#
# Les packages suivants sont nécessaires au fonctionnement de l'interface, à la 
# manipulation des données et aux exports.
#
# Certaines fonctions de traitement se trouvent dans fonctions.R, d'où l'appel
# de ce script.
# ==============================================================================

library(shiny)
library(readr)
library(tidyverse)
library(lubridate)
library(DT)
library(openxlsx)
library(httr)
library(jsonlite)

source("fonctions.R")  # Chargement des fonctions de traitement

# Création du dossier de stockage des périodes de panne s'il n'existe pas.
# Les périodes sont enregistrées séparément des jeux de données annuels.
if(!dir.exists("pannes")){
  dir.create("pannes")
}


# ==============================================================================
# INTERFACE UTILISATEUR
# ==============================================================================
#
# L'interface est organisée en quatre espaces principaux :
#
#   1. Import des données
#       Sélection de l'année, des espèces et import du vidéocomptage, puis import
#       des données environnementales.
#
#   2. Pannes
#       Saisie, modification et sauvegarde des périodes de dysfonctionnement du
#       système vidéo.
#
#   3. Analyse annuelle
#       Exploration des données d'une année à travers plusieurs onglets :
#       synthèse, espèces, tailles et activité horaire.
#
#   4. Analyse interannuelle
#       Comparaison des données sauvegardées pour plusieurs années.
#
# Les éléments affichés dans l'interface sont ensuite alimentés par divers objets
# réactifs et fonctions définis dans la partie serveur (server).
# ==============================================================================

ui <- fluidPage(
  tags$div(
  style="
  background:linear-gradient(90deg,#1f4e79,#2e75b6);
  color:white;
  padding:25px;
  margin-bottom:20px;
  border-radius:10px;
  text-align:center;
  ",
  h1(
    "Passe à poissons de Pont de la Reine",
    style="
    font-weight:bold;
    margin-bottom:5px;
    "),
  h4("Outil d'aide à l'analyse des données de vidéocomptage",
     style="font-weight:normal;
     opacity:0.9;")
  ),
# ------------------------------------------------------------------------------
# Mise en forme de l'interface
# ------------------------------------------------------------------------------
# Les règles CSS suivantes définissent l'apparence générale de l'application :
# onglets, titres de sections, tableaux, boutons et espacements.
#
# Elles n'ont pas de lien avec le traitement des données.
# ------------------------------------------------------------------------------
  tags$head(
    tags$style(HTML("
                    
                    .nav-tabs > li > a {
                    font-weight: bold;
                    font-size: 14px;
                    }
                    
                    .nav-tabs > li.active > a {
                    background-color: #1f4e79 !important;
                    color: white !important;
                    }
                    
                    .tabbable .nav-tabs {
                    margin-bottom: 20px;
                    }
                    
                    .tabbable .nav-tabs {
                    border-radius: 8px 8px 0 0;
                    }
                    
                    table.dataTable thead {
                    background-color: #1f4e79;
                    color: white;
                    }
                    
                    table.dataTable tbody tr:hover {
                    background-color: #f2f7fc;
                    }
                    
                    table.dataTable tbody td {
                    padding: 6px 12px;
                    }
                    
                    table.dataTable td,
                    table.dataTable th {
                    border-right: 1px solid #e5e5e5;
                    }
                    
                    .section-title{
                    background:linear-gradient(90deg,#1f4e79,#2e75b6);
                    color:white;
                    padding:12px 18px;
                    border-radius:8px;
                    font-size:24px;
                    font-weight:bold;
                    margin-top:20px;
                    margin-bottom:15px;
                    }
                    
                    .subsection-title{
                    color:#1f4e79;
                    font-size:20px;
                    font-weight:bold;
                    border-left:4px solid #1f4e79;
                    padding-left:10px;
                    margin-top:20px;
                    margin-bottom:10px;
                    }
                    
                    .form-control{
                    border-radius:8px;
                    }
                    
                    .btn-default{
                    background-color:#1f4e79;
                    color:white;
                    border:none;
                    }
                    
                    .btn-default:hover{
                    background-color:#2e75b6;
                    color:white;
                    }
                    
                    #run_analysis{
                    background-color:#198754 !important;
                    color:white !important;
                    border:none !important;
                    font-size:18px !important;
                    font-weight:bold !important;
                    padding:12px 30px !important;
                    border-radius:10px !important;
                    }
                    
                    #run_analysis:hover{
                    background-color:#157347 !important;
                    color:white !important;
                    }
                    
                    #add_panne{
                    background-color:#198754;
                    color:white;
                    border:none;
                    }
                    
                    #delete_panne{
                    background-color:#dc3545;
                    color:white;
                    border:none;
                    }
                    
                    #save_pannes{
                    background-color:#1f4e79;
                    color:white;
                    border:none;
                    }
                    
                    #recompute_analysis{
                    background-color:#fd7e14;
                    color:white;
                    border:none;
                    }
                  
                    .container-fluid {
                    padding-bottom: 80px;
                    }"))
  ),
                tabsetPanel(
                  # ------------------------------------------------------------
                  # ONGLET 1 - IMPORT DES DONNEES
                  # ------------------------------------------------------------
                  tabPanel("Import des données",
                           # L'utilisateur sélectionne l'année à traiter et les 
                           # espèces à importer depuis l'API d'Hizkia.
                           numericInput("annee","Année des données",value=2025),
                           uiOutput("etat_import"),
                           tags$div(
                             class="section-title",
                             icon("video"),
                             " Vidéocomptage"
                             ),
                           selectizeInput("especes_api","Espèces",
                                          choices=correspondance_especes$code,
                                          selected=c("TRF","TRM","SAT"),
                                          multiple=TRUE),
                           
                           # Bouton d'import automatique des données
                           actionButton("import_video","Importer les données de vidéocomptage pour l'année sélectionnée",
                                        icon=icon("download")),
                           
                           # Données environnementales importées depuis disque local
                           tags$div(
                             class="section-title",
                             icon("water"),
                             " Paramètres environnementaux"),
                           tags$div(class="subsection-title","🌡 Température"), # la température pourra à l'avenir être importée par l'API d'Hizkia
                           fileInput("temp","Données de température (Ibaï Begi)"),
                           tags$div(class="subsection-title","💧 Hydrologie"),
                           fileInput("qmoy","Débit moyen (SHEM Indus)"),
                           fileInput("qmin","Débit minimum (SHEM Indus)"),
                           fileInput("qmax","Débit maximum (SHEM Indus)"),
                           fileInput("heau","Hauteur d'eau (Pescadères)"),
                           
                           # Bouton "Lancer l'analyse" déclenche la préparation et le nettoyage des données,
                           # ainsi que leur sauvegarde au format RDS pour l'année correspondante.
                           actionButton("run_analysis","Lancer l'analyse",icon=icon("play"),class="btn btn-success btn-analyse"),
                           DTOutput("apercu_video")),
                  
                  # ------------------------------------------------------------
                  # ONGLET 2 - PERIODES DE DYSFONCTIONNEMENT
                  # ------------------------------------------------------------
                  # Cet onglet permet de renseigner les périodes de panne du dispositif vidéo.
                  tabPanel("Pannes",
                           
                           # Sélection de l'année par l'utilisateur
                           selectInput("annee_pannes","Année",choices=gsub(
                             "\\.rds$","",list.files("data",pattern="\\.rds$")
                             )),
                           tags$div(
                             class="section-title",
                             icon("triangle-exclamation"),
                             " Périodes de dysfonctionnement du dispositif vidéo"),
                           
                           # Saisie des périodes de panne par l'utilisateur
                           fluidRow(
                             column(
                              6,
                              dateInput(
                                "debut_date",
                                "Début de la panne",
                                value=Sys.Date()),
                              selectInput(
                                "debut_heure",
                                "Heure de début",
                                choices=sprintf("%02d:00",0:23),
                                selected="00:00"
                              )),
                             column(
                              6,
                              dateInput(
                                "fin_date",
                                "Fin de la panne",
                                value=Sys.Date()),
                              selectInput(
                                "fin_heure",
                                "Heure de fin",
                                choices=sprintf("%02d:00",0:23),
                                selected="01:00"
                              ))),
                           
                           # Boutons d'ajout/suppression de pannes
                           actionButton("add_panne","Ajouter la période",icon=icon("plus")),
                           actionButton("delete_panne","Supprimer la période sélectionnée",icon=icon("trash")),
                           
                           # Affichage dans l'interface
                           uiOutput("resume_pannes"),
                           DTOutput("table_pannes"),
                           
                           # Boutons de sauvegarde des pannes et de mise à jour des résultats
                           actionButton("save_pannes","Enregistrer les pannes",icon=icon("save")),
                           actionButton("recompute_analysis","Mettre à jour les analyses",icon=icon("rotate"))),
                  
                  # ------------------------------------------------------------
                  # ONGLET 3 - ANALYSE ANNUELLE
                  # ------------------------------------------------------------
                  #
                  # Cet onglet permet de sélectionner une année déjà enregistrée
                  # dans le dossier "data/" et d'en explorer les données.
                  tabPanel("Analyse annuelle",
                           
                           # Sélection de l'année par l'utilisateur
                           selectInput("annee_analyse","Année",choices=gsub(
                             "\\.rds$","",list.files("data",pattern="\\.rds$")
                           )),
                           
                           # Bouton d'export des données en excel
                           tags$div(style="margin-bottom:12px",
                                    downloadButton("export_excel_annuel","Exporter l'analyse annuelle sous Excel")),
                           
                           tabsetPanel(
                             
                             # SOUS-ONGLET 3.1 - SYNTHESE
                             tabPanel("Synthèse",
                                      
                                      # Résumé des données
                                      uiOutput("resume_global"),
                                      DTOutput("resume_mensuel"),
                                      actionButton("reset_resume","Réinitialiser le tableau"),
                                      
                                      # Tableau des périodes de X jours consécutifs sans montaison
                                      tags$div(
                                        style="margin-top: 20px;",
                                        # Message d'information
                                        tags$label("Afficher les périodes sans montaison d'au moins (jours) : ",
                                                   tags$span(
                                                     "ⓘ",
                                                     title="Cette fonctionnalité permet d'identifier les périodes durant lesquelles aucune montaison n'a été enregistrée pendant plusieurs jours consécutifs. Choisissez la durée minimale.",
                                                     style="cursor: help; font-weight: bold;")),
                                        # Choix du nombre de jours par l'utilisateur
                                        numericInput("seuil_no_passage",NULL,
                                                     value=5,min=1,step=1,width="250px")),
                                      # Rendu tableau
                                      DTOutput("no_passage_periods"),
                                      
                                      # Graphiques
                                      plotOutput("conditions_env"),
                                      tags$div(
                                        style="margin-bottom:15px;",
                                        actionButton("confirm_conditions_env","Télécharger PNG",icon=icon("download"))),
                                      plotOutput("daily_passage"),
                                      tags$div(
                                        style="margin-bottom:15px;",
                                        actionButton("confirm_daily_passage","Télécharger PNG",icon=icon("download")))),
                             
                             # SOUS-ONGLET 3.2 - ESPECES
                             tabPanel("Espèces",
                                      DTOutput("tableau_especes"),
                                      uiOutput("interpretation_especes")),
                             
                             # SOUS-ONGLET 3.3 - TAILLES
                             tabPanel("Tailles",
                                    plotOutput("taille_especes",height="500px"),
                                    tags$div(
                                      style="margin-bottom:15px;",
                                      actionButton("confirm_taille_especes","Télécharger PNG",icon=icon("download"))),
                                    DTOutput("table_taille_especes"),
                                    uiOutput("interpretation_taille")),
                             
                             # SOUS-ONGLET 3.4 - ACTIVITE HORAIRE
                             tabPanel("Activité horaire",
                                    plotOutput("horaire_global",height="400px"),
                                    tags$div(style="margin-bottom:15px;",
                                             actionButton("confirm_horaire_global","Télécharger PNG",icon=icon("download"))),
                                    plotOutput("horaire_mensuel",height="700px"),
                                    tags$div(style="margin-bottom:15px;",
                                             actionButton("confirm_horaire_mensuel","Télécharger PNG",icon=icon("download"))),
                                    DTOutput("table_horaire"))
                             )),
                  
                  # ------------------------------------------------------------
                  # ONGLET 4 - ANALYSE INTERANNUELLE
                  # ------------------------------------------------------------
                  # Permet de sélectionner plusieurs années parmi les jeux de données
                  # sauvegardés dans "data/" afin d'en comparer les résultats.
                  #
                  # Ces comparaisons sont descriptives et n'ont pas de valeur
                  # statistique en l'absence des tests adaptés.
                  # ------------------------------------------------------------
                  tabPanel("Analyse interannuelle",
                           
                           # Sélection des années souhaitées
                           selectInput("annees_comparees","Années",
                                     choices=gsub("\\.rds$",
                                                  "",
                                                  list.files("data",
                                                             pattern="\\.rds$")),
                                     multiple=TRUE,
                                     selected=gsub("\\.rds$","",list.files("data",
                                                                           pattern="\\.rds$"))),
                           
                           # Bouton d'export des données sous Excel
                           tags$div(style="margin-bottom:12px;",
                                  downloadButton("export_excel_interannuel","Exporter l'analyse interannuelle sous Excel")),
                           tabsetPanel(
                             
                             # SOUS-ONGLET 4.1 - SYNTHESE
                             tabPanel("Synthèse",
                                    DTOutput("table_interannuelle"),
                                    
                                    plotOutput("plot_interannuel"),
                                    tags$div(
                                      style="margin-bottom:15px;",
                                      actionButton("confirm_plot_interannuel","Télécharger PNG",icon=icon("download"))),
                                    
                                    plotOutput("plot_interannuel_mensuel"),
                                    tags$div(
                                      style="margin-bottom:15px;",
                                      actionButton("confirm_plot_interannuel_mensuel","Télécharger PNG",icon=icon("download"))),
                                    
                                    DTOutput("table_interannuelle_mensuelle")),
                             
                             # SOUS-ONGLET 4.2 - ESPECES
                             tabPanel("Espèces",
                                    DTOutput("table_interannuelle_especes"),
                                    
                                    plotOutput("plot_interannuel_especes"),
                                    tags$div(
                                      style="margin-bottom:15px;",
                                      actionButton("confirm_plot_interannuel_especes","Télécharger PNG",icon=icon("download")))),
                             
                             # SOUS-ONGLET 4.3 - TAILLES
                             tabPanel("Tailles",
                                    DTOutput("table_taille_interannuelle"),
                                    plotOutput("plot_taille_interannuelle",height="700px"),
                                    tags$div(
                                      style="margin-bottom:15px;",
                                      actionButton("confirm_plot_taille_interannuelle","Télécharger PNG",icon=icon("download")))),
                             
                             # SOUS-ONGLET 4.4 - ACTIVITE HORAIRE
                             tabPanel("Activité horaire",
                                    DTOutput("table_horaire_interannuelle"),
                                    plotOutput("plot_horaire_interannuel",height="500px"),
                                    tags$div(
                                      style="margin-bottom:15px;",
                                      actionButton("confirm_plot_horaire_interannuel","Télécharger PNG",icon=icon("download"))))))))



# ==============================================================================
# LOGIQUE SERVEUR
# ==============================================================================
#
# La partie serveur assure le lien entre les éléments de l'interface et les
# fonctions de traitement définies dans fonctions.R.
#
# Les principaux mécanismes utilisés sont :
#   input : valeurs sélectionnées ou saisies par l'utilisateur
#   output : éléments affichés dans l'interface
#   reactive / reactiveValues : objets permettant de conserver et de mettre à
#                               jour les données pendant la session
#   observeEvent : déclenchement d'une action en réponse à une interaction utilisateur.
#
# Les données sauvegardées permettent ensuite de consulter les résultats sans
# avoir à refaire l'ensemble du traitement.
# ==============================================================================

server<-function(input,output,session){
  
  # ----------------------------------------------------------------------------
  # OBJETS REACTIFS
  # ----------------------------------------------------------------------------
  
  # Compteur utilisé pour forcer le rafraîchissement du tableau de synthèse
  # mensuelle lorsque l'utilisateur demande sa réinitialisation.
  reset_resume <- reactiveVal(0)
  
  # Compteur utilisé pour forcer le rechargement des données traitées après une
  # modification des périodes de panne.
  refresh_analyse <- reactiveVal(0)
  
  # Contient les différents jeux de données utilisés pendant la session.
  # Ces objets sont alimentés progressivement lors de l'import et du lancement
  # de l'analyse.
  donnees <- reactiveValues()
  
  donnees$pannes <- tibble(
    debut=as.POSIXct(character()),
    fin=as.POSIXct(character())
  )
  
  # ----------------------------------------------------------------------------
  # AJOUT D'UNE PERIODE DE PANNE
  # ----------------------------------------------------------------------------
  #
  # Action déclenchée lorsque l'utilisateur clique sur "Ajouter la période"
  observeEvent(input$add_panne,{
    debut<-ymd_hm(
      paste(input$debut_date,input$debut_heure)
    )
    fin <- ymd_hm(
      paste(input$fin_date,input$fin_heure)
    )
    
    # Contrôle 1 : la date de fin doit être postérieure à celle de début
    if(fin <= debut){
      showNotification(
        "La date de fin doit être postérieure à la date de début.",
        type="error"
      )
      return()
    }
    
    # Contrôle 2 : les deux dates doivent appartenir à l'année sélectionnée
    if(year(debut)!=as.numeric(input$annee_pannes)||
       year(fin)!=as.numeric(input$annee_pannes)){
      showNotification(
        "Les dates ne correspondent pas à l'année sélectionnée.",
        type="error"
      )
      return()
    }
    
    # Contrôle 3 : la période ne doit pas déjà exister
    if(!is.null(donnees$pannes)){
      if(any(donnees$pannes$debut==debut &
             donnees$pannes$fin==fin)){
        showNotification(
          "Cette période de panne existe déjà.",
          type="error"
          )
        return()
      }
    }
    
    # Si contrôles validés, ajout de la nouvelle période au tableau
    nouvelle_ligne<-tibble(
      debut=debut,
      fin=fin)
    
    donnees$pannes <- bind_rows(
      donnees$pannes,
      nouvelle_ligne
    ) %>%
      arrange(debut)  # tri chronologique
  })

  # ----------------------------------------------------------------------------
  # AFFICHAGE DU RESUME DES PANNES
  # ----------------------------------------------------------------------------
  #
  # Calcule et affiche le nombre de périodes de panne ainsi que leur durée cumulée.
  # ----------------------------------------------------------------------------
  output$resume_pannes <- renderUI({
    nb_pannes <- nrow(donnees$pannes)
    duree_totale <- round(sum(
      as.numeric(difftime(
        donnees$pannes$fin,
        donnees$pannes$debut,
        units="hours"
      )),
      na.rm=TRUE
    ),
    1)
    tags$div(style="
             background-color:#f2f7fc;
             border-left:6px solid #1f4e79;
             border-radius:8px;
             padding:12px;
             margin-top:15px;
             margin-bottom:15px;
             ",
             tags$b("Nombre de pannes : "),nb_pannes,
             tags$br(),
             tags$b("Durée totale : "),paste0(duree_totale," h"))
  })
  
  # ----------------------------------------------------------------------------
  # AFFICHAGE DU TABLEAU DES PANNES
  # ----------------------------------------------------------------------------
  #
  # Affiche les périodes enregistrées dans donnees$pannes.
  # ----------------------------------------------------------------------------
  output$table_pannes <- renderDT({
    tab <- donnees$pannes %>%
      mutate(Début=format(debut,"%Y-%m-%d %H:%M:%S"),
             Fin = format(fin,"%Y-%m-%d %H:%M:%S"))%>%
      select(Début,Fin)
    datatable_passe(tab)},
    selection="single")
  
  # ----------------------------------------------------------------------------
  # SUPPRESSION D'UNE PERIODE DE PANNE
  # ----------------------------------------------------------------------------
  #
  # Supprime la ligne sélectionnée dans le tableau des périodes de panne.
  # ----------------------------------------------------------------------------
  observeEvent(input$delete_panne,{
    
    # Avertissement si clic sur bouton "Supprimer" alors qu'aucune panne n'est sélectionnée
    if(length(input$table_pannes_rows_selected)==0){
      showNotification("Sélectionnez une panne",type="warning")
      return()
    }
    
    # Suppression
    donnees$pannes <- donnees$pannes %>%
      slice(-input$table_pannes_rows_selected)
  })
  
  # ----------------------------------------------------------------------------
  # CHARGEMENT DES PANNES D'UNE ANNEE
  # ----------------------------------------------------------------------------
  #
  # Lorsqu'une année est sélectionnée dans l'onglet "Pannes", l'application
  # recherche automatiquement le fichier correspondant dans le dossier "pannes /".
  #
  # ----------------------------------------------------------------------------
  
  observeEvent(input$annee_pannes,{
    req(input$annee_pannes)
    fichier_pannes <- paste0(
      "pannes/",
      input$annee_pannes,
      ".rds"
    )
    
    # Si le fichier existe, les périodes enregistrées sont chargées.
    if(file.exists(fichier_pannes)){
      donnees$pannes <- readRDS(fichier_pannes)
      # Dans le cas contraire, un tableau vide est crée pour l'année sélectionnée.
    } else {
      donnees$pannes <- tibble(
        debut=as.POSIXct(character()),
        fin=as.POSIXct(character())
      )
    }
  })
  
  # ============================================================================
  # IMPORT DU VIDEOCOMPTAGE
  # ============================================================================
  #
  # L'importation est déclenchée uniquement lorsque l'utilisateur clique sur le
  # bouton "Importer les données de vidécomptage".
  #
  observeEvent(input$import_video,{
    
    # Vérifie qu'une année a bien été sélectionnée
    req(input$annee)
    
    # Vérifie qu'au moins une espèce a été sélectionnée
    req(length(input$especes_api)>0)
    
    # Intercepte les erreurs pouvant survenir pendant l'import afin d'éviter que
    # l'app ne plante
    tryCatch({
      
      # Barre de progression
      withProgress(
        message="Téléchargement des données d'Hizkia...",
        value=0,
        {
          
          # Effectue une requête API pour chaque espèce sélectionnée puis
          # regroupe les résultats dans un seul tableau
          video<-purrr::map_dfr(input$especes_api,
                                function(x){
                                  
                                  # Mise à jour barre de progression
                                  incProgress(
                                    1/length(input$especes_api),
                                    detail=x
                                    )
                                  
                                  # Importe données correspondant à l'année et à
                                  # l'espèce sélectionnées
                                  import_api(annee=input$annee,
                                             espece=x)
                                })
          }
        )
      
      # Avertissement si l'API ne renvoie aucune donnée pour l'année/l'espèce choisies
      if(nrow(video)==0){
        showNotification(
          paste0("Aucune donnée disponible pour l'année ",input$annee,". Dépouillez les vidéos pour accéder aux données."),
          type="warning",
          duration=8
        )
        return()
      }
      
      # Nettoie et standardise les données récupérées depuis l'API
      donnees$video<-clean_video(video)
      
      # Ajoute le nom complet des espèces à partir de leur code
      donnees$video<-add_species_names(donnees$video,correspondance_especes)
      
      # Informe l'utilisateur du nombre de passages effectivement importés
      showNotification(paste(
        nrow(donnees$video),"passages importés."
      ),
      type="message")
      },error=function(e){
        
        # Message d'erreur
        showNotification(
          paste0("Impossible d'importer les données : ",e$message),
          type="error",
          duration=10
        )
      })
  })
  
  # ============================================================================
  # CONSTRUCTION ET SAUVEGARDE DU JEU DE DONNEES ANNUEL
  # ============================================================================
  #
  # Cette étape est déclenchée lorsque l'utilisateur clique sur "Lancer l'analyse".
  #
  # Les données de vidéocomptage doivent avoir été importés au préalable.
  # Les fichiers "environnementaux" sélectionnés par l'utilisateur sont ensuite
  # nettoyés, standardiséset croisés avec les données de comptage.
  #
  observeEvent(input$run_analysis,{
    
    # Vérifie que les données de comptage ont bien été importées
    req(donnees$video)
    
    # --------------------------------------------------------------------------
    # Nettoyage et standardisation des données environnementales
    # --------------------------------------------------------------------------
    
    # Température de l'eau
    donnees$temp <- clean_temp(read_csv2(input$temp$datapath))
    
    # Débit moyen
    donnees$Qmoy <- clean_Qmoy(read_csv2(input$qmoy$datapath,na="NULL"))
    
    # Débit minimal
    donnees$Qmin <- clean_Qmin(read_csv2(input$qmin$datapath,na="NULL"))
    
    # Débit maximal
    donnees$Qmax <- clean_Qmax(read_csv2(input$qmax$datapath,na="NULL"))
    
    # Hauteur d'eau
    donnees$H_eau <- clean_H_eau(read_csv2(input$heau$datapath))
    
    # --------------------------------------------------------------------------
    # Construction des données hydrologiques
    # --------------------------------------------------------------------------
    
    # Agrège les mesures de hauteur d'eau au pas de temps horaire
    donnees$H_eau_h <- build_H_eau_h(donnees$H_eau)
    
    # Regroupe les différentes variables hydrologiques dans un jeu de données horaire
    donnees$hydro <- build_hydro(donnees$Qmoy,donnees$Qmin,donnees$Qmax,donnees$H_eau_h)
    
    # --------------------------------------------------------------------------
    # Construction des données de vidéocomptage
    # --------------------------------------------------------------------------
    
    # Agrège les données individuelles de vidéocomptage au pas de temps horaire
    donnees$video_h <- build_video_h(donnees$video)
    
    # --------------------------------------------------------------------------
    # Croisement des données et agrégation temporelle
    # --------------------------------------------------------------------------
    
    # Croise les données hydro et de comptage à l'échelle horaire
    donnees$dataset_h <- build_dataset_h(donnees$hydro,donnees$video_h)
    
    # Intègre périodes de panne renseignées par l'utilisateur
    donnees$dataset_h <- apply_pannes(donnees$dataset_h,donnees$pannes)
    
    # Agrège le jeu de données horaire à l'échelle journalière
    donnees$dataset_j <- build_dataset_j(donnees$dataset_h)
    
    # Agrège le jeu de données journalier à l'échelle hebdomadaire et y ajoute
    # les données de température
    donnees$dataset_sem <- build_dataset_sem(donnees$dataset_j,donnees$temp)
    
    # --------------------------------------------------------------------------
    # Vérification avant sauvegarde
    # --------------------------------------------------------------------------
    
    # Les données sont sauvegardées dans un fichier RDS portant le nom de
    # l'année sélectionnée
    fichier <- paste0("data/",input$annee,".rds")
    
    # Empêche l'écrasement d'un jeu de données annuel déjà existant
    # Pour éviter de perdre des données, mais cela signifie qu'il faut contacter
    # l'administrateur de l'app s'il faut réellement remplacer les données d'une
    # année.
    if(file.exists(fichier)){
      showNotification(
        paste("Les données",input$annee,"existent déjà"),
        type="error"
      )
      return()
    }
    
    # --------------------------------------------------------------------------
    # Sauvegarde du jeu de données annuel
    # --------------------------------------------------------------------------
    
    # Sauvegarde dans un seul fichier RDS regroupant l'ensemble des données
    saveRDS(list(video=donnees$video,
                 temp=donnees$temp,
                 qmoy=donnees$Qmoy,
                 qmin=donnees$Qmin,
                 qmax=donnees$Qmax,
                 heau=donnees$H_eau,
                 dataset_h=donnees$dataset_h,
                 dataset_j=donnees$dataset_j,
                 dataset_sem=donnees$dataset_sem,
                 pannes=donnees$pannes,
                 hydro=donnees$hydro,
                 video_h=donnees$video_h),
            file=paste0("data/",input$annee,".rds"))
    
    # --------------------------------------------------------------------------
    # Mise à jour des listes d'années disponibles
    # --------------------------------------------------------------------------
    
    # Actualise la liste des années disponibles individuellement
    updateSelectInput(session,"annee_analyse",
                      choices=gsub(
                        "\\.rds$","",list.files("data",
                                                pattern="\\.rds$")
                      ),
                      selected=as.character(input$annee))
    
    # Actualise la liste des années disponibles dans l'onglet de gestion des
    # périodes de panne
    updateSelectInput(session,"annee_pannes",
                      choices=gsub(
                        "\\.rds$","",list.files("data",
                                                pattern="\\.rds$")
                      ))
    
    # Actualise la liste des années disponibles pour les comparaisons interannuelles.
    updateSelectInput(session,"annees_comparees",
                      choices=gsub(
                        "\\.rds$","",list.files("data",
                                                pattern="\\.rds$")
                      ))
    
    # Informe l'utilisateur que le traitement et la sauvegarde sont terminés
    showNotification("Fichiers importés avec succès")
  })
  
  
  # ============================================================================
  # CHARGEMENT DES DONNEES POUR L'ANALYSE PAR ANNEE
  # ============================================================================
  
  # Charge le fichier RDS correspondant à l'année sélectionnée dans l'onglet
  # "Analyse annuelle"
  donnees_analyse <- reactive({
    refresh_analyse()  # force le rechargement du fichier après modif des pannes
    req(input$annee_analyse)
    readRDS(paste0("data/",input$annee_analyse,".rds"))
  })
  
  
  # ============================================================================
  # ETAT DES DONNEES DISPONIBLES
  # ============================================================================
  
  # Vérifie la présence du fichier annuel et des principaux jeux de données
  # nécessaires aux analyses
  output$etat_import <- renderUI({
    fichier <- paste0("data/",input$annee,".rds")
    
    # Avertissement si aucun fichier annuel existant pour l'année sélectionnée
    if(!file.exists(fichier)){
      return(
        tags$div(
          style="
          background-color:#fff3cd;
          border-left:5px solid #ffc107;
          padding:10px;
          border-radius:5px;
          margin-bottom:15px",
          "Aucune donnée enregistrée pour cette année."
        )
      )
    }
    
    # Charge fichier annuel
    d<-readRDS(fichier)
    
    # Génère une ligne d'état avec voyant vert ou rouge selon disponibilité du dataset
    ligne<-function(ok,texte){
      couleur <- if(ok) "#198754" else "#dc3545"
      symbole <- if(ok) "🟢" else "🔴"
      tags$div(style="margin-bottom:5px",
               paste(symbole,texte))
    }
    tags$div(
      style="
      background-color:#f2f7fc;
      border-left:6px solid #1f4e79;
      border-radius:8px;
      padding:12px;
      margin-bottom:15px;",
      tags$h4("Disponibilité des données pour l'année sélectionnée",
              style="color:#1f4e79;margin-top:0;"),
      
      # Vérifie la présence des données de comptage
      ligne(!is.null(d$video),"Vidéocomptage"),
      
      # Vérifie la présence des données de température
      ligne(!is.null(d$temp),"Température"),
      
      # Vérifie la présence des données hydrologiques
      ligne("Qmoy_avalPE" %in% names(d$dataset_h),"Débit moyen"),
      ligne("Qmin_avalPE" %in% names(d$dataset_h),"Débit minimal"),
      ligne("Qmax_avalPE" %in% names(d$dataset_h),"Débit maximal"),
      ligne("hauteur" %in% names(d$dataset_h),"Hauteur d'eau")
      )
  })
  
  
  # ============================================================================
  # SAUVEGARDE DES PERIODES DE PANNE
  # ============================================================================
  
  # Enregistre les périodes de panne actuellement saisies dans l'app dans un
  # fichier RDS spécifique à l'année sélectionnée
  observeEvent(input$save_pannes,{
    req(input$annee_pannes)
    saveRDS(donnees$pannes,
            file=paste0(
              "pannes/",
              input$annee_pannes,
              ".rds"
            ))
    
    # Confirmation de sauvegarde
    showNotification("Pannes enregistrées",
                     type="message")
  })
  
  # ============================================================================
  # MISE A JOUR DES ANALYSES APRES MODIFICATION DES PANNES
  # ============================================================================
  
  # Recalcule datasets dépendant des périodes de panne sans réimporter ni
  # retraiter les données brutes
  observeEvent(input$recompute_analysis,{
    
    # Vérifie que l'année des périodes de panne correspond à l'année
    # actuellement sélectionnée pour l'analyse
    if(input$annee_pannes != input$annee_analyse){
      showNotification("Attention : l'année Pannes et l'année Analyse sont différentes.",type="error")
      return()
    }
    
    req(input$annee_pannes)
    
    # Recharge dataset annuel déjà sauvegardé
    donnees_stockees <- readRDS(
      paste0(
        "data/",
        input$annee_pannes,".rds"
      )
    )
    
    # Reconstruction dataset horaire 
    dataset_h <- build_dataset_h(donnees_stockees$hydro,donnees_stockees$video_h)
    
    # Intégration des pannes
    dataset_h <- apply_pannes(dataset_h,donnees$pannes)
    
    # Agrégation en datasets journalier/hebdomadaire
    dataset_j <- build_dataset_j(dataset_h)
    dataset_sem <- build_dataset_sem(dataset_j,donnees_stockees$temp)
    
    # Remplace dans l'objet annuel les jeux de données affectés par les modifs
    donnees_stockees$dataset_h <- dataset_h
    donnees_stockees$dataset_j <- dataset_j
    donnees_stockees$dataset_sem <- dataset_sem
    donnees_stockees$pannes <- donnees$pannes
    
    # Sauvegarde du jeu de données mis à jour
    saveRDS(donnees_stockees,
            file=paste0(
              "data/",input$annee_pannes,".rds"
            ))
    
    # Force le rechargement du jeu de données utilisé par l'analyse annuelle.
    refresh_analyse(refresh_analyse()+1)
    
    # Message de confirmation
    showNotification("Analyses mises à jour",type="message")
  })
  
  
  # ============================================================================
  # RESUME ANNUEL
  # ============================================================================
  
  # Calcule et affiche les principaux indicateurs de l'année sélectionnée
  output$resume_global <- renderUI({
    
    # Vérification disponibilité des données
    req(donnees_analyse())
    
    # Utilisation du dataset horaire
    dataset_h <- donnees_analyse()$dataset_h
    
    # Nombre de montaisons enregistrées sur l'année.
    nb_mont <- sum(dataset_h$nb_mont,na.rm=TRUE)
    
    # Nombre de séquences avec poisson(s) enregistrées sur l'année.
    nb_seq <- sum(dataset_h$nb_seq,na.rm=TRUE)
    
    # "Taux de montaison" (montaisons/séquences)
    taux_mont <- round(100*nb_mont/nb_seq,2)
    
    # Calcule la proportion du temps pour laquelle le dispositif est fonctionnel
    pct_fonctionnement <- round(100*sum(!is.na(dataset_h$nb_mont))/
                                  nrow(dataset_h),2)
    tags$div(
      style="
      background-color:#f2f7fc;
      border-left:6px solid #1f4e79;
      border-radius:8px;
      padding:15px;
      margin-bottom:20px;
      ",
      tags$h4("Résumé annuel",
              style="
              color:#1f4e79;
              margin-top:0;
              font-weight:bold;"),
      tags$p(
        tags$b("Montaisons totales : "),
        nb_mont),
      tags$p(
        tags$b("Séquences totales : "),
        nb_seq),
      tags$p(
        tags$b("Taux de montaison : ",
        sprintf("%.2f %%",taux_mont))),
      tags$p(
        tags$b("Temps de fonctionnement : "),
        sprintf("%.2f %%",pct_fonctionnement))
    )
  })
  
  
  # ============================================================================
  # RESUME MENSUEL
  # ============================================================================
  
  # Construit et affiche un tableau récapitulatif mensuel
  output$resume_mensuel <- renderDT({
    
    # Màj du compteur utilisé pour permettre le rafraîchissement du tableau
    # lorsque l'utilisateur le demande.
    reset_resume()
    
    req(donnees_analyse())
    
    # Construction tableau + mise en forme 
    datatable_passe(build_resume_mensuel(donnees_analyse()$dataset_h))
  })
  
  # Force le rafraîchissement
  observeEvent(input$reset_resume,{
    reset_resume(reset_resume()+1)
  })
  
  
  # ============================================================================
  # PERIODES SANS MONTAISON ENREGISTREE
  # ============================================================================
  
  # Recherche périodes durant lesquelles aucune montaison n'a été enregistrée
  # pendant au moins le nombre de jours choisi par l'utilisateur.
  output$no_passage_periods <- renderDT({
    req(donnees_analyse())
    
    # Identifie les périodes répondant au seuil défini dans l'interface
    periodes <- find_no_passage_periods(
      donnees_analyse()$dataset_j,
      seuil_jours=input$seuil_no_passage
    )
    
    # Message si aucune période ne correspond
    if(nrow(periodes)==0){
      return(
        datatable(
          data.frame(
            Information=paste0(
              "Aucune période d'au moins ",
              input$seuil_no_passage,
              " jours sans montaison enregistrée."
            )
          ),
          options=list(dom="t"),
          rownames=FALSE
        )
      )
    }
    
    # Formate les dates et sélectionne uniquement les informations utiles à
    # l'utilisateur
    periodes <- periodes |>
      dplyr::mutate(
        'Date de début'=format(debut,"%d/%m/%Y"),
        'Date de fin'=format(fin,"%d/%m/%Y"),
        'Durée (jours)'=duree
      ) |>
      dplyr::select(
        'Date de début',
        'Date de fin',
        'Durée (jours)'
      )
    
    # Affiche les périodes dans un tableau
    datatable(
      periodes,
      rownames=FALSE,
      options=list(pageLength=10,autoWidth=TRUE)
    )
  })
  
  
  # ============================================================================
  # GRAPHIQUES DE SYNTHESE
  # ============================================================================
  
  # Affiche l'évolution hebdomadaire des montaisons, avec courbes de température
  # et débit
  output$conditions_env <- renderPlot({
    req(donnees_analyse())
    plot_conditions_env(donnees_analyse()$dataset_sem)
  })
  
  # Affiche évolution journalière des montaisons avec mise en perspective des
  # périodes de panne
  output$daily_passage <- renderPlot({
    req(donnees_analyse())
    plot_daily_passage(donnees_analyse()$dataset_j,donnees_analyse()$pannes)
  })
  
  # ============================================================================
  # ANALYSE PAR ESPECE
  # ============================================================================
  
  # Tableau récapitulatif des passages par espèce
  output$tableau_especes <- renderDT({
    req(donnees_analyse())
    datatable_passe(
      build_tableau_especes(donnees_analyse()$video))
  })
  
  # Aide à la lecture des résultats par espèce
  output$interpretation_especes <- renderUI({
    req(donnees_analyse())
    
    # Elements textuels
    phrases <- interpret_especes(donnees_analyse()$video)
    tags$div(style="
             background-color:#f2f7fc;
             border-left:6px solid #1f4e79;
             border-radius:8px;
             padding:15px;
             margin-top:15px;
             ",
             tags$h4("Résumé",
                     style="
                     color:#1f4e79;
                     font-weight:bold;
                     margin-top:0;"),
             lapply(
               phrases$interpretation,
               function(x) tags$p(x)
             ))
  })
  
  
  # ============================================================================
  # ANALYSE DES TAILLES
  # ============================================================================
  
  # Distribution des tailles pour poissons observés en montaison
  output$taille_especes <- renderPlot({
    req(donnees_analyse())
    
    # Sélectionne et prépare les passages correspondant aux montaisons
    video_montaison <- build_video_montaison(donnees_analyse()$video)
    plot_taille_especes(video_montaison)
  })
  
  # Tableau récapitulatif des tailles par espèce
  output$table_taille_especes <- renderDT({
    req(donnees_analyse())
      video_montaison <- build_video_montaison(donnees_analyse()$video)
      datatable_passe(build_table_taille_especes(video_montaison))
  })
  
  # Aide à la lecture des distributions de tailles
  output$interpretation_taille <- renderUI({
    req(donnees_analyse())
    
    # Préparation données de montaison
    video_montaison <- build_video_montaison(donnees_analyse()$video)
    
    # Construction tableau
    table_taille <- build_table_taille_especes(video_montaison)
    phrases <- interpret_taille_especes(table_taille)
    tags$div(style="
             background-color:#f2f7fc;
             border-left:6px solid #1f4e79;
             border-radius:8px;
             padding:15px;
             margin-top:15px;
             ",
             tags$h4("Résumé",
                     style="
                     color:#1f4e79;
                     font-weight:bold;
                     margin-top:0;"),
             lapply(
               phrases$interpretation,
               function(x) tags$p(x)
             ))
  })
  
  
  # ============================================================================
  # ANALYSE DE L'ACTIVITE HORAIRE
  # ============================================================================
  
  # Répartition horaire globale des passages
  output$horaire_global <- renderPlot({
    req(donnees_analyse())
    plot_horaire_global(donnees_analyse()$video)
  })
  
  # Répartition horaire en distinguant les mois
  output$horaire_mensuel <- renderPlot({
    req(donnees_analyse())
    plot_horaire_mensuel(donnees_analyse()$video)
  })
  
  # Construction d'un tableau récapitulatif
  output$table_horaire <- renderDT({
    req(donnees_analyse())
    datatable_passe(
      build_table_horaire(donnees_analyse()$video)    
    )
  })
  
  
  # ============================================================================
  # EXPORT DES RESULTATS ANNUELS
  # ============================================================================
  
  # Fichier Excel contenant principales données transformées 
  output$export_excel_annuel <- downloadHandler(filename = function(){
    paste0("Analyse_",input$annee_analyse,".xlsx")  # nom du fichier
  },
  content = function(file){
    
    # Création nouveau tableur
    wb <- createWorkbook()
    
    # --------------------------------------------------------------------------
    # Informations générales
    # --------------------------------------------------------------------------
    
    # Regroupe les principaux indicateurs calculés pour l'année
    addWorksheet(wb,"Informations")
    dataset_h <- donnees_analyse()$dataset_h
    nb_mont <- sum(dataset_h$nb_mont,na.rm=TRUE)
    nb_seq <- sum(dataset_h$nb_seq,na.rm=TRUE)
    taux_mont <- round(100*nb_mont/nb_seq,1)
    pct_fonctionnement <- round(100*sum(!is.na(dataset_h$nb_mont))/nrow(dataset_h),2)
    infos <- tibble(Indicateur=c(
      "Année","Montaisons totales","Séquences totales",
      "Taux de montaison (%)","Temps de fonctionnement (%)","Date d'export"
    ),
    Valeur=c(input$annee_analyse,nb_mont,nb_seq,taux_mont,
             pct_fonctionnement,format(Sys.time(),"%Y-%m-%d %H:%M")))
    writeData(wb,"Informations",infos)
    
    # --------------------------------------------------------------------------
    # Pannes
    # --------------------------------------------------------------------------
    
    # Exporte périodes de dysfonctionnement du système
    addWorksheet(wb,"Pannes")
    writeData(wb,"Pannes",donnees_analyse()$pannes)
    
    # --------------------------------------------------------------------------
    # Résultats par niveau d'analyse
    # --------------------------------------------------------------------------
    
    # Résumé 
    addWorksheet(wb,"Resume")
    writeData(wb,"Resume",build_resume_mensuel(donnees_analyse()$dataset_h))
    
    # Passages par espèce
    addWorksheet(wb,"Especes")
    writeData(wb,"Especes",build_tableau_especes(donnees_analyse()$video))
    
    # Taille des individus en montaison
    addWorksheet(wb,"Tailles")
    writeData(wb,"Tailles",build_table_taille_especes(donnees_analyse()$video))
    
    # Activité horaire
    addWorksheet(wb,"Horaire")
    writeData(wb,"Horaire",build_table_horaire(donnees_analyse()$video))
    
    # Enregistrement du classeur
    saveWorkbook(wb,file,overwrite=TRUE)
  })
  
  
  # ============================================================================
  # EXPORT DES RESULTATS INTERANNUELS
  # ============================================================================
  
  # Même principe que ci-dessus mais pour données sur plusieurs années
  output$export_excel_interannuel <- downloadHandler(
    filename = function(){   # nom du fichier (date d'export)
      paste0("Analyse_interannuelle_",
             format(Sys.Date(),"%Y%m%d"),".xlsx")
    },
    content=function(file){
      wb<-createWorkbook()  # création du tableur
      
      # Synthèse
      addWorksheet(wb,"Synthese")
      writeData(wb,"Synthese",build_resume_interannuel(input$annees_comparees))
      
      # Comparaison mensuelle
      addWorksheet(wb,"Mensuel")
      writeData(wb,"Mensuel",build_table_interannuelle_mensuelle(input$annees_comparees))
      
      # Comparaison des résultats par espèce
      addWorksheet(wb,"Especes")
      writeData(wb,"Especes",build_table_interannuelle_especes(input$annees_comparees))
      
      # Comparaison des tailles
      addWorksheet(wb,"Tailles")
      writeData(wb,"Tailles",build_table_taille_interannuelle(input$annees_comparees))
      
      # Comparaison de l'activité horaire
      addWorksheet(wb,"Horaire")
      writeData(wb,"Horaire",build_table_horaire_interannuelle(input$annees_comparees))
      
      # Enregistrement du classeur Excel
      saveWorkbook(wb,file,overwrite=TRUE)
    }
  )
  
  
  # ============================================================================
  # EXPORT DES GRAPHIQUES ANNUELS
  # ============================================================================
  
  # Confirmations de téléchargement
  
  confirm_download(input,"confirm_conditions_env","download_conditions_env") # export graphique
  output$download_conditions_env <- downloadHandler(
    filename=function(){  # nom du fichier
      paste0("conditions_env",input$annee_analyse,".png")
    },
    content=function(file){
      png(file,width=1600,height=900,res=150)  # création png
      print(plot_conditions_env(donnees_analyse()$dataset_sem)) # génération du graphique
      dev.off()  # fermeture périphérique et finalisation fichier
    }
  )
  confirm_download(input,"confirm_daily_passage","download_daily_passage")
  output$download_daily_passage <- downloadHandler(
    filename=function(){
      paste0("daily_passage",input$annee_analyse,".png")
    },
    content = function(file){
      png(file,width=1600,height=900,res=150)
      print(plot_daily_passage(donnees_analyse()$dataset_j,donnees_analyse()$pannes))
      dev.off()
    }
  )
  confirm_download(input,"confirm_taille_especes","download_taille_especes")
  output$download_taille_especes <- downloadHandler(
    filename=function(){
      paste0("taille_especes",input$annee_analyse,".png")
    },
    content = function(file){
      png(file,width=1600,height=900,res=150)
      video_montaison <- build_video_montaison(donnees_analyse()$video)
      print(plot_taille_especes(video_montaison))
      dev.off()
    }
  )
  confirm_download(input,"confirm_horaire_global","download_horaire_global")
  output$download_horaire_global <- downloadHandler(
    filename=function(){
      paste0("horaire_global",input$annee_analyse,".png")
    },
    content = function(file){
      png(file,width=1600,height=900,res=150)
      print(plot_horaire_global(donnees_analyse()$video))
      dev.off()
    }
  )
  confirm_download(input,"confirm_horaire_mensuel","download_horaire_mensuel")
  output$download_horaire_mensuel <- downloadHandler(
    filename=function(){
      paste0("horaire_mensuel",input$annee_analyse,".png")
    },
    content = function(file){
      png(file,width=1600,height=900,res=150)
      print(plot_horaire_mensuel(donnees_analyse()$video))
      dev.off()
    }
  )
  
  # ============================================================================
  # AFFICHAGE DES RESULTATS INTERANNUELS
  # ============================================================================

  # Tableau de synthèse
  output$table_interannuelle <- renderDT({
    req(input$annees_comparees)
    datatable_passe(build_resume_interannuel(input$annees_comparees))
  })
  
  # Graphique montrant les montaisons totales entre les années sélectionnées
  output$plot_interannuel <- renderPlot({
    req(input$annees_comparees)
    plot_interannuel(input$annees_comparees)
  })
  
  # Graphique comparant l'évolution mensuelle des montaisons entre les années
    output$plot_interannuel_mensuel <- renderPlot({
    req(input$annees_comparees)
    plot_interannuel_mensuel(input$annees_comparees)
    })
  
  # Tableau de comparaison mensuelle
    output$table_interannuelle_mensuelle <- renderDT({
    req(input$annees_comparees)
    datatable_passe(build_table_interannuelle_mensuelle(input$annees_comparees))
  })
  
  # Tableau comparant résultats par espèce entre les années
    output$table_interannuelle_especes <- renderDT({
    req(input$annees_comparees)
    datatable_passe(build_table_interannuelle_especes(input$annees_comparees))
  })
    
  # Graphique comparant montaisons par espèce
  output$plot_interannuel_especes <- renderPlot({
    req(input$annees_comparees)
    plot_interannuel_especes(input$annees_comparees)
  })
  
  # Tableau comparant les distributions de tailles entre années
  output$table_taille_interannuelle <- renderDT({
    req(input$annees_comparees)
    datatable_passe(build_table_taille_interannuelle(input$annees_comparees))
    })
  
  # Graphique comparant les distributions de taille entre les années.
  output$plot_taille_interannuelle <- renderPlot({
    req(input$annees_comparees)
    plot_taille_interannuelle(input$annees_comparees)
  })
  
  # Tableau comparant l'activité horaire entre les années
  output$table_horaire_interannuelle <- renderDT({
    req(input$annees_comparees)
    datatable_passe(build_table_horaire_interannuelle(input$annees_comparees))
  })
  
  # Graphique comparant l'activité horaire entre les années
  output$plot_horaire_interannuel <- renderPlot({
    req(input$annees_comparees)
    plot_horaire_interannuel(input$annees_comparees)
  })
  

  # ============================================================================
  # EXPORT DES GRAPHIQUES INTERANNUELS
  # ============================================================================
  #
  # Même principe que pour les graphiques annuels
  #
  confirm_download(input,"confirm_plot_interannuel","download_plot_interannuel")
  output$download_plot_interannuel <- downloadHandler(
    filename=function(){
      paste0("Montaisons_interannuelles_",Sys.Date(),".png")
    },
    content=function(file){
      png(file,width=1600,height=900,res=150)
      print(plot_interannuel(input$annees_comparees))
      dev.off()
    }
  )
  
  confirm_download(input,"confirm_plot_interannuel_mensuel","download_plot_interannuel_mensuel")
  output$download_plot_interannuel_mensuel <- downloadHandler(
    filename=function(){
      paste0("Montaisons_mensuelles_",Sys.Date(),".png")
    },
    content=function(file){
      png(file,width=1600,height=900,res=150)
      print(plot_interannuel_mensuel(input$annees_comparees))
      dev.off()
    }
  )
  
  confirm_download(input,"confirm_plot_interannuel_especes","download_plot_interannuel_especes")
  output$download_plot_interannuel_especes <- downloadHandler(
    filename=function(){
      paste0("Montaisons_especes_",Sys.Date(),".png")
    },
    content=function(file){
      png(file,width=1600,height=900,res=150)
      print(plot_interannuel_especes(input$annees_comparees))
      dev.off()
    }
  )
  
  confirm_download(input,"confirm_plot_taille_interannuelle","download_plot_taille_interannuelle")
  output$download_plot_taille_interannuelle <- downloadHandler(
    filename=function(){
      paste0("Tailles_interannuelles_",Sys.Date(),".png")
    },
    content=function(file){
      png(file,width=1600,height=900,res=150)
      print(plot_taille_interannuelle(input$annees_comparees))
      dev.off()
    }
  )
  
  confirm_download(input,"confirm_plot_horaire_interannuel","download_plot_horaire_interannuel")
  output$download_plot_horaire_interannuel <- downloadHandler(
    filename=function(){
      paste0("Horaires_interannuels_",Sys.Date(),".png")
    },
    content=function(file){
      png(file,width=1600,height=900,res=150)
      print(plot_horaire_interannuel(input$annees_comparees))
      dev.off()
    }
  )
}


# ============================================================================
# LANCEMENT DE L'APPLICATION
# ============================================================================

# Assemble l'interface utilisateur et la logique serveur puis lance l'app Shiny.
shinyApp(ui,server)

