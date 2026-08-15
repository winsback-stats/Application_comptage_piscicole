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
#   Ce script contient les fonctions utilisées par l'application pour :
#   - importer les données de vidéocomptage depuis l'API d'Hizkia
#   - importer et standardiser les données environnementales 
#   - nettoyer et mettre en forme les données
#   - construire des jeux de données à différents pas de temps (h, jour, semaine)
#   - intégrer à l'analyse les périodes de panne renseignées par l'utilisateur
#   - produire les indicateurs, tableaux et graphiques d'aide à l'analyse
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
#   Les fonctions de ce script sont appelées depuis le script app.R.
#   Pensez à enregistrer la version précédente avant d'effectuer des modifications.
#
# ==============================================================================


# ==============================================================================
# IMPORT DES DONNEES DE VIDEOCOMPTAGE
# ==============================================================================
#
# Les données de vidéocomptage sont récupérées directement depuis l'API Hizkia.
# La requête est effectuée directement dans l'application par l'utilisateur, qui
# choisit l'année de son choix.
# ==============================================================================

# ------------------------------------------------------------------------------
# import_api()
# ------------------------------------------------------------------------------
# Interroge l'API Hizkia et récupère les données de vidéocomptage du site de Pont
# de la Reine pour une année et une ou plusieurs espèces données.
#
# Arguments :
#   annee : année à importer.
#   espece : code espèce utilisé par l'API Hizkia (ex. TRF).
#
# Retour :
#   Un tibble contenant les données renvoyées par l'API.
#
# La fonction arrête l'exécution si l'API retourne une erreur HTTP.
# ------------------------------------------------------------------------------
import_api <- function(annee,espece){
  
  # Ce token est STRICTEMENT CONFIDENTIEL.
  # Ne pas le transmettre à un tiers sans autorisation préalable.
  # Ne surtout pas transmettre ce token à un logiciel d'intelligence artificielle.
  #
  # En cas de prise de risque, il est nécessaire de changer ce token en suivant
  # la procédure décrite dans le guide développeur.
  token <- "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJmcmVzaCI6ZmFsc2UsImlhdCI6MTc4NDcwOTQzNCwianRpIjoiMTdhYWY5ODctNGFiMi00MGI4LTlhMWYtNGQ1OWZhZjAwMTU4IiwidHlwZSI6ImFjY2VzcyIsInN1YiI6InNoZW0iLCJuYmYiOjE3ODQ3MDk0MzQsImNzcmYiOiI5Yzc5OTUxOC02NmIyLTRjNWEtYmU2NC00MmZjMjcwNzYzOTYifQ.6TjuJPks-oZT7GRoiYC-4lJbZmcMiGSJ_fFryY-GWs4"
  r <- httr::GET(
    "https://api.hizkia.eu/passage/",
    query=list(
      site="pont_de_la_reine",
      espece=espece,
      date=paste0(annee,"0101"),
      date_end=paste0(annee,"1231")
      ),
    httr::add_headers(
      Authorization = paste0("Bearer ",token),
      Accept="application/json"
      )
  )
  httr::stop_for_status(r)
  httr::content(r,as="parsed",simplifyVector=TRUE)|>
    tibble::as_tibble()
}


# ==============================================================================
# NETTOYAGE ET STANDARDISATION DES DONNEES
# ==============================================================================
#
# Les données utilisées par l'application proviennent de plusieurs sources et 
# possèdent des formats et noms de variables différents.
# 
# Les fonctions suivantes convertissent ces données vers un format commun, notamment
# en harmonisant :
#   - les noms de variables ;
#   - les dates et heures ;
#   - les variables utilisées dans les analyses.
#
# Les jeux de données ainsi standardisés peuvent ensuite être croisés et agrégés
# dans les fonctions de constructions des datasets.
# ==============================================================================

# ------------------------------------------------------------------------------
# clean_video()
# ------------------------------------------------------------------------------
# Standardise les données de vidéocomptage provenant de l'API Hizkia.
#
# Principales opérations :
#   - renommage des variables
#   - conversion de la date et de l'heure
#   - création des variables "date" et "heure"
#   - conversion de la taille des individus en cm
#   - sélection des variables conservées pour la suite du traitement.
#
# Correspondances principales :
#   image_timestamp => datetime
#   tag => espece
#   way => sens
#   size => taille
#
# NB : le niveau de turbidité n'est pour le moment pas récupérable via l'API. 
# Dans le cas où ce paramètre devait être intégré à l'application à l'avenir, 
# se rapprocher d'HIZKIA pour mise à jour de l'API. 
# ------------------------------------------------------------------------------
clean_video <- function(video){
  video <- video %>%
    rename(
      datetime = image_timestamp,
      espece = tag,
      sens = way,
      taille = size
    ) %>%
    mutate(
      datetime = lubridate::ymd_hms(datetime),
      date = as.Date(datetime),
      heure = lubridate::hour(datetime),
      taille=ifelse(taille<=10,NA,taille),
      taille=taille/10
    ) %>%
    select(datetime, date, heure, espece, sens, taille)
  return(video)
}

# ------------------------------------------------------------------------------
# clean_temp()
# ------------------------------------------------------------------------------
# Standardise les données de température de l'eau et crée les variales de date 
# et d'heure nécessaires au croisement avec les autres données.
# ------------------------------------------------------------------------------
clean_temp <- function(temp){
  temp <- temp %>%
    rename(datetime = 'Date/Heure',temperature=Température) %>%
    mutate(datetime=ymd_hms(datetime),date=as.Date(datetime),heure=hour(datetime))
  return(temp)
}

# ------------------------------------------------------------------------------
# clean_Qmoy()
# ------------------------------------------------------------------------------
# Standardise les données de débit moyen provenant des deux points de mesure :
#   - débit du gave ;
#   - débit à l'aval de la prise d'eau.
#
# Les données sont ramenées à un format horaire commun. 
# ------------------------------------------------------------------------------
clean_Qmoy <- function(Qmoy){
  Qmoy <- Qmoy %>%
    rename(datetime='Date et heure',
           Qmoy_gave = 'Pont de la Reine Débit Gave (m³/s)',
           Qmoy_avalPE = 'Pont de la Reine Débit Aval Prise (m³/s)') %>%
    mutate(datetime = dmy_hm(datetime),
           date = as.Date(datetime),
           heure = hour(datetime)) %>%
    select(date,heure,Qmoy_avalPE,Qmoy_gave)
  return(Qmoy)
}

# ------------------------------------------------------------------------------
# clean_Qmin()
# ------------------------------------------------------------------------------
# Standardise les données de débit minimum pour les deux points de mesure.
# ------------------------------------------------------------------------------
clean_Qmin <- function(Qmin){
  Qmin <- Qmin %>%
    rename(datetime='Date et heure',
           Qmin_gave = 'Pont de la Reine Débit Gave (m³/s)',
           Qmin_avalPE = 'Pont de la Reine Débit Aval Prise (m³/s)') %>%
    mutate(datetime = dmy_hm(datetime),
           date = as.Date(datetime),
           heure = hour(datetime)) %>%
    select(date,heure,Qmin_avalPE,Qmin_gave)
  return(Qmin)
}

# ------------------------------------------------------------------------------
# clean_Qmax()
# ------------------------------------------------------------------------------
# Standardise les données de débit maximum pour les deux points de mesure.
# ------------------------------------------------------------------------------
clean_Qmax <- function(Qmax){
  Qmax <- Qmax %>%
    rename(datetime='Date et heure',
           Qmax_gave = 'Pont de la Reine Débit Gave (m³/s)',
           Qmax_avalPE = 'Pont de la Reine Débit Aval Prise (m³/s)') %>%
    mutate(datetime = dmy_hm(datetime),
           date = as.Date(datetime),
           heure = hour(datetime)) %>%
    select(date,heure,Qmax_avalPE,Qmax_gave)
  return(Qmax)
}

# ------------------------------------------------------------------------------
# clean_H_eau()
# ------------------------------------------------------------------------------
# Standardise les données de hauteur d'eau et crée les variables de date et d'heure
# nécessaires à leur intégration dans le jeu de données hydrologique.
# ------------------------------------------------------------------------------
clean_H_eau <- function(H_eau){
  H_eau <- H_eau %>%
    rename(datetime = 'Date (TU)',hauteur='Valeur (en cm)') %>%
    mutate(datetime = ymd_hms(datetime),
           date = as.Date(datetime),
           heure = hour(datetime)) %>%
    select(datetime,date,heure,hauteur)
  return(H_eau)
}

# ==============================================================================
# CONSTRUCTION DES DONNEES HORAIRES
# ==============================================================================
#
# Les différentes sources de données sont préparées à un pas de temps horaire
# afin de pouvoir être croisées entre elles.
# ==============================================================================

# ------------------------------------------------------------------------------
# build_H_eau_h()
# ------------------------------------------------------------------------------
# Agrège les mesures de hauteur d'eau à un pas de temps horaire.
#
# Lorsque plusieurs mesures sont disponibles pour une même heure, leur moyenne
# est calculée.
# ------------------------------------------------------------------------------
build_H_eau_h <- function(H_eau){
  H_eau_h <- H_eau %>%
    group_by(date,heure) %>%
    summarise(hauteur=mean(hauteur,na.rm=TRUE),
              .groups="drop")
  return(H_eau_h)
}

# ------------------------------------------------------------------------------
# build_hydro()
# ------------------------------------------------------------------------------
# Regroupe les différentes variables hydrologiques dans un même jeu de données
# horaire.
#
# Données intégrées : débit moyen, débit minimal, débit maximal, hauteur d'eau
#
# Variables dérivées :
#   - amplitude du débit à l'aval de la prise d'eau ;
#   - amplitude du débit du gave ;
#   - ratio entre le débit à l'aval de la prise d'eau et le débit du gave.
#
# NB : ces variables dérivées ne sont pas utilisées dans la version actuelle de 
# l'application. Le choix a été fait de les conserver pour éventuellement donner
# quelques pistes de réflexion au futur développeur...
#
# Retour : un jeu de données hydrologiques horaire.
# ------------------------------------------------------------------------------
build_hydro <- function(Qmoy,Qmin,Qmax,H_eau_h){
  hydro <- Qmoy %>%
    left_join(Qmin, by=c("date","heure")) %>%
    left_join(Qmax, by=c("date","heure")) %>%
    left_join(H_eau_h,by=c("date","heure")) %>%
    mutate(
      deltaQ_avalPE = Qmax_avalPE - Qmin_avalPE,
      deltaQ_gave = Qmax_gave - Qmin_gave,
      ratio_derivation = Qmoy_avalPE / Qmoy_gave)
  return(hydro)
}

# ------------------------------------------------------------------------------
# build_video_h()
# ------------------------------------------------------------------------------
# Agrège les données individuelles de vidéocomptage à l'échelle horaire.
#
# Pour chaque heure sont calculés :
#   - le nombre de montaisons
#   - le nombre de séquences avec observation de poissons
#   - le "taux de montaison" = rapport montaisons/séquences
#   - la taille moyenne des individus en montaison
#   - les tailles minimale et maximale
#   - l'étendue des tailles observées
#
# Les tailles sont calculées uniquement à partir des séquences correspondant
# à une montaison effective (cela évite de comptabiliser plusieurs fois la
# taille d'un même individu).
# ------------------------------------------------------------------------------
build_video_h <- function(video){
  video_h <- video %>%
    group_by(date,heure) %>%
    summarise(nb_mont = sum(sens==1,na.rm=TRUE),
              nb_seq = n(),
              taux_mont = nb_mont / nb_seq,
              taille_moy = mean(taille[sens==1],na.rm=TRUE),
              taille_min = ifelse(nb_mont>0,min(taille[sens==1],na.rm=TRUE),NA),
              taille_max = ifelse(nb_mont>0,max(taille[sens==1],na.rm=TRUE),NA),
              taille_range = ifelse(nb_mont>0,taille_max - taille_min,NA),
              .groups="drop")
  return(video_h)
}

# ------------------------------------------------------------------------------
# build_dataset_h()
# ------------------------------------------------------------------------------
# Croise le jeu de données hydrologique et le jeu de données de vidéocomptage
# horaire afin de constituer un dataset principal à l'échelle horaire.
#
# Le regroupement est effectué à partir de la date et de l'heure.
#
# Les variables de comptage absentes après le croisement sont remplacées par 0.
# Elles correspondent alors à une heure pour laquelle aucune séquence n'a été
# enregistrée dans le jeu de données de comptage.
#
# Le dataset obtenu constitue notamment la base des analyses journalières,
# mensuelles et environnementales.
# ------------------------------------------------------------------------------
build_dataset_h <- function(hydro,video_h){
  dataset_h <- hydro %>%
    left_join(video_h,by=c("date","heure")) %>%
    mutate(
      datetime=make_datetime(year(date),month(date),mday(date),hour=heure),
      mois=month(date),
      nb_mont = replace_na(nb_mont,0),
      nb_seq=replace_na(nb_seq,0),
      taux_mont=replace_na(taux_mont,0)
    ) 
  return(dataset_h)
}

# ------------------------------------------------------------------------------
# apply_pannes()
# ------------------------------------------------------------------------------
# Intègre les périodes de panne du dispositif au jeu de données horaire.
#
# Pour chaque heure, la fonction vérifie si la date et l'heure sont comprises
# dans l'une des périodes de panne renseignées par l'utilisateur.
#
# Lorsqu'une panne est identifiée, le nombre de montaisons et de séquences est
# changé en NA. Cette distinction permet de différencier une période où le
# dispositif est fonctionnel mais qu'aucune montaison n'est observée, d'une
# période où aucune donnée de comptage fiable n'est disponible.
# ------------------------------------------------------------------------------
apply_pannes <- function(dataset_h,periodes_pannes){
  dataset_h <- dataset_h |>
    mutate(
      panne=map_lgl(datetime, \(t) any(t >= periodes_pannes$debut & t <= periodes_pannes$fin)),
      nb_mont=ifelse(panne,NA_real_,nb_mont),
      nb_seq=ifelse(panne,NA_real_,nb_seq),
      taux_mont=ifelse(panne,NA_real_,taux_mont)
    )
  return(dataset_h)
}

# ------------------------------------------------------------------------------
# build_dataset_j()
# ------------------------------------------------------------------------------
# Agrège le dataset horaire à l'échelle journalière.
#
# Variables de comptage :
#   - nombre de montaisons
#   - nombre de séquences
#   - "taux de montaison"
#   - débit moyen
#   - débit minimum
#   - débit maximum
#   - hauteur d'eau moyenne
#   - hauteur d'eau minimale
#   - hauteur d'eau maximale
#
# Un calendrier journalier complet est ensuite créé entre la première et la
# dernière date disponible afin de conserver les dates sans données de comptage
# dans le jeu de données final.
#
# Les variables "mois" et "jour_annee" sont ajoutées pour faciliter les analyses
# et visualisations temporelles.
# ------------------------------------------------------------------------------
build_dataset_j <- function(dataset_h){
  calendrier <- tibble(
    date=seq(min(dataset_h$date),max(dataset_h$date),by="day")
  )
  dataset_j <- dataset_h %>%
    group_by(date) %>%
    summarise(
      nb_mont = sum(nb_mont,na.rm=TRUE),
      nb_seq=sum(nb_seq,na.rm=TRUE),
      taux_mont=nb_mont/nb_seq,
      Qmoy_avalPE = mean(Qmoy_avalPE,na.rm=TRUE),
      Qmoy_gave = mean(Qmoy_gave,na.rm=TRUE),
      Qmin_avalPE = min(Qmoy_avalPE,na.rm=TRUE),
      Qmin_gave = min(Qmoy_gave,na.rm=TRUE),
      Qmax_avalPE = max(Qmoy_avalPE,na.rm=TRUE),
      Qmax_gave = max(Qmoy_gave,na.rm=TRUE),
      Hmoy = mean(hauteur,na.rm=TRUE),
      Hmin = min(hauteur,na.rm=TRUE),
      Hmax = max(hauteur,na.rm=TRUE),
      .groups = "drop"
    ) %>%
    right_join(calendrier,by="date") %>%
    arrange(date) %>%
    mutate(
      nb_mont=replace_na(nb_mont,0),
      nb_seq=replace_na(nb_seq,0),
      taux_mont=ifelse(nb_seq>0,nb_mont/nb_seq,NA),
      mois=month(date,label=TRUE),
      jour_annee=yday(date)
    )
  return(dataset_j)
}

# ------------------------------------------------------------------------------
# build_dataset_sem()
# ------------------------------------------------------------------------------
# Agrège les données journalières à l'échelle hebdomadaire.
#
# Même principe que la fonction précédente.
#
# La température est agrégée séparément puis jointe au dataset hebdomadaire à 
# partir de la semaine.
# ------------------------------------------------------------------------------
build_dataset_sem <- function(dataset_j,temp){
  dataset_sem <- dataset_j %>%
    mutate(semaine=floor_date(date,"week"))%>%
    group_by(semaine)%>%
    summarise(nb_mont=sum(nb_mont,na.rm=TRUE),
              debit=mean(Qmoy_gave,na.rm=TRUE),
              nb_seq=sum(nb_seq,na.rm=TRUE),
              .groups="drop")
  
  temp_sem <- temp %>%
    mutate(semaine = floor_date(date,"week"))%>%
    group_by(semaine)%>%
    summarise(temperature=mean(temperature,na.rm=TRUE),
              .groups="drop")
  
  dataset_sem <- dataset_sem %>%
    left_join(temp_sem, by="semaine")
  return(dataset_sem)
}

# ==============================================================================
# STANDARDISATION DES NOMS D'ESPECES
# ==============================================================================
#
# L'API d'HIZKIA fournit les espèces sous forme de codes.
# Une table de correspondance permet d'associer ces codes aux noms utilisés dans 
# l'interface et les résultats de l'application.
#
# Si un code espèce n'est pas présent dans la table de correspondance, le code
# lui-même est conservé comme nom d'espèce afin de ne pas perdre la donnée.
# ==============================================================================

# ------------------------------------------------------------------------------
# correspondance_especes()
# ------------------------------------------------------------------------------
# Table de correspondance
# ------------------------------------------------------------------------------
correspondance_especes <- tibble(code=c("TRF","TRM"),
                                 nom_espece=c("Truite fario","Truite de mer"))

# ------------------------------------------------------------------------------
# add_species_names()
# ------------------------------------------------------------------------------
# Ajoute le nom complet de l'espèce aux données de vidéocomptage à partir de son
# code.
#
# Argument :
#   video : jeu de données de vidéocomptage.
#
# Retour :
#   Jeu de données contenant les colonnes "espece" et "nom_espece".
# ------------------------------------------------------------------------------
add_species_names <- function(video, correspondance_especes){
  video<-video %>%
    left_join(correspondance_especes,by=c("espece"="code"))%>%
    mutate(nom_espece=ifelse(is.na(nom_espece),
                             espece,
                             nom_espece))
  return(video)
}


# ==============================================================================
# ANALYSES
# ==============================================================================
#
# Les fonctions suivantes sont utilisées pour préparer les tableaux et graphiques
# affichés dans les différentes pages de l'application.
#
# Elles travaillent sur les datasets préparés dans les sections précédentes et
# n'effectuent pas de modélisation statistique.
# ==============================================================================

# ------------------------------------------------------------------------------
# theme_passe()
# ------------------------------------------------------------------------------
# Définit un thème graphique commun pour les visuels de l'application, afin
# d'assurer une présentation homogène des différents graphiques.
# ------------------------------------------------------------------------------
theme_passe <- function(){
  theme_bw()+
    theme(
      plot.title=element_text(hjust=0.5,face="bold",size=14),
      plot.subtitle=element_text(hjust=0.5),
      legend.position="top",
      panel.grid.minor=element_blank(),
      panel.grid.major=element_line(colour="grey90")
    )
}

# ------------------------------------------------------------------------------
# format_tableau()
# ------------------------------------------------------------------------------
# Prépare les noms et certains formats de variables avant affichage dans les
# tableaux de l'app.
#
# Les noms techniques des variables sont remplacés par des intitulés destinés à
# l'utilisateur.
# ------------------------------------------------------------------------------
format_tableau <- function(tab){
  if("mois" %in% names(tab)){
    tab$mois <- stringr::str_to_title(tab$mois)
  }
  noms <- c(
    "mois"="Mois",
    "effectif"="Effectif",
    "pourcentage"="Pourcentage (%)",
    "taille_moy"="Taille moyenne (cm)",
    "taille_min"="Taille minimale (cm)",
    "taille_max"="Taille maximale (cm)",
    "nb_mont"="Nombre de montaisons",
    "heure"="Heure",
    "annee"="Année",
    "nom_espece"="Espèce",
    "taille_med"="Taille médiane (cm)",
    "nb_seq"="Nombre de séquences",
    "taux_mont"="Taux de montaison (Montaisons/Séquences)",
    "montaisons"="Montaisons",
    "sequences"="Séquences",
    "fonctionnement"="Fonctionnement (%)",
    "debut"="Début",
    "fin"="Fin",
    "H_moy"="Hauteur d'eau moyenne (cm)",
    "H_min"="Hauteur d'eau minimale (cm)",
    "H_max"="Hauteur d'eau maximale (cm)",
    "pct_fonctionnement"="Fonctionnement du système vidéo (%)",
    "temps_total"="Durée totale (h)",
    "temps_dispo"="Durée de fonctionnement (h)"
  )
  names(tab) <- ifelse(
    names(tab) %in% names(noms),
    noms[names(tab)],
    names(tab)
  )
  if("mois_num" %in% names(tab)){
    names(tab)[names(tab)=="mois_num"] <- ""
  }
  return(tab)
}

# ------------------------------------------------------------------------------
# datatable_passe()
# ------------------------------------------------------------------------------
# Transforme un tableau de données préparé par format_tableau() en tableau
# interactif affiché dans l'application.
#
# Les options d'affichage sont volontairement limitées afin de privilégier une
# lecture directe des résultats.
# ------------------------------------------------------------------------------
datatable_passe <- function(tab){
  DT::datatable(format_tableau(tab),
                options=list(
                  paging=FALSE,searching=FALSE,info=FALSE,
                  ordering=TRUE,scrollX=TRUE,dom="t",
                  order=list()),
                rownames=FALSE)
}

# ------------------------------------------------------------------------------
# build_resume_mensuel()
# ------------------------------------------------------------------------------
# Calcule les principaux indicateurs de suivi à l'échelle mensuelle :
#   - montaisons
#   - séquences
#   - taux de montaison
#   - durée totale du mois
#   - durée de fonctionnement du système au cours du mois
#   - pourcentage de fonctionnement
#   - hauteur d'eau moyenne, minimale, maximale
#
# Les résultats sont formatés pour être directement affichés dans un tableau.
# ------------------------------------------------------------------------------
build_resume_mensuel <- function(dataset_h){
  resume_mensuel <- dataset_h %>%
    mutate(mois=month(date,label=TRUE)) %>%
    group_by(mois) %>%
    summarise(
      nb_mont = sum(nb_mont,na.rm=TRUE),
      nb_seq=sum(nb_seq,na.rm=TRUE),
      taux_mont = ifelse(nb_seq>0,
                         100*nb_mont/nb_seq,
                         NA),
      temps_total=n(),
      temps_dispo=sum(!panne,na.rm=TRUE),
      pct_fonctionnement=100*temps_dispo/temps_total,
      H_moy=mean(hauteur,na.rm=TRUE),
      H_min=min(hauteur,na.rm=TRUE),
      H_max=max(hauteur,na.rm=TRUE),
      .groups="drop"
    ) %>%
    mutate(
      taux_mont=sprintf("%.2f",taux_mont),
      pct_fonctionnement=ifelse(pct_fonctionnement==100,100,sprintf("%.2f",pct_fonctionnement)),
      H_moy=sprintf("%.2f",H_moy),
      H_min=sprintf("%.2f",H_min),
      H_max=sprintf("%.2f",H_max),
      )
  return(resume_mensuel)
}


# ==============================================================================
# ANALYSE DES PERIODES SANS MONTAISON
# ==============================================================================

# ------------------------------------------------------------------------------
# find_no_passage_periods()
# ------------------------------------------------------------------------------
# Identifie les périodes de plusieurs jours consécutifs durant lesquelles aucune
# montaison n'est enregistrée.
# 
# Argument :
#   seuil_jours : durée minimale d'une période à identifier
#                 Valeur par défaut : 5 jours.
#
# Retour :
#   Tableau contenant la date de début, la date de fin et la durée de chaque
#   période répondant au seuil défini.
#
# Cette fonction décrit les données disponibles ; elle ne permet pas, à elle
# seule, de conclure à une absence réelle de poissons.
# ------------------------------------------------------------------------------
find_no_passage_periods <- function(dataset_j,seuil_jours=5){
  dataset_j <- dataset_j %>%
    mutate(absence=nb_mont==0)
  r<-rle(dataset_j$absence)
  fins<-cumsum(r$lengths)
  debuts<-fins-r$lengths+1
  periodes<-tibble(absence=r$values,
                   debut=dataset_j$date[debuts],
                   fin=dataset_j$date[fins],
                   duree=r$lengths
  ) %>%
    filter(absence,duree>=seuil_jours)
  return(periodes)
}

# ------------------------------------------------------------------------------
# plot_daily_passage()
# ------------------------------------------------------------------------------
# Produit un graphique de suivi journalier des montaisons.
#
# Les périodes de panne sont représentées sur le graphique afin de distinguer
# les périodes d'absence de montaison des périodes durant lesquelles le système
# vidéo n'était pas fonctionnel.
# ------------------------------------------------------------------------------
plot_daily_passage <- function(dataset_j,periodes_pannes){
  annee <- year(min(dataset_j$date))
  ggplot() +
    geom_rect(
      data=periodes_pannes,
      aes(xmin=debut,xmax=fin,
          ymin=-Inf,ymax=Inf,
          fill="Système non fonctionnel"
      ),
      color="red",
      linetype = "dashed",
      alpha=0.2
    ) +
    geom_line(data=dataset_j,
              aes(x=date,y=nb_mont),
              color="blue",
              linewidth=0.7,
              na.rm=FALSE) +
    scale_fill_manual(
      name="Statut vidéo",
      values=c("Système non fonctionnel"="red")
    ) +
    labs(
      title=paste("Suivi de la montaison",annee),
      x="Temps",
      y="Nombre de montaisons"
    ) +
    theme_passe()+
    theme(legend.position = "top",plot.title=element_text(hjust=0.5))
}


# ==============================================================================
# ANALYSES PAR ESPECE
# ==============================================================================

# ------------------------------------------------------------------------------
# build_tableau_especes()
# ------------------------------------------------------------------------------
# Calcule le nombre de montaisons pour chaque couple espèce-mois.
#
# Le résultat est présenté sous forme de tableau croisé avec :
#   - une ligne par mois
#   - une colonne par espèce.
# ------------------------------------------------------------------------------
build_tableau_especes <- function(
    video){
  tableau_especes <- video %>%
    filter(sens==1)%>%
    mutate(
      mois=month(date,label=TRUE,abbr=FALSE)
    )%>%
    group_by(mois,nom_espece)%>%
    summarise(
      nb_mont=n(),
      .groups="drop"
    ) %>%
    pivot_wider(names_from=nom_espece,values_from=nb_mont,values_fill=0)%>%
    arrange(mois)
  return(tableau_especes)
}

# ------------------------------------------------------------------------------
# interpret_especes()
# ------------------------------------------------------------------------------
# Produit un résumé descriptif de l'activité de montaison pour chaque espèce.
#
# Le résumé indique :
#   - le nombre total de montaisons observées
#   - le mois présentant le plus grand nombre de montaisons.
#
# Il s'agit d'une description des données disponibles et non d'une 
# interprétation du fonctionnement biologique du site.
# ------------------------------------------------------------------------------
interpret_especes <- function(video){
  phrases_especes <- video %>%
    filter(sens==1) %>%
    mutate(
      mois = month(date,
                   label=TRUE,
                   abbr=FALSE)
    ) %>%
    group_by(nom_espece,mois)%>%
    summarise(nb_mont=n(),
              .groups="drop")%>%
    group_by(nom_espece)%>%
    mutate(total_espece=sum(nb_mont))%>%
    slice_max(order_by = nb_mont,n=1,with_ties=FALSE)%>%
    ungroup()%>%
    mutate(interpretation=paste0(nom_espece," : ",total_espece," montaisons observées au total, avec une activité maximale en ",mois," (",nb_mont," montaisons)."))
  return(phrases_especes)
}


# ==============================================================================
# CONDITIONS ENVIRONNEMENTALES
# ==============================================================================

# ------------------------------------------------------------------------------
# plot_conditions_env()
# ------------------------------------------------------------------------------
# Représente simultanément les montaisons hebdomadaires, le débit moyen et la 
# température moyenne de l'eau.
#
# Le débit et la température sont redimensionnés graphiquement afin de pouvoir
# être représentés sur le même graphique que le nombre de montaisons.
#
# Cette représentation est destinée à faciliter l'exploration visuelle des
# variations temporelles entre les variables. Elle ne constitue pas une analyse
# statistique de leur relation.
# ------------------------------------------------------------------------------
plot_conditions_env <- function(dataset_sem){
  coef<-max(dataset_sem$nb_mont,na.rm=TRUE)/max(dataset_sem$debit,na.rm=TRUE)
  ggplot(dataset_sem,aes(x=semaine)) +
    geom_col(aes(y=nb_mont,fill="Montaisons"),alpha=0.8) +
    geom_line(aes(y=debit*coef,color="Débit moyen (m3/s)"),linewidth=0.8) +
    geom_line(aes(y=temperature*2*coef,color="Température moyenne de l'eau (°C x2)"),linewidth=0.8)+
    scale_fill_manual(values=c("Montaisons"="forestgreen"),name=NULL)+
    scale_color_manual(values=c("Débit moyen (m3/s)"="darkblue","Température moyenne de l'eau (°C x2)"="red"),name=NULL)+
    scale_y_continuous(name="Nombre de montaisons",sec.axis=sec_axis(~./coef,name="Débit et température")) +
    scale_x_date(date_breaks = "1 month",date_labels="%b")+
    labs(title="Montaisons hebdomadaires et conditions environnementales",x="Semaine")+
    theme_passe()+
    theme(legend.position="top",legend.text=element_text(size=10),plot.title=element_text(hjust=0.5))
}


# ==============================================================================
# TAILLES
# ==============================================================================

# ------------------------------------------------------------------------------
# build_video_montaison()
# ------------------------------------------------------------------------------
# Extrait les séquences correspondant aux montaisons pour lesquelles une taille
# exploitable est disponible.
#
# Les éventuels individus dont la taille est manquante ou absurde sont exclus
# des données pour ces graphiques/tableaux.
# ------------------------------------------------------------------------------
build_video_montaison <- function(video){
  video_montaison <- video %>%
    filter(sens==1)%>%
    filter(!is.na(taille),
           taille>0)%>%
    mutate(mois=month(datetime,label=TRUE))
  return(video_montaison)
}

# ------------------------------------------------------------------------------
# plot_taille_especes()
# ------------------------------------------------------------------------------
# Représente la distribution des tailles observées pour chaque espèce sous forme
# d'histogramme.
# ------------------------------------------------------------------------------
plot_taille_especes <- function(video_montaison){
  ggplot(video_montaison,aes(x=taille)) +
    geom_histogram(
      fill="#2e75b6",
      colour="#1f4e79",
      linewidth=0.3,
      bins=20)+
    facet_wrap(~nom_espece,scales="free_y")+
    labs(title="Distribution des tailles par espèce",
         x="Taille (cm)",y="Nombre d'individus")+
    theme_passe()
}

# ------------------------------------------------------------------------------
# build_table_taille_especes()
# ------------------------------------------------------------------------------
# Calcule, pour chaque espèce :
#   - le nombre d'individus mesurés
#   - la taille moyenne
#   - la taille médiane
#   - la taille minimale
#   - la taille maximale.
# ------------------------------------------------------------------------------
build_table_taille_especes <- function(video_montaison){
  video_montaison %>%
    group_by(nom_espece)%>%
    summarise(n=n(),
              taille_moy=round(mean(taille,na.rm=TRUE),1),
              taille_med=round(median(taille,na.rm=TRUE),1),
              taille_min=round(min(taille,na.rm=TRUE),1),
              taille_max=round(max(taille,na.rm=TRUE),1),
              .groups="drop")
}

# ------------------------------------------------------------------------------
# interpret_taille_especes()
# ------------------------------------------------------------------------------
# Produit un résumé descriptif des tailles observées pour chaque espèce.
#
# Indique le nombre d'individus pris en compte, la taille moyenne et l'intervalle
# des tailles observées.
# ------------------------------------------------------------------------------
interpret_taille_especes <- function(table_taille){
  table_taille %>%
    mutate(interpretation=paste0(nom_espece," : ",n," individus observés, taille moyenne de ",
                                taille_moy," cm (",taille_min," à ",taille_max," cm)."))
}

# ==============================================================================
# ANALYSES HORAIRES
# ==============================================================================

# ------------------------------------------------------------------------------
# build_table_horaire()
# ------------------------------------------------------------------------------
# Calcule le nombre de montaisons par heure et par mois, pour étudier les
# éventuelles différences d'activité selon l'heure de la journée et la 
# période de l'année.
#
# Un total annuel est également calculé pour chaque heure de la journée.
# ------------------------------------------------------------------------------
build_table_horaire <- function(video){
  mois_fr <- c("janvier","février","mars","avril","mai","juin",
               "juillet","août","septembre","octobre","novembre","décembre")
  table_horaire <- video %>%
    filter(sens==1)%>%
    mutate(mois=factor(month(date),
                  levels=1:12,
                  labels=mois_fr,
                  ordered=FALSE))%>%
    group_by(heure,mois)%>%
    summarise(nb_mont=n(),.groups="drop")%>%
    pivot_wider(names_from=mois,values_from=nb_mont,values_fill=0)%>%
    mutate(Total=rowSums(across(-heure)))%>%
    select(heure,Total,janvier,février,mars,avril,mai,juin,
           juillet,août,septembre,octobre,novembre,décembre)%>%
    arrange(heure)
  return(table_horaire)
}

# ------------------------------------------------------------------------------
# plot_horaire_global()
# ------------------------------------------------------------------------------
# Représente la répartition horaire globale des montaisons.
# ------------------------------------------------------------------------------
plot_horaire_global <- function(video){
  video%>%
    filter(sens==1)%>%
    ggplot(aes(x=heure))+
    geom_bar(
      fill="#2e75b6",
      colour="#1f4e79",
      linewidth=0.3
    )+
    labs(title="Répartition horaire des montaisons",
         x="Heure",y="Nombre de montaisons")+
    theme_passe()+
    theme(plot.title=element_text(hjust=0.5))
}

# ------------------------------------------------------------------------------
# plot_horaire_mensuel()
# ------------------------------------------------------------------------------
# Représente la répartition horaire des montaisons séparément pour chaque mois.
# ------------------------------------------------------------------------------
plot_horaire_mensuel <- function(video){
  video%>%
    filter(sens==1)%>%
    mutate(mois=month(date,label=TRUE,abbr=FALSE))%>%
    ggplot(aes(x=heure))+
    geom_bar(
      fill="#2e75b6",
      colour="#1f4e79",
      linewidth=0.3)+
    facet_wrap(~mois)+
    labs(title="Répartition horaire des montaisons par mois",
         x="Heure",y="Nombre de montaisons")+
    theme_passe()
}


# ==============================================================================
# ANALYSES INTERANNUELLES
# ==============================================================================
#
# Les fonctions de cette section chargent les jeux de données annuels sauvegardés
# afin de permettre leur comparaison.
#
# Les résultats présentés sont descriptifs : ils permettent de comparer les
# effectifs, distributions temporelles, espèces, tailles et rythmes horaires
# entre plusieurs années.
# ==============================================================================

# ------------------------------------------------------------------------------
# load_annee()
# ------------------------------------------------------------------------------
# Charge le fichier RDS correspondant à une année donnée depuis le dossier "data/".
#
# Utilisée dans les fonctions suivantes pour récupérer les années souhaitées.
# ------------------------------------------------------------------------------
load_annee <- function(an){
  readRDS(paste0("data/",an,".rds"))
}

# ------------------------------------------------------------------------------
# plot_interannuel()
# ------------------------------------------------------------------------------
# Compare le nombre total de montaisons entre plusieurs années.
# ------------------------------------------------------------------------------
plot_interannuel <- function(annees){
  tab <- purrr::map_dfr(
    annees,
    function(an){
      d<-load_annee(an)
      tibble(annee=factor(an),montaisons=sum(d$dataset_h$nb_mont,na.rm=TRUE))
    }
  )
  ggplot(tab,aes(annee,montaisons))+
    geom_col(
      width=.65,
      fill="#2e75b6",
      colour="#1f4e79",
      linewidth=.5
    ) +
    geom_text(aes(label=scales::comma(montaisons)),
              vjust=-0.4,
              fontface="bold",
              colour="#1f4e79",
              size=4.8)+
    scale_y_continuous(expand=expansion(mult=c(0,.08)))+
    labs(title="Montaisons totales par année",
         x="Année",
         y="Montaisons")+
    theme_passe()
}

# ------------------------------------------------------------------------------
# plot_interannuel_mensuel()
# ------------------------------------------------------------------------------
# Compare la répartition mensuelle des montaisons entre plusieurs années.
# ------------------------------------------------------------------------------
plot_interannuel_mensuel <- function(annees){
  tab <- purrr::map_dfr(
    annees,
    function(an){
      d<-readRDS(paste0("data/",an,".rds"))
      d$dataset_j %>%
        mutate(mois=month(date,label=TRUE,abbr=TRUE)) %>%
        group_by(mois) %>%
        summarise(montaisons=sum(nb_mont,na.rm=TRUE),
                  .groups="drop")%>%
        mutate(annee=an)
    }
  )
  ggplot(tab,aes(mois,montaisons,colour=annee,group=annee))+
    geom_line(linewidth=1.2)+
    geom_point(size=2.8)+
    scale_colour_brewer(palette="Dark2")+
    labs(title="Répartition mensuelle des montaisons",
         subtitle="Comparaison annuelle",
         x="Mois",
         y="Nombre de montaisons",
         colour="Année")+
    theme_passe()
}

# ------------------------------------------------------------------------------
# build_resume_interannuel()
# ------------------------------------------------------------------------------
# Construit un tableau récapitulatif par année contenant :
#   - le nombre de montaisons
#   - le nombre de séquences avec poisson
#   - le "taux de montaison" (montaisons/séquences)
#   - le pourcentage de fonctionnement du système vidéo.
# ------------------------------------------------------------------------------
build_resume_interannuel <- function(annees){
  purrr::map_dfr(annees,function(an){
    d<-load_annee(an)
    dataset_h <- d$dataset_h
    nb_mont<-sum(dataset_h$nb_mont,na.rm=TRUE)
    nb_seq<-sum(dataset_h$nb_seq,na.rm=TRUE)
    taux_mont <- round(100*nb_mont/nb_seq,1)
    pct_fonctionnement<-round(100*sum(!is.na(dataset_h$nb_mont))/
                                nrow(dataset_h),2)
    tibble(
      annee=an,montaisons=nb_mont,sequences=nb_seq,
      taux_mont=taux_mont,fonctionnement=pct_fonctionnement
    )
  })
}

# ------------------------------------------------------------------------------
# build_table_interannuelle_mensuelle()
# ------------------------------------------------------------------------------
# Construit un tableau comparatif des montaisons mensuelles pour les années
# sélectionnées.
# ------------------------------------------------------------------------------
build_table_interannuelle_mensuelle <- function(annees){
  purrr::map_dfr(annees,function(an){
    d<-load_annee(an)
    d$dataset_j %>%
      mutate(mois=month(date,label=TRUE,abbr=TRUE)) %>%
      group_by(mois) %>%
      summarise(montaisons=sum(nb_mont,na.rm=TRUE),
                .groups="drop") %>%
      mutate(annee=an)
  }) %>%
    pivot_wider(names_from=annee,values_from=montaisons)
}

# ------------------------------------------------------------------------------
# build_table_interannuelle_especes()
# ------------------------------------------------------------------------------
# Construit un tableau comparatif des montaisons par couple espèce-année.
# ------------------------------------------------------------------------------
build_table_interannuelle_especes <- function(annees){
  purrr::map_dfr(annees,function(an){
    d<-load_annee(an)
    d$video %>%
      filter(sens==1)%>%
      group_by(nom_espece) %>%
      summarise(effectif=n(),.groups="drop") %>%
      mutate(annee=an)
  }) %>%
    pivot_wider(names_from=annee,values_from=effectif,values_fill=0)
}

# ------------------------------------------------------------------------------
# plot_interannuel_especes()
# ------------------------------------------------------------------------------
# Représente l'évolution du nombre de montaisons observées pour chaque espèce au 
# cours des années sélectionnées.
# ------------------------------------------------------------------------------
plot_interannuel_especes <- function(annees){
  tab <- purrr::map_dfr(annees,function(an){
    d<-load_annee(an)
    d$video %>%
      filter(sens==1)%>%
      group_by(nom_espece)%>%
      summarise(effectif=n(),.groups="drop")%>%
      mutate(annee=as.numeric(an))
  })
  ggplot(tab,aes(x=annee,y=effectif,color=nom_espece,group=nom_espece)) +
    geom_line(linewidth=1) +
    geom_point(size=3) +
    facet_wrap(~nom_espece,scales="free_y")+
    scale_x_continuous(breaks=sort(unique(tab$annee)))+
    labs(title="Evolution par espèce et par année",
         x="Année",y="Nombre de montaisons",color="Espèce")+
    theme_passe()
}

# ------------------------------------------------------------------------------
# build_table_taille_interannuelle()
# ------------------------------------------------------------------------------
# Calcule la taille moyenne des individus en montaison pour chaque couple
# espèce-année.
# ------------------------------------------------------------------------------
build_table_taille_interannuelle <- function(annees){
  purrr::map_dfr(annees,function(an){
    d<-load_annee(an)
    d$video %>%
      filter(sens==1,
             !is.na(taille))%>%
      group_by(nom_espece) %>%
      summarise(taille_moy=round(mean(taille),1),
                .groups="drop")%>%
      mutate(annee=an)
  }) %>%
    pivot_wider(names_from=annee,values_from = taille_moy)
}

# ------------------------------------------------------------------------------
# plot_taille_interannuelle()
# ------------------------------------------------------------------------------
# Distribution des tailles des individus en montaison pour chaque espèce et
# chaque année sélectionnée.
#
# La moyenne est indiquée sur chaque distribution pour faciliter la lecture.
# ------------------------------------------------------------------------------
plot_taille_interannuelle <- function(annees){
  tab <- purrr::map_dfr(annees,function(an){
    d<-load_annee(an)
    d$video %>%
      filter(sens==1,!is.na(taille)) %>%
      mutate(annee=as.factor(an))
  })
  ggplot(tab,aes(x=annee,y=taille))+
    geom_boxplot()+
    stat_summary(fun=mean,geom="point",shape=18,size=3,color="red")+
    facet_wrap(~nom_espece,scales="free_y")+
    labs(title="Distribution des tailles par espèce et par année",
         x="Année",y="Taille (cm)")+
    theme_passe()
}

# ------------------------------------------------------------------------------
# build_table_horaire_interannuelle()
# ------------------------------------------------------------------------------
# Tableau comparatif du nombre de montaisons observées à chaque heure pour les
# différentes années sélectionnées.
# ------------------------------------------------------------------------------
build_table_horaire_interannuelle <- function(annees){
  purrr::map_dfr(annees,function(an){
    d<-load_annee(an)
    d$video %>%
      filter(sens==1) %>%
      group_by(heure) %>%
      summarise(nb_mont=n(),
                .groups="drop")%>%
      mutate(annee=an)
  }) %>%
    pivot_wider(names_from=annee,values_from = nb_mont,values_fill = 0)%>%
    arrange(heure)
}

# ------------------------------------------------------------------------------
# plot_horaire_interannuel()
# ------------------------------------------------------------------------------
# Compare la répartition horaire des montaisons entre plusieurs années.
# ------------------------------------------------------------------------------
plot_horaire_interannuel <- function(annees){
  tab <- purrr::map_dfr(annees,function(an){
    d<-load_annee(an)
    d$video %>%
      filter(sens==1) %>%
      group_by(heure) %>%
      summarise(nb_mont=n(),.groups="drop")%>%
      mutate(annee=an)
  })
  ggplot(tab,aes(x=heure,y=nb_mont,color=annee))+
    geom_line(linewidth=1)+
    geom_point()+
    scale_x_continuous(breaks=0:23)+
    labs(title="Activité horaire interannuelle",
         x="Heure",y="Nombre de montaisons",color="Année")+
    theme_passe()
}


# ==============================================================================
# TELECHARGEMENT DES GRAPHIQUES
# ==============================================================================

# ------------------------------------------------------------------------------
# confirm_download()
# ------------------------------------------------------------------------------
# Affiche une fenêtre de confirmation avant le téléchargement d'un graphique.
#
# L'objectif est de rappeler que les graphiques sont des supports d'exploration,
# et ne constituent en aucun cas des preuves biologiques.
#
# Arguments :
#   input : objet input de l'app Shiny
#   id_bouton : identifiant du bouton déclenchant le téléchargement
#   id_downnload : identifiant du bouton de téléchargement associé.
# ------------------------------------------------------------------------------
confirm_download <- function(input,id_bouton,id_download){
  observeEvent(input[[id_bouton]],{
    showModal(
      modalDialog(
        title="Attention",
        tags$p("Ce graphique seul ne permet pas une interprétation scientifique fiable."),
        tags$p("Il doit être accompagné des analyses statistiques et du contexte biologique associés."),
        footer=tagList(
          tags$button(type="button",
          class="btn btn-danger",
          'data-dismiss'="modal",
          "Annuler"),
          downloadButton(
            outputId = id_download,
            label="Télécharger quand même"
          )
        ),
        easyClose = TRUE
      )
    )
  })
}

