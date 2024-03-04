
library(shiny)
library(shiny.router)
library(shinydashboard)
library(shinybusy)
library(shinythemes)
library(shinyWidgets)
library(readxl)
library(tidyverse)
library(DT)
library(highcharter)
library(lubridate, warn.conflicts = FALSE) 
library(shinyjs)
library(shinycssloaders) 
library(rintrojs)
library(officer)
library(flextable)
library(glue)
library(stringi)
library(plyr)


suppression_accents <- function(text) {
  text <- gsub("[^[:alnum:][:blank:]+?&/\\-^]", "", text)
  text<-gsub("à","a",text)
  text<-gsub("é","e",text)
  text<-gsub("è","e",text)
  text<-gsub("ê","e",text)
  text<-gsub("ô","o",text)
  text<-gsub(" ","_",text)
  text <- gsub("\\s", "", text)  # Supprime tous les espaces
  return(text)
}




Repertoire<-"M:/ArboSSMSI/2-Bureaux/24-BVSSP/241- Projet Diffusion/2411-externe/24106-Applications Shiny/series-chronologiques/proto/data/"


Data<-read_excel(paste0(Repertoire,"Base_series_chronologiques_v1.3.xlsx"))


ListeDesIndicateurs<-sort(unique(Data$Indicateur))


save(ListeDesIndicateurs,file=paste0(Repertoire,"ListeDesIndicateurs.RData"))  



modalites <- sort(unique(Data$Indicateur))


lapply(modalites, function(modalite) { 
  
  
  df_filtre <- Data %>% filter(Indicateur == modalite) 
  
  modalite_clean<-tolower(suppression_accents(modalite))
  
  assign(paste0("data_", modalite_clean), df_filtre) 
  
  save(list = paste0("data_", modalite_clean), file = paste0(Repertoire,"data_", modalite_clean, ".RData")) 
  
  
  
  }) 




























