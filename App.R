library(shiny)
library(readxl)
library(tidyverse)
library(DT)
library(highcharter)
library(shinyjs)
library(shinycssloaders) 
library(rintrojs)
library(officer)
library(flextable)
library(glue)
library(stringi)
library(plyr)
library(fresh) 


source("Fonctions.R",encoding="UTF-8")

source("Modules.R",encoding="UTF-8")


load("data/ListeDesIndicateurs.RData")



liste_des_tabs<-lapply(c("Accueil",ListeDesIndicateurs), function(indicateur){
  
  tabName <- paste0("tab_",tolower(suppression_accents(indicateur)))
  
  PageName <- paste0("page_",tolower(suppression_accents(indicateur)))
  
  uioutput_id <- paste0("uiouput_",tolower(suppression_accents(indicateur)))
  
  bouton_accueil_id <- paste0("bouton_accueil_id_",tolower(suppression_accents(indicateur)))
  
  
if(tabName=="tab_accueil"){

    tabPanel(title="Accueil",value="accueil",
             
           div(class="container-fluid custom-container-fluid",style="padding:0px 20px;background-color:#f8f9f9 !important;", # background-image: url('background.jpg')
                            
                            style="background-color:#fff;flex-direction:column;justify-content:center;", 
                            
                            div(class="row text-center",style="border:1px solid transparent;margin-top:4%;padding:0;",
                                
                                h2("SÉRIES CHRONOLOGIQUES SUR LA DÉLINQUANCE ET L'INSÉCURITÉ")),
                            
                            div(class="row text-start",style="border:1px solid transparent;",
                                
                                div(class="col-md-12",
                                    
                                    h5("Cette application permet de diffuser toutes les séries chronologiques de la délinquance et l'insécurité ayant fait l'objet de travaux de fiabilisation par le service statistique ministériel de la sécurité intérieure (SSMSI)."),
                                    
                                    h5("Ces séries sont actualisées en continu au cours de l'année de manière à maintenir l’application toujours à jour."),
                                    
                                    h5("De nouveaux indicateurs seront ajoutés dès qu’ils auront fait l’objet d’une publication par le SSMSI sur le site :",tags$a(href="https://www.interieur.gouv.fr/Interstats/Actualites", target="_blank","Interstats"))
                                    
                                )
                                
                            ),
                            
                            br(),

                            div(class = "row",
                                
                                lapply(ListeDesIndicateurs, function(bouton) {
                                  
                                  div(class = "col-md-6 col-lg-3",actionButton(paste0("action_", tolower(gsub("\\s|'", "_", iconv(bouton, to = "ASCII//TRANSLIT"))) ), bouton,class="boutons-accueil"))
                                  
                                })),
                            
                            
                            br(),
                            br(),
                            br()))

  } else {

    tabPanel(indicateur, value = tabName, br(), br(),
             
             PageDeContenu(
               
               actionButton(bouton_accueil_id, "Retour à l'accueil",class="btn-back"), # withSpinner(uiOutput(uioutput_id),type=8)
               
               ModuleUI(PageName), 
               
               tagList(uiOutput(uioutput_id), br(), br()
                       
               )))

  }

 
  
})



ui <- fluidPage(
  
tags$head(includeScript("google-analytics_series_chrono.js")),
  
tags$head(includeHTML("google-analytics_series_chrono.html")),  

useShinyjs(),

includeScript(path = "script.js"),

includeCSS("css/accueil.css"),
  

use_googlefont("Roboto"),

use_theme(create_theme(
  theme = "default",
  bs_vars_font(
    family_sans_serif = "'Roboto'"
  )
)),


div(class="nav",
    div(class="items",
        div(class="img",
            tags$ul(
              tags$li(tags$a(href="https://www.interieur.gouv.fr/Interstats/Actualites",target="_blank",
                             tags$img(src = 'RepubliqueSSMSI.png',style="height:95px;")))
              
              
            )
        )
    ),
    div(class="items",
        div(class="titre",
            p("")
))),

includeCSS("css/App.css"),
includeCSS("css/nav.css"),
includeCSS("css/footer.css"),  

do.call(tabsetPanel,c(id="tabsetpanel_id",liste_des_tabs)),

br(),
br(),
br(),

includeHTML("css/footer.html"))


server <- function(input, output, session) {

observeEvent(input$page_accueil,{

    updateTabsetPanel(session, "tabsetpanel_id",selected="accueil")


})
  
  
  
lapply(ListeDesIndicateurs,function(bouton){
  
  bouton_accueil_id <- paste0("bouton_accueil_id_",tolower(suppression_accents(bouton)))
  
  observeEvent(input[[bouton_accueil_id]], {
    
    updateTabsetPanel(session, "tabsetpanel_id",selected="accueil")
    
  })
  
  
})  
  
  

lapply(ListeDesIndicateurs,function(indicateur){
  
  action<-paste0("action_", tolower(gsub("\\s|'", "_", iconv(indicateur, to = "ASCII//TRANSLIT"))) )
  
  tabName <- paste0("tab_",tolower(suppression_accents(indicateur)))
  
  PageName <- paste0("page_",tolower(suppression_accents(indicateur)))
  
  uioutput_id <- paste0("uiouput_",tolower(suppression_accents(indicateur)))
  
  dataName <- paste0("data_",tolower(suppression_accents(indicateur)))
  
  observeEvent(input[[action]], {
    
    updateTabsetPanel(session, "tabsetpanel_id", selected = tabName)
    
    load(paste0("data/", dataName, ".RData"))
    
    TableData <- callModule(ModuleServer,PageName, DataTable = get(dataName) )
    
    output[[uioutput_id]] <- renderUI({
      
      TableData <- TableData()
      
      texte_descriptif <- creation_texte_descriptif(TableData)
      
      figure_et_descriptif <- list(
        
        Creation_charts(TableData),
        
        texte_descriptif
      )
      
      do.call(tagList, figure_et_descriptif)
    })
    
    
  })
  
  
})



# Empêcher l'application d'expirer rapidement
autoInvalidate <- reactiveTimer(10000)
  
observe({
    
    autoInvalidate()
    
    cat(".")
    
}) 
  
  
  
}

shinyApp(ui, server)