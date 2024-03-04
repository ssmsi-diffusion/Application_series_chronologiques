

ModuleUI <- function(id) {

ns <- NS(id)

tagList(

  uiOutput(ns("BoutonsSources")),
  
  uiOutput(ns("BoutonsSousIndic")),
  
  uiOutput(ns("BoutonsNomenclature")),
  
  uiOutput(ns("BoutonsDeclinaison")),
  
  uiOutput(ns("BoutonsStatistiques")),
  
  uiOutput(ns("BoutonsZonesGeo")),
  
  uiOutput(ns("BoutonsUniteCompte")),
  
  
  div(class="moduleUI-container",
    
  h4("Télécharger la/les série(s) :"),
  downloadButton(".xlsx",outputId=ns('telechargementDoubleExcel'),class = "DownloadData",style="margin-bottom:8px;"),
  br(),
  downloadButton(".csv",outputId=ns('telechargementDoubleCSV'),class = "DownloadData"),
    
  ),

  br(),
  br()
  
  
  )


}





ModuleServer <- function(input, output, session,DataTable) {

  ns<-session$ns

  ReactiveSource<-reactive({

    #ns<-session$ns

    liste<-sort(unique(DataTable$Source))

    return(liste)

  })

  output$BoutonsSources<-renderUI({

    ns<-session$ns

    Ma_liste<-ReactiveSource()


    if (length(ReactiveSource())==1)
      return(NULL)

    radioButtons(ns("InputSources"),label=h4("Source(s) :","(",textOutput(ns("NbElementSource"),inline = TRUE),")"),choices=Ma_liste)

  })



  CorrectionSource<-reactive({
    ns<-session$ns
    if(length(ReactiveSource())==1){
      selected<-ReactiveSource()
    } else {
      selected<-input$InputSources
    }

  })

  ########################### Boutons Sous-Indicateurs ###########################################################
  ReactiveSousIndic<-reactive({
    ns<-session$ns
    liste_sous_indicateurs=sort(unique(( DataTable %>% dplyr::filter(DataTable$Source %in% CorrectionSource()))$Sous_indicateur))
  })

  output$BoutonsSousIndic<-renderUI({
    ns<-session$ns
    req(ReactiveSousIndic())
    
    if(length(ReactiveSousIndic())==1)
      return(NULL)
    radioButtons(ns("InputSousIndic"),label=h4("Sous-Indicateur(s) :","(",textOutput(ns("NbElementSousIndic"),inline = TRUE),")"),choices=ReactiveSousIndic(),
                 selected=ifelse(("Ensemble" %in% ReactiveSousIndic()),"Ensemble",ReactiveSousIndic()[1])
    )
  })

  CorrectionSousIndic<-reactive({
    ns<-session$ns
    if("NonRenseigne" %in% ReactiveSousIndic()&length(ReactiveSousIndic())==1) {
      element_selected<-c("NonRenseigne")
    } else if (!("NonRenseigne" %in% ReactiveSousIndic())&length(ReactiveSousIndic())==1) {
      element_selected<-ReactiveSousIndic()
    } else {
      element_selected<-input$InputSousIndic
    }
  })


############################ Bourons Nomenclature ################

ReactiveNomenclature<-reactive({

  #ns<-session$ns

  listeNomenclature=sort(unique((DataTable %>% dplyr::filter(

  DataTable$Source %in% CorrectionSource() &

  DataTable$Sous_indicateur %in% CorrectionSousIndic()))$Nomenclature))


})

output$BoutonsNomenclature<-renderUI({

  #ns<-session$ns

  req(ReactiveNomenclature())
  if(length(ReactiveNomenclature())==1)
    return(NULL)
  radioButtons(ns("InputNomenclature"),label=h4("Nomenclature(s) de définition du champ infractionnel :","(",textOutput(ns("NbElementNomenclature"),inline = TRUE),")"),

  choices=ReactiveNomenclature(),

  selected=input$InputNomenclature)
  })




CorrectionNomenclature<-reactive({
  #ns<-session$ns
    if("NonRenseigne" %in% ReactiveNomenclature() & length(ReactiveNomenclature())==1) {
      element_selected<-c("NonRenseigne")
    } else if (!("NonRenseigne" %in% ReactiveNomenclature()) & length(ReactiveNomenclature())==1) {
      element_selected<-ReactiveNomenclature()
    } else {
      element_selected<-input$InputNomenclature
    }
})




############################ Boutons Declinaison ################

ReactiveDeclinaison<-reactive({

  #ns<-session$ns

  listeDeclinaison=sort(unique((DataTable %>% dplyr::filter(

  DataTable$Source %in% CorrectionSource() &

  DataTable$Sous_indicateur %in% CorrectionSousIndic() &

  DataTable$Nomenclature %in% CorrectionNomenclature()))$Declinaison))})


output$BoutonsDeclinaison<-renderUI({

  #ns<-session$ns

  req(ReactiveDeclinaison())

  if(length(ReactiveDeclinaison())==1)
    return(NULL)

  radioButtons(ns("InputDeclinaison"),label=h4("Déclinaison(s) :","(",textOutput(ns("NbElementDeclinaison"),inline = TRUE),")"),

  choices=ReactiveDeclinaison(),

  selected=ifelse(("Ensemble" %in% ReactiveDeclinaison()),"Ensemble",ReactiveDeclinaison()[1]))


})



CorrectionDeclinaison<-reactive({
  #ns<-session$ns
  if("NonRenseigne" %in% ReactiveDeclinaison() & length(ReactiveDeclinaison())==1) {
    element_selected<-c("NonRenseigne")
  } else if (!("NonRenseigne" %in% ReactiveDeclinaison())&length(ReactiveDeclinaison())==1) {
    element_selected<-ReactiveDeclinaison()
  } else {
    element_selected<-input$InputDeclinaison
  }
})


########################### Boutons Statistique ##############################################################################

ReactiveStatistique<-reactive({

  #ns<-session$ns

  liste_statistique=sort(unique((DataTable %>% filter(

  DataTable$Source %in% CorrectionSource() &

  DataTable$Sous_indicateur %in% CorrectionSousIndic() &

  DataTable$Nomenclature %in% CorrectionNomenclature() &

  DataTable$Declinaison %in% CorrectionDeclinaison()))$Statistique))})



output$BoutonsStatistiques<-renderUI({

  #ns<-session$ns

  req(ReactiveStatistique())

  if(length(ReactiveStatistique())==1)
    return(NULL)

  radioButtons(ns("InputStatistique"),label=h4("Statistique(s) :","(",textOutput(ns("NbElementStatistique"),inline = TRUE),")"),choices=ReactiveStatistique(),selected=ReactiveStatistique()[1])

})



CorrectionStatistique<-reactive({
  #ns<-session$ns
  if(length(ReactiveStatistique())==1){
    selected<-ReactiveStatistique()
  } else {
    selected<-input$InputStatistique
  }
})




########################### Boutons Z.G ###########################################################################

ReactiveZoneGeo<-reactive({

  #ns<-session$ns

  liste_zone_geographique<-sort(unique((DataTable %>% filter(

  DataTable$Source %in% CorrectionSource() &

  DataTable$Sous_indicateur %in% CorrectionSousIndic() &

  DataTable$Nomenclature %in% CorrectionNomenclature() &

  DataTable$Declinaison %in% CorrectionDeclinaison() &

  DataTable$Statistique %in% CorrectionStatistique() ))$Zone_geographique))})



output$BoutonsZonesGeo<-renderUI({

  #ns<-session$ns

  req(ReactiveZoneGeo())

  if(length(ReactiveZoneGeo())==1)
    return(NULL)

  selectInput(ns("InputZoneGeo"),label=h4("Zone Géographique(s) :","(",textOutput(ns("NbElementZoneGeo"),inline = TRUE),")"),choices=ReactiveZoneGeo(),selected=input$InputZoneGeo)

})



CorrectionZoneGeo<-reactive({
  #ns<-session$ns
  if(length(ReactiveZoneGeo())==1){
    selected<-ReactiveZoneGeo()
  } else {
    selected<-input$InputZoneGeo
  }
})



########################### Boutons Unite de compte #########################################################################################

ReactiveUniteCompte<-reactive({

  #ns<-session$ns

  liste_unite_de_compte=sort(unique((DataTable %>% filter(

  DataTable$Source %in% CorrectionSource() &

  DataTable$Sous_indicateur %in% CorrectionSousIndic() &

  DataTable$Nomenclature %in% CorrectionNomenclature() &

  DataTable$Declinaison %in% CorrectionDeclinaison() &

  DataTable$Statistique %in% CorrectionStatistique() &

  DataTable$Zone_geographique %in% CorrectionZoneGeo()))$Unite_de_compte))})


output$BoutonsUniteCompte<-renderUI({

  #ns<-session$ns

  req(ReactiveUniteCompte())

  if(length(ReactiveUniteCompte())==1)
    return(NULL)

  radioButtons(ns("InputUniteCompte"),label=h4("Unités de compte(s) :","(",textOutput(ns("NbElementUnitedeCompte"),inline = TRUE),")"),choices= ReactiveUniteCompte(),selected=input$InputUniteCompte)})



CorrectionUniteCompte<-reactive({
  # ns<-session$ns
  if(length(ReactiveUniteCompte())==1){

    selected<-ReactiveUniteCompte()

  } else {

    selected<-input$InputUniteCompte
  }
})


output$NbElementSource<-renderText({length(ReactiveSource())})
output$NbElementSousIndic<-renderText({length(ReactiveSousIndic() )})
output$NbElementStatistique<-renderText({length(ReactiveStatistique())})
output$NbElementPeriodicite<-renderText({length(ReactivePeriodicite())})
output$NbElementZoneGeo<-renderText({length(ReactiveZoneGeo())})
output$NbElementUnitedeCompte<-renderText({length(ReactiveUniteCompte())})
output$NbElementNomenclature<-renderText({length(ReactiveNomenclature())})
output$NbElementDeclinaison<-renderText({length(ReactiveDeclinaison())})






#############################################################################################################

DataInformations<-reactive({

  serie_chronologique<-DataTable %>% filter (

      DataTable$Source %in% CorrectionSource() &

      DataTable$Sous_indicateur %in% CorrectionSousIndic() &

      DataTable$Nomenclature %in% CorrectionNomenclature() &

      DataTable$Declinaison %in% CorrectionDeclinaison() &

      DataTable$Statistique %in% CorrectionStatistique() &

      DataTable$Zone_geographique %in% CorrectionZoneGeo() &

      DataTable$Unite_de_compte %in% CorrectionUniteCompte()

  )

  DataInformations<-data.frame(serie_chronologique)


})


##########################################################################################################################################  
################################################Telecharegemnt############################################################################
########################################################################################################################################## 

#### CSV

output$telechargementDoubleCSV <- downloadHandler(

  filename = function(){paste0(DataInformations()$Indicateur,'.csv')},

  content = function(file) {

    data<-DataInformations() %>% select(

      "Unité de temps"=Unite_temps, Valeurs,

      "Indicateurs"=Indicateur,Source,"Sous-indicateurs"=Sous_indicateur,Statistique,

      "Zone géographique"=Zone_geographique,"Périodicité"=Periodicite,

      "Unité de compte"=Unite_de_compte,Correction)

    write.table(data,
                col.names=T,
                row.names=FALSE,
                sep=";",file)}

)




#### XLSX

output$telechargementDoubleExcel<- downloadHandler(
  filename = function() {
    paste0(DataInformations()$Indicateur, ".xlsx")
  },
  content = function(file){
    
    feuille<-DataInformations() %>% select( 
      
      "Unité de temps"=Unite_temps, Valeurs,
      
      "Indicateurs"=Indicateur,Source,"Sous-indicateurs"=Sous_indicateur,Statistique, 
                                            
                                            "Zone géographique"=Zone_geographique,"Périodicité"=Periodicite,
                                            
                                            "Unité de compte"=Unite_de_compte,Correction) 
    

    sheets <- mget(ls(pattern = "feuille")) 
    
    names(sheets) <- c("Informations") 
    
    writexl::write_xlsx(sheets, path = file) 
  }
  
) 


return(DataInformations)


}
















































