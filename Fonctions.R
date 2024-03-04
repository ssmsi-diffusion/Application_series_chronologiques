
# load("data/ListeDesIndicateurs.RData")


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



PageDeContenu<-function(bouton_retour_accueil,module,elements){
  
  div(class="row",style="padding:0px 20px;",
      
      div(class="col-md-4 col-lg-3",style="border:1px solid #ccc;min-height:600px !important;background-color:#f8f9f9;",
          
          div(class="col-md-12",bouton_retour_accueil,module)
          
      ),
      
      div(class="col-md-8 col-lg-9",
          
          div(class="col-md-12",elements)
          
      )
      
      
  )
  
}




Creation_charts<-function(donnees){
  
  hc<-highchart() %>%
    hc_chart(
      backgroundColor = "#FFFFFF",
      marginBottom = 120
    ) %>%
    hc_exporting(enabled = TRUE,sourceWidth=1300,sourceHeight=700,formAttributes = list(target = "_blank"),
                 buttons=list(
                   contextButton=list(
                     text= "Télécharger",
                     menuItems=telechargement_graphique,
                     symbol='',y=10))) %>%
    hc_yAxis(title = list(text=unique(donnees$Ordonnees)),
             style = list(color = "#000000", fontSize='15px',fontWeight = "bold",useHTML = TRUE),
             gridLineWidth=0.2,gridLineColor='black',
             labels=list(format = "{value:,.0f}")
    ) %>%
    hc_plotOptions(series = list(
      animation=FALSE,
      showInLegend = FALSE,
      dataLabels = list(enabled =FALSE,style=list(color="#000000")),marker=list(enabled=FALSE,lineWidth=1,fillColor='#1a2980',lineColor='#1a2980') )) %>%
    hc_tooltip(table = TRUE,
               sort = TRUE,
               pointFormat = paste0( '<br> <span style="color:{point.color}">\u25CF</span>',
                                     " {series.name}: {point.y} "),
               headerFormat = '<span style="font-size: 13px"> Date : {point.key}</span>'
    ) %>%
    hc_subtitle(
      text = str_c("Champ : ",unique(donnees$Champ),"<br/> Source :",unique(donnees$SourceGraphique),sep = " "),
      style = list(fontWeight = "bold"),
      align = "left",verticalAlign = 'bottom'
    ) %>%
    hc_legend(layout = 'vertical', align = 'center', verticalAlign = 'top', floating = T, x = 60, y =40)
  
  

  charts_list <- lapply(unique(donnees$Periodicite), function(modalite) {
    
    valeur <- donnees %>% filter(Periodicite == modalite) 
    
    corrections <- unique(valeur$Correction)
    
    
    ##### Pour les figures avec une droite pour indiquer la rupture ...
    
    chartLine <- hc %>% hc_add_series(
      type = "column",
      color="#1a2980",
      name=unique(valeur$Titre),
      data=as.numeric(valeur$Valeurs), 
      dataLabels = list(enabled = TRUE)
      
    ) %>% 
      
      hc_yAxis(labels = list(enabled = FALSE),title = list(text=unique(donnees$Ordonnees)),opposite = FALSE) 
    
    
    
    ##### Pour les figures avec une bande et du texte ...
    
    chartplotBands<-hc %>%
      hc_add_series(
        type = "line", 
        name= unique(valeur$Titre),
        data=as.numeric( valeur$Valeurs ), 
        color="#1a2980",            
        marker=list(enabled=FALSE),
        zoneAxis="x",
        zones=list(
          list(value=0,dashStyle='Solid'),
          list(value=7,dashStyle='Solid'),
          list(value =10,dashStyle='Dot'))
      ) %>% 
      hc_xAxis(categories=as.character.Date(valeur$Unite_temps),
               gridLineWidth=0.2,gridLineColor='black',tickmarkPlacement='on',tickInterval=1,
               plotBands = list(
                 list(
                   label = list(text = ""),
                   color = "#d6dbdf",
                   from =7,
                   to = 10
                 ))) 
    
    
    
    
    #################### Pour les séries double #########
    
    if (length(unique(valeur$Correction))==2) {
      
      valeurs_brut <- valeur %>% filter(Correction == corrections[1])
      
      valeurs_corrigees <- valeur %>% filter(Correction == corrections[2])
      
      
      if("0007" %in% donnees$Identifiant){
        
        chart<-hc %>%
          
          hc_add_series(name = unique(valeurs_brut$Correction),
                        
                        type = "line",
                        
                        showInLegend = TRUE,
                        
                        data = as.numeric(valeurs_brut$Valeurs) ) %>%
          
          hc_add_series(name= unique(valeurs_corrigees$Correction),
                        
                        type = "line",
                        
                        showInLegend = TRUE,
                        
                        data = as.numeric(valeurs_corrigees$Valeurs),
                        
                        dataLabels = list(enabled = TRUE),
                        
                        marker=list(enabled=TRUE,symbol='circle',lineWidth=1,fillColor='red',lineColor='red')) %>%
          
          hc_xAxis(categories=as.character.Date(valeur$Unite_temps),gridLineWidth=0.1,gridLineColor='black',tickmarkPlacement='on') %>%
          
          hc_title(text=unique(valeur$Titre),
                   margin = 20, align = "center",
                   style = list(color = "#000000", fontSize='15px',fontWeight = "normal",useHTML = TRUE)) 
        
        
        
      } else if ("0010" %in% donnees$Identifiant) {
        
        
        chart<-hc %>%
          
          hc_add_series(name=unique(valeurs_brut$Correction),
                        
                        type = "line",
                        
                        showInLegend = TRUE,
                        
                        data = as.numeric(valeurs_brut$Valeurs) ) %>%
          
          hc_add_series(name=unique(valeurs_corrigees$Correction),
                        
                        type = "line",
                        
                        showInLegend = TRUE,
                        
                        data = as.numeric(valeurs_corrigees$Valeurs),
                        
                        dataLabels = list(enabled = FALSE)) %>%
          
          hc_xAxis(categories=as.character.Date(valeur$Unite_temps),gridLineWidth=0.1,gridLineColor='black',tickmarkPlacement='on') %>%
          
          hc_title(text=unique(valeur$Titre),
                   margin = 20, align = "center",
                   style = list(color = "#000000", fontSize='15px',fontWeight = "normal",useHTML = TRUE)) 
        
        
        
      } else {
        
        chart <- hc %>%
          
          hc_add_series(data = as.numeric(valeurs_brut$Valeurs), type = "line", showInLegend = TRUE, name = unique(valeurs_brut$Correction) ) %>%
          
          hc_add_series(data = as.numeric(valeurs_corrigees$Valeurs), type = "line", showInLegend = TRUE, name = unique(valeurs_corrigees$Correction)) %>%
          
          hc_xAxis(categories=as.character.Date(valeur$Unite_temps),gridLineWidth=0.1,gridLineColor='black',tickmarkPlacement='on') %>%
          
          hc_title(text=unique(valeur$Titre),
                   margin = 20, align = "center",
                   style = list(color = "#000000", fontSize='15px',fontWeight = "normal",useHTML = TRUE)) 
        
        
      }
      
      
      
    } else {
      
      if("0001" %in% donnees$Identifiant){
        
        chart<-chartLine %>%
          
          hc_xAxis(
            categories=as.character.Date(valeur$Unite_temps),
            gridLineWidth=0.2,gridLineColor='black',tickmarkPlacement='on',tickInterval=1,
            plotLines = list(
              list(
                label = list(text = ""),
                color = "red",
                dashStyle='Dot',
                value=13,
                width=2))) %>%
          
          hc_title(text=unique(valeur$Titre),
                   margin = 20, align = "center",
                   style = list(color = "#000000", fontSize='15px',fontWeight = "normal",useHTML = TRUE)) 
        
      } else if ("0002" %in% donnees$Identifiant) {
        chart<-chartLine %>%
          hc_xAxis(
            categories=as.character.Date(valeur$Unite_temps),
            gridLineWidth=0.2,gridLineColor='black',tickmarkPlacement='on',tickInterval=1,
            plotLines = list(
              list(
                label = list(text = ""),
                color = "red",
                dashStyle='Dot',
                value=11,
                width=2))) %>%
          
          hc_title(text=unique(valeur$Titre),
                   margin = 20, align = "center",
                   style = list(color = "#000000", fontSize='15px',fontWeight = "normal",useHTML = TRUE)) 
        
      } else if ("0003" %in% donnees$Identifiant){ 
        
        chart<-chartLine %>%
          hc_xAxis(
            categories=as.character.Date(valeur$Unite_temps),
            gridLineWidth=0.2,gridLineColor='black',tickmarkPlacement='on',tickInterval=1,
            plotLines = list(
              list(
                label = list(text = ""),
                color = "red",
                dashStyle='Dot',
                value=9,
                width=2)))  %>%
          
          hc_title(text=unique(valeur$Titre),
                   margin = 20, align = "center",
                   style = list(color = "#000000", fontSize='15px',fontWeight = "normal",useHTML = TRUE)) 
        
      } else if("0004" %in% donnees$Identifiant){
        
        chart<-chartLine %>%
          hc_xAxis(
            categories=as.character.Date(valeur$Unite_temps),
            gridLineWidth=0.2,gridLineColor='black',tickmarkPlacement='on',tickInterval=1,
            plotLines = list(
              list(
                label = list(text = ""),
                color = "red",
                dashStyle='Dot',
                value=13,
                width=2))) %>%
          
          hc_title(text=unique(valeur$Titre),
                   margin = 20, align = "center",
                   style = list(color = "#000000", fontSize='15px',fontWeight = "normal",useHTML = TRUE)) 
        
      } else if ("0005" %in% donnees$Identifiant){
        
        chart<-chartplotBands %>%
          hc_annotations(
            list(
              labelOptions=list(
                backgroundColor='rgba(255,255,255,0.5)'),
              labels =  
                list(
                  list(
                    point = list(x=8,y=200000,xAxis=0,yAxis=0), 
                    text = "Réformulation <br> des questions relatives aux <br> violences sexuelles"
                  ),
                  
                  list(
                    point = list(x=9,y=276000,xAxis =0, yAxis =0),
                    text = "Première enquête <br> post-affaire <br> weinstein"
                  )))) %>%
          
          hc_title(text=unique(valeur$Titre),
                   margin = 20, align = "center",
                   style = list(color = "#000000", fontSize='15px',fontWeight = "normal",useHTML = TRUE)) 
        
      } else if ("0006" %in% donnees$Identifiant){
        
        chart<-chartplotBands %>%
          hc_annotations(
            list(
              labelOptions=list(
                backgroundColor='rgba(255,255,255,0.5)'),
              labels =  
                list(
                  list(
                    point = list(x=8,y=285000,xAxis=0,yAxis=0),
                    text = "Réformulation <br> des questions relatives aux <br> violences sexuelles"
                  ),
                  
                  list(
                    point = list(x=9,y=408000,xAxis=0,yAxis =0),
                    text = "Première enquête <br> post-affaire <br> weinstein"
                  )))) %>%
          
          hc_title(text=unique(valeur$Titre),
                   margin = 20, align = "center",
                   style = list(color = "#000000", fontSize='15px',fontWeight = "normal",useHTML = TRUE))
        
      }  else if ("0008" %in% donnees$Identifiant) {
        
        chart<-hc %>%
          
          hc_add_series(name = unique(valeur$Titre),
                        
                        type = "line",
                        
                        showInLegend = TRUE,
                        
                        data = as.numeric(valeur$Valeurs) ) %>%
          
          hc_xAxis(categories=as.character.Date(valeur$Unite_temps),gridLineWidth=0.2,gridLineColor='black',tickmarkPlacement='on') %>%
          
          hc_yAxis(title = list(text=unique(valeur$Ordonnees)),  
                   
                   style = list(color = "#000000", fontSize='15px',fontWeight = "bold",useHTML = TRUE),
                   
                   gridLineWidth=0.2,gridLineColor='black',
                   
                   labels=list(format = "{value:,.1f}")
          ) %>%
          
          hc_title(text=unique(valeur$Titre),
                   margin = 20, align = "center",
                   style = list(color = "#000000", fontSize='15px',fontWeight = "normal",useHTML = TRUE)) 
        
        ### Modification effectuée le 27/12/2023, Ajout type="column' pour les séries ChartLine ...
        
      } else if ("0009" %in% donnees$Identifiant) {
        
        index_2019<-which(valeur$Unite_temps == "2019")
        
        
        if (c("Actes de vandalisme contre le logement") %in% valeur$Sous_indicateur){ 
          
          chart <- hc %>% 
            
            hc_add_series(name=unique(valeur$Titre),
                          
                          type = "column",
                          
                          color="#1a2980", 
                          
                          showInLegend = FALSE,
                          
                          data = as.numeric(valeur$Valeurs),dataLabels = list(enabled = TRUE)) %>%
            
            hc_title(text=unique(valeur$Titre),
                     margin = 20, align = "center",
                     style = list(color = "#000000", fontSize='15px',fontWeight = "normal",useHTML = TRUE)) %>%
            
            hc_yAxis(labels = list(enabled = FALSE),title = list(text=unique(donnees$Ordonnees)),opposite = FALSE) %>%
            
            hc_xAxis(categories=as.character.Date(valeur$Unite_temps),
                     
                     gridLineWidth=0.2,gridLineColor='black',tickmarkPlacement='on',tickInterval=1)
          
          
        } else {
          
          chart <- chartLine %>%
            
            hc_title(text=unique(valeur$Titre),
                     margin = 20, align = "center",
                     style = list(color = "#000000", fontSize='15px',fontWeight = "normal",useHTML = TRUE)) %>%
            
            
            hc_xAxis(categories=as.character.Date(valeur$Unite_temps),
                     
                     gridLineWidth=0.2,gridLineColor='black',tickmarkPlacement='on',tickInterval=1,
                     
                     plotLines = list(
                       
                       list(
                         label = list(text = ""),
                         
                         color = "red",
                         
                         dashStyle='Dot',
                         
                         value=index_2019-1,
                         
                         width=2)))
          
        }
        
        
        ########### Fin de la Modification du 27/12/2023 ##################
        
        
      } else {
        
        chart <- hc %>%
          
          hc_chart(type = "line") %>%
          
          hc_add_series(data = as.numeric(valeur$Valeurs), name = unique(valeur$Titre)) %>%
          
          hc_xAxis(categories=as.character.Date(valeur$Unite_temps),gridLineWidth=0.1,gridLineColor='black',tickmarkPlacement='on') %>%
          
          hc_title(text=unique(valeur$Titre),
                   margin = 20, align = "center",
                   style = list(color = "#000000", fontSize='15px',fontWeight = "normal",useHTML = TRUE)) 
        
      }
      
      
    }
    
    return(chart)
  })
  
  return(charts_list)
  
}


# Fonction qui prend en entrée la table de données générée par le module et qui 

# va retourner le texte descriptif associé à chaque figure 

creation_texte_descriptif<-function(data_module){
  
  if(length(unique(data_module$Description))==2){  
    
    texte<-tags$div(class="texte-descritif",
                    
                    HTML(
                      
                      paste0("<p> <h4> Figure 1 : </h4></p>",as.character(unique(data_module$Description)[1])),'<br><br>',
                      
                      paste0("<p> <h4> Figure 2 : </h4></p>",as.character(unique(data_module$Description)[2]))
                      
                    ) )
    
  } else {
    
    texte<-tags$div(class="texte-descritif",HTML(paste0("<p> <h4> Sources et définitions : </h4> </p> ",as.character(unique(data_module$Description)))))
    
  }
  
  return(texte)
  
}






telechargement_graphique <- list(
  
  list(
    text = "PNG",
    onclick = JS("function () {
                   this.exportChart({ type: 'image/png' });}")
  ),
  list(
    text = "JPEG",
    onclick = JS("function () {
                   this.exportChart({ type: 'image/jpeg' }); }")
  ),
  
  list(
    text = "PDF",
    onclick = JS("function () {
                   this.exportChart({ type: 'application/pdf'}); }")
  )
  

)

























