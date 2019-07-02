shiny_ronds <-
  function(data,fondMaille,fondMailleElargi=NULL,fondContour,fondSuppl=NULL,idData,varVolume,dom="0",fondChx=NULL)
  {
    options("stringsAsFactors"=FALSE)
    
    # Verification des parametres
    msg_error1<-msg_error2<-msg_error3<-msg_error4<-msg_error5<-msg_error6<-msg_error7<-msg_error8<-msg_error9<-msg_error10<-msg_error11<-msg_error12<-msg_error13<-msg_error14<-msg_error15<-msg_error16<-msg_error17 <- NULL
    
    if(any(class(data)!="data.frame")) msg_error1 <- "Les donnees doivent etre dans un data.frame / "
    if(any(!any(class(fondMaille) %in% "sf"),!any(class(fondMaille) %in% "data.frame"))) msg_error2 <- "Le fond de maille doit etre un objet sf / "
    if(!is.null(fondMailleElargi)) if(any(!any(class(fondMailleElargi) %in% "sf"),!any(class(fondMailleElargi) %in% "data.frame"))) msg_error3 <- "Le fond de maille elargie doit etre un objet sf / "
    if(any(!any(class(fondContour) %in% "sf"),!any(class(fondContour) %in% "data.frame"))) msg_error4 <- "Le fond de contour doit etre un objet sf / "
    if(!is.null(fondSuppl)) if(any(!any(class(fondSuppl) %in% "sf"),!any(class(fondSuppl) %in% "data.frame"))) msg_error5 <- "Le fond supplementaire doit etre un objet sf / "
    if(any(class(idData)!="character")) msg_error6 <- "Le nom de la variable doit etre de type caractere / "
    if(any(class(varVolume)!="character")) msg_error7 <- "Le nom de la variable doit etre de type caractere / "
    if(any(class(dom)!="character")) msg_error8 <- "La valeur doit etre de type caractere ('0', '971', '972', '973', '974' ou '976') / "
    if(!is.null(fondChx)) if(any(!any(class(fondChx) %in% "sf"),!any(class(fondChx) %in% "data.frame"))) msg_error9 <- "Le fond des chx doit etre un objet sf / "
    
    if(length(names(data))<2) msg_error10 <- "Le tableau des donnees n'est pas conforme. Il doit contenir au minimum une variable identifiant et la variable a representer / "
    if(length(names(fondMaille))<3) msg_error11 <- "Le fond de maille n'est pas conforme. La table doit contenir au minimum une variable identifiant, une variable libelle et la geometry / "
    if(!is.null(fondMailleElargi)) if(length(names(fondMailleElargi))<3) msg_error12 <- "Le fond de maille elargie n'est pas conforme. La table doit contenir au minimum une variable identifiant, une variable libelle et la geometry / "
    if(length(names(fondContour))<3) msg_error13 <- "Le fond de contour n'est pas conforme. La table doit contenir au minimum une variable identifiant, une variable libelle et la geometry / "
    if(!is.null(fondSuppl)) if(length(names(fondSuppl))<3) msg_error14 <- "Le fond supplementaire n'est pas conforme. La table doit contenir au minimum une variable identifiant, une variable libelle et la geometry / "
    
    if(!any(names(data) %in% idData))  msg_error15 <- "La variable identifiant les donnees n'existe pas dans la table des donnees / "
    if(!any(names(data) %in% varVolume))  msg_error16 <- "La variable a representer n'existe pas dans la table des donnees / "
    if(!dom %in% c("0","971","972","973","974","976")) msg_error17 <- "La variable dom doit etre '0', '971', '972', '973', '974' ou '976' / "
    
    if(any(!is.null(msg_error1),!is.null(msg_error2),!is.null(msg_error3),!is.null(msg_error4),
           !is.null(msg_error5),!is.null(msg_error6),!is.null(msg_error7),!is.null(msg_error8),
           !is.null(msg_error9),!is.null(msg_error10),!is.null(msg_error11),!is.null(msg_error12),
           !is.null(msg_error13),!is.null(msg_error14),!is.null(msg_error15),!is.null(msg_error16),!is.null(msg_error17)))
    {
      stop(simpleError(paste0(msg_error1,msg_error2,msg_error3,msg_error4,msg_error5,msg_error6,msg_error7,msg_error8,
                              msg_error9,msg_error10,msg_error11,msg_error12,msg_error13,msg_error14,msg_error15,msg_error16,msg_error17)))
    }
    
    nb_up <- reactiveValues(a=0)
    nb_down <- reactiveValues(a=0)
    ordre_analyse <- reactiveValues(a=1,b=2)
    insert_save <- reactiveValues(a=0)
    remove_carte <- reactiveValues(a=0)
    liste_fonds <- reactiveValues(a=c("analyse","maille","contour"))
    m_save_rp <- reactiveValues(a=0)
    
    erreur_maille <- reactiveValues(a=FALSE)
    
    sourc <- "Source : Insee"
    
    names(data)[names(data)==idData] <- "CODE"
    names(fondMaille)[1] <- "CODE"
    names(fondMaille)[2] <- "LIBELLE"
    names(fondContour)[1] <- "CODE"
    names(fondContour)[2] <- "LIBELLE"
    if(!is.null(fondMailleElargi)) 
    {
      names(fondMailleElargi)[1] <- "CODE"
      names(fondMailleElargi)[2] <- "LIBELLE"
      fondMailleElargi$LIBELLE<-iconv(fondMailleElargi$LIBELLE,"latin1","utf8")
    }
    if(!is.null(fondSuppl)) 
    {
      names(fondSuppl)[1] <- "CODE"
      names(fondSuppl)[2] <- "LIBELLE"
      fondSuppl$LIBELLE<-iconv(fondSuppl$LIBELLE,"latin1","utf8")
    }
    
    fondMaille$LIBELLE<-iconv(fondMaille$LIBELLE,"latin1","utf8")
    fondContour$LIBELLE<-iconv(fondContour$LIBELLE,"latin1","utf8")
    
    ui <- navbarPage("OCEANIS", id="menu",
                     
                     theme = shinytheme("superhero"),
                     
                     tabPanel("Carte",value="carte",
                              sidebarLayout(
                                
                                sidebarPanel(width = 3,
                                             style = "overflow-y:scroll; min-height: 840px; max-height: 840px",
                                             h4(HTML("<b><font color=#95BAE2>VARIABLES</font></b>")),
                                             uiOutput("variable_rond_rp"),
                                             tags$hr(style="border: 5px solid #5182B6"), #337ab7
                                             h4(HTML("<b><font color=#95BAE2>FONDS</font></b>")),
                                             fluidRow(
                                               column(width=9, offset=0.5,
                                                      uiOutput("ordre_fonds_rp")
                                               ),
                                               column(width=1,
                                                      br(),
                                                      br(),
                                                      htmlOutput("monter_fond_rp", inline=FALSE),
                                                      htmlOutput("descendre_fond_rp", inline=FALSE)
                                               )
                                             ),
                                             uiOutput("elargi_rp"),
                                             conditionalPanel(condition = 'input.elargi_rp_id',
                                                              uiOutput("opacite_elargi_rp")
                                             ),
                                             uiOutput("ajout_territoire_rp"),
                                             uiOutput("ajout_reg_rp"),
                                             uiOutput("ajout_dep_rp"),
                                             tags$hr(style="border: 5px solid #5182B6"),
                                             h4(HTML("<b><font color=#95BAE2>RONDS</font></b>")),
                                             uiOutput("taille_rond_rp"),
                                             htmlOutput("info_taille_max_rond_rp"),
                                             htmlOutput("info_rapport_rond_rp"),
                                             uiOutput("rapport_rond_rp"),
                                             conditionalPanel(condition = 'input.rapport_rond_rp_id',
                                                              uiOutput("valeur_rapport_rond_rp"),
                                                              htmlOutput("info_rapport_max_rond_rp")
                                             ),
                                             uiOutput("choix_centroid_rp"),
                                             tags$hr(style="border: 5px solid #5182B6"),
                                             h4(HTML("<b><font color=#95BAE2>L\u00c9GENDE</font></b>")),
                                             uiOutput("titre_ronds_legende_rp"),
                                             br(),
                                             uiOutput("affiche_legende_rp"),
                                             br(),
                                             tags$hr(style="border: 5px solid #5182B6"),
                                             h4(HTML("<b><font color=#95BAE2>SAUVEGARDE</font></b>")),
                                             uiOutput("save_carte_rp"),
                                             br(),
                                             conditionalPanel(condition = 'input.mymap_rp_click',
                                                              tags$div(class="dropup",
                                                                       HTML('<button class="btn btn-primary dropdown-toggle" type="button" data-toggle="dropdown" aria-haspopup="true" aria-expanded="false">
                                                                            Exporter en projet Qgis
                                                                            <span class="caret"></span>
                                                                            </button>'),
                                                                       tags$ul(class="dropdown-menu",
                                                                               wellPanel(
                                                                                 style="background: #2B3E50; width:340px",
                                                                                 h4("Export de la carte en projet Qgis"),
                                                                                 br(),
                                                                                 uiOutput("sortie_qgis_rp"),
                                                                                 br(),
                                                                                 uiOutput("titre1_qgis_rp"),
                                                                                 uiOutput("titre2_qgis_rp"),
                                                                                 uiOutput("source_qgis_rp"),
                                                                                 tags$head(tags$style(HTML('#export_qgis_rp_id{background-color:#337ab7}'))),
                                                                                 uiOutput("export_qgis_rp")
                                                                               )
                                                                       )
                                                                       )
                                                              
                                             ),
                                             br(),
                                             uiOutput("aide_image_rp"),
                                             br()
                              ),
                              mainPanel(
                                tags$head(
                                  tags$style(HTML(".leaflet-container { background: #AFC9E0; }"))
                                ),
                                tabsetPanel(id="onglets_rp",
                                            tabPanel(title=HTML("<b>Carte</b>"),value="carte",
                                                     leafletOutput("mymap_rp",width="100%",height = 800)
                                            ),
                                            tabPanel(title=HTML(paste0("<b>Donn","\u00e9","es</b>")),value="donnees",
                                                     h5("S\u00e9lectionnez une ou plusieurs lignes pour ensuite les visualiser sur la carte."),
                                                     DT::dataTableOutput("mydonnees_rp",width="100%",height = 800)),
                                            tabPanel(title=HTML("<b>Maille</b>"),value="maille",
                                                     h5("S\u00e9lectionnez une ou plusieurs lignes pour ensuite les visualiser sur la carte."),
                                                     DT::dataTableOutput("mymaille_rp",width="100%",height = 800)),
                                            tabPanel(title=HTML("<b>Contour</b>"),value="contour",
                                                     h5("S\u00e9lectionnez une ou plusieurs lignes pour ensuite les visualiser sur la carte."),
                                                     DT::dataTableOutput("mycontour_rp",width="100%",height = 800))
                                )
                              )
    )
    )
    )
    
    server <- function(input, output, session) {
      
      #################
      #Onglet Carte
      #################
      
      #Charge les donnees et fonds en memoire et affiche les widgets adequats dans le sidePanel de l'onglet "Carte"
      observe({
        
        # VARIABLES
        
        output$variable_rond_rp <- renderUI({
          selectInput("variable_rond_rp_id", label=h5("Variable des ronds (en volume)"), choices = varVolume, selected = varVolume)
        })
        
        # FONDS
        
        output$ordre_fonds_rp <- renderUI({
          selectInput("ordre_fonds_rp_id", label=h5("Modifier l'ordre des fonds"), choices = liste_fonds$a, multiple=TRUE, selectize=FALSE, selected = NULL)
        })
        output$monter_fond_rp <- renderUI({
          actionButton("monter_fond_rp_id", label="", icon=icon("arrow-up"))
        })
        output$descendre_fond_rp <- renderUI({
          actionButton("descendre_fond_rp_id", label="", icon=icon("arrow-down"))
        })
        
        if(!is.null(fondMailleElargi))
        {
          output$elargi_rp <- renderUI({
            checkboxInput("elargi_rp_id", label = HTML("Afficher une repr\u00e9sentation \u00e9largie de l'analyse<br>(parfois long)"),
                          value = if(is.null(fondMailleElargi)) FALSE else TRUE)
          })
          output$opacite_elargi_rp <- renderUI({
            sliderInput("opacite_elargi_rp_id", label = h5("Opacit\u00e9 de l'analyse \u00e9largie"), value=60, min=0, max=100, step=5, ticks=FALSE)
          })
        }
        
        output$ajout_territoire_rp <- renderUI({
          checkboxInput("ajout_territoire_rp_id", label = "Afficher le fond des territoires",
                        value = if(is.null(fondSuppl)) FALSE else TRUE)
        })
        output$ajout_reg_rp <- renderUI({
          checkboxInput("ajout_reg_rp_id", label = "Afficher le fond des r\u00e9gions",
                        value = FALSE)
        })
        output$ajout_dep_rp <- renderUI({
          checkboxInput("ajout_dep_rp_id", label = "Afficher le fond des d\u00e9partements",
                        value = FALSE)
        })
        
        # RONDS
        
        output$taille_rond_rp <- renderUI({
          numericInput("taille_rond_rp_id", label = h5("Rayon du rond le plus grand (en m\u00e8tres)"), value=round(as.numeric(calcul_max_rayon_metres_rp()[[1]])/1.25,0), min=0, max=round(as.numeric(calcul_max_rayon_metres_rp()[[1]]),0), step=1000)
        })
        
        output$info_taille_max_rond_rp <- renderText({
          HTML(paste0("<font size=2 color=white>Valeur max du rayon le plus grand = ", round(as.numeric(calcul_max_rayon_metres_rp()[[1]]),0)," m</font>"))
        })
        
        output$info_rapport_rond_rp <- renderText({
          HTML(paste0("<font size=2 color=white>Rapport Surface rond / Volume = ", (pi*(as.numeric(calcul_max_rayon_metres_rp()[[1]])/1.25)^2)/as.numeric(calcul_max_rayon_metres_rp()[[2]]),"</font>"))
        })
        
        output$rapport_rond_rp <- renderUI({
          checkboxInput("rapport_rond_rp_id", label = "Modifier la valeur du rapport (permet la comparaison entre cartes)", value=FALSE)
        })
        
        output$valeur_rapport_rond_rp <- renderUI({
          numericInput("valeur_rapport_rond_rp_id", label = h5("Nouvelle valeur du rapport Surface rond / Volume"), value=(pi*(as.numeric(calcul_max_rayon_metres_rp()[[1]])/1.25)^2)/as.numeric(calcul_max_rayon_metres_rp()[[2]]), min=0.1, max=(pi*(as.numeric(calcul_max_rayon_metres_rp()[[1]]))^2)/as.numeric(calcul_max_rayon_metres_rp()[[2]]), step=0.1)
        })
        
        output$info_rapport_max_rond_rp <- renderText({
          HTML(paste0("<font size=2 color=white>Valeur max du rapport = ", (pi*(as.numeric(calcul_max_rayon_metres_rp()[[1]]))^2)/as.numeric(calcul_max_rayon_metres_rp()[[2]]),"</font>"))
        })
        
        if(!is.null(fondChx))
        {
          output$choix_centroid_rp <- renderUI({
            radioButtons("choix_centroid_rp_id", label = h5("Les ronds sont centres sur"), choices=c("les centroides des communes"="centroid","les chx des communes"="chx"), selected = if(!is.null(fondChx)) "chx" else "centroid")
          })
        }else
        {
          output$choix_centroid_rp <- renderUI({
          })
        }
        
        # LEGENDE
        
        output$titre_ronds_legende_rp <- renderUI({
          textInput("titre_ronds_legende_rp_id", label = h5("Titre de la l\u00e9gende des ronds"), value = "")
        })
        
        output$affiche_legende_rp <- renderUI({
          checkboxInput("affiche_legende_rp_id", label = "Activer le d\u00e9placement de la l\u00e9gende au clic",
                        value = TRUE)
        })
        
        # SAUVEGARDE
        
        output$save_carte_rp <- renderUI({
          actionButton("save_carte_rp_id", label=HTML("<font size=3>Sauvegarder la carte dans un onglet</font>"), style="color:#FFFFFF; background-color:#DF691A")
        })
        
        output$entrees_qgis_rp <- renderUI({
          actionButton("entrees_qgis_rp_id", label="Exporter en projet Qgis")
        })
        
        output$sortie_qgis_rp <- renderUI({
          tags$div(class="input-group",
                   HTML('<input type="text" id="sortie_qgis_rp_id" class="form-control" placeholder="Nom du projet" aria-describedby="sortie_qgis_rp_id">
                        <span class="input-group-addon" id="sortie_qgis_rp_id">.qgs</span>'))
        })
        
        output$titre1_qgis_rp <- renderUI({
          textInput("titre1_qgis_rp_id", label = h5("Titre informatif"), value = "", placeholder= "Facultatif")
        })
        
        output$titre2_qgis_rp <- renderUI({
          textInput("titre2_qgis_rp_id", label = h5("Titre descriptif"), value = "", placeholder= "Facultatif")
        })
        
        output$source_qgis_rp <- renderUI({
          textInput("source_qgis_rp_id", label = h5("Source de la carte"), value = sourc)
        })
        
        output$aide_image_rp <- renderUI({
          tags$div(class="dropup",
                   HTML(paste0('<button class="btn btn-primary dropdown-toggle" type="button" data-toggle="dropdown">
                               <i class="fa fa-book fa-fw" aria-hidden="true"></i>
                               Proc','\u00e9','dure pour capture d\'','\u00e9','cran
                               <span class="caret"></span>
                               </button>')),
                   tags$ul(class="dropdown-menu",
                           wellPanel(
                             style="background: #2B3E50; width:340px",
                             div(
                               HTML("<font size=2>Deux possibilit\u00e9s :</font>"),
                               br(),
                               br(),
                               strong(HTML("<font size=3>Par l'Outil Capture</font>")),
                               br(),
                               HTML("<font size=2>1- Ouvrir le logiciel Outil Capture sur le Poste de travail (Menu D\u00e9marrer > Tous les programmes > Accessoires > Outil Capture).</font>"),
                               br(),
                               HTML(paste0("<font size=2>2- S\u00e9lectionner la zone \u00e0 capturer (vous aurez besoin d'adapter la fen\u00eatre AUS pour avoir \u00e0 la fois la carte et la barre des t","\u00e2","ches).</font>")),
                               br(),
                               HTML("<font size=2>3- Enregistrer l'image ou copier la dans le presse-papier.</font>"),
                               br(),
                               br(),
                               strong(HTML(paste0("<font size=3>Par impression d'","\u00e9","cran</font>"))),
                               br(),
                               HTML("<font size=2>1- Appuyer sur la touche clavier \"Impr ecran\".</font>"),
                               br(),
                               HTML("<font size=2>2- Ouvrir un logiciel de retouche image (Paint par exemple).</font>"),
                               br(),
                               HTML("<font size=2>3- Coller l'image et l'enregistrer au format voulu (.jpg, .png, .bmp).</font>")
                             )
                           )
                   )
                   )
        })
        })
      
      # Pour modifier l'ordre des fonds
      observeEvent(list(input$monter_fond_rp_id,input$descendre_fond_rp_id),{
        
        ordre <- c()
        if(as.numeric(input$monter_fond_rp_id)>nb_up$a)
        {
          ordre <- c(2,3)
          nb_up$a <- nb_up$a+1
        }
        if(as.numeric(input$descendre_fond_rp_id)>nb_down$a)
        {
          ordre <- c(1,2)
          nb_down$a <- nb_down$a+1
        }
        
        if(is.null(input$ordre_fonds_rp_id)) pos_select <- 0 else pos_select <- which(liste_fonds$a==input$ordre_fonds_rp_id)
        
        if(pos_select>0)
        {
          if(pos_select==ordre[1]) liste_fonds$a <- liste_fonds$a[c(2,1,3)]
          if(pos_select==ordre[2]) liste_fonds$a <- liste_fonds$a[c(1,3,2)]
          
          updateSelectInput(session, "ordre_fonds_rp_id",
                            choices = liste_fonds$a,
                            selected = input$ordre_fonds_rp_id
          )
        }
      },ignoreInit = TRUE)
      
      # Pour la semio, on calcul le rayon maximal d'un rond de facon a ce que la somme des superficies des ronds ne depasse pas 1/7eme de la superficie du territoire.
      calcul_max_rayon_metres_rp <- reactive({
        #Aire totale du territoire d'etude
        aire_territoire <- as.numeric(sum(st_area(fondMaille[fondMaille$CODE %in% data[,"CODE"],]))) #Superficie du territoire
        #valeur max de la serie de donnees
        suppressWarnings(max_var <- max(data[data[,"CODE"] %in% fondMaille$CODE,varVolume], na.rm = TRUE))
        
        serie <- data[data[,"CODE"] %in% fondMaille$CODE,varVolume]
        serie <- serie[!is.na(serie)]
        #on ramene la serie a un quotient fonction de la valeur max
        quotient <- serie/max_var
        #somme des carres
        somme_quotient <- sum(quotient^2)
        #calcul de la surface max du rond le plus grand
        max_surface_rond <- (aire_territoire/(7*somme_quotient))
        #calcul du rayon max du rond le plus grand
        max_rayon_metres <- sqrt(max_surface_rond/pi)
        
        return(list(max_rayon_metres,max_var))
      })
      
      rayon_rp <- reactive({
        req(input$valeur_rapport_rond_rp_id)
        Sys.sleep(3)
        val <- round(sqrt((input$valeur_rapport_rond_rp_id*isolate(calcul_max_rayon_metres_rp())[[2]])/pi),0)
        return(val)
      })
      
      rayon_react_rp <- rayon_rp %>% debounce(1000)
      
      observeEvent(rayon_react_rp(),{
        req(rayon_react_rp())
        
        if(length(rayon_react_rp())==0) return(NULL)
        if(rayon_react_rp()==0 | is.na(rayon_react_rp())) return(NULL)
        
        isolate(updateNumericInput(session,"taille_rond_rp_id", value=rayon_react_rp()))
        
        isolate(output$info_rapport_rond_rp <- renderText({
          HTML(paste0("<font size=2 color=white>Rapport Surface rond / Volume = ", (pi*(rayon_react_rp())^2)/isolate(calcul_max_rayon_metres_rp())[[2]],"</font>"))
        }))
      })
      
      rapport_rp <- reactive({
        req(input$taille_rond_rp_id)
        
        val <- (pi*(input$taille_rond_rp_id)^2)/isolate(calcul_max_rayon_metres_rp())[[2]]
        max <- (pi*(isolate(calcul_max_rayon_metres_rp())[[1]])^2)/isolate(calcul_max_rayon_metres_rp())[[2]]
        
        return(list(val=val,max=max))
      })
      
      rapport_react_rp <- rapport_rp %>% debounce(1000)
      
      observeEvent(rapport_react_rp(),{
        req(rapport_react_rp())
        
        if(length(rapport_react_rp()$val)==0) return(NULL)
        if(rapport_react_rp()$val==0 | is.na(rapport_react_rp()$val)) return(NULL)
        
        isolate(updateNumericInput(session,"valeur_rapport_rond_rp_id", value=rapport_react_rp()$val))
        
        isolate(output$info_rapport_rond_rp <- renderText({
          HTML(paste0("<font size=2 color=white>Rapport Surface rond / Volume = ", rapport_react_rp()$val,"</font>"))
        }))
      })
      
      choix_centroid_rp <- reactive({
        if(is.null(input$choix_centroid_rp_id))
        {
          centroid <- "centroid"
        }else
        {
          centroid <- input$choix_centroid_rp_id
        }
        return(centroid)
      })
      
      # Pour exporter la carte en projet Qgis
      
      output$export_qgis_rp <- renderUI({
        downloadButton("downloadProjetQgis_rp", label="Exporter")
      })
      
      output$downloadProjetQgis_rp <- downloadHandler(contentType = "zip",
                                                      filename = function(){
                                                        paste0(input$sortie_qgis_rp_id,".zip")
                                                      },
                                                      content = function(file){
                                                        files <- EXPORT_PROJET_QGIS_RP(file)
                                                        
                                                        zip(file,files, flags = "-j9X")
                                                      }
      )
      
      EXPORT_PROJET_QGIS_RP <- function(file)
      {
        showModal(modalDialog(HTML("<i class=\"fa fa-spinner fa-spin fa-2x fa-fw\"></i> <font size=+1>Export du projet Qgis en cours...</font> "), size="m", footer=NULL, style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"))
        
        sortie <- input$sortie_qgis_rp_id
        rep_sortie <- dirname(file)
        files <- c(paste0(rep_sortie,"/",sortie,".qgs"))
        
        rupture <- FALSE
        rupture_elargi <- FALSE
        
        donnees <- analyse_rp()[[1]]$donnees$save
        if(any(donnees<0) & any(donnees>0)) rupture <- TRUE
        
        if(elargi_rp())
        {
          donnees_elargi <- analyse_rp()[[1]]$donnees_elargi$save
          if(any(donnees_elargi<0) & any(donnees_elargi>0)) rupture_elargi <- TRUE
          
          fond_maille_elargi <- st_transform(fond_elargi_rp()[[2]],paste0("+init=epsg:",code_epsg_rp()))
          
          if(rupture_elargi)
          {
            ronds_carte_elargi <- analyse_ronds_sf_rp()[[2]][data.frame(analyse_ronds_sf_rp()[[2]])$save>0,]
            ronds_carte_elargi <- st_transform(ronds_carte_elargi,paste0("+init=epsg:",code_epsg_rp()))
            ronds_carte_rupt_elargi <- analyse_ronds_sf_rp()[[2]][data.frame(analyse_ronds_sf_rp()[[2]])$save<0,]
            ronds_carte_rupt_elargi <- st_transform(ronds_carte_rupt_elargi,paste0("+init=epsg:",code_epsg_rp()))
            
            st_write(ronds_carte_elargi, paste0(rep_sortie,"/",sortie,"_elargi_ronds_carte.shp"), delete_dsn = TRUE, quiet = TRUE)
            st_write(ronds_carte_rupt_elargi, paste0(rep_sortie,"/",sortie,"_elargi_ronds_rupt_carte.shp"), delete_dsn = TRUE, quiet = TRUE)
            st_write(fond_maille_elargi, paste0(rep_sortie,"/fond_maille_elargi.shp"), delete_dsn = TRUE, quiet = TRUE)
            files <- c(paste0(rep_sortie,"/",sortie,"_elargi_ronds_carte.shp"),paste0(rep_sortie,"/",sortie,"_elargi_ronds_carte.dbf"),paste0(rep_sortie,"/",sortie,"_elargi_ronds_carte.prj"),paste0(rep_sortie,"/",sortie,"_elargi_ronds_carte.shx"),files)
            files <- c(paste0(rep_sortie,"/",sortie,"_elargi_ronds_rupt_carte.shp"),paste0(rep_sortie,"/",sortie,"_elargi_ronds_rupt_carte.dbf"),paste0(rep_sortie,"/",sortie,"_elargi_ronds_rupt_carte.prj"),paste0(rep_sortie,"/",sortie,"_elargi_ronds_rupt_carte.shx"),files)
            files <- c(paste0(rep_sortie,"/fond_maille_elargi.shp"),paste0(rep_sortie,"/fond_maille_elargi.dbf"),paste0(rep_sortie,"/fond_maille_elargi.prj"),paste0(rep_sortie,"/fond_maille_elargi.shx"),files)
          }else
          {
            ronds_carte_elargi <- analyse_ronds_sf_rp()[[2]]
            ronds_carte_elargi <- st_transform(ronds_carte_elargi,paste0("+init=epsg:",code_epsg_rp()))
            
            st_write(ronds_carte_elargi, paste0(rep_sortie,"/",sortie,"_elargi_ronds_carte.shp"), delete_dsn = TRUE, quiet = TRUE)
            st_write(fond_maille_elargi, paste0(rep_sortie,"/fond_maille_elargi.shp"), delete_dsn = TRUE, quiet = TRUE)
            files <- c(paste0(rep_sortie,"/",sortie,"_elargi_ronds_carte.shp"),paste0(rep_sortie,"/",sortie,"_elargi_ronds_carte.dbf"),paste0(rep_sortie,"/",sortie,"_elargi_ronds_carte.prj"),paste0(rep_sortie,"/",sortie,"_elargi_ronds_carte.shx"),files)
            files <- c(paste0(rep_sortie,"/fond_maille_elargi.shp"),paste0(rep_sortie,"/fond_maille_elargi.dbf"),paste0(rep_sortie,"/fond_maille_elargi.prj"),paste0(rep_sortie,"/fond_maille_elargi.shx"),files)
          }
        }
        
        if(rupture)
        {
          ronds_carte <- analyse_ronds_sf_rp()[[1]][data.frame(analyse_ronds_sf_rp()[[1]])$save>0,]
          ronds_carte_rupt <- analyse_ronds_sf_rp()[[1]][data.frame(analyse_ronds_sf_rp()[[1]])$save<0,]
          
          ronds_carte <- st_transform(ronds_carte,paste0("+init=epsg:",code_epsg_rp()))
          ronds_carte_rupt <- st_transform(ronds_carte_rupt,paste0("+init=epsg:",code_epsg_rp()))
          
          suppressWarnings(write_ronds_carte <- try(st_write(ronds_carte, paste0(rep_sortie,"/",sortie,"_ronds_carte.shp"), delete_dsn = TRUE, quiet = TRUE),silent=TRUE))
          suppressWarnings(write_ronds_carte_rupt <- try(st_write(ronds_carte_rupt, paste0(rep_sortie,"/",sortie,"_ronds_rupt_carte.shp"), delete_dsn = TRUE, quiet = TRUE),silent=TRUE))
        }else
        {
          ronds_carte <- analyse_ronds_sf_rp()[[1]]
          ronds_carte <- st_transform(ronds_carte,paste0("+init=epsg:",code_epsg_rp()))
          suppressWarnings(write_ronds_carte <- try(st_write(ronds_carte, paste0(rep_sortie,"/",sortie,"_ronds_carte.shp"), delete_dsn = TRUE, quiet = TRUE),silent=TRUE))
        }
        
        fond_ronds <- construction_legende_rp()[[1]][[2]]
        fond_lignes <- construction_legende_rp()[[2]][[7]]
        
        fond_maille <- st_transform(fondMaille,paste0("+init=epsg:",code_epsg_rp()))
        fond_contour <- st_transform(fondContour,paste0("+init=epsg:",code_epsg_rp()))
        if(!is.null(fondSuppl) && input$ajout_territoire_rp_id) fond_territoire <- st_transform(fond_territoire_rp(),paste0("+init=epsg:",code_epsg_rp()))
        if(input$ajout_dep_rp_id) fond_departement <- st_transform(fond_departement_rp(),paste0("+init=epsg:",code_epsg_rp()))
        if(input$ajout_reg_rp_id) fond_region <- st_transform(fond_region_rp(),paste0("+init=epsg:",code_epsg_rp()))
        fond_france <- st_transform(fond_habillage_rp()[[1]],paste0("+init=epsg:",code_epsg_rp()))
        fond_pays <- st_transform(fond_habillage_rp()[[2]],paste0("+init=epsg:",code_epsg_rp()))
        
        st_write(ronds_carte, paste0(rep_sortie,"/",sortie,"_ronds_carte.shp"), delete_dsn = TRUE, quiet = TRUE)
        if(rupture) st_write(ronds_carte_rupt, paste0(rep_sortie,"/",sortie,"_ronds_rupt_carte.shp"), delete_dsn = TRUE, quiet = TRUE)
        if(rupture_elargi) st_write(ronds_carte_rupt_elargi, paste0(rep_sortie,"/",sortie,"_elargi_ronds_rupt_carte.shp"), delete_dsn = TRUE, quiet = TRUE)
        st_write(fond_ronds, paste0(rep_sortie,"/fond_ronds.shp"), delete_dsn = TRUE, quiet = TRUE)
        st_write(fond_lignes, paste0(rep_sortie,"/fond_lignes.shp"), delete_dsn = TRUE, quiet = TRUE)
        st_write(fond_maille, paste0(rep_sortie,"/fond_maille.shp"), delete_dsn = TRUE, quiet = TRUE)
        st_write(fond_contour,paste0(rep_sortie,"/fond_contour.shp"), delete_dsn = TRUE, quiet = TRUE)
        if(exists("fond_territoire")) if(!is.null(fond_territoire)) st_write(fond_territoire, paste0(rep_sortie,"/fond_territoire.shp"), delete_dsn = TRUE, quiet = TRUE)
        if(exists("fond_departement")) if(!is.null(fond_departement)) st_write(fond_departement, paste0(rep_sortie,"/fond_departement.shp"), delete_dsn = TRUE, quiet = TRUE)
        if(exists("fond_region")) if(!is.null(fond_region)) st_write(fond_region,paste0(rep_sortie,"/fond_region.shp"), delete_dsn = TRUE, quiet = TRUE)
        st_write(fond_france,paste0(rep_sortie,"/fond_france.shp"), delete_dsn = TRUE, quiet = TRUE)
        if(exists("fond_pays")) if(!is.null(fond_pays)) st_write(fond_pays,paste0(rep_sortie,"/fond_pays.shp"), delete_dsn = TRUE, quiet = TRUE)
        
        files <- c(paste0(rep_sortie,"/",sortie,"_ronds_carte.shp"),paste0(rep_sortie,"/",sortie,"_ronds_carte.dbf"),paste0(rep_sortie,"/",sortie,"_ronds_carte.prj"),paste0(rep_sortie,"/",sortie,"_ronds_carte.shx"),files)
        if(rupture) files <- c(paste0(rep_sortie,"/",sortie,"_ronds_rupt_carte.shp"),paste0(rep_sortie,"/",sortie,"_ronds_rupt_carte.dbf"),paste0(rep_sortie,"/",sortie,"_ronds_rupt_carte.prj"),paste0(rep_sortie,"/",sortie,"_ronds_rupt_carte.shx"),files)
        if(rupture_elargi) files <- c(paste0(rep_sortie,"/",sortie,"_elargi_ronds_rupt_carte.shp"),paste0(rep_sortie,"/",sortie,"_elargi_ronds_rupt_carte.dbf"),paste0(rep_sortie,"/",sortie,"_elargi_ronds_rupt_carte.prj"),paste0(rep_sortie,"/",sortie,"_elargi_ronds_rupt_carte.shx"),files)
        files <- c(paste0(rep_sortie,"/fond_ronds.shp"),paste0(rep_sortie,"/fond_ronds.dbf"),paste0(rep_sortie,"/fond_ronds.prj"),paste0(rep_sortie,"/fond_ronds.shx"),files)
        files <- c(paste0(rep_sortie,"/fond_lignes.shp"),paste0(rep_sortie,"/fond_lignes.dbf"),paste0(rep_sortie,"/fond_lignes.prj"),paste0(rep_sortie,"/fond_lignes.shx"),files)
        files <- c(paste0(rep_sortie,"/fond_maille.shp"),paste0(rep_sortie,"/fond_maille.dbf"),paste0(rep_sortie,"/fond_maille.prj"),paste0(rep_sortie,"/fond_maille.shx"),files)
        files <- c(paste0(rep_sortie,"/fond_contour.shp"),paste0(rep_sortie,"/fond_contour.dbf"),paste0(rep_sortie,"/fond_contour.prj"),paste0(rep_sortie,"/fond_contour.shx"),files)
        if(exists("fond_territoire")) if(!is.null(fond_territoire)) files <- c(paste0(rep_sortie,"/fond_territoire.shp"),paste0(rep_sortie,"/fond_territoire.dbf"),paste0(rep_sortie,"/fond_territoire.prj"),paste0(rep_sortie,"/fond_territoire.shx"),files)
        if(exists("fond_departement")) if(!is.null(fond_departement)) files <- c(paste0(rep_sortie,"/fond_departement.shp"),paste0(rep_sortie,"/fond_departement.dbf"),paste0(rep_sortie,"/fond_departement.prj"),paste0(rep_sortie,"/fond_departement.shx"),files)
        if(exists("fond_region")) if(!is.null(fond_region)) files <- c(paste0(rep_sortie,"/fond_region.shp"),paste0(rep_sortie,"/fond_region.dbf"),paste0(rep_sortie,"/fond_region.prj"),paste0(rep_sortie,"/fond_region.shx"),files)
        files <- c(paste0(rep_sortie,"/fond_france.shp"),paste0(rep_sortie,"/fond_france.dbf"),paste0(rep_sortie,"/fond_france.prj"),paste0(rep_sortie,"/fond_france.shx"),files)
        if(exists("fond_pays")) if(!is.null(fond_pays)) files <- c(paste0(rep_sortie,"/fond_pays.shp"),paste0(rep_sortie,"/fond_pays.dbf"),paste0(rep_sortie,"/fond_pays.prj"),paste0(rep_sortie,"/fond_pays.shx"),files)
        
        titre1 <- paste0(input$titre1_qgis_rp_id,"\n")
        titre2 <- input$titre2_qgis_rp_id
        source <- input$source_qgis_rp_id
        annee <- format(Sys.time(), format = "%Y")
        
        l <- c()
        if(exists("fond_territoire")) l <- "fond_territoire"
        if(exists("fond_departement")) l <- c(l,"fond_departement")
        if(exists("fond_region")) l <- c(l,"fond_region")
        
        if(elargi_rp())
        {
          l=c(paste0(sortie,"_ronds_carte"),
              paste0(sortie,"_elargi_ronds_carte"),
              "fond_france",
              "fond_contour",
              "fond_maille",
              l,
              "fond_maille_elargi",
              "fond_lignes",
              "fond_ronds"
          )
          if(rupture)
          {
            l=c(l,paste0(sortie,"_ronds_rupt_carte"))
          }
          if(rupture_elargi)
          {
            l=c(l,paste0(sortie,"_elargi_ronds_rupt_carte"))
          }
        }else
        {
          l=c(paste0(sortie,"_ronds_carte"),
              "fond_france",
              "fond_contour",
              "fond_maille",
              l,
              "fond_lignes",
              "fond_ronds"
          )
          if(rupture)
          {
            l=c(paste0(sortie,"_ronds_rupt_carte"),l)
          }
        }
        
        l <- c(l,"fond_pays")
        
        export_projet_qgis_ronds(l,rep_sortie,sortie,titre1,titre2,source,annee)
        
        removeModal()
        
        showModal(modalDialog(HTML(paste0("<font size=+1>Le projet Qgis a \u00e9t\u00e9 cr","\u00e9","ee.</font>")), size="m", footer=NULL, easyClose = TRUE, style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"))
        
        return(files)
      }
      
      elargi_rp <- reactive({
        if(is.null(input$elargi_rp_id))
        {
          elargi <- FALSE
        }else
        {
          elargi <- input$elargi_rp_id
        }
        return(elargi)
      })
      
      code_epsg_rp <- reactive({
        code_epsg <- switch(dom, #DOM
                            "0"="2154",# Lambert 93
                            "971"="32620",# UTM 20 N
                            "972"="32620",# UTM 20 N
                            "973"="2972",# UTM 22 N
                            "974"="2975",# UTM 40 S
                            "976"="4471")# UTM 38 S
        return(code_epsg)
      })
      
      analyse_rp <- reactive({
        req(choix_centroid_rp())
        
        analyse <- k_ronds(fondMaille,fondMailleElargi,names(fondMaille)[1],data,"CODE",varVolume,elargi_rp(),choix_centroid_rp(),fondChx)
        
        if(is.null(analyse))
        {
          showModal(modalDialog(HTML(paste0("<font size=+1>La maille ne correspond pas au niveau g\u00e9ographique du fichier de donn","\u00e9","es.<br><br>Veuillez svp choisir une maille adapt","\u00e9","e ou modifier le fichier de donn","\u00e9","es.</font>")), size="l", footer=NULL, easyClose = TRUE, style = "color: #fff; background-color: #DF691A; border-color: #2e6da4")) #337ab7
          erreur_maille$a <- TRUE
          return(NULL)
        }
        
        analyse$donnees[,"TXT1"] <- paste0("<b> <font color=#2B3E50>",format(analyse$donnees$save, big.mark=" ",decimal.mark=",",nsmall=0),"</font></b>")
        if(elargi_rp())
        {
          analyse$donnees_elargi[,"TXT1"] <- paste0("<b> <font color=#2B3E50>",format(analyse$donnees_elargi$save, big.mark=" ",decimal.mark=",",nsmall=0),"</font></b>")
        }
        
        analyse_WGS84 <- st_transform(analyse$analyse_points,"+init=epsg:4326 +proj=longlat +ellps=WGS84")
        
        return(list(analyse,analyse_WGS84))
      })
      
      fond_habillage_rp <- reactive({
        if(dom=="0")
        {
          pays <- st_transform(sf_paysm(),"+init=epsg:4326 +proj=longlat +ellps=WGS84")
          fra <- st_transform(sf_fram(),"+init=epsg:4326 +proj=longlat +ellps=WGS84")
        }else
        {
          if(dom=="971")
          {
            fra <- st_transform(sf_reg01(),"+init=epsg:4326 +proj=longlat +ellps=WGS84")
            pays <- fra
          }
          if(dom=="972")
          {
            fra <- st_transform(sf_reg02(),"+init=epsg:4326 +proj=longlat +ellps=WGS84")
            pays <- fra
          }
          if(dom=="973")
          {
            fra <- st_transform(sf_reg03(),"+init=epsg:4326 +proj=longlat +ellps=WGS84")
            pays <- st_transform(sf_pays973(),"+init=epsg:4326 +proj=longlat +ellps=WGS84")
          }
          if(dom=="974")
          {
            fra <- st_transform(sf_reg04(),"+init=epsg:4326 +proj=longlat +ellps=WGS84")
            pays <- fra
          }
          if(dom=="976")
          {
            fra <- st_transform(sf_reg06(),"+init=epsg:4326 +proj=longlat +ellps=WGS84")
            pays <- fra
          }
        }
        fond_france <- fra
        fond_pays <- pays
        
        return(list(fond_france,fond_pays))
      })
      
      fond_contour_maille_rp <- reactive({
        test_contour <- try(st_transform(fondContour,"+init=epsg:4326 +proj=longlat +ellps=WGS84"), silent = TRUE)
        test_maille <- try(st_transform(fondMaille,"+init=epsg:4326 +proj=longlat +ellps=WGS84"), silent = TRUE)
        
        if(any(list(class(test_contour),class(test_maille)) %in% "try-error"))
        {
          showModal(modalDialog(HTML(paste0("<font size=+1>Une erreur est survenue dans la cr","\u00e9","ation du territoire.<br><br>Veuillez svp v\u00e9rifier vos donn","\u00e9","es et les variables choisies.</font>")), size="m", footer=NULL, easyClose = TRUE, style = "color: #fff; background-color: #DF691A; border-color: #2e6da4"))
          erreur_maille$a <- TRUE
          return(NULL)
        }else
        {
          contour_WGS84 <- st_transform(fondContour,"+init=epsg:4326 +proj=longlat +ellps=WGS84")
          maille_WGS84 <- st_transform(fondMaille,"+init=epsg:4326 +proj=longlat +ellps=WGS84")
        }
        
        return(list(contour_WGS84,maille_WGS84))
      })
      
      fond_elargi_rp <- reactive({
        req(analyse_rp())
        if(elargi_rp())
        {
          analyse_WGS84_elargi <- st_transform(analyse_rp()[[1]]$analyse_points_elargi,"+init=epsg:4326 +proj=longlat +ellps=WGS84")
          maille_WGS84_elargi <- st_transform(fondMailleElargi,"+init=epsg:4326 +proj=longlat +ellps=WGS84")
          return(list(analyse_WGS84_elargi,maille_WGS84_elargi))
        }else
        {
          return(NULL)
        }
      })
      
      list_bbox_rp <- reactive({
        req(fond_contour_maille_rp())
        
        list_bbox <- list(c(st_bbox(fond_contour_maille_rp()[[1]])[1],st_bbox(fond_contour_maille_rp()[[1]])[3]),c(st_bbox(fond_contour_maille_rp()[[1]])[2],st_bbox(fond_contour_maille_rp()[[1]])[4]))
        return(list_bbox)
      })
      
      calcul_rond_rp <- reactive({
        req(calcul_max_rayon_metres_rp(),input$taille_rond_rp_id)
        if(is.null(input$taille_rond_rp_id)) taille_rond <- 1000
        
        if(!is.null(input$taille_rond_rp_id))
        {
          if(input$taille_rond_rp_id>calcul_max_rayon_metres_rp()[[1]])
          {
            taille_rond_m <- calcul_max_rayon_metres_rp()[[1]]
          }else
          {
            taille_rond_m <- input$taille_rond_rp_id
          }
        }else
        {
          taille_rond_m <- NULL
        }
        
        return(taille_rond_m)
      })
      
      analyse_ronds_sf_rp <- reactive({
        req(analyse_rp(),code_epsg_rp(),calcul_rond_rp())
        # On cree les ronds en projection locale (pl) pour l'export Qgis
        if(elargi_rp())
        {
          req(fond_elargi_rp())
          centres <- rbind(st_coordinates(fond_elargi_rp()[[1]]))
          row.names(centres) <- c(1:(nrow(analyse_rp()[[1]]$donnees_elargi)))
          ronds <- st_sf(geometry=st_sfc(lapply(c(1:nrow(centres)),function(x) st_point(centres[x,])),crs="+init=epsg:4326 +proj=longlat +ellps=WGS84"))
          ronds_pl_elargi <- st_buffer(st_transform(ronds,paste0("+init=epsg:",code_epsg_rp())), calcul_rond_rp()*sqrt(analyse_rp()[[1]]$donnees_elargi[,varVolume]/calcul_max_rayon_metres_rp()[[2]]))
          
          # On cree les ronds 
          dt_ronds_sf <- data.frame(ronds_pl_elargi,stringsAsFactors = F)
          analyse_ronds_sf_elargi <- st_sf(cbind(analyse_rp()[[1]]$donnees_elargi,dt_ronds_sf))
        }else
        {
          analyse_ronds_sf_elargi <- NULL
        }
        
        centres <- rbind(st_coordinates(analyse_rp()[[2]]))
        row.names(centres) <- c(1:(nrow(analyse_rp()[[1]]$donnees)))
        ronds <- st_sf(geometry=st_sfc(lapply(c(1:nrow(centres)),function(x) st_point(centres[x,])),crs="+init=epsg:4326 +proj=longlat +ellps=WGS84"))
        ronds_pl <- st_buffer(st_transform(ronds,paste0("+init=epsg:",code_epsg_rp())), calcul_rond_rp()*sqrt(analyse_rp()[[1]]$donnees[,varVolume]/calcul_max_rayon_metres_rp()[[2]]))
        # On cree les ronds 
        dt_ronds_sf <- data.frame(ronds_pl,stringsAsFactors = F)
        analyse_ronds_sf <- st_sf(cbind(analyse_rp()[[1]]$donnees,dt_ronds_sf))
        return(list(analyse_ronds_sf,analyse_ronds_sf_elargi))
      })
      
      fond_territoire_rp <- reactive({
        if(!is.null(fondSuppl))
        {
          fond_territoire <- st_transform(fondSuppl,"+init=epsg:4326 +proj=longlat +ellps=WGS84")
          return(fond_territoire)
        }else
        {
          return(NULL)
        }
      })
      
      fond_region_rp <- reactive({
        fond_region <- st_transform(sf_regm(),"+init=epsg:4326 +proj=longlat +ellps=WGS84")
        return(fond_region)
      })
      
      fond_departement_rp <- reactive({
        fond_departement <- st_transform(sf_depm(),"+init=epsg:4326 +proj=longlat +ellps=WGS84")
        return(fond_departement)
      })
      
      fond_select_donnees_elargi_rp <- reactive({
        req(analyse_ronds_sf_rp(),analyse_rp())
        
        if(elargi_rp())
        {
          fond_donnees_elargi <- analyse_ronds_sf_rp()[[2]][as.data.frame(analyse_ronds_sf_rp()[[2]])[,"CODE"] %in% analyse_rp()[[1]]$donnees_elargi[input$mydonnees_rp_rows_selected,"CODE"],]
          fond_donnees_elargi <- st_transform(fond_donnees_elargi,"+init=epsg:4326 +proj=longlat +ellps=WGS84")
          return(fond_donnees_elargi)
        }else
        {
          return(NULL)
        }
      })
      
      fond_select_donnees_rp <- reactive({
        req(analyse_ronds_sf_rp(),analyse_rp())
        
        fond_donnees <- analyse_ronds_sf_rp()[[1]][as.data.frame(analyse_ronds_sf_rp()[[1]])[,"CODE"] %in% analyse_rp()[[1]]$donnees[input$mydonnees_rp_rows_selected,"CODE"],]
        if(nrow(fond_donnees)>0)
        {
          fond_donnees <- st_transform(fond_donnees,"+init=epsg:4326 +proj=longlat +ellps=WGS84")
          return(fond_donnees)
        }else
        {
          return(NULL)
        }
      })
      
      fond_select_maille_elargi_rp <- reactive({
        req(fond_elargi_rp())
        if(elargi_rp())
        {
          fond_maille_elargi <- fond_elargi_rp()[[2]][as.data.frame(fond_elargi_rp()[[2]])[,"CODE"] %in% as.data.frame(fondMailleElargi)[input$mymaille_rp_rows_selected,"CODE"],]
          return(fond_maille_elargi)
        }else
        {
          return(NULL)
        }
      })
      
      fond_select_maille_rp <- reactive({
        req(fond_contour_maille_rp())
        
        fond_maille <- fond_contour_maille_rp()[[2]][as.data.frame(fond_contour_maille_rp()[[2]])[,"CODE"] %in% as.data.frame(fondMaille)[input$mymaille_rp_rows_selected,"CODE"],]
        return(fond_maille)
      })
      
      fond_select_contour_rp <- reactive({
        req(fond_contour_maille_rp())
        
        fond_contour <- fond_contour_maille_rp()[[1]][as.data.frame(fond_contour_maille_rp()[[1]])[,"CODE"] %in% as.data.frame(fondContour)[input$mycontour_rp_rows_selected,"CODE"],]
        return(fond_contour)
      })
      
      # CONSTRUCTION DE LA MAP EN LEAFLET
      
      react_fond_rp <- reactive({
        
        if(input$menu=="carte")
        {
          showModal(modalDialog(HTML("<i class=\"fa fa-spinner fa-spin fa-2x fa-fw\"></i><font size=+1>\u00c9laboration de la carte...</font> "), size="m", footer=NULL, style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"))
          
          # Construction de la map par defaut
          
          m <- leaflet(padding = 0,
                       options = leafletOptions(
                         preferCanvas = TRUE,
                         transition = 2,
                         minZoom = 6,
                         maxZoom = 10
                       )) %>%
            
            setMapWidgetStyle(list(background = "#AFC9E0")) %>%
            
            addTiles_insee(attribution = paste0("<a href=\"http://www.insee.fr\">\u00A9 IGN - INSEE ",format(Sys.time(), format = "%Y"),"</a>")) %>%
            
            fitBounds(lng1 = min(list_bbox_rp()[[1]]),
                      lat1 = min(list_bbox_rp()[[2]]),
                      lng2 = max(list_bbox_rp()[[1]]),
                      lat2 = max(list_bbox_rp()[[2]])
            ) %>%
            
            # On ajoute une barre d'echelle
            addScaleBar(position = 'bottomright',
                        options = scaleBarOptions(metric = TRUE, imperial = FALSE)
            ) %>%
            
            # Pour gerer l'ordre des calques
            addMapPane(name = "fond_pays", zIndex = 401) %>%
            addMapPane(name = "fond_france", zIndex = 402) %>%
            addMapPane(name = "fond_habillage", zIndex = 403) %>%
            addMapPane(name = "fond_dep", zIndex = 404) %>%
            addMapPane(name = "fond_reg", zIndex = 405) %>%
            addMapPane(name = "fond_territoire", zIndex = 406) %>%
            addMapPane(name = "fond_trio3", zIndex = 407) %>%
            addMapPane(name = "fond_trio2", zIndex = 408) %>%
            addMapPane(name = "fond_trio1", zIndex = 409) %>%
            addMapPane(name = "selection", zIndex = 410) %>%
            
            addMapPane(name = "fond_legende", zIndex = 411)
          
          # AFFICHAGE DES FONDS D'HABILLAGE
          if(dom %in% c("0","973"))
          {
            # fond des pays
            m <- addPolygons(map = m, data = fond_habillage_rp()[[2]][,"LIBGEO"], opacity = 1, # fond_pays sauf la France
                             stroke = TRUE, color = "white",
                             weight = 1,
                             options = pathOptions(pane = "fond_pays", clickable = F),
                             fill = T, fillColor = "#CCCCCC", fillOpacity = 1
                             
            )
          }
          
          # fond de la France metro ou d'un DOM
          m <- addPolygons(map = m, data = fond_habillage_rp()[[1]][,"LIBGEO"], opacity = 1, # fond_france
                           stroke = TRUE, color = "black",
                           weight = 1.5,
                           options = pathOptions(pane = "fond_france", clickable = F),
                           fill = T, fillColor = "white", fillOpacity = 1
          )
          
          m_save_rp$a <- m
          
          # AFFICHAGE DU FOND TERRITOIRE
          
          if(!is.null(fondSuppl))
          {
            m <- addPolygons(map = m, data = fond_territoire_rp(),
                             stroke = TRUE, color = "#BFBFBF", opacity = 1,
                             weight = 0.5,
                             options = pathOptions(pane = "fond_territoire", clickable = T),
                             popup = paste0("<b> <font color=#2B3E50>",as.data.frame(fond_territoire_rp())[,"LIBELLE"], "</font> </b>"),
                             fill = T, fillColor = "white", fillOpacity = 0.001,
                             group = "territoire"
            )
          }
          
          # AFFICHAGE DES FONDS CONTOUR ET MAILLE
          
          # fond du contour
          m <- addPolygons(map = m, data = fond_contour_maille_rp()[[1]], opacity = 0.3, #contour_WGS84
                           stroke = TRUE, color = "black", weight = 3,
                           options = pathOptions(pane = "fond_trio3", clickable = T),
                           popup = paste0("<b> <font color=#2B3E50>",as.data.frame(fond_contour_maille_rp()[[1]])[,"LIBELLE"], "</font> </b>"),
                           fill = T, fillColor = "white", fillOpacity = 0.3,
                           group = "maille_contour"
          )
          
          # fond de la maille
          m <- addPolygons(map = m, data = fond_contour_maille_rp()[[2]], opacity = 1, #maille_WGS84
                           stroke = TRUE, color = "grey", weight = 1,
                           options = pathOptions(pane = "fond_trio2", clickable = T),
                           popup = paste0("<b> <font color=#2B3E50>",as.data.frame(fond_contour_maille_rp()[[2]])[,"LIBELLE"], "</font> </b>"),
                           fill = T, fillColor = "white", fillOpacity = 0.001,
                           group = "maille_contour"
          )
          
          # AFFICHAGE DE L'ANALYSE
          analyse <- k_ronds(fondMaille,fondMailleElargi,names(fondMaille)[1],data,"CODE",varVolume,FALSE,"centroid",fondChx)
          analyse$donnees[,"TXT1"] <- paste0("<b> <font color=#2B3E50>",format(analyse$donnees$save, big.mark=" ",decimal.mark=",",nsmall=0),"</font></b>")
          analyse_WGS84 <- st_transform(analyse$analyse_points,"+init=epsg:4326 +proj=longlat +ellps=WGS84")
          
          m <- addCircles(map = m,
                          lng = st_coordinates(analyse_WGS84)[,1],
                          lat = st_coordinates(analyse_WGS84)[,2],
                          stroke = TRUE, color = "white",
                          opacity = 1,
                          weight = 1,
                          radius = (calcul_max_rayon_metres_rp()[[1]]/1.25)*sqrt(analyse$donnees[,varVolume]/calcul_max_rayon_metres_rp()[[2]]),
                          options = pathOptions(pane = "fond_trio1", clickable = T),
                          popup = paste0("<b> <font color=#2B3E50>",varVolume," : </font></b>",analyse$donnees$TXT1),
                          fill = T,
                          fillColor = sapply(analyse$donnees$save, function(x) if(x>0){"#CD853F"}else{"#6495ED"}),
                          fillOpacity = 1,
                          group = "taille"
          )
          
          removeModal()
          
          showModal(modalDialog(HTML("<font size=+1>Veuillez patientez svp, la carte va s'afficher dans quelques secondes...<br><br><i class=\"fa fa-hand-pointer-o fa-fw\"></i><b>Double-cliquez</b> ensuite sur la carte pour afficher la l\u00e9gende.</font> "), size="m", footer=NULL, easyClose = TRUE, style = "color: #fff; background-color: #DF691A; border-color: #2e6da4")) #337ab7
          
          return(m)
        }
      })
      
      # MODIFICATION DES FONDS TERRITOIRE D'ETUDES, REGION ET DEPARTEMENT
      
      observeEvent(input$ajout_territoire_rp_id,{
        
        proxy <- leafletProxy("mymap_rp")
        
        clearGroup(map = proxy, group = "territoire")
        
        #fond du territoire d'etudes
        if(!is.null(fondSuppl))
        {
          if(input$ajout_territoire_rp_id)
          {
            proxy <- addPolygons(map = proxy, data = fond_territoire_rp(),
                                 stroke = TRUE, color = "#BFBFBF", opacity = 1,
                                 weight = 0.5,
                                 options = pathOptions(pane = "fond_territoire", clickable = T),
                                 popup = paste0("<b> <font color=#2B3E50>",as.data.frame(fond_territoire_rp())[,"LIBELLE"], "</font> </b>"),
                                 fill = T, fillColor = "white", fillOpacity = 0.001,
                                 group = "territoire"
            )
          }
        }
      },ignoreInit = TRUE)
      
      observeEvent(input$ajout_reg_rp_id,{
        
        proxy <- leafletProxy("mymap_rp")
        
        clearGroup(map = proxy, group = "region")
        
        if(dom=="0")
        {
          if(input$ajout_reg_rp_id)
          {
            #fond des regions
            proxy <- addPolygons(map = proxy, data = fond_region_rp(),
                                 stroke = TRUE, color = "grey", opacity = 1,
                                 weight = 1.5,
                                 options = pathOptions(pane = "fond_reg", clickable = F),
                                 fill = F,
                                 group = "region"
            )
          }
        }
      },ignoreInit = TRUE)
      
      observeEvent(input$ajout_dep_rp_id,{
        
        proxy <- leafletProxy("mymap_rp")
        
        clearGroup(map = proxy, group = "departement")
        
        if(dom=="0")
        {
          if(input$ajout_dep_rp_id)
          {
            #fond des departements
            proxy <- addPolygons(map = proxy, data = fond_departement_rp(),
                                 stroke = TRUE, color = "grey", opacity = 1,
                                 weight = 0.5,
                                 options = pathOptions(pane = "fond_dep", clickable = F),
                                 fill = F,
                                 group = "departement"
            )
          }
        }
      },ignoreInit = TRUE)
      
      # MODIFICATION DE L'ORDRE DES CALQUES
      
      observeEvent(list(input$monter_fond_rp_id,input$descendre_fond_rp_id),{
        
        if(as.numeric(input$monter_fond_rp_id)==0 & as.numeric(input$descendre_fond_rp_id)==0) return(NULL)
        
        proxy <- leafletProxy("mymap_rp")
        
        clearGroup(map = proxy, group = "maille_contour")
        clearGroup(map = proxy, group = "taille")
        
        i <- 1 #pour gerer l'ordre des fonds dans le pane
        for(fond in liste_fonds$a)
        {
          if(fond=="analyse")
          {
            proxy <- addCircles(map = proxy,
                                lng = st_coordinates(analyse_rp()[[2]])[,1],
                                lat = st_coordinates(analyse_rp()[[2]])[,2],
                                stroke = TRUE, color = "white",
                                opacity = 1,
                                weight = 1,
                                radius = calcul_rond_rp()*sqrt(analyse_rp()[[1]]$donnees[,varVolume]/calcul_max_rayon_metres_rp()[[2]]),
                                options = pathOptions(pane = paste0("fond_trio",i), clickable = T),
                                popup = paste0("<b> <font color=#2B3E50>",varVolume," : </font></b>",analyse_rp()[[1]]$donnees$TXT1),
                                fill = T,
                                fillColor = sapply(analyse_rp()[[1]]$donnees$save, function(x) if(x>0){"#CD853F"}else{"#6495ED"}),
                                fillOpacity = 1,
                                group = "taille"
            )
            
            ordre_analyse$a <- i
          }
          
          if(fond=="maille")
          {
            proxy <- addPolygons(map = proxy, data = fond_contour_maille_rp()[[2]], opacity = 1, #maille_WGS84
                                 stroke = TRUE, color = "grey", weight = 1,
                                 options = pathOptions(pane = paste0("fond_trio",i), clickable = T),
                                 popup = paste0("<b> <font color=#2B3E50>",as.data.frame(fond_contour_maille_rp()[[2]])[,"LIBELLE"], "</font> </b>"),
                                 fill = T, fillColor = "white", fillOpacity = 0.001,
                                 group = "maille_contour"
            )
          }
          
          if(fond=="contour")
          {
            proxy <- addPolygons(map = proxy, data = fond_contour_maille_rp()[[1]], opacity = 0.3, #contour_WGS84
                                 stroke = TRUE, color = "black", weight = 3,
                                 options = pathOptions(pane = paste0("fond_trio",i), clickable = T),
                                 popup = paste0("<b> <font color=#2B3E50>",as.data.frame(fond_contour_maille_rp()[[1]])[,"LIBELLE"], "</font> </b>"),
                                 fill = T, fillColor = "white", fillOpacity = 0.3,
                                 group = "maille_contour"
            )
          }
          
          i <- i + 1
        }
        
      },ignoreInit = TRUE)
      
      # MODIFICATION DE LA TAILLE DES RONDS
      
      observeEvent(input$taille_rond_rp_id,{
        req(input$taille_rond_rp_id,calcul_rond_rp())
        
        proxy <- leafletProxy("mymap_rp")
        
        clearGroup(map = proxy, group = "taille")
        
        proxy <- addCircles(map = proxy,
                            lng = st_coordinates(analyse_rp()[[2]])[,1],
                            lat = st_coordinates(analyse_rp()[[2]])[,2],
                            stroke = TRUE, color = "white",
                            opacity = 1,
                            weight = 1,
                            radius = calcul_rond_rp()*sqrt(analyse_rp()[[1]]$donnees[,varVolume]/calcul_max_rayon_metres_rp()[[2]]),
                            options = pathOptions(pane = paste0("fond_trio",ordre_analyse$a), clickable = T),
                            popup = paste0("<b> <font color=#2B3E50>",varVolume," : </font></b>",analyse_rp()[[1]]$donnees$TXT1),
                            fill = T,
                            fillColor = sapply(analyse_rp()[[1]]$donnees$save, function(x) if(x>0){"#CD853F"}else{"#6495ED"}),
                            fillOpacity = 1,
                            group = "taille"
        )
        
      },ignoreInit = TRUE)
      
      # MODIFICATION DE LA POSITION DES RONDS (CHX OU CENTROID)
      
      observeEvent(input$choix_centroid_rp_id,{
        req(input$choix_centroid_rp_id)
        
        proxy <- leafletProxy("mymap_rp")
        
        clearGroup(map = proxy, group = "taille")
        
        proxy <- addCircles(map = proxy,
                            lng = st_coordinates(analyse_rp()[[2]])[,1],
                            lat = st_coordinates(analyse_rp()[[2]])[,2],
                            stroke = TRUE, color = "white",
                            opacity = 1,
                            weight = 1,
                            radius = calcul_rond_rp()*sqrt(analyse_rp()[[1]]$donnees[,varVolume]/calcul_max_rayon_metres_rp()[[2]]),
                            options = pathOptions(pane = paste0("fond_trio",ordre_analyse$a), clickable = T),
                            popup = paste0("<b> <font color=#2B3E50>",varVolume," : </font></b>",analyse_rp()[[1]]$donnees$TXT1),
                            fill = T,
                            fillColor = sapply(analyse_rp()[[1]]$donnees$save, function(x) if(x>0){"#CD853F"}else{"#6495ED"}),
                            fillOpacity = 1,
                            group = "taille"
        )
        
      },ignoreInit = TRUE)
      
      # MODIFICATION DE LA REPRESENTATION ELARGIE
      
      observeEvent(list(input$elargi_rp_id,input$opacite_elargi_rp_id,input$taille_rond_rp_id,input$choix_centroid_rp_id),{
        req(input$elargi_rp_id,input$opacite_elargi_rp_id,input$taille_rond_rp_id)
        
        proxy <- leafletProxy("mymap_rp")
        
        clearGroup(map = proxy, group = "elargi")
        
        if(elargi_rp())
        {
          proxy <- addCircles(map = proxy,
                              lng = st_coordinates(fond_elargi_rp()[[1]])[,1],
                              lat = st_coordinates(fond_elargi_rp()[[1]])[,2],
                              stroke = TRUE, color = "white",
                              opacity = input$opacite_elargi_rp_id/100,
                              weight = 1,
                              radius = calcul_rond_rp()*sqrt(analyse_rp()[[1]]$donnees_elargi[,varVolume]/calcul_max_rayon_metres_rp()[[2]]),
                              options = pathOptions(pane = "fond_trio3", clickable = T),
                              popup = paste0("<b> <font color=#2B3E50>",varVolume," : </font></b>",analyse_rp()[[1]]$donnees_elargi$TXT1),
                              fill = T,
                              fillColor = sapply(analyse_rp()[[1]]$donnees_elargi$save, function(x) if(x>0){"#CD853F"}else{"#6495ED"}),
                              fillOpacity = input$opacite_elargi_rp_id/100,
                              group = "elargi"
          )
          
          proxy <- addPolygons(map = proxy, data = fond_elargi_rp()[[2]], opacity = 1, #maille_WGS84_elargi
                               stroke = TRUE, color = "grey", weight = 1,
                               options = pathOptions(pane = "fond_trio3", clickable = T),
                               popup = paste0("<b> <font color=#2B3E50>",analyse_rp()[[1]]$donnees_elargi[order(analyse_rp()[[1]]$donnees_elargi[,"CODE"]),"LIBELLE"], "</font> </b>"),
                               fill = T, fillColor = "white", fillOpacity = 0.001,
                               group = "elargi"
          )
        }
      },ignoreInit = TRUE)
      
      # MODIFICATION DE LA SELECTION DES OBJETS VIA LES TABLEAUX
      
      observeEvent(list(input$onglets_rp,input$choix_centroid_rp_id),{
        req(input$onglets_rp)
        
        if(input$onglets_rp == "carte")
        {
          proxy <- leafletProxy("mymap_rp")
          
          clearGroup(map = proxy, group = "select_donnees")
          
          if(!is.null(input$mydonnees_rp_rows_selected))
          {
            if(elargi_rp())
            {
              suppressWarnings(proxy <- addCircles(map = proxy,
                                                   lng = st_coordinates(st_centroid(fond_select_donnees_elargi_rp()))[,1],
                                                   lat = st_coordinates(st_centroid(fond_select_donnees_elargi_rp()))[,2],
                                                   stroke = TRUE, color = "#FFFF00",
                                                   opacity = 1,
                                                   weight = 3,
                                                   radius = calcul_rond_rp()*sqrt(analyse_rp()[[1]]$donnees_elargi[analyse_rp()[[1]]$donnees_elargi[,"CODE"] %in% analyse_rp()[[1]]$donnees_elargi[input$mydonnees_rp_rows_selected,"CODE"],varVolume]/calcul_max_rayon_metres_rp()[[2]]),
                                                   options = pathOptions(pane = "selection", clickable = F),
                                                   fill = F,
                                                   group = "select_donnees")
              )
            }else
            {
              suppressWarnings(proxy <- addCircles(map = proxy,
                                                   lng = st_coordinates(st_centroid(fond_select_donnees_rp()))[,1],
                                                   lat = st_coordinates(st_centroid(fond_select_donnees_rp()))[,2],
                                                   stroke = TRUE, color = "#FFFF00",
                                                   opacity = 1,
                                                   weight = 3,
                                                   radius = calcul_rond_rp()*sqrt(analyse_rp()[[1]]$donnees[analyse_rp()[[1]]$donnees[,"CODE"] %in% analyse_rp()[[1]]$donnees[input$mydonnees_rp_rows_selected,"CODE"],varVolume]/calcul_max_rayon_metres_rp()[[2]]),
                                                   options = pathOptions(pane = "selection", clickable = F),
                                                   fill = F,
                                                   group = "select_donnees")
              )
            }
          }
        }
      },ignoreInit = TRUE)
      
      observeEvent(input$onglets_rp,{
        req(input$onglets_rp)
        
        if(input$onglets_rp == "carte")
        {
          proxy <- leafletProxy("mymap_rp")
          
          clearGroup(map = proxy, group = "select_maille")
          
          if(!is.null(input$mymaille_rp_rows_selected))
          {
            if(elargi_rp())
            {
              proxy <- addPolygons(map = proxy, data = fond_select_maille_elargi_rp(),
                                   stroke = FALSE,
                                   options = pathOptions(pane = "selection", clickable = F),
                                   fill = T,
                                   fillColor = "#FFFF00",
                                   fillOpacity = 1,
                                   group = "select_maille"
              )
            }else
            {
              proxy <- addPolygons(map = proxy, data = fond_select_maille_rp(),
                                   stroke = FALSE,
                                   options = pathOptions(pane = "selection", clickable = F),
                                   fill = T,
                                   fillColor = "#FFFF00",
                                   fillOpacity = 1,
                                   group = "select_maille"
              )
            }
          }
        }
      },ignoreInit = TRUE)
      
      observeEvent(input$onglets_rp,{
        req(input$onglets_rp)
        
        if(input$onglets_rp == "carte")
        {
          proxy <- leafletProxy("mymap_rp")
          
          clearGroup(map = proxy, group = "select_contour")
          
          if(!is.null(input$mycontour_rp_rows_selected))
          {
            proxy <- addPolygons(map = proxy, data = fond_select_contour_rp(),
                                 stroke = FALSE,
                                 options = pathOptions(pane = "selection", clickable = F),
                                 fill = T,
                                 fillColor = "#FFFF00",
                                 fillOpacity = 1,
                                 group = "select_contour"
            )
          }
        }
      },ignoreInit = TRUE)
      
      # CONSTRUCTION DE LA LEGENDE
      
      lon_lat_rp <- reactive({
        click <- input$mymap_rp_click
        lon <- click$lng
        lat <- click$lat
        return(list(lon,lat))
      })
      
      observeEvent(list(input$mymap_rp_zoom,input$mymap_rp_click,input$titre_ronds_legende_rp_id,input$taille_rond_rp_id),{
        req(input$taille_rond_rp_id)
        
        if(is.null(input$affiche_legende_rp_id)) return(NULL)
        
        if(input$affiche_legende_rp_id==FALSE) return(NULL)
        
        if(is.null(lon_lat_rp()[[1]])) return(NULL)
        
        CONSTRUCTION_LEGENDE_RP()
      })
      
      construction_legende_rp <- reactive({
        zoom <- as.numeric(input$mymap_rp_zoom)
        coeff <- ((360/(2^zoom))/7.2) # Permet de fixer une distance sur l'ecran. Il s'agit en gros d'une conversion des degres en pixels. Reste constant a longitude egale mais varie un peu selon la latitude
        ronds_leg <- construction_ronds_legende(lon_lat_rp()[[1]],lon_lat_rp()[[2]],code_epsg_rp(),input$taille_rond_rp_id)
        lignes <- construction_lignes_legende(ronds_leg,coeff,code_epsg_rp())
        ronds_leg[[2]] <- cbind(ronds_leg[[2]],VALEUR=c(max(data[,varVolume]),max(data[,varVolume])/3))
        return(list(ronds_leg,lignes,coeff))
      })
      
      CONSTRUCTION_LEGENDE_RP <- function()
      {
        proxy <- leafletProxy("mymap_rp")
        
        proxy <- clearGroup(map=proxy, group="leg")
        proxy <- clearMarkers(map=proxy)
        
        ronds_leg <- construction_legende_rp()[[1]]
        lignes <- construction_legende_rp()[[2]]
        coeff <- construction_legende_rp()[[3]]
        
        # On ajoute un cadre blanc autour de la legende
        latitude2 <- st_bbox(ronds_leg[[1]])[2]
        
        if(lignes[[4]]+coeff*6 < st_bbox(lignes[[1]])[3]+coeff*3)
        {
          longitude2 <- st_bbox(lignes[[1]])[3]+coeff*3
        }else
        {
          longitude2 <- lignes[[4]]+coeff*6
        }
        
        # leaflet du cadre blanc en 1er
        proxy <- addRectangles(map = proxy,
                               lng1 = lignes[[4]]-coeff*0.5, lat1 = lignes[[5]]+coeff*0.5, #x_titre_1 et y_titre_2
                               lng2 = longitude2, lat2 = latitude2-coeff*0.8, #ligne_grand
                               stroke = TRUE,
                               color = paste0("#2B3E50", ";background: #ffffff;
                                              border-left:2px solid #2B3E50;
                                              border-right:2px solid #2B3E50;
                                              border-top:2px solid #2B3E50;
                                              border-bottom:2px solid #2B3E50;
                                              border-radius: 5%"),
                               weight = 1,
                               options = pathOptions(pane = "fond_legende", clickable = F),
                               fill = T,
                               fillColor = "white",
                               fillOpacity = 0.8,
                               group="leg"
                               )
        
        suppressWarnings(proxy <- addCircles(map = proxy,
                                             lng = st_coordinates(st_centroid(ronds_leg[[1]]))[,1],
                                             lat = st_coordinates(st_centroid(ronds_leg[[1]]))[,2],
                                             stroke = TRUE,
                                             opacity = 1,
                                             color = "#2B3E50",
                                             weight = 2,
                                             radius = c(calcul_rond_rp(),calcul_rond_rp()/sqrt(3)),
                                             options = pathOptions(pane = "fond_legende", clickable = F),
                                             fill = T,
                                             fillColor = "white",
                                             fillOpacity = 1,
                                             group="leg")
        )
        
        # leaflet lignes
        proxy <- addPolygons(map = proxy, data = lignes[[1]],
                             stroke = TRUE,
                             opacity = 1,
                             color = "#2B3E50",
                             weight = 2,
                             options = pathOptions(pane = "fond_legende", clickable = F),
                             fill = F,
                             fillOpacity = 1,
                             group="leg"
        )
        
        # leaflet valeur ronds
        proxy <- addLabelOnlyMarkers(map = proxy,
                                     lng = lignes[[2]][2,1], lat = lignes[[2]][2,2], #ligne_grand
                                     label = as.character(format(calcul_max_rayon_metres_rp()[[2]],big.mark=" ",decimal.mark=",",nsmall=0)),
                                     labelOptions = labelOptions(noHide = T, textOnly = TRUE, direction = "right",
                                                                 style = list(
                                                                   "color" = "black",
                                                                   "font-size" = "12px"
                                                                 )),
                                     group="leg"
        )
        
        proxy <- addLabelOnlyMarkers(map = proxy,
                                     lng = lignes[[3]][2,1], lat = lignes[[3]][2,2], #ligne_petit
                                     label = as.character(format(round(calcul_max_rayon_metres_rp()[[2]]/3,0),big.mark=" ",decimal.mark=",",nsmall=0)),
                                     labelOptions = labelOptions(noHide = T, textOnly = TRUE, direction = "right",
                                                                 style = list(
                                                                   "color" = "black",
                                                                   "font-size" = "12px"
                                                                 )),
                                     group="leg"
        )
        
        #leaflet titre 1
        proxy <- addLabelOnlyMarkers(map = proxy,
                                     lng = lignes[[4]], lat = lignes[[5]], #x_titre_1 et y_titre_1
                                     label = input$titre_ronds_legende_rp_id,
                                     labelOptions = labelOptions(noHide = T, textOnly = TRUE, direction = "right",
                                                                 style = list(
                                                                   "color" = "black",
                                                                   "font-size" = "14px"
                                                                 )),
                                     group="leg"
        )
      }
      
      # AJOUT DES ONGLETS SAUVEGARDE
      
      observeEvent(input$save_carte_rp_id,{
        
        showModal(modalDialog(HTML("<i class=\"fa fa-spinner fa-spin fa-2x fa-fw\"></i><font size=+1>Sauvegarde de la carte en cours...</font> "), size="m", footer=NULL, style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"))
        
        insert_save$a <- insert_save$a + 1
        nb_save_carte <- insert_save$a-remove_carte$a
        
        m_save <- m_save_rp$a
        
        if(nb_save_carte>6)
        {
          insert_save$a <- insert_save$a - 1
          showModal(modalDialog(HTML("<font size=+1>Vous ne pouvez pas sauvegarger plus de 6 cartes. Veuillez en supprimer avant de continuer.</font> "), size="l", footer=NULL, easyClose = TRUE, style = "color: #fff; background-color: #DF691A; border-color: #2e6da4")) #337ab7
          return(NULL)
        }
        
        output[[paste0("mymap_save_",insert_save$a,"_rp")]] <- renderLeaflet({
          
          if(!is.null(fondSuppl))
          {
            if(input$ajout_territoire_rp_id)
            {
              m_save <- addPolygons(map = m_save, data = fond_territoire_rp(),
                                    stroke = TRUE, color = "#BFBFBF", opacity = 1,
                                    weight = 0.5,
                                    options = pathOptions(pane = "fond_territoire", clickable = T),
                                    popup = paste0("<b> <font color=#2B3E50>",as.data.frame(fond_territoire_rp())[,"LIBELLE"], "</font> </b>"),
                                    fill = T, fillColor = "white", fillOpacity = 0.001
              )
            }
          }
          
          if(input$ajout_reg_rp_id)
          {
            m_save <- addPolygons(map = m_save, data = fond_region_rp(),
                                  stroke = TRUE, color = "grey", opacity = 1,
                                  weight = 1.5,
                                  options = pathOptions(pane = "fond_reg", clickable = F),
                                  fill = F
            )
          }
          
          if(input$ajout_dep_rp_id)
          {
            m_save <- addPolygons(map = m_save, data = fond_departement_rp(),
                                  stroke = TRUE, color = "grey", opacity = 1,
                                  weight = 0.5,
                                  options = pathOptions(pane = "fond_dep", clickable = F),
                                  fill = F
            )
          }
          
          i <- 1 #pour gerer l'ordre des fonds dans le pane
          
          for(fond in liste_fonds$a)
          {
            if(fond=="analyse")
            {
              m_save <- addCircles(map = m_save,
                                   lng = st_coordinates(analyse_rp()[[2]])[,1],
                                   lat = st_coordinates(analyse_rp()[[2]])[,2],
                                   stroke = TRUE, color = "white",
                                   opacity = 1,
                                   weight = 1,
                                   radius = calcul_rond_rp()*sqrt(analyse_rp()[[1]]$donnees[,varVolume]/calcul_max_rayon_metres_rp()[[2]]),
                                   options = pathOptions(pane = paste0("fond_trio",i), clickable = T),
                                   popup = paste0("<b> <font color=#2B3E50>",varVolume," : </font></b>",analyse_rp()[[1]]$donnees$TXT1),
                                   fill = T,
                                   fillColor = sapply(analyse_rp()[[1]]$donnees$save, function(x) if(x>0){"#CD853F"}else{"#6495ED"}),
                                   fillOpacity = 1
              )
            }
            
            if(fond=="maille")
            {
              m_save <- addPolygons(map = m_save, data = fond_contour_maille_rp()[[2]], opacity = 1, #maille_WGS84
                                    stroke = TRUE, color = "grey", weight = 1,
                                    options = pathOptions(pane = paste0("fond_trio",i), clickable = T),
                                    popup = paste0("<b> <font color=#2B3E50>",as.data.frame(fond_contour_maille_rp()[[2]])[,"LIBELLE"], "</font> </b>"),
                                    fill = T, fillColor = "white", fillOpacity = 0.001
              )
            }
            
            if(fond=="contour")
            {
              m_save <- addPolygons(map = m_save, data = fond_contour_maille_rp()[[1]], opacity = 0.3, #contour_WGS84
                                    stroke = TRUE, color = "black", weight = 3,
                                    options = pathOptions(pane = paste0("fond_trio",i), clickable = T),
                                    popup = paste0("<b> <font color=#2B3E50>",as.data.frame(fond_contour_maille_rp()[[1]])[,"LIBELLE"], "</font> </b>"),
                                    fill = T, fillColor = "white", fillOpacity = 0.3
              )
            }
            
            i <- i + 1
          }
          
          if(elargi_rp())
          {
            m_save <- addCircles(map = m_save,
                                 lng = st_coordinates(fond_elargi_rp()[[1]])[,1],
                                 lat = st_coordinates(fond_elargi_rp()[[1]])[,2],
                                 stroke = TRUE, color = "white",
                                 opacity = input$opacite_elargi_rp_id/100,
                                 weight = 1,
                                 radius = calcul_rond_rp()*sqrt(analyse_rp()[[1]]$donnees_elargi[,varVolume]/calcul_max_rayon_metres_rp()[[2]]),
                                 options = pathOptions(pane = "fond_trio3", clickable = T),
                                 popup = paste0("<b> <font color=#2B3E50>",varVolume," : </font></b>",analyse_rp()[[1]]$donnees_elargi$TXT1),
                                 fill = T,
                                 fillColor = sapply(analyse_rp()[[1]]$donnees_elargi$save, function(x) if(x>0){"#CD853F"}else{"#6495ED"}),
                                 fillOpacity = input$opacite_elargi_rp_id/100
            )
            
            m_save <- addPolygons(map = m_save, data = fond_elargi_rp()[[2]], opacity = 1, #maille_WGS84_elargi
                                  stroke = TRUE, color = "grey", weight = 1,
                                  options = pathOptions(pane = "fond_trio3", clickable = T),
                                  popup = paste0("<b> <font color=#2B3E50>",analyse_rp()[[1]]$donnees_elargi[order(analyse_rp()[[1]]$donnees_elargi[,"CODE"]),"LIBELLE"], "</font> </b>"),
                                  fill = T, fillColor = "white", fillOpacity = 0.001
            )
          }
          
          ronds_leg <- construction_legende_rp()[[1]]
          lignes <- construction_legende_rp()[[2]]
          coeff <- construction_legende_rp()[[3]]
          
          # On ajoute un cadre blanc autour de la legende
          latitude2 <- st_bbox(ronds_leg[[1]])[2]
          
          if(lignes[[4]]+coeff*6 < st_bbox(lignes[[1]])[3]+coeff*3)
          {
            longitude2 <- st_bbox(lignes[[1]])[3]+coeff*3
          }else
          {
            longitude2 <- lignes[[4]]+coeff*6
          }
          
          # leaflet du cadre blanc en 1er
          m_save <- addRectangles(map = m_save,
                                  lng1 = lignes[[4]]-coeff*0.5, lat1 = lignes[[5]]+coeff*0.5, #x_titre_1 et y_titre_2
                                  lng2 = longitude2, lat2 = latitude2-coeff*0.8, #ligne_grand
                                  stroke = TRUE,
                                  color = paste0("#2B3E50", ";background: #ffffff;
                                                 border-left:2px solid #2B3E50;
                                                 border-right:2px solid #2B3E50;
                                                 border-top:2px solid #2B3E50;
                                                 border-bottom:2px solid #2B3E50;
                                                 border-radius: 5%"),
                                  weight = 1,
                                  options = pathOptions(pane = "fond_legende", clickable = F),
                                  fill = T,
                                  fillColor = "white",
                                  fillOpacity = 0.8
                                  )
          
          suppressWarnings(m_save <- addCircles(map = m_save,
                                                lng = st_coordinates(st_centroid(ronds_leg[[1]]))[,1],
                                                lat = st_coordinates(st_centroid(ronds_leg[[1]]))[,2],
                                                stroke = TRUE,
                                                opacity = 1,
                                                color = "#2B3E50",
                                                weight = 2,
                                                radius = c(calcul_rond_rp(),calcul_rond_rp()/sqrt(3)),
                                                options = pathOptions(pane = "fond_legende", clickable = F),
                                                fill = T,
                                                fillColor = "white",
                                                fillOpacity = 1)
          )
          
          # leaflet lignes
          m_save <- addPolygons(map = m_save, data = lignes[[1]],
                                stroke = TRUE,
                                opacity = 1,
                                color = "#2B3E50",
                                weight = 2,
                                options = pathOptions(pane = "fond_legende", clickable = F),
                                fill = F,
                                fillOpacity = 1
          )
          
          # leaflet valeur ronds
          m_save <- addLabelOnlyMarkers(map = m_save,
                                        lng = lignes[[2]][2,1], lat = lignes[[2]][2,2], #ligne_grand
                                        label = as.character(format(calcul_max_rayon_metres_rp()[[2]],big.mark=" ",decimal.mark=",",nsmall=0)),
                                        labelOptions = labelOptions(noHide = T, textOnly = TRUE, direction = "right",
                                                                    style = list(
                                                                      "color" = "black",
                                                                      "font-size" = "12px"
                                                                    ))
          )
          
          m_save <- addLabelOnlyMarkers(map = m_save,
                                        lng = lignes[[3]][2,1], lat = lignes[[3]][2,2], #ligne_petit
                                        label = as.character(format(round(calcul_max_rayon_metres_rp()[[2]]/3,0),big.mark=" ",decimal.mark=",",nsmall=0)),
                                        labelOptions = labelOptions(noHide = T, textOnly = TRUE, direction = "right",
                                                                    style = list(
                                                                      "color" = "black",
                                                                      "font-size" = "12px"
                                                                    ))
          )
          
          #leaflet titre 1
          m_save <- addLabelOnlyMarkers(map = m_save,
                                        lng = lignes[[4]], lat = lignes[[5]], #x_titre_1 et y_titre_1
                                        label = input$titre_ronds_legende_rp_id,
                                        labelOptions = labelOptions(noHide = T, textOnly = TRUE, direction = "right",
                                                                    style = list(
                                                                      "color" = "black",
                                                                      "font-size" = "14px"
                                                                    ))
          )
          
          removeModal()
          
          m_save
        })
        
        output[[paste0("remove_carte_",nb_save_carte,"_rp")]] <- renderUI({
          actionButton(paste0("remove_carte_",nb_save_carte,"_rp_id"),label="X Supprimer la carte", style="color:#FFFFFF; border-color:#FFFFFF; background-color:#2B3E50")
        })
        
        appendTab(inputId = "onglets_rp",
                  tabPanel(title=HTML(paste0("<font color=#AFC9E0> Carte ",insert_save$a,"</font>")),value=paste0("carte",nb_save_carte),uiOutput(paste0("remove_carte_",nb_save_carte,"_rp")),leafletOutput(paste0("mymap_save_",insert_save$a,"_rp"),width="100%",height = 800)),
                  select = TRUE,
                  session = session
        )
        
      }, ignoreInit = TRUE)
      
      observeEvent(input$remove_carte_1_rp_id,{
        remove_carte$a <- remove_carte$a + 1
        removeTab(inputId = "onglets_rp",
                  target = "carte1",
                  session = session
        )
      }, ignoreInit = TRUE)
      
      observeEvent(input$remove_carte_2_rp_id,{
        remove_carte$a <- remove_carte$a + 1
        removeTab(inputId = "onglets_rp",
                  target = "carte2",
                  session = session
        )
      }, ignoreInit = TRUE)
      
      observeEvent(input$remove_carte_3_rp_id,{
        remove_carte$a <- remove_carte$a + 1
        removeTab(inputId = "onglets_rp",
                  target = "carte3",
                  session = session
        )
      }, ignoreInit = TRUE)
      
      observeEvent(input$remove_carte_4_rp_id,{
        remove_carte$a <- remove_carte$a + 1
        removeTab(inputId = "onglets_rp",
                  target = "carte4",
                  session = session
        )
      }, ignoreInit = TRUE)
      
      observeEvent(input$remove_carte_5_rp_id,{
        remove_carte$a <- remove_carte$a + 1
        removeTab(inputId = "onglets_rp",
                  target = "carte5",
                  session = session
        )
      }, ignoreInit = TRUE)
      
      observeEvent(input$remove_carte_6_rp_id,{
        remove_carte$a <- remove_carte$a + 1
        removeTab(inputId = "onglets_rp",
                  target = "carte6",
                  session = session
        )
      }, ignoreInit = TRUE)
      
      
      # TABLEAUX DE DONNEES, MAILLE ET CONTOUR
      
      output$mydonnees_rp <- DT::renderDataTable(datatable({
        if(elargi_rp())
          data <- analyse_rp()[[1]]$donnees_elargi
        else
          data <- analyse_rp()[[1]]$donnees
        
        data <- data[,c("CODE","LIBELLE","save")]
        names(data) <- c("CODE","LIBELLE",varVolume)
        tableau_donnees <- data[,c("CODE","LIBELLE",varVolume)]
      },  style = 'bootstrap'
      ))
      
      output$mymaille_rp <- DT::renderDataTable(datatable({
        if(elargi_rp())
          data <- as.data.frame(fondMailleElargi)
        else
          data <- as.data.frame(fondMaille)
        
        tableau_maille <- data[,c(1:2)]
      },  style = 'bootstrap'
      ))
      
      output$mycontour_rp <- DT::renderDataTable(datatable({
        data <- as.data.frame(fondContour)
        tableau_contour <- data[,c(1:2)]
      },  style = 'bootstrap'
      ))
      
      # ENVOI DU LEAFLET A L'UI
      
      output$mymap_rp <- renderLeaflet({
        react_fond_rp()
      })
      
    }
    
    runApp(shinyApp(ui = ui, server = server), launch.browser = TRUE)
    }


shiny_classes <-
  function(data,fondMaille,fondMailleElargi=NULL,fondContour,fondSuppl=NULL,idData,varRatio,dom="0")
  {
    options("stringsAsFactors"=FALSE)
    
    # Verification des parametres
    msg_error1<-msg_error2<-msg_error3<-msg_error4<-msg_error5<-msg_error6<-msg_error7<-msg_error8<-msg_error9<-msg_error10<-msg_error11<-msg_error12<-msg_error13<-msg_error14<-msg_error15<-msg_error16<-msg_error17 <- NULL
    
    if(any(class(data)!="data.frame")) msg_error1 <- "Les donnees doivent etre dans un data.frame / "
    if(any(!any(class(fondMaille) %in% "sf"),!any(class(fondMaille) %in% "data.frame"))) msg_error2 <- "Le fond de maille doit etre un objet sf / "
    if(!is.null(fondMailleElargi)) if(any(!any(class(fondMailleElargi) %in% "sf"),!any(class(fondMailleElargi) %in% "data.frame"))) msg_error3 <- "Le fond de maille elargie doit etre un objet sf / "
    if(any(!any(class(fondContour) %in% "sf"),!any(class(fondContour) %in% "data.frame"))) msg_error4 <- "Le fond de contour doit etre un objet sf / "
    if(!is.null(fondSuppl)) if(any(!any(class(fondSuppl) %in% "sf"),!any(class(fondSuppl) %in% "data.frame"))) msg_error5 <- "Le fond supplementaire doit etre un objet sf / "
    if(any(class(idData)!="character")) msg_error6 <- "Le nom de la variable doit etre de type caractere / "
    if(any(class(varRatio)!="character")) msg_error7 <- "Le nom de la variable doit etre de type caractere / "
    if(any(class(dom)!="character")) msg_error8 <- "La valeur doit etre de type caractere ('0', '971', '972', '973', '974' ou '976') / "
    
    if(length(names(data))<2) msg_error10 <- "Le tableau des donnees n'est pas conforme. Il doit contenir au minimum une variable identifiant et la variable a representer / "
    if(length(names(fondMaille))<3) msg_error11 <- "Le fond de maille n'est pas conforme. La table doit contenir au minimum une variable identifiant, une variable libelle et la geometry / "
    if(!is.null(fondMailleElargi)) if(length(names(fondMailleElargi))<3) msg_error12 <- "Le fond de maille elargie n'est pas conforme. La table doit contenir au minimum une variable identifiant, une variable libelle et la geometry / "
    if(length(names(fondContour))<3) msg_error13 <- "Le fond de contour n'est pas conforme. La table doit contenir au minimum une variable identifiant, une variable libelle et la geometry / "
    if(!is.null(fondSuppl)) if(length(names(fondSuppl))<3) msg_error14 <- "Le fond supplementaire n'est pas conforme. La table doit contenir au minimum une variable identifiant, une variable libelle et la geometry / "
    
    if(!any(names(data) %in% idData))  msg_error15 <- "La variable identifiant les donnees n'existe pas dans la table des donnees / "
    if(!any(names(data) %in% varRatio))  msg_error16 <- "La variable a representer n'existe pas dans la table des donnees / "
    if(!dom %in% c("0","971","972","973","974","976")) msg_error17 <- "La variable dom doit etre '0', '971', '972', '973', '974' ou '976' / "
    
    if(any(!is.null(msg_error1),!is.null(msg_error2),!is.null(msg_error3),!is.null(msg_error4),
           !is.null(msg_error5),!is.null(msg_error6),!is.null(msg_error7),!is.null(msg_error8),
           !is.null(msg_error10),!is.null(msg_error11),!is.null(msg_error12),!is.null(msg_error13),
           !is.null(msg_error14),!is.null(msg_error15),!is.null(msg_error16),!is.null(msg_error17)))
    {
      stop(simpleError(paste0(msg_error1,msg_error2,msg_error3,msg_error4,msg_error5,msg_error6,msg_error7,msg_error8,
                              msg_error10,msg_error11,msg_error12,msg_error13,msg_error14,msg_error15,msg_error16,msg_error17)))
    }
    
    nb_up <- reactiveValues(a=0)
    nb_down <- reactiveValues(a=0)
    ordre_analyse <- reactiveValues(a=1,b=2)
    insert_save <- reactiveValues(a=0)
    remove_carte <- reactiveValues(a=0)
    liste_fonds_ac <- reactiveValues(a=c("analyse/maille","contour"))
    m_save_ac <- reactiveValues(a=0)
    
    erreur_maille <- reactiveValues(a=FALSE)
    
    max_classes <-  reactiveValues(a=4)
    methode_calcul <- c("fisher","jenks","kmeans","quantile","manuel")
    
    legende <- reactiveValues(a=NULL)
    
    sourc <- "Source : Insee"
    
    names(data)[names(data)==idData] <- "CODE"
    names(fondMaille)[1] <- "CODE"
    names(fondMaille)[2] <- "LIBELLE"
    names(fondContour)[1] <- "CODE"
    names(fondContour)[2] <- "LIBELLE"
    if(!is.null(fondMailleElargi)) 
    {
      names(fondMailleElargi)[1] <- "CODE"
      names(fondMailleElargi)[2] <- "LIBELLE"
      fondMailleElargi$LIBELLE<-iconv(fondMailleElargi$LIBELLE,"latin1","utf8")
    }
    if(!is.null(fondSuppl)) 
    {
      names(fondSuppl)[1] <- "CODE"
      names(fondSuppl)[2] <- "LIBELLE"
      fondSuppl$LIBELLE<-iconv(fondSuppl$LIBELLE,"latin1","utf8")
    }
    
    fondMaille$LIBELLE<-iconv(fondMaille$LIBELLE,"latin1","utf8")
    fondContour$LIBELLE<-iconv(fondContour$LIBELLE,"latin1","utf8")
    
    ui <- navbarPage("OCEANIS", id="menu",
                     
                     theme = shinytheme("superhero"),
                     
                     tabPanel("Carte",value="carte",
                              sidebarLayout(
                                
                                sidebarPanel(width = 3,
                                             style = "overflow-y:scroll; min-height: 840px; max-height: 840px",
                                             h4(HTML("<b><font color=#95BAE2>VARIABLES</font></b>")),
                                             uiOutput("variable_classe_ac"),
                                             tags$hr(style="border: 5px solid #5182B6"), #337ab7
                                             h4(HTML("<b><font color=#95BAE2>FONDS</font></b>")),
                                             fluidRow(
                                               column(width=9, offset=0.5,
                                                      uiOutput("ordre_fonds_ac")
                                               ),
                                               column(width=1,
                                                      br(),
                                                      br(),
                                                      htmlOutput("monter_fond_ac", inline=FALSE),
                                                      htmlOutput("descendre_fond_ac", inline=FALSE)
                                               )
                                             ),
                                             uiOutput("elargi_ac"),
                                             conditionalPanel(condition = 'input.elargi_ac_id',
                                                              uiOutput("opacite_elargi_ac")
                                             ),
                                             uiOutput("ajout_territoire_ac"),
                                             uiOutput("ajout_reg_ac"),
                                             uiOutput("ajout_dep_ac"),
                                             tags$hr(style="border: 5px solid #5182B6"),
                                             h4(HTML("<b><font color=#95BAE2>CLASSES</font></b>")),
                                             uiOutput("liste_classes_ac"),
                                             uiOutput("methode_ac"),
                                             uiOutput("distribution_variable_ac"),
                                             conditionalPanel(condition = 'input.distribution_variable_ac_id',
                                                              verticalLayout(
                                                                wellPanel(
                                                                  style="background: #EDF2F9; width:340px",
                                                                  plotOutput("distribution_ac"),
                                                                  br(),
                                                                  uiOutput("slider_bornes_ac"),
                                                                  uiOutput("valid_slider_bornes_ac")
                                                                )
                                                              )
                                             ),
                                             conditionalPanel(condition = 'input.methode_ac_id=="manuel"',
                                                              uiOutput("zone_bornes_ac"),
                                                              uiOutput("valid_bornes_ac")
                                             ),
                                             tags$hr(style="border: 5px solid #5182B6"),
                                             h4(HTML("<b><font color=#95BAE2>L\u00c9GENDE</font></b>")),
                                             uiOutput("titre_classes_legende_ac"),
                                             br(),
                                             uiOutput("affiche_legende_ac"),
                                             uiOutput("type_legende_ac"),
                                             br(),
                                             tags$hr(style="border: 5px solid #5182B6"),
                                             h4(HTML("<b><font color=#95BAE2>SAUVEGARDE</font></b>")),
                                             uiOutput("save_carte_ac"),
                                             br(),
                                             conditionalPanel(condition = 'input.type_legende_ac_id==1 && input.mymap_ac_click',
                                                              tags$div(class="dropup",
                                                                       HTML('<button class="btn btn-primary dropdown-toggle" type="button" data-toggle="dropdown" aria-haspopup="true" aria-expanded="false">
                                                                            Exporter en projet Qgis
                                                                            <span class="caret"></span>
                                                                            </button>'),
                                                                       tags$ul(class="dropdown-menu",
                                                                               wellPanel(
                                                                                 style="background: #2B3E50; width:340px",
                                                                                 h4("Export de la carte en projet Qgis"),
                                                                                 br(),
                                                                                 uiOutput("sortie_qgis_ac"),
                                                                                 br(),
                                                                                 uiOutput("titre1_qgis_ac"),
                                                                                 uiOutput("titre2_qgis_ac"),
                                                                                 uiOutput("source_qgis_ac"),
                                                                                 tags$head(tags$style(HTML('#export_qgis_ac_id{background-color:#337ab7}'))),
                                                                                 uiOutput("export_qgis_ac")
                                                                               )
                                                                       )
                                                                       )
                                                              
                                             ),
                                             br(),
                                             uiOutput("aide_image_ac"),
                                             br()
                              ),
                              mainPanel(
                                tags$head(
                                  tags$style(HTML(".leaflet-container { background: #AFC9E0; }"))
                                ),
                                tabsetPanel(id="onglets_ac",
                                            tabPanel(title=HTML("<b>Carte</b>"),value="carte",
                                                     leafletOutput("mymap_ac",width="100%",height = 800)),
                                            tabPanel(title=HTML(paste0("<b>Donn","\u00e9","es/Maille</b>")),value="maille",
                                                     h5("S\u00e9lectionnez une ou plusieurs lignes pour ensuite les visualiser sur la carte."),
                                                     DT::dataTableOutput("mymaille_ac",width="100%",height = 800)),
                                            tabPanel(title=HTML("<b>Contour</b>"),value="contour",
                                                     h5("S\u00e9lectionnez une ou plusieurs lignes pour ensuite les visualiser sur la carte."),
                                                     DT::dataTableOutput("mycontour_ac",width="100%",height = 800))
                                )
                              )
    )
    )
    )
    
    server <- function(input, output, session) {
      
      #################
      #Onglet Carte
      #################
      
      #Charge les donnees et fonds en memoire et affiche les widgets adequats dans le sidePanel de l'onglet "Carte"
      
      observe({
        
        # VARIABLES
        
        output$variable_classe_ac <- renderUI({
          selectInput("variable_classe_ac_id", label=h5("Variable des classes (en ratio)"), choices = varRatio, selected = varRatio)
        })
        
        # FONDS
        
        output$ordre_fonds_ac <- renderUI({
          selectInput("ordre_fonds_ac_id", label=h5("Modifier l'ordre des fonds"), choices = liste_fonds_ac$a, multiple=TRUE, selectize=FALSE, selected = "analyse")
        })
        output$monter_fond_ac <- renderUI({
          actionButton("monter_fond_ac_id", label="", icon=icon("arrow-up"))
        })
        output$descendre_fond_ac <- renderUI({
          actionButton("descendre_fond_ac_id", label="", icon=icon("arrow-down"))
        })
        
        if(!is.null(fondMailleElargi))
        {
          output$elargi_ac <- renderUI({
            checkboxInput("elargi_ac_id", label = HTML("Afficher une repr\u00e9sentation \u00e9largie de l'analyse<br>(parfois long)"),
                          value = if(is.null(fondMailleElargi)) FALSE else TRUE)
          })
          output$opacite_elargi_ac <- renderUI({
            sliderInput("opacite_elargi_ac_id", label = h5("Opacit\u00e9 de l'analyse \u00e9largie"), value=60, min=0, max=100, step=5, ticks=FALSE)
          })
        }
        
        output$ajout_territoire_ac <- renderUI({
          checkboxInput("ajout_territoire_ac_id", label = "Afficher le fond des territoires",
                        value = if(is.null(fondSuppl)) FALSE else TRUE)
        })
        output$ajout_reg_ac <- renderUI({
          checkboxInput("ajout_reg_ac_id", label = "Afficher le fond des r\u00e9gions",
                        value = FALSE)
        })
        output$ajout_dep_ac <- renderUI({
          checkboxInput("ajout_dep_ac_id", label = "Afficher le fond des d\u00e9partements",
                        value = FALSE)
        })
        
        # CLASSES
        
        output$liste_classes_ac <- renderUI({
          selectInput("nb_classes_ac_id", label = h5("Nombre de classes"),
                      choices = nb_classes_ac(), selected = 4)
        })
        output$methode_ac <- renderUI({
          selectInput("methode_ac_id", label = h5("M\u00e9thode de calcul des classes"), 
                      choices = methode_calcul, selected="kmeans")
        })
        
        output$distribution_variable_ac <- renderUI({
          bsButton("distribution_variable_ac_id",label="Distribution de la variable", style="btn btn-info", icon = icon("bar-chart-o"),
                   type = "toggle", block = FALSE, disabled = FALSE,
                   value = FALSE)
        })
        
        observeEvent(input$distribution_variable_ac_id,{
          if(!input$distribution_variable_ac_id) return()
          updateButton(session, "distribution_variable_ac_id", value = TRUE)
        }, ignoreInit = TRUE)
        
        observeEvent(input$distribution_variable_ac_id,{
          
          output$distribution_ac <- renderPlot({
            dt_donnees <- data.frame(VAR=as.numeric(analyse_ac()$donnees[,varRatio]))
            ggplot(dt_donnees, aes(x=dt_donnees$VAR)) +
              stat_bin(breaks=unique(sort(c(min(dt_donnees$VAR),new_bornes_ac(),max(dt_donnees$VAR)))), closed = "left", fill="#5182B6", col="white") +
              scale_x_continuous(breaks=unique(sort(c(min(dt_donnees$VAR),new_bornes_ac(),max(dt_donnees$VAR)))), labels = round(unique(sort(c(min(dt_donnees$VAR),new_bornes_ac(),max(dt_donnees$VAR)))),2)) +
              ggtitle(label=paste0("Distribution de la variable  : ",varRatio)) +
              xlab(label = varRatio)
          })
          
          output$slider_bornes_ac <- renderUI({
            lapply(1:(as.numeric(input$nb_classes_ac_id)-1)+1, function(i) {
              sliderInput(inputId = paste0("slider_bornes_", i,"_ac_id"), label = NULL,
                          value = rev(react_bornes_ac()[[1]])[i], min = min(react_bornes_ac()[[1]]), max = max(react_bornes_ac()[[1]]), step = 0.001) #min = rev(react_bornes_ac()[[1]])[i-1], max = rev(react_bornes_ac()[[1]])[i+1]
            })
          })
          
          output$valid_slider_bornes_ac <- renderUI({
            actionButton("valid_slider_bornes_ac_id",label=label_bouton_ac(), icon=icon("refresh"), style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
          })
        },ignoreInit = TRUE)
        
        label_bouton_ac <- eventReactive(input$methode_ac_id,{
          if(input$methode_ac_id=="manuel")
          {
            label_bouton <- "Valider les bornes manuelles"
          }else
          {
            label_bouton <- "Basculer en mode manuel"
          }
          return(label_bouton)
        })
        
        new_bornes_ac <- reactive({
          bornes <- vector()
          for (i in 2:(as.numeric(input$nb_classes_ac_id))) {
            bornes<-c(bornes,input[[paste0("slider_bornes_", i,"_ac_id")]])
          }
          return(bornes)
        })
        
        output$zone_bornes_ac <- renderUI({
          
          if(!is.null(input$methode_ac_id))
          {
            if(input$methode_ac_id=="manuel")
              suppressWarnings(bornes_analyse <- classIntervals(as.numeric(analyse_ac()$donnees[,varRatio]),as.numeric(input$nb_classes_ac_id),style="kmeans",rtimes=10,intervalClosure="left"))
            else
              suppressWarnings(bornes_analyse <- classIntervals(as.numeric(analyse_ac()$donnees[,varRatio]),input$nb_classes_ac_id,style=input$methode_ac_id,rtimes=10,intervalClosure="left"))
            
            carac_bornes <- calcul_bornes(analyse_ac()$donnees,bornes_analyse,varRatio,input$nb_classes_ac_id,input$methode_ac_id)
            
            if(!is.null(input$nb_classes_ac_id))
            {
              if(input$methode_ac_id=="manuel")
              {
                lapply(1:(as.numeric(input$nb_classes_ac_id)-1)+1, function(i) {
                  numericInput(inputId = paste0("bornes_", i,"_ac_id"), label = paste("Choix de la borne ", i-1),
                               value = carac_bornes[[1]][i])
                })
              }
            }
          }
        })
        
        output$valid_bornes_ac <- renderUI({
          actionButton("valid_bornes_ac_id",label="Rafraichir la carte", icon=icon("refresh"), style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
        })
        
        # LEGENDE
        
        output$titre_classes_legende_ac <- renderUI({
          textInput("titre_classes_legende_ac_id", label = h5("Titre de la l\u00e9gende des classes"), value = "")
        })
        
        output$affiche_legende_ac <- renderUI({
          checkboxInput("affiche_legende_ac_id", label = "Activer le d\u00e9placement de la l\u00e9gende au clic",
                        value = TRUE)
        })
        
        output$type_legende_ac <- renderUI({
          radioButtons("type_legende_ac_id", label = h5("Type de l\u00e9gende"),
                       choices = list("Litt\u00e9rale" = 1, "En echelle" = 2),
                       selected = 1, inline = TRUE)
        })
        
        # SAUVEGARDE
        
        output$save_carte_ac <- renderUI({
          actionButton("save_carte_ac_id", label=HTML("<font size=3>Sauvegarder la carte dans un onglet</font>"), style="color:#FFFFFF; background-color:#DF691A")
        })
        
        output$sortie_qgis_ac <- renderUI({
          tags$div(class="input-group",
                   HTML('<input type="text" id="sortie_qgis_ac_id" class="form-control" placeholder="Nom du projet" aria-describedby="sortie_qgis_ac_id">
                        <span class="input-group-addon" id="sortie_qgis_ac_id">.qgs</span>'))
        })
        
        output$titre1_qgis_ac <- renderUI({
          textInput("titre1_qgis_ac_id", label = h5("Titre informatif"), value = "", placeholder= "Facultatif")
        })
        
        output$titre2_qgis_ac <- renderUI({
          textInput("titre2_qgis_ac_id", label = h5("Titre descriptif"), value = "", placeholder= "Facultatif")
        })
        
        output$source_qgis_ac <- renderUI({
          textInput("source_qgis_ac_id", label = h5("Source de la carte"), value = sourc)
        })
        
        output$aide_image_ac <- renderUI({
          tags$div(class="dropup",
                   HTML(paste0('<button class="btn btn-primary dropdown-toggle" type="button" data-toggle="dropdown">
                               <i class="fa fa-book fa-fw" aria-hidden="true"></i>
                               Proc','\u00e9','dure pour capture d\'','\u00e9','cran
                               <span class="caret"></span>
                               </button>')),
                   tags$ul(class="dropdown-menu",
                           wellPanel(
                             style="background: #2B3E50; width:340px",
                             div(
                               HTML("<font size=2>Deux possibilit\u00e9s :</font>"),
                               br(),
                               br(),
                               strong(HTML("<font size=3>Par l'Outil Capture</font>")),
                               br(),
                               HTML("<font size=2>1- Ouvrir le logiciel Outil Capture sur le Poste de travail (Menu D\u00e9marrer > Tous les programmes > Accessoires > Outil Capture).</font>"),
                               br(),
                               HTML(paste0("<font size=2>2- S\u00e9lectionner la zone \u00e0 capturer (vous aurez besoin d'adapter la fen\u00eatre AUS pour avoir \u00e0 la fois la carte et la barre des t","\u00e2","ches).</font>")),
                               br(),
                               HTML("<font size=2>3- Enregistrer l'image ou copier la dans le presse-papier.</font>"),
                               br(),
                               br(),
                               strong(HTML(paste0("<font size=3>Par impression d'","\u00e9","cran</font>"))),
                               br(),
                               HTML("<font size=2>1- Appuyer sur la touche clavier \"Impr ecran\".</font>"),
                               br(),
                               HTML("<font size=2>2- Ouvrir un logiciel de retouche image (Paint par exemple).</font>"),
                               br(),
                               HTML("<font size=2>3- Coller l'image et l'enregistrer au format voulu (.jpg, .png, .bmp).</font>")
                             )
                           )
                   )
                   )
        })
        })
      
      # Pour modifier l'ordre des fonds
      observeEvent(list(input$monter_fond_ac_id,input$descendre_fond_ac_id),{
        
        ordre <- c()
        if(as.numeric(input$monter_fond_ac_id)>nb_up$a)
        {
          ordre <- 2
          nb_up$a <- nb_up$a+1
        }
        if(as.numeric(input$descendre_fond_ac_id)>nb_down$a)
        {
          ordre <- 1
          nb_down$a <- nb_down$a+1
        }
        
        if(is.null(input$ordre_fonds_ac_id)) pos_select <- 0 else pos_select <- which(liste_fonds_ac$a==input$ordre_fonds_ac_id)
        
        if(pos_select>0)
        {
          if(pos_select==ordre) liste_fonds_ac$a <- liste_fonds_ac$a[c(2,1)]
          
          updateSelectInput(session, "ordre_fonds_ac_id",
                            choices = liste_fonds_ac$a,
                            selected = input$ordre_fonds_ac_id
          )
        }
      },ignoreInit = TRUE)
      
      # Pour modifier l'ordre des fonds, ici monter un fond
      observeEvent(input$monter_fond_ac_id,{
        if(is.null(input$ordre_fonds_ac_id)) pos_select <- 1 else pos_select <- which(liste_fonds_ac$a==input$ordre_fonds_ac_id)
        
        if(length(pos_select)==1)
        {
          if(pos_select==2) liste_fonds_ac$a <- liste_fonds_ac$a[c(2,1)]
        }
        updateSelectInput(session, "ordre_fonds_ac_id",
                          choices = liste_fonds_ac$a,
                          selected = input$ordre_fonds_ac_id
        )
      }, ignoreInit = TRUE)
      
      # Pour modifier l'ordre des fonds, ici descendre un fond
      observeEvent(input$descendre_fond_ac_id,{
        if(is.null(input$ordre_fonds_ac_id)) pos_select <- 3 else pos_select <- which(liste_fonds_ac$a==input$ordre_fonds_ac_id)
        
        if(length(pos_select)==1)
        {
          if(pos_select==1) liste_fonds_ac$a <- liste_fonds_ac$a[c(2,1)]
        }
        updateSelectInput(session, "ordre_fonds_ac_id",
                          choices = liste_fonds_ac$a,
                          selected = input$ordre_fonds_ac_id
        )
      }, ignoreInit = TRUE)
      
      # Pour calculer les bornes des classes
      react_bornes_ac <- reactive({
        
        if(is.null(input$nb_classes_ac_id) | is.null(input$methode_ac_id))
        {
          max_classes$a <- 4
          methode <- "kmeans"
          suppressWarnings(bornes_analyse <- classIntervals(as.numeric(analyse_ac()$donnees[,varRatio]),max_classes$a,style=methode,rtimes=10,intervalClosure="left"))
        }else if(input$nb_classes_ac_id=="" | input$methode_ac_id=="")
        {
          max_classes$a <- 4
          methode <- "kmeans"
          suppressWarnings(bornes_analyse <- classIntervals(as.numeric(analyse_ac()$donnees[,varRatio]),max_classes$a,style=methode,rtimes=10,intervalClosure="left"))
        }else
        {
          max_classes$a <- as.numeric(input$nb_classes_ac_id)
          if(is.na(max_classes$a)) return(NULL)
          methode <- as.character(input$methode_ac_id)
          if(!methode %in% c("manuel"))
          {
            suppressWarnings(bornes_analyse <- classIntervals(as.numeric(analyse_ac()$donnees[,varRatio]),max_classes$a,style=methode,rtimes=10,intervalClosure="left"))
          }
        }
        if(methode!="manuel")
        {
          carac_bornes <- calcul_bornes(analyse_ac()$donnees,bornes_analyse,varRatio,max_classes$a,methode)
        }else if(methode=="manuel")
        {
          carac_bornes <- react_bornes_manuel_1_ac()
        }
        
        return(carac_bornes) #list(bornes=carac_bornes[[1]],pal_classes=carac_bornes[[2]])
      })
      
      # Pour calculer les bornes des classes
      react_bornes_init_ac <- reactive({
        
        suppressWarnings(bornes_analyse <- classIntervals(as.numeric(analyse_ac()$donnees[,varRatio]),4,style="kmeans",rtimes=10,intervalClosure="left"))
        carac_bornes <- calcul_bornes(analyse_ac()$donnees,bornes_analyse,varRatio,4,"kmeans")
        return(carac_bornes) #list(bornes=carac_bornes[[1]],pal_classes=carac_bornes[[2]])
      })
      
      # Pour mettre a jour les bornes en mode manuel
      react_bornes_manuel_1_ac <- eventReactive(input$valid_bornes_ac_id,{
        
        suppressWarnings(bornes_analyse <- classIntervals(as.numeric(analyse_ac()$donnees[,varRatio]),max_classes$a,style="kmeans",rtimes=10,intervalClosure="left"))
        
        carac_bornes <- calcul_bornes(analyse_ac()$donnees,bornes_analyse,varRatio,input$nb_classes_ac_id,input$methode_ac_id)
        
        bornes <- vector()
        for (i in 0:(as.numeric(input$nb_classes_ac_id))+1) {
          bornes<-c(bornes,input[[paste0("bornes_", i,"_ac_id")]])
        }
        bornes <- c(carac_bornes[[1]][1],bornes,carac_bornes[[1]][length(carac_bornes[[1]])])
        bornes <- sort(unique(bornes),decreasing = T)
        
        carac_bornes[[1]] <- bornes
        
        return(carac_bornes)
      },ignoreNULL = FALSE)
      
      # Pour mettre a jour les bornes dans la distribution
      observeEvent(input$valid_slider_bornes_ac_id,{
        updateSelectInput(session, inputId = "methode_ac_id", selected = "manuel")
        for (i in 0:(as.numeric(input$nb_classes_ac_id))+1) {
          updateNumericInput(session, inputId = paste0("bornes_", i,"_ac_id"), value = input[[paste0("slider_bornes_", i,"_ac_id")]])
        }
      },ignoreInit = TRUE)
      
      # Pour renvoyer la fourchette de classes possible
      nb_classes_ac <- reactive({
        
        if(elargi_ac())
        {
          donnees <- analyse_ac()$donnees_elargi[,varRatio]
        }else
        {
          donnees <- analyse_ac()$donnees[,varRatio]
        }
        
        suppressWarnings(
          if(min(donnees)<0 & max(donnees)>0) # Si + et -
          {
            if(length(donnees)>3 & length(donnees)<9)
            {
              return(c(3:(length(donnees)-1)))
            }else
            {
              return(c(3:9))
            }
          }else # Si tout + ou tout -
          {
            if(length(donnees)>3 & length(donnees)<5)
            {
              return(c(3:(length(donnees)-1)))
            }else
            {
              return(c(3:5))
            }
          }
        )
      })
      observe({nb_classes_ac()})
      
      # Pour exporter la carte en projet Qgis
      
      output$export_qgis_ac <- renderUI({
        downloadButton("downloadProjetQgis_ac", label="Exporter")
      })
      
      output$downloadProjetQgis_ac <- downloadHandler(contentType = "zip",
                                                      filename = function(){
                                                        paste0(input$sortie_qgis_ac_id,".zip")
                                                      },
                                                      content = function(file){
                                                        files <- EXPORT_PROJET_QGIS_AC(file)
                                                        
                                                        zip(file,files, flags = "-j9X")
                                                      }
      )
      
      EXPORT_PROJET_QGIS_AC <- function(file)
      {
        showModal(modalDialog(HTML("<i class=\"fa fa-spinner fa-spin fa-2x fa-fw\"></i> <font size=+1>Export du projet Qgis en cours...</font> "), size="m", footer=NULL, style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"))
        
        sortie <- input$sortie_qgis_ac_id
        rep_sortie <- dirname(file)
        files <- c(paste0(rep_sortie,"/",sortie,".qgs"))
        
        if(is.null(input$nb_classes_ac_id))
        {
          max_classes <- 4
        }else
        {
          max_classes <- input$nb_classes_ac_id
        }
        
        if(!is.null(lon_lat_ac()[[1]]))
        {
          suppressWarnings(test_affiche_leg <- try(table_classe <- data.frame(classe=c(max_classes:1),label=legende$a,couleurs=analyse_leg_ac()$pal_classes, stringsAsFactors = F),silent=TRUE))
          if(class(test_affiche_leg) %in% "try-error")
          {
            showModal(modalDialog(HTML("<font size=+1><i class=\"fa fa-hand-pointer-o fa-fw\"></i><b>Double-cliquez</b> d'abord sur la carte pour afficher la l\u00e9gende.</font> "), size="m", footer=NULL, easyClose = TRUE, style = "color: #fff; background-color: #DF691A; border-color: #2e6da4")) #337ab7
            return(NULL)
          }else
          {
            table_classe <- data.frame(classe=c(max_classes:1),label=legende$a,couleurs=analyse_leg_ac()$pal_classes, stringsAsFactors = F)
          }
        }else
        {
          showModal(modalDialog(HTML("<font size=+1><i class=\"fa fa-hand-pointer-o fa-fw\"></i><b>Double-cliquez</b> d'abord sur la carte pour afficher la l\u00e9gende.</font> "), size="m", footer=NULL, easyClose = TRUE, style = "color: #fff; background-color: #DF691A; border-color: #2e6da4")) #337ab7
          return(NULL)
        }
        
        if(elargi_ac())
        {
          analyse_donnees_elargi <- analyse_ac()[[3]] #donnees elargi
          analyse_maille_elargi <- analyse_ac()[[4]] #fond maille elargi
          
          names_donnees_elargi <- names(analyse_donnees_elargi)
          analyse_donnees_elargi <- data.frame(analyse_donnees_elargi,val=analyse_donnees_elargi[,varRatio],classe=palette_ac()[[1]](analyse_donnees_elargi[,varRatio]))
          names(analyse_donnees_elargi) <- c(names_donnees_elargi,"val","classe")
          
          analyse_classes_elargi <- merge(table_classe,analyse_donnees_elargi,by.x="couleurs",by.y="classe")
          
          analyse_classes_elargi <- analyse_classes_elargi[,c("CODE","LIBELLE",varRatio,"val","classe")]
          
          analyse_classes_elargi <- analyse_classes_elargi[order(analyse_classes_elargi[,varRatio],decreasing = T),]
          
          analyse_maille_elargi <- merge(analyse_maille_elargi,analyse_classes_elargi[,c("CODE",varRatio,"val","classe")],by="CODE")
          names(analyse_maille_elargi) <- c("CODE","LIBELLE",varRatio,"val","classe","geometry")
          analyse_maille_elargi <- st_sf(analyse_maille_elargi,stringsAsFactors = FALSE)
          
          fond_classes_elargi <- analyse_maille_elargi
          
          fond_classes_elargi <- st_transform(fond_classes_elargi,paste0("+init=epsg:",code_epsg_ac()))
          
          fond_maille_elargi <- st_transform(fond_elargi_ac(),paste0("+init=epsg:",code_epsg_ac()))
          
          suppressWarnings(write_fond_classes_elargi <- try(st_write(fond_classes_elargi, paste0(rep_sortie,"fond_maille_elargi_carte.shp"), delete_dsn = TRUE, quiet = TRUE),silent=TRUE))
          suppressWarnings(write_fond_maille_elargi <- try(st_write(fond_maille_elargi, paste0(rep_sortie,"fond_maille_elargi.shp"), delete_dsn = TRUE, quiet = TRUE),silent=TRUE))
          
          st_write(fond_classes_elargi, paste0(rep_sortie,"/fond_maille_elargi_carte.shp"), delete_dsn = TRUE, quiet = TRUE)
          st_write(fond_maille_elargi,paste0(rep_sortie,"/fond_maille_elargi.shp"), delete_dsn = TRUE, quiet = TRUE)
          files <- c(paste0(rep_sortie,"/fond_maille_elargi_carte.shp"),paste0(rep_sortie,"/fond_maille_elargi_carte.dbf"),paste0(rep_sortie,"/fond_maille_elargi_carte.prj"),paste0(rep_sortie,"/fond_maille_elargi_carte.shx"),files)
          files <- c(paste0(rep_sortie,"/fond_maille_elargi.shp"),paste0(rep_sortie,"/fond_maille_elargi.dbf"),paste0(rep_sortie,"/fond_maille_elargi.prj"),paste0(rep_sortie,"/fond_maille_elargi.shx"),files)
        }
        
        analyse_donnees <- analyse_ac()[[1]] #donnees
        analyse_maille <- analyse_ac()[[2]] #fond maille
        
        names_donnees <- names(analyse_donnees)
        analyse_donnees <- data.frame(analyse_donnees,val=analyse_donnees[,varRatio],classe=palette_ac()[[1]](analyse_donnees[,varRatio]))
        names(analyse_donnees) <- c(names_donnees,"val","classe")
        analyse_classes <- merge(table_classe,analyse_donnees,by.x="couleurs",by.y="classe")
        analyse_classes <- analyse_classes[,c("CODE","LIBELLE",varRatio,"val","classe")]
        
        analyse_classes <- analyse_classes[order(analyse_classes[,varRatio],decreasing = T),]
        
        analyse_maille <- merge(analyse_maille,analyse_classes[,c("CODE",varRatio,"val","classe")],by="CODE")
        names(analyse_maille) <- c("CODE","LIBELLE",varRatio,"val","classe","geometry")
        analyse_maille <- st_sf(analyse_maille,stringsAsFactors = FALSE)
        
        fond_classes <- analyse_maille
        
        fond_contour <- st_transform(fondContour,paste0("+init=epsg:",code_epsg_ac()))
        if(!is.null(fondSuppl) && input$ajout_territoire_ac_id) fond_territoire <- st_transform(fond_territoire_ac(),paste0("+init=epsg:",code_epsg_ac()))
        if(input$ajout_dep_ac_id) fond_departement <- st_transform(fond_departement_ac(),paste0("+init=epsg:",code_epsg_ac()))
        if(input$ajout_reg_ac_id) fond_region <- st_transform(fond_region_ac(),paste0("+init=epsg:",code_epsg_ac()))
        fond_france <- st_transform(fond_habillage_ac()[[1]],paste0("+init=epsg:",code_epsg_ac()))
        fond_pays <- st_transform(fond_habillage_ac()[[2]],paste0("+init=epsg:",code_epsg_ac()))
        
        st_write(fond_classes, paste0(rep_sortie,"/fond_maille_carte.shp"), delete_dsn = TRUE, quiet = TRUE)
        st_write(fond_contour,paste0(rep_sortie,"/fond_contour.shp"), delete_dsn = TRUE, quiet = TRUE)
        if(exists("fond_territoire")) if(!is.null(fond_territoire)) st_write(fond_territoire, paste0(rep_sortie,"/fond_territoire.shp"), delete_dsn = TRUE, quiet = TRUE)
        if(exists("fond_departement")) if(!is.null(fond_departement)) st_write(fond_departement, paste0(rep_sortie,"/fond_departement.shp"), delete_dsn = TRUE, quiet = TRUE)
        if(exists("fond_region")) if(!is.null(fond_region)) st_write(fond_region,paste0(rep_sortie,"/fond_region.shp"), delete_dsn = TRUE, quiet = TRUE)
        st_write(fond_france,paste0(rep_sortie,"/fond_france.shp"), delete_dsn = TRUE, quiet = TRUE)
        if(exists("fond_pays")) if(!is.null(fond_pays)) st_write(fond_pays,paste0(rep_sortie,"/fond_pays.shp"), delete_dsn = TRUE, quiet = TRUE)
        
        files <- c(paste0(rep_sortie,"/fond_maille_carte.shp"),paste0(rep_sortie,"/fond_maille_carte.dbf"),paste0(rep_sortie,"/fond_maille_carte.prj"),paste0(rep_sortie,"/fond_maille_carte.shx"),files)
        files <- c(paste0(rep_sortie,"/fond_contour.shp"),paste0(rep_sortie,"/fond_contour.dbf"),paste0(rep_sortie,"/fond_contour.prj"),paste0(rep_sortie,"/fond_contour.shx"),files)
        if(exists("fond_territoire")) if(!is.null(fond_territoire)) files <- c(paste0(rep_sortie,"/fond_territoire.shp"),paste0(rep_sortie,"/fond_territoire.dbf"),paste0(rep_sortie,"/fond_territoire.prj"),paste0(rep_sortie,"/fond_territoire.shx"),files)
        if(exists("fond_departement")) if(!is.null(fond_departement)) files <- c(paste0(rep_sortie,"/fond_departement.shp"),paste0(rep_sortie,"/fond_departement.dbf"),paste0(rep_sortie,"/fond_departement.prj"),paste0(rep_sortie,"/fond_departement.shx"),files)
        if(exists("fond_region")) if(!is.null(fond_region)) files <- c(paste0(rep_sortie,"/fond_region.shp"),paste0(rep_sortie,"/fond_region.dbf"),paste0(rep_sortie,"/fond_region.prj"),paste0(rep_sortie,"/fond_region.shx"),files)
        files <- c(paste0(rep_sortie,"/fond_france.shp"),paste0(rep_sortie,"/fond_france.dbf"),paste0(rep_sortie,"/fond_france.prj"),paste0(rep_sortie,"/fond_france.shx"),files)
        if(exists("fond_pays")) if(!is.null(fond_pays)) files <- c(paste0(rep_sortie,"/fond_pays.shp"),paste0(rep_sortie,"/fond_pays.dbf"),paste0(rep_sortie,"/fond_pays.prj"),paste0(rep_sortie,"/fond_pays.shx"),files)
        
        chemin_fonds <- rep_sortie
        titre1 <- paste0(input$titre1_qgis_ac_id,"\n")
        titre2 <- input$titre2_qgis_ac_id
        source <- input$source_qgis_ac_id
        annee <- format(Sys.time(), format = "%Y")
        variable_a_representer <- varRatio
        titre_leg_ronds <- ""
        titre_leg_classes <- input$titre_classes_legende_ac_id
        
        l <- c()
        if(exists("fond_territoire")) l <- "fond_territoire"
        if(exists("fond_departement")) l <- c(l,"fond_departement")
        if(exists("fond_region")) l <- c(l,"fond_region")
        
        if(elargi_ac())
        {
          l=c("fond_france",
              "fond_contour",
              l,
              "fond_maille_carte",
              "fond_maille_elargi_carte"
          )
        }else
        {
          l=c("fond_france",
              "fond_contour",
              l,
              "fond_maille_carte"
          )
        }
        
        l <- c(l,"fond_pays")
        
        export_projet_qgis_classes(l,rep_sortie,sortie,titre1,titre2,source,titre_leg_classes,table_classe,variable_a_representer,annee)
        
        removeModal()
        
        showModal(modalDialog(HTML(paste0("<font size=+1>Le projet Qgis a \u00e9t\u00e9 cr","\u00e9","ee.</font>")), size="m", footer=NULL, easyClose = TRUE, style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"))
        
        return(files)
      }
      
      elargi_ac <- reactive({
        if(is.null(input$elargi_ac_id))
        {
          elargi <- FALSE
        }else
        {
          elargi <- input$elargi_ac_id
        }
        return(elargi)
      })
      
      code_epsg_ac <- reactive({
        code_epsg <- switch(dom, #DOM
                            "0"="2154",# Lambert 93
                            "971"="32620",# UTM 20 N
                            "972"="32620",# UTM 20 N
                            "973"="2972",# UTM 22 N
                            "974"="2975",# UTM 40 S
                            "976"="4471")# UTM 38 S
        return(code_epsg)
      })
      
      analyse_ac <- reactive({
        
        suppressWarnings(test_k_classes <- try(k_classes(fondMaille,fondMailleElargi,names(fondMaille)[1],data,"CODE",varRatio,elargi_ac()),silent=T))
        if(class(test_k_classes) %in% "try-error")
        {
          return(NULL)
        }else
        {
          analyse <- k_classes(fondMaille,fondMailleElargi,names(fondMaille)[1],data,"CODE",varRatio,elargi_ac())
        }
        
        if(elargi_ac())
        {
          analyse <- list(donnees=analyse[[1]],fond_maille=fondMaille,donnees_elargi=analyse[[2]],fond_maille_elargi=fondMailleElargi)
        }else
        {
          analyse <- list(donnees=analyse[[1]],fond_maille=fondMaille)
        }
        
        if(is.null(analyse$donnees))
        {
          showModal(modalDialog(HTML(paste0("<font size=+1>La maille ne correspond pas au niveau g\u00e9ographique du fichier de donn","\u00e9","es.<br><br>Veuillez svp choisir une maille adapt","\u00e9","e ou modifier le fichier de donn","\u00e9","es.</font>")), size="l", footer=NULL, easyClose = TRUE, style = "color: #fff; background-color: #DF691A; border-color: #2e6da4")) #337ab7
          erreur_maille$a <- TRUE
          return(NULL)
        }
        
        analyse$donnees[,"TXT1"] <- paste0("<b> <font color=#2B3E50>",format(round(analyse$donnees[,varRatio],3), big.mark=" ",decimal.mark=",",nsmall=0),"</font></b>")
        if(elargi_ac())
        {
          analyse$donnees_elargi[,"TXT1"] <- paste0("<b> <font color=#2B3E50>",format(round(analyse$donnees_elargi[,varRatio],3), big.mark=" ",decimal.mark=",",nsmall=0),"</font></b>")
        }
        
        return(analyse)
      })
      
      analyse_leg_ac <- reactive({
        analyse <- analyse_ac()
        analyse$rupture_classes <- palette_ac()[[2]] #bornes
        analyse$pal_classes <- palette_ac()[[3]] # pal_classes
        return(analyse)
      })
      
      fond_habillage_ac <- reactive({
        
        if(dom=="0")
        {
          pays <- st_transform(sf_paysm(),"+init=epsg:4326 +proj=longlat +ellps=WGS84")
          fra <- st_transform(sf_fram(),"+init=epsg:4326 +proj=longlat +ellps=WGS84")
        }else
        {
          if(dom=="971")
          {
            fra <- st_transform(sf_reg01(),"+init=epsg:4326 +proj=longlat +ellps=WGS84")
            pays <- fra
          }
          if(dom=="972")
          {
            fra <- st_transform(sf_reg02(),"+init=epsg:4326 +proj=longlat +ellps=WGS84")
            pays <- fra
          }
          if(dom=="973")
          {
            fra <- st_transform(sf_reg03(),"+init=epsg:4326 +proj=longlat +ellps=WGS84")
            pays <- st_transform(sf_pays973(),"+init=epsg:4326 +proj=longlat +ellps=WGS84")
          }
          if(dom=="974")
          {
            fra <- st_transform(sf_reg04(),"+init=epsg:4326 +proj=longlat +ellps=WGS84")
            pays <- fra
          }
          if(dom=="976")
          {
            fra <- st_transform(sf_reg06(),"+init=epsg:4326 +proj=longlat +ellps=WGS84")
            pays <- fra
          }
        }
        fond_france <- fra
        fond_pays <- pays
        
        return(list(fond_france,fond_pays))
      })
      
      fond_contour_maille_ac <- reactive({
        
        test_contour <- try(st_transform(fondContour,"+init=epsg:4326 +proj=longlat +ellps=WGS84"), silent = TRUE)
        test_maille <- try(st_transform(fondMaille,"+init=epsg:4326 +proj=longlat +ellps=WGS84"), silent = TRUE)
        
        if(any(list(class(test_contour),class(test_maille)) %in% "try-error"))
        {
          showModal(modalDialog(HTML(paste0("<font size=+1>Une erreur est survenue dans la cr","\u00e9","ation du territoire.<br><br>Veuillez svp v\u00e9rifier vos donn","\u00e9","es et les variables choisies.</font>")), size="m", footer=NULL, easyClose = TRUE, style = "color: #fff; background-color: #DF691A; border-color: #2e6da4"))
          erreur_maille$a <- TRUE
          return(NULL)
        }else
        {
          contour_WGS84 <- st_transform(fondContour,"+init=epsg:4326 +proj=longlat +ellps=WGS84")
          maille_WGS84 <- st_transform(fondMaille,"+init=epsg:4326 +proj=longlat +ellps=WGS84")
        }
        
        return(list(contour_WGS84,maille_WGS84))
      })
      
      fond_elargi_ac <- reactive({
        if(elargi_ac())
        {
          maille_WGS84_elargi <- st_transform(fondMailleElargi,"+init=epsg:4326 +proj=longlat +ellps=WGS84")
        }
        return(maille_WGS84_elargi)
      })
      
      list_bbox_ac <- reactive({
        req(fond_contour_maille_ac())
        
        list_bbox <- list(c(st_bbox(fond_contour_maille_ac()[[1]])[1],st_bbox(fond_contour_maille_ac()[[1]])[3]),c(st_bbox(fond_contour_maille_ac()[[1]])[2],st_bbox(fond_contour_maille_ac()[[1]])[4]))
        return(list_bbox)
      })
      
      palette_ac <- reactive({
        bornes <- react_bornes_ac()[[1]]
        
        if(is.null(bornes)) return(NULL)
        
        if(elargi_ac()) # On redefini le min et le max de la serie pour eviter les valeurs en NA
        {
          bornes[length(bornes)] <- min(as.numeric(analyse_ac()$donnees_elargi[,varRatio]))
          bornes[1] <- max(as.numeric(analyse_ac()$donnees_elargi[,varRatio]))
        }else
        {
          bornes[length(bornes)] <- min(as.numeric(analyse_ac()$donnees[,varRatio]))
          bornes[1] <- max(as.numeric(analyse_ac()$donnees[,varRatio]))
        }
        
        if(length(unique(bornes)) != length(bornes))
        {
          removeModal()
          showModal(modalDialog(HTML(paste0("<font size=+1>Les bornes calculees avec la methode '",input$methode_ac_id,"' ne sont pas uniques. La methode kmeans a donc ete retenue.</font>")), size="l", footer=NULL, easyClose = TRUE, style = "color: #fff; background-color: #DF691A; border-color: #2e6da4")) #337ab7
          Sys.sleep(7)
          suppressWarnings(bornes_analyse <- classIntervals(as.numeric(analyse_ac()$donnees[,varRatio]),max_classes$a,style="kmeans",rtimes=10,intervalClosure="left"))
          carac_bornes <- calcul_bornes(analyse_ac()$donnees,bornes_analyse,varRatio,max_classes$a,"kmeans")
          updateSelectInput(session,"methode_ac_id",choices = methode_calcul, selected="kmeans")
          bornes <- carac_bornes[[1]]
          pal_classes <- carac_bornes[[2]]
        }else
        {
          pal_classes <- react_bornes_ac()[[2]]
        }
        pal_classes[is.na(pal_classes)] <- "grey"
        palette<-colorBin(palette=rev(pal_classes), domain=0:100, bins=bornes, na.color="grey")
        
        return(list(palette,bornes,pal_classes))
      })
      
      palette_init_ac <- reactive({
        
        bornes <- react_bornes_init_ac()[[1]]
        
        if(is.null(bornes)) return(NULL)
        
        test_bornes <- try(bornes[length(bornes)] <- min(as.numeric(analyse_ac()$donnees[,varRatio])), silent=TRUE)
        
        if(class(test_bornes) %in% "try-error")
        {
          return(NULL)
        }else
        {
          bornes[1] <- max(as.numeric(analyse_ac()$donnees[,varRatio]))
        }
        
        pal_classes <- react_bornes_init_ac()[[2]]
        
        pal_classes[is.na(pal_classes)] <- "grey"
        palette<-colorBin(palette=rev(pal_classes), domain=0:100, bins=bornes, na.color="grey")
        
        return(list(palette,bornes,pal_classes))
      })
      
      fond_territoire_ac <- reactive({
        if(!is.null(fondSuppl))
        {
          fond_territoire <- st_transform(fondSuppl,"+init=epsg:4326 +proj=longlat +ellps=WGS84")
          return(fond_territoire)
        }else
        {
          return(NULL)
        }
      })
      
      fond_region_ac <- reactive({
        fond_region <- st_transform(sf_regm(),"+init=epsg:4326 +proj=longlat +ellps=WGS84")
        return(fond_region)
      })
      
      fond_departement_ac <- reactive({
        fond_departement <- st_transform(sf_depm(),"+init=epsg:4326 +proj=longlat +ellps=WGS84")
        return(fond_departement)
      })
      
      fond_select_maille_elargi_ac <- reactive({
        req(fond_elargi_ac())
        
        fond_maille_elargi <- fond_elargi_ac()[as.data.frame(fond_elargi_ac())[,"CODE"] %in% as.data.frame(fondMailleElargi)[input$mymaille_ac_rows_selected,"CODE"],]
        return(fond_maille_elargi)
      })
      
      fond_select_maille_ac <- reactive({
        req(fond_contour_maille_ac())
        
        fond_maille <- fond_contour_maille_ac()[[2]][as.data.frame(fond_contour_maille_ac()[[2]])[,"CODE"] %in% as.data.frame(fondMaille)[input$mymaille_ac_rows_selected,"CODE"],]
        return(fond_maille)
      })
      
      fond_select_contour_ac <- reactive({
        req(fond_contour_maille_ac())
        
        fond_contour <- fond_contour_maille_ac()[[1]][as.data.frame(fond_contour_maille_ac()[[1]])[,"CODE"] %in% as.data.frame(fondContour)[input$mycontour_ac_rows_selected,"CODE"],]
        return(fond_contour)
      })
      
      # CONSTRUCTION DE LA MAP EN LEAFLET
      
      react_fond_ac <- reactive({
        
        if(input$menu=="carte")
        {
          showModal(modalDialog(HTML("<i class=\"fa fa-spinner fa-spin fa-2x fa-fw\"></i><font size=+1>\u00c9laboration de la carte...</font> "), size="m", footer=NULL, style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"))
          
          # Construction de la map par defaut
          
          m <- leaflet(padding = 0,
                       options = leafletOptions(
                         preferCanvas = TRUE,
                         transition = 2,
                         minZoom = 6,
                         maxZoom = 10
                       )) %>%
            
            setMapWidgetStyle(list(background = "#AFC9E0")) %>%
            
            addTiles_insee(attribution = paste0("<a href=\"http://www.insee.fr\">\u00A9 IGN - INSEE ",format(Sys.time(), format = "%Y"),"</a>")) %>%
            
            fitBounds(lng1 = min(list_bbox_ac()[[1]]),
                      lat1 = min(list_bbox_ac()[[2]]),
                      lng2 = max(list_bbox_ac()[[1]]),
                      lat2 = max(list_bbox_ac()[[2]])
            ) %>%
            
            # On ajoute une barre d'echelle
            addScaleBar(position = 'bottomright',
                        options = scaleBarOptions(metric = TRUE, imperial = FALSE)
            ) %>%
            
            # Pour gerer l'ordre des calques
            addMapPane(name = "fond_pays", zIndex = 401) %>%
            addMapPane(name = "fond_france", zIndex = 402) %>%
            addMapPane(name = "fond_habillage", zIndex = 403) %>%
            addMapPane(name = "fond_dep", zIndex = 404) %>%
            addMapPane(name = "fond_reg", zIndex = 405) %>%
            addMapPane(name = "fond_territoire", zIndex = 406) %>%
            addMapPane(name = "fond_duo2", zIndex = 407) %>%
            addMapPane(name = "fond_duo1", zIndex = 408) %>%
            addMapPane(name = "selection", zIndex = 409) %>%
            
            addMapPane(name = "fond_legende", zIndex = 410)
          
          # AFFICHAGE DES FONDS D'HABILLAGE
          
          if(dom %in% c("0","973"))
          {
            # fond des pays
            m <- addPolygons(map = m, data = fond_habillage_ac()[[2]][,"LIBGEO"], opacity = 1, # sauf la France
                             stroke = TRUE, color = "white",
                             weight = 1,
                             options = pathOptions(pane = "fond_pays", clickable = F),
                             fill = T, fillColor = "#CCCCCC", fillOpacity = 1
            )
          }
          
          # fond de la France metro ou d'un DOM
          m <- addPolygons(map = m, data = fond_habillage_ac()[[1]][,"LIBGEO"], opacity = 1,
                           stroke = TRUE, color = "black",
                           weight = 1.5,
                           options = pathOptions(pane = "fond_france", clickable = F),
                           fill = T, fillColor = "white", fillOpacity = 1
          )
          
          m_save_ac$a <- m
          
          # AFFICHAGE DU FOND TERRITOIRE
          
          if(!is.null(fondSuppl))
          {
            m <- addPolygons(map = m, data = fond_territoire_ac(),
                             stroke = TRUE, color = "#BFBFBF", opacity = 1,
                             weight = 0.5,
                             options = pathOptions(pane = "fond_territoire", clickable = T),
                             popup = paste0("<b> <font color=#2B3E50>",as.data.frame(fond_territoire_ac())[,"LIBELLE"], "</font> </b>"),
                             fill = T, fillColor = "white", fillOpacity = 0.001,
                             group = "territoire"
            )
          }
          # AFFICHAGE DU FOND CONTOUR
          m <- addPolygons(map = m, data = fond_contour_maille_ac()[[1]], opacity = 0.3,
                           stroke = TRUE, color = "black", weight = 3,
                           options = pathOptions(pane = "fond_duo2", clickable = T),
                           popup = paste0("<b> <font color=#2B3E50>",as.data.frame(fond_contour_maille_ac()[[1]])[,"LIBELLE"], "</font> </b>"),
                           fill = T, fillColor = "white", fillOpacity = 0.3,
                           group = "duo"
          )
          
          # AFFICHAGE DE LA MAILLE ET DE L'ANALYSE
          suppressWarnings(test_analyse_maille_classe <- try(analyse_ac()$donnees[rev(order(analyse_ac()[[1]][,varRatio])),varRatio],silent=T))
          if(class(test_analyse_maille_classe) %in% "try-error")
          {
            return(NULL)
          }else
          {
            analyse_maille_classe <- analyse_ac()$donnees[rev(order(analyse_ac()[[1]][,varRatio])),varRatio]
          }
          
          analyse_maille <- merge(fond_contour_maille_ac()[[2]][,c("CODE","geometry")],analyse_ac()$donnees[,c("CODE","LIBELLE",varRatio,"TXT1")],by="CODE")
          names(analyse_maille)[3] <- varRatio
          analyse_maille <- analyse_maille[rev(order(as.data.frame(analyse_maille)[,varRatio])),]
          analyse_maille <- st_sf(analyse_maille,stringsAsFactors = FALSE)
          
          m <- addPolygons(map = m, data = analyse_maille, opacity = 1,
                           stroke = TRUE, color = "white", weight = 1,
                           options = pathOptions(pane = "fond_duo1", clickable = T),
                           popup = paste0("<b> <font color=#2B3E50>",as.data.frame(analyse_maille)$LIBELLE, "</font> </b><br><b><font color=#2B3E50>",varRatio," : </font></b>",as.data.frame(analyse_maille)$TXT1),
                           fill = T,
                           fillColor = palette_init_ac()[[1]](analyse_maille_classe),
                           fillOpacity = 1,
                           group = "duo"
          )
          
          removeModal()
          
          showModal(modalDialog(HTML("<font size=+1>Veuillez patientez svp, la carte va s'afficher dans quelques secondes...<br><br><i class=\"fa fa-hand-pointer-o fa-fw\"></i><b>Double-cliquez</b> ensuite sur la carte pour afficher la l\u00e9gende.</font> "), size="m", footer=NULL, easyClose = TRUE, style = "color: #fff; background-color: #DF691A; border-color: #2e6da4")) #337ab7
          
          return(m)
        }
      })
      
      # MODIFICATION DES FONDS TERRITOIRE D'ETUDES, REGION ET DEPARTEMENT
      
      observeEvent(input$ajout_territoire_ac_id,{
        
        proxy <- leafletProxy("mymap_ac")
        
        clearGroup(map = proxy, group = "territoire")
        
        #fond du territoire d'etudes
        if(!is.null(fondSuppl))
        {
          if(input$ajout_territoire_ac_id)
          {
            proxy <- addPolygons(map = proxy, data = fond_territoire_ac(),
                                 stroke = TRUE, color = "#BFBFBF", opacity = 1,
                                 weight = 0.5,
                                 options = pathOptions(pane = "fond_territoire", clickable = T),
                                 popup = paste0("<b> <font color=#2B3E50>",as.data.frame(fond_territoire_ac())[,"LIBELLE"], "</font> </b>"),
                                 fill = T, fillColor = "white", fillOpacity = 0.001,
                                 group = "territoire"
            )
          }
        }
      },ignoreInit = TRUE)
      
      observeEvent(input$ajout_reg_ac_id,{
        
        proxy <- leafletProxy("mymap_ac")
        
        clearGroup(map = proxy, group = "region")
        
        if(dom=="0")
        {
          if(input$ajout_reg_ac_id)
          {
            #fond des regions
            proxy <- addPolygons(map = proxy, data = fond_region_ac(),
                                 stroke = TRUE, color = "grey", opacity = 1,
                                 weight = 1.5,
                                 options = pathOptions(pane = "fond_reg", clickable = F),
                                 fill = F,
                                 group = "region"
            )
          }
        }
      },ignoreInit = TRUE)
      
      observeEvent(input$ajout_dep_ac_id,{
        
        proxy <- leafletProxy("mymap_ac")
        
        clearGroup(map = proxy, group = "departement")
        
        if(dom=="0")
        {
          if(input$ajout_dep_ac_id)
          {
            #fond des departements
            proxy <- addPolygons(map = proxy, data = fond_departement_ac(),
                                 stroke = TRUE, color = "grey", opacity = 1,
                                 weight = 0.5,
                                 options = pathOptions(pane = "fond_dep", clickable = F),
                                 fill = F,
                                 group = "departement"
            )
          }
        }
      },ignoreInit = TRUE)
      
      # MODIFICATION DE L'ORDRE DES CALQUES
      
      observeEvent(list(input$monter_fond_ac_id,input$descendre_fond_ac_id),{
        
        if(as.numeric(input$monter_fond_ac_id)==0 & as.numeric(input$descendre_fond_ac_id)==0) return(NULL)
        
        proxy <- leafletProxy("mymap_ac")
        
        clearGroup(map = proxy, group = "contour")
        clearGroup(map = proxy, group = "classe")
        
        i <- 1 #pour gerer l'ordre des fonds dans le pane
        
        for(fond in liste_fonds_ac$a)
        {
          if(fond=="analyse/maille")
          {
            suppressWarnings(test_analyse_maille_classe <- try(analyse_ac()$donnees[rev(order(analyse_ac()[[1]][,varRatio])),varRatio],silent=T))
            if(class(test_analyse_maille_classe) %in% "try-error")
            {
              return(NULL)
            }else
            {
              analyse_maille_classe <- analyse_ac()$donnees[rev(order(analyse_ac()[[1]][,varRatio])),varRatio]
            }
            
            analyse_maille <- merge(fond_contour_maille_ac()[[2]][,c("CODE","geometry")],analyse_ac()$donnees[,c("CODE","LIBELLE",varRatio,"TXT1")],by="CODE")
            names(analyse_maille)[3] <- varRatio
            analyse_maille <- analyse_maille[rev(order(as.data.frame(analyse_maille)[,varRatio])),]
            analyse_maille <- st_sf(analyse_maille,stringsAsFactors = FALSE)
            
            proxy <- addPolygons(map = proxy, data = analyse_maille, opacity = 1,
                                 stroke = TRUE, color = "white", weight = 1,
                                 options = pathOptions(pane = paste0("fond_duo",i), clickable = T),
                                 popup = paste0("<b> <font color=#2B3E50>",as.data.frame(analyse_maille)$LIBELLE, "</font> </b><br><b><font color=#2B3E50>",varRatio," : </font></b>",as.data.frame(analyse_maille)$TXT1),
                                 fill = T,
                                 fillColor = palette_ac()[[1]](analyse_maille_classe),
                                 fillOpacity = 1,
                                 group = "classe"
            )
            
            ordre_analyse$a <- i
          }
          
          if(fond=="contour")
          {
            proxy <- addPolygons(map = proxy, data = fond_contour_maille_ac()[[1]], opacity = 0.3,
                                 stroke = TRUE, color = "black", weight = 3,
                                 options = pathOptions(pane = paste0("fond_duo",i), clickable = T),
                                 popup = paste0("<b> <font color=#2B3E50>",as.data.frame(fond_contour_maille_ac()[[1]])[,"LIBELLE"], "</font> </b>"),
                                 fill = T, fillColor = "white", fillOpacity = 0.3,
                                 group = "contour"
            )
          }
          
          i <- i + 1
        }
      },ignoreInit = TRUE)
      
      # MODIFICATION DU NOMBRE DE CLASSES, DE LA METHODE OU DES BORNES
      
      observeEvent(list(input$nb_classes_ac_id,input$methode_ac_id,input$valid_bornes_ac_id),{
        req(input$nb_classes_ac_id,input$methode_ac_id)
        
        proxy <- leafletProxy("mymap_ac")
        
        clearGroup(map = proxy, group = "classe")
        
        suppressWarnings(test_analyse_maille_classe <- try(analyse_ac()$donnees[rev(order(analyse_ac()[[1]][,varRatio])),varRatio],silent=T))
        if(class(test_analyse_maille_classe) %in% "try-error")
        {
          return(NULL)
        }else
        {
          analyse_maille_classe <- analyse_ac()$donnees[rev(order(analyse_ac()[[1]][,varRatio])),varRatio]
        }
        
        analyse_maille <- merge(fond_contour_maille_ac()[[2]][,c("CODE","geometry")],analyse_ac()$donnees[,c("CODE","LIBELLE",varRatio,"TXT1")],by="CODE")
        names(analyse_maille)[3] <- varRatio
        analyse_maille <- analyse_maille[rev(order(as.data.frame(analyse_maille)[,varRatio])),]
        analyse_maille <- st_sf(analyse_maille,stringsAsFactors = FALSE)
        
        proxy <- addPolygons(map = proxy, data = analyse_maille, opacity = 1,
                             stroke = TRUE, color = "white", weight = 1,
                             options = pathOptions(pane = paste0("fond_duo",ordre_analyse$a), clickable = T),
                             popup = paste0("<b> <font color=#2B3E50>",as.data.frame(analyse_maille)$LIBELLE, "</font> </b><br><b><font color=#2B3E50>",varRatio," : </font></b>",as.data.frame(analyse_maille)$TXT1),
                             fill = T,
                             fillColor = palette_ac()[[1]](analyse_maille_classe),
                             fillOpacity = 1,
                             group = "classe"
        )
        
      },ignoreInit = TRUE)
      
      # MODIFICATION DE LA REPRESENTATION ELARGIE
      
      observeEvent(list(input$elargi_ac_id,input$opacite_elargi_ac_id,input$nb_classes_ac_id,input$methode_ac_id,input$valid_bornes_ac_id),{
        req(input$elargi_ac_id,input$opacite_elargi_ac_id,input$nb_classes_ac_id,input$methode_ac_id)
        
        proxy <- leafletProxy("mymap_ac")
        
        clearGroup(map = proxy, group = "elargi")
        
        if(elargi_ac())
        {
          analyse_maille_classe_elargi <- analyse_ac()$donnees_elargi[rev(order(analyse_ac()$donnees_elargi[,varRatio])),varRatio]
          
          analyse_maille_elargi <- merge(fond_elargi_ac()[,c("CODE","geometry")],analyse_ac()$donnees_elargi[,c("CODE","LIBELLE",varRatio,"TXT1")],by="CODE")
          names(analyse_maille_elargi)[3] <- varRatio
          analyse_maille_elargi <- analyse_maille_elargi[rev(order(as.data.frame(analyse_maille_elargi)[,varRatio])),]
          analyse_maille_elargi <- st_sf(analyse_maille_elargi,stringsAsFactors = FALSE)
          
          proxy <- addPolygons(map = proxy, data = analyse_maille_elargi, opacity = input$opacite_elargi_ac_id/100,
                               stroke = TRUE, color = "white", weight = 1,
                               options = pathOptions(pane = "fond_duo2", clickable = T),
                               popup = paste0("<b> <font color=#2B3E50>",as.data.frame(analyse_maille_elargi)$LIBELLE, "</font> </b><br><b><font color=#2B3E50>",varRatio," : </font></b>",as.data.frame(analyse_maille_elargi)$TXT1),
                               fill = T,
                               fillColor = palette_ac()[[1]](analyse_maille_classe_elargi),
                               fillOpacity = input$opacite_elargi_ac_id/100,
                               group = "elargi"
          )
        }
      },ignoreInit = TRUE)
      
      # MODIFICATION DE LA SELECTION DES OBJETS VIA LES TABLEAUX
      
      observeEvent(input$onglets_ac,{
        
        if(input$onglets_ac == "carte")
        {
          proxy <- leafletProxy("mymap_ac")
          
          clearGroup(map = proxy, group = "select_donnees")
          
          if(!is.null(input$mymaille_ac_rows_selected))
          {
            if(elargi_ac())
            {
              proxy <- addPolygons(map = proxy, data = fond_select_maille_elargi_ac(),
                                   stroke = TRUE, weight = 3,
                                   color="#FFFF00",
                                   options = pathOptions(pane = "selection", clickable = F),
                                   fill = F,
                                   group = "select_donnees"
              )
            }else
            {
              proxy <- addPolygons(map = proxy, data = fond_select_maille_ac(),
                                   stroke = TRUE, weight = 3,
                                   color="#FFFF00",
                                   options = pathOptions(pane = "selection", clickable = F),
                                   fill = F,
                                   group = "select_donnees"
              )
            }
          }
        }
      },ignoreInit = TRUE)
      
      observeEvent(input$onglets_ac,{
        
        if(input$onglets_ac == "carte")
        {
          proxy <- leafletProxy("mymap_ac")
          
          clearGroup(map = proxy, group = "select_contour")
          
          if(!is.null(input$mycontour_ac_rows_selected))
          {
            proxy <- addPolygons(map = proxy, data = fond_select_contour_ac(),
                                 stroke = TRUE, weight = 3,
                                 color="#FFFF00",
                                 options = pathOptions(pane = "selection", clickable = F),
                                 fill = F,
                                 group = "select_contour"
            )
          }
        }
      },ignoreInit = TRUE)
      
      # CONSTRUCTION DE LA LEGENDE
      
      lon_lat_ac <- reactive({
        click <- input$mymap_ac_click
        lon <- click$lng
        lat <- click$lat
        return(list(lon,lat))
      })
      
      observeEvent(list(input$mymap_ac_zoom,input$mymap_ac_click,input$type_legende_ac_id,input$titre_classes_legende_ac_id,input$nb_classes_ac_id,input$methode_ac_id,input$valid_bornes_ac_id),{
        if(is.null(input$affiche_legende_ac_id)) return(NULL)
        
        if(input$affiche_legende_ac_id==FALSE) return(NULL)
        
        if(is.null(lon_lat_ac()[[1]])) return(NULL)
        
        CONSTRUCTION_LEGENDE_AC()
      })
      
      CONSTRUCTION_LEGENDE_AC <- function()
      {
        proxy <- leafletProxy("mymap_ac")
        
        proxy <- clearGroup(map=proxy, group="leg")
        proxy <- clearMarkers(map=proxy)
        
        zoom <- as.numeric(input$mymap_ac_zoom)
        coeff <- ((360/(2^zoom))/7.2) # Permet de fixer une distance sur l'ecran. Il s'agit en gros d'une conversion des degres en pixels. Reste constant a longitude egale mais varie un peu selon la latitude
        
        position_leg <- t(data_frame(c(lon_lat_ac()[[1]],lon_lat_ac()[[2]])))
        
        if(is.null(input$type_legende_ac_id)) return(NULL)
        
        if(input$type_legende_ac_id==1) # Litterale
        {
          # On cree les rectangles
          if(is.null(input$nb_classes_ac_id))
            max_classes <- 4
          else
            max_classes <- input$nb_classes_ac_id
          
          for(i in 1:max_classes)
          {
            # Coordonnees du point haut/gauche des rectangles de la legende
            x_coord_rectangle <- position_leg[1]
            if(i==1) #1er rectangle
            {
              y_coord_rectangle <- position_leg[2]-coeff
            }else
            {
              y_coord_rectangle <- y_coord_rectangle-coeff*0.7
            }
            assign(paste0("rectangle_",i),list(matrix(c(x_coord_rectangle,y_coord_rectangle,x_coord_rectangle+coeff*1,y_coord_rectangle,x_coord_rectangle+coeff*1,y_coord_rectangle+coeff*0.5,x_coord_rectangle,y_coord_rectangle+coeff*0.5,x_coord_rectangle,y_coord_rectangle),ncol=2, byrow=TRUE)))
          }
          
          # On ajoute un cadre blanc autour de la legende
          y_coord_rectangle <- min(get(paste0("rectangle_",max_classes))[[1]][,2])
          
          # leaflet du cadre blanc en 1er
          proxy <- addRectangles(map = proxy,
                                 lng1 = position_leg[1]-coeff*0.5, lat1 = position_leg[2]+coeff*0.5, #x_titre_1 et y_titre_2
                                 lng2 = x_coord_rectangle+coeff*6, lat2 = y_coord_rectangle-coeff*0.8,
                                 stroke = TRUE,
                                 color = paste0("#2B3E50", ";background: #ffffff;
                                                border-left:2px solid #2B3E50;
                                                border-right:2px solid #2B3E50;
                                                border-top:2px solid #2B3E50;
                                                border-bottom:2px solid #2B3E50;
                                                border-radius: 5%"),
                                 weight = 1,
                                 options = pathOptions(pane = "fond_legende", clickable = F),
                                 fill = T,
                                 fillColor = "white",
                                 fillOpacity = 0.8,
                                 group="leg"
                                 )
          
          # leaflet rectangles et valeurs classes
          classes_leg_texte <- analyse_leg_ac()$rupture_classes
          
          legende$a <- c()
          for(i in 1: max_classes)
          {
            
            proxy <- addPolygons(map = proxy, data = st_polygon(get(paste0("rectangle_",i))),
                                 stroke = FALSE,
                                 options = pathOptions(pane = "fond_legende", clickable = F),
                                 fill = T,
                                 fillColor = analyse_leg_ac()$pal_classes[i],
                                 fillOpacity = 1,
                                 group="leg"
            )
            
            if(i==1)
            {
              label_rectangle <- paste0(format(round(classes_leg_texte[i+1],3), big.mark=" ",decimal.mark=",",nsmall=0)," et plus")
              
              proxy <- addLabelOnlyMarkers(map = proxy,
                                           lng = (max(get(paste0("rectangle_",i))[[1]][,1])+coeff*0.1), lat = mean(get(paste0("rectangle_",i))[[1]][,2]),
                                           label = label_rectangle,
                                           labelOptions = labelOptions(noHide = T, textOnly = TRUE, direction = "right",
                                                                       style = list(
                                                                         "color" = "black",
                                                                         "font-size" = "12px"
                                                                       )),
                                           group="leg"
              )
              legende$a <- c(legende$a,label_rectangle)
              
            }else if (i>1 && i<max_classes)
            {
              label_rectangle <- paste0("De ", format(round(classes_leg_texte[i+1],3), big.mark=" ",decimal.mark=",",nsmall=0)," \u00E0 moins de ", format(round(classes_leg_texte[i],3), big.mark=" ",decimal.mark=",",nsmall=0))
              
              proxy <- addLabelOnlyMarkers(map = proxy,
                                           lng = max(get(paste0("rectangle_",i))[[1]][,1])+coeff*0.1, lat = mean(get(paste0("rectangle_",i))[[1]][,2]),
                                           label = label_rectangle,
                                           labelOptions = labelOptions(noHide = T, textOnly = TRUE, direction = "right",
                                                                       style = list(
                                                                         "color" = "black",
                                                                         "font-size" = "12px"
                                                                       )),
                                           group="leg"
              )
              legende$a <- c(legende$a,label_rectangle)
              
            }else #i==length(max_classes)
            {
              label_rectangle <- paste0("Moins de ", format(round(classes_leg_texte[i],3), big.mark=" ",decimal.mark=",",nsmall=0))
              
              proxy <- addLabelOnlyMarkers(map = proxy,
                                           lng = max(get(paste0("rectangle_",i))[[1]][,1])+coeff*0.1, lat = mean(get(paste0("rectangle_",i))[[1]][,2]),
                                           label = label_rectangle,
                                           labelOptions = labelOptions(noHide = T, textOnly = TRUE, direction = "right",
                                                                       style = list(
                                                                         "color" = "black",
                                                                         "font-size" = "12px"
                                                                       )),
                                           group="leg"
              )
              legende$a <- c(legende$a,label_rectangle)
              
            }
          }
          
          # On ajoute la legende de classes a l'analyse
          
          # leaflet titre 2
          x_titre_2 <- min(st_coordinates(st_polygon(get("rectangle_1")))[,"X"])
          y_titre_2 <- max(st_coordinates(st_polygon(get("rectangle_1")))[,"Y"])+coeff*0.2
          
          proxy <- addLabelOnlyMarkers(map = proxy,
                                       lng = x_titre_2, lat = y_titre_2,
                                       label = input$titre_classes_legende_ac_id,
                                       labelOptions = labelOptions(noHide = T, textOnly = TRUE, direction = "right",
                                                                   style = list(
                                                                     "color" = "black",
                                                                     "font-size" = "14px"
                                                                   )),
                                       group="leg"
          )
        }
        
        if(input$type_legende_ac_id==2) # Numerique
        {
          # On cree les rectangles
          if(is.null(input$nb_classes_ac_id)) return(NULL)
          
          max_classes <- input$nb_classes_ac_id
          
          for(i in 1:max_classes)
          {
            # Coordonnees du point haut/gauche des rectangles de la legende
            x_coord_rectangle <- position_leg[1]
            if(i==1) #1er rectangle
            {
              y_coord_rectangle <- position_leg[2]-coeff
            }else
            {
              y_coord_rectangle <- y_coord_rectangle-coeff*0.5
            }
            assign(paste0("rectangle_",i),list(matrix(c(x_coord_rectangle,y_coord_rectangle,x_coord_rectangle+coeff*1,y_coord_rectangle,x_coord_rectangle+coeff*1,y_coord_rectangle+coeff*0.5,x_coord_rectangle,y_coord_rectangle+coeff*0.5,x_coord_rectangle,y_coord_rectangle),ncol=2, byrow=TRUE)))
          }
          
          # On ajoute un cadre blanc autour de la legende
          y_coord_rectangle <- min(get(paste0("rectangle_",max_classes))[[1]][,2])
          
          # leaflet du cadre blanc en 1er
          proxy <- addRectangles(map = proxy,
                                 lng1 = position_leg[1]-coeff*0.5, lat1 = position_leg[2]+coeff*0.5, #x_titre_1 et y_titre_2
                                 lng2 = x_coord_rectangle+coeff*6, lat2 = y_coord_rectangle-coeff*0.8,
                                 stroke = TRUE,
                                 color = paste0("#2B3E50", ";background: #ffffff;
                                                border-left:2px solid #2B3E50;
                                                border-right:2px solid #2B3E50;
                                                border-top:2px solid #2B3E50;
                                                border-bottom:2px solid #2B3E50;
                                                border-radius: 5%"),
                                 weight = 1,
                                 options = pathOptions(pane = "fond_legende", clickable = F),
                                 fill = T,
                                 fillColor = "white",
                                 fillOpacity = 0.8,
                                 group="leg"
                                 )
          
          # leaflet rectangles et valeurs classes
          classes_leg_num <- analyse_leg_ac()$rupture_classes
          
          for(i in 1: max_classes)
          {
            proxy <- addPolygons(map = proxy, data = st_polygon(get(paste0("rectangle_",i))),
                                 stroke = FALSE,
                                 options = pathOptions(pane = "fond_legende", clickable = F),
                                 fill = T,
                                 fillColor = analyse_leg_ac()$pal_classes[i],
                                 fillOpacity = 1,
                                 group="leg"
            )
            
            if(i<max_classes)
            {
              x1 <- max(get(paste0("rectangle_",i))[[1]][,1])
              y1 <- min(get(paste0("rectangle_",i))[[1]][,2])
              x2 <- max(get(paste0("rectangle_",i))[[1]][,1])+coeff*0.2
              y2 <- min(get(paste0("rectangle_",i))[[1]][,2])
              ligne <- st_linestring(rbind(c(x1,y1),c(x2,y2)))
              
              proxy <- addPolygons(map = proxy, data = ligne,
                                   color = "black",
                                   weight = 1,
                                   options = pathOptions(pane = "fond_legende", clickable = F),
                                   fill = F,
                                   fillOpacity = 1,
                                   group="leg"
              )
              
              proxy <- addLabelOnlyMarkers(map = proxy,
                                           lng = x2, lat = y2,
                                           label = as.character(format(round(classes_leg_num[i+1],3),big.mark=" ",decimal.mark=",",nsmall=0)),
                                           labelOptions = labelOptions(noHide = T, textOnly = TRUE, direction = "right",
                                                                       style = list(
                                                                         "color" = "black",
                                                                         "font-size" = "12px"
                                                                       )),
                                           group="leg"
              )
              
            }
          }
          
          # On ajoute la legende de classes a l'analyse
          
          # leaflet titre 2
          x_titre_2 <- min(get("rectangle_1")[[1]][,1])
          y_titre_2 <- max(get("rectangle_1")[[1]][,2])+coeff*0.2
          
          proxy <- addLabelOnlyMarkers(map = proxy,
                                       lng = x_titre_2, lat = y_titre_2,
                                       label = input$titre_classes_legende_ac_id,
                                       labelOptions = labelOptions(noHide = T, textOnly = TRUE, direction = "right",
                                                                   style = list(
                                                                     "color" = "black",
                                                                     "font-size" = "14px"
                                                                   )),
                                       group="leg"
          )
        }
      }
      
      # AJOUT DES ONGLETS SAUVEGARDE
      
      observeEvent(input$save_carte_ac_id,{
        
        showModal(modalDialog(HTML("<i class=\"fa fa-spinner fa-spin fa-2x fa-fw\"></i><font size=+1>Sauvegarde de la carte en cours...</font> "), size="m", footer=NULL, style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"))
        
        insert_save$a <- insert_save$a + 1
        nb_save_carte <- insert_save$a-remove_carte$a
        
        m_save <- m_save_ac$a
        
        if(nb_save_carte>6)
        {
          insert_save$a <- insert_save$a - 1
          showModal(modalDialog(HTML("<font size=+1>Vous ne pouvez pas sauvegarger plus de 6 cartes. Veuillez en supprimer avant de continuer.</font> "), size="l", footer=NULL, easyClose = TRUE, style = "color: #fff; background-color: #DF691A; border-color: #2e6da4")) #337ab7
          return(NULL)
        }
        
        output[[paste0("mymap_save_",insert_save$a,"_ac")]] <- renderLeaflet({
          
          if(!is.null(fondSuppl))
          {
            if(input$ajout_territoire_ac_id)
            {
              m_save <- addPolygons(map = m_save, data = fond_territoire_ac(),
                                    stroke = TRUE, color = "#BFBFBF", opacity = 1,
                                    weight = 0.5,
                                    options = pathOptions(pane = "fond_territoire", clickable = T),
                                    popup = paste0("<b> <font color=#2B3E50>",as.data.frame(fond_territoire_ac())[,"LIBELLE"], "</font> </b>"),
                                    fill = T, fillColor = "white", fillOpacity = 0.001
              )
            }
          }
          
          if(input$ajout_reg_ac_id)
          {
            m_save <- addPolygons(map = m_save, data = fond_region_ac(),
                                  stroke = TRUE, color = "grey", opacity = 1,
                                  weight = 1.5,
                                  options = pathOptions(pane = "fond_reg", clickable = F),
                                  fill = F
            )
          }
          
          if(input$ajout_dep_ac_id)
          {
            m_save <- addPolygons(map = m_save, data = fond_departement_ac(),
                                  stroke = TRUE, color = "grey", opacity = 1,
                                  weight = 0.5,
                                  options = pathOptions(pane = "fond_dep", clickable = F),
                                  fill = F
            )
          }
          
          i <- 1 #pour gerer l'ordre des fonds dans le pane
          
          for(fond in liste_fonds_ac$a)
          {
            if(fond=="analyse/maille")
            {
              suppressWarnings(test_analyse_maille_classe <- try(analyse_ac()$donnees[rev(order(analyse_ac()[[1]][,varRatio])),varRatio],silent=T))
              if(class(test_analyse_maille_classe) %in% "try-error")
              {
                return(NULL)
              }else
              {
                analyse_maille_classe <- analyse_ac()$donnees[rev(order(analyse_ac()[[1]][,varRatio])),varRatio]
              }
              
              analyse_maille <- merge(fond_contour_maille_ac()[[2]][,c("CODE","geometry")],analyse_ac()$donnees[,c("CODE","LIBELLE",varRatio,"TXT1")],by="CODE")
              names(analyse_maille)[3] <- varRatio
              analyse_maille <- analyse_maille[rev(order(as.data.frame(analyse_maille)[,varRatio])),]
              analyse_maille <- st_sf(analyse_maille,stringsAsFactors = FALSE)
              
              m_save <- addPolygons(map = m_save, data = analyse_maille, opacity = 1,
                                    stroke = TRUE, color = "white", weight = 1,
                                    options = pathOptions(pane = paste0("fond_duo",i), clickable = T),
                                    popup = paste0("<b> <font color=#2B3E50>",as.data.frame(analyse_maille)$LIBELLE, "</font> </b><br><b><font color=#2B3E50>",varRatio," : </font></b>",as.data.frame(analyse_maille)$TXT1),
                                    fill = T,
                                    fillColor = palette_ac()[[1]](analyse_maille_classe),
                                    fillOpacity = 1
              )
            }
            
            if(fond=="contour")
            {
              m_save <- addPolygons(map = m_save, data = fond_contour_maille_ac()[[1]], opacity = 0.3, #contour_WGS84
                                    stroke = TRUE, color = "black", weight = 3,
                                    options = pathOptions(pane = paste0("fond_duo",i), clickable = T),
                                    popup = paste0("<b> <font color=#2B3E50>",as.data.frame(fond_contour_maille_ac()[[1]])[,"LIBELLE"], "</font> </b>"),
                                    fill = T, fillColor = "white", fillOpacity = 0.3
              )
            }
            
            i <- i + 1
          }
          
          if(elargi_ac())
          {
            analyse_maille_classe_elargi <- analyse_ac()$donnees_elargi[rev(order(analyse_ac()$donnees_elargi[,varRatio])),varRatio]
            
            analyse_maille_elargi <- merge(fond_elargi_ac()[,c("CODE","geometry")],analyse_ac()$donnees_elargi[,c("CODE","LIBELLE",varRatio,"TXT1")],by="CODE")
            names(analyse_maille_elargi)[3] <- varRatio
            analyse_maille_elargi <- analyse_maille_elargi[rev(order(as.data.frame(analyse_maille_elargi)[,varRatio])),]
            analyse_maille_elargi <- st_sf(analyse_maille_elargi,stringsAsFactors = FALSE)
            
            m_save <- addPolygons(map = m_save, data = analyse_maille_elargi, opacity = input$opacite_elargi_ac_id/100,
                                  stroke = TRUE, color = "white", weight = 1,
                                  options = pathOptions(pane = "fond_duo2", clickable = T),
                                  popup = paste0("<b> <font color=#2B3E50>",as.data.frame(analyse_maille_elargi)$LIBELLE, "</font> </b><br><b><font color=#2B3E50>",varRatio," : </font></b>",as.data.frame(analyse_maille_elargi)$TXT1),
                                  fill = T,
                                  fillColor = palette_ac()[[1]](analyse_maille_classe_elargi),
                                  fillOpacity = input$opacite_elargi_ac_id/100
            )
          }
          
          zoom <- as.numeric(input$mymap_ac_zoom)
          coeff <- ((360/(2^zoom))/7.2) # Permet de fixer une distance sur l'ecran. Il s'agit en gros d'une conversion des degres en pixels. Reste constant a longitude egale mais varie un peu selon la latitude
          
          position_leg <- t(data_frame(c(lon_lat_ac()[[1]],lon_lat_ac()[[2]])))
          
          if(input$type_legende_ac_id==1) # Litterale
          {
            # On cree les rectangles
            max_classes <- input$nb_classes_ac_id
            
            for(i in 1:max_classes)
            {
              # Coordonnees du point haut/gauche des rectangles de la legende
              x_coord_rectangle <- position_leg[1]
              if(i==1) #1er rectangle
              {
                y_coord_rectangle <- position_leg[2]-coeff
              }else
              {
                y_coord_rectangle <- y_coord_rectangle-coeff*0.7
              }
              assign(paste0("rectangle_",i),list(matrix(c(x_coord_rectangle,y_coord_rectangle,x_coord_rectangle+coeff*1,y_coord_rectangle,x_coord_rectangle+coeff*1,y_coord_rectangle+coeff*0.5,x_coord_rectangle,y_coord_rectangle+coeff*0.5,x_coord_rectangle,y_coord_rectangle),ncol=2, byrow=TRUE)))
            }
            
            # On ajoute un cadre blanc autour de la legende
            y_coord_rectangle <- min(get(paste0("rectangle_",max_classes))[[1]][,2])
            
            # leaflet du cadre blanc en 1er
            m_save <- addRectangles(map = m_save,
                                    lng1 = position_leg[1]-coeff*0.5, lat1 = position_leg[2]+coeff*0.5, #x_titre_1 et y_titre_2
                                    lng2 = x_coord_rectangle+coeff*6, lat2 = y_coord_rectangle-coeff*0.8,
                                    stroke = TRUE,
                                    color = paste0("#2B3E50", ";background: #ffffff;
                                                   border-left:2px solid #2B3E50;
                                                   border-right:2px solid #2B3E50;
                                                   border-top:2px solid #2B3E50;
                                                   border-bottom:2px solid #2B3E50;
                                                   border-radius: 5%"),
                                    weight = 1,
                                    options = pathOptions(pane = "fond_legende", clickable = F),
                                    fill = T,
                                    fillColor = "white",
                                    fillOpacity = 0.8
                                    )
            
            # leaflet rectangles et valeurs classes
            classes_leg_texte <- analyse_leg_ac()$rupture_classes
            
            for(i in 1: max_classes)
            {
              
              m_save <- addPolygons(map = m_save, data = st_polygon(get(paste0("rectangle_",i))),
                                    stroke = FALSE,
                                    options = pathOptions(pane = "fond_legende", clickable = F),
                                    fill = T,
                                    fillColor = analyse_leg_ac()$pal_classes[i],
                                    fillOpacity = 1
              )
              
              if(i==1)
              {
                label_rectangle <- paste0(format(round(classes_leg_texte[i+1],3), big.mark=" ",decimal.mark=",",nsmall=0)," et plus")
                
                m_save <- addLabelOnlyMarkers(map = m_save,
                                              lng = (max(get(paste0("rectangle_",i))[[1]][,1])+coeff*0.1), lat = mean(get(paste0("rectangle_",i))[[1]][,2]),
                                              label = label_rectangle,
                                              labelOptions = labelOptions(noHide = T, textOnly = TRUE, direction = "right",
                                                                          style = list(
                                                                            "color" = "black",
                                                                            "font-size" = "12px"
                                                                          ))
                )
                
              }else if (i>1 && i<max_classes)
              {
                label_rectangle <- paste0("De ", format(round(classes_leg_texte[i+1],3), big.mark=" ",decimal.mark=",",nsmall=0)," \u00E0 moins de ", format(round(classes_leg_texte[i],3), big.mark=" ",decimal.mark=",",nsmall=0))
                
                m_save <- addLabelOnlyMarkers(map = m_save,
                                              lng = max(get(paste0("rectangle_",i))[[1]][,1])+coeff*0.1, lat = mean(get(paste0("rectangle_",i))[[1]][,2]),
                                              label = label_rectangle,
                                              labelOptions = labelOptions(noHide = T, textOnly = TRUE, direction = "right",
                                                                          style = list(
                                                                            "color" = "black",
                                                                            "font-size" = "12px"
                                                                          ))
                )
                
              }else #i==length(max_classes)
              {
                label_rectangle <- paste0("Moins de ", format(round(classes_leg_texte[i],3), big.mark=" ",decimal.mark=",",nsmall=0))
                
                m_save <- addLabelOnlyMarkers(map = m_save,
                                              lng = max(get(paste0("rectangle_",i))[[1]][,1])+coeff*0.1, lat = mean(get(paste0("rectangle_",i))[[1]][,2]),
                                              label = label_rectangle,
                                              labelOptions = labelOptions(noHide = T, textOnly = TRUE, direction = "right",
                                                                          style = list(
                                                                            "color" = "black",
                                                                            "font-size" = "12px"
                                                                          ))
                )
              }
            }
            
            # leaflet titre 2
            x_titre_2 <- min(st_coordinates(st_polygon(get("rectangle_1")))[,"X"])
            y_titre_2 <- max(st_coordinates(st_polygon(get("rectangle_1")))[,"Y"])+coeff*0.2
            
            m_save <- addLabelOnlyMarkers(map = m_save,
                                          lng = x_titre_2, lat = y_titre_2,
                                          label = input$titre_classes_legende_ac_id,
                                          labelOptions = labelOptions(noHide = T, textOnly = TRUE, direction = "right",
                                                                      style = list(
                                                                        "color" = "black",
                                                                        "font-size" = "14px"
                                                                      ))
            )
          }
          
          if(input$type_legende_ac_id==2) # Numerique
          {
            # On cree les rectangles
            if(is.null(input$nb_classes_ac_id)) return(NULL)
            
            max_classes <- input$nb_classes_ac_id
            
            for(i in 1:max_classes)
            {
              # Coordonnees du point haut/gauche des rectangles de la legende
              x_coord_rectangle <- position_leg[1]
              if(i==1) #1er rectangle
              {
                y_coord_rectangle <- position_leg[2]-coeff
              }else
              {
                y_coord_rectangle <- y_coord_rectangle-coeff*0.5
              }
              assign(paste0("rectangle_",i),list(matrix(c(x_coord_rectangle,y_coord_rectangle,x_coord_rectangle+coeff*1,y_coord_rectangle,x_coord_rectangle+coeff*1,y_coord_rectangle+coeff*0.5,x_coord_rectangle,y_coord_rectangle+coeff*0.5,x_coord_rectangle,y_coord_rectangle),ncol=2, byrow=TRUE)))
            }
            
            # On ajoute un cadre blanc autour de la legende
            y_coord_rectangle <- min(get(paste0("rectangle_",max_classes))[[1]][,2])
            
            # leaflet du cadre blanc en 1er
            m_save <- addRectangles(map = m_save,
                                    lng1 = position_leg[1]-coeff*0.5, lat1 = position_leg[2]+coeff*0.5, #x_titre_1 et y_titre_2
                                    lng2 = x_coord_rectangle+coeff*6, lat2 = y_coord_rectangle-coeff*0.8,
                                    stroke = TRUE,
                                    color = paste0("#2B3E50", ";background: #ffffff;
                                                   border-left:2px solid #2B3E50;
                                                   border-right:2px solid #2B3E50;
                                                   border-top:2px solid #2B3E50;
                                                   border-bottom:2px solid #2B3E50;
                                                   border-radius: 5%"),
                                    weight = 1,
                                    options = pathOptions(pane = "fond_legende", clickable = F),
                                    fill = T,
                                    fillColor = "white",
                                    fillOpacity = 0.8
                                    )
            
            # leaflet rectangles et valeurs classes
            classes_leg_num <- analyse_leg_ac()$rupture_classes
            
            for(i in 1: max_classes)
            {
              m_save <- addPolygons(map = m_save, data = st_polygon(get(paste0("rectangle_",i))),
                                    stroke = FALSE,
                                    options = pathOptions(pane = "fond_legende", clickable = F),
                                    fill = T,
                                    fillColor = analyse_leg_ac()$pal_classes[i],
                                    fillOpacity = 1
              )
              
              if(i<max_classes)
              {
                x1 <- max(get(paste0("rectangle_",i))[[1]][,1])
                y1 <- min(get(paste0("rectangle_",i))[[1]][,2])
                x2 <- max(get(paste0("rectangle_",i))[[1]][,1])+coeff*0.2
                y2 <- min(get(paste0("rectangle_",i))[[1]][,2])
                ligne <- st_linestring(rbind(c(x1,y1),c(x2,y2)))
                
                m_save <- addPolygons(map = m_save, data = ligne,
                                      color = "black",
                                      weight = 1,
                                      options = pathOptions(pane = "fond_legende", clickable = F),
                                      fill = F,
                                      fillOpacity = 1
                )
                
                m_save <- addLabelOnlyMarkers(map = m_save,
                                              lng = x2, lat = y2,
                                              label = as.character(format(round(classes_leg_num[i+1],3),big.mark=" ",decimal.mark=",",nsmall=0)),
                                              labelOptions = labelOptions(noHide = T, textOnly = TRUE, direction = "right",
                                                                          style = list(
                                                                            "color" = "black",
                                                                            "font-size" = "12px"
                                                                          ))
                )
                
              }
            }
            
            # On ajoute la legende de classes a l'analyse
            
            # leaflet titre 2
            x_titre_2 <- min(get("rectangle_1")[[1]][,1])
            y_titre_2 <- max(get("rectangle_1")[[1]][,2])+coeff*0.2
            
            m_save <- addLabelOnlyMarkers(map = m_save,
                                          lng = x_titre_2, lat = y_titre_2,
                                          label = input$titre_classes_legende_ac_id,
                                          labelOptions = labelOptions(noHide = T, textOnly = TRUE, direction = "right",
                                                                      style = list(
                                                                        "color" = "black",
                                                                        "font-size" = "14px"
                                                                      ))
            )
          }
          
          removeModal()
          
          m_save
        })
        
        output[[paste0("remove_carte_",nb_save_carte,"_ac")]] <- renderUI({
          actionButton(paste0("remove_carte_",nb_save_carte,"_ac_id"),label="X Supprimer la carte", style="color:#FFFFFF; border-color:#FFFFFF; background-color:#2B3E50")
        })
        
        appendTab(inputId = "onglets_ac",
                  tabPanel(title=HTML(paste0("<font color=#AFC9E0> Carte ",insert_save$a,"</font>")),value=paste0("carte",nb_save_carte),uiOutput(paste0("remove_carte_",nb_save_carte,"_ac")),leafletOutput(paste0("mymap_save_",insert_save$a,"_ac"),width="100%",height = 800)),
                  select = TRUE,
                  session = session
        )
        
      }, ignoreInit = TRUE)
      
      observeEvent(input$remove_carte_1_ac_id,{
        remove_carte$a <- remove_carte$a + 1
        removeTab(inputId = "onglets_ac",
                  target = "carte1",
                  session = session
        )
      }, ignoreInit = TRUE)
      
      observeEvent(input$remove_carte_2_ac_id,{
        remove_carte$a <- remove_carte$a + 1
        removeTab(inputId = "onglets_ac",
                  target = "carte2",
                  session = session
        )
      }, ignoreInit = TRUE)
      
      observeEvent(input$remove_carte_3_ac_id,{
        remove_carte$a <- remove_carte$a + 1
        removeTab(inputId = "onglets_ac",
                  target = "carte3",
                  session = session
        )
      }, ignoreInit = TRUE)
      
      observeEvent(input$remove_carte_4_ac_id,{
        remove_carte$a <- remove_carte$a + 1
        removeTab(inputId = "onglets_ac",
                  target = "carte4",
                  session = session
        )
      }, ignoreInit = TRUE)
      
      observeEvent(input$remove_carte_5_ac_id,{
        remove_carte$a <- remove_carte$a + 1
        removeTab(inputId = "onglets_ac",
                  target = "carte5",
                  session = session
        )
      }, ignoreInit = TRUE)
      
      observeEvent(input$remove_carte_6_ac_id,{
        remove_carte$a <- remove_carte$a + 1
        removeTab(inputId = "onglets_ac",
                  target = "carte6",
                  session = session
        )
      }, ignoreInit = TRUE)
      
      # TABLEAUX DE DONNEES, MAILLE ET CONTOUR
      
      output$mymaille_ac <- DT::renderDataTable(datatable({
        if(elargi_ac())
          data <- analyse_ac()$donnees_elargi
        else
          data <- analyse_ac()$donnees
        
        tableau_maille <- data[order(data$CODE),c("CODE","LIBELLE",varRatio)]
      },  style = 'bootstrap'
      ))
      
      output$mycontour_ac <- DT::renderDataTable(datatable({
        data <- as.data.frame(fondContour)
        tableau_contour <- data[order(data$CODE),c(1:2)]
      },  style = 'bootstrap'
      ))
      
      # ENVOI DU LEAFLET A L'UI
      
      output$mymap_ac <- renderLeaflet({
        react_fond_ac()
      })
      
    }
    
    runApp(shinyApp(ui = ui, server = server), launch.browser = TRUE)
    }


shiny_ronds_classes <-
  function(data,fondMaille,fondMailleElargi=NULL,fondContour,fondSuppl=NULL,idData,varVolume,varRatio,dom="0",fondChx=NULL)
  {
    options("stringsAsFactors"=FALSE)
    
    # Verification des parametres
    msg_error1<-msg_error2<-msg_error3<-msg_error4<-msg_error5<-msg_error6<-msg_error7<-msg_error8<-msg_error9<-msg_error10<-msg_error11<-msg_error12<-msg_error13<-msg_error14<-msg_error15<-msg_error16<-msg_error17<-msg_error18<-msg_error19 <- NULL
    
    if(any(class(data)!="data.frame")) msg_error1 <- "Les donnees doivent etre dans un data.frame / "
    if(any(!any(class(fondMaille) %in% "sf"),!any(class(fondMaille) %in% "data.frame"))) msg_error2 <- "Le fond de maille doit etre un objet sf / "
    if(!is.null(fondMailleElargi)) if(any(!any(class(fondMailleElargi) %in% "sf"),!any(class(fondMailleElargi) %in% "data.frame"))) msg_error3 <- "Le fond de maille elargie doit etre un objet sf / "
    if(any(!any(class(fondContour) %in% "sf"),!any(class(fondContour) %in% "data.frame"))) msg_error4 <- "Le fond de contour doit etre un objet sf / "
    if(!is.null(fondSuppl)) if(any(!any(class(fondSuppl) %in% "sf"),!any(class(fondSuppl) %in% "data.frame"))) msg_error5 <- "Le fond supplementaire doit etre un objet sf / "
    if(any(class(idData)!="character")) msg_error6 <- "Le nom de la variable doit etre de type caractere / "
    if(any(class(varVolume)!="character")) msg_error7 <- "Le nom de la variable doit etre de type caractere / "
    if(any(class(varRatio)!="character")) msg_error8 <- "Le nom de la variable doit etre de type caractere / "
    if(any(class(dom)!="character")) msg_error9 <- "La valeur doit etre de type caractere ('0', '971', '972', '973', '974' ou '976') / "
    if(!is.null(fondChx)) if(any(!any(class(fondChx) %in% "sf"),!any(class(fondChx) %in% "data.frame"))) msg_error10 <- "Le fond des chx doit etre un objet sf / "
    
    if(length(names(data))<3) msg_error11 <- "Le tableau des donnees n'est pas conforme. Il doit contenir au minimum une variable identifiant et les 2 variables a representer / "
    if(length(names(fondMaille))<3) msg_error12 <- "Le fond de maille n'est pas conforme. La table doit contenir au minimum une variable identifiant, une variable libelle et la geometry / "
    if(!is.null(fondMailleElargi)) if(length(names(fondMailleElargi))<3) msg_error13 <- "Le fond de maille elargie n'est pas conforme. La table doit contenir au minimum une variable identifiant, une variable libelle et la geometry / "
    if(length(names(fondContour))<3) msg_error14 <- "Le fond de contour n'est pas conforme. La table doit contenir au minimum une variable identifiant, une variable libelle et la geometry / "
    if(!is.null(fondSuppl)) if(length(names(fondSuppl))<3) msg_error15 <- "Le fond supplementaire n'est pas conforme. La table doit contenir au minimum une variable identifiant, une variable libelle et la geometry / "
    
    if(!any(names(data) %in% idData))  msg_error16 <- "La variable identifiant les donnees n'existe pas dans la table des donnees / "
    if(!any(names(data) %in% varVolume))  msg_error17 <- "La variable a representer n'existe pas dans la table des donnees / "
    if(!any(names(data) %in% varRatio))  msg_error18 <- "La variable a representer n'existe pas dans la table des donnees / "
    if(!dom %in% c("0","971","972","973","974","976")) msg_error19 <- "La variable dom doit etre '0', '971', '972', '973', '974' ou '976' / "
    
    if(any(!is.null(msg_error1),!is.null(msg_error2),!is.null(msg_error3),!is.null(msg_error4),
           !is.null(msg_error5),!is.null(msg_error6),!is.null(msg_error7),!is.null(msg_error8),
           !is.null(msg_error9),!is.null(msg_error10),!is.null(msg_error11),!is.null(msg_error12),
           !is.null(msg_error13),!is.null(msg_error14),!is.null(msg_error15),!is.null(msg_error16),
           !is.null(msg_error17),!is.null(msg_error18),!is.null(msg_error18)))
    {
      stop(simpleError(paste0(msg_error1,msg_error2,msg_error3,msg_error4,msg_error5,msg_error6,msg_error7,msg_error8,
                              msg_error9,msg_error10,msg_error11,msg_error12,msg_error13,msg_error14,msg_error15,msg_error16,msg_error17,msg_error18,msg_error19)))
    }
    
    nb_up <- reactiveValues(a=0)
    nb_down <- reactiveValues(a=0)
    ordre_analyse <- reactiveValues(a=1,b=2)
    insert_save <- reactiveValues(a=0)
    remove_carte <- reactiveValues(a=0)
    liste_fonds <- reactiveValues(a=c("analyse","maille","contour"))
    m_save_rp_ac <- reactiveValues(a=0)
    
    erreur_maille <- reactiveValues(a=FALSE)
    
    max_classes <-  reactiveValues(a=4)
    methode_calcul <- c("fisher","jenks","kmeans","quantile","manuel")
    
    legende <- reactiveValues(a=NULL)
    
    sourc <- "Source : Insee"
    
    names(data)[names(data)==idData] <- "CODE"
    names(fondMaille)[1] <- "CODE"
    names(fondMaille)[2] <- "LIBELLE"
    names(fondContour)[1] <- "CODE"
    names(fondContour)[2] <- "LIBELLE"
    if(!is.null(fondMailleElargi)) 
    {
      names(fondMailleElargi)[1] <- "CODE"
      names(fondMailleElargi)[2] <- "LIBELLE"
      fondMailleElargi$LIBELLE<-iconv(fondMailleElargi$LIBELLE,"latin1","utf8")
    }
    if(!is.null(fondSuppl)) 
    {
      names(fondSuppl)[1] <- "CODE"
      names(fondSuppl)[2] <- "LIBELLE"
      fondSuppl$LIBELLE<-iconv(fondSuppl$LIBELLE,"latin1","utf8")
    }
    
    fondMaille$LIBELLE<-iconv(fondMaille$LIBELLE,"latin1","utf8")
    fondContour$LIBELLE<-iconv(fondContour$LIBELLE,"latin1","utf8")
    
    ui <- navbarPage("OCEANIS", id="menu",
                     
                     theme = shinytheme("superhero"),
                     
                     tabPanel("Carte",value="carte",
                              sidebarLayout(
                                
                                sidebarPanel(width = 3,
                                             style = "overflow-y:scroll; min-height: 840px; max-height: 840px",
                                             h4(HTML("<b><font color=#95BAE2>VARIABLES</font></b>")),
                                             uiOutput("variable_rond_rp_ac"),
                                             uiOutput("variable_classe_rp_ac"),
                                             tags$hr(style="border: 5px solid #5182B6"), #337ab7
                                             h4(HTML("<b><font color=#95BAE2>FONDS</font></b>")),
                                             fluidRow(
                                               column(width=9, offset=0.5,
                                                      uiOutput("ordre_fonds_rp_ac")
                                               ),
                                               column(width=1,
                                                      br(),
                                                      br(),
                                                      htmlOutput("monter_fond_rp_ac", inline=FALSE),
                                                      htmlOutput("descendre_fond_rp_ac", inline=FALSE)
                                               )
                                             ),
                                             uiOutput("elargi_rp_ac"),
                                             conditionalPanel(condition = 'input.elargi_rp_ac_id',
                                                              uiOutput("opacite_elargi_rp_ac")
                                             ),
                                             uiOutput("ajout_territoire_rp_ac"),
                                             uiOutput("ajout_reg_rp_ac"),
                                             uiOutput("ajout_dep_rp_ac"),
                                             tags$hr(style="border: 5px solid #5182B6"),
                                             h4(HTML("<b><font color=#95BAE2>RONDS</font></b>")),
                                             uiOutput("taille_rond_rp_ac"),
                                             htmlOutput("info_taille_max_rond_rp_ac"),
                                             htmlOutput("info_rapport_rond_rp_ac"),
                                             uiOutput("rapport_rond_rp_ac"),
                                             conditionalPanel(condition = 'input.rapport_rond_rp_ac_id',
                                                              uiOutput("valeur_rapport_rond_rp_ac"),
                                                              htmlOutput("info_rapport_max_rond_rp_ac")
                                             ),
                                             uiOutput("choix_centroid_rp_ac"),
                                             tags$hr(style="border: 5px solid #5182B6"),
                                             h4(HTML("<b><font color=#95BAE2>CLASSES</font></b>")),
                                             uiOutput("liste_classes_rp_ac"),
                                             uiOutput("methode_rp_ac"),
                                             uiOutput("distribution_variable_rp_ac"),
                                             conditionalPanel(condition = 'input.distribution_variable_rp_ac_id',
                                                              verticalLayout(
                                                                wellPanel(
                                                                  style="background: #EDF2F9; width:340px",
                                                                  plotOutput("distribution_rp_ac"),
                                                                  br(),
                                                                  uiOutput("slider_bornes_rp_ac"),
                                                                  uiOutput("valid_slider_bornes_rp_ac")
                                                                )
                                                              )
                                             ),
                                             conditionalPanel(condition = 'input.methode_rp_ac_id=="manuel"',
                                                              uiOutput("zone_bornes_rp_ac"),
                                                              uiOutput("valid_bornes_rp_ac")
                                             ),
                                             tags$hr(style="border: 5px solid #5182B6"),
                                             h4(HTML("<b><font color=#95BAE2>L\u00c9GENDE</font></b>")),
                                             uiOutput("titre_ronds_legende_rp_ac"),
                                             uiOutput("titre_classes_legende_rp_ac"),
                                             br(),
                                             uiOutput("affiche_legende_rp_ac"),
                                             uiOutput("type_legende_rp_ac"),
                                             br(),
                                             tags$hr(style="border: 5px solid #5182B6"),
                                             h4(HTML("<b><font color=#95BAE2>SAUVEGARDE</font></b>")),
                                             uiOutput("save_carte_rp_ac"),
                                             br(),
                                             conditionalPanel(condition = 'input.mymap_rp_ac_click',
                                                              tags$div(class="dropup",
                                                                       HTML('<button class="btn btn-primary dropdown-toggle" type="button" data-toggle="dropdown" aria-haspopup="true" aria-expanded="false">
                                                                            Exporter en projet Qgis
                                                                            <span class="caret"></span>
                                                                            </button>'),
                                                                       tags$ul(class="dropdown-menu",
                                                                               wellPanel(
                                                                                 style="background: #2B3E50; width:340px",
                                                                                 h4("Export de la carte en projet Qgis"),
                                                                                 br(),
                                                                                 uiOutput("sortie_qgis_rp_ac"),
                                                                                 br(),
                                                                                 uiOutput("titre1_qgis_rp_ac"),
                                                                                 uiOutput("titre2_qgis_rp_ac"),
                                                                                 uiOutput("source_qgis_rp_ac"),
                                                                                 tags$head(tags$style(HTML('#export_qgis_rp_ac_id{background-color:#337ab7}'))),
                                                                                 uiOutput("export_qgis_rp_ac")
                                                                               )
                                                                       )
                                                                       )
                                                              
                                             ),
                                             br(),
                                             uiOutput("aide_image_rp_ac"),
                                             br()
                              ),
                              mainPanel(
                                tags$head(
                                  tags$style(HTML(".leaflet-container { background: #AFC9E0; }"))
                                ),
                                tabsetPanel(id="onglets_rp_ac",
                                            tabPanel(title=HTML("<b>Carte</b>"),value="carte",
                                                     leafletOutput("mymap_rp_ac",width="100%",height = 800)
                                            ),
                                            tabPanel(title=HTML(paste0("<b>Donn","\u00e9","es</b>")),value="donnees",
                                                     h5("S\u00e9lectionnez une ou plusieurs lignes pour ensuite les visualiser sur la carte."),
                                                     DT::dataTableOutput("mydonnees_rp_ac",width="100%",height = 800)),
                                            tabPanel(title=HTML("<b>Maille</b>"),value="maille",
                                                     h5("S\u00e9lectionnez une ou plusieurs lignes pour ensuite les visualiser sur la carte."),
                                                     DT::dataTableOutput("mymaille_rp_ac",width="100%",height = 800)),
                                            tabPanel(title=HTML("<b>Contour</b>"),value="contour",
                                                     h5("S\u00e9lectionnez une ou plusieurs lignes pour ensuite les visualiser sur la carte."),
                                                     DT::dataTableOutput("mycontour_rp_ac",width="100%",height = 800))
                                )
                              )
    )
    )
    )
    
    server <- function(input, output, session) {
      
      #################
      #Onglet Carte
      #################
      
      #Charge les donnees et fonds en memoire et affiche les widgets adequats dans le sidePanel de l'onglet "Carte"
      observe({
        
        # VARIABLES
        
        output$variable_rond_rp_ac <- renderUI({
          selectInput("variable_rond_rp_ac_id", label=h5("Variable des ronds (en volume)"), choices = varVolume, selected = varVolume)
        })
        output$variable_classe_rp_ac <- renderUI({
          selectInput("variable_classe_rp_ac_id", label=h5("Variable des classes (en ratio)"), choices = varRatio, selected = varRatio)
        })
        
        # FONDS
        
        output$ordre_fonds_rp_ac <- renderUI({
          selectInput("ordre_fonds_rp_ac_id", label=h5("Modifier l'ordre des fonds"), choices = liste_fonds$a, multiple=TRUE, selectize=FALSE, selected = NULL)
        })
        output$monter_fond_rp_ac <- renderUI({
          actionButton("monter_fond_rp_ac_id", label="", icon=icon("arrow-up"))
        })
        output$descendre_fond_rp_ac <- renderUI({
          actionButton("descendre_fond_rp_ac_id", label="", icon=icon("arrow-down"))
        })
        
        if(!is.null(fondMailleElargi))
        {
          output$elargi_rp_ac <- renderUI({
            checkboxInput("elargi_rp_ac_id", label = HTML("Afficher une repr\u00e9sentation \u00e9largie de l'analyse<br>(parfois long)"),
                          value = if(is.null(fondMailleElargi)) FALSE else TRUE)
          })
          output$opacite_elargi_rp_ac <- renderUI({
            sliderInput("opacite_elargi_rp_ac_id", label = h5("Opacit\u00e9 de l'analyse \u00e9largie"), value=60, min=0, max=100, step=5, ticks=FALSE)
          })
        }
        
        output$ajout_territoire_rp_ac <- renderUI({
          checkboxInput("ajout_territoire_rp_ac_id", label = "Afficher le fond des territoires",
                        value = if(is.null(fondSuppl)) FALSE else TRUE)
        })
        output$ajout_reg_rp_ac <- renderUI({
          checkboxInput("ajout_reg_rp_ac_id", label = "Afficher le fond des r\u00e9gions",
                        value = FALSE)
        })
        output$ajout_dep_rp_ac <- renderUI({
          checkboxInput("ajout_dep_rp_ac_id", label = "Afficher le fond des d\u00e9partements",
                        value = FALSE)
        })
        
        # RONDS
        
        output$taille_rond_rp_ac <- renderUI({
          numericInput("taille_rond_rp_ac_id", label = h5("Rayon du rond le plus grand (en m\u00e8tres)"), value=round(as.numeric(calcul_max_rayon_metres_rp_ac()[[1]])/1.25,0), min=0, max=round(as.numeric(calcul_max_rayon_metres_rp_ac()[[1]]),0), step=1000)
        })
        
        output$info_taille_max_rond_rp_ac <- renderText({
          HTML(paste0("<font size=2 color=white>Valeur max du rayon le plus grand = ", round(as.numeric(calcul_max_rayon_metres_rp_ac()[[1]]),0)," m</font>"))
        })
        
        output$info_rapport_rond_rp_ac <- renderText({
          HTML(paste0("<font size=2 color=white>Rapport Surface rond / Volume = ", (pi*(as.numeric(calcul_max_rayon_metres_rp_ac()[[1]])/1.25)^2)/as.numeric(calcul_max_rayon_metres_rp_ac()[[2]]),"</font>"))
        })
        
        output$rapport_rond_rp_ac <- renderUI({
          checkboxInput("rapport_rond_rp_ac_id", label = "Modifier la valeur du rapport (permet la comparaison entre cartes)", value=FALSE)
        })
        
        output$valeur_rapport_rond_rp_ac <- renderUI({
          numericInput("valeur_rapport_rond_rp_ac_id", label = h5("Nouvelle valeur du rapport Surface rond / Volume"), value=(pi*(as.numeric(calcul_max_rayon_metres_rp_ac()[[1]])/1.25)^2)/as.numeric(calcul_max_rayon_metres_rp_ac()[[2]]), min=0.1, max=(pi*(as.numeric(calcul_max_rayon_metres_rp_ac()[[1]]))^2)/as.numeric(calcul_max_rayon_metres_rp_ac()[[2]]), step=0.1)
        })
        
        output$info_rapport_max_rond_rp_ac <- renderText({
          HTML(paste0("<font size=2 color=white>Valeur max du rapport = ", (pi*(as.numeric(calcul_max_rayon_metres_rp_ac()[[1]]))^2)/as.numeric(calcul_max_rayon_metres_rp_ac()[[2]]),"</font>"))
        })
        
        if(!is.null(fondChx))
        {
          output$choix_centroid_rp_ac <- renderUI({
            radioButtons("choix_centroid_rp_ac_id", label = h5("Les ronds sont centres sur"), choices=c("les centroides des communes"="centroid","les chx des communes"="chx"), selected = if(!is.null(fondChx)) "chx" else "centroid")
          })
        }else
        {
          output$choix_centroid_rp_ac <- renderUI({
          })
        }
        
        # CLASSES
        
        output$liste_classes_rp_ac <- renderUI({
          selectInput("nb_classes_rp_ac_id", label = h5("Nombre de classes"),
                      choices = nb_classes_rp_ac(), selected = 4)
        })
        output$methode_rp_ac <- renderUI({
          selectInput("methode_rp_ac_id", label = h5("M\u00e9thode de calcul des classes"), 
                      choices = methode_calcul, selected="kmeans")
        })
        
        output$distribution_variable_rp_ac <- renderUI({
          bsButton("distribution_variable_rp_ac_id",label="Distribution de la variable", style="btn btn-info", icon = icon("bar-chart-o"),
                   type = "toggle", block = FALSE, disabled = FALSE,
                   value = FALSE)
        })
        
        observeEvent(input$distribution_variable_rp_ac_id,{
          if(!input$distribution_variable_rp_ac_id) return()
          updateButton(session, "distribution_variable_rp_ac_id", value = TRUE)
        }, ignoreInit = TRUE)
        
        observeEvent(input$distribution_variable_rp_ac_id,{
          
          output$distribution_rp_ac <- renderPlot({
            dt_donnees <- data.frame(VAR=as.numeric(analyse_rp_ac()[[1]]$donnees[,varRatio]))
            ggplot(dt_donnees, aes(x=dt_donnees$VAR)) +
              stat_bin(breaks=unique(sort(c(min(dt_donnees$VAR),new_bornes_rp_ac(),max(dt_donnees$VAR)))), closed = "left", fill="#5182B6", col="white") +
              scale_x_continuous(breaks=unique(sort(c(min(dt_donnees$VAR),new_bornes_rp_ac(),max(dt_donnees$VAR)))), labels = round(unique(sort(c(min(dt_donnees$VAR),new_bornes_rp_ac(),max(dt_donnees$VAR)))),2)) +
              ggtitle(label=paste0("Distribution de la variable  : ",varRatio)) +
              xlab(label = varRatio)
          })
          
          output$slider_bornes_rp_ac <- renderUI({
            lapply(1:(as.numeric(input$nb_classes_rp_ac_id)-1)+1, function(i) {
              sliderInput(inputId = paste0("slider_bornes_", i,"_rp_ac_id"), label = NULL,
                          value = rev(react_bornes_rp_ac()[[1]])[i], min = min(react_bornes_rp_ac()[[1]]), max = max(react_bornes_rp_ac()[[1]]), step = 0.001) #min = rev(react_bornes_rp_ac()[[1]])[i-1], max = rev(react_bornes_rp_ac()[[1]])[i+1]
            })
          })
          
          output$valid_slider_bornes_rp_ac <- renderUI({
            actionButton("valid_slider_bornes_rp_ac_id",label=label_bouton_rp_ac(), icon=icon("refresh"), style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
          })
        },ignoreInit = TRUE)
        
        label_bouton_rp_ac <- eventReactive(input$methode_rp_ac_id,{
          if(input$methode_rp_ac_id=="manuel")
          {
            label_bouton <- "Valider les bornes manuelles"
          }else
          {
            label_bouton <- "Basculer en mode manuel"
          }
          return(label_bouton)
        })
        
        new_bornes_rp_ac <- reactive({
          bornes <- vector()
          for (i in 2:(as.numeric(input$nb_classes_rp_ac_id))) {
            bornes<-c(bornes,input[[paste0("slider_bornes_", i,"_rp_ac_id")]])
          }
          return(bornes)
        })
        
        output$zone_bornes_rp_ac <- renderUI({
          
          if(!is.null(input$methode_rp_ac_id))
          {
            if(input$methode_rp_ac_id=="manuel")
              suppressWarnings(bornes_analyse <- classIntervals(as.numeric(analyse_rp_ac()[[1]]$donnees[,varRatio]),as.numeric(input$nb_classes_rp_ac_id),style="kmeans",rtimes=10,intervalClosure="left"))
            else
              suppressWarnings(bornes_analyse <- classIntervals(as.numeric(analyse_rp_ac()[[1]]$donnees[,varRatio]),input$nb_classes_rp_ac_id,style=input$methode_rp_ac_id,rtimes=10,intervalClosure="left"))
            
            carac_bornes <- calcul_bornes(analyse_rp_ac()[[1]]$donnees,bornes_analyse,varRatio,input$nb_classes_rp_ac_id,input$methode_rp_ac_id)
            
            if(!is.null(input$nb_classes_rp_ac_id))
            {
              if(input$methode_rp_ac_id=="manuel")
              {
                lapply(1:(as.numeric(input$nb_classes_rp_ac_id)-1)+1, function(i) {
                  numericInput(inputId = paste0("bornes_", i,"_rp_ac_id"), label = paste("Choix de la borne ", i-1),
                               value = carac_bornes[[1]][i])
                })
              }
            }
          }
        })
        
        output$valid_bornes_rp_ac <- renderUI({
          actionButton("valid_bornes_rp_ac_id",label="Rafraichir la carte", icon=icon("refresh"), style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
        })
        
        # LEGENDE
        
        output$titre_ronds_legende_rp_ac <- renderUI({
          textInput("titre_ronds_legende_rp_ac_id", label = h5("Titre de la l\u00e9gende des ronds"), value = "")
        })
        
        output$titre_classes_legende_rp_ac <- renderUI({
          textInput("titre_classes_legende_rp_ac_id", label = h5("Titre de la l\u00e9gende des classes"), value = "")
        })
        
        output$affiche_legende_rp_ac <- renderUI({
          checkboxInput("affiche_legende_rp_ac_id", label = "Activer le d\u00e9placement de la l\u00e9gende au clic",
                        value = TRUE)
        })
        
        output$type_legende_rp_ac <- renderUI({
          radioButtons("type_legende_rp_ac_id", label = h5("Type de l\u00e9gende"),
                       choices = list("Litt\u00e9rale" = 1, "En echelle" = 2),
                       selected = 1, inline = TRUE)
        })
        
        # SAUVEGARDE
        
        output$save_carte_rp_ac <- renderUI({
          actionButton("save_carte_rp_ac_id", label=HTML("<font size=3>Sauvegarder la carte dans un onglet</font>"), style="color:#FFFFFF; background-color:#DF691A")
        })
        
        output$entrees_qgis_rp_ac <- renderUI({
          actionButton("entrees_qgis_rp_ac_id", label="Exporter en projet Qgis")
        })
        
        output$sortie_qgis_rp_ac <- renderUI({
          tags$div(class="input-group",
                   HTML('<input type="text" id="sortie_qgis_rp_ac_id" class="form-control" placeholder="Nom du projet" aria-describedby="sortie_qgis_rp_ac_id">
                        <span class="input-group-addon" id="sortie_qgis_rp_ac_id">.qgs</span>'))
        })
        
        output$titre1_qgis_rp_ac <- renderUI({
          textInput("titre1_qgis_rp_ac_id", label = h5("Titre informatif"), value = "", placeholder= "Facultatif")
        })
        
        output$titre2_qgis_rp_ac <- renderUI({
          textInput("titre2_qgis_rp_ac_id", label = h5("Titre descriptif"), value = "", placeholder= "Facultatif")
        })
        
        output$source_qgis_rp_ac <- renderUI({
          textInput("source_qgis_rp_ac_id", label = h5("Source de la carte"), value = sourc)
        })
        
        output$aide_image_rp_ac <- renderUI({
          tags$div(class="dropup",
                   HTML(paste0('<button class="btn btn-primary dropdown-toggle" type="button" data-toggle="dropdown">
                               <i class="fa fa-book fa-fw" aria-hidden="true"></i>
                               Proc','\u00e9','dure pour capture d\'','\u00e9','cran
                               <span class="caret"></span>
                               </button>')),
                   tags$ul(class="dropdown-menu",
                           wellPanel(
                             style="background: #2B3E50; width:340px",
                             div(
                               HTML("<font size=2>Deux possibilit\u00e9s :</font>"),
                               br(),
                               br(),
                               strong(HTML("<font size=3>Par l'Outil Capture</font>")),
                               br(),
                               HTML("<font size=2>1- Ouvrir le logiciel Outil Capture sur le Poste de travail (Menu D\u00e9marrer > Tous les programmes > Accessoires > Outil Capture).</font>"),
                               br(),
                               HTML(paste0("<font size=2>2- S\u00e9lectionner la zone \u00e0 capturer (vous aurez besoin d'adapter la fen\u00eatre AUS pour avoir \u00e0 la fois la carte et la barre des t","\u00e2","ches).</font>")),
                               br(),
                               HTML("<font size=2>3- Enregistrer l'image ou copier la dans le presse-papier.</font>"),
                               br(),
                               br(),
                               strong(HTML(paste0("<font size=3>Par impression d'","\u00e9","cran</font>"))),
                               br(),
                               HTML("<font size=2>1- Appuyer sur la touche clavier \"Impr ecran\".</font>"),
                               br(),
                               HTML("<font size=2>2- Ouvrir un logiciel de retouche image (Paint par exemple).</font>"),
                               br(),
                               HTML("<font size=2>3- Coller l'image et l'enregistrer au format voulu (.jpg, .png, .bmp).</font>")
                             )
                           )
                   )
                   )
        })
        })
      
      # Pour modifier l'ordre des fonds
      observeEvent(list(input$monter_fond_rp_ac_id,input$descendre_fond_rp_ac_id),{
        
        ordre <- c()
        if(as.numeric(input$monter_fond_rp_ac_id)>nb_up$a)
        {
          ordre <- c(2,3)
          nb_up$a <- nb_up$a+1
        }
        if(as.numeric(input$descendre_fond_rp_ac_id)>nb_down$a)
        {
          ordre <- c(1,2)
          nb_down$a <- nb_down$a+1
        }
        
        if(is.null(input$ordre_fonds_rp_ac_id)) pos_select <- 0 else pos_select <- which(liste_fonds$a==input$ordre_fonds_rp_ac_id)
        
        if(pos_select>0)
        {
          if(pos_select==ordre[1]) liste_fonds$a <- liste_fonds$a[c(2,1,3)]
          if(pos_select==ordre[2]) liste_fonds$a <- liste_fonds$a[c(1,3,2)]
          
          updateSelectInput(session, "ordre_fonds_rp_ac_id",
                            choices = liste_fonds$a,
                            selected = input$ordre_fonds_rp_ac_id
          )
        }
      },ignoreInit = TRUE)
      
      # Pour la semio, on calcul le rayon maximal d'un rond de facon a ce que la somme des superficies des ronds ne depasse pas 1/7eme de la superficie du territoire.
      calcul_max_rayon_metres_rp_ac <- reactive({
        #Aire totale du territoire d'etude
        aire_territoire <- as.numeric(sum(st_area(fondMaille[fondMaille$CODE %in% data[,"CODE"],]))) #Superficie du territoire
        #valeur max de la serie de donnees
        suppressWarnings(max_var <- max(data[data[,"CODE"] %in% fondMaille$CODE,varVolume], na.rm = TRUE))
        
        serie <- data[data[,"CODE"] %in% fondMaille$CODE,varVolume]
        serie <- serie[!is.na(serie)]
        #on ramene la serie a un quotient fonction de la valeur max
        quotient <- serie/max_var
        #somme des carres
        somme_quotient <- sum(quotient^2)
        #calcul de la surface max du rond le plus grand
        max_surface_rond <- (aire_territoire/(7*somme_quotient))
        #calcul du rayon max du rond le plus grand
        max_rayon_metres <- sqrt(max_surface_rond/pi)
        
        return(list(max_rayon_metres,max_var))
      })
      
      rayon_rp_ac <- reactive({
        req(input$valeur_rapport_rond_rp_ac_id)
        Sys.sleep(3)
        val <- round(sqrt((input$valeur_rapport_rond_rp_ac_id*isolate(calcul_max_rayon_metres_rp_ac())[[2]])/pi),0)
        return(val)
      })
      
      rayon_react_rp_ac <- rayon_rp_ac %>% debounce(1000)
      
      observeEvent(rayon_react_rp_ac(),{
        req(rayon_react_rp_ac())
        
        if(length(rayon_react_rp_ac())==0) return(NULL)
        if(rayon_react_rp_ac()==0 | is.na(rayon_react_rp_ac())) return(NULL)
        
        isolate(updateNumericInput(session,"taille_rond_rp_ac_id", value=rayon_react_rp_ac()))
        
        isolate(output$info_rapport_rond_rp_ac <- renderText({
          HTML(paste0("<font size=2 color=white>Rapport Surface rond / Volume = ", (pi*(rayon_react_rp_ac())^2)/isolate(calcul_max_rayon_metres_rp_ac())[[2]],"</font>"))
        }))
      })
      
      rapport_rp_ac <- reactive({
        req(input$taille_rond_rp_ac_id)
        
        val <- (pi*(input$taille_rond_rp_ac_id)^2)/isolate(calcul_max_rayon_metres_rp_ac())[[2]]
        max <- (pi*(isolate(calcul_max_rayon_metres_rp_ac())[[1]])^2)/isolate(calcul_max_rayon_metres_rp_ac())[[2]]
        
        return(list(val=val,max=max))
      })
      
      rapport_react_rp_ac <- rapport_rp_ac %>% debounce(1000)
      
      observeEvent(rapport_react_rp_ac(),{
        req(rapport_react_rp_ac())
        
        if(length(rapport_react_rp_ac()$val)==0) return(NULL)
        if(rapport_react_rp_ac()$val==0 | is.na(rapport_react_rp_ac()$val)) return(NULL)
        
        isolate(updateNumericInput(session,"valeur_rapport_rond_rp_ac_id", value=rapport_react_rp_ac()$val))
        
        isolate(output$info_rapport_rond_rp_ac <- renderText({
          HTML(paste0("<font size=2 color=white>Rapport Surface rond / Volume = ", rapport_react_rp_ac()$val,"</font>"))
        }))
      })
      
      choix_centroid_rp_ac <- reactive({
        if(is.null(input$choix_centroid_rp_ac_id))
        {
          centroid <- "centroid"
        }else
        {
          centroid <- input$choix_centroid_rp_ac_id
        }
        return(centroid)
      })
      
      # Pour calculer les bornes des classes
      react_bornes_rp_ac <- reactive({
        
        if(is.null(input$nb_classes_rp_ac_id) | is.null(input$methode_rp_ac_id))
        {
          max_classes$a <- 4
          methode <- "kmeans"
          suppressWarnings(bornes_analyse <- classIntervals(as.numeric(analyse_rp_ac()[[1]]$donnees[,varRatio]),max_classes$a,style=methode,rtimes=10,intervalClosure="left"))
        }else if(input$nb_classes_rp_ac_id=="" | input$methode_rp_ac_id=="")
        {
          max_classes$a <- 4
          methode <- "kmeans"
          suppressWarnings(bornes_analyse <- classIntervals(as.numeric(analyse_rp_ac()[[1]]$donnees[,varRatio]),max_classes$a,style=methode,rtimes=10,intervalClosure="left"))
        }else
        {
          max_classes$a <- as.numeric(input$nb_classes_rp_ac_id)
          if(is.na(max_classes$a)) return(NULL)
          methode <- as.character(input$methode_rp_ac_id)
          
          if(!methode %in% c("manuel"))
          {
            suppressWarnings(bornes_analyse <- classIntervals(as.numeric(analyse_rp_ac()[[1]]$donnees[,varRatio]),max_classes$a,style=methode,rtimes=10,intervalClosure="left"))
          }
        }
        if(methode!="manuel")
        {
          carac_bornes <- calcul_bornes(analyse_rp_ac()[[1]]$donnees,bornes_analyse,varRatio,max_classes$a,methode)
        }else if(methode=="manuel")
        {
          carac_bornes <- react_bornes_manuel_1_rp_ac()
        }
        
        return(carac_bornes) #list(bornes=carac_bornes[[1]],pal_classes=carac_bornes[[2]])
      })
      
      # Pour calculer les bornes des classes
      react_bornes_init_rp_ac <- reactive({
        
        suppressWarnings(bornes_analyse <- classIntervals(as.numeric(data[,varRatio]),4,style="kmeans",rtimes=10,intervalClosure="left"))
        carac_bornes <- calcul_bornes(data,bornes_analyse,varRatio,4,"kmeans")
        return(carac_bornes) #list(bornes=carac_bornes[[1]],pal_classes=carac_bornes[[2]])
      })
      
      # Pour mettre a jour les bornes en mode manuel
      react_bornes_manuel_1_rp_ac <- eventReactive(input$valid_bornes_rp_ac_id,{
        
        suppressWarnings(bornes_analyse <- classIntervals(as.numeric(analyse_rp_ac()[[1]]$donnees[,varRatio]),max_classes$a,style="kmeans",rtimes=10,intervalClosure="left"))
        
        carac_bornes <- calcul_bornes(analyse_rp_ac()[[1]]$donnees,bornes_analyse,varRatio,input$nb_classes_rp_ac_id,input$methode_rp_ac_id)
        
        bornes <- vector()
        for (i in 0:(as.numeric(input$nb_classes_rp_ac_id))+1) {
          bornes<-c(bornes,input[[paste0("bornes_", i,"_rp_ac_id")]])
        }
        bornes <- c(carac_bornes[[1]][1],bornes,carac_bornes[[1]][length(carac_bornes[[1]])])
        bornes <- sort(unique(bornes),decreasing = T)
        
        carac_bornes[[1]] <- bornes
        
        return(carac_bornes)
      },ignoreNULL = FALSE)
      
      # Pour mettre a jour les bornes dans la distribution
      observeEvent(input$valid_slider_bornes_rp_ac_id,{
        updateSelectInput(session, inputId = "methode_rp_ac_id", selected = "manuel")
        for (i in 0:(as.numeric(input$nb_classes_rp_ac_id))+1) {
          updateNumericInput(session, inputId = paste0("bornes_", i,"_rp_ac_id"), value = input[[paste0("slider_bornes_", i,"_rp_ac_id")]])
        }
      },ignoreInit = TRUE)
      
      # Pour renvoyer la fourchette de classes possible
      nb_classes_rp_ac <- reactive({
        
        if(elargi_rp_ac())
        {
          donnees <- analyse_rp_ac()[[1]]$donnees_elargi[,varRatio]
        }else
        {
          donnees <- analyse_rp_ac()[[1]]$donnees[,varRatio]
        }
        
        suppressWarnings(
          if(min(donnees)<0 & max(donnees)>0) # Si + et -
          {
            if(length(donnees)>3 & length(donnees)<9)
            {
              return(c(3:(length(donnees)-1)))
            }else
            {
              return(c(3:9))
            }
          }else # Si tout + ou tout -
          {
            if(length(donnees)>3 & length(donnees)<5)
            {
              return(c(3:(length(donnees)-1)))
            }else
            {
              return(c(3:5))
            }
          }
        )
      })
      observe({nb_classes_rp_ac()})
      
      # Pour exporter la carte en projet Qgis
      
      output$export_qgis_rp_ac <- renderUI({
        downloadButton("downloadProjetQgis_rp_ac", label="Exporter")
      })
      
      output$downloadProjetQgis_rp_ac <- downloadHandler(contentType = "zip",
                                                         filename = function(){
                                                           paste0(input$sortie_qgis_rp_ac_id,".zip")
                                                         },
                                                         content = function(file){
                                                           files <- EXPORT_PROJET_QGIS_RP_AC(file)
                                                           
                                                           zip(file,files, flags = "-j9X")
                                                         }
      )
      
      EXPORT_PROJET_QGIS_RP_AC <- function(file)
      {
        showModal(modalDialog(HTML("<i class=\"fa fa-spinner fa-spin fa-2x fa-fw\"></i> <font size=+1>Export du projet Qgis en cours...</font> "), size="m", footer=NULL, style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"))
        
        sortie <- input$sortie_qgis_rp_ac_id
        rep_sortie <- dirname(file)
        files <- c(paste0(rep_sortie,"/",sortie,".qgs"))
        
        if(is.null(input$nb_classes_rp_ac_id))
        {
          max_classes <- 4
        }else
        {
          max_classes <- input$nb_classes_rp_ac_id
        }
        
        if(!is.null(lon_lat_rp_ac()[[1]]))
        {
          suppressWarnings(test_affiche_leg <- try(table_classe <- data.frame(classe=c(max_classes:1),label=legende$a,couleurs=analyse_leg_rp_ac()$pal_classes, stringsAsFactors = F),silent=TRUE))
          if(class(test_affiche_leg) %in% "try-error")
          {
            showModal(modalDialog(HTML("<font size=+1><i class=\"fa fa-hand-pointer-o fa-fw\"></i><b>Double-cliquez</b> d'abord sur la carte pour afficher la l\u00e9gende.</font> "), size="m", footer=NULL, easyClose = TRUE, style = "color: #fff; background-color: #DF691A; border-color: #2e6da4")) #337ab7
            return(NULL)
          }else
          {
            table_classe <- data.frame(classe=c(max_classes:1),label=legende$a,couleurs=analyse_leg_rp_ac()$pal_classes, stringsAsFactors = F)
          }
        }else
        {
          showModal(modalDialog(HTML("<font size=+1><i class=\"fa fa-hand-pointer-o fa-fw\"></i><b>Double-cliquez</b> d'abord sur la carte pour afficher la l\u00e9gende.</font> "), size="m", footer=NULL, easyClose = TRUE, style = "color: #fff; background-color: #DF691A; border-color: #2e6da4")) #337ab7
          return(NULL)
        }
        
        if(elargi_rp_ac())
        {
          analyse_donnees_elargi <- analyse_rp_ac()[[1]][[4]] #donnees elargi
          analyse_maille_elargi <- fondMailleElargi #fond maille elargi
          names_donnees_elargi <- names(analyse_donnees_elargi)
          analyse_donnees_elargi <- data.frame(analyse_donnees_elargi,val=analyse_donnees_elargi[,varRatio],classe=palette_rp_ac()[[1]](analyse_donnees_elargi[,varRatio]))
          names(analyse_donnees_elargi) <- c(names_donnees_elargi,"val","classe")
          analyse_classes_elargi <- merge(table_classe,analyse_donnees_elargi,by.x="couleurs",by.y="classe")
          analyse_classes_elargi <- analyse_classes_elargi[,c("CODE","LIBELLE",varVolume,varRatio,"val","classe")]
          
          analyse_classes_elargi <- analyse_classes_elargi[order(analyse_classes_elargi[,varVolume],decreasing = T),]
          
          analyse_ronds_elargi <- analyse_ronds_sf_rp_ac()[[2]]
          
          analyse_ronds_elargi$classe <- analyse_classes_elargi$classe
          analyse_ronds_elargi$COL_BOR <- "white"
          
          fond_ronds_classes_elargi <- analyse_ronds_elargi
          
          fond_maille_elargi <- st_transform(fondMailleElargi,paste0("+init=epsg:",code_epsg_rp_ac()))
          
          st_write(fond_ronds_classes_elargi, paste0(rep_sortie,"/fond_maille_elargi_carte.shp"), delete_dsn = TRUE, quiet = TRUE)
          st_write(fond_maille_elargi, paste0(rep_sortie,"/fond_maille_elargi.shp"), delete_dsn = TRUE, quiet = TRUE)
          files <- c(paste0(rep_sortie,"/fond_maille_elargi_carte.shp"),paste0(rep_sortie,"/fond_maille_elargi_carte.dbf"),paste0(rep_sortie,"/fond_maille_elargi_carte.prj"),paste0(rep_sortie,"/fond_maille_elargi_carte.shx"),files)
          files <- c(paste0(rep_sortie,"/fond_maille_elargi.shp"),paste0(rep_sortie,"/fond_maille_elargi.dbf"),paste0(rep_sortie,"/fond_maille_elargi.prj"),paste0(rep_sortie,"/fond_maille_elargi.shx"),files)
        }
        
        analyse_donnees <- analyse_rp_ac()[[1]][[2]] #donnees
        analyse_maille <- fondMaille #fond maille
        
        names_donnees <- names(analyse_donnees)
        analyse_donnees <- data.frame(analyse_donnees,val=analyse_donnees[,varRatio],classe=palette_rp_ac()[[1]](analyse_donnees[,varRatio]))
        names(analyse_donnees) <- c(names_donnees,"val","classe")
        analyse_classes <- merge(table_classe,analyse_donnees,by.x="couleurs",by.y="classe")
        analyse_classes <- analyse_classes[,c("CODE","LIBELLE",varVolume,varRatio,"val","classe")]
        analyse_classes <- analyse_classes[order(analyse_classes[,varVolume],decreasing = T),]
        analyse_classes$COL_BOR <- "white"
        analyse_ronds <- analyse_ronds_sf_rp_ac()[[1]]
        
        analyse_ronds <- merge(analyse_ronds[,c("CODE","geometry")],analyse_classes[,c("CODE","LIBELLE",varVolume,varRatio,"COL_BOR","val","classe")],by="CODE")
        names(analyse_ronds) <- c("CODE","LIBELLE",varVolume,varRatio,"COL_BOR","val","classe","geometry")
        analyse_ronds <- st_sf(analyse_ronds,stringsAsFactors = FALSE)
        analyse_ronds <- analyse_ronds[order(as.data.frame(analyse_ronds)[,varVolume],decreasing = T),]
        fond_ronds_classes <- analyse_ronds
        fond_ronds_leg <- construction_legende_rp_ac()[[1]][[2]]
        fond_lignes_leg <- construction_legende_rp_ac()[[2]][[7]]
        
        fond_maille <- st_transform(fondMaille,paste0("+init=epsg:",code_epsg_rp_ac()))
        fond_contour <- st_transform(fondContour,paste0("+init=epsg:",code_epsg_rp_ac()))
        if(!is.null(fondSuppl) && input$ajout_territoire_rp_ac_id) fond_territoire <- st_transform(fond_territoire_rp_ac(),paste0("+init=epsg:",code_epsg_rp_ac()))
        if(input$ajout_dep_rp_ac_id) fond_departement <- st_transform(fond_departement_rp_ac(),paste0("+init=epsg:",code_epsg_rp_ac()))
        if(input$ajout_reg_rp_ac_id) fond_region <- st_transform(fond_region_rp_ac(),paste0("+init=epsg:",code_epsg_rp_ac()))
        fond_france <- st_transform(fond_habillage_rp_ac()[[1]],paste0("+init=epsg:",code_epsg_rp_ac()))
        fond_pays <- st_transform(fond_habillage_rp_ac()[[2]],paste0("+init=epsg:",code_epsg_rp_ac()))
        
        st_write(fond_ronds_classes, paste0(rep_sortie,"/fond_maille_carte.shp"), delete_dsn = TRUE, quiet = TRUE)
        st_write(fond_ronds_leg, paste0(rep_sortie,"/fond_ronds_leg.shp"), delete_dsn = TRUE, quiet = TRUE)
        st_write(fond_lignes_leg, paste0(rep_sortie,"/fond_lignes_leg.shp"), delete_dsn = TRUE, quiet = TRUE)
        st_write(fond_maille, paste0(rep_sortie,"/fond_maille.shp"), delete_dsn = TRUE, quiet = TRUE)
        st_write(fond_contour,paste0(rep_sortie,"/fond_contour.shp"), delete_dsn = TRUE, quiet = TRUE)
        if(exists("fond_territoire")) if(!is.null(fond_territoire)) st_write(fond_territoire, paste0(rep_sortie,"/fond_territoire.shp"), delete_dsn = TRUE, quiet = TRUE)
        if(exists("fond_departement")) if(!is.null(fond_departement)) st_write(fond_departement, paste0(rep_sortie,"/fond_departement.shp"), delete_dsn = TRUE, quiet = TRUE)
        if(exists("fond_region")) if(!is.null(fond_region)) st_write(fond_region,paste0(rep_sortie,"/fond_region.shp"), delete_dsn = TRUE, quiet = TRUE)
        st_write(fond_france,paste0(rep_sortie,"/fond_france.shp"), delete_dsn = TRUE, quiet = TRUE)
        if(exists("fond_pays")) if(!is.null(fond_pays)) st_write(fond_pays,paste0(rep_sortie,"/fond_pays.shp"), delete_dsn = TRUE, quiet = TRUE)
        
        files <- c(paste0(rep_sortie,"/fond_maille_carte.shp"),paste0(rep_sortie,"/fond_maille_carte.dbf"),paste0(rep_sortie,"/fond_maille_carte.prj"),paste0(rep_sortie,"/fond_maille_carte.shx"),files)
        files <- c(paste0(rep_sortie,"/fond_ronds_leg.shp"),paste0(rep_sortie,"/fond_ronds_leg.dbf"),paste0(rep_sortie,"/fond_ronds_leg.prj"),paste0(rep_sortie,"/fond_ronds_leg.shx"),files)
        files <- c(paste0(rep_sortie,"/fond_lignes_leg.shp"),paste0(rep_sortie,"/fond_lignes_leg.dbf"),paste0(rep_sortie,"/fond_lignes_leg.prj"),paste0(rep_sortie,"/fond_lignes_leg.shx"),files)
        files <- c(paste0(rep_sortie,"/fond_maille.shp"),paste0(rep_sortie,"/fond_maille.dbf"),paste0(rep_sortie,"/fond_maille.prj"),paste0(rep_sortie,"/fond_maille.shx"),files)
        files <- c(paste0(rep_sortie,"/fond_contour.shp"),paste0(rep_sortie,"/fond_contour.dbf"),paste0(rep_sortie,"/fond_contour.prj"),paste0(rep_sortie,"/fond_contour.shx"),files)
        if(exists("fond_territoire")) if(!is.null(fond_territoire)) files <- c(paste0(rep_sortie,"/fond_territoire.shp"),paste0(rep_sortie,"/fond_territoire.dbf"),paste0(rep_sortie,"/fond_territoire.prj"),paste0(rep_sortie,"/fond_territoire.shx"),files)
        if(exists("fond_departement")) if(!is.null(fond_departement)) files <- c(paste0(rep_sortie,"/fond_departement.shp"),paste0(rep_sortie,"/fond_departement.dbf"),paste0(rep_sortie,"/fond_departement.prj"),paste0(rep_sortie,"/fond_departement.shx"),files)
        if(exists("fond_region")) if(!is.null(fond_region)) files <- c(paste0(rep_sortie,"/fond_region.shp"),paste0(rep_sortie,"/fond_region.dbf"),paste0(rep_sortie,"/fond_region.prj"),paste0(rep_sortie,"/fond_region.shx"),files)
        files <- c(paste0(rep_sortie,"/fond_france.shp"),paste0(rep_sortie,"/fond_france.dbf"),paste0(rep_sortie,"/fond_france.prj"),paste0(rep_sortie,"/fond_france.shx"),files)
        if(exists("fond_pays")) if(!is.null(fond_pays)) files <- c(paste0(rep_sortie,"/fond_pays.shp"),paste0(rep_sortie,"/fond_pays.dbf"),paste0(rep_sortie,"/fond_pays.prj"),paste0(rep_sortie,"/fond_pays.shx"),files)
        
        titre1 <- paste0(input$titre1_qgis_rp_ac_id,"\n")
        titre2 <- input$titre2_qgis_rp_ac_id
        source <- input$source_qgis_rp_ac_id
        annee <- format(Sys.time(), format = "%Y")
        variable_a_representer <- varRatio
        titre_leg_classes <- input$titre_classes_legende_rp_ac_id
        
        l <- c()
        if(exists("fond_territoire")) l <- "fond_territoire"
        if(exists("fond_departement")) l <- c(l,"fond_departement")
        if(exists("fond_region")) l <- c(l,"fond_region")
        
        if(elargi_rp_ac())
        {
          l=c("fond_france",
              "fond_contour",
              "fond_maille",
              l,
              "fond_maille_elargi",
              "fond_ronds_leg",
              "fond_lignes_leg",
              "fond_maille_carte",
              "fond_maille_elargi_carte"
          )
        }else
        {
          l=c("fond_france",
              "fond_contour",
              "fond_maille",
              l,
              "fond_ronds_leg",
              "fond_lignes_leg",
              "fond_maille_carte"
          )
        }
        
        l <- c(l,"fond_pays")
        
        export_projet_qgis_ronds_classes(l,rep_sortie,sortie,titre1,titre2,source,titre_leg_classes,table_classe,variable_a_representer,annee)
        
        removeModal()
        
        showModal(modalDialog(HTML(paste0("<font size=+1>Le projet Qgis a \u00e9t\u00e9 cr","\u00e9","ee.</font>")), size="m", footer=NULL, easyClose = TRUE, style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"))
        
        return(files)
      }
      
      elargi_rp_ac <- reactive({
        if(is.null(input$elargi_rp_ac_id))
        {
          elargi <- FALSE
        }else
        {
          elargi <- input$elargi_rp_ac_id
        }
        return(elargi)
      })
      
      code_epsg_rp_ac <- reactive({
        code_epsg <- switch(dom, #DOM
                            "0"="2154",# Lambert 93
                            "971"="32620",# UTM 20 N
                            "972"="32620",# UTM 20 N
                            "973"="2972",# UTM 22 N
                            "974"="2975",# UTM 40 S
                            "976"="4471")# UTM 38 S
        return(code_epsg)
      })
      
      analyse_rp_ac <- reactive({
        
        suppressWarnings(test_k_ronds <- try(k_ronds(fondMaille,fondMailleElargi,names(fondMaille)[1],data,"CODE",varVolume,elargi_rp_ac(),choix_centroid_rp_ac(),fondChx),silent=T))
        if(class(test_k_ronds) %in% "try-error")
        {
          return(NULL)
        }else
        {
          analyse <- k_ronds(fondMaille,fondMailleElargi,names(fondMaille)[1],data,"CODE",varVolume,elargi_rp_ac(),choix_centroid_rp_ac(),fondChx)
        }
        
        if(is.null(analyse))
        {
          showModal(modalDialog(HTML(paste0("<font size=+1>La maille ne correspond pas au niveau g\u00e9ographique du fichier de donn","\u00e9","es.<br><br>Veuillez svp choisir une maille adapt","\u00e9","e ou modifier le fichier de donn","\u00e9","es.</font>")), size="l", footer=NULL, easyClose = TRUE, style = "color: #fff; background-color: #DF691A; border-color: #2e6da4")) #337ab7
          erreur_maille$a <- TRUE
          return(NULL)
        }
        analyse$donnees[,"TXT1"] <- paste0("<b> <font color=#2B3E50>",format(analyse$donnees$save, big.mark=" ",decimal.mark=",",nsmall=0),"</font></b>")
        analyse$donnees[,"TXT2"] <- paste0("<b> <font color=#2B3E50>",format(analyse$donnees[,varRatio], big.mark=" ",decimal.mark=",",nsmall=0),"</font></b>")
        if(elargi_rp_ac())
        {
          analyse$donnees_elargi[,"TXT1"] <- paste0("<b> <font color=#2B3E50>",format(analyse$donnees_elargi$save, big.mark=" ",decimal.mark=",",nsmall=0),"</font></b>")
          analyse$donnees_elargi[,"TXT2"] <- paste0("<b> <font color=#2B3E50>",format(as.vector(analyse$donnees_elargi[,varRatio]), big.mark=" ",decimal.mark=",",nsmall=0),"</font></b>")
        }
        analyse_WGS84 <- st_transform(analyse$analyse_points,"+init=epsg:4326 +proj=longlat +ellps=WGS84")
        return(list(analyse,analyse_WGS84))
      })
      
      analyse_leg_rp_ac <- reactive({
        analyse <- analyse_rp_ac()[[1]]
        analyse$rupture_classes <- palette_rp_ac()[[2]] #bornes
        analyse$pal_classes <- palette_rp_ac()[[3]] # pal_classes
        return(analyse)
      })
      
      fond_habillage_rp_ac <- reactive({
        if(dom=="0")
        {
          pays <- st_transform(sf_paysm(),"+init=epsg:4326 +proj=longlat +ellps=WGS84")
          fra <- st_transform(sf_fram(),"+init=epsg:4326 +proj=longlat +ellps=WGS84")
        }else
        {
          if(dom=="971")
          {
            fra <- st_transform(sf_reg01(),"+init=epsg:4326 +proj=longlat +ellps=WGS84")
            pays <- fra
          }
          if(dom=="972")
          {
            fra <- st_transform(sf_reg02(),"+init=epsg:4326 +proj=longlat +ellps=WGS84")
            pays <- fra
          }
          if(dom=="973")
          {
            fra <- st_transform(sf_reg03(),"+init=epsg:4326 +proj=longlat +ellps=WGS84")
            pays <- st_transform(sf_pays973(),"+init=epsg:4326 +proj=longlat +ellps=WGS84")
          }
          if(dom=="974")
          {
            fra <- st_transform(sf_reg04(),"+init=epsg:4326 +proj=longlat +ellps=WGS84")
            pays <- fra
          }
          if(dom=="976")
          {
            fra <- st_transform(sf_reg06(),"+init=epsg:4326 +proj=longlat +ellps=WGS84")
            pays <- fra
          }
        }
        fond_france <- fra
        fond_pays <- pays
        
        return(list(fond_france,fond_pays))
      })
      
      fond_contour_maille_rp_ac <- reactive({
        test_contour <- try(st_transform(fondContour,"+init=epsg:4326 +proj=longlat +ellps=WGS84"), silent = TRUE)
        test_maille <- try(st_transform(fondMaille,"+init=epsg:4326 +proj=longlat +ellps=WGS84"), silent = TRUE)
        
        if(any(list(class(test_contour),class(test_maille)) %in% "try-error"))
        {
          showModal(modalDialog(HTML(paste0("<font size=+1>Une erreur est survenue dans la cr","\u00e9","ation du territoire.<br><br>Veuillez svp v\u00e9rifier vos donn","\u00e9","es et les variables choisies.</font>")), size="m", footer=NULL, easyClose = TRUE, style = "color: #fff; background-color: #DF691A; border-color: #2e6da4"))
          erreur_maille$a <- TRUE
          return(NULL)
        }else
        {
          contour_WGS84 <- st_transform(fondContour,"+init=epsg:4326 +proj=longlat +ellps=WGS84")
          maille_WGS84 <- st_transform(fondMaille,"+init=epsg:4326 +proj=longlat +ellps=WGS84")
        }
        
        return(list(contour_WGS84,maille_WGS84))
      })
      
      fond_elargi_rp_ac <- reactive({
        req(analyse_rp_ac())
        if(elargi_rp_ac())
        {
          analyse_WGS84_elargi <- st_transform(analyse_rp_ac()[[1]]$analyse_points_elargi,"+init=epsg:4326 +proj=longlat +ellps=WGS84")
          maille_WGS84_elargi <- st_transform(fondMailleElargi,"+init=epsg:4326 +proj=longlat +ellps=WGS84")
          return(list(analyse_WGS84_elargi,maille_WGS84_elargi))
        }else
        {
          return(NULL)
        }
      })
      
      list_bbox_rp_ac <- reactive({
        req(fond_contour_maille_rp_ac())
        
        list_bbox <- list(c(st_bbox(fond_contour_maille_rp_ac()[[1]])[1],st_bbox(fond_contour_maille_rp_ac()[[1]])[3]),c(st_bbox(fond_contour_maille_rp_ac()[[1]])[2],st_bbox(fond_contour_maille_rp_ac()[[1]])[4]))
        return(list_bbox)
      })
      
      calcul_rond_rp_ac <- reactive({
        req(calcul_max_rayon_metres_rp_ac(),input$taille_rond_rp_ac_id)
        if(is.null(input$taille_rond_rp_ac_id)) taille_rond <- 1000
        
        if(!is.null(input$taille_rond_rp_ac_id))
        {
          if(input$taille_rond_rp_ac_id>calcul_max_rayon_metres_rp_ac()[[1]])
          {
            taille_rond_m <- calcul_max_rayon_metres_rp_ac()[[1]]
          }else
          {
            taille_rond_m <- input$taille_rond_rp_ac_id
          }
        }else
        {
          taille_rond_m <- NULL
        }
        
        return(taille_rond_m)
      })
      
      analyse_ronds_sf_rp_ac <- reactive({
        req(analyse_rp_ac(),code_epsg_rp_ac(),calcul_rond_rp_ac())
        # On cree les ronds en projection locale (pl) pour l'export Qgis
        if(elargi_rp_ac())
        {
          req(fond_elargi_rp_ac())
          centres <- rbind(st_coordinates(fond_elargi_rp_ac()[[1]]))
          row.names(centres) <- c(1:(nrow(analyse_rp_ac()[[1]]$donnees_elargi)))
          ronds <- st_sf(geometry=st_sfc(lapply(c(1:nrow(centres)),function(x) st_point(centres[x,])),crs="+init=epsg:4326 +proj=longlat +ellps=WGS84"))
          ronds_pl_elargi <- st_buffer(st_transform(ronds,paste0("+init=epsg:",code_epsg_rp_ac())), calcul_rond_rp_ac()*sqrt(analyse_rp_ac()[[1]]$donnees_elargi[,varVolume]/calcul_max_rayon_metres_rp_ac()[[2]]))
          
          # On cree les ronds 
          dt_ronds_sf <- data.frame(ronds_pl_elargi,stringsAsFactors = F)
          analyse_ronds_sf_elargi <- st_sf(cbind(analyse_rp_ac()[[1]]$donnees_elargi,dt_ronds_sf))
        }else
        {
          analyse_ronds_sf_elargi <- NULL
        }
        
        centres <- rbind(st_coordinates(analyse_rp_ac()[[2]]))
        row.names(centres) <- c(1:(nrow(analyse_rp_ac()[[1]]$donnees)))
        ronds <- st_sf(geometry=st_sfc(lapply(c(1:nrow(centres)),function(x) st_point(centres[x,])),crs="+init=epsg:4326 +proj=longlat +ellps=WGS84"))
        ronds_pl <- st_buffer(st_transform(ronds,paste0("+init=epsg:",code_epsg_rp_ac())), calcul_rond_rp_ac()*sqrt(analyse_rp_ac()[[1]]$donnees[,varVolume]/calcul_max_rayon_metres_rp_ac()[[2]]))
        # On cree les ronds 
        dt_ronds_sf <- data.frame(ronds_pl,stringsAsFactors = F)
        analyse_ronds_sf <- st_sf(cbind(analyse_rp_ac()[[1]]$donnees,dt_ronds_sf))
        return(list(analyse_ronds_sf,analyse_ronds_sf_elargi))
      })
      
      palette_rp_ac <- reactive({
        
        bornes <- react_bornes_rp_ac()[[1]]
        
        if(is.null(bornes)) return(NULL)
        
        if(elargi_rp_ac()) # On redefini le min et le max de la serie pour eviter les valeurs en NA
        {
          bornes[length(bornes)] <- min(as.numeric(analyse_rp_ac()[[1]]$donnees_elargi[,varRatio]))
          bornes[1] <- max(as.numeric(analyse_rp_ac()[[1]]$donnees_elargi[,varRatio]))
        }else
        {
          bornes[length(bornes)] <- min(as.numeric(analyse_rp_ac()[[1]]$donnees[,varRatio]))
          bornes[1] <- max(as.numeric(analyse_rp_ac()[[1]]$donnees[,varRatio]))
        }
        
        if(length(unique(bornes)) != length(bornes))
        {
          removeModal()
          showModal(modalDialog(HTML(paste0("<font size=+1>Les bornes calculees avec la methode '",input$methode_rp_ac_id,"' ne sont pas uniques. La methode kmeans a donc ete retenue.</font>")), size="l", footer=NULL, style = "color: #fff; background-color: #DF691A; border-color: #2e6da4")) #337ab7
          Sys.sleep(7)
          suppressWarnings(bornes_analyse <- classIntervals(as.numeric(analyse_rp_ac()[[1]]$donnees[,varRatio]),max_classes$a,style="kmeans",rtimes=10,intervalClosure="left"))
          carac_bornes <- calcul_bornes(analyse_rp_ac()[[1]]$donnees,bornes_analyse,varRatio,max_classes$a,"kmeans")
          updateSelectInput(session,"methode_rp_ac_id",choices = methode_calcul, selected="kmeans")
          bornes <- carac_bornes[[1]]
          pal_classes <- carac_bornes[[2]]
        }else
        {
          pal_classes <- react_bornes_rp_ac()[[2]]
        }
        pal_classes[is.na(pal_classes)] <- "grey"
        palette<-colorBin(palette=rev(pal_classes), domain=0:100, bins=bornes, na.color="grey")
        
        return(list(palette,bornes,pal_classes))
      })
      
      fond_territoire_rp_ac <- reactive({
        if(!is.null(fondSuppl))
        {
          fond_territoire <- st_transform(fondSuppl,"+init=epsg:4326 +proj=longlat +ellps=WGS84")
          return(fond_territoire)
        }else
        {
          return(NULL)
        }
      })
      
      fond_region_rp_ac <- reactive({
        fond_region <- st_transform(sf_regm(),"+init=epsg:4326 +proj=longlat +ellps=WGS84")
        return(fond_region)
      })
      
      fond_departement_rp_ac <- reactive({
        fond_departement <- st_transform(sf_depm(),"+init=epsg:4326 +proj=longlat +ellps=WGS84")
        return(fond_departement)
      })
      
      fond_select_donnees_elargi_rp_ac <- reactive({
        req(analyse_ronds_sf_rp_ac(),analyse_rp_ac())
        
        if(elargi_rp_ac())
        {
          fond_donnees_elargi <- analyse_ronds_sf_rp_ac()[[2]][as.data.frame(analyse_ronds_sf_rp_ac()[[2]])[,"CODE"] %in% analyse_rp_ac()[[1]]$donnees_elargi[input$mydonnees_rp_ac_rows_selected,"CODE"],]
          fond_donnees_elargi <- st_transform(fond_donnees_elargi,"+init=epsg:4326 +proj=longlat +ellps=WGS84")
          return(fond_donnees_elargi)
        }else
        {
          return(NULL)
        }
      })
      
      fond_select_donnees_rp_ac <- reactive({
        req(analyse_ronds_sf_rp_ac(),analyse_rp_ac())
        
        fond_donnees <- analyse_ronds_sf_rp_ac()[[1]][as.data.frame(analyse_ronds_sf_rp_ac()[[1]])[,"CODE"] %in% analyse_rp_ac()[[1]]$donnees[input$mydonnees_rp_ac_rows_selected,"CODE"],]
        if(nrow(fond_donnees)>0)
        {
          fond_donnees <- st_transform(fond_donnees,"+init=epsg:4326 +proj=longlat +ellps=WGS84")
          return(fond_donnees)
        }else
        {
          return(NULL)
        }
      })
      
      fond_select_maille_elargi_rp_ac <- reactive({
        req(fond_elargi_rp_ac())
        if(elargi_rp_ac())
        {
          fond_maille_elargi <- fond_elargi_rp_ac()[[2]][as.data.frame(fond_elargi_rp_ac()[[2]])[,"CODE"] %in% as.data.frame(fondMailleElargi)[input$mymaille_rp_ac_rows_selected,"CODE"],]
          return(fond_maille_elargi)
        }else
        {
          return(NULL)
        }
      })
      
      fond_select_maille_rp_ac <- reactive({
        req(fond_contour_maille_rp_ac())
        
        fond_maille <- fond_contour_maille_rp_ac()[[2]][as.data.frame(fond_contour_maille_rp_ac()[[2]])[,"CODE"] %in% as.data.frame(fondMaille)[input$mymaille_rp_ac_rows_selected,"CODE"],]
        return(fond_maille)
      })
      
      fond_select_contour_rp_ac <- reactive({
        req(fond_contour_maille_rp_ac())
        
        fond_contour <- fond_contour_maille_rp_ac()[[1]][as.data.frame(fond_contour_maille_rp_ac()[[1]])[,"CODE"] %in% as.data.frame(fondContour)[input$mycontour_rp_ac_rows_selected,"CODE"],]
        return(fond_contour)
      })
      
      # CONSTRUCTION DE LA MAP EN LEAFLET
      
      react_fond_rp_ac <- reactive({
        
        if(input$menu=="carte")
        {
          showModal(modalDialog(HTML("<i class=\"fa fa-spinner fa-spin fa-2x fa-fw\"></i><font size=+1>\u00c9laboration de la carte...</font> "), size="m", footer=NULL, style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"))
          
          # Construction de la map par defaut
          
          m <- leaflet(padding = 0,
                       options = leafletOptions(
                         preferCanvas = TRUE,
                         transition = 2,
                         minZoom = 6,
                         maxZoom = 10
                       )) %>%
            
            setMapWidgetStyle(list(background = "#AFC9E0")) %>%
            
            addTiles_insee(attribution = paste0("<a href=\"http://www.insee.fr\">\u00A9 IGN - INSEE ",format(Sys.time(), format = "%Y"),"</a>")) %>%
            
            fitBounds(lng1 = min(list_bbox_rp_ac()[[1]]),
                      lat1 = min(list_bbox_rp_ac()[[2]]),
                      lng2 = max(list_bbox_rp_ac()[[1]]),
                      lat2 = max(list_bbox_rp_ac()[[2]])
            ) %>%
            
            # On ajoute une barre d'echelle
            addScaleBar(position = 'bottomright',
                        options = scaleBarOptions(metric = TRUE, imperial = FALSE)
            ) %>%
            
            # Pour gerer l'ordre des calques
            addMapPane(name = "fond_pays", zIndex = 401) %>%
            addMapPane(name = "fond_france", zIndex = 402) %>%
            addMapPane(name = "fond_habillage", zIndex = 403) %>%
            addMapPane(name = "fond_dep", zIndex = 404) %>%
            addMapPane(name = "fond_reg", zIndex = 405) %>%
            addMapPane(name = "fond_territoire", zIndex = 406) %>%
            addMapPane(name = "fond_trio3", zIndex = 407) %>%
            addMapPane(name = "fond_trio2", zIndex = 408) %>%
            addMapPane(name = "fond_trio1", zIndex = 409) %>%
            addMapPane(name = "selection", zIndex = 410) %>%
            
            addMapPane(name = "fond_legende", zIndex = 411)
          
          # AFFICHAGE DES FONDS D'HABILLAGE
          if(dom %in% c("0","973"))
          {
            # fond des pays
            m <- addPolygons(map = m, data = fond_habillage_rp_ac()[[2]][,"LIBGEO"], opacity = 1, # fond_pays sauf la France
                             stroke = TRUE, color = "white",
                             weight = 1,
                             options = pathOptions(pane = "fond_pays", clickable = F),
                             fill = T, fillColor = "#CCCCCC", fillOpacity = 1
                             
            )
          }
          
          # fond de la France metro ou d'un DOM
          m <- addPolygons(map = m, data = fond_habillage_rp_ac()[[1]][,"LIBGEO"], opacity = 1, # fond_france
                           stroke = TRUE, color = "black",
                           weight = 1.5,
                           options = pathOptions(pane = "fond_france", clickable = F),
                           fill = T, fillColor = "white", fillOpacity = 1
          )
          
          m_save_rp_ac$a <- m
          
          # AFFICHAGE DU FOND TERRITOIRE
          
          if(!is.null(fondSuppl))
          {
            m <- addPolygons(map = m, data = fond_territoire_rp_ac(),
                             stroke = TRUE, color = "#BFBFBF", opacity = 1,
                             weight = 0.5,
                             options = pathOptions(pane = "fond_territoire", clickable = T),
                             popup = paste0("<b> <font color=#2B3E50>",as.data.frame(fond_territoire_rp_ac())[,"LIBELLE"], "</font> </b>"),
                             fill = T, fillColor = "white", fillOpacity = 0.001,
                             group = "territoire"
            )
          }
          
          # AFFICHAGE DES FONDS CONTOUR ET MAILLE
          
          # fond du contour
          m <- addPolygons(map = m, data = fond_contour_maille_rp_ac()[[1]], opacity = 0.3, #contour_WGS84
                           stroke = TRUE, color = "black", weight = 3,
                           options = pathOptions(pane = "fond_trio3", clickable = T),
                           popup = paste0("<b> <font color=#2B3E50>",as.data.frame(fond_contour_maille_rp_ac()[[1]])[,"LIBELLE"], "</font> </b>"),
                           fill = T, fillColor = "white", fillOpacity = 0.3,
                           group = "maille_contour"
          )
          
          # fond de la maille
          m <- addPolygons(map = m, data = fond_contour_maille_rp_ac()[[2]], opacity = 1, #maille_WGS84
                           stroke = TRUE, color = "grey", weight = 1,
                           options = pathOptions(pane = "fond_trio2", clickable = T),
                           popup = paste0("<b> <font color=#2B3E50>",as.data.frame(fond_contour_maille_rp_ac()[[2]])[,"LIBELLE"], "</font> </b>"),
                           fill = T, fillColor = "white", fillOpacity = 0.001,
                           group = "maille_contour"
          )
          
          # AFFICHAGE DE L'ANALYSE
          analyse <- k_ronds(fondMaille,fondMailleElargi,names(fondMaille)[1],data,"CODE",varVolume,FALSE,"centroid",fondChx)
          analyse$donnees[,"TXT1"] <- paste0("<b> <font color=#2B3E50>",format(analyse$donnees$save, big.mark=" ",decimal.mark=",",nsmall=0),"</font></b>")
          analyse_WGS84 <- st_transform(analyse$analyse_points,"+init=epsg:4326 +proj=longlat +ellps=WGS84")
          
          suppressWarnings(test_analyse_maille_classe <- try(analyse$donnees[rev(order(analyse$donnees[,varVolume])),varRatio],silent=T))
          if(class(test_analyse_maille_classe) %in% "try-error")
          {
            return(NULL)
          }else
          {  
            analyse_maille_classe <- analyse$donnees[rev(order(analyse$donnees[,varVolume])),varRatio]
          }
          
          bornes <- react_bornes_init_rp_ac()[[1]]
          suppressWarnings(bornes[length(bornes)] <- min(as.numeric(analyse$donnees[,varRatio])))
          suppressWarnings(bornes[1] <- max(as.numeric(analyse$donnees[,varRatio])))
          
          pal_classes <- react_bornes_init_rp_ac()[[2]]
          pal_classes[is.na(pal_classes)] <- "grey"
          palette<-colorBin(palette=rev(pal_classes), domain=0:100, bins=bornes, na.color="grey")
          
          m <- addCircles(map = m,
                          lng = st_coordinates(analyse_WGS84)[,1],
                          lat = st_coordinates(analyse_WGS84)[,2],
                          stroke = TRUE, color = "white",
                          opacity = 1,
                          weight = 1,
                          radius = (calcul_max_rayon_metres_rp_ac()[[1]]/1.25)*sqrt(analyse$donnees[,varVolume]/calcul_max_rayon_metres_rp_ac()[[2]]),
                          options = pathOptions(pane = "fond_trio1", clickable = T),
                          popup = paste0("<b> <font color=#2B3E50>",varVolume," : </font></b>",analyse$donnees$TXT1,"<br>",
                                         "<b> <font color=#2B3E50>",varRatio," : </font></b>",analyse$donnees$TXT2),
                          fill = T,
                          fillColor = palette(analyse_maille_classe),
                          fillOpacity = 1,
                          group = "taille"
          )
          
          removeModal()
          
          showModal(modalDialog(HTML("<font size=+1>Veuillez patientez svp, la carte va s'afficher dans quelques secondes...<br><br><i class=\"fa fa-hand-pointer-o fa-fw\"></i><b>Double-cliquez</b> ensuite sur la carte pour afficher la l\u00e9gende.</font> "), size="m", footer=NULL, easyClose = TRUE, style = "color: #fff; background-color: #DF691A; border-color: #2e6da4")) #337ab7
          
          return(m)
        }
      })
      
      # MODIFICATION DES FONDS TERRITOIRE D'ETUDES, REGION ET DEPARTEMENT
      
      observeEvent(input$ajout_territoire_rp_ac_id,{
        
        proxy <- leafletProxy("mymap_rp_ac")
        
        clearGroup(map = proxy, group = "territoire")
        
        #fond du territoire d'etudes
        if(!is.null(fondSuppl))
        {
          if(input$ajout_territoire_rp_ac_id)
          {
            proxy <- addPolygons(map = proxy, data = fond_territoire_rp_ac(),
                                 stroke = TRUE, color = "#BFBFBF", opacity = 1,
                                 weight = 0.5,
                                 options = pathOptions(pane = "fond_territoire", clickable = T),
                                 popup = paste0("<b> <font color=#2B3E50>",as.data.frame(fond_territoire_rp_ac())[,"LIBELLE"], "</font> </b>"),
                                 fill = T, fillColor = "white", fillOpacity = 0.001,
                                 group = "territoire"
            )
          }
        }
      },ignoreInit = TRUE)
      
      observeEvent(input$ajout_reg_rp_ac_id,{
        
        proxy <- leafletProxy("mymap_rp_ac")
        
        clearGroup(map = proxy, group = "region")
        
        if(dom=="0")
        {
          if(input$ajout_reg_rp_ac_id)
          {
            #fond des regions
            proxy <- addPolygons(map = proxy, data = fond_region_rp_ac(),
                                 stroke = TRUE, color = "grey", opacity = 1,
                                 weight = 1.5,
                                 options = pathOptions(pane = "fond_reg", clickable = F),
                                 fill = F,
                                 group = "region"
            )
          }
        }
      },ignoreInit = TRUE)
      
      observeEvent(input$ajout_dep_rp_ac_id,{
        
        proxy <- leafletProxy("mymap_rp_ac")
        
        clearGroup(map = proxy, group = "departement")
        
        if(dom=="0")
        {
          if(input$ajout_dep_rp_ac_id)
          {
            #fond des departements
            proxy <- addPolygons(map = proxy, data = fond_departement_rp_ac(),
                                 stroke = TRUE, color = "grey", opacity = 1,
                                 weight = 0.5,
                                 options = pathOptions(pane = "fond_dep", clickable = F),
                                 fill = F,
                                 group = "departement"
            )
          }
        }
      },ignoreInit = TRUE)
      
      # MODIFICATION DE L'ORDRE DES CALQUES
      
      observeEvent(list(input$monter_fond_rp_ac_id,input$descendre_fond_rp_ac_id),{
        
        if(as.numeric(input$monter_fond_rp_ac_id)==0 & as.numeric(input$descendre_fond_rp_ac_id)==0) return(NULL)
        
        proxy <- leafletProxy("mymap_rp_ac")
        
        clearGroup(map = proxy, group = "maille_contour")
        clearGroup(map = proxy, group = "taille")
        
        i <- 1 #pour gerer l'ordre des fonds dans le pane
        for(fond in liste_fonds$a)
        {
          if(fond=="analyse")
          {
            suppressWarnings(test_analyse_maille_classe <- try(analyse_rp_ac()[[1]]$donnees[rev(order(analyse_rp_ac()[[1]]$donnees[,varVolume])),varRatio],silent=T))
            if(class(test_analyse_maille_classe) %in% "try-error")
            {
              return(NULL)
            }else
            {
              analyse_maille_classe <- analyse_rp_ac()[[1]]$donnees[rev(order(analyse_rp_ac()[[1]]$donnees[,varVolume])),varRatio]
            }
            
            proxy <- addCircles(map = proxy,
                                lng = st_coordinates(analyse_rp_ac()[[2]])[,1],
                                lat = st_coordinates(analyse_rp_ac()[[2]])[,2],
                                stroke = TRUE, color = "white",
                                opacity = 1,
                                weight = 1,
                                radius = calcul_rond_rp_ac()*sqrt(analyse_rp_ac()[[1]]$donnees[,varVolume]/calcul_max_rayon_metres_rp_ac()[[2]]),
                                options = pathOptions(pane = paste0("fond_trio",i), clickable = T),
                                popup = paste0("<b> <font color=#2B3E50>",varVolume," : </font></b>",analyse_rp_ac()[[1]]$donnees$TXT1,"<br>",
                                               "<b> <font color=#2B3E50>",varRatio," : </font></b>",analyse_rp_ac()[[1]]$donnees$TXT2),
                                fill = T,
                                fillColor = palette_rp_ac()[[1]](analyse_maille_classe),
                                fillOpacity = 1,
                                group = "taille"
            )
            
            ordre_analyse$a <- i
          }
          
          if(fond=="maille")
          {
            proxy <- addPolygons(map = proxy, data = fond_contour_maille_rp_ac()[[2]], opacity = 1, #maille_WGS84
                                 stroke = TRUE, color = "grey", weight = 1,
                                 options = pathOptions(pane = paste0("fond_trio",i), clickable = T),
                                 popup = paste0("<b> <font color=#2B3E50>",as.data.frame(fond_contour_maille_rp_ac()[[2]])[,"LIBELLE"], "</font> </b>"),
                                 fill = T, fillColor = "white", fillOpacity = 0.001,
                                 group = "maille_contour"
            )
          }
          
          if(fond=="contour")
          {
            proxy <- addPolygons(map = proxy, data = fond_contour_maille_rp_ac()[[1]], opacity = 0.3, #contour_WGS84
                                 stroke = TRUE, color = "black", weight = 3,
                                 options = pathOptions(pane = paste0("fond_trio",i), clickable = T),
                                 popup = paste0("<b> <font color=#2B3E50>",as.data.frame(fond_contour_maille_rp_ac()[[1]])[,"LIBELLE"], "</font> </b>"),
                                 fill = T, fillColor = "white", fillOpacity = 0.3,
                                 group = "maille_contour"
            )
          }
          
          i <- i + 1
        }
        
      },ignoreInit = TRUE)
      
      # MODIFICATION DE LA TAILLE DES RONDS, DU NOMBRE DE CLASSES, DE LA METHODE OU DES BORNES
      
      observeEvent(list(input$taille_rond_rp_ac_id,input$nb_classes_rp_ac_id,input$methode_rp_ac_id,input$valid_bornes_rp_ac_id),{
        req(input$taille_rond_rp_ac_id,input$nb_classes_rp_ac_id,input$methode_rp_ac_id)
        req(calcul_rond_rp_ac())
        
        proxy <- leafletProxy("mymap_rp_ac")
        
        clearGroup(map = proxy, group = "taille")
        
        suppressWarnings(test_analyse_maille_classe <- try(analyse_rp_ac()[[1]]$donnees[rev(order(analyse_rp_ac()[[1]]$donnees[,varVolume])),varRatio],silent=T))
        if(class(test_analyse_maille_classe) %in% "try-error")
        {
          return(NULL)
        }else
        {
          analyse_maille_classe <- analyse_rp_ac()[[1]]$donnees[rev(order(analyse_rp_ac()[[1]]$donnees[,varVolume])),varRatio]
        }
        
        proxy <- addCircles(map = proxy,
                            lng = st_coordinates(analyse_rp_ac()[[2]])[,1],
                            lat = st_coordinates(analyse_rp_ac()[[2]])[,2],
                            stroke = TRUE, color = "white",
                            opacity = 1,
                            weight = 1,
                            radius = calcul_rond_rp_ac()*sqrt(analyse_rp_ac()[[1]]$donnees[,varVolume]/calcul_max_rayon_metres_rp_ac()[[2]]),
                            options = pathOptions(pane = paste0("fond_trio",ordre_analyse$a), clickable = T),
                            popup = paste0("<b> <font color=#2B3E50>",varVolume," : </font></b>",analyse_rp_ac()[[1]]$donnees$TXT1,"<br>",
                                           "<b> <font color=#2B3E50>",varRatio," : </font></b>",analyse_rp_ac()[[1]]$donnees$TXT2),
                            fill = T,
                            fillColor = palette_rp_ac()[[1]](analyse_maille_classe),
                            fillOpacity = 1,
                            group = "taille"
        )
        
      },ignoreInit = TRUE)
      
      # MODIFICATION DE LA POSITION DES RONDS (CHX OU CENTROID)
      
      observeEvent(input$choix_centroid_rp_ac_id,{
        req(input$choix_centroid_rp_ac_id)
        
        proxy <- leafletProxy("mymap_rp_ac")
        
        clearGroup(map = proxy, group = "taille")
        
        suppressWarnings(test_analyse_maille_classe <- try(analyse_rp_ac()[[1]]$donnees[rev(order(analyse_rp_ac()[[1]]$donnees[,varVolume])),varRatio],silent=T))
        if(class(test_analyse_maille_classe) %in% "try-error")
        {
          return(NULL)
        }else
        {
          analyse_maille_classe <- analyse_rp_ac()[[1]]$donnees[rev(order(analyse_rp_ac()[[1]]$donnees[,varVolume])),varRatio]
        }
        
        proxy <- addCircles(map = proxy,
                            lng = st_coordinates(analyse_rp_ac()[[2]])[,1],
                            lat = st_coordinates(analyse_rp_ac()[[2]])[,2],
                            stroke = TRUE, color = "white",
                            opacity = 1,
                            weight = 1,
                            radius = calcul_rond_rp_ac()*sqrt(analyse_rp_ac()[[1]]$donnees[,varVolume]/calcul_max_rayon_metres_rp_ac()[[2]]),
                            options = pathOptions(pane = paste0("fond_trio",ordre_analyse$a), clickable = T),
                            popup = paste0("<b> <font color=#2B3E50>",varVolume," : </font></b>",analyse_rp_ac()[[1]]$donnees$TXT1,"<br>",
                                           "<b> <font color=#2B3E50>",varRatio," : </font></b>",analyse_rp_ac()[[1]]$donnees$TXT2),
                            fill = T,
                            fillColor = palette_rp_ac()[[1]](analyse_maille_classe),
                            fillOpacity = 1,
                            group = "taille"
        )
        
      },ignoreInit = TRUE)
      
      # MODIFICATION DE LA REPRESENTATION ELARGIE
      
      observeEvent(list(input$elargi_rp_ac_id,input$opacite_elargi_rp_ac_id,input$taille_rond_rp_ac_id,input$nb_classes_rp_ac_id,input$methode_rp_ac_id,input$valid_bornes_rp_ac_id,input$choix_centroid_rp_ac_id),{
        req(input$elargi_rp_ac_id,input$opacite_elargi_rp_ac_id)
        
        proxy <- leafletProxy("mymap_rp_ac")
        
        clearGroup(map = proxy, group = "elargi")
        
        if(elargi_rp_ac())
        {
          analyse_maille_classe_elargi <- analyse_rp_ac()[[1]]$donnees_elargi[rev(order(analyse_rp_ac()[[1]]$donnees_elargi[,varVolume])),varRatio]
          
          proxy <- addCircles(map = proxy,
                              lng = st_coordinates(fond_elargi_rp_ac()[[1]])[,1],
                              lat = st_coordinates(fond_elargi_rp_ac()[[1]])[,2],
                              stroke = TRUE, color = "white",
                              opacity = input$opacite_elargi_rp_ac_id/100,
                              weight = 1,
                              radius = calcul_rond_rp_ac()*sqrt(analyse_rp_ac()[[1]]$donnees_elargi[,varVolume]/calcul_max_rayon_metres_rp_ac()[[2]]),
                              options = pathOptions(pane = "fond_trio3", clickable = T),
                              popup = paste0("<b> <font color=#2B3E50>",varVolume," : </font></b>",analyse_rp_ac()[[1]]$donnees_elargi$TXT1,"<br>",
                                             "<b> <font color=#2B3E50>",varRatio," : </font></b>",analyse_rp_ac()[[1]]$donnees_elargi$TXT2),
                              fill = T,
                              fillColor = palette_rp_ac()[[1]](analyse_maille_classe_elargi),
                              fillOpacity = input$opacite_elargi_rp_ac_id/100,
                              group = "elargi"
          )
          
          proxy <- addPolygons(map = proxy, data = fond_elargi_rp_ac()[[2]], opacity = 1, #maille_WGS84
                               stroke = TRUE, color = "grey", weight = 1,
                               options = pathOptions(pane = "fond_trio3", clickable = T),
                               popup = paste0("<b> <font color=#2B3E50>",as.data.frame(fond_elargi_rp_ac()[[2]])[,"LIBELLE"], "</font> </b>"),
                               fill = T, fillColor = "white", fillOpacity = 0.001,
                               group = "elargi"
          )
          
        }
      },ignoreInit = TRUE)
      
      # MODIFICATION DE LA SELECTION DES OBJETS VIA LES TABLEAUX
      
      observeEvent(list(input$onglets_rp_ac,input$choix_centroid_rp_ac_id),{
        req(input$onglets_rp_ac)
        
        if(input$onglets_rp_ac == "carte")
        {
          proxy <- leafletProxy("mymap_rp_ac")
          
          clearGroup(map = proxy, group = "select_donnees")
          
          if(!is.null(input$mydonnees_rp_ac_rows_selected))
          {
            if(elargi_rp_ac())
            {
              suppressWarnings(proxy <- addCircles(map = proxy,
                                                   lng = st_coordinates(st_centroid(fond_select_donnees_elargi_rp_ac()))[,1],
                                                   lat = st_coordinates(st_centroid(fond_select_donnees_elargi_rp_ac()))[,2],
                                                   stroke = TRUE, color = "#FFFF00",
                                                   opacity = 1,
                                                   weight = 3,
                                                   radius = calcul_rond_rp_ac()*sqrt(analyse_rp_ac()[[1]]$donnees_elargi[analyse_rp_ac()[[1]]$donnees_elargi[,"CODE"] %in% analyse_rp_ac()[[1]]$donnees_elargi[input$mydonnees_rp_ac_rows_selected,"CODE"],varVolume]/calcul_max_rayon_metres_rp_ac()[[2]]),
                                                   options = pathOptions(pane = "selection", clickable = F),
                                                   fill = F,
                                                   group = "select_donnees")
              )
            }else
            {
              suppressWarnings(proxy <- addCircles(map = proxy,
                                                   lng = st_coordinates(st_centroid(fond_select_donnees_rp_ac()))[,1],
                                                   lat = st_coordinates(st_centroid(fond_select_donnees_rp_ac()))[,2],
                                                   stroke = TRUE, color = "#FFFF00",
                                                   opacity = 1,
                                                   weight = 3,
                                                   radius = calcul_rond_rp_ac()*sqrt(analyse_rp_ac()[[1]]$donnees[analyse_rp_ac()[[1]]$donnees[,"CODE"] %in% analyse_rp_ac()[[1]]$donnees[input$mydonnees_rp_ac_rows_selected,"CODE"],varVolume]/calcul_max_rayon_metres_rp_ac()[[2]]),
                                                   options = pathOptions(pane = "selection", clickable = F),
                                                   fill = F,
                                                   group = "select_donnees")
              )
            }
          }
        }
      },ignoreInit = TRUE)
      
      observeEvent(input$onglets_rp_ac,{
        req(input$onglets_rp_ac)
        
        if(input$onglets_rp_ac == "carte")
        {
          proxy <- leafletProxy("mymap_rp_ac")
          
          clearGroup(map = proxy, group = "select_maille")
          
          if(!is.null(input$mymaille_rp_ac_rows_selected))
          {
            if(elargi_rp_ac())
            {
              proxy <- addPolygons(map = proxy, data = fond_select_maille_elargi_rp_ac(),
                                   stroke = FALSE,
                                   options = pathOptions(pane = "selection", clickable = F),
                                   fill = T,
                                   fillColor = "#FFFF00",
                                   fillOpacity = 90,
                                   group = "select_maille"
              )
            }else
            {
              proxy <- addPolygons(map = proxy, data = fond_select_maille_rp_ac(),
                                   stroke = FALSE,
                                   options = pathOptions(pane = "selection", clickable = F),
                                   fill = T,
                                   fillColor = "#FFFF00",
                                   fillOpacity = 90,
                                   group = "select_maille"
              )
            }
          }
        }
      },ignoreInit = TRUE)
      
      observeEvent(input$onglets_rp_ac,{
        req(input$onglets_rp_ac)
        
        if(input$onglets_rp_ac == "carte")
        {
          proxy <- leafletProxy("mymap_rp_ac")
          
          clearGroup(map = proxy, group = "select_contour")
          
          if(!is.null(input$mycontour_rp_ac_rows_selected))
          {
            proxy <- addPolygons(map = proxy, data = fond_select_contour_rp_ac(),
                                 stroke = FALSE,
                                 options = pathOptions(pane = "selection", clickable = F),
                                 fill = T,
                                 fillColor = "#FFFF00",
                                 fillOpacity = 90,
                                 group = "select_contour"
            )
          }
        }
      },ignoreInit = TRUE)
      
      # CONSTRUCTION DE LA LEGENDE
      
      lon_lat_rp_ac <- reactive({
        click <- input$mymap_rp_ac_click
        lon <- click$lng
        lat <- click$lat
        return(list(lon,lat))
      })
      
      observeEvent(list(input$mymap_rp_ac_zoom,input$mymap_rp_ac_click,input$type_legende_rp_ac_id,input$titre_ronds_legende_rp_ac_id,input$titre_classes_legende_rp_ac_id,input$taille_rond_rp_ac_id,input$nb_classes_rp_ac_id,input$methode_rp_ac_id,input$valid_bornes_rp_ac_id),{
        req(input$taille_rond_rp_ac_id)
        
        if(is.null(input$affiche_legende_rp_ac_id)) return(NULL)
        if(input$affiche_legende_rp_ac_id==FALSE) return(NULL)
        if(is.null(lon_lat_rp_ac()[[1]])) return(NULL)
        CONSTRUCTION_LEGENDE_RP_AC()
      })
      
      construction_legende_rp_ac <- reactive({
        zoom <- as.numeric(input$mymap_rp_ac_zoom)
        coeff <- ((360/(2^zoom))/7.2) # Permet de fixer une distance sur l'ecran. Il s'agit en gros d'une conversion des degres en pixels. Reste constant a longitude egale mais varie un peu selon la latitude
        
        ronds_leg <- construction_ronds_legende(lon_lat_rp_ac()[[1]],lon_lat_rp_ac()[[2]],code_epsg_rp_ac(),input$taille_rond_rp_ac_id)
        lignes <- construction_lignes_legende(ronds_leg,coeff,code_epsg_rp_ac())
        ronds_leg[[2]] <- cbind(ronds_leg[[2]],VALEUR=c(max(data[,varVolume]),max(data[,varVolume])/3))
        
        return(list(ronds_leg,lignes,coeff))
      })
      
      CONSTRUCTION_LEGENDE_RP_AC <- function()
      {
        proxy <- leafletProxy("mymap_rp_ac")
        
        proxy <- clearGroup(map=proxy, group="leg")
        proxy <- clearMarkers(map=proxy)
        
        zoom <- as.numeric(input$mymap_rp_ac_zoom)
        coeff <- ((360/(2^zoom))/7.2) # Permet de fixer une distance sur l'ecran. Il s'agit en gros d'une conversion des degres en pixels. Reste constant a longitude egale mais varie un peu selon la latitude
        
        ronds_leg <- construction_ronds_legende(lon_lat_rp_ac()[[1]],lon_lat_rp_ac()[[2]],code_epsg_rp_ac(),input$taille_rond_rp_ac_id)
        lignes <- construction_lignes_legende(ronds_leg,coeff,code_epsg_rp_ac())
        position_leg_classes <- t(data_frame(c(min(st_coordinates(ronds_leg[[1]])[which(st_coordinates(ronds_leg[[1]])[,4]==1),"X"]),min(st_coordinates(ronds_leg[[1]])[which(st_coordinates(ronds_leg[[1]])[,4]==1),"Y"]))))
        
        if(is.null(input$type_legende_rp_ac_id)) return(NULL)
        
        if(input$type_legende_rp_ac_id==1) # Litterale
        {
          # On cree les rectangles
          if(is.null(input$nb_classes_rp_ac_id))
            max_classes <- 4
          else
            max_classes <- input$nb_classes_rp_ac_id
          
          for(i in 1:max_classes)
          {
            # Coordonnees du point haut/gauche des rectangles de la legende
            x_coord_rectangle <- position_leg_classes[1]
            if(i==1) #1er rectangle
            {
              y_coord_rectangle <- position_leg_classes[2]-coeff*1.5 #1,5cm sous les ronds
            }else
            {
              y_coord_rectangle <- y_coord_rectangle-coeff*0.7
            }
            assign(paste0("rectangle_",i),list(matrix(c(x_coord_rectangle,y_coord_rectangle,x_coord_rectangle+coeff*1,y_coord_rectangle,x_coord_rectangle+coeff*1,y_coord_rectangle+coeff*0.5,x_coord_rectangle,y_coord_rectangle+coeff*0.5,x_coord_rectangle,y_coord_rectangle),ncol=2, byrow=TRUE)))
          }
          
          # On ajoute un cadre blanc autour de la legende
          y_coord_rectangle <- min(get(paste0("rectangle_",max_classes))[[1]][,2])
          
          # leaflet du cadre blanc en 1er
          proxy <- addRectangles(map = proxy,
                                 lng1 = position_leg_classes[1]-coeff*0.5, lat1 = lignes[[5]]+coeff*0.5,
                                 lng2 = x_coord_rectangle+coeff*6, lat2 = y_coord_rectangle-coeff*0.8,
                                 stroke = TRUE,
                                 color = paste0("#2B3E50", ";background: #ffffff;
                                                border-left:2px solid #2B3E50;
                                                border-right:2px solid #2B3E50;
                                                border-top:2px solid #2B3E50;
                                                border-bottom:2px solid #2B3E50;
                                                border-radius: 5%"),
                                 weight = 1,
                                 options = pathOptions(pane = "fond_legende", clickable = F),
                                 fill = T,
                                 fillColor = "white",
                                 fillOpacity = 0.8,
                                 group="leg"
                                 )
          
          # leaflet rectangles et valeurs classes
          classes_leg_texte <- analyse_leg_rp_ac()$rupture_classes
          
          legende$a <- c()
          for(i in 1: max_classes)
          {
            proxy <- addPolygons(map = proxy, data = st_polygon(get(paste0("rectangle_",i))),
                                 stroke = FALSE,
                                 options = pathOptions(pane = "fond_legende", clickable = F),
                                 fill = T,
                                 fillColor = analyse_leg_rp_ac()$pal_classes[i],
                                 fillOpacity = 1,
                                 group="leg"
            )
            
            if(i==1)
            {
              label_rectangle <- paste0(format(round(classes_leg_texte[i+1],3), big.mark=" ",decimal.mark=",",nsmall=0)," et plus")
              
              proxy <- addLabelOnlyMarkers(map = proxy,
                                           lng = (max(get(paste0("rectangle_",i))[[1]][,1])+coeff*0.1), lat = mean(get(paste0("rectangle_",i))[[1]][,2]),
                                           label = label_rectangle,
                                           labelOptions = labelOptions(noHide = T, textOnly = TRUE, direction = "right",
                                                                       style = list(
                                                                         "color" = "black",
                                                                         "font-size" = "12px"
                                                                       )),
                                           group="leg"
              )
              legende$a <- c(legende$a,label_rectangle)
              
            }else if (i>1 && i<max_classes)
            {
              label_rectangle <- paste0("De ", format(round(classes_leg_texte[i+1],3), big.mark=" ",decimal.mark=",",nsmall=0)," \u00E0 moins de ", format(round(classes_leg_texte[i],3), big.mark=" ",decimal.mark=",",nsmall=0))
              
              proxy <- addLabelOnlyMarkers(map = proxy,
                                           lng = max(get(paste0("rectangle_",i))[[1]][,1])+coeff*0.1, lat = mean(get(paste0("rectangle_",i))[[1]][,2]),
                                           label = label_rectangle,
                                           labelOptions = labelOptions(noHide = T, textOnly = TRUE, direction = "right",
                                                                       style = list(
                                                                         "color" = "black",
                                                                         "font-size" = "12px"
                                                                       )),
                                           group="leg"
              )
              legende$a <- c(legende$a,label_rectangle)
              
            }else #i==length(max_classes)
            {
              label_rectangle <- paste0("Moins de ", format(round(classes_leg_texte[i],3), big.mark=" ",decimal.mark=",",nsmall=0))
              
              proxy <- addLabelOnlyMarkers(map = proxy,
                                           lng = max(get(paste0("rectangle_",i))[[1]][,1])+coeff*0.1, lat = mean(get(paste0("rectangle_",i))[[1]][,2]),
                                           label = label_rectangle,
                                           labelOptions = labelOptions(noHide = T, textOnly = TRUE, direction = "right",
                                                                       style = list(
                                                                         "color" = "black",
                                                                         "font-size" = "12px"
                                                                       )),
                                           group="leg"
              )
              legende$a <- c(legende$a,label_rectangle)
              
            }
          }
          # leaflet titre 2
          x_titre_2 <- min(st_coordinates(st_polygon(get("rectangle_1")))[,"X"])
          y_titre_2 <- max(st_coordinates(st_polygon(get("rectangle_1")))[,"Y"])+coeff*0.2
          
          proxy <- addLabelOnlyMarkers(map = proxy,
                                       lng = x_titre_2, lat = y_titre_2,
                                       label = input$titre_classes_legende_rp_ac_id,
                                       labelOptions = labelOptions(noHide = T, textOnly = TRUE, direction = "right",
                                                                   style = list(
                                                                     "color" = "black",
                                                                     "font-size" = "14px"
                                                                   )),
                                       group="leg"
          )
        }
        
        if(input$type_legende_rp_ac_id==2) # Numerique
        {
          # On cree les rectangles
          if(is.null(input$nb_classes_rp_ac_id)) return(NULL)
          
          max_classes <- input$nb_classes_rp_ac_id
          
          for(i in 1:max_classes)
          {
            # Coordonnees du point haut/gauche des rectangles de la legende
            x_coord_rectangle <- position_leg_classes[1]
            if(i==1) #1er rectangle
            {
              y_coord_rectangle <- position_leg_classes[2]-coeff
            }else
            {
              y_coord_rectangle <- y_coord_rectangle-coeff*0.5
            }
            assign(paste0("rectangle_",i),list(matrix(c(x_coord_rectangle,y_coord_rectangle,x_coord_rectangle+coeff*1,y_coord_rectangle,x_coord_rectangle+coeff*1,y_coord_rectangle+coeff*0.5,x_coord_rectangle,y_coord_rectangle+coeff*0.5,x_coord_rectangle,y_coord_rectangle),ncol=2, byrow=TRUE)))
          }
          
          # On ajoute un cadre blanc autour de la legende
          y_coord_rectangle <- min(get(paste0("rectangle_",max_classes))[[1]][,2])
          
          # leaflet du cadre blanc en 1er
          proxy <- addRectangles(map = proxy,
                                 lng1 = position_leg_classes[1]-coeff*0.5, lat1 = lignes[[5]]+coeff*0.5,
                                 lng2 = x_coord_rectangle+coeff*6, lat2 = y_coord_rectangle-coeff*0.8,
                                 stroke = TRUE,
                                 color = paste0("#2B3E50", ";background: #ffffff;
                                                border-left:2px solid #2B3E50;
                                                border-right:2px solid #2B3E50;
                                                border-top:2px solid #2B3E50;
                                                border-bottom:2px solid #2B3E50;
                                                border-radius: 5%"),
                                 weight = 1,
                                 options = pathOptions(pane = "fond_legende", clickable = F),
                                 fill = T,
                                 fillColor = "white",
                                 fillOpacity = 0.8,
                                 group="leg"
                                 )
          
          # leaflet rectangles et valeurs classes
          classes_leg_num <- analyse_leg_rp_ac()$rupture_classes
          
          for(i in 1: max_classes)
          {
            proxy <- addPolygons(map = proxy, data = st_polygon(get(paste0("rectangle_",i))),
                                 stroke = FALSE,
                                 options = pathOptions(pane = "fond_legende", clickable = F),
                                 fill = T,
                                 fillColor = analyse_leg_rp_ac()$pal_classes[i],
                                 fillOpacity = 1,
                                 group="leg"
            )
            
            if(i<max_classes)
            {
              x1 <- max(get(paste0("rectangle_",i))[[1]][,1])
              y1 <- min(get(paste0("rectangle_",i))[[1]][,2])
              x2 <- max(get(paste0("rectangle_",i))[[1]][,1])+coeff*0.2
              y2 <- min(get(paste0("rectangle_",i))[[1]][,2])
              ligne <- st_linestring(rbind(c(x1,y1),c(x2,y2)))
              
              proxy <- addPolygons(map = proxy, data = ligne,
                                   color = "black",
                                   weight = 1,
                                   options = pathOptions(pane = "fond_legende", clickable = F),
                                   fill = F,
                                   fillOpacity = 1,
                                   group="leg"
              )
              
              proxy <- addLabelOnlyMarkers(map = proxy,
                                           lng = x2, lat = y2,
                                           label = as.character(format(round(classes_leg_num[i+1],3),big.mark=" ",decimal.mark=",",nsmall=0)),
                                           labelOptions = labelOptions(noHide = T, textOnly = TRUE, direction = "right",
                                                                       style = list(
                                                                         "color" = "black",
                                                                         "font-size" = "12px"
                                                                       )),
                                           group="leg"
              )
              
            }
          }
          
          # On ajoute la legende de classes a l'analyse
          
          # leaflet titre 2
          x_titre_2 <- min(get("rectangle_1")[[1]][,1])
          y_titre_2 <- max(get("rectangle_1")[[1]][,2])+coeff*0.2
          
          proxy <- addLabelOnlyMarkers(map = proxy,
                                       lng = x_titre_2, lat = y_titre_2,
                                       label = input$titre_classes_legende_rp_ac_id,
                                       labelOptions = labelOptions(noHide = T, textOnly = TRUE, direction = "right",
                                                                   style = list(
                                                                     "color" = "black",
                                                                     "font-size" = "14px"
                                                                   )),
                                       group="leg"
          )
        }
        
        suppressWarnings(proxy <- addCircles(map = proxy,
                                             lng = st_coordinates(st_centroid(ronds_leg[[1]]))[,1],
                                             lat = st_coordinates(st_centroid(ronds_leg[[1]]))[,2],
                                             stroke = TRUE,
                                             opacity = 1,
                                             color = "#2B3E50",
                                             weight = 2,
                                             radius = c(calcul_rond_rp_ac(),calcul_rond_rp_ac()/sqrt(3)),
                                             options = pathOptions(pane = "fond_legende", clickable = F),
                                             fill = T,
                                             fillColor = "white",
                                             fillOpacity = 1,
                                             group="leg"
        ))
        
        # leaflet lignes
        proxy <- addPolygons(map = proxy, data = lignes[[1]],
                             stroke = TRUE,
                             opacity = 1,
                             color = "#2B3E50",
                             weight = 2,
                             options = pathOptions(pane = "fond_legende", clickable = F),
                             fill = F,
                             fillOpacity = 1,
                             group="leg"
        )
        
        # leaflet valeur ronds
        proxy <- addLabelOnlyMarkers(map = proxy,
                                     lng = lignes[[2]][2,1], lat = lignes[[2]][2,2], #ligne_grand
                                     label = as.character(format(calcul_max_rayon_metres_rp_ac()[[2]],big.mark=" ",decimal.mark=",",nsmall=0)),
                                     labelOptions = labelOptions(noHide = T, textOnly = TRUE, direction = "right",
                                                                 style = list(
                                                                   "color" = "black",
                                                                   "font-size" = "12px"
                                                                 )),
                                     group="leg"
        )
        
        proxy <- addLabelOnlyMarkers(map = proxy,
                                     lng = lignes[[3]][2,1], lat = lignes[[3]][2,2], #ligne_petit
                                     label = as.character(format(round(calcul_max_rayon_metres_rp_ac()[[2]]/3,0),big.mark=" ",decimal.mark=",",nsmall=0)),
                                     labelOptions = labelOptions(noHide = T, textOnly = TRUE, direction = "right",
                                                                 style = list(
                                                                   "color" = "black",
                                                                   "font-size" = "12px"
                                                                 )),
                                     group="leg"
        )
        
        #leaflet titre 1
        proxy <- addLabelOnlyMarkers(map = proxy,
                                     lng = lignes[[4]], lat = lignes[[5]], #x_titre_1 et y_titre_1
                                     label = input$titre_ronds_legende_rp_ac_id,
                                     labelOptions = labelOptions(noHide = T, textOnly = TRUE, direction = "right",
                                                                 style = list(
                                                                   "color" = "black",
                                                                   "font-size" = "14px"
                                                                 )),
                                     group="leg"
        )
      }
      
      # AJOUT DES ONGLETS SAUVEGARDE
      
      observeEvent(input$save_carte_rp_ac_id,{
        
        showModal(modalDialog(HTML("<i class=\"fa fa-spinner fa-spin fa-2x fa-fw\"></i><font size=+1>Sauvegarde de la carte en cours...</font> "), size="m", footer=NULL, style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"))
        
        insert_save$a <- insert_save$a + 1
        nb_save_carte <- insert_save$a-remove_carte$a
        
        m_save <- m_save_rp_ac$a
        
        if(nb_save_carte>6)
        {
          insert_save$a <- insert_save$a - 1
          showModal(modalDialog(HTML("<font size=+1>Vous ne pouvez pas sauvegarger plus de 6 cartes. Veuillez en supprimer avant de continuer.</font> "), size="l", footer=NULL, easyClose = TRUE, style = "color: #fff; background-color: #DF691A; border-color: #2e6da4")) #337ab7
          return(NULL)
        }
        
        output[[paste0("mymap_save_",insert_save$a,"_rp_ac")]] <- renderLeaflet({
          
          if(!is.null(fondSuppl))
          {
            if(input$ajout_territoire_rp_ac_id)
            {
              m_save <- addPolygons(map = m_save, data = fond_territoire_rp_ac(),
                                    stroke = TRUE, color = "#BFBFBF", opacity = 1,
                                    weight = 0.5,
                                    options = pathOptions(pane = "fond_territoire", clickable = T),
                                    popup = paste0("<b> <font color=#2B3E50>",as.data.frame(fond_territoire_rp_ac())[,"LIBELLE"], "</font> </b>"),
                                    fill = T, fillColor = "white", fillOpacity = 0.001
              )
            }
          }
          
          if(input$ajout_reg_rp_ac_id)
          {
            m_save <- addPolygons(map = m_save, data = fond_region_rp_ac(),
                                  stroke = TRUE, color = "grey", opacity = 1,
                                  weight = 1.5,
                                  options = pathOptions(pane = "fond_reg", clickable = F),
                                  fill = F
            )
          }
          
          if(input$ajout_dep_rp_ac_id)
          {
            m_save <- addPolygons(map = m_save, data = fond_departement_rp_ac(),
                                  stroke = TRUE, color = "grey", opacity = 1,
                                  weight = 0.5,
                                  options = pathOptions(pane = "fond_dep", clickable = F),
                                  fill = F
            )
          }
          
          i <- 1 #pour gerer l'ordre des fonds dans le pane
          
          for(fond in liste_fonds$a)
          {
            if(fond=="analyse")
            {
              suppressWarnings(test_analyse_maille_classe <- try(analyse_rp_ac()[[1]]$donnees[rev(order(analyse_rp_ac()[[1]]$donnees[,varVolume])),varRatio],silent=T))
              if(class(test_analyse_maille_classe) %in% "try-error")
              {
                return(NULL)
              }else
              {
                analyse_maille_classe <- analyse_rp_ac()[[1]]$donnees[rev(order(analyse_rp_ac()[[1]]$donnees[,varVolume])),varRatio]
              }
              
              m_save <- addCircles(map = m_save,
                                   lng = st_coordinates(analyse_rp_ac()[[2]])[,1],
                                   lat = st_coordinates(analyse_rp_ac()[[2]])[,2],
                                   stroke = TRUE, color = "white",
                                   opacity = 1,
                                   weight = 1,
                                   radius = calcul_rond_rp_ac()*sqrt(analyse_rp_ac()[[1]]$donnees[,varVolume]/calcul_max_rayon_metres_rp_ac()[[2]]),
                                   options = pathOptions(pane = paste0("fond_trio",i), clickable = T),
                                   popup = paste0("<b> <font color=#2B3E50>",varVolume," : </font></b>",analyse_rp_ac()[[1]]$donnees$TXT1,"<br>",
                                                  "<b> <font color=#2B3E50>",varRatio," : </font></b>",analyse_rp_ac()[[1]]$donnees$TXT2),
                                   fill = T,
                                   fillColor = palette_rp_ac()[[1]](analyse_maille_classe),
                                   fillOpacity = 1
              )
            }
            
            if(fond=="maille")
            {
              m_save <- addPolygons(map = m_save, data = fond_contour_maille_rp_ac()[[2]], opacity = 1, #maille_WGS84
                                    stroke = TRUE, color = "grey", weight = 1,
                                    options = pathOptions(pane = paste0("fond_trio",i), clickable = T),
                                    popup = paste0("<b> <font color=#2B3E50>",as.data.frame(fond_contour_maille_rp_ac()[[2]])[,"LIBELLE"], "</font> </b>"),
                                    fill = T, fillColor = "white", fillOpacity = 0.001
              )
            }
            
            if(fond=="contour")
            {
              m_save <- addPolygons(map = m_save, data = fond_contour_maille_rp_ac()[[1]], opacity = 0.3, #contour_WGS84
                                    stroke = TRUE, color = "black", weight = 3,
                                    options = pathOptions(pane = paste0("fond_trio",i), clickable = T),
                                    popup = paste0("<b> <font color=#2B3E50>",as.data.frame(fond_contour_maille_rp_ac()[[1]])[,"LIBELLE"], "</font> </b>"),
                                    fill = T, fillColor = "white", fillOpacity = 0.3
              )
            }
            
            i <- i + 1
          }
          
          if(elargi_rp_ac())
          {
            analyse_maille_classe_elargi <- analyse_rp_ac()[[1]]$donnees_elargi[rev(order(analyse_rp_ac()[[1]]$donnees_elargi[,varVolume])),varRatio]
            
            m_save <- addCircles(map = m_save,
                                 lng = st_coordinates(fond_elargi_rp_ac()[[1]])[,1],
                                 lat = st_coordinates(fond_elargi_rp_ac()[[1]])[,2],
                                 stroke = TRUE, color = "white",
                                 opacity = input$opacite_elargi_rp_ac_id/100,
                                 weight = 1,
                                 radius = calcul_rond_rp_ac()*sqrt(analyse_rp_ac()[[1]]$donnees_elargi[,varVolume]/calcul_max_rayon_metres_rp_ac()[[2]]),
                                 options = pathOptions(pane = "fond_trio3", clickable = T),
                                 popup = paste0("<b> <font color=#2B3E50>",varVolume," : </font></b>",analyse_rp_ac()[[1]]$donnees_elargi$TXT1,"<br>",
                                                "<b> <font color=#2B3E50>",varRatio," : </font></b>",analyse_rp_ac()[[1]]$donnees_elargi$TXT2),
                                 fill = T,
                                 fillColor = palette_rp_ac()[[1]](analyse_maille_classe_elargi),
                                 fillOpacity = input$opacite_elargi_rp_ac_id/100
            )
            
            m_save <- addPolygons(map = m_save, data = fond_contour_maille_rp_ac()[[2]], opacity = 1, #maille_WGS84
                                  stroke = TRUE, color = "grey", weight = 1,
                                  options = pathOptions(pane = "fond_trio3", clickable = T),
                                  popup = paste0("<b> <font color=#2B3E50>",as.data.frame(fond_contour_maille_rp_ac()[[2]])[,"LIBELLE"], "</font> </b>"),
                                  fill = T, fillColor = "white", fillOpacity = 0.001
            )
          }
          
          zoom <- as.numeric(input$mymap_rp_ac_zoom)
          coeff <- ((360/(2^zoom))/7.2) # Permet de fixer une distance sur l'ecran. Il s'agit en gros d'une conversion des degres en pixels. Reste constant a longitude egale mais varie un peu selon la latitude
          
          ronds_leg <- construction_ronds_legende(lon_lat_rp_ac()[[1]],lon_lat_rp_ac()[[2]],code_epsg_rp_ac(),input$taille_rond_rp_ac_id)
          lignes <- construction_lignes_legende(ronds_leg,coeff,code_epsg_rp_ac())
          position_leg_classes <- t(data_frame(c(min(st_coordinates(ronds_leg[[1]])[which(st_coordinates(ronds_leg[[1]])[,4]==1),"X"]),min(st_coordinates(ronds_leg[[1]])[which(st_coordinates(ronds_leg[[1]])[,4]==1),"Y"]))))
          
          if(input$type_legende_rp_ac_id==1) # Litterale
          {
            # On cree les rectangles
            max_classes <- input$nb_classes_rp_ac_id
            
            for(i in 1:max_classes)
            {
              # Coordonnees du point haut/gauche des rectangles de la legende
              x_coord_rectangle <- position_leg_classes[1]
              if(i==1) #1er rectangle
              {
                y_coord_rectangle <- position_leg_classes[2]-coeff*1.5 #1,5cm sous les ronds
              }else
              {
                y_coord_rectangle <- y_coord_rectangle-coeff*0.7
              }
              assign(paste0("rectangle_",i),list(matrix(c(x_coord_rectangle,y_coord_rectangle,x_coord_rectangle+coeff*1,y_coord_rectangle,x_coord_rectangle+coeff*1,y_coord_rectangle+coeff*0.5,x_coord_rectangle,y_coord_rectangle+coeff*0.5,x_coord_rectangle,y_coord_rectangle),ncol=2, byrow=TRUE)))
            }
            
            # On ajoute un cadre blanc autour de la legende
            y_coord_rectangle <- min(get(paste0("rectangle_",max_classes))[[1]][,2])
            
            # leaflet du cadre blanc en 1er
            m_save <- addRectangles(map = m_save,
                                    lng1 = position_leg_classes[1]-coeff*0.5, lat1 = lignes[[5]]+coeff*0.5,
                                    lng2 = x_coord_rectangle+coeff*6, lat2 = y_coord_rectangle-coeff*0.8,
                                    stroke = TRUE,
                                    color = paste0("#2B3E50", ";background: #ffffff;
                                                   border-left:2px solid #2B3E50;
                                                   border-right:2px solid #2B3E50;
                                                   border-top:2px solid #2B3E50;
                                                   border-bottom:2px solid #2B3E50;
                                                   border-radius: 5%"),
                                    weight = 1,
                                    options = pathOptions(pane = "fond_legende", clickable = F),
                                    fill = T,
                                    fillColor = "white",
                                    fillOpacity = 0.8
                                    )
            
            # leaflet rectangles et valeurs classes
            classes_leg_texte <- analyse_leg_rp_ac()$rupture_classes
            
            for(i in 1: max_classes)
            {
              
              m_save <- addPolygons(map = m_save, data = st_polygon(get(paste0("rectangle_",i))),
                                    stroke = FALSE,
                                    options = pathOptions(pane = "fond_legende", clickable = F),
                                    fill = T,
                                    fillColor = analyse_leg_rp_ac()$pal_classes[i],
                                    fillOpacity = 1
              )
              
              if(i==1)
              {
                label_rectangle <- paste0(format(round(classes_leg_texte[i+1],3), big.mark=" ",decimal.mark=",",nsmall=0)," et plus")
                
                m_save <- addLabelOnlyMarkers(map = m_save,
                                              lng = (max(get(paste0("rectangle_",i))[[1]][,1])+coeff*0.1), lat = mean(get(paste0("rectangle_",i))[[1]][,2]),
                                              label = label_rectangle,
                                              labelOptions = labelOptions(noHide = T, textOnly = TRUE, direction = "right",
                                                                          style = list(
                                                                            "color" = "black",
                                                                            "font-size" = "12px"
                                                                          ))
                )
              }else if (i>1 && i<max_classes)
              {
                label_rectangle <- paste0("De ", format(round(classes_leg_texte[i+1],3), big.mark=" ",decimal.mark=",",nsmall=0)," \u00E0 moins de ", format(round(classes_leg_texte[i],3), big.mark=" ",decimal.mark=",",nsmall=0))
                
                m_save <- addLabelOnlyMarkers(map = m_save,
                                              lng = max(get(paste0("rectangle_",i))[[1]][,1])+coeff*0.1, lat = mean(get(paste0("rectangle_",i))[[1]][,2]),
                                              label = label_rectangle,
                                              labelOptions = labelOptions(noHide = T, textOnly = TRUE, direction = "right",
                                                                          style = list(
                                                                            "color" = "black",
                                                                            "font-size" = "12px"
                                                                          ))
                )
              }else #i==length(max_classes)
              {
                label_rectangle <- paste0("Moins de ", format(round(classes_leg_texte[i],3), big.mark=" ",decimal.mark=",",nsmall=0))
                
                m_save <- addLabelOnlyMarkers(map = m_save,
                                              lng = max(get(paste0("rectangle_",i))[[1]][,1])+coeff*0.1, lat = mean(get(paste0("rectangle_",i))[[1]][,2]),
                                              label = label_rectangle,
                                              labelOptions = labelOptions(noHide = T, textOnly = TRUE, direction = "right",
                                                                          style = list(
                                                                            "color" = "black",
                                                                            "font-size" = "12px"
                                                                          ))
                )
              }
            }
            
            # leaflet titre 2
            x_titre_2 <- min(st_coordinates(st_polygon(get("rectangle_1")))[,"X"])
            y_titre_2 <- max(st_coordinates(st_polygon(get("rectangle_1")))[,"Y"])+coeff*0.2
            
            m_save <- addLabelOnlyMarkers(map = m_save,
                                          lng = x_titre_2, lat = y_titre_2,
                                          label = input$titre_classes_legende_rp_ac_id,
                                          labelOptions = labelOptions(noHide = T, textOnly = TRUE, direction = "right",
                                                                      style = list(
                                                                        "color" = "black",
                                                                        "font-size" = "14px"
                                                                      ))
            )
          }
          
          if(input$type_legende_rp_ac_id==2) # Numerique
          {
            # On cree les rectangles
            max_classes <- input$nb_classes_rp_ac_id
            
            for(i in 1:max_classes)
            {
              # Coordonnees du point haut/gauche des rectangles de la legende
              x_coord_rectangle <- position_leg_classes[1]
              if(i==1) #1er rectangle
              {
                y_coord_rectangle <- position_leg_classes[2]-coeff
              }else
              {
                y_coord_rectangle <- y_coord_rectangle-coeff*0.5
              }
              assign(paste0("rectangle_",i),list(matrix(c(x_coord_rectangle,y_coord_rectangle,x_coord_rectangle+coeff*1,y_coord_rectangle,x_coord_rectangle+coeff*1,y_coord_rectangle+coeff*0.5,x_coord_rectangle,y_coord_rectangle+coeff*0.5,x_coord_rectangle,y_coord_rectangle),ncol=2, byrow=TRUE)))
            }
            
            # On ajoute un cadre blanc autour de la legende
            y_coord_rectangle <- min(get(paste0("rectangle_",max_classes))[[1]][,2])
            
            # leaflet du cadre blanc en 1er
            m_save <- addRectangles(map = m_save,
                                    lng1 = position_leg_classes[1]-coeff*0.5, lat1 = lignes[[5]]+coeff*0.5,
                                    lng2 = x_coord_rectangle+coeff*6, lat2 = y_coord_rectangle-coeff*0.8,
                                    stroke = TRUE,
                                    color = paste0("#2B3E50", ";background: #ffffff;
                                                   border-left:2px solid #2B3E50;
                                                   border-right:2px solid #2B3E50;
                                                   border-top:2px solid #2B3E50;
                                                   border-bottom:2px solid #2B3E50;
                                                   border-radius: 5%"),
                                    weight = 1,
                                    options = pathOptions(pane = "fond_legende", clickable = F),
                                    fill = T,
                                    fillColor = "white",
                                    fillOpacity = 0.8
                                    )
            
            # leaflet rectangles et valeurs classes
            classes_leg_num <- analyse_leg_rp_ac()$rupture_classes
            
            for(i in 1: max_classes)
            {
              m_save <- addPolygons(map = m_save, data = st_polygon(get(paste0("rectangle_",i))),
                                    stroke = FALSE,
                                    options = pathOptions(pane = "fond_legende", clickable = F),
                                    fill = T,
                                    fillColor = analyse_leg_rp_ac()$pal_classes[i],
                                    fillOpacity = 1
              )
              
              if(i<max_classes)
              {
                x1 <- max(get(paste0("rectangle_",i))[[1]][,1])
                y1 <- min(get(paste0("rectangle_",i))[[1]][,2])
                x2 <- max(get(paste0("rectangle_",i))[[1]][,1])+coeff*0.2
                y2 <- min(get(paste0("rectangle_",i))[[1]][,2])
                ligne <- st_linestring(rbind(c(x1,y1),c(x2,y2)))
                
                m_save <- addPolygons(map = m_save, data = ligne,
                                      color = "black",
                                      weight = 1,
                                      options = pathOptions(pane = "fond_legende", clickable = F),
                                      fill = F,
                                      fillOpacity = 1
                )
                
                m_save <- addLabelOnlyMarkers(map = m_save,
                                              lng = x2, lat = y2,
                                              label = as.character(format(round(classes_leg_num[i+1],3),big.mark=" ",decimal.mark=",",nsmall=0)),
                                              labelOptions = labelOptions(noHide = T, textOnly = TRUE, direction = "right",
                                                                          style = list(
                                                                            "color" = "black",
                                                                            "font-size" = "12px"
                                                                          ))
                )
                
              }
            }
            
            # On ajoute la legende de classes a l'analyse
            
            # leaflet titre 2
            x_titre_2 <- min(get("rectangle_1")[[1]][,1])
            y_titre_2 <- max(get("rectangle_1")[[1]][,2])+coeff*0.2
            
            m_save <- addLabelOnlyMarkers(map = m_save,
                                          lng = x_titre_2, lat = y_titre_2,
                                          label = input$titre_classes_legende_rp_ac_id,
                                          labelOptions = labelOptions(noHide = T, textOnly = TRUE, direction = "right",
                                                                      style = list(
                                                                        "color" = "black",
                                                                        "font-size" = "14px"
                                                                      ))
            )
          }
          
          suppressWarnings(m_save <- addCircles(map = m_save,
                                                lng = st_coordinates(st_centroid(ronds_leg[[1]]))[,1],
                                                lat = st_coordinates(st_centroid(ronds_leg[[1]]))[,2],
                                                stroke = TRUE,
                                                opacity = 1,
                                                color = "#2B3E50",
                                                weight = 2,
                                                radius = c(calcul_rond_rp_ac(),calcul_rond_rp_ac()/sqrt(3)),
                                                options = pathOptions(pane = "fond_legende", clickable = F),
                                                fill = T,
                                                fillColor = "white",
                                                fillOpacity = 1
          ))
          
          # leaflet lignes
          m_save <- addPolygons(map = m_save, data = lignes[[1]],
                                stroke = TRUE,
                                opacity = 1,
                                color = "#2B3E50",
                                weight = 2,
                                options = pathOptions(pane = "fond_legende", clickable = F),
                                fill = F,
                                fillOpacity = 1
          )
          
          # leaflet valeur ronds
          m_save <- addLabelOnlyMarkers(map = m_save,
                                        lng = lignes[[2]][2,1], lat = lignes[[2]][2,2], #ligne_grand
                                        label = as.character(format(calcul_max_rayon_metres_rp_ac()[[2]],big.mark=" ",decimal.mark=",",nsmall=0)),
                                        labelOptions = labelOptions(noHide = T, textOnly = TRUE, direction = "right",
                                                                    style = list(
                                                                      "color" = "black",
                                                                      "font-size" = "12px"
                                                                    ))
          )
          
          m_save <- addLabelOnlyMarkers(map = m_save,
                                        lng = lignes[[3]][2,1], lat = lignes[[3]][2,2], #ligne_petit
                                        label = as.character(format(round(calcul_max_rayon_metres_rp_ac()[[2]]/3,0),big.mark=" ",decimal.mark=",",nsmall=0)),
                                        labelOptions = labelOptions(noHide = T, textOnly = TRUE, direction = "right",
                                                                    style = list(
                                                                      "color" = "black",
                                                                      "font-size" = "12px"
                                                                    ))
          )
          
          #leaflet titre 1
          m_save <- addLabelOnlyMarkers(map = m_save,
                                        lng = lignes[[4]], lat = lignes[[5]], #x_titre_1 et y_titre_1
                                        label = input$titre_ronds_legende_rp_ac_id,
                                        labelOptions = labelOptions(noHide = T, textOnly = TRUE, direction = "right",
                                                                    style = list(
                                                                      "color" = "black",
                                                                      "font-size" = "14px"
                                                                    ))
          )
          
          removeModal()
          
          m_save
        })
        
        output[[paste0("remove_carte_",nb_save_carte,"_rp_ac")]] <- renderUI({
          actionButton(paste0("remove_carte_",nb_save_carte,"_rp_ac_id"),label="X Supprimer la carte", style="color:#FFFFFF; border-color:#FFFFFF; background-color:#2B3E50")
        })
        
        appendTab(inputId = "onglets_rp_ac",
                  tabPanel(title=HTML(paste0("<font color=#AFC9E0> Carte ",insert_save$a,"</font>")),value=paste0("carte",nb_save_carte),uiOutput(paste0("remove_carte_",nb_save_carte,"_rp_ac")),leafletOutput(paste0("mymap_save_",insert_save$a,"_rp_ac"),width="100%",height = 800)),
                  select = TRUE,
                  session = session
        )
        
      }, ignoreInit = TRUE)
      
      observeEvent(input$remove_carte_1_rp_ac_id,{
        remove_carte$a <- remove_carte$a + 1
        removeTab(inputId = "onglets_rp_ac",
                  target = "carte1",
                  session = session
        )
      }, ignoreInit = TRUE)
      
      observeEvent(input$remove_carte_2_rp_ac_id,{
        remove_carte$a <- remove_carte$a + 1
        removeTab(inputId = "onglets_rp_ac",
                  target = "carte2",
                  session = session
        )
      }, ignoreInit = TRUE)
      
      observeEvent(input$remove_carte_3_rp_ac_id,{
        remove_carte$a <- remove_carte$a + 1
        removeTab(inputId = "onglets_rp_ac",
                  target = "carte3",
                  session = session
        )
      }, ignoreInit = TRUE)
      
      observeEvent(input$remove_carte_4_rp_ac_id,{
        remove_carte$a <- remove_carte$a + 1
        removeTab(inputId = "onglets_rp_ac",
                  target = "carte4",
                  session = session
        )
      }, ignoreInit = TRUE)
      
      observeEvent(input$remove_carte_5_rp_ac_id,{
        remove_carte$a <- remove_carte$a + 1
        removeTab(inputId = "onglets_rp_ac",
                  target = "carte5",
                  session = session
        )
      }, ignoreInit = TRUE)
      
      observeEvent(input$remove_carte_6_rp_ac_id,{
        remove_carte$a <- remove_carte$a + 1
        removeTab(inputId = "onglets_rp_ac",
                  target = "carte6",
                  session = session
        )
      }, ignoreInit = TRUE)
      
      # TABLEAUX DE DONNEES, MAILLE ET CONTOUR
      
      output$mydonnees_rp_ac <- DT::renderDataTable(datatable({
        if(elargi_rp_ac())
          data <- analyse_rp_ac()[[1]]$donnees_elargi
        else
          data <- analyse_rp_ac()[[1]]$donnees
        tableau_donnees <- data[,c("CODE","LIBELLE",varVolume,varRatio)]
      },  style = 'bootstrap'
      ))
      
      output$mymaille_rp_ac <- DT::renderDataTable(datatable({
        if(elargi_rp_ac())
          data <- as.data.frame(fondMailleElargi)
        else
          data <- as.data.frame(fondMaille)
        tableau_maille <- data[,c(1:2)]
      },  style = 'bootstrap'
      ))
      
      output$mycontour_rp_ac <- DT::renderDataTable(datatable({
        data <- as.data.frame(fondContour)
        tableau_contour <- data[,c(1:2)]
      },  style = 'bootstrap'
      ))
      
      # ENVOI DU LEAFLET A L'UI
      
      output$mymap_rp_ac <- renderLeaflet({
        react_fond_rp_ac()
      })
      
    }
    
    runApp(shinyApp(ui = ui, server = server), launch.browser = TRUE)
    }


shiny_classes_ronds <-
  function(data,fondMaille,fondMailleElargi=NULL,fondContour,fondSuppl=NULL,idData,varVolume,varRatio,dom="0",fondChx=NULL)
  {
    options("stringsAsFactors"=FALSE)
    
    # Verification des parametres
    msg_error1<-msg_error2<-msg_error3<-msg_error4<-msg_error5<-msg_error6<-msg_error7<-msg_error8<-msg_error9<-msg_error10<-msg_error11<-msg_error12<-msg_error13<-msg_error14<-msg_error15<-msg_error16<-msg_error17<-msg_error18<-msg_error19 <- NULL
    
    if(any(class(data)!="data.frame")) msg_error1 <- "Les donnees doivent etre dans un data.frame / "
    if(any(!any(class(fondMaille) %in% "sf"),!any(class(fondMaille) %in% "data.frame"))) msg_error2 <- "Le fond de maille doit etre un objet sf / "
    if(!is.null(fondMailleElargi)) if(any(!any(class(fondMailleElargi) %in% "sf"),!any(class(fondMailleElargi) %in% "data.frame"))) msg_error3 <- "Le fond de maille elargie doit etre un objet sf / "
    if(any(!any(class(fondContour) %in% "sf"),!any(class(fondContour) %in% "data.frame"))) msg_error4 <- "Le fond de contour doit etre un objet sf / "
    if(!is.null(fondSuppl)) if(any(!any(class(fondSuppl) %in% "sf"),!any(class(fondSuppl) %in% "data.frame"))) msg_error5 <- "Le fond supplementaire doit etre un objet sf / "
    if(any(class(idData)!="character")) msg_error6 <- "Le nom de la variable doit etre de type caractere / "
    if(any(class(varVolume)!="character")) msg_error7 <- "Le nom de la variable doit etre de type caractere / "
    if(any(class(varRatio)!="character")) msg_error8 <- "Le nom de la variable doit etre de type caractere / "
    if(any(class(dom)!="character")) msg_error9 <- "La valeur doit etre de type caractere ('0', '971', '972', '973', '974' ou '976') / "
    if(!is.null(fondChx)) if(any(!any(class(fondChx) %in% "sf"),!any(class(fondChx) %in% "data.frame"))) msg_error10 <- "Le fond des chx doit etre un objet sf / "
    
    if(length(names(data))<3) msg_error11 <- "Le tableau des donnees n'est pas conforme. Il doit contenir au minimum une variable identifiant et les 2 variables a representer / "
    if(length(names(fondMaille))<3) msg_error12 <- "Le fond de maille n'est pas conforme. La table doit contenir au minimum une variable identifiant, une variable libelle et la geometry / "
    if(!is.null(fondMailleElargi)) if(length(names(fondMailleElargi))<3) msg_error13 <- "Le fond de maille elargie n'est pas conforme. La table doit contenir au minimum une variable identifiant, une variable libelle et la geometry / "
    if(length(names(fondContour))<3) msg_error14 <- "Le fond de contour n'est pas conforme. La table doit contenir au minimum une variable identifiant, une variable libelle et la geometry / "
    if(!is.null(fondSuppl)) if(length(names(fondSuppl))<3) msg_error15 <- "Le fond supplementaire n'est pas conforme. La table doit contenir au minimum une variable identifiant, une variable libelle et la geometry / "
    
    if(!any(names(data) %in% idData))  msg_error16 <- "La variable identifiant les donnees n'existe pas dans la table des donnees / "
    if(!any(names(data) %in% varVolume))  msg_error17 <- "La variable a representer n'existe pas dans la table des donnees / "
    if(!any(names(data) %in% varRatio))  msg_error18 <- "La variable a representer n'existe pas dans la table des donnees / "
    if(!dom %in% c("0","971","972","973","974","976")) msg_error19 <- "La variable dom doit etre '0', '971', '972', '973', '974' ou '976' / "
    
    if(any(!is.null(msg_error1),!is.null(msg_error2),!is.null(msg_error3),!is.null(msg_error4),
           !is.null(msg_error5),!is.null(msg_error6),!is.null(msg_error7),!is.null(msg_error8),
           !is.null(msg_error9),!is.null(msg_error10),!is.null(msg_error11),!is.null(msg_error12),
           !is.null(msg_error13),!is.null(msg_error14),!is.null(msg_error15),!is.null(msg_error16),
           !is.null(msg_error17),!is.null(msg_error18),!is.null(msg_error19)))
    {
      stop(simpleError(paste0(msg_error1,msg_error2,msg_error3,msg_error4,msg_error5,msg_error6,msg_error7,msg_error8,
                              msg_error9,msg_error10,msg_error11,msg_error12,msg_error13,msg_error14,msg_error15,msg_error16,msg_error17,msg_error18,msg_error19)))
    }
    
    nb_up <- reactiveValues(a=0)
    nb_down <- reactiveValues(a=0)
    ordre_analyse <- reactiveValues(a=1,b=2)
    insert_save <- reactiveValues(a=0)
    remove_carte <- reactiveValues(a=0)
    liste_fonds <- reactiveValues(a=c("analyse","maille","contour"))
    m_save_ac_rp <- reactiveValues(a=0)
    
    erreur_maille <- reactiveValues(a=FALSE)
    
    max_classes <-  reactiveValues(a=4)
    methode_calcul <- c("fisher","jenks","kmeans","quantile","manuel")
    
    legende <- reactiveValues(a=NULL)
    
    sourc <- "Source : Insee"
    
    names(data)[names(data)==idData] <- "CODE"
    names(fondMaille)[1] <- "CODE"
    names(fondMaille)[2] <- "LIBELLE"
    names(fondContour)[1] <- "CODE"
    names(fondContour)[2] <- "LIBELLE"
    if(!is.null(fondMailleElargi)) 
    {
      names(fondMailleElargi)[1] <- "CODE"
      names(fondMailleElargi)[2] <- "LIBELLE"
      fondMailleElargi$LIBELLE<-iconv(fondMailleElargi$LIBELLE,"latin1","utf8")
    }
    if(!is.null(fondSuppl)) 
    {
      names(fondSuppl)[1] <- "CODE"
      names(fondSuppl)[2] <- "LIBELLE"
      fondSuppl$LIBELLE<-iconv(fondSuppl$LIBELLE,"latin1","utf8")
    }
    
    fondMaille$LIBELLE<-iconv(fondMaille$LIBELLE,"latin1","utf8")
    fondContour$LIBELLE<-iconv(fondContour$LIBELLE,"latin1","utf8")
    
    ui <- navbarPage("OCEANIS", id="menu",
                     
                     theme = shinytheme("superhero"),
                     
                     tabPanel("Carte",value="carte",
                              sidebarLayout(
                                
                                sidebarPanel(width = 3,
                                             style = "overflow-y:scroll; min-height: 840px; max-height: 840px",
                                             h4(HTML("<b><font color=#95BAE2>VARIABLES</font></b>")),
                                             uiOutput("variable_classe_ac_rp"),
                                             uiOutput("variable_rond_ac_rp"),
                                             tags$hr(style="border: 5px solid #5182B6"), #337ab7
                                             h4(HTML("<b><font color=#95BAE2>FONDS</font></b>")),
                                             fluidRow(
                                               column(width=9, offset=0.5,
                                                      uiOutput("ordre_fonds_ac_rp")
                                               ),
                                               column(width=1,
                                                      br(),
                                                      br(),
                                                      htmlOutput("monter_fond_ac_rp", inline=FALSE),
                                                      htmlOutput("descendre_fond_ac_rp", inline=FALSE)
                                               )
                                             ),
                                             uiOutput("elargi_ac_rp"),
                                             conditionalPanel(condition = 'input.elargi_ac_rp_id',
                                                              uiOutput("opacite_elargi_ac_rp")
                                             ),
                                             uiOutput("ajout_territoire_ac_rp"),
                                             uiOutput("ajout_reg_ac_rp"),
                                             uiOutput("ajout_dep_ac_rp"),
                                             tags$hr(style="border: 5px solid #5182B6"),
                                             h4(HTML("<b><font color=#95BAE2>RONDS</font></b>")),
                                             uiOutput("taille_rond_ac_rp"),
                                             htmlOutput("info_taille_max_rond_ac_rp"),
                                             htmlOutput("info_rapport_rond_ac_rp"),
                                             uiOutput("rapport_rond_ac_rp"),
                                             conditionalPanel(condition = 'input.rapport_rond_ac_rp_id',
                                                              uiOutput("valeur_rapport_rond_ac_rp"),
                                                              htmlOutput("info_rapport_max_rond_ac_rp")
                                             ),
                                             uiOutput("choix_centroid_ac_rp"),
                                             tags$hr(style="border: 5px solid #5182B6"),
                                             h4(HTML("<b><font color=#95BAE2>CLASSES</font></b>")),
                                             uiOutput("liste_classes_ac_rp"),
                                             uiOutput("methode_ac_rp"),
                                             uiOutput("distribution_variable_ac_rp"),
                                             conditionalPanel(condition = 'input.distribution_variable_ac_rp_id',
                                                              verticalLayout(
                                                                wellPanel(
                                                                  style="background: #EDF2F9; width:340px",
                                                                  plotOutput("distribution_ac_rp"),
                                                                  br(),
                                                                  uiOutput("slider_bornes_ac_rp"),
                                                                  uiOutput("valid_slider_bornes_ac_rp")
                                                                )
                                                              )
                                             ),
                                             conditionalPanel(condition = 'input.methode_ac_rp_id=="manuel"',
                                                              uiOutput("zone_bornes_ac_rp"),
                                                              uiOutput("valid_bornes_ac_rp")
                                             ),
                                             tags$hr(style="border: 5px solid #5182B6"),
                                             h4(HTML("<b><font color=#95BAE2>L\u00c9GENDE</font></b>")),
                                             uiOutput("titre_ronds_legende_ac_rp"),
                                             uiOutput("titre_classes_legende_ac_rp"),
                                             br(),
                                             uiOutput("affiche_legende_ac_rp"),
                                             uiOutput("type_legende_ac_rp"),
                                             br(),
                                             tags$hr(style="border: 5px solid #5182B6"),
                                             h4(HTML("<b><font color=#95BAE2>SAUVEGARDE</font></b>")),
                                             uiOutput("save_carte_ac_rp"),
                                             br(),
                                             conditionalPanel(condition = 'input.mymap_ac_rp_click',
                                                              tags$div(class="dropup",
                                                                       HTML('<button class="btn btn-primary dropdown-toggle" type="button" data-toggle="dropdown" aria-haspopup="true" aria-expanded="false">
                                                                            Exporter en projet Qgis
                                                                            <span class="caret"></span>
                                                                            </button>'),
                                                                       tags$ul(class="dropdown-menu",
                                                                               wellPanel(
                                                                                 style="background: #2B3E50; width:340px",
                                                                                 h4("Export de la carte en projet Qgis"),
                                                                                 br(),
                                                                                 uiOutput("sortie_qgis_ac_rp"),
                                                                                 br(),
                                                                                 uiOutput("titre1_qgis_ac_rp"),
                                                                                 uiOutput("titre2_qgis_ac_rp"),
                                                                                 uiOutput("source_qgis_ac_rp"),
                                                                                 tags$head(tags$style(HTML('#export_qgis_ac_rp_id{background-color:#337ab7}'))),
                                                                                 uiOutput("export_qgis_ac_rp")
                                                                               )
                                                                       )
                                                                       )
                                                              
                                             ),
                                             br(),
                                             uiOutput("aide_image_ac_rp"),
                                             br()
                              ),
                              mainPanel(
                                tags$head(
                                  tags$style(HTML(".leaflet-container { background: #AFC9E0; }"))
                                ),
                                tabsetPanel(id="onglets_ac_rp",
                                            tabPanel(title=HTML("<b>Carte</b>"),value="carte",
                                                     leafletOutput("mymap_ac_rp",width="100%",height = 800)
                                            ),
                                            tabPanel(title=HTML(paste0("<b>Donn","\u00e9","es</b>")),value="donnees",
                                                     h5("S\u00e9lectionnez une ou plusieurs lignes pour ensuite les visualiser sur la carte."),
                                                     DT::dataTableOutput("mydonnees_ac_rp",width="100%",height = 800)),
                                            tabPanel(title=HTML("<b>Maille</b>"),value="maille",
                                                     h5("S\u00e9lectionnez une ou plusieurs lignes pour ensuite les visualiser sur la carte."),
                                                     DT::dataTableOutput("mymaille_ac_rp",width="100%",height = 800)),
                                            tabPanel(title=HTML("<b>Contour</b>"),value="contour",
                                                     h5("S\u00e9lectionnez une ou plusieurs lignes pour ensuite les visualiser sur la carte."),
                                                     DT::dataTableOutput("mycontour_ac_rp",width="100%",height = 800))
                                )
                              )
    )
    )
    )
    
    server <- function(input, output, session) {
      
      #################
      #Onglet Carte
      #################
      
      #Charge les donnees et fonds en memoire et affiche les widgets adequats dans le sidePanel de l'onglet "Carte"
      observe({
        
        # VARIABLES
        
        output$variable_classe_ac_rp <- renderUI({
          selectInput("variable_classe_ac_rp_id", label=h5("Variable des classes (en ratio)"), choices = varRatio, selected = varRatio)
        })
        output$variable_rond_ac_rp <- renderUI({
          selectInput("variable_rond_ac_rp_id", label=h5("Variable des ronds (en volume)"), choices = varVolume, selected = varVolume)
        })
        
        # FONDS
        
        output$ordre_fonds_ac_rp <- renderUI({
          selectInput("ordre_fonds_ac_rp_id", label=h5("Modifier l'ordre des fonds"), choices = liste_fonds$a, multiple=TRUE, selectize=FALSE, selected = NULL)
        })
        output$monter_fond_ac_rp <- renderUI({
          actionButton("monter_fond_ac_rp_id", label="", icon=icon("arrow-up"))
        })
        output$descendre_fond_ac_rp <- renderUI({
          actionButton("descendre_fond_ac_rp_id", label="", icon=icon("arrow-down"))
        })
        
        if(!is.null(fondMailleElargi))
        {
          output$elargi_ac_rp <- renderUI({
            checkboxInput("elargi_ac_rp_id", label = HTML("Afficher une repr\u00e9sentation \u00e9largie de l'analyse<br>(parfois long)"),
                          value = if(is.null(fondMailleElargi)) FALSE else TRUE)
          })
          output$opacite_elargi_ac_rp <- renderUI({
            sliderInput("opacite_elargi_ac_rp_id", label = h5("Opacit\u00e9 de l'analyse \u00e9largie"), value=60, min=0, max=100, step=5, ticks=FALSE)
          })
        }
        
        output$ajout_territoire_ac_rp <- renderUI({
          checkboxInput("ajout_territoire_ac_rp_id", label = "Afficher le fond des territoires",
                        value = if(is.null(fondSuppl)) FALSE else TRUE)
        })
        output$ajout_reg_ac_rp <- renderUI({
          checkboxInput("ajout_reg_ac_rp_id", label = "Afficher le fond des r\u00e9gions",
                        value = FALSE)
        })
        output$ajout_dep_ac_rp <- renderUI({
          checkboxInput("ajout_dep_ac_rp_id", label = "Afficher le fond des d\u00e9partements",
                        value = FALSE)
        })
        
        # RONDS
        
        output$taille_rond_ac_rp <- renderUI({
          numericInput("taille_rond_ac_rp_id", label = h5("Rayon du rond le plus grand (en m\u00e8tres)"), value=round(as.numeric(calcul_max_rayon_metres_ac_rp()[[1]])/1.25,0), min=0, max=round(as.numeric(calcul_max_rayon_metres_ac_rp()[[1]]),0), step=1000)
        })
        
        output$info_taille_max_rond_ac_rp <- renderText({
          HTML(paste0("<font size=2 color=white>Valeur max du rayon le plus grand = ", round(as.numeric(calcul_max_rayon_metres_ac_rp()[[1]]),0)," m</font>"))
        })
        
        output$info_rapport_rond_ac_rp <- renderText({
          HTML(paste0("<font size=2 color=white>Rapport Surface rond / Volume = ", (pi*(as.numeric(calcul_max_rayon_metres_ac_rp()[[1]])/1.25)^2)/as.numeric(calcul_max_rayon_metres_ac_rp()[[2]]),"</font>"))
        })
        
        output$rapport_rond_ac_rp <- renderUI({
          checkboxInput("rapport_rond_ac_rp_id", label = "Modifier la valeur du rapport (permet la comparaison entre cartes)", value=FALSE)
        })
        
        output$valeur_rapport_rond_ac_rp <- renderUI({
          numericInput("valeur_rapport_rond_ac_rp_id", label = h5("Nouvelle valeur du rapport Surface rond / Volume"), value=(pi*(as.numeric(calcul_max_rayon_metres_ac_rp()[[1]])/1.25)^2)/as.numeric(calcul_max_rayon_metres_ac_rp()[[2]]), min=0.1, max=(pi*(as.numeric(calcul_max_rayon_metres_ac_rp()[[1]]))^2)/as.numeric(calcul_max_rayon_metres_ac_rp()[[2]]), step=0.1)
        })
        
        output$info_rapport_max_rond_ac_rp <- renderText({
          HTML(paste0("<font size=2 color=white>Valeur max du rapport = ", (pi*(as.numeric(calcul_max_rayon_metres_ac_rp()[[1]]))^2)/as.numeric(calcul_max_rayon_metres_ac_rp()[[2]]),"</font>"))
        })
        
        if(!is.null(fondChx))
        {
          output$choix_centroid_ac_rp <- renderUI({
            radioButtons("choix_centroid_ac_rp_id", label = h5("Les ronds sont centres sur"), choices=c("les centroides des communes"="centroid","les chx des communes"="chx"), selected = if(!is.null(fondChx)) "chx" else "centroid")
          })
        }else
        {
          output$choix_centroid_ac_rp <- renderUI({
          })
        }
        
        # CLASSES
        
        output$liste_classes_ac_rp <- renderUI({
          selectInput("nb_classes_ac_rp_id", label = h5("Nombre de classes"),
                      choices = nb_classes_ac_rp(), selected = 4)
        })
        output$methode_ac_rp <- renderUI({
          selectInput("methode_ac_rp_id", label = h5("M\u00e9thode de calcul des classes"), 
                      choices = methode_calcul, selected="kmeans")
        })
        
        output$distribution_variable_ac_rp <- renderUI({
          bsButton("distribution_variable_ac_rp_id",label="Distribution de la variable", style="btn btn-info", icon = icon("bar-chart-o"),
                   type = "toggle", block = FALSE, disabled = FALSE,
                   value = FALSE)
        })
        
        observeEvent(input$distribution_variable_ac_rp_id,{
          if(!input$distribution_variable_ac_rp_id) return()
          updateButton(session, "distribution_variable_ac_rp_id", value = TRUE)
        }, ignoreInit = TRUE)
        
        observeEvent(input$distribution_variable_ac_rp_id,{
          
          output$distribution_ac_rp <- renderPlot({
            dt_donnees <- data.frame(VAR=as.numeric(analyse_ac_rp()[[1]]$donnees[,varRatio]))
            ggplot(dt_donnees, aes(x=dt_donnees$VAR)) +
              stat_bin(breaks=unique(sort(c(min(dt_donnees$VAR),new_bornes_ac_rp(),max(dt_donnees$VAR)))), closed = "left", fill="#5182B6", col="white") +
              scale_x_continuous(breaks=unique(sort(c(min(dt_donnees$VAR),new_bornes_ac_rp(),max(dt_donnees$VAR)))), labels = round(unique(sort(c(min(dt_donnees$VAR),new_bornes_ac_rp(),max(dt_donnees$VAR)))),2)) +
              ggtitle(label=paste0("Distribution de la variable  : ",varRatio)) +
              xlab(label = varRatio)
          })
          
          output$slider_bornes_ac_rp <- renderUI({
            lapply(1:(as.numeric(input$nb_classes_ac_rp_id)-1)+1, function(i) {
              sliderInput(inputId = paste0("slider_bornes_", i,"_ac_rp_id"), label = NULL,
                          value = rev(react_bornes_ac_rp()[[1]])[i], min = min(react_bornes_ac_rp()[[1]]), max = max(react_bornes_ac_rp()[[1]]), step = 0.001) #min = rev(react_bornes_ac_rp()[[1]])[i-1], max = rev(react_bornes_ac_rp()[[1]])[i+1]
            })
          })
          
          output$valid_slider_bornes_ac_rp <- renderUI({
            actionButton("valid_slider_bornes_ac_rp_id",label=label_bouton_ac_rp(), icon=icon("refresh"), style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
          })
        },ignoreInit = TRUE)
        
        label_bouton_ac_rp <- eventReactive(input$methode_ac_rp_id,{
          if(input$methode_ac_rp_id=="manuel")
          {
            label_bouton <- "Valider les bornes manuelles"
          }else
          {
            label_bouton <- "Basculer en mode manuel"
          }
          return(label_bouton)
        })
        
        new_bornes_ac_rp <- reactive({
          bornes <- vector()
          for (i in 2:(as.numeric(input$nb_classes_ac_rp_id))) {
            bornes<-c(bornes,input[[paste0("slider_bornes_", i,"_ac_rp_id")]])
          }
          return(bornes)
        })
        
        output$zone_bornes_ac_rp <- renderUI({
          
          if(!is.null(input$methode_ac_rp_id))
          {
            if(input$methode_ac_rp_id=="manuel")
              suppressWarnings(bornes_analyse <- classIntervals(as.numeric(analyse_ac_rp()[[1]]$donnees[,varRatio]),as.numeric(input$nb_classes_ac_rp_id),style="kmeans",rtimes=10,intervalClosure="left"))
            else
              suppressWarnings(bornes_analyse <- classIntervals(as.numeric(analyse_ac_rp()[[1]]$donnees[,varRatio]),input$nb_classes_ac_rp_id,style=input$methode_ac_rp_id,rtimes=10,intervalClosure="left"))
            
            carac_bornes <- calcul_bornes(analyse_ac_rp()[[1]]$donnees,bornes_analyse,varRatio,input$nb_classes_ac_rp_id,input$methode_ac_rp_id)
            
            if(!is.null(input$nb_classes_ac_rp_id))
            {
              if(input$methode_ac_rp_id=="manuel")
              {
                lapply(1:(as.numeric(input$nb_classes_ac_rp_id)-1)+1, function(i) {
                  numericInput(inputId = paste0("bornes_", i,"_ac_rp_id"), label = paste("Choix de la borne ", i-1),
                               value = carac_bornes[[1]][i])
                })
              }
            }
          }
        })
        
        output$valid_bornes_ac_rp <- renderUI({
          actionButton("valid_bornes_ac_rp_id",label="Rafraichir la carte", icon=icon("refresh"), style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
        })
        
        # LEGENDE
        
        output$titre_ronds_legende_ac_rp <- renderUI({
          textInput("titre_ronds_legende_ac_rp_id", label = h5("Titre de la l\u00e9gende des ronds"), value = "")
        })
        
        output$titre_classes_legende_ac_rp <- renderUI({
          textInput("titre_classes_legende_ac_rp_id", label = h5("Titre de la l\u00e9gende des classes"), value = "")
        })
        
        output$affiche_legende_ac_rp <- renderUI({
          checkboxInput("affiche_legende_ac_rp_id", label = "Activer le d\u00e9placement de la l\u00e9gende au clic",
                        value = TRUE)
        })
        
        output$type_legende_ac_rp <- renderUI({
          radioButtons("type_legende_ac_rp_id", label = h5("Type de l\u00e9gende"),
                       choices = list("Litt\u00e9rale" = 1, "En echelle" = 2),
                       selected = 1, inline = TRUE)
        })
        
        # SAUVEGARDE
        
        output$save_carte_ac_rp <- renderUI({
          actionButton("save_carte_ac_rp_id", label=HTML("<font size=3>Sauvegarder la carte dans un onglet</font>"), style="color:#FFFFFF; background-color:#DF691A")
        })
        
        output$entrees_qgis_ac_rp <- renderUI({
          actionButton("entrees_qgis_ac_rp_id", label="Exporter en projet Qgis")
        })
        
        output$sortie_qgis_ac_rp <- renderUI({
          tags$div(class="input-group",
                   HTML('<input type="text" id="sortie_qgis_ac_rp_id" class="form-control" placeholder="Nom du projet" aria-describedby="sortie_qgis_ac_rp_id">
                        <span class="input-group-addon" id="sortie_qgis_ac_rp_id">.qgs</span>'))
        })
        
        output$titre1_qgis_ac_rp <- renderUI({
          textInput("titre1_qgis_ac_rp_id", label = h5("Titre informatif"), value = "", placeholder= "Facultatif")
        })
        
        output$titre2_qgis_ac_rp <- renderUI({
          textInput("titre2_qgis_ac_rp_id", label = h5("Titre descriptif"), value = "", placeholder= "Facultatif")
        })
        
        output$source_qgis_ac_rp <- renderUI({
          textInput("source_qgis_ac_rp_id", label = h5("Source de la carte"), value = sourc)
        })
        
        output$aide_image_ac_rp <- renderUI({
          tags$div(class="dropup",
                   HTML(paste0('<button class="btn btn-primary dropdown-toggle" type="button" data-toggle="dropdown">
                               <i class="fa fa-book fa-fw" aria-hidden="true"></i>
                               Proc','\u00e9','dure pour capture d\'','\u00e9','cran
                               <span class="caret"></span>
                               </button>')),
                   tags$ul(class="dropdown-menu",
                           wellPanel(
                             style="background: #2B3E50; width:340px",
                             div(
                               HTML("<font size=2>Deux possibilit\u00e9s :</font>"),
                               br(),
                               br(),
                               strong(HTML("<font size=3>Par l'Outil Capture</font>")),
                               br(),
                               HTML("<font size=2>1- Ouvrir le logiciel Outil Capture sur le Poste de travail (Menu D\u00e9marrer > Tous les programmes > Accessoires > Outil Capture).</font>"),
                               br(),
                               HTML(paste0("<font size=2>2- S\u00e9lectionner la zone \u00e0 capturer (vous aurez besoin d'adapter la fen\u00eatre AUS pour avoir \u00e0 la fois la carte et la barre des t","\u00e2","ches).</font>")),
                               br(),
                               HTML("<font size=2>3- Enregistrer l'image ou copier la dans le presse-papier.</font>"),
                               br(),
                               br(),
                               strong(HTML(paste0("<font size=3>Par impression d'","\u00e9","cran</font>"))),
                               br(),
                               HTML("<font size=2>1- Appuyer sur la touche clavier \"Impr ecran\".</font>"),
                               br(),
                               HTML("<font size=2>2- Ouvrir un logiciel de retouche image (Paint par exemple).</font>"),
                               br(),
                               HTML("<font size=2>3- Coller l'image et l'enregistrer au format voulu (.jpg, .png, .bmp).</font>")
                             )
                           )
                   )
                   )
        })
        })
      
      # Pour modifier l'ordre des fonds
      observeEvent(list(input$monter_fond_ac_rp_id,input$descendre_fond_ac_rp_id),{
        
        ordre <- c()
        if(as.numeric(input$monter_fond_ac_rp_id)>nb_up$a)
        {
          ordre <- c(2,3)
          nb_up$a <- nb_up$a+1
        }
        if(as.numeric(input$descendre_fond_ac_rp_id)>nb_down$a)
        {
          ordre <- c(1,2)
          nb_down$a <- nb_down$a+1
        }
        
        if(is.null(input$ordre_fonds_ac_rp_id)) pos_select <- 0 else pos_select <- which(liste_fonds$a==input$ordre_fonds_ac_rp_id)
        
        if(pos_select>0)
        {
          if(pos_select==ordre[1]) liste_fonds$a <- liste_fonds$a[c(2,1,3)]
          if(pos_select==ordre[2]) liste_fonds$a <- liste_fonds$a[c(1,3,2)]
          
          updateSelectInput(session, "ordre_fonds_ac_rp_id",
                            choices = liste_fonds$a,
                            selected = input$ordre_fonds_ac_rp_id
          )
        }
      },ignoreInit = TRUE)
      
      # Pour la semio, on calcul le rayon maximal d'un rond de facon a ce que la somme des superficies des ronds ne depasse pas 1/7eme de la superficie du territoire.
      calcul_max_rayon_metres_ac_rp <- reactive({
        #Aire totale du territoire d'etude
        aire_territoire <- as.numeric(sum(st_area(fondMaille[fondMaille$CODE %in% data[,"CODE"],]))) #Superficie du territoire
        #valeur max de la serie de donnees
        suppressWarnings(max_var <- max(data[data[,"CODE"] %in% fondMaille$CODE,varVolume], na.rm = TRUE))
        
        serie <- data[data[,"CODE"] %in% fondMaille$CODE,varVolume]
        serie <- serie[!is.na(serie)]
        #on ramene la serie a un quotient fonction de la valeur max
        quotient <- serie/max_var
        #somme des carres
        somme_quotient <- sum(quotient^2)
        #calcul de la surface max du rond le plus grand
        max_surface_rond <- (aire_territoire/(7*somme_quotient))
        #calcul du rayon max du rond le plus grand
        max_rayon_metres <- sqrt(max_surface_rond/pi)
        
        return(list(max_rayon_metres,max_var))
      })
      
      rayon_ac_rp <- reactive({
        req(input$valeur_rapport_rond_ac_rp_id)
        Sys.sleep(3)
        val <- round(sqrt((input$valeur_rapport_rond_ac_rp_id*isolate(calcul_max_rayon_metres_ac_rp())[[2]])/pi),0)
        return(val)
      })
      
      rayon_react_ac_rp <- rayon_ac_rp %>% debounce(1000)
      
      observeEvent(rayon_react_ac_rp(),{
        req(rayon_react_ac_rp())
        
        if(length(rayon_react_ac_rp())==0) return(NULL)
        if(rayon_react_ac_rp()==0 | is.na(rayon_react_ac_rp())) return(NULL)
        
        isolate(updateNumericInput(session,"taille_rond_ac_rp_id", value=rayon_react_ac_rp()))
        
        isolate(output$info_rapport_rond_ac_rp <- renderText({
          HTML(paste0("<font size=2 color=white>Rapport Surface rond / Volume = ", (pi*(rayon_react_ac_rp())^2)/isolate(calcul_max_rayon_metres_ac_rp())[[2]],"</font>"))
        }))
      })
      
      rapport_ac_rp <- reactive({
        req(input$taille_rond_ac_rp_id)
        
        val <- (pi*(input$taille_rond_ac_rp_id)^2)/isolate(calcul_max_rayon_metres_ac_rp())[[2]]
        max <- (pi*(isolate(calcul_max_rayon_metres_ac_rp())[[1]])^2)/isolate(calcul_max_rayon_metres_ac_rp())[[2]]
        
        return(list(val=val,max=max))
      })
      
      rapport_react_ac_rp <- rapport_ac_rp %>% debounce(1000)
      
      observeEvent(rapport_react_ac_rp(),{
        req(rapport_react_ac_rp())
        
        if(length(rapport_react_ac_rp()$val)==0) return(NULL)
        if(rapport_react_ac_rp()$val==0 | is.na(rapport_react_ac_rp()$val)) return(NULL)
        
        isolate(updateNumericInput(session,"valeur_rapport_rond_ac_rp_id", value=rapport_react_ac_rp()$val))
        
        isolate(output$info_rapport_rond_ac_rp <- renderText({
          HTML(paste0("<font size=2 color=white>Rapport Surface rond / Volume = ", rapport_react_ac_rp()$val,"</font>"))
        }))
      })
      
      choix_centroid_ac_rp <- reactive({
        if(is.null(input$choix_centroid_ac_rp_id))
        {
          centroid <- "centroid"
        }else
        {
          centroid <- input$choix_centroid_ac_rp_id
        }
        return(centroid)
      })
      
      # Pour calculer les bornes des classes
      react_bornes_ac_rp <- reactive({
        
        if(is.null(input$nb_classes_ac_rp_id) | is.null(input$methode_ac_rp_id))
        {
          max_classes$a <- 4
          methode <- "kmeans"
          suppressWarnings(bornes_analyse <- classIntervals(as.numeric(analyse_ac_rp()[[1]]$donnees[,varRatio]),max_classes$a,style=methode,rtimes=10,intervalClosure="left"))
        }else if(input$nb_classes_ac_rp_id=="" | input$methode_ac_rp_id=="")
        {
          max_classes$a <- 4
          methode <- "kmeans"
          suppressWarnings(bornes_analyse <- classIntervals(as.numeric(analyse_ac_rp()[[1]]$donnees[,varRatio]),max_classes$a,style=methode,rtimes=10,intervalClosure="left"))
        }else
        {
          max_classes$a <- as.numeric(input$nb_classes_ac_rp_id)
          if(is.na(max_classes$a)) return(NULL)
          methode <- as.character(input$methode_ac_rp_id)
          if(!methode %in% c("manuel"))
          {
            suppressWarnings(bornes_analyse <- classIntervals(as.numeric(analyse_ac_rp()[[1]]$donnees[,varRatio]),max_classes$a,style=methode,rtimes=10,intervalClosure="left"))
          }
        }
        if(methode!="manuel")
        {
          carac_bornes <- calcul_bornes(analyse_ac_rp()[[1]]$donnees,bornes_analyse,varRatio,max_classes$a,methode)
        }else if(methode=="manuel")
        {
          carac_bornes <- react_bornes_manuel_1_ac_rp()
        }
        
        return(carac_bornes) #list(bornes=carac_bornes[[1]],pal_classes=carac_bornes[[2]])
      })
      
      # Pour calculer les bornes des classes
      react_bornes_init_ac_rp <- reactive({
        
        suppressWarnings(bornes_analyse <- classIntervals(as.numeric(data[,varRatio]),4,style="kmeans",rtimes=10,intervalClosure="left"))
        carac_bornes <- calcul_bornes(data,bornes_analyse,varRatio,4,"kmeans")
        return(carac_bornes) #list(bornes=carac_bornes[[1]],pal_classes=carac_bornes[[2]])
      })
      
      # Pour mettre a jour les bornes en mode manuel
      react_bornes_manuel_1_ac_rp <- eventReactive(input$valid_bornes_ac_rp_id,{
        
        suppressWarnings(bornes_analyse <- classIntervals(as.numeric(analyse_ac_rp()[[1]]$donnees[,varRatio]),max_classes$a,style="kmeans",rtimes=10,intervalClosure="left"))
        
        carac_bornes <- calcul_bornes(analyse_ac_rp()[[1]]$donnees,bornes_analyse,varRatio,input$nb_classes_ac_rp_id,input$methode_ac_rp_id)
        
        bornes <- vector()
        for (i in 0:(as.numeric(input$nb_classes_ac_rp_id))+1) {
          bornes<-c(bornes,input[[paste0("bornes_", i,"_ac_rp_id")]])
        }
        bornes <- c(carac_bornes[[1]][1],bornes,carac_bornes[[1]][length(carac_bornes[[1]])])
        bornes <- sort(unique(bornes),decreasing = T)
        
        carac_bornes[[1]] <- bornes
        
        return(carac_bornes)
      },ignoreNULL = FALSE)
      
      # Pour mettre a jour les bornes dans la distribution
      observeEvent(input$valid_slider_bornes_ac_rp_id,{
        updateSelectInput(session, inputId = "methode_ac_rp_id", selected = "manuel")
        for (i in 0:(as.numeric(input$nb_classes_ac_rp_id))+1) {
          updateNumericInput(session, inputId = paste0("bornes_", i,"_ac_rp_id"), value = input[[paste0("slider_bornes_", i,"_ac_rp_id")]])
        }
      },ignoreInit = TRUE)
      
      # Pour renvoyer la fourchette de classes possible
      nb_classes_ac_rp <- reactive({
        
        if(elargi_ac_rp())
        {
          donnees <- analyse_ac_rp()[[1]]$donnees_elargi[,varRatio]
        }else
        {
          donnees <- analyse_ac_rp()[[1]]$donnees[,varRatio]
        }
        
        suppressWarnings(
          if(min(donnees)<0 & max(donnees)>0) # Si + et -
          {
            if(length(donnees)>3 & length(donnees)<9)
            {
              return(c(3:(length(donnees)-1)))
            }else
            {
              return(c(3:9))
            }
          }else # Si tout + ou tout -
          {
            if(length(donnees)>3 & length(donnees)<5)
            {
              return(c(3:(length(donnees)-1)))
            }else
            {
              return(c(3:5))
            }
          }
        )
      })
      observe({nb_classes_ac_rp()})
      
      # Pour exporter la carte en projet Qgis
      
      output$export_qgis_ac_rp <- renderUI({
        downloadButton("downloadProjetQgis_ac_rp", label="Exporter")
      })
      
      output$downloadProjetQgis_ac_rp <- downloadHandler(contentType = "zip",
                                                         filename = function(){
                                                           paste0(input$sortie_qgis_ac_rp_id,".zip")
                                                         },
                                                         content = function(file){
                                                           files <- EXPORT_PROJET_QGIS_AC_RP(file)
                                                           
                                                           zip(file,files, flags = "-j9X")
                                                         }
      )
      
      EXPORT_PROJET_QGIS_AC_RP <- function(file)
      {
        showModal(modalDialog(HTML("<i class=\"fa fa-spinner fa-spin fa-2x fa-fw\"></i> <font size=+1>Export du projet Qgis en cours...</font> "), size="m", footer=NULL, style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"))
        
        sortie <- input$sortie_qgis_ac_rp_id
        rep_sortie <- dirname(file)
        files <- c(paste0(rep_sortie,"/",sortie,".qgs"))
        
        if(is.null(input$nb_classes_ac_rp_id))
        {
          max_classes <- 4
        }else
        {
          max_classes <- input$nb_classes_ac_rp_id
        }
        
        if(!is.null(lon_lat_ac_rp()[[1]]))
        {
          suppressWarnings(test_affiche_leg <- try(table_classe <- data.frame(classe=c(max_classes:1),label=legende$a,couleurs=analyse_leg_ac_rp()$pal_classes, stringsAsFactors = F),silent=TRUE))
          if(class(test_affiche_leg) %in% "try-error")
          {
            showModal(modalDialog(HTML("<font size=+1><i class=\"fa fa-hand-pointer-o fa-fw\"></i><b>Double-cliquez</b> d'abord sur la carte pour afficher la l\u00e9gende.</font> "), size="m", footer=NULL, easyClose = TRUE, style = "color: #fff; background-color: #DF691A; border-color: #2e6da4")) #337ab7
            return(NULL)
          }else
          {
            table_classe <- data.frame(classe=c(max_classes:1),label=legende$a,couleurs=analyse_leg_ac_rp()$pal_classes, stringsAsFactors = F)
          }
        }else
        {
          showModal(modalDialog(HTML("<font size=+1><i class=\"fa fa-hand-pointer-o fa-fw\"></i><b>Double-cliquez</b> d'abord sur la carte pour afficher la l\u00e9gende.</font> "), size="m", footer=NULL, easyClose = TRUE, style = "color: #fff; background-color: #DF691A; border-color: #2e6da4")) #337ab7
          return(NULL)
        }
        
        if(elargi_ac_rp())
        {
          analyse_donnees_elargi <- analyse_ac_rp()[[1]][[4]] #donnees elargi
          analyse_maille_elargi <- fondMailleElargi #fond maille elargi
          
          names_donnees_elargi <- names(analyse_donnees_elargi)
          analyse_donnees_elargi <- data.frame(analyse_donnees_elargi,val=analyse_donnees_elargi[,varRatio],classe=palette_ac_rp()[[1]](analyse_donnees_elargi[,varRatio]))
          names(analyse_donnees_elargi) <- c(names_donnees_elargi,"val","classe")
          
          analyse_classes_elargi <- merge(table_classe,analyse_donnees_elargi,by.x="couleurs",by.y="classe")
          
          analyse_classes_elargi <- analyse_classes_elargi[,c("CODE","LIBELLE",varVolume,varRatio,"val","classe")]
          
          analyse_classes_elargi <- analyse_classes_elargi[order(analyse_classes_elargi[,varVolume],decreasing = T),]
          
          analyse_ronds_elargi <- analyse_ronds_sf_ac_rp()[[2]]
          
          analyse_ronds_elargi$classe <- analyse_classes_elargi$classe
          analyse_ronds_elargi$COL_BOR <- "white"
          
          fond_elargi_ronds <- analyse_ronds_elargi
          
          analyse_maille_elargi <- merge(analyse_maille_elargi,analyse_classes_elargi[,c("CODE",varVolume,varRatio,"val","classe")],by="CODE")
          names(analyse_maille_elargi) <- c("CODE","LIBELLE",varVolume,varRatio,"val","classe","geometry")
          analyse_maille_elargi <- st_sf(analyse_maille_elargi,stringsAsFactors = FALSE)
          
          fond_elargi_classes <- analyse_maille_elargi
          
          fond_maille_elargi <- st_transform(fondMailleElargi,paste0("+init=epsg:",code_epsg_ac_rp()))
          
          st_write(fond_elargi_ronds, paste0(rep_sortie,"/fond_elargi_ronds_carte.shp"), delete_dsn = TRUE, quiet = TRUE)
          st_write(fond_elargi_classes, paste0(rep_sortie,"/fond_maille_elargi_carte.shp"), delete_dsn = TRUE, quiet = TRUE)
          st_write(fond_maille_elargi, paste0(rep_sortie,"/fond_maille_elargi.shp"), delete_dsn = TRUE, quiet = TRUE)
          files <- c(paste0(rep_sortie,"/fond_elargi_ronds_carte.shp"),paste0(rep_sortie,"/fond_elargi_ronds_carte.dbf"),paste0(rep_sortie,"/fond_elargi_ronds_carte.prj"),paste0(rep_sortie,"/fond_elargi_ronds_carte.shx"),files)
          files <- c(paste0(rep_sortie,"/fond_maille_elargi_carte.shp"),paste0(rep_sortie,"/fond_maille_elargi_carte.dbf"),paste0(rep_sortie,"/fond_maille_elargi_carte.prj"),paste0(rep_sortie,"/fond_maille_elargi_carte.shx"),files)
          files <- c(paste0(rep_sortie,"/fond_maille_elargi.shp"),paste0(rep_sortie,"/fond_maille_elargi.dbf"),paste0(rep_sortie,"/fond_maille_elargi.prj"),paste0(rep_sortie,"/fond_maille_elargi.shx"),files)
        }
        
        analyse_donnees <- analyse_ac_rp()[[1]][[2]] #donnees
        analyse_maille <- fondMaille #fond maille
        
        names_donnees <- names(analyse_donnees)
        analyse_donnees <- data.frame(analyse_donnees,val=analyse_donnees[,varRatio],classe=palette_ac_rp()[[1]](analyse_donnees[,varRatio]))
        names(analyse_donnees) <- c(names_donnees,"val","classe")
        analyse_classes <- merge(table_classe,analyse_donnees,by.x="couleurs",by.y="classe")
        analyse_classes <- analyse_classes[,c("CODE","LIBELLE",varVolume,varRatio,"val","classe")]
        
        analyse_classes <- analyse_classes[order(analyse_classes[,varVolume],decreasing = T),]
        
        analyse_ronds <- analyse_ronds_sf_ac_rp()[[1]]
        
        analyse_ronds$classe <- analyse_classes$classe
        analyse_ronds$COL_BOR <- "#303030"
        
        analyse_maille <- merge(analyse_maille,analyse_classes[,c("CODE",varVolume,varRatio,"val","classe")],by="CODE")
        names(analyse_maille) <- c("CODE","LIBELLE",varVolume,varRatio,"val","classe","geometry")
        analyse_maille <- st_sf(analyse_maille,stringsAsFactors = FALSE)
        
        fond_classes <- analyse_maille
        fond_ronds <- analyse_ronds
        
        fond_ronds_leg <- construction_legende_ac_rp()[[1]][[2]]
        fond_lignes_leg <- construction_legende_ac_rp()[[2]][[7]]
        
        fond_maille <- st_transform(fondMaille,paste0("+init=epsg:",code_epsg_ac_rp()))
        fond_contour <- st_transform(fondContour,paste0("+init=epsg:",code_epsg_ac_rp()))
        if(!is.null(fondSuppl) && input$ajout_territoire_ac_rp_id) fond_territoire <- st_transform(fond_territoire_ac_rp(),paste0("+init=epsg:",code_epsg_ac_rp()))
        if(input$ajout_dep_ac_rp_id) fond_departement <- st_transform(fond_departement_ac_rp(),paste0("+init=epsg:",code_epsg_ac_rp()))
        if(input$ajout_reg_ac_rp_id) fond_region <- st_transform(fond_region_ac_rp(),paste0("+init=epsg:",code_epsg_ac_rp()))
        fond_france <- st_transform(fond_habillage_ac_rp()[[1]],paste0("+init=epsg:",code_epsg_ac_rp()))
        fond_pays <- st_transform(fond_habillage_ac_rp()[[2]],paste0("+init=epsg:",code_epsg_ac_rp()))
        
        st_write(fond_ronds, paste0(rep_sortie,"/fond_ronds_carte.shp"), delete_dsn = TRUE, quiet = TRUE)
        st_write(fond_classes, paste0(rep_sortie,"/fond_maille_carte.shp"), delete_dsn = TRUE, quiet = TRUE)
        st_write(fond_ronds_leg, paste0(rep_sortie,"/fond_ronds_leg.shp"), delete_dsn = TRUE, quiet = TRUE)
        st_write(fond_lignes_leg, paste0(rep_sortie,"/fond_lignes_leg.shp"), delete_dsn = TRUE, quiet = TRUE)
        st_write(fond_maille, paste0(rep_sortie,"/fond_maille.shp"), delete_dsn = TRUE, quiet = TRUE)
        st_write(fond_contour,paste0(rep_sortie,"/fond_contour.shp"), delete_dsn = TRUE, quiet = TRUE)
        if(exists("fond_territoire")) if(!is.null(fond_territoire)) st_write(fond_territoire, paste0(rep_sortie,"/fond_territoire.shp"), delete_dsn = TRUE, quiet = TRUE)
        if(exists("fond_departement")) if(!is.null(fond_departement)) st_write(fond_departement, paste0(rep_sortie,"/fond_departement.shp"), delete_dsn = TRUE, quiet = TRUE)
        if(exists("fond_region")) if(!is.null(fond_region)) st_write(fond_region,paste0(rep_sortie,"/fond_region.shp"), delete_dsn = TRUE, quiet = TRUE)
        st_write(fond_france,paste0(rep_sortie,"/fond_france.shp"), delete_dsn = TRUE, quiet = TRUE)
        if(exists("fond_pays")) if(!is.null(fond_pays)) st_write(fond_pays,paste0(rep_sortie,"/fond_pays.shp"), delete_dsn = TRUE, quiet = TRUE)
        
        files <- c(paste0(rep_sortie,"/fond_ronds_carte.shp"),paste0(rep_sortie,"/fond_ronds_carte.dbf"),paste0(rep_sortie,"/fond_ronds_carte.prj"),paste0(rep_sortie,"/fond_ronds_carte.shx"),files)
        files <- c(paste0(rep_sortie,"/fond_maille_carte.shp"),paste0(rep_sortie,"/fond_maille_carte.dbf"),paste0(rep_sortie,"/fond_maille_carte.prj"),paste0(rep_sortie,"/fond_maille_carte.shx"),files)
        files <- c(paste0(rep_sortie,"/fond_ronds_leg.shp"),paste0(rep_sortie,"/fond_ronds_leg.dbf"),paste0(rep_sortie,"/fond_ronds_leg.prj"),paste0(rep_sortie,"/fond_ronds_leg.shx"),files)
        files <- c(paste0(rep_sortie,"/fond_lignes_leg.shp"),paste0(rep_sortie,"/fond_lignes_leg.dbf"),paste0(rep_sortie,"/fond_lignes_leg.prj"),paste0(rep_sortie,"/fond_lignes_leg.shx"),files)
        files <- c(paste0(rep_sortie,"/fond_maille.shp"),paste0(rep_sortie,"/fond_maille.dbf"),paste0(rep_sortie,"/fond_maille.prj"),paste0(rep_sortie,"/fond_maille.shx"),files)
        files <- c(paste0(rep_sortie,"/fond_contour.shp"),paste0(rep_sortie,"/fond_contour.dbf"),paste0(rep_sortie,"/fond_contour.prj"),paste0(rep_sortie,"/fond_contour.shx"),files)
        if(exists("fond_territoire")) if(!is.null(fond_territoire)) files <- c(paste0(rep_sortie,"/fond_territoire.shp"),paste0(rep_sortie,"/fond_territoire.dbf"),paste0(rep_sortie,"/fond_territoire.prj"),paste0(rep_sortie,"/fond_territoire.shx"),files)
        if(exists("fond_departement")) if(!is.null(fond_departement)) files <- c(paste0(rep_sortie,"/fond_departement.shp"),paste0(rep_sortie,"/fond_departement.dbf"),paste0(rep_sortie,"/fond_departement.prj"),paste0(rep_sortie,"/fond_departement.shx"),files)
        if(exists("fond_region")) if(!is.null(fond_region)) files <- c(paste0(rep_sortie,"/fond_region.shp"),paste0(rep_sortie,"/fond_region.dbf"),paste0(rep_sortie,"/fond_region.prj"),paste0(rep_sortie,"/fond_region.shx"),files)
        files <- c(paste0(rep_sortie,"/fond_france.shp"),paste0(rep_sortie,"/fond_france.dbf"),paste0(rep_sortie,"/fond_france.prj"),paste0(rep_sortie,"/fond_france.shx"),files)
        if(exists("fond_pays")) if(!is.null(fond_pays)) files <- c(paste0(rep_sortie,"/fond_pays.shp"),paste0(rep_sortie,"/fond_pays.dbf"),paste0(rep_sortie,"/fond_pays.prj"),paste0(rep_sortie,"/fond_pays.shx"),files)
        
        titre1 <- paste0(input$titre1_qgis_ac_rp_id,"\n")
        titre2 <- input$titre2_qgis_ac_rp_id
        source <- input$source_qgis_ac_rp_id
        annee <- format(Sys.time(), format = "%Y")
        variable_a_representer <- varRatio
        titre_leg_classes <- input$titre_classes_legende_ac_rp_id
        
        l <- c()
        if(exists("fond_territoire")) l <- "fond_territoire"
        if(exists("fond_departement")) l <- c(l,"fond_departement")
        if(exists("fond_region")) l <- c(l,"fond_region")
        
        if(elargi_ac_rp())
        {
          l=c("fond_france",
              "fond_contour",
              "fond_maille",
              l,
              "fond_maille_elargi",
              "fond_ronds_leg",
              "fond_lignes_leg",
              "fond_ronds_carte",
              "fond_elargi_ronds_carte",
              "fond_maille_carte",
              "fond_maille_elargi_carte"
          )
        }else
        {
          l=c("fond_france",
              "fond_contour",
              "fond_maille",
              l,
              "fond_ronds_leg",
              "fond_lignes_leg",
              "fond_ronds_carte",
              "fond_maille_carte"
          )
        }
        
        l <- c(l,"fond_pays")
        
        export_projet_qgis_classes_ronds(l,rep_sortie,sortie,titre1,titre2,source,titre_leg_classes,table_classe,variable_a_representer,annee)
        
        removeModal()
        
        showModal(modalDialog(HTML(paste0("<font size=+1>Le projet Qgis a \u00e9t\u00e9 cr","\u00e9","ee.</font>")), size="m", footer=NULL, easyClose = TRUE, style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"))
        
        return(files)
      }
      
      elargi_ac_rp <- reactive({
        if(is.null(input$elargi_ac_rp_id))
        {
          elargi <- FALSE
        }else
        {
          elargi <- input$elargi_ac_rp_id
        }
        return(elargi)
      })
      
      code_epsg_ac_rp <- reactive({
        code_epsg <- switch(dom, #DOM
                            "0"="2154",# Lambert 93
                            "971"="32620",# UTM 20 N
                            "972"="32620",# UTM 20 N
                            "973"="2972",# UTM 22 N
                            "974"="2975",# UTM 40 S
                            "976"="4471")# UTM 38 S
        return(code_epsg)
      })
      
      analyse_ac_rp <- reactive({
        req(choix_centroid_ac_rp())
        
        suppressWarnings(test_k_ronds <- try(k_ronds(fondMaille,fondMailleElargi,names(fondMaille)[1],data,"CODE",varVolume,elargi_ac_rp(),choix_centroid_ac_rp(),fondChx),silent=T))
        if(class(test_k_ronds) %in% "try-error")
        {
          return(NULL)
        }else
        {
          analyse <- k_ronds(fondMaille,fondMailleElargi,names(fondMaille)[1],data,"CODE",varVolume,elargi_ac_rp(),choix_centroid_ac_rp(),fondChx)
        }
        
        if(is.null(analyse))
        {
          showModal(modalDialog(HTML(paste0("<font size=+1>La maille ne correspond pas au niveau g\u00e9ographique du fichier de donn","\u00e9","es.<br><br>Veuillez svp choisir une maille adapt","\u00e9","e ou modifier le fichier de donn","\u00e9","es.</font>")), size="l", footer=NULL, easyClose = TRUE, style = "color: #fff; background-color: #DF691A; border-color: #2e6da4")) #337ab7
          erreur_maille$a <- TRUE
          return(NULL)
        }
        
        analyse$donnees[,"TXT1"] <- paste0("<b> <font color=#2B3E50>",format(analyse$donnees$save, big.mark=" ",decimal.mark=",",nsmall=0),"</font></b>")
        analyse$donnees[,"TXT2"] <- paste0("<b> <font color=#2B3E50>",format(analyse$donnees[,varRatio], big.mark=" ",decimal.mark=",",nsmall=0),"</font></b>")
        if(elargi_ac_rp())
        {
          analyse$donnees_elargi[,"TXT1"] <- paste0("<b> <font color=#2B3E50>",format(analyse$donnees_elargi$save, big.mark=" ",decimal.mark=",",nsmall=0),"</font></b>")
          analyse$donnees_elargi[,"TXT2"] <- paste0("<b> <font color=#2B3E50>",format(as.vector(analyse$donnees_elargi[,varRatio]), big.mark=" ",decimal.mark=",",nsmall=0),"</font></b>")
        }
        
        analyse_WGS84 <- st_transform(analyse$analyse_points,"+init=epsg:4326 +proj=longlat +ellps=WGS84")
        
        return(list(analyse,analyse_WGS84))
      })
      
      analyse_leg_ac_rp <- reactive({
        analyse <- analyse_ac_rp()[[1]]
        analyse$rupture_classes <- palette_ac_rp()[[2]] #bornes
        analyse$pal_classes <- palette_ac_rp()[[3]] # pal_classes
        return(analyse)
      })
      
      fond_habillage_ac_rp <- reactive({
        if(dom=="0")
        {
          pays <- st_transform(sf_paysm(),"+init=epsg:4326 +proj=longlat +ellps=WGS84")
          fra <- st_transform(sf_fram(),"+init=epsg:4326 +proj=longlat +ellps=WGS84")
        }else
        {
          if(dom=="971")
          {
            fra <- st_transform(sf_reg01(),"+init=epsg:4326 +proj=longlat +ellps=WGS84")
            pays <- fra
          }
          if(dom=="972")
          {
            fra <- st_transform(sf_reg02(),"+init=epsg:4326 +proj=longlat +ellps=WGS84")
            pays <- fra
          }
          if(dom=="973")
          {
            fra <- st_transform(sf_reg03(),"+init=epsg:4326 +proj=longlat +ellps=WGS84")
            pays <- st_transform(sf_pays973(),"+init=epsg:4326 +proj=longlat +ellps=WGS84")
          }
          if(dom=="974")
          {
            fra <- st_transform(sf_reg04(),"+init=epsg:4326 +proj=longlat +ellps=WGS84")
            pays <- fra
          }
          if(dom=="976")
          {
            fra <- st_transform(sf_reg06(),"+init=epsg:4326 +proj=longlat +ellps=WGS84")
            pays <- fra
          }
        }
        fond_france <- fra
        fond_pays <- pays
        
        return(list(fond_france,fond_pays))
      })
      
      fond_contour_maille_ac_rp <- reactive({
        
        test_contour <- try(st_transform(fondContour,"+init=epsg:4326 +proj=longlat +ellps=WGS84"), silent = TRUE)
        test_maille <- try(st_transform(fondMaille,"+init=epsg:4326 +proj=longlat +ellps=WGS84"), silent = TRUE)
        
        if(any(list(class(test_contour),class(test_maille)) %in% "try-error"))
        {
          showModal(modalDialog(HTML(paste0("<font size=+1>Une erreur est survenue dans la cr","\u00e9","ation du territoire.<br><br>Veuillez svp v\u00e9rifier vos donn","\u00e9","es et les variables choisies.</font>")), size="m", footer=NULL, easyClose = TRUE, style = "color: #fff; background-color: #DF691A; border-color: #2e6da4"))
          erreur_maille$a <- TRUE
          return(NULL)
        }else
        {
          contour_WGS84 <- st_transform(fondContour,"+init=epsg:4326 +proj=longlat +ellps=WGS84")
          maille_WGS84 <- st_transform(fondMaille,"+init=epsg:4326 +proj=longlat +ellps=WGS84")
        }
        
        return(list(contour_WGS84,maille_WGS84))
      })
      
      fond_elargi_ac_rp <- reactive({
        req(analyse_ac_rp())
        if(elargi_ac_rp())
        {
          analyse_WGS84_elargi <- st_transform(analyse_ac_rp()[[1]]$analyse_points_elargi,"+init=epsg:4326 +proj=longlat +ellps=WGS84")
          maille_WGS84_elargi <- st_transform(fondMailleElargi,"+init=epsg:4326 +proj=longlat +ellps=WGS84")
          return(list(analyse_WGS84_elargi,maille_WGS84_elargi))
        }else
        {
          return(NULL)
        }
      })
      
      list_bbox_ac_rp <- reactive({
        req(fond_contour_maille_ac_rp())
        
        list_bbox <- list(c(st_bbox(fond_contour_maille_ac_rp()[[1]])[1],st_bbox(fond_contour_maille_ac_rp()[[1]])[3]),c(st_bbox(fond_contour_maille_ac_rp()[[1]])[2],st_bbox(fond_contour_maille_ac_rp()[[1]])[4]))
        return(list_bbox)
      })
      
      calcul_rond_ac_rp <- reactive({
        req(calcul_max_rayon_metres_ac_rp(),input$taille_rond_ac_rp_id)
        if(is.null(input$taille_rond_ac_rp_id)) taille_rond <- 1000
        
        if(!is.null(input$taille_rond_ac_rp_id))
        {
          if(input$taille_rond_ac_rp_id>calcul_max_rayon_metres_ac_rp()[[1]])
          {
            taille_rond_m <- calcul_max_rayon_metres_ac_rp()[[1]]
          }else
          {
            taille_rond_m <- input$taille_rond_ac_rp_id
          }
        }else
        {
          taille_rond_m <- NULL
        }
        
        return(taille_rond_m)
      })
      
      analyse_ronds_sf_ac_rp <- reactive({
        req(analyse_ac_rp(),code_epsg_ac_rp(),calcul_rond_ac_rp())
        
        # On cree les ronds en projection locale (pl) pour l'export Qgis
        if(elargi_ac_rp())
        {
          req(fond_elargi_ac_rp())
          centres <- rbind(st_coordinates(fond_elargi_ac_rp()[[1]]))
          row.names(centres) <- c(1:(nrow(analyse_ac_rp()[[1]]$donnees_elargi)))
          ronds <- st_sf(geometry=st_sfc(lapply(c(1:nrow(centres)),function(x) st_point(centres[x,])),crs="+init=epsg:4326 +proj=longlat +ellps=WGS84"))
          ronds_pl_elargi <- st_buffer(st_transform(ronds,paste0("+init=epsg:",code_epsg_ac_rp())), calcul_rond_ac_rp()*sqrt(analyse_ac_rp()[[1]]$donnees_elargi[,varVolume]/calcul_max_rayon_metres_ac_rp()[[2]]))
          
          # On cree les ronds 
          dt_ronds_sf <- data.frame(ronds_pl_elargi,stringsAsFactors = F)
          analyse_ronds_sf_elargi <- st_sf(cbind(analyse_ac_rp()[[1]]$donnees_elargi,dt_ronds_sf))
        }else
        {
          analyse_ronds_sf_elargi <- NULL
        }
        
        centres <- rbind(st_coordinates(analyse_ac_rp()[[2]]))
        row.names(centres) <- c(1:(nrow(analyse_ac_rp()[[1]]$donnees)))
        ronds <- st_sf(geometry=st_sfc(lapply(c(1:nrow(centres)),function(x) st_point(centres[x,])),crs="+init=epsg:4326 +proj=longlat +ellps=WGS84"))
        ronds_pl <- st_buffer(st_transform(ronds,paste0("+init=epsg:",code_epsg_ac_rp())), calcul_rond_ac_rp()*sqrt(analyse_ac_rp()[[1]]$donnees[,varVolume]/calcul_max_rayon_metres_ac_rp()[[2]]))
        # On cree les ronds 
        dt_ronds_sf <- data.frame(ronds_pl,stringsAsFactors = F)
        analyse_ronds_sf <- st_sf(cbind(analyse_ac_rp()[[1]]$donnees,dt_ronds_sf))
        
        return(list(analyse_ronds_sf,analyse_ronds_sf_elargi))
      })
      
      palette_ac_rp <- reactive({
        
        bornes <- react_bornes_ac_rp()[[1]]
        
        if(is.null(bornes)) return(NULL)
        
        if(elargi_ac_rp()) # On redefini le min et le max de la serie pour eviter les valeurs en NA
        {
          bornes[length(bornes)] <- min(as.numeric(analyse_ac_rp()[[1]]$donnees_elargi[,varRatio]))
          bornes[1] <- max(as.numeric(analyse_ac_rp()[[1]]$donnees_elargi[,varRatio]))
        }else
        {
          bornes[length(bornes)] <- min(as.numeric(analyse_ac_rp()[[1]]$donnees[,varRatio]))
          bornes[1] <- max(as.numeric(analyse_ac_rp()[[1]]$donnees[,varRatio]))
        }
        
        if(length(unique(bornes)) != length(bornes))
        {
          removeModal()
          showModal(modalDialog(HTML(paste0("<font size=+1>Les bornes calculees avec la methode '",input$methode_ac_rp_id,"' ne sont pas uniques. La methode kmeans a donc ete retenue.</font>")), size="l", footer=NULL, style = "color: #fff; background-color: #DF691A; border-color: #2e6da4")) #337ab7
          Sys.sleep(7)
          suppressWarnings(bornes_analyse <- classIntervals(as.numeric(analyse_ac_rp()[[1]]$donnees[,varRatio]),max_classes$a,style="kmeans",rtimes=10,intervalClosure="left"))
          carac_bornes <- calcul_bornes(analyse_ac_rp()[[1]]$donnees,bornes_analyse,varRatio,max_classes$a,"kmeans")
          updateSelectInput(session,"methode_ac_rp_id",choices = methode_calcul, selected="kmeans")
          bornes <- carac_bornes[[1]]
          pal_classes <- carac_bornes[[2]]
        }else
        {
          pal_classes <- react_bornes_ac_rp()[[2]]
        }
        pal_classes[is.na(pal_classes)] <- "grey"
        palette<-colorBin(palette=rev(pal_classes), domain=0:100, bins=bornes, na.color="grey")
        
        return(list(palette,bornes,pal_classes))
      })
      
      fond_territoire_ac_rp <- reactive({
        
        if(!is.null(fondSuppl))
        {
          fond_territoire <- st_transform(fondSuppl,"+init=epsg:4326 +proj=longlat +ellps=WGS84")
          return(fond_territoire)
        }else
        {
          return(NULL)
        }
      })
      
      fond_region_ac_rp <- reactive({
        fond_region <- st_transform(sf_regm(),"+init=epsg:4326 +proj=longlat +ellps=WGS84")
        return(fond_region)
      })
      
      fond_departement_ac_rp <- reactive({
        fond_departement <- st_transform(sf_depm(),"+init=epsg:4326 +proj=longlat +ellps=WGS84")
        return(fond_departement)
      })
      
      fond_select_donnees_elargi_ac_rp <- reactive({
        req(analyse_ronds_sf_ac_rp(),analyse_ac_rp())
        
        if(elargi_ac_rp())
        {
          fond_donnees_elargi <- analyse_ronds_sf_ac_rp()[[2]][as.data.frame(analyse_ronds_sf_ac_rp()[[2]])[,"CODE"] %in% analyse_ac_rp()[[1]]$donnees_elargi[input$mydonnees_ac_rp_rows_selected,"CODE"],]
          fond_donnees_elargi <- st_transform(fond_donnees_elargi,"+init=epsg:4326 +proj=longlat +ellps=WGS84")
          return(fond_donnees_elargi)
        }else
        {
          return(NULL)
        }
      })
      
      fond_select_donnees_ac_rp <- reactive({
        req(analyse_ronds_sf_ac_rp(),analyse_ac_rp())
        
        fond_donnees <- analyse_ronds_sf_ac_rp()[[1]][as.data.frame(analyse_ronds_sf_ac_rp()[[1]])[,"CODE"] %in% analyse_ac_rp()[[1]]$donnees[input$mydonnees_ac_rp_rows_selected,"CODE"],]
        if(nrow(fond_donnees)>0)
        {
          fond_donnees <- st_transform(fond_donnees,"+init=epsg:4326 +proj=longlat +ellps=WGS84")
          return(fond_donnees)
        }else
        {
          return(NULL)
        }
      })
      
      fond_select_maille_elargi_ac_rp <- reactive({
        req(fond_elargi_ac_rp())
        if(elargi_ac_rp())
        {
          fond_maille_elargi <- fond_elargi_ac_rp()[[2]][as.data.frame(fond_elargi_ac_rp()[[2]])[,"CODE"] %in% as.data.frame(fondMailleElargi)[input$mymaille_ac_rp_rows_selected,"CODE"],]
          return(fond_maille_elargi)
        }else
        {
          return(NULL)
        }
      })
      
      fond_select_maille_ac_rp <- reactive({
        req(fond_contour_maille_ac_rp())
        
        fond_maille <- fond_contour_maille_ac_rp()[[2]][as.data.frame(fond_contour_maille_ac_rp()[[2]])[,"CODE"] %in% as.data.frame(fondMaille)[input$mymaille_ac_rp_rows_selected,"CODE"],]
        return(fond_maille)
      })
      
      fond_select_contour_ac_rp <- reactive({
        req(fond_contour_maille_ac_rp())
        
        fond_contour <- fond_contour_maille_ac_rp()[[1]][as.data.frame(fond_contour_maille_ac_rp()[[1]])[,"CODE"] %in% as.data.frame(fondContour)[input$mycontour_ac_rp_rows_selected,"CODE"],]
        return(fond_contour)
      })
      
      # CONSTRUCTION DE LA MAP EN LEAFLET
      
      react_fond_ac_rp <- reactive({
        
        if(input$menu=="carte")
        {
          showModal(modalDialog(HTML("<i class=\"fa fa-spinner fa-spin fa-2x fa-fw\"></i><font size=+1>\u00c9laboration de la carte...</font> "), size="m", footer=NULL, style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"))
          
          # Construction de la map par defaut
          
          m <- leaflet(padding = 0,
                       options = leafletOptions(
                         preferCanvas = TRUE,
                         transition = 2,
                         minZoom = 6,
                         maxZoom = 10
                       )) %>%
            
            setMapWidgetStyle(list(background = "#AFC9E0")) %>%
            
            addTiles_insee(attribution = paste0("<a href=\"http://www.insee.fr\">\u00A9 IGN - INSEE ",format(Sys.time(), format = "%Y"),"</a>")) %>%
            
            fitBounds(lng1 = min(list_bbox_ac_rp()[[1]]),
                      lat1 = min(list_bbox_ac_rp()[[2]]),
                      lng2 = max(list_bbox_ac_rp()[[1]]),
                      lat2 = max(list_bbox_ac_rp()[[2]])
            ) %>%
            
            # On ajoute une barre d'echelle
            addScaleBar(position = 'bottomright',
                        options = scaleBarOptions(metric = TRUE, imperial = FALSE)
            ) %>%
            
            # Pour gerer l'ordre des calques
            addMapPane(name = "fond_pays", zIndex = 401) %>%
            addMapPane(name = "fond_france", zIndex = 402) %>%
            addMapPane(name = "fond_habillage", zIndex = 403) %>%
            addMapPane(name = "fond_dep", zIndex = 404) %>%
            addMapPane(name = "fond_reg", zIndex = 405) %>%
            addMapPane(name = "fond_territoire", zIndex = 406) %>%
            addMapPane(name = "fond_trio3", zIndex = 407) %>%
            addMapPane(name = "fond_trio2", zIndex = 408) %>%
            addMapPane(name = "fond_trio1", zIndex = 409) %>%
            addMapPane(name = "selection", zIndex = 410) %>%
            
            addMapPane(name = "fond_legende", zIndex = 411)
          
          # AFFICHAGE DES FONDS D'HABILLAGE
          if(dom %in% c("0","973"))
          {
            # fond des pays
            m <- addPolygons(map = m, data = fond_habillage_ac_rp()[[2]][,"LIBGEO"], opacity = 1, # fond_pays sauf la France
                             stroke = TRUE, color = "white",
                             weight = 1,
                             options = pathOptions(pane = "fond_pays", clickable = F),
                             fill = T, fillColor = "#CCCCCC", fillOpacity = 1
                             
            )
          }
          
          # fond de la France metro ou d'un DOM
          m <- addPolygons(map = m, data = fond_habillage_ac_rp()[[1]][,"LIBGEO"], opacity = 1, # fond_france
                           stroke = TRUE, color = "black",
                           weight = 1.5,
                           options = pathOptions(pane = "fond_france", clickable = F),
                           fill = T, fillColor = "white", fillOpacity = 1
          )
          
          m_save_ac_rp$a <- m
          
          # AFFICHAGE DU FOND TERRITOIRE
          
          if(!is.null(fondSuppl))
          {
            m <- addPolygons(map = m, data = fond_territoire_ac_rp(),
                             stroke = TRUE, color = "#BFBFBF", opacity = 1,
                             weight = 0.5,
                             options = pathOptions(pane = "fond_territoire", clickable = T),
                             popup = paste0("<b> <font color=#2B3E50>",as.data.frame(fond_territoire_ac_rp())[,"LIBELLE"], "</font> </b>"),
                             fill = T, fillColor = "white", fillOpacity = 0.001,
                             group = "territoire"
            )
          }
          
          # AFFICHAGE DES FONDS CONTOUR ET MAILLE
          
          # fond du contour
          m <- addPolygons(map = m, data = fond_contour_maille_ac_rp()[[1]], opacity = 0.3, #contour_WGS84
                           stroke = TRUE, color = "black", weight = 3,
                           options = pathOptions(pane = "fond_trio3", clickable = T),
                           popup = paste0("<b> <font color=#2B3E50>",as.data.frame(fond_contour_maille_ac_rp()[[1]])[,"LIBELLE"], "</font> </b>"),
                           fill = T, fillColor = "white", fillOpacity = 0.3,
                           group = "maille_contour"
          )
          
          # AFFICHAGE DE L'ANALYSE
          analyse <- k_ronds(fondMaille,fondMailleElargi,names(fondMaille)[1],data,"CODE",varVolume,FALSE,"centroid",fondChx)
          analyse$donnees[,"TXT1"] <- paste0("<b> <font color=#2B3E50>",format(analyse$donnees$save, big.mark=" ",decimal.mark=",",nsmall=0),"</font></b>")
          analyse$donnees[,"TXT2"] <- paste0("<b> <font color=#2B3E50>",format(analyse$donnees[,varRatio], big.mark=" ",decimal.mark=",",nsmall=0),"</font></b>")
          analyse_WGS84 <- st_transform(analyse$analyse_points,"+init=epsg:4326 +proj=longlat +ellps=WGS84")
          
          m <- addCircles(map = m,
                          lng = st_coordinates(analyse_WGS84)[,1],
                          lat = st_coordinates(analyse_WGS84)[,2],
                          stroke = TRUE, color = "#303030",
                          opacity = 1,
                          weight = 1.5,
                          radius = (calcul_max_rayon_metres_ac_rp()[[1]]/1.25)*sqrt(analyse$donnees[,varVolume]/calcul_max_rayon_metres_ac_rp()[[2]]),
                          options = pathOptions(pane = "fond_trio1", clickable = T),
                          popup = paste0("<b> <font color=#2B3E50>",varVolume," : </font></b>",analyse$donnees$TXT1),
                          fill = F,
                          group = "taille"
          )
          
          suppressWarnings(test_analyse_maille_classe <- try(analyse$donnees[rev(order(analyse$donnees[,varVolume])),varRatio],silent=T))
          if(class(test_analyse_maille_classe) %in% "try-error")
          {
            return(NULL)
          }else
          {
            analyse_maille_classe <- analyse$donnees[rev(order(analyse$donnees[,varVolume])),varRatio]
          }
          
          bornes <- react_bornes_init_ac_rp()[[1]]
          
          bornes[length(bornes)] <- min(as.numeric(analyse$donnees[,varRatio]))
          bornes[1] <- max(as.numeric(analyse$donnees[,varRatio]))
          
          pal_classes <- react_bornes_init_ac_rp()[[2]]
          
          pal_classes[is.na(pal_classes)] <- "grey"
          palette<-colorBin(palette=rev(pal_classes), domain=0:100, bins=bornes, na.color="grey")
          
          analyse_maille <- merge(fond_contour_maille_ac_rp()[[2]][,c("CODE","geometry")],analyse$donnees[,c("CODE","LIBELLE",varVolume,varRatio,"TXT1","TXT2")],by="CODE")
          names(analyse_maille)[3] <- varVolume
          names(analyse_maille)[4] <- varRatio
          analyse_maille <- analyse_maille[rev(order(as.data.frame(analyse_maille)[,varVolume])),]
          analyse_maille <- st_sf(analyse_maille,stringsAsFactors = FALSE)
          
          m <- addPolygons(map = m, data = analyse_maille, opacity = 1,
                           stroke = TRUE, color = "white", weight = 1,
                           options = pathOptions(pane = "fond_trio2", clickable = T),
                           popup = paste0("<b> <font color=#2B3E50>",as.data.frame(analyse_maille)$LIBELLE, "</font> </b><br>",
                                          "<b><font color=#2B3E50>",varRatio," : </font></b>",as.data.frame(analyse_maille)$TXT2),
                           fill = T,
                           fillColor = palette(analyse_maille_classe),
                           fillOpacity = 1,
                           group = "classe"
          )
          
          removeModal()
          
          showModal(modalDialog(HTML("<font size=+1>Veuillez patientez svp, la carte va s'afficher dans quelques secondes...<br><br><i class=\"fa fa-hand-pointer-o fa-fw\"></i><b>Double-cliquez</b> ensuite sur la carte pour afficher la l\u00e9gende.</font> "), size="m", footer=NULL, easyClose = TRUE, style = "color: #fff; background-color: #DF691A; border-color: #2e6da4")) #337ab7
          
          return(m)
        }
      })
      
      # MODIFICATION DES FONDS TERRITOIRE D'ETUDES, REGION ET DEPARTEMENT
      
      observeEvent(input$ajout_territoire_ac_rp_id,{
        
        proxy <- leafletProxy("mymap_ac_rp")
        
        clearGroup(map = proxy, group = "territoire")
        
        #fond du territoire d'etudes
        if(!is.null(fondSuppl))
        {
          if(input$ajout_territoire_ac_rp_id)
          {
            proxy <- addPolygons(map = proxy, data = fond_territoire_ac_rp(),
                                 stroke = TRUE, color = "#BFBFBF", opacity = 1,
                                 weight = 0.5,
                                 options = pathOptions(pane = "fond_territoire", clickable = T),
                                 popup = paste0("<b> <font color=#2B3E50>",as.data.frame(fond_territoire_ac_rp())[,"LIBELLE"], "</font> </b>"),
                                 fill = T, fillColor = "white", fillOpacity = 0.001,
                                 group = "territoire"
            )
          }
        }
      },ignoreInit = TRUE)
      
      observeEvent(input$ajout_reg_ac_rp_id,{
        
        proxy <- leafletProxy("mymap_ac_rp")
        
        clearGroup(map = proxy, group = "region")
        
        if(dom=="0")
        {
          if(input$ajout_reg_ac_rp_id)
          {
            #fond des regions
            proxy <- addPolygons(map = proxy, data = fond_region_ac_rp(),
                                 stroke = TRUE, color = "grey", opacity = 1,
                                 weight = 1.5,
                                 options = pathOptions(pane = "fond_reg", clickable = F),
                                 fill = F,
                                 group = "region"
            )
          }
        }
      },ignoreInit = TRUE)
      
      observeEvent(input$ajout_dep_ac_rp_id,{
        
        proxy <- leafletProxy("mymap_ac_rp")
        
        clearGroup(map = proxy, group = "departement")
        
        if(dom=="0")
        {
          if(input$ajout_dep_ac_rp_id)
          {
            #fond des departements
            proxy <- addPolygons(map = proxy, data = fond_departement_ac_rp(),
                                 stroke = TRUE, color = "grey", opacity = 1,
                                 weight = 0.5,
                                 options = pathOptions(pane = "fond_dep", clickable = F),
                                 fill = F,
                                 group = "departement"
            )
          }
        }
      },ignoreInit = TRUE)
      
      # MODIFICATION DE L'ORDRE DES CALQUES
      
      observeEvent(list(input$monter_fond_ac_rp_id,input$descendre_fond_ac_rp_id),{
        
        if(as.numeric(input$monter_fond_ac_rp_id)==0 & as.numeric(input$descendre_fond_ac_rp_id)==0) return(NULL)
        
        proxy <- leafletProxy("mymap_ac_rp")
        
        clearGroup(map = proxy, group = "maille_contour")
        clearGroup(map = proxy, group = "taille")
        
        i <- 1 #pour gerer l'ordre des fonds dans le pane
        for(fond in liste_fonds$a)
        {
          if(fond=="analyse")
          {
            proxy <- addCircles(map = proxy,
                                lng = st_coordinates(analyse_ac_rp()[[2]])[,1],
                                lat = st_coordinates(analyse_ac_rp()[[2]])[,2],
                                stroke = TRUE, color = "#303030",
                                opacity = 1,
                                weight = 1.5,
                                radius = calcul_rond_ac_rp()*sqrt(analyse_ac_rp()[[1]]$donnees[,varVolume]/calcul_max_rayon_metres_ac_rp()[[2]]),
                                options = pathOptions(pane = paste0("fond_trio",i), clickable = T),
                                popup = paste0("<b> <font color=#2B3E50>",varVolume," : </font></b>",analyse_ac_rp()[[1]]$donnees$TXT1),
                                fill = F,
                                group = "taille"
            )
            
            ordre_analyse$a <- i
          }
          
          if(fond=="maille")
          {
            suppressWarnings(test_analyse_maille_classe <- try(analyse_ac_rp()[[1]]$donnees[rev(order(analyse_ac_rp()[[1]]$donnees[,varVolume])),varRatio],silent=T))
            if(class(test_analyse_maille_classe) %in% "try-error")
            {
              return(NULL)
            }else
            {
              analyse_maille_classe <- analyse_ac_rp()[[1]]$donnees[rev(order(analyse_ac_rp()[[1]]$donnees[,varVolume])),varRatio]
            }
            
            analyse_maille <- merge(fond_contour_maille_ac_rp()[[2]][,c("CODE","geometry")],analyse_ac_rp()[[1]]$donnees[,c("CODE","LIBELLE",varVolume,varRatio,"TXT1","TXT2")],by="CODE")
            names(analyse_maille)[3] <- varVolume
            names(analyse_maille)[4] <- varRatio
            analyse_maille <- analyse_maille[rev(order(as.data.frame(analyse_maille)[,varVolume])),]
            analyse_maille <- st_sf(analyse_maille,stringsAsFactors = FALSE)
            
            proxy <- addPolygons(map = proxy, data = analyse_maille, opacity = 1,
                                 stroke = TRUE, color = "white", weight = 1,
                                 options = pathOptions(pane = paste0("fond_trio",i), clickable = T),
                                 popup = paste0("<b> <font color=#2B3E50>",as.data.frame(analyse_maille)$LIBELLE, "</font> </b><br>",
                                                "<b><font color=#2B3E50>",varRatio," : </font></b>",as.data.frame(analyse_maille)$TXT2),
                                 fill = T,
                                 fillColor = palette_ac_rp()[[1]](analyse_maille_classe),
                                 fillOpacity = 1,
                                 group = "classe"
            )
          }
          
          if(fond=="contour")
          {
            proxy <- addPolygons(map = proxy, data = fond_contour_maille_ac_rp()[[1]], opacity = 0.3, #contour_WGS84
                                 stroke = TRUE, color = "black", weight = 3,
                                 options = pathOptions(pane = paste0("fond_trio",i), clickable = T),
                                 popup = paste0("<b> <font color=#2B3E50>",as.data.frame(fond_contour_maille_ac_rp()[[1]])[,"LIBELLE"], "</font> </b>"),
                                 fill = T, fillColor = "white", fillOpacity = 0.3,
                                 group = "maille_contour"
            )
          }
          
          i <- i + 1
        }
        
      },ignoreInit = TRUE)
      
      # MODIFICATION DE LA TAILLE DES RONDS
      
      observeEvent(input$taille_rond_ac_rp_id,{
        req(input$taille_rond_ac_rp_id,calcul_rond_ac_rp())
        
        proxy <- leafletProxy("mymap_ac_rp")
        
        clearGroup(map = proxy, group = "taille")
        
        proxy <- addCircles(map = proxy,
                            lng = st_coordinates(analyse_ac_rp()[[2]])[,1],
                            lat = st_coordinates(analyse_ac_rp()[[2]])[,2],
                            stroke = TRUE, color = "#303030",
                            opacity = 1,
                            weight = 1.5,
                            radius = calcul_rond_ac_rp()*sqrt(analyse_ac_rp()[[1]]$donnees[,varVolume]/calcul_max_rayon_metres_ac_rp()[[2]]),
                            options = pathOptions(pane = paste0("fond_trio",ordre_analyse$a), clickable = T),
                            popup = paste0("<b> <font color=#2B3E50>",varVolume," : </font></b>",analyse_ac_rp()[[1]]$donnees$TXT1),
                            fill = F,
                            group = "taille"
        )
        
      },ignoreInit = TRUE)
      
      # MODIFICATION DE LA POSITION DES RONDS (CHX OU CENTROID)
      
      observeEvent(input$choix_centroid_ac_rp_id,{
        req(input$choix_centroid_ac_rp_id)
        
        proxy <- leafletProxy("mymap_ac_rp")
        
        clearGroup(map = proxy, group = "taille")
        
        proxy <- addCircles(map = proxy,
                            lng = st_coordinates(analyse_ac_rp()[[2]])[,1],
                            lat = st_coordinates(analyse_ac_rp()[[2]])[,2],
                            stroke = TRUE, color = "#303030",
                            opacity = 1,
                            weight = 1.5,
                            radius = calcul_rond_ac_rp()*sqrt(analyse_ac_rp()[[1]]$donnees[,varVolume]/calcul_max_rayon_metres_ac_rp()[[2]]),
                            options = pathOptions(pane = paste0("fond_trio",ordre_analyse$a), clickable = T),
                            popup = paste0("<b> <font color=#2B3E50>",varVolume," : </font></b>",analyse_ac_rp()[[1]]$donnees$TXT1),
                            fill = F,
                            group = "taille"
        )
        
      },ignoreInit = TRUE)
      
      # MODIFICATION DU NOMBRE DE CLASSES, DE LA METHODE OU DES BORNES
      
      observeEvent(list(input$nb_classes_ac_rp_id,input$methode_ac_rp_id,input$valid_bornes_ac_rp_id),{
        req(input$nb_classes_ac_rp_id,input$methode_ac_rp_id)
        
        proxy <- leafletProxy("mymap_ac_rp")
        
        clearGroup(map = proxy, group = "classe")
        
        suppressWarnings(test_analyse_maille_classe <- try(analyse_ac_rp()[[1]]$donnees[rev(order(analyse_ac_rp()[[1]]$donnees[,varVolume])),varRatio],silent=T))
        if(class(test_analyse_maille_classe) %in% "try-error")
        {
          return(NULL)
        }else
        {
          analyse_maille_classe <- analyse_ac_rp()[[1]]$donnees[rev(order(analyse_ac_rp()[[1]]$donnees[,varVolume])),varRatio]
        }
        
        analyse_maille <- merge(fond_contour_maille_ac_rp()[[2]][,c("CODE","geometry")],analyse_ac_rp()[[1]]$donnees[,c("CODE","LIBELLE",varVolume,varRatio,"TXT1","TXT2")],by="CODE")
        names(analyse_maille)[3] <- varVolume
        names(analyse_maille)[4] <- varRatio
        analyse_maille <- analyse_maille[rev(order(as.data.frame(analyse_maille)[,varVolume])),]
        analyse_maille <- st_sf(analyse_maille,stringsAsFactors = FALSE)
        
        proxy <- addPolygons(map = proxy, data = analyse_maille, opacity = 1,
                             stroke = TRUE, color = "white", weight = 1,
                             options = pathOptions(pane = paste0("fond_trio",ordre_analyse$b), clickable = T),
                             popup = paste0("<b> <font color=#2B3E50>",as.data.frame(analyse_maille)$LIBELLE, "</font> </b><br>",
                                            "<b><font color=#2B3E50>",varRatio," : </font></b>",as.data.frame(analyse_maille)$TXT2),
                             fill = T,
                             fillColor = palette_ac_rp()[[1]](analyse_maille_classe),
                             fillOpacity = 1,
                             group = "classe"
        )
        
      },ignoreInit = TRUE)
      
      # MODIFICATION DE LA REPRESENTATION ELARGIE
      
      observeEvent(list(input$elargi_ac_rp_id,input$opacite_elargi_ac_rp_id,input$taille_rond_ac_rp_id,input$nb_classes_ac_rp_id,input$methode_ac_rp_id,input$valid_bornes_ac_rp_id,input$choix_centroid_ac_rp_id),{
        req(input$elargi_ac_rp_id,input$opacite_elargi_ac_rp_id,input$taille_rond_ac_rp_id,input$nb_classes_ac_rp_id,input$methode_ac_rp_id)
        
        proxy <- leafletProxy("mymap_ac_rp")
        
        clearGroup(map = proxy, group = "elargi")
        
        if(elargi_ac_rp())
        {
          analyse_maille_classe_elargi <- analyse_ac_rp()[[1]]$donnees_elargi[rev(order(analyse_ac_rp()[[1]]$donnees_elargi[,varVolume])),varRatio]
          
          analyse_maille_elargi <- merge(fond_elargi_ac_rp()[[2]][,c("CODE","geometry")],analyse_ac_rp()[[1]]$donnees_elargi[,c("CODE","LIBELLE",varVolume,varRatio,"TXT1","TXT2")],by="CODE")
          names(analyse_maille_elargi)[3] <- varVolume
          names(analyse_maille_elargi)[4] <- varRatio
          analyse_maille_elargi <- analyse_maille_elargi[rev(order(as.data.frame(analyse_maille_elargi)[,varVolume])),]
          analyse_maille_elargi <- st_sf(analyse_maille_elargi,stringsAsFactors = FALSE)
          
          proxy <- addPolygons(map = proxy, data = analyse_maille_elargi, opacity = input$opacite_elargi_ac_rp_id/100,
                               stroke = TRUE, color = "white", weight = 1,
                               options = pathOptions(pane = "fond_trio3", clickable = T),
                               popup = paste0("<b> <font color=#2B3E50>",as.data.frame(analyse_maille_elargi)$LIBELLE, "</font> </b><br>",
                                              "<b><font color=#2B3E50>",varRatio," : </font></b>",as.data.frame(analyse_maille_elargi)$TXT2),
                               fill = T,
                               fillColor = palette_ac_rp()[[1]](analyse_maille_classe_elargi),
                               fillOpacity = input$opacite_elargi_ac_rp_id/100,
                               group = "elargi"
          )
          
          proxy <- addCircles(map = proxy,
                              lng = st_coordinates(fond_elargi_ac_rp()[[1]])[,1],
                              lat = st_coordinates(fond_elargi_ac_rp()[[1]])[,2],
                              stroke = TRUE, color = "#303030",
                              opacity = input$opacite_elargi_ac_rp_id/100,
                              weight = 1.5,
                              radius = calcul_rond_ac_rp()*sqrt(analyse_ac_rp()[[1]]$donnees_elargi[,varVolume]/calcul_max_rayon_metres_ac_rp()[[2]]),
                              options = pathOptions(pane = "fond_trio3", clickable = T),
                              popup = paste0("<b> <font color=#2B3E50>",varVolume," : </font></b>",analyse_ac_rp()[[1]]$donnees_elargi$TXT1),
                              fill = F,
                              group = "elargi"
          )
        }
      },ignoreInit = TRUE)
      
      # MODIFICATION DE LA SELECTION DES OBJETS VIA LES TABLEAUX
      
      observeEvent(list(input$onglets_ac_rp,input$choix_centroid_ac_rp_id),{
        req(input$onglets_ac_rp)
        
        if(input$onglets_ac_rp == "carte")
        {
          proxy <- leafletProxy("mymap_ac_rp")
          
          clearGroup(map = proxy, group = "select_donnees")
          
          if(!is.null(input$mydonnees_ac_rp_rows_selected))
          {
            if(elargi_ac_rp())
            {
              suppressWarnings(proxy <- addCircles(map = proxy,
                                                   lng = st_coordinates(st_centroid(fond_select_donnees_elargi_ac_rp()))[,1],
                                                   lat = st_coordinates(st_centroid(fond_select_donnees_elargi_ac_rp()))[,2],
                                                   stroke = TRUE, color = "#FFFF00",
                                                   opacity = 1,
                                                   weight = 3,
                                                   radius = calcul_rond_ac_rp()*sqrt(analyse_ac_rp()[[1]]$donnees_elargi[analyse_ac_rp()[[1]]$donnees_elargi[,"CODE"] %in% analyse_ac_rp()[[1]]$donnees_elargi[input$mydonnees_ac_rp_rows_selected,"CODE"],varVolume]/calcul_max_rayon_metres_ac_rp()[[2]]),
                                                   options = pathOptions(pane = "selection", clickable = F),
                                                   fill = F,
                                                   group = "select_donnees")
              )
            }else
            {
              suppressWarnings(proxy <- addCircles(map = proxy,
                                                   lng = st_coordinates(st_centroid(fond_select_donnees_ac_rp()))[,1],
                                                   lat = st_coordinates(st_centroid(fond_select_donnees_ac_rp()))[,2],
                                                   stroke = TRUE, color = "#FFFF00",
                                                   opacity = 1,
                                                   weight = 3,
                                                   radius = calcul_rond_ac_rp()*sqrt(analyse_ac_rp()[[1]]$donnees[analyse_ac_rp()[[1]]$donnees[,"CODE"] %in% analyse_ac_rp()[[1]]$donnees[input$mydonnees_ac_rp_rows_selected,"CODE"],varVolume]/calcul_max_rayon_metres_ac_rp()[[2]]),
                                                   options = pathOptions(pane = "selection", clickable = F),
                                                   fill = F,
                                                   group = "select_donnees")
              )
            }
          }
        }
      },ignoreInit = TRUE)
      
      observeEvent(input$onglets_ac_rp,{
        req(input$onglets_ac_rp)
        
        if(input$onglets_ac_rp == "carte")
        {
          proxy <- leafletProxy("mymap_ac_rp")
          
          clearGroup(map = proxy, group = "select_maille")
          
          if(!is.null(input$mymaille_ac_rp_rows_selected))
          {
            if(elargi_ac_rp())
            {
              proxy <- addPolygons(map = proxy, data = fond_select_maille_elargi_ac_rp(),
                                   stroke = TRUE, weight = 3,
                                   color="#FFFF00",
                                   options = pathOptions(pane = "selection", clickable = F),
                                   fill = F,
                                   group = "select_maille"
              )
            }else
            {
              proxy <- addPolygons(map = proxy, data = fond_select_maille_ac_rp(),
                                   stroke = TRUE, weight = 3,
                                   color="#FFFF00",
                                   options = pathOptions(pane = "selection", clickable = F),
                                   fill = F,
                                   group = "select_maille"
              )
            }
          }
        }
      },ignoreInit = TRUE)
      
      observeEvent(input$onglets_ac_rp,{
        req(input$onglets_ac_rp)
        
        if(input$onglets_ac_rp == "carte")
        {
          proxy <- leafletProxy("mymap_ac_rp")
          
          clearGroup(map = proxy, group = "select_contour")
          
          if(!is.null(input$mycontour_ac_rp_rows_selected))
          {
            proxy <- addPolygons(map = proxy, data = fond_select_contour_ac_rp(),
                                 stroke = TRUE, weight = 3,
                                 color="#FFFF00",
                                 options = pathOptions(pane = "selection", clickable = F),
                                 fill = F,
                                 group = "select_contour"
            )
          }
        }
      },ignoreInit = TRUE)
      
      # CONSTRUCTION DE LA LEGENDE
      
      lon_lat_ac_rp <- reactive({
        click <- input$mymap_ac_rp_click
        lon <- click$lng
        lat <- click$lat
        return(list(lon,lat))
      })
      
      observeEvent(list(input$mymap_ac_rp_zoom,input$mymap_ac_rp_click,input$type_legende_ac_rp_id,input$titre_ronds_legende_ac_rp_id,input$titre_classes_legende_ac_rp_id,input$taille_rond_ac_rp_id,input$nb_classes_ac_rp_id,input$methode_ac_rp_id,input$valid_bornes_ac_rp_id),{
        req(input$taille_rond_ac_rp_id)
        
        if(is.null(input$affiche_legende_ac_rp_id)) return(NULL)
        
        if(input$affiche_legende_ac_rp_id==FALSE) return(NULL)
        
        if(is.null(lon_lat_ac_rp()[[1]])) return(NULL)
        
        CONSTRUCTION_LEGENDE_AC_RP()
      })
      
      construction_legende_ac_rp <- reactive({
        zoom <- as.numeric(input$mymap_ac_rp_zoom)
        coeff <- ((360/(2^zoom))/7.2) # Permet de fixer une distance sur l'ecran. Il s'agit en gros d'une conversion des degres en pixels. Reste constant a longitude egale mais varie un peu selon la latitude
        ronds_leg <- construction_ronds_legende(lon_lat_ac_rp()[[1]],lon_lat_ac_rp()[[2]],code_epsg_ac_rp(),input$taille_rond_ac_rp_id)
        lignes <- construction_lignes_legende(ronds_leg,coeff,code_epsg_ac_rp())
        ronds_leg[[2]] <- cbind(ronds_leg[[2]],VALEUR=c(max(data[,varVolume]),max(data[,varVolume])/3))
        return(list(ronds_leg,lignes,coeff))
      })
      
      CONSTRUCTION_LEGENDE_AC_RP <- function()
      {
        proxy <- leafletProxy("mymap_ac_rp")
        
        proxy <- clearGroup(map=proxy, group="leg")
        proxy <- clearMarkers(map=proxy)
        
        zoom <- as.numeric(input$mymap_ac_rp_zoom)
        coeff <- ((360/(2^zoom))/7.2) # Permet de fixer une distance sur l'ecran. Il s'agit en gros d'une conversion des degres en pixels. Reste constant a longitude egale mais varie un peu selon la latitude
        
        ronds_leg <- construction_ronds_legende(lon_lat_ac_rp()[[1]],lon_lat_ac_rp()[[2]],code_epsg_ac_rp(),input$taille_rond_ac_rp_id)
        
        lignes <- construction_lignes_legende(ronds_leg,coeff,code_epsg_ac_rp())
        
        position_leg_classes <- t(data_frame(c(min(st_coordinates(ronds_leg[[1]])[which(st_coordinates(ronds_leg[[1]])[,4]==1),"X"]),min(st_coordinates(ronds_leg[[1]])[which(st_coordinates(ronds_leg[[1]])[,4]==1),"Y"]))))
        
        if(is.null(input$type_legende_ac_rp_id)) return()
        
        if(input$type_legende_ac_rp_id==1) # Litterale
        {
          # On cree les rectangles
          if(is.null(input$nb_classes_ac_rp_id))
            max_classes <- 4
          else
            max_classes <- input$nb_classes_ac_rp_id
          
          for(i in 1:max_classes)
          {
            # Coordonnees du point haut/gauche des rectangles de la legende
            x_coord_rectangle <- position_leg_classes[1]
            if(i==1) #1er rectangle
            {
              y_coord_rectangle <- position_leg_classes[2]-coeff*1.5 #1,5cm sous les ronds
            }else
            {
              y_coord_rectangle <- y_coord_rectangle-coeff*0.7
            }
            assign(paste0("rectangle_",i),list(matrix(c(x_coord_rectangle,y_coord_rectangle,x_coord_rectangle+coeff*1,y_coord_rectangle,x_coord_rectangle+coeff*1,y_coord_rectangle+coeff*0.5,x_coord_rectangle,y_coord_rectangle+coeff*0.5,x_coord_rectangle,y_coord_rectangle),ncol=2, byrow=TRUE)))
          }
          
          # On ajoute un cadre blanc autour de la legende
          y_coord_rectangle <- min(get(paste0("rectangle_",max_classes))[[1]][,2])
          
          # leaflet du cadre blanc en 1er
          proxy <- addRectangles(map = proxy,
                                 lng1 = position_leg_classes[1]-coeff*0.5, lat1 = lignes[[5]]+coeff*0.5,
                                 lng2 = x_coord_rectangle+coeff*6, lat2 = y_coord_rectangle-coeff*0.8,
                                 stroke = TRUE,
                                 color = paste0("#2B3E50", ";background: #ffffff;
                                                border-left:2px solid #2B3E50;
                                                border-right:2px solid #2B3E50;
                                                border-top:2px solid #2B3E50;
                                                border-bottom:2px solid #2B3E50;
                                                border-radius: 5%"),
                                 weight = 1,
                                 options = pathOptions(pane = "fond_legende", clickable = F),
                                 fill = T,
                                 fillColor = "white",
                                 fillOpacity = 0.8,
                                 group="leg"
                                 )
          
          # leaflet rectangles et valeurs classes
          classes_leg_texte <- analyse_leg_ac_rp()$rupture_classes
          legende$a <- c()
          for(i in 1: max_classes)
          {
            
            proxy <- addPolygons(map = proxy, data = st_polygon(get(paste0("rectangle_",i))),
                                 stroke = FALSE,
                                 options = pathOptions(pane = "fond_legende", clickable = F),
                                 fill = T,
                                 fillColor = analyse_leg_ac_rp()$pal_classes[i],
                                 fillOpacity = 1,
                                 group="leg"
            )
            
            if(i==1)
            {
              label_rectangle <- paste0(format(round(classes_leg_texte[i+1],3), big.mark=" ",decimal.mark=",",nsmall=0)," et plus")
              
              proxy <- addLabelOnlyMarkers(map = proxy,
                                           lng = (max(get(paste0("rectangle_",i))[[1]][,1])+coeff*0.1), lat = mean(get(paste0("rectangle_",i))[[1]][,2]),
                                           label = label_rectangle,
                                           labelOptions = labelOptions(noHide = T, textOnly = TRUE, direction = "right",
                                                                       style = list(
                                                                         "color" = "black",
                                                                         "font-size" = "12px"
                                                                       )),
                                           group="leg"
              )
              legende$a <- c(legende$a,label_rectangle)
              
            }else if (i>1 && i<max_classes)
            {
              label_rectangle <- paste0("De ", format(round(classes_leg_texte[i+1],3), big.mark=" ",decimal.mark=",",nsmall=0)," \u00E0 moins de ", format(round(classes_leg_texte[i],3), big.mark=" ",decimal.mark=",",nsmall=0))
              
              proxy <- addLabelOnlyMarkers(map = proxy,
                                           lng = max(get(paste0("rectangle_",i))[[1]][,1])+coeff*0.1, lat = mean(get(paste0("rectangle_",i))[[1]][,2]),
                                           label = label_rectangle,
                                           labelOptions = labelOptions(noHide = T, textOnly = TRUE, direction = "right",
                                                                       style = list(
                                                                         "color" = "black",
                                                                         "font-size" = "12px"
                                                                       )),
                                           group="leg"
              )
              legende$a <- c(legende$a,label_rectangle)
              
            }else #i==length(max_classes)
            {
              label_rectangle <- paste0("Moins de ", format(round(classes_leg_texte[i],3), big.mark=" ",decimal.mark=",",nsmall=0))
              
              proxy <- addLabelOnlyMarkers(map = proxy,
                                           lng = max(get(paste0("rectangle_",i))[[1]][,1])+coeff*0.1, lat = mean(get(paste0("rectangle_",i))[[1]][,2]),
                                           label = label_rectangle,
                                           labelOptions = labelOptions(noHide = T, textOnly = TRUE, direction = "right",
                                                                       style = list(
                                                                         "color" = "black",
                                                                         "font-size" = "12px"
                                                                       )),
                                           group="leg"
              )
              legende$a <- c(legende$a,label_rectangle)
              
            }
          }
          
          # On ajoute la legende de classes a l'analyse
          
          # leaflet titre 2
          x_titre_2 <- min(st_coordinates(st_polygon(get("rectangle_1")))[,"X"])
          y_titre_2 <- max(st_coordinates(st_polygon(get("rectangle_1")))[,"Y"])+coeff*0.2
          
          proxy <- addLabelOnlyMarkers(map = proxy,
                                       lng = x_titre_2, lat = y_titre_2,
                                       label = input$titre_classes_legende_ac_rp_id,
                                       labelOptions = labelOptions(noHide = T, textOnly = TRUE, direction = "right",
                                                                   style = list(
                                                                     "color" = "black",
                                                                     "font-size" = "14px"
                                                                   )),
                                       group="leg"
          )
        }
        
        if(input$type_legende_ac_rp_id==2) # Numerique
        {
          # On cree les rectangles
          if(is.null(input$nb_classes_ac_rp_id)) return(NULL)
          
          max_classes <- input$nb_classes_ac_rp_id
          
          for(i in 1:max_classes)
          {
            # Coordonnees du point haut/gauche des rectangles de la legende
            x_coord_rectangle <- position_leg_classes[1]
            if(i==1) #1er rectangle
            {
              y_coord_rectangle <- position_leg_classes[2]-coeff
            }else
            {
              y_coord_rectangle <- y_coord_rectangle-coeff*0.5
            }
            assign(paste0("rectangle_",i),list(matrix(c(x_coord_rectangle,y_coord_rectangle,x_coord_rectangle+coeff*1,y_coord_rectangle,x_coord_rectangle+coeff*1,y_coord_rectangle+coeff*0.5,x_coord_rectangle,y_coord_rectangle+coeff*0.5,x_coord_rectangle,y_coord_rectangle),ncol=2, byrow=TRUE)))
          }
          
          # On ajoute un cadre blanc autour de la legende
          y_coord_rectangle <- min(get(paste0("rectangle_",max_classes))[[1]][,2])
          
          # leaflet du cadre blanc en 1er
          proxy <- addRectangles(map = proxy,
                                 lng1 = position_leg_classes[1]-coeff*0.5, lat1 = lignes[[5]]+coeff*0.5,
                                 lng2 = x_coord_rectangle+coeff*6, lat2 = y_coord_rectangle-coeff*0.8,
                                 stroke = TRUE,
                                 color = paste0("#2B3E50", ";background: #ffffff;
                                                border-left:2px solid #2B3E50;
                                                border-right:2px solid #2B3E50;
                                                border-top:2px solid #2B3E50;
                                                border-bottom:2px solid #2B3E50;
                                                border-radius: 5%"),
                                 weight = 1,
                                 options = pathOptions(pane = "fond_legende", clickable = F),
                                 fill = T,
                                 fillColor = "white",
                                 fillOpacity = 0.8,
                                 group="leg"
                                 )
          
          # leaflet rectangles et valeurs classes
          classes_leg_num <- analyse_leg_ac_rp()$rupture_classes
          
          for(i in 1: max_classes)
          {
            proxy <- addPolygons(map = proxy, data = st_polygon(get(paste0("rectangle_",i))),
                                 stroke = FALSE,
                                 options = pathOptions(pane = "fond_legende", clickable = F),
                                 fill = T,
                                 fillColor = analyse_leg_ac_rp()$pal_classes[i],
                                 fillOpacity = 1,
                                 group="leg"
            )
            
            if(i<max_classes)
            {
              x1 <- max(get(paste0("rectangle_",i))[[1]][,1])
              y1 <- min(get(paste0("rectangle_",i))[[1]][,2])
              x2 <- max(get(paste0("rectangle_",i))[[1]][,1])+coeff*0.2
              y2 <- min(get(paste0("rectangle_",i))[[1]][,2])
              ligne <- st_linestring(rbind(c(x1,y1),c(x2,y2)))
              
              proxy <- addPolygons(map = proxy, data = ligne,
                                   color = "black",
                                   weight = 1,
                                   options = pathOptions(pane = "fond_legende", clickable = F),
                                   fill = F,
                                   fillOpacity = 1,
                                   group="leg"
              )
              
              proxy <- addLabelOnlyMarkers(map = proxy,
                                           lng = x2, lat = y2,
                                           label = as.character(format(round(classes_leg_num[i+1],3),big.mark=" ",decimal.mark=",",nsmall=0)),
                                           labelOptions = labelOptions(noHide = T, textOnly = TRUE, direction = "right",
                                                                       style = list(
                                                                         "color" = "black",
                                                                         "font-size" = "12px"
                                                                       )),
                                           group="leg"
              )
              
            }
          }
          
          # On ajoute la legende de classes a l'analyse
          
          # leaflet titre 2
          x_titre_2 <- min(get("rectangle_1")[[1]][,1])
          y_titre_2 <- max(get("rectangle_1")[[1]][,2])+coeff*0.2
          
          proxy <- addLabelOnlyMarkers(map = proxy,
                                       lng = x_titre_2, lat = y_titre_2,
                                       label = input$titre_classes_legende_ac_rp_id,
                                       labelOptions = labelOptions(noHide = T, textOnly = TRUE, direction = "right",
                                                                   style = list(
                                                                     "color" = "black",
                                                                     "font-size" = "14px"
                                                                   )),
                                       group="leg"
          )
        }
        
        suppressWarnings(proxy <- addCircles(map = proxy,
                                             lng = st_coordinates(st_centroid(ronds_leg[[1]]))[,1],
                                             lat = st_coordinates(st_centroid(ronds_leg[[1]]))[,2],
                                             stroke = TRUE,
                                             opacity = 1,
                                             color = "#2B3E50",
                                             weight = 2,
                                             radius = c(calcul_rond_ac_rp(),calcul_rond_ac_rp()/sqrt(3)),
                                             options = pathOptions(pane = "fond_legende", clickable = F),
                                             fill = T,
                                             fillColor = "white",
                                             fillOpacity = 1,
                                             group="leg"
        ))
        
        # leaflet lignes
        proxy <- addPolygons(map = proxy, data = lignes[[1]],
                             stroke = TRUE,
                             opacity = 1,
                             color = "#2B3E50",
                             weight = 2,
                             options = pathOptions(pane = "fond_legende", clickable = F),
                             fill = F,
                             fillOpacity = 1,
                             group="leg"
        )
        
        # leaflet valeur ronds
        proxy <- addLabelOnlyMarkers(map = proxy,
                                     lng = lignes[[2]][2,1], lat = lignes[[2]][2,2], #ligne_grand
                                     label = as.character(format(calcul_max_rayon_metres_ac_rp()[[2]],big.mark=" ",decimal.mark=",",nsmall=0)),
                                     labelOptions = labelOptions(noHide = T, textOnly = TRUE, direction = "right",
                                                                 style = list(
                                                                   "color" = "black",
                                                                   "font-size" = "12px"
                                                                 )),
                                     group="leg"
        )
        
        proxy <- addLabelOnlyMarkers(map = proxy,
                                     lng = lignes[[3]][2,1], lat = lignes[[3]][2,2], #ligne_petit
                                     label = as.character(format(round(calcul_max_rayon_metres_ac_rp()[[2]]/3,0),big.mark=" ",decimal.mark=",",nsmall=0)),
                                     labelOptions = labelOptions(noHide = T, textOnly = TRUE, direction = "right",
                                                                 style = list(
                                                                   "color" = "black",
                                                                   "font-size" = "12px"
                                                                 )),
                                     group="leg"
        )
        
        #leaflet titre 1
        proxy <- addLabelOnlyMarkers(map = proxy,
                                     lng = lignes[[4]], lat = lignes[[5]], #x_titre_1 et y_titre_1
                                     label = input$titre_ronds_legende_ac_rp_id,
                                     labelOptions = labelOptions(noHide = T, textOnly = TRUE, direction = "right",
                                                                 style = list(
                                                                   "color" = "black",
                                                                   "font-size" = "14px"
                                                                 )),
                                     group="leg"
        )
      }
      
      # AJOUT DES ONGLETS SAUVEGARDE
      
      observeEvent(input$save_carte_ac_rp_id,{
        
        showModal(modalDialog(HTML("<i class=\"fa fa-spinner fa-spin fa-2x fa-fw\"></i><font size=+1>Sauvegarde de la carte en cours...</font> "), size="m", footer=NULL, style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"))
        
        insert_save$a <- insert_save$a + 1
        nb_save_carte <- insert_save$a-remove_carte$a
        
        m_save <- m_save_ac_rp$a
        
        if(nb_save_carte>6)
        {
          insert_save$a <- insert_save$a - 1
          showModal(modalDialog(HTML("<font size=+1>Vous ne pouvez pas sauvegarger plus de 6 cartes. Veuillez en supprimer avant de continuer.</font> "), size="l", footer=NULL, easyClose = TRUE, style = "color: #fff; background-color: #DF691A; border-color: #2e6da4")) #337ab7
          return(NULL)
        }
        
        output[[paste0("mymap_save_",insert_save$a,"_ac_rp")]] <- renderLeaflet({
          
          if(!is.null(fondSuppl))
          {
            if(input$ajout_territoire_ac_rp_id)
            {
              m_save <- addPolygons(map = m_save, data = fond_territoire_ac_rp(),
                                    stroke = TRUE, color = "#BFBFBF", opacity = 1,
                                    weight = 0.5,
                                    options = pathOptions(pane = "fond_territoire", clickable = T),
                                    popup = paste0("<b> <font color=#2B3E50>",as.data.frame(fond_territoire_ac_rp())[,"LIBELLE"], "</font> </b>"),
                                    fill = T, fillColor = "white", fillOpacity = 0.001
              )
            }
          }
          
          if(input$ajout_reg_ac_rp_id)
          {
            m_save <- addPolygons(map = m_save, data = fond_region_ac_rp(),
                                  stroke = TRUE, color = "grey", opacity = 1,
                                  weight = 1.5,
                                  options = pathOptions(pane = "fond_reg", clickable = F),
                                  fill = F
            )
          }
          
          if(input$ajout_dep_ac_rp_id)
          {
            m_save <- addPolygons(map = m_save, data = fond_departement_ac_rp(),
                                  stroke = TRUE, color = "grey", opacity = 1,
                                  weight = 0.5,
                                  options = pathOptions(pane = "fond_dep", clickable = F),
                                  fill = F
            )
          }
          
          i <- 1 #pour gerer l'ordre des fonds dans le pane
          
          for(fond in liste_fonds$a)
          {
            if(fond=="analyse")
            {
              m_save <- addCircles(map = m_save,
                                   lng = st_coordinates(analyse_ac_rp()[[2]])[,1],
                                   lat = st_coordinates(analyse_ac_rp()[[2]])[,2],
                                   stroke = TRUE, color = "#303030",
                                   opacity = 1,
                                   weight = 1.5,
                                   radius = calcul_rond_ac_rp()*sqrt(analyse_ac_rp()[[1]]$donnees[,varVolume]/calcul_max_rayon_metres_ac_rp()[[2]]),
                                   options = pathOptions(pane = paste0("fond_trio",i), clickable = T),
                                   popup = paste0("<b> <font color=#2B3E50>",varVolume," : </font></b>",analyse_ac_rp()[[1]]$donnees$TXT1),
                                   fill = F
              )
            }
            
            if(fond=="maille")
            {
              suppressWarnings(test_analyse_maille_classe <- try(analyse_ac_rp()[[1]]$donnees[rev(order(analyse_ac_rp()[[1]]$donnees[,varVolume])),varRatio],silent=T))
              if(class(test_analyse_maille_classe) %in% "try-error")
              {
                return(NULL)
              }else
              {
                analyse_maille_classe <- analyse_ac_rp()[[1]]$donnees[rev(order(analyse_ac_rp()[[1]]$donnees[,varVolume])),varRatio]
              }
              
              analyse_maille <- merge(fond_contour_maille_ac_rp()[[2]][,c("CODE","geometry")],analyse_ac_rp()[[1]]$donnees[,c("CODE","LIBELLE",varVolume,varRatio,"TXT1","TXT2")],by="CODE")
              names(analyse_maille)[3] <- varVolume
              names(analyse_maille)[4] <- varRatio
              analyse_maille <- analyse_maille[rev(order(as.data.frame(analyse_maille)[,varVolume])),]
              analyse_maille <- st_sf(analyse_maille,stringsAsFactors = FALSE)
              
              m_save <- addPolygons(map = m_save, data = analyse_maille, opacity = 1,
                                    stroke = TRUE, color = "white", weight = 1,
                                    options = pathOptions(pane = paste0("fond_trio",i), clickable = T),
                                    popup = paste0("<b> <font color=#2B3E50>",as.data.frame(analyse_maille)$LIBELLE, "</font> </b><br>",
                                                   "<b><font color=#2B3E50>",varRatio," : </font></b>",as.data.frame(analyse_maille)$TXT2),
                                    fill = T,
                                    fillColor = palette_ac_rp()[[1]](analyse_maille_classe),
                                    fillOpacity = 1
              )
            }
            
            if(fond=="contour")
            {
              m_save <- addPolygons(map = m_save, data = fond_contour_maille_ac_rp()[[1]], opacity = 0.3, #contour_WGS84
                                    stroke = TRUE, color = "black", weight = 3,
                                    options = pathOptions(pane = paste0("fond_trio",i), clickable = T),
                                    popup = paste0("<b> <font color=#2B3E50>",as.data.frame(fond_contour_maille_ac_rp()[[1]])[,"LIBELLE"], "</font> </b>"),
                                    fill = T, fillColor = "white", fillOpacity = 0.3
              )
            }
            
            i <- i + 1
          }
          
          if(elargi_ac_rp())
          {
            analyse_maille_classe_elargi <- analyse_ac_rp()[[1]]$donnees_elargi[rev(order(analyse_ac_rp()[[1]]$donnees_elargi[,varVolume])),varRatio]
            
            analyse_maille_elargi <- merge(fond_elargi_ac_rp()[[2]][,c("CODE","geometry")],analyse_ac_rp()[[1]]$donnees_elargi[,c("CODE","LIBELLE",varVolume,varRatio,"TXT1","TXT2")],by="CODE")
            names(analyse_maille_elargi)[3] <- varVolume
            names(analyse_maille_elargi)[4] <- varRatio
            analyse_maille_elargi <- analyse_maille_elargi[rev(order(as.data.frame(analyse_maille_elargi)[,varVolume])),]
            analyse_maille_elargi <- st_sf(analyse_maille_elargi,stringsAsFactors = FALSE)
            
            m_save <- addPolygons(map = m_save, data = analyse_maille_elargi, opacity = input$opacite_elargi_ac_rp_id/100,
                                  stroke = TRUE, color = "white", weight = 1,
                                  options = pathOptions(pane = "fond_trio3", clickable = T),
                                  popup = paste0("<b> <font color=#2B3E50>",as.data.frame(analyse_maille_elargi)$LIBELLE, "</font> </b><br>",
                                                 "<b><font color=#2B3E50>",varRatio," : </font></b>",as.data.frame(analyse_maille_elargi)$TXT2),
                                  fill = T,
                                  fillColor = palette_ac_rp()[[1]](analyse_maille_classe_elargi),
                                  fillOpacity = input$opacite_elargi_ac_rp_id/100
            )
            
            m_save <- addCircles(map = m_save,
                                 lng = st_coordinates(fond_elargi_ac_rp()[[1]])[,1],
                                 lat = st_coordinates(fond_elargi_ac_rp()[[1]])[,2],
                                 stroke = TRUE, color = "#303030",
                                 opacity = input$opacite_elargi_ac_rp_id/100,
                                 weight = 1.5,
                                 radius = calcul_rond_ac_rp()*sqrt(analyse_ac_rp()[[1]]$donnees_elargi[,varVolume]/calcul_max_rayon_metres_ac_rp()[[2]]),
                                 options = pathOptions(pane = "fond_trio3", clickable = T),
                                 popup = paste0("<b> <font color=#2B3E50>",varVolume," : </font></b>",analyse_ac_rp()[[1]]$donnees_elargi$TXT1),
                                 fill = F
            )
          }
          
          zoom <- as.numeric(input$mymap_ac_rp_zoom)
          coeff <- ((360/(2^zoom))/7.2) # Permet de fixer une distance sur l'ecran. Il s'agit en gros d'une conversion des degres en pixels. Reste constant a longitude egale mais varie un peu selon la latitude
          
          ronds_leg <- construction_ronds_legende(lon_lat_ac_rp()[[1]],lon_lat_ac_rp()[[2]],code_epsg_ac_rp(),input$taille_rond_ac_rp_id)
          
          lignes <- construction_lignes_legende(ronds_leg,coeff,code_epsg_ac_rp())
          
          position_leg_classes <- t(data_frame(c(min(st_coordinates(ronds_leg[[1]])[which(st_coordinates(ronds_leg[[1]])[,4]==1),"X"]),min(st_coordinates(ronds_leg[[1]])[which(st_coordinates(ronds_leg[[1]])[,4]==1),"Y"]))))
          
          if(input$type_legende_ac_rp_id==1) # Litterale
          {
            # On cree les rectangles
            max_classes <- input$nb_classes_ac_rp_id
            
            for(i in 1:max_classes)
            {
              # Coordonnees du point haut/gauche des rectangles de la legende
              x_coord_rectangle <- position_leg_classes[1]
              if(i==1) #1er rectangle
              {
                y_coord_rectangle <- position_leg_classes[2]-coeff*1.5 #1,5cm sous les ronds
              }else
              {
                y_coord_rectangle <- y_coord_rectangle-coeff*0.7
              }
              assign(paste0("rectangle_",i),list(matrix(c(x_coord_rectangle,y_coord_rectangle,x_coord_rectangle+coeff*1,y_coord_rectangle,x_coord_rectangle+coeff*1,y_coord_rectangle+coeff*0.5,x_coord_rectangle,y_coord_rectangle+coeff*0.5,x_coord_rectangle,y_coord_rectangle),ncol=2, byrow=TRUE)))
            }
            
            # On ajoute un cadre blanc autour de la legende
            y_coord_rectangle <- min(get(paste0("rectangle_",max_classes))[[1]][,2])
            
            # leaflet du cadre blanc en 1er
            m_save <- addRectangles(map = m_save,
                                    lng1 = position_leg_classes[1]-coeff*0.5, lat1 = lignes[[5]]+coeff*0.5,
                                    lng2 = x_coord_rectangle+coeff*6, lat2 = y_coord_rectangle-coeff*0.8,
                                    stroke = TRUE,
                                    color = paste0("#2B3E50", ";background: #ffffff;
                                                   border-left:2px solid #2B3E50;
                                                   border-right:2px solid #2B3E50;
                                                   border-top:2px solid #2B3E50;
                                                   border-bottom:2px solid #2B3E50;
                                                   border-radius: 5%"),
                                    weight = 1,
                                    options = pathOptions(pane = "fond_legende", clickable = F),
                                    fill = T,
                                    fillColor = "white",
                                    fillOpacity = 0.8
                                    )
            
            # leaflet rectangles et valeurs classes
            classes_leg_texte <- analyse_leg_ac_rp()$rupture_classes
            
            for(i in 1: max_classes)
            {
              
              m_save <- addPolygons(map = m_save, data = st_polygon(get(paste0("rectangle_",i))),
                                    stroke = FALSE,
                                    options = pathOptions(pane = "fond_legende", clickable = F),
                                    fill = T,
                                    fillColor = analyse_leg_ac_rp()$pal_classes[i],
                                    fillOpacity = 1
              )
              
              if(i==1)
              {
                label_rectangle <- paste0(format(round(classes_leg_texte[i+1],3), big.mark=" ",decimal.mark=",",nsmall=0)," et plus")
                
                m_save <- addLabelOnlyMarkers(map = m_save,
                                              lng = (max(get(paste0("rectangle_",i))[[1]][,1])+coeff*0.1), lat = mean(get(paste0("rectangle_",i))[[1]][,2]),
                                              label = label_rectangle,
                                              labelOptions = labelOptions(noHide = T, textOnly = TRUE, direction = "right",
                                                                          style = list(
                                                                            "color" = "black",
                                                                            "font-size" = "12px"
                                                                          ))
                )
                
              }else if (i>1 && i<max_classes)
              {
                label_rectangle <- paste0("De ", format(round(classes_leg_texte[i+1],3), big.mark=" ",decimal.mark=",",nsmall=0)," \u00E0 moins de ", format(round(classes_leg_texte[i],3), big.mark=" ",decimal.mark=",",nsmall=0))
                
                m_save <- addLabelOnlyMarkers(map = m_save,
                                              lng = max(get(paste0("rectangle_",i))[[1]][,1])+coeff*0.1, lat = mean(get(paste0("rectangle_",i))[[1]][,2]),
                                              label = label_rectangle,
                                              labelOptions = labelOptions(noHide = T, textOnly = TRUE, direction = "right",
                                                                          style = list(
                                                                            "color" = "black",
                                                                            "font-size" = "12px"
                                                                          ))
                )
                
              }else #i==length(max_classes)
              {
                label_rectangle <- paste0("Moins de ", format(round(classes_leg_texte[i],3), big.mark=" ",decimal.mark=",",nsmall=0))
                
                m_save <- addLabelOnlyMarkers(map = m_save,
                                              lng = max(get(paste0("rectangle_",i))[[1]][,1])+coeff*0.1, lat = mean(get(paste0("rectangle_",i))[[1]][,2]),
                                              label = label_rectangle,
                                              labelOptions = labelOptions(noHide = T, textOnly = TRUE, direction = "right",
                                                                          style = list(
                                                                            "color" = "black",
                                                                            "font-size" = "12px"
                                                                          ))
                )
              }
            }
            
            # On ajoute la legende de classes a l'analyse
            
            # leaflet titre 2
            x_titre_2 <- min(st_coordinates(st_polygon(get("rectangle_1")))[,"X"])
            y_titre_2 <- max(st_coordinates(st_polygon(get("rectangle_1")))[,"Y"])+coeff*0.2
            
            m_save <- addLabelOnlyMarkers(map = m_save,
                                          lng = x_titre_2, lat = y_titre_2,
                                          label = input$titre_classes_legende_ac_rp_id,
                                          labelOptions = labelOptions(noHide = T, textOnly = TRUE, direction = "right",
                                                                      style = list(
                                                                        "color" = "black",
                                                                        "font-size" = "14px"
                                                                      ))
            )
          }
          
          if(input$type_legende_ac_rp_id==2) # Numerique
          {
            # On cree les rectangles
            max_classes <- input$nb_classes_ac_rp_id
            
            for(i in 1:max_classes)
            {
              # Coordonnees du point haut/gauche des rectangles de la legende
              x_coord_rectangle <- position_leg_classes[1]
              if(i==1) #1er rectangle
              {
                y_coord_rectangle <- position_leg_classes[2]-coeff
              }else
              {
                y_coord_rectangle <- y_coord_rectangle-coeff*0.5
              }
              assign(paste0("rectangle_",i),list(matrix(c(x_coord_rectangle,y_coord_rectangle,x_coord_rectangle+coeff*1,y_coord_rectangle,x_coord_rectangle+coeff*1,y_coord_rectangle+coeff*0.5,x_coord_rectangle,y_coord_rectangle+coeff*0.5,x_coord_rectangle,y_coord_rectangle),ncol=2, byrow=TRUE)))
            }
            
            # On ajoute un cadre blanc autour de la legende
            y_coord_rectangle <- min(get(paste0("rectangle_",max_classes))[[1]][,2])
            
            # leaflet du cadre blanc en 1er
            m_save <- addRectangles(map = m_save,
                                    lng1 = position_leg_classes[1]-coeff*0.5, lat1 = lignes[[5]]+coeff*0.5,
                                    lng2 = x_coord_rectangle+coeff*6, lat2 = y_coord_rectangle-coeff*0.8,
                                    stroke = TRUE,
                                    color = paste0("#2B3E50", ";background: #ffffff;
                                                   border-left:2px solid #2B3E50;
                                                   border-right:2px solid #2B3E50;
                                                   border-top:2px solid #2B3E50;
                                                   border-bottom:2px solid #2B3E50;
                                                   border-radius: 5%"),
                                    weight = 1,
                                    options = pathOptions(pane = "fond_legende", clickable = F),
                                    fill = T,
                                    fillColor = "white",
                                    fillOpacity = 0.8
                                    )
            
            # leaflet rectangles et valeurs classes
            classes_leg_num <- analyse_leg_ac_rp()$rupture_classes
            
            for(i in 1: max_classes)
            {
              m_save <- addPolygons(map = m_save, data = st_polygon(get(paste0("rectangle_",i))),
                                    stroke = FALSE,
                                    options = pathOptions(pane = "fond_legende", clickable = F),
                                    fill = T,
                                    fillColor = analyse_leg_ac_rp()$pal_classes[i],
                                    fillOpacity = 1
              )
              
              if(i<max_classes)
              {
                x1 <- max(get(paste0("rectangle_",i))[[1]][,1])
                y1 <- min(get(paste0("rectangle_",i))[[1]][,2])
                x2 <- max(get(paste0("rectangle_",i))[[1]][,1])+coeff*0.2
                y2 <- min(get(paste0("rectangle_",i))[[1]][,2])
                ligne <- st_linestring(rbind(c(x1,y1),c(x2,y2)))
                
                m_save <- addPolygons(map = m_save, data = ligne,
                                      color = "black",
                                      weight = 1,
                                      options = pathOptions(pane = "fond_legende", clickable = F),
                                      fill = F,
                                      fillOpacity = 1
                )
                
                m_save <- addLabelOnlyMarkers(map = m_save,
                                              lng = x2, lat = y2,
                                              label = as.character(format(round(classes_leg_num[i+1],3),big.mark=" ",decimal.mark=",",nsmall=0)),
                                              labelOptions = labelOptions(noHide = T, textOnly = TRUE, direction = "right",
                                                                          style = list(
                                                                            "color" = "black",
                                                                            "font-size" = "12px"
                                                                          ))
                )
                
              }
            }
            
            # On ajoute la legende de classes a l'analyse
            
            # leaflet titre 2
            x_titre_2 <- min(get("rectangle_1")[[1]][,1])
            y_titre_2 <- max(get("rectangle_1")[[1]][,2])+coeff*0.2
            
            m_save <- addLabelOnlyMarkers(map = m_save,
                                          lng = x_titre_2, lat = y_titre_2,
                                          label = input$titre_classes_legende_ac_rp_id,
                                          labelOptions = labelOptions(noHide = T, textOnly = TRUE, direction = "right",
                                                                      style = list(
                                                                        "color" = "black",
                                                                        "font-size" = "14px"
                                                                      ))
            )
          }
          
          suppressWarnings(m_save <- addCircles(map = m_save,
                                                lng = st_coordinates(st_centroid(ronds_leg[[1]]))[,1],
                                                lat = st_coordinates(st_centroid(ronds_leg[[1]]))[,2],
                                                stroke = TRUE,
                                                opacity = 1,
                                                color = "#2B3E50",
                                                weight = 2,
                                                radius = c(calcul_rond_ac_rp(),calcul_rond_ac_rp()/sqrt(3)),
                                                options = pathOptions(pane = "fond_legende", clickable = F),
                                                fill = T,
                                                fillColor = "white",
                                                fillOpacity = 1
          ))
          
          # leaflet lignes
          m_save <- addPolygons(map = m_save, data = lignes[[1]],
                                stroke = TRUE,
                                opacity = 1,
                                color = "#2B3E50",
                                weight = 2,
                                options = pathOptions(pane = "fond_legende", clickable = F),
                                fill = F,
                                fillOpacity = 1
          )
          
          # leaflet valeur ronds
          m_save <- addLabelOnlyMarkers(map = m_save,
                                        lng = lignes[[2]][2,1], lat = lignes[[2]][2,2], #ligne_grand
                                        label = as.character(format(calcul_max_rayon_metres_ac_rp()[[2]],big.mark=" ",decimal.mark=",",nsmall=0)),
                                        labelOptions = labelOptions(noHide = T, textOnly = TRUE, direction = "right",
                                                                    style = list(
                                                                      "color" = "black",
                                                                      "font-size" = "12px"
                                                                    ))
          )
          
          m_save <- addLabelOnlyMarkers(map = m_save,
                                        lng = lignes[[3]][2,1], lat = lignes[[3]][2,2], #ligne_petit
                                        label = as.character(format(round(calcul_max_rayon_metres_ac_rp()[[2]]/3,0),big.mark=" ",decimal.mark=",",nsmall=0)),
                                        labelOptions = labelOptions(noHide = T, textOnly = TRUE, direction = "right",
                                                                    style = list(
                                                                      "color" = "black",
                                                                      "font-size" = "12px"
                                                                    ))
          )
          
          #leaflet titre 1
          m_save <- addLabelOnlyMarkers(map = m_save,
                                        lng = lignes[[4]], lat = lignes[[5]], #x_titre_1 et y_titre_1
                                        label = input$titre_ronds_legende_ac_rp_id,
                                        labelOptions = labelOptions(noHide = T, textOnly = TRUE, direction = "right",
                                                                    style = list(
                                                                      "color" = "black",
                                                                      "font-size" = "14px"
                                                                    ))
          )
          
          removeModal()
          
          m_save
        })
        
        output[[paste0("remove_carte_",nb_save_carte,"_ac_rp")]] <- renderUI({
          actionButton(paste0("remove_carte_",nb_save_carte,"_ac_rp_id"),label="X Supprimer la carte", style="color:#FFFFFF; border-color:#FFFFFF; background-color:#2B3E50")
        })
        
        appendTab(inputId = "onglets_ac_rp",
                  tabPanel(title=HTML(paste0("<font color=#AFC9E0> Carte ",insert_save$a,"</font>")),value=paste0("carte",nb_save_carte),uiOutput(paste0("remove_carte_",nb_save_carte,"_ac_rp")),leafletOutput(paste0("mymap_save_",insert_save$a,"_ac_rp"),width="100%",height = 800)),
                  select = TRUE,
                  session = session
        )
        
      }, ignoreInit = TRUE)
      
      observeEvent(input$remove_carte_1_ac_rp_id,{
        remove_carte$a <- remove_carte$a + 1
        removeTab(inputId = "onglets_ac_rp",
                  target = "carte1",
                  session = session
        )
      }, ignoreInit = TRUE)
      
      observeEvent(input$remove_carte_2_ac_rp_id,{
        remove_carte$a <- remove_carte$a + 1
        removeTab(inputId = "onglets_ac_rp",
                  target = "carte2",
                  session = session
        )
      }, ignoreInit = TRUE)
      
      observeEvent(input$remove_carte_3_ac_rp_id,{
        remove_carte$a <- remove_carte$a + 1
        removeTab(inputId = "onglets_ac_rp",
                  target = "carte3",
                  session = session
        )
      }, ignoreInit = TRUE)
      
      observeEvent(input$remove_carte_4_ac_rp_id,{
        remove_carte$a <- remove_carte$a + 1
        removeTab(inputId = "onglets_ac_rp",
                  target = "carte4",
                  session = session
        )
      }, ignoreInit = TRUE)
      
      observeEvent(input$remove_carte_5_ac_rp_id,{
        remove_carte$a <- remove_carte$a + 1
        removeTab(inputId = "onglets_ac_rp",
                  target = "carte5",
                  session = session
        )
      }, ignoreInit = TRUE)
      
      observeEvent(input$remove_carte_6_ac_rp_id,{
        remove_carte$a <- remove_carte$a + 1
        removeTab(inputId = "onglets_ac_rp",
                  target = "carte6",
                  session = session
        )
      }, ignoreInit = TRUE)
      
      # TABLEAUX DE DONNEES, MAILLE ET CONTOUR
      
      output$mydonnees_ac_rp <- DT::renderDataTable(datatable({
        if(elargi_ac_rp())
          data <- analyse_ac_rp()[[1]]$donnees_elargi
        else
          data <- analyse_ac_rp()[[1]]$donnees
        tableau_donnees <- data[,c("CODE","LIBELLE",varVolume,varRatio)]
      },  style = 'bootstrap'
      ))
      
      output$mymaille_ac_rp <- DT::renderDataTable(datatable({
        if(elargi_ac_rp())
          data <- as.data.frame(fondMailleElargi)
        else
          data <- as.data.frame(fondMaille)
        tableau_maille <- data[,c(1:2)]
      },  style = 'bootstrap'
      ))
      
      output$mycontour_ac_rp <- DT::renderDataTable(datatable({
        data <- as.data.frame(fondContour)
        tableau_contour <- data[,c(1:2)]
      },  style = 'bootstrap'
      ))
      
      # ENVOI DU LEAFLET A L'UI
      
      output$mymap_ac_rp <- renderLeaflet({
        react_fond_ac_rp()
      })
      
    }
    
    runApp(shinyApp(ui = ui, server = server), launch.browser = TRUE)
    }


shiny_typo <-
  function(data,fondMaille,fondContour,fondSuppl=NULL,idData,varTypo,dom="0")
  {
    options("stringsAsFactors"=FALSE)
    
    # Verification des parametres
    msg_error1<-msg_error2<-msg_error3<-msg_error4<-msg_error5<-msg_error6<-msg_error7<-msg_error8<-msg_error9<-msg_error10<-msg_error11<-msg_error12<-msg_error13<-msg_error14 <- NULL
    
    if(any(class(data)!="data.frame")) msg_error1 <- "Les donnees doivent etre dans un data.frame / "
    if(any(!any(class(fondMaille) %in% "sf"),!any(class(fondMaille) %in% "data.frame"))) msg_error2 <- "Le fond de maille doit etre un objet sf / "
    if(any(!any(class(fondContour) %in% "sf"),!any(class(fondContour) %in% "data.frame"))) msg_error3 <- "Le fond de contour doit etre un objet sf / "
    if(!is.null(fondSuppl)) if(any(!any(class(fondSuppl) %in% "sf"),!any(class(fondSuppl) %in% "data.frame"))) msg_error4 <- "Le fond supplementaire doit etre un objet sf / "
    if(any(class(idData)!="character")) msg_error5 <- "Le nom de la variable doit etre de type caractere / "
    if(any(class(varTypo)!="character")) msg_error6 <- "Le nom de la variable doit etre de type caractere / "
    if(any(class(dom)!="character")) msg_error7 <- "La valeur doit etre de type caractere ('0', '971', '972', '973', '974' ou '976') / "
    
    if(length(names(data))<2) msg_error8 <- "Le tableau des donnees n'est pas conforme. Il doit contenir au minimum une variable identifiant et la variable a representer / "
    if(length(names(fondMaille))<3) msg_error9 <- "Le fond de maille n'est pas conforme. La table doit contenir au minimum une variable identifiant, une variable libelle et la geometry / "
    if(length(names(fondContour))<3) msg_error10 <- "Le fond de contour n'est pas conforme. La table doit contenir au minimum une variable identifiant, une variable libelle et la geometry / "
    if(!is.null(fondSuppl)) if(length(names(fondSuppl))<3) msg_error11 <- "Le fond supplementaire n'est pas conforme. La table doit contenir au minimum une variable identifiant, une variable libelle et la geometry / "
    
    if(!any(names(data) %in% idData))  msg_error12 <- "La variable identifiant les donnees n'existe pas dans la table des donnees / "
    if(!any(names(data) %in% varTypo))  msg_error13 <- "La variable a representer n'existe pas dans la table des donnees / "
    if(!dom %in% c("0","971","972","973","974","976")) msg_error14 <- "La variable dom doit etre '0', '971', '972', '973', '974' ou '976' / "
    
    if(any(!is.null(msg_error1),!is.null(msg_error2),!is.null(msg_error3),!is.null(msg_error4),
           !is.null(msg_error5),!is.null(msg_error6),!is.null(msg_error7),!is.null(msg_error8),
           !is.null(msg_error10),!is.null(msg_error11),!is.null(msg_error12),!is.null(msg_error13),
           !is.null(msg_error14)))
    {
      stop(simpleError(paste0(msg_error1,msg_error2,msg_error3,msg_error4,msg_error5,msg_error6,msg_error7,msg_error8,
                              msg_error10,msg_error11,msg_error12,msg_error13,msg_error14)))
    }
    
    nb_up <- reactiveValues(a=0)
    nb_down <- reactiveValues(a=0)
    ordre_analyse <- reactiveValues(a=1,b=2)
    insert_save <- reactiveValues(a=0)
    remove_carte <- reactiveValues(a=0)
    liste_fonds_ty <- reactiveValues(a=c("analyse/maille","contour"))
    m_save_ty <- reactiveValues(a=0)
    
    erreur_maille <- reactiveValues(a=FALSE)
    
    legende <- reactiveValues(a=NULL)
    
    sourc <- "Source : Insee"
    
    names(data)[names(data)==idData] <- "CODE"
    names(fondMaille)[1] <- "CODE"
    names(fondMaille)[2] <- "LIBELLE"
    names(fondContour)[1] <- "CODE"
    names(fondContour)[2] <- "LIBELLE"
    if(!is.null(fondSuppl)) 
    {
      names(fondSuppl)[1] <- "CODE"
      names(fondSuppl)[2] <- "LIBELLE"
      fondSuppl$LIBELLE<-iconv(fondSuppl$LIBELLE,"latin1","utf8")
    }
    
    fondMaille$LIBELLE<-iconv(fondMaille$LIBELLE,"latin1","utf8")
    fondContour$LIBELLE<-iconv(fondContour$LIBELLE,"latin1","utf8")
    
    ui <- navbarPage("OCEANIS", id="menu",
                     
                     theme = shinytheme("superhero"),
                     
                     tabPanel("Carte",value="carte",
                              sidebarLayout(
                                
                                sidebarPanel(width = 3,
                                             style = "overflow-y:scroll; min-height: 840px; max-height: 840px",
                                             h4(HTML("<b><font color=#95BAE2>VARIABLES</font></b>")),
                                             uiOutput("variable_typo_ty"),
                                             tags$hr(style="border: 5px solid #5182B6"), #337ab7
                                             h4(HTML("<b><font color=#95BAE2>FONDS</font></b>")),
                                             fluidRow(
                                               column(width=9, offset=0.5,
                                                      uiOutput("ordre_fonds_ty")
                                               ),
                                               column(width=1,
                                                      br(),
                                                      br(),
                                                      htmlOutput("monter_fond_ty", inline=FALSE),
                                                      htmlOutput("descendre_fond_ty", inline=FALSE)
                                               )
                                             ),
                                             uiOutput("ajout_territoire_ty"),
                                             uiOutput("ajout_reg_ty"),
                                             uiOutput("ajout_dep_ty"),
                                             tags$hr(style="border: 5px solid #5182B6"),
                                             h4(HTML("<b><font color=#95BAE2>TYPOLOGIE</font></b>")),
                                             uiOutput("zone_modif_libelle_ty"),
                                             tags$hr(style="border: 5px solid #5182B6"),
                                             h4(HTML("<b><font color=#95BAE2>L\u00c9GENDE</font></b>")),
                                             uiOutput("titre_typo_legende_ty"),
                                             br(),
                                             uiOutput("affiche_legende_ty"),
                                             br(),
                                             tags$hr(style="border: 5px solid #5182B6"),
                                             h4(HTML("<b><font color=#95BAE2>SAUVEGARDE</font></b>")),
                                             uiOutput("save_carte_ty"),
                                             br(),
                                             conditionalPanel(condition = 'input.mymap_ty_click',
                                                              tags$div(class="dropup",
                                                                       HTML('<button class="btn btn-primary dropdown-toggle" type="button" data-toggle="dropdown" aria-haspopup="true" aria-expanded="false">
                                                                            Exporter en projet Qgis
                                                                            <span class="caret"></span>
                                                                            </button>'),
                                                                       tags$ul(class="dropdown-menu",
                                                                               wellPanel(
                                                                                 style="background: #2B3E50; width:340px",
                                                                                 h4("Export de la carte en projet Qgis"),
                                                                                 br(),
                                                                                 uiOutput("sortie_qgis_ty"),
                                                                                 br(),
                                                                                 uiOutput("titre1_qgis_ty"),
                                                                                 uiOutput("titre2_qgis_ty"),
                                                                                 uiOutput("source_qgis_ty"),
                                                                                 tags$head(tags$style(HTML('#export_qgis_ty_id{background-color:#337ab7}'))),
                                                                                 uiOutput("export_qgis_ty")
                                                                               )
                                                                       )
                                                                       )
                                                              
                                             ),
                                             br(),
                                             uiOutput("aide_image_ty"),
                                             br()
                              ),
                              mainPanel(
                                tags$head(
                                  tags$style(HTML(".leaflet-container { background: #AFC9E0; }"))
                                ),
                                tabsetPanel(id="onglets_ty",
                                            tabPanel(title=HTML("<b>Carte</b>"),value="carte",
                                                     leafletOutput("mymap_ty",width="100%",height = 800)),
                                            tabPanel(title=HTML(paste0("<b>Donn","\u00e9","es/Maille</b>")),value="donnees",
                                                     h5("S\u00e9lectionnez une ou plusieurs lignes pour ensuite les visualiser sur la carte."),
                                                     DT::dataTableOutput("mydonnees_ty",width="100%",height = 800)),
                                            tabPanel(title=HTML("<b>Contour</b>"),value="contour",
                                                     h5("S\u00e9lectionnez une ou plusieurs lignes pour ensuite les visualiser sur la carte."),
                                                     DT::dataTableOutput("mycontour_ty",width="100%",height = 800))
                                )
                              )
    )
    )
    )
    
    server <- function(input, output, session) {
      
      #################
      #Onglet Carte
      #################
      
      #Charge les donnees et fonds en memoire et affiche les widgets adequats dans le sidePanel de l'onglet "Carte"
      
      observe({
        
        # VARIABLES
        
        output$variable_typo_ty <- renderUI({
          selectInput("variable_typo_ty_id", label=h5("Variable typologique"), choices = varTypo, selected = varTypo)
        })
        
        # FONDS
        
        output$ordre_fonds_ty <- renderUI({
          selectInput("ordre_fonds_ty_id", label=h5("Modifier l'ordre des fonds"), choices = liste_fonds_ty$a, multiple=TRUE, selectize=FALSE, selected = "analyse")
        })
        output$monter_fond_ty <- renderUI({
          actionButton("monter_fond_ty_id", label="", icon=icon("arrow-up"))
        })
        output$descendre_fond_ty <- renderUI({
          actionButton("descendre_fond_ty_id", label="", icon=icon("arrow-down"))
        })
        
        output$ajout_territoire_ty <- renderUI({
          checkboxInput("ajout_territoire_ty_id", label = "Afficher le fond des territoires",
                        value = if(is.null(fondSuppl)) FALSE else TRUE)
        })
        output$ajout_reg_ty <- renderUI({
          checkboxInput("ajout_reg_ty_id", label = "Afficher le fond des r\u00e9gions",
                        value = FALSE)
        })
        output$ajout_dep_ty <- renderUI({
          checkboxInput("ajout_dep_ty_id", label = "Afficher le fond des d\u00e9partements",
                        value = FALSE)
        })
        
        # TYPOLOGIE
        
        observeEvent(list(input$affiche_carte_charge_ty_id,input$affiche_carte_liste_ty_id),{
          output$zone_modif_libelle_ty <- renderUI({
            lapply(1:(as.numeric(length(unique(analyse_ty()[[1]]$classe)))), function(i) {
              textInput(inputId = paste0("lib_typo_", i,"_ty_id"), label = paste("Libelle de la modalite", i),
                        value = sort(unique(analyse_ty()[[1]]$valeur))[i])
            })
          })
        })
        
        # LEGENDE
        
        output$titre_typo_legende_ty <- renderUI({
          textInput("titre_typo_legende_ty_id", label = h5("Titre de la l\u00e9gende de la typologie"), value = "")
        })
        
        output$affiche_legende_ty <- renderUI({
          checkboxInput("affiche_legende_ty_id", label = "Activer le d\u00e9placement de la l\u00e9gende au clic",
                        value = TRUE)
        })
        
        # SAUVEGARDE
        
        output$save_carte_ty <- renderUI({
          actionButton("save_carte_ty_id", label=HTML("<font size=3>Sauvegarder la carte dans un onglet</font>"), style="color:#FFFFFF; background-color:#DF691A")
        })
        
        output$entrees_qgis_ty <- renderUI({
          actionButton("entrees_qgis_ty_id", label="Exporter en projet Qgis")
        })
        
        output$sortie_qgis_ty <- renderUI({
          tags$div(class="input-group",
                   HTML('<input type="text" id="sortie_qgis_ty_id" class="form-control" placeholder="Nom du projet" aria-describedby="sortie_qgis_ty_id">
                        <span class="input-group-addon" id="sortie_qgis_ty_id">.qgs</span>'))
        })
        
        output$titre1_qgis_ty <- renderUI({
          textInput("titre1_qgis_ty_id", label = h5("Titre informatif"), value = "", placeholder= "Facultatif")
        })
        
        output$titre2_qgis_ty <- renderUI({
          textInput("titre2_qgis_ty_id", label = h5("Titre descriptif"), value = "", placeholder= "Facultatif")
        })
        
        output$source_qgis_ty <- renderUI({
          textInput("source_qgis_ty_id", label = h5("Source de la carte"), value = sourc)
        })
        
        output$aide_image_ty <- renderUI({
          tags$div(class="dropup",
                   HTML(paste0('<button class="btn btn-primary dropdown-toggle" type="button" data-toggle="dropdown">
                               <i class="fa fa-book fa-fw" aria-hidden="true"></i>
                               Proc','\u00e9','dure pour capture d\'','\u00e9','cran
                               <span class="caret"></span>
                               </button>')),
                   tags$ul(class="dropdown-menu",
                           wellPanel(
                             style="background: #2B3E50; width:340px",
                             div(
                               HTML("<font size=2>Deux possibilit\u00e9s :</font>"),
                               br(),
                               br(),
                               strong(HTML("<font size=3>Par l'Outil Capture</font>")),
                               br(),
                               HTML("<font size=2>1- Ouvrir le logiciel Outil Capture sur le Poste de travail (Menu D\u00e9marrer > Tous les programmes > Accessoires > Outil Capture).</font>"),
                               br(),
                               HTML(paste0("<font size=2>2- S\u00e9lectionner la zone \u00e0 capturer (vous aurez besoin d'adapter la fen\u00eatre AUS pour avoir \u00e0 la fois la carte et la barre des t","\u00e2","ches).</font>")),
                               br(),
                               HTML("<font size=2>3- Enregistrer l'image ou copier la dans le presse-papier.</font>"),
                               br(),
                               br(),
                               strong(HTML(paste0("<font size=3>Par impression d'","\u00e9","cran</font>"))),
                               br(),
                               HTML("<font size=2>1- Appuyer sur la touche clavier \"Impr ecran\".</font>"),
                               br(),
                               HTML("<font size=2>2- Ouvrir un logiciel de retouche image (Paint par exemple).</font>"),
                               br(),
                               HTML("<font size=2>3- Coller l'image et l'enregistrer au format voulu (.jpg, .png, .bmp).</font>")
                             )
                           )
                   )
                   )
        })
        })
      
      # Pour modifier l'ordre des fonds
      observeEvent(list(input$monter_fond_ty_id,input$descendre_fond_ty_id),{
        
        if(as.numeric(input$monter_fond_ty_id)==0 & as.numeric(input$descendre_fond_ty_id)==0) return(NULL)
        ordre <- c()
        if(as.numeric(input$monter_fond_ty_id)>nb_up$a)
        {
          ordre <- 2
          nb_up$a <- nb_up$a+1
        }
        if(as.numeric(input$descendre_fond_ty_id)>nb_down$a)
        {
          ordre <- 1
          nb_down$a <- nb_down$a+1
        }
        
        if(is.null(input$ordre_fonds_ty_id)) pos_select <- 0 else pos_select <- which(liste_fonds_ty$a==input$ordre_fonds_ty_id)
        
        if(pos_select>0)
        {
          if(pos_select==ordre) liste_fonds_ty$a <- liste_fonds_ty$a[c(2,1)]
          
          updateSelectInput(session, "ordre_fonds_ty_id",
                            choices = liste_fonds_ty$a,
                            selected = input$ordre_fonds_ty_id
          )
        }
      },ignoreInit = TRUE)
      
      # Pour exporter la carte en projet Qgis
      
      output$export_qgis_ty <- renderUI({
        downloadButton("downloadProjetQgis_ty", label="Exporter")
      })
      
      output$downloadProjetQgis_ty <- downloadHandler(contentType = "zip",
                                                      filename = function(){
                                                        paste0(input$sortie_qgis_ty_id,".zip")
                                                      },
                                                      content = function(file){
                                                        files <- EXPORT_PROJET_QGIS_TY(file)
                                                        
                                                        zip(file,files, flags = "-j9X")
                                                      }
      )
      
      # Pour exporter la carte en projet Qgis
      EXPORT_PROJET_QGIS_TY <- function(file)
      {
        showModal(modalDialog(HTML("<i class=\"fa fa-spinner fa-spin fa-2x fa-fw\"></i> <font size=+1>Export du projet Qgis en cours...</font> "), size="m", footer=NULL, style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"))
        
        sortie <- input$sortie_qgis_ty_id
        rep_sortie <- dirname(file)
        files <- c(paste0(rep_sortie,"/",sortie,".qgs"))
        
        nb_typo <- length(unique(analyse_ty()[[1]]$classe))
        
        if(!is.null(lon_lat_ty()[[1]]))
        {
          suppressWarnings(test_affiche_leg <- try(table_typo <- data.frame(classe=c(nb_typo:1),label=unique(as.data.frame(analyse_ty()[[1]])[,varTypo]),couleurs=unique(palette_ty()$col), stringsAsFactors = F),silent=TRUE))
          if(class(test_affiche_leg) %in% "try-error")
          {
            showModal(modalDialog(HTML("<font size=+1><i class=\"fa fa-hand-pointer-o fa-fw\"></i><b>Double-cliquez</b> d'abord sur la carte pour afficher la l\u00e9gende.</font> "), size="m", footer=NULL, easyClose = TRUE, style = "color: #fff; background-color: #DF691A; border-color: #2e6da4")) #337ab7
            return(NULL)
          }else
          {
            table_typo <- data.frame(classe=c(nb_typo:1),label=unique(as.data.frame(analyse_ty()[[1]])[,varTypo]),couleurs=unique(palette_ty()$col), stringsAsFactors = F)
          }
        }else
        {
          showModal(modalDialog(HTML("<font size=+1><i class=\"fa fa-hand-pointer-o fa-fw\"></i><b>Double-cliquez</b> d'abord sur la carte pour afficher la l\u00e9gende.</font> "), size="m", footer=NULL, easyClose = TRUE, style = "color: #fff; background-color: #DF691A; border-color: #2e6da4")) #337ab7
          return(NULL)
        }
        
        fond_typo <- st_sf(analyse_ty()[[1]])
        
        fond_contour <- st_transform(fondContour,paste0("+init=epsg:",code_epsg_ty()))
        if(!is.null(fondSuppl) && input$ajout_territoire_ty_id) fond_territoire <- st_transform(fond_territoire_ty(),paste0("+init=epsg:",code_epsg_ty()))
        if(input$ajout_dep_ty_id) fond_departement <- st_transform(fond_departement_ty(),paste0("+init=epsg:",code_epsg_ty()))
        if(input$ajout_reg_ty_id) fond_region <- st_transform(fond_region_ty(),paste0("+init=epsg:",code_epsg_ty()))
        fond_france <- st_transform(fond_habillage_ty()[[1]],paste0("+init=epsg:",code_epsg_ty()))
        fond_pays <- st_transform(fond_habillage_ty()[[2]],paste0("+init=epsg:",code_epsg_ty()))
        
        st_write(fond_typo, paste0(rep_sortie,"/fond_maille_typo_carte.shp"), delete_dsn = TRUE, quiet = TRUE)
        st_write(fond_contour,paste0(rep_sortie,"/fond_contour.shp"), delete_dsn = TRUE, quiet = TRUE)
        if(exists("fond_territoire")) if(!is.null(fond_territoire)) st_write(fond_territoire, paste0(rep_sortie,"/fond_territoire.shp"), delete_dsn = TRUE, quiet = TRUE)
        if(exists("fond_departement")) if(!is.null(fond_departement)) st_write(fond_departement, paste0(rep_sortie,"/fond_departement.shp"), delete_dsn = TRUE, quiet = TRUE)
        if(exists("fond_region")) if(!is.null(fond_region)) st_write(fond_region,paste0(rep_sortie,"/fond_region.shp"), delete_dsn = TRUE, quiet = TRUE)
        st_write(fond_france,paste0(rep_sortie,"/fond_france.shp"), delete_dsn = TRUE, quiet = TRUE)
        if(exists("fond_pays")) if(!is.null(fond_pays)) st_write(fond_pays,paste0(rep_sortie,"/fond_pays.shp"), delete_dsn = TRUE, quiet = TRUE)
        
        files <- c(paste0(rep_sortie,"/fond_maille_typo_carte.shp"),paste0(rep_sortie,"/fond_maille_typo_carte.dbf"),paste0(rep_sortie,"/fond_maille_typo_carte.prj"),paste0(rep_sortie,"/fond_maille_typo_carte.shx"),files)
        files <- c(paste0(rep_sortie,"/fond_contour.shp"),paste0(rep_sortie,"/fond_contour.dbf"),paste0(rep_sortie,"/fond_contour.prj"),paste0(rep_sortie,"/fond_contour.shx"),files)
        if(exists("fond_territoire")) if(!is.null(fond_territoire)) files <- c(paste0(rep_sortie,"/fond_territoire.shp"),paste0(rep_sortie,"/fond_territoire.dbf"),paste0(rep_sortie,"/fond_territoire.prj"),paste0(rep_sortie,"/fond_territoire.shx"),files)
        if(exists("fond_departement")) if(!is.null(fond_departement)) files <- c(paste0(rep_sortie,"/fond_departement.shp"),paste0(rep_sortie,"/fond_departement.dbf"),paste0(rep_sortie,"/fond_departement.prj"),paste0(rep_sortie,"/fond_departement.shx"),files)
        if(exists("fond_region")) if(!is.null(fond_region)) files <- c(paste0(rep_sortie,"/fond_region.shp"),paste0(rep_sortie,"/fond_region.dbf"),paste0(rep_sortie,"/fond_region.prj"),paste0(rep_sortie,"/fond_region.shx"),files)
        files <- c(paste0(rep_sortie,"/fond_france.shp"),paste0(rep_sortie,"/fond_france.dbf"),paste0(rep_sortie,"/fond_france.prj"),paste0(rep_sortie,"/fond_france.shx"),files)
        if(exists("fond_pays")) if(!is.null(fond_pays)) files <- c(paste0(rep_sortie,"/fond_pays.shp"),paste0(rep_sortie,"/fond_pays.dbf"),paste0(rep_sortie,"/fond_pays.prj"),paste0(rep_sortie,"/fond_pays.shx"),files)
        
        chemin_fonds <- rep_sortie
        titre1 <- paste0(input$titre1_qgis_ty_id,"\n")
        titre2 <- input$titre2_qgis_ty_id
        source <- input$source_qgis_ty_id
        annee <- format(Sys.time(), format = "%Y")
        variable_a_representer <- varTypo
        titre_leg_typo <- input$titre_typo_legende_ty_id
        
        l <- c()
        if(exists("fond_territoire")) l <- "fond_territoire"
        if(exists("fond_departement")) l <- c(l,"fond_departement")
        if(exists("fond_region")) l <- c(l,"fond_region")
        
        l=c("fond_france",
            "fond_contour",
            l,
            "fond_maille_typo_carte"
        )
        
        if(exists("fond_pays")) l <- c(l,"fond_pays")
        
        export_projet_qgis_typo(l,rep_sortie,sortie,titre1,titre2,source,titre_leg_typo,table_typo,variable_a_representer,annee)
        
        removeModal()
        
        showModal(modalDialog(HTML(paste0("<font size=+1>Le projet Qgis a \u00e9t\u00e9 cr","\u00e9","ee.</font>")), size="m", footer=NULL, easyClose = TRUE, style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"))
        
        return(files)
      }
      
      code_epsg_ty <- reactive({
        code_epsg <- switch(dom, #DOM
                            "0"="2154",# Lambert 93
                            "971"="32620",# UTM 20 N
                            "972"="32620",# UTM 20 N
                            "973"="2972",# UTM 22 N
                            "974"="2975",# UTM 40 S
                            "976"="4471")# UTM 38 S
        return(code_epsg)
      })
      
      analyse_ty <- reactive({
        
        analyse<-k_typo(fondMaille,names(fondMaille)[!sapply(fondMaille[-length(names(fondMaille))],is.numeric)][1],data,"CODE",varTypo)
        
        if(is.null(analyse))
        {
          showModal(modalDialog(HTML(paste0("<font size=+1>La maille ne correspond pas au niveau g\u00e9ographique du fichier de donn","\u00e9","es.<br><br>Veuillez svp choisir une maille adapt","\u00e9","e ou modifier le fichier de donn","\u00e9","es.</font>")), size="l", footer=NULL, easyClose = TRUE, style = "color: #fff; background-color: #DF691A; border-color: #2e6da4")) #337ab7
          erreur_maille$a <- TRUE
          return(NULL)
        }
        
        analyse <- analyse[[1]]
        analyse[,"TXT1"] <- paste0("<b> <font color=#2B3E50>",format(as.data.frame(analyse)[,varTypo], big.mark=" ",decimal.mark=",",nsmall=0),"</font></b>")
        analyse_WGS84 <- st_transform(analyse,"+init=epsg:4326 +proj=longlat +ellps=WGS84")
        return(list(analyse,analyse_WGS84))
      })
      
      fond_habillage_ty <- reactive({
        
        if(dom=="0")
        {
          pays <- st_transform(sf_paysm(),"+init=epsg:4326 +proj=longlat +ellps=WGS84")
          fra <- st_transform(sf_fram(),"+init=epsg:4326 +proj=longlat +ellps=WGS84")
        }else
        {
          if(dom=="971")
          {
            fra <- st_transform(sf_reg01(),"+init=epsg:4326 +proj=longlat +ellps=WGS84")
            pays <- fra
          }
          if(dom=="972")
          {
            fra <- st_transform(sf_reg02(),"+init=epsg:4326 +proj=longlat +ellps=WGS84")
            pays <- fra
          }
          if(dom=="973")
          {
            fra <- st_transform(sf_reg03(),"+init=epsg:4326 +proj=longlat +ellps=WGS84")
            pays <- st_transform(sf_pays973(),"+init=epsg:4326 +proj=longlat +ellps=WGS84")
          }
          if(dom=="974")
          {
            fra <- st_transform(sf_reg04(),"+init=epsg:4326 +proj=longlat +ellps=WGS84")
            pays <- fra
          }
          if(dom=="976")
          {
            fra <- st_transform(sf_reg06(),"+init=epsg:4326 +proj=longlat +ellps=WGS84")
            pays <- fra
          }
        }
        fond_france <- fra
        fond_pays <- pays
        
        return(list(fond_france,fond_pays))
      })
      
      fond_contour_maille_ty <- reactive({
        
        test_contour <- try(st_transform(fondContour,"+init=epsg:4326 +proj=longlat +ellps=WGS84"), silent = TRUE)
        test_maille <- try(st_transform(fondMaille,"+init=epsg:4326 +proj=longlat +ellps=WGS84"), silent = TRUE)
        
        if(any(list(class(test_contour),class(test_maille)) %in% "try-error"))
        {
          showModal(modalDialog(HTML(paste0("<font size=+1>Une erreur est survenue dans la cr","\u00e9","ation du territoire.<br><br>Veuillez svp v\u00e9rifier vos donn","\u00e9","es et les variables choisies.</font>")), size="m", footer=NULL, easyClose = TRUE, style = "color: #fff; background-color: #DF691A; border-color: #2e6da4"))
          erreur_maille$a <- TRUE
          return(NULL)
        }else
        {
          contour_WGS84 <- st_transform(fondContour,"+init=epsg:4326 +proj=longlat +ellps=WGS84")
          maille_WGS84 <- st_transform(fondMaille,"+init=epsg:4326 +proj=longlat +ellps=WGS84")
        }
        
        return(list(contour_WGS84,maille_WGS84))
      })
      
      list_bbox_ty <- reactive({
        req(fond_contour_maille_ty())
        
        list_bbox <- list(c(st_bbox(fond_contour_maille_ty()[[1]])[1],st_bbox(fond_contour_maille_ty()[[1]])[3]),c(st_bbox(fond_contour_maille_ty()[[1]])[2],st_bbox(fond_contour_maille_ty()[[1]])[4]))
        return(list_bbox)
      })
      
      palette_ty <- reactive({
        nb_col <- length(unique(as.data.frame(analyse_ty()[[1]])[,"classe"]))
        pal_typo <- substr(rainbow(256)[nb_opposes(256)[1:nb_col]],1,7)
        pal_typo <- data.frame(cbind(pal_typo,unique(as.data.frame(analyse_ty()[[1]])[,"classe"])))
        names(pal_typo) <- c("col","classe")
        analyse <- merge(as.data.frame(analyse_ty()[[1]]),pal_typo,by="classe")
        analyse <- analyse[order(as.data.frame(analyse)[,varTypo]),]
        return(analyse)
      })
      
      palette_init_ty <- reactive({
        nb_col <- length(unique(as.data.frame(analyse_ty()[[1]])[,"classe"]))
        pal_typo <- substr(rainbow(256)[nb_opposes(256)[1:nb_col]],1,7)
        pal_typo <- data.frame(cbind(pal_typo,unique(as.data.frame(analyse_ty()[[1]])[,"classe"])))
        names(pal_typo) <- c("col","classe")
        analyse <- merge(as.data.frame(analyse_ty()[[1]]),pal_typo,by="classe")
        analyse <- analyse[order(as.data.frame(analyse)[,varTypo]),]
        return(analyse)
      })
      
      fond_territoire_ty <- reactive({
        if(!is.null(fondSuppl))
        {
          fond_territoire <- st_transform(fondSuppl,"+init=epsg:4326 +proj=longlat +ellps=WGS84")
          return(fond_territoire)
        }else
        {
          return(NULL)
        }
      })
      
      fond_region_ty <- reactive({
        fond_region <- st_transform(sf_regm(),"+init=epsg:4326 +proj=longlat +ellps=WGS84")
        return(fond_region)
      })
      
      fond_departement_ty <- reactive({
        fond_departement <- st_transform(sf_depm(),"+init=epsg:4326 +proj=longlat +ellps=WGS84")
        return(fond_departement)
      })
      
      fond_select_donnees_ty <- reactive({
        req(analyse_ty())
        
        fond_donnees <- analyse_ty()[[1]][input$mydonnees_ty_rows_selected,]
        fond_donnees <- st_transform(fond_donnees,"+init=epsg:4326 +proj=longlat +ellps=WGS84")
        return(fond_donnees)
      })
      
      fond_select_contour_ty <- reactive({
        req(fond_contour_maille_ty())
        
        fond_contour <- fond_contour_maille_ty()[[1]][as.data.frame(fond_contour_maille_ty()[[1]])[,"CODE"] %in% as.data.frame(fondContour)[input$mycontour_ty_rows_selected,"CODE"],]
        return(fond_contour)
      })
      
      # CONSTRUCTION DE LA MAP EN LEAFLET
      
      react_fond_ty <- reactive({
        
        if(input$menu=="carte")
        {
          showModal(modalDialog(HTML("<i class=\"fa fa-spinner fa-spin fa-2x fa-fw\"></i><font size=+1>\u00c9laboration de la carte...</font> "), size="m", footer=NULL, style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"))
          
          # Construction de la map par defaut
          
          m <- leaflet(padding = 0,
                       options = leafletOptions(
                         preferCanvas = TRUE,
                         transition = 2,
                         minZoom = 6,
                         maxZoom = 10
                       )) %>%
            
            setMapWidgetStyle(list(background = "#AFC9E0")) %>%
            
            addTiles_insee(attribution = paste0("<a href=\"http://www.insee.fr\">\u00A9 IGN - INSEE ",format(Sys.time(), format = "%Y"),"</a>")) %>%
            
            fitBounds(lng1 = min(list_bbox_ty()[[1]]),
                      lat1 = min(list_bbox_ty()[[2]]),
                      lng2 = max(list_bbox_ty()[[1]]),
                      lat2 = max(list_bbox_ty()[[2]])
            ) %>%
            
            # On ajoute une barre d'echelle
            addScaleBar(position = 'bottomright',
                        options = scaleBarOptions(metric = TRUE, imperial = FALSE)
            ) %>%
            
            # Pour gerer l'ordre des calques
            addMapPane(name = "fond_pays", zIndex = 401) %>%
            addMapPane(name = "fond_france", zIndex = 402) %>%
            addMapPane(name = "fond_habillage", zIndex = 403) %>%
            addMapPane(name = "fond_dep", zIndex = 404) %>%
            addMapPane(name = "fond_reg", zIndex = 405) %>%
            addMapPane(name = "fond_territoire", zIndex = 406) %>%
            addMapPane(name = "fond_duo2", zIndex = 407) %>%
            addMapPane(name = "fond_duo1", zIndex = 408) %>%
            addMapPane(name = "selection", zIndex = 409) %>%
            
            addMapPane(name = "fond_legende", zIndex = 410)
          
          # AFFICHAGE DES FONDS D'HABILLAGE
          
          if(dom %in% c("0","973"))
          {
            # fond des pays
            m <- addPolygons(map = m, data = fond_habillage_ty()[[2]][,"LIBGEO"], opacity = 1, # sauf la France
                             stroke = TRUE, color = "white",
                             weight = 1,
                             options = pathOptions(pane = "fond_pays", clickable = F),
                             fill = T, fillColor = "#CCCCCC", fillOpacity = 1
            )
          }
          
          # fond de la France metro ou d'un DOM
          m <- addPolygons(map = m, data = fond_habillage_ty()[[1]][,"LIBGEO"], opacity = 1,
                           stroke = TRUE, color = "black",
                           weight = 1.5,
                           options = pathOptions(pane = "fond_france", clickable = F),
                           fill = T, fillColor = "white", fillOpacity = 1
          )
          
          m_save_ty$a <- m
          
          # AFFICHAGE DU FOND TERRITOIRE
          
          if(!is.null(fondSuppl))
          {
            m <- addPolygons(map = m, data = fond_territoire_ty(),
                             stroke = TRUE, color = "#BFBFBF", opacity = 1,
                             weight = 0.5,
                             options = pathOptions(pane = "fond_territoire", clickable = T),
                             popup = paste0("<b> <font color=#2B3E50>",as.data.frame(fond_territoire_ty())[,"LIBELLE"], "</font> </b>"),
                             fill = T, fillColor = "white", fillOpacity = 0.001,
                             group = "territoire"
            )
          }
          # AFFICHAGE DU FOND CONTOUR
          m <- addPolygons(map = m, data = fond_contour_maille_ty()[[1]], opacity = 0.3,
                           stroke = TRUE, color = "black", weight = 3,
                           options = pathOptions(pane = "fond_duo2", clickable = T),
                           popup = paste0("<b> <font color=#2B3E50>",as.data.frame(fond_contour_maille_ty()[[1]])[,"LIBELLE"], "</font> </b>"),
                           fill = T, fillColor = "white", fillOpacity = 0.3,
                           group = "duo"
          )
          
          # AFFICHAGE DE LA MAILLE ET DE L'ANALYSE
          
          m <- addPolygons(map = m, data = analyse_ty()[[2]], opacity = 1,
                           stroke = TRUE, color = "white", weight = 1,
                           options = pathOptions(pane = "fond_duo1", clickable = T),
                           popup = paste0("<b> <font color=#2B3E50>",as.data.frame(analyse_ty()[[2]])[,"LIBELLE"], "</font> </b><br><b><font color=#2B3E50>",varTypo," : </font></b>",analyse_ty()[[2]]$TXT1,"<br>"),
                           fill = T,
                           fillColor = palette_init_ty()$col,
                           fillOpacity = 1,
                           group = "duo"
          )
          
          removeModal()
          
          showModal(modalDialog(HTML("<font size=+1>Veuillez patientez svp, la carte va s'afficher dans quelques secondes...<br><br><i class=\"fa fa-hand-pointer-o fa-fw\"></i><b>Double-cliquez</b> ensuite sur la carte pour afficher la l\u00e9gende.</font> "), size="m", footer=NULL, easyClose = TRUE, style = "color: #fff; background-color: #DF691A; border-color: #2e6da4")) #337ab7
          
          return(m)
        }
      })
      
      # MODIFICATION DES FONDS TERRITOIRE D'ETUDES, REGION ET DEPARTEMENT
      
      observeEvent(input$ajout_territoire_ty_id,{
        
        proxy <- leafletProxy("mymap_ty")
        
        clearGroup(map = proxy, group = "territoire")
        
        #fond du territoire d'etudes
        if(!is.null(fondSuppl))
        {
          if(input$ajout_territoire_ty_id)
          {
            proxy <- addPolygons(map = proxy, data = fond_territoire_ty(),
                                 stroke = TRUE, color = "#BFBFBF", opacity = 1,
                                 weight = 0.5,
                                 options = pathOptions(pane = "fond_territoire", clickable = T),
                                 popup = paste0("<b> <font color=#2B3E50>",as.data.frame(fond_territoire_ty())[,"LIBELLE"], "</font> </b>"),
                                 fill = T, fillColor = "white", fillOpacity = 0.001,
                                 group = "territoire"
            )
          }
        }
      },ignoreInit = TRUE)
      
      observeEvent(input$ajout_reg_ty_id,{
        
        proxy <- leafletProxy("mymap_ty")
        
        clearGroup(map = proxy, group = "region")
        
        if(dom=="0")
        {
          if(input$ajout_reg_ty_id)
          {
            #fond des regions
            proxy <- addPolygons(map = proxy, data = fond_region_ty(),
                                 stroke = TRUE, color = "grey", opacity = 1,
                                 weight = 1.5,
                                 options = pathOptions(pane = "fond_reg", clickable = F),
                                 fill = F,
                                 group = "region"
            )
          }
        }
      },ignoreInit = TRUE)
      
      observeEvent(input$ajout_dep_ty_id,{
        
        proxy <- leafletProxy("mymap_ty")
        
        clearGroup(map = proxy, group = "departement")
        
        if(dom=="0")
        {
          if(input$ajout_dep_ty_id)
          {
            #fond des departements
            proxy <- addPolygons(map = proxy, data = fond_departement_ty(),
                                 stroke = TRUE, color = "grey", opacity = 1,
                                 weight = 0.5,
                                 options = pathOptions(pane = "fond_dep", clickable = F),
                                 fill = F,
                                 group = "departement"
            )
          }
        }
      },ignoreInit = TRUE)
      
      # MODIFICATION DE L'ORDRE DES CALQUES
      
      observeEvent(list(input$monter_fond_ty_id,input$descendre_fond_ty_id),{
        
        if(as.numeric(input$monter_fond_ty_id)==0 & as.numeric(input$descendre_fond_ty_id)==0) return(NULL)
        
        proxy <- leafletProxy("mymap_ty")
        
        clearGroup(map = proxy, group = "contour")
        clearGroup(map = proxy, group = "classe")
        
        i <- 1 #pour gerer l'ordre des fonds dans le pane
        
        for(fond in liste_fonds_ty$a)
        {
          if(fond=="analyse/maille")
          {
            proxy <- addPolygons(map = proxy, data = analyse_ty()[[2]], opacity = 1,
                                 stroke = TRUE, color = "white", weight = 1,
                                 options = pathOptions(pane = paste0("fond_duo",i), clickable = T),
                                 popup = paste0("<b> <font color=#2B3E50>",as.data.frame(analyse_ty()[[2]])[,"LIBELLE"], "</font> </b><br><b><font color=#2B3E50>",varTypo," : </font></b>",analyse_ty()[[2]]$TXT1,"<br>"),
                                 fill = T,
                                 fillColor = palette_ty()$col,
                                 fillOpacity = 1,
                                 group = "classe"
            )
            
            ordre_analyse$a <- i
          }
          
          if(fond=="contour")
          {
            proxy <- addPolygons(map = proxy, data = fond_contour_maille_ty()[[1]], opacity = 0.3,
                                 stroke = TRUE, color = "black", weight = 3,
                                 options = pathOptions(pane = paste0("fond_duo",i), clickable = T),
                                 popup = paste0("<b> <font color=#2B3E50>",as.data.frame(fond_contour_maille_ty()[[1]])[,"LIBELLE"], "</font> </b>"),
                                 fill = T, fillColor = "white", fillOpacity = 0.3,
                                 group = "contour"
            )
          }
          
          i <- i + 1
        }
      },ignoreInit = TRUE)
      
      # MODIFICATION DE LA SELECTION DES OBJETS VIA LES TABLEAUX
      
      observeEvent(input$onglets_ty,{
        
        if(input$onglets_ty == "carte")
        {
          proxy <- leafletProxy("mymap_ty")
          
          clearGroup(map = proxy, group = "select_donnees")
          
          if(!is.null(input$mydonnees_ty_rows_selected))
          {
            proxy <- addPolygons(map = proxy, data = fond_select_donnees_ty(),
                                 stroke = TRUE, weight = 3,
                                 color="black", opacity = 1,
                                 options = pathOptions(pane = "selection", clickable = F),
                                 fill = F,
                                 group = "select_donnees"
            )
          }
        }
      },ignoreInit = TRUE)
      
      observeEvent(input$onglets_ty,{
        
        if(input$onglets_ty == "carte")
        {
          proxy <- leafletProxy("mymap_ty")
          
          clearGroup(map = proxy, group = "select_contour")
          
          if(!is.null(input$mycontour_ty_rows_selected))
          {
            proxy <- addPolygons(map = proxy, data = fond_select_contour_ty(),
                                 stroke = TRUE, weight = 3,
                                 color="#FFFF00",
                                 options = pathOptions(pane = "selection", clickable = F),
                                 fill = F,
                                 group = "select_contour"
            )
          }
        }
      },ignoreInit = TRUE)
      
      # CONSTRUCTION DE LA LEGENDE
      
      lon_lat_ty <- reactive({
        click <- input$mymap_ty_click
        lon <- click$lng
        lat <- click$lat
        return(list(lon,lat))
      })
      
      observeEvent(list(input$mymap_ty_zoom,input$mymap_ty_click,input$titre_typo_legende_ty_id),{
        if(is.null(input$affiche_legende_ty_id)) return(NULL)
        
        if(input$affiche_legende_ty_id==FALSE) return(NULL)
        
        if(is.null(lon_lat_ty()[[1]])) return(NULL)
        
        CONSTRUCTION_LEGENDE_TY()
      })
      
      CONSTRUCTION_LEGENDE_TY <- function(lon,lat)
      {
        proxy <- leafletProxy("mymap_ty")
        proxy <- clearGroup(map=proxy, group="leg")
        proxy <- clearMarkers(map=proxy)
        
        zoom <- as.numeric(input$mymap_ty_zoom)
        coeff <- ((360/(2^zoom))/7.2) # Permet de fixer une distance sur l'ecran. Il s'agit en gros d'une conversion des degres en pixels. Reste constant a longitude egale mais varie un peu selon la latitude
        
        position_leg <- t(data_frame(c(lon_lat_ty()[[1]],lon_lat_ty()[[2]])))
        
        # On cree les rectangles
        nb_typo <- length(unique(analyse_ty()[[1]]$classe))
        
        for(i in 1:nb_typo)
        {
          # Coordonnees du point haut/gauche des rectangles de la legende
          x_coord_rectangle <- position_leg[1]
          if(i==1) #1er rectangle
          {
            y_coord_rectangle <- position_leg[2]-coeff
          }else
          {
            y_coord_rectangle <- y_coord_rectangle-coeff*0.7
          }
          assign(paste0("rectangle_",i),list(matrix(c(x_coord_rectangle,y_coord_rectangle,x_coord_rectangle+coeff*1,y_coord_rectangle,x_coord_rectangle+coeff*1,y_coord_rectangle+coeff*0.5,x_coord_rectangle,y_coord_rectangle+coeff*0.5,x_coord_rectangle,y_coord_rectangle),ncol=2, byrow=TRUE)))
        }
        
        # On ajoute un cadre blanc autour de la legende
        y_coord_rectangle <- min(get(paste0("rectangle_",nb_typo))[[1]][,2])
        
        # leaflet du cadre blanc en 1er
        proxy <- addRectangles(map = proxy,
                               lng1 = position_leg[1]-coeff*0.5, lat1 = position_leg[2]+coeff*0.5, #x_titre_1 et y_titre_2
                               lng2 = x_coord_rectangle+coeff*6, lat2 = y_coord_rectangle-coeff*0.8,
                               stroke = TRUE,
                               color = paste0("#2B3E50", ";background: #ffffff;
                                              border-left:2px solid #2B3E50;
                                              border-right:2px solid #2B3E50;
                                              border-top:2px solid #2B3E50;
                                              border-bottom:2px solid #2B3E50;
                                              border-radius: 5%"),
                               weight = 1,
                               options = pathOptions(pane = "fond_legende", clickable = F),
                               fill = T,
                               fillColor = "white",
                               fillOpacity = 0.8,
                               group="leg"
                               )
        
        palette <- unique(palette_ty()[order(palette_ty()$valeur),"col"])
        
        for(i in 1: nb_typo)
        {
          if(is.null(input$lib_typo_1_ty_id))
          {
            typo_leg_texte <- sort(unique(analyse_ty()[[1]]$valeur))[i]
          }else
          {
            typo_leg_texte <- input[[paste0("lib_typo_", i,"_ty_id")]]
          }
          
          proxy <- addPolygons(map = proxy, data = st_polygon(get(paste0("rectangle_",i))),
                               stroke = FALSE,
                               options = pathOptions(pane = "fond_legende", clickable = F),
                               fill = T,
                               fillColor = palette[i],
                               fillOpacity = 1,
                               group="leg"
          )
          
          proxy <- addLabelOnlyMarkers(map = proxy,
                                       lng = (max(get(paste0("rectangle_",i))[[1]][,1])+coeff*0.1), lat = mean(get(paste0("rectangle_",i))[[1]][,2]),
                                       label = typo_leg_texte,
                                       labelOptions = labelOptions(noHide = T, textOnly = TRUE, direction = "right",
                                                                   style = list(
                                                                     "color" = "black",
                                                                     "font-size" = "12px"
                                                                   )),
                                       group="leg"
          )
        }
        
        # leaflet titre
        x_titre_2 <- min(st_coordinates(st_polygon(get("rectangle_1")))[,"X"])
        y_titre_2 <- max(st_coordinates(st_polygon(get("rectangle_1")))[,"Y"])+coeff*0.2
        
        proxy <- addLabelOnlyMarkers(map = proxy,
                                     lng = x_titre_2, lat = y_titre_2,
                                     label = input$titre_typo_legende_ty_id,
                                     labelOptions = labelOptions(noHide = T, textOnly = TRUE, direction = "right",
                                                                 style = list(
                                                                   "color" = "black",
                                                                   "font-size" = "14px"
                                                                 )),
                                     group="leg"
        )
      }
      
      # AJOUT DES ONGLETS SAUVEGARDE
      
      observeEvent(input$save_carte_ty_id,{
        
        showModal(modalDialog(HTML("<i class=\"fa fa-spinner fa-spin fa-2x fa-fw\"></i><font size=+1>Sauvegarde de la carte en cours...</font> "), size="m", footer=NULL, style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"))
        
        insert_save$a <- insert_save$a + 1
        nb_save_carte <- insert_save$a-remove_carte$a
        
        m_save <- m_save_ty$a
        
        if(nb_save_carte>6)
        {
          insert_save$a <- insert_save$a - 1
          showModal(modalDialog(HTML("<font size=+1>Vous ne pouvez pas sauvegarger plus de 6 cartes. Veuillez en supprimer avant de continuer.</font> "), size="l", footer=NULL, easyClose = TRUE, style = "color: #fff; background-color: #DF691A; border-color: #2e6da4")) #337ab7
          return(NULL)
        }
        
        output[[paste0("mymap_save_",insert_save$a,"_ty")]] <- renderLeaflet({
          
          if(!is.null(fondSuppl))
          {
            if(input$ajout_territoire_ty_id)
            {
              m_save <- addPolygons(map = m_save, data = fond_territoire_ty(),
                                    stroke = TRUE, color = "#BFBFBF", opacity = 1,
                                    weight = 0.5,
                                    options = pathOptions(pane = "fond_territoire", clickable = T),
                                    popup = paste0("<b> <font color=#2B3E50>",as.data.frame(fond_territoire_ty())[,"LIBELLE"], "</font> </b>"),
                                    fill = T, fillColor = "white", fillOpacity = 0.001
              )
            }
          }
          
          if(input$ajout_reg_ty_id)
          {
            m_save <- addPolygons(map = m_save, data = fond_region_ty(),
                                  stroke = TRUE, color = "grey", opacity = 1,
                                  weight = 1.5,
                                  options = pathOptions(pane = "fond_reg", clickable = F),
                                  fill = F
            )
          }
          
          if(input$ajout_dep_ty_id)
          {
            m_save <- addPolygons(map = m_save, data = fond_departement_ty(),
                                  stroke = TRUE, color = "grey", opacity = 1,
                                  weight = 0.5,
                                  options = pathOptions(pane = "fond_dep", clickable = F),
                                  fill = F
            )
          }
          
          i <- 1 #pour gerer l'ordre des fonds dans le pane
          
          for(fond in liste_fonds_ty$a)
          {
            if(fond=="analyse/maille")
            {
              m_save <- addPolygons(map = m_save, data = analyse_ty()[[2]], opacity = 1,
                                    stroke = TRUE, color = "white", weight = 1,
                                    options = pathOptions(pane = paste0("fond_duo",i), clickable = T),
                                    popup = paste0("<b> <font color=#2B3E50>",as.data.frame(analyse_ty()[[2]])[,"LIBELLE"], "</font> </b><br><b><font color=#2B3E50>",varTypo," : </font></b>",analyse_ty()[[2]]$TXT1,"<br>"),
                                    fill = T,
                                    fillColor = palette_ty()$col,
                                    fillOpacity = 1
              )
            }
            
            if(fond=="contour")
            {
              m_save <- addPolygons(map = m_save, data = fond_select_contour_ty(),
                                    stroke = TRUE, weight = 5,
                                    color="black", opacity = 1,
                                    options = pathOptions(pane = paste0("fond_duo",i), clickable = T),
                                    fill = F
              )
            }
            
            i <- i + 1
          }
          
          zoom <- as.numeric(input$mymap_ty_zoom)
          coeff <- ((360/(2^zoom))/7.2) # Permet de fixer une distance sur l'ecran. Il s'agit en gros d'une conversion des degres en pixels. Reste constant a longitude egale mais varie un peu selon la latitude
          
          position_leg <- t(data_frame(c(lon_lat_ty()[[1]],lon_lat_ty()[[2]])))
          
          # On cree les rectangles
          nb_typo <- length(unique(analyse_ty()[[1]]$classe))
          
          for(i in 1:nb_typo)
          {
            # Coordonnees du point haut/gauche des rectangles de la legende
            x_coord_rectangle <- position_leg[1]
            if(i==1) #1er rectangle
            {
              y_coord_rectangle <- position_leg[2]-coeff
            }else
            {
              y_coord_rectangle <- y_coord_rectangle-coeff*0.7
            }
            assign(paste0("rectangle_",i),list(matrix(c(x_coord_rectangle,y_coord_rectangle,x_coord_rectangle+coeff*1,y_coord_rectangle,x_coord_rectangle+coeff*1,y_coord_rectangle+coeff*0.5,x_coord_rectangle,y_coord_rectangle+coeff*0.5,x_coord_rectangle,y_coord_rectangle),ncol=2, byrow=TRUE)))
          }
          
          # On ajoute un cadre blanc autour de la legende
          y_coord_rectangle <- min(get(paste0("rectangle_",nb_typo))[[1]][,2])
          
          # leaflet du cadre blanc en 1er
          m_save <- addRectangles(map = m_save,
                                  lng1 = position_leg[1]-coeff*0.5, lat1 = position_leg[2]+coeff*0.5, #x_titre_1 et y_titre_2
                                  lng2 = x_coord_rectangle+coeff*6, lat2 = y_coord_rectangle-coeff*0.8,
                                  stroke = TRUE,
                                  color = paste0("#2B3E50", ";background: #ffffff;
                                                 border-left:2px solid #2B3E50;
                                                 border-right:2px solid #2B3E50;
                                                 border-top:2px solid #2B3E50;
                                                 border-bottom:2px solid #2B3E50;
                                                 border-radius: 5%"),
                                  weight = 1,
                                  options = pathOptions(pane = "fond_legende", clickable = F),
                                  fill = T,
                                  fillColor = "white",
                                  fillOpacity = 0.8
                                  )
          
          palette <- unique(palette_ty()[order(palette_ty()$valeur),"col"])
          
          for(i in 1: nb_typo)
          {
            if(is.null(input$lib_typo_1_ty_id))
            {
              typo_leg_texte <- sort(unique(analyse_ty()[[1]]$valeur))[i]
            }else
            {
              typo_leg_texte <- input[[paste0("lib_typo_", i,"_ty_id")]]
            }
            
            m_save <- addPolygons(map = m_save, data = st_polygon(get(paste0("rectangle_",i))),
                                  stroke = FALSE,
                                  options = pathOptions(pane = "fond_legende", clickable = F),
                                  fill = T,
                                  fillColor = palette[i],
                                  fillOpacity = 1
            )
            
            m_save <- addLabelOnlyMarkers(map = m_save,
                                          lng = (max(get(paste0("rectangle_",i))[[1]][,1])+coeff*0.1), lat = mean(get(paste0("rectangle_",i))[[1]][,2]),
                                          label = typo_leg_texte,
                                          labelOptions = labelOptions(noHide = T, textOnly = TRUE, direction = "right",
                                                                      style = list(
                                                                        "color" = "black",
                                                                        "font-size" = "12px"
                                                                      ))
            )
          }
          
          # leaflet titre
          x_titre_2 <- min(st_coordinates(st_polygon(get("rectangle_1")))[,"X"])
          y_titre_2 <- max(st_coordinates(st_polygon(get("rectangle_1")))[,"Y"])+coeff*0.2
          
          m_save <- addLabelOnlyMarkers(map = m_save,
                                        lng = x_titre_2, lat = y_titre_2,
                                        label = input$titre_typo_legende_ty_id,
                                        labelOptions = labelOptions(noHide = T, textOnly = TRUE, direction = "right",
                                                                    style = list(
                                                                      "color" = "black",
                                                                      "font-size" = "14px"
                                                                    ))
          )
          
          removeModal()
          
          m_save
        })
        
        output[[paste0("remove_carte_",nb_save_carte,"_ty")]] <- renderUI({
          actionButton(paste0("remove_carte_",nb_save_carte,"_ty_id"),label="X Supprimer la carte", style="color:#FFFFFF; border-color:#FFFFFF; background-color:#2B3E50")
        })
        appendTab(inputId = "onglets_ty",
                  tabPanel(title=HTML(paste0("<font color=#AFC9E0> Carte ",insert_save$a,"</font>")),value=paste0("carte",nb_save_carte),uiOutput(paste0("remove_carte_",nb_save_carte,"_ty")),leafletOutput(paste0("mymap_save_",insert_save$a,"_ty"),width="100%",height = 800)),
                  select = TRUE,
                  session = session
        )
      }, ignoreInit = TRUE)
      
      observeEvent(input$remove_carte_1_ty_id,{
        remove_carte$a <- remove_carte$a + 1
        removeTab(inputId = "onglets_ty",
                  target = "carte1",
                  session = session
        )
      }, ignoreInit = TRUE)
      
      observeEvent(input$remove_carte_2_ty_id,{
        remove_carte$a <- remove_carte$a + 1
        removeTab(inputId = "onglets_ty",
                  target = "carte2",
                  session = session
        )
      }, ignoreInit = TRUE)
      
      observeEvent(input$remove_carte_3_ty_id,{
        remove_carte$a <- remove_carte$a + 1
        removeTab(inputId = "onglets_ty",
                  target = "carte3",
                  session = session
        )
      }, ignoreInit = TRUE)
      
      observeEvent(input$remove_carte_4_ty_id,{
        remove_carte$a <- remove_carte$a + 1
        removeTab(inputId = "onglets_ty",
                  target = "carte4",
                  session = session
        )
      }, ignoreInit = TRUE)
      
      observeEvent(input$remove_carte_5_ty_id,{
        remove_carte$a <- remove_carte$a + 1
        removeTab(inputId = "onglets_ty",
                  target = "carte5",
                  session = session
        )
      }, ignoreInit = TRUE)
      
      observeEvent(input$remove_carte_6_ty_id,{
        remove_carte$a <- remove_carte$a + 1
        removeTab(inputId = "onglets_ty",
                  target = "carte6",
                  session = session
        )
      }, ignoreInit = TRUE)
      
      # TABLEAUX DE DONNEES, MAILLE ET CONTOUR
      
      output$mydonnees_ty <- DT::renderDataTable(datatable({
        data <- as.data.frame(analyse_ty()[[1]])
        tableau_donnees <- data[,c("CODE","LIBELLE",varTypo)]
      },  style = 'bootstrap'
      ))
      
      output$mycontour_ty <- DT::renderDataTable(datatable({
        data <- as.data.frame(fondContour)
        tableau_contour <- data[,c(1:2)]
      },  style = 'bootstrap'
      ))
      
      # ENVOI DU LEAFLET A L'UI
      
      output$mymap_ty <- renderLeaflet({
        react_fond_ty()
      })
      
    }
    
    runApp(shinyApp(ui = ui, server = server), launch.browser = TRUE)
    }


shiny_oursins <-
  function(data,fondMaille,fondContour,fondSuppl=NULL,idDataDepart,idDataArrivee,varFlux,decalageAllerRetour=0,decalageCentroid=0,dom="0")
  {
    options("stringsAsFactors"=FALSE)
    
    # Verification des parametres
    msg_error1<-msg_error2<-msg_error3<-msg_error4<-msg_error5<-msg_error6<-msg_error7<-msg_error8<-msg_error9<-msg_error10<-msg_error11<-msg_error12<-msg_error13<-msg_error14<-msg_error15<-msg_error16<-msg_error17<-msg_error18 <- NULL
    
    if(any(class(data)!="data.frame")) msg_error1 <- "Les donnees doivent etre dans un data.frame / "
    if(any(!any(class(fondMaille) %in% "sf"),!any(class(fondMaille) %in% "data.frame"))) msg_error2 <- "Le fond de maille doit etre un objet sf / "
    if(any(!any(class(fondContour) %in% "sf"),!any(class(fondContour) %in% "data.frame"))) msg_error3 <- "Le fond de contour doit etre un objet sf / "
    if(!is.null(fondSuppl)) if(any(!any(class(fondSuppl) %in% "sf"),!any(class(fondSuppl) %in% "data.frame"))) msg_error4 <- "Le fond supplementaire doit etre un objet sf / "
    if(any(class(idDataDepart)!="character")) msg_error5 <- "Le nom de la variable de depart doit etre de type caractere / "
    if(any(class(idDataArrivee)!="character")) msg_error6 <- "Le nom de la variable d'arrivee doit etre de type caractere / "
    if(any(class(varFlux)!="character")) msg_error7 <- "Le nom de la variable doit etre de type caractere / "
    if(any(class(decalageAllerRetour)!="numeric")) msg_error8 <- "La variable decalageAllerRetour doit etre de type numerique / "
    if(any(class(decalageCentroid)!="numeric")) msg_error9 <- "La variable decalageCentroid doit etre de type numerique / "
    if(any(class(dom)!="character")) msg_error10 <- "La valeur doit etre de type caractere ('0', '971', '972', '973', '974' ou '976') / "
    
    if(length(names(data))<3) msg_error11 <- "Le tableau des donnees n'est pas conforme. Il doit contenir au minimum une variable identifiant de depart, une variable identifiant d'arrivee et la variable a representer / "
    if(length(names(fondMaille))<3) msg_error12 <- "Le fond de maille n'est pas conforme. La table doit contenir au minimum une variable identifiant, une variable libelle et la geometry / "
    if(length(names(fondContour))<3) msg_error13 <- "Le fond de contour n'est pas conforme. La table doit contenir au minimum une variable identifiant, une variable libelle et la geometry / "
    if(!is.null(fondSuppl)) if(length(names(fondSuppl))<3) msg_error14 <- "Le fond supplementaire n'est pas conforme. La table doit contenir au minimum une variable identifiant, une variable libelle et la geometry / "
    
    if(!any(names(data) %in% idDataDepart))  msg_error15 <- "La variable identifiant de depart n'existe pas dans la table des donnees / "
    if(!any(names(data) %in% idDataArrivee))  msg_error16 <- "La variable identifiant d'arrivee n'existe pas dans la table des donnees / "
    if(!any(names(data) %in% varFlux))  msg_error17 <- "La variable a representer n'existe pas dans la table des donnees / "
    if(!dom %in% c("0","971","972","973","974","976")) msg_error18 <- "La variable dom doit etre '0', '971', '972', '973', '974' ou '976' / "
    
    if(any(!is.null(msg_error1),!is.null(msg_error2),!is.null(msg_error3),!is.null(msg_error4),
           !is.null(msg_error5),!is.null(msg_error6),!is.null(msg_error7),!is.null(msg_error8),
           !is.null(msg_error10),!is.null(msg_error11),!is.null(msg_error12),!is.null(msg_error13),
           !is.null(msg_error14),!is.null(msg_error15),!is.null(msg_error16),!is.null(msg_error17),!is.null(msg_error18)))
    {
      stop(simpleError(paste0(msg_error1,msg_error2,msg_error3,msg_error4,msg_error5,msg_error6,msg_error7,msg_error8,
                              msg_error10,msg_error11,msg_error12,msg_error13,msg_error14,msg_error15,msg_error16,msg_error17,msg_error18)))
    }
    
    nb_up <- reactiveValues(a=0)
    nb_down <- reactiveValues(a=0)
    ordre_analyse <- reactiveValues(a=1,b=2)
    insert_save <- reactiveValues(a=0)
    remove_carte <- reactiveValues(a=0)
    liste_fonds <- reactiveValues(a=c("analyse","maille","contour"))
    m_save_ou <- reactiveValues(a=0)
    
    flux_min <- reactiveValues(a=100)
    distance_max <- reactiveValues(a=300)
    flux_majeur <- reactiveValues(a=10)
    
    erreur_maille <- reactiveValues(a=FALSE)
    
    sourc <- "Source : Insee"
    
    names(data)[names(data)==idDataDepart] <- "CODE1"
    names(data)[names(data)==idDataArrivee] <- "CODE2"
    names(fondMaille)[1] <- "CODE"
    names(fondMaille)[2] <- "LIBELLE"
    names(fondContour)[1] <- "CODE"
    names(fondContour)[2] <- "LIBELLE"
    if(!is.null(fondSuppl)) 
    {
      names(fondSuppl)[1] <- "CODE"
      names(fondSuppl)[2] <- "LIBELLE"
      fondSuppl$LIBELLE<-iconv(fondSuppl$LIBELLE,"latin1","utf8")
    }
    
    fondMaille$LIBELLE<-iconv(fondMaille$LIBELLE,"latin1","utf8")
    fondContour$LIBELLE<-iconv(fondContour$LIBELLE,"latin1","utf8")
    
    ui <- navbarPage("OCEANIS", id="menu",
                     
                     theme = shinytheme("superhero"),
                     
                     tabPanel("Carte",value="carte",
                              sidebarLayout(
                                
                                sidebarPanel(width = 3,
                                             style = "overflow-y:scroll; min-height: 840px; max-height: 840px",
                                             h4(HTML("<b><font color=#95BAE2>VARIABLES</font></b>")),
                                             uiOutput("variable_flux_ou"),
                                             tags$hr(style="border: 5px solid #5182B6"), #337ab7
                                             h4(HTML("<b><font color=#95BAE2>FONDS</font></b>")),
                                             fluidRow(
                                               column(width=9, offset=0.5,
                                                      uiOutput("ordre_fonds_ou")
                                               ),
                                               column(width=1,
                                                      br(),
                                                      br(),
                                                      htmlOutput("monter_fond_ou", inline=FALSE),
                                                      htmlOutput("descendre_fond_ou", inline=FALSE)
                                               )
                                             ),
                                             uiOutput("ajout_territoire_ou"),
                                             uiOutput("ajout_reg_ou"),
                                             uiOutput("ajout_dep_ou"),
                                             tags$hr(style="border: 5px solid #5182B6"),
                                             h4(HTML("<b><font color=#95BAE2>FLUX</font></b>")),
                                             uiOutput("flux_min_ou"),
                                             uiOutput("distance_max_ou"),
                                             uiOutput("flux_majeur_ou"),
                                             uiOutput("epaisseur_trait_ou"),
                                             uiOutput("decalage_aller_retour_ou"),
                                             uiOutput("decalage_centroid_ou"),
                                             tags$hr(style="border: 5px solid #5182B6"),
                                             h4(HTML("<b><font color=#95BAE2>SAUVEGARDE</font></b>")),
                                             uiOutput("save_carte_ou"),
                                             br(),
                                             tags$div(class="dropup",
                                                      HTML('<button class="btn btn-primary dropdown-toggle" type="button" data-toggle="dropdown" aria-haspopup="true" aria-expanded="false">
                                                           Exporter en projet Qgis
                                                           <span class="caret"></span>
                                                           </button>'),
                                                      tags$ul(class="dropdown-menu",
                                                              wellPanel(
                                                                style="background: #2B3E50; width:340px",
                                                                h4("Export de la carte en projet Qgis"),
                                                                br(),
                                                                uiOutput("sortie_qgis_ou"),
                                                                br(),
                                                                uiOutput("titre1_qgis_ou"),
                                                                uiOutput("titre2_qgis_ou"),
                                                                uiOutput("source_qgis_ou"),
                                                                tags$head(tags$style(HTML('#export_qgis_ou_id{background-color:#337ab7}'))),
                                                                uiOutput("export_qgis_ou")
                                                              )
                                                      )
                                                      ),
                                             br(),
                                             uiOutput("aide_image_ou"),
                                             br()
                                ),
                                mainPanel(
                                  tags$head(
                                    tags$style(HTML(".leaflet-container { background: #AFC9E0; }"))
                                  ),
                                  tabsetPanel(id="onglets_ou",
                                              tabPanel(title=HTML("<b>Carte</b>"),value="carte",
                                                       leafletOutput("mymap_ou",width="100%",height = 800)
                                              ),
                                              tabPanel(title=HTML(paste0("<b>Donn","\u00e9","es</b>")),value="donnees",
                                                       h5("S\u00e9lectionnez une ou plusieurs lignes pour ensuite les visualiser sur la carte."),
                                                       DT::dataTableOutput("mydonnees_ou",width="100%",height = 800)),
                                              tabPanel(title=HTML("<b>Maille</b>"),value="maille",
                                                       h5("S\u00e9lectionnez une ou plusieurs lignes pour ensuite les visualiser sur la carte."),
                                                       DT::dataTableOutput("mymaille_ou",width="100%",height = 800)),
                                              tabPanel(title=HTML("<b>Contour</b>"),value="contour",
                                                       h5("S\u00e9lectionnez une ou plusieurs lignes pour ensuite les visualiser sur la carte."),
                                                       DT::dataTableOutput("mycontour_ou",width="100%",height = 800))
                                  )
                                )
                     )
                     )
                     )
    
    server <- function(input, output, session) {
      
      #################
      #Onglet Carte
      #################
      
      #Charge les donnees et fonds en memoire et affiche les widgets adequats dans le sidePanel de l'onglet "Carte"
      observe({
        
        # VARIABLES
        
        output$variable_flux_ou <- renderUI({
          selectInput("variable_flux_ou_id", label=h5("Variable des flux (en volume)"), choices = varFlux, selected = varFlux)
        })
        
        # FONDS
        
        output$ordre_fonds_ou <- renderUI({
          selectInput("ordre_fonds_ou_id", label=h5("Modifier l'ordre des fonds"), choices = liste_fonds$a, multiple=TRUE, selectize=FALSE, selected = "analyse")
        })
        output$monter_fond_ou <- renderUI({
          actionButton("monter_fond_ou_id", label="", icon=icon("arrow-up"))
        })
        output$descendre_fond_ou <- renderUI({
          actionButton("descendre_fond_ou_id", label="", icon=icon("arrow-down"))
        })
        
        output$ajout_territoire_ou <- renderUI({
          checkboxInput("ajout_territoire_ou_id", label = "Afficher le fond des territoires",
                        value = if(is.null(fondSuppl)) FALSE else TRUE)
        })
        output$ajout_reg_ou <- renderUI({
          checkboxInput("ajout_reg_ou_id", label = "Afficher le fond des r\u00e9gions",
                        value = FALSE)
        })
        output$ajout_dep_ou <- renderUI({
          checkboxInput("ajout_dep_ou_id", label = "Afficher le fond des d\u00e9partements",
                        value = FALSE)
        })
        
        # FLUX
        
        output$flux_min_ou <- renderUI({
          numericInput("flux_min_ou_id", label = h5("Seuil de flux minimal"), value=flux_min$a, step=10)
        })
        
        output$distance_max_ou <- renderUI({
          numericInput("distance_max_ou_id", label = h5("Seuil de distance maximale (km)"), value=distance_max$a, step=10)
        })
        
        output$flux_majeur_ou <- renderUI({
          numericInput("flux_majeur_ou_id", label = h5("Nombre de flux majeurs"), value=flux_majeur$a, step=1)
        })
        
        output$epaisseur_trait_ou <- renderUI({
          sliderInput("epaisseur_trait_ou_id", label = h5(paste0("Epaisseur des traits")), value=2, min=1, max=5, step=1, ticks = TRUE)
        })
        
        output$decalage_aller_retour_ou <- renderUI({
          numericInput("decalage_aller_retour_ou_id", label = h5(paste0("D","\u00e9","calage des lignes allers-retours (km)")), value=decalageAllerRetour, step=1)
        })
        
        output$decalage_centroid_ou <- renderUI({
          numericInput("decalage_centroid_ou_id", label = h5(paste0("D","\u00e9","calage des lignes du centroid (km)")), value=decalageCentroid, step=1)
        })
        
        # SAUVEGARDE
        
        output$save_carte_ou <- renderUI({
          actionButton("save_carte_ou_id", label=HTML("<font size=3>Sauvegarder la carte dans un onglet</font>"), style="color:#FFFFFF; background-color:#DF691A")
        })
        
        output$entrees_qgis_ou <- renderUI({
          actionButton("entrees_qgis_ou_id", label="Exporter en projet Qgis")
        })
        
        output$sortie_qgis_ou <- renderUI({
          tags$div(class="input-group",
                   HTML('<input type="text" id="sortie_qgis_ou_id" class="form-control" placeholder="Nom du projet" aria-describedby="sortie_qgis_ou_id">
                        <span class="input-group-addon" id="sortie_qgis_ou_id">.qgs</span>'))
        })
        
        output$titre1_qgis_ou <- renderUI({
          textInput("titre1_qgis_ou_id", label = h5("Titre informatif"), value = "", placeholder= "Facultatif")
        })
        
        output$titre2_qgis_ou <- renderUI({
          textInput("titre2_qgis_ou_id", label = h5("Titre descriptif"), value = "", placeholder= "Facultatif")
        })
        
        output$source_qgis_ou <- renderUI({
          textInput("source_qgis_ou_id", label = h5("Source de la carte"), value = sourc)
        })
        
        output$aide_image_ou <- renderUI({
          tags$div(class="dropup",
                   HTML(paste0('<button class="btn btn-primary dropdown-toggle" type="button" data-toggle="dropdown">
                               <i class="fa fa-book fa-fw" aria-hidden="true"></i>
                               Proc','\u00e9','dure pour capture d\'','\u00e9','cran
                               <span class="caret"></span>
                               </button>')),
                   tags$ul(class="dropdown-menu",
                           wellPanel(
                             style="background: #2B3E50; width:340px",
                             div(
                               HTML("<font size=2>Deux possibilit\u00e9s :</font>"),
                               br(),
                               br(),
                               strong(HTML("<font size=3>Par l'Outil Capture</font>")),
                               br(),
                               HTML("<font size=2>1- Ouvrir le logiciel Outil Capture sur le Poste de travail (Menu D\u00e9marrer > Tous les programmes > Accessoires > Outil Capture).</font>"),
                               br(),
                               HTML(paste0("<font size=2>2- S\u00e9lectionner la zone \u00e0 capturer (vous aurez besoin d'adapter la fen\u00eatre AUS pour avoir \u00e0 la fois la carte et la barre des t","\u00e2","ches).</font>")),
                               br(),
                               HTML("<font size=2>3- Enregistrer l'image ou copier la dans le presse-papier.</font>"),
                               br(),
                               br(),
                               strong(HTML(paste0("<font size=3>Par impression d'","\u00e9","cran</font>"))),
                               br(),
                               HTML("<font size=2>1- Appuyer sur la touche clavier \"Impr ecran\".</font>"),
                               br(),
                               HTML("<font size=2>2- Ouvrir un logiciel de retouche image (Paint par exemple).</font>"),
                               br(),
                               HTML("<font size=2>3- Coller l'image et l'enregistrer au format voulu (.jpg, .png, .bmp).</font>")
                             )
                           )
                   )
                   )
        })
        })
      
      # Pour modifier l'ordre des fonds
      observeEvent(list(input$monter_fond_ou_id,input$descendre_fond_ou_id),{
        
        if(as.numeric(input$monter_fond_ou_id)==0 & as.numeric(input$descendre_fond_ou_id)==0) return(NULL)
        ordre <- c()
        
        if(as.numeric(input$monter_fond_ou_id)>nb_up$a)
        {
          ordre <- c(2,3)
          nb_up$a <- nb_up$a+1
        }
        if(as.numeric(input$descendre_fond_ou_id)>nb_down$a)
        {
          ordre <- c(1,2)
          nb_down$a <- nb_down$a+1
        }
        
        if(is.null(input$ordre_fonds_ou_id)) pos_select <- 0 else pos_select <- which(liste_fonds$a==input$ordre_fonds_ou_id)
        
        if(pos_select>0)
        {
          if(pos_select==ordre[1]) liste_fonds$a <- liste_fonds$a[c(2,1,3)]
          if(pos_select==ordre[2]) liste_fonds$a <- liste_fonds$a[c(1,3,2)]
          
          updateSelectInput(session, "ordre_fonds_ou_id",
                            choices = liste_fonds$a,
                            selected = input$ordre_fonds_ou_id
          )
        }
      },ignoreInit = TRUE)
      
      
      flux_min_ou <- reactive({
        
        if(is.null(input$flux_min_ou_id)) return(flux_min$a)
        
        return(input$flux_min_ou_id)
      })
      
      distance_max_ou <- reactive({
        
        if(is.null(input$distance_max_ou_id)) return(distance_max$a)
        
        return(input$distance_max_ou_id)
      })
      
      flux_majeur_ou <- reactive({
        
        if(is.null(input$flux_majeur_ou_id)) return(flux_majeur$a)
        
        return(input$flux_majeur_ou_id)
      })
      
      # Pour exporter la carte en projet Qgis
      
      output$export_qgis_ou <- renderUI({
        downloadButton("downloadProjetQgis_ou", label="Exporter")
      })
      
      output$downloadProjetQgis_ou <- downloadHandler(contentType = "zip",
                                                      filename = function(){
                                                        paste0(input$sortie_qgis_ou_id,".zip")
                                                      },
                                                      content = function(file){
                                                        files <- EXPORT_PROJET_QGIS_OU(file)
                                                        
                                                        zip(file,files, flags = "-j9X")
                                                      }
      )
      
      # Pour exporter la carte en projet Qgis
      EXPORT_PROJET_QGIS_OU <- function(file)
      {
        showModal(modalDialog(HTML("<i class=\"fa fa-spinner fa-spin fa-2x fa-fw\"></i> <font size=+1>Export du projet Qgis en cours...</font> "), size="m", footer=NULL, style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"))
        
        sortie <- input$sortie_qgis_ou_id
        rep_sortie <- dirname(file)
        files <- c(paste0(rep_sortie,"/",sortie,".qgs"))
        
        # fond_flux <- st_sf(analyse_ou()[[1]])
        fond_flux <- analyse_apres_filtre_ou()[[1]]
        fond_flux <- st_transform(fond_flux,paste0("+init=epsg:",code_epsg_ou()))
        
        fond_maille <- st_transform(fondMaille,paste0("+init=epsg:",code_epsg_ou()))
        fond_contour <- st_transform(fondContour,paste0("+init=epsg:",code_epsg_ou()))
        if(!is.null(fondSuppl) && input$ajout_territoire_ou_id) fond_territoire <- st_transform(fond_territoire_ou(),paste0("+init=epsg:",code_epsg_ou()))
        if(input$ajout_dep_ou_id) fond_departement <- st_transform(fond_departement_ou(),paste0("+init=epsg:",code_epsg_ou()))
        if(input$ajout_reg_ou_id) fond_region <- st_transform(fond_region_ou(),paste0("+init=epsg:",code_epsg_ou()))
        fond_france <- st_transform(fond_habillage_ou()[[1]],paste0("+init=epsg:",code_epsg_ou()))
        fond_pays <- st_transform(fond_habillage_ou()[[2]],paste0("+init=epsg:",code_epsg_ou()))
        
        st_write(fond_flux, paste0(rep_sortie,"/fond_flux.shp"), delete_dsn = TRUE, quiet = TRUE)
        st_write(fond_maille, paste0(rep_sortie,"/fond_maille.shp"), delete_dsn = TRUE, quiet = TRUE)
        st_write(fond_contour,paste0(rep_sortie,"/fond_contour.shp"), delete_dsn = TRUE, quiet = TRUE)
        if(exists("fond_territoire")) if(!is.null(fond_territoire)) st_write(fond_territoire, paste0(rep_sortie,"/fond_territoire.shp"), delete_dsn = TRUE, quiet = TRUE)
        if(exists("fond_departement")) if(!is.null(fond_departement)) st_write(fond_departement, paste0(rep_sortie,"/fond_departement.shp"), delete_dsn = TRUE, quiet = TRUE)
        if(exists("fond_region")) if(!is.null(fond_region)) st_write(fond_region,paste0(rep_sortie,"/fond_region.shp"), delete_dsn = TRUE, quiet = TRUE)
        st_write(fond_france,paste0(rep_sortie,"/fond_france.shp"), delete_dsn = TRUE, quiet = TRUE)
        if(exists("fond_pays")) if(!is.null(fond_pays)) st_write(fond_pays,paste0(rep_sortie,"/fond_pays.shp"), delete_dsn = TRUE, quiet = TRUE)
        
        files <- c(paste0(rep_sortie,"/fond_flux.shp"),paste0(rep_sortie,"/fond_flux.dbf"),paste0(rep_sortie,"/fond_flux.prj"),paste0(rep_sortie,"/fond_flux.shx"),files)
        files <- c(paste0(rep_sortie,"/fond_maille.shp"),paste0(rep_sortie,"/fond_maille.dbf"),paste0(rep_sortie,"/fond_maille.prj"),paste0(rep_sortie,"/fond_maille.shx"),files)
        files <- c(paste0(rep_sortie,"/fond_contour.shp"),paste0(rep_sortie,"/fond_contour.dbf"),paste0(rep_sortie,"/fond_contour.prj"),paste0(rep_sortie,"/fond_contour.shx"),files)
        if(exists("fond_territoire")) if(!is.null(fond_territoire)) files <- c(paste0(rep_sortie,"/fond_territoire.shp"),paste0(rep_sortie,"/fond_territoire.dbf"),paste0(rep_sortie,"/fond_territoire.prj"),paste0(rep_sortie,"/fond_territoire.shx"),files)
        if(exists("fond_departement")) if(!is.null(fond_departement)) files <- c(paste0(rep_sortie,"/fond_departement.shp"),paste0(rep_sortie,"/fond_departement.dbf"),paste0(rep_sortie,"/fond_departement.prj"),paste0(rep_sortie,"/fond_departement.shx"),files)
        if(exists("fond_region")) if(!is.null(fond_region)) files <- c(paste0(rep_sortie,"/fond_region.shp"),paste0(rep_sortie,"/fond_region.dbf"),paste0(rep_sortie,"/fond_region.prj"),paste0(rep_sortie,"/fond_region.shx"),files)
        files <- c(paste0(rep_sortie,"/fond_france.shp"),paste0(rep_sortie,"/fond_france.dbf"),paste0(rep_sortie,"/fond_france.prj"),paste0(rep_sortie,"/fond_france.shx"),files)
        if(exists("fond_pays")) if(!is.null(fond_pays)) files <- c(paste0(rep_sortie,"/fond_pays.shp"),paste0(rep_sortie,"/fond_pays.dbf"),paste0(rep_sortie,"/fond_pays.prj"),paste0(rep_sortie,"/fond_pays.shx"),files)
        
        chemin_fonds <- rep_sortie
        titre1 <- paste0(input$titre1_qgis_ou_id,"\n")
        titre2 <- input$titre2_qgis_ou_id
        source <- input$source_qgis_ou_id
        annee <- format(Sys.time(), format = "%Y")
        variable_a_representer <- varFlux
        
        l <- c()
        if(exists("fond_territoire")) l <- "fond_territoire"
        if(exists("fond_departement")) l <- c(l,"fond_departement")
        if(exists("fond_region")) l <- c(l,"fond_region")
        
        l=c("fond_france",
            "fond_contour",
            "fond_maille",
            l,
            "fond_flux"
        )
        
        if(exists("fond_pays")) l <- c(l,"fond_pays")
        
        export_projet_qgis_oursins(l,rep_sortie,sortie,titre1,titre2,source,2,"#303030",annee)
        
        removeModal()
        
        showModal(modalDialog(HTML(paste0("<font size=+1>Le projet Qgis a \u00e9t\u00e9 cr","\u00e9","ee.</font>")), size="m", footer=NULL, easyClose = TRUE, style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"))
        
        return(files)
      }
      
      code_epsg_ou <- reactive({
        code_epsg <- switch(dom, #DOM
                            "0"="2154",# Lambert 93
                            "971"="32620",# UTM 20 N
                            "972"="32620",# UTM 20 N
                            "973"="2972",# UTM 22 N
                            "974"="2975",# UTM 40 S
                            "976"="4471")# UTM 38 S
        return(code_epsg)
      })
      
      analyse_ou <- reactive({
        req(input$decalage_aller_retour_ou_id,input$decalage_centroid_ou_id)
        
        suppressWarnings(test_k_oursins <- try(k_oursins(fondMaille,names(fondMaille)[1],data,"CODE1","CODE2",varFlux,input$decalage_aller_retour_ou_id,input$decalage_centroid_ou_id),silent=TRUE))
        
        if(!class(test_k_oursins) %in% "try-error")
        {
          analyse<-k_oursins(fondMaille,names(fondMaille)[1],data,"CODE1","CODE2",varFlux,input$decalage_aller_retour_ou_id,input$decalage_centroid_ou_id)
        }else
        {
          showModal(modalDialog(HTML(paste0("<font size=+1>La maille ne correspond pas au niveau g\u00e9ographique du fichier de donn","\u00e9","es.<br><br>Veuillez svp choisir une maille adapt","\u00e9","e ou modifier le fichier de donn","\u00e9","es.</font>")), size="l", footer=NULL, easyClose = TRUE, style = "color: #fff; background-color: #DF691A; border-color: #2e6da4")) #337ab7
          erreur_maille$a <- TRUE
          return(NULL)
        }
        
        if(is.null(analyse))
        {
          showModal(modalDialog(HTML(paste0("<font size=+1>La maille ne correspond pas au niveau g\u00e9ographique du fichier de donn","\u00e9","es.<br><br>Veuillez svp choisir une maille adapt","\u00e9","e ou modifier le fichier de donn","\u00e9","es.</font>")), size="l", footer=NULL, easyClose = TRUE, style = "color: #fff; background-color: #DF691A; border-color: #2e6da4")) #337ab7
          erreur_maille$a <- TRUE
          return(NULL)
        }
        
        analyse_WGS84 <- st_transform(analyse[[1]],"+init=epsg:4326 +proj=longlat +ellps=WGS84")
        
        return(list(analyse[[1]],analyse_WGS84))
      })
      
      fond_habillage_ou <- reactive({
        
        if(dom=="0")
        {
          pays <- st_transform(sf_paysm(),"+init=epsg:4326 +proj=longlat +ellps=WGS84")
          fra <- st_transform(sf_fram(),"+init=epsg:4326 +proj=longlat +ellps=WGS84")
        }else
        {
          if(dom=="971")
          {
            fra <- st_transform(sf_reg01(),"+init=epsg:4326 +proj=longlat +ellps=WGS84")
            pays <- fra
          }
          if(dom=="972")
          {
            fra <- st_transform(sf_reg02(),"+init=epsg:4326 +proj=longlat +ellps=WGS84")
            pays <- fra
          }
          if(dom=="973")
          {
            fra <- st_transform(sf_reg03(),"+init=epsg:4326 +proj=longlat +ellps=WGS84")
            pays <- st_transform(sf_pays973(),"+init=epsg:4326 +proj=longlat +ellps=WGS84")
          }
          if(dom=="974")
          {
            fra <- st_transform(sf_reg04(),"+init=epsg:4326 +proj=longlat +ellps=WGS84")
            pays <- fra
          }
          if(dom=="976")
          {
            fra <- st_transform(sf_reg06(),"+init=epsg:4326 +proj=longlat +ellps=WGS84")
            pays <- fra
          }
        }
        fond_france <- fra
        fond_pays <- pays
        
        return(list(fond_france,fond_pays))
      })
      
      fond_contour_maille_ou <- reactive({
        
        test_contour <- try(st_transform(fondContour,"+init=epsg:4326 +proj=longlat +ellps=WGS84"), silent = TRUE)
        test_maille <- try(st_transform(fondMaille,"+init=epsg:4326 +proj=longlat +ellps=WGS84"), silent = TRUE)
        
        if(any(list(class(test_contour),class(test_maille)) %in% "try-error"))
        {
          showModal(modalDialog(HTML(paste0("<font size=+1>Une erreur est survenue dans la cr","\u00e9","ation du territoire.<br><br>Veuillez svp v\u00e9rifier vos donn","\u00e9","es et les variables choisies.</font>")), size="m", footer=NULL, easyClose = TRUE, style = "color: #fff; background-color: #DF691A; border-color: #2e6da4"))
          erreur_maille$a <- TRUE
          return(NULL)
        }else
        {
          contour_WGS84 <- st_transform(fondContour,"+init=epsg:4326 +proj=longlat +ellps=WGS84")
          maille_WGS84 <- st_transform(fondMaille,"+init=epsg:4326 +proj=longlat +ellps=WGS84")
        }
        
        return(list(contour_WGS84,maille_WGS84))
      })
      
      list_bbox_ou <- reactive({
        req(fond_contour_maille_ou())
        
        list_bbox <- list(c(st_bbox(fond_contour_maille_ou()[[1]])[1],st_bbox(fond_contour_maille_ou()[[1]])[3]),c(st_bbox(fond_contour_maille_ou()[[1]])[2],st_bbox(fond_contour_maille_ou()[[1]])[4]))
        return(list_bbox)
      })
      
      fond_territoire_ou <- reactive({
        
        if(!is.null(fondSuppl))
        {
          fond_territoire <- st_transform(fondSuppl,"+init=epsg:4326 +proj=longlat +ellps=WGS84")
          return(fond_territoire)
        }else
        {
          return(NULL)
        }
      })
      
      fond_region_ou <- reactive({
        fond_region <- st_transform(sf_regm(),"+init=epsg:4326 +proj=longlat +ellps=WGS84")
        return(fond_region)
      })
      
      fond_departement_ou <- reactive({
        fond_departement <- st_transform(sf_depm(),"+init=epsg:4326 +proj=longlat +ellps=WGS84")
        return(fond_departement)
      })
      
      fond_select_donnees_ou <- reactive({
        req(analyse_apres_filtre_ou())
        fond_donnees <- analyse_apres_filtre_ou()[[1]][input$mydonnees_ou_rows_selected,]
        return(fond_donnees)
      })
      
      fond_select_maille_ou <- reactive({
        req(fond_contour_maille_ou())
        
        fond_maille <- fond_contour_maille_ou()[[2]][as.data.frame(fond_contour_maille_ou()[[2]])[,"CODE"] %in% as.data.frame(fondMaille)[input$mymaille_ou_rows_selected,"CODE"],]
        return(fond_maille)
      })
      
      fond_select_contour_ou <- reactive({
        req(fond_contour_maille_ou())
        
        fond_contour <- fond_contour_maille_ou()[[1]][as.data.frame(fond_contour_maille_ou()[[1]])[,"CODE"] %in% as.data.frame(fondContour)[input$mycontour_ou_rows_selected,"CODE"],]
        return(fond_contour)
      })
      
      analyse_apres_filtre_ou <- reactive({
        req(analyse_ou(),flux_majeur_ou(),distance_max_ou(),flux_min_ou())
        
        if(flux_majeur_ou()>nrow(analyse_ou()[[2]]))
        {
          nb_flux_majeur <- nrow(analyse_ou()[[2]])
        }else
        {
          nb_flux_majeur <- flux_majeur_ou()
          if(nb_flux_majeur<1) nb_flux_majeur <- 1
        }
        
        analyse_WGS84_list <- split(analyse_ou()[[2]],factor(analyse_ou()[[2]]$CODE1))
        analyse_WGS84_1 <- data.frame()
        aa <- lapply(1:length(analyse_WGS84_list), function(x) analyse_WGS84_1 <<- rbind(analyse_WGS84_1,as.data.frame(analyse_WGS84_list[[x]])[rev(order(as.data.frame(analyse_WGS84_list[[x]])[,varFlux]))[c(1:nb_flux_majeur)],]))
        analyse_WGS84_1 <- analyse_WGS84_1[!is.na(analyse_WGS84_1[,varFlux]),]
        
        analyse_WGS84_list <- split(analyse_ou()[[2]],factor(analyse_ou()[[2]]$CODE2))
        analyse_WGS84_2 <- data.frame()
        aa <- lapply(1:length(analyse_WGS84_list), function(x) analyse_WGS84_2 <<- rbind(analyse_WGS84_2,as.data.frame(analyse_WGS84_list[[x]])[rev(order(as.data.frame(analyse_WGS84_list[[x]])[,varFlux]))[c(1:nb_flux_majeur)],]))
        analyse_WGS84_2 <- analyse_WGS84_2[!is.na(analyse_WGS84_2[,varFlux]),]
        
        analyse_WGS84 <- unique(rbind(analyse_WGS84_1,analyse_WGS84_2))
        
        analyse_WGS84 <- st_as_sf(analyse_WGS84)
        
        analyse_WGS84 <- analyse_WGS84[as.vector(st_length(analyse_WGS84))<=distance_max_ou()*1000,]
        
        analyse_WGS84 <- analyse_WGS84[as.data.frame(analyse_WGS84)[,varFlux]>=flux_min_ou(),]
        
        analyse_WGS84 <- analyse_WGS84[rev(order(as.data.frame(analyse_WGS84)[,varFlux])),]
        
        donnees <- merge(as.data.frame(analyse_WGS84)[,c("CODE1","CODE2")],data,by=c("CODE1","CODE2"),all.x=T)
        
        donnees <- sort(donnees[,varFlux], decreasing = TRUE)
        
        return(list(analyse_WGS84,donnees))
      })
      
      analyse_apres_filtre_init_ou <- reactive({
        req(analyse_ou())
        
        if(nrow(analyse_ou()[[2]])<10)
        {
          nb_flux_majeur <- nrow(analyse_ou()[[2]])
        }else
        {
          nb_flux_majeur <- 10
        }
        
        analyse_WGS84_list <- split(analyse_ou()[[2]],factor(analyse_ou()[[2]]$CODE1))
        analyse_WGS84_1 <- data.frame()
        aa <- lapply(1:length(analyse_WGS84_list), function(x) analyse_WGS84_1 <<- rbind(analyse_WGS84_1,as.data.frame(analyse_WGS84_list[[x]])[rev(order(as.data.frame(analyse_WGS84_list[[x]])[,varFlux]))[c(1:nb_flux_majeur)],]))
        analyse_WGS84_1 <- analyse_WGS84_1[!is.na(analyse_WGS84_1[,varFlux]),]
        
        analyse_WGS84_list <- split(analyse_ou()[[2]],factor(analyse_ou()[[2]]$CODE2))
        analyse_WGS84_2 <- data.frame()
        aa <- lapply(1:length(analyse_WGS84_list), function(x) analyse_WGS84_2 <<- rbind(analyse_WGS84_2,as.data.frame(analyse_WGS84_list[[x]])[rev(order(as.data.frame(analyse_WGS84_list[[x]])[,varFlux]))[c(1:nb_flux_majeur)],]))
        analyse_WGS84_2 <- analyse_WGS84_2[!is.na(analyse_WGS84_2[,varFlux]),]
        
        analyse_WGS84 <- unique(rbind(analyse_WGS84_1,analyse_WGS84_2))
        
        analyse_WGS84 <- st_as_sf(analyse_WGS84)
        
        analyse_WGS84 <- analyse_WGS84[as.vector(st_length(analyse_WGS84))<=300*1000,]
        
        analyse_WGS84 <- analyse_WGS84[as.data.frame(analyse_WGS84)[,varFlux]>=100,]
        
        analyse_WGS84 <- analyse_WGS84[rev(order(as.data.frame(analyse_WGS84)[,varFlux])),]
        
        donnees <- merge(as.data.frame(analyse_WGS84)[,c("CODE1","CODE2")],data,by=c("CODE1","CODE2"),all.x=T)
        
        donnees <- sort(donnees[,varFlux], decreasing = TRUE)
        
        return(list(analyse_WGS84,donnees))
      })
      
      # Pour construire la map en leaflet
      react_fond_ou <- reactive({
        
        if(input$menu=="carte")
        {
          showModal(modalDialog(HTML("<i class=\"fa fa-spinner fa-spin fa-2x fa-fw\"></i><font size=+1>\u00c9laboration de la carte...</font> "), size="m", footer=NULL, style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"))
          
          # Construction de la map par defaut
          
          m <- leaflet(padding = 0,
                       options = leafletOptions(
                         preferCanvas = TRUE,
                         transition = 2,
                         minZoom = 6,
                         maxZoom = 10
                       )) %>%
            
            setMapWidgetStyle(list(background = "#AFC9E0")) %>%
            
            addTiles_insee(attribution = paste0("<a href=\"http://www.insee.fr\">\u00A9 IGN - INSEE ",format(Sys.time(), format = "%Y"),"</a>")) %>%
            
            fitBounds(lng1 = min(list_bbox_ou()[[1]]),
                      lat1 = min(list_bbox_ou()[[2]]),
                      lng2 = max(list_bbox_ou()[[1]]),
                      lat2 = max(list_bbox_ou()[[2]])
            ) %>%
            
            # On ajoute une barre d'echelle
            addScaleBar(position = 'bottomright',
                        options = scaleBarOptions(metric = TRUE, imperial = FALSE)
            ) %>%
            
            # Pour gerer l'ordre des calques
            addMapPane(name = "fond_pays", zIndex = 401) %>%
            addMapPane(name = "fond_france", zIndex = 402) %>%
            addMapPane(name = "fond_habillage", zIndex = 403) %>%
            addMapPane(name = "fond_dep", zIndex = 404) %>%
            addMapPane(name = "fond_reg", zIndex = 405) %>%
            addMapPane(name = "fond_territoire", zIndex = 406) %>%
            addMapPane(name = "fond_trio3", zIndex = 407) %>%
            addMapPane(name = "fond_trio2", zIndex = 408) %>%
            addMapPane(name = "fond_trio1", zIndex = 409) %>%
            addMapPane(name = "selection", zIndex = 410) %>%
            
            addMapPane(name = "fond_legende", zIndex = 411)
          
          # AFFICHAGE DES FONDS D'HABILLAGE
          
          if(dom %in% c("0","973"))
          {
            # fond des pays
            m <- addPolygons(map = m, data = fond_habillage_ou()[[2]][,"LIBGEO"], opacity = 1, # sauf la France
                             stroke = TRUE, color = "white",
                             weight = 1,
                             options = pathOptions(pane = "fond_pays", clickable = F),
                             fill = T, fillColor = "#CCCCCC", fillOpacity = 1
            )
          }
          # fond de la France metro ou d'un DOM
          m <- addPolygons(map = m, data = fond_habillage_ou()[[1]][,"LIBGEO"], opacity = 1, # fond_france
                           stroke = TRUE, color = "black",
                           weight = 1.5,
                           options = pathOptions(pane = "fond_france", clickable = F),
                           fill = T, fillColor = "white", fillOpacity = 1
          )
          
          m_save_ou$a <- m
          
          # AFFICHAGE DU FOND TERRITOIRE
          
          if(!is.null(fondSuppl))
          {
            m <- addPolygons(map = m, data = fond_territoire_ou(),
                             stroke = TRUE, color = "#BFBFBF", opacity = 1,
                             weight = 0.5,
                             options = pathOptions(pane = "fond_territoire", clickable = T),
                             popup = paste0("<b> <font color=#2B3E50>",as.data.frame(fond_territoire_ou())[,"LIBELLE"], "</font> </b>"),
                             fill = T, fillColor = "white", fillOpacity = 0.001,
                             group = "territoire"
            )
          }
          
          # AFFICHAGE DES FONDS CONTOUR ET MAILLE
          
          # fond du contour
          m <- addPolygons(map = m, data = fond_contour_maille_ou()[[1]], opacity = 0.3,
                           stroke = TRUE, color = "black", weight = 3,
                           options = pathOptions(pane = "fond_trio3", clickable = T),
                           popup = paste0("<b> <font color=#2B3E50>",as.data.frame(fond_contour_maille_ou()[[1]])[,"LIBELLE"], "</font> </b>"),
                           fill = T, fillColor = "white", fillOpacity = 0.3,
                           group = "maille_contour"
          )
          
          # fond de la maille
          m <- addPolygons(map = m, data = fond_contour_maille_ou()[[2]], opacity = 1,
                           stroke = TRUE, color = "grey", weight = 1,
                           options = pathOptions(pane = "fond_trio2", clickable = T),
                           popup = paste0("<b> <font color=#2B3E50>",as.data.frame(fond_contour_maille_ou()[[2]])[,"LIBELLE"], "</font> </b>"),
                           fill = T, fillColor = "white", fillOpacity = 0.001,
                           group = "maille_contour"
          )
          
          # AFFICHAGE DE L'ANALYSE
          
          analyse_WGS84 <- analyse_apres_filtre_init_ou()[[1]]
          donnees <- analyse_apres_filtre_init_ou()[[2]]
          
          m <- addPolylines(map = m,
                            data = analyse_WGS84,
                            stroke = TRUE, color = "#303030",
                            opacity = 1,
                            weight = 2,
                            options = pathOptions(pane = "fond_trio1", clickable = T),
                            popup = paste0("<b><font color=#2B3E50>",varFlux," : ",donnees,"</font></b>"),
                            group = "fleche"
          )
          
          removeModal()
          
          showModal(modalDialog(HTML("<font size=+1>Veuillez patientez svp, la carte va s'afficher dans quelques secondes...</font> "), size="m", footer=NULL, easyClose = TRUE, style = "color: #fff; background-color: #DF691A; border-color: #2e6da4")) #337ab7
          
          return(m)
        }
      })
      
      # MODIFICATION DES FONDS TERRITOIRE D'ETUDES, REGION ET DEPARTEMENT
      
      observeEvent(input$ajout_territoire_ou_id,{
        
        proxy <- leafletProxy("mymap_ou")
        
        clearGroup(map = proxy, group = "territoire")
        
        # fond du territoire d'etudes
        if(!is.null(fondSuppl))
        {
          if(input$ajout_territoire_ou_id)
          {
            proxy <- addPolygons(map = proxy, data = fond_territoire_ou(),
                                 stroke = TRUE, color = "#BFBFBF", opacity = 1,
                                 weight = 0.5,
                                 options = pathOptions(pane = "fond_territoire", clickable = T),
                                 popup = paste0("<b> <font color=#2B3E50>",as.data.frame(fond_territoire_ou())[,"LIBELLE"], "</font> </b>"),
                                 fill = T, fillColor = "white", fillOpacity = 0.001,
                                 group = "territoire"
            )
          }
        }
      },ignoreInit = TRUE)
      
      observeEvent(input$ajout_reg_ou_id,{
        
        proxy <- leafletProxy("mymap_ou")
        
        clearGroup(map = proxy, group = "region")
        
        if(dom=="0")
        {
          if(input$ajout_reg_ou_id)
          {
            #fond des regions
            proxy <- addPolygons(map = proxy, data = fond_region_ou(),
                                 stroke = TRUE, color = "grey", opacity = 1,
                                 weight = 1.5,
                                 options = pathOptions(pane = "fond_reg", clickable = F),
                                 fill = F,
                                 group = "region"
            )
          }
        }
      },ignoreInit = TRUE)
      
      observeEvent(input$ajout_dep_ou_id,{
        
        proxy <- leafletProxy("mymap_ou")
        
        clearGroup(map = proxy, group = "departement")
        
        if(dom=="0")
        {
          if(input$ajout_dep_ou_id)
          {
            # fond des departements
            proxy <- addPolygons(map = proxy, data = fond_departement_ou(),
                                 stroke = TRUE, color = "grey", opacity = 1,
                                 weight = 0.5,
                                 options = pathOptions(pane = "fond_dep", clickable = F),
                                 fill = F,
                                 group = "departement"
            )
          }
        }
      },ignoreInit = TRUE)
      
      # MODIFICATION DE L'ORDRE DES CALQUES
      
      observeEvent(list(input$monter_fond_ou_id,input$descendre_fond_ou_id),{
        
        if(as.numeric(input$monter_fond_ou_id)==0 & as.numeric(input$descendre_fond_ou_id)==0) return(NULL)
        
        proxy <- leafletProxy("mymap_ou")
        
        clearGroup(map = proxy, group = "maille_contour")
        clearGroup(map = proxy, group = "fleche")
        
        i <- 1 #pour gerer l'ordre des fonds dans le pane
        
        for(fond in liste_fonds$a)
        {
          if(fond=="analyse")
          {
            analyse_WGS84 <- analyse_apres_filtre_ou()[[1]]
            donnees <- analyse_apres_filtre_ou()[[2]]
            
            proxy <- addPolylines(map = proxy,
                                  data = analyse_WGS84,
                                  stroke = TRUE, color = "#303030",
                                  opacity = 1,
                                  weight = input$epaisseur_trait_ou_id,
                                  options = pathOptions(pane = paste0("fond_trio",i), clickable = T),
                                  popup = paste0("<b><font color=#2B3E50>",varFlux," : ",donnees,"</font></b>"),
                                  group = "fleche"
            )
            
            ordre_analyse$a <- i
          }
          
          if(fond=="maille")
          {
            proxy <- addPolygons(map = proxy, data = fond_contour_maille_ou()[[2]], opacity = 1,
                                 stroke = TRUE, color = "grey", weight = 1,
                                 options = pathOptions(pane = paste0("fond_trio",i), clickable = T),
                                 popup = paste0("<b> <font color=#2B3E50>",as.data.frame(fond_contour_maille_ou()[[2]])[,"LIBELLE"], "</font> </b>"),
                                 fill = T, fillColor = "white", fillOpacity = 0.001,
                                 group = "maille_contour"
            )
          }
          
          if(fond=="contour")
          {
            proxy <- addPolygons(map = proxy, data = fond_contour_maille_ou()[[1]], opacity = 0.3,
                                 stroke = TRUE, color = "black", weight = 3,
                                 options = pathOptions(pane = paste0("fond_trio",i), clickable = T),
                                 popup = paste0("<b> <font color=#2B3E50>",as.data.frame(fond_contour_maille_ou()[[1]])[,"LIBELLE"], "</font> </b>"),
                                 fill = T, fillColor = "white", fillOpacity = 0.3,
                                 group = "maille_contour"
            )
          }
          
          i <- i + 1
        }
      },ignoreInit = TRUE)
      
      # MODIFICATION DES FLUX
      
      observeEvent(list(input$flux_min_ou_id,input$distance_max_ou_id,input$flux_majeur_ou_id,input$epaisseur_trait_ou_id,input$decalage_aller_retour_ou_id,input$decalage_centroid_ou_id),{
        req(input$flux_min_ou_id,input$distance_max_ou_id,input$flux_majeur_ou_id,input$epaisseur_trait_ou_id,input$decalage_aller_retour_ou_id,input$decalage_centroid_ou_id)
        
        proxy <- leafletProxy("mymap_ou")
        
        clearGroup(map = proxy, group = "fleche")
        
        analyse_WGS84 <- analyse_apres_filtre_ou()[[1]]
        donnees <- analyse_apres_filtre_ou()[[2]]
        
        proxy <- addPolylines(map = proxy,
                              data = analyse_WGS84,
                              stroke = TRUE, color = "#303030",
                              opacity = 1,
                              weight = input$epaisseur_trait_ou_id,
                              options = pathOptions(pane = paste0("fond_trio",ordre_analyse$a), clickable = T),
                              popup = paste0("<b><font color=#2B3E50>",varFlux," : ",donnees,"</font></b>"),
                              group = "fleche"
        )
      },ignoreInit = TRUE)
      
      # MODIFICATION DE LA SELECTION DES OBJETS VIA LES TABLEAUX
      
      observeEvent(input$onglets_ou,{
        req(input$onglets_ou)
        
        if(input$onglets_ou == "carte")
        {
          proxy <- leafletProxy("mymap_ou")
          
          clearGroup(map = proxy, group = "select_donnees")
          
          if(!is.null(input$mydonnees_ou_rows_selected))
          {
            fond_select_donnees <- st_transform(fond_select_donnees_ou(),"+init=epsg:4326 +proj=longlat +ellps=WGS84")
            proxy <- addPolylines(map = proxy,
                                  data = fond_select_donnees,
                                  stroke = TRUE, color = "#FFFF00",
                                  opacity = 1,
                                  weight = input$epaisseur_trait_ou_id,
                                  options = pathOptions(pane = "selection", clickable = F),
                                  fill = F,
                                  group = "select_donnees"
            )
          }
        }
      },ignoreInit = TRUE)
      
      observeEvent(input$onglets_ou,{
        req(input$onglets_ou)
        
        if(input$onglets_ou == "carte")
        {
          proxy <- leafletProxy("mymap_ou")
          
          clearGroup(map = proxy, group = "select_maille")
          
          if(!is.null(input$mymaille_ou_rows_selected))
          {
            proxy <- addPolygons(map = proxy, data = fond_select_maille_ou(),
                                 stroke = FALSE,
                                 options = pathOptions(pane = "selection", clickable = F),
                                 fill = T,
                                 fillColor = "#FFFF00",
                                 fillOpacity = 1,
                                 group = "select_maille"
            )
          }
        }
      },ignoreInit = TRUE)
      
      observeEvent(input$onglets_ou,{
        req(input$onglets_ou)
        
        if(input$onglets_ou == "carte")
        {
          proxy <- leafletProxy("mymap_ou")
          
          clearGroup(map = proxy, group = "select_contour")
          
          if(!is.null(input$mycontour_ou_rows_selected))
          {
            proxy <- addPolygons(map = proxy, data = fond_select_contour_ou(),
                                 stroke = FALSE,
                                 options = pathOptions(pane = "selection", clickable = F),
                                 fill = T,
                                 fillColor = "#FFFF00",
                                 fillOpacity = 1,
                                 group = "select_contour"
            )
          }
        }
      },ignoreInit = TRUE)
      
      # AJOUT DES ONGLETS SAUVEGARDE
      
      observeEvent(input$save_carte_ou_id,{
        
        showModal(modalDialog(HTML("<i class=\"fa fa-spinner fa-spin fa-2x fa-fw\"></i><font size=+1>Sauvegarde de la carte en cours...</font> "), size="m", footer=NULL, style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"))
        
        insert_save$a <- insert_save$a + 1
        nb_save_carte <- insert_save$a-remove_carte$a
        
        m_save <- m_save_ou$a
        
        if(nb_save_carte>6)
        {
          insert_save$a <- insert_save$a - 1
          showModal(modalDialog(HTML("<font size=+1>Vous ne pouvez pas sauvegarger plus de 6 cartes. Veuillez en supprimer avant de continuer.</font> "), size="l", footer=NULL, easyClose = TRUE, style = "color: #fff; background-color: #DF691A; border-color: #2e6da4")) #337ab7
          return(NULL)
        }
        
        output[[paste0("mymap_save_",insert_save$a,"_ou")]] <- renderLeaflet({
          
          if(!is.null(fondSuppl))
          {
            if(input$ajout_territoire_ou_id)
            {
              m_save <- addPolygons(map = m_save, data = fond_territoire_ou(),
                                    stroke = TRUE, color = "#BFBFBF", opacity = 1,
                                    weight = 0.5,
                                    options = pathOptions(pane = "fond_territoire", clickable = T),
                                    popup = paste0("<b> <font color=#2B3E50>",as.data.frame(fond_territoire_ou())[,"LIBELLE"], "</font> </b>"),
                                    fill = T, fillColor = "white", fillOpacity = 0.001
              )
            }
          }
          
          if(input$ajout_reg_ou_id)
          {
            m_save <- addPolygons(map = m_save, data = fond_region_ou(),
                                  stroke = TRUE, color = "grey", opacity = 1,
                                  weight = 1.5,
                                  options = pathOptions(pane = "fond_reg", clickable = F),
                                  fill = F
            )
          }
          
          if(input$ajout_dep_ou_id)
          {
            m_save <- addPolygons(map = m_save, data = fond_departement_ou(),
                                  stroke = TRUE, color = "grey", opacity = 1,
                                  weight = 0.5,
                                  options = pathOptions(pane = "fond_dep", clickable = F),
                                  fill = F
            )
          }
          
          i <- 1 #pour gerer l'ordre des fonds dans le pane
          
          for(fond in liste_fonds$a)
          {
            if(fond=="analyse")
            {
              analyse_WGS84 <- analyse_apres_filtre_ou()[[1]]
              donnees <- analyse_apres_filtre_ou()[[2]]
              
              m_save <- addPolylines(map = m_save,
                                     data = analyse_WGS84,
                                     stroke = TRUE, color = "#303030",
                                     opacity = 1,
                                     weight = input$epaisseur_trait_ou_id,
                                     options = pathOptions(pane = paste0("fond_trio",i), clickable = T),
                                     popup = paste0("<b><font color=#2B3E50>",varFlux," : ",donnees,"</font></b>")
              )
            }
            
            if(fond=="maille")
            {
              m_save <- addPolygons(map = m_save, data = fond_contour_maille_ou()[[2]], opacity = 1,
                                    stroke = TRUE, color = "grey", weight = 1,
                                    options = pathOptions(pane = paste0("fond_trio",i), clickable = T),
                                    popup = paste0("<b> <font color=#2B3E50>",as.data.frame(fond_contour_maille_ou()[[2]])[,"LIBELLE"], "</font> </b>"),
                                    fill = T, fillColor = "white", fillOpacity = 0.001
              )
            }
            
            if(fond=="contour")
            {
              m_save <- addPolygons(map = m_save, data = fond_contour_maille_ou()[[1]], opacity = 0.3,
                                    stroke = TRUE, color = "black", weight = 3,
                                    options = pathOptions(pane = paste0("fond_trio",i), clickable = T),
                                    popup = paste0("<b> <font color=#2B3E50>",as.data.frame(fond_contour_maille_ou()[[1]])[,"LIBELLE"], "</font> </b>"),
                                    fill = T, fillColor = "white", fillOpacity = 0.3
              )
            }
            
            i <- i + 1
          }
          
          removeModal()
          
          m_save
        })
        
        output[[paste0("remove_carte_",nb_save_carte,"_ou")]] <- renderUI({
          actionButton(paste0("remove_carte_",nb_save_carte,"_ou_id"),label="X Supprimer la carte", style="color:#FFFFFF; border-color:#FFFFFF; background-color:#2B3E50")
        })
        appendTab(inputId = "onglets_ou",
                  tabPanel(title=HTML(paste0("<font color=#AFC9E0> Carte ",insert_save$a,"</font>")),value=paste0("carte",nb_save_carte),uiOutput(paste0("remove_carte_",nb_save_carte,"_ou")),leafletOutput(paste0("mymap_save_",insert_save$a,"_ou"),width="100%",height = 800)),
                  select = TRUE,
                  session = session
        )
      }, ignoreInit = TRUE)
      
      observeEvent(input$remove_carte_1_ou_id,{
        remove_carte$a <- remove_carte$a + 1
        removeTab(inputId = "onglets_ou",
                  target = "carte1",
                  session = session
        )
      }, ignoreInit = TRUE)
      
      observeEvent(input$remove_carte_2_ou_id,{
        remove_carte$a <- remove_carte$a + 1
        removeTab(inputId = "onglets_ou",
                  target = "carte2",
                  session = session
        )
      }, ignoreInit = TRUE)
      
      observeEvent(input$remove_carte_3_ou_id,{
        remove_carte$a <- remove_carte$a + 1
        removeTab(inputId = "onglets_ou",
                  target = "carte3",
                  session = session
        )
      }, ignoreInit = TRUE)
      
      observeEvent(input$remove_carte_4_ou_id,{
        remove_carte$a <- remove_carte$a + 1
        removeTab(inputId = "onglets_ou",
                  target = "carte4",
                  session = session
        )
      }, ignoreInit = TRUE)
      
      observeEvent(input$remove_carte_5_ou_id,{
        remove_carte$a <- remove_carte$a + 1
        removeTab(inputId = "onglets_ou",
                  target = "carte5",
                  session = session
        )
      }, ignoreInit = TRUE)
      
      observeEvent(input$remove_carte_6_ou_id,{
        remove_carte$a <- remove_carte$a + 1
        removeTab(inputId = "onglets_ou",
                  target = "carte6",
                  session = session
        )
      }, ignoreInit = TRUE)
      
      # TABLEAUX DE DONNEES, MAILLE ET CONTOUR
      
      output$mydonnees_ou <- DT::renderDataTable(datatable({
        analyse_WGS84 <- analyse_apres_filtre_ou()[[1]]
        data <- as.data.frame(analyse_WGS84)
        tableau_donnees <- data[,c("CODE1","CODE2",varFlux)]
      },  style = 'bootstrap'
      ))
      
      output$mymaille_ou <- DT::renderDataTable(datatable({
        data <- as.data.frame(fondMaille)
        tableau_maille <- data[,c(1:2)]
      },  style = 'bootstrap'
      ))
      
      output$mycontour_ou <- DT::renderDataTable(datatable({
        data <- as.data.frame(fondContour)
        tableau_contour <- data[,c(1:2)]
      },  style = 'bootstrap'
      ))
      
      # ENVOI DU LEAFLET A L'UI
      
      output$mymap_ou <- renderLeaflet({
        react_fond_ou()
      })
    }
    
    runApp(shinyApp(ui = ui, server = server), launch.browser = TRUE)
    }


shiny_joignantes <-
  function(data,fondMaille,typeMaille,fondContour,fondSuppl=NULL,idDataDepart,idDataArrivee,varFlux,decalageAllerRetour=0,decalageCentroid=0,dom="0")
  {
    options("stringsAsFactors"=FALSE)
    
    # Verification des parametres
    msg_error1<-msg_error2<-msg_error3<-msg_error4<-msg_error5<-msg_error6<-msg_error7<-msg_error8<-msg_error9<-msg_error10<-msg_error11<-msg_error12<-msg_error13<-msg_error14<-msg_error15<-msg_error16<-msg_error17<-msg_error18<-msg_error19<-msg_error20 <- NULL
    
    if(any(class(data)!="data.frame")) msg_error1 <- "Les donnees doivent etre dans un data.frame / "
    if(any(!any(class(fondMaille) %in% "sf"),!any(class(fondMaille) %in% "data.frame"))) msg_error2 <- "Le fond de maille doit etre un objet sf / "
    if(any(!any(class(fondContour) %in% "sf"),!any(class(fondContour) %in% "data.frame"))) msg_error3 <- "Le fond de contour doit etre un objet sf / "
    if(!is.null(fondSuppl)) if(any(!any(class(fondSuppl) %in% "sf"),!any(class(fondSuppl) %in% "data.frame"))) msg_error4 <- "Le fond supplementaire doit etre un objet sf / "
    if(any(class(idDataDepart)!="character")) msg_error5 <- "Le nom de la variable de depart doit etre de type caractere / "
    if(any(class(idDataArrivee)!="character")) msg_error6 <- "Le nom de la variable d'arrivee doit etre de type caractere / "
    if(any(class(varFlux)!="character")) msg_error7 <- "Le nom de la variable doit etre de type caractere / "
    if(any(class(typeMaille)!="character")) msg_error8 <- "La valeur doit etre de type caractere ('REG', 'DEP', 'ZE', 'AU', 'BV', 'UU', 'EPCI' ou 'DEPCOM') / "
    if(any(class(decalageAllerRetour)!="numeric")) msg_error9 <- "La variable decalageAllerRetour doit etre de type numerique / "
    if(any(class(decalageCentroid)!="numeric")) msg_error10 <- "La variable decalageCentroid doit etre de type numerique / "
    if(any(class(dom)!="character")) msg_error11 <- "La valeur doit etre de type caractere ('0', '971', '972', '973', '974' ou '976') / "
    
    if(length(names(data))<3) msg_error12 <- "Le tableau des donnees n'est pas conforme. Il doit contenir au minimum une variable identifiant de depart, une variable identifiant d'arrivee et la variable a representer / "
    if(length(names(fondMaille))<3) msg_error13 <- "Le fond de maille n'est pas conforme. La table doit contenir au minimum une variable identifiant, une variable libelle et la geometry / "
    if(length(names(fondContour))<3) msg_error14 <- "Le fond de contour n'est pas conforme. La table doit contenir au minimum une variable identifiant, une variable libelle et la geometry / "
    if(!is.null(fondSuppl)) if(length(names(fondSuppl))<3) msg_error15 <- "Le fond supplementaire n'est pas conforme. La table doit contenir au minimum une variable identifiant, une variable libelle et la geometry / "
    
    if(!any(names(data) %in% idDataDepart))  msg_error16 <- "La variable identifiant de depart n'existe pas dans la table des donnees / "
    if(!any(names(data) %in% idDataArrivee))  msg_error17 <- "La variable identifiant d'arrivee n'existe pas dans la table des donnees / "
    if(!any(names(data) %in% varFlux))  msg_error18 <- "La variable a representer n'existe pas dans la table des donnees / "
    if(!typeMaille %in% c("REG", "DEP", "ZE", "AU", "BV", "UU", "EPCI", "DEPCOM")) msg_error19 <- "La variable typeMaille doit etre 'REG', 'DEP', 'ZE', 'AU', 'BV', 'UU', 'EPCI' ou 'DEPCOM' / "
    if(!dom %in% c("0","971","972","973","974","976")) msg_error20 <- "La variable dom doit etre '0', '971', '972', '973', '974' ou '976' / "
    
    if(any(!is.null(msg_error1),!is.null(msg_error2),!is.null(msg_error3),!is.null(msg_error4),
           !is.null(msg_error5),!is.null(msg_error6),!is.null(msg_error7),!is.null(msg_error8),
           !is.null(msg_error10),!is.null(msg_error11),!is.null(msg_error12),!is.null(msg_error13),
           !is.null(msg_error14),!is.null(msg_error15),!is.null(msg_error16),!is.null(msg_error17),
           !is.null(msg_error18),!is.null(msg_error19),!is.null(msg_error20)))
    {
      stop(simpleError(paste0(msg_error1,msg_error2,msg_error3,msg_error4,msg_error5,msg_error6,msg_error7,msg_error8,
                              msg_error10,msg_error11,msg_error12,msg_error13,msg_error14,msg_error15,msg_error16,
                              msg_error17,msg_error18,msg_error19,msg_error20)))
    }
    
    nb_up <- reactiveValues(a=0)
    nb_down <- reactiveValues(a=0)
    ordre_analyse <- reactiveValues(a=1,b=2)
    insert_save <- reactiveValues(a=0)
    remove_carte <- reactiveValues(a=0)
    liste_fonds <- reactiveValues(a=c("analyse","maille","contour"))
    m_save_fj <- reactiveValues(a=0)
    
    flux_min <- reactiveValues(a=100)
    distance_max <- reactiveValues(a=300)
    flux_majeur <- reactiveValues(a=10)
    largeur <- reactiveValues(a=0)
    
    erreur_maille <- reactiveValues(a=FALSE)
    
    legende <- reactiveValues(a=NULL)
    
    sourc <- "Source : Insee"
    
    names(data)[names(data)==idDataDepart] <- "CODE1"
    names(data)[names(data)==idDataArrivee] <- "CODE2"
    names(fondMaille)[1] <- "CODE"
    names(fondMaille)[2] <- "LIBELLE"
    names(fondContour)[1] <- "CODE"
    names(fondContour)[2] <- "LIBELLE"
    if(!is.null(fondSuppl)) 
    {
      names(fondSuppl)[1] <- "CODE"
      names(fondSuppl)[2] <- "LIBELLE"
      fondSuppl$LIBELLE<-iconv(fondSuppl$LIBELLE,"latin1","utf8")
    }
    
    fondMaille$LIBELLE<-iconv(fondMaille$LIBELLE,"latin1","utf8")
    fondContour$LIBELLE<-iconv(fondContour$LIBELLE,"latin1","utf8")
    
    ui <- navbarPage("OCEANIS", id="menu",
                     
                     theme = shinytheme("superhero"),
                     
                     tabPanel("Carte",value="carte",
                              sidebarLayout(
                                
                                sidebarPanel(width = 3,
                                             style = "overflow-y:scroll; min-height: 840px; max-height: 840px",
                                             h4(HTML("<b><font color=#95BAE2>VARIABLES</font></b>")),
                                             uiOutput("variable_flux_fj"),
                                             tags$hr(style="border: 5px solid #5182B6"), #337ab7
                                             h4(HTML("<b><font color=#95BAE2>FONDS</font></b>")),
                                             fluidRow(
                                               column(width=9, offset=0.5,
                                                      uiOutput("ordre_fonds_fj")
                                               ),
                                               column(width=1,
                                                      br(),
                                                      br(),
                                                      htmlOutput("monter_fond_fj", inline=FALSE),
                                                      htmlOutput("descendre_fond_fj", inline=FALSE)
                                               )
                                             ),
                                             uiOutput("ajout_territoire_fj"),
                                             uiOutput("ajout_reg_fj"),
                                             uiOutput("ajout_dep_fj"),
                                             tags$hr(style="border: 5px solid #5182B6"),
                                             h4(HTML("<b><font color=#95BAE2>FLUX</font></b>")),
                                             uiOutput("flux_min_fj"),
                                             uiOutput("distance_max_fj"),
                                             uiOutput("largeur_fj"),
                                             uiOutput("flux_majeur_fj"),
                                             uiOutput("decalage_aller_retour_fj"),
                                             uiOutput("decalage_centroid_fj"),
                                             tags$hr(style="border: 5px solid #5182B6"),
                                             h4(HTML("<b><font color=#95BAE2>L\u00c9GENDE</font></b>")),
                                             uiOutput("titre_flux_legende_fj"),
                                             br(),
                                             uiOutput("affiche_legende_fj"),
                                             br(),
                                             tags$hr(style="border: 5px solid #5182B6"),
                                             h4(HTML("<b><font color=#95BAE2>SAUVEGARDE</font></b>")),
                                             uiOutput("save_carte_fj"),
                                             br(),
                                             conditionalPanel(condition = 'input.mymap_fj_click',
                                                              tags$div(class="dropup",
                                                                       HTML('<button class="btn btn-primary dropdown-toggle" type="button" data-toggle="dropdown" aria-haspopup="true" aria-expanded="false">
                                                                            Exporter en projet Qgis
                                                                            <span class="caret"></span>
                                                                            </button>'),
                                                                       tags$ul(class="dropdown-menu",
                                                                               wellPanel(
                                                                                 style="background: #2B3E50; width:340px",
                                                                                 h4("Export de la carte en projet Qgis"),
                                                                                 br(),
                                                                                 uiOutput("sortie_qgis_fj"),
                                                                                 br(),
                                                                                 uiOutput("titre1_qgis_fj"),
                                                                                 uiOutput("titre2_qgis_fj"),
                                                                                 uiOutput("source_qgis_fj"),
                                                                                 tags$head(tags$style(HTML('#export_qgis_fj_id{background-color:#337ab7}'))),
                                                                                 uiOutput("export_qgis_fj")
                                                                               )
                                                                       )
                                                                       )
                                             ),
                                             br(),
                                             uiOutput("aide_image_fj"),
                                             br()
                              ),
                              mainPanel(
                                tags$head(
                                  tags$style(HTML(".leaflet-container { background: #AFC9E0; }"))
                                ),
                                tabsetPanel(id="onglets_fj",
                                            tabPanel(title=HTML("<b>Carte</b>"),value="carte",
                                                     leafletOutput("mymap_fj",width="100%",height = 800)
                                            ),
                                            tabPanel(title=HTML(paste0("<b>Donn","\u00e9","es</b>")),value="donnees",
                                                     h5("S\u00e9lectionnez une ou plusieurs lignes pour ensuite les visualiser sur la carte."),
                                                     DT::dataTableOutput("mydonnees_fj",width="100%",height = 800)),
                                            tabPanel(title=HTML("<b>Maille</b>"),value="maille",
                                                     h5("S\u00e9lectionnez une ou plusieurs lignes pour ensuite les visualiser sur la carte."),
                                                     DT::dataTableOutput("mymaille_fj",width="100%",height = 800)),
                                            tabPanel(title=HTML("<b>Contour</b>"),value="contour",
                                                     h5("S\u00e9lectionnez une ou plusieurs lignes pour ensuite les visualiser sur la carte."),
                                                     DT::dataTableOutput("mycontour_fj",width="100%",height = 800))
                                )
                              )
    )
    )
    )
    
    server <- function(input, output, session) {
      
      #################
      #Onglet Carte
      #################
      
      #Charge les donnees et fonds en memoire et affiche les widgets adequats dans le sidePanel de l'onglet "Carte"
      observe({
        
        # VARIABLES
        
        output$variable_flux_fj <- renderUI({
          selectInput("variable_flux_fj_id", label=h5("Variable des flux (en volume)"), choices = varFlux, selected = varFlux)
        })
        
        # FONDS
        
        output$ordre_fonds_fj <- renderUI({
          selectInput("ordre_fonds_fj_id", label=h5("Modifier l'ordre des fonds"), choices = liste_fonds$a, multiple=TRUE, selectize=FALSE, selected = "analyse")
        })
        output$monter_fond_fj <- renderUI({
          actionButton("monter_fond_fj_id", label="", icon=icon("arrow-up"))
        })
        output$descendre_fond_fj <- renderUI({
          actionButton("descendre_fond_fj_id", label="", icon=icon("arrow-down"))
        })
        
        output$ajout_territoire_fj <- renderUI({
          checkboxInput("ajout_territoire_fj_id", label = "Afficher le fond des territoires",
                        value = if(is.null(fondSuppl)) FALSE else TRUE)
        })
        output$ajout_reg_fj <- renderUI({
          checkboxInput("ajout_reg_fj_id", label = "Afficher le fond des r\u00e9gions",
                        value = FALSE)
        })
        output$ajout_dep_fj <- renderUI({
          checkboxInput("ajout_dep_fj_id", label = "Afficher le fond des d\u00e9partements",
                        value = FALSE)
        })
        
        # FLUX
        
        output$flux_min_fj <- renderUI({
          numericInput("flux_min_fj_id", label = h5("Seuil de flux minimal"), value=flux_min$a, step=10)
        })
        
        output$distance_max_fj <- renderUI({
          numericInput("distance_max_fj_id", label = h5("Seuil de distance maximale (km)"), value=distance_max$a, step=10)
        })
        
        output$flux_majeur_fj <- renderUI({
          numericInput("flux_majeur_fj_id", label = h5("Nombre de flux majeurs"), value=flux_majeur$a, step=1)
        })
        
        output$largeur_fj <- renderUI({
          numericInput("largeur_fj_id", label = h5("Largeur de la fleche la plus large (km)"), value=largeur$a, step=10)
        })
        
        output$decalage_aller_retour_fj <- renderUI({
          numericInput("decalage_aller_retour_fj_id", label = h5(paste0("D","\u00e9","calage des fleches allers-retours (km)")), value=decalageAllerRetour, step=1)
        })
        
        output$decalage_centroid_fj <- renderUI({
          numericInput("decalage_centroid_fj_id", label = h5(paste0("D","\u00e9","calage des fleches du centroid (km)")), value=decalageCentroid, step=1)
        })
        
        # LEGENDE
        
        output$titre_flux_legende_fj <- renderUI({
          textInput("titre_flux_legende_fj_id", label = h5("Titre de la l\u00e9gende des flux"), value = "")
        })
        
        output$affiche_legende_fj <- renderUI({
          checkboxInput("affiche_legende_fj_id", label = "Activer le d\u00e9placement de la l\u00e9gende au clic",
                        value = TRUE)
        })
        
        # SAUVEGARDE
        
        output$save_carte_fj <- renderUI({
          actionButton("save_carte_fj_id", label=HTML("<font size=3>Sauvegarder la carte dans un onglet</font>"), style="color:#FFFFFF; background-color:#DF691A")
        })
        
        output$entrees_qgis_fj <- renderUI({
          actionButton("entrees_qgis_fj_id", label="Exporter en projet Qgis")
        })
        
        output$sortie_qgis_fj <- renderUI({
          tags$div(class="input-group",
                   HTML('<input type="text" id="sortie_qgis_fj_id" class="form-control" placeholder="Nom du projet" aria-describedby="sortie_qgis_fj_id">
                        <span class="input-group-addon" id="sortie_qgis_fj_id">.qgs</span>'))
        })
        
        output$titre1_qgis_fj <- renderUI({
          textInput("titre1_qgis_fj_id", label = h5("Titre informatif"), value = "", placeholder= "Facultatif")
        })
        
        output$titre2_qgis_fj <- renderUI({
          textInput("titre2_qgis_fj_id", label = h5("Titre descriptif"), value = "", placeholder= "Facultatif")
        })
        
        output$source_qgis_fj <- renderUI({
          textInput("source_qgis_fj_id", label = h5("Source de la carte"), value = sourc)
        })
        
        output$aide_image_fj <- renderUI({
          tags$div(class="dropup",
                   HTML(paste0('<button class="btn btn-primary dropdown-toggle" type="button" data-toggle="dropdown">
                               <i class="fa fa-book fa-fw" aria-hidden="true"></i>
                               Proc','\u00e9','dure pour capture d\'','\u00e9','cran
                               <span class="caret"></span>
                               </button>')),
                   tags$ul(class="dropdown-menu",
                           wellPanel(
                             style="background: #2B3E50; width:340px",
                             div(
                               HTML("<font size=2>Deux possibilit\u00e9s :</font>"),
                               br(),
                               br(),
                               strong(HTML("<font size=3>Par l'Outil Capture</font>")),
                               br(),
                               HTML("<font size=2>1- Ouvrir le logiciel Outil Capture sur le Poste de travail (Menu D\u00e9marrer > Tous les programmes > Accessoires > Outil Capture).</font>"),
                               br(),
                               HTML(paste0("<font size=2>2- S\u00e9lectionner la zone \u00e0 capturer (vous aurez besoin d'adapter la fen\u00eatre AUS pour avoir \u00e0 la fois la carte et la barre des t","\u00e2","ches).</font>")),
                               br(),
                               HTML("<font size=2>3- Enregistrer l'image ou copier la dans le presse-papier.</font>"),
                               br(),
                               br(),
                               strong(HTML(paste0("<font size=3>Par impression d'","\u00e9","cran</font>"))),
                               br(),
                               HTML("<font size=2>1- Appuyer sur la touche clavier \"Impr ecran\".</font>"),
                               br(),
                               HTML("<font size=2>2- Ouvrir un logiciel de retouche image (Paint par exemple).</font>"),
                               br(),
                               HTML("<font size=2>3- Coller l'image et l'enregistrer au format voulu (.jpg, .png, .bmp).</font>")
                             )
                           )
                   )
                   )
        })
        })
      
      # Pour modifier l'ordre des fonds
      observeEvent(list(input$monter_fond_fj_id,input$descendre_fond_fj_id),{
        
        if(as.numeric(input$monter_fond_fj_id)==0 & as.numeric(input$descendre_fond_fj_id)==0) return(NULL)
        ordre <- c()
        if(as.numeric(input$monter_fond_fj_id)>nb_up$a)
        {
          ordre <- c(2,3)
          nb_up$a <- nb_up$a+1
        }
        if(as.numeric(input$descendre_fond_fj_id)>nb_down$a)
        {
          ordre <- c(1,2)
          nb_down$a <- nb_down$a+1
        }
        
        if(is.null(input$ordre_fonds_fj_id)) pos_select <- 0 else pos_select <- which(liste_fonds$a==input$ordre_fonds_fj_id)
        
        if(pos_select>0)
        {
          if(pos_select==ordre[1]) liste_fonds$a <- liste_fonds$a[c(2,1,3)]
          if(pos_select==ordre[2]) liste_fonds$a <- liste_fonds$a[c(1,3,2)]
          
          updateSelectInput(session, "ordre_fonds_fj_id",
                            choices = liste_fonds$a,
                            selected = input$ordre_fonds_fj_id
          )
        }
      },ignoreInit = TRUE)
      
      flux_min_fj <- reactive({
        
        if(is.null(input$flux_min_fj_id)) return(flux_min$a)
        
        return(input$flux_min_fj_id)
      })
      
      distance_max_fj <- reactive({
        
        if(is.null(input$distance_max_fj_id)) return(distance_max$a)
        
        return(input$distance_max_fj_id)
      })
      
      flux_majeur_fj <- reactive({
        
        if(is.null(input$flux_majeur_fj_id)) return(flux_majeur$a)
        
        return(input$flux_majeur_fj_id)
      })
      
      # Pour exporter la carte en projet Qgis
      
      output$export_qgis_fj <- renderUI({
        downloadButton("downloadProjetQgis_fj", label="Exporter")
      })
      
      output$downloadProjetQgis_fj <- downloadHandler(contentType = "zip",
                                                      filename = function(){
                                                        paste0(input$sortie_qgis_fj_id,".zip")
                                                      },
                                                      content = function(file){
                                                        files <- EXPORT_PROJET_QGIS_FJ(file)
                                                        
                                                        zip(file,files, flags = "-j9X")
                                                      }
      )
      
      # Pour exporter la carte en projet Qgis
      EXPORT_PROJET_QGIS_FJ <- function(file)
      {
        showModal(modalDialog(HTML("<i class=\"fa fa-spinner fa-spin fa-2x fa-fw\"></i> <font size=+1>Export du projet Qgis en cours...</font> "), size="m", footer=NULL, style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"))
        
        sortie <- input$sortie_qgis_fj_id
        rep_sortie <- dirname(file)
        files <- c(paste0(rep_sortie,"/",sortie,".qgs"))
        
        fond_flux <- analyse_apres_filtre_fj()[[1]]
        
        vmax <- max(abs(data.frame(analyse_fj()[[2]])[,varFlux]))
        coord_fleche_max_pl <- st_coordinates(analyse_fj()[[1]][abs(data.frame(analyse_fj()[[1]])[,varFlux])==vmax,])
        large_pl <- max(st_distance(st_sfc(st_point(c(coord_fleche_max_pl[5,1],coord_fleche_max_pl[5,2])),st_point(c(coord_fleche_max_pl[6,1],coord_fleche_max_pl[6,2])))))
        long_pl <- large_pl*2
        if(large_pl>100000) long_pl <- 100000
        if(large_pl<1000) long_pl <- 1000
        flux_leg <- flux_legende_joignantes_pl(lon_lat_fj()[[1]],lon_lat_fj()[[2]],long_pl,large_pl,code_epsg_fj())
        flux_leg <- cbind(flux_leg,VALEUR=c(max(data[,varFlux]),max(data[,varFlux])/3))
        fond_flux_leg <- flux_leg
        
        fond_flux <- st_transform(fond_flux,paste0("+init=epsg:",code_epsg_fj()))
        
        fond_maille <- st_transform(fondMaille,paste0("+init=epsg:",code_epsg_fj()))
        fond_contour <- st_transform(fondContour,paste0("+init=epsg:",code_epsg_fj()))
        if(!is.null(fondSuppl) && input$ajout_territoire_fj_id) fond_territoire <- st_transform(fond_territoire_fj(),paste0("+init=epsg:",code_epsg_fj()))
        if(input$ajout_dep_fj_id) fond_departement <- st_transform(fond_departement_fj(),paste0("+init=epsg:",code_epsg_fj()))
        if(input$ajout_reg_fj_id) fond_region <- st_transform(fond_region_fj(),paste0("+init=epsg:",code_epsg_fj()))
        fond_france <- st_transform(fond_habillage_fj()[[1]],paste0("+init=epsg:",code_epsg_fj()))
        fond_pays <- st_transform(fond_habillage_fj()[[2]],paste0("+init=epsg:",code_epsg_fj()))
        
        st_write(fond_flux, paste0(rep_sortie,"/fond_flux.shp"), delete_dsn = TRUE, quiet = TRUE)
        st_write(fond_flux_leg, paste0(rep_sortie,"/fond_flux_leg.shp"), delete_dsn = TRUE, quiet = TRUE)
        st_write(fond_maille, paste0(rep_sortie,"/fond_maille.shp"), delete_dsn = TRUE, quiet = TRUE)
        st_write(fond_contour,paste0(rep_sortie,"/fond_contour.shp"), delete_dsn = TRUE, quiet = TRUE)
        if(exists("fond_territoire")) if(!is.null(fond_territoire)) st_write(fond_territoire, paste0(rep_sortie,"/fond_territoire.shp"), delete_dsn = TRUE, quiet = TRUE)
        if(exists("fond_departement")) if(!is.null(fond_departement)) st_write(fond_departement, paste0(rep_sortie,"/fond_departement.shp"), delete_dsn = TRUE, quiet = TRUE)
        if(exists("fond_region")) if(!is.null(fond_region)) st_write(fond_region,paste0(rep_sortie,"/fond_region.shp"), delete_dsn = TRUE, quiet = TRUE)
        st_write(fond_france,paste0(rep_sortie,"/fond_france.shp"), delete_dsn = TRUE, quiet = TRUE)
        if(exists("fond_pays")) if(!is.null(fond_pays)) st_write(fond_pays,paste0(rep_sortie,"/fond_pays.shp"), delete_dsn = TRUE, quiet = TRUE)
        
        files <- c(paste0(rep_sortie,"/fond_flux.shp"),paste0(rep_sortie,"/fond_flux.dbf"),paste0(rep_sortie,"/fond_flux.prj"),paste0(rep_sortie,"/fond_flux.shx"),files)
        files <- c(paste0(rep_sortie,"/fond_flux_leg.shp"),paste0(rep_sortie,"/fond_flux_leg.dbf"),paste0(rep_sortie,"/fond_flux_leg.prj"),paste0(rep_sortie,"/fond_flux_leg.shx"),files)
        files <- c(paste0(rep_sortie,"/fond_maille.shp"),paste0(rep_sortie,"/fond_maille.dbf"),paste0(rep_sortie,"/fond_maille.prj"),paste0(rep_sortie,"/fond_maille.shx"),files)
        files <- c(paste0(rep_sortie,"/fond_contour.shp"),paste0(rep_sortie,"/fond_contour.dbf"),paste0(rep_sortie,"/fond_contour.prj"),paste0(rep_sortie,"/fond_contour.shx"),files)
        if(exists("fond_territoire")) if(!is.null(fond_territoire)) files <- c(paste0(rep_sortie,"/fond_territoire.shp"),paste0(rep_sortie,"/fond_territoire.dbf"),paste0(rep_sortie,"/fond_territoire.prj"),paste0(rep_sortie,"/fond_territoire.shx"),files)
        if(exists("fond_departement")) if(!is.null(fond_departement)) files <- c(paste0(rep_sortie,"/fond_departement.shp"),paste0(rep_sortie,"/fond_departement.dbf"),paste0(rep_sortie,"/fond_departement.prj"),paste0(rep_sortie,"/fond_departement.shx"),files)
        if(exists("fond_region")) if(!is.null(fond_region)) files <- c(paste0(rep_sortie,"/fond_region.shp"),paste0(rep_sortie,"/fond_region.dbf"),paste0(rep_sortie,"/fond_region.prj"),paste0(rep_sortie,"/fond_region.shx"),files)
        files <- c(paste0(rep_sortie,"/fond_france.shp"),paste0(rep_sortie,"/fond_france.dbf"),paste0(rep_sortie,"/fond_france.prj"),paste0(rep_sortie,"/fond_france.shx"),files)
        if(exists("fond_pays")) if(!is.null(fond_pays)) files <- c(paste0(rep_sortie,"/fond_pays.shp"),paste0(rep_sortie,"/fond_pays.dbf"),paste0(rep_sortie,"/fond_pays.prj"),paste0(rep_sortie,"/fond_pays.shx"),files)
        
        chemin_fonds <- rep_sortie
        titre1 <- paste0(input$titre1_qgis_fj_id,"\n")
        titre2 <- input$titre2_qgis_fj_id
        source <- input$source_qgis_fj_id
        annee <- format(Sys.time(), format = "%Y")
        variable_a_representer <- varFlux
        
        l <- c()
        if(exists("fond_territoire")) l <- "fond_territoire"
        if(exists("fond_departement")) l <- c(l,"fond_departement")
        if(exists("fond_region")) l <- c(l,"fond_region")
        
        l=c("fond_france",
            "fond_contour",
            "fond_maille",
            l,
            "fond_flux",
            "fond_flux_leg"
        )
        
        if(exists("fond_pays")) l <- c(l,"fond_pays")
        
        export_projet_qgis_fleches_joignantes(l,rep_sortie,sortie,titre1,titre2,source,"#CD853F","#303030",annee)
        
        removeModal()
        
        showModal(modalDialog(HTML(paste0("<font size=+1>Le projet Qgis a \u00e9t\u00e9 cr","\u00e9","ee.</font>")), size="m", footer=NULL, easyClose = TRUE, style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"))
        
        return(files)
      }
      
      code_epsg_fj <- reactive({
        code_epsg <- switch(dom, #DOM
                            "0"="2154",# Lambert 93
                            "971"="32620",# UTM 20 N
                            "972"="32620",# UTM 20 N
                            "973"="2972",# UTM 22 N
                            "974"="2975",# UTM 40 S
                            "976"="4471")# UTM 38 S
        return(code_epsg)
      })
      
      largeur_max_fj <- reactive({
        
        if(is.null(input$largeur_fj_id)) return(NULL)
        
        if (typeMaille=="REG") largeur$a<-100
        if (typeMaille=="DEP") largeur$a<-30
        if (substr(typeMaille,1,2)=="ZE") largeur$a<-10
        if (substr(typeMaille,1,2)=="AU") largeur$a<-6
        if (substr(typeMaille,1,2)=="BV") largeur$a<-6
        if (substr(typeMaille,1,2)=="UU") largeur$a<-6
        if (typeMaille=="EPCI") largeur$a<-4
        if (typeMaille=="DEPCOM") largeur$a<-2
        
        if(input$largeur_fj_id==0)
        {
          largeur_max <- largeur$a*1000
        }else
        {
          largeur_max <- input$largeur_fj_id*1000
        }
        
        return(largeur_max)
      })
      
      analyse_fj <- reactive({
        req(input$decalage_aller_retour_fj_id,input$decalage_centroid_fj_id)
        
        suppressWarnings(test_k_joignantes <- try(k_joignantes(fondMaille,names(fondMaille)[1],data,"CODE1","CODE2",varFlux,largeur_max_fj(),input$decalage_aller_retour_fj_id,input$decalage_centroid_fj_id),silent=TRUE))
        
        if(!class(test_k_joignantes) %in% "try-error")
        {
          analyse<-k_joignantes(fondMaille,names(fondMaille)[1],data,"CODE1","CODE2",varFlux,largeur_max_fj(),input$decalage_aller_retour_fj_id,input$decalage_centroid_fj_id)
        }else
        {
          showModal(modalDialog(HTML(paste0("<font size=+1>La maille ne correspond pas au niveau g\u00e9ographique du fichier de donn","\u00e9","es.<br><br>Veuillez svp choisir une maille adapt","\u00e9","e ou modifier le fichier de donn","\u00e9","es.</font>")), size="l", footer=NULL, easyClose = TRUE, style = "color: #fff; background-color: #DF691A; border-color: #2e6da4")) #337ab7
          erreur_maille$a <- TRUE
          return(NULL)
        }
        
        if(is.null(analyse))
        {
          showModal(modalDialog(HTML(paste0("<font size=+1>La maille ne correspond pas au niveau g\u00e9ographique du fichier de donn","\u00e9","es.<br><br>Veuillez svp choisir une maille adapt","\u00e9","e ou modifier le fichier de donn","\u00e9","es.</font>")), size="l", footer=NULL, easyClose = TRUE, style = "color: #fff; background-color: #DF691A; border-color: #2e6da4")) #337ab7
          erreur_maille$a <- TRUE
          return(NULL)
        }
        
        analyse_WGS84 <- st_transform(analyse[[1]],"+init=epsg:4326 +proj=longlat +ellps=WGS84")
        return(list(analyse[[1]],analyse_WGS84))
      })
      
      fond_habillage_fj <- reactive({
        
        if(dom=="0")
        {
          pays <- st_transform(sf_paysm(),"+init=epsg:4326 +proj=longlat +ellps=WGS84")
          fra <- st_transform(sf_fram(),"+init=epsg:4326 +proj=longlat +ellps=WGS84")
        }else
        {
          if(dom=="971")
          {
            fra <- st_transform(sf_reg01(),"+init=epsg:4326 +proj=longlat +ellps=WGS84")
            pays <- fra
          }
          if(dom=="972")
          {
            fra <- st_transform(sf_reg02(),"+init=epsg:4326 +proj=longlat +ellps=WGS84")
            pays <- fra
          }
          if(dom=="973")
          {
            fra <- st_transform(sf_reg03(),"+init=epsg:4326 +proj=longlat +ellps=WGS84")
            pays <- st_transform(sf_pays973(),"+init=epsg:4326 +proj=longlat +ellps=WGS84")
          }
          if(dom=="974")
          {
            fra <- st_transform(sf_reg04(),"+init=epsg:4326 +proj=longlat +ellps=WGS84")
            pays <- fra
          }
          if(dom=="976")
          {
            fra <- st_transform(sf_reg06(),"+init=epsg:4326 +proj=longlat +ellps=WGS84")
            pays <- fra
          }
        }
        fond_france <- fra
        fond_pays <- pays
        
        return(list(fond_france,fond_pays))
      })
      
      fond_contour_maille_fj <- reactive({
        
        test_contour <- try(st_transform(fondContour,"+init=epsg:4326 +proj=longlat +ellps=WGS84"), silent = TRUE)
        test_maille <- try(st_transform(fondMaille,"+init=epsg:4326 +proj=longlat +ellps=WGS84"), silent = TRUE)
        
        if(any(list(class(test_contour),class(test_maille)) %in% "try-error"))
        {
          showModal(modalDialog(HTML(paste0("<font size=+1>Une erreur est survenue dans la cr","\u00e9","ation du territoire.<br><br>Veuillez svp v\u00e9rifier vos donn","\u00e9","es et les variables choisies.</font>")), size="m", footer=NULL, easyClose = TRUE, style = "color: #fff; background-color: #DF691A; border-color: #2e6da4"))
          erreur_maille$a <- TRUE
          return(NULL)
        }else
        {
          contour_WGS84 <- st_transform(fondContour,"+init=epsg:4326 +proj=longlat +ellps=WGS84")
          maille_WGS84 <- st_transform(fondMaille,"+init=epsg:4326 +proj=longlat +ellps=WGS84")
        }
        
        return(list(contour_WGS84,maille_WGS84))
      })
      
      list_bbox_fj <- reactive({
        req(fond_contour_maille_fj())
        
        list_bbox <- list(c(st_bbox(fond_contour_maille_fj()[[1]])[1],st_bbox(fond_contour_maille_fj()[[1]])[3]),c(st_bbox(fond_contour_maille_fj()[[1]])[2],st_bbox(fond_contour_maille_fj()[[1]])[4]))
        return(list_bbox)
      })
      
      fond_territoire_fj <- reactive({
        if(!is.null(fondSuppl))
        {
          fond_territoire <- st_transform(fondSuppl,"+init=epsg:4326 +proj=longlat +ellps=WGS84")
          return(fond_territoire)
        }else
        {
          return(NULL)
        }
      })
      
      fond_region_fj <- reactive({
        fond_region <- st_transform(sf_regm(),"+init=epsg:4326 +proj=longlat +ellps=WGS84")
        return(fond_region)
      })
      
      fond_departement_fj <- reactive({
        fond_departement <- st_transform(sf_depm(),"+init=epsg:4326 +proj=longlat +ellps=WGS84")
        return(fond_departement)
      })
      
      fond_select_donnees_fj <- reactive({
        req(analyse_apres_filtre_fj())
        
        fond_donnees <- analyse_apres_filtre_fj()[[1]][input$mydonnees_fj_rows_selected,]
        fond_donnees <- st_transform(fond_donnees,"+init=epsg:4326 +proj=longlat +ellps=WGS84")
        return(fond_donnees)
      })
      
      fond_select_maille_fj <- reactive({
        req(fond_contour_maille_fj())
        
        fond_maille <- fond_contour_maille_fj()[[2]][as.data.frame(fond_contour_maille_fj()[[2]])[,"CODE"] %in% as.data.frame(fondMaille)[input$mymaille_fj_rows_selected,"CODE"],]
        return(fond_maille)
      })
      
      fond_select_contour_fj <- reactive({
        req(fond_contour_maille_fj())
        
        fond_contour <- fond_contour_maille_fj()[[1]][as.data.frame(fond_contour_maille_fj()[[1]])[,"CODE"] %in% as.data.frame(fondContour)[input$mycontour_fj_rows_selected,"CODE"],]
        return(fond_contour)
      })
      
      analyse_apres_filtre_fj <- reactive({
        req(analyse_fj(),flux_majeur_fj(),distance_max_fj(),flux_min_fj())
        
        if(flux_majeur_fj()>nrow(analyse_fj()[[2]]))
        {
          nb_flux_majeur <- nrow(analyse_fj()[[2]])
        }else
        {
          nb_flux_majeur <- flux_majeur_fj()
          if(nb_flux_majeur<1) nb_flux_majeur <- 1
        }
        
        analyse_WGS84_list <- split(analyse_fj()[[2]],factor(analyse_fj()[[2]]$CODE1))
        analyse_WGS84_1 <- data.frame()
        aa <- lapply(1:length(analyse_WGS84_list), function(x) analyse_WGS84_1 <<- rbind(analyse_WGS84_1,as.data.frame(analyse_WGS84_list[[x]])[rev(order(as.data.frame(analyse_WGS84_list[[x]])[,varFlux]))[c(1:nb_flux_majeur)],]))
        analyse_WGS84_1 <- analyse_WGS84_1[!is.na(analyse_WGS84_1[,varFlux]),]
        
        analyse_WGS84_list <- split(analyse_fj()[[2]],factor(analyse_fj()[[2]]$CODE2))
        analyse_WGS84_2 <- data.frame()
        aa <- lapply(1:length(analyse_WGS84_list), function(x) analyse_WGS84_2 <<- rbind(analyse_WGS84_2,as.data.frame(analyse_WGS84_list[[x]])[rev(order(as.data.frame(analyse_WGS84_list[[x]])[,varFlux]))[c(1:nb_flux_majeur)],]))
        analyse_WGS84_2 <- analyse_WGS84_2[!is.na(analyse_WGS84_2[,varFlux]),]
        
        analyse_WGS84 <- unique(rbind(analyse_WGS84_1,analyse_WGS84_2))
        
        analyse_WGS84 <- st_as_sf(analyse_WGS84)
        
        analyse_WGS84 <- analyse_WGS84[as.vector(st_length(analyse_WGS84)/2.2)<=distance_max_fj()*1000,]
        
        analyse_WGS84 <- analyse_WGS84[as.data.frame(analyse_WGS84)[,varFlux]>=flux_min_fj(),]
        
        analyse_WGS84 <- analyse_WGS84[rev(order(as.data.frame(analyse_WGS84)[,varFlux])),]
        
        donnees <- merge(as.data.frame(analyse_WGS84)[,c("CODE1","CODE2")],data,by=c("CODE1","CODE2"),all.x=T)
        
        donnees <- sort(donnees[,varFlux], decreasing = TRUE)
        
        return(list(analyse_WGS84,donnees))
      })
      
      analyse_apres_filtre_init_fj <- reactive({
        
        analyse <- analyse_fj()
        
        if(nrow(analyse[[2]])<10)
        {
          nb_flux_majeur <- nrow(analyse[[2]])
        }else
        {
          nb_flux_majeur <- 10
        }
        
        analyse_WGS84_list <- split(analyse[[2]],factor(analyse[[2]]$CODE1))
        analyse_WGS84_1 <- data.frame()
        aa <- lapply(1:length(analyse_WGS84_list), function(x) analyse_WGS84_1 <<- rbind(analyse_WGS84_1,as.data.frame(analyse_WGS84_list[[x]])[rev(order(as.data.frame(analyse_WGS84_list[[x]])[,varFlux]))[c(1:nb_flux_majeur)],]))
        analyse_WGS84_1 <- analyse_WGS84_1[!is.na(analyse_WGS84_1[,varFlux]),]
        
        analyse_WGS84_list <- split(analyse[[2]],factor(analyse[[2]]$CODE2))
        analyse_WGS84_2 <- data.frame()
        aa <- lapply(1:length(analyse_WGS84_list), function(x) analyse_WGS84_2 <<- rbind(analyse_WGS84_2,as.data.frame(analyse_WGS84_list[[x]])[rev(order(as.data.frame(analyse_WGS84_list[[x]])[,varFlux]))[c(1:nb_flux_majeur)],]))
        analyse_WGS84_2 <- analyse_WGS84_2[!is.na(analyse_WGS84_2[,varFlux]),]
        
        analyse_WGS84 <- unique(rbind(analyse_WGS84_1,analyse_WGS84_2))
        
        analyse_WGS84 <- st_as_sf(analyse_WGS84)
        
        analyse_WGS84 <- analyse_WGS84[as.vector(st_length(analyse_WGS84)/2.2)<=300*1000,]
        
        analyse_WGS84 <- analyse_WGS84[as.data.frame(analyse_WGS84)[,varFlux]>=100,]
        
        analyse_WGS84 <- analyse_WGS84[rev(order(as.data.frame(analyse_WGS84)[,varFlux])),]
        
        donnees <- merge(as.data.frame(analyse_WGS84)[,c("CODE1","CODE2")],data,by=c("CODE1","CODE2"),all.x=T)
        
        donnees <- sort(donnees[,varFlux], decreasing = TRUE)
        
        return(list(analyse_WGS84,donnees))
      })
      
      # Pour construire la map en leaflet
      react_fond_fj <- reactive({
        
        if(input$menu=="carte")
        {
          showModal(modalDialog(HTML("<i class=\"fa fa-spinner fa-spin fa-2x fa-fw\"></i><font size=+1>\u00c9laboration de la carte...</font> "), size="m", footer=NULL, style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"))
          
          # Construction de la map par defaut
          
          m <- leaflet(padding = 0,
                       options = leafletOptions(
                         preferCanvas = TRUE,
                         transition = 2,
                         minZoom = 6,
                         maxZoom = 10
                       )) %>%
            
            setMapWidgetStyle(list(background = "#AFC9E0")) %>%
            
            addTiles_insee(attribution = paste0("<a href=\"http://www.insee.fr\">\u00A9 IGN - INSEE ",format(Sys.time(), format = "%Y"),"</a>")) %>%
            
            fitBounds(lng1 = min(list_bbox_fj()[[1]]),
                      lat1 = min(list_bbox_fj()[[2]]),
                      lng2 = max(list_bbox_fj()[[1]]),
                      lat2 = max(list_bbox_fj()[[2]])
            ) %>%
            
            # On ajoute une barre d'echelle
            addScaleBar(position = 'bottomright',
                        options = scaleBarOptions(metric = TRUE, imperial = FALSE)
            ) %>%
            
            # Pour gerer l'ordre des calques
            addMapPane(name = "fond_pays", zIndex = 401) %>%
            addMapPane(name = "fond_france", zIndex = 402) %>%
            addMapPane(name = "fond_habillage", zIndex = 403) %>%
            addMapPane(name = "fond_dep", zIndex = 404) %>%
            addMapPane(name = "fond_reg", zIndex = 405) %>%
            addMapPane(name = "fond_territoire", zIndex = 406) %>%
            addMapPane(name = "fond_trio3", zIndex = 407) %>%
            addMapPane(name = "fond_trio2", zIndex = 408) %>%
            addMapPane(name = "fond_trio1", zIndex = 409) %>%
            addMapPane(name = "selection", zIndex = 410) %>%
            
            addMapPane(name = "fond_legende", zIndex = 411)
          
          # AFFICHAGE DES FONDS D'HABILLAGE
          
          if(dom %in% c("0","973"))
          {
            # fond des pays
            m <- addPolygons(map = m, data = fond_habillage_fj()[[2]][,"LIBGEO"], opacity = 1, # sauf la France
                             stroke = TRUE, color = "white",
                             weight = 1,
                             options = pathOptions(pane = "fond_pays", clickable = F),
                             fill = T, fillColor = "#CCCCCC", fillOpacity = 1
            )
          }
          # fond de la France metro ou d'un DOM
          m <- addPolygons(map = m, data = fond_habillage_fj()[[1]][,"LIBGEO"], opacity = 1, # fond_france
                           stroke = TRUE, color = "black",
                           weight = 1.5,
                           options = pathOptions(pane = "fond_france", clickable = F),
                           fill = T, fillColor = "white", fillOpacity = 1
          )
          
          m_save_fj$a <- m
          
          # AFFICHAGE DU FOND TERRITOIRE
          
          if(!is.null(fondSuppl))
          {
            m <- addPolygons(map = m, data = fond_territoire_fj(),
                             stroke = TRUE, color = "#BFBFBF", opacity = 1,
                             weight = 0.5,
                             options = pathOptions(pane = "fond_territoire", clickable = T),
                             popup = paste0("<b> <font color=#2B3E50>",as.data.frame(fond_territoire_fj())[,"LIBELLE"], "</font> </b>"),
                             fill = T, fillColor = "white", fillOpacity = 0.001,
                             group = "territoire"
            )
          }
          
          # AFFICHAGE DES FONDS CONTOUR ET MAILLE
          
          # fond du contour
          m <- addPolygons(map = m, data = fond_contour_maille_fj()[[1]], opacity = 0.3,
                           stroke = TRUE, color = "black", weight = 3,
                           options = pathOptions(pane = "fond_trio3", clickable = T),
                           popup = paste0("<b> <font color=#2B3E50>",as.data.frame(fond_contour_maille_fj()[[1]])[,"LIBELLE"], "</font> </b>"),
                           fill = T, fillColor = "white", fillOpacity = 0.3,
                           group = "maille_contour"
          )
          
          # fond de la maille
          m <- addPolygons(map = m, data = fond_contour_maille_fj()[[2]], opacity = 1,
                           stroke = TRUE, color = "grey", weight = 1,
                           options = pathOptions(pane = "fond_trio2", clickable = T),
                           popup = paste0("<b> <font color=#2B3E50>",as.data.frame(fond_contour_maille_fj()[[2]])[,"LIBELLE"], "</font> </b>"),
                           fill = T, fillColor = "white", fillOpacity = 0.001,
                           group = "maille_contour"
          )
          
          # AFFICHAGE DE L'ANALYSE
          
          analyse_WGS84 <- analyse_apres_filtre_init_fj()[[1]]
          donnees <- analyse_apres_filtre_init_fj()[[2]]
          
          m <- addPolygons(map = m,
                           data = analyse_WGS84,
                           stroke = TRUE, color = "#303030",
                           opacity = 1,
                           weight = 1,
                           options = pathOptions(pane = "fond_trio1", clickable = T),
                           popup = paste0("<b><font color=#2B3E50>",varFlux," : ",donnees,"</font></b>"),
                           fill = T, fillColor = "#CD853F", fillOpacity = 1,
                           group = "fleche"
          )
          
          removeModal()
          
          showModal(modalDialog(HTML("<font size=+1>Veuillez patientez svp, la carte va s'afficher dans quelques secondes...<br><br><i class=\"fa fa-hand-pointer-o fa-fw\"></i><b>Double-cliquez</b> ensuite sur la carte pour afficher la l\u00e9gende.</font> "), size="m", footer=NULL, easyClose = TRUE, style = "color: #fff; background-color: #DF691A; border-color: #2e6da4")) #337ab7
          
          return(m)
        }
      })
      
      # MODIFICATION DES FONDS TERRITOIRE D'ETUDES, REGION ET DEPARTEMENT
      
      observeEvent(input$ajout_territoire_fj_id,{
        
        proxy <- leafletProxy("mymap_fj")
        
        clearGroup(map = proxy, group = "territoire")
        
        # fond du territoire d'etudes
        if(!is.null(fondSuppl))
        {
          if(input$ajout_territoire_fj_id)
          {
            proxy <- addPolygons(map = proxy, data = fond_territoire_fj(),
                                 stroke = TRUE, color = "#BFBFBF", opacity = 1,
                                 weight = 0.5,
                                 options = pathOptions(pane = "fond_territoire", clickable = T),
                                 popup = paste0("<b> <font color=#2B3E50>",as.data.frame(fond_territoire_fj())[,"LIBELLE"], "</font> </b>"),
                                 fill = T, fillColor = "white", fillOpacity = 0.001,
                                 group = "territoire"
            )
          }
        }
      },ignoreInit = TRUE)
      
      observeEvent(input$ajout_reg_fj_id,{
        
        proxy <- leafletProxy("mymap_fj")
        
        clearGroup(map = proxy, group = "region")
        
        if(dom=="0")
        {
          if(input$ajout_reg_fj_id)
          {
            #fond des regions
            proxy <- addPolygons(map = proxy, data = fond_region_fj(),
                                 stroke = TRUE, color = "grey", opacity = 1,
                                 weight = 1.5,
                                 options = pathOptions(pane = "fond_reg", clickable = F),
                                 fill = F,
                                 group = "region"      
            )
          }
        }
      },ignoreInit = TRUE)
      
      observeEvent(input$ajout_dep_fj_id,{
        
        proxy <- leafletProxy("mymap_fj")
        
        clearGroup(map = proxy, group = "departement")
        
        if(dom=="0")
        {
          if(input$ajout_dep_fj_id)
          {
            # fond des departements
            proxy <- addPolygons(map = proxy, data = fond_departement_fj(),
                                 stroke = TRUE, color = "grey", opacity = 1,
                                 weight = 0.5,
                                 options = pathOptions(pane = "fond_dep", clickable = F),
                                 fill = F,
                                 group = "departement"
            )
          }
        }
      },ignoreInit = TRUE)
      
      # MODIFICATION DE L'ORDRE DES CALQUES
      
      observeEvent(list(input$monter_fond_fj_id,input$descendre_fond_fj_id),{
        
        if(as.numeric(input$monter_fond_fj_id)==0 & as.numeric(input$descendre_fond_fj_id)==0) return(NULL)
        
        proxy <- leafletProxy("mymap_fj")
        
        clearGroup(map = proxy, group = "maille_contour")
        clearGroup(map = proxy, group = "fleche")
        
        i <- 1 #pour gerer l'ordre des fonds dans le pane
        
        for(fond in liste_fonds$a)
        {
          if(fond=="analyse")
          {
            analyse_WGS84 <- analyse_apres_filtre_fj()[[1]]
            donnees <- analyse_apres_filtre_fj()[[2]]
            
            proxy <- addPolygons(map = proxy,
                                 data = analyse_WGS84,
                                 stroke = TRUE, color = "#303030",
                                 opacity = 1,
                                 weight = 1,
                                 options = pathOptions(pane = paste0("fond_trio",i), clickable = T),
                                 popup = paste0("<b><font color=#2B3E50>",varFlux," : ",donnees,"</font></b>"),
                                 fill = T, fillColor = "#CD853F", fillOpacity = 1,
                                 group = "fleche"
            )
            
            ordre_analyse$a <- i
          }
          
          if(fond=="maille")
          {
            proxy <- addPolygons(map = proxy, data = fond_contour_maille_fj()[[2]], opacity = 1,
                                 stroke = TRUE, color = "grey", weight = 1,
                                 options = pathOptions(pane = paste0("fond_trio",i), clickable = T),
                                 popup = paste0("<b> <font color=#2B3E50>",as.data.frame(fond_contour_maille_fj()[[2]])[,"LIBELLE"], "</font> </b>"),
                                 fill = T, fillColor = "white", fillOpacity = 0.001,
                                 group = "maille_contour"
            )
          }
          
          if(fond=="contour")
          {
            proxy <- addPolygons(map = proxy, data = fond_contour_maille_fj()[[1]], opacity = 0.3,
                                 stroke = TRUE, color = "black", weight = 3,
                                 options = pathOptions(pane = paste0("fond_trio",i), clickable = T),
                                 popup = paste0("<b> <font color=#2B3E50>",as.data.frame(fond_contour_maille_fj()[[1]])[,"LIBELLE"], "</font> </b>"),
                                 fill = T, fillColor = "white", fillOpacity = 0.3,
                                 group = "maille_contour"
            )
          }
          
          i <- i + 1
        }
      },ignoreInit = TRUE)
      
      # MODIFICATION DES FLUX
      
      observeEvent(list(input$flux_min_fj_id,input$distance_max_fj_id,input$flux_majeur_fj_id,input$largeur_fj_id,input$decalage_aller_retour_fj_id,input$decalage_centroid_fj_id),{
        req(input$flux_min_fj_id,input$distance_max_fj_id,input$flux_majeur_fj_id,input$largeur_fj_id,input$decalage_aller_retour_fj_id,input$decalage_centroid_fj_id)
        
        proxy <- leafletProxy("mymap_fj")
        
        clearGroup(map = proxy, group = "fleche")
        
        analyse_WGS84 <- analyse_apres_filtre_fj()[[1]]
        donnees <- analyse_apres_filtre_fj()[[2]]
        
        proxy <- addPolygons(map = proxy,
                             data = analyse_WGS84,
                             stroke = TRUE, color = "#303030",
                             opacity = 1,
                             weight = 1,
                             options = pathOptions(pane = paste0("fond_trio",ordre_analyse$a), clickable = T),
                             popup = paste0("<b><font color=#2B3E50>",varFlux," : ",donnees,"</font></b>"),
                             fill = T, fillColor = "#CD853F", fillOpacity = 1,
                             group = "fleche"
        )
      },ignoreInit = TRUE)
      
      # MODIFICATION DE LA SELECTION DES OBJETS VIA LES TABLEAUX
      
      observeEvent(input$onglets_fj,{
        req(input$onglets_fj)
        
        if(input$onglets_fj == "carte")
        {
          proxy <- leafletProxy("mymap_fj")
          
          clearGroup(map = proxy, group = "select_donnees")
          
          if(!is.null(input$mydonnees_fj_rows_selected))
          {
            proxy <- addPolylines(map = proxy,
                                  data = fond_select_donnees_fj(),
                                  stroke = TRUE, color = "#FFFF00",
                                  opacity = 1,
                                  weight = 2,
                                  options = pathOptions(pane = "selection", clickable = F),
                                  fill = F,
                                  group = "select_donnees"
            )
          }
        }
      },ignoreInit = TRUE)
      
      observeEvent(input$onglets_fj,{
        req(input$onglets_fj)
        
        if(input$onglets_fj == "carte")
        {
          proxy <- leafletProxy("mymap_fj")
          
          clearGroup(map = proxy, group = "select_maille")
          
          if(!is.null(input$mymaille_fj_rows_selected))
          {
            proxy <- addPolygons(map = proxy, data = fond_select_maille_fj(),
                                 stroke = FALSE,
                                 options = pathOptions(pane = "selection", clickable = F),
                                 fill = T,
                                 fillColor = "#FFFF00",
                                 fillOpacity = 1,
                                 group = "select_maille"
            )
          }
        }
      },ignoreInit = TRUE)
      
      observeEvent(input$onglets_fj,{
        req(input$onglets_fj)
        
        if(input$onglets_fj == "carte")
        {
          proxy <- leafletProxy("mymap_fj")
          
          clearGroup(map = proxy, group = "select_contour")
          
          if(!is.null(input$mycontour_fj_rows_selected))
          {
            proxy <- addPolygons(map = proxy, data = fond_select_contour_fj(),
                                 stroke = FALSE,
                                 options = pathOptions(pane = "selection", clickable = F),
                                 fill = T,
                                 fillColor = "#FFFF00",
                                 fillOpacity = 1,
                                 group = "select_contour"
            )
          }
        }
      },ignoreInit = TRUE)
      
      # CONSTRUCTION DE LA LEGENDE
      
      lon_lat_fj <- reactive({
        click <- input$mymap_fj_click
        lon <- click$lng
        lat <- click$lat
        return(list(lon,lat))
      })
      
      observeEvent(list(input$mymap_fj_zoom,input$mymap_fj_click,input$titre_flux_legende_fj_id,input$flux_min_fj_id,input$distance_max_fj_id,input$flux_majeur_fj_id,input$largeur_fj_id),{
        if(is.null(input$affiche_legende_fj_id)) return(NULL)
        
        if(input$affiche_legende_fj_id==FALSE) return(NULL)
        
        if(is.null(lon_lat_fj()[[1]])) return(NULL)
        
        CONSTRUCTION_LEGENDE_FJ()
      })
      
      CONSTRUCTION_LEGENDE_FJ <- function()
      {
        proxy <- leafletProxy("mymap_fj")
        
        proxy <- clearGroup(map=proxy, group="leg")
        proxy <- clearMarkers(map=proxy)
        
        zoom <- as.numeric(input$mymap_fj_zoom)
        coeff <- ((360/(2^zoom))/7.2) # Permet de fixer une distance sur l'ecran. Il s'agit en gros d'une conversion des degres en pixels. Reste constant a longitude egale mais varie un peu selon la latitude
        
        vmax <- max(abs(data.frame(analyse_fj()[[2]])[,varFlux]))
        
        coord_fleche_max <- st_coordinates(analyse_fj()[[2]][abs(data.frame(analyse_fj()[[2]])[,varFlux])==vmax,])
        large <- max(st_distance(st_sfc(st_point(c(coord_fleche_max[2,1],coord_fleche_max[2,2])),st_point(c(coord_fleche_max[6,1],coord_fleche_max[6,2])))))
        
        long <- coeff*2
        
        flux_legWGS84_j <- flux_legende_joignantes(lon_lat_fj()[[1]],lon_lat_fj()[[2]],long,large)
        flux_legWGS84 <- flux_legWGS84_j[[1]]
        pointe1 <- flux_legWGS84_j[[2]]
        pointe2 <- flux_legWGS84_j[[3]]
        
        # leaflet du cadre blanc en 1er
        proxy <- addRectangles(map = proxy,
                               lng1 = st_bbox(flux_legWGS84)[1]-coeff/2, lat1 = st_bbox(flux_legWGS84)[2]-coeff/2,
                               lng2 = st_bbox(flux_legWGS84)[3]+coeff*3, lat2 = st_bbox(flux_legWGS84)[4]+coeff*1.2,
                               stroke = FALSE,
                               options = pathOptions(pane = "fond_legende", clickable = F),
                               fill = T,
                               fillColor = "white",
                               fillOpacity = 0.8,
                               group="leg"
        )
        
        suppressWarnings(proxy <- addPolygons(map = proxy,
                                              data=flux_legWGS84,
                                              stroke = TRUE,
                                              opacity = 1,
                                              color = "#2B3E50",
                                              weight = 2,
                                              options = pathOptions(pane = "fond_legende", clickable = F),
                                              fill = T,
                                              fillColor = "white",
                                              fillOpacity = 1,
                                              group="leg"
        ))
        
        # leaflet valeur flux
        proxy <- addLabelOnlyMarkers(map = proxy,
                                     lng = pointe1[1], lat = pointe1[2], #grande fleche
                                     label = as.character(format(vmax,big.mark=" ",decimal.mark=",",nsmall=0)),
                                     labelOptions = labelOptions(noHide = T, textOnly = TRUE, direction = "right",
                                                                 style = list(
                                                                   "color" = "black",
                                                                   "font-size" = "12px"
                                                                 )),
                                     group="leg"
        )
        
        proxy <- addLabelOnlyMarkers(map = proxy,
                                     lng = pointe2[1], lat = pointe2[2], #petite fleche
                                     label = as.character(format(round(vmax/3,0),big.mark=" ",decimal.mark=",",nsmall=0)),
                                     labelOptions = labelOptions(noHide = T, textOnly = TRUE, direction = "right",
                                                                 style = list(
                                                                   "color" = "black",
                                                                   "font-size" = "12px"
                                                                 )),
                                     group="leg"
        )
        
        #leaflet titre 1
        proxy <- addLabelOnlyMarkers(map = proxy,
                                     lng = st_bbox(flux_legWGS84)[1]-coeff/3, lat = st_bbox(flux_legWGS84)[4]+coeff/2,
                                     label = input$titre_flux_legende_fj_id,
                                     labelOptions = labelOptions(noHide = T, textOnly = TRUE, direction = "right",
                                                                 style = list(
                                                                   "color" = "black",
                                                                   "font-size" = "14px"
                                                                 )),
                                     group="leg"
        )
      }
      
      # AJOUT DES ONGLETS SAUVEGARDE
      
      observeEvent(input$save_carte_fj_id,{
        
        showModal(modalDialog(HTML("<i class=\"fa fa-spinner fa-spin fa-2x fa-fw\"></i><font size=+1>Sauvegarde de la carte en cours...</font> "), size="m", footer=NULL, style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"))
        
        insert_save$a <- insert_save$a + 1
        nb_save_carte <- insert_save$a-remove_carte$a
        
        m_save <- m_save_fj$a
        
        if(nb_save_carte>6)
        {
          insert_save$a <- insert_save$a - 1
          showModal(modalDialog(HTML("<font size=+1>Vous ne pouvez pas sauvegarger plus de 6 cartes. Veuillez en supprimer avant de continuer.</font> "), size="l", footer=NULL, easyClose = TRUE, style = "color: #fff; background-color: #DF691A; border-color: #2e6da4")) #337ab7
          return(NULL)
        }
        
        output[[paste0("mymap_save_",insert_save$a,"_fj")]] <- renderLeaflet({
          
          if(!is.null(fondSuppl))
          {
            if(input$ajout_territoire_fj_id)
            {
              m_save <- addPolygons(map = m_save, data = fond_territoire_fj(),
                                    stroke = TRUE, color = "#BFBFBF", opacity = 1,
                                    weight = 0.5,
                                    options = pathOptions(pane = "fond_territoire", clickable = T),
                                    popup = paste0("<b> <font color=#2B3E50>",as.data.frame(fond_territoire_fj())[,"LIBELLE"], "</font> </b>"),
                                    fill = T, fillColor = "white", fillOpacity = 0.001
              )
            }
          }
          
          if(input$ajout_reg_fj_id)
          {
            m_save <- addPolygons(map = m_save, data = fond_region_fj(),
                                  stroke = TRUE, color = "grey", opacity = 1,
                                  weight = 1.5,
                                  options = pathOptions(pane = "fond_reg", clickable = F),
                                  fill = F
            )
          }
          
          if(input$ajout_dep_fj_id)
          {
            m_save <- addPolygons(map = m_save, data = fond_departement_fj(),
                                  stroke = TRUE, color = "grey", opacity = 1,
                                  weight = 0.5,
                                  options = pathOptions(pane = "fond_dep", clickable = F),
                                  fill = F
            )
          }
          
          i <- 1 #pour gerer l'ordre des fonds dans le pane
          
          for(fond in liste_fonds$a)
          {
            if(fond=="analyse")
            {
              analyse_WGS84 <- analyse_apres_filtre_fj()[[1]]
              donnees <- analyse_apres_filtre_fj()[[2]]
              
              m_save <- addPolygons(map = m_save,
                                    data = analyse_WGS84,
                                    stroke = TRUE, color = "#303030",
                                    opacity = 1,
                                    weight = 1,
                                    options = pathOptions(pane = paste0("fond_trio",i), clickable = T),
                                    popup = paste0("<b><font color=#2B3E50>",varFlux," : ",donnees,"</font></b>"),
                                    fill = T, fillColor = "#CD853F", fillOpacity = 1
              )
            }
            
            if(fond=="maille")
            {
              m_save <- addPolygons(map = m_save, data = fond_contour_maille_fj()[[2]], opacity = 1,
                                    stroke = TRUE, color = "grey", weight = 1,
                                    options = pathOptions(pane = paste0("fond_trio",i), clickable = T),
                                    popup = paste0("<b> <font color=#2B3E50>",as.data.frame(fond_contour_maille_fj()[[2]])[,"LIBELLE"], "</font> </b>"),
                                    fill = T, fillColor = "white", fillOpacity = 0.001
              )
            }
            
            if(fond=="contour")
            {
              m_save <- addPolygons(map = m_save, data = fond_contour_maille_fj()[[1]], opacity = 0.3,
                                    stroke = TRUE, color = "black", weight = 3,
                                    options = pathOptions(pane = paste0("fond_trio",i), clickable = T),
                                    popup = paste0("<b> <font color=#2B3E50>",as.data.frame(fond_contour_maille_fj()[[1]])[,"LIBELLE"], "</font> </b>"),
                                    fill = T, fillColor = "white", fillOpacity = 0.3
              )
            }
            
            i <- i + 1
          }
          
          zoom <- as.numeric(input$mymap_fj_zoom)
          coeff <- ((360/(2^zoom))/7.2) # Permet de fixer une distance sur l'ecran. Il s'agit en gros d'une conversion des degres en pixels. Reste constant a longitude egale mais varie un peu selon la latitude
          
          vmax <- max(abs(data.frame(analyse_fj()[[2]])[,varFlux]))
          
          coord_fleche_max <- st_coordinates(analyse_fj()[[2]][abs(data.frame(analyse_fj()[[2]])[,varFlux])==vmax,])
          large <- max(st_distance(st_sfc(st_point(c(coord_fleche_max[5,1],coord_fleche_max[5,2])),st_point(c(coord_fleche_max[6,1],coord_fleche_max[6,2])))))
          
          long <- coeff*2
          
          flux_legWGS84_j <- flux_legende_joignantes(lon_lat_fj()[[1]],lon_lat_fj()[[2]],long,large)
          flux_legWGS84 <- flux_legWGS84_j[[1]]
          pointe1 <- flux_legWGS84_j[[2]]
          pointe2 <- flux_legWGS84_j[[3]]
          
          # leaflet du cadre blanc en 1er
          m_save <- addRectangles(map = m_save,
                                  lng1 = st_bbox(flux_legWGS84)[1]-coeff/2, lat1 = st_bbox(flux_legWGS84)[2]-coeff/2,
                                  lng2 = st_bbox(flux_legWGS84)[3]+coeff*3, lat2 = st_bbox(flux_legWGS84)[4]+coeff*1.2,
                                  stroke = FALSE,
                                  options = pathOptions(pane = "fond_legende", clickable = F),
                                  fill = T,
                                  fillColor = "white",
                                  fillOpacity = 0.8
          )
          
          suppressWarnings(m_save <- addPolygons(map = m_save,
                                                 data=flux_legWGS84,
                                                 stroke = TRUE,
                                                 opacity = 1,
                                                 color = "#2B3E50",
                                                 weight = 2,
                                                 options = pathOptions(pane = "fond_legende", clickable = F),
                                                 fill = T,
                                                 fillColor = "white",
                                                 fillOpacity = 1
          ))
          
          # leaflet valeur flux
          m_save <- addLabelOnlyMarkers(map = m_save,
                                        lng = pointe1[1], lat = pointe1[2], #grande fleche
                                        label = as.character(format(vmax,big.mark=" ",decimal.mark=",",nsmall=0)),
                                        labelOptions = labelOptions(noHide = T, textOnly = TRUE, direction = "right",
                                                                    style = list(
                                                                      "color" = "black",
                                                                      "font-size" = "12px"
                                                                    ))
          )
          
          m_save <- addLabelOnlyMarkers(map = m_save,
                                        lng = pointe2[1], lat = pointe2[2], #petite fleche
                                        label = as.character(format(round(vmax/3,0),big.mark=" ",decimal.mark=",",nsmall=0)),
                                        labelOptions = labelOptions(noHide = T, textOnly = TRUE, direction = "right",
                                                                    style = list(
                                                                      "color" = "black",
                                                                      "font-size" = "12px"
                                                                    ))
          )
          
          #leaflet titre 1
          m_save <- addLabelOnlyMarkers(map = m_save,
                                        lng = st_bbox(flux_legWGS84)[1]-coeff/3, lat = st_bbox(flux_legWGS84)[4]+coeff/2,
                                        label = input$titre_flux_legende_fj_id,
                                        labelOptions = labelOptions(noHide = T, textOnly = TRUE, direction = "right",
                                                                    style = list(
                                                                      "color" = "black",
                                                                      "font-size" = "14px"
                                                                    ))
          )
          
          removeModal()
          
          m_save
        })
        
        output[[paste0("remove_carte_",nb_save_carte,"_fj")]] <- renderUI({
          actionButton(paste0("remove_carte_",nb_save_carte,"_fj_id"),label="X Supprimer la carte", style="color:#FFFFFF; border-color:#FFFFFF; background-color:#2B3E50")
        })
        appendTab(inputId = "onglets_fj",
                  tabPanel(title=HTML(paste0("<font color=#AFC9E0> Carte ",insert_save$a,"</font>")),value=paste0("carte",nb_save_carte),uiOutput(paste0("remove_carte_",nb_save_carte,"_fj")),leafletOutput(paste0("mymap_save_",insert_save$a,"_fj"),width="100%",height = 800)),
                  select = TRUE,
                  session = session
        )
      }, ignoreInit = TRUE)
      
      observeEvent(input$remove_carte_1_fj_id,{
        remove_carte$a <- remove_carte$a + 1
        removeTab(inputId = "onglets_fj",
                  target = "carte1",
                  session = session
        )
      }, ignoreInit = TRUE)
      
      observeEvent(input$remove_carte_2_fj_id,{
        remove_carte$a <- remove_carte$a + 1
        removeTab(inputId = "onglets_fj",
                  target = "carte2",
                  session = session
        )
      }, ignoreInit = TRUE)
      
      observeEvent(input$remove_carte_3_fj_id,{
        remove_carte$a <- remove_carte$a + 1
        removeTab(inputId = "onglets_fj",
                  target = "carte3",
                  session = session
        )
      }, ignoreInit = TRUE)
      
      observeEvent(input$remove_carte_4_fj_id,{
        remove_carte$a <- remove_carte$a + 1
        removeTab(inputId = "onglets_fj",
                  target = "carte4",
                  session = session
        )
      }, ignoreInit = TRUE)
      
      observeEvent(input$remove_carte_5_fj_id,{
        remove_carte$a <- remove_carte$a + 1
        removeTab(inputId = "onglets_fj",
                  target = "carte5",
                  session = session
        )
      }, ignoreInit = TRUE)
      
      observeEvent(input$remove_carte_6_fj_id,{
        remove_carte$a <- remove_carte$a + 1
        removeTab(inputId = "onglets_fj",
                  target = "carte6",
                  session = session
        )
      }, ignoreInit = TRUE)
      
      # TABLEAUX DE DONNEES, MAILLE ET CONTOUR
      
      output$mydonnees_fj <- DT::renderDataTable(datatable({
        analyse_WGS84 <- analyse_apres_filtre_fj()[[1]]
        data <- as.data.frame(analyse_WGS84)
        tableau_donnees <- data[,c("CODE1","CODE2",varFlux)]
      },  style = 'bootstrap'
      ))
      
      output$mymaille_fj <- DT::renderDataTable(datatable({
        data <- as.data.frame(fondMaille)
        tableau_maille <- data[,c(1:2)]
      },  style = 'bootstrap'
      ))
      
      output$mycontour_fj <- DT::renderDataTable(datatable({
        data <- as.data.frame(fondContour)
        tableau_contour <- data[,c(1:2)]
      },  style = 'bootstrap'
      ))
      
      # ENVOI DU LEAFLET A L'UI
      
      output$mymap_fj <- renderLeaflet({
        react_fond_fj()
      })
    }
    
    runApp(shinyApp(ui = ui, server = server), launch.browser = TRUE)
    }


shiny_saphirs <-
  function(data,fondMaille,typeMaille,fondContour,fondSuppl=NULL,idDataDepart,idDataArrivee,varFlux,direction="Ent",dom="0")
  {
    options("stringsAsFactors"=FALSE)
    
    # Verification des parametres
    msg_error1<-msg_error2<-msg_error3<-msg_error4<-msg_error5<-msg_error6<-msg_error7<-msg_error8<-msg_error9<-msg_error10<-msg_error11<-msg_error12<-msg_error13<-msg_error14<-msg_error15<-msg_error16<-msg_error17<-msg_error18<-msg_error19<-msg_error20 <- NULL
    
    if(any(class(data)!="data.frame")) msg_error1 <- "Les donnees doivent etre dans un data.frame / "
    if(any(!any(class(fondMaille) %in% "sf"),!any(class(fondMaille) %in% "data.frame"))) msg_error2 <- "Le fond de maille doit etre un objet sf / "
    if(any(!any(class(fondContour) %in% "sf"),!any(class(fondContour) %in% "data.frame"))) msg_error3 <- "Le fond de contour doit etre un objet sf / "
    if(!is.null(fondSuppl)) if(any(!any(class(fondSuppl) %in% "sf"),!any(class(fondSuppl) %in% "data.frame"))) msg_error4 <- "Le fond supplementaire doit etre un objet sf / "
    if(any(class(idDataDepart)!="character")) msg_error5 <- "Le nom de la variable de depart doit etre de type caractere / "
    if(any(class(idDataArrivee)!="character")) msg_error6 <- "Le nom de la variable d'arrivee doit etre de type caractere / "
    if(any(class(varFlux)!="character")) msg_error7 <- "Le nom de la variable doit etre de type caractere / "
    if(any(class(typeMaille)!="character")) msg_error8 <- "La valeur doit etre de type caractere ('REG', 'DEP', 'ZE', 'AU', 'BV', 'UU', 'EPCI' ou 'DEPCOM') / "
    if(any(class(direction)!="character")) msg_error9 <- "La valeur doit etre de type caractere ('Ent', 'Sor' ou 'Sol') / "
    if(any(class(dom)!="character")) msg_error10 <- "La valeur doit etre de type caractere ('0', '971', '972', '973', '974' ou '976') / "
    
    if(length(names(data))<3) msg_error11 <- "Le tableau des donnees n'est pas conforme. Il doit contenir au minimum une variable identifiant de depart, une variable identifiant d'arrivee et la variable a representer / "
    if(length(names(fondMaille))<3) msg_error12 <- "Le fond de maille n'est pas conforme. La table doit contenir au minimum une variable identifiant, une variable libelle et la geometry / "
    if(length(names(fondContour))<3) msg_error13 <- "Le fond de contour n'est pas conforme. La table doit contenir au minimum une variable identifiant, une variable libelle et la geometry / "
    if(!is.null(fondSuppl)) if(length(names(fondSuppl))<3) msg_error14 <- "Le fond supplementaire n'est pas conforme. La table doit contenir au minimum une variable identifiant, une variable libelle et la geometry / "
    
    if(!any(names(data) %in% idDataDepart))  msg_error15 <- "La variable identifiant de depart n'existe pas dans la table des donnees / "
    if(!any(names(data) %in% idDataArrivee))  msg_error16 <- "La variable identifiant d'arrivee n'existe pas dans la table des donnees / "
    if(!any(names(data) %in% varFlux))  msg_error17 <- "La variable a representer n'existe pas dans la table des donnees / "
    if(!typeMaille %in% c("REG", "DEP", "ZE", "AU", "BV", "UU", "EPCI", "DEPCOM")) msg_error18 <- "La variable typeMaille doit etre 'REG', 'DEP', 'ZE', 'AU', 'BV', 'UU', 'EPCI' ou 'DEPCOM' / "
    if(!direction %in% c("Ent", "Sor", "Sol")) msg_error19 <- "La variable direction doit etre 'Ent', 'Sor' ou 'Sol' / "
    if(!dom %in% c("0","971","972","973","974","976")) msg_error20 <- "La variable dom doit etre '0', '971', '972', '973', '974' ou '976' / "
    
    if(any(!is.null(msg_error1),!is.null(msg_error2),!is.null(msg_error3),!is.null(msg_error4),
           !is.null(msg_error5),!is.null(msg_error6),!is.null(msg_error7),!is.null(msg_error8),
           !is.null(msg_error10),!is.null(msg_error11),!is.null(msg_error12),!is.null(msg_error13),
           !is.null(msg_error14),!is.null(msg_error15),!is.null(msg_error16),!is.null(msg_error17),
           !is.null(msg_error18),!is.null(msg_error19),!is.null(msg_error20)))
    {
      stop(simpleError(paste0(msg_error1,msg_error2,msg_error3,msg_error4,msg_error5,msg_error6,msg_error7,msg_error8,
                              msg_error10,msg_error11,msg_error12,msg_error13,msg_error14,msg_error15,msg_error16,
                              msg_error17,msg_error18,msg_error19,msg_error20)))
    }
    
    nb_up <- reactiveValues(a=0)
    nb_down <- reactiveValues(a=0)
    ordre_analyse <- reactiveValues(a=1,b=2)
    insert_save <- reactiveValues(a=0)
    remove_carte <- reactiveValues(a=0)
    liste_fonds <- reactiveValues(a=c("analyse","maille","contour"))
    m_save_fs <- reactiveValues(a=0)
    
    flux_min <- reactiveValues(a=100)
    distance_max <- reactiveValues(a=300)
    flux_majeur <- reactiveValues(a=10)
    largeur <- reactiveValues(a=0)
    longueur <- reactiveValues(a=3000)
    
    if (typeMaille=="REG") largeur$a<-100
    if (typeMaille=="DEP") largeur$a<-30
    if (substr(typeMaille,1,2)=="ZE") largeur$a<-10
    if (substr(typeMaille,1,2)=="AU") largeur$a<-6
    if (substr(typeMaille,1,2)=="BV") largeur$a<-6
    if (substr(typeMaille,1,2)=="UU") largeur$a<-6
    if (typeMaille=="EPCI") largeur$a<-4
    if (typeMaille=="DEPCOM") largeur$a<-2
    
    erreur_maille <- reactiveValues(a=FALSE)
    
    legende <- reactiveValues(a=NULL)
    
    sourc <- "Source : Insee"
    
    names(data)[names(data)==idDataDepart] <- "CODE1"
    names(data)[names(data)==idDataArrivee] <- "CODE2"
    names(fondMaille)[1] <- "CODE"
    names(fondMaille)[2] <- "LIBELLE"
    names(fondContour)[1] <- "CODE"
    names(fondContour)[2] <- "LIBELLE"
    if(!is.null(fondSuppl)) 
    {
      names(fondSuppl)[1] <- "CODE"
      names(fondSuppl)[2] <- "LIBELLE"
      fondSuppl$LIBELLE<-iconv(fondSuppl$LIBELLE,"latin1","utf8")
    }
    
    fondMaille$LIBELLE<-iconv(fondMaille$LIBELLE,"latin1","utf8")
    fondContour$LIBELLE<-iconv(fondContour$LIBELLE,"latin1","utf8")
    
    ui <- navbarPage("OCEANIS", id="menu",
                     
                     theme = shinytheme("superhero"),
                     
                     tabPanel("Carte",value="carte",
                              sidebarLayout(
                                
                                sidebarPanel(width = 3,
                                             style = "overflow-y:scroll; min-height: 840px; max-height: 840px",
                                             h4(HTML("<b><font color=#95BAE2>VARIABLES</font></b>")),
                                             uiOutput("variable_flux_fs"),
                                             tags$hr(style="border: 5px solid #5182B6"), #337ab7
                                             h4(HTML("<b><font color=#95BAE2>FONDS</font></b>")),
                                             fluidRow(
                                               column(width=9, offset=0.5,
                                                      uiOutput("ordre_fonds_fs")
                                               ),
                                               column(width=1,
                                                      br(),
                                                      br(),
                                                      htmlOutput("monter_fond_fs", inline=FALSE),
                                                      htmlOutput("descendre_fond_fs", inline=FALSE)
                                               )
                                             ),
                                             uiOutput("ajout_territoire_fs"),
                                             uiOutput("ajout_reg_fs"),
                                             uiOutput("ajout_dep_fs"),
                                             tags$hr(style="border: 5px solid #5182B6"),
                                             h4(HTML("<b><font color=#95BAE2>FLUX</font></b>")),
                                             uiOutput("flux_min_fs"),
                                             uiOutput("largeur_fs"),
                                             tags$hr(style="border: 5px solid #5182B6"),
                                             h4(HTML("<b><font color=#95BAE2>L\u00c9GENDE</font></b>")),
                                             uiOutput("titre_flux_legende_fs"),
                                             br(),
                                             uiOutput("affiche_legende_fs"),
                                             br(),
                                             tags$hr(style="border: 5px solid #5182B6"),
                                             h4(HTML("<b><font color=#95BAE2>SAUVEGARDE</font></b>")),
                                             uiOutput("save_carte_fs"),
                                             br(),
                                             conditionalPanel(condition = 'input.mymap_fs_click',
                                                              tags$div(class="dropup",
                                                                       HTML('<button class="btn btn-primary dropdown-toggle" type="button" data-toggle="dropdown" aria-haspopup="true" aria-expanded="false">
                                                                            Exporter en projet Qgis
                                                                            <span class="caret"></span>
                                                                            </button>'),
                                                                       tags$ul(class="dropdown-menu",
                                                                               wellPanel(
                                                                                 style="background: #2B3E50; width:340px",
                                                                                 h4("Export de la carte en projet Qgis"),
                                                                                 br(),
                                                                                 uiOutput("sortie_qgis_fs"),
                                                                                 br(),
                                                                                 uiOutput("titre1_qgis_fs"),
                                                                                 uiOutput("titre2_qgis_fs"),
                                                                                 uiOutput("source_qgis_fs"),
                                                                                 tags$head(tags$style(HTML('#export_qgis_fs_id{background-color:#337ab7}'))),
                                                                                 uiOutput("export_qgis_fs")
                                                                               )
                                                                       )
                                                                       )
                                             ),
                                             br(),
                                             uiOutput("aide_image_fs"),
                                             br()
                              ),
                              mainPanel(
                                tags$head(
                                  tags$style(HTML(".leaflet-container { background: #AFC9E0; }"))
                                ),
                                tabsetPanel(id="onglets_fs",
                                            tabPanel(title=HTML("<b>Carte</b>"),value="carte",
                                                     leafletOutput("mymap_fs",width="100%",height = 800)
                                            ),
                                            tabPanel(title=HTML(paste0("<b>Donn","\u00e9","es</b>")),value="donnees",
                                                     h5("S\u00e9lectionnez une ou plusieurs lignes pour ensuite les visualiser sur la carte."),
                                                     DT::dataTableOutput("mydonnees_fs",width="100%",height = 800)),
                                            tabPanel(title=HTML("<b>Maille</b>"),value="maille",
                                                     h5("S\u00e9lectionnez une ou plusieurs lignes pour ensuite les visualiser sur la carte."),
                                                     DT::dataTableOutput("mymaille_fs",width="100%",height = 800)),
                                            tabPanel(title=HTML("<b>Contour</b>"),value="contour",
                                                     h5("S\u00e9lectionnez une ou plusieurs lignes pour ensuite les visualiser sur la carte."),
                                                     DT::dataTableOutput("mycontour_fs",width="100%",height = 800))
                                )
                              )
    )
    )
    )
    
    server <- function(input, output, session) {
      
      #################
      #Onglet Carte
      #################
      
      #Charge les donnees et fonds en memoire et affiche les widgets adequats dans le sidePanel de l'onglet "Carte"
      observe({
        
        # VARIABLES
        
        output$variable_flux_fs <- renderUI({
          selectInput("variable_flux_fs_id", label=h5("Variable des flux (en volume)"), choices = varFlux, selected = varFlux)
        })
        
        # FONDS
        
        output$ordre_fonds_fs <- renderUI({
          selectInput("ordre_fonds_fs_id", label=h5("Modifier l'ordre des fonds"), choices = liste_fonds$a, multiple=TRUE, selectize=FALSE, selected = "analyse")
        })
        output$monter_fond_fs <- renderUI({
          actionButton("monter_fond_fs_id", label="", icon=icon("arrow-up"))
        })
        output$descendre_fond_fs <- renderUI({
          actionButton("descendre_fond_fs_id", label="", icon=icon("arrow-down"))
        })
        
        output$ajout_territoire_fs <- renderUI({
          checkboxInput("ajout_territoire_fs_id", label = "Afficher le fond des territoires",
                        value = if(is.null(fondSuppl)) FALSE else TRUE)
        })
        output$ajout_reg_fs <- renderUI({
          checkboxInput("ajout_reg_fs_id", label = "Afficher le fond des r\u00e9gions",
                        value = FALSE)
        })
        output$ajout_dep_fs <- renderUI({
          checkboxInput("ajout_dep_fs_id", label = "Afficher le fond des d\u00e9partements",
                        value = FALSE)
        })
        
        # FLUX
        
        if(direction!="Sol")
        {
          output$flux_min_fs <- renderUI({
            numericInput("flux_min_fs_id", label = h5("Seuil de flux minimal"), value=flux_min$a, step=10)
          })
        }
        
        output$largeur_fs <- renderUI({
          numericInput("largeur_fs_id", label = h5("Largeur de la fleche la plus large (km)"), value=largeur$a, step=10)
        })
        
        # LEGENDE
        
        output$titre_flux_legende_fs <- renderUI({
          textInput("titre_flux_legende_fs_id", label = h5("Titre de la l\u00e9gende des flux"), value = "")
        })
        
        output$affiche_legende_fs <- renderUI({
          checkboxInput("affiche_legende_fs_id", label = "Activer le d\u00e9placement de la l\u00e9gende au clic",
                        value = TRUE)
        })
        
        # SAUVEGARDE
        
        output$save_carte_fs <- renderUI({
          actionButton("save_carte_fs_id", label=HTML("<font size=3>Sauvegarder la carte dans un onglet</font>"), style="color:#FFFFFF; background-color:#DF691A")
        })
        
        output$entrees_qgis_fs <- renderUI({
          actionButton("entrees_qgis_fs_id", label="Exporter en projet Qgis")
        })
        
        output$sortie_qgis_fs <- renderUI({
          tags$div(class="input-group",
                   HTML('<input type="text" id="sortie_qgis_fs_id" class="form-control" placeholder="Nom du projet" aria-describedby="sortie_qgis_fs_id">
                        <span class="input-group-addon" id="sortie_qgis_fs_id">.qgs</span>'))
        })
        
        output$titre1_qgis_fs <- renderUI({
          textInput("titre1_qgis_fs_id", label = h5("Titre informatif"), value = "", placeholder= "Facultatif")
        })
        
        output$titre2_qgis_fs <- renderUI({
          textInput("titre2_qgis_fs_id", label = h5("Titre descriptif"), value = "", placeholder= "Facultatif")
        })
        
        output$source_qgis_fs <- renderUI({
          textInput("source_qgis_fs_id", label = h5("Source de la carte"), value = sourc)
        })
        
        output$aide_image_fs <- renderUI({
          tags$div(class="dropup",
                   HTML(paste0('<button class="btn btn-primary dropdown-toggle" type="button" data-toggle="dropdown">
                               <i class="fa fa-book fa-fw" aria-hidden="true"></i>
                               Proc','\u00e9','dure pour capture d\'','\u00e9','cran
                               <span class="caret"></span>
                               </button>')),
                   tags$ul(class="dropdown-menu",
                           wellPanel(
                             style="background: #2B3E50; width:340px",
                             div(
                               HTML("<font size=2>Deux possibilit\u00e9s :</font>"),
                               br(),
                               br(),
                               strong(HTML("<font size=3>Par l'Outil Capture</font>")),
                               br(),
                               HTML("<font size=2>1- Ouvrir le logiciel Outil Capture sur le Poste de travail (Menu D\u00e9marrer > Tous les programmes > Accessoires > Outil Capture).</font>"),
                               br(),
                               HTML(paste0("<font size=2>2- S\u00e9lectionner la zone \u00e0 capturer (vous aurez besoin d'adapter la fen\u00eatre AUS pour avoir \u00e0 la fois la carte et la barre des t","\u00e2","ches).</font>")),
                               br(),
                               HTML("<font size=2>3- Enregistrer l'image ou copier la dans le presse-papier.</font>"),
                               br(),
                               br(),
                               strong(HTML(paste0("<font size=3>Par impression d'","\u00e9","cran</font>"))),
                               br(),
                               HTML("<font size=2>1- Appuyer sur la touche clavier \"Impr ecran\".</font>"),
                               br(),
                               HTML("<font size=2>2- Ouvrir un logiciel de retouche image (Paint par exemple).</font>"),
                               br(),
                               HTML("<font size=2>3- Coller l'image et l'enregistrer au format voulu (.jpg, .png, .bmp).</font>")
                             )
                           )
                   )
                   )
        })
        })
      
      # Pour modifier l'ordre des fonds
      observeEvent(list(input$monter_fond_fs_id,input$descendre_fond_fs_id),{
        
        if(as.numeric(input$monter_fond_fs_id)==0 & as.numeric(input$descendre_fond_fs_id)==0) return(NULL)
        ordre <- c()
        if(as.numeric(input$monter_fond_fs_id)>nb_up$a)
        {
          ordre <- c(2,3)
          nb_up$a <- nb_up$a+1
        }
        if(as.numeric(input$descendre_fond_fs_id)>nb_down$a)
        {
          ordre <- c(1,2)
          nb_down$a <- nb_down$a+1
        }
        
        if(is.null(input$ordre_fonds_fs_id)) pos_select <- 0 else pos_select <- which(liste_fonds$a==input$ordre_fonds_fs_id)
        
        if(pos_select>0)
        {
          if(pos_select==ordre[1]) liste_fonds$a <- liste_fonds$a[c(2,1,3)]
          if(pos_select==ordre[2]) liste_fonds$a <- liste_fonds$a[c(1,3,2)]
          
          updateSelectInput(session, "ordre_fonds_fs_id",
                            choices = liste_fonds$a,
                            selected = input$ordre_fonds_fs_id
          )
        }
      },ignoreInit = TRUE)
      
      flux_min_fs <- reactive({
        
        if(is.null(input$flux_min_fs_id)) return(flux_min$a)
        
        return(input$flux_min_fs_id)
      })
      
      # Pour exporter la carte en projet Qgis
      
      output$export_qgis_fs <- renderUI({
        downloadButton("downloadProjetQgis_fs", label="Exporter")
      })
      
      output$downloadProjetQgis_fs <- downloadHandler(contentType = "zip",
                                                      filename = function(){
                                                        paste0(input$sortie_qgis_fs_id,".zip")
                                                      },
                                                      content = function(file){
                                                        files <- EXPORT_PROJET_QGIS_FS(file)
                                                        
                                                        zip(file,files, flags = "-j9X")
                                                      }
      )
      
      # Pour exporter la carte en projet Qgis
      EXPORT_PROJET_QGIS_FS <- function(file)
      {
        showModal(modalDialog(HTML("<i class=\"fa fa-spinner fa-spin fa-2x fa-fw\"></i> <font size=+1>Export du projet Qgis en cours...</font> "), size="m", footer=NULL, style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"))
        
        sortie <- input$sortie_qgis_fs_id
        rep_sortie <- dirname(file)
        files <- c(paste0(rep_sortie,"/",sortie,".qgs"))
        
        rupture <- FALSE
        
        donnees <- analyse_fs()[[1]]$save
        if(max(donnees)>0 & min(donnees)<0) rupture <- TRUE
        
        if(rupture)
        {
          fond_flux_sortie <- st_sf(analyse_fs()[[1]][analyse_fs()[[1]]$save<0,])
          fond_flux_entree <- st_sf(analyse_fs()[[1]][analyse_fs()[[1]]$save>=0,])
          
          fond_flux_sortie <- st_transform(fond_flux_sortie,paste0("+init=epsg:",code_epsg_fs()))
          fond_flux_entree <- st_transform(fond_flux_entree,paste0("+init=epsg:",code_epsg_fs()))
          
          suppressWarnings(write_fond_flux_sortie <- try(st_write(fond_flux_sortie, paste0(rep_sortie,"fond_flux_sortie.shp"), delete_dsn = TRUE, quiet = TRUE),silent=TRUE))
          suppressWarnings(write_fond_flux_entree <- try(st_write(fond_flux_entree, paste0(rep_sortie,"fond_flux_entree.shp"), delete_dsn = TRUE, quiet = TRUE),silent=TRUE))
        }
        if(min(donnees)>=0)
        {
          fond_flux_entree <- st_sf(analyse_fs()[[1]])
          fond_flux_entree <- st_transform(fond_flux_entree,paste0("+init=epsg:",code_epsg_fs()))
          suppressWarnings(write_fond_flux_entree <- try(st_write(fond_flux_entree, paste0(rep_sortie,"fond_flux_entree.shp"), delete_dsn = TRUE, quiet = TRUE),silent=TRUE))
        }
        if(max(donnees)<0)
        {
          fond_flux_sortie <- st_sf(analyse_fs()[[1]])
          fond_flux_sortie <- st_transform(fond_flux_sortie,paste0("+init=epsg:",code_epsg_fs()))
          suppressWarnings(write_fond_flux_sortie <- try(st_write(fond_flux_sortie, paste0(rep_sortie,"fond_flux_sortie.shp"), delete_dsn = TRUE, quiet = TRUE),silent=TRUE))
        }
        
        vmax <- max(abs(data.frame(analyse_fs()[[2]])[,varFlux]))
        
        coord_fleche_max_pl <- st_coordinates(analyse_fs()[[1]][abs(data.frame(analyse_fs()[[1]])[,varFlux])==vmax,])
        large_pl <- max(st_distance(st_sfc(st_point(c(coord_fleche_max_pl[5,1],coord_fleche_max_pl[5,2])),st_point(c(coord_fleche_max_pl[6,1],coord_fleche_max_pl[6,2])))))
        long_pl <- large_pl*1.5
        
        if(large_pl>100000) long_pl <- 100000
        if(large_pl<1000) long_pl <- 1000
        
        flux_leg <- flux_legende_saphirs_pl(lon_lat_fs()[[1]],lon_lat_fs()[[2]],long_pl,large_pl,code_epsg_fs())
        flux_leg <- cbind(flux_leg,VALEUR=c(max(data[,varFlux]),max(data[,varFlux])/3))
        fond_flux_leg <- flux_leg
        
        fond_maille <- st_transform(fondMaille,paste0("+init=epsg:",code_epsg_fs()))
        fond_contour <- st_transform(fondContour,paste0("+init=epsg:",code_epsg_fs()))
        if(!is.null(fondSuppl) && input$ajout_territoire_fs_id) fond_territoire <- st_transform(fond_territoire_fs(),paste0("+init=epsg:",code_epsg_fs()))
        if(input$ajout_dep_fs_id) fond_departement <- st_transform(fond_departement_fs(),paste0("+init=epsg:",code_epsg_fs()))
        if(input$ajout_reg_fs_id) fond_region <- st_transform(fond_region_fs(),paste0("+init=epsg:",code_epsg_fs()))
        fond_france <- st_transform(fond_habillage_fs()[[1]],paste0("+init=epsg:",code_epsg_fs()))
        fond_pays <- st_transform(fond_habillage_fs()[[2]],paste0("+init=epsg:",code_epsg_fs()))
        
        if(rupture | min(donnees)>=0) st_write(fond_flux_entree, paste0(rep_sortie,"/fond_flux_entree.shp"), delete_dsn = TRUE, quiet = TRUE)
        if(rupture | max(donnees)<0) st_write(fond_flux_sortie, paste0(rep_sortie,"/fond_flux_sortie.shp"), delete_dsn = TRUE, quiet = TRUE)
        st_write(fond_flux_leg, paste0(rep_sortie,"/fond_flux_leg.shp"), delete_dsn = TRUE, quiet = TRUE)
        suppressWarnings(st_write(fond_maille, paste0(rep_sortie,"/fond_maille.shp"), delete_dsn = TRUE, quiet = TRUE))
        suppressWarnings(st_write(fond_contour,paste0(rep_sortie,"/fond_contour.shp"), delete_dsn = TRUE, quiet = TRUE))
        if(exists("fond_territoire")) if(!is.null(fond_territoire)) st_write(fond_territoire, paste0(rep_sortie,"/fond_territoire.shp"), delete_dsn = TRUE, quiet = TRUE)
        if(exists("fond_departement")) if(!is.null(fond_departement)) st_write(fond_departement, paste0(rep_sortie,"/fond_departement.shp"), delete_dsn = TRUE, quiet = TRUE)
        if(exists("fond_region")) if(!is.null(fond_region)) st_write(fond_region,paste0(rep_sortie,"/fond_region.shp"), delete_dsn = TRUE, quiet = TRUE)
        st_write(fond_france,paste0(rep_sortie,"/fond_france.shp"), delete_dsn = TRUE, quiet = TRUE)
        if(exists("fond_pays")) if(!is.null(fond_pays)) st_write(fond_pays,paste0(rep_sortie,"/fond_pays.shp"), delete_dsn = TRUE, quiet = TRUE)
        
        if(rupture | min(donnees)>=0) files <- c(paste0(rep_sortie,"/fond_flux_entree.shp"),paste0(rep_sortie,"/fond_flux_entree.dbf"),paste0(rep_sortie,"/fond_flux_entree.prj"),paste0(rep_sortie,"/fond_flux_entree.shx"),files)
        if(rupture | max(donnees)<0) files <- c(paste0(rep_sortie,"/fond_flux_sortie.shp"),paste0(rep_sortie,"/fond_flux_sortie.dbf"),paste0(rep_sortie,"/fond_flux_sortie.prj"),paste0(rep_sortie,"/fond_flux_sortie.shx"),files)
        files <- c(paste0(rep_sortie,"/fond_flux_leg.shp"),paste0(rep_sortie,"/fond_flux_leg.dbf"),paste0(rep_sortie,"/fond_flux_leg.prj"),paste0(rep_sortie,"/fond_flux_leg.shx"),files)
        files <- c(paste0(rep_sortie,"/fond_maille.shp"),paste0(rep_sortie,"/fond_maille.dbf"),paste0(rep_sortie,"/fond_maille.prj"),paste0(rep_sortie,"/fond_maille.shx"),files)
        files <- c(paste0(rep_sortie,"/fond_contour.shp"),paste0(rep_sortie,"/fond_contour.dbf"),paste0(rep_sortie,"/fond_contour.prj"),paste0(rep_sortie,"/fond_contour.shx"),files)
        if(exists("fond_territoire")) if(!is.null(fond_territoire)) files <- c(paste0(rep_sortie,"/fond_territoire.shp"),paste0(rep_sortie,"/fond_territoire.dbf"),paste0(rep_sortie,"/fond_territoire.prj"),paste0(rep_sortie,"/fond_territoire.shx"),files)
        if(exists("fond_departement")) if(!is.null(fond_departement)) files <- c(paste0(rep_sortie,"/fond_departement.shp"),paste0(rep_sortie,"/fond_departement.dbf"),paste0(rep_sortie,"/fond_departement.prj"),paste0(rep_sortie,"/fond_departement.shx"),files)
        if(exists("fond_region")) if(!is.null(fond_region)) files <- c(paste0(rep_sortie,"/fond_region.shp"),paste0(rep_sortie,"/fond_region.dbf"),paste0(rep_sortie,"/fond_region.prj"),paste0(rep_sortie,"/fond_region.shx"),files)
        files <- c(paste0(rep_sortie,"/fond_france.shp"),paste0(rep_sortie,"/fond_france.dbf"),paste0(rep_sortie,"/fond_france.prj"),paste0(rep_sortie,"/fond_france.shx"),files)
        if(exists("fond_pays")) if(!is.null(fond_pays)) files <- c(paste0(rep_sortie,"/fond_pays.shp"),paste0(rep_sortie,"/fond_pays.dbf"),paste0(rep_sortie,"/fond_pays.prj"),paste0(rep_sortie,"/fond_pays.shx"),files)
        
        chemin_fonds <- rep_sortie
        titre1 <- paste0(input$titre1_qgis_fs_id,"\n")
        titre2 <- input$titre2_qgis_fs_id
        source <- input$source_qgis_fs_id
        annee <- format(Sys.time(), format = "%Y")
        variable_a_representer <- varFlux
        
        l <- c()
        if(exists("fond_territoire")) l <- "fond_territoire"
        if(exists("fond_departement")) l <- c(l,"fond_departement")
        if(exists("fond_region")) l <- c(l,"fond_region")
        
        l=c("fond_france",
            "fond_contour",
            "fond_maille",
            l)
        
        if(rupture | min(donnees)>=0) l=c(l,"fond_flux_entree")
        if(rupture | max(donnees)<0) l=c(l,"fond_flux_sortie")
        
        l <- c(l,"fond_flux_leg")
        
        if(exists("fond_pays")) l <- c(l,"fond_pays")
        
        export_projet_qgis_fleches_saphirs(l,rep_sortie,sortie,titre1,titre2,source,"#CD853F","#6495ED","#303030",annee)
        
        removeModal()
        
        showModal(modalDialog(HTML(paste0("<font size=+1>Le projet Qgis a \u00e9t\u00e9 cr","\u00e9","ee.</font>")), size="m", footer=NULL, easyClose = TRUE, style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"))
        
        return(files)
      }
      
      code_epsg_fs <- reactive({
        code_epsg <- switch(dom, #DOM
                            "0"="2154",# Lambert 93
                            "971"="32620",# UTM 20 N
                            "972"="32620",# UTM 20 N
                            "973"="2972",# UTM 22 N
                            "974"="2975",# UTM 40 S
                            "976"="4471")# UTM 38 S
        return(code_epsg)
      })
      
      largeur_fs <- reactive({
        
        if(is.null(input$largeur_fs_id))
        {
          largeur <- largeur$a
        }else if(is.na(input$largeur_fs_id))
        {
          largeur <- largeur$a
        }else
        {
          largeur <- input$largeur_fs_id
        }
        return(largeur)
      })
      
      largeur_max_fs <- reactive({
        
        if(is.null(input$largeur_fs_id)) return(NULL)
        
        if (is.na(input$largeur_fs_id)) largeur$a<-1
        
        if (typeMaille=="REG") largeur$a<-100
        if (typeMaille=="DEP") largeur$a<-30
        if (substr(typeMaille,1,2)=="ZE") largeur$a<-10
        if (substr(typeMaille,1,2)=="AU") largeur$a<-6
        if (substr(typeMaille,1,2)=="BV") largeur$a<-6
        if (substr(typeMaille,1,2)=="UU") largeur$a<-6
        if (typeMaille=="EPCI") largeur$a<-4
        if (typeMaille=="DEPCOM") largeur$a<-2
        
        if(!is.na(input$largeur_fs_id))
        {
          if(input$largeur_fs_id==0)
          {
            largeur_max <- largeur$a*1000
          }else
          {
            largeur_max <- input$largeur_fs_id*1000
          }
        }else
        {
          largeur_max <- 1
        }
        
        return(largeur_max)
      })
      
      analyse_fs <- reactive({
        
        if(is.null(largeur_max_fs())) return(NULL)
        
        if(typeMaille=="REG") longueur$a<-100000
        if(typeMaille=="DEP") longueur$a<-30000
        if(!typeMaille %in% c("France metro","REG","DEP")) longueur$a<-10000
        
        suppressWarnings(test_k_saphir <- try(k_saphir(fondMaille,names(fondMaille)[1],data,"CODE1","CODE2",varFlux,largeur_max_fs(),longueur$a,direction),silent=TRUE))
        
        if(!class(test_k_saphir) %in% "try-error")
        {
          analyse<-k_saphir(fondMaille,names(fondMaille)[1],data,"CODE1","CODE2",varFlux,largeur_max_fs(),longueur$a,direction)
        }else
        {
          showModal(modalDialog(HTML(paste0("<font size=+1>La maille ne correspond pas au niveau g\u00e9ographique du fichier de donn","\u00e9","es.<br><br>Veuillez svp choisir une maille adapt","\u00e9","e ou modifier le fichier de donn","\u00e9","es.</font>")), size="l", footer=NULL, easyClose = TRUE, style = "color: #fff; background-color: #DF691A; border-color: #2e6da4")) #337ab7
          erreur_maille$a <- TRUE
          return(NULL)
        }
        
        if(is.null(analyse))
        {
          showModal(modalDialog(HTML(paste0("<font size=+1>La maille ne correspond pas au niveau g\u00e9ographique du fichier de donn","\u00e9","es.<br><br>Veuillez svp choisir une maille adapt","\u00e9","e ou modifier le fichier de donn","\u00e9","es.</font>")), size="l", footer=NULL, easyClose = TRUE, style = "color: #fff; background-color: #DF691A; border-color: #2e6da4")) #337ab7
          erreur_maille$a <- TRUE
          return(NULL)
        }
        
        analyse_WGS84 <- st_transform(analyse[[1]],"+init=epsg:4326 +proj=longlat +ellps=WGS84")
        
        return(list(analyse[[1]],analyse_WGS84))
      })
      
      fond_habillage_fs <- reactive({
        
        if(dom=="0")
        {
          pays <- st_transform(sf_paysm(),"+init=epsg:4326 +proj=longlat +ellps=WGS84")
          fra <- st_transform(sf_fram(),"+init=epsg:4326 +proj=longlat +ellps=WGS84")
        }else
        {
          if(dom=="971")
          {
            fra <- st_transform(sf_reg01(),"+init=epsg:4326 +proj=longlat +ellps=WGS84")
            pays <- fra
          }
          if(dom=="972")
          {
            fra <- st_transform(sf_reg02(),"+init=epsg:4326 +proj=longlat +ellps=WGS84")
            pays <- fra
          }
          if(dom=="973")
          {
            fra <- st_transform(sf_reg03(),"+init=epsg:4326 +proj=longlat +ellps=WGS84")
            pays <- st_transform(sf_pays973(),"+init=epsg:4326 +proj=longlat +ellps=WGS84")
          }
          if(dom=="974")
          {
            fra <- st_transform(sf_reg04(),"+init=epsg:4326 +proj=longlat +ellps=WGS84")
            pays <- fra
          }
          if(dom=="976")
          {
            fra <- st_transform(sf_reg06(),"+init=epsg:4326 +proj=longlat +ellps=WGS84")
            pays <- fra
          }
        }
        fond_france <- fra
        fond_pays <- pays
        
        return(list(fond_france,fond_pays))
      })
      
      fond_contour_maille_fs <- reactive({
        
        test_contour <- try(st_transform(fondContour,"+init=epsg:4326 +proj=longlat +ellps=WGS84"), silent = TRUE)
        test_maille <- try(st_transform(fondMaille,"+init=epsg:4326 +proj=longlat +ellps=WGS84"), silent = TRUE)
        
        if(any(list(class(test_contour),class(test_maille)) %in% "try-error"))
        {
          showModal(modalDialog(HTML(paste0("<font size=+1>Une erreur est survenue dans la cr","\u00e9","ation du territoire.<br><br>Veuillez svp v\u00e9rifier vos donn","\u00e9","es et les variables choisies.</font>")), size="m", footer=NULL, easyClose = TRUE, style = "color: #fff; background-color: #DF691A; border-color: #2e6da4"))
          erreur_maille$a <- TRUE
          return(NULL)
        }else
        {
          contour_WGS84 <- st_transform(fondContour,"+init=epsg:4326 +proj=longlat +ellps=WGS84")
          maille_WGS84 <- st_transform(fondMaille,"+init=epsg:4326 +proj=longlat +ellps=WGS84")
        }
        
        return(list(contour_WGS84,maille_WGS84))
      })
      
      list_bbox_fs <- reactive({
        req(fond_contour_maille_fs())
        
        list_bbox <- list(c(st_bbox(fond_contour_maille_fs()[[1]])[1],st_bbox(fond_contour_maille_fs()[[1]])[3]),c(st_bbox(fond_contour_maille_fs()[[1]])[2],st_bbox(fond_contour_maille_fs()[[1]])[4]))
        return(list_bbox)
      })
      
      fond_territoire_fs <- reactive({
        if(!is.null(fondSuppl))
        {
          fond_territoire <- st_transform(fondSuppl,"+init=epsg:4326 +proj=longlat +ellps=WGS84")
          return(fond_territoire)
        }else
        {
          return(NULL)
        }
      })
      
      fond_region_fs <- reactive({
        fond_region <- st_transform(sf_regm(),"+init=epsg:4326 +proj=longlat +ellps=WGS84")
        return(fond_region)
      })
      
      fond_departement_fs <- reactive({
        fond_departement <- st_transform(sf_depm(),"+init=epsg:4326 +proj=longlat +ellps=WGS84")
        return(fond_departement)
      })
      
      fond_select_donnees_fs <- reactive({
        req(analyse_fs())
        
        fond_donnees <- analyse_fs()[[1]][input$mydonnees_fs_rows_selected,]
        fond_donnees <- st_transform(fond_donnees,"+init=epsg:4326 +proj=longlat +ellps=WGS84")
        return(fond_donnees)
      })
      
      fond_select_maille_fs <- reactive({
        req(fond_contour_maille_fs())
        
        fond_maille <- fond_contour_maille_fs()[[2]][as.data.frame(fond_contour_maille_fs()[[2]])[,"CODE"] %in% as.data.frame(fondMaille)[input$mymaille_fs_rows_selected,"CODE"],]
        return(fond_maille)
      })
      
      fond_select_contour_fs <- reactive({
        req(fond_contour_maille_fs())
        
        fond_contour <- fond_contour_maille_fs()[[1]][as.data.frame(fond_contour_maille_fs()[[1]])[,"CODE"] %in% as.data.frame(fondContour)[input$mycontour_fs_rows_selected,"CODE"],]
        return(fond_contour)
      })
      
      # Pour construire la map en leaflet
      react_fond_fs <- reactive({
        
        if(is.null(analyse_fs())) return(NULL)
        
        if(input$menu=="carte")
        {
          showModal(modalDialog(HTML("<i class=\"fa fa-spinner fa-spin fa-2x fa-fw\"></i><font size=+1>\u00c9laboration de la carte...</font> "), size="m", footer=NULL, style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"))
          
          # Construction de la map par defaut
          
          m <- leaflet(padding = 0,
                       options = leafletOptions(
                         preferCanvas = TRUE,
                         transition = 2,
                         minZoom = 6,
                         maxZoom = 10
                       )) %>%
            
            setMapWidgetStyle(list(background = "#AFC9E0")) %>%
            
            addTiles_insee(attribution = paste0("<a href=\"http://www.insee.fr\">\u00A9 IGN - INSEE ",format(Sys.time(), format = "%Y"),"</a>")) %>%
            
            fitBounds(lng1 = min(list_bbox_fs()[[1]]),
                      lat1 = min(list_bbox_fs()[[2]]),
                      lng2 = max(list_bbox_fs()[[1]]),
                      lat2 = max(list_bbox_fs()[[2]])
            ) %>%
            
            # On ajoute une barre d'echelle
            addScaleBar(position = 'bottomright',
                        options = scaleBarOptions(metric = TRUE, imperial = FALSE)
            ) %>%
            
            # Pour gerer l'ordre des calques
            addMapPane(name = "fond_pays", zIndex = 401) %>%
            addMapPane(name = "fond_france", zIndex = 402) %>%
            addMapPane(name = "fond_habillage", zIndex = 403) %>%
            addMapPane(name = "fond_dep", zIndex = 404) %>%
            addMapPane(name = "fond_reg", zIndex = 405) %>%
            addMapPane(name = "fond_territoire", zIndex = 406) %>%
            addMapPane(name = "fond_trio3", zIndex = 407) %>%
            addMapPane(name = "fond_trio2", zIndex = 408) %>%
            addMapPane(name = "fond_trio1", zIndex = 409) %>%
            addMapPane(name = "selection", zIndex = 410) %>%
            
            addMapPane(name = "fond_legende", zIndex = 411)
          
          # AFFICHAGE DES FONDS D'HABILLAGE
          
          if(dom %in% c("0","973"))
          {
            # fond des pays
            m <- addPolygons(map = m, data = fond_habillage_fs()[[2]][,"LIBGEO"], opacity = 1, # sauf la France
                             stroke = TRUE, color = "white",
                             weight = 1,
                             options = pathOptions(pane = "fond_pays", clickable = F),
                             fill = T, fillColor = "#CCCCCC", fillOpacity = 1
            )
          }
          # fond de la France metro ou d'un DOM
          m <- addPolygons(map = m, data = fond_habillage_fs()[[1]][,"LIBGEO"], opacity = 1, # fond_france
                           stroke = TRUE, color = "black",
                           weight = 1.5,
                           options = pathOptions(pane = "fond_france", clickable = F),
                           fill = T, fillColor = "white", fillOpacity = 1
          )
          
          m_save_fs$a <- m
          
          # AFFICHAGE DU FOND TERRITOIRE
          
          if(!is.null(fondSuppl))
          {
            m <- addPolygons(map = m, data = fond_territoire_fs(),
                             stroke = TRUE, color = "#BFBFBF", opacity = 1,
                             weight = 0.5,
                             options = pathOptions(pane = "fond_territoire", clickable = T),
                             popup = paste0("<b> <font color=#2B3E50>",as.data.frame(fond_territoire_fs())[,"LIBELLE"], "</font> </b>"),
                             fill = T, fillColor = "white", fillOpacity = 0.001,
                             group = "territoire"
            )
          }
          
          # AFFICHAGE DES FONDS CONTOUR ET MAILLE
          
          # fond du contour
          m <- addPolygons(map = m, data = fond_contour_maille_fs()[[1]], opacity = 0.3,
                           stroke = TRUE, color = "black", weight = 3,
                           options = pathOptions(pane = "fond_trio3", clickable = T),
                           popup = paste0("<b> <font color=#2B3E50>",as.data.frame(fond_contour_maille_fs()[[1]])[,"LIBELLE"], "</font> </b>"),
                           fill = T, fillColor = "white", fillOpacity = 0.3,
                           group = "maille_contour"
          )
          
          # fond de la maille
          m <- addPolygons(map = m, data = fond_contour_maille_fs()[[2]], opacity = 1,
                           stroke = TRUE, color = "grey", weight = 1,
                           options = pathOptions(pane = "fond_trio2", clickable = T),
                           popup = paste0("<b> <font color=#2B3E50>",as.data.frame(fond_contour_maille_fs()[[2]])[,"LIBELLE"], "</font> </b>"),
                           fill = T, fillColor = "white", fillOpacity = 0.001,
                           group = "maille_contour"
          )
          
          # AFFICHAGE DE L'ANALYSE
          
          analyse <- isolate(analyse_fs())
          analyse_WGS84 <- analyse[[2]]
          
          if(direction!="Sol")
          {
            analyse_WGS84 <- analyse_WGS84[as.data.frame(analyse_WGS84)[,varFlux]>=100,]
            donnees <- merge(as.data.frame(analyse_WGS84)[,c("CODE1","CODE2")],data,by=c("CODE1","CODE2"),all.x=T)
            donnees <- sort(donnees[,varFlux],decreasing = TRUE)
          }else
          {
            donnees <- as.data.frame(analyse_WGS84)[,varFlux]
          }
          
          m <- addPolylines(map = m,
                            data = analyse_WGS84,
                            stroke = TRUE, color = "#303030",
                            opacity = 1,
                            weight = 1,
                            options = pathOptions(pane = "fond_trio1", clickable = T),
                            popup = paste0("<b><font color=#2B3E50>",varFlux," : ",donnees,"</font></b>"),
                            fill = T, fillColor = sapply(donnees, function(x) if(x>0){"#CD853F"}else{"#6495ED"}), fillOpacity = 1,
                            group = "fleche"
          )
          
          removeModal()
          
          showModal(modalDialog(HTML("<font size=+1>Veuillez patientez svp, la carte va s'afficher dans quelques secondes...<br><br><i class=\"fa fa-hand-pointer-o fa-fw\"></i><b>Double-cliquez</b> ensuite sur la carte pour afficher la l\u00e9gende.</font> "), size="m", footer=NULL, easyClose = TRUE, style = "color: #fff; background-color: #DF691A; border-color: #2e6da4")) #337ab7
          
          return(m)
        }
      })
      
      # MODIFICATION DES FONDS TERRITOIRE D'ETUDES, REGION ET DEPARTEMENT
      
      observeEvent(input$ajout_territoire_fs_id,{
        
        proxy <- leafletProxy("mymap_fs")
        
        clearGroup(map = proxy, group = "territoire")
        
        # fond du territoire d'etudes
        if(!is.null(fondSuppl))
        {
          if(input$ajout_territoire_fs_id)
          {
            proxy <- addPolygons(map = proxy, data = fond_territoire_fs(),
                                 stroke = TRUE, color = "#BFBFBF", opacity = 1,
                                 weight = 0.5,
                                 options = pathOptions(pane = "fond_territoire", clickable = T),
                                 popup = paste0("<b> <font color=#2B3E50>",as.data.frame(fond_territoire_fs())[,"LIBELLE"], "</font> </b>"),
                                 fill = T, fillColor = "white", fillOpacity = 0.001,
                                 group = "territoire"
            )
          }
        }
      },ignoreInit = TRUE)
      
      observeEvent(input$ajout_reg_fs_id,{
        
        proxy <- leafletProxy("mymap_fs")
        
        clearGroup(map = proxy, group = "region")
        
        if(dom=="0")
        {
          if(input$ajout_reg_fs_id)
          {
            #fond des regions
            proxy <- addPolygons(map = proxy, data = fond_region_fs(),
                                 stroke = TRUE, color = "grey", opacity = 1,
                                 weight = 1.5,
                                 options = pathOptions(pane = "fond_reg", clickable = F),
                                 fill = F,
                                 group = "region"
            )
          }
        }
      },ignoreInit = TRUE)
      
      observeEvent(input$ajout_dep_fs_id,{
        
        proxy <- leafletProxy("mymap_fs")
        
        clearGroup(map = proxy, group = "departement")
        
        if(dom=="0")
        {
          if(input$ajout_dep_fs_id)
          {
            # fond des departements
            proxy <- addPolygons(map = proxy, data = fond_departement_fs(),
                                 stroke = TRUE, color = "grey", opacity = 1,
                                 weight = 0.5,
                                 options = pathOptions(pane = "fond_dep", clickable = F),
                                 fill = F,
                                 group = "departement"
            )
          }
        }
      },ignoreInit = TRUE)
      
      # MODIFICATION DE L'ORDRE DES CALQUES
      
      observeEvent(list(input$monter_fond_fs_id,input$descendre_fond_fs_id),{
        
        if(as.numeric(input$monter_fond_fs_id)==0 & as.numeric(input$descendre_fond_fs_id)==0) return(NULL)
        
        proxy <- leafletProxy("mymap_fs")
        
        clearGroup(map = proxy, group = "maille_contour")
        clearGroup(map = proxy, group = "fleche")
        
        i <- 1 #pour gerer l'ordre des fonds dans le pane
        
        for(fond in liste_fonds$a)
        {
          if(fond=="analyse")
          {
            analyse_WGS84 <- analyse_fs()[[2]]
            
            if(direction!="Sol")
            {
              analyse_WGS84 <- analyse_WGS84[as.data.frame(analyse_WGS84)[,varFlux]>=flux_min_fs(),]
              donnees <- merge(as.data.frame(analyse_WGS84)[,c("CODE1","CODE2")],data,by=c("CODE1","CODE2"),all.x=T)
              donnees <- sort(donnees[,varFlux],decreasing = TRUE)
            }else
            {
              donnees <- as.data.frame(analyse_WGS84)[,varFlux]
            }
            
            proxy <- addPolylines(map = proxy,
                                  data = analyse_WGS84,
                                  stroke = TRUE, color = "#303030",
                                  opacity = 1,
                                  weight = 1,
                                  options = pathOptions(pane = paste0("fond_trio",i), clickable = T),
                                  popup = paste0("<b><font color=#2B3E50>",varFlux," : ",donnees,"</font></b>"),
                                  fill = T, fillColor = sapply(donnees, function(x) if(x>0){"#CD853F"}else{"#6495ED"}), fillOpacity = 1,
                                  group = "fleche"
            )
            
            ordre_analyse$a <- i
          }
          
          if(fond=="maille")
          {
            proxy <- addPolygons(map = proxy, data = fond_contour_maille_fs()[[2]], opacity = 1,
                                 stroke = TRUE, color = "grey", weight = 1,
                                 options = pathOptions(pane = paste0("fond_trio",i), clickable = T),
                                 popup = paste0("<b> <font color=#2B3E50>",as.data.frame(fond_contour_maille_fs()[[2]])[,"LIBELLE"], "</font> </b>"),
                                 fill = T, fillColor = "white", fillOpacity = 0.001,
                                 group = "maille_contour"
            )
          }
          
          if(fond=="contour")
          {
            proxy <- addPolygons(map = proxy, data = fond_contour_maille_fs()[[1]], opacity = 0.3,
                                 stroke = TRUE, color = "black", weight = 3,
                                 options = pathOptions(pane = paste0("fond_trio",i), clickable = T),
                                 popup = paste0("<b> <font color=#2B3E50>",as.data.frame(fond_contour_maille_fs()[[1]])[,"LIBELLE"], "</font> </b>"),
                                 fill = T, fillColor = "white", fillOpacity = 0.3,
                                 group = "maille_contour"
            )
          }
          
          i <- i + 1
        }
      },ignoreInit = TRUE)
      
      # MODIFICATION DES FLUX
      
      observeEvent(list(input$flux_min_fs_id,largeur_fs()),{
        
        proxy <- leafletProxy("mymap_fs")
        
        clearGroup(map = proxy, group = "fleche")
        
        analyse_WGS84 <- analyse_fs()[[2]]
        
        if(direction!="Sol")
        {
          if(is.na(input$flux_min_fs_id)) flux_min <- 1 else flux_min <- input$flux_min_fs_id
          
          analyse_WGS84 <- analyse_WGS84[as.data.frame(analyse_WGS84)[,varFlux]>=flux_min,]
          donnees <- merge(as.data.frame(analyse_WGS84)[,c("CODE1","CODE2")],data,by=c("CODE1","CODE2"),all.x=T)
          donnees <- sort(donnees[,varFlux],decreasing = TRUE)
        }else
        {
          donnees <- as.data.frame(analyse_WGS84)[,varFlux]
        }
        
        proxy <- addPolylines(map = proxy,
                              data = analyse_WGS84,
                              stroke = TRUE, color = "#303030",
                              opacity = 1,
                              weight = 1,
                              options = pathOptions(pane = paste0("fond_trio",ordre_analyse$a), clickable = T),
                              popup = paste0("<b><font color=#2B3E50>",varFlux," : ",donnees,"</font></b>"),
                              fill = T, fillColor = sapply(donnees, function(x) if(x>0){"#CD853F"}else{"#6495ED"}), fillOpacity = 1,
                              group = "fleche"
        )
      },ignoreInit = TRUE)
      
      # MODIFICATION DE LA SELECTION DES OBJETS VIA LES TABLEAUX
      
      observeEvent(input$onglets_fs,{
        req(input$onglets_fs)
        
        if(input$onglets_fs == "carte")
        {
          proxy <- leafletProxy("mymap_fs")
          
          clearGroup(map = proxy, group = "select_donnees")
          
          if(!is.null(input$mydonnees_fs_rows_selected))
          {
            suppressWarnings(proxy <- addPolylines(map = proxy,
                                                   data = fond_select_donnees_fs(),
                                                   stroke = TRUE, color = "#FFFF00",
                                                   opacity = 1,
                                                   weight = 2,
                                                   options = pathOptions(pane = "selection", clickable = F),
                                                   fill = F,
                                                   group = "select_donnees")
            )
          }
        }
      },ignoreInit = TRUE)
      
      observeEvent(input$onglets_fs,{
        req(input$onglets_fs)
        
        if(input$onglets_fs == "carte")
        {
          proxy <- leafletProxy("mymap_fs")
          
          clearGroup(map = proxy, group = "select_maille")
          
          if(!is.null(input$mymaille_fs_rows_selected))
          {
            proxy <- addPolygons(map = proxy, data = fond_select_maille_fs(),
                                 stroke = FALSE,
                                 options = pathOptions(pane = "selection", clickable = F),
                                 fill = T,
                                 fillColor = "#FFFF00",
                                 fillOpacity = 1,
                                 group = "select_maille"
            )
          }
        }
      },ignoreInit = TRUE)
      
      observeEvent(input$onglets_fs,{
        req(input$onglets_fs)
        
        if(input$onglets_fs == "carte")
        {
          proxy <- leafletProxy("mymap_fs")
          
          clearGroup(map = proxy, group = "select_contour")
          
          if(!is.null(input$mycontour_fs_rows_selected))
          {
            proxy <- addPolygons(map = proxy, data = fond_select_contour_fs,
                                 stroke = FALSE,
                                 options = pathOptions(pane = "selection", clickable = F),
                                 fill = T,
                                 fillColor = "#FFFF00",
                                 fillOpacity = 1,
                                 group = "select_contour"
            )
          }
        }
      },ignoreInit = TRUE)
      
      # CONSTRUCTION DE LA LEGENDE
      
      lon_lat_fs <- reactive({
        click <- input$mymap_fs_click
        lon <- click$lng
        lat <- click$lat
        return(list(lon,lat))
      })
      
      observeEvent(list(input$mymap_fs_zoom,input$mymap_fs_click,input$titre_flux_legende_fs_id,input$flux_min_fs_id,input$largeur_fs_id),{
        if(is.null(input$affiche_legende_fs_id)) return(NULL)
        
        if(input$affiche_legende_fs_id==FALSE) return(NULL)
        
        if(is.null(lon_lat_fs()[[1]])) return(NULL)
        
        CONSTRUCTION_LEGENDE_FS()
      })
      
      CONSTRUCTION_LEGENDE_FS <- function(lon,lat)
      {
        proxy <- leafletProxy("mymap_fs")
        
        proxy <- clearGroup(map=proxy, group="leg")
        proxy <- clearMarkers(map=proxy)
        
        zoom <- as.numeric(input$mymap_fs_zoom)
        coeff <- ((360/(2^zoom))/7.2) # Permet de fixer une distance sur l'ecran. Il s'agit en gros d'une conversion des degres en pixels. Reste constant a longitude egale mais varie un peu selon la latitude
        
        vmax <- max(abs(data.frame(analyse_fs()[[2]])[,varFlux]))
        
        coord_fleche_max <- st_coordinates(analyse_fs()[[2]][abs(data.frame(analyse_fs()[[2]])[,varFlux])==vmax,])
        large <- max(st_distance(st_sfc(st_point(c(coord_fleche_max[5,1],coord_fleche_max[5,2])),st_point(c(coord_fleche_max[6,1],coord_fleche_max[6,2])))))
        
        long <- coeff*1.5
        
        flux_legWGS84_s <- flux_legende_saphirs(lon_lat_fs()[[1]],lon_lat_fs()[[2]],long,large)
        flux_legWGS84 <- flux_legWGS84_s[[1]]
        pointe1 <- flux_legWGS84_s[[2]]
        pointe2 <- flux_legWGS84_s[[3]]
        
        # leaflet du cadre blanc en 1er
        proxy <- addRectangles(map = proxy,
                               lng1 = st_bbox(flux_legWGS84)[1]-coeff/2, lat1 = st_bbox(flux_legWGS84)[2]-coeff/2,
                               lng2 = st_bbox(flux_legWGS84)[3]+coeff*3, lat2 = st_bbox(flux_legWGS84)[4]+coeff*1.2,
                               stroke = FALSE,
                               options = pathOptions(pane = "fond_legende", clickable = F),
                               fill = T,
                               fillColor = "white",
                               fillOpacity = 0.8,
                               group="leg"
        )
        
        suppressWarnings(proxy <- addPolygons(map = proxy,
                                              data=flux_legWGS84,
                                              stroke = TRUE,
                                              opacity = 1,
                                              color = "#2B3E50",
                                              weight = 2,
                                              options = pathOptions(pane = "fond_legende", clickable = F),
                                              fill = T,
                                              fillColor = "white",
                                              fillOpacity = 1,
                                              group="leg"
        ))
        
        # leaflet valeur flux
        proxy <- addLabelOnlyMarkers(map = proxy,
                                     lng = pointe1[1], lat = pointe1[2], #grande fleche
                                     label = as.character(format(vmax,big.mark=" ",decimal.mark=",",nsmall=0)),
                                     labelOptions = labelOptions(noHide = T, textOnly = TRUE, direction = "right",
                                                                 style = list(
                                                                   "color" = "black",
                                                                   "font-size" = "12px"
                                                                 )),
                                     group="leg"
        )
        
        proxy <- addLabelOnlyMarkers(map = proxy,
                                     lng = pointe2[1], lat = pointe2[2], #petite fleche
                                     label = as.character(format(round(vmax/3,0),big.mark=" ",decimal.mark=",",nsmall=0)),
                                     labelOptions = labelOptions(noHide = T, textOnly = TRUE, direction = "right",
                                                                 style = list(
                                                                   "color" = "black",
                                                                   "font-size" = "12px"
                                                                 )),
                                     group="leg"
        )
        
        #leaflet titre 1
        proxy <- addLabelOnlyMarkers(map = proxy,
                                     lng = st_bbox(flux_legWGS84)[1]-coeff/3, lat = st_bbox(flux_legWGS84)[4]+coeff/2,
                                     label = input$titre_flux_legende_fs_id,
                                     labelOptions = labelOptions(noHide = T, textOnly = TRUE, direction = "right",
                                                                 style = list(
                                                                   "color" = "black",
                                                                   "font-size" = "14px"
                                                                 )),
                                     group="leg"
        )
      }
      
      # AJOUT DES ONGLETS SAUVEGARDE
      
      observeEvent(input$save_carte_fs_id,{
        
        showModal(modalDialog(HTML("<i class=\"fa fa-spinner fa-spin fa-2x fa-fw\"></i><font size=+1>Sauvegarde de la carte en cours...</font> "), size="m", footer=NULL, style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"))
        
        insert_save$a <- insert_save$a + 1
        nb_save_carte <- insert_save$a-remove_carte$a
        
        m_save <- m_save_fs$a
        
        if(nb_save_carte>6)
        {
          insert_save$a <- insert_save$a - 1
          showModal(modalDialog(HTML("<font size=+1>Vous ne pouvez pas sauvegarger plus de 6 cartes. Veuillez en supprimer avant de continuer.</font> "), size="l", footer=NULL, easyClose = TRUE, style = "color: #fff; background-color: #DF691A; border-color: #2e6da4")) #337ab7
          return(NULL)
        }
        
        output[[paste0("mymap_save_",insert_save$a,"_fs")]] <- renderLeaflet({
          
          if(!is.null(fondSuppl))
          {
            if(input$ajout_territoire_fs_id)
            {
              m_save <- addPolygons(map = m_save, data = fond_territoire_fs(),
                                    stroke = TRUE, color = "#BFBFBF", opacity = 1,
                                    weight = 0.5,
                                    options = pathOptions(pane = "fond_territoire", clickable = T),
                                    popup = paste0("<b> <font color=#2B3E50>",as.data.frame(fond_territoire_fs())[,"LIBELLE"], "</font> </b>"),
                                    fill = T, fillColor = "white", fillOpacity = 0.001
              )
            }
          }
          
          if(input$ajout_reg_fs_id)
          {
            m_save <- addPolygons(map = m_save, data = fond_region_fs(),
                                  stroke = TRUE, color = "grey", opacity = 1,
                                  weight = 1.5,
                                  options = pathOptions(pane = "fond_reg", clickable = F),
                                  fill = F
            )
          }
          
          if(input$ajout_dep_fs_id)
          {
            m_save <- addPolygons(map = m_save, data = fond_departement_fs(),
                                  stroke = TRUE, color = "grey", opacity = 1,
                                  weight = 0.5,
                                  options = pathOptions(pane = "fond_dep", clickable = F),
                                  fill = F
            )
          }
          
          i <- 1 #pour gerer l'ordre des fonds dans le pane
          
          for(fond in liste_fonds$a)
          {
            if(fond=="analyse")
            {
              analyse_WGS84 <- analyse_fs()[[2]]
              
              if(direction!="Sol")
              {
                analyse_WGS84 <- analyse_WGS84[as.data.frame(analyse_WGS84)[,varFlux]>=flux_min_fs(),]
                donnees <- merge(as.data.frame(analyse_WGS84)[,c("CODE1","CODE2")],data,by=c("CODE1","CODE2"),all.x=T)
                donnees <- sort(donnees[,varFlux],decreasing = TRUE)
              }else
              {
                donnees <- as.data.frame(analyse_WGS84)[,varFlux]
              }
              
              m_save <- addPolylines(map = m_save,
                                     data = analyse_WGS84,
                                     stroke = TRUE, color = "#303030",
                                     opacity = 1,
                                     weight = 1,
                                     options = pathOptions(pane = paste0("fond_trio",i), clickable = T),
                                     popup = paste0("<b><font color=#2B3E50>",varFlux," : ",donnees,"</font></b>"),
                                     fill = T, fillColor = sapply(donnees, function(x) if(x>0){"#CD853F"}else{"#6495ED"}), fillOpacity = 1
              )
            }
            
            if(fond=="maille")
            {
              m_save <- addPolygons(map = m_save, data = fond_contour_maille_fs()[[2]], opacity = 1,
                                    stroke = TRUE, color = "grey", weight = 1,
                                    options = pathOptions(pane = paste0("fond_trio",i), clickable = T),
                                    popup = paste0("<b> <font color=#2B3E50>",as.data.frame(fond_contour_maille_fs()[[2]])[,"LIBELLE"], "</font> </b>"),
                                    fill = T, fillColor = "white", fillOpacity = 0.001
              )
            }
            
            if(fond=="contour")
            {
              m_save <- addPolygons(map = m_save, data = fond_contour_maille_fs()[[1]], opacity = 0.3,
                                    stroke = TRUE, color = "black", weight = 3,
                                    options = pathOptions(pane = paste0("fond_trio",i), clickable = T),
                                    popup = paste0("<b> <font color=#2B3E50>",as.data.frame(fond_contour_maille_fs()[[1]])[,"LIBELLE"], "</font> </b>"),
                                    fill = T, fillColor = "white", fillOpacity = 0.3
              )
            }
            
            i <- i + 1
          }
          
          zoom <- as.numeric(input$mymap_fs_zoom)
          coeff <- ((360/(2^zoom))/7.2) # Permet de fixer une distance sur l'ecran. Il s'agit en gros d'une conversion des degres en pixels. Reste constant a longitude egale mais varie un peu selon la latitude
          
          vmax <- max(abs(data.frame(analyse_fs()[[2]])[,varFlux]))
          
          coord_fleche_max <- st_coordinates(analyse_fs()[[2]][abs(data.frame(analyse_fs()[[2]])[,varFlux])==vmax,])
          large <- max(st_distance(st_sfc(st_point(c(coord_fleche_max[5,1],coord_fleche_max[5,2])),st_point(c(coord_fleche_max[6,1],coord_fleche_max[6,2])))))
          
          long <- coeff*1.5
          
          flux_legWGS84_s <- flux_legende_saphirs(lon_lat_fs()[[1]],lon_lat_fs()[[2]],long,large)
          flux_legWGS84 <- flux_legWGS84_s[[1]]
          pointe1 <- flux_legWGS84_s[[2]]
          pointe2 <- flux_legWGS84_s[[3]]
          
          # leaflet du cadre blanc en 1er
          m_save <- addRectangles(map = m_save,
                                  lng1 = st_bbox(flux_legWGS84)[1]-coeff/2, lat1 = st_bbox(flux_legWGS84)[2]-coeff/2,
                                  lng2 = st_bbox(flux_legWGS84)[3]+coeff*3, lat2 = st_bbox(flux_legWGS84)[4]+coeff*1.2,
                                  stroke = FALSE,
                                  options = pathOptions(pane = "fond_legende", clickable = F),
                                  fill = T,
                                  fillColor = "white",
                                  fillOpacity = 0.8
          )
          
          suppressWarnings(m_save <- addPolygons(map = m_save,
                                                 data=flux_legWGS84,
                                                 stroke = TRUE,
                                                 opacity = 1,
                                                 color = "#2B3E50",
                                                 weight = 2,
                                                 options = pathOptions(pane = "fond_legende", clickable = F),
                                                 fill = T,
                                                 fillColor = "white",
                                                 fillOpacity = 1
          ))
          
          # leaflet valeur flux
          m_save <- addLabelOnlyMarkers(map = m_save,
                                        lng = pointe1[1], lat = pointe1[2], #grande fleche
                                        label = as.character(format(vmax,big.mark=" ",decimal.mark=",",nsmall=0)),
                                        labelOptions = labelOptions(noHide = T, textOnly = TRUE, direction = "right",
                                                                    style = list(
                                                                      "color" = "black",
                                                                      "font-size" = "12px"
                                                                    ))
          )
          
          m_save <- addLabelOnlyMarkers(map = m_save,
                                        lng = pointe2[1], lat = pointe2[2], #petite fleche
                                        label = as.character(format(round(vmax/3,0),big.mark=" ",decimal.mark=",",nsmall=0)),
                                        labelOptions = labelOptions(noHide = T, textOnly = TRUE, direction = "right",
                                                                    style = list(
                                                                      "color" = "black",
                                                                      "font-size" = "12px"
                                                                    ))
          )
          
          #leaflet titre 1
          m_save <- addLabelOnlyMarkers(map = m_save,
                                        lng = st_bbox(flux_legWGS84)[1]-coeff/3, lat = st_bbox(flux_legWGS84)[4]+coeff/2,
                                        label = input$titre_flux_legende_fs_id,
                                        labelOptions = labelOptions(noHide = T, textOnly = TRUE, direction = "right",
                                                                    style = list(
                                                                      "color" = "black",
                                                                      "font-size" = "14px"
                                                                    ))
          )
          
          removeModal()
          
          m_save
        })
        
        output[[paste0("remove_carte_",nb_save_carte,"_fs")]] <- renderUI({
          actionButton(paste0("remove_carte_",nb_save_carte,"_fs_id"),label="X Supprimer la carte", style="color:#FFFFFF; border-color:#FFFFFF; background-color:#2B3E50")
        })
        appendTab(inputId = "onglets_fs",
                  tabPanel(title=HTML(paste0("<font color=#AFC9E0> Carte ",insert_save$a,"</font>")),value=paste0("carte",nb_save_carte),uiOutput(paste0("remove_carte_",nb_save_carte,"_fs")),leafletOutput(paste0("mymap_save_",insert_save$a,"_fs"),width="100%",height = 800)),
                  select = TRUE,
                  session = session
        )
      }, ignoreInit = TRUE)
      
      observeEvent(input$remove_carte_1_fs_id,{
        remove_carte$a <- remove_carte$a + 1
        removeTab(inputId = "onglets_fs",
                  target = "carte1",
                  session = session
        )
      }, ignoreInit = TRUE)
      
      observeEvent(input$remove_carte_2_fs_id,{
        remove_carte$a <- remove_carte$a + 1
        removeTab(inputId = "onglets_fs",
                  target = "carte2",
                  session = session
        )
      }, ignoreInit = TRUE)
      
      observeEvent(input$remove_carte_3_fs_id,{
        remove_carte$a <- remove_carte$a + 1
        removeTab(inputId = "onglets_fs",
                  target = "carte3",
                  session = session
        )
      }, ignoreInit = TRUE)
      
      observeEvent(input$remove_carte_4_fs_id,{
        remove_carte$a <- remove_carte$a + 1
        removeTab(inputId = "onglets_fs",
                  target = "carte4",
                  session = session
        )
      }, ignoreInit = TRUE)
      
      observeEvent(input$remove_carte_5_fs_id,{
        remove_carte$a <- remove_carte$a + 1
        removeTab(inputId = "onglets_fs",
                  target = "carte5",
                  session = session
        )
      }, ignoreInit = TRUE)
      
      observeEvent(input$remove_carte_6_fs_id,{
        remove_carte$a <- remove_carte$a + 1
        removeTab(inputId = "onglets_fs",
                  target = "carte6",
                  session = session
        )
      }, ignoreInit = TRUE)
      
      # TABLEAUX DE DONNEES, MAILLE ET CONTOUR
      
      output$mydonnees_fs <- DT::renderDataTable(datatable({
        if(direction!="Sol")
        {
          analyse_WGS84 <- analyse_fs()[[2]][as.data.frame(analyse_fs()[[2]])[,varFlux]>=flux_min_fs(),]
          data <- merge(as.data.frame(analyse_WGS84)[,c("CODE1","CODE2")],data,by=c("CODE1","CODE2"),all.x=T)
        }else
        {
          data <- as.data.frame(analyse_fs()[[2]])
        }
        tableau_donnees <- data[rev(order(data[,varFlux])),c("CODE1","CODE2",varFlux)]
      },  style = 'bootstrap'
      ))
      
      output$mymaille_fs <- DT::renderDataTable(datatable({
        data <- as.data.frame(fondMaille)
        tableau_maille <- data[,c(1:2)]
      },  style = 'bootstrap'
      ))
      
      output$mycontour_fs <- DT::renderDataTable(datatable({
        data <- as.data.frame(fondContour)
        tableau_contour <- data[,c(1:2)]
      },  style = 'bootstrap'
      ))
      
      # ENVOI DU LEAFLET A L'UI
      
      output$mymap_fs <- renderLeaflet({
        react_fond_fs()
      })
    }
    
    runApp(shinyApp(ui = ui, server = server), launch.browser = TRUE)
    }
