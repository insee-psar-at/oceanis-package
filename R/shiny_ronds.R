shiny_ronds <-
function(data,fondMaille,fondMailleElargi=NULL,fondContour,fondSuppl=NULL,idData,varVolume,emprise="FRM",fondEtranger=NULL,fondChx=NULL)
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
    if(any(class(emprise)!="character")) msg_error8 <- "La valeur doit etre de type caractere ('FRM', '971', '972', '973', '974', '976' ou '999') / "
    if(!is.null(fondChx)) if(any(!any(class(fondChx) %in% "sf"),!any(class(fondChx) %in% "data.frame"))) msg_error9 <- "Le fond des chx doit etre un objet sf / "

    if(length(names(data))<2) msg_error10 <- "Le tableau des donnees n'est pas conforme. Il doit contenir au minimum une variable identifiant et la variable a representer / "
    if(length(names(fondMaille))<3) msg_error11 <- "Le fond de maille n'est pas conforme. La table doit contenir au minimum une variable identifiant, une variable libelle et la geometry / "
    if(!is.null(fondMailleElargi)) if(length(names(fondMailleElargi))<3) msg_error12 <- "Le fond de maille elargie n'est pas conforme. La table doit contenir au minimum une variable identifiant, une variable libelle et la geometry / "
    if(length(names(fondContour))<3) msg_error13 <- "Le fond de contour n'est pas conforme. La table doit contenir au minimum une variable identifiant, une variable libelle et la geometry / "
    if(!is.null(fondSuppl)) if(length(names(fondSuppl))<3) msg_error14 <- "Le fond supplementaire n'est pas conforme. La table doit contenir au minimum une variable identifiant, une variable libelle et la geometry / "

    if(!any(names(data) %in% idData))  msg_error15 <- "La variable identifiant les donnees n'existe pas dans la table des donnees / "
    if(!any(names(data) %in% varVolume))  msg_error16 <- "La variable a representer n'existe pas dans la table des donnees / "
    if(!emprise %in% c("FRM","971","972","973","974","976","999")) msg_error17 <- "La variable emprise doit etre 'FRM', '971', '972', '973', '974', '976', '999' / "
    if(!is.null(fondEtranger)) if(any(!any(class(fondEtranger) %in% "sf"),!any(class(fondEtranger) %in% "data.frame"))) msg_error18 <- "Le fond etranger doit etre un objet sf / "
    if(!is.null(fondEtranger)) if(length(names(fondEtranger))<3) msg_error19 <- "Le fond etranger n'est pas conforme. La table doit contenir au minimum une variable identifiant, une variable libelle et la geometry / "

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
    epsg_etranger <- NULL
    if(!is.null(fondEtranger))
    {
      names(fondEtranger)[1] <- "CODE"
      names(fondEtranger)[2] <- "LIBGEO"
      fondEtranger$LIBGEO<-iconv(fondEtranger$LIBGEO,"latin1","utf8")

      if(substr(st_crs(fondEtranger)[1]$input,1,5) == "EPSG:")
      {
        epsg_etranger <- substr(st_crs(fondEtranger)[1]$input,6,9)
      }else
      {
        epsg_etranger <- st_crs(fondEtranger)[1]$input
      }

      if(is.na(epsg_etranger) | epsg_etranger=="4326")
      {
        epsg_etranger <- "3395" # Mercator
      }
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
                                             style = "overflow-y:scroll; min-height: 1000px; max-height: 1000px",
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
                                                     leafletOutput("mymap_rp",width="112%",height = 950)
                                            ),
                                            tabPanel(title=HTML(paste0("<b>Donn","\u00e9","es</b>")),value="donnees",
                                                     h5("S\u00e9lectionnez une ou plusieurs lignes pour ensuite les visualiser sur la carte."),
                                                     DT::dataTableOutput("mydonnees_rp",width="112%",height = 950)),
                                            tabPanel(title=HTML("<b>Maille</b>"),value="maille",
                                                     h5("S\u00e9lectionnez une ou plusieurs lignes pour ensuite les visualiser sur la carte."),
                                                     DT::dataTableOutput("mymaille_rp",width="112%",height = 950)),
                                            tabPanel(title=HTML("<b>Contour</b>"),value="contour",
                                                     h5("S\u00e9lectionnez une ou plusieurs lignes pour ensuite les visualiser sur la carte."),
                                                     DT::dataTableOutput("mycontour_rp",width="112%",height = 950))
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
                               HTML("<font size=2>1- Ouvrir un logiciel de capture (Outil Capture de Windows par exemple).</font>"),
                               br(),
                               HTML(paste0("<font size=2>2- S\u00e9lectionner la zone \u00e0 capturer.</font>")),
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
        suppressWarnings(max_var <- max(abs(data[data[,"CODE"] %in% fondMaille$CODE,varVolume]), na.rm = TRUE))

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
                                                        owd <- setwd(tempdir())
                                                        on.exit(setwd(owd))
                                                        
                                                        rep_sortie <- dirname(file)
                                                        
                                                        dir.create("layers",showWarnings = F)
                                                        
                                                        files <- EXPORT_PROJET_QGIS_RP(file)
                                                        
                                                        zip::zip(zipfile = paste0("./",basename(file)),
                                                                 files = files,
                                                                 mode = "cherry-pick")
                                                      }
      )

      EXPORT_PROJET_QGIS_RP <- function(file)
      {
        showModal(modalDialog(HTML("<i class=\"fa fa-spinner fa-spin fa-2x fa-fw\"></i> <font size=+1>Export du projet Qgis en cours...</font> "), size="m", footer=NULL, style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"))

        sortie <- input$sortie_qgis_rp_id
        
        files <- c("layers", paste0(sortie,".qgs"))
        
        rep_sortie <- dirname(file)

        rupture <- FALSE
        rupture_elargi <- FALSE

        donnees <- analyse_rp()[[1]]$donnees$save
        if(any(donnees<0) & any(donnees>0)) rupture <- TRUE

        if(elargi_rp())
        {
          donnees_elargi <- analyse_rp()[[1]]$donnees_elargi$save
          if(any(donnees_elargi<0) & any(donnees_elargi>0)) rupture_elargi <- TRUE

          fond_maille_elargi <- st_transform(fond_elargi_rp()[[2]], crs= as.numeric(code_epsg_rp()))

          if(rupture_elargi)
          {
            ronds_carte_elargi <- analyse_ronds_sf_rp()[[2]][data.frame(analyse_ronds_sf_rp()[[2]])$save>0,]
            ronds_carte_elargi <- st_transform(ronds_carte_elargi, crs= as.numeric(code_epsg_rp()))
            ronds_carte_rupt_elargi <- analyse_ronds_sf_rp()[[2]][data.frame(analyse_ronds_sf_rp()[[2]])$save<0,]
            ronds_carte_rupt_elargi <- st_transform(ronds_carte_rupt_elargi, crs= as.numeric(code_epsg_rp()))

            suppressWarnings(st_write(ronds_carte_elargi, paste0(rep_sortie,"/layers/",sortie,"_elargi_ronds_carte.shp"), delete_dsn = TRUE, quiet = TRUE))
            suppressWarnings(st_write(ronds_carte_rupt_elargi, paste0(rep_sortie,"/layers/",sortie,"_elargi_ronds_rupt_carte.shp"), delete_dsn = TRUE, quiet = TRUE))
            suppressWarnings(st_write(fond_maille_elargi, paste0(rep_sortie,"/layers/fond_maille_elargi.shp"), delete_dsn = TRUE, quiet = TRUE))
          }else
          {
            ronds_carte_elargi <- analyse_ronds_sf_rp()[[2]]
            ronds_carte_elargi <- st_transform(ronds_carte_elargi, crs= as.numeric(code_epsg_rp()))

            suppressWarnings(st_write(ronds_carte_elargi, paste0(rep_sortie,"/layers/",sortie,"_elargi_ronds_carte.shp"), delete_dsn = TRUE, quiet = TRUE))
            suppressWarnings(st_write(fond_maille_elargi, paste0(rep_sortie,"/layers/fond_maille_elargi.shp"), delete_dsn = TRUE, quiet = TRUE))
          }
        }

        if(rupture)
        {
          ronds_carte <- analyse_ronds_sf_rp()[[1]][data.frame(analyse_ronds_sf_rp()[[1]])$save>0,]
          ronds_carte_rupt <- analyse_ronds_sf_rp()[[1]][data.frame(analyse_ronds_sf_rp()[[1]])$save<0,]

          ronds_carte <- st_transform(ronds_carte, crs= as.numeric(code_epsg_rp()))
          ronds_carte_rupt <- st_transform(ronds_carte_rupt, crs= as.numeric(code_epsg_rp()))

          suppressWarnings(write_ronds_carte <- try(st_write(ronds_carte, paste0(rep_sortie,"/layers/",sortie,"_ronds_carte.shp"), delete_dsn = TRUE, quiet = TRUE),silent=TRUE))
          suppressWarnings(write_ronds_carte_rupt <- try(st_write(ronds_carte_rupt, paste0(rep_sortie,"/layers/",sortie,"_ronds_rupt_carte.shp"), delete_dsn = TRUE, quiet = TRUE),silent=TRUE))
        }else
        {
          ronds_carte <- analyse_ronds_sf_rp()[[1]]
          ronds_carte <- st_transform(ronds_carte, crs= as.numeric(code_epsg_rp()))
          suppressWarnings(write_ronds_carte <- try(st_write(ronds_carte, paste0(rep_sortie,"/layers/",sortie,"_ronds_carte.shp"), delete_dsn = TRUE, quiet = TRUE),silent=TRUE))
        }

        fond_ronds_leg <- construction_legende_rp()[[1]][[2]]
        
        fond_maille <- st_transform(fondMaille, crs= as.numeric(code_epsg_rp()))
        fond_contour <- st_transform(fondContour, crs= as.numeric(code_epsg_rp()))
        if(!is.null(fondSuppl) && input$ajout_territoire_rp_id) fond_territoire <- st_transform(fond_territoire_rp(), crs= as.numeric(code_epsg_rp()))
        if(input$ajout_dep_rp_id) fond_departement <- st_transform(fond_departement_rp(), crs= as.numeric(code_epsg_rp()))
        if(input$ajout_reg_rp_id) fond_region <- st_transform(fond_region_rp(), crs= as.numeric(code_epsg_rp()))
        fond_france <- st_transform(fond_habillage_rp()[[1]], crs= as.numeric(code_epsg_rp()))
        fond_pays <- st_transform(fond_habillage_rp()[[2]], crs= as.numeric(code_epsg_rp()))

        suppressWarnings(st_write(ronds_carte, paste0(rep_sortie,"/layers/",sortie,"_ronds_carte.shp"), delete_dsn = TRUE, quiet = TRUE))
        if(rupture) suppressWarnings(st_write(ronds_carte_rupt, paste0(rep_sortie,"/layers/",sortie,"_ronds_rupt_carte.shp"), delete_dsn = TRUE, quiet = TRUE))
        if(rupture_elargi) suppressWarnings(st_write(ronds_carte_rupt_elargi, paste0(rep_sortie,"/layers/",sortie,"_elargi_ronds_rupt_carte.shp"), delete_dsn = TRUE, quiet = TRUE))
        suppressWarnings(st_write(fond_ronds_leg, paste0(rep_sortie,"/layers/fond_ronds_leg.shp"), delete_dsn = TRUE, quiet = TRUE))
        suppressWarnings(st_write(fond_maille, paste0(rep_sortie,"/layers/fond_maille.shp"), delete_dsn = TRUE, quiet = TRUE))
        suppressWarnings(st_write(fond_contour,paste0(rep_sortie,"/layers/fond_contour.shp"), delete_dsn = TRUE, quiet = TRUE))
        if(exists("fond_territoire")) if(!is.null(fond_territoire)) suppressWarnings(st_write(fond_territoire, paste0(rep_sortie,"/layers/fond_territoire.shp"), delete_dsn = TRUE, quiet = TRUE))
        if(exists("fond_departement")) if(!is.null(fond_departement)) suppressWarnings(st_write(fond_departement, paste0(rep_sortie,"/layers/fond_departement.shp"), delete_dsn = TRUE, quiet = TRUE))
        if(exists("fond_region")) if(!is.null(fond_region)) suppressWarnings(st_write(fond_region,paste0(rep_sortie,"/layers/fond_region.shp"), delete_dsn = TRUE, quiet = TRUE))
        suppressWarnings(st_write(fond_france,paste0(rep_sortie,"/layers/fond_france.shp"), delete_dsn = TRUE, quiet = TRUE))
        if(exists("fond_pays")) if(!is.null(fond_pays)) suppressWarnings(st_write(fond_pays,paste0(rep_sortie,"/layers/fond_pays.shp"), delete_dsn = TRUE, quiet = TRUE))

        titre1 <- paste0(input$titre1_qgis_rp_id,"\n")
        titre2 <- input$titre2_qgis_rp_id
        source <- input$source_qgis_rp_id
        annee <- format(Sys.time(), format = "%Y")

        l <- c()
        
        l <- c(l,"fond_ronds_leg")

        if(elargi_rp())
        {
          l=c(l,
              paste0(sortie,"_ronds_carte"),
              paste0(sortie,"_elargi_ronds_carte"),
              "fond_maille_elargi"
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
          l=c(l,
              paste0(sortie,"_ronds_carte")
          )
          if(rupture)
          {
            l=c(l,paste0(sortie,"_ronds_rupt_carte"))
          }
        }
        
        l <- c(l,"fond_france","fond_contour","fond_maille")
        
        if(exists("fond_territoire")) l <- c(l,"fond_territoire")
        if(exists("fond_departement")) l <- c(l,"fond_departement")
        if(exists("fond_region")) l <- c(l,"fond_region")
        
        if(exists("fond_pays")) l <- c(l,"fond_pays")

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
        code_epsg <- switch(emprise,
                            "FRM"="2154",# Lambert 93
                            "971"="5490",# UTM 20 N
                            "972"="5490",# UTM 20 N
                            "973"="2972",# UTM 22 N
                            "974"="2975",# UTM 40 S
                            "976"="4471",# UTM 38 S
                            "999"=epsg_etranger)
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

        analyse_WGS84 <- st_transform(analyse$analyse_points,crs=4326)

        return(list(analyse,analyse_WGS84))
      })

      fond_habillage_rp <- reactive({

        if(emprise=="FRM")
        {
          fond_pays <- st_transform(sf_paysm(),crs=4326)
          fond_france <- st_transform(sf_fram(),crs=4326)
        }else if(emprise!="999")
        {
          if(emprise=="971")
          {
            fond_france <- st_transform(sf_reg01(),crs=4326)
            fond_pays <- fond_france
          }
          if(emprise=="972")
          {
            fond_france <- st_transform(sf_reg02(),crs=4326)
            fond_pays <- fond_france
          }
          if(emprise=="973")
          {
            fond_france <- st_transform(sf_reg03(),crs=4326)
            fond_pays <- st_transform(sf_pays973(),crs=4326)
          }
          if(emprise=="974")
          {
            fond_france <- st_transform(sf_reg04(),crs=4326)
            fond_pays <- fond_france
          }
          if(emprise=="976")
          {
            fond_france <- st_transform(sf_reg06(),crs=4326)
            fond_pays <- fond_france
          }
        }else if(emprise=="999")
        {
          fond_france <- st_transform(fondEtranger,crs=4326)
          fond_pays <- fond_france
        }else{}

        return(list(fond_france,fond_pays))
      })

      fond_contour_maille_rp <- reactive({
        test_contour <- try(st_transform(fondContour,crs=4326), silent = TRUE)
        test_maille <- try(st_transform(fondMaille,crs=4326), silent = TRUE)

        if(any(list(class(test_contour),class(test_maille)) %in% "try-error"))
        {
          showModal(modalDialog(HTML(paste0("<font size=+1>Une erreur est survenue dans la cr","\u00e9","ation du territoire.<br><br>Veuillez svp v\u00e9rifier vos donn","\u00e9","es et les variables choisies.</font>")), size="m", footer=NULL, easyClose = TRUE, style = "color: #fff; background-color: #DF691A; border-color: #2e6da4"))
          erreur_maille$a <- TRUE
          return(NULL)
        }else
        {
          contour_WGS84 <- st_transform(fondContour,crs=4326)
          maille_WGS84 <- st_transform(fondMaille,crs=4326)
        }

        return(list(contour_WGS84,maille_WGS84))
      })

      fond_elargi_rp <- reactive({
        req(analyse_rp())
        if(elargi_rp())
        {
          analyse_WGS84_elargi <- st_transform(analyse_rp()[[1]]$analyse_points_elargi,crs=4326)
          maille_WGS84_elargi <- st_transform(fondMailleElargi,crs=4326)
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
            showModal(modalDialog(HTML(paste0("Le rayon du rond le plus grand est trop \u00e9lev\u00e9 et ne permet pas de respecter la r\u00e8gle s\u00e9miologique des 1/7\u00e8me. Le rayon max conseill\u00e9 est ",round(calcul_max_rayon_metres_rp()[[1]],2)," m\u00e8tres.")), size="l", footer=NULL, easyClose = TRUE, style = "color: #fff; background-color: #DF691A; border-color: #2e6da4")) #337ab7
          }
          taille_rond_m <- input$taille_rond_rp_id
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
          ronds <- st_sf(geometry=st_sfc(lapply(c(1:nrow(centres)),function(x) st_point(centres[x,])),crs=4326))
          ronds_pl_elargi <- st_buffer(st_transform(ronds, crs= as.numeric(code_epsg_rp())), calcul_rond_rp()*sqrt(analyse_rp()[[1]]$donnees_elargi[,varVolume]/calcul_max_rayon_metres_rp()[[2]]))

          # On cree les ronds
          dt_ronds_sf <- data.frame(ronds_pl_elargi,stringsAsFactors = F)
          analyse_ronds_sf_elargi <- st_sf(cbind(analyse_rp()[[1]]$donnees_elargi,dt_ronds_sf))
        }else
        {
          analyse_ronds_sf_elargi <- NULL
        }

        centres <- rbind(st_coordinates(analyse_rp()[[2]]))
        row.names(centres) <- c(1:(nrow(analyse_rp()[[1]]$donnees)))
        ronds <- st_sf(geometry=st_sfc(lapply(c(1:nrow(centres)),function(x) st_point(centres[x,])),crs=4326))
        ronds_pl <- st_buffer(st_transform(ronds, crs= as.numeric(code_epsg_rp())), calcul_rond_rp()*sqrt(analyse_rp()[[1]]$donnees[,varVolume]/calcul_max_rayon_metres_rp()[[2]]))
        # On cree les ronds
        dt_ronds_sf <- data.frame(ronds_pl,stringsAsFactors = F)
        analyse_ronds_sf <- st_sf(cbind(analyse_rp()[[1]]$donnees,dt_ronds_sf))
        return(list(analyse_ronds_sf,analyse_ronds_sf_elargi))
      })

      fond_territoire_rp <- reactive({
        if(!is.null(fondSuppl))
        {
          fond_territoire <- st_transform(fondSuppl,crs=4326)
          return(fond_territoire)
        }else
        {
          return(NULL)
        }
      })

      fond_region_rp <- reactive({
        fond_region <- st_transform(sf_regm(),crs=4326)
        return(fond_region)
      })

      fond_departement_rp <- reactive({
        fond_departement <- st_transform(sf_depm(),crs=4326)
        return(fond_departement)
      })

      fond_select_donnees_elargi_rp <- reactive({
        req(analyse_ronds_sf_rp(),analyse_rp())

        if(elargi_rp())
        {
          fond_donnees_elargi <- analyse_ronds_sf_rp()[[2]][as.data.frame(analyse_ronds_sf_rp()[[2]])[,"CODE"] %in% analyse_rp()[[1]]$donnees_elargi[input$mydonnees_rp_rows_selected,"CODE"],]
          fond_donnees_elargi <- st_transform(fond_donnees_elargi,crs=4326)
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
          fond_donnees <- st_transform(fond_donnees,crs=4326)
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

          if(is.null(fondEtranger))
          {
            proj4 <- st_crs(fondMaille)$proj4string
          }else{
            proj4 <- st_crs(fondEtranger)$proj4string
          }
          
          # Construction de la map par defaut

          m <- leaflet(padding = 0,
                       options = leafletOptions(
                         preferCanvas = TRUE,
                         transition = 2,
                         crs = leafletCRS(crsClass = "L.Proj.CRS",
                                          code = paste0("EPSG:", code_epsg_rp()),
                                          proj4def = proj4,
                                          resolutions = 2^(16:1)
                         )
                       )) %>%

            setMapWidgetStyle(list(background = "#AFC9E0")) %>%

            addTiles_insee(attribution = paste0("<a href=\"http://www.insee.fr\">OCEANIS - \u00A9 IGN - INSEE ",format(Sys.time(), format = "%Y"),"</a>")) %>%

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
            addMapPane(name = "fond_dep", zIndex = 403) %>%
            addMapPane(name = "fond_reg", zIndex = 404) %>%
            addMapPane(name = "fond_territoire", zIndex = 405) %>%
            addMapPane(name = "fond_trio3", zIndex = 406) %>%
            addMapPane(name = "fond_trio2", zIndex = 407) %>%
            addMapPane(name = "fond_trio1", zIndex = 408) %>%
            addMapPane(name = "selection", zIndex = 409) %>%

            addMapPane(name = "fond_legende", zIndex = 410)

          # AFFICHAGE DES FONDS D'HABILLAGE

          if(emprise %in% c("FRM","973"))
          {
            # fond des pays
            m <- addPolygons(map = m, data = fond_habillage_rp()[[2]][,"LIBGEO"], opacity = 1, # fond_pays sauf la France
                             stroke = TRUE, color = "white",
                             weight = 1,
                             options = pathOptions(pane = "fond_pays", clickable = F),
                             fill = T, fillColor = "#CCCCCC", fillOpacity = 1
            )
          }

          # fond de la France metro, DOM ou etranger
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
          analyse_WGS84 <- st_transform(analyse$analyse_points,crs=4326)

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
                          fillColor = sapply(analyse$donnees$save, function(x) if(x>0){"#EB617F"}else{"#286AC7"}),
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

        if(emprise=="FRM")
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

        if(emprise=="FRM")
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
                                fillColor = sapply(analyse_rp()[[1]]$donnees$save, function(x) if(x>0){"#EB617F"}else{"#286AC7"}),
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
                            fillColor = sapply(analyse_rp()[[1]]$donnees$save, function(x) if(x>0){"#EB617F"}else{"#286AC7"}),
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
                            fillColor = sapply(analyse_rp()[[1]]$donnees$save, function(x) if(x>0){"#EB617F"}else{"#286AC7"}),
                            fillOpacity = 1,
                            group = "taille"
        )

      },ignoreInit = TRUE)

      # MODIFICATION DE LA REPRESENTATION ELARGIE

      observeEvent(list(input$elargi_rp_id,input$opacite_elargi_rp_id,input$taille_rond_rp_id,input$choix_centroid_rp_id),{
        req(input$opacite_elargi_rp_id,input$taille_rond_rp_id)

        proxy <- leafletProxy("mymap_rp")
        
        clearGroup(map = proxy, group = "elargi")

        if(elargi_rp())
        {
          proxy <- addPolygons(map = proxy, data = fond_elargi_rp()[[2]], opacity = 1, #maille_WGS84_elargi
                               stroke = TRUE, color = "grey", weight = 1,
                               options = pathOptions(pane = "fond_trio1", clickable = T),
                               popup = paste0("<b> <font color=#2B3E50>",analyse_rp()[[1]]$donnees_elargi[order(analyse_rp()[[1]]$donnees_elargi[,"CODE"]),"LIBELLE"], "</font> </b>"),
                               fill = T, fillColor = "white", fillOpacity = 0.001,
                               group = "elargi"
          )
          
          proxy <- addCircles(map = proxy,
                              lng = st_coordinates(fond_elargi_rp()[[1]])[,1],
                              lat = st_coordinates(fond_elargi_rp()[[1]])[,2],
                              stroke = TRUE, color = "white",
                              opacity = input$opacite_elargi_rp_id/100,
                              weight = 1,
                              radius = calcul_rond_rp()*sqrt(analyse_rp()[[1]]$donnees_elargi[,varVolume]/calcul_max_rayon_metres_rp()[[2]]),
                              options = pathOptions(pane = "fond_trio1", clickable = T),
                              popup = paste0("<b> <font color=#2B3E50>",varVolume," : </font></b>",analyse_rp()[[1]]$donnees_elargi$TXT1),
                              fill = T,
                              fillColor = sapply(analyse_rp()[[1]]$donnees_elargi$save, function(x) if(x>0){"#EB617F"}else{"#286AC7"}),
                              fillOpacity = input$opacite_elargi_rp_id/100,
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

      construction_legende_rp <- reactive({
        ronds_leg <- construction_ronds_legende(lon_lat_rp()[[1]],lon_lat_rp()[[2]],code_epsg_rp(),input$taille_rond_rp_id)
        lignes <- construction_lignes_legende(ronds_leg,code_epsg_rp())
        ronds_leg[[2]] <- cbind(ronds_leg[[2]],ETI_VAL=c(max(abs(data[,varVolume]), na.rm = TRUE),max(abs(data[,varVolume]), na.rm = TRUE)/3))
        return(list(ronds_leg,lignes))
      })
      
      observeEvent(list(input$mymap_rp_zoom,input$mymap_rp_click,input$titre_ronds_legende_rp_id,input$taille_rond_rp_id),{
        req(input$taille_rond_rp_id)

        if(is.null(input$affiche_legende_rp_id)) return(NULL)

        if(input$affiche_legende_rp_id==FALSE) return(NULL)

        if(is.null(lon_lat_rp()[[1]])) return(NULL)

        proxy <- leafletProxy("mymap_rp")
        
        proxy <- clearGroup(map=proxy, group="leg")
        proxy <- clearGroup(map=proxy, group="leg_rectangle")
        proxy <- clearMarkers(map=proxy)
        
        ronds_leg <- construction_legende_rp()[[1]]
        lignes <- construction_legende_rp()[[2]]
        
        # On ajoute un cadre blanc autour de la legende
        bbox_ronds <- st_bbox(ronds_leg[[2]])
        bbox_lignes <- st_bbox(lignes[[2]])
        rectangle <- c(bbox_ronds[1],bbox_ronds[2],bbox_lignes[3],bbox_ronds[4])
        large <- rectangle[3]-rectangle[1]
        rectangle[1] <- rectangle[1] - large / 3
        rectangle[2] <- rectangle[2] - large / 3
        rectangle[3] <- rectangle[3] + large / 6 * nchar(max(abs(data[,varVolume]), na.rm = TRUE))
        rectangle[4] <- rectangle[4] + large
        
        vec <- matrix(c(rectangle[1],rectangle[2],   rectangle[3],rectangle[2],   rectangle[3],rectangle[4],   rectangle[1],rectangle[4],   rectangle[1],rectangle[2]),5,2,byrow=T)
        rectangle <- st_sfc(st_polygon(list(vec)), crs = as.numeric(code_epsg_rp()))
        
        rectangle <- st_transform(rectangle, crs = 4326)
        
        # leaflet du cadre blanc en 1er
        proxy <- addPolygons(map = proxy,
                             data = rectangle,
                             stroke = FALSE,
                             options = pathOptions(pane = "fond_legende", clickable = F),
                             fill = T,
                             fillColor = "white",
                             fillOpacity = 0.8,
                             group = "leg_rectangle"
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
                                             group = "leg")
        )
        
        # leaflet lignes
        proxy <- addPolygons(map = proxy,
                             data = lignes[[1]],
                             stroke = TRUE,
                             opacity = 1,
                             color = "#2B3E50",
                             weight = 2,
                             options = pathOptions(pane = "fond_legende", clickable = F),
                             fill = F,
                             fillOpacity = 1,
                             group = "leg"
        )
        
        # leaflet valeur ronds
        proxy <- addLabelOnlyMarkers(map = proxy,
                                     lng = st_bbox(lignes[[1]][1,])[3],
                                     lat = st_bbox(lignes[[1]][1,])[4], #ligne_grand
                                     label = as.character(format(round(calcul_max_rayon_metres_rp()[[2]],0),big.mark=" ",decimal.mark=",",nsmall=0)),
                                     labelOptions = labelOptions(noHide = T, textOnly = TRUE, direction = "right",
                                                                 style = list(
                                                                   "color" = "black",
                                                                   "font-size" = "12px"
                                                                 )),
                                     group = "leg"
        )
        
        proxy <- addLabelOnlyMarkers(map = proxy,
                                     lng = st_bbox(lignes[[1]][2,])[3],
                                     lat = st_bbox(lignes[[1]][2,])[4], #ligne_petit
                                     label = as.character(format(round(calcul_max_rayon_metres_rp()[[2]]/3,0),big.mark=" ",decimal.mark=",",nsmall=0)),
                                     labelOptions = labelOptions(noHide = T, textOnly = TRUE, direction = "right",
                                                                 style = list(
                                                                   "color" = "black",
                                                                   "font-size" = "12px"
                                                                 )),
                                     group = "leg"
        )
        
        #leaflet titre
        
        rectangle <- st_transform(rectangle, crs = as.numeric(code_epsg_rp()))
        
        pt_titre <- st_sfc(st_geometry(st_point(c(as.numeric(st_bbox(rectangle)[1]) + large/3,
                                                  as.numeric(st_bbox(rectangle)[4]) - large/3))),
                           crs = as.numeric(code_epsg_rp()))
        
        pt_titre <- st_transform(pt_titre, crs = 4326)
        
        proxy <- addLabelOnlyMarkers(map = proxy,
                                     lng = st_coordinates(pt_titre)[1],
                                     lat = st_coordinates(pt_titre)[2],
                                     label = input$titre_ronds_legende_rp_id,
                                     labelOptions = labelOptions(noHide = T, textOnly = TRUE, direction = "right",
                                                                 style = list(
                                                                   "color" = "black",
                                                                   "font-size" = "14px"
                                                                 )),
                                     group = "leg"
        )
      })
      
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
            if(isolate(input$ajout_territoire_rp_id))
            {
              m_save <- addPolygons(map = m_save, data = isolate(fond_territoire_rp()),
                                    stroke = TRUE, color = "#BFBFBF", opacity = 1,
                                    weight = 0.5,
                                    options = pathOptions(pane = "fond_territoire", clickable = T),
                                    popup = paste0("<b> <font color=#2B3E50>",as.data.frame(isolate(fond_territoire_rp()))[,"LIBELLE"], "</font> </b>"),
                                    fill = T, fillColor = "white", fillOpacity = 0.001
              )
            }
          }

          if(isolate(input$ajout_reg_rp_id))
          {
            m_save <- addPolygons(map = m_save, data = isolate(fond_region_rp()),
                                  stroke = TRUE, color = "grey", opacity = 1,
                                  weight = 1.5,
                                  options = pathOptions(pane = "fond_reg", clickable = F),
                                  fill = F
            )
          }

          if(isolate(input$ajout_dep_rp_id))
          {
            m_save <- addPolygons(map = m_save, data = isolate(fond_departement_rp()),
                                  stroke = TRUE, color = "grey", opacity = 1,
                                  weight = 0.5,
                                  options = pathOptions(pane = "fond_dep", clickable = F),
                                  fill = F
            )
          }

          i <- 1 #pour gerer l'ordre des fonds dans le pane

          for(fond in isolate(liste_fonds$a))
          {
            if(fond=="analyse")
            {
              m_save <- addCircles(map = m_save,
                                   lng = st_coordinates(isolate(analyse_rp())[[2]])[,1],
                                   lat = st_coordinates(isolate(analyse_rp())[[2]])[,2],
                                   stroke = TRUE, color = "white",
                                   opacity = 1,
                                   weight = 1,
                                   radius = isolate(calcul_rond_rp())*sqrt(isolate(analyse_rp())[[1]]$donnees[,varVolume]/isolate(calcul_max_rayon_metres_rp())[[2]]),
                                   options = pathOptions(pane = paste0("fond_trio",i), clickable = T),
                                   popup = paste0("<b> <font color=#2B3E50>",varVolume," : </font></b>",isolate(analyse_rp())[[1]]$donnees$TXT1),
                                   fill = T,
                                   fillColor = sapply(isolate(analyse_rp())[[1]]$donnees$save, function(x) if(x>0){"#EB617F"}else{"#286AC7"}),
                                   fillOpacity = 1
              )
            }

            if(fond=="maille")
            {
              m_save <- addPolygons(map = m_save, data = isolate(fond_contour_maille_rp())[[2]], opacity = 1, #maille_WGS84
                                    stroke = TRUE, color = "grey", weight = 1,
                                    options = pathOptions(pane = paste0("fond_trio",i), clickable = T),
                                    popup = paste0("<b> <font color=#2B3E50>",as.data.frame(isolate(fond_contour_maille_rp())[[2]])[,"LIBELLE"], "</font> </b>"),
                                    fill = T, fillColor = "white", fillOpacity = 0.001
              )
            }

            if(fond=="contour")
            {
              m_save <- addPolygons(map = m_save, data = isolate(fond_contour_maille_rp())[[1]], opacity = 0.3, #contour_WGS84
                                    stroke = TRUE, color = "black", weight = 3,
                                    options = pathOptions(pane = paste0("fond_trio",i), clickable = T),
                                    popup = paste0("<b> <font color=#2B3E50>",as.data.frame(isolate(fond_contour_maille_rp())[[1]])[,"LIBELLE"], "</font> </b>"),
                                    fill = T, fillColor = "white", fillOpacity = 0.3
              )
            }

            i <- i + 1
          }

          if(isolate(elargi_rp()))
          {
            m_save <- addCircles(map = m_save,
                                 lng = st_coordinates(isolate(fond_elargi_rp())[[1]])[,1],
                                 lat = st_coordinates(isolate(fond_elargi_rp())[[1]])[,2],
                                 stroke = TRUE, color = "white",
                                 opacity = isolate(input$opacite_elargi_rp_id)/100,
                                 weight = 1,
                                 radius = isolate(calcul_rond_rp())*sqrt(isolate(analyse_rp())[[1]]$donnees_elargi[,varVolume]/isolate(calcul_max_rayon_metres_rp())[[2]]),
                                 options = pathOptions(pane = "fond_trio3", clickable = T),
                                 popup = paste0("<b> <font color=#2B3E50>",varVolume," : </font></b>",isolate(analyse_rp())[[1]]$donnees_elargi$TXT1),
                                 fill = T,
                                 fillColor = sapply(isolate(analyse_rp())[[1]]$donnees_elargi$save, function(x) if(x>0){"#EB617F"}else{"#286AC7"}),
                                 fillOpacity = isolate(input$opacite_elargi_rp_id)/100
            )

            m_save <- addPolygons(map = m_save, data = isolate(fond_elargi_rp())[[2]], opacity = 1, #maille_WGS84_elargi
                                  stroke = TRUE, color = "grey", weight = 1,
                                  options = pathOptions(pane = "fond_trio3", clickable = T),
                                  popup = paste0("<b> <font color=#2B3E50>",isolate(analyse_rp())[[1]]$donnees_elargi[order(isolate(analyse_rp())[[1]]$donnees_elargi[,"CODE"]),"LIBELLE"], "</font> </b>"),
                                  fill = T, fillColor = "white", fillOpacity = 0.001
            )
          }
          
          if(!is.null(isolate(lon_lat_rp()[[1]])))
          {
            ronds_leg <- isolate(construction_legende_rp())[[1]]
            lignes <- isolate(construction_legende_rp())[[2]]
            
            # On ajoute un cadre blanc autour de la legende
            bbox_ronds <- st_bbox(ronds_leg[[2]])
            bbox_lignes <- st_bbox(lignes[[2]])
            rectangle <- c(bbox_ronds[1],bbox_ronds[2],bbox_lignes[3],bbox_ronds[4])
            large <- rectangle[3]-rectangle[1]
            rectangle[1] <- rectangle[1] - large / 3
            rectangle[2] <- rectangle[2] - large / 3
            rectangle[3] <- rectangle[3] + large / 6 * nchar(max(abs(data[,varVolume]), na.rm = TRUE))
            rectangle[4] <- rectangle[4] + large
            
            vec <- matrix(c(rectangle[1],rectangle[2],   rectangle[3],rectangle[2],   rectangle[3],rectangle[4],   rectangle[1],rectangle[4],   rectangle[1],rectangle[2]),5,2,byrow=T)
            rectangle <- st_sfc(st_polygon(list(vec)), crs = as.numeric(isolate(code_epsg_rp())))
            
            rectangle <- st_transform(rectangle, crs = 4326)
            
            # leaflet du cadre blanc en 1er
            m_save <- addPolygons(map = m_save,
                                  data = rectangle,
                                  stroke = FALSE,
                                  options = pathOptions(pane = "fond_legende", clickable = F),
                                  fill = T,
                                  fillColor = "white",
                                  fillOpacity = 0.8,
                                  group = "leg_rectangle"
            )
            
            suppressWarnings(m_save <- addCircles(map = m_save,
                                                  lng = st_coordinates(st_centroid(ronds_leg[[1]]))[,1],
                                                  lat = st_coordinates(st_centroid(ronds_leg[[1]]))[,2],
                                                  stroke = TRUE,
                                                  opacity = 1,
                                                  color = "#2B3E50",
                                                  weight = 2,
                                                  radius = c(isolate(calcul_rond_rp()),isolate(calcul_rond_rp())/sqrt(3)),
                                                  options = pathOptions(pane = "fond_legende", clickable = F),
                                                  fill = T,
                                                  fillColor = "white",
                                                  fillOpacity = 1,
                                                  group = "leg")
            )
            
            # leaflet lignes
            m_save <- addPolygons(map = m_save,
                                  data = lignes[[1]],
                                  stroke = TRUE,
                                  opacity = 1,
                                  color = "#2B3E50",
                                  weight = 2,
                                  options = pathOptions(pane = "fond_legende", clickable = F),
                                  fill = F,
                                  fillOpacity = 1,
                                  group = "leg"
            )
            
            # leaflet valeur ronds
            m_save <- addLabelOnlyMarkers(map = m_save,
                                          lng = st_bbox(lignes[[1]][1,])[3],
                                          lat = st_bbox(lignes[[1]][1,])[4], #ligne_grand
                                          label = as.character(format(isolate(calcul_max_rayon_metres_rp())[[2]],big.mark=" ",decimal.mark=",",nsmall=0)),
                                          labelOptions = labelOptions(noHide = T, textOnly = TRUE, direction = "right",
                                                                      style = list(
                                                                        "color" = "black",
                                                                        "font-size" = "12px"
                                                                      )),
                                          group = "leg"
            )
            
            m_save <- addLabelOnlyMarkers(map = m_save,
                                          lng = st_bbox(lignes[[1]][2,])[3],
                                          lat = st_bbox(lignes[[1]][2,])[4], #ligne_petit
                                          label = as.character(format(round(isolate(calcul_max_rayon_metres_rp())[[2]]/3,0),big.mark=" ",decimal.mark=",",nsmall=0)),
                                          labelOptions = labelOptions(noHide = T, textOnly = TRUE, direction = "right",
                                                                      style = list(
                                                                        "color" = "black",
                                                                        "font-size" = "12px"
                                                                      )),
                                          group = "leg"
            )
            
            #leaflet titre
            
            rectangle <- st_transform(rectangle, crs = as.numeric(isolate(code_epsg_rp())))
            
            pt_titre <- st_sfc(st_geometry(st_point(c(as.numeric(st_bbox(rectangle)[1]) + large/3,
                                                      as.numeric(st_bbox(rectangle)[4]) - large/3))),
                               crs = as.numeric(isolate(code_epsg_rp())))
            
            pt_titre <- st_transform(pt_titre, crs = 4326)
            
            m_save <- addLabelOnlyMarkers(map = m_save,
                                          lng = st_coordinates(pt_titre)[1],
                                          lat = st_coordinates(pt_titre)[2],
                                          label = isolate(input$titre_ronds_legende_rp_id),
                                          labelOptions = labelOptions(noHide = T, textOnly = TRUE, direction = "right",
                                                                      style = list(
                                                                        "color" = "black",
                                                                        "font-size" = "14px"
                                                                      )),
                                          group = "leg"
            )
          }
          
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

      output$mydonnees_rp <- DT::renderDataTable(DT::datatable({
        if(elargi_rp())
          data <- analyse_rp()[[1]]$donnees_elargi
        else
          data <- analyse_rp()[[1]]$donnees

        data <- data[,c("CODE","LIBELLE","save")]
        names(data) <- c("CODE","LIBELLE",varVolume)
        tableau_donnees <- data[,c("CODE","LIBELLE",varVolume)]
      },  style = 'bootstrap'
      ))

      output$mymaille_rp <- DT::renderDataTable(DT::datatable({
        if(elargi_rp())
          data <- as.data.frame(fondMailleElargi)
        else
          data <- as.data.frame(fondMaille)

        tableau_maille <- data[,c(1:2)]
      },  style = 'bootstrap'
      ))

      output$mycontour_rp <- DT::renderDataTable(DT::datatable({
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
